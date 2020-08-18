{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | The main entry point of the application. There's a lot to take in here, but
-- everything's split into three parts:
--
--   1. The computation thread. This thread simply runs the Accelerate program
--      in a loop.
--   2. An input handling thread. Since we only have to support camera movement
--      we can simply run this on a different thread. The benefit over doing
--      this on the same thread that handles the graphical output is that our
--      rendering does not get blocked whenever we're handling using input.
--   3. The rendering thread. This is where we take the results generated by the
--      first thread and pass them on to an OpenGL fragment shader. This way we
--      can utilize our GPU to average all the samples and to do some simple
--      post processing.
--
-- All of these three things are run on seperate threads, and they communicate
-- using only a single 'Result' object wrapped in an 'MVar'.
module Main where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import qualified Data.Array.Accelerate.IO.Data.Vector.Storable
                                               as A
import           Data.ByteString.Char8          ( ByteString )
import qualified Data.ByteString.Char8         as BS
import           Data.Int                       ( Int32 )
import qualified Data.Text                     as T
import qualified Data.Vector.Storable          as V
import           Data.Vector.Storable           ( unsafeWith )
import           Foreign.C.Types                ( CInt(..) )
import qualified Graphics.GLUtil               as GLU
import qualified Graphics.Rendering.OpenGL     as GL
import qualified Options.Applicative           as O
import           SDL                     hiding ( Point
                                                , get
                                                , translation
                                                )
import qualified SDL.Font                      as Font
import qualified SDL
import           System.IO
import           System.IO.Temp

import qualified Files
import           Lib
import           Scene.Trace
import           Scene.Objects                  ( Camera
                                                , Direction
                                                , Point
                                                , RenderResult
                                                , rotation'
                                                )
import           Scene.World                    ( initialCamera )
import           Util

-- | A compiled rendering function for a specific camera position and
-- orientation. We store this function next to the other rendering data for
-- simplicity's sake. See 'compileFor' for more information.
type CompiledFunction
  = Camera -> (Int, RenderResult) -> (Int, RenderResult)

-- | The rendering results. The resulting 'Color' matrix should be divided by
-- the number of iterations in order to obtain the final averaged image.
--
-- We store both the compilation function and the compiled rendering function
-- here since the former is dependant on the parsed command line arguments, and
-- the latter has to be updated whenever the camera position changes.
data Result = Result
  { _resultCompute :: CompiledFunction
  , _resultValue   :: (Int, RenderResult)
  , _resultCamera  :: Camera
  }

makeFields ''Result

-- | Texture units IDs that the rendering texture and any rendered text should
-- get bound to.
resultTexUnit, textTexUnit :: GL.GLuint
resultTexUnit = 0
textTexUnit = 1

-- | The (Euclidean) distance the camera should be able to move in one seconds.
movementSpeed :: Float
movementSpeed = 3.0

type Options = Algorithm

cliOptions :: O.Parser Options
cliOptions = O.option
  (O.eitherReader $ \case
    s | s == optionStreams -> Right Streams
    s | s == optionInline -> Right Inline
    s ->
      Left
        $  "'"
        ++ s
        ++ "' is not a valid option, valid options are '"
        ++ optionStreams
        ++ "' and '"
        ++ optionInline
        ++ "'."
  )
  (  O.long "variant"
  <> O.metavar (optionStreams ++ '|' : optionInline)
  <> O.help
       "The algorithm to use. See the doumentation in 'Scene.Trace' for more details."
  <> O.value Inline
  )

main :: IO ()
main = do
  arguments <- O.execParser $ O.info
    (cliOptions O.<**> O.helper)
    (  O.fullDesc
    <> O.header
         "tracer - An collection of inefficient path tracing algorithms written in Accelerate"
    )

  initializeAll
  Font.initialize

  window <- createWindow "Leipe Mocro Tracer" $ defaultWindow
    { -- We'll only grab the mouse input when the right button is pressed
      windowInputGrabbed    = False
    , windowInitialSize     = V2 (CInt screenWidth) (CInt screenHeight)
    , windowGraphicsContext = OpenGLContext
                                $ defaultOpenGL { glProfile = Core Normal 3 3 }
    }

  let compute' = compileFor arguments
  seeds   <- initialOutput
  mResult <- newMVar $! Result { _resultCompute = compute'
                               , _resultValue   = compute' initialCamera (0, seeds)
                               , _resultCamera  = initialCamera
                               }

  putStrLn
    $  "Using the '"
    ++ show arguments
    ++ "' algorithm, see 'tracer --help' for more options"
  when (arguments == Streams) $ putStrLn
    $  "\nWARNING: The streams algorithm is slow and currently does not "
    ++ "work properly with the CUDA backend"

  putStrLn ""
  putStrLn "          Camera Controls"
  putStrLn "  ───────────────────────────────"
  putStrLn "      W     [Shift] Sprint"
  putStrLn "    A   D   [Space] Move up"
  putStrLn "      S     [Ctrl] Move down"
  putStrLn "            [RMB + drag] Rotate"
  putStrLn ""

  computationThreadId <- forkOS $ computationLoop mResult
  graphicsThreadId    <- forkOS $ graphicsLoop window mResult
  inputLoop mResult

  killThread graphicsThreadId
  killThread computationThreadId

-- | Create a function for rendering a single sample based on the current camera
-- position. This function is meant to be reused until the 'Camera' position
-- gets updated. This way Accelerate does not have to recompile every frame.
compileFor :: Options -> CompiledFunction
compileFor !config =
  let dewit = runN (render config) screenPixels
  in  \(!c) (!iterations, !acc) -> (iterations + 1, dewit (scalar c) acc)

-- | Perform the actual path tracing. This is done in a seperate thread that
-- shares and 'MVar' with the rendering thread to prevent one of the processes
-- from blocking another.
--
-- We use a 'State' monad here to keep track of when we should reseed our RNGs.
computationLoop :: MVar Result -> IO ()
computationLoop mResult = flip evalStateT reseedInterval $ forever $ do
  currentIterations <- liftIO $ do
    result <- takeMVar mResult

    -- We can gain some performance by calculating multiple samples at once, but
    -- it'll reduce the responsiveness of our application. By doing this only
    -- once we reach a certain threshold we can still make use of this
    -- optimization while keeping it responsive.
    -- TODO: Get rid of this hack
    let batchSize                = max 30 $ (result ^. value . _1) `div` 50
        value'@(!iterations, !_) = if (result ^. value . _1) > 100
          then doTimes batchSize (result ^. compute $ result ^. camera) (result ^. value)
          else ((result ^. compute) (result ^. camera) (result ^. value))
        !result' = result & value .~ value'

    void $! putMVar mResult $! result'
    return iterations

  -- The RNGs should be reseeded every 2000 iterations to prevent convergence
  reseedAt <- get
  if currentIterations > reseedAt
    then do
      liftIO $ do
        -- This could potentially reseed an empty rendering result if camera
        -- movement happens exactly between here and the 'lifIO' block above,
        -- but it won't cause any data races. We only have to update the texture
        -- so we'll leave the first element of the tuple alone.
        result   <- takeMVar mResult
        reseeded <- reseed $ result ^. value . _2

        void $! putMVar mResult $! result & value . _2 .~ reseeded
      modify (+ reseedInterval)
    else when (currentIterations < reseedInterval) $ put reseedInterval
 where
  -- | How many samples we can render before we have to reseed the RNGs
  reseedInterval :: Int
  reseedInterval = 2000
  -- | Compose a function with itself @n@ times.
  doTimes :: Int -> (a -> a) -> a -> a
  doTimes n f = (!! n) . iterate f

-- | Handle the user input. The 'Result' 'MVar' gets updated whenever the camera
-- gets moved. This should be run in the main thread, since quitting the
-- application will end this action.
inputLoop :: MVar Result -> IO ()
inputLoop mResult = time >>= go
 where
  initialDeltas :: (Point, Direction)
  initialDeltas = (V3 0.0 0.0 0.0, V3 0.0 0.0 0.0)

  -- | Cap the camera rotation's roll (the vertical rotation). Otherwise
  -- continuously rotating the camera upwards results in some wierd behaviour.
  clampRoll :: Direction -> Direction
  clampRoll (V3 roll pitch yaw) = V3 (min maxRoll $ max minRoll roll) pitch yaw
   where
    minRoll = negate pi / 2 + 0.001
    maxRoll = pi / 2 - 0.001

  go :: Float -> IO ()
  go lastFrame = do
    events             <- map eventPayload <$> pollEvents

    currentFrame       <- time
    keyDown            <- getKeyboardState
    allowMouseMovement <- (==) RelativeLocation <$> getMouseLocationMode
    let elapsed = currentFrame - lastFrame
        movementDistance =
          movementSpeed * elapsed * if keyDown ScancodeLShift then 1 else 0.25
        shouldQuit =
          QuitEvent `elem` events || keyDown ScancodeQ || keyDown ScancodeEscape

    -- Camera movement
    -- We use the 'State' monad to accumulate camera movements before processing
    -- them to prevent unneeded camera updates.
    !deltas <- flip execStateT initialDeltas $ do
      forM_
        events
        (\case
          -- The right mouse button enables mouse look. SDL's relative mouse
          -- location mode also implicitely hides the cursor and enables window
          -- grab.
          MouseButtonEvent MouseButtonEventData { mouseButtonEventButton = ButtonRight, mouseButtonEventMotion = motion }
            -> void $ setMouseLocationMode $ if motion == Pressed
              then RelativeLocation
              else AbsoluteLocation
          -- Mouse movement should only be precessed while the right mouse
          -- button is being held down
          MouseMotionEvent MouseMotionEventData { mouseMotionEventRelMotion = V2 (adjustSensitivity -> dx) (adjustSensitivity -> dy) }
            | allowMouseMovement
            -> modify $ \(t, r) -> (t, r + V3 dy dx 0.0)
          _ -> return ()
        )

      when (keyDown ScancodeW) $ modify $ \(t, r) -> (t + V3 0.0 0.0 (-1.0), r)
      when (keyDown ScancodeS) $ modify $ \(t, r) -> (t + V3 0.0 0.0 1.0, r)
      when (keyDown ScancodeA) $ modify $ \(t, r) -> (t + V3 (-1.0) 0.0 0.0, r)
      when (keyDown ScancodeD) $ modify $ \(t, r) -> (t + V3 1.0 0.0 0.0, r)
      when (keyDown ScancodeLCtrl) $ modify $ \(t, r) -> (t + V3 0.0 (-1.0) 0.0, r)
      when (keyDown ScancodeSpace) $ modify $ \(t, r) -> (t + V3 0.0 1.0 0.0, r)

    -- We should update the camera only when we receive user input. If the
    -- camera does get moved, we should reset the current result.
    when (deltas /= initialDeltas) $ do
      emptyOutput <- initialOutput
      result      <- takeMVar mResult

      -- The distance the camera moves depends on the time elapsed since the
      -- last frame and whether the shift key is being held down. We'll
      -- normalize the distance here so that strafing while moving forward moves
      -- you the same distance as moving in only one direction would.
      let (\v -> normalize v ^* movementDistance -> translation, rotation) =
            deltas
          updatedCamera = result ^. camera & rotation' +~ rotation
                                           & rotation' %~ clampRoll
                                           & translate translation
          !result' = result & value .~ (result ^. compute) updatedCamera (0, emptyOutput)
                            & camera .~ updatedCamera

      putMVar mResult $! result'

    unless shouldQuit $ go currentFrame

-- | Render the texture created by Accelerate using OpenGL.
graphicsLoop :: Window -> MVar Result -> IO ()
graphicsLoop window mResult = do
  -- 'Font.decode' should be able to load the font from am bytestring, but the
  -- bindings seem to be broken
  font <- loadFont "open-sans.ttf" Files.font 28
  Font.setHinting font Font.Light

  void $ glCreateContext window
  (program, vao)  <- initResources

  -- Right now the window is not resizable so we can just create the surface
  -- once and then forget about it
  V2 width height <- SDL.get $ windowSize window
  textSurface     <- createRGBSurface (V2 width height) RGBA8888

  forever $ do
    let textureSize = GL.TextureSize2D screenWidth screenHeight
    GL.viewport
      $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    result <- readMVar mResult

    let (iterations, texture)        = result ^. value

        (((), ((((), r), g), b)), _) = A.toVectors texture
        pixelBuffer                  = V.zipWith3 V3 r g b

    GL.clearColor $= GL.Color4 0.5 0.5 0.5 1.0
    GL.clear [GL.ColorBuffer]

    -- We'll render the number of iterations (and any other text) to an SDL
    -- surface, which we can then transfer to a GPU buffer.
    iterationSurface <- Font.blended font
                                     (V4 255 255 255 255)
                                     (T.pack . show $ iterations)

    unlockSurface textSurface
    surfaceFillRect textSurface Nothing (V4 0 0 0 0)
    void $! surfaceBlit iterationSurface Nothing textSurface (Just (P $ V2 10 0))
    freeSurface iterationSurface
    lockSurface textSurface

    GL.activeTexture $= GL.TextureUnit textTexUnit
    textPixels <- surfacePixels textSurface
    GL.texImage2D GL.Texture2D
                  GL.NoProxy
                  0
                  GL.RGBA'
                  textureSize
                  0
                  (GL.PixelData GL.RGBA GL.UnsignedByte textPixels)

    -- We have to transform our @[V3 Float]@ into a format the OpenGL pixel
    -- transfer knows how to deal with. We could use a combination of
    -- 'concatMap' and 'GLU.withPixels' here, but that takes almost 200
    -- miliseconds combined.
    GL.activeTexture $= GL.TextureUnit resultTexUnit
    unsafeWith pixelBuffer $ \p -> GL.texImage2D
      GL.Texture2D
      GL.NoProxy
      0
      -- OpenGL will neatly normalize our texture to [0, 1] floating point
      -- values if we use the 'GL.RGB'' internal representation instead. Not
      -- like we've done this or anything.
      GL.RGB32F
      textureSize
      0
      (GL.PixelData GL.RGB GL.Float p)

    -- This GLint version is **very** important. The Haskell bindings will
    -- hapilly accept a uint here, but OpenGL expects the texture units to be
    -- signed integers. Passing an insigned integer here results in the wrong
    -- texture being loaded. Normally this is not an issue, but the Haskell
    -- bindings use 'Word32' values instead of enums for 'GL.activeTexture'.
    GLU.setUniform program "u_texture" (fromIntegral resultTexUnit :: GL.GLint)
    GLU.setUniform program "u_text" (fromIntegral textTexUnit :: GL.GLint)
    -- TODO: There has to be a better way to extract this Scalar Int
    GLU.setUniform program "u_iterations" (fromIntegral iterations :: GL.GLint)

    GL.bindVertexArrayObject $= Just vao
    GL.drawArrays GL.Triangles 0 6

    glSwapWindow window

-- | Iniitalize the OpenGL shaders and all static buffers.
initResources :: IO (GLU.ShaderProgram, GL.VertexArrayObject)
initResources = do
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  program' <- GLU.simpleShaderProgramBS Files.vertexShader Files.fragmentShader

  let vertexAttrib = GLU.getAttrib program' "v_pos"
  vao' <- GLU.makeVAO $ do
    vertexBuffer <- screenQuad
    GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
    GL.vertexAttribPointer vertexAttrib
      $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 GLU.offset0)
    GL.vertexAttribArray vertexAttrib $= GL.Enabled

  GL.currentProgram $= Just (GLU.program program')

  -- We have to make sure the maximum number of mipmaps is set to zero,
  -- otherwise the texture will be incomplete and not render at all
  [resultTex, textTex] <- GL.genObjectNames @GL.TextureObject 2
  GL.activeTexture $= GL.TextureUnit resultTexUnit
  GL.textureBinding GL.Texture2D $= Just resultTex
  GL.textureLevelRange GL.Texture2D $= (0, 0)

  GL.activeTexture $= GL.TextureUnit textTexUnit
  GL.textureBinding GL.Texture2D $= Just textTex
  GL.textureLevelRange GL.Texture2D $= (0, 0)

  return (program', vao')

-- | Create a screen quad that the fragment shader can be drawn on.
screenQuad :: IO GL.BufferObject
screenQuad = GLU.makeBuffer
  GL.ArrayBuffer
  ([ V2 (-1.0) (-1.0)
   , V2 1.0    (-1.0)
   , V2 (-1.0) 1.0
   , V2 1.0    (-1.0)
   , V2 (-1.0) 1.0
   , V2 1.0    1.0
   ] :: [V2 Float]
  )

-- | Convert mouse movement from actual screen pixels into a floating point
-- representation that can be used for camera rotation.
adjustSensitivity :: Int32 -> Float
adjustSensitivity n = fromIntegral n * (-0.001)

-- | Load a font from a bytestring. This is a workaround for 'SDL.Font.decode'
-- not loading the font properly. Instead of reading directly from memory we'll
-- simply write the font to disk and let sdl2_ttf read from there.
loadFont :: String -> ByteString -> Font.PointSize -> IO Font.Font
loadFont name bs size = withSystemTempFile name $ \path handle -> do
  hSetEncoding handle latin1
  BS.hPut handle bs

  Font.load path size
