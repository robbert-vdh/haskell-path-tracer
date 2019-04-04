{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Control.Concurrent
import Control.Lens
import Control.Monad (forM_, forever, unless, void, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO.Data.Vector.Storable as A
import Data.Int (Int32)
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (unsafeWith)
import Data.Word (Word32)
import Foreign.C.Types (CInt(..))
import qualified Graphics.GLUtil as GLU
import qualified Graphics.Rendering.OpenGL as GL
import Linear (V2)
import SDL hiding (Point, get, translation)
import qualified SDL

import Lib
import Scene
import Scene.Objects (Camera, Color, Direction, Point, rotation')
import Scene.World (initialCamera)
import TH
import Util

-- | A compiled rendering function for a specific camera position and
-- orientation. We store this function next to the other rendering data for
-- simplicity's sake. See 'compileFor' for more information.
type CompiledFunction = A.Matrix (Color, Word32) -> A.Matrix (Color, Word32)

-- | The rendering results. The resulting 'Color' matrix should be divided by
-- the number of iterations in order to obtain the final averaged image.
--
-- Because the computation function is also dependant in the camera, we simply
-- store it next to the other values.
data Result = Result
  { _resultCompute :: CompiledFunction
  , _resultTexture :: A.Matrix (Color, Word32)
  , _resultIterations :: Int
  , _resultCamera :: Camera
  }

makeFields ''Result

main :: IO ()
main = do
  initializeAll

  window <-
    createWindow (T.pack "Leipe Mocro Tracer") $
    defaultWindow
      -- We'll only grab the mouse input when the right button is pressed
      { windowInputGrabbed = False
      , windowInitialSize = V2 (CInt screenWidth) (CInt screenHeight)
      , windowOpenGL = Just defaultOpenGL {glProfile = Core Normal 3 3}
      }

  let compute' = compileFor $ scalar initialCamera
  seeds <- initialOutput
  mResult <-
    newMVar $!
    Result
      { _resultCompute = compute'
      , _resultTexture = compute' seeds
      , _resultIterations = 1
      , _resultCamera = initialCamera
      }

  computationThreadId <- forkOS $ computationLoop mResult
  graphicsThreadId <- forkOS $ graphicsLoop window mResult
  inputLoop mResult

  killThread graphicsThreadId
  killThread computationThreadId

-- | Create a function for rendering a single sample based on the current camera
-- position. This function is meant to be reused until the 'Camera' position
-- gets updated.
compileFor :: A.Scalar Camera -> CompiledFunction
compileFor = runN render screenPixels

-- | Perform the actual path tracing. This is done in a seperate thread that
-- shares and 'MVar' with the rendering thread to prevent one of the processes
-- from blocking another.
--
-- We use a 'State' monad here to keep track of when we should reseed our RNGs.
computationLoop :: MVar Result -> IO ()
computationLoop mResult =
  flip evalStateT reseedInterval $ forever $ do
    result <- liftIO $ do
      result <- takeMVar mResult

      -- We can gain some performance by calculating multiple samples at once,
      -- but it'll reduce the responsiveness of our application. By doing this
      -- only once we reach a certain threshold we can still make use of this
      -- optimization while keeping it responsive.
      let batchSize = max 30 $ (result ^. iterations) `div` 50
          !(!iterations', !texture') =
            if (result ^. iterations) > 100
              then (batchSize, doTimes batchSize (result ^. compute) $! (result ^. texture))
              else (1, (result ^. compute) $! (result ^. texture))
          !result' = result & texture .~ texture' & iterations +~ iterations'

      void $ putMVar mResult $! result'
      print $ result' ^. iterations

      return result

    -- The RNGs should be reseeded every 2000 iterations to prevent convergence
    -- TODO: Refactor out this double read/swap and any data races
    reseedAt <- get
    if (result ^. iterations) > reseedAt
      then do
        liftIO $ do
          reseeded <- reseed $ result ^. texture
          void $ swapMVar mResult $ result & texture .~ reseeded
        modify (+ reseedInterval)
      else when ((result ^. iterations) < reseedInterval) $ put reseedInterval
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
inputLoop mResult = go
  where
    initialDeltas :: (Point, Direction)
    initialDeltas = (V3 0.0 0.0 0.0, V3 0.0 0.0 0.0)

    -- | Cap the camera rotation's roll (the vertical rotation). Otherwise
    -- continuously rotating the camera upwards results in some wierd behaviour.
    clampRoll :: Direction -> Direction
    clampRoll (V3 roll pitch yaw) =
      V3 (min maxRoll $ max minRoll roll) pitch yaw
      where
        minRoll = negate pi / 2 + 0.001
        maxRoll = pi / 2 - 0.001

    go = do
      events <- map eventPayload <$> pollEvents

      keyDown <- getKeyboardState
      allowMouseMovement <- (==) RelativeLocation <$> getMouseLocationMode
      let shouldQuit =
            QuitEvent `elem` events || keyDown ScancodeQ || keyDown ScancodeEscape

      -- Camera movement
      -- We use the 'State' monad to accumulate camera movements before processing
      -- them to prevent unneeded camera updates.
      deltas@(translation, rotation) <- flip execStateT initialDeltas $ do
        forM_ events
          (\case
            -- The right mouse button enables mouse look. SDL's relative mouse
            -- location mode also implicitely hides the cursor and enables
            -- window grab.
            MouseButtonEvent MouseButtonEventData { mouseButtonEventButton = ButtonRight
                                                  , mouseButtonEventMotion = motion
                                                  , ..
                                                  } ->
              void $ setMouseLocationMode $ if motion == Pressed
                then RelativeLocation
                else AbsoluteLocation
            -- Mouse movement should only be precessed while the right mouse
            -- button is being held down
            MouseMotionEvent MouseMotionEventData { mouseMotionEventRelMotion =
                                                      V2 (adjustSensitivity -> dx)
                                                         (adjustSensitivity -> dy)
                                                  , ..
                                                  } | allowMouseMovement ->
              modify $ \(t, r) -> (t, r + V3 dy dx 0.0)
            _ -> return ())

        -- TODO: Make the distance relative to the elapsed time
        -- TODO: Either print or overlay the keybindings when the application
        --       start
        let movementSpeed = if keyDown ScancodeLShift then 0.2 else 0.05
        when (keyDown ScancodeW) $
          modify $ \(t, r) -> (t + V3 0.0 0.0 (negate movementSpeed), r)
        when (keyDown ScancodeS) $
          modify $ \(t, r) -> (t + V3 0.0 0.0 movementSpeed, r)
        when (keyDown ScancodeA) $
          modify $ \(t, r) -> (t + V3 (negate movementSpeed) 0.0 0.0, r)
        when (keyDown ScancodeD) $
          modify $ \(t, r) -> (t + V3 movementSpeed 0.0 0.0, r)
        when (keyDown ScancodeLCtrl) $
          modify $ \(t, r) -> (t + V3 0.0 (negate movementSpeed) 0.0, r)
        when (keyDown ScancodeSpace) $
          modify $ \(t, r) -> (t + V3 0.0 movementSpeed 0.0, r)

      -- We should update the camera only when we receive user input. If the
      -- camera does get moved, we should reset the current result.
      when (deltas /= initialDeltas) $ do
        emptyOutput <- initialOutput
        result <- takeMVar mResult

        let updatedCamera = result ^. camera & rotation' +~ rotation
                                            & rotation' %~ clampRoll
                                            & translate translation
            compute' = compileFor $ scalar updatedCamera
            result' = result & iterations .~ 1 & texture .~ emptyOutput
                            & camera .~ updatedCamera & compute .~ compute'

        putMVar mResult result'

      unless shouldQuit go

-- | Render the texture created by Accelerate using OpenGL.
graphicsLoop :: Window -> MVar Result -> IO ()
graphicsLoop window mResult = do
  void $ glCreateContext window
  (program, vao) <- initResources

  forever $ do
    V2 width height <- SDL.get $ windowSize window
    GL.viewport $=
      (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

    -- XXX: This is a LOT faster than using 'A.toList' but I feel dirty even
    --      looking at it. Is there really not a better way?
    result <- readMVar mResult
    let (((), ((((), r), g), b)), _) = A.toVectors $ result ^. texture
        pixelBuffer = V.zipWith3 V3 r g b

    GL.clearColor $= GL.Color4 0.5 0.5 0.5 1.0
    GL.clear [GL.ColorBuffer]

    -- We have to transform our @[V3 Float]@ into a format the OpenGL pixel
    -- transfer knows how to deal with. We could use a combination of
    -- 'concatMap' and 'GLU.withPixels' here, but that takes almost 200
    -- miliseconds combined.
    unsafeWith pixelBuffer $ \p ->
      GL.texImage2D
        GL.Texture2D
        GL.NoProxy
        0
        -- OpenGL will neatly normalize our texture to [0, 1] floating point
        -- values if we use the 'GL.RGB'' internal representation instead. Not
        -- like we've done this or anything.
        GL.RGB32F
        (GL.TextureSize2D screenWidth screenHeight)
        0
        (GL.PixelData GL.RGB GL.Float p)

    GLU.setUniform program "u_iterations"
      (fromIntegral (result ^. iterations) :: GL.GLint)
    GLU.setUniform program "u_texture" (0 :: GL.GLint)

    GL.bindVertexArrayObject $= Just vao
    GL.drawArrays GL.Triangles 0 6

    glSwapWindow window

-- | Iniitalize the OpenGL shaders and all static buffers.
initResources :: IO (GLU.ShaderProgram, GL.VertexArrayObject)
initResources = do
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  program' <- GLU.simpleShaderProgramBS
    $(readShaderQ "app/shaders/vs.glsl")
    $(readShaderQ "app/shaders/fs.glsl")

  let vertexAttrib = GLU.getAttrib program' "v_pos"
  vao' <- GLU.makeVAO $ do
    vertexBuffer <- screenQuad
    GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
    GL.vertexAttribPointer vertexAttrib $=
      (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 GLU.offset0)
    GL.vertexAttribArray vertexAttrib $= GL.Enabled

  GL.currentProgram $= (Just $ GLU.program program')

  -- We have to make sure the maximum number of mipmaps is set to zero,
  -- otherwise the texture will be incomplete and not render at all
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just (GL.TextureObject 0)
  GL.textureLevelRange GL.Texture2D $= (0, 0)

  return (program', vao')

-- | Create a screen quad that the fragment shader can be drawn on.
screenQuad :: IO GL.BufferObject
screenQuad =
  GLU.makeBuffer
    GL.ArrayBuffer
    ([ V2 (-1.0) (-1.0)
     , V2 1.0 (-1.0)
     , V2 (-1.0) 1.0
     , V2 1.0 (-1.0)
     , V2 (-1.0) 1.0
     , V2 1.0 1.0
     ] :: [V2 Float])

-- | Convert mouse movement from actual screen pixels into a floating point
-- representation that can be used for camera rotation.
adjustSensitivity :: Int32 -> Float
adjustSensitivity n = fromIntegral n * (-0.001)
