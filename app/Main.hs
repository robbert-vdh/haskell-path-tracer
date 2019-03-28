{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent hiding (newChan, writeChan)
import Control.Concurrent.Chan.Unagi.NoBlocking
import Control.Exception
import Control.Lens
import Control.Monad (forM_, unless, when)
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
import SDL
import SDL.Vect (Point(..))

import Lib
import Scene
import Scene.Objects (Camera, Color, rotation')
import Scene.World (initialCamera)
import TH
import Util

-- | The resources that the rendering loop relies on. All of these are static
-- once created, although the inner values of the 'MVar's can change.
--
-- The general idea behind this setup is that we can use message passing to
-- safely send any updates (e.g. camera movement) to the computation thread
-- without having to manually block the 'MVar'.
data Resources = Resources
  { _resourcesWindow :: Window
  , _resourcesProgram :: GLU.ShaderProgram
  , _resourcesVao :: GL.VertexArrayObject
  , _resourcesMResult :: MVar Result
  , _resourcesEvents :: InChan InputEvent
  }

-- | Any camera movement should be encoded as one of these events and passed to
-- the 'Chan InputEvent' messaging queue contained in the shared 'State'.
data InputEvent
  = Move (V3 Float)
  | Rotate (V3 Float)
  deriving Show

-- | The rendering results. The resulting 'Color' matrix should be divided by
-- the number of iterations in order to obtain the final averaged image.
data Result = Result
  { _resultTexture :: A.Matrix (Color, Word32)
  , _resultIterations :: Int
  , _resultCamera :: Camera
  }

makeFields ''Resources
makeFields ''Result

main :: IO ()
main = do
  initializeAll

  window' <-
    createWindow (T.pack "Leipe Mocro Tracer") $
    defaultWindow
      -- We'll only grab the mouse input when the right button is pressed
      { windowInputGrabbed = False
      , windowInitialSize = V2 (CInt screenWidth) (CInt screenHeight)
      , windowOpenGL = Just defaultOpenGL {glProfile = Core Normal 3 3}
      }
  _glContext <- glCreateContext window'
  (program', vao') <- initResources

  let compute = compileFor initialCamera
  seeds <- initialOutput
  mResult' <-
    newMVar $!
    Result
      { _resultTexture = compute seeds
      , _resultIterations = 1
      , _resultCamera = initialCamera
      }
  (inQueue, outQueue) <- newChan
  [eventStream] <- streamChan 1 outQueue

  computationThreadId <- forkOS $ computationLoop compute eventStream mResult'
  graphicsLoop $ Resources window' program' vao' mResult' inQueue

  killThread computationThreadId

-- | Create a function for rendering a single sample based on the current camera
-- position. This function is meant to be reused until the 'Camera' position
-- gets updated.
--
-- TODO: Find out if there is a difference in performance between 'Exp a' and
--       'the' applied to 'Acc (Scalar a)'
compileFor :: Camera -> (A.Matrix (Color, Word32) -> A.Matrix (Color, Word32))
compileFor c = run1 $ render (A.constant c) (A.use screenPixels)

-- | Perform the actual path tracing. This is done in a seperate thread that
-- shares and 'MVar' with the rendering thread to prevent one of the processes
-- from blocking another.
computationLoop ::
     (A.Matrix (Color, Word32) -> A.Matrix (Color, Word32))
  -> Stream InputEvent
  -> MVar Result
  -> IO ()
computationLoop f eventQueue mResult' = readMVar mResult' >>= go f eventQueue
  where
    -- | Retrieve a list of unprocessed input events form the event queue.
    --
    -- TODO: Rewrite this using 'Control.Monad.Trans.State', and maybe
    --       'Data.Seq' instead of lists
    poll q =
      tryReadNext q >>= \case
        Next x q' -> do
          (xs, q'') <- poll q'
          return (xs ++ [x], q'')
        Pending -> return ([], q)

    go compute queue result = do
      -- HACK: This evaluate is needed because we don't actually read fromt he
      --       MVar here
      texture' <- evaluate $! compute (result ^. texture)
      let result' = result & texture .~ texture' & iterations +~ 1
      _ <- swapMVar mResult' $! result'
      print $ result ^. iterations

      (inputEvents, queue') <- poll queue
      if null inputEvents
        then go compute queue' result'
        else do
          let updatedCamera =
                foldl
                  (\c ->
                     \case
                       Move delta -> translate delta c
                       Rotate delta -> c & rotation' +~ delta)
                  (result ^. camera)
                  inputEvents
              compute' = compileFor updatedCamera

          -- The rendering should be reset after moving the camera
          emptyOutput <- initialOutput
          go compute' queue' $
            result' & iterations .~ 1 & texture .~ emptyOutput & camera .~ updatedCamera

-- | Perform all the necesary I/O to handle user input and to render the texture
-- created by Accelerate using OpenGL.
graphicsLoop :: Resources -> IO ()
graphicsLoop resources = do
  sdlEvents <- map eventPayload <$> pollEvents
  V2 width height <- get $ windowSize $ resources ^. window
  GL.viewport $=
    (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

  -- TODO: Maybe use 'mapEvents' to fold all events into a single structure
  --       instead of having to loop over it multiple times
  keyDown <- getKeyboardState
  mouseDown <- getMouseButtons
  let allowMouseMovement = mouseDown ButtonRight
      shouldQuit =
        QuitEvent `elem` sdlEvents ||
        keyDown ScancodeQ || keyDown ScancodeEscape

  -- Camera movement
  windowGrab (resources ^. window) $= allowMouseMovement
  cursorVisible $= not allowMouseMovement
  when allowMouseMovement $
    forM_ sdlEvents
      (\case
         MouseMotionEvent MouseMotionEventData { mouseMotionEventRelMotion =
                                                 V2 (adjustSensitivity -> dx)
                                                    (adjustSensitivity -> dy) , .. } -> do
           writeChan (resources ^. events) (Rotate $ V3 dx dy 0.0)
           warpMouse WarpCurrentFocus $ P $ V2 (width `quot` 2) (height `quot` 2)
         _ -> return ())

  -- TODO: Make the distance relative to the elapsed time
  when (keyDown ScancodeW) $
    writeChan (resources ^. events) (Move $ V3 0 0 (-0.1))
  when (keyDown ScancodeS) $
    writeChan (resources ^. events) (Move $ V3 0 0 0.1)
  when (keyDown ScancodeA) $
    writeChan (resources ^. events) (Move $ V3 (-0.1) 0 0)
  when (keyDown ScancodeD) $
    writeChan (resources ^. events) (Move $ V3 0.1 0 0)

  -- XXX: This is a LOT faster than using 'A.toList' but I feel dirty even
  --      looking at it. Is there really not a better way?
  result <- readMVar $ resources ^. mResult
  let (((), ((((), r), g), b)), _) = A.toVectors $ result ^. texture
      pixelBuffer = V.zipWith3 V3 r g b

  GL.clearColor $= GL.Color4 0.5 0.5 0.5 1.0
  GL.clear [GL.ColorBuffer]

  -- We have to transform our @[V3 Float]@ into a format the OpenGL pixel
  -- transfer knows how to deal with. We could use a combination of 'concatMap'
  -- and 'GLU.withPixels' here, but that takes almost 200 miliseconds combined.
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

  GLU.setUniform (resources ^. program) "u_iterations"
    (fromIntegral (result ^. iterations) :: GL.GLint)
  GLU.setUniform (resources ^. program) "u_texture"
    (0 :: GL.GLint)

  GL.bindVertexArrayObject $= (Just $ resources ^. vao)
  GL.drawArrays GL.Triangles 0 6

  glSwapWindow $ resources ^. window
  unless shouldQuit $ graphicsLoop resources

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
adjustSensitivity n = fromIntegral n * 0.001
