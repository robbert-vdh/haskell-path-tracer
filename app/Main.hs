{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad (unless)
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO.Data.Vector.Storable as A
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import Data.Word (Word32)
import Data.Vector.Storable (unsafeWith)
import Foreign.C.Types (CInt(..))
import qualified Graphics.GLUtil as GLU
import qualified Graphics.Rendering.OpenGL as GL
import Linear (V2)
import SDL

import Lib
import Scene
import Scene.Objects (Color)
import TH
import Util

-- | The resources that the rendering loop relies on. All of these are static
-- once created, although the inner values of the 'MVar's can change.
data Resources = Resources
  { _window :: Window
  , _program :: GLU.ShaderProgram
  , _vao :: GL.VertexArrayObject
  , _state :: State
  }

-- | The state that gets shared between the computation and rendering threads.
-- The general idea behind this setup is that we can use message passing to
-- safely send any updates (e.g. camera movement) to the computation thread
-- without having to manually block the 'MVar'.
data State = State
  { _mResult :: MVar Result
  , _events :: Chan InputEvent
  }

data InputEvent = InputEvent

-- | The rendering results. The resulting 'Color' matrix should be divided by
-- the number of iterations in order to obtain the final averaged image.
data Result = Result
  { _texture :: A.Matrix (Color, Word32)
  , _iterations :: Int
  }

makeLenses ''Resources
makeLenses ''State
makeLenses ''Result

main :: IO ()
main = do
  initializeAll

  window' <-
    createWindow (T.pack "Leipe Mocro Tracer") $
    defaultWindow
      { windowInputGrabbed = False
      , windowInitialSize = V2 (CInt screenWidth) (CInt screenHeight)
      , windowOpenGL = Just defaultOpenGL {glProfile = Core Normal 3 3}
      }
  _glContext <- glCreateContext window'
  (program', vao') <- initResources

  seeds <- initialOutput
  result <- newMVar $! Result { _texture = compute seeds, _iterations = 1 }
  eventQueue <- newChan
  let state' = State result eventQueue

  computationThreadId <- forkOS $ computationLoop state'
  graphicsLoop $ Resources window' program' vao' state'

  killThread computationThreadId

-- | Render a single sample based on the a matrix of @(<screen pixel>, <rng
-- seed>)@ pairs and the previously computed output.
--
-- TODO: The camera is hardcoded for now, but this obviously should not be the
--       case!
compute ::
     A.Array A.DIM2 (Color, Word32)
  -> A.Array A.DIM2 (Color, Word32)
compute = runN (render theCamera) screenPixels

-- | Perform the actual path tracing. This is done in a seperate thread that
-- shares and 'MVar' with the rendering thread to prevent one of the processes
-- from blocking another.
computationLoop :: State -> IO ()
computationLoop (State mResult' eventQueue) = readMVar mResult' >>= go
  where
    go result = do
      -- HACK: This evaluate is needed because we don't actually read fromt he
      --       MVar here
      texture' <- evaluate $! compute $ result ^. texture
      let result' = result & texture .~ texture' & iterations +~ 1
      _ <- swapMVar mResult' $! result'
      print $ result ^. iterations

      -- Reset image with space press
      keysState <- getKeyboardState
      let spaceDown = keysState ScancodeSpace

      if spaceDown
        then do
          emptyOutput <- initialOutput
          go $ result' & iterations .~ 1 & texture .~ emptyOutput
        else go result'

-- getWASD :: (Scancode -> Bool) -> m String
-- getWASD ks = do
--   wasd <- map ks [ScancodeW, ScancodeA, ScancodeS, ScancodeD]
--   return [l | (l, c) <- (zip "WASD" wasd), c]

-- | Perform all the necesary I/O to handle user input and to render the texture
-- created by Accelerate using OpenGL.
graphicsLoop :: Resources -> IO ()
graphicsLoop resources = do
  sdlEvents <- pollEvents

  isPressed <- getKeyboardState
  let shouldQuit =
        any (\(eventPayload -> p) -> p == QuitEvent) sdlEvents ||
        isPressed ScancodeQ || isPressed ScancodeEscape

  -- XXX: This is a LOT faster than using 'A.toList' but I feel dirty even
  --      looking at it. Is there really not a better way?
  result <- readMVar $ resources ^. state . mResult
  let (((), ((((), r), g), b)), _) = A.toVectors $ result ^. texture
      pixelBuffer = V.zipWith3 V3 r g b

  V2 width height <- get $ windowSize $ resources ^. window
  GL.viewport $=
    (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

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
