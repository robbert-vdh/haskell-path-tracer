{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent
import Control.Monad (unless)
import Data.Array.Accelerate ((:.)((:.)), Z(Z))
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Data.Functor as A
import qualified Data.Array.Accelerate.IO.Data.Vector.Storable as A
import qualified Data.Array.Accelerate.Linear as A ()
import qualified Data.Text as T
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (unsafeWith)
import qualified Graphics.GLUtil as GLU
import qualified Graphics.Rendering.OpenGL as GL
import Linear (V2)
import SDL

import Lib
import TH

main :: IO ()
main = do
  initializeAll

  window <-
    createWindow (T.pack "Leipe Mocro Tracer") $
    defaultWindow
      { windowInputGrabbed = False -- Change this to True when implementing user input
      , windowOpenGL = Just defaultOpenGL {glProfile = Core Normal 3 3}
      }
  _glContext <- glCreateContext window
  (program, vao) <- initResources

  result <- newMVar $! run initialOutput
  computationThreadId <- forkOS $ computationLoop result
  -- TODO: Pack all the things we reuse (Accelerate arrays, windows, programs,
  --       buffers etc.) in a struct
  graphicsLoop window program vao result
  killThread computationThreadId

-- | Perform the actual path tracing. This is done in a seperate thread that
-- shares and 'MVar' with the rendering thread to prevent one of the processes
-- from blocking another.
computationLoop :: MVar (A.Matrix Color) -> IO ()
computationLoop mvar = readMVar mvar >>= go
  where
    compute = runN doSomething
    go texture = do
      let texture' = compute texture
      _ <- swapMVar mvar $! texture'
      go texture'

-- | Perform all the necesary I/O to handle user input and to render the texture
-- created by Accelerate using OpenGL. The state is read by copying from an
-- 'MVar'.
graphicsLoop ::
     Window
  -> GLU.ShaderProgram
  -> GL.VertexArrayObject
  -> MVar (A.Matrix Color)
  -> IO ()
graphicsLoop window program vao result = do
  events <- pollEvents

  -- TODO: When we add camera movement we should simply keep track of a 'Set' of
  --       pressed keys.
  let shouldQuit =
        any
          (\Event {eventPayload} ->
             case eventPayload of
               KeyboardEvent KeyboardEventData { keyboardEventKeyMotion
                                               , keyboardEventKeysym = key
                                               } ->
                 keyboardEventKeyMotion == Pressed &&
                 keysymKeycode key `elem` [KeycodeEscape, KeycodeQ]
               QuitEvent -> True
               _ -> False)
          events

  -- XXX: This is a LOT faster than using 'A.toList' but I feel dirty even
  --      looking at it. Is there really not a better way?
  ((((), r), g), b) <- A.toVectors <$> readMVar result
  let texture = V.zipWith3 V3 r g b

  V2 width height <- get $ windowSize window
  GL.viewport $=
    (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

  GL.clearColor $= GL.Color4 0.5 0.5 0.5 1.0
  GL.clear [GL.ColorBuffer]

  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just (GL.TextureObject 0)
  GL.textureLevelRange GL.Texture2D $= (0, 0)

  -- We have to transform our @[V3 Float]@ into a format the OpenGL pixel
  -- transfer knows how to deal with. We could use a combination of 'concatMap'
  -- and 'GLU.withPixels' here, but that takes almost 200 miliseconds combined.
  unsafeWith texture $ \p ->
    GL.texImage2D
      GL.Texture2D
      GL.NoProxy
      0
      GL.RGB'
      (GL.TextureSize2D 800 600)
      0
      (GL.PixelData GL.RGB GL.Float p)

  GLU.setUniform program "u_iterations" (1 :: GL.GLint)
  GLU.setUniform program "u_texture" (0 :: GL.GLint)

  GL.bindVertexArrayObject $= Just vao
  GL.drawArrays GL.Triangles 0 6

  glSwapWindow window
  unless shouldQuit $ graphicsLoop window program vao result

-- | Iniitalize the OpenGL shaders and all static buffers.
--
-- TODO: Copy+paste the rest of https://github.com/haskell-game/sdl2/blob/master/examples/OpenGLExample.hs
initResources :: IO (GLU.ShaderProgram, GL.VertexArrayObject)
initResources = do
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  program <- GLU.simpleShaderProgramBS
    $(readShaderQ "app/shaders/vs.glsl")
    $(readShaderQ "app/shaders/fs.glsl")

  let vertexAttrib = GLU.getAttrib program "v_pos"
  vao <- GLU.makeVAO $ do
    vertexBuffer <- screenQuad
    GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
    GL.vertexAttribPointer vertexAttrib $=
      (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 GLU.offset0)
    GL.vertexAttribArray vertexAttrib $= GL.Enabled

  GL.currentProgram $= (Just $ GLU.program program)

  return (program, vao)

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

-- * TODO: Remove or move everything below this section

type Color = V3 Float

-- | Act as if we were actually doing something useful.
initialOutput :: A.Acc (A.Matrix Color)
initialOutput = A.map calcColor $ A.use orderedArray
  where
    orderedArray =
      A.fromFunction (Z :. 600 :. 800) $ \(Z :. y :. x) ->
        fromIntegral (x + y * 800) / 6.9
    calcColor :: A.Exp Float -> A.Exp Color
    calcColor e = A.lift $ V3 (A.sin e) (A.cos e) e

-- | Just a simple operation to perform on the texture that can be performed on
-- the computation thread.
doSomething :: A.Acc (A.Matrix Color) -> A.Acc (A.Matrix Color)
doSomething = A.map (A.fmap ((`A.mod'` 1) . (+ 0.0069)))
