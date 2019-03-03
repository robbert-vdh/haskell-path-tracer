{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (unless)
import Debug.Trace
import Linear (V2)
import SDL
import qualified Data.Text as T
import qualified Graphics.GLUtil as GLU
import qualified Graphics.Rendering.OpenGL as GL

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
  -- TODO: Pack all the things we reuse (Accelerate arrays, windows, programs,
  --       buffers etc.) in a struct
  (program, vao) <- initResources
  loop window program vao

loop :: Window -> GLU.ShaderProgram -> GL.VertexArrayObject -> IO ()
loop window program vao = do
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

  -- Does SDL do this for us?
  V2 width height <- get $ windowSize window
  GL.viewport $=
    (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

  GL.clearColor $= GL.Color4 0.5 0.5 0.5 1.0
  GL.clear [GL.ColorBuffer]

  GL.bindVertexArrayObject $= Just vao
  GL.drawArrays GL.Triangles 0 6

  glSwapWindow window
  unless shouldQuit $ loop window program vao

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
