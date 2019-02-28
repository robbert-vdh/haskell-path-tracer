{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Graphics.Rendering.OpenGL as GL
import SDL

import Lib

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
  loop window

loop :: Window -> IO ()
loop window = do
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
  glSwapWindow window
  unless shouldQuit $ loop window

-- | Iniitalize the OpenGL shaders.
compileShaders :: IO GL.Program
compileShaders = do
  vs <- GL.createShader GL.VertexShader
  GL.shaderSourceBS vs $= BS.pack $(readShaderQ "app/shaders/vs.glsl")
  GL.compileShader vs
  fail
    "TODO: Copy+paste the rest of https://github.com/haskell-game/sdl2/blob/master/examples/OpenGLExample.hs"
