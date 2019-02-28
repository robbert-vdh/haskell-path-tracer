{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad (unless)
import qualified Data.Text as T
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
