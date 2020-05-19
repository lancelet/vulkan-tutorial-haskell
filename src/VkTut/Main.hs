{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VkTut.Main
  ( main,
  )
where

import Control.Monad (unless)
import Control.Monad.Extra (whileM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Managed (runManaged)
import qualified SDL
import qualified VkTut.SDLWin as SDLWin

main :: IO ()
main = do
  putStrLn "Launching Vulkan Application"
  runManaged $ do
    SDLWin.sdl
    _window <- SDLWin.sdlWindow "Vulkan" 800 600
    mainLoop $ pure ()

-- | Application main loop.
mainLoop ::
  (MonadIO m) =>
  -- | Drawing action.
  m () ->
  -- | Action for the main loop.
  m ()
mainLoop draw = whileM $ do
  events :: [SDL.Event] <- SDL.pollEvents
  let quit :: Bool
      quit = any isSDLQuitEvent events
  unless quit draw
  pure (not quit)

-- | Check if an SDL event is a quit event.
--
-- A quit event is either:
--   - pressing q on the keyboard
--   - an explicit SDL.QuitEvent
isSDLQuitEvent :: SDL.Event -> Bool
isSDLQuitEvent = \case
  SDL.Event _ts SDL.QuitEvent -> True
  event | isKeyRelease event, isKey event SDL.KeycodeQ -> True
  _ -> False

-- | Check if an SDL Event is a key release.
isKeyRelease :: SDL.Event -> Bool
isKeyRelease = \case
  SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released _ _)) ->
    True
  _ -> False

-- | Check if an SDL event matches a given key code.
isKey :: SDL.Event -> SDL.Keycode -> Bool
isKey event keyCode = case event of
  SDL.Event
    _
    ( SDL.KeyboardEvent
        (SDL.KeyboardEventData _ _ _ (SDL.Keysym _ code _))
      )
      | code == keyCode ->
        True
  _ -> False
