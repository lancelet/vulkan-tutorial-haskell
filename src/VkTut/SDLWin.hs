module VkTut.SDLWin (sdl, sdlWindow) where

import Control.Exception.Safe (bracket, bracket_)
import Control.Monad.Managed (Managed, managed, managed_)
import Data.Text (Text)
import Foreign.C.Types (CInt)
import qualified SDL
import qualified SDL.Video.Vulkan as SDL

-- | Managed instance of SDL.
--
-- This provides a managed instance of SDL and the Vulkan library.
sdl :: Managed ()
sdl = managed_ (bracket_ acquireSDL releaseSDL)

-- | Acquire an SDL instance and load the Vulkan library.
acquireSDL :: IO ()
acquireSDL = do
  SDL.initializeAll
  SDL.vkLoadLibrary Nothing

-- | Unload the Vulkan library and quit SDL.
releaseSDL :: IO ()
releaseSDL = do
  SDL.vkUnloadLibrary
  SDL.quit

-- | Managed instance of an SDL window.
sdlWindow ::
  -- | Title of the SDL window.
  Text ->
  -- | Width of the SDL window.
  Int ->
  -- | Height of the SDL window.
  Int ->
  -- | Managed instance of the SDL window.
  Managed SDL.Window
sdlWindow title width height =
  managed
    ( bracket
        (acquireSDLWindow title width height)
        SDL.destroyWindow
    )

-- | Create an SDL window.
acquireSDLWindow ::
  -- | Title of the SDL window.
  Text ->
  -- | Width of the SDL window.
  Int ->
  -- | Height of the SDL window.
  Int ->
  -- | IO action to create the window.
  IO SDL.Window
acquireSDLWindow title width height =
  let initialSize :: SDL.V2 CInt
      initialSize = SDL.V2 (fromIntegral width) (fromIntegral height)
      --
      config :: SDL.WindowConfig
      config =
        SDL.defaultWindow
          { SDL.windowInitialSize = initialSize,
            SDL.windowGraphicsContext = SDL.VulkanContext
          }
   in SDL.createWindow title config
