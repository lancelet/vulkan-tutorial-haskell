{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module VkTut.Main
  ( main,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Exception.Safe (bracket, bracket_)
import Control.Monad.Extra (whileM)
import Control.Monad.Managed (Managed, managed, managed_, runManaged)
import Data.Bits ((.|.))
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Foreign (Ptr, freeHaskellFunPtr)
import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import qualified Vulkan as Vk

main :: IO ()
main = do
  putStrLn "Hello Word"
  runManaged $ do
    withSDL
    vkInstance <- withInstance
    withDebugUtils vkInstance debugCallback

mainLoop :: IO () -> IO ()
mainLoop draw = whileM $ do
  quit <- maybe False isSDLQuitEvent <$> SDL.pollEvent
  if quit
    then pure False
    else draw >> pure True

---- Instance -----------------------------------------------------------------

withInstance ::
  Managed (Vk.Instance)
withInstance = do
  -- list the available extensions
  availableExtensionNames <- fmap Vk.extensionName . snd <$>
    Vk.enumerateInstanceExtensionProperties Nothing
  liftIO $ Text.putStrLn "Available extensions:"
  forM_ availableExtensionNames $ \namebs ->
    liftIO $ Text.putStrLn $ "  " <> Text.decodeUtf8 namebs
  -- list the available layers
  availableLayerNames <- fmap Vk.layerName . snd <$>
    Vk.enumerateInstanceLayerProperties
  liftIO $ Text.putStrLn "Available layers:"
  forM_ availableLayerNames $ \namebs ->
    liftIO $ Text.putStrLn $ "  " <> Text.decodeUtf8 namebs
  let
    createInfo :: Vk.InstanceCreateInfo '[Vk.DebugUtilsMessengerCreateInfoEXT] 
    createInfo = undefined
    --
    acquire :: IO (Vk.Instance)
    acquire = Vk.createInstance createInfo Nothing
    --
    release :: Vk.Instance -> IO ()
    release vkInstance = Vk.destroyInstance vkInstance Nothing
  managed (bracket acquire release)
  
---- Debugging Messages -------------------------------------------------------

-- | Setup debug utils.
withDebugUtils ::
  -- | Vulkan instance.
  Vk.Instance ->
  -- | Haskell-based debug callback.
  Vk.FN_vkDebugUtilsMessengerCallbackEXT ->
  -- | Managed action with debug utils setup.
  Managed ()
withDebugUtils vkInstance hsCallback = do
  callbackPtr <- withDebugCallback hsCallback
  let createInfo = debugUtilsMessengerCreateInfo callbackPtr
  let
    acquire :: IO Vk.DebugUtilsMessengerEXT
    acquire = Vk.createDebugUtilsMessengerEXT vkInstance createInfo Nothing
    --
    release :: Vk.DebugUtilsMessengerEXT -> IO ()
    release u = Vk.destroyDebugUtilsMessengerEXT vkInstance u Nothing
  managed (bracket acquire release) >> pure ()

foreign import ccall "wrapper"
  mkDebugCallback ::
    Vk.FN_vkDebugUtilsMessengerCallbackEXT ->
    IO Vk.PFN_vkDebugUtilsMessengerCallbackEXT

withDebugCallback ::
  Vk.FN_vkDebugUtilsMessengerCallbackEXT ->
  Managed Vk.PFN_vkDebugUtilsMessengerCallbackEXT
withDebugCallback hsCallback =
  let acquire :: IO Vk.PFN_vkDebugUtilsMessengerCallbackEXT
      acquire = mkDebugCallback hsCallback
      --
      release :: Vk.PFN_vkDebugUtilsMessengerCallbackEXT -> IO ()
      release = freeHaskellFunPtr
   in managed (bracket acquire release)

debugCallback ::
  Vk.DebugUtilsMessageSeverityFlagBitsEXT ->
  Vk.DebugUtilsMessageTypeFlagsEXT ->
  Ptr Vk.DebugUtilsMessengerCallbackDataEXT ->
  Ptr () ->
  IO Vk.Bool32
debugCallback _severity _messageType pCallbackData _pUserData = do
  callbackData <- Vk.peekCStruct pCallbackData
  let message = Text.decodeUtf8 $ Vk.message callbackData
  Text.putStrLn $ "validation layer: " <> message
  pure Vk.FALSE

debugUtilsMessengerCreateInfo ::
  Vk.PFN_vkDebugUtilsMessengerCallbackEXT ->
  Vk.DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfo pCallback =
  Vk.zero
    { Vk.messageSeverity =
        Vk.DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
      Vk.messageType =
        Vk.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
      Vk.pfnUserCallback = pCallback
    }

---- SDL ----------------------------------------------------------------------

withSDL :: Managed ()
withSDL =
  let -- bracket acquire and release of SDL itself
      sdlBracket :: forall r. IO r -> IO r
      sdlBracket = bracket_ SDL.initializeAll SDL.quit
      -- bracket acquire and release of SDL Vulkan library
      sdlVkBracket :: forall r. IO r -> IO r
      sdlVkBracket = bracket_ (SDL.vkLoadLibrary Nothing) SDL.vkUnloadLibrary
   in managed_ (sdlBracket . sdlVkBracket)

-- | Check if an SDL event is a quit event.
isSDLQuitEvent :: SDL.Event -> Bool
isSDLQuitEvent event =
  case event of
    SDL.Event _ SDL.QuitEvent -> True
    SDL.Event
      _
      ( SDL.KeyboardEvent
          ( SDL.KeyboardEventData
              _
              SDL.Released
              False
              (SDL.Keysym _ code _)
            )
        )
        | code == SDL.KeycodeQ || code == SDL.KeycodeEscape ->
          True
    _ -> False
