{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VkTut.SDLWin
  ( sdl,
    window,
    vulkanInstance,
  )
where

import Control.Exception.Safe (bracket, bracket_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (Managed, managed, managed_)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Word (Word32)
import Foreign.C (CInt, CString)
import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import qualified Vulkan as Vk
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import qualified Vulkan.Utils.Debug as VkDebug

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
window ::
  -- | Title of the SDL window.
  Text ->
  -- | Width of the SDL window.
  Int ->
  -- | Height of the SDL window.
  Int ->
  -- | Managed instance of the SDL window.
  Managed SDL.Window
window title width height =
  managed
    ( bracket
        (acquireWindow title width height)
        SDL.destroyWindow
    )

-- | Create an SDL window.
acquireWindow ::
  -- | Title of the SDL window.
  Text ->
  -- | Width of the SDL window.
  Int ->
  -- | Height of the SDL window.
  Int ->
  -- | IO action to create the window.
  IO SDL.Window
acquireWindow title width height =
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

-- | Managed Vulkan instance.
vulkanInstance ::
  -- | SDL window.
  SDL.Window ->
  -- | Name of the application.
  Text ->
  -- | Name of the engine.
  Text ->
  -- | Extra required layer names.
  [Text] ->
  -- | Extra required extension names.
  [Text] ->
  -- | Use debug layers or not.
  Bool ->
  -- | API version.
  Word32 ->
  -- | Managed Vulkan instance.
  Managed Vk.Instance
vulkanInstance
  win
  appName
  engineName
  extraLayers
  extraExtensions
  useDebugLayers
  apiVersion =
    managed
      ( bracket
          ( acquireVulkanInstance
              win
              appName
              engineName
              extraLayers
              extraExtensions
              useDebugLayers
              apiVersion
          )
          releaseVulkanInstance
      )

-- | Acquire a Vulkan Instance.
acquireVulkanInstance ::
  -- | SDL window.
  SDL.Window ->
  -- | Name of the application.
  Text ->
  -- | Name of the engine.
  Text ->
  -- | Extra required layer names.
  [Text] ->
  -- | Extra required extension names.
  [Text] ->
  -- | Use debug layers or not.
  Bool ->
  -- | API version.
  Word32 ->
  -- | Managed Vulkan instance.
  IO Vk.Instance
acquireVulkanInstance
  win
  appName
  engineName
  extraLayers
  extraExtensions
  useDebugLayers
  apiVersion = do
    -- find the extensions that SDL needs in order to operate
    sdlExtCS :: [CString] <- SDL.vkGetInstanceExtensions win
    sdlExt :: [ByteString] <- liftIO $ traverse BS.packCString sdlExtCS
    -- debug extensions
    let debugExt :: [ByteString]
        debugExt = [Vk.EXT_DEBUG_UTILS_EXTENSION_NAME | useDebugLayers]
    -- debug (validation) layers
    let debugLayers :: [ByteString]
        debugLayers = ["VK_LAYER_KHRONOS_validation" | useDebugLayers]
    -- vector of all required extensions
    let extensions :: Vector ByteString
        extensions =
          V.fromList $
            sdlExt
              <> (Text.encodeUtf8 <$> extraExtensions)
              <> debugExt
    -- vector of all required layers
    let layers :: Vector ByteString
        layers =
          V.fromList $
            (Text.encodeUtf8 <$> extraLayers)
              <> debugLayers
    -- application information
    let appInfo :: Vk.ApplicationInfo
        appInfo =
          Vk.zero
            { Vk.applicationName = Just (Text.encodeUtf8 appName),
              Vk.engineName = Just (Text.encodeUtf8 engineName),
              Vk.apiVersion = apiVersion
            }
    -- creation information struct (for no debug)
    let createInfoNoDebug :: Vk.InstanceCreateInfo '[]
        createInfoNoDebug =
          Vk.zero
            { Vk.applicationInfo = Just appInfo,
              Vk.enabledLayerNames = layers,
              Vk.enabledExtensionNames = extensions
            }
    -- creation information struct (when using debug)
    let createInfoDebug ::
          Vk.InstanceCreateInfo '[Vk.DebugUtilsMessengerCreateInfoEXT]
        createInfoDebug =
          createInfoNoDebug
            ::& debugUtilsMessengerCreateInfo
            :& ()
    -- choose correct creation info struct
    if useDebugLayers
      then Vk.createInstance createInfoDebug Nothing
      else Vk.createInstance createInfoNoDebug Nothing

-- | Release the Vulkan instance.
releaseVulkanInstance :: Vk.Instance -> IO ()
releaseVulkanInstance = flip Vk.destroyInstance Nothing

-- | Creation information for the debug messenger.
debugUtilsMessengerCreateInfo :: Vk.DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfo =
  let messageSeverity :: Vk.DebugUtilsMessageSeverityFlagBitsEXT
      messageSeverity =
        Vk.DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
          -- .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
      --
      messageType :: Vk.DebugUtilsMessageTypeFlagsEXT
      messageType =
        Vk.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
   in Vk.zero
        { Vk.messageSeverity = messageSeverity,
          Vk.messageType = messageType,
          Vk.pfnUserCallback = VkDebug.debugCallbackPtr
        }
