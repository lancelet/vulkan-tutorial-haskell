{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module VkTut.Main
  ( main,
  )
where

import Control.Exception (Exception, throw)
import Control.Monad (unless)
import Data.Bits ((.|.))
import Foreign (FunPtr, Ptr)
import qualified Foreign
import Foreign.C.String (peekCString, withCString)
import qualified Graphics.Vulkan as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_debug_utils as Vk
import Linear (V4 (V4))
import qualified SDL
import SDL (($=))
import qualified SDL.Video.Vulkan as SDL

main :: IO ()
main = do
  putStrLn "Hello Word"
  --
  SDL.initializeAll
  SDL.vkLoadLibrary Nothing
  SDL.vkGetVkGetInstanceProcAddr
  --
  --
  window <- SDL.createWindow "Vulkan" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  appLoop renderer

appLoop :: SDL.Renderer -> IO ()
appLoop renderer = do
  events <- SDL.pollEvents
  let qPressed = any eventIsQPress events
  SDL.rendererDrawColor renderer $= V4 0 0 255 255
  SDL.clear renderer
  SDL.present renderer
  unless qPressed (appLoop renderer)

-- | Check if an SDL event was the 'Q' key being pressed.
eventIsQPress :: SDL.Event -> Bool
eventIsQPress event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
        && SDL.keysymKeycode
          (SDL.keyboardEventKeysym keyboardEvent)
          == SDL.KeycodeQ
    _ -> False

---- INSTANCE -----------------------------------------------------------------

withVulkanInstance :: (Vk.VkInstance -> IO Vk.VkResult) -> IO Vk.VkResult
withVulkanInstance action =
  withCString "Hello Triangle" $ \progNamePtr ->
    withCString "No Engine" $ \engineNamePtr -> do
      --
      debugCreate :: Vk.VkDebugUtilsMessengerCreateInfoEXT <-
        Vk.newVkData populateDebugMessengerCreateInfo
      --
      appInfo :: Vk.VkApplicationInfo <- Vk.newVkData $ \ptr -> do
        Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_APPLICATION_INFO
        Vk.writeField @"pNext" ptr (Foreign.castPtr $ Vk.unsafePtr debugCreate)
        Vk.writeField @"pApplicationName" ptr progNamePtr
        Vk.writeField @"applicationVersion" ptr (Vk._VK_MAKE_VERSION 1 0 0)
        Vk.writeField @"pEngineName" ptr engineNamePtr
        Vk.writeField @"engineVersion" ptr (Vk._VK_MAKE_VERSION 1 0 0)
        Vk.writeField @"apiVersion" ptr (Vk._VK_MAKE_VERSION 1 2 0)
      pure undefined

---- DEBUG CALLBACK -----------------------------------------------------------

data DebugMessengerException
  = DebugMessengerException Vk.VkResult
  deriving (Show)

instance Exception DebugMessengerException

setupDebugMessenger ::
  Vk.VkInstance ->
  Vk.HS_vkCreateDebugUtilsMessengerEXT ->
  IO Vk.VkDebugUtilsMessengerEXT
setupDebugMessenger vkInstance createDebugUtilsMessengerEXT =
  Foreign.alloca $ \(debugUtilsMessengerExt :: Ptr Vk.VkDebugUtilsMessengerEXT) -> do
    Vk.newVkData $ \createInfo -> do
      populateDebugMessengerCreateInfo createInfo
      vkResult <-
        createDebugUtilsMessengerEXT
          vkInstance
          createInfo
          Vk.VK_NULL_HANDLE
          debugUtilsMessengerExt
      if vkResult == Vk.VK_SUCCESS
        then pure ()
        else throw $ DebugMessengerException vkResult
      pure ()
    Foreign.peek debugUtilsMessengerExt

lookupCreateDebugUtilsMessengerEXT ::
  Vk.VkInstance ->
  SDL.VkGetInstanceProcAddrFunc ->
  IO Vk.HS_vkCreateDebugUtilsMessengerEXT
lookupCreateDebugUtilsMessengerEXT vkInstance gipar = do
  let vkInstance' :: SDL.VkInstance = Foreign.castPtr vkInstance
  fp :: FunPtr Vk.HS_vkCreateDebugUtilsMessengerEXT <-
    Foreign.castFunPtr
      <$> gipar vkInstance' Vk.VkCreateDebugUtilsMessengerEXT
  pure $ unwrapCreateDebugUtilsMessengerEXT fp

foreign import ccall "dynamic"
  unwrapCreateDebugUtilsMessengerEXT ::
    Vk.PFN_vkCreateDebugUtilsMessengerEXT ->
    Vk.HS_vkCreateDebugUtilsMessengerEXT

-- | Create VkDebugUtilsMessangerCreateInfoEXT that points to our debug
--   message function.
populateDebugMessengerCreateInfo ::
  Ptr Vk.VkDebugUtilsMessengerCreateInfoEXT ->
  IO ()
populateDebugMessengerCreateInfo p = do
  callbackPtr <- Vk.newVkDebugUtilsMessengerCallbackEXT debugCallback
  Vk.clearStorable p
  Vk.writeField
    @"sType"
    p
    Vk.VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
  Vk.writeField
    @"messageSeverity"
    p
    ( Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
        .|. Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
        .|. Vk.VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
    )
  Vk.writeField
    @"messageType"
    p
    ( Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
        .|. Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
        .|. Vk.VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
    )
  Vk.writeField
    @"pfnUserCallback"
    p
    callbackPtr

-- do at the end:
-- Foreign.freeHaskellFunPtr callbackPtr

-- | The debug callback function.
debugCallback ::
  Vk.VkDebugUtilsMessageSeverityFlagBitsEXT ->
  Vk.VkDebugUtilsMessageTypeFlagsEXT ->
  Ptr Vk.VkDebugUtilsMessengerCallbackDataEXT ->
  Ptr Vk.Void ->
  IO Vk.VkBool32
debugCallback severity msgType pCallbackData _pUserData = do
  message <- readStringFieldPtr @"pMessage" pCallbackData
  putStrLn $ "validation layer: " <> message <> "\n"
  pure Vk.VK_FALSE

-- | Read a string field from a structure that contains a (char*).
readStringFieldPtr ::
  forall fname a.
  ( Vk.CanReadFieldArray fname a,
    Vk.FieldType fname a ~ Ptr Vk.CChar
  ) =>
  -- | Structure from which to extract field 'fname'
  Ptr a ->
  -- | IO action which extracts the field.
  IO String
readStringFieldPtr px = Vk.readField @fname px >>= peekCString
