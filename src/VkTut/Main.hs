{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VkTut.Main
  ( main,
  )
where

import Control.Exception.Safe (bracket, bracket_)
import Control.Monad.Extra (whileM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed (Managed, managed, managed_, runManaged)
import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as V
import Foreign (Ptr, freeHaskellFunPtr)
import Foreign.C (CString, peekCString)
import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import qualified Vulkan as Vk
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

main :: IO ()
main = do
  putStrLn "Hello Word"
  runManaged $ do
    withSDL
    window <- withWindow "Vulkan" windowWidth windowHeight
    vkInstance <- withInstance window "Hello Triangle"
    withDebugUtils vkInstance
    liftIO $ mainLoop (pure ())

mainLoop :: IO () -> IO ()
mainLoop draw = whileM $ do
  quit <- maybe False isSDLQuitEvent <$> SDL.pollEvent
  if quit
    then pure False
    else draw >> pure True

---- Instance -----------------------------------------------------------------

withInstance ::
  SDL.Window ->
  Text ->
  Managed (Vk.Instance)
withInstance window appName = do
  -- list the available extensions
  availExtNames <-
    fmap Vk.extensionName . snd
      <$> Vk.enumerateInstanceExtensionProperties Nothing
  liftIO $ Text.putStrLn "Available extensions:"
  liftIO $ mapM_ (\n -> Text.putStrLn $ "  " <> Text.decodeUtf8 n) availExtNames
  -- list the available layers
  availLyrNames <-
    fmap Vk.layerName . snd
      <$> Vk.enumerateInstanceLayerProperties
  liftIO $ Text.putStrLn "Available layers:"
  liftIO $ mapM_ (\n -> Text.putStrLn $ "  " <> Text.decodeUtf8 n) availLyrNames
  -- list the required SDL extensions
  windowExtensions :: [CString] <- SDL.vkGetInstanceExtensions window
  liftIO $ Text.putStrLn "Required SDL window extensions:"
  liftIO $
    mapM_
      ( \cstr -> do
          txt <- Text.pack <$> peekCString cstr
          Text.putStrLn $ "  " <> txt
      )
      windowExtensions
  windowExtensionsBS :: [ByteString] <-
    liftIO $
      traverse BS.packCString windowExtensions
  --
  let requiredLayers = V.fromList ["VK_LAYER_KHRONOS_validation"]
      requiredExtensions =
        V.fromList $
          Vk.EXT_DEBUG_UTILS_EXTENSION_NAME : windowExtensionsBS
  --
  let createInfo :: Vk.InstanceCreateInfo '[Vk.DebugUtilsMessengerCreateInfoEXT]
      createInfo =
        Vk.zero
          { Vk.applicationInfo =
              Just
                Vk.zero
                  { Vk.applicationName = Just (Text.encodeUtf8 appName),
                    Vk.apiVersion = Vk.API_VERSION_1_1
                  },
            Vk.enabledLayerNames = requiredLayers,
            Vk.enabledExtensionNames = requiredExtensions
          }
          ::& debugUtilsMessengerCreateInfo
          :& ()
      --
      acquire :: IO (Vk.Instance)
      acquire = Vk.createInstance createInfo Nothing
      --
      release :: Vk.Instance -> IO ()
      release vkInstance = Vk.destroyInstance vkInstance Nothing
  managed (bracket acquire release)

---- Debugging Messages -------------------------------------------------------

foreign import ccall unsafe "debug-callback.c &debugCallback"
  debugCallbackPtr :: Vk.PFN_vkDebugUtilsMessengerCallbackEXT

debugUtilsMessengerCreateInfo :: Vk.DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfo =
  Vk.zero
    { Vk.messageSeverity =
        Vk.DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
      Vk.messageType =
        Vk.DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
          .|. Vk.DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
      Vk.pfnUserCallback = debugCallbackPtr
    }

withDebugUtils ::
  Vk.Instance ->
  Managed ()
withDebugUtils vkInstance = do
  let
    createInfo :: Vk.DebugUtilsMessengerCreateInfoEXT
    createInfo = debugUtilsMessengerCreateInfo
    --
    acquire :: IO Vk.DebugUtilsMessengerEXT
    acquire = Vk.createDebugUtilsMessengerEXT vkInstance createInfo Nothing
    --
    release :: Vk.DebugUtilsMessengerEXT -> IO ()
    release u = Vk.destroyDebugUtilsMessengerEXT vkInstance u Nothing
  managed (bracket acquire release) >> pure ()

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

withWindow :: Text -> Int -> Int -> Managed SDL.Window
withWindow title width height =
  let acquire :: IO SDL.Window
      acquire =
        SDL.createWindow
          title
          ( SDL.defaultWindow
              { SDL.windowInitialSize =
                  SDL.V2
                    (fromIntegral width)
                    (fromIntegral height),
                SDL.windowGraphicsContext = SDL.VulkanContext
              }
          )
      --
      release :: SDL.Window -> IO ()
      release = SDL.destroyWindow
   in managed (bracket acquire release)

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
