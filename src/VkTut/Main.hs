{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module VkTut.Main
  ( main,
  )
where

import Control.Exception.Safe (bracket, bracket_)
import Control.Monad.Extra (whileM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed (Managed, managed, managed_, runManaged)
import Data.Bits ((.&.), (.|.), Bits, zeroBits)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import Foreign (Ptr, castPtr, freeHaskellFunPtr)
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
    surface <- withSDLWindowSurface vkInstance window
    physicalDevice <-
      pickPhysicalDevice
        vkInstance
        surface
        ( Vk.SurfaceFormatKHR
            Vk.FORMAT_B8G8R8_UNORM
            Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR
        )
    liftIO $ mainLoop (pure ())

mainLoop :: IO () -> IO ()
mainLoop draw = whileM $ do
  quit <- maybe False isSDLQuitEvent <$> SDL.pollEvent
  if quit
    then pure False
    else draw >> pure True

---- Instance and Devices -----------------------------------------------------

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
                    Vk.apiVersion = Vk.API_VERSION_1_0
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

data DeviceScore
  = DeviceScore
      { devicePhysicalDevice :: Vk.PhysicalDevice,
        deviceScoreGraphicsQueue :: Word32,
        deviceScorePresentQueue :: Word32,
        deviceScoreFormat :: Vk.SurfaceFormatKHR,
        deviceScorePresentMode :: Vk.PresentModeKHR,
        deviceScoreSurfaceCapabilities :: Vk.SurfaceCapabilitiesKHR,
        deviceScoreMemory :: Word64
      }
  deriving (Show)

pickPhysicalDevice ::
  MonadIO m =>
  Vk.Instance ->
  Vk.SurfaceKHR ->
  Vk.SurfaceFormatKHR ->
  m DeviceScore
pickPhysicalDevice vkInstance surface desiredFormat = do
  -- list the names of physical devices
  (_, devices) <- Vk.enumeratePhysicalDevices vkInstance
  names <-
    traverse
      (fmap (Text.decodeUtf8 . Vk.deviceName) . Vk.getPhysicalDeviceProperties)
      devices
  liftIO $ Text.putStrLn "Available physical devices:"
  liftIO $ mapM_ (\n -> Text.putStrLn $ "  " <> n) names
  -- pick the physical device with the best score
  scores <- traverse (deviceOverallScore surface desiredFormat) devices
  let allowedDeviceScores :: V.Vector DeviceScore
      allowedDeviceScores = V.mapMaybe id scores
  let best = V.maximumBy (comparing deviceScoreMemory) allowedDeviceScores
  name <-
    fmap (Text.decodeUtf8 . Vk.deviceName)
      $ Vk.getPhysicalDeviceProperties
      $ devicePhysicalDevice best
  liftIO $ Text.putStrLn $ "Chose device: " <> name
  pure best

deviceOverallScore ::
  MonadIO m =>
  Vk.SurfaceKHR ->
  Vk.SurfaceFormatKHR ->
  Vk.PhysicalDevice ->
  m (Maybe DeviceScore)
deviceOverallScore surface desiredFormat device = do
  hasSwapChain <- deviceHasSwapChain device
  graphicsQueueVec <- getGraphicsQueueIndices device
  presentQueueVec <- getPresentQueueIndices device surface
  format <- getFormat device surface desiredFormat
  presentMode <- getPresentMode device surface
  surfCaps <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR device surface
  memScore <- deviceMemScore device
  --
  let deviceOk :: Bool
      deviceOk =
        hasSwapChain
          && (not . V.null) graphicsQueueVec
          && (not . V.null) presentQueueVec
          && isJust presentMode
  --
  if deviceOk
    then
      pure $ Just $
        DeviceScore
          device
          (V.head graphicsQueueVec)
          (V.head presentQueueVec)
          format
          (fromJust presentMode)
          surfCaps
          memScore
    else pure Nothing

fromJust :: Maybe a -> a
fromJust maybeX = case maybeX of
  Just x -> x
  Nothing -> error "fromJust called on Nothing"

deviceMemScore :: MonadIO m => Vk.PhysicalDevice -> m Word64
deviceMemScore device = do
  heaps <- Vk.memoryHeaps <$> Vk.getPhysicalDeviceMemoryProperties device
  let totalSize = sum $ (Vk.size :: Vk.MemoryHeap -> Vk.DeviceSize) <$> heaps
  pure totalSize

deviceHasSwapChain :: MonadIO m => Vk.PhysicalDevice -> m Bool
deviceHasSwapChain device = do
  (_, extensions) <- Vk.enumerateDeviceExtensionProperties device Nothing
  pure $
    V.any
      ((Vk.KHR_SWAPCHAIN_EXTENSION_NAME ==) . Vk.extensionName)
      extensions

getGraphicsQueueIndices ::
  MonadIO m =>
  Vk.PhysicalDevice ->
  m (V.Vector Word32)
getGraphicsQueueIndices device = do
  queueFamilyProperties <- Vk.getPhysicalDeviceQueueFamilyProperties device
  let isGraphicsQueue :: Vk.QueueFamilyProperties -> Bool
      isGraphicsQueue q =
        (Vk.QUEUE_GRAPHICS_BIT .&&. Vk.queueFlags q) && (Vk.queueCount q > 0)
      --
      graphicsQueueIndices :: V.Vector Word32
      graphicsQueueIndices =
        fromIntegral . fst
          <$> V.filter
            (isGraphicsQueue . snd)
            (V.indexed queueFamilyProperties)
  pure graphicsQueueIndices

getPresentQueueIndices ::
  forall m.
  MonadIO m =>
  Vk.PhysicalDevice ->
  Vk.SurfaceKHR ->
  m (V.Vector Word32)
getPresentQueueIndices device surface = do
  queueFamilyProperties <- Vk.getPhysicalDeviceQueueFamilyProperties device
  let isPresentQueue :: Word32 -> m Bool
      isPresentQueue i = Vk.getPhysicalDeviceSurfaceSupportKHR device i surface
  V.filterM
    isPresentQueue
    (V.generate (V.length queueFamilyProperties) fromIntegral)

getFormat ::
  MonadIO m =>
  Vk.PhysicalDevice ->
  Vk.SurfaceKHR ->
  Vk.SurfaceFormatKHR ->
  m Vk.SurfaceFormatKHR
getFormat device surface desiredFormat = do
  (_, formats) <- Vk.getPhysicalDeviceSurfaceFormatsKHR device surface
  pure $ case formats of
    v | V.null v -> desiredFormat
    v -> case V.head v of
      Vk.SurfaceFormatKHR Vk.FORMAT_UNDEFINED _ -> desiredFormat
      _
        | V.any
            ( \f ->
                Vk.format (f :: Vk.SurfaceFormatKHR)
                  == Vk.format (desiredFormat :: Vk.SurfaceFormatKHR)
                  && Vk.colorSpace (f :: Vk.SurfaceFormatKHR)
                  == Vk.colorSpace (desiredFormat :: Vk.SurfaceFormatKHR)
            )
            formats ->
          desiredFormat
      _ -> V.head formats

getPresentMode ::
  MonadIO m =>
  Vk.PhysicalDevice ->
  Vk.SurfaceKHR ->
  m (Maybe Vk.PresentModeKHR)
getPresentMode device surface = do
  (_, presentModes) <- Vk.getPhysicalDeviceSurfacePresentModesKHR device surface
  let desiredPresentModes =
        V.fromList
          [ Vk.PRESENT_MODE_MAILBOX_KHR,
            Vk.PRESENT_MODE_FIFO_KHR,
            Vk.PRESENT_MODE_IMMEDIATE_KHR
          ]
  pure
    $ V.headM
    $ V.filter (`V.elem` presentModes) desiredPresentModes

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
  let createInfo :: Vk.DebugUtilsMessengerCreateInfoEXT
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

withSDLWindowSurface :: Vk.Instance -> SDL.Window -> Managed Vk.SurfaceKHR
withSDLWindowSurface vkInstance window =
  let acquire :: IO Vk.SurfaceKHR
      acquire =
        Vk.SurfaceKHR
          <$> SDL.vkCreateSurface window (castPtr (Vk.instanceHandle vkInstance))
      --
      release :: Vk.SurfaceKHR -> IO ()
      release s = Vk.destroySurfaceKHR vkInstance s Nothing
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

(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)
