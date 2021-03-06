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
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import Foreign (castPtr)
import Foreign.C (CString, peekCString)
import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import qualified Vulkan as Vk
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import qualified Vulkan.CStruct.Extends as Vk

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

main :: IO ()
main = do
  putStrLn "Hello World"
  runManaged $ do
    withSDL
    vulkanWindow <- withVulkanWindow "Hello Triangle" 800 600
    liftIO $ mainLoop (pure ())

mainLoop :: IO () -> IO ()
mainLoop draw = whileM $ do
  quit <- maybe False isSDLQuitEvent <$> SDL.pollEvent
  if quit
    then pure False
    else draw >> pure True

---- Instance and Devices -----------------------------------------------------

data VulkanWindow
  = VulkanWindow
      { vwSdlWindow :: SDL.Window,
        vwDevice :: Vk.Device,
        vwSurface :: Vk.SurfaceKHR,
        vwSwapchain :: Vk.SwapchainKHR,
        vwExtent :: Vk.Extent2D,
        vwFormat :: Vk.Format,
        vwImageViews :: V.Vector Vk.ImageView,
        vwGraphicsQueue :: Vk.Queue,
        vwGraphicsQueueFamilyIndex :: Word32,
        vwPresentQueue :: Vk.Queue
      }

withVulkanWindow :: Text -> Int -> Int -> Managed VulkanWindow
withVulkanWindow title width height = do
  let desiredSurfFormat :: Vk.SurfaceFormatKHR
      desiredSurfFormat =
        Vk.SurfaceFormatKHR
          Vk.FORMAT_B8G8R8_UNORM
          Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR
  --
  sdlWindow <- withSDLWindow title width height
  vkInstance <- withInstance sdlWindow "Hello Triangle"
  withDebugUtils vkInstance
  surface <- withSDLWindowSurface vkInstance sdlWindow
  physicalDeviceInfo <- pickPhysicalDevice vkInstance surface desiredSurfFormat
  logicalDeviceInfo <- createLogicalDevice physicalDeviceInfo
  swapchainInfo <- withSwapchain surface physicalDeviceInfo logicalDeviceInfo
  imageViews <- withImageViews (ldiDevice logicalDeviceInfo) swapchainInfo
  --
  pure $
    VulkanWindow
      sdlWindow
      (ldiDevice logicalDeviceInfo)
      surface
      (sciSwapchain swapchainInfo)
      (sciExtent swapchainInfo)
      (sciFormat swapchainInfo)
      imageViews
      (ldiGraphicsQueue logicalDeviceInfo)
      (deviceInfoGraphicsQueueIndex physicalDeviceInfo)
      (ldiPresentQueue logicalDeviceInfo)

withInstance ::
  SDL.Window ->
  Text ->
  Managed Vk.Instance
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

data LogicalDeviceInfo
  = LogicalDeviceInfo
      { ldiDevice :: Vk.Device,
        ldiGraphicsQueue :: Vk.Queue,
        ldiPresentQueue :: Vk.Queue
      }

createLogicalDevice ::
  DeviceInfo ->
  Managed LogicalDeviceInfo
createLogicalDevice devInfo = do
  device <- withDevice devInfo
  graphicsQueue <-
    Vk.getDeviceQueue
      device
      (deviceInfoGraphicsQueueIndex devInfo)
      0
  presentQueue <-
    Vk.getDeviceQueue
      device
      (deviceInfoPresentQueueIndex devInfo)
      0
  pure $ LogicalDeviceInfo device graphicsQueue presentQueue

withDevice :: DeviceInfo -> Managed Vk.Device
withDevice devInfo = do
  let deviceCreateInfo :: Vk.DeviceCreateInfo '[]
      deviceCreateInfo =
        Vk.zero
          { Vk.queueCreateInfos =
              V.fromList
                [ Vk.SomeStruct $
                    Vk.zero
                      { Vk.queueFamilyIndex = i,
                        Vk.queuePriorities = V.singleton 1.0
                      }
                  | i <-
                      nub
                        [ deviceInfoGraphicsQueueIndex devInfo,
                          deviceInfoPresentQueueIndex devInfo
                        ]
                ],
            Vk.enabledExtensionNames =
              V.fromList [Vk.KHR_SWAPCHAIN_EXTENSION_NAME]
          }
  let acquire :: IO Vk.Device
      acquire =
        Vk.createDevice
          (deviceInfoPhysicalDevice devInfo)
          deviceCreateInfo
          Nothing
      --
      release :: Vk.Device -> IO ()
      release device = Vk.destroyDevice device Nothing
  managed (bracket acquire release)

data DeviceInfo
  = DeviceInfo
      { deviceInfoPhysicalDevice :: Vk.PhysicalDevice,
        deviceInfoGraphicsQueueIndex :: Word32,
        deviceInfoPresentQueueIndex :: Word32,
        deviceInfoFormat :: Vk.SurfaceFormatKHR,
        deviceInfoPresentMode :: Vk.PresentModeKHR,
        deviceInfoSurfaceCapabilities :: Vk.SurfaceCapabilitiesKHR,
        deviceInfoMemory :: Word64
      }
  deriving (Show)

pickPhysicalDevice ::
  MonadIO m =>
  Vk.Instance ->
  Vk.SurfaceKHR ->
  Vk.SurfaceFormatKHR ->
  m DeviceInfo
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
  let allowedDeviceScores :: V.Vector DeviceInfo
      allowedDeviceScores = V.mapMaybe id scores
  let best = V.maximumBy (comparing deviceInfoMemory) allowedDeviceScores
  name <-
    fmap (Text.decodeUtf8 . Vk.deviceName)
      $ Vk.getPhysicalDeviceProperties
      $ deviceInfoPhysicalDevice best
  liftIO $ Text.putStrLn $ "Chose device: " <> name
  pure best

deviceOverallScore ::
  MonadIO m =>
  Vk.SurfaceKHR ->
  Vk.SurfaceFormatKHR ->
  Vk.PhysicalDevice ->
  m (Maybe DeviceInfo)
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
        DeviceInfo
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

data SwapchainInfo
  = SwapchainInfo
      { sciSwapchain :: Vk.SwapchainKHR,
        sciFormat :: Vk.Format,
        sciExtent :: Vk.Extent2D
      }

withSwapchain ::
  Vk.SurfaceKHR ->
  DeviceInfo ->
  LogicalDeviceInfo ->
  Managed SwapchainInfo
withSwapchain surface di ldi = do
  --
  let (sharingMode, queueFamilyIndices) =
        if ldiGraphicsQueue ldi == ldiPresentQueue ldi
          then (Vk.SHARING_MODE_EXCLUSIVE, V.empty)
          else
            ( Vk.SHARING_MODE_CONCURRENT,
              V.fromList
                [ deviceInfoGraphicsQueueIndex di,
                  deviceInfoPresentQueueIndex di
                ]
            )
  --
  let maxImageCount =
        Vk.maxImageCount
          (deviceInfoSurfaceCapabilities di :: Vk.SurfaceCapabilitiesKHR)
  let minImageCount =
        min
          ( Vk.minImageCount
              (deviceInfoSurfaceCapabilities di :: Vk.SurfaceCapabilitiesKHR)
              + 1
          )
          maxImageCount
  --
  let imageExtent =
        case Vk.currentExtent
          (deviceInfoSurfaceCapabilities di :: Vk.SurfaceCapabilitiesKHR) of
          Vk.Extent2D w h
            | w == maxBound,
              h == maxBound ->
              Vk.Extent2D
                (fromIntegral windowWidth)
                (fromIntegral windowHeight)
          e -> e
  --
  let imageFormat =
        (Vk.format :: Vk.SurfaceFormatKHR -> Vk.Format)
          (deviceInfoFormat di)
  --
  let createInfo :: Vk.SwapchainCreateInfoKHR '[]
      createInfo =
        Vk.zero
          { Vk.surface = surface,
            Vk.minImageCount = minImageCount,
            Vk.imageFormat = imageFormat,
            Vk.imageColorSpace = Vk.colorSpace (deviceInfoFormat di),
            Vk.imageExtent = imageExtent,
            Vk.imageArrayLayers = 1,
            Vk.imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
            Vk.imageSharingMode = sharingMode,
            Vk.queueFamilyIndices = queueFamilyIndices,
            Vk.preTransform =
              Vk.currentTransform
                (deviceInfoSurfaceCapabilities di :: Vk.SurfaceCapabilitiesKHR),
            Vk.compositeAlpha = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
            Vk.presentMode = deviceInfoPresentMode di,
            Vk.clipped = True
          }
  --
  let acquire :: IO SwapchainInfo
      acquire =
        SwapchainInfo
          <$> Vk.createSwapchainKHR
            (ldiDevice ldi)
            createInfo
            Nothing
          <*> pure imageFormat
          <*> pure imageExtent
      --
      release :: SwapchainInfo -> IO ()
      release swapchainInfo =
        Vk.destroySwapchainKHR
          (ldiDevice ldi)
          (sciSwapchain swapchainInfo)
          Nothing
  --
  managed (bracket acquire release)

withImageViews :: Vk.Device -> SwapchainInfo -> Managed (V.Vector Vk.ImageView)
withImageViews device sci = do
  let format = sciFormat sci
  (_, images) <- liftIO $ Vk.getSwapchainImagesKHR device (sciSwapchain sci)
  let
    withIV :: Vk.Image -> Managed Vk.ImageView
    withIV = withImageView device format
  traverse withIV images

withImageView :: Vk.Device -> Vk.Format -> Vk.Image -> Managed Vk.ImageView
withImageView device format image = do
  let
    createInfo :: Vk.ImageViewCreateInfo '[]
    createInfo = Vk.zero
      { Vk.image = image,
        Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D,
        Vk.format = format,
        Vk.components = Vk.zero
                        { Vk.r = Vk.COMPONENT_SWIZZLE_IDENTITY,
                          Vk.g = Vk.COMPONENT_SWIZZLE_IDENTITY,
                          Vk.b = Vk.COMPONENT_SWIZZLE_IDENTITY,
                          Vk.a = Vk.COMPONENT_SWIZZLE_IDENTITY
                        },
        Vk.subresourceRange =
          Vk.zero
          { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT,
            Vk.baseMipLevel = 0,
            Vk.levelCount = 1,
            Vk.baseArrayLayer = 0,
            Vk.layerCount = 1
          }
      }
    --
    acquire :: IO Vk.ImageView
    acquire = Vk.createImageView device createInfo Nothing
    --
    release :: Vk.ImageView -> IO ()
    release v = Vk.destroyImageView device v Nothing
  --
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

withSDLWindow :: Text -> Int -> Int -> Managed SDL.Window
withSDLWindow title width height =
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
