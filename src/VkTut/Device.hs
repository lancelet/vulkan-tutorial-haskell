{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VkTut.Device
  ( PhysicalDeviceInfo (..),
    pickPhysicalDevice,
    device,
  )
where

import Control.Exception.Safe (bracket)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Managed (Managed, managed)
import Data.Bits ((.&.), Bits, zeroBits)
import Data.ByteString (ByteString)
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Word (Word32, Word64)
import qualified Vulkan as Vk
import Vulkan.CStruct.Extends (SomeStruct (SomeStruct))

-- | Information about a physical device.
data PhysicalDeviceInfo
  = PhysicalDeviceInfo
      { pdiPhysicalDevice :: Vk.PhysicalDevice,
        pdiName :: Text,
        pdiGraphicsQueueIndex :: Word32,
        pdiPresentQueueIndex :: Word32,
        pdiSurfaceFormat :: Vk.SurfaceFormatKHR,
        pdiPresentMode :: Vk.PresentModeKHR,
        pdiSurfaceCapabilities :: Vk.SurfaceCapabilitiesKHR,
        pdiMemory :: Word64
      }

-- | Managed Vulkan device.
device :: PhysicalDeviceInfo -> Managed Vk.Device
device pdi =
  managed $
    bracket
      (acquireDevice pdi)
      releaseDevice

-- | Acquire a Vulkan device.
acquireDevice :: PhysicalDeviceInfo -> IO Vk.Device
acquireDevice pdi = do
  let queueCreateInfos :: Vector (SomeStruct Vk.DeviceQueueCreateInfo)
      queueCreateInfos =
        V.fromList
          [ SomeStruct $
              Vk.zero
                { Vk.queueFamilyIndex = i,
                  Vk.queuePriorities = V.singleton 1.0
                }
            | i <- nub [pdiGraphicsQueueIndex pdi, pdiPresentQueueIndex pdi]
          ]
      --
      enabledExtensionNames :: Vector ByteString
      enabledExtensionNames = V.fromList [Vk.KHR_SWAPCHAIN_EXTENSION_NAME]
      --
      createInfo :: Vk.DeviceCreateInfo '[]
      createInfo =
        Vk.zero
          { Vk.queueCreateInfos = queueCreateInfos,
            Vk.enabledExtensionNames = enabledExtensionNames
          }
  --
  Vk.createDevice (pdiPhysicalDevice pdi) createInfo Nothing

-- | Release a Vulkan device.
releaseDevice :: Vk.Device -> IO ()
releaseDevice = flip Vk.destroyDevice Nothing

-- | Pick a physical device to use.
--
-- Physical devices are filtered according to whether or not they're
-- acceptable, and after filtering, the device with the largest total amount
-- of memory is returned.
pickPhysicalDevice ::
  MonadIO m =>
  -- | Vulkan instance.
  Vk.Instance ->
  -- | The surface.
  Vk.SurfaceKHR ->
  -- | The desired surface format.
  Vk.SurfaceFormatKHR ->
  -- | Prioritised list of desired present modes.
  [Vk.PresentModeKHR] ->
  -- | Chosen physical device.
  m PhysicalDeviceInfo
pickPhysicalDevice vkInstance surface desiredFormat desiredPresentModes = do
  -- get all the physical devices
  (_, physDevs) <- Vk.enumeratePhysicalDevices vkInstance
  -- find the properties of the physical devices
  mInfos <-
    traverse
      (physicalDeviceInfo surface desiredFormat desiredPresentModes)
      physDevs
  -- keep the physical devices that are acceptable
  let infos :: Vector PhysicalDeviceInfo
      infos = V.mapMaybe id mInfos
  -- Choose the physical device with the largest memory
  pure $ V.maximumBy (comparing pdiMemory) infos

-- | Obtain physical device information of an acceptable physical device.
physicalDeviceInfo ::
  MonadIO m =>
  -- | The surface.
  Vk.SurfaceKHR ->
  -- | The desired surface format.
  Vk.SurfaceFormatKHR ->
  -- | Prioritised list of desired present modes.
  [Vk.PresentModeKHR] ->
  -- | The physical device.
  Vk.PhysicalDevice ->
  -- | Physical device information, iff this physical device is acceptable.
  m (Maybe PhysicalDeviceInfo)
physicalDeviceInfo surface desiredFormat desiredPresentModes physDev = do
  -- query all the required parameters from the physical device
  hasSwapchain <- physicalDeviceHasSwapchain physDev
  graphicsQueueIndices <- physicalDeviceGraphicsQueueIndices physDev
  presentQueueIndices <- physicalDevicePresentQueueIndices physDev surface
  surfaceFormat <- physicalDeviceSurfaceFormat physDev surface desiredFormat
  mPresentMode <- physicalDevicePresentMode physDev surface desiredPresentModes
  surfCaps <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR physDev surface
  memory <- physicalDeviceTotalMemory physDev
  -- establish if this device is acceptable
  let deviceOk :: Bool
      deviceOk =
        hasSwapchain
          && (not . V.null) graphicsQueueIndices
          && (not . V.null) presentQueueIndices
          && isJust mPresentMode
  -- device parameters
  let graphicsQueueIndex :: Word32
      graphicsQueueIndex = V.head graphicsQueueIndices
      --
      presentQueueIndex :: Word32
      presentQueueIndex = V.head presentQueueIndices
      --
      presentMode :: Vk.PresentModeKHR
      presentMode = case mPresentMode of
        Just x -> x
        Nothing ->
          error $
            "Should already have determined that the present mode is not empty!"
  -- device name
  name :: Text <-
    Text.decodeUtf8 . Vk.deviceName
      <$> Vk.getPhysicalDeviceProperties physDev
  -- return the device, if it's OK
  if deviceOk
    then
      pure $ Just $
        PhysicalDeviceInfo
          physDev
          name
          graphicsQueueIndex
          presentQueueIndex
          surfaceFormat
          presentMode
          surfCaps
          memory
    else pure Nothing

-- | Get the indices of graphics queues.
physicalDeviceGraphicsQueueIndices ::
  MonadIO m =>
  Vk.PhysicalDevice ->
  m (Vector Word32)
physicalDeviceGraphicsQueueIndices physDev = do
  qfps :: Vector Vk.QueueFamilyProperties <-
    Vk.getPhysicalDeviceQueueFamilyProperties physDev
  let nonEmpty :: Vk.QueueFamilyProperties -> Bool
      nonEmpty qfp = Vk.queueCount qfp > 0
      --
      isGraphicsQueue :: Vk.QueueFamilyProperties -> Bool
      isGraphicsQueue = anyBitsSet Vk.QUEUE_GRAPHICS_BIT . Vk.queueFlags
      --
      isNonEmptyGraphicsQueue :: Vk.QueueFamilyProperties -> Bool
      isNonEmptyGraphicsQueue qfp = isGraphicsQueue qfp && nonEmpty qfp
      --
      graphicsQueues :: Vector (Int, Vk.QueueFamilyProperties)
      graphicsQueues = V.filter (isNonEmptyGraphicsQueue . snd) (V.indexed qfps)
  pure $ fromIntegral . fst <$> graphicsQueues

-- | Get the indices of present queues.
physicalDevicePresentQueueIndices ::
  forall m.
  MonadIO m =>
  Vk.PhysicalDevice ->
  Vk.SurfaceKHR ->
  m (Vector Word32)
physicalDevicePresentQueueIndices physDev surface = do
  nqfps :: Int <- V.length <$> Vk.getPhysicalDeviceQueueFamilyProperties physDev
  let isPresQIdx :: Word32 -> m Bool
      isPresQIdx i = Vk.getPhysicalDeviceSurfaceSupportKHR physDev i surface
  V.filterM isPresQIdx (V.generate nqfps fromIntegral)

-- | Get the physical device surface format using a desired surface format.
physicalDeviceSurfaceFormat ::
  MonadIO m =>
  -- | The physical device to query.
  Vk.PhysicalDevice ->
  -- | The surface.
  Vk.SurfaceKHR ->
  -- | Desired surface format.
  Vk.SurfaceFormatKHR ->
  -- | Action to determine the actual surface format.
  m Vk.SurfaceFormatKHR
physicalDeviceSurfaceFormat physDev surface desiredFormat = do
  (_result, srfFmts) <- Vk.getPhysicalDeviceSurfaceFormatsKHR physDev surface
  let -- resolve duplicate functions
      format :: Vk.SurfaceFormatKHR -> Vk.Format
      format = Vk.format
      -- resolve duplicate functions
      colorSpace :: Vk.SurfaceFormatKHR -> Vk.ColorSpaceKHR
      colorSpace = Vk.colorSpace
      --
      fmtMatches :: Vk.SurfaceFormatKHR -> Bool
      fmtMatches sfmt = format desiredFormat == format sfmt
      --
      csMatches :: Vk.SurfaceFormatKHR -> Bool
      csMatches sfmt = colorSpace desiredFormat == colorSpace sfmt
  pure $
    if V.null srfFmts
      then desiredFormat
      else case V.head srfFmts of
        srfFmt | fmtMatches srfFmt, csMatches srfFmt -> desiredFormat
        srfFmt -> srfFmt

-- | Obtain a physical device presentation mode if possible.
physicalDevicePresentMode ::
  MonadIO m =>
  -- | The physical device to query.
  Vk.PhysicalDevice ->
  -- | The surface.
  Vk.SurfaceKHR ->
  -- | Prioritised list of desired presentation modes.
  [Vk.PresentModeKHR] ->
  -- | Action to provide an available presentation mode, if possible.
  m (Maybe Vk.PresentModeKHR)
physicalDevicePresentMode physDev surface desiredModes = do
  (_result, modes) <- Vk.getPhysicalDeviceSurfacePresentModesKHR physDev surface
  let desiredPModes :: Vector Vk.PresentModeKHR
      desiredPModes = V.fromList desiredModes
  pure
    $ V.headM
    $ V.filter (`V.elem` modes) desiredPModes

-- | Find the total memory of a physical device.
physicalDeviceTotalMemory :: MonadIO m => Vk.PhysicalDevice -> m Word64
physicalDeviceTotalMemory physDev = do
  memProps <- Vk.getPhysicalDeviceMemoryProperties physDev
  let heaps :: Vector Vk.MemoryHeap
      heaps = Vk.memoryHeaps memProps
      --
      heapSize :: Vk.MemoryHeap -> Vk.DeviceSize
      heapSize = Vk.size
  pure $ sum (heapSize <$> heaps)

-- | Check if a Vulkan physical device has a swapchain.
physicalDeviceHasSwapchain :: MonadIO m => Vk.PhysicalDevice -> m Bool
physicalDeviceHasSwapchain physDev = do
  (_result, extensions) <- Vk.enumerateDeviceExtensionProperties physDev Nothing
  let isSwapchainExt :: Vk.ExtensionProperties -> Bool
      isSwapchainExt = (Vk.KHR_SWAPCHAIN_EXTENSION_NAME ==) . Vk.extensionName
  pure $ V.any isSwapchainExt extensions

-- | Check if any bits from a mask are set.
anyBitsSet :: Bits a => a -> a -> Bool
anyBitsSet mask bits = mask .&. bits /= zeroBits
