#!/usr/bin/env bash
#
# Shell for macOS on my machine.
#
# I have the macOS Vulkan SDK extracted at ~/vulkansdk-${VULKAN_SDK_VERSION}.
# I'm also using Nix (although not very well!).


export VULKAN_SDK_VERSION='macos-1.2.135.0'
export VULKAN_SDK="$HOME/vulkansdk-$VULKAN_SDK_VERSION/macOS"
export PATH="$VULKAN_SDK/bin:$PATH"
export DYLD_LIBRARY_PATH="$VULKAN_SDK/lib:$DYLD_LIBRARY_PATH"
export VK_ICD_FILENAMES="$VULKAN_SDK/share/vulkan/icd.d/MoltenVK_icd.json"
export VK_LAYER_PATH="$VULKAN_SDK/share/vulkan/explicit_layer.d"

echo "Environment:"
echo "  VULKAN_SDK_VERSION='$VULKAN_SDK_VERSION'"
echo "  VULKAN_SDK='$VULKAN_SDK'"
echo "  PATH='$PATH'"
echo "  DYLD_LIBRARY_PATH='$DYLD_LIBRARY_PATH'"
echo "  VK_ICD_FILENAMES='$VK_ICD_FILENAMES'"
echo "  VK_LAYER_PATH='$VK_LAYER_PATH'"

nix-shell -p pkgconfig SDL2
