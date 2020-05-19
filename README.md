# Vulkan Tutorial

This is just me following the
[Vulkan Tutorial](https://vulkan-tutorial.com), and trying to translate it into
Haskell. So it's not very interesting.

## Miscellaneous Hints

For `ghcid`, to allow `libvulkan.dylib` to be found:

```
install_name_tool -add_rpath "$VULKAN_SDK/lib" <path/to/libHSvulkan.dylib>
```

To fix permissions errors for `libvulkan.dylib`:

```
cd $VULKAN_SDK/lib
xattr -d com.apple.quarantine libvulkan.dylib
```
