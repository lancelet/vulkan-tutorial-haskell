# Vulkan Tutorial

This is just me following the
[Vulkan Tutorial](https://vulkan-tutorial.com), and trying to translate it into
Haskell. So it's not very interesting.

## macOS Getting Started

Download the [Vulkan SDK](https://vulkan.lunarg.com/sdk/home) for macOS and
extract it to a local location (eg. `$HOME`). Check the following files to
update the locations to where you extracted the SDK:
  - `shell.sh` - the `VULKAN_SDK_VERSION` and `VULKAN_SDK` environment 
    variables.
  - `stack.yaml` - the `extra-include-dirs` and `extra-lib-dirs` settings.
  
Enter a shell with the correct Nix dependencies and environment variables
configured for Vulkan development:

```bash
./shell.sh
```

Build and run the stack project:

```bash
stack build
stack run
```
