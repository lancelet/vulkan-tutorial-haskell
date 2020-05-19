#!/usr/bin/env bash
#
# Run a Hoogle instance locally with the installed packages' documentation.
#
# From this GitHub issue for stack:
#  https://github.com/commercialhaskell/stack/issues/5228

stack haddock

readonly local_doc_root=$(stack path --local-doc-root)
readonly snapshot_doc=$(stack path --snapshot-doc-root)
readonly database=$(stack path --local-hoogle-root)/database.foo

# Install Hoogle as a stack build tool if necessary
if ! stack exec which hoogle > /dev/null; then
    stack build --copy-compiler-tool hoogle
fi

stack exec -- hoogle generate   \
      --local="$local_doc_root" \
      --local="$snapshot_doc"   \
      --database="$database"

stack exec -- hoogle server \
      --local               \
      --port=8080           \
      --database="$database"
