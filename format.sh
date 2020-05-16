#!/usr/bin/env bash
#
# Format the source code.

# Usage string
function usage() {
    scriptname=$(basename "$0")
    echo "$scriptname - source formatting utility"
    echo "usage: $scriptname [options]"
    echo "  options:"
    echo "    -v, --verbose    Produce verbose output"
    echo "    -h, --help       Display this help message"
}

# Command-line arguments
verbose=false
while [ "$1" != "" ]; do
    param=$(echo "$1" | awk -F= '{print $1}')
    # value=$(echo "$1" | awk -F= '{print $2}')
    case $param in
        -v | --verbose)
            verbose=true
            ;;
        -h | --help)
            usage
            exit
            ;;
        *)
            echo "ERROR: unknown parameter '$param'"
            usage
            exit 1
            ;;
    esac
    shift
done

# Install tools if necessary
declare -ra tools=('cabal-fmt' 'ormolu')
for tool_name in "${tools[@]}"
do
    if ! stack exec which "$tool_name" > /dev/null
    then
        echo "Required tool $tool_name was not found; installing it using stack"
        stack build --copy-compiler-tool "$tool_name"
    fi
done

# Find cabal files
declare -a cabal_files
while IFS= read -r line; do
    cabal_files+=("$line")
done < <(find . -name '*.cabal')
readonly cabal_files

# Find Haskell files
declare -a haskell_files
while IFS= read -r line; do
    haskell_files+=("$line")
done < <(find . -name '*.hs' | grep --invert-match '\.stack-work')
readonly haskell_files

# Format cabal files
if $verbose; then
    echo "Formatting cabal files:"
    for name in "${cabal_files[@]}"; do
        echo "    $name"
    done
fi
stack exec cabal-fmt -- --tabular -i "${cabal_files[@]}"

# Format Haskell files
if $verbose; then
    echo "Formatting Haskell files:"
    for name in "${haskell_files[@]}"; do
        echo "    $name"
    done
fi
stack exec ormolu -- --mode=inplace "${haskell_files[@]}"
