#!/bin/bash

set -eo pipefail

prefix="$1"

if [ -z "$prefix" ]; then
  echo "usage: ./remap-build <prefix>"
  echo ""
  exit 1
fi

echo "Remapping Build with Prefix: $prefix"

find build \( -name "*.html" -o -name "*.json" \) \
  -exec sed -i.bak "s*/static/*/$prefix/static/*g" {} \;

find build -name "*.js" \
  -exec sed -i.bak "s*window.BUILD_MOUNT_PATH*\"$prefix\"*g" {} \;
