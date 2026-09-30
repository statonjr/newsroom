#!/usr/bin/env bash
# Install a jolt release for a target and put it on the job's PATH.
# Usage: ci/install-jolt.sh v0.8.15 x86_64-linux
set -euo pipefail
ver="$1"
target="$2"
dest="$HOME/.jolt-dist"
mkdir -p "$dest"
base="https://github.com/jolt-lang/jolt/releases/download/${ver}/jolt-${ver}-${target}"
case "$target" in
  *windows)
    curl -fsSL -o jolt.zip "${base}.zip"
    unzip -q jolt.zip -d "$dest"
    exe=$(find "$dest" -name jolt.exe | head -1) ;;
  *)
    curl -fsSL -o jolt.tar.gz "${base}.tar.gz"
    tar xzf jolt.tar.gz -C "$dest"
    exe=$(find "$dest" -name jolt -type f -perm -u+x | head -1) ;;
esac
[ -n "$exe" ] || { echo "no jolt binary in the ${ver} ${target} archive"; exit 1; }
dirname "$exe" >> "$GITHUB_PATH"
"$exe" --version
