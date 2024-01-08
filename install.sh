#!/bin/bash

abort() {
  printf "%s\n" "$@" >&2
  exit 1
}

# First check OS.
OS="$(uname)"
if [[ "${OS}" == "Linux" ]]
then
  CYBER_LINUX=1
  CYBER_PATH=/usr/local/bin
elif [[ "${OS}" == "Darwin" ]]
then
  CYBER_MACOS=1
  CYBER_PATH=/usr/local/bin
else
  abort "Install is only supported on macOS and Linux."
fi

# Required installation paths.
# FIND: v0.4
if [[ -n "${CYBER_MACOS}" ]]
then
  UNAME_MACHINE="$(/usr/bin/uname -m)"
  if [[ "${UNAME_MACHINE}" == "arm64" ]]
  then
    CYBER_BUNDLE="https://github.com/fubark/cyber/releases/download/0.3/cyber-macos-arm64.tar.gz"
  else
    CYBER_BUNDLE="https://github.com/fubark/cyber/releases/download/0.3/cyber-macos-x64.tar.gz"
  fi
else
  UNAME_MACHINE="$(uname -m)"
  CYBER_BUNDLE="https://github.com/fubark/cyber/releases/download/0.3/cyber-linux-x64.tar.gz"
fi

sudo sh -c "mkdir -p ${CYBER_PATH}; \
    echo 'Downloading ${CYBER_BUNDLE}.'; \
    curl -fsSL $CYBER_BUNDLE | tar -xz -C $CYBER_PATH; \
    chmod a+x ${CYBER_PATH}/cyber; \
    echo 'Installed to ${CYBER_PATH}/cyber.'"
