#!/usr/bin/env bash
set -euo pipefail

: "${FOURMOLU_VERSION:?FOURMOLU_VERSION must be set}"
: "${HLINT_VERSION:?HLINT_VERSION must be set}"
: "${TOOLS_CACHE_DIR:?TOOLS_CACHE_DIR must be set}"
: "${GITHUB_ENV:?GITHUB_ENV must be set}"

if [[ "$(uname -s)" != "Linux" ]]; then
  echo "install-tools.sh only supports Linux runners" >&2
  exit 1
fi

mkdir -p "${TOOLS_CACHE_DIR}"

fourmolu_repo="fourmolu/fourmolu"
fourmolu_tag="v${FOURMOLU_VERSION}"
fourmolu_pattern="fourmolu-${FOURMOLU_VERSION}-linux-x86_64"
fourmolu_dir="${TOOLS_CACHE_DIR}/fourmolu/${FOURMOLU_VERSION}"
fourmolu_bin="${fourmolu_dir}/fourmolu"

hlint_repo="ndmitchell/hlint"
hlint_tag="v${HLINT_VERSION}"
hlint_archive="hlint-${HLINT_VERSION}-x86_64-linux.tar.gz"
hlint_dir="${TOOLS_CACHE_DIR}/hlint/${HLINT_VERSION}"
hlint_bin="${hlint_dir}/hlint"

download_asset() {
  local repo=$1
  local tag=$2
  local pattern=$3
  local dest_dir=$4
  gh release download "${tag}" --repo "${repo}" --pattern "${pattern}" --dir "${dest_dir}" --clobber
}

prepare_fourmolu() {
  if [[ -x "${fourmolu_bin}" ]]; then
    echo "Fourmolu already prepared at ${fourmolu_bin}"
  else
    echo "Downloading Fourmolu ${FOURMOLU_VERSION}"
    mkdir -p "${fourmolu_dir}"
    tmp_dir="$(mktemp -d)"
    download_asset "${fourmolu_repo}" "${fourmolu_tag}" "${fourmolu_pattern}" "${tmp_dir}"
    local archive
    archive=$(find "${tmp_dir}" -maxdepth 1 -type f -name "${fourmolu_pattern}" -print -quit)
    if [[ -z "${archive}" ]]; then
      echo "Failed to download Fourmolu asset" >&2
      exit 1
    fi
    install -m 0755 "${archive}" "${fourmolu_bin}"
    rm -rf "${tmp_dir}"
  fi
  printf 'FOURMOLU_BIN=%s\n' "${fourmolu_bin}" >>"${GITHUB_ENV}"
}

prepare_hlint() {
  if [[ -x "${hlint_bin}" ]]; then
    echo "HLint already prepared at ${hlint_bin}"
  else
    echo "Downloading HLint ${HLINT_VERSION}"
    mkdir -p "${hlint_dir}"
    tmp_dir="$(mktemp -d)"
    download_asset "${hlint_repo}" "${hlint_tag}" "${hlint_archive}" "${tmp_dir}"
    tar -xzf "${tmp_dir}/${hlint_archive}" -C "${tmp_dir}"
    local extracted_bin
    extracted_bin="$(find "${tmp_dir}" -type f -name hlint -print -quit)"
    if [[ -z "${extracted_bin}" ]]; then
      echo "Failed to locate hlint binary in archive" >&2
      exit 1
    fi
    install -m 0755 "${extracted_bin}" "${hlint_bin}"
    rm -rf "${tmp_dir}"
  fi
  printf 'HLINT_BIN=%s\n' "${hlint_bin}" >>"${GITHUB_ENV}"
}

prepare_fourmolu
prepare_hlint
