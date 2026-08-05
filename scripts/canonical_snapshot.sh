#!/usr/bin/env bash
#
# Publish or restore the immutable analysis-input snapshot.
#
# Usage:
#   scripts/canonical_snapshot.sh publish <staging-dir> <tag> <asset-name>
#   scripts/canonical_snapshot.sh restore <descriptor-json> <destination-dir>

set -euo pipefail

command="${1:-}"
repository="${GITHUB_REPOSITORY:-zuizui0223/hotarubukuro}"
api="https://api.github.com"

die() {
  echo "ERROR: $*" >&2
  exit 1
}

require_token() {
  [[ -n "${GITHUB_TOKEN:-}" ]] || die "GITHUB_TOKEN is required for release publication."
}

auth_args=()
if [[ -n "${GITHUB_TOKEN:-}" ]]; then
  auth_args=(-H "Authorization: Bearer ${GITHUB_TOKEN}")
fi

publish() {
  local staging="$1" tag="$2" asset="$3"
  [[ -d "$staging" ]] || die "Staging directory not found: $staging"
  require_token

  local archive="${PWD}/${asset}"
  tar --sort=name \
      --mtime='UTC 2020-01-01' \
      --owner=0 --group=0 --numeric-owner \
      --format=gnu \
      -czf "$archive" -C "$staging" .
  local sha
  sha="$(sha256sum "$archive" | awk '{print $1}')"
  echo "archive=$archive"
  echo "sha256=$sha"
  echo "bytes=$(stat -c%s "$archive")"

  local release_id
  release_id="$(curl -sS "${auth_args[@]}" \
    -H "Accept: application/vnd.github+json" \
    "${api}/repos/${repository}/releases/tags/${tag}" | jq -r '.id // empty')"

  if [[ -z "$release_id" ]]; then
    local target="${GITHUB_HEAD_REF:-${GITHUB_REF_NAME:-${GITHUB_SHA:-}}}"
    echo "Creating release ${tag} targeting ${target}"
    local create_status
    create_status="$(curl -sS -w '%{http_code}' -o /tmp/snapshot-release.json -X POST \
      "${auth_args[@]}" \
      -H "Accept: application/vnd.github+json" \
      "${api}/repos/${repository}/releases" \
      -d "$(jq -n --arg tag "$tag" --arg target "$target" '{
             tag_name: $tag,
             target_commitish: $target,
             name: $tag,
             body: "Immutable analysis-input snapshot. Contents and per-file SHA-256 hashes are recorded in SNAPSHOT_MANIFEST.csv inside the archive and in inputs/canonical_snapshot.json in the repository.",
             draft: false,
             prerelease: false
           }')")"
    echo "release create HTTP ${create_status}"
    cat /tmp/snapshot-release.json
    release_id="$(jq -r '.id // empty' /tmp/snapshot-release.json)"
    if [[ -z "$release_id" ]]; then
      die "Could not create release ${tag}: HTTP ${create_status}. See the response body above."
    fi
  fi

  local existing
  existing="$(curl -sS "${auth_args[@]}" \
    -H "Accept: application/vnd.github+json" \
    "${api}/repos/${repository}/releases/${release_id}/assets" \
    | jq -r --arg name "$asset" '.[] | select(.name == $name) | .id')"
  if [[ -n "$existing" ]]; then
    die "Asset ${asset} already exists on release ${tag}. Publish a new tag instead of overwriting it."
  fi

  local upload_status
  upload_status="$(curl -sS -w '%{http_code}' -o /tmp/snapshot-upload.json -X POST \
    "${auth_args[@]}" \
    -H "Accept: application/vnd.github+json" \
    -H "Content-Type: application/gzip" \
    --data-binary "@${archive}" \
    "https://uploads.github.com/repos/${repository}/releases/${release_id}/assets?name=${asset}")"
  if [[ "$upload_status" != "201" ]]; then
    cat /tmp/snapshot-upload.json >&2 || true
    die "Snapshot upload failed with HTTP ${upload_status}."
  fi
  echo "published=${tag}/${asset}"
}

restore() {
  local descriptor="$1" destination="$2"
  [[ -f "$descriptor" ]] || die "Snapshot descriptor not found: $descriptor."

  local tag asset expected
  tag="$(jq -r '.release_tag' "$descriptor")"
  asset="$(jq -r '.asset_name' "$descriptor")"
  expected="$(jq -r '.asset_sha256' "$descriptor")"
  [[ -n "$tag" && "$tag" != "null" ]] || die "descriptor has no release_tag"
  [[ -n "$expected" && "$expected" != "null" && "$expected" != "" ]] \
    || die "descriptor has no asset_sha256; refusing to restore an unverifiable input"

  local release_json asset_id
  release_json="$(curl -sS --fail "${auth_args[@]}" \
    -H "Accept: application/vnd.github+json" \
    "${api}/repos/${repository}/releases/tags/${tag}")" \
    || die "Could not read public release ${tag}. Set GITHUB_TOKEN if anonymous API access is rate-limited."
  asset_id="$(printf '%s' "$release_json" \
    | jq -r --arg name "$asset" '.assets[] | select(.name == $name) | .id')"
  [[ -n "$asset_id" ]] || die "Release ${tag} has no asset named ${asset}."

  mkdir -p "$destination"
  local archive="${destination}/${asset}"
  local attempt
  for attempt in 1 2 3 4; do
    if curl -sS --fail --location \
        "${auth_args[@]}" \
        -H "Accept: application/octet-stream" \
        "${api}/repos/${repository}/releases/assets/${asset_id}" \
        -o "$archive"; then
      break
    fi
    echo "download attempt ${attempt} failed; retrying" >&2
    sleep $((2 ** attempt))
  done
  [[ -s "$archive" ]] || die "Snapshot asset download produced no file. Set GITHUB_TOKEN if anonymous access is rate-limited."

  local observed
  observed="$(sha256sum "$archive" | awk '{print $1}')"
  if [[ "$observed" != "$expected" ]]; then
    die "Snapshot checksum mismatch. expected=${expected} observed=${observed}"
  fi
  echo "asset_sha256_verified=${observed}"

  tar -xzf "$archive" -C "$destination"
  rm -f "$archive"
  echo "restored=${destination}"
}

case "$command" in
  publish) shift; publish "$@" ;;
  restore) shift; restore "$@" ;;
  *) die "Usage: $0 {publish|restore} ..." ;;
esac
