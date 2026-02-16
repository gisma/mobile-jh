#!/usr/bin/env bash
set -euo pipefail

FORCE=0
for arg in "$@"; do
  case "$arg" in
    --force) FORCE=1 ;;
    *) echo "Unknown option: $arg"; exit 1 ;;
  esac
done

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

IN="${REPO_ROOT}/data/export_kmz"
OUT="${REPO_ROOT}/data/geojson"
mkdir -p "$OUT"

command -v ogr2ogr >/dev/null || { echo "FEHLER: ogr2ogr fehlt."; exit 3; }
command -v ogrinfo >/dev/null || { echo "FEHLER: ogrinfo fehlt."; exit 3; }

echo "IN=$IN"
echo "OUT=$OUT"
echo "FORCE=$FORCE"

shopt -s nullglob
for kmz in "$IN"/JH_*_master.kmz; do
  base="$(basename "$kmz")"
  nr="$(echo "$base" | sed -E 's/^JH_([0-9]+)_master\.kmz$/\1/')"

  if [[ -z "$nr" || "$nr" == "$base" ]]; then
    echo "WARN: Konnte NR nicht lesen: $base"
    continue
  fi

  out="${OUT}/jh_${nr}.geojson"

  if [[ -f "$out" && "$FORCE" -ne 1 ]]; then
    echo "SKIP: $out existiert"
    continue
  fi

  # ersten Layer aus dem KMZ holen (Format: "1: LayerName (Point)")
  layer="$(
    ogrinfo -ro -q "$kmz" \
      | sed -n 's/^[[:space:]]*[0-9]\+:[[:space:]]\([^()]*\).*/\1/p' \
      | head -n 1 \
      | sed 's/[[:space:]]*$//'
  )"

  if [[ -z "$layer" ]]; then
    echo "ERROR: Kein Layer gefunden in $base"
    exit 4
  fi

  echo "Konvertiere $base (Layer: $layer) → $(basename "$out")"

  rm -f "$out"  # sicher überschreiben, falls FORCE=1 oder halbkaputt
  ogr2ogr -f GeoJSON "$out" "$kmz" "$layer"

  echo "Fertig: $out"
done
