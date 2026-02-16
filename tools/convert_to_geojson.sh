#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

IN="${REPO_ROOT}/data/export_kmz"
OUT="${REPO_ROOT}/data/geojson"

mkdir -p "$OUT"

command -v ogr2ogr >/dev/null || { echo "FEHLER: ogr2ogr fehlt."; exit 3; }

echo "IN=$IN"
echo "OUT=$OUT"

# -----------------------------
# 1) DJH_alle_JH.kml – DIREKT
# -----------------------------
KML_FILE="${IN}/DJH_alle_JH.kml"

if [[ -f "$KML_FILE" ]]; then
  echo "Konvertiere DJH_alle_JH.kml → djh_alle_jh.geojson"

  ogr2ogr \
    -f GeoJSON \
    "${OUT}/djh_alle_jh.geojson" \
    "$KML_FILE"

  echo "Fertig: ${OUT}/djh_alle_jh.geojson"
fi

# -----------------------------
# 2) KMZ wie bisher
# -----------------------------
shopt -s nullglob
for kmz in "$IN"/JH_*_master.kmz; do
  base="$(basename "$kmz")"
  nr="$(echo "$base" | sed -E 's/^JH_([0-9]+)_master\.kmz$/\1/')"

  if [[ -z "$nr" || "$nr" == "$base" ]]; then
    echo "WARN: Konnte NR nicht lesen: $base"
    continue
  fi

  out="${OUT}/jh_${nr}.geojson"

  if [[ -f "$out" ]]; then
    echo "SKIP: $out existiert"
    continue
  fi

  echo "Konvertiere $base → jh_${nr}.geojson"

  ogr2ogr \
    -f GeoJSON \
    "$out" \
    "$kmz"

  echo "Fertig: $out"
done
