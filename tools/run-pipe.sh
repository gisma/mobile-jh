#!/usr/bin/env bash
set -euo pipefail

# Repo-Root = eine Ebene über tools/
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

echo "REPO_ROOT=${REPO_ROOT}"

# Zielordner sicherstellen
mkdir -p "${REPO_ROOT}/data/export-kmz"
mkdir -p "${REPO_ROOT}/data/geojson"

echo
echo "STEP 1/3: R → KMZ"
echo "----------------------------------------"
# Erwartung: tools/export_kmz_batch.R schreibt nach data/export-kmz
Rscript "${REPO_ROOT}/tools/export_kmz_batch.R"

echo
echo "STEP 2/3: KMZ → GeoJSON"
echo "----------------------------------------"
bash "${REPO_ROOT}/tools/convert_to_geojson.sh"

echo
echo "STEP 3/3: GeoJSON → Manifest"
echo "----------------------------------------"
python3 "${REPO_ROOT}/tools/build_manifest.py"

echo
echo "DONE"
echo "KMZ:      ${REPO_ROOT}/data/export-kmz"
echo "GeoJSON:  ${REPO_ROOT}/data/geojson"
echo "Manifest: ${REPO_ROOT}/data/manifest.json"
