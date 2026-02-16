#!/usr/bin/env bash
set -euo pipefail

FORCE=1

# ------------------------------
# Argument Parsing
# ------------------------------
for arg in "$@"; do
  case "$arg" in
    --force)
      FORCE=1
      ;;
    *)
      echo "Unknown option: $arg"
      exit 1
      ;;
  esac
done

# ------------------------------
# Path Setup
# ------------------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

EXPORT_DIR="${REPO_ROOT}/data/export_kmz"
GEOJSON_DIR="${REPO_ROOT}/data/geojson"
MANIFEST_FILE="${REPO_ROOT}/data/manifest.json"

echo "REPO_ROOT=${REPO_ROOT}"
echo "FORCE=${FORCE}"
echo

# ------------------------------
# Safe Directory Creation
# ------------------------------
safe_mkdir() {
  local dir="$1"
  if [[ ! -d "$dir" ]]; then
    echo "Creating directory: $dir"
    mkdir -p "$dir"
  else
    echo "Directory exists: $dir"
  fi
}

safe_mkdir "$EXPORT_DIR"
safe_mkdir "$GEOJSON_DIR"

# ------------------------------
# Safe File Check
# ------------------------------
check_overwrite() {
  local file="$1"

  if [[ -f "$file" ]]; then
    if [[ "$FORCE" -eq 1 ]]; then
      echo "Overwriting existing file (force): $file"
    else
      echo "ERROR: File exists: $file"
      echo "Run with --force to overwrite."
      exit 1
    fi
  fi
}

# ------------------------------
# STEP 1
# ------------------------------
echo "STEP 1/4: R → KMZ"
echo "----------------------------------------"

if compgen -G "${EXPORT_DIR}/*.kmz" > /dev/null; then
  if [[ "$FORCE" -eq 1 ]]; then
    echo "KMZ existieren – FORCE aktiv → neu erzeugen"
    rm -f "${EXPORT_DIR}"/*.kmz
    Rscript "${REPO_ROOT}/tools/batch-jh-export.r"
  else
    echo "SKIP: KMZ existieren bereits in ${EXPORT_DIR}"
  fi
else
  echo "Keine KMZ gefunden → erzeuge neu"
  Rscript "${REPO_ROOT}/tools/batch-jh-export.r"
fi


# ------------------------------
# STEP 2
# ------------------------------
echo
echo "STEP 2/4: KMZ → GeoJSON (convert_to_geojson.sh)"
echo "----------------------------------------"
bash "${REPO_ROOT}/tools/convert_to_geojson.sh" 

# ------------------------------
# STEP 3
# ------------------------------
echo
echo "STEP 3/4: KML → GeoJSON (main_convert_to_geojson.py)"
echo "----------------------------------------"
python3 "${REPO_ROOT}/tools/main_convert_to_geojson.py"

if ! compgen -G "${GEOJSON_DIR}/*.geojson" > /dev/null; then
  echo "ERROR: No GeoJSON files generated in ${GEOJSON_DIR}"
  exit 1
fi

# ------------------------------
# STEP 4
# ------------------------------
echo
echo "STEP 4/4: GeoJSON → Manifest"
echo "----------------------------------------"


python3 "${SCRIPT_DIR}/tools/build_manifest.py"



# ------------------------------
# DONE
# ------------------------------
echo
echo "DONE"
echo "KMZ:      ${EXPORT_DIR}"
echo "GeoJSON:  ${GEOJSON_DIR}"
echo "Manifest: ${MANIFEST_FILE}"
