#!/usr/bin/env bash
set -euo pipefail

ROOT="${HOME}/Desktop/DJH/mobile-jh"   # dein „1 Ebene tiefer“-Root
IN="${ROOT}/export_kmz"               # oder export_kmz – muss exakt stimmen
OUT="${ROOT}/mobile-jh/data"          # wenn dein Repo wirklich ROOT/mobile-jh/... ist; sonst unten anpassen

echo "ROOT=$ROOT"
echo "IN=$IN"
echo "OUT=$OUT"

mkdir -p "$OUT"
shopt -s nullglob
files=( "$IN"/JH_*_master.kmz )

if (( ${#files[@]} == 0 )); then
  echo "FEHLER: Keine KMZ gefunden: $IN/JH_*_master.kmz"
  ls -lah "$IN"
  exit 2
fi

# Dateien einsammeln (ohne stilles Wegschlucken)
files=( "$IN"/JH_*_master.kmz )
if (( ${#files[@]} == 0 )); then
  echo "FEHLER: Keine KMZ gefunden mit Pattern: $IN/JH_*_master.kmz"
  echo "Tipp: ls -lah \"$IN\""
  exit 2
fi

command -v ogrinfo >/dev/null || { echo "FEHLER: ogrinfo fehlt (GDAL)."; exit 3; }
command -v ogr2ogr >/dev/null || { echo "FEHLER: ogr2ogr fehlt (GDAL)."; exit 3; }
command -v python3 >/dev/null || { echo "FEHLER: python3 fehlt."; exit 3; }

list_layers() {
  ogrinfo -ro -q "$1" 2>/dev/null | awk -F': ' '/^[ ]*[0-9]+: /{print $2}'
}

for kmz in "${files[@]}"; do
  nr="$(basename "$kmz" | sed -E 's/^JH_([0-9]+)_master\.kmz$/\1/')"
  out="${OUT}/jh_${nr}.geojson"

  echo "Konvertiere $kmz → $(basename "$out")"

  tmpdir="$(mktemp -d)"
  # cleanup nur für diesen Loop-Durchlauf
  cleanup() { rm -rf "$tmpdir"; }
  trap cleanup RETURN

  i=0
  while IFS= read -r layer; do
    [[ -z "$layer" ]] && continue
    i=$((i+1))
    part="${tmpdir}/part_${i}.geojson"

    ogr2ogr -f GeoJSON "$part" "$kmz"  \
      -skipfailures \
      -dialect SQLITE \
      -sql "SELECT *, '${layer}' AS category_group FROM \"${layer}\""
  done < <(list_layers "$kmz")

  python3 - <<'PY' "$tmpdir" "$out"
import json, sys, glob, os
tmpdir, out = sys.argv[1], sys.argv[2]
features = []
for fn in sorted(glob.glob(os.path.join(tmpdir, "part_*.geojson"))):
    with open(fn, "r", encoding="utf-8") as f:
        gj = json.load(f)
    features.extend(gj.get("features", []))
fc = {"type":"FeatureCollection", "features": features}
with open(out, "w", encoding="utf-8") as f:
    json.dump(fc, f, ensure_ascii=False)
print(f"Wrote {out} with {len(features)} features")
PY

done
