#!/usr/bin/env bash
set -euo pipefail

ROOT="$HOME/Desktop/DJH"
IN="$ROOT/export_kmz"
OUT="$ROOT/mobile-jh/data"

mkdir -p "$OUT"

shopt -s nullglob
files=("$IN"/JH_*_master.kmz)

if [ ${#files[@]} -eq 0 ]; then
  echo "Keine KMZ-Dateien in $IN gefunden."
  exit 0
fi

# Layernamen aus ogrinfo holen (Folder in KML/KMZ erscheinen als Layer)
get_layers() {
  ogrinfo -ro -so "$1" 2>/dev/null \
    | sed -n 's/^[[:space:]]*[0-9]\+:[[:space:]]*//p' \
    | sed 's/[[:space:]]*(.*$//'
}

for f in "${files[@]}"; do
  nr=$(basename "$f" | sed -E 's/JH_([0-9]+)_master\.kmz/\1/')
  out="$OUT/jh_${nr}.geojson"

  echo "Konvertiere $f → $(basename "$out")"

  # Layerliste
  mapfile -t layers < <(get_layers "$f")

  if [ ${#layers[@]} -eq 0 ]; then
    echo "  WARN: keine Layer gefunden (überspringe)."
    continue
  fi

  # Ziel ggf. löschen
  rm -f "$out"

  # 1) erster Layer erzeugt das GeoJSON (Layername wird auf 'merged' gesetzt)
  first="${layers[0]}"
  ogr2ogr -f GeoJSON "$out" "$f" "$first" \
    -nln merged \
    -lco RFC7946=YES \
    -lco WRITE_BBOX=NO \
    -skipfailures

  # 2) restliche Layer in denselben Ziel-Layer append-en
  if [ ${#layers[@]} -gt 1 ]; then
    for lyr in "${layers[@]:1}"; do
      ogr2ogr -f GeoJSON -update -append "$out" "$f" "$lyr" \
        -nln merged \
        -skipfailures
    done
  fi
done

echo "Fertig."
