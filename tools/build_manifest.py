#!/usr/bin/env python3
import json, glob, os, re
from collections import Counter

DATA_DIR = "data"
OUT = os.path.join(DATA_DIR, "jh_manifest.json")

SKIP_TOP = {"Information"}  # wenn das Top ist -> nimm nächsthöheres

def get_nr(fn):
  m = re.search(r"jh_(\d+)\.geojson$", os.path.basename(fn))
  return m.group(1) if m else None

manifest = {}
files = sorted(glob.glob(os.path.join(DATA_DIR, "jh_*.geojson")))
for fn in files:
  nr = get_nr(fn)
  if not nr:
    continue
  with open(fn, "r", encoding="utf-8") as f:
    gj = json.load(f)

  c = Counter()
  total = 0
  for feat in gj.get("features", []):
    geom = (feat.get("geometry") or {}).get("type")
    if geom not in ("Point", "MultiPoint"):
      continue
    p = feat.get("properties") or {}
    # falls vorhanden: nur POIs zählen
    if "kind" in p and p["kind"] != "POI":
      continue
    g = p.get("category_group") or "Unbekannt"
    if not g or g == "Unbekannt":
      continue
    c[g] += 1
    total += 1

  if total == 0 or not c:
    manifest[nr] = {"dominant_group": "Unbekannt", "dominant_count": 0, "poi_total": 0}
  else:
    ordered = c.most_common()

    # 1) Top-Group bestimmen
    dom, dom_n = ordered[0]

    # 2) Wenn Top in SKIP_TOP ist: nächsthöhere nehmen (falls vorhanden)
    if dom in SKIP_TOP:
      picked = None
      for g, n in ordered[1:]:
        if g not in SKIP_TOP and g != "Unbekannt":
          picked = (g, n)
          break
      if picked is not None:
        dom, dom_n = picked
      # else: bleibt "Information" (es gibt keine Alternative)

    manifest[nr] = {"dominant_group": dom, "dominant_count": dom_n, "poi_total": total}

with open(OUT, "w", encoding="utf-8") as f:
  json.dump(manifest, f, ensure_ascii=False)

print(f"Wrote {OUT} ({len(manifest)} JH)")
