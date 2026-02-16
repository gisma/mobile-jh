#!/usr/bin/env python3
import json
import os
from collections import defaultdict

INFILE = "data/master.geojson"
OUTDIR = "data"

def get_prop(d, keys):
  for k in keys:
    if k in d and d[k] not in (None, ""):
      return d[k]
  return None

def is_point(geom):
  return geom and geom.get("type") in ("Point", "MultiPoint")

def main():
  os.makedirs(OUTDIR, exist_ok=True)

  with open(INFILE, "r", encoding="utf-8") as f:
    gj = json.load(f)

  feats = gj.get("features", [])
  by_nr = defaultdict(list)
  jh_only = []

  for ft in feats:
    props = ft.get("properties") or {}
    geom = ft.get("geometry")
    if not is_point(geom):
      continue

    kind = str(props.get("kind", "")).upper()
    nr = get_prop(props, ["nr", "NR", "jh_nr", "JH_NR"])

    # Wenn nr fehlt, kann man nichts zuordnen → skip
    if nr is None:
      continue

    # Normalisiere nr zu int, wenn möglich
    try:
      nr_int = int(str(nr).strip())
    except:
      continue

    if kind == "JH":
      jh_only.append(ft)
      by_nr[nr_int].append(ft)   # optional JH in jh_<nr>.geojson mit drin
    else:
      by_nr[nr_int].append(ft)

  # jh_only.geojson
  out_jh_only = {
    "type": "FeatureCollection",
    "features": jh_only
  }
  with open(os.path.join(OUTDIR, "jh_only.geojson"), "w", encoding="utf-8") as f:
    json.dump(out_jh_only, f, ensure_ascii=False)

  # jh_<nr>.geojson
  index = []
  for nr_int, lst in sorted(by_nr.items()):
    out = {"type": "FeatureCollection", "features": lst}
    fn = f"jh_{nr_int}.geojson"
    with open(os.path.join(OUTDIR, fn), "w", encoding="utf-8") as f:
      json.dump(out, f, ensure_ascii=False)
    index.append({"nr": nr_int, "file": fn, "count": len(lst)})

  with open(os.path.join(OUTDIR, "jh_index.json"), "w", encoding="utf-8") as f:
    json.dump(index, f, ensure_ascii=False, indent=2)

  print(f"OK: jh_only.geojson = {len(jh_only)} JH")
  print(f"OK: jh_*.geojson   = {len(index)} Dateien")

if __name__ == "__main__":
  main()
