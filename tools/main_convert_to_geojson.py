#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import glob
import json
import os
import xml.etree.ElementTree as ET

KML_DIR = "data/export_kmz"
INPUT_GLOB = os.path.join(KML_DIR, "DJH*.kml")
OUT_PATH = "data/geojson/DJH_alle_JH.geojson"

KML_NS = {"kml": "http://www.opengis.net/kml/2.2"}

def _strip(s: str) -> str:
    return s.strip() if isinstance(s, str) else s

def parse_kml_file(path: str):
    tree = ET.parse(path)
    root = tree.getroot()

    features = []

    # Find all Placemarks (any depth)
    for pm in root.findall(".//kml:Placemark", KML_NS):
        # Geometry: Point only (as in your example)
        pt = pm.find(".//kml:Point/kml:coordinates", KML_NS)
        if pt is None or pt.text is None:
            continue

        coord_txt = _strip(pt.text)
        # KML coords: "lon,lat" or "lon,lat,alt"
        parts = [p for p in coord_txt.replace("\n", " ").split() if p]
        # Some KMLs may have multiple coordinate tuples; take first for Point
        lonlatalt = parts[0].split(",")
        if len(lonlatalt) < 2:
            continue

        try:
            lon = float(lonlatalt[0])
            lat = float(lonlatalt[1])
        except ValueError:
            continue

        props = {}

        # Optional name element
        name_el = pm.find("kml:name", KML_NS)
        if name_el is not None and name_el.text:
            props["name"] = _strip(name_el.text)

        # ExtendedData / SchemaData / SimpleData
        for sd in pm.findall(".//kml:ExtendedData//kml:SimpleData", KML_NS):
            key = sd.get("name")
            val = _strip(sd.text) if sd.text is not None else ""
            if key:
                props[key] = val

        # Optional: try to cast common numeric fields
        if "nr" in props:
            try:
                props["nr"] = int(str(props["nr"]).strip())
            except ValueError:
                pass

        feature = {
            "type": "Feature",
            "geometry": {"type": "Point", "coordinates": [lon, lat]},
            "properties": props
        }
        features.append(feature)

    return features

def main():
    in_files = sorted(glob.glob(INPUT_GLOB))
    if not in_files:
        raise SystemExit(f"Keine Dateien gefunden: {INPUT_GLOB}")

    all_features = []
    for fp in in_files:
        all_features.extend(parse_kml_file(fp))

    geojson = {"type": "FeatureCollection", "features": all_features}

    os.makedirs(os.path.dirname(OUT_PATH), exist_ok=True)
    with open(OUT_PATH, "w", encoding="utf-8") as f:
        json.dump(geojson, f, ensure_ascii=False, indent=2)

    print(f"Wrote {len(all_features)} features -> {OUT_PATH}")

if __name__ == "__main__":
    main()
