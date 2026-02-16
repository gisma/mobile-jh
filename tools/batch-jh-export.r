# export_kmz_batch.R
# Batch-Export: KMZ (für Google My Maps) je Jugendherberge + POIs aus lokalem Geofabrik-PBF via osmium
# Folder-Version:
# - doc.kml enthält Folder: "JH" + je category_group ein Folder
# - dadurch: keine endlose Legende nach Einzel-Namen, sondern Layer/Folders
# - POIs ohne Namen werden konsequent entfernt
#
# Pakete:
# install.packages(c("sf","dplyr","stringr","glue","zip","tibble","xml2"))

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(glue)
  library(zip)
  library(tibble)
  library(xml2)
})

# ----------------------------
# Einstellungen
# ----------------------------
cfg <- list(
  gpkg      = "~/Desktop/DJH/mobile-jh/mobile-jh/data/djh-adressen_geocoded.gpkg",
  layer     = NULL,  # oder "dein_layername"
  pbf       = path.expand(Sys.getenv("JH_PBF", unset = "~/Desktop/DJH/germany-latest.osm.pbf")),
  cache_dir = "cache_pbf",
  out_dir   = "~/Desktop/DJH/mobile-jh/mobile-jh/data/export_kmz",
  theme     = "master",
  radius_m  = 2500,
  force     = FALSE
)

dir.create(cfg$out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(cfg$cache_dir, showWarnings = FALSE, recursive = TRUE)

cat("WD: ", getwd(), "\n")
cat("out_dir raw: ", cfg$out_dir, "\n")
cat("out_dir exists: ", dir.exists(cfg$out_dir), "\n")
cat("out_dir write access (0=OK): ", file.access(cfg$out_dir, 2), "\n")

# harter Schreibtest
testfile <- file.path(cfg$out_dir, paste0(".__write_test__", Sys.getpid(), ".tmp"))
ok <- tryCatch({ writeLines("x", testfile); TRUE }, error=function(e) e)
cat("write test: ", if (identical(ok, TRUE)) "OK" else paste("FAIL:", conditionMessage(ok)), "\n")
if (file.exists(testfile)) unlink(testfile)

tmp_test <- file.path(cfg$out_dir, ".__write_test__.tmp")
ok_write <- tryCatch({ writeLines("x", tmp_test); TRUE }, error = function(e) FALSE)
if (file.exists(tmp_test)) unlink(tmp_test)
if (!ok_write) stop("Out-Dir nicht beschreibbar: ", normalizePath(cfg$out_dir, mustWork = FALSE))

# -----------------------------
# Theme Profiles
# -----------------------------
theme_profiles <- list(
  bildung_erweitert = tibble::tribble(
    ~key,         ~value,               ~label,
    "amenity",     "library",            "Bibliothek",
    "amenity",     "school",             "Schule",
    "amenity",     "college",            "Hochschule",
    "amenity",     "university",         "Universität",
    "amenity",     "arts_centre",        "Kunstzentrum",
    "amenity",     "community_centre",   "Gemeindezentrum",
    "tourism",     "museum",             "Museum",
    "tourism",     "gallery",            "Galerie",
    "tourism",     "attraction",         "Attraktion",
    "tourism",     "information",        "Besucherinformation",
    "tourism",     "theme_park",         "Freizeitpark",
    "historic",    NA,                   "Historisches Objekt (alle)",
    "man_made",    "observatory",        "Sternwarte",
    "amenity",     "theatre",            "Theater"
  ),
  natur_paedagogik = tibble::tribble(
    ~key,        ~value,                  ~label,
    "boundary",   "protected_area",        "Schutzgebiet",
    "leisure",    "nature_reserve",        "Naturschutzgebiet",
    "tourism",    "information",           "Besucherinformation",
    "amenity",    "research_institute",    "Forschungsinstitut",
    "natural",    NA,                      "Natur-Feature (alle)",
    "tourism",    "viewpoint",             "Aussichtspunkt",
    "leisure",    "park",                  "Park",
    "amenity",    "public_bookcase",       "Bücherschrank"
  ),
  kinder = tibble::tribble(
    ~key,       ~value,            ~label,
    "leisure",   "playground",      "Spielplatz",
    "amenity",   "kindergarten",    "Kindergarten",
    "amenity",   "childcare",       "Kinderbetreuung",
    "leisure",   "sports_centre",   "Sportzentrum",
    "leisure",   "swimming_pool",   "Schwimmbad",
    "amenity",   "youth_centre",    "Jugendzentrum"
  ),
  kultur = tibble::tribble(
    ~key,        ~value,                 ~label,
    "historic",   "castle",               "Burg",
    "historic",   "ruins",                "Ruine",
    "historic",   "memorial",             "Denkmal",
    "historic",   "monument",             "Monument",
    "historic",   "archaeological_site",  "Archäologie",
    "tourism",    "museum",               "Museum",
    "amenity",    "theatre",              "Theater",
    "amenity",    "cinema",               "Kino"
  ),
  outdoor = tibble::tribble(
    ~key,        ~value,            ~label,
    "route",      "hiking",          "Wanderroute",
    "route",      "bicycle",         "Radroute",
    "leisure",    "fitness_station", "Fitnessstation",
    "leisure",    "sports_centre",   "Sportzentrum",
    "leisure",    "marina",          "Yachthafen",
    "leisure",    "water_park",      "Wasserpark",
    "tourism",    "viewpoint",       "Aussichtspunkt"
  ),
  wissenschaft = tibble::tribble(
    ~key,         ~value,               ~label,
    "amenity",     "research_institute", "Forschungsinstitut",
    "man_made",    "observatory",        "Sternwarte",
    "amenity",     "planetarium",        "Planetarium",
    "tourism",     "museum",             "Museum"
  )
)

theme_profiles$master <- dplyr::bind_rows(theme_profiles[names(theme_profiles) != "master"]) %>%
  distinct(key, value, label)

# -----------------------------
# Helpers
# -----------------------------
normalize_nr <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_extract(x, "\\d+")
  suppressWarnings(as.integer(x))
}

pick_first_col <- function(df, candidates) {
  cn <- intersect(candidates, names(df))
  if (length(cn) == 0) return(rep(NA_character_, nrow(df)))
  out <- df[[cn[1]]]
  if (length(cn) > 1) {
    for (k in cn[-1]) out <- ifelse(is.na(out) | out == "", df[[k]], out)
  }
  as.character(out)
}

ensure_geometry_name <- function(x, target = "geometry") {
  stopifnot(inherits(x, "sf"))
  g <- attr(x, "sf_column")
  if (is.null(g) || !nzchar(g) || !(g %in% names(x))) {
    stop("SF-Objekt hat keine gültige Geometriespalte (attr 'sf_column' fehlt/ungültig).")
  }
  if (g != target) {
    names(x)[names(x) == g] <- target
    attr(x, "sf_column") <- target
  }
  x
}

empty_sf <- function(crs = 4326) {
  sf::st_sf(geometry = sf::st_sfc(crs = crs))
}

bbox_key <- function(bb) {
  # WICHTIG: feste Nachkommastellen, keine signifikanten Stellen.
  # Sonst kollidieren Cache-Keys bei ähnlichen BBoxen.
  paste0(
    "bb",
    sprintf("%.7f", as.numeric(bb["xmin"])), "_",
    sprintf("%.7f", as.numeric(bb["ymin"])), "_",
    sprintf("%.7f", as.numeric(bb["xmax"])), "_",
    sprintf("%.7f", as.numeric(bb["ymax"]))
  )
}


# -----------------------------
# osmium helpers
# -----------------------------
run_osmium <- function(args) {
  out <- system2("osmium", args = args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    stop("osmium failed (exit ", status, "):\n", paste(out, collapse = "\n"))
  }
  out
}

# -----------------------------
# POI Grabber (robust: Cache heilen + Retry)
# -----------------------------
grab_pois_from_pbf <- function(pbf_path, bb, theme, cache_dir, force = FALSE) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(pbf_path)) stop("PBF nicht gefunden: ", pbf_path)
  if (!theme %in% names(theme_profiles)) stop("Theme unbekannt: ", theme)
  
  tags <- theme_profiles[[theme]] %>% distinct(key, value, label)
  
  bb_tag <- bbox_key(bb)
  bbox_pbf  <- file.path(cache_dir, paste0(bb_tag, "_bbox.osm.pbf"))
  filt_pbf  <- file.path(cache_dir, paste0(bb_tag, "_", theme, "_filt.osm.pbf"))
  out_geojs <- file.path(cache_dir, paste0(bb_tag, "_", theme, "_pois.geojson"))
  
  is_bad_pbf <- function(f) (!file.exists(f)) || (is.na(file.size(f))) || (file.size(f) < 256)
  
  if (isTRUE(force)) {
    for (f in c(bbox_pbf, filt_pbf, out_geojs)) if (file.exists(f)) unlink(f, force = TRUE)
  }
  
  # build filters once
  filters <- c()
  for (i in seq_len(nrow(tags))) {
    k <- tags$key[i]; v <- tags$value[i]
    if (is.na(v) || v == "") filters <- c(filters, k) else filters <- c(filters, paste0(k, "=", v))
  }
  filters <- unique(filters)
  
  for (attempt in 1:2) {
    
    # bbox extract (immer neu wenn kaputt)
    if (is_bad_pbf(bbox_pbf)) {
      if (file.exists(bbox_pbf)) unlink(bbox_pbf, force = TRUE)
      bb_str <- paste(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"], sep = ",")
      err <- tryCatch({
        run_osmium(c("extract", "-b", bb_str, "-o", bbox_pbf, pbf_path))
        NULL
      }, error = function(e) e)
      if (!is.null(err) || is_bad_pbf(bbox_pbf)) {
        if (attempt == 1) {
          if (file.exists(bbox_pbf)) unlink(bbox_pbf, force = TRUE)
          next
        }
        stop("bbox extract erzeugte leere/kaputte PBF: ", bbox_pbf)
      }
    }
    
    # tags-filter (wenn kaputt/fehlt)
    if (is_bad_pbf(filt_pbf)) {
      if (file.exists(filt_pbf)) unlink(filt_pbf, force = TRUE)
      err <- tryCatch({
        run_osmium(c("tags-filter", "--overwrite", "-o", filt_pbf, bbox_pbf, filters))
        NULL
      }, error = function(e) e)
      
      # typischer Fehler: "blob contains no data..." => bbox_pbf korrupt -> kill + retry
      if (!is.null(err) || is_bad_pbf(filt_pbf)) {
        if (attempt == 1) {
          if (file.exists(out_geojs)) unlink(out_geojs, force = TRUE)
          if (file.exists(filt_pbf)) unlink(filt_pbf, force = TRUE)
          if (file.exists(bbox_pbf)) unlink(bbox_pbf, force = TRUE)
          next
        }
        stop(err)
      }
    }
    
    # export geojson
    if (!file.exists(out_geojs)) {
      err <- tryCatch({
        run_osmium(c("export", "-f", "geojson", "-o", out_geojs, filt_pbf))
        NULL
      }, error = function(e) e)
      if (!is.null(err)) {
        if (attempt == 1) {
          if (file.exists(out_geojs)) unlink(out_geojs, force = TRUE)
          if (file.exists(filt_pbf)) unlink(filt_pbf, force = TRUE)
          if (file.exists(bbox_pbf)) unlink(bbox_pbf, force = TRUE)
          next
        }
        stop(err)
      }
    }
    
    x <- tryCatch(sf::read_sf(out_geojs), error = function(e) NULL)
    if (is.null(x) || !inherits(x, "sf") || nrow(x) == 0 || is.null(sf::st_geometry(x))) {
      if (attempt == 1) {
        if (file.exists(out_geojs)) unlink(out_geojs, force = TRUE)
        if (file.exists(filt_pbf)) unlink(filt_pbf, force = TRUE)
        if (file.exists(bbox_pbf)) unlink(bbox_pbf, force = TRUE)
        next
      }
      return(empty_sf(4326))
    }
    
    x <- x %>% filter(sf::st_geometry_type(geometry) %in% c("POINT", "MULTIPOINT"))
    if (nrow(x) == 0) return(empty_sf(4326))
    x <- sf::st_cast(x, "POINT", warn = FALSE)
    
    # Kategorie mappen
    x$category <- NA_character_
    for (i in seq_len(nrow(tags))) {
      k <- tags$key[i]; v <- tags$value[i]; lab <- tags$label[i]
      if (!k %in% names(x)) next
      if (is.na(v) || v == "") {
        hit <- !is.na(x[[k]]) & x[[k]] != ""
      } else {
        hit <- !is.na(x[[k]]) & x[[k]] == v
      }
      x$category[is.na(x$category) & hit] <- lab
    }
    
    x$poi_name <- pick_first_col(x, c("name", "name:de", "name:en", "Name", "NAME"))
    x <- ensure_geometry_name(x, "geometry")
    return(x)
  }
  
  empty_sf(4326)
}

# ----------------------------
# Kategorie-Gruppierung (My Maps übersichtlich)
# ----------------------------
category_group <- function(cat) {
  case_when(
    cat %in% c("Bibliothek","Schule","Hochschule","Universität","Kunstzentrum","Gemeindezentrum") ~ "Bildungsinfrastruktur",
    cat %in% c("Kindergarten","Kinderbetreuung","Jugendzentrum","Spielplatz") ~ "Kinder & Jugend",
    cat %in% c("Museum","Galerie","Theater","Kino","Archäologie","Burg","Ruine","Denkmal","Monument","Attraktion") ~ "Kultur & Geschichte",
    cat %in% c("Park","Naturschutzgebiet","Schutzgebiet","Natur-Feature (alle)","Aussichtspunkt") ~ "Natur & Landschaft",
    cat %in% c("Sportzentrum","Schwimmbad","Fitnessstation","Wasserpark","Yachthafen") ~ "Freizeit & Sport",
    cat %in% c("Wanderroute","Radroute") ~ "Routen",
    cat %in% c("Forschungsinstitut","Sternwarte","Planetarium") ~ "Wissenschaft",
    cat %in% c("Besucherinformation") ~ "Information",
    TRUE ~ NA_character_
  )
}

# ----------------------------
# KML "Beschreibung" (My Maps Info-Fenster)
# ----------------------------
mk_desc <- function(df) {
  safe <- function(x) ifelse(is.na(x) | x == "", "", x)
  lines <- c(
    glue("<b>Kategorie:</b> {safe(df$category_group)} — {safe(df$category)}"),
    glue("<b>Distanz:</b> {safe(df$dist_m)} m"),
    if ("website" %in% names(df)) glue("<b>Website:</b> {safe(df$website)}") else "",
    if ("wikipedia" %in% names(df)) glue("<b>Wikipedia:</b> {safe(df$wikipedia)}") else "",
    if ("phone" %in% names(df)) glue("<b>Telefon:</b> {safe(df$phone)}") else "",
    if (all(c("addr:street","addr:housenumber","addr:postcode","addr:city") %in% names(df))) {
      glue("<b>Adresse:</b> {safe(df$`addr:street`)} {safe(df$`addr:housenumber`)}, {safe(df$`addr:postcode`)} {safe(df$`addr:city`)}")
    } else ""
  )
  paste(lines[lines != ""], collapse = "<br>")
}

# ----------------------------
# Export: eine JH -> KMZ (Folder doc.kml)
# ----------------------------
kml_placemarks_from_sf <- function(x_sf, tmpdir) {
  if (!inherits(x_sf, "sf") || nrow(x_sf) == 0) return(list())
  x_sf <- ensure_geometry_name(x_sf, "geometry")
  if (!st_is_longlat(x_sf)) x_sf <- st_transform(x_sf, 4326)
  
  # sf->KML (temporär) und daraus Placemark-Knoten extrahieren
  kml_path <- file.path(tmpdir, paste0(".__part__", Sys.getpid(), "_", sample.int(1e9,1), ".kml"))
  st_write(x_sf, kml_path, driver = "KML", quiet = TRUE, append = FALSE)
  
  doc <- read_xml(kml_path)
  unlink(kml_path)
  
  # namespace-robust:
  pms <- xml2::xml_find_all(doc, ".//*[local-name()='Placemark']")
  
  clone_node <- function(node) {
    # node -> string -> neues mini-doc -> root node zurück
    xml2::xml_root(xml2::read_xml(as.character(node)))
  }
  
  pms_cloned <- lapply(pms, clone_node)
  
  # WICHTIG: Rückgabe!
  pms_cloned
}


write_folder_kml <- function(folders_named_placemarks, out_kml_path) {
  # Basisskelett KML
  base <- read_xml(
    '<?xml version="1.0" encoding="UTF-8"?>
     <kml xmlns="http://www.opengis.net/kml/2.2">
       <Document>
         <name>doc</name>
       </Document>
     </kml>'
  )
  
  doc_node <- xml_find_first(base, ".//*[local-name()='Document']")
  
  for (nm in names(folders_named_placemarks)) {
    folder_node <- xml2::xml_add_child(doc_node, "Folder")
    xml2::xml_add_child(folder_node, "name", nm)
    
    pms <- folders_named_placemarks[[nm]]
    if (length(pms) > 0) {
      for (pm in pms) {
        xml2::xml_add_child(folder_node, .value = pm)
      }
    }
  }
  
  write_xml(base, out_kml_path, options = "format")
}

export_one_jh_kmz <- function(jh_row, pois_sf, out_kmz) {
  stopifnot(inherits(jh_row, "sf"))
  jh_row <- ensure_geometry_name(jh_row, "geometry")
  
  out_kmz <- normalizePath(out_kmz, mustWork = FALSE)
  out_dir <- dirname(out_kmz)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (file.access(out_dir, 2) != 0) stop("Output-Ordner nicht beschreibbar: ", out_dir)
  
  if (file.exists(out_kmz)) {
    try(unlink(out_kmz, force = TRUE), silent = TRUE)
    if (file.exists(out_kmz)) stop("Ziel-KMZ existiert und kann nicht gelöscht werden: ", out_kmz)
  }
  
  tmpdir <- tempfile("kmz_")
  dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
  
  # --- JH sicherstellen: name + Description
  if (!("name" %in% names(jh_row))) jh_row$name <- "JH"
  if (!("Description" %in% names(jh_row))) jh_row$Description <- ""
  
  # --- POIs: falls vorhanden, name + Description sicherstellen
  pois_ok <- inherits(pois_sf, "sf") && nrow(pois_sf) > 0 && !is.null(st_geometry(pois_sf))
  if (pois_ok) {
    pois_sf <- ensure_geometry_name(pois_sf, "geometry")
    if (!("name" %in% names(pois_sf))) pois_sf$name <- pick_first_col(pois_sf, c("poi_name","name"))
    if (!("Description" %in% names(pois_sf))) pois_sf$Description <- ""
  }
  
  # --- Folder-Inhalte vorbereiten (Placemark-Liste je Folder)
  folders <- list()
  
  # Folder: JH (ein Placemark)  ---- WICHTIG: hier die Adressfelder mitgeben
  folders[["JH"]] <- kml_placemarks_from_sf(
    jh_row %>% select(any_of(c(
      "name","Description",
      "nr","jh_name","ort","strasse","plz","telefon","address","bundesland",
      "geometry"
    ))),
    tmpdir
  )
  
  # Folder: POI-Gruppen
  if (pois_ok && ("category_group" %in% names(pois_sf))) {
    grps <- sort(unique(pois_sf$category_group))
    grps <- grps[!is.na(grps) & grps != ""]
    for (g in grps) {
      xg <- pois_sf %>% filter(category_group == g)
      
      # harte Filterung gegen "Unbenannt"
      xg$name <- str_trim(as.character(xg$name))
      xg <- xg %>% filter(!is.na(name), name != "", !grepl("^\\s*$", name))
      
      if (nrow(xg) == 0) next
      
      # schlank halten
      xg <- xg %>% select(any_of(c(
        "name","Description","category_group","category","dist_m",
        "website","wikipedia","phone",
        "addr:street","addr:housenumber","addr:postcode","addr:city",
        "geometry"
      )))
      folders[[g]] <- kml_placemarks_from_sf(xg, tmpdir)
    }
  }
  
  # doc.kml schreiben (Folder)
  doc_kml <- file.path(tmpdir, "doc.kml")
  write_folder_kml(folders, doc_kml)
  
  # zip -> temp kmz -> copy
  tmp_kmz <- tempfile(pattern = "jh_", tmpdir = tempdir(), fileext = ".kmz")
  if (file.exists(tmp_kmz)) unlink(tmp_kmz, force = TRUE)
  
  zip::zip(zipfile = tmp_kmz, files = "doc.kml", root = tmpdir)
  
  ok <- file.copy(tmp_kmz, out_kmz, overwrite = TRUE)
  unlink(tmp_kmz, force = TRUE)
  unlink(tmpdir, recursive = TRUE, force = TRUE)
  
  if (!ok) stop("Konnte KMZ nicht ins Ziel schreiben: ", out_kmz)
  out_kmz
}


# ----------------------------
# Load JH
# ----------------------------
read_jh <- function(gpkg, layer = NULL) {
  x <- if (is.null(layer)) st_read(gpkg, quiet = TRUE) else st_read(gpkg, layer = layer, quiet = TRUE)
  x <- ensure_geometry_name(x, "geometry")
  if (!st_is_longlat(x)) x <- st_transform(x, 4326)
  
  if (!("nr" %in% names(x))) stop("Spalte 'nr' fehlt.")
  x$nr <- normalize_nr(x$nr)
  
  # Name/Ort wie bisher
  x$jh_name <- pick_first_col(x, c("jh_name","name","name.x","Name","NAME"))
  x$ort     <- pick_first_col(x, c("ort","city","addr:city","Ort","ORT"))
  
  # NEU: Adresse/Telefon/Bundesland (inkl. typische Varianten)
  x$strasse    <- pick_first_col(x, c("strasse","straße","street","Strasse","Straße","STRASSE","STREET"))
  x$plz        <- pick_first_col(x, c("plz","postcode","zip","PLZ","POSTCODE","ZIP"))
  x$telefon    <- pick_first_col(x, c("telefon","phone","tel","Telefon","PHONE","TEL"))
  x$address    <- pick_first_col(x, c("address","adresse","Address","Adresse","ADDRESS","ADRESSE"))
  x$bundesland <- pick_first_col(x, c("bundesland","state","Bundesland","STATE","BL"))
  
  x <- x %>% filter(!is.na(nr))
  x
}


jh_sf <- read_jh(cfg$gpkg, cfg$layer)

# Gesamtübersicht aller JH als KML (ALLE FELDER übernehmen)
all_jh_kml <- file.path(cfg$out_dir, "DJH_alle_JH.kml")

jh_sf_kml <- ensure_geometry_name(jh_sf, "geometry")

# KML/OGR: nur atomare Datentypen -> nicht-atomare Spalten nach character wandeln
is_atomic_col <- function(v) {
  is.null(dim(v)) && (is.character(v) || is.numeric(v) || is.integer(v) || is.logical(v) || is.factor(v))
}

for (nm in names(jh_sf_kml)) {
  if (nm == "geometry") next
  v <- jh_sf_kml[[nm]]
  
  # factors -> character
  if (is.factor(v)) {
    jh_sf_kml[[nm]] <- as.character(v)
    next
  }
  
  # atomar ok -> lassen
  if (is_atomic_col(v)) next
  
  # alles andere (list, matrix, POSIXlt, etc.) -> in character serialisieren
  jh_sf_kml[[nm]] <- vapply(seq_len(nrow(jh_sf_kml)), function(i) {
    x <- v[i]
    if (inherits(x, "POSIXt")) return(format(as.POSIXct(x), tz = "UTC", usetz = TRUE))
    if (is.list(x)) {
      # Listelemente stabil zu Text
      return(paste(unlist(x), collapse = "; "))
    }
    # Fallback
    paste(as.character(x), collapse = "; ")
  }, character(1))
}

# optional: falls 'name' fehlt, setze einen sinnvollen Label-String
if (!("name" %in% names(jh_sf_kml))) {
  jh_sf_kml$name <- if ("jh_name" %in% names(jh_sf_kml)) jh_sf_kml$jh_name else "JH"
}

# optional: falls 'Description' fehlt, leer anlegen (My Maps Popup kann das nutzen)
if (!("Description" %in% names(jh_sf_kml))) {
  jh_sf_kml$Description <- ""
}

# Schreiben: keine Attribut-Reduktion mehr
sf::st_write(
  jh_sf_kml,
  all_jh_kml,
  driver = "KML",
  quiet = TRUE,
  append = FALSE,
  delete_dsn = TRUE
)

message("JH gesamt KML (alle Felder): ", all_jh_kml)


# ----------------------------
# Batch pro JH
# ----------------------------
if (Sys.which("osmium") == "") stop("osmium nicht gefunden. Prüfe PATH / Installation.")
if (!file.exists(cfg$pbf)) stop("PBF nicht gefunden: ", cfg$pbf)

for (i in seq_len(nrow(jh_sf))) {
  jh <- ensure_geometry_name(jh_sf[i, ], "geometry")
  
  nr <- jh$nr
  nm <- ifelse(is.na(jh$jh_name) | jh$jh_name == "", paste0("nr", nr), jh$jh_name)
  safe_nm <- str_replace_all(nm, "[^A-Za-z0-9_-]+", "_")
  
  # Buffer bbox
  jh_utm  <- st_transform(jh, 25832)
  buf_utm <- st_buffer(jh_utm, dist = cfg$radius_m)
  buf_wgs <- st_transform(buf_utm, 4326)
  bb <- st_bbox(buf_wgs)
  
  # POIs holen
  pois <- grab_pois_from_pbf(
    pbf_path  = cfg$pbf,
    bb        = bb,
    theme     = cfg$theme,
    cache_dir = cfg$cache_dir,
    force     = cfg$force
  )
  
  if (inherits(pois, "sf") && nrow(pois) > 0) {
    pois <- ensure_geometry_name(pois, "geometry")
    
    # nur klassifizierte Kategorien
    pois <- pois %>% filter(!is.na(category) & category != "")
    
    # Distanz + Name normalisieren
    pois_utm <- st_transform(pois, 25832)
    pois$dist_m <- as.integer(round(as.numeric(st_distance(pois_utm, jh_utm)), 0))
    
    pois$poi_name <- pick_first_col(pois, c("poi_name","name","name:de","name:en","Name","NAME"))
    pois$poi_name <- str_trim(pois$poi_name)
    
    # UNNAMED RAUS (hart)
    pois <- pois %>% filter(!is.na(poi_name), poi_name != "", !grepl("^\\s*$", poi_name))
    
    # Gruppierung
    pois$category_group <- category_group(pois$category)
    pois <- pois %>% filter(!is.na(category_group), category_group != "")
    
    # Description
    df0 <- st_drop_geometry(pois)
    desc <- apply(df0, 1, function(row) mk_desc(as.data.frame(as.list(row), stringsAsFactors = FALSE)))
    pois$Description <- desc
    
    # KML label
    pois$name <- pois$poi_name
  } else {
    pois <- empty_sf(4326)
  }
  
  # JH Label/Description
  # JH Label/Description (jetzt inkl. Adresse/Telefon/Bundesland)
  safe <- function(x) ifelse(is.na(x) | x == "", "", as.character(x))
  
  jh$name <- paste0("JH ", nr, " — ", nm)
  
  addr_line <- ""
  if (safe(jh$strasse) != "" || safe(jh$plz) != "") {
    addr_line <- glue("<b>Adresse:</b> {safe(jh$strasse)} {safe(jh$plz)} {safe(jh$ort)}")
  } else if (safe(jh$address) != "") {
    addr_line <- glue("<b>Adresse:</b> {safe(jh$address)}")
  }
  
  tel_line <- if (safe(jh$telefon) != "") glue("<b>Telefon:</b> {safe(jh$telefon)}") else ""
  bl_line  <- if (safe(jh$bundesland) != "") glue("<b>Bundesland:</b> {safe(jh$bundesland)}") else ""
  
  jh$Description <- glue(
    "<b>JH Nr:</b> {nr}<br>",
    "<b>Name:</b> {nm}<br>",
    if (safe(jh$ort) != "") glue("<b>Ort:</b> {safe(jh$ort)}<br>") else "",
    if (addr_line != "") glue("{addr_line}<br>") else "",
    if (tel_line  != "") glue("{tel_line}<br>") else "",
    if (bl_line   != "") glue("{bl_line}") else ""
  )
  
  out_kmz <- file.path(cfg$out_dir, glue("JH_{nr}_{cfg$theme}.kmz"))
  
  export_one_jh_kmz(
    jh %>% select(any_of(c(
      "name","Description",
      "nr","jh_name","ort","strasse","plz","telefon","address","bundesland",
      "geometry"
    ))),
    pois,
    out_kmz
  )
  
  
  message("OK: ", out_kmz, " (POIs: ", ifelse(inherits(pois, "sf"), nrow(pois), 0), ")")
}

message("Fertig. Import in Google My Maps: Karte erstellen -> Import -> KMZ wählen.")
