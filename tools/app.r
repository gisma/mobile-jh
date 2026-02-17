# app.R — JH Grabber (Shiny) — stabil (osmium 1.16), lokale PBF, Farben+Icons+Filter+Export
# - Auswahlkarte: Jugendherbergen (farben nach Bundesland, falls vorhanden)
# - Klick auf JH setzt Nr (und schreibt in Eingabefeld)
# - Analyse: POIs aus lokaler Geofabrik-PBF per osmium extract + tags-filter + export (GeoJSON)
# - Ergebnis: Karte mit Basemap-Auswahl + Cluster (nur im "alles"-Modus), Gruppenfilter/Highlight über Tabellen
# - Entfernt POIs ohne Namen (konsequent)
# - Exporte: GPKG (POIs + JH + Buffer) + Summary CSV + HTML (Karte+Tabellen)

suppressPackageStartupMessages({
  library(shiny)
  library(sf)
  library(dplyr)
  library(stringr)
  library(leaflet)
  library(DT)
  library(htmltools)
  library(htmlwidgets)
  library(tibble)
})

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
# Helpers (robust)
# -----------------------------
normalize_nr <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_extract(x, "\\d+")
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

mk_colors <- function(n) {
  if (n <= 0) character(0) else grDevices::hcl.colors(n, palette = "Dark 3")
}

# Gruppenlogik (Kategorien -> Gruppen)
category_to_group <- function(cat) {
  cat <- as.character(cat)
  dplyr::case_when(
    cat %in% c("Besucherinformation") ~ "Info",
    cat %in% c("Museum","Galerie","Theater","Kino","Archäologie","Burg","Ruine","Denkmal","Monument","Historisches Objekt (alle)") ~ "Kultur & Geschichte",
    cat %in% c("Bibliothek","Schule","Hochschule","Universität","Kunstzentrum","Gemeindezentrum","Kindergarten","Kinderbetreuung","Jugendzentrum") ~ "Bildungsinfrastruktur",
    cat %in% c("Sportzentrum","Schwimmbad","Fitnessstation","Wanderroute","Radroute","Yachthafen","Wasserpark") ~ "Sport & Outdoor",
    cat %in% c("Park","Naturschutzgebiet","Schutzgebiet","Natur-Feature (alle)","Aussichtspunkt") ~ "Natur",
    TRUE ~ "Sonstiges"
  )
}

# -----------------------------
# Online Marker PNGs (Leaflet-color-markers) — keine lokalen Assets nötig
# (raw.githack ist stabiler als raw.githubusercontent für manche Setups)
# -----------------------------
marker_png <- function(color = "blue") {
  base <- "https://raw.githack.com/pointhi/leaflet-color-markers/master/img"
  list(
    iconUrl = paste0(base, "/marker-icon-2x-", color, ".png"),
    shadowUrl = paste0(base, "/marker-shadow.png")
  )
}

make_png_icon <- function(color = "blue") {
  u <- marker_png(color)
  
  leaflet::makeIcon(
    iconUrl = u$iconUrl,
    iconWidth  = 25,
    iconHeight = 41,
    iconAnchorX = 12,
    iconAnchorY = 41,
    popupAnchorX = 1,
    popupAnchorY = -34,
    shadowUrl = u$shadowUrl,
    shadowWidth  = 41,
    shadowHeight = 41,
    shadowAnchorX = 12,
    shadowAnchorY = 41
  )
}


# Legend HTML (JH + Gruppen)
legend_html_groups <- function(df_counts, group_colors, jh_label) {
  df_counts <- df_counts %>% arrange(desc(n), group)
  items <- lapply(seq_len(nrow(df_counts)), function(i) {
    g <- df_counts$group[i]
    n <- df_counts$n[i]
    col <- group_colors[[g]]
    if (is.null(col) || !nzchar(col)) col <- "blue"
    icon_url <- marker_png(col)$iconUrl
    
    tags$div(style="display:flex;align-items:center;gap:8px;margin:2px 0;",
             tags$img(src = icon_url, style="width:14px;height:22px;"),
             tags$span(style="white-space:nowrap;font-size:12px;",
                       htmlEscape(g), ": ", n
             )
    )
  })
  
  as.character(tags$div(
    style="background:rgba(255,255,255,0.95);padding:8px 10px;border:1px solid #ddd;border-radius:8px;max-height:260px;overflow:auto;",
    tags$div(style="font-weight:600;margin-bottom:6px;font-size:12px;", "Legende (Gruppe — Anzahl)"),
    tags$div(style="font-size:12px;margin-bottom:6px;",
             tags$b("JH: "), htmlEscape(jh_label)
    ),
    items
  ))
}

# -----------------------------
# osmium helpers (osmium 1.16.0: keine --with-refs, keine --id-list)
# -----------------------------
run_osmium <- function(args) {
  out <- system2("osmium", args = args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    stop("osmium failed (exit ", status, "):\n", paste(out, collapse = "\n"))
  }
  out
}

bbox_key <- function(bb) {
  paste0(
    "bb",
    format(bb["xmin"], digits = 7), "_",
    format(bb["ymin"], digits = 7), "_",
    format(bb["xmax"], digits = 7), "_",
    format(bb["ymax"], digits = 7)
  )
}

empty_sf <- function(crs = 4326) {
  sf::st_sf(geometry = sf::st_sfc(crs = crs))
}

grab_pois_from_pbf <- function(pbf_path, bb, theme, cache_dir, force = FALSE) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  tags <- theme_profiles[[theme]] %>% distinct(key, value, label)
  bb_tag <- bbox_key(bb)
  
  bbox_pbf  <- file.path(cache_dir, paste0(bb_tag, "_bbox.osm.pbf"))
  filt_pbf  <- file.path(cache_dir, paste0(bb_tag, "_", theme, "_filt.osm.pbf"))
  out_geojs <- file.path(cache_dir, paste0(bb_tag, "_", theme, "_pois.geojson"))
  
  if (isTRUE(force)) {
    for (f in c(bbox_pbf, filt_pbf, out_geojs)) if (file.exists(f)) unlink(f, force = TRUE)
  }
  
  # 1) bbox extract
  if (!file.exists(bbox_pbf)) {
    bb_str <- paste(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"], sep = ",")
    run_osmium(c("extract", "-b", bb_str, "-o", bbox_pbf, pbf_path))
  }
  
  # 2) tags-filter
  if (!file.exists(filt_pbf)) {
    filters <- c()
    for (i in seq_len(nrow(tags))) {
      k <- tags$key[i]
      v <- tags$value[i]
      if (is.na(v) || v == "") filters <- c(filters, k) else filters <- c(filters, paste0(k, "=", v))
    }
    filters <- unique(filters)
    # osmium 1.16: tags-filter INPUT FILTERS... ; -o OUTPUT
    run_osmium(c("tags-filter", "--overwrite", "-o", filt_pbf, bbox_pbf, filters))
  }
  
  # 3) export geojson
  if (!file.exists(out_geojs)) {
    run_osmium(c("export", "-f", "geojson", "-o", out_geojs, filt_pbf))
  }
  
  x <- tryCatch(sf::read_sf(out_geojs), error = function(e) NULL)
  if (is.null(x) || nrow(x) == 0) return(empty_sf(4326))
  if (!inherits(x, "sf")) return(empty_sf(4326))
  if (is.null(sf::st_geometry(x))) return(empty_sf(4326))
  
  # nur Punkte
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
  
  # Name
  x$poi_name <- pick_first_col(x, c("name","name:de","name:en","Name","NAME"))
  x
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif; }
      .small { color:#555; font-size:12px; }
      .box { border:1px solid #ddd; border-radius:10px; padding:12px; margin:10px 0; }
      .h { font-weight:700; margin:6px 0 10px 0; }
      .leaflet-container { border-radius:10px; }
    "))
  ),
  titlePanel("JH Grabber (Shiny) — Klick auf JH → Analyse (lokale PBF)"),
  
  fluidRow(
    column(
      width = 3,
      div(class="box",
          div(class="h", "Inputs"),
          textInput("gpkg", "JH GPKG", value = "djh-adressen_geocoded.gpkg"),
          textInput("layer", "Layer (leer = default)", value = ""),
          textInput("pbf", "PBF-Pfad (lokal)", value = Sys.getenv("JH_PBF", unset = "~/Desktop/DJH/germany-latest.osm.pbf")),
          selectInput("theme", "Theme", choices = names(theme_profiles), selected = "master"),
          numericInput("radius", "Radius (m)", value = 2500, min = 200, max = 20000, step = 100),
          checkboxInput("force", "Cache ignorieren (neu rechnen)", value = FALSE),
          textInput("nr_manual", "Nr (optional, überschreibt Klick)", value = ""),
          actionButton("run", "Analyse für ausgewählte JH", class="btn-primary"),
          tags$hr(),
          downloadButton("dl_html", "Report herunterladen (HTML)"),
          downloadButton("dl_gpkg", "Export: POIs+JH+Buffer (GPKG)"),
          downloadButton("dl_csv",  "Export: Summary (CSV)"),
          tags$hr(),
          verbatimTextOutput("status"),
          tags$hr(),
          div(class="small",
              "Hinweis: cache_pbf/ wird im App-Verzeichnis angelegt. ",
              "Diese App nutzt osmium extract + osmium tags-filter (kein Overpass). ",
              "POIs ohne Namen werden entfernt."
          )
      )
    ),
    
    column(
      width = 9,
      div(class="box",
          div(class="h", "1) Auswahl: Jugendherbergen"),
          leafletOutput("map_jh", height = 420)
      ),
      div(class="box",
          div(class="h", "2) Ergebnis: POIs im Radius"),
          leafletOutput("map_res", height = 520),
          tags$hr(),
          div(class="h", "Übersicht nach Gruppen (klickbar → Filter/Highlight)"),
          DTOutput("tbl_sum"),
          tags$hr(),
          div(class="h", "POIs (Top je Gruppe, klickbar → Zoom/Popup + Filter)"),
          DTOutput("tbl_top")
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  app_dir <- normalizePath(getwd())
  cache_dir <- file.path(app_dir, "cache_pbf")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  
  sel_nr <- reactiveVal(NA_integer_)
  sel_group <- reactiveVal(NA_character_)  # Filter state
  
  # -------- JH Layer --------
  jh_sf <- reactive({
    req(file.exists(path.expand(input$gpkg)))
    lyr <- trimws(input$layer)
    
    x <- if (lyr == "") st_read(path.expand(input$gpkg), quiet = TRUE) else st_read(path.expand(input$gpkg), layer = lyr, quiet = TRUE)
    if (!st_is_longlat(x)) x <- st_transform(x, 4326)
    if (!("nr" %in% names(x))) stop("Spalte 'nr' fehlt im JH-Layer.")
    
    x$nr <- normalize_nr(x$nr)
    x$jh_name <- pick_first_col(x, c("jh_name","name","name.x","Name","NAME"))
    x$ort <- pick_first_col(x, c("ort","city","addr:city","Ort","ORT"))
    x$bundesland <- pick_first_col(x, c("bundesland","Bundesland","state","land","BL"))
    
    x <- x %>% filter(!is.na(nr))
    x
  })
  
  # -------- Auswahlkarte --------
  output$map_jh <- renderLeaflet({
    x <- jh_sf()
    has_bl <- any(!is.na(x$bundesland) & x$bundesland != "")
    
    if (has_bl) {
      bls <- sort(unique(x$bundesland[x$bundesland != "" & !is.na(x$bundesland)]))
      cols <- mk_colors(length(bls))
      pal_bl <- colorFactor(cols, domain = bls)
      col_vec <- ifelse(is.na(x$bundesland) | x$bundesland=="", "#2b8cbe", pal_bl(x$bundesland))
    } else {
      pal_bl <- NULL
      col_vec <- rep("#2b8cbe", nrow(x))
    }
    
    leaflet(x) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Karte") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellit") %>%
      addLayersControl(baseGroups = c("Karte","Satellit"), options = layersControlOptions(collapsed = TRUE)) %>%
      addCircleMarkers(
        radius = 5,
        stroke = TRUE, weight = 1,
        color = col_vec, fillOpacity = 0.9,
        layerId = ~as.character(nr),
        popup = ~paste0("<b>JH Nr: ", nr, "</b><br>", htmlEscape(jh_name), "<br>", htmlEscape(ort),
                        ifelse(!is.na(bundesland) & bundesland!="", paste0("<br><i>", htmlEscape(bundesland), "</i>"), ""))
      ) %>%
      {
        if (!is.null(pal_bl)) addLegend(., "bottomright", pal = pal_bl, values = ~bundesland, title="Bundesland", opacity = 1) else .
      }
  })
  
  observeEvent(input$map_jh_marker_click, {
    id <- input$map_jh_marker_click$id
    if (!is.null(id) && nzchar(id)) {
      sel_nr(as.integer(id))
      updateTextInput(session, "nr_manual", value = id)
    }
  })
  
  active_nr <- reactive({
    m <- normalize_nr(input$nr_manual)
    if (!is.na(m)) return(m)
    sel_nr()
  })
  
  # -------- Result store --------
  rv <- reactiveValues(
    jh = empty_sf(4326),
    buffer = empty_sf(4326),
    pois = empty_sf(4326),
    summary = data.frame(),
    top = data.frame(),
    group_colors = list()
  )
  
  # -------- Status --------
  output$status <- renderText({
    x <- tryCatch(jh_sf(), error = function(e) NULL)
    paste0(
      "App dir: ", app_dir, "\n",
      "Cache:   ", cache_dir, "\n",
      "osmium:  ", Sys.which("osmium"), "\n\n",
      "JH loaded: ", if (is.null(x)) "NO" else nrow(x), "\n",
      "Selected (click): ", ifelse(is.na(sel_nr()), "(none)", sel_nr()), "\n",
      "Active Nr: ", ifelse(is.na(active_nr()), "(none)", active_nr()), "\n",
      "Theme: ", input$theme, " | Radius: ", input$radius, " m\n",
      "PBF exists: ", file.exists(path.expand(input$pbf)), "\n",
      "Group filter: ", ifelse(is.na(sel_group()), "(none)", sel_group())
    )
  })
  
  # -------- Ergebnis-Karte init --------
  output$map_res <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group="Karte") %>%
      addProviderTiles(providers$Esri.WorldImagery, group="Satellit") %>%
      addLayersControl(
        baseGroups = c("Karte","Satellit"),
        overlayGroups = c("JH","Radius","POIs"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(lng = 10.0, lat = 51.0, zoom = 6)
  })
  
  # -------- Tabellen init --------
  output$tbl_sum <- renderDT({
    datatable(
      data.frame(group=character(0), n=integer(0), min_dist_m=integer(0), p50_dist_m=integer(0)),
      selection = "single",
      options = list(pageLength = 8, autoWidth = TRUE, dom = "tip", ordering = TRUE)
    )
  })
  
  output$tbl_top <- renderDT({
    datatable(
      data.frame(group=character(0), poi_name=character(0), dist_m=integer(0), category=character(0)),
      selection = "single",
      options = list(pageLength = 10, autoWidth = TRUE, dom = "tip", ordering = TRUE)
    )
  })
  
  # -------- Map draw (single function) --------
  draw_result <- function(group_filter = NA_character_, zoom_to_poi = NULL) {
    leafletProxy("map_res") %>% clearMarkers() %>% clearShapes() %>% clearControls()
    
    req(inherits(rv$jh, "sf"))
    req(inherits(rv$buffer, "sf"))
    
    jh_wgs  <- st_transform(rv$jh, 4326)
    buf_wgs <- st_transform(rv$buffer, 4326)
    bb <- st_bbox(buf_wgs)
    
    # Basemaps + overlay control
    leafletProxy("map_res") %>%
      addProviderTiles(providers$CartoDB.Positron, group="Karte") %>%
      addProviderTiles(providers$Esri.WorldImagery, group="Satellit") %>%
      addLayersControl(
        baseGroups = c("Karte","Satellit"),
        overlayGroups = c("JH","Radius","POIs"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addPolygons(data = buf_wgs, color = "#2b8cbe", weight = 2, fillOpacity = 0.06, group="Radius") %>%
      addMarkers(
        data = jh_wgs,
        icon = make_png_icon("red"),
        popup = ~paste0("<b>JH Nr. ", nr, "</b><br>", htmlEscape(jh_name), "<br>", htmlEscape(ort)),
        group="JH"
      ) %>%
      fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    
    pois <- rv$pois
    if (!inherits(pois, "sf") || nrow(pois) == 0) {
      jh_lab <- paste0(rv$jh$jh_name[1], " (Nr. ", rv$jh$nr[1], ")")
      leafletProxy("map_res") %>% addControl(
        html = as.character(tags$div(
          style="background:rgba(255,255,255,0.95);padding:8px 10px;border:1px solid #ddd;border-radius:8px;",
          tags$div(style="font-weight:600;margin-bottom:6px;font-size:12px;", "Legende"),
          tags$div(style="font-size:12px;", tags$b("JH: "), htmlEscape(jh_lab)),
          tags$div(style="font-size:12px;margin-top:6px;", "Keine POIs im Radius.")
        )),
        position = "topright"
      )
      return(invisible())
    }
    
    # Filter
    gf <- trimws(as.character(group_filter))
    is_filtered <- !is.na(gf) && nzchar(gf)
    if (is_filtered) {
      pois_f <- pois %>% filter(.data$group == gf)
      if (nrow(pois_f) > 0) pois <- pois_f
    }
    
    pois_wgs <- st_transform(pois, 4326)
    
    # Cluster nur im unfiltered Modus (sonst wirkt Filter „tot“)
    use_cluster <- !is_filtered
    cl_opt <- if (use_cluster) markerClusterOptions(chunkedLoading = TRUE, disableClusteringAtZoom = 16) else NULL
    
    # Pro Gruppe als eigene overlayGroup zeichnen (für späteres show/hide möglich)
    all_groups <- names(rv$group_colors)
    if (length(all_groups) == 0) {
      all_groups <- sort(unique(as.character(rv$pois$group)))
    }
    
    # LayerIds für „Top“-klick (Zoom/Popup)
    # (stabil: id aus osm_id falls vorhanden, sonst row-index)
    if (!("osm_id" %in% names(pois_wgs))) {
      pois_wgs$osm_id <- seq_len(nrow(pois_wgs))
    }
    pois_wgs$layer_id <- paste0("poi_", pois_wgs$osm_id)
    
    for (g in unique(pois_wgs$group)) {
      color <- rv$group_colors[[g]]
      if (is.null(color) || !nzchar(color)) color <- "blue"
      icon_g <- make_png_icon(color)
      
      dd <- pois_wgs %>% filter(.data$group == g)
      
      leafletProxy("map_res") %>%
        addMarkers(
          data = dd,
          icon = icon_g,
          layerId = ~layer_id,
          label = ~paste0(group, " — ", poi_name),
          popup = ~paste0(
            "<b>", htmlEscape(poi_name), "</b>",
            "<br>Gruppe: ", htmlEscape(group),
            "<br>Kategorie: ", htmlEscape(category),
            "<br>Distanz (m): ", dist_m
          ),
          clusterOptions = cl_opt,
          group = "POIs"
        )
    }
    
    # Legende: Farben + Counts
    jh_lab <- paste0(rv$jh$jh_name[1], " (Nr. ", rv$jh$nr[1], ")")
    if (is.data.frame(rv$summary) && nrow(rv$summary) > 0) {
      leafletProxy("map_res") %>%
        addControl(
          html = legend_html_groups(rv$summary, rv$group_colors, jh_label = jh_lab),
          position = "topright"
        )
    }
    
    # Optional: auf POI zoomen + Popup
    if (!is.null(zoom_to_poi) && nrow(zoom_to_poi) == 1) {
      p <- sf::st_transform(zoom_to_poi, 4326)
      xy <- sf::st_coordinates(p)[1,]
      popup_html <- paste0(
        "<b>", htmlEscape(p$poi_name[1]), "</b>",
        "<br>Gruppe: ", htmlEscape(p$group[1]),
        "<br>Kategorie: ", htmlEscape(p$category[1]),
        "<br>Distanz (m): ", p$dist_m[1]
      )
      leafletProxy("map_res") %>%
        flyTo(lng = xy[1], lat = xy[2], zoom = 16) %>%
        clearPopups() %>%
        addPopups(lng = xy[1], lat = xy[2], popup = popup_html)
    }
    
    invisible()
  }
  
  # -------- Run analysis --------
  observeEvent(input$run, {
    req(file.exists(path.expand(input$pbf)))
    
    nr <- active_nr()
    validate(need(!is.na(nr), "Keine Nr ausgewählt (Klick oder manuelle Eingabe)."))
    
    xjh <- jh_sf()
    jh <- xjh %>% filter(nr == !!nr)
    validate(need(nrow(jh) >= 1, paste0("Nr ", nr, " nicht im JH-Layer gefunden.")))
    jh <- jh[1,]
    
    # Buffer
    jh_utm <- st_transform(jh, 25832)
    buf_utm <- st_buffer(jh_utm, dist = input$radius)
    buf_wgs <- st_transform(buf_utm, 4326)
    bb <- st_bbox(buf_wgs)
    
    # POIs
    pois <- grab_pois_from_pbf(
      pbf_path = path.expand(input$pbf),
      bb = bb,
      theme = input$theme,
      cache_dir = cache_dir,
      force = isTRUE(input$force)
    )
    
    if (inherits(pois, "sf") && nrow(pois) > 0) {
      # nur gemappte Kategorien
      pois <- pois %>% filter(!is.na(category) & category != "")
      
      # Namen: konsequent raus, wenn leer/NA
      pois$poi_name <- as.character(pois$poi_name)
      pois$poi_name <- ifelse(is.na(pois$poi_name), "", trimws(pois$poi_name))
      pois <- pois %>% filter(.data$poi_name != "")
      
      # Gruppen
      pois$group <- category_to_group(pois$category)
      
      # Distanz integer (m)
      pois_utm <- st_transform(pois, 25832)
      pois$dist_m <- as.integer(round(as.numeric(st_distance(pois_utm, jh_utm))))
    } else {
      pois <- empty_sf(4326)
      pois$category <- character(0)
      pois$group <- character(0)
      pois$poi_name <- character(0)
      pois$dist_m <- integer(0)
    }
    
    # Summary / Top
    if (inherits(pois, "sf") && nrow(pois) > 0) {
      sumtab <- pois %>%
        st_drop_geometry() %>%
        group_by(group) %>%
        summarise(
          n = n(),
          min_dist_m = min(dist_m, na.rm = TRUE),
          p50_dist_m = as.integer(round(stats::median(dist_m, na.rm = TRUE))),
          .groups = "drop"
        ) %>%
        arrange(desc(n), min_dist_m)
      
      top <- pois %>%
        st_drop_geometry() %>%
        arrange(group, dist_m) %>%
        group_by(group) %>%
        slice_head(n = 10) %>%
        ungroup() %>%
        select(group, poi_name, dist_m, category)
    } else {
      sumtab <- data.frame(group=character(0), n=integer(0), min_dist_m=integer(0), p50_dist_m=integer(0))
      top <- data.frame(group=character(0), poi_name=character(0), dist_m=integer(0), category=character(0))
    }
    
    # Farben pro Gruppe -> PNG-Farbnamen
    # verfügbare Farben in leaflet-color-markers: blue, red, green, orange, yellow, violet, grey, black
    group_palette <- c(
      "Info" = "blue",
      "Kultur & Geschichte" = "red",
      "Bildungsinfrastruktur" = "orange",
      "Sport & Outdoor" = "violet",
      "Natur" = "green",
      "Sonstiges" = "grey"
    )
    
    # fehlende Gruppen -> fallback
    if (nrow(sumtab) > 0) {
      for (g in sumtab$group) {
        if (is.na(group_palette[[g]]) || !nzchar(group_palette[[g]])) group_palette[[g]] <- "blue"
      }
    }
    
    rv$jh <- jh
    rv$buffer <- buf_wgs
    rv$pois <- pois
    rv$summary <- sumtab
    rv$top <- top
    rv$group_colors <- as.list(group_palette)
    
    sel_group(NA_character_) # reset filter
    
    # Tabellen aktualisieren (kompakter, integer)
    output$tbl_sum <- renderDT({
      datatable(
        rv$summary,
        selection = "single",
        options = list(pageLength = 8, autoWidth = TRUE, dom = "tip")
      )
    })
    
    output$tbl_top <- renderDT({
      datatable(
        rv$top,
        selection = "single",
        options = list(pageLength = 10, autoWidth = TRUE, dom = "tip")
      )
    })
    
    # Karte zeichnen
    draw_result(group_filter = NA_character_)
  })
  
  # -------- Klick Summary -> Filter/Highlight --------
  observeEvent(input$tbl_sum_rows_selected, {
    req(is.data.frame(rv$summary))
    if (nrow(rv$summary) == 0) return()
    
    idx <- input$tbl_sum_rows_selected
    if (length(idx) != 1) return()
    g <- rv$summary$group[idx]
    
    if (!is.na(sel_group()) && identical(sel_group(), g)) {
      sel_group(NA_character_)
      draw_result(group_filter = NA_character_)
    } else {
      sel_group(g)
      draw_result(group_filter = g)
    }
  })
  
  # -------- Klick Top-POI -> Filter + Zoom + Popup --------
  observeEvent(input$tbl_top_rows_selected, {
    req(is.data.frame(rv$top))
    if (nrow(rv$top) == 0) return()
    idx <- input$tbl_top_rows_selected
    if (length(idx) != 1) return()
    
    g <- rv$top$group[idx]
    sel_group(g)
    
    # passenden POI in sf suchen (über poi_name+dist+group möglichst stabil)
    if (!inherits(rv$pois, "sf") || nrow(rv$pois) == 0) {
      draw_result(group_filter = g)
      return()
    }
    
    nm <- rv$top$poi_name[idx]
    dm <- rv$top$dist_m[idx]
    ct <- rv$top$category[idx]
    
    cand <- rv$pois %>%
      filter(.data$group == g, .data$poi_name == nm, .data$dist_m == dm, .data$category == ct)
    
    if (nrow(cand) == 0) {
      cand <- rv$pois %>%
        filter(.data$group == g, .data$poi_name == nm, .data$category == ct) %>%
        arrange(abs(.data$dist_m - dm)) %>%
        slice_head(n = 1)
    } else {
      cand <- cand %>% slice_head(n = 1)
    }
    
    draw_result(group_filter = g, zoom_to_poi = cand)
  })
  
  # -----------------------------
  # Exporte
  # -----------------------------
  output$dl_gpkg <- downloadHandler(
    filename = function() {
      nr <- ifelse(is.na(active_nr()), "NA", active_nr())
      paste0("jh_", nr, "_pois_export.gpkg")
    },
    content = function(file) {
      req(inherits(rv$jh, "sf"))
      req(inherits(rv$buffer, "sf"))
      
      if (file.exists(file)) unlink(file, force = TRUE)
      
      st_write(rv$jh, file, layer = "jh", quiet = TRUE)
      st_write(rv$buffer, file, layer = "buffer", quiet = TRUE)
      
      if (inherits(rv$pois, "sf") && nrow(rv$pois) > 0) {
        st_write(rv$pois, file, layer = "pois", quiet = TRUE)
      } else {
        st_write(empty_sf(4326), file, layer = "pois", quiet = TRUE)
      }
    }
  )
  
  output$dl_csv <- downloadHandler(
    filename = function() {
      nr <- ifelse(is.na(active_nr()), "NA", active_nr())
      paste0("jh_", nr, "_summary.csv")
    },
    content = function(file) {
      if (is.data.frame(rv$summary) && nrow(rv$summary) > 0) {
        write.csv(rv$summary, file, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        write.csv(data.frame(group=character(0), n=integer(0), min_dist_m=integer(0), p50_dist_m=integer(0)),
                  file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )
  
  output$dl_html <- downloadHandler(
    filename = function() {
      nr <- ifelse(is.na(active_nr()), "NA", active_nr())
      paste0("jh_", nr, "_report.html")
    },
    content = function(file) {
      # kleine statische HTML-Seite (Karte + Tabellen), ohne Shiny-Interaktion
      req(inherits(rv$jh, "sf"))
      
      # Karte als widget bauen
      # (Filterzustand ignorieren: immer Vollansicht)
      # Wir rendern hier "hart" mit leaflet (ohne leafletProxy)
      jh_wgs  <- st_transform(rv$jh, 4326)
      buf_wgs <- st_transform(rv$buffer, 4326)
      bb <- st_bbox(buf_wgs)
      
      m <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron, group="Karte") %>%
        addProviderTiles(providers$Esri.WorldImagery, group="Satellit") %>%
        addLayersControl(baseGroups=c("Karte","Satellit"), overlayGroups=c("JH","Radius","POIs"),
                         options=layersControlOptions(collapsed=TRUE)) %>%
        addPolygons(data = buf_wgs, color="#2b8cbe", weight=2, fillOpacity=0.06, group="Radius") %>%
        addMarkers(data = jh_wgs, icon = make_png_icon("red"),
                   popup = ~paste0("<b>JH Nr. ", nr, "</b><br>", htmlEscape(jh_name), "<br>", htmlEscape(ort)),
                   group="JH") %>%
        fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
      
      if (inherits(rv$pois, "sf") && nrow(rv$pois) > 0) {
        pois_wgs <- st_transform(rv$pois, 4326)
        cl_opt <- markerClusterOptions(chunkedLoading = TRUE, disableClusteringAtZoom = 16)
        
        for (g in unique(pois_wgs$group)) {
          col <- rv$group_colors[[g]]
          if (is.null(col) || !nzchar(col)) col <- "blue"
          m <- m %>%
            addMarkers(
              data = pois_wgs %>% filter(.data$group == g),
              icon = make_png_icon(col),
              popup = ~paste0("<b>", htmlEscape(poi_name), "</b>",
                              "<br>Gruppe: ", htmlEscape(group),
                              "<br>Kategorie: ", htmlEscape(category),
                              "<br>Distanz (m): ", dist_m),
              clusterOptions = cl_opt,
              group="POIs"
            )
        }
        
        jh_lab <- paste0(rv$jh$jh_name[1], " (Nr. ", rv$jh$nr[1], ")")
        if (is.data.frame(rv$summary) && nrow(rv$summary) > 0) {
          m <- m %>% addControl(html = legend_html_groups(rv$summary, rv$group_colors, jh_label = jh_lab), position = "topright")
        }
      }
      
      # Tabellen
      sum_tbl <- if (is.data.frame(rv$summary)) rv$summary else data.frame()
      top_tbl <- if (is.data.frame(rv$top)) rv$top else data.frame()
      
      page <- tags$html(
        tags$head(
          tags$meta(charset="utf-8"),
          tags$meta(name="viewport", content="width=device-width, initial-scale=1"),
          tags$style(HTML("
            body { font-family: system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif; margin: 18px; }
            .box { border:1px solid #ddd; border-radius:10px; padding:12px; margin:10px 0; }
            h1 { font-size: 18px; margin: 0 0 10px 0; }
            h2 { font-size: 14px; margin: 0 0 8px 0; }
            table { border-collapse: collapse; width: 100%; }
            th, td { border: 1px solid #eee; padding: 6px 8px; font-size: 12px; }
            th { background:#fafafa; text-align:left; }
          "))
        ),
        tags$body(
          tags$h1("JH Grabber Report"),
          tags$div(class="box",
                   tags$h2("Karte"),
                   m
          ),
          tags$div(class="box",
                   tags$h2("Übersicht nach Gruppen"),
                   tags$table(
                     tags$thead(tags$tr(lapply(names(sum_tbl), tags$th))),
                     tags$tbody(
                       lapply(seq_len(nrow(sum_tbl)), function(i) tags$tr(lapply(sum_tbl[i,], function(v) tags$td(as.character(v)))))
                     )
                   )
          ),
          tags$div(class="box",
                   tags$h2("POIs (Top je Gruppe)"),
                   tags$table(
                     tags$thead(tags$tr(lapply(names(top_tbl), tags$th))),
                     tags$tbody(
                       lapply(seq_len(nrow(top_tbl)), function(i) tags$tr(lapply(top_tbl[i,], function(v) tags$td(as.character(v)))))
                     )
                   )
          )
        )
      )
      
      htmlwidgets::saveWidget(as_widget(page), file = file, selfcontained = TRUE)
    }
  )
}

shinyApp(ui, server)
