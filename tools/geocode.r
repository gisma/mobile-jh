suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(sf)
  library(tidygeocoder)
  library(rnaturalearth)
})

# --------------------------
# 1) CSV laden
# --------------------------
bak_dir = getwd()
setwd("~/Desktop/DJH/mobile-jh/mobile-jh/data/")
in_csv  <- "djh-adressen.csv"
out_gpkg <- "djh-adressen_geocoded.gpkg"

df <- read_delim(
  in_csv,
  delim = ";",
  col_types = cols(.default = col_character()),
  locale = locale(encoding = "UTF-8")
) |>
  janitor::clean_names() |>
  mutate(
    plz = str_extract(plz, "\\d{5}"),
    address = paste(strasse, plz, ort, "Deutschland", sep = ", ")
  )

# --------------------------
# 2) Geokodierung (OSM)
# --------------------------

df_geo <- df |>
  geocode(
    address = address,
    method = "osm",
    lat = lat,
    long = long
  )

# --------------------------
# 3) sf-Objekt erzeugen
# --------------------------

sf_pts <- df_geo |>
  filter(!is.na(lat), !is.na(long)) |>
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# --------------------------
# 4) Bundesländer laden
# --------------------------

states <- rnaturalearth::ne_states(
  country = "Germany",
  returnclass = "sf"
)

states <- st_transform(states, 4326)

# --------------------------
# 5) Räumliche Zuordnung
# --------------------------

sf_joined <- st_join(
  sf_pts,
  states[, c("name")],
  left = TRUE
)

sf_joined <- sf_joined |>
  rename(bundesland = name.y)

# --------------------------
# 6) Speichern
# --------------------------

if (file.exists(out_gpkg)) file.remove(out_gpkg)

st_write(sf_joined, out_gpkg, quiet = TRUE)
setwd(bak_dir)
cat("Fertig:", out_gpkg, "\n")


