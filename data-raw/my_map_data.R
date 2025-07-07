library(sf)
library(rmapshaper)
library(MapNorway)

MapBydeler = load_bydeler("data-raw/bydel2020_mednavn.gml","data-raw/fylker.geojson")

MapKommuner = load_kommuner("data-raw/kommune.gml","data-raw/fylker.geojson")

MapFylker =
  st_read("data-raw/fylker.geojson") %>%
  st_transform(crs = st_crs(MapKommuner)) %>%
  ms_simplify(keep = 0.05, keep_shapes = TRUE)

MapFylker = MapFylker[, c("navn","nummer")]


MapKommunerBydeler =
  create_KommuneBydel_map(
  "data-raw/kommune.gml",
  "data-raw/bydel2020_mednavn.gml",
  "data-raw/fylker.geojson",
  simplification_keep_ratio = .0375)


usethis::use_data(MapKommuner, overwrite = TRUE)
usethis::use_data(MapFylker, overwrite = TRUE)
usethis::use_data(MapKommunerBydeler, overwrite = TRUE)
