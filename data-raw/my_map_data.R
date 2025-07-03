library(sf)
library(rmapshaper)
library(MapNorway)

MapBydeler = load_bydeler("data-raw/bydel2020_mednavn.gml","data-raw/fylker.geojson")

MapKommuner = load_kommuner("data-raw/kommune.gml","data-raw/fylker.geojson")

MapFylker =
  st_read("data-raw/fylker.geojson") %>%
  st_transform(crs = st_crs(MapKommuner)) %>%
  ms_simplify(keep = 0.0005, keep_shapes = TRUE)

MapFylker = MapFylker[, c("navn","nummer")]

MapFylkerBydeler = create_inset_map(MapFylker,MapBydeler)



usethis::use_data(MapKommuner)
usethis::use_data(MapFylker)
usethis::use_data(MapFylkerBydeler)
