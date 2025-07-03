#' Map of Norwegian Municipalities (Kommuner)
#'
#' An `sf` dataset containing the geographic boundaries of Norwegian
#' municipalities. The data is based on administrative boundaries and includes
#' corresponding municipality and county information. Note that one feature has an
#' empty geometry.
#'
#' @format An `sf` data frame with 524 features (rows) and 5 columns:
#' \describe{
#'   \item{kommunenavn}{Character. The name of the municipality (e.g., "Tromsø").}
#'   \item{kommunenummer}{Character. The official 4-digit municipality number.}
#'   \item{navn}{Character. The name of the county (fylke) the municipality belongs to.}
#'   \item{nummer}{Character. The official 2-digit county number.}
#'   \item{omrade}{sfc_GEOMETRY. The `sf` geometry column containing the polygon/multipolygon for each municipality.}
#' }
#'
#'
#' @source
#' The data is derived from public sources, likely Statens kartverk (https://www.geonorge.no/)
#' or Statistics Norway (https://kart.ssb.no/), and has been simplified for efficient plotting.
#' The maps use boundaries and municipality numbers from 2024.
#'
#' @keywords datasets
#' #'
#' @examples
#' # Check if the 'kommuner' data is available
#' if (exists("kommuner")) {
#'
#'   # Using ggplot2 to show the different levels
#'   if (requireNamespace("ggplot2", quietly = TRUE)) {
#'     library(ggplot2)
#'     ggplot(data = kommuner) +
#'       geom_sf(aes(fill = level)) +
#'       theme_minimal() +
#'       labs(
#'         title = "Norwegian municiåalities",
#'         fill = "Administrative Level"
#'        )
#'   }
#' }
"MapKommuner"

#' Map of Norwegian Counties (Fylker)
#'
#' An `sf` dataset containing the geographic boundaries of Norwegian
#' counties (fylker). The data is based on administrative boundaries.
#'
#' @format An `sf` data frame with features for each county and 4 columns:
#' \describe{
#'   \item{fid}{Character. A feature ID, likely from the original data source.}
#'   \item{navn}{Character. The name of the county (e.g., "Troms").}
#'   \item{nummer}{Character. The official 2-digit county number.}
#'   \item{geometry}{sfc_MULTIPOLYGON. The `sf` geometry column containing the multipolygon for each county.}
#' }
#'
#' @source
#' The data is derived from public sources, likely Statens kartverk (https://www.geonorge.no/)
#' or Statistics Norway (https://kart.ssb.no/).
#' The maps use boundaries and fylke numbers from 2024.
#'
#' @keywords datasets
#' #'
#' @examples
#' # Check if the 'fylke' data is available
#' if (exists("fylke")) {
#'
#'   # Using ggplot2 to show the different levels
#'   if (requireNamespace("ggplot2", quietly = TRUE)) {
#'     library(ggplot2)
#'     ggplot(data = fylke) +
#'       geom_sf(aes(fill = level)) +
#'       theme_minimal() +
#'       labs(
#'         title = "Norwegian Counties",
#'         fill = "Administrative Level"
#'        )
#'   }
#' }
"MapFylker"

#' Map of Norwegian Counties and City Districts
#'
#' An `sf` dataset containing geographic boundaries for Norwegian counties
#' (fylker) combined with a more detailed level for city districts (bydeler)
#' in Oslo, Bergen, Stavanger and Trondheim. Cities with their disctricts are
#' shown around the county map.
#' The `level` column distinguishes between the two geographic types.
#'
#' @format An `sf` data frame with 58 features and 4 columns:
#' \describe{
#'   \item{navn}{Character. The name of the geographic area (e.g., "Troms", "Grünerløkka").}
#'   \item{nummer}{Character. The official administrative number.}
#'   \item{level}{Character. The administrative level of the feature, either "fylke" (county) or "bydel" (city district).}
#'   \item{geometry}{sfc_GEOMETRY. The `sf` geometry column for each area.}
#' }
#'
#' @source
#' The data is a composite, derived from public sources like Statens kartverk
#' (https://www.geonorge.no/) or Statistics Norway (https://kart.ssb.no/).
#' The maps use boundaries and municipality or city-district numbers from 2024.
#'
#' @keywords datasets
#'
#' @examples
#' # Check if the 'fylke_bydeler' data is available
#' if (exists("fylke_bydeler")) {
#'
#'   # Using ggplot2 to show the different levels
#'   if (requireNamespace("ggplot2", quietly = TRUE)) {
#'     library(ggplot2)
#'     ggplot(data = fylke_bydeler) +
#'       geom_sf(aes(fill = level)) +
#'       theme_minimal() +
#'       labs(
#'         title = "Norwegian Counties and City Districts",
#'         fill = "Administrative Level"
#'        )
#'   }
#' }
"MapFylkerBydeler"


#' MapKommunerBydeler: Simplified Map Data for Norwegian Municipalities and City Districts
#'
#' A simplified `sf` (simple feature) data frame containing the geographical boundaries
#' for Norwegian municipalities (kommuner) and selected city districts (bydeler),
#' excluding the main municipalities of Oslo, Bergen, Stavanger, and Trondheim
#' (as their bydeler are included instead). The geometries are simplified for
#' faster plotting and smaller file size. This dataset is intended for use
#' as a base map for visualizing data at the municipality or city district level in Norway.
#'
#' @format A `sf` data frame with 391 features and 5 fields:
#' \describe{
#'   \item{Nr}{Character or numeric. The official municipality (kommunenummer) or
#'     city district (bydelnr) code.}
#'   \item{fylkenavn}{Character. The name of the county (fylke) to which the
#'     municipality or city district belongs.}
#'   \item{navn}{Character. The official name of the municipality or city district.}
#'   \item{fylkeNR}{Character or numeric. The official county (fylkenummer) code.}
#'   \item{geometry}{`sfc_GEOMETRY`. The spatial geometry of the feature,
#'     which can be `MULTIPOLYGON` or `POLYGON`.}
#'   \item{level}{Character. Indicates whether the feature represents a "kommune"
#'     or a "bydel". (Note: Based on the provided `head()` output, this column
#'     `level` is not explicitly shown, but it's a common and useful addition
#'     when combining different administrative levels. If this column is not
#'     generated by `prepare_norway_map_data`, it should be added or this
#'     description adjusted.)}
#' }
#'
#' @source Data derived from Geonorge (kommune.gml, bydel2020_mednavn.gml) and
#'   Statistisk sentralbyrå (SSB) (Fylker.geojson). Data are from 2024.
#'
#' @details
#' The data has been processed to:
#' \itemize{
#'   \item Harmonize Coordinate Reference Systems (CRS) to ETRS89 / UTM zone 33N.
#'   \item Ensure consistent geometry column naming (`geometry`).
#'   \item Perform spatial intersections to clip municipalities and city districts
#'         to county boundaries.
#'   \item Exclude the main polygons for Oslo, Bergen, Stavanger, and Trondheim,
#'         as their city districts are included instead.
#'   \item Simplify geometries using `rmapshaper::ms_simplify` with `keep = 0.0005`
#'         for reduced file size and faster rendering.
#'   \item Rename columns to `Nr`, `navn`, `fylkenavn`, `fylkeNR` for consistency.
#' }
#'
#' @seealso
#' \code{\link{create_KommuneBydel_map}} for the function used to generate this data.
#'
#' @examples
#' \dontrun{
#' # To load this data (assuming it's part of your package data)
#' # data(MapKommunerBydeler)
#' # plot(MapKommunerBydeler)
#'
#' # Example of plotting a subset (e.g., a specific county)
#' # library(dplyr)
#' # library(ggplot2)
#' # MapKommunerBydeler %>%
#' #   filter(fylkenavn == "Troms") %>%
#' #   ggplot() +
#' #   geom_sf() +
#' #   labs(title = "Municipalities and Bydeler in Troms")
#' }
"MapKommunerBydeler"
