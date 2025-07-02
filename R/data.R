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
