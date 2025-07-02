#' Load, Process, and Simplify City District (Bydel) Data
#'
#' This function reads city district and county boundary files, intersects them
#' to add county information to each district, simplifies the geometries, and
#' returns a processed list of city districts for major Norwegian cities.
#'
#' @param bydel_file Character string. The path to the spatial file containing
#'   city district boundaries (e.g., "data-raw/bydel2020_mednavn.gml").
#' @param fylke_file Character string. The path to the spatial file containing
#'   county boundaries (e.g., "data-raw/fylker.geojson").
#'
#' @details
#' The function expects two spatial data files readable by `sf::st_read()`:
#' \itemize{
#'   \item \code{bydel_file}: Must contain at least the columns `bydelnavn`
#'         (district name) and `bydelnr` (district number). The function uses
#'         `bydelnr` to filter for Oslo (starting with "03"), Bergen ("46"),
#'         Stavanger ("11"), and Trondheim ("50").
#'   \item \code{fylke_file}: Must contain at least the columns `navn` (county
#'         name) and `nummer` (county number). These attributes are joined to
#'         the district data during the intersection.
#' }
#' The function also cleans the data by removing the "Marka" district from Oslo.
#'
#' @return A named list of simplified `sf` objects. Each element of the list
#'   (e.g., `Oslo`) is an `sf` data frame with columns `bydelnavn`, `bydelnr`,
#'   `navn` (from fylke), and `nummer` (from fylke).
#'
#' @importFrom sf st_read st_transform st_crs st_intersection
#' @importFrom rmapshaper ms_simplify
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#'   # This example requires the specific data files in the correct format.
#'   bydeler_data <- load_bydeler(
#'     bydel_file = "data-raw/bydel2020_mednavn.gml",
#'     fylke_file = "data-raw/fylker.geojson"
#'   )
#'
#'   # View the processed districts for Oslo, which now include fylke info
#'   print(head(bydeler_data$Oslo))
#' }
load_bydeler <- function(bydel_file, fylke_file) {

  # Check if the files exist before trying to read them
  if (!file.exists(bydel_file)) stop("File not found: ", bydel_file)
  if (!file.exists(fylke_file)) stop("File not found: ", fylke_file)

  # Read the spatial data from the files
  bydeler_raw <- sf::st_read(bydel_file)
  fylker <- sf::st_read(fylke_file)

  # Create a list, filtering districts for each major city based on 'bydelnr'
  bydeler <- list(
    Oslo = bydeler_raw[grepl("^03|^3", bydeler_raw$bydelnr), ],
    Bergen = bydeler_raw[grepl("^46", bydeler_raw$bydelnr), ],
    Stavanger = bydeler_raw[grepl("^11", bydeler_raw$bydelnr), ],
    Trondheim = bydeler_raw[grepl("^50", bydeler_raw$bydelnr), ]
  )

  # Simplify the data for each city, keeping only name and number columns
  bydeler <- lapply(bydeler, function(b) {
    b[, c("bydelnavn", "bydelnr")]
  })

  # Specific data cleaning: Remove the "Marka" district from Oslo
  if ("Oslo" %in% names(bydeler) && "bydelnavn" %in% names(bydeler$Oslo)) {
    bydeler[["Oslo"]] <- bydeler[["Oslo"]][bydeler[["Oslo"]]$bydelnavn != "Marka", ]
  }

  # Ensure fylker CRS matches the bydeler data for intersection
  fylker_transformed <- fylker %>%
    sf::st_transform(crs = sf::st_crs(bydeler[[1]]))

  # Intersect and simplify each city's districts
  for (b in 1:length(bydeler)) {
    # Add county info and simplify
    bydeler[[b]] <- sf::st_intersection(bydeler[[b]], fylker_transformed)[, c("bydelnavn", "bydelnr", "navn", "nummer")]
    bydeler[[b]] <- rmapshaper::ms_simplify(bydeler[[b]], keep = 0.05, keep_shapes = TRUE)
  }

  # Return the final processed list
  return(bydeler)
}


#' Load, Intersect, and Simplify Municipality (Kommune) Data
#'
#' Reads municipality and county boundary files, performs a spatial
#' intersection to assign county information to each municipality, cleans up
#' resulting geometries, and simplifies them for efficient plotting.
#'
#' @param kommune_file Character string. The path to the spatial file containing
#'   municipality boundaries.
#' @param fylke_file Character string. The path to the spatial file containing
#'   county (fylke) boundaries. The Fylke file is expected to show only land masses
#'   and to not include see area belonging to a fylke.
#'
#' @details
#' The function expects two spatial data files readable by `sf::st_read()`:
#' \itemize{
#'   \item \code{kommune_file}: Should contain at least the columns
#'         `kommunenavn` (municipality name) and `kommunenummer` (municipality number).
#'   \item \code{fylke_file}: Should contain at least the columns `navn` (county name)
#'         and `nummer` (county number). The function will transfer these attributes
#'         to the municipalities during the intersection.
#' }
#' The function cleans the data by removing `GEOMETRYCOLLECTION` features that
#' can result from the intersection, ensuring a clean set of polygons.
#'
#' @return A simplified `sf` object of municipalities, with corresponding county
#'   data (`navn`, `nummer`) appended.
#'
#' @importFrom sf st_read st_transform st_crs st_intersection st_geometry_type
#' @importFrom rmapshaper ms_simplify
#' @export
#'
#' @examples
#' \dontrun{
#'   # This example requires the specific data files.
#'   # Ensure the files are in your working directory or provide full paths.
#'   kommuner_data <- load_and_process_kommuner(
#'     kommune_file = "data-raw/kommune.gml",
#'     fylke_file = "data-raw/fylker.geojson"
#'   )
#'
#'   # View the first few rows of the processed data
#'   print(head(kommuner_data))
#'
#'   # Plot the simplified municipalities
#'   plot(st_geometry(kommuner_data))
#' }
load_kommuner <- function(kommune_file, fylke_file) {

  # --- 1. Input Validation ---
  if (!file.exists(kommune_file)) stop("Municipality file not found: ", kommune_file)
  if (!file.exists(fylke_file)) stop("County file not found: ", fylke_file)

  # --- 2. Load Data ---
  kommuner <- sf::st_read(kommune_file)
  fylker <- sf::st_read(fylke_file)

  # --- 3. Process Data ---
  # Ensure county CRS matches municipality CRS before intersection
  fylker_transformed <- sf::st_transform(fylker, crs = sf::st_crs(kommuner))

  # Intersect to assign county data to municipalities. Suppress warnings about
  # attribute assumptions, which are expected here.
  kommuner_intersected <- suppressWarnings(
    sf::st_intersection(kommuner, fylker_transformed)
  )

  # Remove problematic GEOMETRYCOLLECTION types that can arise from intersection
  kommuner_clean <- kommuner_intersected[
    sf::st_geometry_type(kommuner_intersected) != "GEOMETRYCOLLECTION",
  ]

  # Simplify geometries for faster plotting and smaller file size
  # The 'keep' parameter determines the level of detail retained.
  kommuner_simplified <- rmapshaper::ms_simplify(
    kommuner_clean,
    keep = 0.0005,
    keep_shapes = TRUE
  )

  # Select and reorder the final set of columns
  final_columns <- c("kommunenavn", "kommunenummer", "navn", "nummer")
  kommuner_final <- kommuner_simplified[, final_columns]

  return(kommuner_final)
}

#' Create a Map with County and City District Insets
#'
#' This function takes a map of Norwegian counties (`fylker`) and a list of
#' city districts (`bydeler`) maps and creates a composite map. The city districts
#' are scaled and placed as insets around the main county map.
#'
#' @param fylker An `sf` object representing the base map of Norwegian counties.
#'   It should contain the columns `navn` and `nummer`.
#' @param bydeler A named list of `sf` objects, where each list element is a
#'   city and contains the `sf` data for its districts. Each `sf` object
#'   should contain `bydelnavn` and `bydelnr`.
#' @param scaling_params A named numeric vector specifying the scaling divisor for
#'   each city inset. Larger numbers result in smaller insets. Names must
#'   match the names in the `bydeler` list.
#' @param x_offset_params A named numeric vector (0-1) for placing the insets
#'   horizontally. 0 is the far left, 1 is the far right of the main map's
#'   bounding box.
#' @param y_offset_params A named numeric vector (0-1) for placing the insets
#'   vertically. 0 is the bottom, 1 is the top of the main map's
#'   bounding box.
#'
#' @return An `sf` object combining the county geometries with the scaled and
#'   translated city district inset geometries. A new `level` column is added
#'   to distinguish between "fylke" and "bydel".
#'
#' @importFrom sf st_bbox st_geometry st_crs st_set_crs st_sf st_drop_geometry st_transform
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#'   # This example requires the data to be loaded first, for instance using
#'   # the load_and_process_kommuner() and load_and_process_bydeler() functions.
#'
#'   # Define parameters for the insets
#'   scaling <- c(Oslo = 2.5, Bergen = 3.5, Stavanger = 3.5, Trondheim = 3.5)
#'   x_offsets <- c(Oslo = 0.005, Bergen = 0.9, Stavanger = 1, Trondheim = 0.7)
#'   y_offsets <- c(Oslo = 0.005, Bergen = 0.5, Stavanger = 0.0, Trondheim = 0.8)
#'
#'   # Generate the final map object
#'   map_with_insets <- create_inset_map(
#'     fylker = fylke,
#'     bydeler = bydeler,
#'     scaling_params = scaling,
#'     x_offset_params = x_offsets,
#'     y_offset_params = y_offsets
#'   )
#'
#'   # Plot using ggplot2
#'   if (requireNamespace("ggplot2", quietly = TRUE)) {
#'     library(ggplot2)
#'     ggplot(data = map_with_insets) +
#'       geom_sf(aes(fill = level))
#'   }
#' }
create_inset_map <- function(fylker,
                             bydeler,
                             scaling_params = c(Oslo = 2.5, Bergen = 3.5, Stavanger = 3.5, Trondheim = 3.5),
                             x_offset_params = c(Oslo = 0.005, Bergen = 0.9, Stavanger = 1, Trondheim = 0.7),
                             y_offset_params = c(Oslo = 0.005, Bergen = 0.5, Stavanger = 0.0, Trondheim = 0.8)) {

  # --- 1. Calculate Scaling Factors ---
  fylke_bbox <- sf::st_bbox(fylker)
  scale_factors <- vapply(names(bydeler), function(city_name) {
    bydel_bbox <- sf::st_bbox(bydeler[[city_name]])
    (fylke_bbox$xmax - fylke_bbox$xmin) / (bydel_bbox$xmax - bydel_bbox$xmin) / scaling_params[[city_name]]
  }, FUN.VALUE = numeric(1))

  # --- 2. Scale Geometries ---
  bydeler_scaled <- lapply(names(bydeler), function(city_name) {
    geom <- sf::st_geometry(bydeler[[city_name]])
    scaled_geom <- geom * scale_factors[[city_name]]
    sf::st_set_crs(scaled_geom, sf::st_crs(geom))
  })

  # --- 3. Calculate Translation Offsets ---
  widths <- vapply(bydeler_scaled, function(b) sf::st_bbox(b)$xmax - sf::st_bbox(b)$xmin, FUN.VALUE = numeric(1))
  heights <- vapply(bydeler_scaled, function(b) sf::st_bbox(b)$ymax - sf::st_bbox(b)$ymin, FUN.VALUE = numeric(1))

  new_x_coords <- vapply(seq_along(bydeler), function(j) {
    city_name <- names(bydeler)[j]
    fylke_bbox$xmax - widths[j] - (fylke_bbox$xmax - fylke_bbox$xmin) * x_offset_params[[city_name]]
  }, FUN.VALUE = numeric(1))

  new_y_coords <- vapply(seq_along(bydeler), function(j) {
    city_name <- names(bydeler)[j]
    fylke_bbox$ymin + (fylke_bbox$ymax - fylke_bbox$ymin) * y_offset_params[[city_name]]
  }, FUN.VALUE = numeric(1))

  # --- 4. Translate Geometries ---
  bydeler_translated <- lapply(seq_along(bydeler_scaled), function(j) {
    geom <- bydeler_scaled[[j]]
    current_bbox <- sf::st_bbox(geom)
    translation_vector <- c(new_x_coords[j] - current_bbox$xmin, new_y_coords[j] - current_bbox$ymin)
    translated_geom <- geom + translation_vector
    sf::st_set_crs(translated_geom, sf::st_crs(geom))
  })

  # --- 5. Rebuild sf objects and Standardize ---
  target_crs <- sf::st_crs(fylker)
  bydeler_inset_list <- lapply(seq_along(bydeler), function(j) {
    city_name <- names(bydeler)[j]

    # Create a clean data frame with standardized names
    attrs <- sf::st_drop_geometry(bydeler[[city_name]])
    clean_df <- data.frame(
      navn = attrs$bydelnavn,
      nummer = attrs$bydelnr
    )

    # Combine with translated geometry
    sf_obj <- sf::st_sf(clean_df, geometry = bydeler_translated[[j]])
    sf::st_transform(sf_obj, crs = target_crs)
  })

  # --- 6. Combine and Finalize ---
  bydeler_inset <- do.call(rbind, bydeler_inset_list)
  bydeler_inset$level <- "bydel"

  fylker_final <- fylker
  fylker_final$level <- "fylke"

  # Manually align columns for a robust rbind
  names(bydeler_inset) <- c("navn", "nummer", "geometry", "level")
  fylker_final <- fylker_final[, c("navn", "nummer", "geometry", "level")]

  final_map <- rbind(fylker_final, bydeler_inset)

  return(final_map)
}


