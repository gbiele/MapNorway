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


#' Create Inset Map for Norwegian Cities
#'
#' This function generates an `sf` object that includes the main map data (`M`)
#' along with scaled and translated inset maps for specific major Norwegian cities
#' (Oslo, Bergen, Stavanger/Sandnes, Trondheim). The inset maps display the city
#' districts (bydeler) of these cities at an enlarged scale and positioned in
#' a corner of the overall map bounding box.
#'
#' @param M An `sf` (simple feature) data frame representing the main map of Norway.
#'   It is expected to contain columns like `Nr`, `navn`, `fylkenavn`, `fylkeNR`,
#'   `storby` (indicating if a feature belongs to a major city), and `level`
#'   (e.g., "kommune" or "bydel"). The `geometry` column is also required.
#' @param scaling_params A named numeric vector. Specifies the scaling factor for
#'   each city's inset map relative to the main map's bounding box. Higher values
#'   result in smaller insets (less scaling). Names should correspond to city names
#'   (e.g., "Oslo", "Bergen", "Stavanger/Sandnes", "Trondheim").
#' @param x_offset_params A named numeric vector. Specifies the x-offset as a
#'   proportion of the main map's width for positioning the inset maps. Values
#'   between 0 and 1. Names should correspond to city names.
#' @param y_offset_params A named numeric vector. Specifies the y-offset as a
#'   proportion of the main map's height for positioning the inset maps. Values
#'   between 0 and 1. Names should correspond to city names.
#'
#' @return An `sf` data frame that combines the original map `M` with the
#'   generated inset maps. A new logical column `inset` is added, which is `TRUE`
#'   for inset features and `FALSE` for main map features.
#' @export
#'
#' @import sf
#' @importFrom magrittr %>%
#' @importFrom dplyr select rename
#'
#' @examples
#' \dontrun{
#' # Assuming MapKommunerBydeler is loaded (e.g., from package data)
#' # data(MapKommunerBydeler)
#'
#' # Ensure 'storby' and 'level' columns exist in MapKommunerBydeler for this example
#' # MapKommunerBydeler$storby <- NA
#' # MapKommunerBydeler$level <- "kommune"
#' # MapKommunerBydeler[MapKommunerBydeler$navn %in% c("Oslo", "Bergen", "Stavanger", "Trondheim"), "storby"] <- MapKommunerBydeler[MapKommunerBydeler$navn %in% c("Oslo", "Bergen", "Stavanger", "Trondheim"), "navn"]
#' # MapKommunerBydeler[MapKommunerBydeler$navn %in% c("Oslo", "Bergen", "Stavanger", "Trondheim"), "level"] <- "bydel" # Placeholder for actual bydeler
#' # MapKommunerBydeler[MapKommunerBydeler$navn == "Stavanger", "storby"] <- "Stavanger/Sandnes"
#' # MapKommunerBydeler[MapKommunerBydeler$navn == "Trondheim", "storby"] <- "Trondheim"
#'
#' # Example usage with default parameters
#' # map_with_insets <- create_inset_map(M = MapKommunerBydeler)
#' # plot(map_with_insets, col = ifelse(map_with_insets$inset, "red", "grey"), border = "black")
#'
#' # Example with custom scaling and offsets
#' # custom_scaling <- c(Oslo = 2.0, Bergen = 3.0, `Stavanger/Sandnes` = 3.0, Trondheim = 3.0)
#' # custom_x_offset <- c(Oslo = 0.01, Bergen = 0.85, `Stavanger/Sandnes` = 0.95, Trondheim = 0.75)
#' # custom_y_offset <- c(Oslo = 0.01, Bergen = 0.45, `Stavanger/Sandnes` = 0.05, Trondheim = 0.75)
#' # map_with_custom_insets <- create_inset_map(
#' #   M = MapKommunerBydeler,
#' #   scaling_params = custom_scaling,
#' #   x_offset_params = custom_x_offset,
#' #   y_offset_params = custom_y_offset
#' # )
#' # plot(map_with_custom_insets, col = ifelse(map_with_custom_insets$inset, "blue", "lightgrey"))
#' }
create_inset_map <- function(M,
                             scaling_params = c(Oslo = 2.5, Bergen = 3.5, `Stavanger/Sandnes` = 3.5, Trondheim = 3.5),
                             x_offset_params = c(Oslo = 0.005, Bergen = 0.9, `Stavanger/Sandnes` = 1, Trondheim = 0.7),
                             y_offset_params = c(Oslo = 0.005, Bergen = 0.5, `Stavanger/Sandnes` = 0.0, Trondheim = 0.8)) {

  # Define the major cities for which insets will be created
  byer <- c("Oslo", "Bergen", "Stavanger/Sandnes", "Trondheim")

  # Filter the main map data (M) to get the city districts for each major city
  # This assumes 'M' has a 'storby' column that identifies these major cities.
  bydeler <- lapply(byer, function(b) {
    M[which(M$storby == b), ]
  })
  names(bydeler) <- byer

  # Special handling for Oslo: remove "Marka" if present, as it's often
  # excluded from central Oslo city district maps.
  if ("Oslo" %in% names(bydeler) && "Marka" %in% bydeler[["Oslo"]]$navn) {
    bydeler[["Oslo"]] <- bydeler[["Oslo"]][bydeler[["Oslo"]]$navn != "Marka", ]
  }

  # --- 1. Calculate Scaling Factors ---
  # Determine the bounding box of the entire main map (M)
  M_bbox <- sf::st_bbox(M)

  # Calculate a scaling factor for each city's inset.
  # This factor determines how much larger the inset will appear relative to
  # its original size compared to the main map's overall extent.
  scale_factors <- vapply(names(bydeler), function(city_name) {
    bydel_bbox <- sf::st_bbox(bydeler[[city_name]])
    # Calculate the ratio of the main map's width to the city's bydel width,
    # then divide by the user-defined scaling_params for fine-tuning.
    (M_bbox$xmax - M_bbox$xmin) / (bydel_bbox$xmax - bydel_bbox$xmin) / scaling_params[[city_name]]
  }, FUN.VALUE = numeric(1))

  # --- 2. Scale Geometries ---
  # Apply the calculated scaling factors to the geometries of each city's bydeler.
  # Geometries are scaled relative to their own origin, then their CRS is reset.
  bydeler_scaled <- lapply(names(bydeler), function(city_name) {
    geom <- sf::st_geometry(bydeler[[city_name]])
    scaled_geom <- geom * scale_factors[[city_name]]
    sf::st_set_crs(scaled_geom, sf::st_crs(geom)) # Preserve CRS after scaling
  })

  # --- 3. Calculate Translation Offsets ---
  # Calculate the width and height of the scaled inset geometries.
  widths <- vapply(bydeler_scaled, function(b) sf::st_bbox(b)$xmax - sf::st_bbox(b)$xmin, FUN.VALUE = numeric(1))
  heights <- vapply(bydeler_scaled, function(b) sf::st_bbox(b)$ymax - sf::st_bbox(b)$ymin, FUN.VALUE = numeric(1))

  # Determine the new x-coordinates for the top-left corner of each inset.
  # This positions the inset relative to the main map's right edge and user-defined offset.
  new_x_coords <- vapply(seq_along(bydeler), function(j) {
    city_name <- names(bydeler)[j]
    M_bbox$xmax - widths[j] - (M_bbox$xmax - M_bbox$xmin) * x_offset_params[[city_name]]
  }, FUN.VALUE = numeric(1))

  # Determine the new y-coordinates for the bottom-left corner of each inset.
  # This positions the inset relative to the main map's bottom edge and user-defined offset.
  new_y_coords <- vapply(seq_along(bydeler), function(j) {
    city_name <- names(bydeler)[j]
    M_bbox$ymin + (M_bbox$ymax - M_bbox$ymin) * y_offset_params[[city_name]]
  }, FUN.VALUE = numeric(1))

  # --- 4. Translate Geometries ---
  # Apply the calculated translation vectors to move the scaled geometries to their
  # final inset positions within the main map's coordinate system.
  bydeler_translated <- lapply(seq_along(bydeler_scaled), function(j) {
    geom <- bydeler_scaled[[j]]
    current_bbox <- sf::st_bbox(geom)
    # Calculate the vector needed to move the current bbox's min_x, min_y
    # to the desired new_x_coords, new_y_coords.
    translation_vector <- c(new_x_coords[j] - current_bbox$xmin, new_y_coords[j] - current_bbox$ymin)
    translated_geom <- geom + translation_vector # Apply translation
    sf::st_set_crs(translated_geom, sf::st_crs(geom)) # Preserve CRS after translation
  })

  # --- 5. Rebuild sf objects and Standardize ---
  # Get the target CRS from the main map (M) to ensure consistency.
  target_crs <- sf::st_crs(M)

  # Reconstruct sf objects for each inset, combining the original attributes
  # with the newly scaled and translated geometries.
  bydeler_inset_list <- lapply(seq_along(bydeler), function(j) {
    city_name <- names(bydeler)[j]

    # Get attributes from the original (unscaled/untranslated) bydel sf object.
    # We use sf::st_drop_geometry to get a data.frame of attributes.
    attrs <- sf::st_drop_geometry(bydeler[[city_name]])

    # Create a new sf object by combining these attributes with the translated geometry.
    sf_obj <- sf::st_sf(attrs, geometry = bydeler_translated[[j]])

    # Ensure the CRS of the inset matches the main map's CRS.
    sf::st_transform(sf_obj, crs = target_crs)
  })

  # --- 6. Combine and Finalize ---
  # Combine all individual inset sf objects into a single sf data frame.
  bydeler_inset <- do.call(rbind, bydeler_inset_list)

  # Add an 'inset' column to distinguish between main map features and inset features.
  bydeler_inset$inset <- TRUE
  M$inset <- FALSE # Mark main map features as not being an inset

  # Combine the main map data with the inset map data.
  M_inset <- rbind(M, bydeler_inset)

  return(M_inset)
}



#' Prepare Norway Map Data
#'
#' This function loads geospatial data for Norwegian municipalities (kommuner),
#' city districts (bydeler), and counties (fylker), harmonizes their Coordinate
#' Reference Systems (CRS) and geometry column names, performs spatial intersections
#' to ensure data consistency, filters out specific large municipalities,
#' combines the processed kommune and bydel data, and simplifies the geometries.
#'
#' @param kommune_path A character string. The file path to the GML file containing
#'   kommune (municipality) geographical data. Expected columns: `kommunenummer` (numeric/character),
#'   `kommunenavn` (character), and a geometry column named `omrade`.
#' @param bydel_path A character string. The file path to the GML file containing
#'   bydel (city district) geographical data. Expected columns: `bydelnr` (numeric/character),
#'   `bydelnavn` (character), and a geometry column named `geom`.
#' @param fylker_path A character string. The file path to the GeoJSON file containing
#'   fylke (county) geographical data. Expected columns: `navn` (character),
#'   `nummer` (numeric/character), and a geometry column named `geometry`.
#' @param kommuner_to_exclude A character vector. Names of municipalities to be
#'   excluded from the final map data (e.g., "Oslo", "Bergen").
#' @param simplification_keep_ratio A numeric value between 0 and 1. The proportion
#'   of vertices to keep when simplifying geometries using `rmapshaper::ms_simplify`.
#'   A lower value results in more simplification.
#'
#' @return An `sf` (simple feature) data frame containing the processed and combined
#'   map data for Norway, with harmonized CRS, geometry names, and simplified geometries.
#'   The returned data frame will have columns: `Nr` (municipality/bydel number),
#'   `navn` (municipality/bydel name), `fylkenavn` (county name), `fylkeNR` (county number),
#'   and `geometry` (the spatial geometry).
#' @export
#'
#' @import sf
#' @import data.table
#' @importFrom magrittr %>%
#' @importFrom dplyr select rename distinct
#' @importFrom rmapshaper ms_simplify
#'
#' @examples
#' \dontrun{
#' # Assuming data files are in a 'data-raw' directory relative to your R project
#' # final_map <- prepare_norway_map_data(
#' #   kommune_path = "data-raw/kommune.gml",
#' #   bydel_path = "data-raw/bydel2020_mednavn.gml",
#' #   fylker_path = "data-raw/Fylker.geojson",
#' #   kommuner_to_exclude = c("Oslo", "Bergen", "Stavanger", "Trondheim - Tråante"),
#' #   simplification_keep_ratio = 0.0005
#' # )
#' # plot(final_map)
#' }
create_KommuneBydel_map <- function(kommune_path, bydel_path, fylker_path,
                                    kommuner_to_exclude = c("Oslo", "Bergen", "Stavanger", "Trondheim - Tråante"),
                                    simplification_keep_ratio = 0.0005) {

  # --- 1. Load Geodata ---
  # Read the geospatial data from the specified GML and GeoJSON files.
  # sf::st_read automatically detects the geometry column and CRS.
  kommune_sf <- sf::st_read(kommune_path, quiet = TRUE)
  bydel_sf <- sf::st_read(bydel_path, quiet = TRUE)
  fylker_sf <- sf::st_read(fylker_path, quiet = TRUE)

  # --- 2. Harmonize Geometry Column Names ---
  # It's good practice to have a consistent name for the geometry column,
  # typically 'geometry', especially when combining or manipulating sf objects.
  # We use sf::st_set_geometry to explicitly set the geometry column to 'geometry'.

  # For kommune_sf, the geometry column is 'omrade'. Rename it to 'geometry'.
  kommune_sf <- kommune_sf %>%
    sf::st_set_geometry("omrade") %>% # Set 'omrade' as the active geometry column
    dplyr::rename(geometry = omrade) # Rename the column itself to 'geometry'

  # For bydel_sf, the geometry column is 'geom'. Rename it to 'geometry'.
  bydel_sf <- bydel_sf %>%
    sf::st_set_geometry("geom") %>% # Set 'geom' as the active geometry column
    dplyr::rename(geometry = geom) # Rename the column itself to 'geometry'

  # For fylker_sf, the geometry column is already named 'geometry', so no change is needed.

  # --- 3. Ensure Consistent Coordinate Reference System (CRS) ---
  # It's crucial for spatial operations that all layers share the same CRS.
  # Fylker.geojson is in ETRS89 / UTM zone 33N (a projected CRS), which is often
  # suitable for spatial analysis and plotting in Norway.
  # kommune.gml and bydel2020_mednavn.gml are in WGS 84 (a geographic CRS).
  # We will transform kommune and bydel to match the Fylker CRS.

  # Get the target CRS from the fylker_sf object
  target_crs <- sf::st_crs(fylker_sf)

  # Transform the kommune and bydel simple feature collections to the target CRS
  kommune_sf <- kommune_sf %>% sf::st_transform(crs = target_crs)
  bydel_sf <- bydel_sf %>% sf::st_transform(crs = target_crs)

  # --- 4. Reduce 'kommune' to the Intersection with 'fylke' ---
  # This step clips the geometries of the kommune features to the boundaries of the fylker.
  # sf::st_intersection returns the overlapping parts of the geometries and combines attributes.
  # We select the original 'kommunenummer' and 'kommunenavn' and the new intersected geometry,
  # along with 'navn' and 'nummer' from the fylke layer.
  kommune_intersected <- sf::st_intersection(kommune_sf, fylker_sf) %>%
    dplyr::select(kommunenummer, kommunenavn, geometry, navn, nummer)

  # --- 5. Reduce 'bydel' to the Intersection with 'fylke' ---
  # Similar to the kommune processing, this clips bydel geometries to fylke boundaries.
  bydel_intersected <- sf::st_intersection(bydel_sf, fylker_sf) %>%
    dplyr::select(bydelnr, bydelnavn, geometry, navn, nummer)

  # --- 6. Remove Specific Kommuner ---
  # Filter out the specified kommuner from the processed kommune data.
  kommune_filtered <- kommune_intersected[!kommune_intersected$kommunenavn %in% kommuner_to_exclude, ]

  # --- 7. Combine 'bydel' Map Data with 'kommune' Map Data ---
  # To combine (rbind) the sf objects, their non-geometry column names must be identical.
  # Rename the bydel columns to match the kommune columns.
  bydel_renamed <- bydel_intersected %>%
    dplyr::rename(kommunenummer = bydelnr, kommunenavn = bydelnavn)

  # --- 8. Simplify Geometries ---
  # Reduce the complexity of the geometries for faster plotting and smaller file size.
  NrStorbyer = c(3301,3205,3107,3105,4003,4001,4204,3905)


  kommune_filtered <- rbind(
    rmapshaper::ms_simplify(
      kommune_filtered[!(kommune_filtered$kommunenummer %in% NrStorbyer),],
      keep = simplification_keep_ratio,
      keep_shapes = TRUE),
    rmapshaper::ms_simplify(
      kommune_filtered[kommune_filtered$kommunenummer %in% NrStorbyer,],
      keep = simplification_keep_ratio*100,
      keep_shapes = TRUE)
  )

  bydel_renamed <- rmapshaper::ms_simplify(
    bydel_renamed,
    keep = simplification_keep_ratio*100,
    keep_shapes = TRUE
  )

  # Combine the filtered kommune data and the renamed bydel data.
  # The rbind function from base R works seamlessly with sf objects,
  # as long as column names and types are compatible.
  kommune_bydel <- rbind(kommune_filtered, bydel_renamed)


  # --- 9. Rename Columns for Final Output ---
  # Rename columns to more generic and consistent names for the package output.
  kommune_bydel <- kommune_bydel %>%
    dplyr::rename(
      fylkenavn = navn,
      fylkeNR = nummer,
      Nr = kommunenummer,
      navn = kommunenavn
    )

  kommune_bydel$level = "kommune"
  kommune_bydel$level[kommune_bydel$Nr > 9999] = "bydel"
  kommune_bydel$storby = NA
  kommune_bydel$storby[grepl("^301",as.character(kommune_bydel$Nr))] = "Oslo"
  kommune_bydel$storby[grepl("^1103",as.character(kommune_bydel$Nr)) | kommune_bydel$Nr == "Sandnes"] = "Stavanger/Sandnes"
  kommune_bydel$storby[grepl("^4601",as.character(kommune_bydel$Nr))] = "Bergen"
  kommune_bydel$storby[grepl("^5001",as.character(kommune_bydel$Nr))] = "Trondheim"
  kommune_bydel$storby[kommune_bydel$Nr == 3301] = "Drammen"
  kommune_bydel$storby[kommune_bydel$Nr == 3205] = "Lillestrøm"
  kommune_bydel$storby[kommune_bydel$Nr %in% c(3107,3105)] = "Fredrikstad/Sarpsborg"
  kommune_bydel$storby[kommune_bydel$Nr %in% c(4003,4001)] = "Porsgrunn/Skien"
  kommune_bydel$storby[kommune_bydel$Nr == 4204] = "Kristiansand"
  kommune_bydel$storby[kommune_bydel$Nr == 3905] = "Tønsberg"
  return(kommune_bydel)
}
