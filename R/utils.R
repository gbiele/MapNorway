#' Load and Process City District (Bydel) Data
#'
#' This function reads a spatial data file containing city district boundaries,
#' filters them for the municipalities of Oslo, Bergen, Stavanger, and
#' Trondheim, and cleans up the data.
#'
#' @param filename Character string. The full path to the input file
#'   containing the city district data (e.g., "data-raw/bydel2020_mednavn.gml").
#'
#' @details The input file is expected to be a spatial data file readable by
#'   `sf::st_read()` (e.g., GML, GeoPackage, Shapefile). The data must contain
#'   at least the following columns:
#'   \itemize{
#'     \item \code{bydelnr}: A character vector with the official district
#'           number. The function uses this to identify districts belonging to
#'           Oslo (starting with "03"), Bergen ("46"), Stavanger ("11"), and
#'           Trondheim ("50").
#'     \item \code{bydelnavn}: A character vector with the name of the district.
#'           Used to identify and remove the "Marka" district from Oslo.
#'   }
#'
#' @return A named list of `sf` objects. Each element of the list corresponds
#'   to a city (Oslo, Bergen, Stavanger, Trondheim) and contains the `sf`
#'   data frame for its districts.
#'
#' @importFrom sf st_read
#' @export
#'
#' @examples
#' \dontrun{
#'   # This example requires a specific data file in the correct format.
#'   # Make sure the file is in your working directory or provide the full path.
#'   bydeler_data <- load_and_process_bydeler("data-raw/bydel2020_mednavn.gml")
#'
#'   # View the districts for Oslo
#'   print(bydeler_data$Oslo)
#'
#'   # Plot the districts for Bergen
#'   plot(st_geometry(bydeler_data$Bergen))
#' }
load_bydeler <- function(filename) {

  # Check if the file exists before trying to read it
  if (!file.exists(filename)) {
    stop("File not found: ", filename)
  }

  # Read the spatial data from the file
  bydeler_raw <- sf::st_read(filename)

  # Create a list, filtering districts for each major city based on 'bydelnr'
  bydeler <- list(
    Oslo = bydeler_raw[grepl("^03", bydeler_raw$bydelnr), ],
    Bergen = bydeler_raw[grepl("^46", bydeler_raw$bydelnr), ],
    Stavanger = bydeler_raw[grepl("^11", bydeler_raw$bydelnr), ],
    Trondheim = bydeler_raw[grepl("^50", bydeler_raw$bydelnr), ]
  )

  # Simplify the data for each city, keeping only name and number columns
  bydeler <- lapply(bydeler, function(b) {
    b[, c("bydelnavn", "bydelnr")]
  })

  # Specific data cleaning: Remove the "Marka" district from Oslo, as it is
  # a large, mostly uninhabited forest area and not a typical city district.
  if ("Oslo" %in% names(bydeler) && "bydelnavn" %in% names(bydeler$Oslo)) {
    bydeler[["Oslo"]] <- bydeler[["Oslo"]][bydeler[["Oslo"]]$bydelnavn != "Marka", ]
  }

  # Return the final processed list
  return(bydeler)
}
