#' Create an Interactive Leaflet Map of Median Income
#'
#' This function takes a spatial data frame and generates an interactive,
#' zoomable map using the leaflet package, styled similarly to the ggplot2 version.
#'
#' @param sf_data An sf object containing the map data. Must include columns
#'   "med_inc" for median income, "N" for population/weight, and a column
#'   with area names (e.g., "kommunenavn" or "navn").
#' @param fill Variable used for fill colors (e.g., "med_inc").
#' @param name_col The name of the column to use for popups, as a string.
#' @param display_digits Numeric. The number of digits to display for the income
#'   values in popups and the legend. This is passed to the `accuracy` argument
#'   of `scales::label_number`. For integer values, use `1`. For one decimal place, use `0.1`, etc.
#'
#' @return A leaflet map widget.
#'
#' @importFrom leaflet leaflet addProviderTiles addPolygons addLegend addControl
#' @importFrom leaflet colorNumeric highlightOptions providers labelOptions
#' @importFrom sf st_transform
#' @importFrom stats weighted.mean
#' @importFrom scales label_number
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming 'm' is your sf data frame from the previous steps
#'   # and contains the necessary columns.
#'   # Example with dummy data for illustration
#'   library(sf)
#'   library(dplyr)
#'   library(leaflet)
#'   library(scales)
#'
#'   # Create some dummy sf data
#'   set.seed(123)
#'   dummy_sf_data <- st_as_sf(data.frame(
#'     id = 1:5,
#'     navn = c("Area A", "Area B", "Area C", "Area D", "Area E"),
#'     med_inc = c(50000.12, 65234.56, 42123.78, 78901.23, 55000.00),
#'     N = c(1000, 1500, 800, 2000, 1200),
#'     geometry = st_sfc(
#'       st_polygon(list(cbind(c(0,0,1,1,0),c(0,1,1,0,0)))),
#'       st_polygon(list(cbind(c(1,1,2,2,1),c(0,1,1,0,0)))),
#'       st_polygon(list(cbind(c(0,0,1,1,0),c(1,2,2,1,1)))),
#'       st_polygon(list(cbind(c(1,1,2,2,1),c(1,2,2,1,1)))),
#'       st_polygon(list(cbind(c(2,2,3,3,2),c(0,1,1,0,0))))
#'     )
#'   ), crs = 4326)
#'
#'   # Create map with default digit display (integer)
#'   map1 <- leaflet_map(dummy_sf_data, name_col = "navn")
#'   print(map1)
#'
#'   # Create map showing one decimal place
#'   map2 <- leaflet_map(dummy_sf_data, name_col = "navn", display_digits = 0.1)
#'   print(map2)
#'
#'   # Create map showing no decimal places (rounding to nearest thousand)
#'   map3 <- leaflet_map(dummy_sf_data, name_col = "navn", display_digits = 1000)
#'   print(map3)
#' }
leaflet_map <- function(sf_data, fill = "med_inc", name_col = "navn", display_digits = 1) {

  # --- 1. CRS Transformation ---
  # Leaflet requires data in WGS 84 (EPSG:4326) for plotting.
  # We transform the data to the correct CRS to prevent warnings.
  sf_data <- sf::st_transform(sf_data, crs = 4326)

  # --- 2. Data Preparation ---
  # Leaflet's color functions don't have a 'transform' argument, so we apply
  # the log transformation to the data directly. We add a small amount to
  # avoid log(0) issues if any income is zero.
  # Ensure the fill column is treated as numeric for log transformation
  sf_data$log_value <- log(as.numeric(sf_data[[fill]]) + 1) # Add 1 to avoid log(0)

  # Calculate the midpoint on the original, non-transformed data
  mid_point <- stats::weighted.mean(as.numeric(sf_data[[fill]]), w = sf_data$N, na.rm = TRUE)

  # We need to find the log-transformed value that corresponds to our midpoint
  log_mid_point <- log(mid_point + 1) # Add 1 for consistency with log_value

  # Define the number formatter using scales::label_number with the new display_digits
  number_formatter <- scales::label_number(big.mark = " ", accuracy = display_digits)

  # Pre-generate the popup HTML content to avoid NSE issues.
  # Use the formatter for the fill value
  sf_data$popup_content <- paste(
    "<b>", sf_data[[name_col]], "</b><br/>",
    fill, ": ", number_formatter(as.numeric(sf_data[[fill]]))
  )

  # Pre-generate the label content for hover events.
  sf_data$hover_label <- sf_data[[name_col]]

  # --- 3. Create the Diverging Color Palette ---
  # We create a palette function that maps the log-transformed data to colors.
  # The domain is the range of the log-transformed values.
  pal <- leaflet::colorNumeric(
    palette = c("red", "white", "green3"),
    domain = sf_data$log_value,
    na.color = "transparent"
  )

  # --- 4. Create the Leaflet Map ---
  map <- leaflet::leaflet(data = sf_data) %>%
    # Add a clean, light base map
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%

    # Add the polygon layers
    leaflet::addPolygons(
      fillColor = ~pal(log_value), # Use the palette on the log-transformed data
      fillOpacity = 0.7,
      color = "#333333", # Border color
      weight = 1,
      popup = ~popup_content, # Use the pre-generated popup content
      label = ~hover_label,    # Add labels that appear on hover
      labelOptions = leaflet::labelOptions( # Style the hover labels
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      ),
      # Highlight polygons on mouse hover
      highlightOptions = leaflet::highlightOptions(
        color = "white", weight = 2, bringToFront = TRUE
      )
    ) %>%

    # Add the legend
    leaflet::addLegend(
      pal = pal,
      values = ~log_value, # Values are also from the log-transformed data
      opacity = 0.7,
      title = fill,
      position = "bottomright",
      # Custom function to format legend labels back to original scale
      labFormat = function(type, cuts, p) {
        # 'cuts' are the log-scaled values for the legend breaks.
        # We transform them back to the original scale for display.
        # Subtract 1 to reverse the log(value + 1) transformation
        original_labels <- exp(cuts) - 1
        # Use the same formatter as for popups
        paste0(number_formatter(original_labels))
      }
    ) %>%

    # Add a title to the map
    leaflet::addControl(
      "<h3>Kommuner og Bydeler</h3>",
      position = "topright"
    )

  return(map)
}
