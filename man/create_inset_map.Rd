% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{create_inset_map}
\alias{create_inset_map}
\title{Create Inset Map for Norwegian Cities}
\usage{
create_inset_map(
  M,
  scaling_params = c(Oslo = 2.5, Bergen = 3.5, `Stavanger/Sandnes` = 3.5, Trondheim =
    3.5),
  x_offset_params = c(Oslo = 0.005, Bergen = 0.9, `Stavanger/Sandnes` = 1, Trondheim =
    0.7),
  y_offset_params = c(Oslo = 0.005, Bergen = 0.5, `Stavanger/Sandnes` = 0, Trondheim =
    0.8)
)
}
\arguments{
\item{M}{An \code{sf} (simple feature) data frame representing the main map of Norway.
It is expected to contain columns like \code{Nr}, \code{navn}, \code{fylkenavn}, \code{fylkeNR},
\code{storby} (indicating if a feature belongs to a major city), and \code{level}
(e.g., "kommune" or "bydel"). The \code{geometry} column is also required.}

\item{scaling_params}{A named numeric vector. Specifies the scaling factor for
each city's inset map relative to the main map's bounding box. Higher values
result in smaller insets (less scaling). Names should correspond to city names
(e.g., "Oslo", "Bergen", "Stavanger/Sandnes", "Trondheim").}

\item{x_offset_params}{A named numeric vector. Specifies the x-offset as a
proportion of the main map's width for positioning the inset maps. Values
between 0 and 1. Names should correspond to city names.}

\item{y_offset_params}{A named numeric vector. Specifies the y-offset as a
proportion of the main map's height for positioning the inset maps. Values
between 0 and 1. Names should correspond to city names.}
}
\value{
An \code{sf} data frame that combines the original map \code{M} with the
generated inset maps. A new logical column \code{inset} is added, which is \code{TRUE}
for inset features and \code{FALSE} for main map features.
}
\description{
This function generates an \code{sf} object that includes the main map data (\code{M})
along with scaled and translated inset maps for specific major Norwegian cities
(Oslo, Bergen, Stavanger/Sandnes, Trondheim). The inset maps display the city
districts (bydeler) of these cities at an enlarged scale and positioned in
a corner of the overall map bounding box.
}
\examples{
\dontrun{
# Assuming MapKommunerBydeler is loaded (e.g., from package data)
# data(MapKommunerBydeler)

# Ensure 'storby' and 'level' columns exist in MapKommunerBydeler for this example
# MapKommunerBydeler$storby <- NA
# MapKommunerBydeler$level <- "kommune"
# MapKommunerBydeler[MapKommunerBydeler$navn \%in\% c("Oslo", "Bergen", "Stavanger", "Trondheim"), "storby"] <- MapKommunerBydeler[MapKommunerBydeler$navn \%in\% c("Oslo", "Bergen", "Stavanger", "Trondheim"), "navn"]
# MapKommunerBydeler[MapKommunerBydeler$navn \%in\% c("Oslo", "Bergen", "Stavanger", "Trondheim"), "level"] <- "bydel" # Placeholder for actual bydeler
# MapKommunerBydeler[MapKommunerBydeler$navn == "Stavanger", "storby"] <- "Stavanger/Sandnes"
# MapKommunerBydeler[MapKommunerBydeler$navn == "Trondheim", "storby"] <- "Trondheim"

# Example usage with default parameters
# map_with_insets <- create_inset_map(M = MapKommunerBydeler)
# plot(map_with_insets, col = ifelse(map_with_insets$inset, "red", "grey"), border = "black")

# Example with custom scaling and offsets
# custom_scaling <- c(Oslo = 2.0, Bergen = 3.0, `Stavanger/Sandnes` = 3.0, Trondheim = 3.0)
# custom_x_offset <- c(Oslo = 0.01, Bergen = 0.85, `Stavanger/Sandnes` = 0.95, Trondheim = 0.75)
# custom_y_offset <- c(Oslo = 0.01, Bergen = 0.45, `Stavanger/Sandnes` = 0.05, Trondheim = 0.75)
# map_with_custom_insets <- create_inset_map(
#   M = MapKommunerBydeler,
#   scaling_params = custom_scaling,
#   x_offset_params = custom_x_offset,
#   y_offset_params = custom_y_offset
# )
# plot(map_with_custom_insets, col = ifelse(map_with_custom_insets$inset, "blue", "lightgrey"))
}
}
