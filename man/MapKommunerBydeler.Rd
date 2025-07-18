% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{MapKommunerBydeler}
\alias{MapKommunerBydeler}
\title{MapKommunerBydeler: Simplified Map Data for Norwegian Municipalities and City Districts}
\format{
A \code{sf} data frame with 391 features and 5 fields:
\describe{
\item{Nr}{Character or numeric. The official municipality (kommunenummer) or
city district (bydelnr) code.}
\item{fylkenavn}{Character. The name of the county (fylke) to which the
municipality or city district belongs.}
\item{navn}{Character. The official name of the municipality or city district.}
\item{fylkeNR}{Character or numeric. The official county (fylkenummer) code.}
\item{geometry}{\code{sfc_GEOMETRY}. The spatial geometry of the feature,
which can be \code{MULTIPOLYGON} or \code{POLYGON}.}
\item{storby}{Character. The name of a large city.}
\item{level}{Character. Indicates whether the feature represents a "kommune"
or a "bydel". (Note: Based on the provided \code{head()} output, this column
\code{level} is not explicitly shown, but it's a common and useful addition
when combining different administrative levels. If this column is not
generated by \code{prepare_norway_map_data}, it should be added or this
description adjusted.)}
}
}
\source{
Data derived from Geonorge (kommune.gml, bydel2020_mednavn.gml) and
Statistisk sentralbyrå (SSB) (Fylker.geojson). Data are from 2024.
}
\usage{
MapKommunerBydeler
}
\description{
A simplified \code{sf} (simple feature) data frame containing the geographical boundaries
for Norwegian municipalities (kommuner) and selected city districts (bydeler),
excluding the main municipalities of Oslo, Bergen, Stavanger, and Trondheim
(as their bydeler are included instead). The geometries are simplified for
faster plotting and smaller file size. This dataset is intended for use
as a base map for visualizing data at the municipality or city district level in Norway.
}
\details{
The data has been processed to:
\itemize{
\item Harmonize Coordinate Reference Systems (CRS) to ETRS89 / UTM zone 33N.
\item Ensure consistent geometry column naming (\code{geometry}).
\item Perform spatial intersections to clip municipalities and city districts
to county boundaries.
\item Exclude the main polygons for Oslo, Bergen, Stavanger, and Trondheim,
as their city districts are included instead.
\item Simplify geometries using \code{rmapshaper::ms_simplify} with \code{keep = 0.0005}
for reduced file size and faster rendering.
\item Rename columns to \code{Nr}, \code{navn}, \code{fylkenavn}, \code{fylkeNR} for consistency.
}
}
\examples{
\dontrun{
# To load this data (assuming it's part of your package data)
# data(MapKommunerBydeler)
# plot(MapKommunerBydeler)

# Example of plotting a subset (e.g., a specific county)
# library(dplyr)
# library(ggplot2)
# MapKommunerBydeler \%>\%
#   filter(fylkenavn == "Troms") \%>\%
#   ggplot() +
#   geom_sf() +
#   labs(title = "Municipalities and Bydeler in Troms")
}
}
\seealso{
\code{\link{create_KommuneBydel_map}} for the function used to generate this data.
}
\keyword{datasets}
