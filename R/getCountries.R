#' getCountries
#'
#' This function gets the central Europe country boundaries
#'
#' @import rworldmap
#' @import sp
#' @param usePlot TRUE if the countries should be plotted, false otherwise
#' @return a SpatialPolygonsDataFrame with the countries

getCountries <- function(usePlot = FALSE) {
  countries <- getMap(resolution="low")
  region <- c("Czech Republic","Germany","Poland", "Slovakia")
  cze <- countries[countries$SOVEREIGNT %in% region,]
  cze_utm <- spTransform(cze, CRS("+proj=utm +zone=33"))

  if (usePlot) {
    plot(cze_utm, add=TRUE)
    scalebar(100000, label="100 km")
  }
  return(cze_utm)
}
