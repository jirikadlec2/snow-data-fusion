#' getCountries
#'
#' This function gets the Czechia study area
#'
#' @import rworldmap
#' @import sp
#' @return a SpatialPolygonsDataFrame with the Czechia border

getCzechia <- function() {
  countries <- rworldmap::getMap(resolution="low")
  region <- "Czech Republic"
  cze <- countries[countries$SOVEREIGNT == region,]
  cze_utm <- sp::spTransform(cze, sp::CRS("+proj=utm +zone=33"))
  return(cze_utm)
}
