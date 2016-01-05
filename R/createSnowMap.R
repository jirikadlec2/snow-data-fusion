 #' createSnowMap
#'
#' This function gets the snow probability based on the other data
#'
#' @import raster
#' @param originalDate The snow map date
#' @return the snow probability raster

createSnowMap <- function(originalDate) {

  # study area definition
  czechia <- getCzechia()
  cz_extent <- extent(czechia)
  
  modis <- getModis()
  originalR <- reclassModis(original)
  
  # study area definition
  stations <- getStations(originalDate)
  reports <- getReports(originalDate)
  tracks <- getTracks(originalDate)

}
