#' reclassModis
#'
#' This function reclassifies the MODIS raster to 0 (bare), 1 (snow), 2 (nodata)
#'
#' @import raster
#' @param modis The original modis raster object
#' @return the reclassified modis raster object

reclassModis <- function(modis) {
  #assign 201 to snowy pixels
  modis[modis > 100] <- 201

  #assign 200 to bare pixels
  modis[modis == 100] <- 200
  modis[modis == 0] <- 200

  #assign 202 to cloudy pixels
  modis[modis < 200] <- 202

  #reclass to 0, 1, 2
  modis <- modis - 200
  return(modis)
}
