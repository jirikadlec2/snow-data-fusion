#' imposeCloud
#'
#' This function imposes the cloud on an existing modis data set
#'
#' @import raster
#' @import rgdal
#' @param original the original Modis raster
#' @param cloudy The cloudy Modis raster
#' to be imposed
#' @return the percentage of cloud cover inside study area

imposeCloud <- function(original, cloudy) {

  rImpose <- cloudy == 2
  rImpose[rImpose == 1] <- NA
  rOrigMasked <- mask(original, mask=rImpose)
  rOrigMasked[is.na(rOrigMasked)] <- 2
  return(rOrigMasked)
}
