#' sampleModisPoints
#'
#' This function gets a sample of MODIS points (to allow for least-cost to MODIS)
#'
#' @import sp
#' @import raster
#' @param sampleFromCategory category to sample from. Use 0 for absent and 1 for present
#' @param pixelsPerPoint number of pixels represented by one sample point
#' @return the spatialPoints object with sampled MODIS points


sampleModisPoints <- function(modis, sampleFromCategory, pixelsPerPoint=400) {

  modis[modis != sampleFromCategory] <- NA
  validCells <- length(which(!is.na(values(modis))))
  nPoints <- floor(validCells / pixelsPerPoint)
  if (nPoints == 0) {
    print(paste("nPoints:", nPoints))
    return(data.frame())
  } else {
    samplePoints <- sampleRandom(modis, nPoints, sp=TRUE)
    return(samplePoints)
  }
}
