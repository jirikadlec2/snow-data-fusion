 #' createSnowMap
#'
#' This function gets the snow probability based on the other data
#'
#' @import raster
#' @param originalDate The snow map date
#' @param threshold The probability threshold (values below are shown as 'no data')
#' @return the snow probability raster

createSnowMap <- function(originalDate, threshold) {

  # study area definition
  czechia <- getCzechia()
  cz_extent <- extent(czechia)
  extent_ll <- c(12.0, 48.5, 18.9, 51.1)

  modis_file <- getModis(originalDate, extent_ll[1], extent_ll[2], extent_ll[3], extent_ll[4])
  modis <- raster(modis_file)
  modisR <- reclassModis(modis)

  # stations, reports, tracks
  stations <- spTransform(getStations(originalDate), CRS("+proj=utm +zone=33"))
  reports <- getReports(originalDate)
  tracks <- getTracks(originalDate)

  # probability map calculation
  snowProb <- getSnowProbability(modisR, stations, reports, tracks)

  # to save the raster
  probFile <- paste("C:/jiri/Dropbox/PHD/crowdsourcing/data/output/", originalDate, ".tif", sep="")
  writeRaster(snowProb, probFile)

  snowProb50 <- snowProb
  snowProb50[snowProb50 < threshold] <- NA

  snowFile50 <- paste("C:/jiri/Dropbox/PHD/crowdsourcing/data/output/snowprob_", originalDate, ".tif", sep="")
  writeRaster(snowProb50, snowFile50)

  plot(snowProb50)
  return(snowProb50)
}
