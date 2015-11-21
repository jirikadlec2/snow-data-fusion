#' getCloudCover
#'
#' This function gets the cloud cover inside of study area
#'
#' @import raster
#' @import rgdal
#' @import sp
#' @param modis The reclassified modis raster object
#' @param studyArea the study area polygon
#' @return the percentage of cloud cover inside study area

getCloudCover <- function(modis, studyArea) {

  modisStudy <- mask(modis, studyArea)
  modisStudyCloud <- modisStudy == 2
  modisStudyAll <- !is.na(modisStudy)
  cloudPercent <- cellStats(modisStudyCloud, sum) / cellStats(modisStudyAll, sum)
  return(cloudPercent * 100)
}
