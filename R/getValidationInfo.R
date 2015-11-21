#' getValidationInfo
#'
#' This function checks the number of tracks, reports, cloud cover
#' for each validation date
#'
#' @import raster
#' @import rgdal
#' @param selectedDate the date tested
#' @param studyArea the study area 'mask' to be imposed
#' @param dataFolder the folder where the data is located
#' @return a named vector with info

getValidationInfo <- function(selectedDates, studyArea, dataFolder=".") {

  resultInfo <- data.frame()

  for(selectedDate in selectedDates) {

    originalFile <- paste(dataFolder, "/", "modis", selectedDate, ".tif", sep="")

    original <- raster(originalFile)
    originalR <- reclassModis(original)

    stations <- getStations(selectedDate)
    reports <- getReports(selectedDate)
    tracks <- getTracks(selectedDate)

    Nstations <- nrow(stations)
    Nreports <- nrow(reports)
    Ntracks <- nrow(tracks)
    PercCloud <- getCloudCover(originalR, studyArea)

    result <- data.frame(selectedDate, PercCloud, Nstations, Nreports, Ntracks)

    resultInfo <- rbind(resultInfo, result)
  }
  return(resultInfo)
}
