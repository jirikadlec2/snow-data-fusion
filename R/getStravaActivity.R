#' getStravaActivity
#'
#' This function gets the activity from Strava
#'
#' @import XML
#' @import httr
#' @import sp
#' @import plyr
#' @param activity_id The unique ID of the Strava activity
#' @return a SpatialPointsDataFrame with the activity track.
#' it has columns lat, lon, snodep and present.
#' present is 0 for bare ground, 1 for snow (no data are excluded)
 
getStravaActivity <- function(activity_id) {

  url <- paste("http://raceshape.com/strava.export.php?ride=",
               activity_id, "&type=tcx", sep="")
  my_file <- paste(activity_id, ".tcx", sep="")
  GET(url, write_disk(my_file, overwrite=TRUE))
  Sys.sleep(30)
  GET(url, write_disk(my_file, overwrite=TRUE))
  
  doc <- xmlParse(my_file)
  nodes <- getNodeSet(doc, "//ns:Trackpoint", "ns")
  mydf  <- plyr::ldply(nodes, as.data.frame(xmlToList))
  names(mydf) <- c("time", "lat", "lon", "elev", "dist")
  mydf$lat <- as.numeric(as.character(mydf$lat))
  mydf$lon <- as.numeric(as.character(mydf$lon))
  coordinates(mydf) <- ~lon+lat
  plot(mydf, pch=16, col="red")
  return(mydf)
}