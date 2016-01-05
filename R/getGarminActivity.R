#' getGarminActivity
#'
#' This function gets the activity from Garmin
#'
#' @import XML
#' @import httr
#' @import sp
#' @import plyr
#' @param activity_id The unique ID of the Garmin activity
#' @return a SpatialPointsDataFrame with the activity track.

getGarminActivity <- function(activity_id) {
  url <- paste("https://connect.garmin.com/proxy/activity-service-1.1/tcx/activity/",
               activity_id, "?full=true", sep="")
  my_file <- paste(folder, "/", activity_id, ".tcx", sep="")
  GET(url, write_disk(my_file, overwrite=TRUE))
  Sys.sleep(3)
  
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