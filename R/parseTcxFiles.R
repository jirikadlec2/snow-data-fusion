#' parseTcxFiles
#'
#' This function parses all of the Tcx files
#' and creates a combined 'tracks' shapefile
#'
#' @import XML
#' @import httr
#' @import sp
#' @import plyr
#' @param my_files The list of the track tcx files
#' @return a SpatialPointsDataFrame with the activity tracks.

parseTcxFiles <- function(my_files) {
  
  line.list <- list()
  begtimes <- NULL
  endtimes <- NULL
  actids <- NULL
  for (my_file in my_files) {
    parsed <- FALSE
    err <- tryCatch({
      doc <- xmlParse(my_file)
      parsed = TRUE
    }, error = function(e) {
      print(conditionMessage(e))
    })
    
    if (!parsed) {
      activity_id = substr(my_file, 1, nchar(my_file) - 4)
      getActivity(".", activity_id)
      err <- tryCatch({
        doc <- xmlParse(my_file)
        parsed = TRUE
      }, error = function(e) {
        print(conditionMessage(e))
      })
    }
    
    if (!parsed) {
      next
    }
    
    times <- xpathSApply(doc, "//ns:Time", xmlValue, namespaces="ns")
    latitudes <- xpathSApply(doc, "//ns:LatitudeDegrees", xmlValue, namespaces="ns")
    longitudes <- xpathSApply(doc, "//ns:LongitudeDegrees", xmlValue, namespaces="ns")
    
    if (length(latitudes) == 0) {
      print(paste(my_file, "no data in gps file!"))
      next
    }
    
    
    #make line
    lin <- Line(cbind(as.numeric(longitudes), as.numeric(latitudes)))
    
    #add line to list
    LS <- Lines(list(lin), ID = my_file)
    
    line.list <- c(line.list, LS)
    begtimes <- c(begtimes, times[1])
    endtimes <- c(endtimes, times[1])
    actids <- c(actids, my_file)
  }
  
  SL <- SpatialLines(line.list)
  SLDF <- SpatialLinesDataFrame(SL, data.frame(tcx=actids, begtime=begtimes,
                                               endtime=endtimes, row.names=actids))
  return(SLDF)
}