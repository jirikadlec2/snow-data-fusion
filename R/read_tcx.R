library(XML)
library(sp)
library(rgdal)

##############################################
# REDOWNLOAD and activity
##############################################
getActivity <- function(folder, activity_id) {
  url <- paste("https://connect.garmin.com/proxy/activity-service-1.1/tcx/activity/",
               activity_id, "?full=true", sep="")
  my_file <- paste(folder, "/", activity_id, ".tcx", sep="")
  GET(url, write_disk(my_file, overwrite=TRUE))
  Sys.sleep(3)
}

##############################################
# READ a single TCX file
##############################################
parseTcx <- function(my_file) {
  doc <- xmlParse(my_file)

  times <- xpathSApply(doc, "//ns:Time", xmlValue, namespaces="ns")
  latitudes <- xpathSApply(doc, "//ns:LatitudeDegrees", xmlValue, namespaces="ns")
  longitudes <- xpathSApply(doc, "//ns:LongitudeDegrees", xmlValue, namespaces="ns")

  #make line
  lin <- Line(cbind(as.numeric(longitudes), as.numeric(latitudes)))
  LS <- Lines(list(lin), ID = my_file)
  SL <- SpatialLines(list(LS))
  SLDF <- SpatialLinesDataFrame(SL, data.frame(tcx=tcx.file, begtime=times[1],
                                               endtime=times[length(times)], row.names=my_file))

  return(SLDF)
}

##############################################
# READ multiple TCX files
##############################################
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



setwd("C:/Temp/data/garmin")

tcx.files <- list.files(".", "*.tcx")
tcx.shape <- parseTcxFiles(tcx.files)
tcx.shape$begdate <- as.POSIXct(tcx.shape$begtime)
tcx.shape$enddate <- as.POSIXct(tcx.shape$endtime)
writeOGR(tcx.shape, ".", "garmin_tracks_all2", driver="ESRI Shapefile")
