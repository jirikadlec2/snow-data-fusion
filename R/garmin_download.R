##############################################
# STRAVA GPX Exporter Library                #
##############################################

library(httr)
library(sp)
library(rgdal)
library(XML)

parseTcx <- function(my_file) {
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

############################################################
getActivity <- function(folder, activity_id) {
  #activity_id <- 269583360
  url <- paste("https://connect.garmin.com/proxy/activity-service-1.1/tcx/activity/",
               activity_id, "?full=true", sep="")
  my_file <- paste(folder, "/", activity_id, ".tcx", sep="")
  GET(url, write_disk(my_file, overwrite=TRUE))
  Sys.sleep(3)
}
############################################################

############################################################
# HTML parsing for STRAVA activities
############################################################
#activities_file <- download.file(destfile="activities.html",
#url="https://www.strava.com/activities/search?activity_type=NordicSki&city=Prague&country=Czech+Republic&distance_end=200&distance_start=0&elev_gain_end=15000&elev_gain_start=0&keywords=&lat_lng=50.0755381%2C14.43780049999998&location=Praha&page=1&state=Hlavn%C3%AD+m%C4%9Bsto+Praha&time_end=10.0&time_start=0.0&type=&utf8=%E2%9C%93")

download_garmin_activities <- function (download_folder, activities_file) {
  activities <- read.table(activities_file, stringsAsFactors=FALSE)

  i <- 1
  for (i in 1: nrow(activities)) {
    print(i)
    act_url <- activities$V1[i]
    activity_id <- substr(act_url, 37, nchar(act_url))
    activity_file <- paste(activity_id, ".tcx", sep="")
    getActivity(download_folder, activity_id)
  }
}

#run this code:
download_garmin_activities(activities_file = "C:/dev/github/garmin-client/garmin_tracks2.txt",
                           download_folder = "C:/temp/data/garmin")
