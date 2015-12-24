#########################################################
# ELEVATION statistics of the GARMIN and STRAVA tracks  #
#########################################################
library(sp)
library(rgdal)
library(raster)
library(XML)
setwd("C:/jiri/Dropbox/PHD/crowdsourcing/data/garmin")


parseTcxElevations <- function(my_file) {
  doc <- xmlParse(my_file)
  elevations <- as.numeric(xpathSApply(doc, "//ns:AltitudeMeters", xmlValue, namespaces="ns"))
  return(elevations)
}


garmin_tracks <- readOGR(".", "garmin_tracks_2012-2015", stringsAsFactors=FALSE)
garmin_tracks$mean_elev <- NA
all_elevations <- c()

setwd("C:/temp/data/garmin")
i <- 1
for(i in 1:nrow(garmin_tracks)) {
  print(i)

  tcx <- garmin_tracks$tcx[i]
  elevs <- parseTcxElevations(tcx)
  all_elevations <- c(all_elevations, elevs)
  mean_elev <- mean(elevs)
  print(mean_elev)
  garmin_tracks$mean_elev[i] <- mean_elev
}

dem <- raster("C:/jiri/Dropbox/PHD/crowdsourcing/data/dem/srtm_500m.tif")
plot(dem)


garmin_utm <- spTransform(garmin_tracks, CRS("+proj=utm +zone=33"))
plot(garmin_utm, add=TRUE)

plot(garmin_utm)

track1 <- garmin_utm[1,]
plot(track1)
obj <- track1@lines[1][[1]]@Lines[[1]]@coords
