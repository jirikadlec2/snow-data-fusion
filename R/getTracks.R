#' getTracks
#'
#' This function gets the tracks reports strava / garmin
#' @import httr
#' @import rgdal
#' @import sp
#' @param selected.date The date of the track observations.
#' If NULL then all tracks are returned.
#' @param UTM if TRUE then transform output to WGS84 UTM Zone 33 N
#' @return a SpatialPointsDataFrame with the tracks.

getTracks <- function(selected.date=NULL, UTM=TRUE) {

  strava_tracks <- data.frame()
  garmin_tracks <- data.frame()

  new_track_file <- "strava_tracks_saved3.shp"
  if (!file.exists(new_track_file)) {
    resource_id <- "fd64576da2374abaab78a0a4d9d9f3cd"
    resource_uri <- paste("http://hydroshare.org/hsapi/resource/", resource_id, sep="")
    track_file <- "strava.zip"
    GET(resource_uri, write_disk(track_file, overwrite=TRUE))
    res <- unzip(track_file)
    res.shp <- grepl("*.shp", res)
    shpfile <- res[res.shp]
    shpfolder <- substr(shpfile, 1, nchar(shpfile) - nchar(basename(shpfile)) - 1)
    shpname <- substr(basename(shpfile), 1, nchar(basename(shpfile)) - 4)
    strava_tracks <- readOGR(shpfolder, shpname, stringsAsFactors = FALSE, verbose = FALSE)
    writeOGR(strava_tracks, ".", "strava_tracks_saved3", driver="ESRI Shapefile")
  } else {
    strava_tracks <- readOGR(".", "strava_tracks_saved3", stringsAsFactors = FALSE, verbose = FALSE)
  }
  if (!is.null(selected.date)) {
    Ntracks <- length(which(strava_tracks$begdate == selected.date))
  } else {
    Ntracks <- nrow(strava_tracks)
  }

  if (Ntracks > 0) {
    if (!is.null(selected.date)) {
      strava_tracks <- strava_tracks[strava_tracks$begdate == selected.date,]
    }
  } else {
    strava_tracks <- data.frame()
  }

  #changing Strava track ID's
  if (nrow(strava_tracks) > 0) {
    ids <- c()
    for(i in 1: nrow(strava_tracks)) {
      ids <- c(ids, strava_tracks@lines[[i]]@ID)
    }
    new_ids <- as.character(as.numeric(ids) + 50000)
    strava_tracks <- spChFIDs(strava_tracks, new_ids)
  }

  garmin_track_file <- "garmin_tracks_saved3.shp"
  if (!file.exists(garmin_track_file)) {
    resource_id <- "da453fdbd4e54876b4dc2be2b7ca4e00"
    resource_uri <- paste("http://hydroshare.org/hsapi/resource/", resource_id, sep="")
    track_file <- "garmin.zip"
    GET(resource_uri, write_disk(track_file, overwrite=TRUE))
    res <- unzip(track_file)
    res.shp <- grepl("*.shp", res)
    shpfile <- res[res.shp]
    shpfolder <- substr(shpfile, 1, nchar(shpfile) - nchar(basename(shpfile)) - 1)
    shpname <- substr(basename(shpfile), 1, nchar(basename(shpfile)) - 4)
    garmin_tracks <- readOGR(shpfolder, shpname, stringsAsFactors = FALSE)
    writeOGR(garmin_tracks, ".", "garmin_tracks_saved3", driver="ESRI Shapefile")
  } else {
    garmin_tracks <- readOGR(".", "garmin_tracks_saved3", stringsAsFactors=FALSE, verbose = FALSE)
  }
  if (is.null(selected.date)) {
    NtracksGarmin <- nrow(garmin_tracks)
  } else {
    NtracksGarmin <- length(which(garmin_tracks$begdate == selected.date))
  }
  if (NtracksGarmin > 0) {
    if (!is.null(selected.date)){
      garmin_tracks <- garmin_tracks[garmin_tracks$begdate == selected.date,]
    }
  } else {
    garmin_tracks <- data.frame()
  }

  # combining them together (garmin+strava)
  out_tracks_combined <- data.frame()
  if (nrow(strava_tracks) > 0) {
    strava_tracks$date <- NA
    strava_tracks$datetime <- NA
    strava_tracks$month <- NA
    strava_tracks$weekday <- NA
  }
  if (nrow(garmin_tracks) > 0 & nrow(strava_tracks) > 0) {
    out_tracks_combined <- rbind(garmin_tracks, strava_tracks)
  } else {
    if(nrow(garmin_tracks) > 0) {
      out_tracks_combined <- garmin_tracks
    } else {
      out_tracks_combined <- strava_tracks
    }
  }

  # reprojection if needed
  if (UTM & nrow(out_tracks_combined) > 0) {
    out_tracks_combined <- spTransform(out_tracks_combined, CRS("+proj=utm +zone=33"))
  }
  return(out_tracks_combined)
}
