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

  out_tracks_strava <- data.frame()
  out_tracks_garmin <- data.frame()

  new_track_file <- "strava_tracks_saved2.shp"
  if (!file.exists(new_track_file)) {
    resource_id <- "7f1c480ef2c44657a0918c8207bdf662"
    resource_uri <- paste("http://hydroshare.org/hsapi/resource/", resource_id, sep="")
    track_file <- "strava.zip"
    GET(resource_uri, write_disk(track_file, overwrite=TRUE))
    res <- unzip(track_file)
    res.shp <- grepl("*.shp", res)
    shpfile <- res[res.shp]
    shpfolder <- substr(shpfile, 1, nchar(shpfile) - nchar(basename(shpfile)) - 1)
    shpname <- substr(basename(shpfile), 1, nchar(basename(shpfile)) - 4)
    tracks <- readOGR(shpfolder, shpname, stringsAsFactors = FALSE, verbose = FALSE)
    writeOGR(tracks, ".", "strava_tracks_saved2", driver="ESRI Shapefile")
  } else {
    tracks <- readOGR(".", "strava_tracks_saved2", stringsAsFactors = FALSE, verbose = FALSE)
  }
  if (!is.null(selected.date)) {
    Ntracks <- length(which(tracks$begdate == selected.date))
  } else {
    Ntracks <- nrow(tracks)
  }

  if (Ntracks > 0) {
    if (!is.null(selected.date)) {
      sel.tracks <- tracks[tracks$begdate == selected.date,]
    } else {
      sel.tracks <- tracks
    }

    if (UTM) {
      tracks_utm <- spTransform(sel.tracks, CRS("+proj=utm +zone=33"))
      out_tracks_strava <- tracks_utm
    } else {
      out_tracks_strava <- sel.tracks
    }
  } else {
    out_tracks_strava <- data.frame()
  }

  #changing Strava track ID's
  ids <- c()
  for(i in 1: nrow(out_tracks_strava)) {
    ids <- c(ids, out_tracks_strava@lines[[i]]@ID)
  }
  new_ids <- as.character(as.numeric(ids) + 50000)
  out_tracks_strava <- spChFIDs(out_tracks_strava, new_ids)

  garmin_track_file <- "garmin_tracks_saved2.shp"
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
    writeOGR(garmin_tracks, ".", "garmin_tracks_saved2", driver="ESRI Shapefile")
  } else {
    garmin_tracks <- readOGR(".", "garmin_tracks_saved2", stringsAsFactors=FALSE, verbose = FALSE)
  }
  if (is.null(selected.date)) {
    NtracksGarmin <- nrow(garmin_tracks)
  } else {
    NtracksGarmin <- length(which(garmin_tracks$begdate == selected.date))
  }
  if (NtracksGarmin > 0) {
    if (!is.null(selected.date)){
      sel.tracks.garmin <- garmin_tracks[garmin_tracks$begdate == selected.date,]
    } else {
      sel.tracks.garmin <- garmin_tracks
    }
    if (UTM) {
      garmin_tracks_utm <- spTransform(sel.tracks.garmin, CRS("+proj=utm +zone=33"))
      out_tracks_garmin <- garmin_tracks_utm
    } else {
      out_tracks_garmin <- sel.tracks.garmin
    }
  } else {
    out_tracks_garmin <- data.frame()
  }

  # combining them together (garmin+strava)
  out_tracks_combined <- data.frame()
  if (nrow(out_tracks_strava) > 0) {
    out_tracks_strava$date <- NA
    out_tracks_strava$datetime <- NA
    out_tracks_strava$month <- NA
    out_tracks_strava$weekday <- NA
  }
  if (nrow(out_tracks_garmin) > 0 & nrow(out_tracks_strava) > 0) {
    out_tracks_combined <- rbind(out_tracks_garmin, out_tracks_strava)
  } else {
    if(nrow(out_tracks_garmin) > 0) {
      out_tracks_combined <- out_tracks_garmin
    } else {
      out_tracks_combined <- out_tracks_strava
    }
  }

  return(out_tracks_combined)
}
