#' getTracks
#'
#' This function gets the tracks reports strava / garmin
#' @import httr
#' @import rgdal
#' @import sp
#' @param selected.date The date of the satellite observation
#' @param UTM if TRUE then transform output to WGS84 UTM Zone 33 N
#' @return a SpatialPointsDataFrame with the tracks.

getTracks <- function(selected.date, UTM=TRUE) {

  out_tracks_strava <- data.frame()
  out_tracks_garmin <- data.frame()

  new_track_file <- "strava_tracks_saved.shp"
  if (!file.exists(new_track_file)) {
    resource_id <- "7d8751fef6234ae794b4b1ca31156b8f"
    resource_uri <- paste("http://hydroshare.org/hsapi/resource/", resource_id, sep="")
    track_file <- "strava.zip"
    GET(resource_uri, write_disk(track_file, overwrite=TRUE))
    res <- unzip(track_file)
    res.shp <- grepl("*.shp", res)
    shpfile <- res[res.shp]
    shpfolder <- substr(shpfile, 1, nchar(shpfile) - nchar(basename(shpfile)) - 1)
    shpname <- substr(basename(shpfile), 1, nchar(basename(shpfile)) - 4)
    tracks <- readOGR(shpfolder, shpname, stringsAsFactors = FALSE)
    writeOGR(tracks, ".", "strava_tracks_saved", driver="ESRI Shapefile")
  } else {
    tracks <- readOGR(".", "strava_tracks_saved", stringsAsFactors = FALSE)
  }
  Ntracks <- length(which(tracks$begdate == selected.date))
  if (Ntracks > 0) {
    sel.tracks <- tracks[tracks$begdate == selected.date,]
    proj4string(sel.tracks) <- CRS("+proj=longlat")
    if (UTM) {
      tracks_utm <- spTransform(sel.tracks, CRS("+proj=utm +zone=33"))
      out_tracks_strava <- tracks_utm
    } else {
      out_tracks_strava <- sel.tracks
    }
  } else {
    out_tracks_strava <- data.frame()
  }

  garmin_track_file <- "garmin_tracks_saved.shp"
  if (!file.exists(garmin_track_file)) {
    resource_id <- "d96b094c6afa4e8fb147e3aa8c86a689"
    resource_uri <- paste("http://hydroshare.org/hsapi/resource/", resource_id, sep="")
    track_file <- "garmin.zip"
    GET(resource_uri, write_disk(track_file, overwrite=TRUE))
    res <- unzip(track_file)
    res.shp <- grepl("*.shp", res)
    shpfile <- res[res.shp]
    shpfolder <- substr(shpfile, 1, nchar(shpfile) - nchar(basename(shpfile)) - 1)
    shpname <- substr(basename(shpfile), 1, nchar(basename(shpfile)) - 4)
    garmin_tracks <- readOGR(shpfolder, shpname, stringsAsFactors = FALSE)
    writeOGR(garmin_tracks, ".", "garmin_tracks_saved", driver="ESRI Shapefile")
  } else {
    garmin_tracks <- readOGR(".", "garmin_tracks_saved", stringsAsFactors=FALSE)
  }
  NtracksGarmin <- length(which(garmin_tracks$begdate == selected.date))
  if (NtracksGarmin > 0) {
    sel.tracks.garmin <- garmin_tracks[garmin_tracks$begdate == selected.date,]
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
  out_tracks_strava$date <- NA
  out_tracks_strava$datetime <- NA
  out_tracks_strava$month <- NA
  out_tracks_strava$weekday <- NA
  out_tracks_combined <- rbind(out_tracks_garmin, out_tracks_strava)

  return(out_tracks_combined)
}
