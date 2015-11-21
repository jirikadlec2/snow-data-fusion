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
    tracks <- readOGR(".", "strava_tracks_saved")
  }
  Ntracks <- length(which(tracks$begdate == selected.date))
  if (Ntracks > 0) {
    sel.tracks <- tracks[tracks$begdate == selected.date,]
    projection(sel.tracks) <- CRS("+proj=longlat")
    if (UTM) {
      tracks_utm <- spTransform(sel.tracks, CRS("+proj=utm +zone=33"))
      return(tracks_utm)
    } else {
      return(sel.tracks)
    }
  } else {
    return(data.frame())
  }
}
