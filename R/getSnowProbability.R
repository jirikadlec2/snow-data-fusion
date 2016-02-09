#' getSnowProbability
#'
#' This function gets the snow probability based on the other data
#'
#' @import raster
#' @param modis The reclassified modis raster object
#' @param stations The stations SpatialPointsDataFrame (assumed UTM)
#' @param reports the volunteer reports SpatialPointsDataFrame
#' @param tracks the GPS tracks SpatialLinesDataFrame
#' @param exponent the distance exponent for inverse distance
#' @return the snow probability raster

getSnowProbability <- function(modis, stations, reports = data.frame(), tracks = data.frame(), exponent = 3) {

  #modis
  #reclassify and filter modis
  modis_reclas <- reclassModis(modis)
  modis_filter <- filterModis(modis)

  modis.present <- modis_filter == 1
  modis.present[modis.present == 0] <- NA

  modis.absent <- modis_filter == 0
  modis.absent[modis.absent == 0] <- NA

  #need to check that modis has some cloud-free pixels
  use_modis_abs <- TRUE
  if (is.infinite(cellStats(modis.absent, "max"))) {
    use_modis_abs <- FALSE
  }
  use_modis_prez <- TRUE
  if (is.infinite(cellStats(modis.present, "max"))) {
    use_modis_prez <- FALSE
  }

  # read samples from MODIS
  modisPoints_prez <- data.frame()
  modisPoints_abs <- data.frame()

  if (use_modis_prez) {
    modisPoints_prez <- sampleModisPoints(modis_filter, 1)
    plot(modisPoints_prez, add=TRUE)
  }
  if (use_modis_abs) {
    modisPoints_abs <- sampleModisPoints(modis_filter, 0)
    points(modisPoints_abs)
  }

  if (use_modis_abs) {
    d.modis.abs <- distanceFromPoints(modis, modisPoints_abs)
  }
  if (use_modis_prez) {
    d.modis.prez <- distanceFromPoints(modis, modisPoints_prez)
  }

  #stations
  if (!grepl("utm", proj4string(stations))) {
    stations <- spTransform(stations, CRS("+proj=utm +zone=33"))
  }
  stations.prez <- stations[stations$present == TRUE,]
  stations.abs <- stations[stations$present == FALSE,]
  d.station.abs <- distanceFromPoints(modis, stations.abs)
  d.station.prez <- distanceFromPoints(modis, stations.prez)
  #inverse distance - stations
  inv.d.station.abs <- 1 /(d.station.abs^exponent)
  inv.d.station.prez <- 1 / (d.station.prez^exponent)

  #reports
  if (is.null(reports)) {
    reports <- data.frame()
  }
  if (nrow(reports) > 0) {
    reports_utm <- spTransform(reports, CRS("+proj=utm +zone=33"))
    reports.prez <- reports_utm[reports_utm$present == 1,]
    reports.abs <- reports_utm[reports_utm$present == 0,]
    d.report.abs <- distanceFromPoints(modis, reports.abs)
    d.report.prez <- distanceFromPoints(modis, reports.prez)
    inv.d.report.abs <- 1 / (d.report.abs^exponent)
    inv.d.report.prez <- 1 / (d.report.prez^exponent)
  } else {
    inv.d.report.abs <- modis * 0
    inv.d.report.prez <- modis * 0
  }

  #tracks
  if (is.null(tracks)) {
    tracks <- data.frame()
  }
  if (nrow(tracks) > 0) {
    track_points <- sampleTrackPoints(tracks)
    d.track.prez <- distanceFromPoints(modis, track_points)
    inv.d.track.prez <- 1 / (d.track.prez^exponent)
  } else {
    inv.d.track.prez <- modis * 0
  }

  #inverse distance - modis
  if (use_modis_abs) {
    inv.d.modis.abs <- 1/(d.modis.abs^exponent)
  } else {
    inv.d.modis.abs <- modis * 0
  }
  if (use_modis_prez) {
    inv.d.modis.prez <- 1/(d.modis.prez^exponent)
  } else {
    inv.d.modis.prez <- modis * 0
  }

  #weighed averages
  prez.conf <- inv.d.modis.prez + inv.d.station.prez + inv.d.report.prez + inv.d.track.prez
  abs.conf <- inv.d.modis.abs + inv.d.station.abs + inv.d.report.abs

  conf <- prez.conf / (prez.conf + abs.conf)
  conf[is.na(conf)] <- 1

  return(conf)
}
