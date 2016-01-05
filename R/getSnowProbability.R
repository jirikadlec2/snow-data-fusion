#' getSnowProbability
#'
#' This function gets the snow probability based on the other data
#'
#' @import raster
#' @param modis The reclassified modis raster object
#' @param stations The stations SpatialPointsDataFrame
#' @param reports the volunteer reports SpatialPointsDataFrame
#' @param tracks the GPS tracks SpatialLinesDataFrame
#' @param exponent the distance exponent for inverse distance
#' @return the snow probability raster

getSnowProbability <- function(modis, stations, reports, tracks, exponent = 3) {

  #modis
  modis.present <- modis == 1
  modis.present[modis.present == 0] <- NA
  modis.absent <- modis == 0 | modis == 100
  modis.absent[modis.absent == 0] <- NA
  d.modis.abs <- distance(modis.absent)
  d.modis.prez <- distance(modis.present)

  #stations
  stations_utm <- spTransform(stations, CRS("+proj=utm +zone=33"))
  stations.prez <- stations_utm[stations_utm$present == TRUE,]
  stations.abs <- stations_utm[stations_utm$present == FALSE,]
  d.station.abs <- distanceFromPoints(modis, stations.abs)
  d.station.prez <- distanceFromPoints(modis, stations.prez)

  #reports
  if (nrow(reports) > 0) {
    reports_utm <- spTransform(reports, CRS("+proj=utm +zone=33"))
    reports.prez <- reports_utm[reports_utm$present == 1,]
    reports.abs <- reports_utm[reports_utm$present == 0,]
    d.report.abs <- distanceFromPoints(modis, reports.abs)
    d.report.prez <- distanceFromPoints(modis, reports.prez)
    inv.d.report.abs <- 1 / (d.report.abs^exponent)
    inv.d.report.prez <- 1 / (d.report.prez^exponent)
  }

  #tracks
  if (nrow(tracks) > 0) {
    tracks_utm <- spTransform(tracks, CRS("+proj=utm +zone=33"))
    track.ras <- rasterize(tracks, modis)
    d.track.prez <- distance(track.ras)
    inv.d.track.prez <- 1 / (d.track.prez^exponent)
  }

  #inverse distance
  inv.d.modis.abs <- 1/(d.modis.abs^exponent)
  inv.d.modis.prez <- 1/(d.modis.prez^exponent)
  inv.d.station.abs <- 1 /(d.station.abs^exponent)
  inv.d.station.prez <- 1 / (d.station.prez^exponent)
  


  #weighed averages
  if (nrow(tracks) > 0) {
    prez.conf <- inv.d.modis.prez + inv.d.station.prez + inv.d.report.prez + inv.d.track.prez
    prez.abs.conf <- inv.d.modis.abs + inv.d.station.abs + inv.d.report.abs + inv.d.modis.prez + inv.d.station.prez + inv.d.report.prez + inv.d.track.prez
  } else if (nrow(reports) > 0) {
    prez.conf <- inv.d.modis.prez + inv.d.station.prez + inv.d.report.prez
    prez.abs.conf <- inv.d.modis.abs + inv.d.station.abs + inv.d.report.abs + inv.d.modis.prez + inv.d.station.prez + inv.d.report.prez
  } else {
    prez.conf <- inv.d.modis.prez + inv.d.station.prez
    prez.abs.conf <- inv.d.modis.abs + inv.d.station.abs + inv.d.modis.prez + inv.d.station.prez
  }
  conf <- prez.conf / prez.abs.conf
  cnan <- is.na(conf)
  conf[is.na(conf)] <- 1

  return(conf)
}
