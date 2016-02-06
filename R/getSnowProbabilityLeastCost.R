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

getSnowProbabilityLeastCost <- function(tr, modis, stations, reports = NULL, tracks = NULL, exponent = 3) {

  #modis
  #reclassify and filter modis
  modis_reclas <- reclassModis(modis)
  modis_filter <- filterModis(modis_reclas)

  modis.present <- modis_filter == 1
  modis.present[modis.present == 0] <- NA

  modis.absent <- modis_filter == 0
  modis.absent[modis.absent == 0] <- NA

  dem <- raster("C:/jiri/Dropbox/PHD/crowdsourcing/data/DEM/dem_utm_500m.tif")
  cze <- getCzechia()

  plot(modis_filter, main="modis - filtered")
  plot(cze, add=TRUE)


  # read stations
  stations_prez <- stations[stations$present == TRUE,]
  points(stations_prez, pch=16, col="blue")
  stations_abs <- stations[stations$present == FALSE,]
  points(stations_abs, pch=1, col="red")

  # read reports
  if(nrow(reports) > 0) {
    reports_prez <- reports[reports$present == TRUE,]
    points(reports_prez, col="blue", pch=15)
    reports_abs <- reports[reports$present == FALSE,]
    points(reports_abs, col="red", pch=0)
  } else {
    reports_prez <- data.frame()
    reports_abs <- data.frame()
  }

  # read tracks / track points
  if (nrow(tracks) > 0) {
    tracks_prez <- sampleTrackPoints(tracks)
    points(tracks_prez, col="blue", pch=2)
  }

  # read samples from MODIS
  modisPoints_prez <- sampleModisPoints(modis_filter, 1)
  modisPoints_abs <- sampleModisPoints(modis_filter, 0)

  plot(modis_filter)

  if (nrow(modisPoints_prez) > 0) {
    plot(modisPoints_prez, add=TRUE)
  }
  if (nrow(modisPoints_abs) > 0) {
    points(modisPoints_abs)
  }

  #################################
  # transition matrix construction
  #tr1 <- transition(dem, mean, 8)

  # using permeability instead of friction
  #tr2 <- transition(dem, function(x) {1 / mean(x)}, 8)
  #tr2 <- geoCorrection(tr2)

  # magnifying elevation effect by exponent
  # transition_exponent <- 1.5
  # tr <- transition(dem, function(x) {1 / (mean(x))^transition_exponent}, 8)
  # tr <- geoCorrection(tr)

  # to-coordinates
  tocoords <- rasterToPoints(dem, spatial = TRUE)
  names(tocoords) <- "z"

  # for each PRESENT STATION do the COST #
  costStaPrez <- getNearestCostDistance(tr, stations_prez, tocoords)
  plot(costStaPrez)
  points(stations_prez)

  # for each ABSENT STATION do the COST #
  costStaAbs <- getNearestCostDistance(tr, stations_abs, tocoords)
  plot(costStaAbs)
  points(stations_abs)

  # for each PRESENT REPORT do the COST #
  if (nrow(reports_prez) > 0) {
    costRepPrez <- getNearestCostDistance(tr, reports_prez, tocoords)
    plot(costRepPrez)
    points(reports_prez)
  }

  # for each ABSENT REPORT do the COST #
  if (nrow(reports_abs) > 0) {
    costRepAbs <- getNearestCostDistance(tr, reports_abs, tocoords)
    plot(costRepAbs)
    points(reports_abs)
  }

  # for each TRACK do the COST #
  if (nrow(tracks) > 0) {
    costTraPrez <- getNearestCostDistance(tr, tracks_prez, tocoords)
    plot(costTraPrez)
    points(tracks_prez)
  }

  # for each PRESENT MODIS do the COST #
  if (nrow(modisPoints_prez) > 0) {
    costModPrez <- getNearestCostDistance(tr, modisPoints_prez, tocoords)
    plot(costModPrez)
    points(modisPoints_prez)
  }

  # for each ABSENT MODIS do the COST #
  if (nrow(modisPoints_abs) > 0) {
    costModAbs <- getNearestCostDistance(tr, modisPoints_abs, tocoords)
    plot(costModAbs)
    points(modisPoints_abs)
  }

  #########################################################################
  # experimental snow probability based on the COST DIST STATION + REPORT #
  #########################################################################
  if (nrow(stations_prez) > 0) {
    cost_sta_prez_inv <- 1/(costStaPrez^exponent)
  } else {
    cost_sta_prez_inv <- dem * 0
  }

  if (nrow(stations_abs) > 0) {
    cost_sta_abs_inv <- 1/(costStaAbs^exponent)
  } else {
    cost_sta_abs_inv <- dem * 0
  }

  if (nrow(reports_prez) > 0) {
    cost_rep_prez_inv <- 1/(costRepPrez^exponent)
  } else {
    cost_rep_prez_inv <- dem * 0
  }

  if (nrow(reports_abs) > 0) {
    cost_rep_abs_inv <- 1/(costRepAbs^exponent)
  } else {
    cost_rep_abs_inv <- dem * 0
  }

  if (nrow(tracks) > 0) {
    cost_tra_prez_inv <- 1/(costTraPrez^exponent)
  } else {
    cost_tra_prez_inv <- dem * 0
  }

  if (nrow(modisPoints_prez) > 0) {
    cost_mod_prez_inv <- 1/(costModPrez^exponent)
  } else {
    cost_mod_prez_inv <- dem * 0
  }

  if (nrow(modisPoints_abs) > 0) {
    cost_mod_abs_inv <- 1/(costModAbs^exponent)
  } else {
    cost_mod_abs_inv <- dem * 0
  }

  prez.conf <- (cost_sta_prez_inv + cost_rep_prez_inv + cost_tra_prez_inv + cost_mod_prez_inv)
  abs.conf <- (cost_sta_abs_inv + cost_rep_abs_inv + cost_mod_abs_inv)
  conf <- prez.conf / (prez.conf + abs.conf)
  plot(conf)
  plot(cze, add=TRUE)

  # plot map (0.5 probability..)
  prob05 <- conf
  prob05[prob05 < 0.5] <- NA
  plot(prob05)
  plot(cze, add=TRUE)
  if (nrow(stations_prez) > 0) {
    points(stations_prez, pch=16, col="blue")
  }
  if (nrow(stations_abs) > 0) {
    points(stations_abs, pch=1, col="red")
  }
  if (nrow(reports) > 0) {
    if (nrow(reports_prez) > 0) {
      points(reports_prez, pch=15, col="blue")
    }
    if (nrow(reports_abs) > 0) {
      points(reports_abs, pch=0, col="red")
    }
  }
  if (nrow(tracks) > 0) {
    points(tracks_prez, col="blue", pch=2)
  }
  title(main=paste("station+reports cost dist exp =", exponent))

  confRes <- resample(conf, modis)
  return(confRes)
}
