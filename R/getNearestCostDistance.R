#' getNearestCostDistance
#'
#' This function gets nearest cost distance
#'
#' @import gDistance
#' @import raster
#' @import sp
#' @param transitionMatrix the transition matrix
#' @param fromCoords the stations, reports or tracks
#' @param toCoords the remaining raster pixels
#' @return the shortest least cost distance raster

getNearestCostDistance <- function(transitionMatrix, fromCoords, toCoords) {
  # cost distance
  cost <- costDistance(transitionMatrix, fromCoords, toCoords)
  n <- length(fromCoords)

  toCoords$z <- cost[1,]
  costRas <- rasterFromXYZ(toCoords, 500)

  if (n > 1) {
  i <- 2
    for (i in 2: n) {
      toCoords$z <- cost[i,]
      costras2 <- rasterFromXYZ(toCoords, 500)

      costRas <- min(costRas, costras2)
    }
  }
  return(costRas)
}
