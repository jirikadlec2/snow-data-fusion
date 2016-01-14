#' getStations
#'
#' This function gets the meteorological stations snow data from CUAHSI
#'
#' @import XML
#' @import httr
#' @import sp
#' @param selected.date The date of the satellite observation
#' @return a SpatialPointsDataFrame with the stations.
#' it has columns lat, lon, snodep and present.
#' present is 0 for bare ground, 1 for snow (no data are excluded)

getStations <- function(selected.date) {
  st_uri <- "http://hydrodata.info/api/sites?var=snih"
  vals_uri <- "http://hydrodata.info/api/values"
  stations <- read.table(st_uri, sep="\t", header=TRUE, stringsAsFactors = FALSE)
  #distinguish SYNOP stations
  synop.ids <- c(2,3,9,10,20,22,23,24,30,33,34,41,42,45,47,48,49,51,
                 52,53,60,63,76,80,81,82,223,231,232,233,253)
  stations$synop <- stations$id %in% synop.ids

  #download data values from each station
  d <- data.frame()
  i <- 1
  for (i in 1:nrow(stations)) {
    id <- stations$id[i]
    uri <- paste(vals_uri, "?var=snih&st=", id, sep="")
    vals <- read.table(uri, sep="\t", header=TRUE, stringsAsFactors = FALSE)
    selected.vals <- vals[vals$datum == selected.date, ]
    if (nrow(selected.vals) == 0) {
      next
    }
    #check for NA values
    val <- vals[vals$datum == selected.date, 2]

    if (is.na(val)) {
      if (stations$synop[i] == TRUE) {
        val <- 0
      } else {
        val <- NA
      }
    }
    #assign value
    if (!is.na(val)) {
      d <- rbind(d, c(lat=stations$lat[i], lon=stations$lon[i], snodep=val))
    }
  }
  #assign presence / absence
  names(d) <- c("lat", "lon", "snodep")
  d$present <- d$snodep > 0

  coordinates(d) <- ~lon+lat
  proj4string(d) <- CRS("+proj=longlat")
  return(d)
}
