#' getReportsArchive
#'
#' This function gets the volunteer snow depth reports from HydroShare archive
#'
#' @import XML
#' @import httr
#' @import sp
#' @param selected.date The date of the satellite observation
#' @param UTM if TRUE then transform output to WGS84 UTM Zone 33 N
#' @return a SpatialPointsDataFrame with the stations.
#' it has columns lat, lon, snodep and present.
#' present is 0 for bare ground, 1 for snow (no data are excluded)

getReportsArchive <- function(selected.date, UTM=TRUE) {
  resource_id <- "d2d47cfe84be4bc9ba3352eec7ceb57e"
  zip_file <- "volunteer.zip"
  resource_uri <- paste("http://hydroshare.org/hsapi/resource/", resource_id, sep="")
  GET(resource_uri, write_disk(zip_file, overwrite=TRUE))
  res <- unzip(zip_file)
  res.csv <- grepl("*.csv", res)
  csvfile <- res[res.csv]

  # get data from hydroshare unzipped file
  volunteer.data <- read.csv(csvfile, header=TRUE, stringsAsFactors = FALSE)
  names(volunteer.data) <- c("date", "time", "lat", "lon",
                             "site", "snow_depth_cm")

  # extract values for spedific date
  numSelected <- length(which(volunteer.data$date == selected.date))

  if (numSelected > 0) {

    snow.selected <- volunteer.data[volunteer.data$date == selected.date, ]

    #presence field
    presence <- snow.selected$snow_depth_cm >= 1
    absence <- snow.selected$snow_depth_cm <= 0
    unknown <- snow.selected$snow_depth_cm > 0 & snow.selected$snow_depth_cm < 1
    snow.selected$present[presence] <- 1
    snow.selected$present[absence] <- 0
    snow.selected$present[unknown] <- 2
    coordinates(snow.selected) <- ~lon+lat
    projection(snow.selected) <- CRS("+proj=longlat")

    if (UTM) {
      snow_utm <- spTransform(snow.selected, CRS("+proj=utm +zone=33"))
      return(snow_utm)
    } else {
      return(snow.selected)
    }
  } else {
    return(data.frame())
  }
}
