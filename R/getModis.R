#' getModis
#'
#' This function gets the MODIS raster from the Cryoland WCS dataset
#'
#' @import XML
#' @import httr
#' @param selected.date The date of the satellite observation
#' @param west Optional parameter: The west longitude of the geographic
#'  bounding box in decimal degrees. Allowed values are between -180.0 and +180.0
#' @param south Optional parameter: The south latitude of the geographic
#'  bounding box in decimal degrees. Allowed values are between -90.0 and +90.0
#' @param east Optional parameter: The east longitude of the geographic
#'  bounding box in decimal degrees. Allowed values are between -180.0 and +180.0
#' @param north Optional parameter: The north latitude of the geographic
#'  bounding box in decimal degrees. Allowed values are between -90.0 and +90.0
#' @return a name of the saved raster file

getModis <- function(selected.date, west, south, east, north) {
  base_uri <- "http://neso.cryoland.enveo.at/cryoland/ows?Service=WCS"
  #start date, end date
  start_date_t <- strptime(selected.date, "%Y-%m-%d")
  start_date <- strftime(start_date_t, "%Y-%m-%d")
  end_date_t <- as.Date(start_date_t) + 1
  end_date <- strftime(end_date_t, "%Y-%m-%d")

  metadata_uri <- paste(base_uri,
                        '&Request=DescribeEOCoverageSet',
                        '&EOID=daily_FSC_PanEuropean_Optical',
                        '&subset=phenomenonTime("', start_date,'","', end_date, '")', sep="")
  coverage_resp <- GET(metadata_uri)
  doc <- content(coverage_resp)
  ns <- c(xsd="http://www.w3.org/2001/XMLSchema",
          xsi="http://www.w3.org/2001/XMLSchema-instance",
          wcs="http://www.opengis.net/wcs/2.0")
  coverage_ids <- xpathSApply(doc, "//wcs:CoverageId", xmlValue, namespaces=ns)
  coverage_id <- coverage_ids[1]

  # Get Coverage
  data_uri <- paste(base_uri, '&version=2.0.1&request=GetCoverage&CoverageID=', coverage_id, '&Format=image/tiff&subset=lat(',
                    south, ',', north, ')&subset=lon(', west, ',', east, ')',
                    '&subsettingCRS=http://www.opengis.net/def/crs/EPSG/0/4326',
                    '&OutputCRS=http://www.opengis.net/def/crs/EPSG/0/32633', sep="")

  ras_file <- paste("modis", start_date, ".tif", sep="")
  GET(data_uri, write_disk(ras_file, overwrite=TRUE))
  return(ras_file)
}
