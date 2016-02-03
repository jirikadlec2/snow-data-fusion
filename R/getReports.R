#' getReports
#'
#' This function gets the volunteer snow depth reports from in-pocasi
#'
#' @import XML
#' @import httr
#' @import sp
#' @param selected.date The date of the satellite observation
#' @param UTM if TRUE then transform output to WGS84 UTM Zone 33 N
#' @return a SpatialPointsDataFrame with the stations.
#' it has columns lat, lon, snodep and present.
#' present is 0 for bare ground, 1 for snow (no data are excluded)

getReports <- function(selected.date, UTM=TRUE) {

  url <- "http://www.in-pocasi.cz/pocasi-u-vas/seznam.php?historie="
  url <- paste(url, strftime(selected.date, "%m-%d-%Y"), sep="")

  obsdata <- GET(url)

  html <- htmlParse(obsdata)
  refs <- xpathSApply(html, "//a", xmlValue)
  f <- xpathSApply(html, "//a[@class='tooltip']", xmlValue)
  fb <- xpathSApply(html, "//a[@class='tooltip']//b", xmlValue)
  fbs <- xpathSApply(html, "//a[@class='tooltip']//strong", xmlValue)
  hrefs <- xpathSApply(html, "//a[@class='tooltip']", xmlGetAttr, "href")

  entries <- data.frame(site=fbs, sno=fb, link=hrefs, stringsAsFactors=FALSE)
  entries$snow_depth_cm <- ifelse(grepl("cm", entries$sno), substr(entries$sno, 1, nchar(entries$sno) -3), 0.5)

  #getting coordinates
  entries$lat <- NA
  entries$lon <- NA
  for (i in 1: nrow(entries)) {
    link <- entries$link[i]
    html2 <- htmlParse(GET(link))
    imgsrc <- xpathSApply(html2, "//img", xmlGetAttr, "src")
    imgmapka <- imgsrc[grepl("mapka", imgsrc)]
    imgparts <- strsplit(imgmapka[[1]], "-")
    numparts <- length(imgparts[[1]])
    entries$lat[i] <- as.numeric(imgparts[[1]][numparts - 1])
    entries$lon[i] <- as.numeric(imgparts[[1]][numparts])
  }
  entries$present <- entries$snow_depth_cm >= 1

  coordinates(entries) <- ~lon+lat
  projection(entries) <- CRS("+proj=longlat")

  if (UTM) {
    snow_utm <- spTransform(entries, CRS("+proj=utm +zone=33"))
    return(snow_utm)
  } else {
    return(entries)
  }
}
