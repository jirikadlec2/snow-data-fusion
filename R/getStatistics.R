#' getStatistics
#'
#' This function gets various statistics for tracks, stations, reports
#'
#' @import XML
#' @import httr
#' @import sp
#' @import raster
#' @import rgdal
#' @import ggplot2
#' @return various statistics data
#' it has columns lat, lon, snodep and present.
#' present is 0 for bare ground, 1 for snow (no data are excluded)
#' 
getStatistics <- function() {

  setwd("C:/jiri/Dropbox/PHD/crowdsourcing/data/garmin")


  parseTcxElevations <- function(my_file) {
    doc <- xmlParse(my_file)
    elevations <- as.numeric(xpathSApply(doc, "//ns:AltitudeMeters", xmlValue, namespaces="ns"))
    return(elevations)
  }

  garmin_tracks <- readOGR(".", "garmin_tracks_2012-2015", stringsAsFactors=FALSE)
  garmin_tracks$mean_elev <- NA
  all_elevations <- c()
  
  setwd("C:/temp/data/garmin")
  i <- 1
  for(i in 1:nrow(garmin_tracks)) {
    print(i)
  
    tcx <- garmin_tracks$tcx[i]
    elevs <- parseTcxElevations(tcx)
    all_elevations <- c(all_elevations, elevs)
    mean_elev <- mean(elevs)
    print(mean_elev)
    garmin_tracks$mean_elev[i] <- mean_elev
  }
  write.table(all_elevations, "garmin_all_elevations.txt", sep="")

  valid_elevations <- all_elevations[all_elevations > 115 & all_elevations < 1605]
  elevations <- data.frame(elevation=valid_elevations)
  library(ggplot2)
  ggplot(data=elevations, aes(elevations$elevation)) + 
    geom_histogram(breaks=seq(100, 1600, by=100), fill="darkgray", col="black") +
    xlab("elevation (m)") +
    ylab("number of track points")
    xlim(c(0, 1700))

  garmin_utm <- spTransform(garmin_tracks, CRS("+proj=utm +zone=33"))
  plot(garmin_utm, add=TRUE)
  
  plot(garmin_utm)

  ###################################################################
  # same histogram statistics for the volunteer report points       #
  ###################################################################
  reports <- read.csv("C:/jiri/Dropbox/PHD/crowdsourcing/data/reports/snowdata.csv", stringsAsFactors=FALSE)
  coordinates(reports) <- ~LATITUDE+LONGITUDE
  proj4string(reports) <- "+proj=longlat"
  plot(reports)
  # extracting the elevation of the reports
  reportsUTM <- spTransform(reports, CRS("+proj=utm +zone=33"))

  dem <- raster("C:/jiri/Dropbox/PHD/crowdsourcing/data/dem/srtm_500m.tif")
  plot(dem)
  plot(reportsUTM, add=TRUE)
  reportsUTM$elevation <- extract(dem, reportsUTM)
  hist(reportsUTM$elevation)

  report_elevation <- data.frame(elevation=reportsUTM$elevation)
  ggplot(data=report_elevation, aes(report_elevation$elevation)) + 
    geom_histogram(breaks=seq(100, 1600, by=100), fill="darkgray", col="black") +
    xlab("elevation (m)") +
    ylab("number of snow reports") +
    xlim(c(0, 1700))
  summary(report_elevation)
  quantile(report_elevation, 0.9, na.rm=TRUE)
  quantile(report_elevation, 0.1, na.rm=TRUE)

  names(reports) <- c("date", "time", "site", "snowdepth")
  head(reports$date)
  
  reports2 <- data.frame(reports$date, reports$time, reports$site, reports$snowdepth, stringsAsFactors=FALSE)
  reports_daily <- aggregate(reports.snowdepth~reports.date, data=reports2, length)
  reports_daily$date <- as.Date(reports_daily$reports.date)
  plot(reports_daily$date, reports_daily$reports.snowdepth, type="l")
  qplot(reports.date, reports.snowdepth, data=reports_daily, geom="line", ylab="number of reports")
  
  reports2$date = as.Date(reports2$reports.date)
  reports2$month <- strftime(reports2$date, format="%m")

  reports_monthly <- aggregate(reports.snowdepth~month, data=reports2, length)
  reports_monthly2 <- rbind(reports_monthly[4:5,], reports_monthly[1:3,])
  reports_monthly2$month2 <- c("Nov", "Dec", "Jan", "Feb", "Mar")
  reports_monthly_sum <- sum(reports_monthly2$reports.snowdepth)
  reports_monthly2$percent <- 100 * (reports_monthly2$reports.snowdepth / reports_monthly_sum)
  barplot(reports_monthly2$reports.snowdepth, names.arg=reports_monthly2$month2, ylab="Number of reports")
         
  #garmin tracks by day of week
  reports2$weekday <- strftime(reports2$date, "%w")
  reports_weekday <- aggregate(reports.snowdepth~weekday, data=reports2, length)
  weekdays <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  barplot(reports_weekday$reports.snowdepth, ylab="Number of reports", names.arg=weekdays)

  # elevation distribution plot: stations, tracks, reports
  report_elev_sorted <- sort(report_elevation$elevation)
  track_elev_sorted <- sort(all_elevations)
  
  elev_breaks <- seq(0, 1600, by=100)
  track_elev_cut <- cut(track_elev_sorted, elev_breaks, right=FALSE)
  track_elev_freq <- table(track_elev_cut)
  track_elev_sum <- sum(track_elev_freq)
  track_elev_cumfreq <- cumsum(track_elev_freq)

  track_elev_percent <- (track_elev_freq / track_elev_sum) * 100
  track_elev_cumpercent <- cumsum(track_elev_percent)
  plot(seq(100, 1600, by=100), track_elev_cumpercent, type="l", xlab="elevation (m)", ylab="cumulative frequency (%)")
  
  # elevation distribution: reports
  reports_elev_cut <- cut(report_elev_sorted, elev_breaks, right=FALSE)
  reports_elev_freq <- table(reports_elev_cut)
  reports_elev_sum <- sum(reports_elev_freq)
  reports_elev_percent <- (reports_elev_freq / reports_elev_sum) * 100
  reports_elev_cumpercent <- cumsum(reports_elev_percent)
  lines(seq(100, 1600, by=100), reports_elev_cumpercent, col="gray")

  # elevation distribution: stations
  head(stations)
  proj4string(stations) <- "+proj=longlat"
  stations_utm <- spTransform(stations, CRS("+proj=utm +zone=33"))
  stations_utm$elevation <- extract(dem, stations_utm)
  
  stations_elev_sorted <- sort(stations_utm$elevation)
  stations_elev_cut <- cut(stations_elev_sorted, elev_breaks, right=FALSE)
  stations_elev_freq <- table(stations_elev_cut)
  stations_elev_sum <- sum(stations_elev_freq)
  stations_elev_percent <- (stations_elev_freq / stations_elev_sum) * 100
  stations_elev_cumpercent <- cumsum(stations_elev_percent)
  lines(seq(100, 1600, by=100), stations_elev_cumpercent, col="red", lty=2)

  legend("bottomright", c("tracks", "reports", "stations"), col=c("black", "gray", "red"), lty=c(1, 1, 2))
}