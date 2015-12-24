######################################################
# STATISTICS of the STRAVA and GARMIN tracks         #
######################################################
library(sp)
library(rgdal)
library(ggplot2)

# (1) analysis of GARMIN tracks 
setwd("C:/jiri/Dropbox/PHD/crowdsourcing/data/garmin")

garmin_tracks <- readOGR(".", "garmin_tracks_all3", stringsAsFactors=FALSE)
garmin_tracks$date <- as.Date(garmin_tracks$begdate)
garmin_data <- garmin_tracks@data


# analyze the frequency for each date // day
garmin_2015 <- garmin_tracks[garmin_tracks$date > "2014-11-01" & garmin_tracks$date < "2015-05-01",]
garmin_2014 <- garmin_tracks[garmin_tracks$date > "2013-11-01" & garmin_tracks$date < "2014-05-01",]
garmin_2013 <- garmin_tracks[garmin_tracks$date > "2012-11-01" & garmin_tracks$date < "2013-05-01",]
garmin_2012 <- garmin_tracks[garmin_tracks$date > "2011-11-01" & garmin_tracks$date < "2012-05-01",]
garmin_2011 <- garmin_tracks[garmin_tracks$date > "2010-11-01" & garmin_tracks$date < "2011-05-01",]
garmin_2010 <- garmin_tracks[garmin_tracks$date > "2009-11-01" & garmin_tracks$date < "2010-05-01",]
garmin_2009 <- garmin_tracks[garmin_tracks$date > "2008-11-01" & garmin_tracks$date < "2009-05-01",]
garmin_2008 <- garmin_tracks[garmin_tracks$date > "2007-11-01" & garmin_tracks$date < "2008-05-01",]

plot(garmin_2015, main=paste("garmin 2014-2015, N =", nrow(garmin_2015)))
plot(garmin_2014, main=paste("garmin 2013-2014, N =", nrow(garmin_2014)))
plot(garmin_2013, main=paste("garmin 2012-2013, N =", nrow(garmin_2013)))
plot(garmin_2012, main=paste("garmin 2011-2012, N =", nrow(garmin_2012)))

#garmin tracks by year: eliminate summer months
garmin_tracks$datetime <- as.POSIXlt(garmin_tracks$date, origin="1970-01-01")
garmin_tracks$month <- strftime(garmin_tracks$datetime, format="%m")
garmin_winter <- garmin_tracks[garmin_tracks$month %in% c("11", "12", "01", "02", "03", "04") & garmin_tracks$date > "2011-11-01",]
proj4string(garmin_winter) <- CRS("+proj=longlat")
writeOGR(garmin_winter, ".", "garmin_tracks_2012-2015", driver="ESRI Shapefile")

garmin_daily <- aggregate(tcx~date, data=garmin_winter, length)
plot(tcx~date, data=garmin_daily, type="l", ylab="number of tracks")
qplot(date, tcx, data=garmin_daily, geom="line", ylab="number of tracks")

#monthly
garmin_monthly <- aggregate(tcx~month, data=garmin_winter, length)
garmin_monthly2 <- rbind(garmin_monthly[5:6,], garmin_monthly[1:4,])
garmin_monthly2$month2 <- c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr")
barplot(garmin_monthly2$tcx / 4, names.arg=garmin_monthly2$month2, ylab="Number of tracks")

#garmin tracks by day of week
garmin_winter$weekday <- strftime(garmin_winter$datetime, "%w")
garmin_weekday <- aggregate(tcx~weekday, data=garmin_winter, length)
weekdays <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
barplot(garmin_weekday$tcx, ylab="Number of tracks", names.arg=weekdays)

#yearly-seasonal 
