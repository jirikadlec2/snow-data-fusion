#' getSampleDates
#'
#' This function gets the sample dates based on min/max cloud threshold
#' @param cloudTable the table with two columns: date, cloudpercent
#' @param minCloud the min cloud percent (default=0)
#' @param maxCloud the max cloud percent (default=100)
#' @param N the sample size
#' @return a list of the dates selected

getSampleDates <- function(cloudTable, minCloud = 0, maxCloud = 100, N = 10,
                           maxMonth = 4, maxDay = 15,
                           minMonth = 11, minDay = 15) {

  cloudTable$month <- strftime(as.Date(as.character(cloudTable$datum)), "%m")
  cloudTable$day <- strftime(as.Date(as.character(cloudTable$datum)), "%d")
  cloudTable$auxYear <- "2000"
  cloudTable$DayOfYear <- as.Date(paste(cloudTable$auxYear, cloudTable$month, cloudTable$day, sep="-"))

  eligibleDate1 <- sprintf("%04d-%02d-%02d", 2000, maxMonth, maxDay)
  eligibleDate2 <- sprintf("%04d-%02d-%02d", 2000, minMonth, minDay)
  eligibleTable <- cloudTable[cloudTable$DayOfYear < eligibleDate1 | cloudTable$DayOfYear > eligibleDate2,]

  cloud.days <- eligibleTable[eligibleTable$cloudpercent > minCloud & eligibleTable$cloudpercent < maxCloud,]

  cloud.index <- sample(1:nrow(cloud.days), size=N)
  cloud.random <- cloud.days[cloud.index,]

  return(cloud.random[,1])
}
