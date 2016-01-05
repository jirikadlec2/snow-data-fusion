#' getGarminActivityIDs
#'
#' This function gets list of activity ID's from Garmin Connect
#'
#' @import XML
#' @import httr
#' @import sp
#' @param activities_html The html file with the activity ID's
#' @return a vector of the activity ID's

getGarminActivityIDs <- function (activities_file) {
  
  activities <- read.table(activities_file, stringsAsFactors=FALSE)
  activity_list <- c()
  
  i <- 1
  for (i in 1: nrow(activities)) {
    print(i)
    act_url <- activities$V1[i]
    activity_id <- substr(act_url, 37, nchar(act_url))
    activity_list <- c(activity_list, activity_id)
  }
  return(activity_list)
}