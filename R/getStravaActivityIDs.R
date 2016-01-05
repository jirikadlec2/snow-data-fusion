#' getStravaActivityIDs
#'
#' This function gets list of activity ID's from Strava
#'
#' @import XML
#' @import httr
#' @import sp
#' @param activities_html The html file with the activity ID's
#' @return a vector of the activity ID's

getStravaActivityIDs <- function (activities_html) {
  html.raw <- htmlTreeParse(activities_html, useInternalNodes=TRUE)
  html.parse <- xpathApply(html.raw, "//a", xmlAttrs)
  activ.parse <- grep('*/activities/[0-9]+', unlist(html.parse), value=TRUE)
  
  activity_ids <- c()
  
  i <- 1
  for (i in 1: length(activ.parse)) {
    activ.parse[i]
    activity_id <- as.numeric(substr(activ.parse[i], 13, 23))
    activity_ids <- c(activity_ids, activity_id)
  }
  return(activity_ids)
}