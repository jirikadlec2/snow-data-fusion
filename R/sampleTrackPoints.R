#' sampleTrackPoints
#'
#' This function gets a sample of track points (to simplify the tracks)
#'
#' @import sp
#' @param pointSpacing spacing of points (in meters) for simplified track
#' @return the spatialPoints object with sampled track points


sampleTrackPoints <- function(tracks, pointSpacing=5000) {

  # track lengths
  trackLengths <- SpatialLinesLengths(tracks)
  pointsPerTrack <- ceiling(trackLengths / pointSpacing)

  myCoords <- c()
  i <- 1
  nTracks <- nrow(tracks)
  for (i in 1: nTracks) {
    track1 <- tracks[i,]@lines[[1]]@Lines[[1]]
    track1coords <- track1@coords
    ncoords <- dim(track1coords)[1]
    samp <- sample(1:ncoords, size=pointsPerTrack[i])
    sampleCoords <- track1coords[samp,]

    myCoords <- rbind(myCoords, sampleCoords)
  }
  myCoordsDF <- data.frame(x=myCoords[,1], y=myCoords[,2])
  coordinates(myCoordsDF) <- ~x+y
  return(myCoordsDF)
}
