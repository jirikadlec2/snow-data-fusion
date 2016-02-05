#' validatePair
#'
#' This function runs a validation by imposing a cloud on
#' a original dataset, and validating the accuracy
#'
#' @import raster
#' @import rgdal
#' @import PresenceAbsence
#' @param originalDate The reclassified modis raster object
#' @param cloudyDate The reclassified modis cloudy raster object,
#' to be imposed
#' @param studyArea the study area 'mask' to be imposed
#' @param dataFolder the folder where the data is located
#' @return a named vector with sensitivity, specificity, false positive, false negative

validatePair <- function(originalDate, cloudyDate, studyArea, dataFolder=".", useReports=TRUE, useTracks=TRUE, tr=NULL) {

  originalFile <- paste(dataFolder, "/", "modis", originalDate, ".tif", sep="")
  cloudyFile <- paste(dataFolder, "/", "modis", cloudyDate, ".tif", sep="")

  # if files don't exist, download them
  if (!file.exists(originalFile)) {
    getModis(originalDate, dataFolder)
  }
  if (!file.exists(cloudyFile)) {
    getModis(cloudyDate, dataFolder)
  }

  # for plotting of snow colors
  snow_colors <- c("beige", "blue", "gray")

  original <- raster(originalFile)
  cloudy <- raster(cloudyFile)
  originalR <- reclassModis(original)
  cloudyR <- reclassModis(cloudy)
  plot(cloudyR)

  plot(cloudyR, col=snow_colors, axes=FALSE, box=FALSE, legend=FALSE)
  legend_x = bbox(originalR)[1,1]+100000
  legend_y = bbox(originalR)[2,2]
  legend(x=legend_x, y=legend_y, legend = c("Snow-free", "Snow", "Cloud"),
         fill = snow_colors, cex=0.7, horiz=TRUE)
  plot(studyArea, add=TRUE)

  #cloud mask for CZECHIA for display
  cloudyR_cloud <- cloudyR == 2
  cloudyR[cloudyR == 0] <- NA
  cloudyR_cze <- mask(cloudyR, studyArea)
  par(mar=c(0,0,0,0))
  plot(cloudyR_cze, col=c("gray"), axes=FALSE, box=FALSE, legend=FALSE)
  legend_x = bbox(cloudyR_cze)[1,1]+100000
  legend_y = bbox(cloudyR_cze)[2,2]
  legend(x=legend_x, y=legend_y, legend = c("Cloud"),
         fill = c("gray"), cex=0.7, horiz=TRUE)

  #mask - czechia for display
  originalR_cze <- mask(originalR, studyArea)
  par(mar=c(0,0,0,0))
  plot(originalR_cze, col=snow_colors, axes=FALSE, box=FALSE, legend=FALSE)
  legend_x = bbox(originalR_cze)[1,1]+100000
  legend_y = bbox(originalR_cze)[2,2]
  legend(x=legend_x, y=legend_y, legend = c("Snow-free", "Snow", "Cloud"),
         fill = snow_colors, cex=0.7, horiz=TRUE)

  modisCloud <- imposeCloud(originalR, cloudyR)
  combinedR_cze <- mask(modisCloud, studyArea)
  par(mar=c(0,0,0,0))
  plot(combinedR_cze, col=snow_colors, axes=FALSE, box=FALSE, legend=FALSE)
  legend_x = bbox(originalR_cze)[1,1]+100000
  legend_y = bbox(originalR_cze)[2,2]
  legend(x=legend_x, y=legend_y, legend = c("Snow-free", "Snow", "Cloud"),
         fill = snow_colors, cex=0.7, horiz=TRUE)

  stations <- getStations(originalDate)
  reports <- getReports(originalDate)
  tracks <- getTracks(originalDate)


  points(stations)

  # plot also with the stations, points, tracks
  par(mar=c(0,0,0,0))
  plot(combinedR_cze, col=snow_colors, axes=FALSE, box=FALSE, legend=FALSE)
  if(nrow(tracks) > 0) {
    lines(tracks, col="red")
  }
  points(stations, pch=1)
  points(reports, pch=16)
  legend_x = bbox(originalR_cze)[1,1]+100000
  legend_y = bbox(originalR_cze)[2,2]
  legend_y2 = bbox(originalR_cze)[2,1]
  legend(x=legend_x, y=legend_y,
         legend = c("Snow-free", "Snow", "Cloud"),
         fill = snow_colors,
         cex=0.7,
         horiz=TRUE)
  legend(x=legend_x, y=legend_y2, legend=c("tracks", "stations + reports"),
         col=c("red", "black"),
         lty=c(1, 0),
         pch=c(NA, 16),
         cex=0.7,
         horiz=TRUE)

  if (!useReports) {
    reports <- data.frame()
  }
  if (!useTracks) {
    tracks <- data.frame()
  }

  prob <- getSnowProbabilityLeastCost(tr, modisCloud, stations, reports, tracks)
  prob_cze <- mask(prob, studyArea)

  prob_colors <- brewer.pal(7, "Blues")
  plot(prob_cze, col=prob_colors, axes=FALSE, box=FALSE, legend=TRUE)

  #snow extent (threshold=0.5)
  extent <- prob > 0.5
  snow_colors <- c("beige", "blue")
  extent_cze <- mask(extent, studyArea)
  par(mar=c(0,0,0,0))
  plot(extent_cze, col=snow_colors, axes=FALSE, box=FALSE, legend=FALSE)
  legend_x = bbox(originalR_cze)[1,1]+100000
  legend_y = bbox(originalR_cze)[2,2]
  legend(x=legend_x, y=legend_y, legend = c("Snow-free", "Snow"),
         fill = snow_colors, cex=0.7, horiz=TRUE)


  #now mask the calculated prob by the imposed cloud mask
  imposedCloudMask <- cloudyR == 2
  imposedCloudMask[imposedCloudMask == 0] <- NA
  probTest <- mask(prob, imposedCloudMask)
  probReal <- mask(originalR, imposedCloudMask)

  #now also mask by the study area mask
  probTest <- mask(probTest, studyArea)
  probReal <- mask(probReal, studyArea)

  #remove cloud-covered areas from probReal: we don't test for them
  probReal[probReal == 2] <- NA

  plot(probTest, main="probTest")
  plot(probReal, main="probReal")

  testVals <- values(probTest)
  realVals <- values(probReal)
  comparisonDF <- data.frame(observed=realVals, predicted=testVals)
  validationDF <- comparisonDF[complete.cases(comparisonDF),]

  predictedFile <- paste("predicted", originalDate, cloudyDate, "r.tif", sep="_")
  writeRaster(prob, paste(dataFolder, predictedFile, sep="/"), overwrite=TRUE)

  validation <- data.frame(id=1:nrow(validationDF), observed=validationDF$observed,
                           predicted=validationDF$predicted)
  rocFile <- paste("roc", originalDate, cloudyDate, "r.png", sep="_")

  #to save PNG image of the ROC file
  png(paste(dataFolder, rocFile, sep="/"))
  auc.roc.plot(validation)
  dev.off()

  #to get the confusion matrix
  confmat <- cmx(validation)
  sensitivity <- confmat[1,1]
  specificity <- confmat[2,2]
  falsePos <- confmat[1,2]
  falseNeg <- confmat[2,1]
  PCC_full <- pcc(confmat)
  PCC <- PCC_full$PCC
  kappa_full <- Kappa(confmat)
  kappa <- kappa_full$Kappa
  AUC <- auc(validation, st.dev=FALSE)
  comEr <- falsePos / (falsePos + sensitivity)
  omEr <- falseNeg / (falseNeg + specificity)

  # check number of stations, tracks, reports
  Nstations <- nrow(stations)
  Ntracks <- nrow(tracks)
  Nreports <- nrow(reports)

  result <- data.frame(originalDate, cloudyDate,
                       Nstations, Nreports, Ntracks,
                       sensitivity, specificity, falsePos, falseNeg,
                       PCC, kappa, AUC, comEr, omEr)

  resultF <- paste(dataFolder, "result", "_", originalDate, "_",
                   cloudyDate, ".csv", sep="")
  write.csv(result, resultF)

  # print out some results
  result_summary <- sprintf("%s %s PCC=%2f kappa=%2f AUC=%2f ComE=%2f OmE=%2f",
                            originalDate, cloudyDate, PCC, kappa, AUC,
                            comEr, omEr)
  print(result_summary)
  return(result)
}
