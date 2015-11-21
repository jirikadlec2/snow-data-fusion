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

validatePair <- function(originalDate, cloudyDate, studyArea, dataFolder=".") {

  originalFile <- paste(dataFolder, "/", "modis", originalDate, ".tif", sep="")
  cloudyFile <- paste(dataFolder, "/", "modis", cloudyDate, ".tif", sep="")

  original <- raster(originalFile)
  cloudy <- raster(cloudyFile)
  originalR <- reclassModis(original)
  cloudyR <- reclassModis(cloudy)
  modisCloud <- imposeCloud(originalR, cloudyR)
  stations <- getStations(originalDate)
  reports <- getReports(originalDate)
  tracks <- getTracks(originalDate)
  prob <- getSnowProbability(modisCloud, stations, reports, tracks)

  #now mask the calculated prob by the imposed cloud mask
  imposedCloudMask <- cloudyR == 2
  imposedCloudMask[imposedCloudMask == 0] <- NA
  probTest <- mask(prob, imposedCloudMask)
  probReal <- mask(originalR, imposedCloudMask)

  #now also mask by the study area mask
  probTest <- mask(probTest, studyArea)
  probReal <- mask(probReal, studyArea)

  testDF <- as.data.frame(probTest)
  realDF <- as.data.frame(probReal)
  comparisonDF <- cbind(testDF, realDF)
  validationDF <- comparisonDF[complete.cases(comparisonDF),]

  predictedFile <- paste("predicted", originalDate, cloudyDate, "r.tif", sep="_")
  writeRaster(prob, predictedFile)

  validation <- data.frame(id=1:nrow(validationDF), observed=validationDF[,2],
                           predicted=validationDF[,1])
  rocFile <- paste("roc", originalDate, cloudyDate, "r.png", sep="_")

  png(rocFile)
    auc.roc.plot(validation)
  dev.off()

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

  result <- data.frame(originalDate, cloudyDate,
                       sensitivity, specificity, falsePos, falseNeg,
                       PCC, PCC.sd = PCC_full$PCC.sd, kappa,
                       kappa.sd = kappa_full$Kappa.sd, AUC)
  return(result)
}
