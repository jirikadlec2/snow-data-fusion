#' runValidation
#'
#' This function runs a validation by imposing a cloud on
#' a multiple datasets, and validating the accuracy
#'
#' @import raster
#' @import rgdal
#' @import PresenceAbsence
#' @import RColorBrewer
#' @param cloudy_days a vector of selected cloudy dates for validation
#' @param sunny_days a vector of selected cloud-free dates for validation
#' @param dataFolder the data folder with MODIS rasters
#' @param outputFile the output file name
#' @param useReports determines if we should use the reports
#' @param useTracks determines if we should use the tracks
#' @return a data frame vector with sensitivity, specificity,
#' false positive, false negative

runValidation <- function(cloudy_days, sunny_days, dataFolder, outputFile, useReports=TRUE, useTracks=TRUE) {
  validation_result <- data.frame()

  studyArea <- getCzechia()
  originalDate <- sunny_days[1]
  cloudyDate <- cloudy_days[1]

  # magnifying elevation effect by exponent
  dem <- raster("C:/jiri/Dropbox/PHD/crowdsourcing/data/DEM/dem_utm_500m.tif")
  transition_exponent <- 1.5
  tr <- transition(dem, function(x) {1 / (mean(x))^transition_exponent}, 8)
  tr <- geoCorrection(tr)

  for(originalDate in sunny_days) {
    for(cloudyDate in cloudy_days) {
      # print(paste("validation for", originalDate, cloudyDate))

      #err <- tryCatch({
        val_result <- validatePair(originalDate, cloudyDate, studyArea, dataFolder, useReports, useTracks, tr)
        validation_result <- rbind(validation_result, val_result)
      #}, error = function(e) { print(conditionMessage(e)) }
      #)
    }
  }
  validation_result_file <- paste(dataFolder, "/validation", outputFile, Sys.Date(), ".csv",sep="")
  write.table(validation_result,
              validation_result_file,
              row.names=FALSE, col.names=TRUE, sep=",")

  #make a validation summary
  vsummary1 <- data.frame(originalDate=validation_result$originalDate,
                          cloudyDate=validation_result$cloudyDate,
                          PCC=validation_result$PCC,
                          AUC=validation_result$AUC,
                          kappa=validation_result$kappa,
                          comEr=validation_result$comEr,
                          omEr=validation_result$omEr)
  vsummaryRS <- reshape(vsummary1, timevar = 'cloudyDate', idvar=c('originalDate'), direction='wide')
  PCCmin <- aggregate(PCC~originalDate, data=vsummary1, FUN=min)
  PCCmean <- aggregate(PCC~originalDate, data=vsummary1, FUN=mean)
  PCCmax <- aggregate(PCC~originalDate, data=vsummary1, FUN=max)
  AUCmin <- aggregate(AUC~originalDate, data=vsummary1, FUN=min)
  AUCmean <- aggregate(AUC~originalDate, data=vsummary1, FUN=mean)
  AUCmax <- aggregate(AUC~originalDate, data=vsummary1, FUN=max)
  kappamin <- aggregate(kappa~originalDate, data=vsummary1, FUN=min)
  kappamean <- aggregate(kappa~originalDate, data=vsummary1, FUN=mean)
  kappamax <- aggregate(kappa~originalDate, data=vsummary1, FUN=max)
  ComEmin <- aggregate(comEr~originalDate, data=vsummary1, FUN=min)
  ComEmean <- aggregate(comEr~originalDate, data=vsummary1, FUN=mean)
  ComEmax <- aggregate(comEr~originalDate, data=vsummary1, FUN=max)
  omEmin <- aggregate(omEr~originalDate, data=vsummary1, FUN=min)
  omEmean <- aggregate(omEr~originalDate, data=vsummary1, FUN=mean)
  omEmax <- aggregate(omEr~originalDate, data=vsummary1, FUN=max)

  validation_result_info <- data.frame(date=vsummaryRS$originalDate,
                                       pcc.min=PCCmin$PCC,
                                       pcc.mean=PCCmean$PCC,
                                       pcc.max=PCCmax$PCC,
                                       auc.min=AUCmin$AUC,
                                       auc.mean=AUCmean$AUC,
                                       auc.max=AUCmax$AUC,
                                       kappa.min=kappamin$kappa,
                                       kappa.mean=kappamean$kappa,
                                       kappa.max=kappamax$kappa,
                                       comEr.min=ComEmin$comEr,
                                       comEr.mean=ComEmean$comEr,
                                       comEr.max=ComEmax$comEr,
                                       omEr.min=omEmin$omEr,
                                       omEr.mean=omEmean$omEr,
                                       omEr.max=omEmax$omEr)

  validation_summary_file <- paste(dataFolder, "/", outputFile, "-summary", Sys.Date(), ".csv",sep="")
  write.table(validation_result_info, validation_summary_file, row.names=FALSE, col.names=TRUE, sep=",")

  #make boxplot
  #vsummary1$trial <- rep(1:10, each=10)
  #boxplot(PCC~trial, data=vsummary1, xlab='trial', ylab='PCC(%)')

  return(list(vsummary=vsummary1, vresult=validation_result))

}
