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
#' @return a data frame vector with sensitivity, specificity,
#' false positive, false negative

runValidation <- function(cloudy_days, sunny_days, dataFolder) {
  validation_result <- data.frame()
  #cloud_file <- "C:/jiri/Dropbox/PHD/crowdsourcing/data/modis/cloud_percent.csv"

  #cloud_table <- read.csv(cloud_file, header=TRUE, stringsAsFactors = FALSE)
  #sunny_days <- getSampleDates(cloud_table, N=10, minCloud = 0, maxCloud = 25, maxMonth = 4, maxDay = 15, minMonth = 11, minDay = 15)
  #cloudy_days <- getSampleDates(cloud_table, N=10, minCloud = 75, maxCloud = 100, maxMonth = 4, maxDay = 15, minMonth = 11, minDay = 15)

  studyArea <- getCzechia()
  originalDate <- sunny_days[1]
  cloudyDate <- cloudy_days[1]

  for(originalDate in sunny_days) {
    for(cloudyDate in cloudy_days) {
      print(paste("validation for", originalDate, cloudyDate))

      err <- tryCatch({

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
        lines(tracks, col="red")
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



        prob <- getSnowProbability(modisCloud, stations, reports, tracks)
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
        probReal[probReal == 2] <- 0

        testDF <- as.data.frame(probTest)
        realDF <- as.data.frame(probReal)
        comparisonDF <- cbind(testDF, realDF)
        validationDF <- comparisonDF[complete.cases(comparisonDF),]

        predictedFile <- paste("predicted", originalDate, cloudyDate, "r.tif", sep="_")
        writeRaster(prob, paste(dataFolder, predictedFile, sep="/"))

        validation <- data.frame(id=1:nrow(validationDF), observed=validationDF[,2],
                                 predicted=validationDF[,1])
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
        commissionError <- falsePos / (falsePos + sensitivity)
        omissionError <- falseNeg / (falseNeg + specificity)

        result <- data.frame(originalDate, cloudyDate,
                             sensitivity, specificity, falsePos, falseNeg,
                             PCC, PCC.sd = PCC_full$PCC.sd, kappa,
                             kappa.sd = kappa_full$Kappa.sd, AUC,
                             commissionError, omissionError)



        #validation <- validatePair(sunny_days[1], cloudy_days[1], studyArea, dataFolder)



        validation_result <- rbind(validation_result, result)
      },error = function(e) {
        print(conditionMessage(e))
      })

    }
  }
  validation_result_file <- paste(dataFolder, "/validation", Sys.Date(), ".csv",sep="")
  write.table(validation_result,
              validation_result_file,
              row.names=FALSE, col.names=TRUE, sep=",")

  #make a validation summary
  vsummary1 <- data.frame(originalDate=validation_result$originalDate,
                          cloudyDate=validation_result$cloudyDate,
                          PCC=validation_result$PCC,
                          AUC=validation_result$AUC)
  vsummaryRS <- reshape(vsummary1, timevar = 'cloudyDate', idvar=c('originalDate'), direction='wide')
  PCCmin <- aggregate(PCC~originalDate, data=vsummary1, FUN=min)
  PCCmean <- aggregate(PCC~originalDate, data=vsummary1, FUN=mean)
  PCCmax <- aggregate(PCC~originalDate, data=vsummary1, FUN=max)
  AUCmin <- aggregate(AUC~originalDate, data=vsummary1, FUN=min)
  AUCmean <- aggregate(AUC~originalDate, data=vsummary1, FUN=mean)
  AUCmax <- aggregate(AUC~originalDate, data=vsummary1, FUN=max)

  validation_result_info <- data.frame(date=vsummaryRS$originalDate,
                                       pcc.min=PCCmin$PCC,
                                       pcc.mean=PCCmean$PCC,
                                       pcc.max=PCCmax$PCC,
                                       auc.min=AUCmin$AUC,
                                       auc.mean=AUCmean$AUC,
                                       auc.max=AUCmax$AUC)

  validation_summary_file <- paste(dataFolder, "/validation-summary", Sys.Date(), ".csv",sep="")
  write.table(validation_result_info, validation_summary_file, row.names=FALSE, col.names=TRUE, sep=",")

  #make boxplot
  vsummary1$trial <- rep(1:10, each=10)
  boxplot(PCC~trial, data=vsummary1, xlab='trial', ylab='PCC(%)')

}
