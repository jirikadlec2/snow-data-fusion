#' filterModis
#'
#' This function filters the MODIS raster to remove isolated snow/snowfree patches
#'
#' @import raster
#' @param modis The original modis raster object
#' @return the filtered modis raster object


filterModis <- function(modis) {

  #################################################
  # Majority filter to Fill PATCHES in MODIS
  #################################################
  my_mode <- function(x) {
    zeros <- which(x==0)
    ones <- which(x==1)
    twos <- which(x==2)
    counts <- c(length(zeros), length(ones), length(twos))
    res <- c(0, 1, 2)

    maxcount <- max(counts)
    out <- max(which(counts==maxcount)) - 1
    return(out)
  }

  my_matrix <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), nrow=5)
  modis_filter <- focal(modis, my_matrix, fun=my_mode)
  return(modis_filter)
}
