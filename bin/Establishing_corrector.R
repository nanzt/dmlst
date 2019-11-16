
source("dailyMeanLSTMethods.R")


#' Prepare necessary data for corrector functions. Those means are computed on a specified number of pixels 
#' with complete four times of observations by the Cos-Sin method with all combinations of observation availabity. 
#'
#' @param data_ForCorrector A data frame combines the means computed by the pixels with complete 4 obs in the previous steps 
#' which can be used for correction without further processing and the pixels with complete 4 obs but not yet processed for
#' means which should be further processed before using in correction.
#' @param verbose Verbose mode
#' @param d The processing date.
#'
#' @return A data frame ready for correction.
#' @export
#'
#' @examples
Establishing_corrector <- function(data_ForCorrector,verbose,d)
{
  Predicted_SinLineardata_ForCorrect <- data_ForCorrector[,'SinLineardata_means']
  Raw_SinLineardata <- data_ForCorrector[,2:11]

  sinCosTerraMeans_ForCorrect=data.frame(0)
  sinCosTerraMeans_ForCorrect=sinCosTerraMeans_ForCorrect[-1,]
  sinCosAquaMeans_ForCorrect=data.frame(0)
  sinCosAquaMeans_ForCorrect=sinCosAquaMeans_ForCorrect[-1,]
  sinCosTerraDayAquaNightMeans_ForCorrect=data.frame(0)
  sinCosTerraDayAquaNightMeans_ForCorrect=sinCosTerraDayAquaNightMeans_ForCorrect[-1,]
  sinCosAquaDayTerraNightMeans_ForCorrect=data.frame(0)
  sinCosAquaDayTerraNightMeans_ForCorrect=sinCosAquaDayTerraNightMeans_ForCorrect[-1,]
  sinCosTerraDayAquaDayTerraNightMeans_ForCorrect=data.frame(0)
  sinCosTerraDayAquaDayTerraNightMeans_ForCorrect=sinCosTerraDayAquaDayTerraNightMeans_ForCorrect[-1,]
  sinCosTerraDayAquaDayAquaNightMeans_ForCorrect=data.frame(0)
  sinCosTerraDayAquaDayAquaNightMeans_ForCorrect=sinCosTerraDayAquaDayAquaNightMeans_ForCorrect[-1,]
  sinCosTerraDayTerraNightAquaNightMeans_ForCorrect=data.frame(0)
  sinCosTerraDayTerraNightAquaNightMeans_ForCorrect=sinCosTerraDayTerraNightAquaNightMeans_ForCorrect[-1,]
  sinCosAquaDayTerraNightAquaNightMeans_ForCorrect=data.frame(0)
  sinCosAquaDayTerraNightAquaNightMeans_ForCorrect=sinCosAquaDayTerraNightAquaNightMeans_ForCorrect[-1,]
  
  data_ForCorrect=data.frame(0)
  data_ForCorrect=data_ForCorrect[-1,]
  length2 <- nrow(data_ForCorrector)
  export <- paste("Preparing corrector functions using",length2,"training pixels","...")
  message(export)

  # compute all means on the unprocessed four obs pixels with all methods (Cos-Sin with different inputs)
  sinCosTerraMeans_ForCorrect <- SinCos_Terra(Raw_SinLineardata, d)
  sinCosAquaMeans_ForCorrect <- SinCos_Aqua(Raw_SinLineardata, d)
  sinCosTerraDayAquaNightMeans_ForCorrect <- SinCos_TerraDayAquaNight(Raw_SinLineardata, d)
  sinCosAquaDayTerraNightMeans_ForCorrect <- SinCos_AquaDayTerraNight(Raw_SinLineardata, d)
  sinCosTerraDayAquaDayTerraNightMeans_ForCorrect <- SinCos_TerraDayAquaDayTerraNight(Raw_SinLineardata, d)
  sinCosTerraDayAquaDayAquaNightMeans_ForCorrect <- SinCos_TerraDayAquaDayAquaNight(Raw_SinLineardata, d)
  sinCosTerraDayTerraNightAquaNightMeans_ForCorrect <- SinCos_TerraDayTerraNightAquaNight(Raw_SinLineardata, d)
  sinCosAquaDayTerraNightAquaNightMeans_ForCorrect <- SinCos_AquaDayTerraNightAquaNight(Raw_SinLineardata, d)


  export2 <- paste("...",length2,"pixels ready.")
  message(export2)
  data_ForCorrect <- cbind(Predicted_SinLineardata_ForCorrect,sinCosTerraMeans_ForCorrect,
                           sinCosAquaMeans_ForCorrect,sinCosTerraDayAquaNightMeans_ForCorrect,
                           sinCosAquaDayTerraNightMeans_ForCorrect,sinCosTerraDayAquaDayTerraNightMeans_ForCorrect,
                           sinCosTerraDayAquaDayAquaNightMeans_ForCorrect,
                           sinCosTerraDayTerraNightAquaNightMeans_ForCorrect,
                           sinCosAquaDayTerraNightAquaNightMeans_ForCorrect)
  cnames3=c('sin_linear','sinCosTerraMeans','sinCosAquaMeans','sinCosTerraDayAquaNightMeans','sinCosAquaDayTerraNightMeans',
            'sinCosTerraDayAquaDayTerraNightMeans','sinCosTerraDayAquaDayAquaNightMeans',
            'sinCosTerraDayTerraNightAquaNightMeans','sinCosAquaDayTerraNightAquaNightMeans')
  colnames(data_ForCorrect)=cnames3
  
  # return data ready for correction
  # this data.frame can be used to establish correction functions
  # this data.frame is prepared for Correct()
  return(data_ForCorrect)
}