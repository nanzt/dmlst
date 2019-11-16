
#' Perform the correction step
#'
#' @param data_ForCorrect The training set for correction, which can be prepared by Establishing_corrector().
#' @param Predicted_data The dataset to be corrected, which is returned by the prediction step.
#' @param verbose Verbose mode.
#'
#' @return The corrected dataset
#' @export
#'
#' @examples
Correct <- function(data_ForCorrect,Predicted_data,verbose){
  # the sin_linear results on the 4-obs pixels. Compute their CDF. 
  sinLinear_CDF <- ecdf(data_ForCorrect[,'sin_linear'])
  maximum <- ceiling(max(data_ForCorrect[,'sin_linear']))
  minimum <-  floor(min(data_ForCorrect[,'sin_linear']))
  invcdf_yoh <- inverse(sinLinear_CDF, lower = minimum, upper = maximum) 
  # the results from various cos-sin methods performed on 4-obs pixels. Compute their CDF. 
  sinCosTerraMeans_CDF <- ecdf(data_ForCorrect[,'sinCosTerraMeans'])
  sinCosAquaMeans_CDF <- ecdf(data_ForCorrect[,'sinCosAquaMeans'])
  sinCosTerraDayAquaNightMeans_CDF <- ecdf(data_ForCorrect[,'sinCosTerraDayAquaNightMeans'])
  sinCosAquaDayTerraNightMeans_CDF <- ecdf(data_ForCorrect[,'sinCosAquaDayTerraNightMeans'])
  sinCosTerraDayAquaDayTerraNightMeans_CDF <- ecdf(data_ForCorrect[,'sinCosTerraDayAquaDayTerraNightMeans'])
  sinCosTerraDayAquaDayAquaNightMeans_CDF <- ecdf(data_ForCorrect[,'sinCosTerraDayAquaDayAquaNightMeans'])
  sinCosTerraDayTerraNightAquaNightMeans_CDF <- ecdf(data_ForCorrect[,'sinCosTerraDayTerraNightAquaNightMeans'])
  sinCosAquaDayTerraNightAquaNightMeans_CDF <- ecdf(data_ForCorrect[,'sinCosAquaDayTerraNightAquaNightMeans'])
  # the correct functions for all Cos-sin variants
  sinCosTerraMeans_Correct <- function(x){
    
    fmh <- sinCosTerraMeans_CDF(x)
    correxmp <- invcdf_yoh(fmh)
    return(correxmp)
  }
  sinCosAquaMeans_Correct <- function(x){
    
    fmh <- sinCosAquaMeans_CDF(x)
    correxmp <- invcdf_yoh(fmh)
    return(correxmp)
  }
  sinCosTerraDayAquaNightMeans_Correct <- function(x){
    
    fmh <- sinCosTerraDayAquaNightMeans_CDF(x)
    correxmp <- invcdf_yoh(fmh)
    return(correxmp)
  }
  sinCosAquaDayTerraNightMeans_Correct <- function(x){
    
    fmh <- sinCosAquaDayTerraNightMeans_CDF(x)
    correxmp <- invcdf_yoh(fmh)
    return(correxmp)
  }
  sinCosTerraDayAquaDayTerraNightMeans_Correct <- function(x){
    
    fmh <- sinCosTerraDayAquaDayTerraNightMeans_CDF(x)
    correxmp <- invcdf_yoh(fmh)
    return(correxmp)
  }
  sinCosTerraDayAquaDayAquaNightMeans_Correct <- function(x){
    
    fmh <- sinCosTerraDayAquaDayAquaNightMeans_CDF(x)
    correxmp <- invcdf_yoh(fmh)
    return(correxmp)
  }
  sinCosTerraDayTerraNightAquaNightMeans_Correct <- function(x){
    
    fmh <- sinCosTerraDayTerraNightAquaNightMeans_CDF(x)
    correxmp <- invcdf_yoh(fmh)
    return(correxmp)
  }
  sinCosAquaDayTerraNightAquaNightMeans_Correct <- function(x){
    
    fmh <- sinCosAquaDayTerraNightAquaNightMeans_CDF(x)
    correxmp <- invcdf_yoh(fmh)
    return(correxmp)
  }
  
  # correction
  Predicted_data_ForCorrect <- nrow(Predicted_data) 
  corrected_data=data.frame(0)
  corrected_data=corrected_data[-1,]
  export <- paste("Start to perform correction to",Predicted_data_ForCorrect,"pixels","...")
  message(export)

  sinCosTerra_data <- subset(Predicted_data,(method=='SinCos_Terra'))
  sinCosTerraMeans <- sinCosTerra_data[,'means']
  sinCosTerraMeans_Corrected_data <- sapply(sinCosTerraMeans,sinCosTerraMeans_Correct)
  #corrected_data <- cbind(corrected_data,sinCosTerraMeans_Corrected_data)

  sinCosAqua_data <- subset(Predicted_data,(method=='SinCos_Aqua'))
  sinCosAquaMeans <- sinCosAqua_data[,'means']
  sinCosAquaMeans_Corrected_data <- sapply(sinCosAquaMeans,sinCosAquaMeans_Correct)
  #corrected_data <- cbind(corrected_data,sinCosAquaMeans_Corrected_data)

  sinCosTerraDayAquaNight_data <- subset(Predicted_data,(method=='SinCos_TerraDayAquaNight'))
  sinCosTerraDayAquaNightMeans <- sinCosTerraDayAquaNight_data[,'means']
  sinCosTerraDayAquaNightMeans_Corrected_data <- sapply(sinCosTerraDayAquaNightMeans,sinCosTerraDayAquaNightMeans_Correct)
  #corrected_data <- cbind(corrected_data,sinCosTerraDayAquaNightMeans_Corrected_data)
  
  sinCosAquaDayTerraNight_data <- subset(Predicted_data,(method=='SinCos_AquaDayTerraNight'))
  sinCosAquaDayTerraNightMeans <- sinCosAquaDayTerraNight_data[,'means']
  sinCosAquaDayTerraNightMeans_Corrected_data <- sapply(sinCosAquaDayTerraNightMeans,sinCosAquaDayTerraNightMeans_Correct)
  #corrected_data <- cbind(corrected_data,sinCosAquaDayTerraNightMeans_Corrected_data)
  
  sinCosTerraDayAquaDayTerraNight_data <- subset(Predicted_data,(method=='SinCos_TerraDayAquaDayTerraNight'))
  sinCosTerraDayAquaDayTerraNightMeans <- sinCosTerraDayAquaDayTerraNight_data[,'means']
  sinCosTerraDayAquaDayTerraNightMeans_Corrected_data <- sapply(sinCosTerraDayAquaDayTerraNightMeans,sinCosTerraDayAquaDayTerraNightMeans_Correct)
  #corrected_data <- cbind(corrected_data,sinCosTerraDayAquaDayTerraNightMeans_Corrected_data)
  
  sinCosTerraDayAquaDayAquaNight_data <- subset(Predicted_data,(method=='SinCos_TerraDayAquaDayAquaNight'))
  sinCosTerraDayAquaDayAquaNightMeans <- sinCosTerraDayAquaDayAquaNight_data[,'means']
  sinCosTerraDayAquaDayAquaNightMeans_Corrected_data <- sapply(sinCosTerraDayAquaDayAquaNightMeans,sinCosTerraDayAquaDayAquaNightMeans_Correct)
  #corrected_data <- cbind(corrected_data,sinCosTerraDayAquaDayAquaNightMeans_Corrected_data)
  
  sinCosTerraDayTerraNightAquaNight_data <- subset(Predicted_data,(method=='SinCos_TerraDayTerraNightAquaNight'))
  sinCosTerraDayTerraNightAquaNightMeans <- sinCosTerraDayTerraNightAquaNight_data[,'means']
  sinCosTerraDayTerraNightAquaNightMeans_Corrected_data <- sapply(sinCosTerraDayTerraNightAquaNightMeans,sinCosTerraDayTerraNightAquaNightMeans_Correct)
  #corrected_data <- cbind(corrected_data,sinCosTerraDayTerraNightAquaNightMeans_Corrected_data)

  sinCosAquaDayTerraNightAquaNight_data <- subset(Predicted_data,(method=='SinCos_AquaDayTerraNightAquaNight'))
  sinCosAquaDayTerraNightAquaNightMeans <- sinCosAquaDayTerraNightAquaNight_data[,'means']
  sinCosAquaDayTerraNightAquaNightMeans_Corrected_data <- sapply(sinCosAquaDayTerraNightAquaNightMeans,sinCosAquaDayTerraNightAquaNightMeans_Correct)
  #corrected_data <- cbind(corrected_data,sinCosAquaDayTerraNightAquaNightMeans_Corrected_data)
  
  sinlinear_data <- subset(Predicted_data,(method=='SinLinear'))
  sinlinearMeans <- sinlinear_data[,'means']
  corrected_data <-  c(sinCosTerraMeans_Corrected_data,sinCosAquaMeans_Corrected_data,
                       sinCosTerraDayAquaNightMeans_Corrected_data,
                       sinCosAquaDayTerraNightMeans_Corrected_data,
                       sinCosTerraDayAquaDayTerraNightMeans_Corrected_data,
                       sinCosTerraDayAquaDayAquaNightMeans_Corrected_data,
                       sinCosTerraDayTerraNightAquaNightMeans_Corrected_data,
                       sinCosAquaDayTerraNightAquaNightMeans_Corrected_data,
                       sinlinearMeans)

  export2 <- paste("...",Predicted_data_ForCorrect,"pixels corrected.")
  message(export2)
  # output the corrected dataset
  return(corrected_data)
}