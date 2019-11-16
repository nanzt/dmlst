
source("Prediction_priv.R")

Predict <- function(lstDS2,output_params,d)
{
  export_Pixelnum <- nrow(lstDS2)
  export <- paste("Start to predict daily mean LST for",export_Pixelnum,"pixels","...")
  message(export)
  # this data.frame stores means and associated correlation values computed on the pixels
  # with incomplete LST observations 
  Predicted_data=data.frame(0)
  Predicted_data=Predicted_data[-1,]

  # compute means for the pixels, which have different times of LST obs, 
  # by calling corresponding methods
  sinCosTerra_data <- subset(lstDS2,(TerraDay!=0 & TerraNight!=0 & TerraDayViewtime!=255 & TerraNightViewtime!=255)&
                                  (AquaDay==0 & AquaNight==0 & AquaDayViewtime==255 & AquaNightViewtime==255))
  sinCosAqua_data <- subset(lstDS2,(AquaDay!=0 & AquaNight!=0 & AquaDayViewtime!=255 & AquaNightViewtime!=255)&
                                  (TerraDay==0 & TerraNight==0 & TerraDayViewtime==255 & TerraNightViewtime==255))
  sinCosTerraDayAquaNight_data <- subset(lstDS2,(TerraDay!=0 & AquaNight!=0 & TerraDayViewtime!=255 & AquaNightViewtime!=255)&
                                  (TerraNight==0 & AquaDay==0 & TerraNightViewtime==255 & AquaDayViewtime==255))
  sinCosAquaDayTerraNight_data <- subset(lstDS2,(TerraNight!=0 & AquaDay!=0 & TerraNightViewtime!=255 & AquaDayViewtime!=255)&
                                  (TerraDay==0 & AquaNight==0 & TerraDayViewtime==255 & AquaNightViewtime==255))
  sinCosTerraDayAquaDayTerraNight_data <- subset(lstDS2,(TerraDay!=0 & TerraNight!=0 & AquaDay!=0 & TerraDayViewtime!=255 
                                                         & TerraNightViewtime!=255 & AquaDayViewtime!=255)&
                                  (AquaNight==0 & AquaNightViewtime==255))
  sinCosTerraDayAquaDayAquaNight_data <- subset(lstDS2,(TerraDay!=0 & AquaDay!=0 & AquaNight!=0 & TerraDayViewtime!=255 
                                                        & AquaDayViewtime!=255 & AquaNightViewtime!=255)&
                                  (TerraNight==0 & TerraNightViewtime==255))
  sinCosTerraDayTerraNightAquaNight_data <- subset(lstDS2,(TerraDay!=0 & TerraNight!=0 & AquaNight!=0 & TerraDayViewtime!=255 
                                                           & TerraNightViewtime!=255 & AquaNightViewtime!=255)&
                                  (AquaDay==0 & AquaDayViewtime==255))
  sinCosAquaDayTerraNightAquaNight_data <- subset(lstDS2,(TerraNight!=0 & AquaDay!=0 & AquaNight!=0 & TerraNightViewtime!=255 
                                                          & AquaDayViewtime!=255 & AquaNightViewtime!=255)&
                                  (TerraDay==0 & TerraDayViewtime==255))
  sinLinear_data <- subset(lstDS2,(TerraDay!=0 & TerraNight!=0 & AquaDay!=0 & AquaNight!=0 & TerraDayViewtime!=255 
                                   & TerraNightViewtime!=255 & AquaDayViewtime!=255 & AquaNightViewtime!=255))

if(nrow(sinCosTerra_data)!= 0)
{
  Predicted_sinCosTerraMeans_data <- SinCos_Terra_DF(sinCosTerra_data, d)
  Predicted_data <- rbind(Predicted_data,Predicted_sinCosTerraMeans_data)
}
if(nrow(sinCosAqua_data)!= 0)
{
  Predicted_sinCosAquaMeans_data <- SinCos_Aqua_DF(sinCosAqua_data, d)
  Predicted_data <- rbind(Predicted_data,Predicted_sinCosAquaMeans_data)
}
if(nrow(sinCosTerraDayAquaNight_data)!= 0)
{
  Predicted_sinCosTerraDayAquaNightMeans_data <- SinCos_TerraDayAquaNight_DF(sinCosTerraDayAquaNight_data, d)
  Predicted_data <- rbind(Predicted_data,Predicted_sinCosTerraDayAquaNightMeans_data)
}
if(nrow(sinCosAquaDayTerraNight_data)!= 0)
{
  Predicted_sinCosAquaDayTerraNightMeans_data <- SinCos_AquaDayTerraNight_DF(sinCosAquaDayTerraNight_data, d)
  Predicted_data <- rbind(Predicted_data,Predicted_sinCosAquaDayTerraNightMeans_data)
}
if(nrow(sinCosTerraDayAquaDayTerraNight_data)!= 0)
{
  Predicted_sinCosTerraDayAquaDayTerraNightMeans_data <- SinCos_TerraDayAquaDayTerraNight_DF(
    sinCosTerraDayAquaDayTerraNight_data, d)
  Predicted_data <- rbind(Predicted_data,Predicted_sinCosTerraDayAquaDayTerraNightMeans_data)
}
if(nrow(sinCosTerraDayAquaDayAquaNight_data)!= 0)
{
  Predicted_sinCosTerraDayAquaDayAquaNightMeans_data <- SinCos_TerraDayAquaDayAquaNight_DF(
    sinCosTerraDayAquaDayAquaNight_data, d)
  Predicted_data <- rbind(Predicted_data,Predicted_sinCosTerraDayAquaDayAquaNightMeans_data)
}
if(nrow(sinCosTerraDayTerraNightAquaNight_data)!= 0)
{
  Predicted_sinCosTerraDayTerraNightAquaNightMeans_data <- SinCos_TerraDayTerraNightAquaNight_DF(
    sinCosTerraDayTerraNightAquaNight_data, d)
  Predicted_data <- rbind(Predicted_data,Predicted_sinCosTerraDayTerraNightAquaNightMeans_data)
}
if(nrow(sinCosAquaDayTerraNightAquaNight_data)!= 0)
{
  Predicted_sinCosAquaDayTerraNightAquaNightMeans_data <- SinCos_AquaDayTerraNightAquaNight_DF(
    sinCosAquaDayTerraNightAquaNight_data, d)
  Predicted_data <- rbind(Predicted_data,Predicted_sinCosAquaDayTerraNightAquaNightMeans_data)
}
if(nrow(sinLinear_data)!= 0)
{
  Predicted_sinLinear_data <- sinlinear_DF(sinLinear_data, d)
  Predicted_data <- rbind(Predicted_data,Predicted_sinLinear_data)
}

  export2 <- paste("...",export_Pixelnum,"Pixels done.")
  message(export2)

  FixedOutput_colnames <- c('lon','lat','method','TerraDay','TerraNight','AquaDay','AquaNight',
                            'TerraDayViewtime','TerraNightViewtime','AquaDayViewtime','AquaNightViewtime','means')
  Output_colnames <- c(FixedOutput_colnames,output_params)
  Predicted_data <- subset(Predicted_data, select=Output_colnames)
  
  return(Predicted_data)

}