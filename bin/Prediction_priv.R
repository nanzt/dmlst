
source("dailyMeanLSTMethods.R")
source("helperfunc.R")

#' Daily means and output params using the Sin-Cos approach with Terra two obvs.
#'
#' @param x a data.frame containing columns: Lat, Lon, TerraDay, TerraNight,
#'   TerraDayViewtime, TerraNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#'
#' @return a dataframe
#' @export
#'
#' @examples
SinCos_Terra_DF <- function(x, d, tmaxshift=0.6)
{
  
  SunVari  <- sunRiseSet(x,d, tmaxshift)
  trise    <- SunVari$trise
  tset     <- SunVari$tset
  daylen   <- SunVari$daylen
  nightlen <- SunVari$nightlen
  tm       <- SunVari$tm
  
  TerraDayUTC <- Viewtime2UTC(x$TerraDayViewtime,x$Lon)
  TerraNightUTC <- Viewtime2UTC(x$TerraNightViewtime,x$Lon)
  AquaDayUTC <- Viewtime2UTC(x$AquaDayViewtime,x$Lon)
  AquaNightUTC <- Viewtime2UTC(x$AquaNightViewtime,x$Lon)
  TerraDay2    <- LST2Celsius(x$TerraDay)
  TerraNight2    <- LST2Celsius(x$TerraNight)
  #AquaDay2    <- LST2Celsius(x$AquaDay)
  #AquaNight2  <- LST2Celsius(x$AquaNight)
  AquaDay2    <- -999
  AquaNight2  <- -999

  matrix2solv <- cbind(x["TerraDay"],x["TerraNight"], TerraDayUTC, TerraNightUTC, trise, tm, daylen, nightlen)
  
  TaT0 <- as.data.frame( t(apply(matrix2solv,1, SinCos2insSolv, DayDataCol="TerraDay", 
                               NightDataCol="TerraNight", DayViewtimeCol="TerraDayUTC", 
                               NightViewtimeCol = "TerraNightUTC", 
                               TriseCol="trise", TmaxCol="tm", DayLengthCol="daylen", NightLengthCol="nightlen")) )
  names(TaT0) <- c("Ta","T0")

  means <- SinCos_Terra(x,d)
  method <- "SinCos_Terra"  

  ABab <- data.frame(0)
  ABab <- ABab[-1,]
  ABab <- cbind(-999,-999,-999,-999)
  names(ABab) <- c("A","B","a","b")
  w <- -999  
  t0 <- -999

  output_DF <- cbind(x$Lon,x$Lat,method,TerraDay2,TerraNight2,AquaDay2,AquaNight2,TerraDayUTC,TerraNightUTC,
                 AquaDayUTC,AquaNightUTC,means,TaT0["Ta"],TaT0["T0"],trise,tset,tm,daylen,nightlen,ABab["A"],
                 ABab["B"],ABab["a"],ABab["b"],
                 w,t0)
  # name the columns in the data.frame
  cnames=c('lon','lat','method','TerraDay','TerraNight','AquaDay','AquaNight','TerraDayViewtime',
           'TerraNightViewtime','AquaDayViewtime','AquaNightViewtime','means',
           'Ta','T0','trise','tset','tm','daylen','nightlen','A','B','a','b','w','t0')
  colnames(output_DF)=cnames
  return(output_DF)

}


#' Daily means and output params using the Sin-Cos approach with Aqua two obvs.
#'
#' @param x a data.frame containing columns: Lat, Lon, TerraDay, TerraNight,
#'   TerraDayViewtime, TerraNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#'
#' @return a dataframe
#' @export
#'
#' @examples
SinCos_Aqua_DF <- function(x, d, tmaxshift=0.6)
{
  
  SunVari  <- sunRiseSet(x,d, tmaxshift)
  trise    <- SunVari$trise
  tset     <- SunVari$tset
  daylen   <- SunVari$daylen
  nightlen <- SunVari$nightlen
  tm       <- SunVari$tm
  
  TerraDayUTC <- Viewtime2UTC(x$TerraDayViewtime,x$Lon)
  TerraNightUTC <- Viewtime2UTC(x$TerraNightViewtime,x$Lon)
  AquaDayUTC <- Viewtime2UTC(x$AquaDayViewtime,x$Lon)
  AquaNightUTC <- Viewtime2UTC(x$AquaNightViewtime,x$Lon)
  #TerraDay2    <- LST2Celsius(x$TerraDay)
  #TerraNight2  <- LST2Celsius(x$TerraNight)
  TerraDay2    <- -999
  TerraNight2  <- -999
  AquaDay2    <- LST2Celsius(x$AquaDay)
  AquaNight2  <- LST2Celsius(x$AquaNight)

  matrix2solv <- cbind(x["AquaDay"],x["AquaNight"], AquaDayUTC, AquaNightUTC,
                       trise, tm, daylen, nightlen)
  TaT0 <- as.data.frame( t(apply(matrix2solv,1, SinCos2insSolv, DayDataCol="AquaDay",
                                 NightDataCol="AquaNight", DayViewtimeCol="AquaDayUTC",
                                 NightViewtimeCol = "AquaNightUTC",
                                 TriseCol="trise", TmaxCol="tm", DayLengthCol="daylen",
                                 NightLengthCol="nightlen")) )
  names(TaT0) <- c("Ta","T0")

  means <- SinCos_Aqua(x,d)
  method <- "SinCos_Aqua"  

  ABab <- data.frame(0)
  ABab <- ABab[-1,]
  ABab <- cbind(-999,-999,-999,-999)
  names(ABab) <- c("A","B","a","b")
  w <- -999
  t0 <- -999

  output_DF <- cbind(x$Lon,x$Lat,method,TerraDay2,TerraNight2,AquaDay2,AquaNight2,TerraDayUTC,TerraNightUTC,
                 AquaDayUTC,AquaNightUTC,means,TaT0["Ta"],TaT0["T0"],trise,tset,tm,daylen,nightlen,ABab["A"],ABab["B"],
                 ABab["a"],ABab["b"],
                 w,t0)
  # name the columns in the data.frame
  cnames=c('lon','lat','method','TerraDay','TerraNight','AquaDay','AquaNight','TerraDayViewtime',
           'TerraNightViewtime','AquaDayViewtime','AquaNightViewtime','means',
           'Ta','T0','trise','tset','tm','daylen','nightlen','A','B','a','b','w','t0')
  colnames(output_DF)=cnames
  return(output_DF)

}


#' Daily means and output params using the Sin-Cos approach with two obvs, eg. TerraDay and AquaNight.
#'
#' @param x a data.frame containing columns: Lat, Lon, TerraDay, TerraNight,
#'   TerraDayViewtime, TerraNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#'
#' @return a dataframe
#' @export
#'
#' @examples
SinCos_TerraDayAquaNight_DF <- function(x, d, tmaxshift=0.6)
{
  
  SunVari  <- sunRiseSet(x,d, tmaxshift)
  trise    <- SunVari$trise
  tset     <- SunVari$tset
  daylen   <- SunVari$daylen
  nightlen <- SunVari$nightlen
  tm       <- SunVari$tm
  
  TerraDayUTC <- Viewtime2UTC(x$TerraDayViewtime,x$Lon)
  TerraNightUTC <- Viewtime2UTC(x$TerraNightViewtime,x$Lon)
  AquaDayUTC <- Viewtime2UTC(x$AquaDayViewtime,x$Lon)
  AquaNightUTC <- Viewtime2UTC(x$AquaNightViewtime,x$Lon)
  TerraDay2    <- LST2Celsius(x$TerraDay)
  #TerraNight2    <- LST2Celsius(x$TerraNight)
  #AquaDay2    <- LST2Celsius(x$AquaDay)
  TerraNight2    <- -999
  AquaDay2    <- -999
  AquaNight2  <- LST2Celsius(x$AquaNight)

  matrix2solv <- cbind(x["TerraDay"],x["AquaNight"], TerraDayUTC, AquaNightUTC, trise, tm, daylen, nightlen)
  TaT0 <- as.data.frame( t(apply(matrix2solv,1, SinCos2insSolv, DayDataCol="TerraDay",
                                 NightDataCol="AquaNight", DayViewtimeCol="TerraDayUTC",
                                 NightViewtimeCol = "AquaNightUTC",
                                 TriseCol="trise", TmaxCol="tm", 
                                 DayLengthCol="daylen", NightLengthCol="nightlen")) )
  names(TaT0) <- c("Ta","T0")

  means <- SinCos_TerraDayAquaNight(x,d)
  method <- "SinCos_TerraDayAquaNight"  

  ABab <- data.frame(0)
  ABab <- ABab[-1,]
  ABab <- cbind(-999,-999,-999,-999)
  names(ABab) <- c("A","B","a","b")
  w <- -999
  t0 <- -999

  output_DF <- cbind(x$Lon,x$Lat,method,TerraDay2,TerraNight2,AquaDay2,AquaNight2,TerraDayUTC,TerraNightUTC,
                 AquaDayUTC,AquaNightUTC,means,TaT0["Ta"],TaT0["T0"],trise,tset,tm,daylen,nightlen,ABab["A"],ABab["B"],
                 ABab["a"],ABab["b"],
                 w,t0)
  # name the columns in the data.frame
  cnames=c('lon','lat','method','TerraDay','TerraNight','AquaDay','AquaNight','TerraDayViewtime',
           'TerraNightViewtime','AquaDayViewtime','AquaNightViewtime','means',
           'Ta','T0','trise','tset','tm','daylen','nightlen','A','B','a','b','w','t0')
  colnames(output_DF)=cnames
  return(output_DF)

}


#' Daily means and output params using the Sin-Cos approach with two obvs, eg. AquaDay and TerraNight.
#'
#' @param x a data.frame containing columns: Lat, Lon, TerraDay, TerraNight,
#'   TerraDayViewtime, TerraNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#'
#' @return a dataframe
#' @export
#'
#' @examples
SinCos_AquaDayTerraNight_DF <- function(x, d, tmaxshift=0.6)
{
  
  SunVari  <- sunRiseSet(x,d, tmaxshift)
  trise    <- SunVari$trise
  tset     <- SunVari$tset
  daylen   <- SunVari$daylen
  nightlen <- SunVari$nightlen
  tm       <- SunVari$tm
  
  TerraDayUTC <- Viewtime2UTC(x$TerraDayViewtime,x$Lon)
  TerraNightUTC <- Viewtime2UTC(x$TerraNightViewtime,x$Lon)
  AquaDayUTC <- Viewtime2UTC(x$AquaDayViewtime,x$Lon)
  AquaNightUTC <- Viewtime2UTC(x$AquaNightViewtime,x$Lon)
  #TerraDay2    <- LST2Celsius(x$TerraDay)
  TerraDay2    <- -999
  TerraNight2    <- LST2Celsius(x$TerraNight)
  AquaDay2    <- LST2Celsius(x$AquaDay)
  #AquaNight2  <- LST2Celsius(x$AquaNight)
  AquaNight2  <- -999

  matrix2solv <- cbind(x["AquaDay"],x["TerraNight"], AquaDayUTC, TerraNightUTC, trise, tm, 
                       daylen, nightlen)
  TaT0 <- as.data.frame( t(apply(matrix2solv,1, SinCos2insSolv, DayDataCol="AquaDay",
                                 NightDataCol="TerraNight", DayViewtimeCol="AquaDayUTC",
                                 NightViewtimeCol = "TerraNightUTC",
                                 TriseCol="trise", TmaxCol="tm", 
                                 DayLengthCol="daylen", NightLengthCol="nightlen")) )
  names(TaT0) <- c("Ta","T0")

  means <- SinCos_AquaDayTerraNight(x,d)
  method <- "SinCos_AquaDayTerraNight"  

  ABab <- data.frame(0)
  ABab <- ABab[-1,]
  ABab <- cbind(-999,-999,-999,-999)
  names(ABab) <- c("A","B","a","b")
  w <- -999
  t0 <- -999

  output_DF <- cbind(x$Lon,x$Lat,method,TerraDay2,TerraNight2,AquaDay2,AquaNight2,TerraDayUTC,TerraNightUTC,
                 AquaDayUTC,AquaNightUTC,means,TaT0["Ta"],TaT0["T0"],trise,tset,tm,daylen,nightlen,ABab["A"],ABab["B"],
                 ABab["a"],ABab["b"],
                 w,t0)
  # name the columns in the data.frame
  cnames=c('lon','lat','method','TerraDay','TerraNight','AquaDay','AquaNight','TerraDayViewtime',
           'TerraNightViewtime','AquaDayViewtime','AquaNightViewtime','means',
           'Ta','T0','trise','tset','tm','daylen','nightlen','A','B','a','b','w','t0')
  colnames(output_DF)=cnames
  return(output_DF)

}

#' Daily means and output params using the Sin-Cos approach with three obvs: two Day LST (Terra Day and Aqua Day) and Terra Night.
#'
#' @param x a data.frame containing columns: Lat, Lon, TerraDay, TerraNight,
#'   TerraDayViewtime, TerraNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#'
#' @return a dataframe
#' @export
#'
#' @examples
SinCos_TerraDayAquaDayTerraNight_DF <- function(x, d, tmaxshift=0.6)
{
  DayCols = c("TerraDay", "AquaDay")
  NightCols = "TerraNight"
  DayViewtimeCols = c("TerraDayViewtime", "AquaDayViewtime")
  NightViewtimeCols = "TerraNightViewtime"

  SunVari  <- sunRiseSet(x,d, tmaxshift)
  trise    <- SunVari$trise
  tset     <- SunVari$tset
  daylen   <- SunVari$daylen
  nightlen <- SunVari$nightlen
  tm       <- SunVari$tm
   
  DayUTC <- Viewtime2UTC(x[DayViewtimeCols],x[["Lon"]])
  NightUTC <- Viewtime2UTC(x[NightViewtimeCols],x[["Lon"]])
  names(DayUTC) <- DayViewtimeCols
  names(NightUTC) <- NightViewtimeCols

  TerraDayUTC <- Viewtime2UTC(x$TerraDayViewtime,x$Lon)
  TerraNightUTC <- Viewtime2UTC(x$TerraNightViewtime,x$Lon)
  AquaDayUTC <- Viewtime2UTC(x$AquaDayViewtime,x$Lon)
  AquaNightUTC <- Viewtime2UTC(x$AquaNightViewtime,x$Lon)
  TerraDay2    <- LST2Celsius(x$TerraDay)
  TerraNight2    <- LST2Celsius(x$TerraNight)
  AquaDay2    <- LST2Celsius(x$AquaDay)
  #AquaNight2  <- LST2Celsius(x$AquaNight)
  AquaNight2  <- -999

  matrix2solv <- cbind(x[DayCols], x[NightCols],DayUTC,NightUTC,
                       trise, tm, daylen, nightlen)
  TaT0 <- as.data.frame( t(apply(matrix2solv,1, SinCosMinsSolv, DayCols, NightCols,
                                 DayViewtimeCols, NightViewtimeCols,
                                 TriseCol="trise", TmaxCol="tm",
                                 DayLengthCol="daylen", NightLengthCol="nightlen")) )
  names(TaT0) <- c("Ta","T0")

  means <- SinCos_TerraDayAquaDayTerraNight(x,d)
  method <- "SinCos_TerraDayAquaDayTerraNight"  

  ABab <- data.frame(0)
  ABab <- ABab[-1,]
  ABab <- cbind(-999,-999,-999,-999)
  names(ABab) <- c("A","B","a","b")
  w <- -999
  t0 <- -999

  output_DF <- cbind(x$Lon,x$Lat,method,TerraDay2,TerraNight2,AquaDay2,AquaNight2,TerraDayUTC,TerraNightUTC,
                 AquaDayUTC,AquaNightUTC,means,TaT0["Ta"],TaT0["T0"],trise,tset,tm,daylen,nightlen,ABab["A"],ABab["B"],
                 ABab["a"],ABab["b"],
                 w,t0)
  # name the columns in the data.frame
  cnames=c('lon','lat','method','TerraDay','TerraNight','AquaDay','AquaNight','TerraDayViewtime',
           'TerraNightViewtime','AquaDayViewtime','AquaNightViewtime','means',
           'Ta','T0','trise','tset','tm','daylen','nightlen','A','B','a','b','w','t0')
  colnames(output_DF)=cnames
  return(output_DF)

}

#' Daily means and output params using the Sin-Cos approach with three obs: two Day LST (Terra Day and Aqua Day) and Aqua Night.
#'
#' @param x a data.frame containing columns: Lat, Lon, TerraDay, TerraNight,
#'   TerraDayViewtime, TerraNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#'
#' @return a dataframe
#' @export
#'
#' @examples
SinCos_TerraDayAquaDayAquaNight_DF <- function(x, d, tmaxshift=0.6)
{
  DayCols = c("TerraDay", "AquaDay")
  NightCols = "AquaNight"
  DayViewtimeCols = c("TerraDayViewtime", "AquaDayViewtime")
  NightViewtimeCols = "AquaNightViewtime"
  
  SunVari  <- sunRiseSet(x,d, tmaxshift)
  trise    <- SunVari$trise
  tset     <- SunVari$tset
  daylen   <- SunVari$daylen
  nightlen <- SunVari$nightlen
  tm       <- SunVari$tm
 
  DayUTC <- Viewtime2UTC(x[DayViewtimeCols],x[["Lon"]])
  NightUTC <- Viewtime2UTC(x[NightViewtimeCols],x[["Lon"]])
  names(DayUTC) <- DayViewtimeCols
  names(NightUTC) <- NightViewtimeCols

  TerraDayUTC <- Viewtime2UTC(x$TerraDayViewtime,x$Lon)
  TerraNightUTC <- Viewtime2UTC(x$TerraNightViewtime,x$Lon)
  AquaDayUTC <- Viewtime2UTC(x$AquaDayViewtime,x$Lon)
  AquaNightUTC <- Viewtime2UTC(x$AquaNightViewtime,x$Lon)
  TerraDay2    <- LST2Celsius(x$TerraDay)
  #TerraNight2    <- LST2Celsius(x$TerraNight)
  TerraNight2    <- -999
  AquaDay2    <- LST2Celsius(x$AquaDay)
  AquaNight2  <- LST2Celsius(x$AquaNight)

  matrix2solv <- cbind(x[DayCols], x[NightCols],DayUTC,NightUTC,
                       trise, tm, daylen, nightlen)
  TaT0 <- as.data.frame( t(apply(matrix2solv,1, SinCosMinsSolv, DayCols, NightCols,
                                 DayViewtimeCols, NightViewtimeCols,
                                 TriseCol="trise", TmaxCol="tm",
                                 DayLengthCol="daylen", NightLengthCol="nightlen")) )
  names(TaT0) <- c("Ta","T0")

  means <- SinCos_TerraDayAquaDayAquaNight(x,d)
  method <- "SinCos_TerraDayAquaDayAquaNight"  

  ABab <- data.frame(0)
  ABab <- ABab[-1,]
  ABab <- cbind(-999,-999,-999,-999)
  names(ABab) <- c("A","B","a","b")
  w <- -999
  t0 <- -999
  output_DF <- cbind(x$Lon,x$Lat,method,TerraDay2,TerraNight2,AquaDay2,AquaNight2,TerraDayUTC,TerraNightUTC,
                 AquaDayUTC,AquaNightUTC,means,TaT0["Ta"],TaT0["T0"],trise,tset,tm,daylen,nightlen,ABab["A"],ABab["B"],
                 ABab["a"],ABab["b"],
                 w,t0)
  # name the columns in the data.frame
  cnames=c('lon','lat','method','TerraDay','TerraNight','AquaDay','AquaNight','TerraDayViewtime',
           'TerraNightViewtime','AquaDayViewtime','AquaNightViewtime','means',
           'Ta','T0','trise','tset','tm','daylen','nightlen','A','B','a','b','w','t0')
  colnames(output_DF)=cnames
  return(output_DF)

}

#' Daily means and output params using the Sin-Cos approach with three obs: TerraDay, TerraNight and AquaNight.
#'
#' @param x a data.frame containing columns: Lat, Lon, TerraDay, TerraNight,
#'   TerraDayViewtime, TerraNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#'
#' @return a dataframe
#' @export
#'
#' @examples
SinCos_TerraDayTerraNightAquaNight_DF <- function(x, d, tmaxshift=0.6)
{
  DayCols = "TerraDay"
  NightCols = c("TerraNight", "AquaNight")
  DayViewtimeCols = "TerraDayViewtime"
  NightViewtimeCols = c("TerraNightViewtime", "AquaNightViewtime")
  
  SunVari  <- sunRiseSet(x,d, tmaxshift)
  trise    <- SunVari$trise
  tset     <- SunVari$tset
  daylen   <- SunVari$daylen
  nightlen <- SunVari$nightlen
  tm       <- SunVari$tm

  DayUTC <- Viewtime2UTC(x[DayViewtimeCols],x[["Lon"]])
  NightUTC <- Viewtime2UTC(x[NightViewtimeCols],x[["Lon"]])
  names(DayUTC) <- DayViewtimeCols
  names(NightUTC) <- NightViewtimeCols
  
  TerraDayUTC <- Viewtime2UTC(x$TerraDayViewtime,x$Lon)
  TerraNightUTC <- Viewtime2UTC(x$TerraNightViewtime,x$Lon)
  AquaDayUTC <- Viewtime2UTC(x$AquaDayViewtime,x$Lon)
  AquaNightUTC <- Viewtime2UTC(x$AquaNightViewtime,x$Lon)
  TerraDay2    <- LST2Celsius(x$TerraDay)
  TerraNight2    <- LST2Celsius(x$TerraNight)
  #AquaDay2    <- LST2Celsius(x$AquaDay)
  AquaDay2    <- -999
  AquaNight2  <- LST2Celsius(x$AquaNight)
  
  matrix2solv <- cbind(x[DayCols], x[NightCols],DayUTC,NightUTC,
                       trise, tm, daylen, nightlen)
  TaT0 <- as.data.frame( t(apply(matrix2solv,1, SinCosMinsSolv, DayCols, NightCols,
                                 DayViewtimeCols, NightViewtimeCols,
                                 TriseCol="trise", TmaxCol="tm",
                                 DayLengthCol="daylen", NightLengthCol="nightlen")) )
  names(TaT0) <- c("Ta","T0")

  means <- SinCos_TerraDayTerraNightAquaNight(x,d)
  method <- "SinCos_TerraDayTerraNightAquaNight"  

  ABab <- data.frame(0)
  ABab <- ABab[-1,]
  ABab <- cbind(-999,-999,-999,-999)
  names(ABab) <- c("A","B","a","b")
  w <- -999
  t0 <- -999

  output_DF <- cbind(x$Lon,x$Lat,method,TerraDay2,TerraNight2,AquaDay2,AquaNight2,TerraDayUTC,TerraNightUTC,
                 AquaDayUTC,AquaNightUTC,means,TaT0["Ta"],TaT0["T0"],trise,tset,tm,daylen,nightlen,ABab["A"],ABab["B"],
                 ABab["a"],ABab["b"],
                 w,t0)
  # name the columns in the data.frame
  cnames=c('lon','lat','method','TerraDay','TerraNight','AquaDay','AquaNight','TerraDayViewtime',
           'TerraNightViewtime','AquaDayViewtime','AquaNightViewtime','means',
           'Ta','T0','trise','tset','tm','daylen','nightlen','A','B','a','b','w','t0')
  colnames(output_DF)=cnames
  return(output_DF)

}


#' Daily means and output params using the Sin-Cos approach with three obs: AquaDay, TerraNight and AquaNight.
#'
#' @param x a data.frame containing columns: Lat, Lon, TerraDay, TerraNight,
#'   TerraDayViewtime, TerraNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#'
#' @return a dataframe
#' @export
#'
#' @examples
SinCos_AquaDayTerraNightAquaNight_DF <- function(x, d, tmaxshift=0.6)
{
  DayCols = "AquaDay"
  NightCols = c("TerraNight", "AquaNight")
  DayViewtimeCols = "AquaDayViewtime"
  NightViewtimeCols = c("TerraNightViewtime", "AquaNightViewtime")
  
  SunVari  <- sunRiseSet(x,d, tmaxshift)
  trise    <- SunVari$trise
  tset     <- SunVari$tset
  daylen   <- SunVari$daylen
  nightlen <- SunVari$nightlen
  tm       <- SunVari$tm

  DayUTC <- Viewtime2UTC(x[DayViewtimeCols],x[["Lon"]])
  NightUTC <- Viewtime2UTC(x[NightViewtimeCols],x[["Lon"]])
  names(DayUTC) <- DayViewtimeCols
  names(NightUTC) <- NightViewtimeCols
  
  TerraDayUTC <- Viewtime2UTC(x$TerraDayViewtime,x$Lon)
  TerraNightUTC <- Viewtime2UTC(x$TerraNightViewtime,x$Lon)
  AquaDayUTC <- Viewtime2UTC(x$AquaDayViewtime,x$Lon)
  AquaNightUTC <- Viewtime2UTC(x$AquaNightViewtime,x$Lon)
  #TerraDay2    <- LST2Celsius(x$TerraDay)
  TerraDay2    <- -999
  TerraNight2    <- LST2Celsius(x$TerraNight)
  AquaDay2    <- LST2Celsius(x$AquaDay)
  AquaNight2  <- LST2Celsius(x$AquaNight)

  matrix2solv <- cbind(x[DayCols], x[NightCols],DayUTC,NightUTC,
                       trise, tm, daylen, nightlen)
  TaT0 <- as.data.frame( t(apply(matrix2solv,1, SinCosMinsSolv, DayCols, NightCols,
                                 DayViewtimeCols, NightViewtimeCols,
                                 TriseCol="trise", TmaxCol="tm",
                                 DayLengthCol="daylen", NightLengthCol="nightlen")) )
  names(TaT0) <- c("Ta","T0")

  means <- SinCos_AquaDayTerraNightAquaNight(x,d)
  method <- "SinCos_AquaDayTerraNightAquaNight"  

  ABab <- data.frame(0)
  ABab <- ABab[-1,]
  ABab <- cbind(-999,-999,-999,-999)
  names(ABab) <- c("A","B","a","b")
  w <- -999
  t0 <- -999

  output_DF <- cbind(x$Lon,x$Lat,method,TerraDay2,TerraNight2,AquaDay2,AquaNight2,TerraDayUTC,TerraNightUTC,
                 AquaDayUTC,AquaNightUTC,means,TaT0["Ta"],TaT0["T0"],trise,tset,tm,daylen,nightlen,ABab["A"],ABab["B"],
                 ABab["a"],ABab["b"],
                 w,t0)
  # name the columns in the data.frame
  cnames=c('lon','lat','method','TerraDay','TerraNight','AquaDay','AquaNight','TerraDayViewtime',
           'TerraNightViewtime','AquaDayViewtime','AquaNightViewtime','means',
           'Ta','T0','trise','tset','tm','daylen','nightlen','A','B','a','b','w','t0')
  colnames(output_DF)=cnames
  return(output_DF)

}



#' Daily means and output params using the Sin-Linear approach with four obs: TerraDay TerraNight AquaDay and AquaNight.
#'
#' @param x a data.frame containing columns: Lat, Lon, TerraDay, TerraNight,
#'   TerraDayViewtime, TerraNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#'
#' @return a dataframe
#' @export
#'
#' @examples
sinlinear_DF <- function(x, d, tmaxshift=0.6)
{
  SunVari  <- sunRiseSet(x,d, tmaxshift)
  trise    <- SunVari$trise
  tset     <- SunVari$tset
  daylen   <- SunVari$daylen
  nightlen <- SunVari$nightlen
  tm       <- SunVari$tm
  w        <- pi/(tm-trise)
  t0       <- (trise+tm)/2

  TerraDayUTC <- Viewtime2UTC(x$TerraDayViewtime,x$Lon)
  TerraNightUTC <- Viewtime2UTC(x$TerraNightViewtime,x$Lon)
  AquaDayUTC <- Viewtime2UTC(x$AquaDayViewtime,x$Lon)
  AquaNightUTC <- Viewtime2UTC(x$AquaNightViewtime,x$Lon)
  TerraDay2    <- LST2Celsius(x$TerraDay)
  TerraNight2    <- LST2Celsius(x$TerraNight)
  AquaDay2    <- LST2Celsius(x$AquaDay)
  AquaNight2  <- LST2Celsius(x$AquaNight)

  DayDataCols <- c("TerraDay","AquaDay")
  DayViewtimeCols <- c("TerraDayUTC","AquaDayUTC")
  NightDataCols <- c("TerraNight","AquaNight")
  NightViewtimeCols <- c("TerraNightUTC","AquaNightUTC")

  matrix2solv <- cbind(x[DayDataCols], x[NightDataCols],
                       TerraDayUTC,TerraNightUTC, AquaDayUTC, AquaNightUTC,
                       trise, tset, w,t0)
  ABab <- as.data.frame( t(apply(matrix2solv,1,SinLinear2Solv,DayDataCols=DayDataCols,NightDataCols=NightDataCols,
                                 DayViewtimeCols= DayViewtimeCols,NightViewtimeCols=NightViewtimeCols,
                TriseCol="trise", TsetCol="tset",wCol="w",t0Col="t0")))
  names(ABab) <- c("A","B","a","b")

  method <- "SinLinear"
  means <- SinLinear(x,d)

  TaT0 <- data.frame(0)
  TaT0 <- TaT0[-1,]
  TaT0 <- cbind(-999,-999)
  names(TaT0) <- c("Ta","T0")
  
  output_DF <- cbind(x$Lon,x$Lat,method,TerraDay2,TerraNight2,AquaDay2,AquaNight2,TerraDayUTC,TerraNightUTC,
                 AquaDayUTC,AquaNightUTC,means,TaT0["Ta"],TaT0["T0"],trise,tset,tm,daylen,nightlen,ABab["A"],ABab["B"],
                 ABab["a"],ABab["b"],w,t0)
  # name the columns in the data.frame
  cnames=c('lon','lat','method','TerraDay','TerraNight','AquaDay','AquaNight','TerraDayViewtime',
           'TerraNightViewtime','AquaDayViewtime','AquaNightViewtime','means',
           'Ta','T0','trise','tset','tm','daylen','nightlen','A','B','a','b','w','t0')
  colnames(output_DF) <- cnames
  rownames(output_DF) <- rownames(x)
  return(output_DF)

}