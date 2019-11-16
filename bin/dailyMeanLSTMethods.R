
source("helperfunc.R")

#' Using Min/Max approach to calcuate daily mean LST
#' 
#' @param x a data.frame containing AquaDay and AquaNight 
#' @return a vector
MaxMin <- function(x){
  if(!all(c("AquaDay", "AquaNight") %in% names(x))) 
    stop("x does not contain required column(s): AquaDay or AquaNight. ")
  means <- (LST2Celsius(x[["AquaDay"]])+LST2Celsius(x[["AquaNight"]])) /2
  return(means)
}


#' Solve Ta (amplitude)  and T0 (mean) by fitting a Sin-Cos equation using two inputs
#' 
#' @param m the input matrix/data.frame containing names specified by the rest params
#' @param DayDataCol the columns names for Day obs
#' @param NightDataCol the columan name for Night obs
#' @param DayViewtimeCol col name for the view time of Day obs
#' @param NightViewtimeCol col name for view time of Night obs
#' @param TriseCol columan name for trise
#' @param TmaxCol col name for tmax
#' @param DayLengthCol col name for daylength
#' @param NightLength col name for nightlength
#' @return a vector, params Ta (amplitude) and T0 (mean)
SinCos2insSolv <- function(m, DayDataCol, NightDataCol, DayViewtimeCol, NightViewtimeCol, 
                           TriseCol, TmaxCol, DayLengthCol, NightLengthCol) {
  a <- matrix(c(sin(pi+2*pi*(m[NightViewtimeCol]-m[DayLengthCol])/m[NightLengthCol]/4),1,
                cos(2*pi*(m[DayViewtimeCol]-m[TmaxCol])/(m[TmaxCol]-m[TriseCol])/2),1),
              nrow=2,byrow=TRUE)
  b <- matrix(c(LST2Celsius(m[NightDataCol]),LST2Celsius(m[DayDataCol])),nrow=2)
  return(solve(a,b))
}


#' Fitting Ta and T0 using multiple inputs from Terra and Aqua to a Sin-Cos approach 
#' @param m, the input named data.frame or matrix
#' @param DayDataCol the columns specifying one or two day obvs
#' @param NightDataCol the columns specifying one or two day obvs
#' @param DayViewtimeCol the columns specifying one or two viewtime corresponding to day obvs
#' @param NightViewtimeCol the cols specifying one or two viewtime corresponding to night obvs
#' @param TriseCol columan name for trise
#' @param TmaxCol col name for tmax
#' @param DayLengthCol col name for daylength
#' @param NightLength col name for nightlength
#' @return a vector, params Ta (amplitude) and T0 (mean)
#' @note If only two obs input, it will call SinCos2insSolv() instead
SinCosMinsSolv <- function(m, DayDataCol, NightDataCol, DayViewtimeCol, NightViewtimeCol,
                           TriseCol, TmaxCol, DayLengthCol, NightLengthCol) {
  # in case two inputs
  if (length(DayDataCol) + length(NightDataCol) < 3) 
#    stop("At least 3 obs inputs.")
    return(SinCos2insSolv(m,DayDataCol, NightDataCol, DayViewtimeCol, NightViewtimeCol,
                   TriseCol, TmaxCol, DayLengthCol, NightLengthCol))
  
  
  a <- c(sin(pi+2*pi*(m[NightViewtimeCol]-m[[DayLengthCol]])/m[[NightLengthCol]]/4),
             #   cos(2*pi*(m[DayViewtimeCol[1]]-m[TmaxCol])/(m[TmaxCol]-m[TriseCol])/2),
                cos(2*pi*(m[DayViewtimeCol]-m[[TmaxCol]])/(m[[TmaxCol]]-m[[TriseCol]])/2))
  
  b <- c(LST2Celsius(m[NightDataCol]),LST2Celsius(m[DayDataCol]))
  X <- lsfit(a,b) # solve b = a*X +e
  T0 <- X$coef[1]
  Ta <- X$coef[2]
  return(c(Ta,T0))
}


#' Calc temperatures at given times by applying Sin-Cos method with specified parameters 
#' 
#' @param t a vector contains time points
#' @param Ta amplitude
#' @param T0 mean
#' @param trise the time of sun rise
#' @param tset the time of sun set
#' @param tm the time of highest temperature
#' @param daylen day length
#' @param nightlen night length
#' @return a data frame
#' @note the length of parameters in sin-cos method should be equal.
TempBySinCosMethod <- function(t, Ta, T0, trise, tset, tm,  daylen, nightlen)
{
  # for each t and each row of other parameters
  atemp <- function(m, t1)
  {
    if (t1>=m["trise"] && t1 <m["tset"]) # for day
      lst <- cos(2*pi*(t1-m["tm"])/(m["tm"]-m["trise"])/2)*m["Ta"] + m["T0"]
    else if(t1 >= m["tset"]) # for night
      lst <- sin(pi+2*pi*(t1-m["daylen"])/m["nightlen"]/4)*m["Ta"] + m["T0"]
    else 
      lst <- cos(2*pi*(m["trise"]-m["tm"])/(m["tm"]-m["trise"])/2)*m["Ta"] + m["T0"]
    return(lst)
  }
  
  m <- cbind(Ta, T0, trise, tset, tm, daylen, nightlen)
  
  mlst <- NULL
  for (tt in t)
  {
    lst <- apply(m, 1, atemp, t1=tt)
    mlst <- cbind(mlst, lst)
  }
  
  colnames(mlst) <- t
  
  return(mlst)
}

#' Daily means using the Sin-Cos approach with Terra two obvs.
#' 
#' @param x a data.frame containing columns: Lat, Lon, TerraDay, TerraNight,
#'   TerraDayViewtime, TerraNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#' @return a vector
SinCos_Terra <- function(x, d, tmaxshift = 0.6){

  SunVari  <- sunRiseSet(x,d, tmaxshift)
  trise    <- SunVari$trise
  tset     <- SunVari$tset
  daylen   <- SunVari$daylen
  nightlen <- SunVari$nightlen
  tm       <- SunVari$tm
  
  TerraDayUTC <- Viewtime2UTC(x$TerraDayViewtime,x$Lon)
  TerraNightUTC <- Viewtime2UTC(x$TerraNightViewtime,x$Lon)
  
  #matrix2solv <- cbind(x["TerraDay"],x["TerraNight"], TerraDayUTC, TerraNightUTC, trise, tm, tset, daylen, nightlen)
  matrix2solv <- cbind(x["TerraDay"],x["TerraNight"], TerraDayUTC, TerraNightUTC, trise, tm, daylen, nightlen)
  
  TaT0 <- as.data.frame( t(apply(matrix2solv,1, SinCos2insSolv, DayDataCol="TerraDay", 
                               NightDataCol="TerraNight", DayViewtimeCol="TerraDayUTC", 
                               NightViewtimeCol = "TerraNightUTC", 
                               TriseCol="trise", TmaxCol="tm", DayLengthCol="daylen", NightLengthCol="nightlen")) )
  names(TaT0) <- c("Ta","T0")
  
  paralist <- cbind(matrix2solv,TaT0)    
  temps <- TempBySinCosMethod(0:23, TaT0["Ta"], TaT0["T0"], trise, tset, tm, daylen, nightlen)

  means <- rowMeans(temps)
  return(means)
}
  

#' Daily means using the Sin-Cos approach with Aqua two obvs.
#'
#' @param x a data.frame containing columns: Lat, Lon, AquaDay, AquaNight,
#'   AquaDayViewtime, AquaNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#' @return a vector
SinCos_Aqua <-function(x, d, tmaxshift = 0.6)
{

  SunVari  <- sunRiseSet(x,d, tmaxshift)
  trise    <- SunVari$trise
  tset     <- SunVari$tset
  daylen   <- SunVari$daylen
  nightlen <- SunVari$nightlen
  tm       <- SunVari$tm

  AquaDayUTC <- Viewtime2UTC(x$AquaDayViewtime,x$Lon)
  AquaNightUTC <- Viewtime2UTC(x$AquaNightViewtime,x$Lon)

  matrix2solv <- cbind(x["AquaDay"],x["AquaNight"], AquaDayUTC, AquaNightUTC,
                       trise, tm, daylen, nightlen)
  TaT0 <- as.data.frame( t(apply(matrix2solv,1, SinCos2insSolv, DayDataCol="AquaDay",
                                 NightDataCol="AquaNight", DayViewtimeCol="AquaDayUTC",
                                 NightViewtimeCol = "AquaNightUTC",
                                 TriseCol="trise", TmaxCol="tm", DayLengthCol="daylen",
                                 NightLengthCol="nightlen")) )
  names(TaT0) <- c("Ta","T0")
  temps <- TempBySinCosMethod(0:23, TaT0["Ta"], TaT0["T0"], trise, tset, tm, daylen, nightlen)

  means <- rowMeans(temps)
  return(means)
}


#' Daily means using the Sin-Cos approach with two obvs, eg. TerraDay and AquaNight.
#'
#' @param x a data.frame containing columns: Lat, Lon, TerraDay, AquaNight,
#'   TerraDayViewtime, AquaNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#' @return a vector
SinCos_TerraDayAquaNight <- function(x, d, tmaxshift = 0.6)
{

  SunVari  <- sunRiseSet(x,d, tmaxshift)
  trise    <- SunVari$trise
  tset     <- SunVari$tset
  daylen   <- SunVari$daylen
  nightlen <- SunVari$nightlen
  tm       <- SunVari$tm

  TerraDayUTC <- Viewtime2UTC(x$TerraDayViewtime,x$Lon)
  AquaNightUTC <- Viewtime2UTC(x$AquaNightViewtime,x$Lon)


  matrix2solv <- cbind(x["TerraDay"],x["AquaNight"], TerraDayUTC, AquaNightUTC, trise, tm, daylen, nightlen)
  TaT0 <- as.data.frame( t(apply(matrix2solv,1, SinCos2insSolv, DayDataCol="TerraDay",
                                 NightDataCol="AquaNight", DayViewtimeCol="TerraDayUTC",
                                 NightViewtimeCol = "AquaNightUTC",
                                 TriseCol="trise", TmaxCol="tm", 
                                 DayLengthCol="daylen", NightLengthCol="nightlen")) )
  names(TaT0) <- c("Ta","T0")

  temps <- TempBySinCosMethod(0:23, TaT0["Ta"], TaT0["T0"], trise, tset, tm, daylen, nightlen)

  means <- rowMeans(temps)
  return(means)
}


#' Daily means using the Sin-Cos approach with two obvs, eg. AquaDay and TerraNight.
#'
#' @param x a data.frame containing columns: Lat, Lon, AquaDay, TerraNight,
#'   AquaDayViewtime, TerraNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#' @return a vector
SinCos_AquaDayTerraNight <- function(x, d, tmaxshift = 0.6)
{
  SunVari  <- sunRiseSet(x,d, tmaxshift)
  trise    <- SunVari$trise
  tset     <- SunVari$tset
  daylen   <- SunVari$daylen
  nightlen <- SunVari$nightlen
  tm       <- SunVari$tm

  TerraNightUTC <- Viewtime2UTC(x$TerraNightViewtime,x$Lon)
  AquaDayUTC <- Viewtime2UTC(x$AquaDayViewtime,x$Lon)


  matrix2solv <- cbind(x["AquaDay"],x["TerraNight"], AquaDayUTC, TerraNightUTC, trise, tm, 
                       daylen, nightlen)
  TaT0 <- as.data.frame( t(apply(matrix2solv,1, SinCos2insSolv, DayDataCol="AquaDay",
                                 NightDataCol="TerraNight", DayViewtimeCol="AquaDayUTC",
                                 NightViewtimeCol = "TerraNightUTC",
                                 TriseCol="trise", TmaxCol="tm", 
                                 DayLengthCol="daylen", NightLengthCol="nightlen")) )
  names(TaT0) <- c("Ta","T0")

  temps <- TempBySinCosMethod(0:23, TaT0["Ta"], TaT0["T0"], trise, tset, tm, daylen, nightlen)

  means <- rowMeans(temps)
  return(means)
}

#' Daily means using the Sin-Cos approach with three obvs: two Day LST (Terra Day and Aqua Day) and Terra Night.
#'
#' @param x a data.frame at least containing columns: Lat, Lon, TerraDay, AquaDay, TerraNight,
#'   TerraDayViewtime, AquaDayViewtime, and TerraNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#' @return a vector
SinCos_TerraDayAquaDayTerraNight <-function(x, d, tmaxshift = 0.6)
{
  return(SinCos_MObs(x,d, tmaxshift, DayCols = c("TerraDay", "AquaDay"), NightCols = "TerraNight",
         DayViewtimeCols = c("TerraDayViewtime", "AquaDayViewtime"),
         NightViewtimeCols = "TerraNightViewtime"))
}


#' Daily means using the Sin-Cos approach with three obs: two Day LST (Terra Day and Aqua Day) and Aqua Night.
#'
#' @param x a data.frame at least containing columns: Lat, Lon, TerraDay, AquaDay, AquaNight,
#'   TerraDayViewtime, AquaDayViewtime, AquaNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#' @return a vector
SinCos_TerraDayAquaDayAquaNight <- function(x, d, tmaxshift = 0.6)
{
  return(SinCos_MObs(x,d, tmaxshift, DayCols = c("TerraDay", "AquaDay"), NightCols = "AquaNight",
                     DayViewtimeCols = c("TerraDayViewtime", "AquaDayViewtime"),
                     NightViewtimeCols = "AquaNightViewtime"))
}

 
#' Daily means using the Sin-Cos approach with three obs: TerraDay, TerraNight and AquaNight.
#'
#' @param x a data.frame at least containing columns: Lat, Lon, TerraDay, TerraNight, AquaNight, 
#'   TerraDayViewtime, TerraNightViewtime, and AquaNightViewtime
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#' @return a vector
SinCos_TerraDayTerraNightAquaNight <- function(x, d, tmaxshift = 0.6)
{
  return(SinCos_MObs(x,d, tmaxshift, DayCols = "TerraDay", NightCols = c("TerraNight", "AquaNight"),
                     DayViewtimeCols = "TerraDayViewtime",
                     NightViewtimeCols = c("TerraNightViewtime", "AquaNightViewtime")))
}


#' Daily means using the Sin-Cos approach with three obs: AquaDay, TerraNight and AquaNight.
#'
#' @param x a data.frame at least containing columns: Lat, Lon, AquaDay,TerraNight, AquaNight,
#'   AquaDayViewtime, TerraNightViewtime and AquaNightViewtime 
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#' @return a vector
SinCos_AquaDayTerraNightAquaNight <- function(x, d, tmaxshift = 0.6)
{
  return(SinCos_MObs(x,d, tmaxshift, DayCols = "AquaDay", NightCols = c("TerraNight", "AquaNight"),
                     DayViewtimeCols = "AquaDayViewtime",
         NightViewtimeCols = c("TerraNightViewtime", "AquaNightViewtime")))
}

#' 
#' Daily means using the Sin-Cos approach with multiple obvs, some of TerraDay, TerraNight, AquaDay, and AquaNight.  
#'
#' @param x a data.frame containing columns: Lat, Lon, Day and night obvs and corresponding view time.
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift, the maximum temperature time minus the solar noon
#' @param DayCols cols holding day obs
#' @param NightCols cols holding night obs
#' @param DayViewtimeCols cols holding corresponding day viewtime
#' @param NightViewtimeCols cols holding corresponding night viewtime
#' @return a vector
#' @note this function can use for computing using any combination of day and night lsts. 
#' The default para settings produce same results as SinCos_4Obs
SinCos_MObs <- function(x, d, tmaxshift = 0.6, DayCols = c("TerraDay","AquaDay"), 
                        NightCols = c("TerraNight", "AquaNight"),
                        DayViewtimeCols = c("TerraDayViewtime","AquaDayViewtime"),
                        NightViewtimeCols = c("TerraNightViewtime", "AquaNightViewtime"))
{
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

  matrix2solv <- cbind(x[DayCols], x[NightCols],DayUTC,NightUTC,
                       trise, tm, daylen, nightlen)
  TaT0 <- as.data.frame( t(apply(matrix2solv,1, SinCosMinsSolv, DayCols, NightCols,
                                 DayViewtimeCols, NightViewtimeCols,
                                 TriseCol="trise", TmaxCol="tm",
                                 DayLengthCol="daylen", NightLengthCol="nightlen")) )
  names(TaT0) <- c("Ta","T0")

  temps <- TempBySinCosMethod(0:23, TaT0["Ta"], TaT0["T0"], trise, tset, tm, daylen, nightlen)

  means <- rowMeans(temps)
  return(means)
}
#' 
#' Daily means using the Sin-Cos approach with four obvs, eg. TerraDay, TerraNight, AquaDay, and AquaNight 
#'
#' @param x a data.frame containing columns: Lat, Lon, TerraDay, TerraNight, AquaDay, AquaNight,
#'   TerraDayViewtime, TerraNightViewtime, AquaDayViewtime, AquaNightViewtime,
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#' @return a vector
SinCos_4Obs <- function(x, d, tmaxshift = 0.6)
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

  DayDataCol <- c("TerraDay","AquaDay")
  DayViewtimeCol <- c("TerraDayUTC","AquaDayUTC")
  NightDataCol <- c("TerraNight","AquaNight")
  NightViewtimeCol <- c("TerraNightUTC","AquaNightUTC")

  matrix2solv <- cbind(x["TerraDay"], x["TerraNight"],x["AquaDay"], x["AquaNight"],TerraDayUTC,TerraNightUTC, 
                       AquaDayUTC, AquaNightUTC, trise, tm, daylen, nightlen)
  TaT0 <- as.data.frame( t(apply(matrix2solv,1, SinCosMinsSolv, DayDataCol, NightDataCol,
                                 DayViewtimeCol, NightViewtimeCol,
                                 TriseCol="trise", TmaxCol="tm", 
                                 DayLengthCol="daylen", NightLengthCol="nightlen")) )
  names(TaT0) <- c("Ta","T0")

  temps <- TempBySinCosMethod(0:23, TaT0["Ta"], TaT0["T0"], trise, tset, tm, daylen, nightlen)

  means <- rowMeans(temps)
  return(means)
}




#' Solve the parameters required by sin-Linear approach using 4 LST obs
#'
#' @param m data frame holding LST obs and associated view times, whose names are specified by other params.
#' @param DayDataCols names of cols for day LST obs, e.g., c("TerraDay", "AquaDay")
#' @param NightDataCols names of cols for night LST obs, e.g., c("TerraNight", "AquaNight")
#' @param DayViewtimeCols names of cols for view times of day LST obs, e.g. c("TerraDayViewtime", "AquaDayViewtime")
#' @param NightViewtimeCols names of cols for view times of night LST obs, e.g. c("TerraNightViewtime", 
#' "AquaNightViewtime")
#' @param TriseCol col name for sunrise time
#' @param TsetCol col name for sunset time
#' @param wCol col name for param w
#' @param t0Col col name for param t0
#' @return a vector holding params (A, B, a b). A, B are param for fitting day period 
#' and a, b are params for fiting night period
SinLinear2Solv <- function(m, DayDataCols,NightDataCols, DayViewtimeCols, NightViewtimeCols,
                           TriseCol, TsetCol, wCol,t0Col) {
  
  # for night fitting, using linear
  # NightViewtimeCols and NightDataCols both contain two cols
 # y <- matrix(c(m[NightViewtimeCol1],1,m[NightViewtimeCol2],1),nrow=2,byrow=TRUE)
  y <- matrix(c(m[NightViewtimeCols],1,1),nrow=2)
  g <- matrix(c(LST2Celsius(m[NightDataCols])),nrow=2)
  X <- solve(y,g)
  a <- X[1]
  b <- X[2]

  Tset <- m[[TsetCol]]*a+b

  #DayViewtimeCols and DayDataCols both contain two cols
  Y <- c(sin(m[[wCol]]*(m[DayViewtimeCols]-m[[t0Col]])),sin(m[[wCol]]*(m[[TsetCol]]-m[[t0Col]])))
  G <- c(LST2Celsius(m[DayDataCols]), Tset)
  X <- lsfit(Y,G)
  B <- X$coef[1]
  A <- X$coef[2]
  return(c(A,B,a,b))
}

#' Calc temperatures at given times by applying Sin-Linear method with specified parameters
#'
#' @param t a vector contains time points
#' @param A amplitude for sin
#' @param B vertical offset
#' @param a linear coefficient
#' @param b the intercept
#' @param trise the time of sun rise
#' @param tset the time of sun set
#' @param w the angular frequency
#' @param t0 the Horizontal offset
#' @return data frame
#' @note the length of parameters in sin-linear method should be equal.
TempBySinLinearMethod <- function(t, A, B, a, b, trise, tset, w, t0)
{
  # for each t and each row of other parameters
  atemp <- function(m, t1)
  {
    if (t1>=m[5] && t1 <m[6]) {# for day
      lst <- sin(m[7]*(t1-m[8]))*m[1]+ m[2]
    }else if(t1 >= m[6]){    # for night
      lst <- t1*m[3] + m[4]
    }else
      lst <- sin(m[7]*(m[5]-m[8]))*m[1]+ m[2]
    return(lst)
  }
  m <- cbind(A, B, a, b, trise, tset, w, t0)

  mlst <- NULL
  for (tt in t)
  {
    lst <- apply(m, 1, atemp, t1=tt)
    mlst <- cbind(mlst, lst)
  }

  colnames(mlst) <- t

  return(mlst)

}

#' Daily means using the Sin-Linear approach with four obs: TerraDay TerraNight AquaDay and AquaNight.
#'
#' @param x a data.frame containing columns: Lat, Lon, TerraDay, TerraNight, AquaDay, AquaNight,
#'   TerraDayViewtime, TerraNightViewtime, AquaDayViewtime, AquaNightViewtime,
#' @param d date from the MODIS LST file name; it's UTC based. e.g. 2010001
#' @param tmaxshift the maximum temperature time minus the solar noon
#' @return a vector
#' sin-linear
SinLinear <- function(x, d, tmaxshift = 0.6)
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

  matrix2solv <- c(matrix2solv,ABab)
  temps <- TempBySinLinearMethod(0:23, matrix2solv$A, matrix2solv$B, matrix2solv$a, matrix2solv$b, 
                                 matrix2solv$trise, matrix2solv$tset, matrix2solv$w, matrix2solv$t0)
  means <- rowMeans(temps)
  return(means)
}

