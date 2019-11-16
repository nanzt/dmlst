# 
# day =1
# year = 2010
# tifInPath = "Data/"

#'  Return a list including LST and associated viewtime files in a day
#'  
#'  @param day the day, 1 to 365 or 366 in a leap year
#'  @param year the year
#'  @param tifInPath the path where we can find the LST tif files
#'  @return a list including four LST files and four associated view time files in a day,
#'    with names in order "TerraDay", "TerraNight", "AquaDay", "AquaNight",
#'    "TerraDayViewtime", "TerraNightViewtime", "AquaDayViewtime",
#'    "AquaNightViewtime",
#'  @note use names, e.g. xx["TerraDay"], to obtain the list element
filesForLstInADay <- function(day, year, tifInPath)
{
  require(stringr)
  searchDay  <- str_pad(day, 3, pad = "0")
#MYD.+2010001.*LST_Day.+tif$
  searchStr1  <- paste("MOD.+", year,searchDay,".*LST_Day.+tif$",sep='') # Terra Day

  searchStr2  <- paste("MOD.+", year,searchDay,".*LST_Night.+tif$",sep='') # Terra Night
  searchStr3  <- paste("MYD.+", year,searchDay,".*LST_Day.+tif$",sep='') # Aqua Day
  searchStr4  <- paste("MYD.+", year,searchDay,".*LST_Night.+tif$",sep='') # Aqua Night
  viewtimeStr1 <-   paste("MOD.+", year,searchDay,".*Day_view_time.+tif$",sep='') # Terra Day Viewtime
  viewtimeStr2  <- paste("MOD.+", year,searchDay,".*Night_view_time.+tif$",sep='') # Terra Night Viewtime
  viewtimeStr3  <- paste("MYD.+", year,searchDay,".*Day_view_time.+tif$",sep='') # Aqua Day Viewtime
  viewtimeStr4  <- paste("MYD.+", year,searchDay,".*Night_view_time.+tif$",sep='') # Aqua Night Viewtime

  file1   <- list.files(tifInPath,  pattern = searchStr1 , full.names = TRUE, recursive  = FALSE)
  file2   <- list.files(tifInPath,  pattern = searchStr2 , full.names = TRUE, recursive  = FALSE)
  file3   <- list.files(tifInPath,  pattern = searchStr3 , full.names = TRUE, recursive  = FALSE)
  file4   <- list.files(tifInPath,  pattern = searchStr4 , full.names = TRUE, recursive  = FALSE)
  file5   <- list.files(tifInPath,  pattern = viewtimeStr1 , full.names = TRUE, recursive  = FALSE)
  file6   <- list.files(tifInPath,  pattern = viewtimeStr2 , full.names = TRUE, recursive  = FALSE)
  file7   <- list.files(tifInPath,  pattern = viewtimeStr3 , full.names = TRUE, recursive  = FALSE)
  file8   <- list.files(tifInPath,  pattern = viewtimeStr4 , full.names = TRUE, recursive  = FALSE)
  
  if (length(file1) !=1 | length(file2) !=1 | length(file3) !=1 |length(file4) !=1 |
      length(file5) !=1 | length(file6) !=1 | length(file7) !=1 |length(file8) !=1){
    return(NULL)
  }
  else{
    ret <- cbind(file1,file2,file3,file4, file5, file6, file7, file8)

    names(ret) <- cbind("TerraDay","TerraNight","AquaDay","AquaNight","TerraDayViewtime","TerraNightViewtime","AquaDayViewtime","AquaNightViewtime")

    return(ret)

  }

}


#' Combine the LST and viewtime files in a day and create a data.frame including
#' lon, lat, four LST values, and four view time. Only records that all LST and
#' viewtime are present are collected.
#' 
#' @param fileList the list returned by filesForLstInADay()
#' @param sizeReturned the number of elements returned
#' @return a data.frame including lon, lat, four LST values and four view time.
#' @seealso \code{\link{filesForLstInADay}};
lstDataset <- function(fileList,verbose) {
  if (!all(c("TerraDay", "TerraNight", "AquaDay", "AquaNight", 
            "TerraDayViewtime", "TerraNightViewtime", "AquaDayViewtime", "AquaNightViewtime")
          %in% names(fileList)))
    stop("fileList not include necessary names")

  #convert raster to points
  require(raster)
  tiffile <- fileList["TerraDay"]
  raster <- stack(tiffile) 

  points <- rasterToPoints(raster, NULL, TRUE) # return a SpatialPointsDataFrame object
  
  #start to combine
  tmp      <- NULL
  for (fileName in fileList)
  {
    rasterData <- stack(fileName)

    LSTExtract <- extract(rasterData, points, sp = FALSE) 
    tmp <- cbind(tmp,LSTExtract)
    if(verbose==TRUE)
    {
      last6 <- paste("... reading",fileName)
      message(last6)
    }
  }
  # combine the coords of points
  LSTValues <- cbind(coordinates(points), tmp)
  #change names 
  colnames(LSTValues) <- c("Lon", "Lat", names(fileList))
               
  # filter out missing pixels

  # LSTValues <- subset(LSTValues, LSTValues[,"TerraDay"] != 0 & 
  #            LSTValues[,"TerraNight"] != 0 & LSTValues[,"AquaDay"] != 0 & LSTValues[,"AquaNight"] != 0 
  #            & LSTValues[,"TerraDayViewtime"] != 255 & 
  #              LSTValues[,"TerraNightViewtime"] != 255 & LSTValues[,"AquaDayViewtime"] != 255 
  #            & LSTValues[,"AquaNightViewtime"] != 255 
  #            )
  
  
  
  # sizeAct <- sizeReturned
  # if ( sizeReturned > nrow(LSTValues)) sizeAct <- nrow(LSTValues)
  
   set.seed(123)
   indices <- sort(sample(1:nrow(LSTValues)))
   retLST   <- data.frame(LSTValues[indices, ])
  
  
  #retLST   <- data.frame(LSTValues)
  return(retLST)
}


#' Caculate sunrise, sunset, day length, night length and tmax, in UTC
#' 
#' @param x data.frame containing columns: Lat, Lon
#' @param d given date, in format yyyyddd, d is in UTC
#' @param tmaxshift, the time of max temperature minus solar noon
#' @param LatCol column name of Lat
#' @param LonCol column name of Lon
#' @return data.frame
sunRiseSet <- function(x, d, tmaxshift = 0.6, LatCol = "Lat", LonCol = "Lon"){
  #in case the input d is numeric
  if (is.numeric(d)) d <- toString(d)

  require(lubridate)
  theday <- parse_date_time(d,'Yj','UTC')

  theday1 <- as.Date(theday)

  
  
  require(suncalc)
  dat1 <- data.frame(date = rep(theday1,nrow(x)), lat = x[[LatCol]], lon = x[[LonCol]])

  #sunrs holds sunrise and sunset times in UTC
  sunrs <- getSunlightTimes(data = dat1, keep= c("sunrise", "sunset", "solarNoon"), tz = 'UTC')
  trise <- as.vector(difftime(sunrs[["sunrise"]],theday, units="hours"))
  tset <- as.vector(difftime(sunrs[["sunset"]],theday, units="hours"))
  daylen    <- tset - trise
  nightlen  <- 24-daylen
  tm <- as.vector(difftime(sunrs[["solarNoon"]], theday, units="hours")) + tmaxshift

  return(data.frame("trise" = trise, "tset"= tset, "daylen" = daylen, "nightlen" = nightlen, "tm" = tm))
}




#' Convert raw LST values to Celsius degree
#' 
#' @param x raw LST vector, matrix, data.frame, etc
#' @return in Celsius
LST2Celsius <- function(x)
{
 # if (!is.vector(x) && ncol(x) >1) stop("x neither a vector nor one column.")
  return (x*0.02 - 273.15)
}






#' Convert view time in local time to UTC time
#' 
#' @param x view time data
#' @param lon a vector, representing longitude
#' @return in UTC time
#' @note the Aqua night pass time is approx 1:30 am next day local time, so it
#'   needs to adjust by adding 24. x and lon should be equal in rows.
Viewtime2UTC <- function(x, lon)
{
  if (!is.vector(lon)) stop ("lon not vector.")
  #make sure equal length of rows
  if (is.vector(x)) r <- length(x)
  else r <- nrow(x)
  if (r != length(lon)) stop("x and lon not equal length in rows.")
  
  if(is.data.frame(x)) x <- as.matrix(x)
  
   utc <- x * 0.1 - lon /15.0
   utc <- ifelse(utc <0, utc + 24, utc)
   return(utc)
}



#' replicate rows
#' 
#' @param x a vector to replicate
#' @param n the times of replication
#' @return a matrix
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

