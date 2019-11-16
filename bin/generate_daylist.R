#' Process the date interval
#'
#' @param year Current year
#' @param input_startyear Starting year in the config file
#' @param input_endyear Ending year in the config file
#' @param input_startday Starting day in the config file
#' @param input_endday Ending day in the config file
#'
#' @return The day list in the current year
#' @export
#'
#' @examples
generate_daylist <- function(year,input_startyear,input_endyear,input_startday,input_endday)
{
  if(input_endyear==input_startyear){
    start_day <- input_startday
    end_day <- input_endday
    daylist <-  start_day:end_day
  }
  else{
    if(year==input_startyear){
      start_day <- input_startday
      if(leap_year(year)==TRUE){
        end_day <- 366
      }
      else{
        end_day <- 365
      }
      daylist <-  start_day:end_day
    }
    else if(year==input_endyear){
      end_day <- input_endday
      start_day <- 1
      daylist <-  start_day:end_day
    }
    else{
      start_day <- 1
      if(leap_year(year)==TRUE){
        end_day <- 366
      }
      else{
        end_day <- 365
      }
      daylist <-  start_day:end_day
    }
  }
  # return the days in the current year
  return(daylist)

}