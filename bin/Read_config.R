
#' Read and parse the contents in the config file
#'
#' @return A list containing parsed items
#' @export
#'
#' @examples
Read_config <- function(){
  read_result <- fromJSON(file = "config.json")
  
  # date format should be yyyyddd; separate the yyyy and ddd parts.
  start_date <- read_result[['start_date']]
  end_date <- read_result[['end_date']]
  input_startyear <- substring(start_date, 1, 4)
  input_endyear <- substring(end_date, 1, 4)
  if(substring(start_date,5,5)=='0' & substring(start_date,6,6)!='0')
  {
    input_startday <- substring(start_date,6,7)
  }else if(substring(start_date,5,6)=='00'){
    input_startday <- substring(start_date,7,7)
  }else{
    input_startday <- substring(start_date,5,7)
  }
  if(substring(end_date,5,5)=='0' & substring(end_date,6,6)!='0')
  {
    input_endday <- substring(end_date,6,7)
  }else if(substring(end_date,5,6)=='00'){
    input_endday <- substring(end_date,7,7)
  }else{
    input_endday <- substring(end_date,5,7)
  }
  yearlist <- input_startyear:input_endyear
  
  
  # check if the input dates are wrong
  if(input_endyear<input_startyear){
    warning("Ending date must later than starting date.")
  }
  if(input_endyear==input_startyear){
    if(input_startday>input_endday){
      warning("Ending date must later than starting date.")
    }
  }
  
  # Parse parameters, source and output paths
  output_params <- c()
  output_paramsnum <- 0
  while(output_paramsnum<length(read_result[['output_params']]))
  {
    output_params <- append(output_params,read_result[['output_params']][[output_paramsnum+1]])
    output_paramsnum=output_paramsnum+1
  }
  TifInPath <- read_result[['source_path']]
  output_dataPath <- read_result[['output_path']]
 

  # Get debug mode and pixel numbers for debug
  debugMode <- read_result[['debug']]
  debug_pixelNum <- read_result[['pixel_num']]
  # Get the variable of train_size and verbose mode
  train_size <- read_result[['train_size']]
  verbose <- read_result[['verbose']]
  
  # check the validity of verbose
  if(verbose!=TRUE & verbose!=FALSE)
  {
    verbose=TRUE
    message("Wrong value for verbose option.Use TRUE by default.")
  }

  # Check the existence of the output path. Create if not.
  if(!(file.exists(output_dataPath)))
  {
    dir.create(output_dataPath)
  }

  # The output item list
  Read_config_result <- list(input_startyear,input_startday,input_endyear,input_endday,output_params,TifInPath,
                             output_dataPath,debugMode,debug_pixelNum,train_size,verbose)
  # Name the columns in the list. 
  names(Read_config_result) <- c("input_startyear","input_startday","input_endyear","input_endday",
                          "output_params","TifInPath","output_dataPath","debugMode","debug_pixelNum","train_size","verbose")
  # return the list
  return(Read_config_result)
}