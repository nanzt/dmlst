
source("dailyMeanLSTMethods.R")
source("Establishing_corrector.R")
source("Correct.R")
source("PrintVersionInfo.r")
source("Read_config.R")
source("prepare_train_pixels.R")
source("generate_daylist.R")
source("Predict.R")


library(tictoc)
suppressMessages(library(GoFKernel))
suppressMessages(library(lubridate))
library(rjson)
library(stringr)
suppressMessages(library(raster))
suppressMessages(library(sp))
suppressMessages(library(suncalc))

# insert version information
printVersionInfo("1.0.0","Aug 3, 2019")


# Read and parse the config file
Read_config_result <- Read_config()
# Retrieve starting and ending years
yearlist <- Read_config_result[['input_startyear']]:Read_config_result[['input_endyear']]

tic()
# Loop through years
for(year in yearlist){
  
  # generate the day list from the interval specified in the config file
  daylist <- generate_daylist(year,Read_config_result[['input_startyear']],Read_config_result[['input_endyear']],
                               Read_config_result[['input_startday']],Read_config_result[['input_endday']])

for(day in daylist){
  
  options(warn=-1)

  # formatting the date str, 3 width, padding with 0
  searchDay  <- str_pad(day, 3, pad = "0")
  # adding year to the date string
  searchStr  <- paste(year, searchDay, sep='')
  d <- searchStr
  
  # get the filelist for the processing day
  fileList <- filesForLstInADay(day, year, Read_config_result[['TifInPath']])
  if(is.null(fileList)){
    msgstr=paste(d,"No source data found!")
    message(msgstr)
    next
  }
  
  message("Start to read source files...")
  # create dataframe containing lat/lon and necessary data
  lstDS <- lstDataset(fileList,Read_config_result[['verbose']])
  length <- nrow(lstDS)
  message("... Completed in reading source files.")
  # assign the processed pixel numbers if in debug mode and select random pixels
  if(Read_config_result[['debugMode']]==TRUE)
  {
    export_Pixelnum <- Read_config_result[['debug_pixelNum']]
    # selection pixels of this amount randonly
    set.seed(NULL)
    sam <- sample(1:length,Read_config_result[['debug_pixelNum']])
    lstDS2 <- lstDS[sam,]
  }
  else if(Read_config_result[['debugMode']]==FALSE)
  {
    export_Pixelnum <- length
    lstDS2 <- lstDS
  }
  # debugMode is false by default
  else
  {
    Read_config_result[['debugMode']]=FALSE
    export_Pixelnum <- length
    lstDS2 <- lstDS
    message("Wrong value for debug option. Use FALSE by default.")
  }
  
  # predict the means of the pixels 
  Predicted_data <- Predict(lstDS2,Read_config_result[['output_params']],d)

  # process the train_size option
  data_ForCorrector <- prepare_train_pixels(Read_config_result[['train_size']],lstDS,d)
  
  # prepare the relationship for upcoming correction
  data_Corrector <- Establishing_corrector(data_ForCorrector,Read_config_result[['verbose']],d)
  
  # handle the correction
  corrected_data <- Correct(data_Corrector,Predicted_data,Read_config_result[['verbose']])
  
  # add the corrected data to the previous data.frame
  Predicted_data <- cbind(Predicted_data,corrected_data)
  
  # reorder the data.frame
  # rownames(Predicted_data) <- seq(1,nrow(Predicted_data),1)

  # output the results
  message("Write to CSV...")
  write.csv(Predicted_data, file=paste(Read_config_result[['output_dataPath']],"/",d,".csv", sep=''))
  output5 <- paste("Processing",d,"completed.")
  message(output5)

  
  }
}
toc()