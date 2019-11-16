source("dailyMeanLSTMethods.R")

#' Prepare training pixels for corrector. The pixels are selected from those pixels with complete four
#' times of LST obs. 
#'
#' @param train_size The size of the train set
#' @param lstDS The complete LST dataset
#' @param d The processing date.
#'
#' @return A training pixel data frame
#' @export
#'
#' @examples
prepare_train_pixels <- function(train_size,lstDS,d){
  # convert train_size to numeric
  train_size <- as.numeric(train_size)

  export <- paste("Prepare",train_size,"training pixels","...")
  message(export)
  
  sinLinear_data <- subset(lstDS,(TerraDay!=0 & TerraNight!=0 & AquaDay!=0 & AquaNight!=0 & TerraDayViewtime!=255 
                                  & TerraNightViewtime!=255 & AquaDayViewtime!=255 & AquaNightViewtime!=255))
  length <- nrow(sinLinear_data) 
  # if train_size is less than the total number of 4-obs pixels, 
  # select train_size pixels randomly and prepare them for correction
  if(train_size <= length)
  {
  	# randomly select train_size pixels for training
  	set.seed(NULL)
  	sam <- sample(1:length,train_size)
  	sinLinear_data <- sinLinear_data[sam,]

  	SinLineardata_means <- SinLinear(sinLinear_data, d)
  	Raw_SinLineardata <- sinLinear_data
  }
  # if train_size is larger than the total number of 4-obs pixels,
  # select all 4-obs pixels as the training set
  else
  {
	SinLineardata_means <- SinLinear(sinLinear_data, d)
  	Raw_SinLineardata <- sinLinear_data
  }
  
  export2 <- paste("...",train_size,"pixels ready for training.")
  message(export2)
  # combine the processed pixels' means and unprocessed pixels into one data.frame
  # the number of rows in this data.frame depends on train_size
  # this data.frame is prepared for Establishing_correctorh()
  train_size_output <- cbind(SinLineardata_means,Raw_SinLineardata)
  return(train_size_output)
}