A tool for estimating daily mean MODIS LST based on a predictor-corrector approach (pcDMLST)
Version 1.0.0

Authors: 
Zhuotong Nan (giscn@msn.com), 
Xuehui Sun, 
Ben Yang

Nanjing Normal University
Web: https://permalab.nanzt.info

Nov 20, 2019
 + English translation to the Readme file.
Aug 3, 2019
 + Initial release

1. Description
This tool implements a two-stage predictor-corrector approach to estimate daily mean MODIS LST. 
The related paper to describe this approach are now under review.
 * \bin -Program codes
  * main.R -The entry point function;
  * Predict.R -To make initial estimation of daily mean LST values for the valid pixels;
  * Prection_priv.R -To calculate the means and return dataframes containing means and associated parameters;
  * prepare_train_pixel.R -To prepare the pixels for training that have complete four daily MODIS LST observations on them;
  * Establishing_corrector.R -To derive the necessary functions for the upcoming correction;
  * Correct.R - To perform the bias correction procedure;
  
  * helperfunc.R -Some auxillary functions;
  * generate_daylist.R -To generate a list of days ready for the iteration;
  * Read_config.R -To do with reading parameters from the config file;
  * PrintVersionInfo.r -A function showing the version information;

  * config.json -The configurate file, which should be properly configurated for your needs;
  * dailyMeanLSTMethods.R -Included core functions responsible for fitting the diurnal temeprature cycle curve with varying numbers of input parameters;
 * \sampledata -Include an example data for testing this tool;
 * \Readme.txt -This file.

2. Development environ.
This tool is developed with R. A version of 3.5.3+ is required.

3. A note about the source data
The source data include TERRA/MOD11 and AQUA/MYD11 in format of TIF. For each day, four images of LST observations and relating four images of view times are required. That is, in total, eight images are required for each day, i.e., Terra/Aqua LST_Day£¬Terra/Aqua LST_Night£¬Terra/Aqua Day_view_time and Terra/Aqua Night_view_time. Those images should be in same extent. Those tif images can be extracted from the  HDF file of Terra/Aqua MODIS LST products.

4. Prerequisite
a. R3.5.3 or above has been installed. Either RGui or Rstudio can run this tool.
 * RGui can be obtained from https://cran.r-project.org/bin/windows/base/ ;
 
b. Some packages are necessary for running this tool, including tictoc, GoFKernel, lubridate, rjson, stringr. If they are not already installed, you will be prompted for installing them by using the following commands: 
 install.packages('tictoc')
 install.packages('GoFKernel')
 install.packages('lubridate')
 install.packages('rjson') 
 install.packages('stringr')
 ...
Please note the installation may take a while to complete.

c. The config.json file should be located in the same folder as the .R files. No inclusion of Chinese characters in the path is highly advised;

5. How to run
a. Set up config.json: 
 * start_date and end_date specifies the starting and ending dates, respectively, with a format of "yyyyddd";
 * output_params specifies the parameters that will be output for diagnosis;
 * The parameters that can be output include "Ta" and "T0" from the Cosine-Sine method, "A", "B", "a", "b", "w", and "t0" from the Sine-Linear method£¬and "trise", "tset", "tm", "daylen", and "nightlen" shared by both methods. A parameter value of -999 indicates place holder;
 * source_path specifies the path of source LST images (in .TIF);
 * output_path is the path of output data;
 * debug is an option for entering a debug mode. A value of TRUE indicates a debug mode by which the option pixel_num becomes effective; FALSE for an optional mode by which the option pixel_num is deprecated and all valid pixels, instead of the number that specified by pixel_num, will be calculated;
 * pixel_num specifies the number of pixels participating the calculation. It is invalidated when debug is set to FALSE;
 * train_size specifies the number of pixels that are in presence of complete four daily MODIS LST observations for establishing the functions for bias correction;
 * verbose sets the tool run in a verbose mode.

b. Open a command/terminal window, change to the directory where main.R is located, and run "RScript main.R" (without quotes).
 * cd path/to/the/bin/directory
 * RScript main.R

6. Output
The output includes a .cvs file for each day. In case of multiple days to be processes, all .cvs files are in the same output folder that specifed by the option output_path.

7. Troubleshooting
a. If warned of 'RScript' is not recognized as an internal or external command, operable program or batch file,
it is because the system cannot find RScript.exe. Make sure RScript.exe installed and the path of it included in the system path variable.
 * Locate the path of RScript.exe, which is generally under c:\program file\R\R-3.6.0\bin by default;
 * Locate the Path variable within System variable through Control Panel > System and Security > System > Advanced system settings > Environment Variables, click Edit, and add the path of RScript;
 * Test it by typing RScript in the cmd window. Note the case is sensitive. 

b. If prompted by Fatal error: cannot open file 'main.R': No such file or directory. You need to change the current directory to that main.R is located in the cmd window.
 * cd path/to/the/bin/directory
 
c. For messages like
In file(filename, "r", encoding = encoding):
  Cannot open file 'dailyMeanLSTMethods.R': No such file or direcotry
it indicates it lacks necessary files. Re-download the tool, extract it to a folder and follow the instructions to run.



