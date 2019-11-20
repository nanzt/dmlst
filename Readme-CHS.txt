基于predictor-corrector的MODIS LST 日均温度计算工具 (pcDMLST)
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

1.项目描述
该程序包实现了利用一种predictor-corrector的方法计算日均MODIS地表温度（daily mean MODIS LST）。
 * \bin 程序代码
  * main.R是控制程序；
  * Predict.R 初步估算像元的日均地表温度；
  * Prection_priv.R 均值估算及返回包括均值与参数的数据框
  * prepare_train_pixel.R是负责准备用于correction的4值像元；
  * Establishing_corrector.R是负责校正准备；
  * Correct.R是负责数据校正；
  
  * helperfunc.R包含一些辅助函数；
  * generate_daylist.R生成待处理的日期列表；
  * Read_config.R是负责读取配置文件；
  * PrintVersionInfo.r 打印软件版本信息函数；

  * config.json 配置文件；
  * dailyMeanLSTMethods.R包括求解LST日变化曲线的各种基础方法；
 * \sampledata 例子数据
 * \Readme.txt 本文件

2.开发环境
该程序采用R语言开发，需要 R 3.5.3+ 版本。

3.源数据
使用TERRA的MOD11数据和来自AQUA的MYD11数据，数据格式为tif，需要两个卫星每天的4次观测以及对应成像时间，共8个数据，即 Terra/Aqua LST_Day，Terra/Aqua LST_Night，Terra/Aqua Day_view_time和Terra/Aqua Night_view_time。这些数据应在相同的区域内。这些tif文件都可以从Terra/Aqua MODIS LST 的HDF数据中提取得到。

4.前置条件
a.安装R3.5.3及以上版本，RGui及Rstudio均可运行；
 * RGui 可从https://cran.r-project.org/bin/windows/base/ 下载最新版本；
 
b.该程序包需要安装R中的tictoc、GoFKernel、lubridate、rjson、stringr包，如果在运行时提示没有安装以上包，则需手动安装：
 install.packages('tictoc')
 install.packages('GoFKernel')
 install.packages('lubridate')
 install.packages('rjson') 
 install.packages('stringr')
 ...
注意这些库的安装可能需要一些时间。

c.各种.R文件与配置文件config.json务必放在同一文件夹下，建议在路径中不含中文；

5.运行
a.配置文件为config.json: 
 * start_date及end_date为起止日期，输入格式为“yyyyddd”；
 * output_params指定输出的模型参数；
 * 全部的可输出的参数包括"Ta"、"T0"、"trise"、"tset"、"tm"、"daylen"、"nightlen"、"A"、"B"、"a"、"b"、"w"、"t0"，其中Ta, T0来自sin-cos方法，A, B, a, b, w, t0六个参数来自sin-linear方法，其余参数为sin-cos方法和sin-linear方法共有，参数为-999代表没有数据；
 * source_path为源数据路径；
 * output_path为输出目标路径；
 * debug选项决定是否测试模式，TRUE表示测试模式，此时pixel_num选项有效；FALSE表示运行模式，此时pixel_num无效，全部的有效像元均参与计算；
 * pixel_num参与计算的像元数量，debug为FALSE时此选项无效；
 * train_size为建立correction的4值像元数量；
 * verbose为输出冗余信息模式。

b.打开cmd，cd到main.R所在目录，运行RScript main.R
 * cd path/to/the/bin/directory
 * RScript main.R

6. 输出
一天的数据输出包括一个.CVS文件，所有.CSV文件均在一个文件夹下，位置由output_path参数决定。

7.错误及解决
a.如果提示 'RScript' is not recognized as an internal or external command, operable program or batch file,
是由于没有找到RScript所在的路径。需要将RScript所在的路径添加到系统可执行文件路径变量里。
 * 首先确定RScript.exe所在的路径，一般在 c:\program file\R\R-3.6.0\bin 类似的目录下可以找到该文件；
 * Control Panel > System and Security > System > Advanced system settings > Environment Variables 找到 System variables 下的Path变量，点击Edit，将RScript所在的路径添加进去。
 * 在cmd下敲入RScript检查是否已经可以找到该命令。注意大小写敏感。

b.如果提示 Fatal error: cannot open file 'main.R': No such file or directory. 需要在cmd里将当前目录切换到 main.R所在的目录。
 * cd path/to/the/bin/directory
 
c.若出现类似于
In file(filename, "r", encoding = encoding):
  Cannot open file 'dailyMeanLSTMethods.R': No such file or direcotry
表明需要的R文件并非都在同个bin目录下，建议重新下载该程序包。

