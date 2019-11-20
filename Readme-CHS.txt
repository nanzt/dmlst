����predictor-corrector��MODIS LST �վ��¶ȼ��㹤�� (pcDMLST)
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

1.��Ŀ����
�ó����ʵ��������һ��predictor-corrector�ķ��������վ�MODIS�ر��¶ȣ�daily mean MODIS LST����
 * \bin �������
  * main.R�ǿ��Ƴ���
  * Predict.R ����������Ԫ���վ��ر��¶ȣ�
  * Prection_priv.R ��ֵ���㼰���ذ�����ֵ����������ݿ�
  * prepare_train_pixel.R�Ǹ���׼������correction��4ֵ��Ԫ��
  * Establishing_corrector.R�Ǹ���У��׼����
  * Correct.R�Ǹ�������У����
  
  * helperfunc.R����һЩ����������
  * generate_daylist.R���ɴ�����������б�
  * Read_config.R�Ǹ����ȡ�����ļ���
  * PrintVersionInfo.r ��ӡ����汾��Ϣ������

  * config.json �����ļ���
  * dailyMeanLSTMethods.R�������LST�ձ仯���ߵĸ��ֻ���������
 * \sampledata ��������
 * \Readme.txt ���ļ�

2.��������
�ó������R���Կ�������Ҫ R 3.5.3+ �汾��

3.Դ����
ʹ��TERRA��MOD11���ݺ�����AQUA��MYD11���ݣ����ݸ�ʽΪtif����Ҫ��������ÿ���4�ι۲��Լ���Ӧ����ʱ�䣬��8�����ݣ��� Terra/Aqua LST_Day��Terra/Aqua LST_Night��Terra/Aqua Day_view_time��Terra/Aqua Night_view_time����Щ����Ӧ����ͬ�������ڡ���Щtif�ļ������Դ�Terra/Aqua MODIS LST ��HDF��������ȡ�õ���

4.ǰ������
a.��װR3.5.3�����ϰ汾��RGui��Rstudio�������У�
 * RGui �ɴ�https://cran.r-project.org/bin/windows/base/ �������°汾��
 
b.�ó������Ҫ��װR�е�tictoc��GoFKernel��lubridate��rjson��stringr�������������ʱ��ʾû�а�װ���ϰ��������ֶ���װ��
 install.packages('tictoc')
 install.packages('GoFKernel')
 install.packages('lubridate')
 install.packages('rjson') 
 install.packages('stringr')
 ...
ע����Щ��İ�װ������ҪһЩʱ�䡣

c.����.R�ļ��������ļ�config.json��ط���ͬһ�ļ����£�������·���в������ģ�

5.����
a.�����ļ�Ϊconfig.json: 
 * start_date��end_dateΪ��ֹ���ڣ������ʽΪ��yyyyddd����
 * output_paramsָ�������ģ�Ͳ�����
 * ȫ���Ŀ�����Ĳ�������"Ta"��"T0"��"trise"��"tset"��"tm"��"daylen"��"nightlen"��"A"��"B"��"a"��"b"��"w"��"t0"������Ta, T0����sin-cos������A, B, a, b, w, t0������������sin-linear�������������Ϊsin-cos������sin-linear�������У�����Ϊ-999����û�����ݣ�
 * source_pathΪԴ����·����
 * output_pathΪ���Ŀ��·����
 * debugѡ������Ƿ����ģʽ��TRUE��ʾ����ģʽ����ʱpixel_numѡ����Ч��FALSE��ʾ����ģʽ����ʱpixel_num��Ч��ȫ������Ч��Ԫ��������㣻
 * pixel_num����������Ԫ������debugΪFALSEʱ��ѡ����Ч��
 * train_sizeΪ����correction��4ֵ��Ԫ������
 * verboseΪ���������Ϣģʽ��

b.��cmd��cd��main.R����Ŀ¼������RScript main.R
 * cd path/to/the/bin/directory
 * RScript main.R

6. ���
һ��������������һ��.CVS�ļ�������.CSV�ļ�����һ���ļ����£�λ����output_path����������

7.���󼰽��
a.�����ʾ 'RScript' is not recognized as an internal or external command, operable program or batch file,
������û���ҵ�RScript���ڵ�·������Ҫ��RScript���ڵ�·����ӵ�ϵͳ��ִ���ļ�·�������
 * ����ȷ��RScript.exe���ڵ�·����һ���� c:\program file\R\R-3.6.0\bin ���Ƶ�Ŀ¼�¿����ҵ����ļ���
 * Control Panel > System and Security > System > Advanced system settings > Environment Variables �ҵ� System variables �µ�Path���������Edit����RScript���ڵ�·����ӽ�ȥ��
 * ��cmd������RScript����Ƿ��Ѿ������ҵ������ע���Сд���С�

b.�����ʾ Fatal error: cannot open file 'main.R': No such file or directory. ��Ҫ��cmd�ｫ��ǰĿ¼�л��� main.R���ڵ�Ŀ¼��
 * cd path/to/the/bin/directory
 
c.������������
In file(filename, "r", encoding = encoding):
  Cannot open file 'dailyMeanLSTMethods.R': No such file or direcotry
������Ҫ��R�ļ����Ƕ���ͬ��binĿ¼�£������������ظó������

