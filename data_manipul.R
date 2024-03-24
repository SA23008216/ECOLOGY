#HEADER------------------------------------
#
#Author: wangcijian
#Copyright   Copyright 2024-wangcijian
#Email:3310166542@qq.com
#
#Date:2024-03-24
#
#Script Name:data_manipul.R
#
#Script Description:
#1.import and save data; 
#2.inspect data structure; 
#3.check whether a column or row has missing data;
#4.extract values from a column or select/add acolumn;
#5.transform a wider table to along format;
#6.visualize the data.
#
#SETUP ----------------------------------------------

#import and save data
#从指定路径下载文件surveys_wide.csv，通过dest参数将下载的文件保存至指定位置
download.file("tinyurl.com/dcmac2017dec/data/surveys_wide.csv",  
              dest="D:/ruanjian/Rstudio/ECOLOGY/surveys_wide.csv")
#加载tidyverse包
library(tidyverse)
#将ECOLOGY路径下的CSV文件读取到一个名为 surveys_wide 的变量中
surveys_wide <- read.csv("D:/ruanjian/Rstudio/ECOLOGY/surveys_wide.csv")


#inspect data structure
#str()函数来查看surveys_wide数据框的结构和摘要信息
str(surveys_wide)

#check whether a column or row has missing data
#检查整个数据框是否有缺失值
any(is.na(surveys_wide))

#extract values from a column or select/add acolumn
#将surveys_wide数据框作为管道的起点，将结果保存到surveys_sml的新数据框中
surveys_sml <- surveys_wide %>% 
#对前面数据框进行筛选操作，只保留year=1982的行，并传递结果
filter(year == 1982) %>%
#保留前面筛选获得数据中的plot_id、year和month这三列，舍弃其他列
select(plot_id, year, month)
#通过之前的筛选和选择操作创建新的数据框
surveys_sml

#transform a wider table to along format
surveys_long <- surveys_sml %>%
#利用gather()函数将数据框surveys_wide从宽格式转换为长格式
#year存储列名的列名，month存储值的列名。
gather(key = "year", value = "month")
str(surveys_long)


#visualize the data
# dot plots:基于surveys_wide数据集，以year为x轴变量，以DM为y轴变量，创建一个散点图
ggplot(data = surveys_wide, 
       aes(x = year, y = DM)) +
geom_point() 


