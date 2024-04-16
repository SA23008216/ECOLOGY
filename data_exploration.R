#HEADER------------------------------------
#
#Author: wangcijian
#Copyright   Copyright 2024-wangcijian
#Email:3310166542@qq.com
#
#Date:2024-04-14
#
#Script Name:homework-05-data_exploration.R
#检测Doubs数据集中环境因素之间是否存在共线性，并对鱼类与环境因素之间关系进行分析和可视化
#
#Script Description:
#1.处理Doubs数据集中的缺失数据
#2.检测环境因素之间是否存在共线性
#3.对鱼类与环境因素之间关系进行分析和可视化
#SETUP ----------------------------------------------

#1.处理Doubs数据集中的缺失数据
#下载并调用相关包
install.packages("pacman")
library(pacman)   #加载pacman包以简化R包的管理和加载过程
library(ade4)
p_load(tidyverse)   #加载指定的tidyverse包，tidyverse包内含很多个常用的包，可用于数据整理、可视化和分析数据
p_load(mice)   #加载指定的mice包，缺失值分析包，多重插补：利用数据中的信息来估计缺失值，进行合理的数据填充
p_load(VIM)    #加载指定的VIM包，可视化缺失数据的R包，提供方法来分析和可视化数据集中的缺失值模式
#获取ade4包内置的doubs数据集
data("doubs")
#查看doubs结构
str(doubs)
summary(doubs)

#导入数据，将数据集变为变量
doubs_data <- doubs
#数据检视
doubs_data %>% glimpse()
#env：包含环境相关的数据，共有30行和11列。包含数值型变量（双精度型）和字符型变量。
#fish：包含与鱼类相关的数据，共有30行和27列。包含数值型变量和字符型变量。
#xy：包含位置坐标的数据，共有30行和2列，均为是双精度型变量。
#species：包含了鱼类物种的信息，共有27行和4列，所有变量都是字符型变量。

#缺失值检验
is.na(doubs_data)  #返回值为FALSE，env、fish、xy和species内都没有缺失值
#数据整体缺失率（前面步骤显示没有缺失值的话，后续整体缺失率不用检查）
sum(is.na(doubs_data))
mean(is.na(doubs_data)) 

#2.检测环境因素之间是否存在共线性
#下载并调用相关包
install.packages("car")
install.packages("vcd")
install.packages("FactoMineR")
install.packages("irr")
library(irr)
library(car)
library(corrplot)
library(vcd)
library(FactoMineR) #用于主成分分析
library(ggplot2)    #用于创建可视化图表

#①相关分析，使用corrplot包中的函数将相关性矩阵可视化
correlation_matrix <- cor(doubs_data$env)  
corrplot(correlation_matrix, method = "circle")  
#圆形图表中，相关性的强度通过圆的直径和颜色来表示，直径越大颜色越深表示相关性越强，而颜色越浅表示相关性越弱。

#②主成分分析（PCA），将多维数据转换为更少维度的数据，更好地理解数据的结构和模式
#提取环境变量数据，并存储在env_data中
env_data <- doubs_data$env
#计算环境变量之间的相关系数矩阵
cor_matrix <- cor(env_data)     #相关系数矩阵显示了每对环境变量之间的线性相关程度，取值范围从-1到1，其中1表示完全正相关，-1表示完全负相关，0表示无相关性。
#主成分分析
pca_result <- dudi.pca(cor_matrix, scannf = FALSE)
#查看主成分解释比例
summary(pca_result)   #每个主成分对数据方差的解释程度和在数据投影中的贡献
#查看主成分载荷值
print(pca_result$co)
#在第一主成分（Comp1）上：dfs、flo、har、pho、nit，这些变量在第一主成分上的载荷值都接近于 1，表示它们之间具有很高的正相关性，可能存在共线性。
#在第二主成分（Comp2）上：pH、oxy、bdo，这些变量在第二主成分上的载荷值较高，表示它们之间可能存在一定程度的相关性。

#③容忍度和VIF
tolerance_vif <- vif(lm(doubs$fish$Cogo ~ ., data = doubs_data$env))
tolerance_vif  #建议去除“dfs”、“alt”、“flo”和“amm”环境变量
#容忍度的取值范围在0到1之间，越接近1表示自变量之间的共线性越低，越接近0表示共线性越高。
#VIF的值越大，表示自变量之间的共线性越高。VIF大于10认为存在较严重的共线性问题。


#3.对鱼类与环境因素之间关系进行分析和可视化
#导入所需的库
library(ggplot2)
library(dplyr)

#①热图，可视化呈现鱼的种类鱼环境变量之间的相关性
#计算鱼类数量和环境变量之间的相关系数
combined_data <- cor(doubs_data$fish, doubs_data$env)
#绘制相关系数矩阵的热图
corrplot(combined_data, method = "color")

#②主成分回归（PCR），了解自变量的主成分与因变量之间的线性关系
pcr_model <- lm(doubs$fish$Cogo ~ ., data = doubs_data$env)  #拟合线性回归模型，选鱼种类Cogo作为因变量，环境变量作为自变量
summary(pcr_model) 
#主成分回归的诊断图
options(ggrepel.max.overlaps = 50)  #增加max.overlaps的值
plot(pcr_model)

#③线性回归图
#以鱼类群Phph作为因变量，环境变量oxy作为自变量，探究两者间关系
#绘制散点图，鱼类群（Phph）位于 y 轴，环境变量斜坡（oxy）位于 x 轴
plot(doubs$fish$Phph ~ doubs_data$env$oxy, xlab = "oxy", ylab = "Phph", main = "Linear Regression Plot")
#通过线性回归模型拟合在散点图上添加一条直线,拟合鱼类种群和环境变量斜坡之间的线性关系，分析它们之间的相关性和趋势。
abline(lm(doubs$fish$Phph ~ doubs_data$env$oxy), col = "red")
#绘制线性回归模型的诊断图
model=lm(doubs$fish$Phph ~ doubs_data$env$oxy)
plot(model)
#Residuals vs Fitted：残差与拟合值的散点图，用于检查残差是否具有常量方差和是否存在非线性关系。
#Normal Q-Q：用于检查残差是否近似正态分布。
#Scale-Location：用于检查残差是否具有恒定的方差。
#Residuals vs Leverage：用于检查是否有异常值或高杠杆点。

#④箱线图
#以鱼种类Phph作为因变量，环境变量pH作为自变量，探究两者间关系
#绘制箱线图，鱼类群（Phph）位于 y 轴，环境变量斜坡（pH）位于 x 轴
boxplot(doubs$fish$Phph ~ doubs_data$env$pH, xlab = "pH", ylab = "Phph", main = "Boxplot")