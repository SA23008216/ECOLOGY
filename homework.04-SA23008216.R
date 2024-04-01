#HEADER------------------------------------
#
#Author: wangcijian
#Copyright   Copyright 2024-wangcijian
#Email:3310166542@qq.com
#
#Date:2024-03-31
#
#Script Name:Creating a regression model of mpg as target and others as features using random forest algorithm with caret package.
#
#Script Description:
#1.读取数据
#2.数据准备与预处理
#3.特征选择(递归特征消除)和可视化
#4.训练调整模型
#5.评估模型的性能
#
#SETUP ----------------------------------------------


#1.读取数据
#安装caret包，导入必要的库
install.packages("caret")
install.packages("randomForest")
install.packages("skimr")
install.packages("corrplot")
library(corrplot)
library(skimr)
library(caret)
library(randomForest)
library(ggplot2)
#加载 mtcars 数据集
data(mtcars)

#2.数据准备与预处理
#设置随机种子为100
set.seed(100)
#获取训练数据的行号，70%的数据被选为训练集，剩下的30%被用作测试集。
trainRowNumbers <- createDataPartition(mtcars$mpg, p=0.70, list=FALSE)
#创建训练数据集
trainData <- mtcars[trainRowNumbers,]
#创建测试数据集
testData <- mtcars[-trainRowNumbers,]
#查看创建的训练数据集和测试数据集是否正常
head(trainData)
head(testData)
#将mpg作为目标变量，其他变量作为特征
x = trainData[, 2:11]
y = trainData$mpg
#对训练数据集进行摘要统计
skimmed <- skim(trainData)
#查看摘要统计结果
print(skimmed)     #无缺失值，无须进行数据处理
#随机森林模型对数据宽容度大，数据预处理可看情况进行


#3.特征选择和可视化
#使用递归特征消除方法选择特征
#定义RFE算法的控制参数
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
#查看数据结构
str(trainData)
#执行RFE算法，找到最好预测能力的特征组合，并储存于results
results <- rfe(trainData[,-1], trainData$mpg, sizes = c(1:10), rfeControl = control)#从RFE算法的结果中提取所选择的最佳特征
print(results)
selected_features <- c("disp")

#特征可视化
#创建密度图矩阵，用于观察特征之间的关系
featurePlot(x = as.matrix(trainData[, selected_features]), 
            y = as.factor(trainData$mpg), 
            plot = "density",
            strip = strip.custom(par.strip.text = list(cex = 0.7))
)
#创建箱线图
featurePlot(x = as.matrix(trainData[, selected_features]), 
            y = as.factor(trainData$mpg), 
            plot = "box",
            strip = strip.custom(par.strip.text = list(cex = 0.7))
)

#4.训练调整模型
#查看在caret软件包中可用的算法列表
modelnames <- paste(names(getModelInfo()), collapse=',  ')
modelnames
#查找随机森林模型的相关信息，了解可用的参数选项
modelLookup('rf')
#设置随机种子
set.seed(100)
#使用随机森林算法训练模型，并在训练数据上进行预测
#使用randomForest函数训练随机森林模型(mpg是目标变量,disp是预测变量)
rf_model <- randomForest(mpg ~ disp, data = trainData)
#保存模型
saveRDS(rf_model, "rf_model.rds")
#可视化特征重要性
varImpPlot(rf_model, main = "Feature Importance with Random Forest")
#存储训练好的随机森林模型
model_rf  #结果显示最优模型的mtry参数值为2
#绘制随机森林模型的精度图
plot(model_rf, main = "Model Accuracies with randomForest")


#5.评估模型的性能
#查看trainData与testData的结构
str(trainData)
str(testData)
#对随机森林模型进行预测
predictions <- predict(rf_model, newdata = testData)

#计算均方根误差（RMSE）
RMSE <- sqrt(mean((predictions - testData$mpg)^2))
print(paste("RMSE:", RMSE))  #RMSE:9.05292403550986(80%数据为测试集) 
                             #RMSE:3.05898528094602(70%数据为测试集)
#计算决定系数
R_squared <- cor( testData$mpg,predictions)^2
print(paste("R-squared:", R_squared))  #R-squared:NA(80%数据为测试集)
                                      #R-squared:0.840224281786148(70%数据为测试集)
#可视化预测结果
plot(testData$mpg, predictions, main = "Actual vs. Predicted", xlab = "Actual", ylab = "Predicted")
abline(0, 1, col = "red")   #相较于80%数据为测试集，70%数据为测试集时，散点更趋近于红色直线
