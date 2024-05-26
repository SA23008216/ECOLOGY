#HEADER------------------------------------
#
#Author: wangcijian
#Copyright   Copyright 2024-wangcijian
#Email:3310166542@qq.com
#
#Date:2024-05-26
#
#Script Name:ts_fs_ml, Timeseries EDA and ML
#
#Script Description:
#1.从网站下载Le Doubs鱼类监测数据：https://figshare.com/articles/dataset/Data_for_Contemporary_loss_of_genetic_diversity_in_wild_fish_populations_reduces_biomass_stability_over_time_/13095380
#2.聚焦于VERCah站的鱼类密度数据，选择VAI的鱼种创建时间序列对象并将其可视化。
#3.提取这个时间序列的特征，并用Tidy models包建立一个预测模型。
#
#SETUP ----------------------------------------------
# 01-针对VAI鱼种创建时间序列对象并将其可视化

#A）从网址https://figshare.com/articles/dataset/Data_for_Contemporary_loss_of_genetic_diversity_in_wild_fish_populations_reduces_biomass_stability_over_time_/13095380下载监测数据，并通过read.table()函数导入到R中。
#使用read.table函数导入数据
VAI <- read.table("D:/ruanjian/Rstudio/ECOLOGY/Prunier_et_al._RawBiomassData.txt", header = TRUE)
#查看数据的结构和内容
str(VAI)  #查看数据框架的结构
head(VAI)  #查看前几行数据

#使用dplyr包对数据进行处理
data_clean <- VAI |>
  dplyr::select(-YEAR) |>  #去除名为YEAR的列
  drop_na() |>   #删除包含缺失值的观测行
  distinct()  #识别和删除重复的数据行
#查看处理后的数据
head(data_clean)

# B）在数据中筛选出站点为VERCah且物种为VAI的密度数据，并创建一个时间序列对象。
#检查站点的唯一值和频数统计,确保数据中的站点信息没有重复或错误
unique_stations <- unique(data_clean$STATION)
table_stations <- table(data_clean$STATION)

#检查物种的唯一值和频数统计，确保数据的准确性和完整性
unique_species <- unique(data_clean$SP)
table_species <- table(data_clean$SP)

#筛选出站点为"VERCah"且物种为"VAI"的数据
mydata <- subset(data_clean, STATION == "VERCah" & SP == "VAI")

# C) 可视化工具对时间序列对象进行可视化.

#①timetk包的plot_time_series函数可视化
#使用timetk()创建时间序列
install.packages("timetk")
library(timetk)   #timetk包用于时间序列数据处理和分析
mydata <- data_clean |>
  subset(STATION=="VERCah" & SP == "VAI") 
#将mydata转换为tibble对象
datatk_ts <- mydata |>    
  tk_tbl() |>      #将数据框转换为tibble格式，以便后续的数据处理和分析
  select(-1) |>    #选择除第一列之外的所有列
  rename(date = DATE) |>  #更改列名
  relocate(date, .before = STATION) |>   #重新排列列的顺序
  pivot_longer(               #将BIOMASS列和DENSITY列转换为长格式
    cols = c("BIOMASS", "DENSITY"))
#绘制时间序列图形
  datatk_ts |>    
  group_by(name) |>   #分组操作，按照 name 列的值进行分组
  summarise_by_time(  #对数据按年进行汇总
    date, 
    .by = "year",
    value = first(value)) |>  #对value列取每年的第一个观测值
  pad_by_time(date, .by = "year") |>  #填充数据，确保每一年都有数据点
  plot_time_series(date, value,    #制时间序列图形
                   .facet_ncol = 2,
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "VAI(VERCah station) of Le Doubs river"
  )


#②TSrepe包对数据降维并可视化
install.packages("TSrepr")
library(TSrepr)    #TSrepr包是用于时间序列数据表示和降维的R语言包
#ts()创建时间序列
biom_ts <- ts(mydata[,-c(1:6)],   #选择第7列（密度）列作为时间序列数据
                 start = c(1994), # 设置时间序列的起始年份为1994
                 frequency = 1)  # 设置时间序列的频率为1，表示数据按年份观测
#通过autoplot()函数对biom_ts进行绘图，并添加标题和坐标轴标签，生成图形p1
p1 <- autoplot(biom_ts) +
    ggtitle("VAI(VERCah station) of Le Doubs river") +
    ylab("Changes") + xlab("Year")
#对mydata$BIOMASS进行离散小波变换  
data_dwt <- repr_dwt(mydata$BIOMASS, level = 1) 
#转换为时间序列对象
data_dwt_ts <- ts(data_dwt,
                    start= c(1994,1),
                    frequency =1)
#生成图形p2  
p2 <- autoplot(data_dwt_ts) +
    ggtitle("VAI(VERCah station) of Le Doubs river") +
    ylab("Changes") + xlab("Year")
#patchwork包，可以用来组合多个图形。  
library(patchwork)
p1+p2  #使用patchwork包中的运算符+将图形p1和p2组合在一起，以便比较两个图形


#02-提取时间序列的特征，并用Tidy models包建立一个预测模型

#A）时间序列数据的探索性数据分析

#①缺失值分析
#安装依赖包
install.packages("DataExplorer")
install.packages("ggthemes")
#加载所需包
library(DataExplorer)
library(ggthemes)
#对时间序列数据进行按年份的汇总，然后填充确保每年都有数据
datatk_ts1 <- 
datatk_ts |>    
  group_by(name) |>   #分组操作，按照 name 列的值进行分组
  summarise_by_time(  #对数据按年进行汇总
    date, 
    .by = "year",
    value = first(value)) |>  #对value列取每年的第一个观测值
  pad_by_time(date, .by = "year")
#显示数据集中最后几个观测值
tail(datatk_ts1)
#对数据集datatk_ts2进行缺失值可视化
datatk_ts1 |>
  plot_missing(
    ggtheme = theme_calc(), 
    title = "Percent Missing Values by Column"
  )

#②数据集进行缺失值可视化和插补
datatk_ts2 <- datatk_ts1 |>
  group_by(name) |>   #对数据按照name列进行分组
  pad_by_time(date, .by = "year") |>   #对时间序列数据进行填充
  mutate_at(vars(value), .funs = ts_impute_vec, period = 1) #对value列中的缺失值进行插补，设置插补周期为1
#对填充后的时间序列数据进行可视化
datatk_ts2 |>
  plot_time_series(date, value, 
                   .facet_ncol = 2, 
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "VAI(VERCah station) of Le Doubs river"
  ) 
#显示datatk_ts2数据的最后几行，用于查看填充后的数据
tail(datatk_ts2)

#③异常值分析
#时间序列数据集datatk_ts2中查找异常值
datatk_ts2 |>
  group_by(name) |>
  plot_anomaly_diagnostics(
    .date = date,
    .value = value,  #数值列的名称
    .facet_ncol = 2,  #图表中每行的面板数量
    .interactive=FALSE,  #禁用交互式功能
    .title = "Anomaly Diagnostics",  #图表的标题
    .anom_color ="#FF0000",   #异常值的颜色
    .max_anomalies = 0.07,   #所允许的最大异常值比例
    .alpha = 0.05  #显著性水平
  )

#④自相关函数（ACF）图
datatk_ts2 |>
  group_by(name) |>  #将数据按照名称（或者其他标识符）进行分组，以便后续针对每个分组生成自相关函数图
  plot_acf_diagnostics(  #绘制自相关函数图
    date, value,     #date表示时间变量的名称，用于X轴;value表示观测值的变量名称，用于Y轴         
    .lags = "5 years",   #自相关函数图中的滞后（lags）数量，设置为5年,自相关函数将计算出1到5年的滞后
    .interactive = FALSE  #不生成交互式图表，而是生成静态图
  )

#⑤交叉相关函数（CCF）图
#加载所需的包
install.packages("tidyquant")
install.packages("gt")
library(tidyquant)
library(gt) 
#创建datatk_ts3数据集
datatk_ts3 <- datatk_ts2 |>
  #将数据从长格式转换为宽格式，以便每个变量有一个列
  pivot_wider(names_from = name, values_from = value) |>
  #按年汇总数据
  summarise_by_time(
    .date_var = date, 
    .by = "year", 
    across(BIOMASS:DENSITY, .fns = mean)
  )
#生成交叉相关函数（CCF）图并可视化
datatk_ts3 |> 
  #使用 tk_acf_diagnostics 函数生成 CCF 图
  tk_acf_diagnostics(
    date,                   #时间变量的名称
    BIOMASS,                #X轴变量的名称
    .ccf_vars = DENSITY,    #Y轴变量的名称
  ) |> 
  #选择所需的列
  select(lag, CCF_DENSITY) |> 
  #仅保留前10个滞后值
  slice(1:10) |> 
  #使用 gt 函数将数据转换为漂亮的表格格式
  gt() |> 
  #对表格进行着色以突出显示CCF_DENSITY列
  data_color(
    columns = c("CCF_DENSITY"),
    fn = scales::col_numeric(
      palette = c("#ffffff", "#f2fbd2", "#FA6047", "#F25555", "#FA1B1B"), 
      domain = NULL
    )
)
#生成CCF图
datatk_ts3 |> 
  plot_acf_diagnostics(
    date,                   #时间变量的名称
    BIOMASS,                #X轴变量的名称
    .ccf_vars = DENSITY,    #Y轴变量的名称
    .show_ccf_vars_only = TRUE,  #仅显示 CCF 变量
    .interactive=FALSE,     #不生成交互式图表
    .line_color = "black",  #线条颜色
    .point_color = palette_light()[[2]],  #点的颜色
    .line_size = 1.5,       #线条大小
    .title = "Cross Correlation of BIOMASS and DENSITY"  #图表标题
  ) 

#B) 提取时间序列特征值
#导入所需的库
install.packages("tidymodels")
install.packages("modeltime")
library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(lubridate)
#从原始数据中选择指定站点和物种的数据
mydata <- data_clean |>
  subset(STATION == "VERCah" & SP == "VAI")
#创建生物量时间序列
biomtk_ts <- mydata |>
  tk_tbl() |>                           #将数据转换为 tibble 格式
  mutate(DATE = as.Date(DATE)) |>
  select(DATE, BIOMASS)                #选择日期和生物量两列数据
#检查时间序列的规律性
biomtk_ts |>
  tk_summary_diagnostics(.date_var = DATE)

# ①基于日历的特征
#对生物量进行对数变换和标准化，并添加基于日历的特征
biomtk_ts_features_C <- biomtk_ts |>
  mutate(BIOMASS = log1p(x = BIOMASS)) |>           #对生物量进行对数变换
  mutate(BIOMASS = standardize_vec(BIOMASS)) |>     #对生物量进行标准化
  tk_augment_timeseries_signature(.date_var = DATE) |>  #添加基于日历的特征
  glimpse()                                          #显示新特征信息
biomtk_ts_features_C                                  #显示新特征

#在新特征上进行线性回归
timetk::plot_time_series_regression(.date_var = DATE,
                                    .data = biomtk_ts_features_C,
                                    .formula = BIOMASS ~ as.numeric(DATE) + index.num
                                    + year + half + quarter + month + month.lbl,
                                    .show_summary = TRUE)

#②傅立叶项特征
#对生物量进行对数变换和标准化，并添加傅立叶项特征
biomtk_ts_features_F <- biomtk_ts |>
  mutate(BIOMASS = log1p(x = BIOMASS)) |>           #对生物量进行对数变换
  mutate(BIOMASS = standardize_vec(BIOMASS)) |>     #对生物量进行标准化
  tk_augment_fourier(.date_var = DATE, .periods = 5, .K = 1)  #添加傅立叶项特征
biomtk_ts_features_F                                  #显示新特征

#在新特征上进行线性回归
plot_time_series_regression(.date_var = DATE,
                            .data = biomtk_ts_features_F,
                            .formula = BIOMASS ~ as.numeric(DATE) + 
                              DATE_sin5_K1 + DATE_cos5_K1,
                            .show_summary = TRUE)


#③ Lag features（滞后特征）
#对生物量进行对数变换和标准化，并添加滞后特征
biomtk_ts_features_L <- biomtk_ts |>
  mutate(BIOMASS = log1p(x = BIOMASS)) |>           #对生物量进行对数变换
  mutate(BIOMASS = standardize_vec(BIOMASS)) |>     #对生物量进行标准化
  tk_augment_lags(.value = BIOMASS, .lags = c(4, 7))  #添加滞后特征
biomtk_ts_features_L                                  #显示新特征

#在新特征上进行线性回归
plot_time_series_regression(.date_var = DATE,
                            .data = biomtk_ts_features_L,
                            .formula = BIOMASS ~ as.numeric(DATE) + 
                              BIOMASS_lag4 + BIOMASS_lag7,
                            .show_summary = TRUE)

#C)数据预处理和特征工程，建模
# 导入必要的包
library(tidyverse)  #数据处理
library(timetk)     #时间序列处理

#对BIOMASS进行对数转换和标准化，并添加滞后和移动窗口统计特征
biomtk_ts_features_M <- biomtk_ts %>%
  mutate(BIOMASS = log1p(BIOMASS)) %>%                    #对BIOMASS进行对数转换
  mutate(BIOMASS = scale(BIOMASS)) %>%                    #对BIOMASS进行标准化
  tk_augment_lags(.value = BIOMASS, .lags = c(4, 7)) %>%  #添加BIOMASS的4期和7期滞后特征
  tk_augment_slidify(.value   = contains("BIOMASS"),      #添加移动窗口统计特征
                     .f       = ~ mean(.x, na.rm = TRUE), #统计窗口内的平均值
                     .period  = c(3, 6),                  #窗口的长度为3和6
                     .partial = TRUE,                     #允许不完整的窗口
                     .align   = "center")                 #窗口对齐方式为中心对齐
#显示处理后的数据
biomtk_ts_features_M
#进行线性回归建模
plot_time_series_regression(.date_var = DATE,                    #指定日期变量
                            .data = biomtk_ts_features_M,       #数据
                            .formula = BIOMASS ~ as.numeric(DATE) + 
                              BIOMASS_lag4 + BIOMASS_lag7 +     #回归模型的公式，包括日期和滞后特征
                              BIOMASS_roll_3 + BIOMASS_roll_6,  #以及移动窗口统计特征
                            .show_summary = TRUE)               #显示回归模型摘要信息


#D）建立预测模型
# ①加载必要的库
library(tidyverse)  #数据处理和可视化
library(timetk)     #时间序列处理
library(tidymodels) #机器学习建模
library(modeltime)  #时间序列建模
library(tidyquant)  #金融数据分析
#加载数据并选择相关列
mydata <- data_clean |>
  subset(STATION == "VERCah" & SP == "VAI")
#将数据转换为tibble格式，并选择索引、日期和生物量列
biomtk_ts <- mydata |> 
  tk_tbl() |> 
  select(index, DATE, BIOMASS)
#绘制时间序列数据
ggplot(biomtk_ts, aes(x = DATE, y = BIOMASS)) +
  geom_line() +
  ggtitle("VAI(VERCah station) of Le Doubs river")

# ②划分训练集和测试集，并创建特征
#计算数据总行数
n_rows <- nrow(biomtk_ts)
#计算训练集行数
train_rows <- round(0.8 * n_rows)
#将数据分割为训练集和测试集
train_data <- biomtk_ts |>
  slice(1:train_rows)
test_data <- biomtk_ts |>
  slice((train_rows):n_rows)
#绘制训练集和测试集
ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "训练集"), 
            linewidth = 1) +
  geom_line(data = test_data, 
            aes(x = DATE, y = BIOMASS, color = "测试集"), 
            linewidth = 1) +
  scale_color_manual(values = c("训练集" = "blue", 
                                "测试集" = "red")) +
  labs(title = "训练集和测试集", 
       x = "日期", y = "生物量") +
  theme_minimal()

#使用recipes创建特征
recipe_spec_final <- recipe(BIOMASS ~ ., train_data) |>
#将缺失值替换为指定值
step_mutate_at(index, fn = ~if_else(is.na(.), -12345, . )) |>
#创建时间序列特征签名
step_timeseries_signature(DATE) |>
#删除日期列
step_rm(DATE) |>
#删除零方差特征
step_zv(all_predictors()) |>
#对所有名义变量进行独热编码
step_dummy(all_nominal_predictors(), one_hot = TRUE)

#③训练和评估模型
#训练提升树模型
bt <- workflow() |>
  add_model(
    boost_tree("regression") |> set_engine("xgboost")
  ) |>
  add_recipe(recipe_spec_final) |>
  fit(train_data)
#评估模型性能
bt_test <- bt |> 
  predict(test_data) |>
  bind_cols(test_data) 
#绘制模型预测
pbt <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "训练集"), 
            linewidth = 1) +
  geom_line(data = bt_test, 
            aes(x = DATE, y = BIOMASS, color = "测试集"), 
            linewidth = 1) +
  geom_line(data = bt_test, 
            aes(x = DATE, y = .pred, color = "预测值"), 
            linewidth = 1) +
  scale_color_manual(values = c("训练集" = "blue", 
                                "测试集" = "red",
                                "预测值" ="black")) +
  labs(title = "提升树模型 - 训练集/测试集和预测", 
       x = "日期", y = "生物量") +
  theme_minimal()
#计算预测误差
bt_test |>
  metrics(BIOMASS, .pred)


#E)训练随机森林模型
install.packages("ranger")
library(ranger)
#训练提升树模型
bt <- workflow() %>%
  add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
  add_recipe(recipe_spec_final) %>%
  fit(train_data)
#训练随机森林模型
rf <- workflow() %>%
  add_model(rand_forest("regression") %>% set_engine("ranger")) %>%
  add_recipe(recipe_spec_final) %>%
  fit(train_data)
#创建Modeltime表
model_tbl <- modeltime_table(bt, rf)
#校准Modeltime表
calibrated_tbl <- model_tbl %>%
  modeltime_calibrate(new_data = test_data)
#模型评估
evaluation <- calibrated_tbl %>%
  modeltime_accuracy(test_data) %>%
  arrange(rmse)
#绘制预测结果
forecast_plot <- calibrated_tbl %>%
  modeltime_forecast(
    new_data = test_data,
    actual_data = biomtk_ts,
    keep_data = TRUE
  ) %>%
  plot_modeltime_forecast(
    .facet_ncol = 2,
    .conf_interval_show = FALSE,
    .interactive = TRUE
  )