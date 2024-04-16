#HEADER------------------------------------
#
#Author: wangcijian
#Copyright   Copyright 2024-wangcijian
#Email:3310166542@qq.com
#
#Date:2024-04-14
#
#Script Name:Homework-04-r_database.R
#将ade4包中内置的Doubs数据集上传到PostgreSQL或SQLite中
#Script Description:
#1.了解并成功下载reticulate和rdataretriever包
#2.了解PostgreSQL和SQLite两个数据库
#3.将ade4包中内置的Doubs数据集上传到PostgreSQL或SQLite中
#
#SETUP ----------------------------------------------

#1.下载reticulate和rdataretriever包

#安装R中的reticulate包，此包提供了R与Python的接口，便于在R中调用Python的功能和库
install.packages('reticulate')   
#安装Miniconda，在系统上创建一个独立的Python环境
reticulate::install_miniconda(force = TRUE) 
#在Python环境中安装名为retriever的Python包
reticulate::py_install('retriever') 
#安装rdataretriever包,此包是在R中运行retriever的工具
install.packages('rdataretriever') 
#调用rdataretriever包中的get_updates()函数，用于更新rdataretriever包中的数据集信息
rdataretriever::get_updates() 


#2.将ade4包中内置的Doubs数据集上传到SQLite中

#下载并调用后续步骤所需要的包
install.packages("ade4")
devtools::install_github("r-dbi/RSQLite")   #从GitHub上获取并安装最新的RSQLite包，安装到R环境中
library(rdataretriever)
library(ade4)
library(DBI)  #DBI是一个R语言的包，用于提供统一的接口，使其能够在R中连接和操作不同类型的数据库，如SQLite、MySQL、PostgreSQL等
library(RSQLite)

#获取ade4包内置的doubs数据集
data("doubs")
#查看doubs数据集的结构
str(doubs)
#指定SQLite数据库文件的路径和名称
db_file <- "D:/ruanjian/liufanglin/RSQLite_2.3.6/RSQLite/RSQLite-r_database/RSQLite-r_database.sqlite"
#连接到SQLite数据库
con <- dbConnect(RSQLite::SQLite(), dbname = db_file)
#创建SQLite连接，con是一个SQLite数据库连接对象
con <- dbConnect(RSQLite::SQLite(), "D:/ruanjian/liufanglin/RSQLite_2.3.6/RSQLite/RSQLite-r_database/RSQLite-r_database.sqlite")

#使用dbWriteTable()函数将doubs数据集中的数据写入到 SQLite 数据库中的不同表中
dbWriteTable(con, "env", doubs[[1]], append = FALSE)
dbWriteTable(con, "fish", doubs[[2]], append = FALSE)
dbWriteTable(con, "xy", doubs[[3]], append = FALSE)
dbWriteTable(con, "species", doubs[[4]], append = FALSE)


#3.将ade4包中内置的Doubs数据集上传到PostgreSQL中
#下载并调用后续步骤所需要的包
install.packages("RPostgreSQL")
install.packages("ade4")
library(rdataretriever)
library(ade4)
library(RPostgreSQL) 
library(DBI)   #DBI是一个R语言的包，用于提供统一的接口，使其能够在R中连接和操作不同类型的数据库，如SQLite、MySQL、PostgreSQL等

#获取ade4包内置的doubs数据集
data("doubs")
#查看doubs数据集的结构
str(doubs)
#连接到PostgreSQL数据库（须下载PostgreSQL软件）
con <- dbConnect(PostgreSQL(), dbname = "doubs", host = "localhost", port = "5432", user = "20240414", password = "20240414")
#使用dbWriteTable()函数将doubs数据集中的数据写入到PostgreSQL数据库中的不同表中
dbWriteTable(con, "env", doubs[[1]], append = FALSE)
dbWriteTable(con, "fish", doubs[[2]], append = FALSE)
dbWriteTable(con, "xy", doubs[[3]], append = FALSE)
dbWriteTable(con, "species", doubs[[4]], append = FALSE)


#4.若想将doubs数据集内数据合为一个大表格，可采用下述代码
#从数据库中读取每个独立表的数据
library(data.table)
env <- as.data.table(dbReadTable(con, "env"))
fish <- as.data.table(dbReadTable(con, "fish"))
xy <- as.data.table(dbReadTable(con, "xy"))
species <- as.data.table(dbReadTable(con, "species"))
#将每个独立表作为大表的变量
big_doubs <- data.table(env = env, fish = fish, xy = xy, species = species)   #在此，由于species只有27行，数据将会循环补齐，须注意。
#查看大表的结构
str(big_doubs)
#将大表写入数据库中
dbWriteTable(con, "big_doubs", big_doubs, append = FALSE)

#关闭连接
dbDisconnect(con)
