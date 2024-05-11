#HEADER------------------------------------
#
#Author: wangcijian
#Copyright   Copyright 2024-wangcijian
#Email:3310166542@qq.com
#
#Date:2024-04-25
#
#Script Name:geodata_,manipul.R
#
#Script Description:
#1.对Doubs河进行2公里缓冲区设置，并从地图中裁剪以提取集水区和坡度的栅格值。
#2.将提取的数据与Doubs数据集中的其他环境因素合并为一个数据框，并最终将数据框转换为一个包含几何列的sf对象。
#
#SETUP ----------------------------------------------

#1.对Doubs河进行2公里缓冲区设置（UTM投影坐标系）
#下载并调用相关的包
install.packages("terra", repos="https://rspatial.r-universe.dev")   #添加terra的官方源
library(terra)
#注：terra包是用于处理地理空间数据的强大工具。它提供了一系列功能，包括读取、处理、分析和可视化地理空间数据。
install.packages("sf", repos="https://cloud.r-project.org")    #添加sf的官方源
library(sf)
#注：sf包是在R中用于处理矢量地理空间数据（如点、线、多边形）的工具包。
install.packages("qgisprocess")
library(qgisprocess)
#注：qgisprocess包是在R语言中调用QGIS处理算法的软件包。它提供了一个接口，让用户能够在R中使用QGIS的功能。

#读取文件:从QGIS上获得的doubs采样点数据、doubs河数据和下载的dem数据
doubs_point <- sf::st_read("D:/ruanjian/Rstudio/QGIS/doubs/doubs-point/doubs-point.shp")
doubs_river <- sf::st_read("D:/ruanjian/Rstudio/QGIS/doubs/doubs-river/doubs-river.shp")
doubs_dem <- terra::rast("D:/ruanjian/Rstudio/QGIS/doubs/map(3).tif")  #读取栅格数据

#数据通常以地球表面上的经纬度表示，但这种表示方式在距离和面积方面可能不准确。故需要将地理数据投影到合适的投影坐标系中。
#选择UTM投影坐标系: Zone 31N (EPSG:32631)
utm_proj <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
#将数据转换到UTM投影坐标系32631
doubs_river_utm <- st_transform(doubs_river, crs = utm_proj)

#创建Doubs River缓冲区
doubs_river_buffer <- st_buffer(doubs_river_utm, dist = 2000)
#绘制缓冲区
plot(st_geometry(doubs_river_buffer), axes = TRUE, col = "lightblue", lwd = 2, main = "Doubs River 2000m Buffer", xlab = "Longitude", ylab = "Latitude")
#添加原始的Doubs River
plot(st_geometry(doubs_river_utm), add = TRUE, col = "blue", lwd = 2)
#添加图例
legend("topright", legend = c("Doubs River", "Buffer (2000m)"), fill = c("blue", "lightblue"), bty = "n")
#添加网格线
grid()

#绘制投影坐标系下的Doubs River缓冲区区
library(ggplot2)
#创建一个空 ggplot2图层
p <- ggplot() 
#添加Doubs River缓冲区
p <- p + geom_sf(data = doubs_river_buffer, fill = "lightblue", color = "black", size = 0.5)
#设置标题和坐标轴标签
p <- p + labs(title = "Doubs River 2000m Buffer", x = "Longitude", y = "Latitude")
#显示图形
print(p)

#获得doubs_dem文件的地理坐标系CRS，便于后续转换为UTM投影坐标系
terra::crs(doubs_dem)  
#设置doubs_dem文件的UTM投影坐标系
utm_crs <- "EPSG:32631"  #将经纬度坐标转换为平面坐标
#转换坐标参考系
doubs_dem_utm <- terra::project(doubs_dem,utm_crs)  #转换到UTM,确保数据在同一投影系统中
terra::crs(doubs_dem_utm)  #获取转换后的doubs_dem_utm对象的坐标参考系
#裁剪和遮罩
doubs_dem_utm_cropped = crop(doubs_dem_utm,doubs_river_buffer)  #按缓冲区裁剪
doubs_dem_utm_masked = mask(doubs_dem_utm_cropped,doubs_river_buffer)   #遮罩
#可视化
plot(doubs_dem_utm_masked,axes =TRUE)  #可视化遮罩后的数据


#2.从地图中裁剪以提取集水区和坡度的栅格值
#①调用qgisprocess包内函数查找关于"wetness"及"sagang:sagwetnessindex"的算法
library(qgisprocess)
qgis_algorithms()
#显示特定算法的帮助
qgis_show_help("native:buffer")
#搜索包含"wetness"关键词的算法，并选取前两个结果
qgis_search_algorithms("wetness") |>
  dplyr::select(provider_title,algorithm) |>
  head(2)

#在QGIS中运行湿度指数算法，并将输出保存到临时文件
topo_total = qgisprocess::qgis_run_algorithm(
  alg = "sagang:sagwetnessindex",
  DEM = doubs_dem_utm_masked,
  SLOPE_TYPE = 1,
  SLOPE = tempfile(fileext = ".sdat"),
  AREA = tempfile(fileext = ".sdat"),
  .quiet = TRUE)

#从topo_total中选择AREA和SLOPE列
topo_select <- topo_total[c("AREA","SLOPE")] |>
#将数据展开为一维数组
  unlist() |>
#将一维数组转换为栅格对象
  rast()

names(topo_select) = c("carea","cslope")
origin(topo_select) = origin(doubs_dem_utm_masked)
topo_char = c(doubs_dem_utm_masked,topo_select)
topo_env <- terra::extract(topo_char,doubs_points_utm,ID = FALSE)


#②调用extraxt函数进行栅格值计算
library(raster)
install.packages("hydroTSM")
library(hydroTSM)   #注：水文时间序列分析

#恢复裁剪后doubs_dem_utm_masked的经纬度信息
doubs_dem_mas <- terra::project(doubs_dem_utm_masked, crs(doubs_dem))

#提取doubs_dem_mas文件的集水区,并存储结果于指定路径
watershed <- area(doubs_dem_mas)
writeRaster(watershed, filename = "D:/ruanjian/Rstudio/QGIS/doubs/watershed.tif", format = "GTiff", overwrite = TRUE)
#提取doubs_dem_mas文件的坡度，并储存结果于指定路径
slope <- terrain(doubs_dem_mas, opt = "slope")
writeRaster(slope, filename = "D:/ruanjian/Rstudio/QGIS/doubs/slope.tif", format = "GTiff", overwrite = TRUE)

#从QGIS导出的CSV文件中读取采样点数据
sample_points <- read.csv("D:/ruanjian/Rstudio/QGIS/doubs/doubs_point.csv")
#加载存储集水区和存储坡度的栅格数据
watershed_raster <- raster("D:/ruanjian/Rstudio/QGIS/doubs/watershed.tif")
slope_raster<-raster("D:/ruanjian/Rstudio/QGIS/doubs/slope.tif")
#从裁剪后的集水区和坡度栅格中提取与采样点位置对应的值，并将其存储在watershed_values和slope_values变量中
watershed_values <- raster::extract(watershed_raster, sample_points[, c("mapX", "mapY")], method = "bilinear")
slope_values <- raster::extract(slope_raster, sample_points[, c("mapX", "mapY")], method = "bilinear")

#3.将提取的栅格数据与Doubs数据集中的其他环境因素合并为一个数据框
#调用doubs数据集
library(ade4)
data("doubs")
#计算fish的丰度，并定义为fish_abundance
fish_abundance <- rowSums(doubs$fish)
#读取doubs河采样点的xy值并去除第一列
coords <- read.csv("D:/ruanjian/Rstudio/QGIS/doubs/doubs_utm.csv")
coordsxy <- coords[,-1]
#获取取doubs河采样点的经纬度
doubs_point_lt <- read.csv("D:/ruanjian/Rstudio/QGIS/doubs/doubs_point.csv")
colnames(doubs_point_lt) <- c("longitude", "latitude")
doubs_point_lt <- doubs_point_lt[, c("longitude", "latitude")]
#将目前获得的doubs河的数据整合为一个数据框
doubs_env_1 <- cbind(carea = watershed_values, cslope = slope_values,doubs$env, spe_abund = fish_abundance,doubs_point_lt,geometry=coordsxy)


#4.将数据框转换为一个包含几何列的sf对象
library(sf)
#创建sf对象
doubs_env_sf <- st_as_sf(doubs_env_1, coords = c("geometry.x", "geometry.y"), crs = st_crs(32631))
#打印sf对象
print(doubs_env_sf)