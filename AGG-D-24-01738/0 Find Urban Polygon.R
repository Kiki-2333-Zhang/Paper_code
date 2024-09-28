library(terra)
library(sf)
library(readstata13)

##### Cannot project directly to EPSG4326. Too Large

##### Find City Needed ##### 
china_city <- read_sf("F:/OneDrive/graduate/data/China/TianDiTu/GS（2024）0650/市.shp")
names(china_city)[1:4] <- c("city", "cityN", "province", "provinceN")
china_city <- china_city[china_city$city != "三沙市" & china_city$province != "台湾省",]

data.psu <- read.dta13("F:/OneDrive/graduate/data/CHARLS/2011 Wave1/data 20130312/psu.dta")
city <- as.data.frame(unique(data.psu$city))
names(city) <- "name"
city$no <- seq(1, nrow(city))
city$use <- city$name
city[city$name == "哈尔滨","use"] <- "哈尔滨市"
rm(data.psu)

city[!(city$use %in% china_city$city),"use"]
city <- city[city$use != "巢湖市",]
city[city$use == "北京", "use"] <- "北京市"
city[city$use == "天津", "use"] <- "天津市"
city[city$use == "海东地区", "use"] <- "海东市"
city[city$use == "襄樊市", "use"] <- "襄阳市"

Use <- merge(china_city[,c(1,5)], city, by.x = "city", by.y = "use", all.y = TRUE)
st_geometry(Use) <- Use$geometry
rm(china_city, city)

##### Generate City Polygon #####
files <- list.files("F:\\Data\\CLUD-U\\Original\\", pattern = "Urban")
files <- files[3:5]

raster2011 <- rast(paste0("F:\\Data\\CLUD-U\\Original\\", files[1]))
raster2015 <- rast(paste0("F:\\Data\\CLUD-U\\Original\\", files[2]))
raster2018 <- rast(paste0("F:\\Data\\CLUD-U\\Original\\", files[3]))
crs_2011 <- crs(raster2011)
crs_2018 <- crs(raster2018) 

urban2011 <- st_drop_geometry(Use)
rural2011 <- st_drop_geometry(Use)
urban2015 <- st_drop_geometry(Use)
rural2015 <- st_drop_geometry(Use)
urban2018 <- st_drop_geometry(Use)
rural2018 <- st_drop_geometry(Use)

for (i in 1:125){ 
  ## Turn crs(polygon) from wgs84 to albers
  poly <- vect(st_geometry(Use[i,]))
  poly_2011 <- project(poly, crs_2011)
  poly_2018 <- project(poly, crs_2018)
  ## Use polygon to extract urban/rural boundary polygon
  city_2011 <- crop(raster2011, poly_2011, mask = TRUE)
  city_2015 <- crop(raster2015, poly_2011, mask = TRUE)
  city_2018 <- crop(raster2018, poly_2018, mask = TRUE)
  urban_2011 <- as.polygons(city_2011, values = 1)
  urban_2011 <- aggregate(urban_2011)
  rural_2011 <- erase(poly_2011, urban_2011)
  urban_2015 <- as.polygons(city_2015, values = 1)
  urban_2015 <- aggregate(urban_2015)
  rural_2015 <- erase(poly_2011, urban_2015)
  urban_2018 <- as.polygons(city_2018, values = 1)
  urban_2018 <- aggregate(urban_2018)
  rural_2018 <- erase(poly_2018, urban_2018)
  ## Turn crs urban/rural polygon from albers to wgs 84
  urban_2011 <- project(urban_2011, "epsg:4326")
  rural_2011 <- project(rural_2011, "epsg:4326")
  urban_2015 <- project(urban_2015, "epsg:4326")
  rural_2015 <- project(rural_2015, "epsg:4326")
  urban_2018 <- project(urban_2018, "epsg:4326")
  rural_2018 <- project(rural_2018, "epsg:4326")
  ## Save to exact place
  urban2011[i,4] <- st_as_sf(urban_2011)
  rural2011[i,4] <- st_as_sf(rural_2011)
  urban2015[i,4] <- st_as_sf(urban_2015)
  rural2015[i,4] <- st_as_sf(rural_2015)
  urban2018[i,4] <- st_as_sf(urban_2018)
  rural2018[i,4] <- st_as_sf(rural_2018)
  print(i)
}
rm(list = grep("_|raster|poly", ls(), value = TRUE))

rural2011 <- st_as_sf(rural2011, sf_column_name = "geometry")
st_crs(rural2011) <- 4326 
rural2015 <- st_as_sf(rural2015, sf_column_name = "geometry")
st_crs(rural2015) <- 4326
rural2018 <- st_as_sf(rural2018, sf_column_name = "geometry")
st_crs(rural2018) <- 4326
urban2011 <- st_as_sf(urban2011, sf_column_name = "geometry")
st_crs(urban2011) <- 4326
urban2015 <- st_as_sf(urban2015, sf_column_name = "geometry")
st_crs(urban2015) <- 4326
urban2018 <- st_as_sf(urban2018, sf_column_name = "geometry")
st_crs(urban2018) <- 4326

save(All = Use, urban2011, rural2011, urban2015, rural2015, urban2018, rural2018, file = "CHARLS_map.RData")

