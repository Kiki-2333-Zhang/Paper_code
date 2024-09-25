library(sf)
library(terra)
library(exactextractr)

##### Find temperature data ######
## Sort needed days
file2015 <- list.files("E:/EuropeanAsia2015/")
file2016 <- list.files("E:/EuropeanAsia2016/")
file2017 <- list.files("E:/EuropeanAsia2017/")
file2018 <- list.files("E:/EuropeanAsia2018/")
keep <- seq.Date(as.Date("2018-06-01"), as.Date("2018-08-31"), by = "days")
keep <- keep - as.Date("2017-12-31")
keep <- as.character(keep)
keep <- paste0(keep, ".tif")
# keep <- format(keep, "%m%d")
keep <- paste0(keep, collapse = "|")
file2015 <- grep(keep, file2015, value = TRUE)
file2016 <- grep(keep, file2016, value = TRUE) # Should be 366 days, but the data is 365.
file2017 <- grep(keep, file2017, value = TRUE)
file2018 <- grep(keep, file2018, value = TRUE)
rm(keep)

calculate <- function(df){
  max <- max(df$value, na.rm = TRUE)
  mean <- mean(df$value, na.rm = TRUE)
  min <- min(df$value, na.rm = TRUE)
  return(c(max, mean, min))
}


## Load files
load("F:/OneDrive/graduate/my_research/In_progress/heatwave/mental health/file/Version 9/0 Find Urban Polygon/CHARLS_map.RData")
rm(rural2011, urban2011)

df2015 <- data.frame()
for (i in 1:length(file2015)){
  temp <- st_drop_geometry(Use)
  year <- substr(file2015[i],24,27)
  date <- substr(file2015[i],33,35)
  date <- as.Date(as.numeric(date), origin = "2015-01-01") - 1
  date <- format(date, "%Y%m%d")
  type <- substr(file2015[i],29,31)
  raster <- rast(paste0("E:/EuropeanAsia2015/", file2015[i]))
  stats <- exact_extract(raster, rural2015)
  stats <- lapply(stats, calculate)
  stats <- do.call(rbind.data.frame, stats)
  names(stats) <- c("max", "mean", "min")
  temp1 <- cbind(temp, stats)
  temp1$loc <- "rural"
  stats <- exact_extract(raster, urban2015)
  stats <- lapply(stats, calculate)
  stats <- do.call(rbind.data.frame, stats)
  names(stats) <- c("max", "mean", "min")
  temp2 <- cbind(temp, stats)
  temp2$loc <- "urban"
  stats <- exact_extract(raster, Use)
  stats <- lapply(stats, calculate)
  stats <- do.call(rbind.data.frame, stats)
  names(stats) <- c("max", "mean", "min")
  temp3 <- cbind(temp, stats)
  temp3$loc <- "all"
  temp <- rbind(temp1, temp2, temp3)
  temp$date <- date
  temp$type <- type
  df2015 <- rbind(df2015, temp)
  print(i)
}

df2016 <- data.frame()
for (i in 1:length(file2016)){
  temp <- st_drop_geometry(Use)
  year <- substr(file2016[i],24,27)
  date <- substr(file2016[i],33,35)
  date <- as.Date(as.numeric(date), origin = "2016-01-01")  # 366 Day!
  date <- format(date, "%Y%m%d")
  type <- substr(file2016[i],29,31)
  raster <- rast(paste0("E:/EuropeanAsia2016/", file2016[i]))
  stats <- exact_extract(raster, rural2015)
  stats <- lapply(stats, calculate)
  stats <- do.call(rbind.data.frame, stats)
  names(stats) <- c("max", "mean", "min")
  temp1 <- cbind(temp, stats)
  temp1$loc <- "rural"
  stats <- exact_extract(raster, urban2015)
  stats <- lapply(stats, calculate)
  stats <- do.call(rbind.data.frame, stats)
  names(stats) <- c("max", "mean", "min")
  temp2 <- cbind(temp, stats)
  temp2$loc <- "urban"
  stats <- exact_extract(raster, Use)
  stats <- lapply(stats, calculate)
  stats <- do.call(rbind.data.frame, stats)
  names(stats) <- c("max", "mean", "min")
  temp3 <- cbind(temp, stats)
  temp3$loc <- "all"
  temp <- rbind(temp1, temp2, temp3)
  temp$date <- date
  temp$type <- type
  df2016 <- rbind(df2016, temp)
  print(i)
}

df2017 <- data.frame()
for (i in 1:length(file2017)){
  temp <- st_drop_geometry(Use)
  year <- substr(file2017[i],24,27)
  date <- substr(file2017[i],33,35)
  date <- as.Date(as.numeric(date), origin = "2017-01-01") - 1
  date <- format(date, "%Y%m%d")
  type <- substr(file2017[i],29,31)
  raster <- rast(paste0("E:/EuropeanAsia2017/", file2017[i]))
  stats <- exact_extract(raster, rural2018)
  stats <- lapply(stats, calculate)
  stats <- do.call(rbind.data.frame, stats)
  names(stats) <- c("max", "mean", "min")
  temp1 <- cbind(temp, stats)
  temp1$loc <- "rural"
  stats <- exact_extract(raster, urban2018)
  stats <- lapply(stats, calculate)
  stats <- do.call(rbind.data.frame, stats)
  names(stats) <- c("max", "mean", "min")
  temp2 <- cbind(temp, stats)
  temp2$loc <- "urban"
  stats <- exact_extract(raster, Use)
  stats <- lapply(stats, calculate)
  stats <- do.call(rbind.data.frame, stats)
  names(stats) <- c("max", "mean", "min")
  temp3 <- cbind(temp, stats)
  temp3$loc <- "all"
  temp <- rbind(temp1, temp2, temp3)
  temp$date <- date
  temp$type <- type
  df2017 <- rbind(df2017, temp)
  print(i)
}

df2018 <- data.frame()
for (i in 1:length(file2018)){
  temp <- st_drop_geometry(Use)
  year <- substr(file2018[i],24,27)
  date <- substr(file2018[i],33,35)
  date <- as.Date(as.numeric(date), origin = "2018-01-01") - 1
  date <- format(date, "%Y%m%d")
  type <- substr(file2018[i],29,31)
  raster <- rast(paste0("E:/EuropeanAsia2018/", file2018[i]))
  stats <- exact_extract(raster, rural2018)
  stats <- lapply(stats, calculate)
  stats <- do.call(rbind.data.frame, stats)
  names(stats) <- c("max", "mean", "min")
  temp1 <- cbind(temp, stats)
  temp1$loc <- "rural"
  stats <- exact_extract(raster, urban2018)
  stats <- lapply(stats, calculate)
  stats <- do.call(rbind.data.frame, stats)
  names(stats) <- c("max", "mean", "min")
  temp2 <- cbind(temp, stats)
  temp2$loc <- "urban"
  stats <- exact_extract(raster, Use)
  stats <- lapply(stats, calculate)
  stats <- do.call(rbind.data.frame, stats)
  names(stats) <- c("max", "mean", "min")
  temp3 <- cbind(temp, stats)
  temp3$loc <- "all"
  temp <- rbind(temp1, temp2, temp3)
  temp$date <- date
  temp$type <- type
  df2018 <- rbind(df2018, temp)
  print(i)
}

rm(list = grep("urban|rural|temp|stats|file|type|year|i|raster|date|calculate", ls(), value = TRUE))

df2015$year <- substr(df2015$date, 1, 4)
df2015$month <- substr(df2015$date, 5,6)
df2015$day <- substr(df2015$date,7,8)
df2016$year <- substr(df2016$date, 1, 4)
df2016$month <- substr(df2016$date, 5,6)
df2016$day <- substr(df2016$date,7,8)
df2017$year <- substr(df2017$date, 1, 4)
df2017$month <- substr(df2017$date, 5,6)
df2017$day <- substr(df2017$date,7,8)
df2018$year <- substr(df2018$date, 1, 4)
df2018$month <- substr(df2018$date, 5,6)
df2018$day <- substr(df2018$date,7,8)
df2015$max <- df2015$max / 10
df2015$mean <- df2015$mean / 10
df2015$min <- df2015$min / 10
df2016$max <- df2016$max / 10
df2016$mean <- df2016$mean / 10
df2016$min <- df2016$min / 10
df2017$max <- df2017$max / 10
df2017$mean <- df2017$mean / 10
df2017$min <- df2017$min / 10
df2018$max <- df2018$max / 10
df2018$mean <- df2018$mean / 10
df2018$min <- df2018$min / 10


names(df2015)
data <- aggregate(cbind(max, mean, min) ~ month + name + type + loc, df2015, mean)
data$year <- 2015
temp <- aggregate(cbind(max, mean, min) ~ month + name + type + loc, df2016, mean)
temp$year <- 2016
data <- rbind(data, temp)
temp <- aggregate(cbind(max, mean, min) ~ month + name + type + loc, df2017, mean)
temp$year <- 2017
data <- rbind(data, temp)
temp <- aggregate(cbind(max, mean, min) ~ month + name + type + loc, df2018, mean)
temp$year <- 2018
data <- rbind(data, temp)
rm(temp)

data[data$name == "广州市" & data$loc != "all",]

names(data)
m.max <- lm(mean ~ month + loc + year + name, data[data$type == "MAX" & data$loc != "all",])
m.min <- lm(mean ~ month + loc + year + name, data[data$type == "MIN" & data$loc != "all",])
summary(m.max)
summary(m.min)
rm(m.max, m.min)
save(data, df2015, df2016, df2017, df2018, Use, file = "Summertime Temp.RData")

old <- rbind(df2015, df2016, df2017)
rm(df2015, df2016, df2017, data)


##### Calculate Heat Index for 2018 #####
### Calculate threshold, 90, 95, 98, quantile of 2015, 2016, 2017; 30, 35, 37 for Day;
old <- merge(aggregate(mean ~ type + month + name + loc, old, max), 
             aggregate(mean ~ type + month + name + loc, old, min), 
             by = c("type", "month", "name", "loc"))
names(old)
names(old)[5:6] <- c("max", "min")
old <- old[old$loc != "all",]
tt <- as.data.frame(t(apply(old[,5:6], 1, quantile, probs = c(90, 95, 98)/100)))
names(tt) <- c("T_90", "T_95", "T_98")
old <- cbind(old, tt)
rm(tt)


### Tidy Daily temperature
names(df2018)
names(old)
df2018 <- merge(df2018, old[,c(1:4, 7:9)], by = c("type", "month", "name", "loc"))
data <- df2018
rm(df2018)
data <- subset(data, select = -c(max, min))
data <- data[order(data$name, data$loc, data$type, data$month, data$date),]

data$exceed_25 <- with(data, mean >= 25) 
data$exceed_35 <- with(data, mean >= 35) 
data$exceed_37 <- with(data, mean >= 37) 
data$exceed_40 <- with(data, mean >= 40) 
data$exceed_t90 <- with(data, mean >= T_90) 
data$exceed_t95 <- with(data, mean >= T_95) 
data$exceed_t98 <- with(data, mean >= T_98) 


### Generate Heatwave
library(stringr)
log2seq <- function(log){  # Transform several logical value to a binary string
  log <- as.integer(log)
  log <- as.character(log)
  log <- toString(log)
  seq <- gsub(", ", "", log)
  return(seq)
}

data.month <- aggregate(cbind(exceed_25, exceed_35, exceed_37, exceed_40, exceed_t90, exceed_t95, exceed_t98)
                        ~ name + loc + month + type, data, log2seq)

# Days are defines as number of days whose daily Max exceed threshold.
# Dura are defines as times that daily MAX exceed threshold for 3 consecutive days.
data.month$Days_35 <- str_count(data.month$exceed_35, "1")
data.month$Days_37 <- str_count(data.month$exceed_37, "1")
data.month$Days_40 <- str_count(data.month$exceed_40, "1")
data.month$Dura3_35 <- str_count(data.month$exceed_35, "[1]{3,}")
data.month$Dura3_37 <- str_count(data.month$exceed_37, "[1]{3,}")
data.month$Dura3_40 <- str_count(data.month$exceed_40, "[1]{3,}")
data.month$Days_t90 <- str_count(data.month$exceed_t90, "1")
data.month$Days_t95 <- str_count(data.month$exceed_t95, "1")
data.month$Days_t98 <- str_count(data.month$exceed_t98, "1")
data.month$Dura3_t90 <- str_count(data.month$exceed_t90, "[1]{3,}")
data.month$Dura3_t95 <- str_count(data.month$exceed_t95, "[1]{3,}")
data.month$Dura3_t98 <- str_count(data.month$exceed_t98, "[1]{3,}")

names(data.month)
temp <- data.month[,c(1:7, 9:11)]  # Consider objective threshold
temp <- reshape(temp, timevar = "type", idvar = c("name", "loc", "month"), direction = "wide")
compare_binary <- function(a, b) {
  vec_a <- as.numeric(strsplit(a, "")[[1]])
  vec_b <- as.numeric(strsplit(b, "")[[1]])
  result1 <- as.numeric(vec_a == 1 & vec_b == 0)
  result2 <- as.numeric(vec_a == 0 & vec_b == 1)
  result3 <- as.numeric(vec_a == 1 & vec_b == 1)
  list(
    paste(result1, collapse = ""),
    paste(result2, collapse = ""),
    paste(result3, collapse = "")
  )
}
results <- do.call(rbind, apply(temp, 1, function(row) 
  compare_binary(row["exceed_t90.MAX"], row["exceed_t90.MIN"])))
temp <- cbind(temp, as.data.frame(matrix(unlist(results),nrow = 750)))
results <- do.call(rbind, apply(temp, 1, function(row) 
  compare_binary(row["exceed_t95.MAX"], row["exceed_t95.MIN"])))
temp <- cbind(temp, as.data.frame(matrix(unlist(results),nrow = 750)))
results <- do.call(rbind, apply(temp, 1, function(row) 
  compare_binary(row["exceed_t98.MAX"], row["exceed_t98.MIN"])))
temp <- cbind(temp, as.data.frame(matrix(unlist(results),nrow = 750)))
results <- do.call(rbind, apply(temp, 1, function(row) 
  compare_binary(row["exceed_35.MAX"], row["exceed_25.MIN"])))
temp <- cbind(temp, as.data.frame(matrix(unlist(results),nrow = 750)))
results <- do.call(rbind, apply(temp, 1, function(row) 
  compare_binary(row["exceed_37.MAX"], row["exceed_25.MIN"])))
temp <- cbind(temp, as.data.frame(matrix(unlist(results),nrow = 750)))
names(temp)
names(temp)[16:30] <- c("DHW_t90", "NHW_t90", "CHW_t90", "DHW_t95", "NHW_t95", "CHW_t95", 
                        "DHW_t98", "NHW_t98", "CHW_t98", "DHW_35", "NHW_35", "CHW_35",
                        "DHW_37", "NHW_37", "CHW_37")
# DHW is defined as Daily Max exceed threshold, but Daily Min does not.
# NHW is defined as Daily Max exceed threshold, but Daily Max does not.
# CHW is defined as Daily Max and Min all exceed threshold.
temp$DHW_t90 <- str_count(temp$DHW_t90, "[1]{2,}")
temp$NHW_t90 <- str_count(temp$NHW_t90, "[1]{2,}")
temp$CHW_t90 <- str_count(temp$CHW_t90, "[1]{2,}")
temp$DHW_t95 <- str_count(temp$DHW_t95, "[1]{2,}")
temp$NHW_t95 <- str_count(temp$NHW_t95, "[1]{2,}")
temp$CHW_t95 <- str_count(temp$CHW_t95, "[1]{2,}")
temp$DHW_t98 <- str_count(temp$DHW_t98, "[1]{2,}")
temp$NHW_t98 <- str_count(temp$NHW_t98, "[1]{2,}")
temp$CHW_t98 <- str_count(temp$NHW_t98, "[1]{2,}")
temp$DHW_35 <- str_count(temp$DHW_35, "[1]{2,}")
temp$NHW_35 <- str_count(temp$NHW_35, "[1]{2,}")
temp$CHW_35 <- str_count(temp$CHW_35, "[1]{2,}")
temp$DHW_37 <- str_count(temp$DHW_37, "[1]{2,}")
temp$NHW_37 <- str_count(temp$NHW_37, "[1]{2,}")
temp$CHW_37 <- str_count(temp$CHW_37, "[1]{2,}")

data.month <- data.month[data.month$type != "MIN",
                         !(names(data.month) %in% grep("exceed|type", names(data.month), value = TRUE))]
names(temp)
data.month <- merge(data.month, temp[,c(1:3, 16:30)], by = c("name", "loc", "month"))
rm(temp, results)

# CDD, 21, 26 as reference
names(data)
data.day <- aggregate(mean ~ month + name + loc + date + year, data[, 1:10], mean)
data.day <- data.day[order(data.day$name, data.day$loc, data.day$date),]
data.day$CDD21 <- data.day$mean - 21
data.day$CDD26 <- data.day$mean - 26
data.day[data.day < 0] <- 0
data.day <- aggregate(cbind(CDD21, CDD26) ~ name + loc + month, data.day, sum)
data.month <- merge(data.month, data.day, by = c("name", "loc", "month"))
rm(data, data.day, old, Use, compare_binary, log2seq)


##### Merge with CHARLS ######
data.psu <- readstata13::read.dta13("F:/OneDrive/graduate/data/CHARLS/2011 Wave1/data 20130312/psu.dta")
names(data.psu)
data.psu <- data.psu[,c(1,3)]
data.psu <- data.psu[data.psu$city != "巢湖市",]
mean(data.month$name %in% data.psu$city)
mean(data.psu$city %in% data.month$name)

data.psu <- merge(data.psu, data.month, by.x = "city", by.y = "name", all = TRUE)
data.psu <- data.psu[data.psu$month != "06", ]

saveRDS(data.psu, file = "CHARLS_temp.rds")

