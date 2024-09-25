library(ggplot2)
library(ggforce)
library(scales)

##### Map Urban/Rural #####
grid_size <- 10  # Reduced from 50 to make each grid cell larger
x <- seq(-1, 1, length.out = grid_size)
y <- seq(-1, 1, length.out = grid_size)
df <- expand.grid(x = x, y = y)

df$distance <- sqrt(df$x^2 + df$y^2)

df$temp <- ifelse(df$distance <= 0.5,
                  30 - df$distance * 10,  # Urban temperatures (higher, decreasing outwards)
                  25 - (df$distance - 0.5) * 5)  # Rural temperatures (lower, decreasing outwards)

df$border <- ifelse(df$distance <= 0.43, 1,
                    ifelse(df$distance <= 0.6, 2,
                           ifelse(df$distance <= 1.1, 3, 4)))  # 1 for Urban, 2 for Rural, 3 for Others


set.seed(123)  
df$temp <- df$temp + rnorm(nrow(df), mean = 0, sd = 0.5)

schematic <- ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = temp), color = "white", size = 0.5) +  # color = as.factor(df$border))
  scale_fill_gradientn(colors = c("lightblue", "yellow", "orange", "red"),
                       name = "Temp (°C)",
                       limits = c(min(df$temp), max(df$temp))) +
  # scale_color_manual(values = c("darkgreen", "navy", "purple", NA)) + # "#950606", "#ba8e23", "white"
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = "black", fill = NA, size = 1) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 0.5), color = "black", fill = NA, alpha = 0.05, size = 1) + # fill = "grey70"
  annotate("text", x = 0.5, y = 1.07, label = "City Administrative Boundary", size = 5, family = "serif") +
  annotate("segment", x = 0.6, xend = 0.45, y = 1, yend = 0.89,
           arrow = arrow(length = unit(0.3, "cm")), size = 0.8) + 
  annotate("text", x = -0.4, y = -0.6, label = "Urban Boundary", size = 5, family = "serif") + 
  annotate("segment", x = -0.4, xend = -0.27, y = -0.55, yend = -0.42,
           arrow = arrow(length = unit(0.3, "cm")), size = 0.8) +
  # annotate("text", x = 0, y = 0.7, label = "Rural Area", size = 7, family = "serif") +
  # annotate("text", x = 0, y = 0, label = "Urban Area", size = 7, family = "serif") +
  geom_rect(aes(xmin = 0, xmax = 0.2222222, ymin = 0, ymax = 0.2222222), 
            fill = NA, color = "#950606", size = 1) +
  annotate("text", x = -0.04, y = -0.05, label = "Urban Pixel", 
           hjust = 0, size = 4, family = "serif") +
  geom_rect(aes(xmin = 0.6666666, xmax = 0.888888, ymin = 0.2222222, ymax = 0.4444444), 
            fill = NA, color = "#ba8e23", size = 1) +
  geom_rect(aes(xmin = 0.6666666, xmax = 0.888888, ymin = 0.4444444, ymax = 0.6666666), 
            fill = NA, color = "#ba8e23", size = 1) +
  annotate("text", x = 0.63, y = 0.16, label = "Rural Pixel", 
           hjust = 0, size = 4, family = "serif") +
  geom_rect(aes(xmin = 0, xmax = 0.2222222, ymin = 0.4444444, ymax = 0.6666666), 
            fill = NA, color = "#6e5415", size = 1) +
  annotate("text", x = -0.15, y = 0.71, label = "Urban & Rural Pixel", 
           hjust = 0, size = 4, family = "serif") +
  geom_rect(aes(xmin = 0.8888888, xmax = 1.1111111, ymin = -1.1111111, ymax = -0.8888888), 
            fill = NA, color = "black", size = 1) +
  annotate("segment", x = 0.8, xend = 0.88, y = -0.9999999, yend = -0.99999999,
           arrow = arrow(length = unit(0.3, "cm")), size = 0.7) +
  annotate("text", x = 0.313, y = -1, label = "Temperature = 20", 
           hjust = 0, size = 4, family = "serif") +
  coord_fixed(ratio = 1) +
  theme_void() +
  labs(caption = "Urban Temperature = Mean Value of Urban Pixel\nRural Temperature = Mean Value of Rural Pixel") + 
  theme(# legend.position=c(1.1,0.9), 
        # legend.direction = "horizontal",
        # legend.title.position = "bottom",
        # legend.title = element_text(hjust = 0.5),
        legend.position = "right", 
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "serif"),
        plot.caption = element_text(hjust=0.5, vjust = 2, size=rel(1.2)))
rm(df, grid_size,x,y)


##### China Plot #####
library(sf)
library(sjmisc)

china_city <- read_sf("F:/OneDrive/graduate/data/China/TianDiTu/GS（2024）0650/市.shp")
names(china_city)[1:4] <- c("city", "cityN", "province", "provinceN")
china_line <- read_sf("F:/OneDrive/graduate/data/China/TianDiTu/GS（2024）0650/十段线.shp")
china_cline <- read_sf("F:/OneDrive/graduate/data/China/TianDiTu/GS（2024）0650/市_境界线.shp")
psu <- readstata13::read.dta13("F:/OneDrive/graduate/data/CHARLS/2011 Wave1/data 20130312/psu.dta")

data <- readRDS("F:/OneDrive/graduate/my_research/In_progress/heatwave/mental health/file/Version 9/3 Match Data and Run Model/data.rds")
data <- data[,c("communityID", "city", "urban", "depression", "Dura3_35")]
data <- merge(psu[,c(1,3)], data, by = "communityID", all.y = TRUE)
data <- data[,c(2,4:6)]
names(data)
names(data)[1] <- "city"
data[data$city == "哈尔滨",]$city <- "哈尔滨市"
Use <- aggregate(cbind(depression, Dura3_35) ~ city + urban, data, mean)

Use[!(Use$city %in% china_city$city),"city"]
Use[Use$city == "北京", "city"] <- "北京市"
Use[Use$city == "天津", "city"] <- "天津市"
Use[Use$city == "海东地区", "city"] <- "海东市"
Use[Use$city == "襄樊市", "city"] <- "襄阳市"

Use <- Use[Use$urban == 0, ] # Change this for urban and rural
Use <- merge(china_city, Use, by = "city", all = TRUE)
table(Use$depression)
table(Use$Dura3_35)
Use$depression1 <- cut(Use$depression, breaks = seq(3,15,4), include.lowest = TRUE)
Use$HW1 <- cut(Use$Dura3_35, breaks = c(0,1,2,3), include.lowest = TRUE)
table(Use$depression1)
table(Use$HW1)



# bivariate map
library(biscale)
library(cowplot)
library(ggplot2)
try <- bi_class(Use, x = HW1, y = depression1)

# create map
map <- ggplot() +
  geom_sf(data = try, aes(fill = bi_class, geometry = geometry), color = "#241E3E", size = 0.1, show.legend = FALSE) +
  geom_sf(data = china_line, fill=NA, size=0.5, color="#241E3E") +
  geom_sf(data = china_cline, fill="white", size=0.5, color="#241E3E") +
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=104") +
  bi_scale_fill(pal = "GrPink", dim = 3, na.value = "white") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))
map

legend <- bi_legend(pal = "GrPink", dim = 3, 
                    xlab = "Heatwaves", ylab = "Depression", 
                    size = 10) +
  bi_theme(base_family = "serif", base_size = 10)
legend  

# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.205,0.1,0.27,0.27)

rural <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.205,0.1,0.27,0.27) + 
  draw_line(x = c(0.44,0.52), y = c(0.305,0.305), color = "#241E3E", size = 1) + # Add City Boundary
  draw_label("City Boundary", 0.48,0.28,color = "#241E3E", size = 10, , fontfamily = "serif") + 
  draw_line(x = c(0.46,0.5),y = c(0.22,0.22), color = "#241E3E", size = 1) + # Add No Data Legend
  draw_line(x = c(0.46,0.46),y = c(0.22,0.18), color = "#241E3E", size = 1) + 
  draw_line(x = c(0.5,0.5),y = c(0.18,0.22), color = "#241E3E", size = 1) + 
  draw_line(x = c(0.46,0.5),y = c(0.18,0.18), color = "#241E3E", size = 1) + 
  draw_label("No Data", 0.48,0.16, color = "#241E3E", size = 10, , fontfamily = "serif") +
  draw_label("(b) Rural Plot", 0.4,0.95, color = "#241E3E", size = 15, fontfamily = "serif")
rm(backup, china_city, china_cline, china_line, data, finalPlot, legend, map, psu, try)
rural
urban







  


