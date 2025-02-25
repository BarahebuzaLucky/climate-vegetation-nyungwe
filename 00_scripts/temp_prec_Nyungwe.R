# installing and loading packages

install.packages(c("raster", "sp", "rgdal", "sf"))
library(geodata)
library(raster)
library(sp)
library(rgdal)
library(sf)
library(tidyverse)
library(ggplot2)
library(terra)
# Let's download worldClim Data (Temperature and Precipitation)
# Mean temperature and precipitation at 2.5 arc-minute resolution (~5 km)

temp <- worldclim_global(var = "tmin", res = 2.5, path = "01_data")
prec <- worldclim_global(var = "prec", res = 2.5, path = "01_data") 

# Defining Nyungwe Region --> I should double check if this is correct

nyungwe <- extent(28.50, 30.00, -3.00, -1.00)


# crop and mask the data for Nyungwe

temp_nyungwe <- crop(temp, nyungwe)
prec_nyungwe <- crop(prec, nyungwe)

# calculating the mean annual temperature and precipitation

mean_temp <- app(temp_nyungwe, mean)
mean_prec <- app(prec_nyungwe, sum)

# we are going to plot the data using ggplot
# Let's convert raster data to data frames first

temp_df <- as.data.frame(mean_temp, xy = TRUE)
prec_df <- as.data.frame(mean_prec, xy = TRUE)

# plot mean temperature

ggplot(temp_df, aes(x, y, fill = mean)) +
  geom_tile()+
  scale_fill_viridis_c()+
  labs(title = "Mean Annual Temperature in Nyungwe",
       x = "Longitude", y = "Latitude", fill = " Temp(C)")+
  theme_minimal()

# plot annual precipitation

ggplot(prec_df, aes(x, y, fill = sum)) +
  geom_tile()+
  scale_fill_viridis_c()+
  labs(title = "Total Annual Precipitation in Nyungwe",
       x = "Longitude", y = "Latitude", fill = " Precip(mm)")+
  theme_minimal()

## Let's explore new approaches following Benjamin Bell.

# Ignore the following 2 lines
Rwanda <- worldclim_country("RW", "tmin", path = "01_data/", version = "2.1")
plot(Rwanda)

# Loading temperature data per month

temp1 <- raster("01_data/climate/wc2.1_2.5m/wc2.1_2.5m_tmin_01.tif")
temp2 <- raster("01_data/climate/wc2.1_2.5m/wc2.1_2.5m_tmin_02.tif")
temp3 <- raster("01_data/climate/wc2.1_2.5m/wc2.1_2.5m_tmin_03.tif")
temp4 <- raster("01_data/climate/wc2.1_2.5m/wc2.1_2.5m_tmin_04.tif")
temp5 <- raster("01_data/climate/wc2.1_2.5m/wc2.1_2.5m_tmin_05.tif")
temp6 <- raster("01_data/climate/wc2.1_2.5m/wc2.1_2.5m_tmin_06.tif")
temp7 <- raster("01_data/climate/wc2.1_2.5m/wc2.1_2.5m_tmin_07.tif")
temp8 <- raster("01_data/climate/wc2.1_2.5m/wc2.1_2.5m_tmin_08.tif")
temp9 <- raster("01_data/climate/wc2.1_2.5m/wc2.1_2.5m_tmin_09.tif")
temp10 <- raster("01_data/climate/wc2.1_2.5m/wc2.1_2.5m_tmin_10.tif")
temp11 <- raster("01_data/climate/wc2.1_2.5m/wc2.1_2.5m_tmin_11.tif")
temp12 <- raster("01_data/climate/wc2.1_2.5m/wc2.1_2.5m_tmin_12.tif")

# Defining coordinate for the Western part of Rwanda / where Nyungwe is located

longitude <- c(28.50, 30.00)
latitude <- c(-3.00, -1.00)

nyung <- data.frame(longitude, latitude)

# Extracting data from WorldClim for Nyungwe

temp_data <- nyung
temp_data$Jan <- raster::extract(temp1, nyung)
temp_data$Feb <- raster::extract(temp2, nyung)
temp_data$Mar <- raster::extract(temp3, nyung)
temp_data$Apr <- raster::extract(temp4, nyung)
temp_data$May <- raster::extract(temp5, nyung)
temp_data$Jun <- raster::extract(temp6, nyung)
temp_data$Jul <- raster::extract(temp7, nyung)
temp_data$Aug <- raster::extract(temp8, nyung)
temp_data$Sep <- raster::extract(temp9, nyung)
temp_data$Oct <- raster::extract(temp10, nyung)
temp_data$Nov <- raster::extract(temp11, nyung)
temp_data$Dec <- raster::extract(temp12, nyung)

# ploting the January temperature for Nyungwe

color <- colorRampPalette(c("purple", "blue", "skyblue", "green",
                              "lightgreen", "yellow", "orange", "red", 
                              "darkred"))
Clr <- brewer.pal(11, "RdBu")
plot(temp1, xlim = c(28.50, 30.00), ylim = c(-3.00, -1.00), col = Clr)

# Subsetting WorldClim data to create annual temperature map

ny_temp1 <- crop(temp1, nyungwe)
ny_temp2 <- crop(temp2, nyungwe)
ny_temp3 <- crop(temp3, nyungwe)
ny_temp4 <- crop(temp4, nyungwe)
ny_temp5 <- crop(temp5, nyungwe)
ny_temp6 <- crop(temp6, nyungwe)
ny_temp7 <- crop(temp7, nyungwe)
ny_temp8 <- crop(temp8, nyungwe)
ny_temp9 <- crop(temp9, nyungwe)
ny_temp10 <- crop(temp10, nyungwe)
ny_temp11 <- crop(temp11, nyungwe)
ny_temp12 <- crop(temp12, nyungwe)

# Let's plot now

plot(ny_temp1, color(100))
maps::map("world", add = TRUE, lwd = 1.5)

# Add place names

place <- c("Mt Bigugu", "Karisimbi")

place_lon <- c(29.25,29.5)
place_lat <- c(-2.41,-1.5)

points(place_lon, place_lat, col = "black", pch = 16)
text(place_lon, place_lat, labels = place, pos = 2)

# Add title
title(main = "Mean January Temperature\n(1970 - 2000)")

# Creating seasonal plots

ny_temp_spring <- mean(ny_temp3, ny_temp4, ny_temp5) # long rainy season
ny_temp_summer <- mean(ny_temp6, ny_temp7, ny_temp8, ny_temp9) # long dry season
ny_temp_autumn <- mean(ny_temp10, ny_temp11, ny_temp12) # short rain season
ny_temp_winter <- mean(ny_temp1, ny_temp2) # short dry season

# setting up a layout for the plots

layout(matrix(1:4, ncol = 2, byrow = TRUE))

# Spring

plot(ny_temp_spring, zlim = c(3, 18), col = Clr)
maps::map("world", add = TRUE, lwd = 1.5)
points(place_lon, place_lat, col = "black", pch = 16)
text(place_lon, place_lat, labels = place, pos = 2)
title(main = "Mean Spring Temperature\n(1970 - 2000)")

# Summer

plot(ny_temp_summer, zlim = c(3, 18), col = Clr)
maps::map("world", add = TRUE, lwd = 1.5)
points(place_lon, place_lat, col = "black", pch = 16)
text(place_lon, place_lat, labels = place, pos = 2)
title(main = "Mean Summer Temperature\n(1970 - 2000)")

# Autumn

plot(ny_temp_autumn, zlim = c(3, 18), col = Clr)
maps::map("world", add = TRUE, lwd = 1.5)
points(place_lon, place_lat, col = "black", pch = 16)
text(place_lon, place_lat, labels = place, pos = 2)
title(main = "Mean Autumn Temperature\n(1970 - 2000)")

# Winter

plot(ny_temp_winter, zlim = c(3, 18), col = Clr)
maps::map("world", add = TRUE, lwd = 1.5)
points(place_lon, place_lat, col = "black", pch = 16)
text(place_lon, place_lat, labels = place, pos = 2)
title(main = "Mean Winter Temperature\n(1970 - 2000)")



