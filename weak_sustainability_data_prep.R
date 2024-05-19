# This script creates a county level panel data set on land cover from the NLCD data set

# Loading libraries

library(exactextractr)
#library(RColorBrewer) 
#library(modelsummary)
#library(colorspace)
#library(stargazer)
#library(sandwich)
#library(leaflet)
#library(ggplot2)      
#library(ggrepel)
library(raster)
#library(lmtest)
library(tigris)
#library(terra)
#library(dplyr)
#library(tidyr)
#library(readr)
library(terra)
library(sp)
library(sf)

# Project directory

direc <- 'D:/NLCD/'

# Reading in county level GIS data

states <- c('01', '04', '05', '06', '08', '09', '10', '11', '12', '13', '16', '17', '18', '19', '20', '21', '22',
            '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38',
            '39', '40', '41', '42', '44', '45', '46', '47', '48', '49', '50', '51', '53', '54', '55', '56')

counties <- as.data.frame(NULL)

for (s in states) {
  
  print(paste0('Collecting county level GIS data for state FIPS ', s, '.......'))
  
  counties <- rbind(counties, counties(state = s, year = 2020))
  
}

# Defining parameters for reclassifying SpatRaster data in the main loop

m.all <- c(0, 9, 0, 9, 99, 1)
m.dev <- c(0, 19, 0, 19, 99, 1)
m.bar <- c(0, 29, 0, 29, 99, 1)
m.for <- c(0, 39, 0, 39, 99, 1)
m.scrub <- c(0, 49, 0, 49, 99, 1)
m.grass <- c(0, 69, 0, 69, 99, 1)
m.ag <- c(0, 79, 0, 79, 99, 1)
m.wet <- c(0, 89, 0, 89, 99, 1)

m.all <- matrix(m.all, ncol = 3, byrow = TRUE)
m.dev <- matrix(m.dev, ncol = 3, byrow = TRUE)
m.bar <- matrix(m.bar, ncol = 3, byrow = TRUE)
m.for <- matrix(m.for, ncol = 3, byrow = TRUE)
m.scrub <- matrix(m.scrub, ncol = 3, byrow = TRUE)
m.grass <- matrix(m.grass, ncol = 3, byrow = TRUE)
m.ag <- matrix(m.ag, ncol = 3, byrow = TRUE)
m.wet <- matrix(m.wet, ncol = 3, byrow = TRUE)

water <- c()
developed <- c()
barren <- c()
forested <- c()
scrubs <- c()
grasslands <- c()
agriculture <- c()
wetlands <- c()

# Main loop for 2021

for (i in 1:nrow(counties)) {
  
  print(paste0('Creating 2021 land cover data for county ', i, ' of 3,108.......'))
  
  tmp <- get_nlcd(template = as(counties[i,], 'Spatial'), year = 2021, force.redo = TRUE, label = 'fuck')
  
  land <- reclassify(raster(tmp), m.all)
  dev_land <- reclassify(raster(tmp), m.dev)
  bar_land <- reclassify(raster(tmp), m.bar)
  for_land <- reclassify(raster(tmp), m.for)
  scrub_land <- reclassify(raster(tmp), m.scrub)
  grass_land <- reclassify(raster(tmp), m.grass)
  ag_land <- reclassify(raster(tmp), m.ag)
  wet_land <- reclassify(raster(tmp), m.wet)
  
  res.all <- exact_extract(land, counties[i,], 'mean')
  res.dev <- exact_extract(dev_land, counties[i,], 'mean')
  res.bar <- exact_extract(bar_land, counties[i,], 'mean')
  res.for <- exact_extract(for_land, counties[i,], 'mean')
  res.scrub <- exact_extract(scrub_land, counties[i,], 'mean')
  res.grass <- exact_extract(grass_land, counties[i,], 'mean')
  res.ag <- exact_extract(ag_land, counties[i,], 'mean')
  res.wet <- exact_extract(wet_land, counties[i,], 'mean')
  
  water <- c(water, res.all - res.dev)
  developed <- c(developed, res.dev - res.bar)
  barren <- c(barren, res.bar - res.for)
  forested <- c(forested, res.for - res.scrub)
  scrubs <- c(scrubs, res.scrub - res.grass)
  grasslands <- c(grasslands, res.grass - res.ag)
  agriculture <- c(agriculture, res.ag - res.wet)
  wetlands <- c(wetlands, res.wet)
  
}

# Main loop for 2011

for (i in 1:nrow(counties)) {
  
  print(paste0('Creating 2011 land cover data for county ', i, ' of 3,108.......'))
  
  tmp <- get_nlcd(template = as(counties[i,], 'Spatial'), year = 2011, force.redo = TRUE, label = 'fuck')
  
  land <- reclassify(raster(tmp), m.all)
  dev_land <- reclassify(raster(tmp), m.dev)
  bar_land <- reclassify(raster(tmp), m.bar)
  for_land <- reclassify(raster(tmp), m.for)
  scrub_land <- reclassify(raster(tmp), m.scrub)
  grass_land <- reclassify(raster(tmp), m.grass)
  ag_land <- reclassify(raster(tmp), m.ag)
  wet_land <- reclassify(raster(tmp), m.wet)
  
  res.all <- exact_extract(land, counties[i,], 'mean')
  res.dev <- exact_extract(dev_land, counties[i,], 'mean')
  res.bar <- exact_extract(bar_land, counties[i,], 'mean')
  res.for <- exact_extract(for_land, counties[i,], 'mean')
  res.scrub <- exact_extract(scrub_land, counties[i,], 'mean')
  res.grass <- exact_extract(grass_land, counties[i,], 'mean')
  res.ag <- exact_extract(ag_land, counties[i,], 'mean')
  res.wet <- exact_extract(wet_land, counties[i,], 'mean')
  
  water <- c(water, res.all - res.dev)
  developed <- c(developed, res.dev - res.bar)
  barren <- c(barren, res.bar - res.for)
  forested <- c(forested, res.for - res.scrub)
  scrubs <- c(scrubs, res.scrub - res.grass)
  grasslands <- c(grasslands, res.grass - res.ag)
  agriculture <- c(agriculture, res.ag - res.wet)
  wetlands <- c(wetlands, res.wet)
  
}

# Main loop for 2001

for (i in 1:nrow(counties)) {
  
  print(paste0('Creating 2001 land cover data for county ', i, ' of 3,108.......'))
  
  tmp <- get_nlcd(template = as(counties[i,], 'Spatial'), year = 2001, force.redo = TRUE, label = 'fuck')
  
  land <- reclassify(raster(tmp), m.all)
  dev_land <- reclassify(raster(tmp), m.dev)
  bar_land <- reclassify(raster(tmp), m.bar)
  for_land <- reclassify(raster(tmp), m.for)
  scrub_land <- reclassify(raster(tmp), m.scrub)
  grass_land <- reclassify(raster(tmp), m.grass)
  ag_land <- reclassify(raster(tmp), m.ag)
  wet_land <- reclassify(raster(tmp), m.wet)
  
  res.all <- exact_extract(land, counties[i,], 'mean')
  res.dev <- exact_extract(dev_land, counties[i,], 'mean')
  res.bar <- exact_extract(bar_land, counties[i,], 'mean')
  res.for <- exact_extract(for_land, counties[i,], 'mean')
  res.scrub <- exact_extract(scrub_land, counties[i,], 'mean')
  res.grass <- exact_extract(grass_land, counties[i,], 'mean')
  res.ag <- exact_extract(ag_land, counties[i,], 'mean')
  res.wet <- exact_extract(wet_land, counties[i,], 'mean')
  
  water <- c(water, res.all - res.dev)
  developed <- c(developed, res.dev - res.bar)
  barren <- c(barren, res.bar - res.for)
  forested <- c(forested, res.for - res.scrub)
  scrubs <- c(scrubs, res.scrub - res.grass)
  grasslands <- c(grasslands, res.grass - res.ag)
  agriculture <- c(agriculture, res.ag - res.wet)
  wetlands <- c(wetlands, res.wet)
  
}

# Create and save a data.frame

locs <- c(unique(counties$GEOID), unique(counties$GEOID), unique(counties$GEOID))
years <- c(rep(2021, length(locs)/3), rep(2011, length(locs)/3), rep(2001, length(locs)/3))

df <- as.data.frame(cbind(locs, years, water, developed, barren, forested, scrubs, grasslands, agriculture, wetlands))
colnames(df) <- c('County', 'Year', 'Water', 'Development', 'Barren', 'Forests', 'Shrublands', 'Grasslands', 'Agriculture', 'Wetlands')

write.csv(df, paste0(direc, 'county_level_proportions_2001_2011_2021.csv'), row.names = FALSE)

################### NLCD LAND USE CODES ###################

## 1       water   11                   Open Water #476BA0
## 2       water   12           Perennial Ice/Snow #D1DDF9
## 3   developed   21        Developed, Open Space #DDC9C9
## 4   developed   22     Developed, Low Intensity #D89382
## 5   developed   23  Developed, Medium Intensity #ED0000
## 6   developed   24    Developed, High Intensity #AA0000
## 7      barren   31 Barren Land (Rock/Sand/Clay) #B2ADA3
## 8      forest   41             Deciduous Forest #68AA63
## 9      forest   42             Evergreen Forest #1C6330
## 10     forest   43                 Mixed Forest #B5C98E
## 11  shrubland   51                  Dwarf Scrub #A58C30
## 12  shrubland   52                  Scrub/Shrub #CCBA7C
## 13 herbaceous   71         Grassland/Herbaceous #E2E2C1
## 14 herbaceous   72            Sedge/Herbaceuous #C9C977
## 15 herbaceous   73                      Lichens #99C147
## 16 herbaceous   74                         Moss #77AD93
## 17    planted   81                  Pasture/Hay #DBD83D
## 18    planted   82             Cultivated Crops #AA7028
## 19   wetlands   90               Woody Wetlands #BAD8EA
## 20   wetlands   95 Emergent Herbaceous Wetlands #70A3BA

