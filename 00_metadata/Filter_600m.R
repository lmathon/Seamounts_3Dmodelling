library(raster)
library(ggOceanMaps)
library(tidyverse)

all_bathy <- raster("00_metadata/environmental/Bathymetrie_GEBCO_2020_19_Jul_2021_56faf792d27a/gebco_2020_n-14.0_s-27.0_w155.0_e175.0.tif")

plot(all_bathy)

all_bathy[all_bathy[] < -600 ] = NA # check the use of braces to acces values of the raster
all_bathy[all_bathy[] > 0 ] = NA
plot(all_bathy)

all_bathy

writeRaster(all_bathy,"00_metadata/environmental/Bathymetrie_GEBCO_2020_19_Jul_2021_56faf792d27a/bathy_600.tif",options=c('TFW=YES'))


monts <- read.csv("00_metadata/Seamounts_Alain/NewCalSeamountAlain.csv", sep=";")

monts_600 <- monts %>%
  filter(Summit.depth < 600)

write.csv(monts_600, "00_metadata/Seamounts_Alain/NewCalSeamountAlain_600.csv", row.names = F)
