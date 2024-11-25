############################
#
# Use areal interpolation to calculate the percentage of 2020
# population in 2014 EDAs
#
#
# november 2024 | amcadams
#
############################

library(cmapgeo)
library(tidyverse)
library(sf)
library(terra)

# import Disinvested Areas layer from CMAP V drive
DAs <- st_read(dsn = "V:/Policy/ONTO2050/Layers/LocalStrategyMaps.gdb",
        layer = "Disinvested_and_EDA") %>%
  filter(DA_EDAcomb == "Disinvested" | DA_EDAcomb == "Both")
  
# import 2020 Census blocks
cen20blocks <- st_read(dsn = "V:/Demographic_and_Forecast/Census/2020",
                       layer = "dhc_blocks_2020")

# import 2020 municipal boundaries
munis <- st_read(dsn = "V:/Administrative_and_Political_Boundaries/Municipalities/Muni_CMAP_Current.gdb",
                 layer = "MunisDissolved_2020") %>%
  filter(REGION == 1)



#quickplot of a layer
ggplot(data = munis) + geom_sf()


