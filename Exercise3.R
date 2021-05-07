library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times
caro60 <- read_delim("caro60.csv",",")
caro60 <- st_as_sf(caro60, coords = c("E", "N"), crs = 2056, remove = FALSE)