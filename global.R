library(tidyverse)
library(broom)
library(raster)
library(leaflet)
library(sp)
library(rgdal)
library(readxl)

# Load a shapefile of Africa
africa <- rgdal::readOGR('spatial_data', 'AfricanCountries')

# Read in the raw data
landscape <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx')