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
qualy <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                        sheet = 'Qualitative Overview')
fas <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                  sheet = 'IMF FAS 2017')
afsd <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                   sheet = 'AFSD 2016')
findex <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                   sheet = 'Findex')
gdp <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                   sheet = 'GDP Growth')
gsma_names <- names(read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                                 sheet = 'Unique subsc ', skip = 2)[0,])
unique_subscribers <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                  sheet = 'Unique subsc ', skip = 3)
names(gsma) <- gsma_names; rm(gsma_names)

gsma_names <- names(read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                               sheet = 'Smartphone adoption', skip = 2)[0,])
smartphone_adoption <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                                 sheet = 'Smartphone adoption', skip = 3)
names(smartphone_adoption) <- gsma_names; rm(gsma_names)

tech_hubs <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                  sheet = 'tech hubs', skip = 2) 
tech_hubs <- tech_hubs[,(ncol(tech_hubs) - c(1,0))]
names(tech_hubs) <- c('country', 'hubs')
ufa <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                  sheet = 'UFA 2014')

# Ignoring these for now due to double headers
# gpps_accounts <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                   sheet = 'GPPS Accounts')
# gpss_access_points <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                             sheet = 'GPPS Accounts')
gpss_retail_transactions <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                            sheet = 'GPSS Retail transactions')
wb_dev <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                            sheet = 'WBDev Ind')
