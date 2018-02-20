library(tidyverse)
library(broom)
library(raster)
library(leaflet)
library(sp)
library(rgdal)
library(readxl)
library(reshape2)

# Load a shapefile of Africa
africa <- rgdal::readOGR('spatial_data', 'AfricanCountries')

##########
# read in qualiative overview data
##########
qualy <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                        sheet = 'Qualitative Overview')

# this data seems to contain only long character strings as variables, not sure if its useful for anything.
# first remove columns that are entirely NA
qualy <- qualy[,colSums(is.na(qualy))<nrow(qualy)]

# replace N/A with NA 
qualy <- as.data.frame(apply(qualy, 2, function(x){
  gsub('N/A', NA, x)
}), stringsAsFactors = F)

# rename first variblle to be country
colnames(qualy)[1] <- 'country'

# melt data into long format 
qualy <- melt(qualy, id.vars = 'country')

# add a year variable with all NAs 
qualy$year <- NA


##########
# read in IMF FAS 2017 data
##########
fas <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                  sheet = 'IMF FAS 2017')

# This data has info on ATM transactions and online mobile banking
# change first column name to country 
names(fas)[1] <- 'country'
names(fas)[2] <- 'year'


# make all but the first two columns numeric
fas[, 3:ncol(fas)] <- as.data.frame(apply(fas[, 3:ncol(fas)], 2, function(x){
  round(as.numeric(x), 2)
}))

# melt data to long form
temp <- melt(fas, id.vars = c('country', 'year'))




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
