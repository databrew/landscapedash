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
# read in and clean qualiative overview data
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
qualy1 <- gather(qualy, key, value, -country)

# add a year variable with all NAs 
qualy$year <- NA

##########
# read in and clean IMF FAS 2017 data
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
fas <- gather(fas, key, value, -c(country:year))

##########
# read in and clean AFSD 2016
##########
afsd <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                   sheet = 'AFSD 2016', skip = 3)

# this data has two columns for each variable, one in english and one in french. I'm going to assume that I can get rid of french columns 
# remove every french column
afsd$`Country - Pays` <- afsd$`Country - Capitale` <- afsd$`Country - Monnaie` <- 
  afsd$`Indicator - Nom` <- afsd$`Country - RegionId` <- afsd$Indicator <- afsd$Scale <- NULL


# remove Country and rename Country Name as country
afsd$Country <- NULL
names(afsd)[1] <- 'country'

# melt data to make long
afsd <- gather(afsd, key, value, -c(country:Units))

# THIS IS WEAR IM STUCK -  ONCE I COLLAPSE THE DATA INTO LONG FORMAT TO GET KEY (YEAR)
# AND VALUE, I DONT KNOW WHAT TO DO WITH THESE OTHER VARIABLE (COUNTRY - CAPITAL, CURRENCY, INDICATOR NAME, UNITS). I KNOW 
# WHAT I CARE ABOUT HERE IS THE INDICATOR NAME AND IT WOULD BE NICE TO HAVE THE UNITS AS WELL. SO IS IT OK TO JUST PASTE THOSE COLUMNS
# TOGETHER, CALL IT "KEY" AND REMOVE STUFF LIKE THE COUNTRY'S CAPITAL AND CURRENCY? BECAUSE THEY ARE JUST EXTRA INFORMATION BUT NOT THE 
# VARIABLE WE'RE INTERESTED IN (BELOW):

# rename variable to year
names(afsd)[6] <- 'year'

# combine indicator name and units into one variable called key 
afsd$key <- paste0(afsd$`Indicator Name`, ' in ',afsd$Units)

# keep only the 4 variable i need
afsd <- afsd[, c('country', 'key', 'year', 'value')]

##########
# read in findex and clean
##########
findex <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                     sheet = 'Findex', skip = 1)

# remove country code 
findex$`Country Code` <- NULL

# gather data
findex <- gather(findex, key, value, -c(`Country Name`:`Year`))


##########
# read and clean GDP Growth sheet
##########
gdp <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                  sheet = 'GDP Growth')

# remove any row that is all NA
remove_ind <- apply(gdp, 1, function(x) all(is.na(x)))
gdp <- gdp[ !remove_ind, ]

# remove last row (has almost all NA).
gdp <- gdp %>% filter(!grepl('IMF', `Real GDP growth (Annual percent change)`))

# rename first column to country and then create a variable that we just replace (Real GDP Growth (Annual Percent Change))
names(gdp)[1] <- 'country'
gdp$variable <- 'Real GDP Growth (Annual Percent Change)'

# make data long with gatherm and name "key" "year", since those are the column names 
gdp <- gather(gdp, year, value, `1980`:`2022`) # are the future years projections?

# replace "no data" with NA in value by converting it to numeric
gdp$value <- as.numeric(gdp$value)


##########
# read in Unique subsc sheet
##########

# read in data, skip first two rows
unique_subscribers <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                                 sheet = 'Unique subsc ', skip = 2)

# remove first row because its just a an aggregate africa variable. 
unique_subscribers <- unique_subscribers[-1, ]

# remove IDs (can I do this ?)
unique_subscribers <- unique_subscribers %>% dplyr::select(-starts_with('id'))

# remove region and subregion (can I do this as well?)
unique_subscribers <- unique_subscribers %>% dplyr::select(-contains('egion'))

# gather data into long format
unique_subscribers <- gather(unique_subscribers, year, value, -Country)

# make country lower case 
names(unique_subscribers)[1] <- 'country'

# extract (strsplit) the 'Q' info into one variable called variable with Unique Subscribers" and "Q" info pasted together
unique_subscribers$key <- paste0(unlist(lapply(strsplit(unique_subscribers$year, ' '), 
                                        function(x){
                                          x[1]  
                                          })),' ','Unique Subscribers')

# extract the Q information out of the year variable 
unique_subscribers$year <- as.numeric(substr(unique_subscribers$year, 4, 7))


#
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
