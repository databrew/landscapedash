library(tidyverse)
library(broom)
library(raster)
library(leaflet)
library(sp)
library(rgdal)
library(readxl)
library(Hmisc)

# Load a shapefile of Africa
africa <- rgdal::readOGR('spatial_data', 'AfricanCountries')

##########
# read in and clean qualiative overview data
##########
# Questions for joe: the "value" in this data set is not not numeric. Do we keep it?

qualy <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                    sheet = 'Qualitative Overview')

# this data seems to contain only long character strings as variables, not sure if its useful for anything.
# first remove columns that are entirely NA

# replace N/A with NA 
qualy <- as.data.frame(apply(qualy, 2, function(x){
  gsub('N/A', NA, x)
}), stringsAsFactors = F)

# rename first variblle to be country
colnames(qualy)[1] <- 'country'

# melt data into long format 
qualy <- gather(qualy, key, value, -country)

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

# ONCE I COLLAPSE THE DATA INTO LONG FORMAT TO GET KEY (YEAR)
# AND VALUE, I DONT KNOW WHAT TO DO WITH THESE OTHER VARIABLE (COUNTRY - CAPITAL, CURRENCY, INDICATOR NAME, UNITS). I KNOW 
# WHAT I CARE ABOUT HERE IS THE INDICATOR NAME AND IT WOULD BE NICE TO HAVE THE UNITS AS WELL. SO IS IT OK TO JUST PASTE THOSE COLUMNS
# TOGETHER, CALL IT "KEY" AND REMOVE STUFF LIKE THE COUNTRY'S CAPITAL AND CURRENCY? BECAUSE THEY ARE JUST EXTRA INFORMATION BUT NOT THE 
# VARIABLE WE'RE INTERESTED IN.

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

# rename Country Name to country
names(findex)[1] <- 'country'
names(findex)[2] <- 'year'


# gather data
findex <- gather(findex, key, value, -c(`country`,`year`))

# make value numeric
findex$value <- as.numeric(findex$value)

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
gdp$key <- 'Real GDP Growth (Annual Percent Change)'

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
                                               })),' ','Percentage Unique Subscribers')

# extract the Q information out of the year variable 
unique_subscribers$year <- substr(unique_subscribers$year, 4, 7)

##########
# read in and clean Smartphone Adoption
##########

smart_phone_adoption <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                                   sheet = 'Smartphone adoption', skip = 2)

# remove first row because its just a an aggregate africa variable. 
smart_phone_adoption <- smart_phone_adoption[-1, ]

# remove IDs (can I do this ?)
smart_phone_adoption <- smart_phone_adoption %>% dplyr::select(-starts_with('id'))

# remove region and subregion (can I do this as well?)
smart_phone_adoption <- smart_phone_adoption %>% dplyr::select(-contains('egion'))

# gather data into long format
smart_phone_adoption <- gather(smart_phone_adoption, year, value, -Country)

# make country lower case 
names(smart_phone_adoption)[1] <- 'country'

# extract (strsplit) the 'Q' info into one variable called variable with Unique Subscribers" and "Q" info pasted together
smart_phone_adoption$key <- paste0(unlist(lapply(strsplit(smart_phone_adoption$year, ' '), 
                                                 function(x){
                                                   x[1]  
                                                 })),' ','Percentage with Smart Phone')

# extract the Q information out of the year variable 
smart_phone_adoption$year <- substr(smart_phone_adoption$year, 4, 7)


##########
# read in tech hub data and clean
##########

# read in data and get the only two columns that actually have data
tech_hubs <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                        sheet = 'tech hubs', skip = 2) 
tech_hubs <- tech_hubs[,(ncol(tech_hubs) - c(1,0))]
names(tech_hubs) <- c('country', 'value')

# add key and year column
tech_hubs$key <- 'Tech Hubs'
tech_hubs$year <- NA
tech_hubs$year <- as.character(tech_hubs$year)
tech_hubs$value <- as.numeric(tech_hubs$value)


##########
# read in UFA 2014 and clean
##########
ufa <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                  sheet = 'UFA 2014')

# remove X_1 because all NA and country code
ufa$X__1 <- ufa$`Country Code` <-  NULL

# create year variable filled with 2014 and rename Country Name to country
ufa$year <- '2014'
names(ufa)[1] <- 'country'

# rename to value and create key filled with "# of unbanked adults"
names(ufa)[2] <- 'value'
ufa$key <- "# of unbanked adults"

# remove last row (africa)
ufa <- ufa %>% dplyr::filter(country != 'Africa')

# make value numeric
ufa$value <- as.numeric(ufa$value)


##########
# read in GPSS Retail transactions and clean
##########
gpss_retail_transactions <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                                       sheet = 'GPSS Retail transactions')

# remove country code
gpss_retail_transactions$`Country code` <- NULL

# rename Country name to country and year
names(gpss_retail_transactions)[1] <- 'country'
names(gpss_retail_transactions)[2] <- 'year'
gpss_retail_transactions$year <- as.character(gpss_retail_transactions$year)

# gather columns into long format 
gpss_retail_transactions <- gather(gpss_retail_transactions, key, value, -c(country, year, `Variable name (see variable key in C1)`))

# clean up variable names
gpss_retail_transactions$`Variable name (see variable key in C1)` <- 
  gsub('_', ' ', gpss_retail_transactions$`Variable name (see variable key in C1)`) 


# make first letter capital
gpss_retail_transactions$`Variable name (see variable key in C1)` <- 
  Hmisc::capitalize(gpss_retail_transactions$`Variable name (see variable key in C1)`)

# combine to key columns 
gpss_retail_transactions$key <- paste0(gpss_retail_transactions$key, ' ',
                                       gpss_retail_transactions$`Variable name (see variable key in C1)`)

gpss_retail_transactions$`Variable name (see variable key in C1)` <- NULL


##########
# read in WEBDev Ind and clean
##########
wb_dev <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                     sheet = 'WBDev Ind')

# remove country code
wb_dev$`Country Code` <- NULL

# rename Country name to country
names(wb_dev)[1] <- 'country'

# rename Series Name to key
names(wb_dev)[2] <- 'key'

# gather data to make long
wb_dev <- gather(wb_dev, year, value, -c(country, key))

# clean year variable 
wb_dev$year <- substr(wb_dev$year, 1,4)

# change value to numeric to turn ".." into NA 
wb_dev$value <- as.numeric(wb_dev$value)


# Ignoring these for now due to double headers
# gpps_accounts <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                   sheet = 'GPPS Accounts')
# gpss_access_points <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                             sheet = 'GPPS Accounts')

##########
# bind all of the data together 
##########
full_data <- bind_rows(afsd,
                       fas,
                       findex,
                       gdp,
                       gpss_retail_transactions,
                       # qualy, ignoring because its "value" is a character and cant bind with other numerics, its bad data anyway
                       smart_phone_adoption,
                       tech_hubs,
                       ufa,
                       unique_subscribers,
                       wb_dev)

unique(full_data$country)
