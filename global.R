library(tidyverse)
library(broom)
library(raster)
library(leaflet)
library(sp)
library(rgdal)
library(readxl)
library(Hmisc)
library(htmlTable)
library(leaflet)
library(RColorBrewer)
# library(rgeos)
library(sf)

if('prepared_data.RData' %in% dir()){
  load('prepared_data.RData')
} else {
  
  # Load a shapefile of Africa
  # africa <- rgdal::readOGR('spatial_data/africa_shp/', 'AfricanCountries')
  africa <- st_read('spatial_data/africa_shp/', 'AfricanCountries')
  # Simplify  
  africa_polys <- st_simplify(africa, preserveTopology = TRUE, dTolerance = 0.1)
  africa <- as(st_zm(africa_polys), "Spatial")
  
  # Get countries by region
  countries_by_region <- read_csv('spatial_data/countries_by_region.csv')
  # Replace "Middle Africa" with "Central Africa"
  countries_by_region$`sub-region` <-
    ifelse(countries_by_region$`sub-region` == 'Middle Africa',
           'Central Africa',
           countries_by_region$`sub-region`)
  countries_by_region$`sub-region` <- 
    gsub('Western ', 'West ', countries_by_region$`sub-region`)
  countries_by_region$`sub-region` <- 
    gsub('Eastern ', 'East ', countries_by_region$`sub-region`)
  
  # Keep only Africa and certain columns
  countries_by_region <- countries_by_region %>%
    filter(region == 'Africa') %>%
    dplyr::rename(sub_region = `sub-region`,
                  country = name,
                  iso2 = `alpha-2`,
                  iso3 = `alpha-3`) %>%
    dplyr::filter(sub_region != 'Northern Africa') %>%
    dplyr::select(country, region, sub_region, iso2, iso3)
  
  # Join region and country code information to the africa shapefile
  africa@data <- 
    left_join(africa@data,
              countries_by_region,
              by = c('ISO_CC' = 'iso2'))
  africa@data$iso2 <- africa@data$ISO_CC
  
  # Remove all those with no info (ie, north africa)
  africa <- africa[!is.na(africa@data$sub_region),]
  
  ##########
  # read in glossary data
  ##########
  glossary <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                         sheet = 'Glossary',
                         skip = 1)
  
  ##########
  # read in and clean qualiative overview data
  ##########
  qualy <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                      sheet = 'Qualitative Overview')
  
  # replace N/A with NA 
  qualy <- as.data.frame(apply(qualy, 2, function(x){
    gsub('N/A', NA, x)
  }), stringsAsFactors = F)
  
  # rename first variblle to be country
  colnames(qualy)[1] <- 'country'
  
  # Remove garbage variable
  qualy$X__2 <- NULL
  
  # melt data into long format 
  qualy <- gather(qualy, key, value, -country)
  
  # add a year variable with all NAs 
  qualy$year <- NA
  
  # recode the two congos
  qualy$country <- gsub('Congo, Democratic Republic of', 'Congo (Democratic Republic of the)', qualy$country)
  qualy$country <- gsub('Congo, Republic of', 'Congo', qualy$country)
  qualy$country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", qualy$country)
  qualy$country <- gsub("Gambia, The", "Gambia", qualy$country)
  qualy$country <- gsub("Tanzania", "Tanzania, United Republic of", qualy$country)
  
  # larger Democratic Republic of the Congo to the southeast (capital: Kinshasa), formerly known as Zaire and sometimes referred to as Congo-Kinshasa
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
  
  # remove aggregate "Africa" value from country
  fas <- fas %>% dplyr::filter(country != 'Africa')
  
  fas$country <- gsub('Congo, Democratic Republic of', 'Congo (Democratic Republic of the)', fas$country)
  fas$country <- gsub('Congo, Republic of', 'Congo', fas$country)
  fas$country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", fas$country)
  fas$country <- gsub("Gambia, The", "Gambia", fas$country)
  fas$country <- gsub("Tanzania", "Tanzania, United Republic of", fas$country)
  
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
  
  # rename variable to year
  names(afsd)[6] <- 'year'
  
  # combine indicator name and units into one variable called key 
  afsd$key <- paste0(afsd$`Indicator Name`, ' in ',afsd$Units)
  
  # keep only the 4 variable i need
  afsd <- afsd[, c('country', 'key', 'year', 'value')]
  
  
  # remove aggregate "Africa" value from country
  afsd <- afsd %>% filter(country != 'African Development Bank Group')
  
  # remove website address from country
  afsd <- afsd %>% filter(!grepl('http://dataporta', country))
  
  # recode country
  afsd$country <- gsub('Congo, Democratic Republic', 'Congo (Democratic Republic of the)', afsd$country)
  afsd$country <- gsub('Gambia, The', 'Gambia', afsd$country)
  afsd$country <- gsub("Tanzania", "Tanzania, United Republic of", afsd$country)
  afsd$country <- gsub("Sao Tome & Principe", "Sao Tome and Principe", afsd$country)
  
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
  
  # remove aggregate "Africa" value from country
  findex <- findex %>% dplyr::filter(country != 'Africa')
  
  
  findex$country <- gsub('Congo, Dem. Rep.', 'Congo (Democratic Republic of the)', findex$country)
  findex$country <- gsub('Congo, Rep.', 'Congo', findex$country)
  findex$country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", findex$country)
  findex$country <- gsub("Tanzania", "Tanzania, United Republic of", findex$country)
  
  ##########
  # read in findex  original and clean
  ##########
  findex_original <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                                sheet = 'Findex original')
  
  # remove country code and indicator code
  findex_original$`Country Code` <- findex_original$`Indicator Code` <- NULL
  
  # gather data to make long
  findex_original <- gather(findex_original, key, value, `2011`:`MRV`)
  
  # recode column names
  names(findex_original) <- c('country', 'key', 'year', 'value')
  
  # recode names
  findex_original$country <- gsub('Congo, Dem. Rep.', 'Congo (Democratic Republic of the)', findex_original$country)
  findex_original$country <- gsub('Congo, Rep.', 'Congo', findex_original$country)
  findex_original$country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", findex_original$country)
  findex_original$country <- gsub("Tanzania", "Tanzania, United Republic of", findex_original$country)
  
  
  
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
  
  # 
  
  # make data long with gatherm and name "key" "year", since those are the column names 
  gdp <- gather(gdp, year, value, `1980`:`2022`) # are the future years projections?
  
  # replace "no data" with NA in value by converting it to numeric
  gdp$value <- as.numeric(gdp$value)
  
  # recode country
  gdp$country <- gsub('Congo, Dem. Rep. of the', 'Congo (Democratic Republic of the)', gdp$country)
  gdp$country <- gsub('Congo, Republic of', 'Congo', gdp$country)
  gdp$country <- gsub('Gambia, The', 'Gambia', gdp$country)
  gdp$country <- gsub('Tanzania', 'Tanzania, United Republic of', gdp$country)
  
  # get regional african data from gdp - first make a grepl character object that has 
  # all regional data
  regional_string <- 'Africa (Regions)|North Africa|Sub-Saharan Africa|North Africa'
  regional_data_gdp <- gdp %>% filter(grepl(regional_string, gdp$country))
  
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
  
  # recod country column
  unique_subscribers$country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", unique_subscribers$country)
  unique_subscribers$country <- gsub("Congo, Democratic Republic", "Congo (Democratic Republic of the)", unique_subscribers$country)
  unique_subscribers$country <- gsub("Tanzania", "Tanzania, United Republic of", unique_subscribers$country)
  unique_subscribers$country <- gsub("Reunion", "Réunion", unique_subscribers$country)
  
  
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
  
  # clean country column
  smart_phone_adoption$country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", smart_phone_adoption$country)
  smart_phone_adoption$country <- gsub("Congo, Democratic Republic", "Congo (Democratic Republic of the)", smart_phone_adoption$country)
  smart_phone_adoption$country <- gsub("Reunion", "Réunion", smart_phone_adoption$country)
  smart_phone_adoption$country <- gsub("Tanzania", "Tanzania, United Republic of", smart_phone_adoption$country)
  
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
  
  # clean up country column
  tech_hubs$country <- gsub("Congo, Democratic Republic", "Congo (Democratic Republic of the)", tech_hubs$country)
  tech_hubs$country <- gsub("Reunion", "Réunion" , tech_hubs$country)
  tech_hubs$country <- gsub("Sao Tomé and Principe", "Sao Tome and Principe", tech_hubs$country)
  tech_hubs$country <- gsub("Tanzania", "Tanzania, United Republic of", tech_hubs$country)
  
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
  
  # recode country column
  ufa$country <- gsub("Democratic Republic of the Congo", "Congo (Democratic Republic of the)",ufa$country)
  ufa$country <- gsub("United Republic of Tanzania", "Tanzania, United Republic of",ufa$country)
  
  
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
  
  # Create some extra metrics
  extra_metrics <- 
    gpss_retail_transactions %>%
    filter(key %in% c('Value in USD Credit card',
                      'Value in USD Debit card')) %>%
    group_by(country, year) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    mutate(key = 'Card payment trans. Value') %>%
    ungroup 
  
  gpss_retail_transactions <- 
    bind_rows(gpss_retail_transactions,
              extra_metrics)

  extra_metrics <- 
    gpss_retail_transactions %>%
    filter(key %in% c('Value in USD Credit transfers internet')) %>%
    group_by(country, year) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    mutate(key = 'Internet banking Trans. Value') %>%
    ungroup 
  
  gpss_retail_transactions <- 
    bind_rows(gpss_retail_transactions,
              extra_metrics)
  
  ##########
  # Read in gpps accounts
  ##########
  gpps_accounts <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
                       sheet = 'GPPS Accounts',
                       skip = 5)
  # Make long
  gpps_accounts <- gather(gpps_accounts, year, value, `2015`:`2010__6`)
  gpps_accounts$key <- unlist(lapply(strsplit(gpps_accounts$year, "__"), function(x){x[2]}))
  gpps_accounts$key[is.na(gpps_accounts$key)] <- 0
  gpps_accounts$year <- unlist(lapply(strsplit(gpps_accounts$year, "__"), function(x){x[1]}))
  # Manual create dictionary for the pre header row
  dict <- data.frame(key = as.character(0:6),
                     new_key = c('Number of deposit transaction accounts',
                                 'Number of debit cards in circulation',
                                 'Number of credit cards in circulation',
                                 'Number of e-money accounts',
                                 'Number of card-based e-money accounts',
                                 'Number of mobile money accounts',
                                 'Number of online money accounts'))
  # Join 
  gpps_accounts <-
    left_join(gpps_accounts,
              dict,
              by = 'key')
  gpps_accounts <- gpps_accounts %>%
    mutate(key = new_key) %>%
    dplyr::select(-new_key)
  
  # Remove the garbage
  gpps_accounts <- 
    gpps_accounts %>%
    filter(!is.na(Country)) %>%
  filter(Country != 'Africa') %>%
    dplyr::rename(country = Country) %>%
    dplyr::mutate(value = as.numeric(value))
  
  # Create some new metrics
  new_metrics <- gpps_accounts %>%
    dplyr::filter(key %in%
                    c('Number of credit cards in circulation',
                      'Number of debit cards in circulation')) %>%
    group_by(country,year) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(key = 'Number of cards (debit+credit)')
  
  gpps_accounts <-
    bind_rows(gpps_accounts,
              new_metrics)
  
  ##########
  # gpps access points
  ##########
  gpps_access_points <- 
    read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
               sheet = 'GPPS Access points',
               skip = 1)
  # names(gpps_access_points)[7] <- 
  # gpps_access_points$`2010` <- as.numeric(unlist(gpps_access_points[,7]))
  
  # Make long
  gpps_access_points <- gather(gpps_access_points, year, value, `2015`:`2010__11`)
  gpps_access_points$key <- unlist(lapply(strsplit(gpps_access_points$year, "__"), function(x){x[2]}))
  gpps_access_points$key[is.na(gpps_access_points$key)] <- 0
  gpps_access_points$year <- unlist(lapply(strsplit(gpps_access_points$year, "__"), function(x){x[1]}))
  # Manual create dictionary for the pre header row
  dict <- data.frame(key = as.character(0:12),
                     new_key = c('Number of ATMs',
                                 'Number of POS terminals',
                                 'Number of merchants',
                                 'Number of ATM networks',
                                 'Number of POS networks',
                                 'Total number of branches of PSPs',
                                 'Of which: Number of branches of commercial banks',
                                 'Number of branches of other deposit-taking institutions',
                                 'Number of branches of other PSPs',
                                 'Total number of agents',
                                 'Number of agents of commercial banks',
                                 'Number of agents of other deposit-taking institutions',
                                 'Number of agents of other non-bank PSPs'))
  # Join 
  gpps_access_points <-
    left_join(gpps_access_points,
              dict,
              by = 'key')
  gpps_access_points <- gpps_access_points %>%
    mutate(key = new_key) %>%
    dplyr::select(-new_key)
  
  # Remove the garbage
  gpps_access_points <- 
    gpps_access_points %>%
    filter(!is.na(Country)) %>%
    filter(Country != 'Africa') %>%
    dplyr::rename(country = Country) %>%
    dplyr::mutate(value = as.numeric(value))
  
  #####
  
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
  
  # recode country column
  wb_dev$country <- gsub("Congo, Dem. Rep.", "Congo (Democratic Republic of the)", wb_dev$country)
  wb_dev$country <- gsub("Congo, Rep.", "Congo", wb_dev$country)
  wb_dev$country <- gsub("Cote d'Ivoire", "Côte d'Ivoire", wb_dev$country)
  wb_dev$country <- gsub("Gambia, The", "Gambia", wb_dev$country)
  wb_dev$country <- gsub("Tanzania, The", "Tanzania, United Republic of" , wb_dev$country)
  
  # get regional data
  regional_string <- 'Sub-Saharan|North Africa'
  regional_data_wb <- wb_dev %>% filter(grepl(regional_string, wb_dev$country))

  ##########
  # bind all of the data together 
  ##########
  df <- bind_rows(afsd,
                  fas,
                  findex,
                  findex_original,
                  gdp,
                  gpss_retail_transactions,
                  gpps_accounts,
                  gpps_access_points,
                  smart_phone_adoption,
                  tech_hubs,
                  ufa,
                  unique_subscribers,
                  wb_dev)
  
  # REMOVE THESE LATER
  df$key <- gsub("(", "", df$key, fixed = T)
  df$key <- gsub(")", "", df$key, fixed = T)
  df$key <- gsub("[", "", df$key, fixed = T)
  df$key <- gsub("]", "", df$key, fixed = T)
  df$key <- gsub("+", "", df$key, fixed = T)

  
  ##########
  # get data from africa@data
  ##########
  # get iso2 and sub_region from africa
  temp_africa <- africa@data[, c('country','iso2', "sub_region")]
  temp_africa <- temp_africa %>% group_by(iso2, sub_region) %>% distinct()
  
  # (1) get iso2 and sub_region for country in df - use inner_join to drop extra countries
  # in df
  df <- inner_join(df, temp_africa, by = 'country')
  
  # Arrange descending from most modern to least
  df <- df %>% arrange(desc(year), key, country)

  # (2) combine gdp and wb regional data
  df_regional <- bind_rows(regional_data_gdp,
                           regional_data_wb)
  # Arrange descending from most modern to least
  df_regional <- df_regional %>% arrange(desc(year))
  
  # (3) Same thing as in (1) - get iso2 and sub_region for qualy
  df_qualy <- inner_join(qualy, temp_africa, by = 'country')
  df_qualy <- df_qualy %>% arrange(desc(year))
  
  
  # Get which indicators are not 100% NA for any given year by region
  okay_indicators <- df %>%
    filter(!is.na(value)) %>%
    group_by(year, key, sub_region) %>%
    summarise(ok = length(value) > 0) %>%
    ungroup %>%
    arrange(year) %>%
    filter(ok)
  
  # Get which indicators are not 100% NA for any given year by country
  okay_indicators_country <- df %>%
    filter(!is.na(value)) %>%
    group_by(year, key, country) %>%
    summarise(ok = length(value) > 0) %>%
    ungroup %>%
    arrange(year) %>%
    filter(ok)
  
  # Correct country names
  df <- df %>%
    mutate(country = 
             ifelse(country == 'Cabo Verde',
                    'Cape Verde',
                    ifelse(country == 'Congo (Democratic Republic of the)',
                           'Congo DRC',
                           ifelse(grepl('saint helen', tolower(country)),
                                        'Saint Helena',
                                  ifelse(grepl('tanzania', tolower(country)),
                                         'Tanzania',
                                         country)))))

  save(africa,
       df,
       df_qualy, 
       df_regional,
       glossary,
       okay_indicators,
       okay_indicators_country,
       countries_by_region,
       wb_dev,
       file = 'prepared_data.RData')
}


# Create vector of indicators
indicators <- sort(unique(df$key))

# Create vector of countries
countries <- sort(unique(df$country))

# Create a vector of years
years <- sort(unique(df$year))

# Define a vector of sub_regions
sub_regions <- sort(unique(countries_by_region$sub_region))

# Manual removal of bizarre west african values
africa <- africa[!(africa@data$sub_region == 'Western Africa' &
                     coordinates(africa)[,2] < 0),]
africa <- africa[!coordinates(africa)[,2] < -35,]

# Make sure year is numeric
df$year <- as.numeric(as.character(df$year))

# No na values
df <- df %>% filter(!is.na(value))

# For those with no year, assume 2017
df$year[is.na(df$year)] <- 2017

# No repeats
df <- df %>%
  dplyr::distinct(country, key, year, value, .keep_all = TRUE)


# Replace the key with the "cleaned" indicator name from the glossary
df <- df %>%
  mutate(key_original = key) %>%
  left_join(glossary %>%
              dplyr::select(`Indicator Name in source`,
                            `Indicator Name`) %>%
              dplyr::rename(key_original = `Indicator Name in source`),
            by = 'key_original') %>%
  # Overwrite the key if there is an equivalent name in the glossary
  mutate(key = ifelse(!is.na(`Indicator Name`),
                      `Indicator Name`,
                      key)) %>%
  dplyr::select(-`Indicator Name`, key_original)

# Manual column name changes
df$key[df$key == 'Number of cards debitcredit'] <- 'Number of cards (debit+credit)'
df$key[df$key == 'Mobile money agents'] <- 'Registered MM agents'
df$key[df$key == 'Number of agents of other non-bank PSPs'] <- 'MM bank agents'
df$key[df$key == 'Number of agents of commercial banks'] <- 'MM non-bank agents'
df$key[df$key == 'Automated Teller Machines ATMs'] <- 'ATMs'
df$key[df$key == 'Number of POS terminals'] <- 'POS'
df$key[df$key == 'Q4 Percentage Unique Subscribers'] <- 'Unique mobile penetration'
df$key[df$key == 'Q4 Percentage with Smart Phone'] <- 'Smartphone penetration'
df$key[df$key == 'Account at a financial institution % age 15 w1'] <- '% of adults with FI account(1)'
df$key[df$key == 'Population, age 15'] <- 'Adult population'
df$key[df$key == 'GDP per capita, PPP current international $'] <- 'GDP per capita (PPP)'
df$key[df$key == 'Total Assets \r\nUS$ Million in US$ Million'] <- 'Bank assets/GDP'
df$key[df$key == '# of unbanked adults'] <- 'Nu of unbanked'
df$key[df$key == 'Poverty headcount ratio at $2 a day PPP % of population'] <- '% of population living below $1.9 PPP'
df$key[df$key == 'Urban population % of total'] <- 'Share of urban population'
df$key[df$key == 'Literacy rate, adult total % of people ages 15 and above'] <- 'Literacy rate'
new_rows <- df %>%
  filter(key == "Account % age 15 ts") %>%
  mutate(key = 'MM or FI account')
df <- df %>% bind_rows(new_rows)

new_rows <- df %>%
  filter(key == 'Mobile account % age 15 w2') %>%
  mutate(key = 'Mobile money')
df <- df %>% bind_rows(new_rows)

new_rows <- df %>%
  filter(key == 'Used an account to make a transaction through a mobile phone % age 15 w2') %>%
  mutate(key = 'Mobile banking')
df <- df %>% bind_rows(new_rows)

new_rows <- df %>%
  filter(key == 'Debit card % age 15 ts') %>%
  mutate(key = 'Debit cards')
df <- df %>% bind_rows(new_rows)

new_rows <- df %>%
  filter(key == 'Credit card % age 15 ts') %>%
  mutate(key = 'Credit cards')
df <- df %>% bind_rows(new_rows)

# Make sure most recent data at top
df <- 
  df %>%
  arrange(desc(year), country, key)
