library(tidyverse)
library(broom)
library(raster)
library(leaflet)
library(sp)
library(rgdal)
library(readxl)
library(htmlTable)
library(leaflet)
library(RColorBrewer)

# Load a shapefile of Africa
africa <- rgdal::readOGR('spatial_data/africa_shp/', 'AfricanCountries')

# Get countries by region
countries_by_region <- read_csv('spatial_data/countries_by_region.csv')
# Keep only Africa and certain columns
countries_by_region <- countries_by_region %>%
  filter(region == 'Africa') %>%
  dplyr::rename(sub_region = `sub-region`,
                country = name,
                iso2 = `alpha-2`,
                iso3 = `alpha-3`) %>%
  dplyr::filter(sub_region != 'Northern Africa') %>%
  dplyr::select(country, region, sub_region, iso2, iso3)

# Define a vector of sub_regions
sub_regions <- sort(unique(countries_by_region$sub_region))

# Join region and country code information to the africa shapefile
africa@data <- 
  left_join(africa@data,
            countries_by_region,
            by = c('ISO_CC' = 'iso2'))
africa@data$iso2 <- africa@data$ISO_CC

# Remove all those with no info (ie, north africa)
africa <- africa[!is.na(africa@data$sub_region),]

# Create some dummy data
df <- 
  expand.grid(country = sort(unique(africa@data$COUNTRY)),
              key = c('Poverty rate',
                      'Cell phone penetration',
                      'Access to financial services'),
              year = c(2000:2016,NA)) %>%
  left_join(africa@data %>% 
              filter(!duplicated(COUNTRY)) %>%
              dplyr::select(COUNTRY, iso2, sub_region),
            by = c('country' = 'COUNTRY'))
df$value <- rnorm(mean = 50, n = nrow(df), sd = 15)
df <- df %>% sample_n(round(0.9 * nrow(df)))

# Create vector of indicators
indicators <- sort(unique(df$key))

# Create vector of countries
countries <- sort(unique(df$country))

# Define theme
theme_landscape <- function (base_size = 15, y_comma = TRUE, white_bg = FALSE, outer_line = FALSE) {
  extrafont::loadfonts(quiet = TRUE)
  palette <- colorRampPalette(c("seashell", "white", "black"))(9)
  mint_cream <- "#F5FFFA"
  if (white_bg) {
    color_background <- "white"
  }
  else {
    color_background <- mint_cream
  }
  if (outer_line) {
    outer_line_color <- "black"
  }
  else {
    outer_line_color <- color_background
  }
  color_grid_major = palette[6]
  color_axis_text = palette[8]
  color_axis_title = palette[8]
  color = palette[8]
  color_title = palette[9]
  base_size1 = base_size
  out <- theme_bw(base_size = base_size1) + theme(panel.background = element_rect(fill = color_background, 
                                                                                  color = color_background)) + theme(plot.background = element_rect(fill = color_background, 
                                                                                                                                                    color = outer_line_color)) + theme(panel.border = element_rect(color = color_background)) + 
    theme(panel.grid.major = element_line(color = color_grid_major, 
                                          size = 0.25)) + theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) + theme(legend.background = element_rect(fill = color_background)) + 
    theme(legend.text = element_text(size = base_size * 0.5, 
                                     color = color_axis_title)) + theme(plot.title = element_text(color = color_title, 
                                                                                                  size = base_size * 1.2, vjust = 1.25)) + theme(plot.subtitle = element_text(color = color_title, 
                                                                                                                                                                              size = base_size * 0.9, vjust = 1.25)) + theme(axis.text.x = element_text(size = base_size * 
                                                                                                                                                                                                                                                          0.8, color = color_axis_text)) + theme(axis.text.y = element_text(size = base_size * 
                                                                                                                                                                                                                                                                                                                              0.8, color = color_axis_text)) + theme(axis.title.x = element_text(size = base_size * 
                                                                                                                                                                                                                                                                                                                                                                                                   0.8, color = color_axis_title, vjust = 0)) + theme(axis.title.y = element_text(size = base_size * 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    0.8, color = color_axis_title, vjust = 1.25)) + theme(plot.margin = unit(c(0.35, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               0.2, 0.3, 0.35), "cm")) + theme(complete = TRUE) + theme(legend.key = element_blank()) + 
    theme(strip.background = element_blank()) +
    theme(plot.background = element_rect(fill = '#ecf0f5', colour = '#ecf0f5')) +
    theme(panel.background = element_rect(fill = '#ecf0f5', colour = '#ecf0f5'))
  if (y_comma) {
    out <- list(out, scale_y_continuous(label = scales::comma))
  }
  else {
    out <- list(out)
  }
  return(out)
}


# # Read in the raw data
# qualy <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                         sheet = 'Qualitative Overview')
# fas <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                   sheet = 'IMF FAS 2017')
# afsd <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                    sheet = 'AFSD 2016')
# findex <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                    sheet = 'Findex')
# gdp <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                    sheet = 'GDP Growth')
# gsma_names <- names(read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                                  sheet = 'Unique subsc ', skip = 2)[0,])
# unique_subscribers <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                   sheet = 'Unique subsc ', skip = 3)
# names(gsma) <- gsma_names; rm(gsma_names)
# 
# gsma_names <- names(read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                                sheet = 'Smartphone adoption', skip = 2)[0,])
# smartphone_adoption <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                                  sheet = 'Smartphone adoption', skip = 3)
# names(smartphone_adoption) <- gsma_names; rm(gsma_names)
# 
# tech_hubs <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                   sheet = 'tech hubs', skip = 2) 
# tech_hubs <- tech_hubs[,(ncol(tech_hubs) - c(1,0))]
# names(tech_hubs) <- c('country', 'hubs')
# ufa <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                   sheet = 'UFA 2014')
# 
# # Ignoring these for now due to double headers
# # gpps_accounts <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
# #                   sheet = 'GPPS Accounts')
# # gpss_access_points <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
# #                             sheet = 'GPPS Accounts')
# gpss_retail_transactions <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                             sheet = 'GPSS Retail transactions')
# wb_dev <- read_excel('data/18-02-17 Africa DFS landscape data tool.xlsx',
#                             sheet = 'WBDev Ind')
