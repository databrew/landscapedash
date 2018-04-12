# Read in all data
source('global.R')

# Read in the excel spreadsheet
hm <- read_excel('heatmap/heatmap.xlsx', skip = 2)

# # Use the column headers in hm (row 3) to reorganize the data
# x <- df %>%
#   filter(key %in% names(hm) | key_original %in% names(hm))

# To much unmatching. Just make wide for everything
wide <- df %>%
  filter(year <= 2016) %>%
  arrange(desc(year)) %>%
  mutate(dummy = 1) %>%
  group_by(country, key) %>%
  mutate(dummy = cumsum(dummy)) %>%
  filter(dummy == 1) %>%
  summarise(value = dplyr::first(value)) %>%
  ungroup %>%
  spread(key = key,
         value = value)

write_csv(wide, 'heatmap_big.csv')

write_csv(data.frame(key = sort(unique(df$key))), 'valid_keys.csv')
