source('global.R')
# Document which indicators are missing for which countries, and when
left <- expand.grid(country = sort(unique(df$country)),
                    key = sort(unique(df$key)),
                    year = sort(unique(df$year)))
right <- df %>% 
  mutate(missing = is.na(value)) %>%
  dplyr::select(country, key, year, missing) 
joined <- left_join(x = left,
                    y = right) %>%
  mutate(missing = ifelse(is.na(missing), TRUE, missing)) %>%
  group_by(key, year) %>%
  mutate(flag = all(missing)) %>%
  ungroup %>%
  dplyr::filter(!flag) %>%
  dplyr::select(-flag) %>%
  filter(year >= 2010,
         year <= 2017)
summarized <- 
  joined %>%
  group_by(key, year) %>%
  summarise(countries_not_missing = paste0(sort(unique(country[missing])), collapse =';'),
            countries_missing = paste0(sort(unique(country[!missing])), collapse = ';'))
joined <- joined %>%
  dplyr::filter(missing) %>%
  dplyr::select(-missing) 

write_csv(joined, 'missing_details.csv')
write_csv(summarized, 'missing_overview.csv')
