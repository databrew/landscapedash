# Make leaflet map
make_leaf <- function(map, data){
  require(leaflet)
  require(RColorBrewer)
  
  # Join data and map
  map@data <- left_join(map@data,
                        data %>%
                          dplyr::select(iso2,
                                        key,
                                        value),
                        by = 'iso2')
  
  # Prepare colors
  bins <- unique(c(0, quantile(map$value, na.rm = TRUE), Inf))
  pal <- colorBin("YlOrRd", domain = map$value, bins = bins)
  
  # Popups
  avg_val <- mean(map@data$value, na.rm = TRUE)
  pops <- map@data %>%
    mutate(average_value = avg_val) %>%
    mutate(link = 'Click here') %>%
    dplyr::select(country,
                  sub_region,
                  key,
                  value,
                  average_value,
                  link) %>%
    mutate(value = round(value, digits = 2),
           average_value = round(average_value, digits = 2))
  names(pops) <- Hmisc::capitalize(gsub('_', ' ', names(pops)))
  
  popups = lapply(rownames(pops), function(row){
    x <- pops[row.names(pops) == row,]
    htmlTable(x,
              rnames = FALSE,
              caption = paste0('Some caption'))
  })
  
  leaflet() %>%
    addProviderTiles('Stamen.TonerLite') %>%
    addPolygons(data = map,
                fillColor = ~pal(value),
                weight = 0.2,
                opacity = 1,
                color = "white",
                fillOpacity = 0.7,
                popup = popups) %>%
    addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL,
              position = "bottomright")
}

# Theme for charts

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

