# Theme for charts
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
    theme(panel.background = element_rect(fill = '#ecf0f5', colour = '#ecf0f5')) +
    theme(panel.grid.major = element_blank())
  if (y_comma) {
    out <- list(out, scale_y_continuous(label = scales::comma))
  }
  else {
    out <- list(out)
  }
  return(out)
}


# Define function for printing nice html tables
prettify_scroll <- function (the_table, remove_underscores_columns = TRUE, cap_columns = TRUE,
                             cap_characters = TRUE, comma_numbers = TRUE, date_format = "%B %d, %Y",
                             round_digits = 2, remove_row_names = TRUE, remove_line_breaks = TRUE,
                             data_table = TRUE, nrows = 5, download_options = FALSE, no_scroll = TRUE,
                             scroll_x = TRUE){
  column_names <- names(the_table)
  the_table <- data.frame(the_table)
  names(the_table) <- column_names
  classes <- lapply(the_table, function(x) {
    unlist(class(x))[1]
  })
  if (cap_columns) {
    names(the_table) <- Hmisc::capitalize(names(the_table))
  }
  if (remove_underscores_columns) {
    names(the_table) <- gsub("_", " ", names(the_table))
  }
  for (j in 1:ncol(the_table)) {
    the_column <- the_table[, j]
    the_class <- classes[j][1]
    if (the_class %in% c("character", "factor")) {
      if (cap_characters) {
        the_column <- as.character(the_column)
        the_column <- Hmisc::capitalize(the_column)
      }
      if (remove_line_breaks) {
        the_column <- gsub("\n", " ", the_column)
      }
    }
    else if (the_class %in% c("POSIXct", "Date")) {
      the_column <- format(the_column, format = date_format)
    }
    else if (the_class %in% c("numeric", "integer")) {
      the_column <- round(the_column, digits = round_digits)
      if (comma_numbers) {
        if(!grepl('year', tolower(names(the_table)[j]))){
          the_column <- scales::comma(the_column)
        }
      }
    }
    the_table[, j] <- the_column
  }
  if (remove_row_names) {
    row.names(the_table) <- NULL
  }
  if (data_table) {
    if (download_options) {
      if(no_scroll){
        the_table <- DT::datatable(the_table, options = list(#pageLength = nrows,
          # scrollY = '300px', 
          paging = FALSE,
          scrollX = scroll_x,
          dom = "Bfrtip", buttons = list("copy", "print",
                                         list(extend = "collection", buttons = "csv",
                                              text = "Download"))), rownames = FALSE, extensions = "Buttons")
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
                                                             scrollX = scroll_x,
                                                             # scrollY = '300px', paging = FALSE,
                                                             dom = "Bfrtip", buttons = list("copy", "print",
                                                                                            list(extend = "collection", buttons = "csv",
                                                                                                 text = "Download"))), rownames = FALSE, extensions = "Buttons")
      }
      
    }
    else {
      if(no_scroll){
        the_table <- DT::datatable(the_table, options = list(#pageLength = nrows,
          # scrollY = '300px', 
          paging = FALSE,
          scrollX = scroll_x,
          columnDefs = list(list(className = "dt-right",
                                 targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
                                                             scrollX = scroll_x,
                                                             columnDefs = list(list(className = "dt-right",
                                                                                    targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      }
    }
  }
  return(the_table)
}


