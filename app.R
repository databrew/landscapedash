library(shiny)
library(shinydashboard)
source('functions.R')
source('global.R')

header <- dashboardHeader(title="DFS Landscape Dashboard")
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'sidebar',
    menuItem(
      text='DFS Market Overview',
      tabName='dfs_market_overivew',
      icon=icon('eye')),
    menuItem(
      text='Country Dashboard',
      tabName='country_dashboard',
      icon=icon('map')),
    menuItem(
      text='Country Analysis',
      tabName='country_analysis',
      icon=icon('stats', lib = 'glyphicon')),
    menuItem(
      text='X-Market Analysis',
      tabName='x_market_analysis',
      icon=icon('flash', lib = 'glyphicon')),
    menuItem(
      text='Glossary',
      tabName='glossary',
      icon=icon('book', lib = 'glyphicon')),
    menuItem(
      text = 'About',
      tabName = 'about',
      icon = icon('cog', lib = 'glyphicon'))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  fluidPage(
    column(10,
           tabItems(
             tabItem(
               tabName="dfs_market_overivew",
               fluidPage(
                 fluidRow(
                   column(3,
                          selectInput('dfs_market_overview_region',
                                      'Region',
                                      choices = sub_regions,
                                      selected = sub_regions,
                                      multiple = TRUE)),
                   column(3,
                          uiOutput('dfs_market_overview_indicator_ui')),
                   column(3,
                          sliderInput('dfs_market_overview_year',
                                      'Year',
                                      min = 2000,
                                      max = 2017,
                                      value = 2016,
                                      step = 1,
                                      sep = '')),
                   column(3,
                          radioButtons('dfs_market_overview_view',
                                       label = 'View type',
                                       choices = c('Map view',
                                                   'Chart view')))
                 ),
                 fluidRow(
                   uiOutput('df_market_overview_ui')
                 )
               )
             ),
             tabItem(
               tabName="country_dashboard",
               fluidPage(
                 fluidRow(uiOutput('country_dashboard_country_ui')),
                 fluidRow(
                   tabsetPanel(
                     tabPanel('Market overview'),
                     tabPanel('Qualitative overview'),
                     tabPanel('Additional analyses')
                   ))
                 
               )
             ),
             tabItem(
               tabName="country_analysis",
               fluidPage(
                 fluidRow(uiOutput('country_analysis_country_ui')),
                 fluidRow(
                   tabsetPanel(
                     tabPanel('Recommended analyses'),
                     tabPanel('Custom analyses')
                   )
                 ))
             ),
             tabItem(
               tabName="x_market_analysis",
               fluidPage(
                 fluidRow(
                   selectInput('x_market_analysis_region',
                               'Region',
                               choices = sub_regions,
                               selected = sub_regions,
                               multiple = TRUE)),
                 fluidRow(
                   tabsetPanel(
                     tabPanel('Recommended analyses'),
                     tabPanel('Custom analyses')
                   )
                 ) 
               )
             ),
             tabItem(
               tabName="glossary",
               fluidPage()
             ),
             tabItem(
               tabName = 'about',
               fluidPage(
                 fluidRow(h4("The dashboard was developed as a part of activities under the ", 
                             a(href = 'http://www.ifc.org/wps/wcm/connect/region__ext_content/ifc_external_corporate_site/sub-saharan+africa/priorities/financial+inclusion/za_ifc_partnership_financial_inclusion',
                               target='_blank',
                               "Partnership for Financial Inclusion"),
                             " (a $37.4 million joint initiative of the ",
                             a(href = "http://www.ifc.org/wps/wcm/connect/corp_ext_content/ifc_external_corporate_site/home",
                               target='_blank',
                               'IFC'),
                             " and the ",
                             a(href = "http://www.mastercardfdn.org/",
                               target='_blank',
                               'MasterCard Foundation'),
                             " to expand microfinance and advance digital financial services in Sub-Saharan Africa) by the FIG Africa Digital Financial Services unit (the MEL team).")),
                 br(),
                 fluidRow(div(img(src='partnershiplogo.png', 
                                  align = "center",
                                  height = '90'), style="text-align: center;"),
                          br(), 
                          style = 'text-align:center;'
                 ),
                 br(),
                 fluidRow(
                   shinydashboard::box(
                     title = 'Soren Heitmann',
                     fluidPage(
                       fluidRow(
                         div(a(img(src='about/Soren Heitmann.jpg', 
                                   align = "center",
                                   height = '80'),
                               href="mailto:sheitmann@ifc.org"), 
                             style="text-align: center;")
                       ),
                       fluidRow(h5('Project Lead'),
                                h5('Johannesburg, ', 
                                   a(href = 'mailto:sheitmann@ifc.org',
                                     'sheitmann@ifc.org'))),
                       fluidRow(helpText("Soren has a background in database management, software engineering and web technology. He manages the applied research and integrated monitoring, evaluation and learning program for the IFC-MasterCard Foundation Partnership for Financial Inclusion. He works at the nexus of data-driven research and technology to help drive learning and innovation within IFC’s Digital Financial Services projects in Sub-Saharan Africa."))
                     ),
                     width = 4),
                   shinydashboard::box(
                     title = 'Oleksiy Anokhin',
                     fluidPage(
                       fluidRow(
                         div(a(img(src='about/Oleksiy Anokhin.jpg', 
                                   align = "center",
                                   height = '80'),
                               href="mailto:oanokhin@ifc.org"), 
                             style="text-align: center;")
                       ),
                       fluidRow(h5('Project Specialist'),
                                h5('Washington, DC, ', 
                                   a(href = 'mailto:oanokhin@ifc.org',
                                     'oanokhin@ifc.org'))),
                       fluidRow(helpText("Oleksiy focuses on data-driven visualization solutions for international development. He is passionate about using programmatic tools (such as interactive dashboards) for better planning and implementation of projects, as well as for effective communication of projects results to various stakeholders."))
                     ),
                     width = 4),
                   shinydashboard::box(
                     title = 'Joe Brew',
                     fluidPage(
                       fluidRow(
                         div(a(img(src='about/Joe Brew.png', 
                                   align = "center",
                                   height = '80'),
                               href="mailto:jbrew1@worldbank.org"), 
                             style="text-align: center;")
                       ),
                       fluidRow(h5('Data Scientist'),
                                h5('Amsterdam, ', 
                                   a(href = 'mailto:jbrew1@worldbank.org',
                                     'jbrew1@worldbank.org'))),
                       fluidRow(helpText("Joe is a data scientist for", a(href = 'http://databrew.cc/', 'DataBrew.'), "He has a background in epidemiology and development economics. He works in both industry as a consultant as well as academia. His research focuses on the economics of malaria elimination programs in Sub-Saharan Africa."))
                     ),
                     width = 4)
                 ),
                 fluidRow(br(),
                          div(a(actionButton(inputId = "email", label = "Contact", 
                                             icon = icon("envelope", lib = "font-awesome")),
                                href="mailto:sheitmann@ifc.org",
                                align = 'center')), 
                          style = 'text-align:center;'
                 )
               )
             )
           )),
    column(2,
           div(actionButton('download', 'Download', icon = icon('download')),
               style = 'text-align:center;'))
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
  
  # Observe the download button and show modal 
  # for download optoins
  observeEvent(input$download, {
    showModal(modalDialog(
      title = "Download options",
      fluidPage(
        fluidRow(
          column(8,
                 radioButtons('download_type', 'What would you like to download?',
                              choices = c('Raw data (csv)',
                                          'Chart (png)',
                                          'Report (pdf)'))),
          column(4,
                 actionButton('download_confirm',
                              'Download',
                              icon = icon('download')))
        )
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Get a reactive selected indicator, so as to carry through between
  # re-rendering of the drop-down menu
  selected_indicator <- reactiveVal(value = NULL)
  observeEvent(input$dfs_market_overview_indicator, {
    this <- input$dfs_market_overview_indicator
    if(!is.null(this)){
      selected_indicator(this)
    }
  })
  
  # Indicator selection ui
  output$dfs_market_overview_indicator_ui <- renderUI({
    selected_year <- input$dfs_market_overview_year
    if(is.null(selected_year)){
      selected_year <- as.numeric(format(Sys.Date(), '%Y'))
    }
    
    # Only allow those indicators which have some values for the year in question
    available_indicators <- okay_indicators %>% filter(year == selected_year) %>%
      .$key %>% unlist
    
    # If the previously selected indicator is available, use it
    use_si <- FALSE
    si <- selected_indicator()
    if(!is.null(si)){
      if(length(si) > 0){
        if(si %in% available_indicators){
          use_si <- TRUE
        }
      }
    }
    
    if(use_si){
      selectInput('dfs_market_overview_indicator',
                  'Indicator',
                  choices = available_indicators,
                  selected = si)
    } else {
      selectInput('dfs_market_overview_indicator',
                  'Indicator',
                  choices = available_indicators)
    }

    
  })
  
  # Reactive shapefile for map
  afr <- reactive({
    out <- africa
    selected_sub_regions <- input$dfs_market_overview_region
    out <- out[out@data$sub_region %in% selected_sub_regions,]
    return(out)
  })
  
  # Reactive dataset after filtering for region, year and indicator
  df_filtered <- reactive({
    selected_sub_regions <- input$dfs_market_overview_region
    selected_key <- input$dfs_market_overview_indicator
    selected_year <- input$dfs_market_overview_year

    out <- df %>%
      filter(sub_region %in% selected_sub_regions,
             year == selected_year)
    if(is.null(selected_key)){
      selected_key <- out$key[1]
      
    }
    out <- out %>%
      filter(key == selected_key) %>%
      # Keep only one observation per country/key/year combination
      distinct(country, key, year, .keep_all = TRUE)
    message('df filtered is ')
    print(head(out))
    return(out)
  })
  
  output$dfs_market_overview_plot <-
    renderPlot({
      plot_data <- df_filtered()
      plot_data <- plot_data %>%
        filter(!is.na(value)) %>%
        arrange(value) %>%
        mutate(country = gsub('_', '\n', country)) 
      if(nrow(plot_data) > 0){
        plot_data <- plot_data %>%
          mutate(country = factor(country,
                                  levels = plot_data$country)) %>%
          mutate(ranking = 1:nrow(plot_data))
        cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(nrow(plot_data))
        ggplot(data = plot_data,
               aes(x = country,
                   y = value)) +
          geom_bar(stat = 'identity',
                   aes(fill = factor(ranking))) +
          theme_landscape() +
          theme(axis.text.x = element_text(angle = 90)) +
          labs(x = '',
               y = '') +
          scale_fill_manual(name = '',
                            values = cols) +
          theme(legend.position = 'none')
      } else {
        NULL
      }
      
    })
  
  
  # Plot vs. map ui for df_market_overview_plot
  output$df_market_overview_ui <- 
    renderUI({
      make_plot <- input$dfs_market_overview_view == 'Chart view'
      if(make_plot){
        plotOutput('dfs_market_overview_plot')
      } else {
        leafletOutput('dfs_market_overview_leaf')
      }
    })
  
  # UIs for country inputs
  country <- reactiveVal(value = countries[1])
  
  observeEvent(input$country_dashboard_country, {
    country(input$country_dashboard_country)
  })
  observeEvent(input$country_analysis_country, {
    country(input$country_analysis_country)
  })
  
  output$country_dashboard_country_ui <- renderUI({
    selected_country <- country()
    selectInput('country_dashboard_country',
                'Country',
                choices = countries,
                selected = selected_country,
                multiple = FALSE)
  })
  output$country_analysis_country_ui <- renderUI({
    selected_country <- country()
    selectInput('country_analysis_country',
                'Country',
                choices = countries,
                selected = selected_country,
                multiple = FALSE)
  })
  
  output$dfs_market_overview_leaf <-
    renderLeaflet({
      # Get map
      map <- afr()
      coords <- coordinates(map)
      l <- leaflet() %>%
        addTiles()
      if(nrow(coords) > 0){
        l <- l %>%
          fitBounds(min(coords[,1], na.rm = TRUE),
                    min(coords[,2], na.rm = TRUE),
                    max(coords[,1], na.rm = TRUE),
                    max(coords[,2], na.rm = TRUE))
      }
      return(l)
    })
  
  observeEvent(
    c(input$dfs_market_overview_indicator,
      input$dfs_market_overview_year), {
        year <- input$dfs_market_overview_year
        print('YEAR IS ')
        print(year)
      data <- df_filtered()
      map <- afr()
      
      if(nrow(data) > 0 & nrow(map) > 0){
        # Join data and map
        map@data <- left_join(map@data,
                              data %>%
                                dplyr::select(iso2,
                                              key,
                                              value) %>%
                                distinct(iso2,
                                         key, .keep_all = TRUE))
        
        # Prepare colors
        vals <- map@data$value
        if(all(is.na(vals))){
          vals <- 1
        }
        pal <- colorNumeric(
          palette = "YlOrRd",
          domain = vals)
        
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
        popups <- lapply(rownames(pops), function(row){
          knitr::kable(pops[row.names(pops) == row,], format = 'html')
        })
        
        
      
        l <- leafletProxy('dfs_market_overview_leaf') %>% 
          clearControls() %>%
          clearPopups() %>%
          clearShapes() %>%
          addPolygons(data = map,
                      stroke = FALSE, 
                      smoothFactor = 0.2, 
                      fillOpacity = 0.7,
                      color = ~pal(value),
                      popup = popups)
        l <- l %>%
          addLegend(pal = pal, values = map@data$value, opacity = 0.7,
                    position = "bottomright",
                    # title = title, # too wide!
                    title = NULL)
        return(l)
        
      }
    })
  
  
}

shinyApp(ui, server)