library(shiny)
library(shinydashboard)
library(leaflet.extras)
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
      icon = icon('cog', lib = 'glyphicon')),
    actionButton('download',
                 'Download',
                 icon = icon('download', 'fa-1'))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  fluidPage(
    column(12,
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
                   column(2,
                          selectizeInput('dfs_market_overview_year',
                                      'Year',
                                      choices = seq(min(df$year, na.rm = TRUE),
                                                    max(df$year, na.rm = TRUE)),
                                      selected = 2016)),#,
                                      # min = min(df$year, na.rm = TRUE),
                                      # max = max(df$year, na.rm = TRUE),
                                      # value = 2016,
                                      # step = 1,
                                      # sep = '')),
                   column(2,
                          radioButtons('dfs_market_overview_view',
                                       label = 'View type',
                                       choices = c('Map view',
                                                   'Chart view'))),
                   column(2,
                          uiOutput('map_ui'))
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
                     id = 'tab_country_dashboard',
                     tabPanel('Market overview',
                              br(),
                              fluidRow(
                                column(3,
                                       h3(textOutput('country_text'))),
                                column(9,
                                       h3(textOutput('region_text')))
                              ),
                              fluidRow(
                                shinydashboard::box(
                                  title = 'Level of DFS market development',
                                  footer = '',
                                  status = 'warning',
                                  solidHeader = TRUE,
                                  background = NULL,
                                  width = 12,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  fluidPage(
                                    column(6,
                                           plotOutput('level_of_dfs_market_development')),
                                    column(6,
                                           plotOutput('level_of_dfs_market_development2'))
                                  )
                                )
                              ),
                              
                              fluidRow(
                                shinydashboard::box(
                                  title = 'Digital Financial Services Market',
                                  footer = '',
                                  status = 'primary',
                                  solidHeader = TRUE,
                                  background = NULL,
                                  width = 6,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  fluidPage(
                                    DT::dataTableOutput('digital_financial_services_market_table')
                                  )
                                ),
                                shinydashboard::box(
                                  title = 'Financial access points',
                                  footer = '',
                                  status = 'primary',
                                  solidHeader = TRUE,
                                  background = NULL,
                                  width = 6,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  fluidPage(
                                    DT::dataTableOutput('financial_access_points_table')
                                  )
                                )
                              ),
                              
                              
                              fluidRow(
                                shinydashboard::box(
                                  title = 'Drivers of DFS growth',
                                  footer = '',
                                  status = 'primary',
                                  solidHeader = TRUE,
                                  background = NULL,
                                  width = 6,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  fluidPage(
                                    DT::dataTableOutput('drivers_of_dfs_growth_table')
                                  )
                                ),
                                shinydashboard::box(
                                  title = 'Macro drivers of growth',
                                  footer = '',
                                  status = 'primary',
                                  solidHeader = TRUE,
                                  background = NULL,
                                  width = 6,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  fluidPage(
                                    DT::dataTableOutput('macro_drivers_of_growth_table')
                                  )
                                )
                              ),
                              
                              
                              fluidRow(
                                shinydashboard::box(
                                  title = 'Mobile Money Market',
                                  footer = '',
                                  status = 'primary',
                                  solidHeader = TRUE,
                                  background = NULL,
                                  width = 6,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                 plotOutput('plot_mm_mkt')
                                ),
                                shinydashboard::box(
                                  title = 'Mobile Money Transactions',
                                  footer = '',
                                  status = 'danger',
                                  solidHeader = TRUE,
                                  background = NULL,
                                  width = 6,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  plotOutput('plot_mm_trans')
                                )),
                              fluidRow(
                                shinydashboard::box(
                                  title = 'Mobile Money by Region',
                                  footer = '',
                                  status = 'info',
                                  solidHeader = TRUE,
                                  background = NULL,
                                  width = 6,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  plotOutput('plot_mm_ssa')
                                ),
                                
                                shinydashboard::box(
                                  title = 'Transactions by Region',
                                  footer = '',
                                  status = 'info',
                                  solidHeader = TRUE,
                                  background = NULL,
                                  width = 6,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  plotOutput('plot_mm_ssa_new')
                                )
                                
                              ),
                              fluidRow(
                                shinydashboard::box(
                                  title = 'Drivers of Growth',
                                  footer = '',
                                  status = 'success',
                                  solidHeader = TRUE,
                                  background = NULL,
                                  width = 6,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  DT::dataTableOutput('tab_mm_pen')
                                ),
                                shinydashboard::box(
                                  title = 'DFS Market and Financial Access Points',
                                  footer = '',
                                  status = 'warning',
                                  solidHeader = TRUE,
                                  background = NULL,
                                  width = 6,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  DT::dataTableOutput('tab_mm_mkt')
                                )
                              )
                              ),
                     tabPanel('Qualitative overview',
                              br(),
                              shinydashboard::box(
                                title = NULL,
                                footer = '',
                                status = 'warning',
                                solidHeader = TRUE,
                                background = NULL,
                                width = 12,
                                collapsible = TRUE,
                                collapsed = FALSE,
                                DT::dataTableOutput('qualitative_overview')
                              )),
                     tabPanel('Additional analyses',
                              br(),
                              fluidPage(
                                fluidRow(
                                  column(12,
                                         align = 'center',
                                         selectInput('country_dashboard_additional_analyses_input',
                                                     'Select an analysis',
                                                     choices = c('Analysis 1',
                                                                 'Analysis 2',
                                                                 'Analysis 3')))
                                ),
                                fluidRow(
                                  shinydashboard::box(
                                    title = 'Under construction',
                                    status = 'warning',
                                    solidHeader = TRUE,
                                    background = NULL,
                                    width = 12,
                                    collapsible = TRUE,
                                    collapsed = FALSE,
                                    h3('Waiting on input from client',
                                       align = 'center')
                                  )
                                )
                              ))
                   ))
                 
               )
             ),
             tabItem(
               tabName="country_analysis",
               fluidPage(
                 fluidRow(uiOutput('country_analysis_country_ui')),
                 fluidRow(
                   tabsetPanel(
                     id = 'tab_country_analysis',
                     tabPanel('Recommended analyses',
                              br(),
                              shinydashboard::box(
                                title = 'Under construction',
                                status = 'warning',
                                solidHeader = TRUE,
                                background = NULL,
                                width = 12,
                                collapsible = TRUE,
                                collapsed = FALSE,
                                h3('Waiting on input from client',
                                   align = 'center')
                              )),
                     tabPanel('Custom analyses',
                              fluidRow(
                                column(3,
                                       align = 'center',
                                       uiOutput('country_analysis_indicator_ui')),
                                column(3,
                                       align = 'center',
                                       sliderInput('country_analysis_country_year',
                                                   'Filter years',
                                                   min = min(df$year, na.rm = TRUE),
                                                   max = max(df$year, na.rm = TRUE),
                                                   value = c(2000, 2016),
                                                   step = 1,
                                                   sep = '')),
                                column(3,
                                       align = 'center',
                                       selectInput('country_analysis_chart_type',
                                                   'Chart type',
                                                   choices = c('Bars',
                                                               'Points',
                                                               'Lines'))),
                                column(3,
                                       align = 'center',
                                       selectInput('c',
                                                   'Benchmark',
                                                   choices = letters))
                              ),
                                br(),
                                shinydashboard::box(
                                  title = 'Custom plot',
                                  status = 'warning',
                                  solidHeader = TRUE,
                                  background = NULL,
                                  width = 12,
                                  collapsible = TRUE,
                                  collapsed = FALSE,
                                  plotOutput('country_analysis_plot')
                                ))
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
                     id = 'tab_x_market_analysis',
                     tabPanel('Recommended analyses',
                              br(),
                              shinydashboard::box(
                                title = 'Under construction',
                                status = 'warning',
                                solidHeader = TRUE,
                                background = NULL,
                                width = 12,
                                collapsible = TRUE,
                                collapsed = FALSE,
                                h3('Waiting on input from client.',
                                   align = 'center')
                              )),
                     tabPanel('Custom analyses',
                              br(),
                              shinydashboard::box(
                                title = 'Under construction',
                                status = 'warning',
                                solidHeader = TRUE,
                                background = NULL,
                                width = 12,
                                collapsible = TRUE,
                                collapsed = FALSE,
                                h3('Waiting on input from client',
                                   align = 'center')
                              ))
                   )
                 ) 
               )
             ),
             tabItem(
               tabName="glossary",
               fluidPage(
                 fluidRow(
                   column(12,
                          DT::dataTableOutput('glossary_table'))
                 )
               )
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
           )
           )
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
                              choices = c('Data (csv)',
                                          'Report (pdf)'))),
          column(4,
                 actionButton('download_confirm',
                              'Next',
                              icon = icon('download')))
        )
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Observe the download confirmation, and download stuff
  observeEvent(input$download_confirm,{
    
    # Get the currently selected tab
    sidebar_selected <- input$sidebar
    message('side bar is ', sidebar_selected)
    tab_name <- paste0('tab_',
                       tolower(gsub(' ', '_', sidebar_selected)))
    message('tab name is ', tab_name)
    tab_selected <- input[[tab_name]]
    
    showModal(modalDialog(
      title = "Download scope",
      fluidPage(
        fluidRow(
          column(8,
                 radioButtons('download_type', 'How much would you like to download?',
                              choices = c('This tab',
                                          'All info'))),
          column(4,
                 actionButton('download_confirm_2',
                              'Download',
                              icon = icon('download')))
        )
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$download_confirm_2, {
    showModal(modalDialog(
      title = "Not quite ready...",
      fluidPage(
        fluidRow(h2('Under construction.'))
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
    sr <- input$dfs_market_overview_region
    available_indicators <- okay_indicators %>% 
      filter(year == selected_year) %>%
      filter(sub_region %in% sr) %>%
      .$key %>% unlist
    available_indicators <- sort(unique(available_indicators))
    
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
    
    title <- 'Indicator'
    if(use_si){
      selectInput('dfs_market_overview_indicator',
                  title,
                  choices = available_indicators,
                  selected = si)
    } else {
      selectInput('dfs_market_overview_indicator',
                  title,
                  choices = available_indicators,
                  selected = available_indicators[1])
    }
    
    
  })
  
  # Reactive shapefile for map
  afr <- reactive({
    out <- africa
    selected_sub_regions <- input$dfs_market_overview_region
    out <- out[out@data$sub_region %in% selected_sub_regions,]
    return(out)
  })
  
  # Data frame for labeling maps
  africa_df <- reactive({
    # Data frame for labeling maps
    aa <- afr()
    aa_df <- aa@data
    coords <- coordinates(aa)
    aa_df$x <- coords[,1]
    aa_df$y <- coords[,2]
    aa_df <- 
      aa_df %>%
      arrange(desc(Shape_STAr)) %>%
      mutate(COUNTRY = gsub(' ', '\n', COUNTRY)) %>%
      group_by(country = COUNTRY) %>%
      summarise(x = dplyr::first(x),
                y = dplyr::first(y))
    return(aa_df)
  })
  
  # Reactive dataset after filtering for region, year and indicator
  df_filtered <- reactive({
    selected_sub_regions <- input$dfs_market_overview_region
    selected_key <- input$dfs_market_overview_indicator
    selected_year <- input$dfs_market_overview_year
    
    message('selected_sub_regions is ', paste0(selected_sub_regions, collapse = ';'))
    message('selected_key is ', selected_key)
    message('selected_year is ', selected_year)
    
    out <- df %>%
      filter(sub_region %in% selected_sub_regions,
             year == selected_year)
    
    message('out is ')
    print(head(out))
    
    if(is.null(selected_key)){
      selected_key <- out$key[1]
      message('new selected key ', selected_key)
    }
    out <- out %>%
      filter(!is.na(value)) %>%
      filter(key == selected_key) %>%
      # Keep only one observation per country/key/year combination
      distinct(country, key, year, .keep_all = TRUE)
    
    message('new out is ')
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
        
        # Get the arrange input
        arrange_by <- input$dfs_market_overview_plot_arrange
        if(is.null(arrange_by)){
          arrange_by <- 'Ascending by value'
        }
        
        # Arrange according to input
        # (already ascending by value, so no need to modify if that)
        if(arrange_by == 'Descending by value'){
          plot_data <- plot_data %>% arrange(desc(value))
        } else if(arrange_by == 'Alphabetically'){
          plot_data <- plot_data %>% arrange(country)
        } else if(arrange_by == 'Reverse alphabetically'){
          plot_data <- plot_data %>% arrange(desc(country))
        } else if(arrange_by == 'By region'){
          plot_data <- plot_data %>% arrange(sub_region)
        }
        
        plot_data <- plot_data %>%
          mutate(country = factor(country,
                                  levels = plot_data$country)) %>%
          mutate(ranking = 1:nrow(plot_data))
        
        avg <- round(mean(plot_data$value, na.rm = TRUE), digits = 2)
        
        label_df <- data.frame(x = levels(plot_data$country)[1],
                               y = avg,
                               label = avg)
        
        
        cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(length(unique(plot_data$sub_region)))
        
        if(arrange_by == 'By region'){
          g <- ggplot(data = plot_data,
                      aes(x = country,
                          y = value)) +
            geom_bar(stat = 'identity',
                     alpha = 0.9,
                     aes(fill = factor(sub_region))) +
            scale_fill_manual(name = '',
                              values = cols) 
        } else {
          g <- ggplot(data = plot_data,
                      aes(x = country,
                          y = value)) +
            geom_bar(stat = 'identity',
                     fill = 'blue',
                     alpha = 0.7)
                     
        }
        
        g +
          theme_landscape() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(x = '',
               y = '') +
          geom_hline(yintercept = avg,
                     lty = 2,
                     alpha = 0.8) +
          geom_label(data = label_df,
                     aes(x = x,
                         y = y,
                         label = paste0('Average:\n',
                                        label)),
                     nudge_x = 0.5)
      } else {
        NULL
      }
      
    })
  
  
  # Plot vs. map ui for df_market_overview_plot
  output$df_market_overview_ui <- 
    renderUI({
      make_plot <- input$dfs_market_overview_view == 'Chart view'
      if(make_plot){
        fluidPage(
          fluidRow(
            column(4),
            column(4,
                   align = 'center',
                   selectInput('dfs_market_overview_plot_arrange',
                               'Arrange chart by',
                               choices = c('Ascending by value',
                                           'Descending by value',
                                           'Alphabetically',
                                           'Reverse alphabetically',
                                           'By region'),
                               selected = 'Ascending by value')),
            column(4)
          ),
          fluidRow(plotOutput('dfs_market_overview_plot'))
          )
        
      } else {
        leafletOutput('dfs_market_overview_leaf', height = 500)
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
    message('Selected country is ', selected_country)
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
      
      # Get data
      year <- input$dfs_market_overview_year
      data <- df_filtered()
      
      
      l <- leaflet() %>%
        addProviderTiles('CartoDB.PositronNoLabels') %>%
        addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)
        
      if(nrow(coords) > 0){
        l <- l %>%
          fitBounds(min(coords[,1], na.rm = TRUE),
                    min(coords[,2], na.rm = TRUE),
                    max(coords[,1], na.rm = TRUE),
                    max(coords[,2], na.rm = TRUE))
      }

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
          x <- pops[row.names(pops) == row,]
          captions <- x$Key
          x$Key <- NULL
          x <- gather(x,a,b,Country:Link) %>%
            mutate(a = paste0(a, '   ')) %>%
            mutate(b = paste0('    ', b))
          names(x) <- c(' ', '  ')
          knitr::kable(x, 
                       rnames = FALSE,
                       caption = captions,
                       align = 'lr',
                       # align = paste(rep("l", ncol(x)), collapse = ''),
                       format = 'html')
        })
        
        # Circle or not
        circles <- FALSE
        if(!is.null(input$circles)){
          if(input$circles == 'Circles'){
            circles <- TRUE
          }
        }
        
        message('circles is ', circles)
        
        if(circles){
          
          aff <- map@data
          coords <- coordinates(map)
          aff$lng <- coords[,1]
          aff$lat <- coords[,2]
          
          aff <- aff %>%
            arrange(desc(Shape_STAr)) %>%
            dplyr::distinct(country, .keep_all = TRUE)
          
          make_small <- 
            function(x){
              ((1 + percent_rank(x)) * 1.4)^3
            }
          
          l <- l %>%
            clearControls() %>%
            clearPopups() %>%
            clearShapes() %>%
            addCircleMarkers(data = aff,
                             lng = aff$lng,
                             lat = aff$lat,
                             color = 'black',
                             weight = 1,
                             fillColor = ~pal(value),
                             radius = make_small(aff$value),
                             popup = popups) %>%
            addPolylines(data = map,
                         color = 'black',
                         weight = 1,
                         opacity = 1)
          l <- l %>%
            addLegend(pal = pal, values = map@data$value, opacity = 0.7,
                      position = "bottomright",
                      # title = title, # too wide!
                      title = NULL)
        } else {
          l <- l %>% 
            clearControls() %>%
            clearPopups() %>%
            clearShapes() %>%
            addPolygons(data = map,
                        stroke = TRUE,
                        weight = 1,
                        smoothFactor = 0.2, 
                        fillOpacity = 0.7,
                        color = ~pal(value),
                        popup = popups,
                        label = map@data$country) %>%
            addPolylines(data = map,
                         color = 'black',
                         weight = 1,
                         opacity = 1)# %>%
          l <- l %>%
            addLegend(pal = pal, values = map@data$value, opacity = 0.7,
                      position = "bottomright",
                      # title = title, # too wide!
                      title = NULL)
        }
      }
      
      
      return(l)
    })
  
  # observeEvent(
  #   c(input$dfs_market_overview_indicator,
  #     input$dfs_market_overview_year,
  #     input$dfs$market_overview_region), {
  #       year <- input$dfs_market_overview_year
  #       data <- df_filtered()
  #       map <- afr()
  #       
  #       if(nrow(data) > 0 & nrow(map) > 0){
  #         # Join data and map
  #         map@data <- left_join(map@data,
  #                               data %>%
  #                                 dplyr::select(iso2,
  #                                               key,
  #                                               value) %>%
  #                                 distinct(iso2,
  #                                          key, .keep_all = TRUE))
  #         
  #         # Prepare colors
  #         vals <- map@data$value
  #         if(all(is.na(vals))){
  #           vals <- 1
  #         }
  #         pal <- colorNumeric(
  #           palette = "YlOrRd",
  #           domain = vals)
  #         
  #         # Popups
  #         avg_val <- mean(map@data$value, na.rm = TRUE)
  #         pops <- map@data %>%
  #           mutate(average_value = avg_val) %>%
  #           mutate(link = 'Click here') %>%
  #           dplyr::select(country,
  #                         sub_region,
  #                         key,
  #                         value,
  #                         average_value,
  #                         link) %>%
  #           mutate(value = round(value, digits = 2),
  #                  average_value = round(average_value, digits = 2))
  #         names(pops) <- Hmisc::capitalize(gsub('_', ' ', names(pops)))
  #         popups <- lapply(rownames(pops), function(row){
  #           x <- pops[row.names(pops) == row,]
  #           captions <- x$Key
  #           x$Key <- NULL
  #           x <- gather(x,a,b,Country:Link) %>%
  #             mutate(a = paste0(a, '   ')) %>%
  #             mutate(b = paste0('    ', b))
  #           names(x) <- c(' ', '  ')
  #           knitr::kable(x, 
  #                        rnames = FALSE,
  #                        caption = captions,
  #                        align = 'lr',
  #                        # align = paste(rep("l", ncol(x)), collapse = ''),
  #                        format = 'html')
  #         })
  #         
  #         # Circle or not
  #         circles <- FALSE
  #         if(!is.null(input$circles)){
  #           if(input$circles == 'Circles'){
  #             circles <- TRUE
  #           }
  #         }
  #         
  #         message('circles is ', circles)
  #         
  #         if(circles){
  #           
  #           aff <- map@data
  #           coords <- coordinates(map)
  #           aff$lng <- coords[,1]
  #           aff$lat <- coords[,2]
  #           
  #           l <- leafletProxy('dfs_market_overview_leaf') %>% 
  #             clearControls() %>%
  #             clearPopups() %>%
  #             clearShapes() %>%
  #             addCircleMarkers(data = aff,
  #                              lng = aff$lng,
  #                              lat = aff$lat,
  #                              radius = aff$value,
  #                              popup = popups) %>%
  #             addPolylines(data = map,
  #                          color = 'black',
  #                          weight = 1,
  #                          opacity = 1)# %>%
  #           # l <- l %>%
  #           #   addLegend(pal = pal, values = map@data$value, opacity = 0.7,
  #           #             position = "bottomright",
  #           #             # title = title, # too wide!
  #           #             title = NULL)
  #           return(l)
  #         } else {
  #           l <- leafletProxy('dfs_market_overview_leaf') %>% 
  #             clearControls() %>%
  #             clearPopups() %>%
  #             clearShapes() %>%
  #             addPolygons(data = map,
  #                         stroke = TRUE,
  #                         weight = 1,
  #                         smoothFactor = 0.2, 
  #                         fillOpacity = 0.7,
  #                         color = ~pal(value),
  #                         popup = popups,
  #                         label = map@data$country) %>%
  #             addPolylines(data = map,
  #                          color = 'black',
  #                          weight = 1,
  #                          opacity = 1)# %>%
  #           l <- l %>%
  #             addLegend(pal = pal, values = map@data$value, opacity = 0.7,
  #                       position = "bottomright",
  #                       # title = title, # too wide!
  #                       title = NULL)
  #           return(l)
  #         }
  #       }
  #       
  #     })
  
  # Observe zoom and change
  observeEvent(input$dfs_market_overview_leaf_zoom, {
    adf <- africa_df()
    zoom_level <- input$dfs_market_overview_leaf_zoom
    message('Zoom level is ', zoom_level)
    text_size <- paste0((round((zoom_level * 1.5)^1.3)), 'px')
    leafletProxy('dfs_market_overview_leaf') %>%
      # removeMarker(layerId = 'country_names') %>%
      # clearGroup(layerId = 'country_names') %>%
      clearGroup(group = 'country_names') %>%
      addLabelOnlyMarkers(#layerId = 'country_names',
                          # data = adf,
        group = 'country_names',
                          lng = adf$x,
                          lat = adf$y,
                          label = adf$country,
                          labelOptions = labelOptions(noHide = TRUE,
                                                      textsize = text_size,
                                                      textOnly = TRUE,
                                                      # direction = 'middle',
                                                      offset = c(-10,0),
                                                      opacity = 0.8))
  })
  
  # reactive object that filters by country
  all_country <- reactive({
    a_country <- country()
    sub_country <- df %>% dplyr::filter(country == a_country)
  })
  
  # reactive object that filters by country
  get_regions <- reactive({
    region_dat <- df %>% dplyr::select(sub_region, key, year, value)
  })
  
  # Digital financial services market table
  output$digital_financial_services_market_table <- 
    DT::renderDataTable({
      sub_dat <- all_country()
      # Keep only the relevant columns
      columns <- c('Registered MM Accounts',
                   'MM transaction volume',
                   'MM Transaction Value',
                   'Number of cards (debit+credit)',
                   'Card payment trans. Value',
                   'Internet banking Trans. Value')
      # Create left side of table (to ensure that the table is there even
      # if no values)
      left <- data_frame(key = columns)
      # Get country-specific values
      right <- sub_dat %>% 
        dplyr::filter(key %in% columns) %>%
        dplyr::distinct(key, value, .keep_all = TRUE) %>%
        dplyr::filter(!duplicated(key)) %>%
        dplyr::select(key, value)
      # Joined
      joined <- left_join(left, right,
                          by = 'key')
      joined$value <- round(joined$value)
      
      DT::datatable(joined,
                    colnames = c('', ''),
                    rownames = FALSE,
                    options=list(dom='t',
                                 ordering=F,
                                 pageLength = nrow(joined)))
    })
  
  # Financial access points table
  output$financial_access_points_table <- 
    DT::renderDataTable({
      sub_dat <- all_country()
      columns <- c('Registered MM agents',
                   'MM bank agents',
                   'MM non-bank agents',
                   'Bank branches',
                   'ATMs',
                   'POS')
      # Create left side of table (to ensure that the table is there even
      # if no values)
      left <- data_frame(key = columns)
      # Get country-specific values
      right <- sub_dat %>% 
        dplyr::filter(key %in% columns) %>%
        dplyr::distinct(key, value, .keep_all = TRUE) %>%
        dplyr::filter(!duplicated(key)) %>%
        dplyr::select(key, value)
      # Joined
      joined <- left_join(left, right,
                          by = 'key')
      
      DT::datatable(joined,
                    colnames = c('', ''),
                    rownames = FALSE,
                    options=list(dom='t',
                                 ordering=F,
                                 pageLength = nrow(joined)))
      
    })
  
  # create tables: (1) tab_mm_mkt which are the first two tables on page 5 - mobile market accounts and financial access points
  # and (2) the last two, the drivers of dfs growth
  
  # table 1
  output$tab_mm_mkt <- DT::renderDataTable({
    
    sub_dat <- all_country()
    # first create a table that has the variables they want and fill it with NAs. it also has a column of "better names" that will show up on the app once the data is merged
    new_name = c("Active MM Accounts", "MM Transaction Volume", "MM Transaction Value USD",'Credit Card Volume',
                 'Debit Card Volume', 'Total Credit Card Value (USD)', 'Total Debit Card Value (USD)', 'Total Credit Card Internet Value (USD)',
                 'Total Debit Card Internet Value (USD)', "Total MM Agents", "Number of Banks", "Number of ATMs", "Total Volume Debit Card POS", 
                 "Total Volume Credit Card POS", "Total Volume E Card POS") 
    
    dat_name = c("Mobile money accounts: active", "Mobile money transactions: number", "Mobile money transactions: value", "Volume Credit card", "Volume Debit card",
                 "Value in USD Credit card", "Value in USD Debit card", "Value in USD Credit card internet" , "Value in USD Debit card internet","Mobile money agent outlets:
                 registered", "Number of bank branches in Number", "Number of ATMs in NA","Volume Debit card pos", "Volume E money card pos", "Volume Credit card pos" )
    
    new_table = data.frame(dat_name, new_name)  
    
    
    sub_dat <- sub_dat %>% dplyr::filter(year <= as.numeric(format(Sys.Date(), '%Y')) - 1)
    
    # Keep only most recent year
    most_recent <- max(sub_dat$year, na.rm = TRUE)
    sub_dat <- sub_dat %>% filter(year == most_recent)
    
    # subset data frame by all the variable we need 
    var_string <- "Mobile money accounts: active|Mobile money transactions: number|Mobile money transactions: value|Volume Credit card|Volume Debit card|Value in USD Credit card|Value in USD Credit card|Value in USD Credit card internet|Value in USD Debit card internet|Mobile money agent outlets: registered|Number of bank branches in Number| Number of ATMs in NA|Volume Debit card pos|Volume E money card pos|Volume Credit card pos"
    
    # subset by var_string 
    sub_dat <- sub_dat[grepl(var_string, sub_dat$key),]
    
    # keep lastest available data - year already sorted, remove NAs, and remove duplicates which automatically remove the second duplcate
    sub_dat <- sub_dat[complete.cases(sub_dat),]
    sub_dat <- sub_dat[!duplicated(sub_dat$key),]
    
    # remove unneed colmns 
    sub_dat$iso2 <- sub_dat$sub_region <- sub_dat$country <- sub_dat$year <- NULL
  
    # left join sub_dat onto new_table, this way the variable will remain on the table just with NAs if not avaialble for country.
    final_table <- left_join(new_table, sub_dat, by = c("dat_name" = "key"))
    
    # rempove dat_name column and fill NA with "NA"
    final_table$dat_name <- NULL
    
    # remove NAs
    final_table <- final_table[!is.na(final_table$value),]
    # final_table[is.na(final_table)] <- 'NA'
    
    # round 
    final_table$value <- round(final_table$value, 2)
    
    DT::datatable(final_table, 
                  colnames = c('', ''),
                  rownames = FALSE)
    
  })
  
  # Note on Market penetration is the percentage of a target market that consumes a product or service. Market penetration can also be a measure of one company's sales as a percentage of all sales for a product. In a broad sense, market penetration is a measure of individuals in a target market who consume something versus those who do not. 
  # number of people who bought over population of country?
  
  # table 2
  output$tab_mm_pen <- DT::renderDataTable({
    sub_dat <- all_country()
    
    # USE ACTUAL VARIABLE NAMES INSTEAD OF GSUBED WITHOUT REGEX
    # first create a table that has the variables they want and fill it with NAs. it also has a column of "better names" that will show up on the app once the data is merged
    new_name = c("Total Population","Unique Mobile Phone Penetration", "Smartphone Penetration", "% of Adults with FI Account",'Tech Hubs', 
                 "GDP per capita", "Real GDP Growth Annual Percent Change", "Bank Assets/GDP", "# of Unbanked Adults", "Poverty gap at 1.90 per day",
                 "Adult Literacy rate") 
    
    dat_name = c("Population, total" ,"Q4 Percentage Unique Subscribers", "Q4 Percentage with Smart Phone", "Account at a financial institution % age 15 ts", 
                 "Tech Hubs", "GDP per capita, PPP current international $", "Real GDP Growth Annual Percent Change", 
                 "Assets \nas % of GDP in as % of GDP", "# of unbanked adults", "Poverty gap at $1.90 a day 2011 PPP %", 
                 "Literacy rate, adult total % of people ages 15 and above")
    
    new_table = data.frame(dat_name, new_name)  
    
    sub_dat <- sub_dat %>% dplyr::filter(year <= as.numeric(format(Sys.Date(), '%Y')) - 1)
    
    # Keep only most recent year
    most_recent <- max(sub_dat$year, na.rm = TRUE)
    sub_dat <- sub_dat %>% filter(year == most_recent)
    
    # remove Q1-Q3 in variables for smart phone subscribers
    sub_dat <- sub_dat %>% filter(!grepl('Q1|Q2|Q3', sub_dat$key)) 
    
    # subset data frame by all the variable we need 
    var_string <- "Population, total|Q4 Percentage Unique Subscribers|Q4 Percentage with Smart Phone|Account at a financial institution % age 15 ts|Tech Hubs|Population ages 0-14, total|GDP per capita, PPP current international $|Real GDP Growth Annual Percent Change|Assets \nas % of GDP in as % of GDP|# of unbanked adults|Poverty gap at $1.90 a day 2011 PPP %|Literacy rate, adult total % of people ages 15 and above"
    
    # subset by var_string 
    sub_dat <- sub_dat[grepl(var_string, sub_dat$key),]
    
    # remove unneed colmns 
    sub_dat$iso2 <- sub_dat$sub_region <- sub_dat$country <- sub_dat$year <- NULL
    
    # left join sub_dat onto new_table, this way the variable will remain on the table just with NAs if not avaialble for country.
    final_table <- left_join(new_table, sub_dat, by = c("dat_name" = "key"))
    
    # round 
    final_table$value <- round(final_table$value, 2)
    
    # rempove dat_name column and fill NA with "NA"
    final_table$dat_name <- NULL
    # remove NAs
    final_table <- final_table[!is.na(final_table$value),]
    # final_table[is.na(final_table)] <- 'NA'
    
    # round 
    final_table$value <- round(final_table$value, 2)

    DT::datatable(final_table, 
                  colnames = c('', ''),
                  rownames = FALSE)
    
  })
  
  output$plot_mm_mkt <- renderPlot({
    sub_dat <- all_country()
        # need % who have mm or fi account, percent who use mm, % who use mobile banking, % with debit cards, % with credit cards
    # the chart is kinda dumb if MM is the same as mobile money.
    
    # REGEX MESSES THIS UP - WILL NEED TO CHANGE
    vars <- c("Mobile account, income, poorest 40% % ages 15 w2" ,"Account at a financial institution % age 15 ts",
              "Mobile account, income, richest 60% % ages 15 w2","Mobile account, male % age 15 w2",
              "Mobile account, female % age 15 w2", "Debit card % age 15 ts",
              "Credit card % age 15 ts")
              
    sub_dat <- sub_dat %>% dplyr::filter(year <= as.numeric(format(Sys.Date(), '%Y')) -1)

    # keep only relevant indicators
    sub_dat <- sub_dat %>%
      filter(key %in% vars) %>%
      filter(!is.na(value))
    
    # Keep only most recent year
    most_recent <- max(sub_dat$year, na.rm = TRUE)
    sub_dat <- sub_dat %>% filter(year == most_recent)
    
    # Only plot if data available
    
    if(nrow(sub_dat) == 0){
      this_country <- country()
      ggplot() +
        theme_landscape() +
        labs(title = paste0('No data available for ', this_country))
      
    } else {
      # remove unneed colmns
      sub_dat <- sub_dat %>%
        dplyr::select(key, value)
      
      # round
      sub_dat$value <- round(sub_dat$value, 2)
      
      # Define colors
      cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(nrow(sub_dat))
      
      # Replace spaces in key with line breaks
      sub_dat$key <- gsub(' ', '\n', sub_dat$key)
      
      # plot
      ggplot(data = sub_dat,
             aes(x = key,
                 y = value)) +
        geom_bar(stat = 'identity',
                 aes(fill = key),
                 alpha = 0.7) +
        theme_landscape() +
        labs(x = '',
             y = '',
             title = '',
             subtitle = paste0('Data as of ', most_recent, ' (most recent available)')) +
        scale_fill_manual(name = '',
                          values = cols) +
        theme(axis.text.x = element_text(
                                         size = 12)) +
        theme(legend.position = 'none') 
    }
    
  })
  
  
  # plot_mm_trans
  
  output$plot_mm_trans <- renderPlot({
    
    options(scipen=999)
    
    # get data
    sub_dat <- all_country()
    
    # # instead of just doing credit card here, i should add all debit, credit, and e commerce, but for the sake of time doing this for now
    vars <- c( "Value in USD Mobile money", "Value in USD Atm", "Value in USD Cheques", "Value in USD Credit card",  "Value in USD Debit card",
               "Value in USD E money card", "Value in USD Credit card internet", "Value in USD Debit card internet","Value in USD E money card internet",
               "Value in USD Credit card pos", "Value in USD Debit card pos",  "Value in USD E money card pos")
                                                                               
            

    # filter by year
    sub_dat <- sub_dat %>% dplyr::filter(year < as.numeric(format(Sys.Date(), '%Y'))- 1)

    # keep only relevant indicators
    sub_dat <- sub_dat %>%
      filter(key %in% vars) %>%
      filter(!is.na(value))

    # Keep only most recent year
    most_recent <- suppressWarnings(max(sub_dat$year, na.rm = TRUE))
    sub_dat <- sub_dat %>% filter(year == most_recent)
    
    # Only plot if data available
    
    if(nrow(sub_dat) == 0){
      this_country <- country()
      ggplot() +
        theme_landscape() +
        labs(title = paste0('No data available for ', this_country))
      
    } else {
      # Data exists, so let's make a plot
      # remove unneed colmns
      sub_dat <- sub_dat %>%
        dplyr::select(key, value)
      
      cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(nrow(sub_dat))
      
      # Replace spaces in key with line breaks
      sub_dat$key <- gsub(' ', '\n', sub_dat$key)
      
      # round value 
      sub_dat$value <- round(sub_dat$value)
      
      # plot
      ggplot(data = sub_dat,
             aes(x = key,
                 y = value)) +
        geom_bar(stat = 'identity',
                 aes(fill = key)) +
        theme_landscape() +
        theme(legend.position = 'none') +
        labs(x = '',
             y = '',
             title = '',
             subtitle = paste0('Data as of ', most_recent, ' (most recent available)')) +
        scale_fill_manual(name = '',
                          values = cols) +
        theme(axis.text.x = element_text(
                                         size = 12)) 
    }
    
    
    
    
  })
  
  # plot_mm_ssa
  output$plot_mm_ssa <- renderPlot({
    
    # get region and data vars, and country name
    sub_dat <- all_country()
    region_dat <- get_regions()
    this_country <- country()
    
    # put country data into sub_region data for comparison and remove unneeded columns to match region_dat
    sub_dat$sub_region <- sub_dat$country
    sub_dat$country <- NULL
    sub_dat$iso2 <-NULL
    
    # combine region and country data for comparison
    region_dat <- bind_rows(region_dat,
                            sub_dat)
    
    # get variables
    vars <- c("Mobile account % age 15 w2" , "Mobile money balance value % of GDP", "Mobile money transactions: value % of GDP" )
    
    # keep only relevant indicators
    region_dat <- region_dat %>%
      filter(key %in% vars) %>%
      filter(!is.na(value))
    
    # subset for year
    region_dat <- region_dat %>% dplyr::filter(year < as.numeric(format(Sys.Date(), '%Y'))- 1)
    
    
    # Keep only most recent year
    most_recent <- suppressWarnings(max(region_dat$year, na.rm = TRUE))
    region_dat <- region_dat %>% filter(year == most_recent)
    
    

    # Only plot if data available and country still exists after subsetting for variables
    if(nrow(region_dat) == 0 | all(!grepl(this_country, region_dat$sub_region))){
      this_country <- country()
      ggplot() +
        theme_landscape() +
        labs(title = paste0('No data available for ', this_country))
      
    } else {
      
      # get the mean value for each region and country
      region_dat <- region_dat %>% group_by(sub_region,key) %>% summarise(mean_value = mean(value, na.rm = T))
      
      # get colors based on sub region
      cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(length(unique(region_dat$sub_region)))
      
      # Replace spaces in key with line breaks
      region_dat$key <- gsub(' ', '\n', region_dat$key)
      
      # make country the ref level, so it shows up on top for legend
      region_dat$sub_region <- relevel(factor(region_dat$sub_region),ref = this_country)
      
      # round value 
      region_dat$mean_value <- round(region_dat$mean_value)
      
      # plot
      ggplot(data = region_dat,
             aes(x = key,
                 y = mean_value)) +
        geom_bar(stat = 'identity', position = 'dodge',
                 aes(fill = sub_region)) +
        theme_landscape() +
        labs(x = '',
             y = '',
             title = '',
             subtitle = paste0('Data as of ', most_recent, ' (most recent available)')) +
        scale_fill_manual(name = '',
                          values = cols) +
        theme(axis.text.x = element_text(
                                         size = 12)) 
    }
    
    
  })
  
  
  
  # plot_mm_ssa
  output$plot_mm_ssa_new <- renderPlot({
    
    # ANY REGION ANALYSIS SHOULD TECHNICALLY HAVE THE COUNTRY CHOSEN REMOVED FROM THE REGION DATA.
    # get region and data vars, and country name
    sub_dat <- all_country()
    region_dat <- get_regions()
    this_country <- country()
    
    # put country data into sub_region data for comparison and remove unneeded columns to match region_dat
    sub_dat$sub_region <- sub_dat$country
    sub_dat$country <- NULL
    sub_dat$iso2 <-NULL
    
    # combine region and country data for comparison
    region_dat <- bind_rows(region_dat,
                            sub_dat)
    # get vars
    vars <- c("Average transaction size in USD Atm" , "Average transaction size in USD Credit card", "Average transaction size in USD Debit card", 
              "Average transaction size in USD E money", "Average transaction size in USD Online money", "Average transaction size in USD Mobile money")
    
    # keep only relevant indicators
    region_dat <- region_dat %>%
      filter(key %in% vars) %>%
      filter(!is.na(value))
    
    # subset by year
    region_dat <- region_dat %>% dplyr::filter(year < as.numeric(format(Sys.Date(), '%Y'))- 1)
    
    
    # Keep only most recent year
    most_recent <- suppressWarnings(max(region_dat$year, na.rm = TRUE))
    region_dat <- region_dat %>% filter(year == most_recent)
  
    # Only plot if data available and if country still exists after subsetting by variables
    if(nrow(region_dat) == 0 | all(!grepl(this_country, region_dat$sub_region))){
      this_country <- country()
      ggplot() +
        theme_landscape() +
        labs(title = paste0('No data available for ', this_country))
      
    } else {
      
      # get the mean value for comparison
      region_dat <- region_dat %>% group_by(sub_region,key) %>% summarise(mean_value = mean(value, na.rm = T)) %>%
        ungroup %>%
        mutate(key = gsub('Average', 'Avg.', key)) %>%
        mutate(key = gsub('transaction', 'trans.', key))
      
      # get colors based on sub_regions for legend
      cols <- colorRampPalette(brewer.pal(n = 8, 'Spectral'))(length(unique(region_dat$sub_region)))
      
      # Replace spaces in key with line breaks
      region_dat$key <- gsub(' ', '\n', region_dat$key)
      
      # relevel so country shouws up on top of legend
      region_dat$sub_region <- relevel(factor(region_dat$sub_region), ref = this_country)
      
      # round value 
      region_dat$mean_value <- round(region_dat$mean_value)
      
      # plot
      ggplot(data = region_dat,
             aes(x = key,
                 y = mean_value)) +
        geom_bar(stat = 'identity', position = 'dodge',
                 aes(fill = sub_region)) +
        theme_landscape() +
        labs(x = '',
             y = '',
             title = '',
             subtitle = paste0('Data as of ', most_recent, ' (most recent available)')) +
        scale_fill_manual(name = '',
                          values = cols) +
        theme(axis.text.x = element_text(
                                         size = 12)) 
    }
  })
  
  output$qualitative_overview <-
    DT::renderDataTable({
      this_country <- country()
      x <- df_qualy %>%
        filter(country == this_country) %>%
        dplyr::select(key, value) %>% 
        dplyr::filter(!is.na(value))    
      DT::datatable(x, rownames = NULL)
      })
  
  output$glossary_table <- 
    DT::renderDataTable({
      prettify_scroll(glossary,
                    scroll_x = TRUE,
                    download_options = TRUE)
    })
  
  output$country_analysis_indicator_ui <-
    renderUI({
      years <- input$country_analysis_country_year
      the_country <- input$country_analysis_country
      if(is.null(years)){
        years <- range(df$year, na.rm = TRUE)
      }
      available_indicators <- okay_indicators_country %>% 
        filter(year >= years[1],
               year <= years[2]) %>%
        filter(country %in% the_country) %>%
        .$key %>% unlist
      available_indicators <- sort(unique(available_indicators))
      
      
      selectInput('country_analysis_indicator',
                  'Indicators',
                  choices = available_indicators,
                  selected = available_indicators[1],
                  multiple = TRUE)
    })
  
  # Country analysis plot
  output$country_analysis_plot <-
    renderPlot({
      # Get data for country  
      data <- all_country()
      # Get filtered years
      years <- input$country_analysis_country_year
      the_country <- input$country_analysis_country
      if(is.null(years)){
        years <- range(df$year, na.rm = TRUE)
      }
      # Filter for only years in question
      data <- data %>%
        filter(year >= years[1],
               year <= years[2]) 
      # Get chart type
      chart_type <- input$country_analysis_chart_type
      
      # Get indicator
      indicator <- input$country_analysis_indicator
      if(is.null(indicator) | length(indicator) > 3 | length(indicator) < 1){
        # Too many indicators, spit back empty chart
        ggplot() +
          theme_landscape() +
          labs(title = 'Select 1-3 indicators')
      } else if(nrow(data) == 0)  {
        # No data, just return empty chart
        ggplot() +
          theme_landscape() +
          labs(title = 'No data available')
      } else {
        # Data is good, reshape for plot
        plot_data <- data %>%
          filter(key %in% indicator)
        n_indicators <- length(unique(plot_data$key))
        cols <- colorRampPalette(brewer.pal(n = 8, name = 'Spectral'))(n_indicators)
        if(chart_type == 'Bars'){
          ggplot(data = plot_data) +
            geom_bar(aes(x = year,
                         y = value,
                         fill = key),
                     stat = 'identity',
                     position = 'dodge',
                     alpha = 0.6,
                     color = 'black') +
            theme_landscape() +
            theme(legend.position = 'bottom',
                  legend.background = element_rect(fill = '#ecf0f5',
                                                   color = '#ecf0f5')) +
            scale_fill_manual(name = '',
                              values = cols) +
            labs(x = 'Year',
                 y = '')
        } else if(chart_type == 'Points'){
          if(n_indicators == 1){
            ggplot(data = plot_data,
                   aes(x = country,
                       y = value)) +
              geom_point() +
              theme_landscape() +
              labs(x = ' ', y = '') +
              facet_wrap(~year)
          } else if(n_indicators == 2){
            wide <- plot_data %>%
              spread(key = key, value = value)
            
            labs <- names(wide)[5:6]
            names(wide)[5:6] <- c('x', 'y')
            ggplot(data = wide,
                   aes(x = x,
                       y = y)) +
              geom_point() +
              facet_wrap(~year) +
              labs(x = labs[1],
                   y = labs[2]) +
              geom_smooth() +
              theme_landscape()
            
          } else if(n_indicators == 3){
            wide <- plot_data %>%
              spread(key = key, value = value)
            
            labs <- names(wide)[5:7]
            names(wide)[5:7] <- c('x', 'y', 'z')
            ggplot(data = wide,
                   aes(x = x,
                       y = y,
                       color = z)) +
              geom_point() +
              facet_wrap(~year) +
              labs(x = labs[1],
                   y = labs[2]) +
              geom_smooth() +
              theme_landscape() +
              scale_color_continuous(name = labs[3],
                                     low = 'darkgreen',
                                     high = 'red') +
              theme(legend.position = 'bottom',
                    legend.background = element_rect(fill = '#ecf0f5',
                                                     color = '#ecf0f5'))
            
          }
        } else if(chart_type == 'Lines'){
          ggplot(data = plot_data) +
            geom_point(aes(x = year,
                          y = value,
                          color = key,
                          group = key),
                      alpha = 0.9,
                      size = 2) +
            geom_line(aes(x = year,
                         y = value,
                         color = key,
                         group = key),
                     alpha = 1,
                     size = 2) +
            theme_landscape() +
            theme(legend.position = 'bottom',
                  legend.background = element_rect(fill = '#ecf0f5',
                                                   color = '#ecf0f5')) +
            scale_color_manual(name = '',
                              values = cols) +
            labs(x = 'Year',
                 y = '')
        }
      } 
    })
  
  output$drivers_of_dfs_growth_table <- 
    DT::renderDataTable({
      
      a_country <- country()
      sub_dat <- all_country()
      keys <- c('Regulatory environment',
                'Unique mobile penetration',
                'Smartphone penetration',
                '% of adults with FI account(1)',
                'Tech Hubs')
      out <- sub_dat %>%
        filter(year <= as.numeric(format(Sys.Date(), '%Y'))) %>%
        filter(key %in% keys) %>%
        dplyr::select(key, value) %>%
        distinct(key, .keep_all = TRUE)
      out$value <- as.character(out$value)
      
      # Get 2020 data
      out2020 <- sub_dat %>%
        filter(year == 2020) %>%
        filter(key %in% keys) %>%
        dplyr::select(key, value) %>%
        distinct(key, value)
      out2020$`2020` <- as.character(out2020$value)
      out2020$value <- NULL
      
      x <- data_frame(key = keys[1],
                      value = qualy$value[qualy$key == 'Regulatory Environment' & 
                                        qualy$country == a_country])
      out <- bind_rows(x,
                       out)
      
      # Get a left side
      left <- data_frame(key = keys)
      # Join together
      joined <- left_join(left, out,
                          by = 'key')
      # Join with 2020 stuff
      joined <- left_join(joined,
                          out2020,
                          by = 'key')

      DT::datatable(joined,
                    colnames = c('', '', '2020'),
                    rownames = FALSE,
                    options=list(dom='t',
                                 ordering=F,
                                 pageLength = nrow(joined)))
    })
  
  output$macro_drivers_of_growth_table <- 
    DT::renderDataTable({
      sub_dat <- all_country()
      # Keep only the relevant columns
      columns <- c('Adult population',
                   'GDP per capita (PPP)',
                   'GDP growth forecast (20)',
                   'Bank assets/GDP',
                   'Nu of unbanked',
                   '% of population living below $1.9 PPP',
                   'Share of urban population',
                    'Literacy rate')
      # Filter down to keep only relevant columns
      # Create left side of table (to ensure that the table is there even
      # if no values)
      left <- data_frame(key = columns)
      # Get country-specific values
      right <- sub_dat %>% 
        dplyr::filter(key %in% columns) %>%
        dplyr::distinct(key, value, .keep_all = TRUE) %>%
        dplyr::filter(!duplicated(key)) %>%
        dplyr::select(key, value)
      # Get 2020 growth forecast
      growth <- 
        sub_dat %>%
        filter(year == 2020) %>%
        filter(key == 'Real GDP Growth Annual Percent Change') %>%
        dplyr::select(key, value)
      if(nrow(growth) >= 1){
        growth <- growth %>%
          mutate(key = 'GDP growth forecast (20)')
        growth <- growth[1,]
      }
        
      right <- bind_rows(right, growth)
      # Joined
      joined <- left_join(left, right,
                          by = 'key')
      joined$value <- round(joined$value, digits = 1)
      
      DT::datatable(joined,
                    colnames = c('', ''),
                    rownames = FALSE,
                    options=list(dom='t',
                                 ordering=F,
                                 pageLength = nrow(joined)))
    })
  
  output$country_text <- 
    renderText({
      the_country <- country()
      paste0('Country: ', the_country)
    })
  output$region_text <-
    renderText({
      the_country <- country()
      the_region <- africa@data$sub_region[africa@data$country == the_country]
      the_region <- the_region[1]
      paste0('Region: ', the_region)
    })
  
  output$level_of_dfs_market_development <- renderPlot({
    # Get data for first plot
    sub_dat <- all_country()
    # Define indicators
    keys <- c('MM or FI account',
              'Mobile money',
              'Mobile banking',
              'Debit cards',
              'Credit cards')
    right <- 
      sub_dat %>%
      filter(!is.na(value)) %>%
      filter(key %in% keys) %>%
      dplyr::distinct(key, value)
    left <- data_frame(key = keys)
    joined <- left_join(left,
                        right,
                        by = 'key')
    joined$key <- gsub(' ', '\n', joined$key)
    joined$key <- factor(joined$key, levels = unique(joined$key))
    g1 <- ggplot(data = joined,
           aes(x = key,
               y = value)) +
      geom_bar(stat = 'identity',
               alpha = 0.7,
               fill = 'darkblue') +
      theme_landscape() +
      labs(x = '', y = '',
           title = 'DFS penetration: % of adults who use...') #+
      # theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    
    g2 <- 
      ggplot(data = data.frame(x = c('Mobile money',
                                     'Internet banking',
                                     'E-commerce (cards)',
                                     'POS (cards)',
                                     'Debit cheques',
                                     'RTGS'),
                               key = rep(c('Value (USD Bio)', 'Volume (Mio)'), each=2),
                               y = rnorm(n = 12, mean = 1000, sd = 200)),
             aes(x = x,
                 y = y,
                 group = key,
                 fill = key)) +
      geom_bar(stat = 'identity',
               alpha = 0.7,
               position = 'dodge') +
      theme_landscape() +
      labs(x = '', y = '',
           title = 'Transactions',
           subtitle = '(Just a placeholder chart; not yet imlemented)') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(name = '',
                        values = c('darkorange', 'blue')) 
    
    Rmisc::multiplot(g1, g2)
  })
  
  output$level_of_dfs_market_development2 <- renderPlot({
    ggplot(data = data.frame(x = c('MM account penetration (adults) %',
                                     'MM transactions per 1000 adults (thousands)',
                                     'MM trans value/GDP (%)',
                                     'Value per transaction (USD)',
                                     'Transaction value per account (USD)'),
                               key = rep(c('Kenya',
                                           'Average SSA', 
                                           'Highest SSA'), each=5),
                               y = rnorm(n = 15, mean = 1000, sd = 200)) %>%
             mutate(x = gsub(' ', '\n', x)),
             aes(x = x,
                 y = y,
                 group = key,
                 fill = key)) +
      geom_bar(stat = 'identity',
               alpha = 0.7,
               position = 'dodge') +
      theme_landscape() +
      labs(x = '', y = '',
           title = 'Relative Level of Mobile Money Market Development',
           subtitle = '(Just a placeholder chart; not yet imlemented)') +
      # theme(axis.text.x = element_text(angle = 45, hjust = 1,
      #                                  size = 9)) +
      scale_fill_manual(name = '',
                        values = c('darkorange', 'blue', 'grey')) 
  })
  
  output$map_ui <-
    renderUI({
      ok <- input$dfs_market_overview_view == 'Map view'
      if(ok){
        radioButtons('circles',
                     'Map type',
                     choices = c('Choropleth',
                                 'Circles'))
      } else {
        NULL
      }
    })
}

shinyApp(ui, server)