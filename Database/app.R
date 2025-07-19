# NASCAR Unified Analytics Platform
# Combines database explorer, dashboard generation, and lap-by-lap analysis

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(shinycssloaders)
library(shinyWidgets)
library(jsonlite)
library(tidyr)
library(shinyjs)
library(openxlsx)
library(flexdashboard)

#--------------------- Data Loading Functions ---------------------#

# Load main NASCAR database
load_nascar_database <- function() {
  if (file.exists("NascarDatabase.csv")) {
    return(read_csv("NascarDatabase.csv"))
  } else {
    return(NULL)
  }
}

# Load RaceIDs
load_race_ids <- function() {
  if (file.exists("RaceIDs.xlsx")) {
    return(read_excel("RaceIDs.xlsx"))
  } else {
    return(NULL)
  }
}

# NASCAR API functions (from lap analyzer)
get_season_races <- function(season) {
  url <- paste0("https://cf.nascar.com/cacher/", season, "/race_list_basic.json")
  
  tryCatch({
    json_data <- fromJSON(url)
    
    cup_races <- json_data$series_1 %>%
      select(race_id, race_name, track_name) %>%
      mutate(series_id = 1, series_name = "Cup Series", season = season)
    
    xfinity_races <- json_data$series_2 %>%
      select(race_id, race_name, track_name) %>%
      mutate(series_id = 2, series_name = "Xfinity Series", season = season)
    
    truck_races <- json_data$series_3 %>%
      select(race_id, race_name, track_name) %>%
      mutate(series_id = 3, series_name = "Truck Series", season = season)
    
    return(rbind(cup_races, xfinity_races, truck_races))
  }, error = function(e) {
    message("Error retrieving season races:", e$message)
    return(NULL)
  })
}

get_lap_data <- function(race_season, race_id, series_id) {
  url <- paste0('https://cf.nascar.com/cacher/', race_season, '/', series_id, '/', race_id, '/lap-times.json')
  
  tryCatch({
    json_data <- fromJSON(url)
    
    flag_status <- json_data$flags %>%
      mutate(
        FlagState = as.numeric(FlagState),
        LapsCompleted = as.numeric(LapsCompleted),
        IsCaution = FlagState %in% c(2, 8)
      ) %>%
      select(LapsCompleted, FlagState, IsCaution)
    
    drivers <- json_data$laps %>%
      rename(FinishingPosition = RunningPos)
    
    return(list(
      drivers = drivers,
      flag_status = flag_status,
      race_id = race_id,
      series_id = series_id,
      race_season = race_season
    ))
  }, error = function(e) {
    message("Error retrieving lap data:", e$message)
    return(NULL)
  })
}

#--------------------- UI Definition ---------------------#

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "NASCAR Analytics Platform",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Data Explorer", tabName = "data_explorer", icon = icon("table")),
      menuItem("Dashboard Generator", tabName = "dashboard_gen", icon = icon("chart-bar")),
      menuItem("Lap Analysis", tabName = "lap_analysis", icon = icon("tachometer-alt")),
      menuItem("Driver Analysis", tabName = "driver_analysis", icon = icon("user")),
      menuItem("Track Analysis", tabName = "track_analysis", icon = icon("road")),
      menuItem("Team Performance", tabName = "team_analysis", icon = icon("users")),
      menuItem("Season Trends", tabName = "season_trends", icon = icon("calendar")),
      menuItem("Custom Analytics", tabName = "custom_analytics", icon = icon("cogs")),
      menuItem("Data Export", tabName = "data_export", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f4f4f4; }
        .box { border-radius: 3px; box-shadow: 0 1px 3px rgba(0,0,0,.12); }
        .box:hover { box-shadow: 0 3px 6px rgba(0,0,0,.16); }
        .dataTable th { background-color: #f0f0f0; }
        .loading-spinner { 
          display: inline-block; width: 20px; height: 20px; 
          border: 3px solid rgba(0,0,0,0.1); border-radius: 50%; 
          border-top-color: #0078d4; animation: spin 1s ease-in-out infinite; 
        }
        @keyframes spin { to { transform: rotate(360deg); } }
      "))
    ),
    
    tabItems(
      # Data Explorer Tab - Optimized for database only
      tabItem(tabName = "data_explorer",
              fluidRow(
                box(
                  title = "Data Filters", status = "primary", solidHeader = TRUE, width = 12,
                  collapsible = TRUE,
                  fluidRow(
                    column(2, 
                           selectizeInput("season_filter", "Season:", 
                                          choices = NULL, multiple = TRUE,
                                          options = list(placeholder = "All Seasons"))
                    ),
                    column(2, 
                           selectizeInput("driver_filter", "Driver:", 
                                          choices = NULL, multiple = TRUE,
                                          options = list(placeholder = "All Drivers"))
                    ),
                    column(2, 
                           selectizeInput("track_filter", "Track:", 
                                          choices = NULL, multiple = TRUE,
                                          options = list(placeholder = "All Tracks"))
                    ),
                    column(2, 
                           selectizeInput("team_filter", "Team:", 
                                          choices = NULL, multiple = TRUE,
                                          options = list(placeholder = "All Teams"))
                    ),
                    column(2, 
                           selectizeInput("series_filter", "Series:", 
                                          choices = c("Cup Series" = 1, "Xfinity Series" = 2, "Truck Series" = 3),
                                          multiple = TRUE,
                                          options = list(placeholder = "All Series"))
                    ),
                    column(2, 
                           actionButton("reset_filters", "Reset All Filters", 
                                        class = "btn-warning", style = "margin-top: 25px;"))
                  ),
                  fluidRow(
                    column(9,
                           tags$div(
                             style = "margin-top: 10px;",
                             span(id = "filter_count", "Filters applied: 0", 
                                  style = "font-weight: bold; color: #337ab7; font-size: 16px;")
                           )
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Column Configuration", status = "info", solidHeader = TRUE, width = 12,
                  collapsible = TRUE, collapsed = FALSE,
                  fluidRow(
                    column(12,
                           tags$p("Drag columns to reorder, uncheck to hide:"),
                           uiOutput("column_selector_sortable")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "NASCAR Data Explorer", status = "primary", solidHeader = TRUE, width = 12,
                  withSpinner(DT::dataTableOutput("main_explorer_table")),
                  br(),
                  fluidRow(
                    column(3, downloadButton("download_filtered_csv", "Download CSV", class = "btn-success")),
                    column(3, downloadButton("download_filtered_excel", "Download Excel", class = "btn-success")),
                    column(3, actionButton("create_custom_chart", "Create Chart", class = "btn-info")),
                    column(3, actionButton("generate_dashboard", "Generate Dashboard", class = "btn-warning"))
                  )
                )
              )
      ),
      
      # Dashboard Generator Tab
      tabItem(tabName = "dashboard_gen",
              fluidRow(
                box(
                  title = "Dashboard Configuration", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("dash_track", "Primary Track:", choices = NULL),
                  selectInput("dash_similar_tracks", "Similar Tracks:", choices = NULL, multiple = TRUE),
                  numericInput("dash_start_year", "Start Year:", value = 2022, min = 2020, max = 2025),
                  numericInput("dash_end_year", "End Year:", value = 2025, min = 2020, max = 2025),
                  selectInput("dash_series", "Series:", 
                              choices = c("Cup Series" = 1, "Xfinity Series" = 2, "Truck Series" = 3),
                              selected = 1),
                  numericInput("dash_race_id", "Race ID (for entry list):", value = 5556),
                  br(),
                  actionButton("generate_dashboard_btn", "Generate Dashboard", 
                               class = "btn-primary btn-lg", icon = icon("chart-bar"))
                ),
                
                box(
                  title = "Dashboard Preview", status = "info", solidHeader = TRUE, width = 8,
                  uiOutput("dashboard_preview")
                )
              ),
              
              fluidRow(
                conditionalPanel(
                  condition = "input.generate_dashboard_btn > 0",
                  box(
                    title = "Generated Dashboard", status = "success", solidHeader = TRUE, width = 12,
                    height = "800px",
                    withSpinner(uiOutput("generated_dashboard"))
                  )
                )
              )
      ),
      
      # Lap Analysis Tab
      tabItem(tabName = "lap_analysis",
              fluidRow(
                box(
                  title = "Race Selection for Lap Analysis", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("lap_season", "Season:", choices = c(2025:2020), selected = 2025),
                  selectInput("lap_series", "Series:", 
                              choices = c("Cup Series" = 1, "Xfinity Series" = 2, "Truck Series" = 3), 
                              selected = 1),
                  uiOutput("lap_race_selector"),
                  actionButton("load_lap_data", "Load Lap Data", class = "btn-primary")
                ),
                
                box(
                  title = "Lap Analysis Options", status = "info", solidHeader = TRUE, width = 6,
                  conditionalPanel(
                    condition = "input.load_lap_data > 0",
                    selectInput("lap_drivers", "Select Drivers:", choices = NULL, multiple = TRUE),
                    numericInput("fantasy_lap", "Fantasy Scoring at Lap:", value = 100, min = 1),
                    checkboxInput("show_cautions", "Show Caution Periods", value = TRUE),
                    checkboxInput("show_fastest_laps", "Show Fastest Laps", value = TRUE)
                  )
                )
              ),
              
              conditionalPanel(
                condition = "input.load_lap_data > 0",
                fluidRow(
                  box(
                    title = "Running Position Chart", status = "primary", solidHeader = TRUE, width = 12,
                    withSpinner(plotlyOutput("lap_position_chart", height = "500px"))
                  )
                ),
                
                fluidRow(
                  box(
                    title = "Fastest Lap Analysis", status = "primary", solidHeader = TRUE, width = 6,
                    withSpinner(plotlyOutput("lap_fastest_analysis", height = "400px"))
                  ),
                  box(
                    title = "Fantasy Scoring", status = "success", solidHeader = TRUE, width = 6,
                    withSpinner(DT::dataTableOutput("lap_fantasy_table"))
                  )
                )
              )
      ),
      
      # Driver Analysis Tab
      tabItem(tabName = "driver_analysis",
              fluidRow(
                box(
                  title = "Driver Performance Analysis", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("analysis_drivers", "Select Drivers (up to 10):", 
                              choices = NULL, multiple = TRUE),
                  selectInput("analysis_season", "Season:", choices = NULL),
                  selectInput("analysis_tracks", "Tracks:", choices = NULL, multiple = TRUE),
                  selectInput("driver_metric", "Primary Metric:",
                              choices = c("Average Finish" = "avg_finish",
                                          "Top 5 Rate" = "top5_rate", 
                                          "Top 10 Rate" = "top10_rate",
                                          "Laps Led" = "laps_led",
                                          "DK Points" = "dk_points",
                                          "FD Points" = "fd_points")),
                  checkboxInput("show_trend_analysis", "Show Trend Lines", value = TRUE)
                ),
                
                box(
                  title = "Performance Comparison", status = "primary", solidHeader = TRUE, width = 8,
                  withSpinner(plotlyOutput("driver_comparison_plot", height = "400px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Driver Statistics", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(DT::dataTableOutput("driver_stats_detailed"))
                ),
                box(
                  title = "Head-to-Head Comparison", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("driver_head_to_head", height = "400px"))
                )
              )
      ),
      
      # Track Analysis Tab
      tabItem(tabName = "track_analysis",
              fluidRow(
                box(
                  title = "Track Analysis Controls", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("track_analysis_track", "Select Track:", choices = NULL),
                  selectInput("track_analysis_seasons", "Seasons:", choices = NULL, multiple = TRUE),
                  selectInput("track_metric", "Analysis Metric:",
                              choices = c("Average Speed" = "avg_speed",
                                          "Number of Leaders" = "leaders",
                                          "Cautions" = "cautions",
                                          "Dominator Points" = "dominator",
                                          "Fantasy Scoring" = "fantasy")),
                  checkboxInput("compare_by_year", "Compare by Year", value = TRUE)
                ),
                
                box(
                  title = "Track Performance Trends", status = "primary", solidHeader = TRUE, width = 8,
                  withSpinner(plotlyOutput("track_analysis_plot", height = "400px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Track Winners Analysis", status = "success", solidHeader = TRUE, width = 6,
                  withSpinner(DT::dataTableOutput("track_winners_analysis"))
                ),
                box(
                  title = "Track Characteristics", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(plotlyOutput("track_characteristics", height = "400px"))
                )
              )
      ),
      
      # Team Analysis Tab  
      tabItem(tabName = "team_analysis",
              fluidRow(
                box(
                  title = "Team Performance Analysis", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("analysis_teams", "Select Teams:", choices = NULL, multiple = TRUE),
                  selectInput("team_season", "Season:", choices = NULL),
                  radioButtons("team_view", "Analysis Type:",
                               choices = c("Wins & Top 5s" = "wins_top5",
                                           "Average Finish" = "avg_finish",
                                           "Fantasy Performance" = "fantasy",
                                           "Consistency" = "consistency")),
                  checkboxInput("include_team_comparison", "Multi-Team Comparison", value = TRUE)
                ),
                
                box(
                  title = "Team Performance Chart", status = "primary", solidHeader = TRUE, width = 8,
                  withSpinner(plotlyOutput("team_analysis_plot", height = "400px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Team Summary Statistics", status = "info", solidHeader = TRUE, width = 12,
                  withSpinner(DT::dataTableOutput("team_summary_detailed"))
                )
              )
      ),
      
      # Season Trends Tab
      tabItem(tabName = "season_trends",
              fluidRow(
                box(
                  title = "Season Trends Analysis", status = "primary", solidHeader = TRUE, width = 4,
                  checkboxGroupInput("season_metrics", "Select Metrics:",
                                     choices = c("Average Speed" = "avg_speed",
                                                 "Field Size" = "field_size",
                                                 "Race Leaders" = "leaders",
                                                 "Cautions" = "cautions",
                                                 "Fantasy Scoring" = "fantasy"),
                                     selected = "avg_speed"),
                  sliderInput("season_range", "Season Range:",
                              min = 2020, max = 2025, value = c(2022, 2025), step = 1),
                  selectInput("trend_tracks", "Track Filter:", choices = NULL, multiple = TRUE),
                  checkboxInput("normalize_metrics", "Normalize Metrics", value = FALSE)
                ),
                
                box(
                  title = "Season Trends Visualization", status = "primary", solidHeader = TRUE, width = 8,
                  withSpinner(plotlyOutput("season_trends_plot", height = "400px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Championship Analysis", status = "success", solidHeader = TRUE, width = 12,
                  withSpinner(DT::dataTableOutput("championship_analysis"))
                )
              )
      ),
      
      # Custom Analytics Tab
      tabItem(tabName = "custom_analytics",
              fluidRow(
                box(
                  title = "Custom Analytics Builder", status = "primary", solidHeader = TRUE, width = 4,
                  h4("Data Selection"),
                  selectInput("custom_groupby", "Group By:",
                              choices = c("Driver" = "Full_Name",
                                          "Team" = "team_name", 
                                          "Track" = "track_name",
                                          "Season" = "race_season")),
                  
                  selectInput("custom_metrics", "Metrics:",
                              choices = c("Races" = "races",
                                          "Wins" = "wins",
                                          "Top 5s" = "top5s", 
                                          "Top 10s" = "top10s",
                                          "Average Finish" = "avg_finish",
                                          "Laps Led" = "laps_led",
                                          "Fantasy Points" = "fantasy_points"),
                              multiple = TRUE, selected = c("races", "wins")),
                  
                  h4("Filters"),
                  selectInput("custom_season_filter", "Season:", choices = NULL, multiple = TRUE),
                  selectInput("custom_track_filter", "Track:", choices = NULL, multiple = TRUE),
                  numericInput("custom_min_races", "Minimum Races:", value = 1, min = 1),
                  
                  h4("Visualization"),
                  selectInput("custom_chart_type", "Chart Type:",
                              choices = c("Bar Chart" = "bar",
                                          "Line Chart" = "line",
                                          "Scatter Plot" = "scatter",
                                          "Box Plot" = "box")),
                  
                  br(),
                  actionButton("generate_custom", "Generate Analysis", class = "btn-success"),
                  br(), br(),
                  downloadButton("download_custom", "Download Results", class = "btn-primary")
                ),
                
                box(
                  title = "Custom Analysis Results", status = "primary", solidHeader = TRUE, width = 8,
                  withSpinner(DT::dataTableOutput("custom_analysis_table"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Custom Visualization", status = "info", solidHeader = TRUE, width = 12,
                  withSpinner(plotlyOutput("custom_analysis_plot", height = "500px"))
                )
              )
      ),
      
      # Data Export Tab
      tabItem(tabName = "data_export",
              fluidRow(
                box(
                  title = "Export Configuration", status = "primary", solidHeader = TRUE, width = 6,
                  h4("Data Selection"),
                  checkboxGroupInput("export_data_types", "Include Data Types:",
                                     choices = c("Race Results" = "results",
                                                 "Driver Statistics" = "drivers", 
                                                 "Team Performance" = "teams",
                                                 "Track Analysis" = "tracks",
                                                 "Fantasy Scoring" = "fantasy",
                                                 "Lap-by-Lap Data" = "laps"),
                                     selected = c("results", "drivers")),
                  
                  h4("Export Options"),
                  selectInput("export_format", "Export Format:",
                              choices = c("Excel Workbook (.xlsx)" = "excel",
                                          "CSV Files (ZIP)" = "csv",
                                          "JSON Format" = "json")),
                  
                  checkboxInput("export_include_charts", "Include Charts (Excel only)", value = TRUE),
                  checkboxInput("export_summary_stats", "Include Summary Statistics", value = TRUE),
                  
                  br(),
                  downloadButton("export_complete_data", "Export Data Package", 
                                 class = "btn-success btn-lg", icon = icon("download"))
                ),
                
                box(
                  title = "Export Preview", status = "info", solidHeader = TRUE, width = 6,
                  h4("Data Summary"),
                  verbatimTextOutput("export_summary"),
                  h4("Recent Exports"),
                  DT::dataTableOutput("export_history")
                )
              )
      )
    )
  )
)

#--------------------- Server Function ---------------------#

server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    nascar_data = NULL,
    race_ids = NULL,
    filtered_data = NULL,
    lap_data = NULL,
    custom_analysis = NULL
  )
  
  # Load initial data - optimized for performance
  observe({
    withProgress(message = 'Loading NASCAR Database...', {
      values$nascar_data <- load_nascar_database()
      values$race_ids <- load_race_ids()
      
      if (!is.null(values$nascar_data)) {
        # Cache unique values for filters to improve performance
        incProgress(0.3, detail = "Processing seasons...")
        seasons <- sort(unique(values$nascar_data$race_season), decreasing = TRUE)
        
        incProgress(0.5, detail = "Processing drivers...")
        drivers <- sort(unique(values$nascar_data$Full_Name[!is.na(values$nascar_data$Full_Name)]))
        
        incProgress(0.7, detail = "Processing tracks...")
        tracks <- sort(unique(values$nascar_data$track_name[!is.na(values$nascar_data$track_name)]))
        
        incProgress(0.8, detail = "Processing teams...")
        teams <- sort(unique(values$nascar_data$team_name[!is.na(values$nascar_data$team_name)]))
        
        incProgress(0.9, detail = "Processing finishing status...")
        statuses <- sort(unique(values$nascar_data$finishing_status[!is.na(values$nascar_data$finishing_status)]))
        
        # Update filter choices (removed status filter)
        updateSelectizeInput(session, "season_filter", choices = seasons)
        updateSelectizeInput(session, "driver_filter", choices = drivers)
        updateSelectizeInput(session, "track_filter", choices = tracks)
        updateSelectizeInput(session, "team_filter", choices = teams)
        
        incProgress(1.0, detail = "Complete!")
      }
    })
  })
  
  # Column selector with sortable interface
  output$column_selector_sortable <- renderUI({
    req(values$nascar_data)
    
    # Specific column order and display names as requested
    default_columns <- c(
      "Full_Name" = "Driver Name",
      "start_ps" = "Start",
      "ps" = "Finish", 
      "ARP" = "ARP",
      "SpdRk" = "Speed Rank",
      "fl" = "FL",
      "ll" = "LL",
      "DKSP" = "DK Speed Points",
      "FDSP" = "FD Speed Points",
      "DKDomRank" = "DK Dom Rank",
      "FDDomRank" = "FD Dom Rank",
      "car_number" = "Car Number",
      "team_name" = "Team",
      "finishing_status" = "Finishing Status",
      "LapsDown" = "Laps Down",
      "race_season" = "Season",
      "track_name" = "Track"
    )
    
    # Filter to only include columns that exist in the data
    available_columns <- default_columns[names(default_columns) %in% names(values$nascar_data)]
    
    # Initialize column order and selection if not set
    if (is.null(values$column_order)) {
      values$column_order <- names(available_columns)
    }
    if (is.null(values$selected_columns)) {
      values$selected_columns <- names(available_columns)  # Default to all selected
    }
    
    # Create checkboxes for each column
    column_checkboxes <- lapply(seq_along(available_columns), function(i) {
      col <- names(available_columns)[i]
      div(
        style = "display: inline-block; margin-right: 15px; margin-bottom: 5px; min-width: 150px;",
        checkboxInput(
          inputId = paste0("col_", col),
          label = available_columns[col],
          value = TRUE,  # Always start with all selected
          width = "auto"
        )
      )
    })
    
    tagList(
      tags$p("Select columns to display (all selected by default):"),
      tags$div(
        id = "column_container",
        style = "min-height: 100px; border: 1px solid #ddd; padding: 10px; border-radius: 4px; background-color: #f9f9f9;",
        column_checkboxes
      )
    )
  })
  
  # Update selected columns when checkboxes change
  observe({
    req(values$nascar_data)
    
    # Get the default column list
    default_columns <- c("Full_Name", "start_ps", "ps", "ARP", "SpdRk", "fl", "ll", 
                         "DKSP", "FDSP", "DKDomRank", "FDDomRank", "car_number", 
                         "team_name", "finishing_status", "LapsDown", "race_season", "track_name")
    
    # Filter to only columns that exist in data
    available_columns <- default_columns[default_columns %in% names(values$nascar_data)]
    
    # Initialize if needed
    if (is.null(values$column_order)) {
      values$column_order <- available_columns
    }
    
    # Check which columns are selected
    selected <- c()
    for (col in available_columns) {
      checkbox_id <- paste0("col_", col)
      if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) {
        selected <- c(selected, col)
      }
    }
    
    # Only update if we have some selection (prevents empty table on initial load)
    if (length(selected) > 0) {
      values$selected_columns <- selected
    } else if (is.null(values$selected_columns)) {
      # Fallback to all columns if nothing selected and not initialized
      values$selected_columns <- available_columns
    }
  })
  
  # Optimized filtered data with performance improvements
  filtered_data <- reactive({
    req(values$nascar_data)
    
    withProgress(message = 'Applying filters...', {
      data <- values$nascar_data
      
      # Apply filters efficiently
      incProgress(0.2, detail = "Season filter...")
      if (!is.null(input$season_filter) && length(input$season_filter) > 0) {
        data <- data %>% filter(race_season %in% input$season_filter)
      }
      
      incProgress(0.4, detail = "Driver filter...")
      if (!is.null(input$driver_filter) && length(input$driver_filter) > 0) {
        data <- data %>% filter(Full_Name %in% input$driver_filter)
      }
      
      incProgress(0.6, detail = "Track filter...")
      if (!is.null(input$track_filter) && length(input$track_filter) > 0) {
        data <- data %>% filter(track_name %in% input$track_filter)
      }
      
      incProgress(0.8, detail = "Team filter...")
      if (!is.null(input$team_filter) && length(input$team_filter) > 0) {
        data <- data %>% filter(team_name %in% input$team_filter)
      }
      
      incProgress(0.9, detail = "Series filter...")
      if (!is.null(input$series_filter) && length(input$series_filter) > 0) {
        data <- data %>% filter(series_id %in% as.numeric(input$series_filter))
      }
      
      incProgress(1.0, detail = "Complete!")
      return(data)
    })
  })
  
  # Update filter count
  observe({
    filter_count <- 0
    if (!is.null(input$season_filter) && length(input$season_filter) > 0) filter_count <- filter_count + 1
    if (!is.null(input$driver_filter) && length(input$driver_filter) > 0) filter_count <- filter_count + 1
    if (!is.null(input$track_filter) && length(input$track_filter) > 0) filter_count <- filter_count + 1
    if (!is.null(input$team_filter) && length(input$team_filter) > 0) filter_count <- filter_count + 1
    if (!is.null(input$series_filter) && length(input$series_filter) > 0) filter_count <- filter_count + 1
    
    shinyjs::html("filter_count", paste("Filters applied:", filter_count))
  })
  
  # Enhanced main explorer table with conditional formatting
  output$main_explorer_table <- DT::renderDataTable({
    req(filtered_data(), values$selected_columns)
    
    withProgress(message = 'Rendering table...', {
      # Ensure we have valid columns to display
      valid_columns <- intersect(values$selected_columns, names(filtered_data()))
      if (length(valid_columns) == 0) {
        return(NULL)
      }
      
      # Rename columns for display
      display_data <- filtered_data() %>%
        select(all_of(valid_columns)) %>%
        rename_with(~ case_when(
          .x == "start_ps" ~ "Start",
          .x == "ps" ~ "Finish", 
          .x == "fl" ~ "FL",
          .x == "ll" ~ "LL",
          .x == "DKSP" ~ "DK Speed Pts",
          .x == "FDSP" ~ "FD Speed Pts",
          TRUE ~ .x
        ))
      
      incProgress(0.5, detail = "Applying formatting...")
      
      # Create the datatable
      dt <- DT::datatable(
        display_data,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          deferRender = TRUE,  # Improve performance for large datasets
          scroller = TRUE,     # Virtual scrolling for large datasets
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          )
        ),
        filter = 'top',
        rownames = FALSE,
        escape = FALSE
      )
      
      # Apply conditional formatting for position-related columns (green-white-red gradient)
      position_columns <- intersect(c("Start", "Finish", "ARP", "SpdRk"), 
                                    names(display_data))
      
      incProgress(0.8, detail = "Adding color formatting...")
      
      for (col in position_columns) {
        if (col %in% names(display_data)) {
          # Create green-white-red gradient (1 = best/green, 20 = white, 40 = worst/red)
          dt <- dt %>% formatStyle(
            col,
            backgroundColor = styleInterval(
              cuts = c(1, 5, 10, 15, 20, 25, 30, 35, 40),
              values = c("#00ff00", "#66ff66", "#ccffcc", "#ffffff", "#ffffff", 
                         "#ffcccc", "#ff6666", "#ff3333", "#ff0000", "#cc0000")
            ),
            color = "black",
            fontWeight = "bold"
          )
        }
      }
      
      incProgress(1.0, detail = "Complete!")
      return(dt)
    })
  })
  
  # Enhanced download handlers with filtered data
  output$download_filtered_csv <- downloadHandler(
    filename = function() {
      paste("nascar_filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(filtered_data(), values$selected_columns)
      export_data <- filtered_data() %>% 
        select(all_of(values$selected_columns)) %>%
        rename_with(~ case_when(
          .x == "start_ps" ~ "Start",
          .x == "ps" ~ "Finish", 
          .x == "fl" ~ "FL",
          .x == "ll" ~ "LL",
          .x == "DKSP" ~ "DK Speed Pts",
          .x == "FDSP" ~ "FD Speed Pts",
          TRUE ~ .x
        ))
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  output$download_filtered_excel <- downloadHandler(
    filename = function() {
      paste("nascar_filtered_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(filtered_data(), values$selected_columns)
      export_data <- filtered_data() %>% 
        select(all_of(values$selected_columns)) %>%
        rename_with(~ case_when(
          .x == "start_ps" ~ "Start",
          .x == "ps" ~ "Finish", 
          .x == "fl" ~ "FL",
          .x == "ll" ~ "LL",
          .x == "DKSP" ~ "DK Speed Pts",
          .x == "FDSP" ~ "FD Speed Pts",
          TRUE ~ .x
        ))
      
      wb <- createWorkbook()
      addWorksheet(wb, "NASCAR Data")
      writeData(wb, "NASCAR Data", export_data)
      
      # Add conditional formatting to Excel for position columns (using display names)
      position_columns <- intersect(c("Start", "Finish", "ARP", "SpdRk"), 
                                    names(export_data))
      
      if (length(position_columns) > 0) {
        # Create green-white-red color scale styles
        greenStyle <- createStyle(bgFill = "#00ff00")
        whiteStyle <- createStyle(bgFill = "#ffffff") 
        redStyle <- createStyle(bgFill = "#ff0000")
        
        # Apply conditional formatting (green-white-red gradient)
        for (col in position_columns) {
          col_num <- which(names(export_data) == col)
          conditionalFormatting(wb, "NASCAR Data", 
                                cols = col_num, rows = 2:(nrow(export_data) + 1),
                                type = "colourScale", 
                                style = c(greenStyle, whiteStyle, redStyle),
                                rule = c(1, 20, 40))
        }
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Reset filters with improved UX
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "season_filter", selected = character(0))
    updateSelectizeInput(session, "driver_filter", selected = character(0))
    updateSelectizeInput(session, "track_filter", selected = character(0))
    updateSelectizeInput(session, "team_filter", selected = character(0))
    updateSelectizeInput(session, "series_filter", selected = character(0))
    
    showNotification("All filters have been reset", type = "message", duration = 2)
  })
  
  # Dashboard generation
  observeEvent(input$generate_dashboard_btn, {
    # Show loading indicator
    showNotification("Generating dashboard... This may take a moment.", 
                     type = "message", duration = 3)
    
    # Here you would integrate the dashboard generation logic
    # from your CupDashboard.Rmd file
    output$generated_dashboard <- renderUI({
      # Placeholder for dashboard generation
      tags$div(
        class = "alert alert-success",
        h4("Dashboard Generated Successfully!"),
        p("Track:", input$dash_track),
        p("Similar Tracks:", paste(input$dash_similar_tracks, collapse = ", ")),
        p("Year Range:", input$dash_start_year, "-", input$dash_end_year),
        # Add actual dashboard content here
        plotlyOutput("dashboard_sample_plot")
      )
    })
  })
  
  # Sample dashboard plot
  output$dashboard_sample_plot <- renderPlotly({
    req(values$nascar_data, input$dash_track)
    
    # Create a sample plot based on selected track
    track_data <- values$nascar_data %>%
      filter(track_name == input$dash_track,
             race_season >= input$dash_start_year,
             race_season <= input$dash_end_year) %>%
      group_by(Full_Name) %>%
      summarize(
        Races = n(),
        AvgFinish = mean(ps, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(Races >= 3) %>%
      arrange(AvgFinish) %>%
      head(10)
    
    plot_ly(track_data, x = ~reorder(Full_Name, -AvgFinish), y = ~AvgFinish,
            type = 'bar', name = 'Average Finish') %>%
      layout(title = paste("Top 10 Average Finishers at", input$dash_track),
             xaxis = list(title = "Driver"),
             yaxis = list(title = "Average Finish"))
  })
  
  # Lap analysis functionality
  output$lap_race_selector <- renderUI({
    races <- get_season_races(input$lap_season)
    if (!is.null(races)) {
      race_choices <- setNames(races$race_id, 
                               paste(races$race_name, "-", races$track_name))
      selectInput("selected_lap_race", "Select Race:", choices = race_choices)
    }
  })
  
  # Custom analytics
  observeEvent(input$generate_custom, {
    req(values$nascar_data)
    
    # Generate custom analysis based on user selections
    custom_data <- values$nascar_data %>%
      group_by(across(all_of(input$custom_groupby))) %>%
      summarize(
        Races = n(),
        Wins = sum(ps == 1, na.rm = TRUE),
        Top5s = sum(ps <= 5, na.rm = TRUE),
        Top10s = sum(ps <= 10, na.rm = TRUE),
        AvgFinish = mean(ps, na.rm = TRUE),
        LapsLed = sum(LapsLed, na.rm = TRUE),
        FantasyPoints = mean(DKPoints, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(Races >= input$custom_min_races)
    
    values$custom_analysis <- custom_data
  })
  
  output$custom_analysis_table <- DT::renderDataTable({
    req(values$custom_analysis)
    
    DT::datatable(
      values$custom_analysis,
      options = list(
        scrollX = TRUE,
        pageLength = 15
      ),
      filter = 'top'
    ) %>%
      formatRound(c("AvgFinish", "FantasyPoints"), 2)
  })
  
  # Custom analysis plot
  output$custom_analysis_plot <- renderPlotly({
    req(values$custom_analysis, input$custom_metrics)
    
    if (input$custom_chart_type == "bar") {
      metric_col <- input$custom_metrics[1]
      plot_ly(values$custom_analysis, 
              x = ~get(input$custom_groupby), 
              y = ~get(metric_col),
              type = 'bar') %>%
        layout(title = paste(metric_col, "by", input$custom_groupby),
               xaxis = list(title = input$custom_groupby),
               yaxis = list(title = metric_col))
    }
  })
  
  # Export functionality
  output$export_complete_data <- downloadHandler(
    filename = function() {
      paste("nascar_analytics_export_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      wb <- createWorkbook()
      
      if ("results" %in% input$export_data_types) {
        addWorksheet(wb, "Race Results")
        writeData(wb, "Race Results", values$nascar_data)
      }
      
      if ("drivers" %in% input$export_data_types) {
        driver_stats <- values$nascar_data %>%
          group_by(Full_Name, race_season) %>%
          summarize(
            Races = n(),
            Wins = sum(ps == 1, na.rm = TRUE),
            Top5s = sum(ps <= 5, na.rm = TRUE),
            Top10s = sum(ps <= 10, na.rm = TRUE),
            AvgFinish = mean(ps, na.rm = TRUE),
            TotalLapsLed = sum(LapsLed, na.rm = TRUE),
            AvgDKPoints = mean(DKPoints, na.rm = TRUE),
            AvgFDPoints = mean(FDPoints, na.rm = TRUE),
            .groups = 'drop'
          )
        addWorksheet(wb, "Driver Statistics")
        writeData(wb, "Driver Statistics", driver_stats)
      }
      
      if ("teams" %in% input$export_data_types) {
        team_stats <- values$nascar_data %>%
          group_by(team_name, race_season) %>%
          summarize(
            Races = n(),
            Wins = sum(ps == 1, na.rm = TRUE),
            Top5s = sum(ps <= 5, na.rm = TRUE),
            Top10s = sum(ps <= 10, na.rm = TRUE),
            AvgFinish = mean(ps, na.rm = TRUE),
            TotalLapsLed = sum(LapsLed, na.rm = TRUE),
            .groups = 'drop'
          )
        addWorksheet(wb, "Team Performance")
        writeData(wb, "Team Performance", team_stats)
      }
      
      if ("tracks" %in% input$export_data_types) {
        track_stats <- values$nascar_data %>%
          group_by(track_name, race_season) %>%
          summarize(
            Races = n(),
            AvgSpeed = mean(Speed, na.rm = TRUE),
            AvgLeaders = mean(number_of_leaders, na.rm = TRUE),
            AvgCautions = mean(number_of_cautions, na.rm = TRUE),
            FieldSize = mean(number_of_cars_in_field, na.rm = TRUE),
            .groups = 'drop'
          )
        addWorksheet(wb, "Track Analysis")
        writeData(wb, "Track Analysis", track_stats)
      }
      
      if ("fantasy" %in% input$export_data_types) {
        fantasy_stats <- values$nascar_data %>%
          select(Full_Name, race_season, track_name, team_name, 
                 ps, start_ps, DKPoints, FDPoints, DKRank, FDRank,
                 LapsLed, DKSP, FDSP) %>%
          arrange(desc(DKPoints))
        addWorksheet(wb, "Fantasy Scoring")
        writeData(wb, "Fantasy Scoring", fantasy_stats)
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Export summary
  output$export_summary <- renderText({
    if (!is.null(values$nascar_data)) {
      paste0(
        "Total Records: ", nrow(values$nascar_data), "\n",
        "Seasons: ", min(values$nascar_data$race_season, na.rm = TRUE), " - ", 
        max(values$nascar_data$race_season, na.rm = TRUE), "\n",
        "Unique Drivers: ", length(unique(values$nascar_data$Full_Name)), "\n",
        "Unique Tracks: ", length(unique(values$nascar_data$track_name)), "\n",
        "Unique Teams: ", length(unique(values$nascar_data$team_name))
      )
    }
  })
  
  # Additional functionality for driver analysis
  observe({
    if (!is.null(values$nascar_data)) {
      updateSelectInput(session, "analysis_drivers",
                        choices = sort(unique(values$nascar_data$Full_Name)))
      updateSelectInput(session, "analysis_season",
                        choices = sort(unique(values$nascar_data$race_season), decreasing = TRUE))
      updateSelectInput(session, "analysis_tracks",
                        choices = sort(unique(values$nascar_data$track_name)))
    }
  })
  
  # Driver comparison plot
  output$driver_comparison_plot <- renderPlotly({
    req(values$nascar_data, input$analysis_drivers, input$analysis_season)
    
    if (length(input$analysis_drivers) == 0) return(NULL)
    
    driver_data <- values$nascar_data %>%
      filter(Full_Name %in% input$analysis_drivers,
             race_season == input$analysis_season) %>%
      arrange(race_id)
    
    if (input$driver_metric == "avg_finish") {
      p <- plot_ly(driver_data, x = ~race_id, y = ~ps, color = ~Full_Name,
                   type = 'scatter', mode = 'lines+markers') %>%
        layout(title = "Finishing Position by Race",
               xaxis = list(title = "Race"),
               yaxis = list(title = "Finishing Position", autorange = "reversed"))
    } else if (input$driver_metric == "dk_points") {
      p <- plot_ly(driver_data, x = ~race_id, y = ~DKPoints, color = ~Full_Name,
                   type = 'scatter', mode = 'lines+markers') %>%
        layout(title = "DraftKings Points by Race",
               xaxis = list(title = "Race"),
               yaxis = list(title = "DK Points"))
    } else if (input$driver_metric == "laps_led") {
      p <- plot_ly(driver_data, x = ~race_id, y = ~LapsLed, color = ~Full_Name,
                   type = 'scatter', mode = 'lines+markers') %>%
        layout(title = "Laps Led by Race",
               xaxis = list(title = "Race"),
               yaxis = list(title = "Laps Led"))
    }
    
    if (input$show_trend_analysis) {
      p <- p %>% add_lines(y = ~fitted(loess(ps ~ race_id)), 
                           line = list(dash = "dash"), showlegend = FALSE)
    }
    
    return(p)
  })
  
  # Driver detailed statistics
  output$driver_stats_detailed <- DT::renderDataTable({
    req(values$nascar_data, input$analysis_drivers, input$analysis_season)
    
    if (length(input$analysis_drivers) == 0) return(NULL)
    
    stats <- values$nascar_data %>%
      filter(Full_Name %in% input$analysis_drivers,
             race_season == input$analysis_season) %>%
      group_by(Full_Name) %>%
      summarize(
        Races = n(),
        Wins = sum(ps == 1, na.rm = TRUE),
        Top5s = sum(ps <= 5, na.rm = TRUE),
        Top10s = sum(ps <= 10, na.rm = TRUE),
        AvgFinish = round(mean(ps, na.rm = TRUE), 2),
        BestFinish = min(ps, na.rm = TRUE),
        WorstFinish = max(ps, na.rm = TRUE),
        LapsLed = sum(LapsLed, na.rm = TRUE),
        AvgDKPoints = round(mean(DKPoints, na.rm = TRUE), 2),
        MaxDKPoints = round(max(DKPoints, na.rm = TRUE), 2),
        AvgFDPoints = round(mean(FDPoints, na.rm = TRUE), 2),
        MaxFDPoints = round(max(FDPoints, na.rm = TRUE), 2),
        .groups = 'drop'
      ) %>%
      arrange(AvgFinish)
    
    DT::datatable(stats, options = list(dom = 't', scrollX = TRUE))
  })
  
  # Track analysis functionality
  observe({
    if (!is.null(values$nascar_data)) {
      updateSelectInput(session, "track_analysis_track",
                        choices = sort(unique(values$nascar_data$track_name)))
      updateSelectInput(session, "track_analysis_seasons",
                        choices = sort(unique(values$nascar_data$race_season), decreasing = TRUE))
    }
  })
  
  output$track_analysis_plot <- renderPlotly({
    req(values$nascar_data, input$track_analysis_track)
    
    track_data <- values$nascar_data %>%
      filter(track_name == input$track_analysis_track)
    
    if (!is.null(input$track_analysis_seasons) && length(input$track_analysis_seasons) > 0) {
      track_data <- track_data %>%
        filter(race_season %in% input$track_analysis_seasons)
    }
    
    if (input$track_metric == "avg_speed") {
      summary_data <- track_data %>%
        group_by(race_season, race_name) %>%
        summarize(AvgSpeed = mean(Speed, na.rm = TRUE), .groups = 'drop')
      
      p <- plot_ly(summary_data, x = ~race_season, y = ~AvgSpeed,
                   type = 'scatter', mode = 'lines+markers',
                   text = ~race_name, hoverinfo = 'text+x+y') %>%
        layout(title = paste("Average Speed Trends at", input$track_analysis_track),
               xaxis = list(title = "Season"),
               yaxis = list(title = "Average Speed (mph)"))
    } else if (input$track_metric == "leaders") {
      summary_data <- track_data %>%
        group_by(race_season, race_name) %>%
        summarize(Leaders = first(number_of_leaders), .groups = 'drop')
      
      p <- plot_ly(summary_data, x = ~race_season, y = ~Leaders,
                   type = 'scatter', mode = 'lines+markers',
                   text = ~race_name, hoverinfo = 'text+x+y') %>%
        layout(title = paste("Number of Leaders at", input$track_analysis_track),
               xaxis = list(title = "Season"),
               yaxis = list(title = "Number of Leaders"))
    } else if (input$track_metric == "dominator") {
      summary_data <- track_data %>%
        group_by(race_season, race_name) %>%
        summarize(
          AvgDomPoints = mean(DKSP, na.rm = TRUE),
          MaxDomPoints = max(DKSP, na.rm = TRUE),
          .groups = 'drop'
        )
      
      p <- plot_ly(summary_data, x = ~race_season, y = ~MaxDomPoints,
                   type = 'scatter', mode = 'lines+markers',
                   text = ~race_name, hoverinfo = 'text+x+y') %>%
        layout(title = paste("Dominator Points Trends at", input$track_analysis_track),
               xaxis = list(title = "Season"),
               yaxis = list(title = "Max Dominator Points"))
    }
    
    return(p)
  })
  
  # Track winners analysis
  output$track_winners_analysis <- DT::renderDataTable({
    req(values$nascar_data, input$track_analysis_track)
    
    winners <- values$nascar_data %>%
      filter(track_name == input$track_analysis_track, ps == 1) %>%
      select(race_season, race_name, Full_Name, team_name, start_ps, LapsLed, DKPoints) %>%
      arrange(desc(race_season))
    
    DT::datatable(winners, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  colnames = c("Season", "Race", "Winner", "Team", "Start", "Laps Led", "DK Points"))
  })
  
  # Team analysis functionality
  observe({
    if (!is.null(values$nascar_data)) {
      updateSelectInput(session, "analysis_teams",
                        choices = sort(unique(values$nascar_data$team_name)))
      updateSelectInput(session, "team_season",
                        choices = sort(unique(values$nascar_data$race_season), decreasing = TRUE))
    }
  })
  
  output$team_analysis_plot <- renderPlotly({
    req(values$nascar_data, input$analysis_teams, input$team_season)
    
    if (length(input$analysis_teams) == 0) return(NULL)
    
    team_data <- values$nascar_data %>%
      filter(team_name %in% input$analysis_teams,
             race_season == input$team_season)
    
    if (input$team_view == "wins_top5") {
      summary_data <- team_data %>%
        group_by(team_name) %>%
        summarize(
          Wins = sum(ps == 1, na.rm = TRUE),
          Top5s = sum(ps <= 5, na.rm = TRUE),
          Races = n(),
          .groups = 'drop'
        ) %>%
        mutate(
          WinRate = Wins / Races * 100,
          Top5Rate = Top5s / Races * 100
        )
      
      p <- plot_ly(summary_data, x = ~team_name, y = ~WinRate, 
                   type = 'bar', name = 'Win Rate') %>%
        add_trace(y = ~Top5Rate, name = 'Top 5 Rate') %>%
        layout(title = "Team Win and Top 5 Rates",
               xaxis = list(title = "Team"),
               yaxis = list(title = "Percentage"),
               barmode = 'group')
    } else if (input$team_view == "avg_finish") {
      summary_data <- team_data %>%
        group_by(team_name) %>%
        summarize(AvgFinish = mean(ps, na.rm = TRUE), .groups = 'drop')
      
      p <- plot_ly(summary_data, x = ~team_name, y = ~AvgFinish,
                   type = 'bar') %>%
        layout(title = "Team Average Finish",
               xaxis = list(title = "Team"),
               yaxis = list(title = "Average Finish", autorange = "reversed"))
    }
    
    return(p)
  })
  
  # Team summary statistics
  output$team_summary_detailed <- DT::renderDataTable({
    req(values$nascar_data, input$team_season)
    
    team_stats <- values$nascar_data %>%
      filter(race_season == input$team_season) %>%
      group_by(team_name) %>%
      summarize(
        Races = n(),
        Wins = sum(ps == 1, na.rm = TRUE),
        Top5s = sum(ps <= 5, na.rm = TRUE),
        Top10s = sum(ps <= 10, na.rm = TRUE),
        AvgFinish = round(mean(ps, na.rm = TRUE), 2),
        TotalLapsLed = sum(LapsLed, na.rm = TRUE),
        AvgDKPoints = round(mean(DKPoints, na.rm = TRUE), 2),
        AvgFDPoints = round(mean(FDPoints, na.rm = TRUE), 2),
        .groups = 'drop'
      ) %>%
      arrange(desc(Wins), AvgFinish)
    
    DT::datatable(team_stats, 
                  options = list(pageLength = 15, scrollX = TRUE),
                  filter = 'top')
  })
  
  # Season trends functionality
  observe({
    if (!is.null(values$nascar_data)) {
      updateSelectInput(session, "trend_tracks",
                        choices = c("All Tracks" = "", sort(unique(values$nascar_data$track_name))))
    }
  })
  
  output$season_trends_plot <- renderPlotly({
    req(values$nascar_data, input$season_metrics)
    
    trend_data <- values$nascar_data %>%
      filter(race_season >= input$season_range[1],
             race_season <= input$season_range[2])
    
    if (!is.null(input$trend_tracks) && input$trend_tracks != "" && length(input$trend_tracks) > 0) {
      trend_data <- trend_data %>%
        filter(track_name %in% input$trend_tracks)
    }
    
    # Create summary by season
    if ("avg_speed" %in% input$season_metrics) {
      speed_data <- trend_data %>%
        group_by(race_season) %>%
        summarize(AvgSpeed = mean(Speed, na.rm = TRUE), .groups = 'drop')
      
      p <- plot_ly(speed_data, x = ~race_season, y = ~AvgSpeed,
                   type = 'scatter', mode = 'lines+markers', name = 'Average Speed') %>%
        layout(title = "Season Trends Analysis",
               xaxis = list(title = "Season"),
               yaxis = list(title = "Value"))
    }
    
    if ("field_size" %in% input$season_metrics) {
      field_data <- trend_data %>%
        group_by(race_season) %>%
        summarize(AvgFieldSize = mean(number_of_cars_in_field, na.rm = TRUE), .groups = 'drop')
      
      if (exists("p")) {
        p <- p %>% add_trace(data = field_data, x = ~race_season, y = ~AvgFieldSize,
                             name = 'Field Size', yaxis = 'y2')
      } else {
        p <- plot_ly(field_data, x = ~race_season, y = ~AvgFieldSize,
                     type = 'scatter', mode = 'lines+markers', name = 'Field Size')
      }
    }
    
    return(p)
  })
  
  # Championship analysis
  output$championship_analysis <- DT::renderDataTable({
    req(values$nascar_data)
    
    champ_data <- values$nascar_data %>%
      group_by(race_season, Full_Name) %>%
      summarize(
        Points = sum(ChampionshipPoints, na.rm = TRUE),
        Wins = sum(ps == 1, na.rm = TRUE),
        Top5s = sum(ps <= 5, na.rm = TRUE),
        Top10s = sum(ps <= 10, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      group_by(race_season) %>%
      arrange(desc(Points)) %>%
      slice_head(n = 10) %>%
      mutate(Position = row_number()) %>%
      ungroup() %>%
      filter(Position <= 5) %>%
      arrange(race_season, Position)
    
    DT::datatable(champ_data,
                  options = list(pageLength = 20, scrollX = TRUE),
                  colnames = c("Season", "Driver", "Points", "Wins", "Top 5s", "Top 10s", "Championship Position"))
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "db_season_filter", selected = character(0))
    updateSelectInput(session, "driver_filter", selected = character(0))
    updateSelectInput(session, "track_filter", selected = character(0))
    updateSelectInput(session, "team_filter", selected = character(0))
  })
}

# Run the application
shinyApp(ui = ui, server = server)