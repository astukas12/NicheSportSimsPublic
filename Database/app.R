# NASCAR Data Explorer with Integrated Profile Builder
# Enhanced version with profile generation functionality

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)
library(readxl)
library(writexl)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(openxlsx)

#--------------------- Profile Generation Functions ---------------------#

# Create dominator profiles for race data
create_race_profile <- function(race_data, race_info) {
  profile <- race_data %>%
    mutate(
      # DK Points: FL worth 0.45, LL worth 0.25  
      DKDomPoints = (fast_laps * 0.45) + (lead_laps * 0.25),
      
      # FD Points: Only LL worth 0.1
      FDDomPoints = lead_laps * 0.1
    ) %>%
    # Only keep drivers who scored dominator points
    filter(DKDomPoints > 0 | FDDomPoints > 0) %>%
    select(
      RaceID = race_id,
      RaceName = race_name, 
      Season = race_season,
      TrackName = track_name,
      Driver = Full_Name,
      Team = team_name,
      StartPos = start_ps,
      FinPos = ps,
      LeadLaps = lead_laps,
      FastLaps = fast_laps,
      DKDomPoints,
      FDDomPoints
    ) %>%
    arrange(desc(FDDomPoints), desc(DKDomPoints))
  
  return(profile)
}

# Calculate target track behavioral profile
calculate_target_track_profile <- function(nascar_data, track_name, series_id) {
  track_historical <- nascar_data %>%
    filter(
      track_name == !!track_name, 
      series_id == !!series_id,
      !is.na(LapsLed), !is.na(fast_laps), !is.na(start_ps), !is.na(ps)
    )
  
  # Filter by race_type_id if column exists
  if ("race_type_id" %in% names(track_historical)) {
    track_historical <- track_historical %>% filter(race_type_id == 1)
  }
  
  if (nrow(track_historical) == 0) return(NULL)
  
  # Group by race and calculate correlations for each race
  race_profiles <- track_historical %>%
    group_by(race_season, race_id) %>%
    summarise(
      ll_start_cor = cor(start_ps, LapsLed, use = "complete.obs"),
      ll_finish_cor = cor(ps, LapsLed, use = "complete.obs"),
      fl_start_cor = cor(start_ps, fast_laps, use = "complete.obs"),
      fl_finish_cor = cor(ps, fast_laps, use = "complete.obs"),
      ll_concentration = sum(head(sort(LapsLed, decreasing = TRUE), 3), na.rm = TRUE) / sum(LapsLed, na.rm = TRUE),
      fl_concentration = sum(head(sort(fast_laps, decreasing = TRUE), 3), na.rm = TRUE) / sum(fast_laps, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(!is.na(ll_start_cor), !is.na(ll_finish_cor))
  
  if (nrow(race_profiles) == 0) return(NULL)
  
  # Calculate average behavioral patterns
  profile <- race_profiles %>%
    summarise(
      races_analyzed = n(),
      avg_ll_start_correlation = mean(ll_start_cor, na.rm = TRUE),
      avg_ll_finish_correlation = mean(ll_finish_cor, na.rm = TRUE),
      avg_fl_start_correlation = mean(fl_start_cor, na.rm = TRUE),
      avg_fl_finish_correlation = mean(fl_finish_cor, na.rm = TRUE),
      avg_ll_concentration = mean(ll_concentration, na.rm = TRUE),
      avg_fl_concentration = mean(fl_concentration, na.rm = TRUE)
    )
  
  return(profile)
}

# Calculate behavioral similarity between historical race and target track
calculate_behavioral_similarity <- function(race_data, target_profile) {
  if (is.null(target_profile) || nrow(race_data) == 0) return(0)
  
  # Calculate this race's behavioral metrics
  hist_ll_start_cor <- cor(race_data$start_ps, race_data$LapsLed, use = "complete.obs")
  hist_ll_finish_cor <- cor(race_data$ps, race_data$LapsLed, use = "complete.obs")
  hist_fl_start_cor <- cor(race_data$start_ps, race_data$fast_laps, use = "complete.obs")
  hist_fl_finish_cor <- cor(race_data$ps, race_data$fast_laps, use = "complete.obs")
  
  # Calculate concentration (top-3 dominance)
  hist_ll_concentration <- sum(head(sort(race_data$LapsLed, decreasing = TRUE), 3), na.rm = TRUE) / sum(race_data$LapsLed, na.rm = TRUE)
  hist_fl_concentration <- sum(head(sort(race_data$fast_laps, decreasing = TRUE), 3), na.rm = TRUE) / sum(race_data$fast_laps, na.rm = TRUE)
  
  # Calculate similarity scores (closer = higher score)
  ll_start_sim <- ifelse(!is.na(hist_ll_start_cor), 1 - abs(hist_ll_start_cor - target_profile$avg_ll_start_correlation), 0)
  ll_finish_sim <- ifelse(!is.na(hist_ll_finish_cor), 1 - abs(hist_ll_finish_cor - target_profile$avg_ll_finish_correlation), 0)
  fl_start_sim <- ifelse(!is.na(hist_fl_start_cor), 1 - abs(hist_fl_start_cor - target_profile$avg_fl_start_correlation), 0)
  fl_finish_sim <- ifelse(!is.na(hist_fl_finish_cor), 1 - abs(hist_fl_finish_cor - target_profile$avg_fl_finish_correlation), 0)
  ll_conc_sim <- ifelse(!is.na(hist_ll_concentration), 1 - abs(hist_ll_concentration - target_profile$avg_ll_concentration), 0)
  fl_conc_sim <- ifelse(!is.na(hist_fl_concentration), 1 - abs(hist_fl_concentration - target_profile$avg_fl_concentration), 0)
  
  # Weighted behavioral score
  behavioral_score <- (ll_start_sim * 0.15) + 
    (ll_finish_sim * 0.10) + 
    (fl_start_sim * 0.10) + 
    (fl_finish_sim * 0.05) + 
    (ll_conc_sim * 0.05) + 
    (fl_conc_sim * 0.05)
  
  return(list(
    behavioral_score = behavioral_score,
    ll_start_cor = hist_ll_start_cor,
    ll_finish_cor = hist_ll_finish_cor,
    fl_start_cor = hist_fl_start_cor,
    fl_finish_cor = hist_fl_finish_cor,
    ll_concentration = hist_ll_concentration,
    fl_concentration = hist_fl_concentration
  ))
}

# Load main NASCAR database and filter by race_type_id from RaceIDs file
load_nascar_database <- function() {
  if (file.exists("NascarDatabase.csv")) {
    data <- read_csv("NascarDatabase.csv")
    
    # Load RaceIDs to get the race_type_id filter
    if (file.exists("RaceIDs.xlsx")) {
      race_ids <- read_excel("RaceIDs.xlsx")
      
      # Get only regular races (race_type_id = 1) from RaceIDs
      if ("race_type_id" %in% names(race_ids)) {
        regular_race_ids <- race_ids %>% 
          filter(race_type_id == 1) %>% 
          pull(race_id)
        
        # Filter main data to only include regular races
        data <- data %>% filter(race_id %in% regular_race_ids)
      }
    }
    
    return(data)
  } else {
    return(NULL)
  }
}

# Load race list (non-historical races, regular races only if column exists)
load_race_list <- function() {
  if (file.exists("RaceIDs.xlsx")) {
    race_data <- read_excel("RaceIDs.xlsx")
    # Filter out races marked as historical AND only include regular races (race_type_id = 1)
    filtered_data <- race_data %>% filter(Historical == "N")
    # Filter by race_type_id = 1 if column exists
    if ("race_type_id" %in% names(filtered_data)) {
      return(filtered_data %>% filter(race_type_id == 1))
    } else {
      return(filtered_data)
    }
  } else {
    return(NULL)
  }
}

#--------------------- UI Definition ---------------------#

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "NASCAR Data Explorer",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Data Explorer", tabName = "data_explorer", icon = icon("table")),
      menuItem("Profile Builder", tabName = "profile_builder", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    # Custom CSS - Black and Gold Theme
    tags$head(
      tags$style(HTML("
        /* Override dashboard header colors */
        .skin-blue .main-header {
          background-color: #000000 !important;
        }
        .skin-blue .main-header .logo {
          background-color: #000000 !important;
          color: #FFD700 !important; 
        }
        .skin-blue .main-header .logo:hover {
          background-color: #000000 !important;
        }
        .skin-blue .main-header .navbar {
          background-color: #000000 !important;
        }
        
        /* Override dashboard sidebar colors */
        .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
          background-color: #222222 !important;
        }
        .skin-blue .sidebar a {
          color: #FFD700 !important; 
        }
        .skin-blue .sidebar-menu > li.active > a, 
        .skin-blue .sidebar-menu > li:hover > a {
          color: #ffffff !important;
          background: #333333 !important;
          border-left-color: #FFD700 !important;
        }
        
        /* Main content area */
        .content-wrapper, .right-side { 
          background-color: #1a1a1a !important; 
          color: #ffffff !important;
        }
        
        /* Customize box headers */
        .box.box-primary .box-header {
          background-color: #333333 !important;
          color: #FFD700 !important;
        }
        .box { 
          background-color: #2d2d2d !important;
          border: 1px solid #444444 !important;
          border-radius: 3px; 
          box-shadow: 0 1px 3px rgba(0,0,0,.3) !important;
          color: #ffffff !important;
        }
        .box:hover { 
          box-shadow: 0 3px 6px rgba(255, 215, 0, 0.3) !important; 
        }
        .box-header {
          background-color: #333333 !important;
          color: #FFD700 !important;
          border-bottom: 1px solid #555555 !important;
        }
        .box-header .box-title {
          color: #FFD700 !important;
        }
        .box-primary {
          border-top-color: #FFD700 !important;
        }
        .box-info {
          border-top-color: #FFD700 !important;
        }
        .box-success {
          border-top-color: #FFD700 !important;
        }
        .box-warning {
          border-top-color: #DAA520 !important;
        }
        
        /* Style buttons */
        .btn-primary {
          background-color: #FFD700 !important;
          border-color: #DAA520 !important;
          color: #000000 !important;
        }
        .btn-primary:hover, .btn-primary:focus {
          background-color: #DAA520 !important;
          border-color: #B8860B !important;
          color: #000000 !important;
        }
        .btn-success {
          background-color: #FFD700 !important;
          border-color: #DAA520 !important;
          color: #000000 !important;
        }
        .btn-success:hover {
          background-color: #DAA520 !important;
          border-color: #B8860B !important;
          color: #000000 !important;
        }
        .btn-warning {
          background-color: #DAA520 !important;
          border-color: #B8860B !important;
          color: #000000 !important;
        }
        .btn-warning:hover {
          background-color: #B8860B !important;
          border-color: #8B7355 !important;
          color: #000000 !important;
        }
        
        /* DataTable styling */
        .dataTable th { 
          background-color: #333333 !important;
          color: #FFD700 !important;
          border-bottom: 2px solid #FFD700 !important;
        }
        .dataTable td {
          background-color: #2d2d2d !important;
          color: #ffffff !important;
          border-bottom: 1px solid #444444 !important;
        }
        .dataTable tbody tr:hover {
          background-color: #404040 !important;
        }
        .dataTables_wrapper {
          color: #ffffff !important;
        }
        .dataTables_info, .dataTables_length label, .dataTables_filter label {
          color: #ffffff !important;
        }
        .dataTables_paginate .paginate_button {
          background-color: #333333 !important;
          color: #FFD700 !important;
          border: 1px solid #555555 !important;
        }
        .dataTables_paginate .paginate_button:hover {
          background-color: #FFD700 !important;
          color: #000000 !important;
        }
        .dataTables_paginate .paginate_button.current {
          background-color: #FFD700 !important;
          color: #000000 !important;
        }
        
        /* Form controls styling */
        .form-control {
          background-color: #404040 !important;
          border: 1px solid #666666 !important;
          color: #ffffff !important;
        }
        .form-control:focus {
          border-color: #FFD700 !important;
          box-shadow: 0 0 0 0.2rem rgba(255, 215, 0, 0.25) !important;
        }
        .selectize-control.single .selectize-input,
        .selectize-control.multi .selectize-input {
          background-color: #404040 !important;
          border: 1px solid #666666 !important;
          color: #ffffff !important;
        }
        .selectize-dropdown {
          background-color: #333333 !important;
          border: 1px solid #666666 !important;
          color: #ffffff !important;
        }
        .selectize-dropdown-content .option {
          color: #ffffff !important;
        }
        .selectize-dropdown-content .option:hover {
          background-color: #FFD700 !important;
          color: #000000 !important;
        }
        
        /* Style tabs */
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #FFD700 !important;
        }
        
        /* Additional styles for gold accents */
        .pagination > .active > a, 
        .pagination > .active > span, 
        .pagination > .active > a:hover, 
        .pagination > .active > span:hover, 
        .pagination > .active > a:focus, 
        .pagination > .active > span:focus {
          background-color: #FFD700 !important;
          border-color: #DAA520 !important;
          color: #000000 !important;
        }
        
        /* Style for sliders and other inputs */
        .irs-bar,
        .irs-bar-edge,
        .irs-single,
        .irs-from,
        .irs-to {
          background: #FFD700 !important;
          border-color: #DAA520 !important;
          color: #000000 !important;
        }
        
        /* Style for checkboxes and radio buttons */
        input[type='checkbox']:checked, 
        input[type='radio']:checked {
          background-color: #FFD700 !important;
          border-color: #DAA520 !important;
        }
        
        /* Labels */
        label {
          color: #ffffff !important;
        }
        
        /* Style loader spinners */
        .shiny-spinner .load-container .loader {
          border-top-color: #FFD700 !important;
        }
        .loading-spinner { 
          display: inline-block; width: 20px; height: 20px; 
          border: 3px solid rgba(255,255,255,0.1); border-radius: 50%; 
          border-top-color: #FFD700; animation: spin 1s ease-in-out infinite; 
        }
        @keyframes spin { to { transform: rotate(360deg); } }
      "))
    ),
    
    tabItems(
      # Data Explorer Tab
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
                           selectizeInput("series_filter", "Series:", 
                                          choices = c("Cup Series" = 1, "Xfinity Series" = 2, "Truck Series" = 3),
                                          multiple = TRUE,
                                          options = list(placeholder = "All Series"))
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
                           div(
                             actionButton("reset_filters", "Reset All Filters", 
                                          class = "btn-warning", style = "margin-top: 25px; margin-bottom: 10px; width: 100%;"),
                             br(),
                             downloadButton("download_filtered_csv", "Download CSV", 
                                            class = "btn-success", style = "width: 100%;")
                           )
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "NASCAR Data Explorer", status = "primary", solidHeader = TRUE, width = 12,
                  withSpinner(DT::dataTableOutput("main_explorer_table"))
                )
              )
      ),
      
      # Profile Builder Tab
      tabItem(tabName = "profile_builder",
              fluidRow(
                box(
                  title = "Race Profile Configuration", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(3,
                           selectizeInput("target_series", "Series:",
                                          choices = c("Cup Series" = 1, "Xfinity Series" = 2, "Truck Series" = 3),
                                          selected = NULL,
                                          options = list(placeholder = "Select Series"))
                    ),
                    column(3,
                           selectizeInput("target_track", "Track(s):",
                                          choices = NULL,
                                          multiple = TRUE,
                                          options = list(placeholder = "Select Track(s)"))
                    ),
                    column(3,
                           numericInput("season_start", "Season Range Start:", 
                                        value = 2019, min = 2000, max = 2024, step = 1)
                    ),
                    column(3,
                           numericInput("season_end", "Season Range End:", 
                                        value = 2024, min = 2000, max = 2024, step = 1)
                    )
                  ),
                  fluidRow(
                    column(12,
                           div(style = "margin-top: 15px; text-align: center;",
                               actionButton("generate_dominator_profiles", "Generate Dominator Profiles", 
                                            class = "btn-primary", style = "padding: 10px 30px; font-size: 16px;"))
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Generated Dominator Profiles", status = "success", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(12,
                           div(
                             downloadButton("download_profiles_xlsx", "Download Excel File", 
                                            class = "btn-success", style = "margin-bottom: 15px;"),
                             withSpinner(DT::dataTableOutput("dominator_profiles_table"))
                           )
                    )
                  )
                )
              ),
              
              fluidRow(
                column(6,
                       box(
                         title = "Profile Summary", status = "info", solidHeader = TRUE, width = 12,
                         withSpinner(verbatimTextOutput("profile_summary_text"))
                       )
                ),
                column(6,
                       box(
                         title = "Race Weights", status = "warning", solidHeader = TRUE, width = 12,
                         withSpinner(DT::dataTableOutput("race_weights_table"))
                       )
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
    race_list = NULL,
    dominator_profiles = NULL,
    race_weights = NULL,
    race_summary = NULL
  )
  
  # Load initial data
  observe({
    withProgress(message = 'Loading NASCAR Database...', {
      values$nascar_data <- load_nascar_database()
      values$race_list <- load_race_list()
      
      if (!is.null(values$nascar_data)) {
        incProgress(0.3, detail = "Processing seasons...")
        seasons <- sort(unique(values$nascar_data$race_season), decreasing = TRUE)
        
        incProgress(0.5, detail = "Processing drivers...")
        drivers <- sort(unique(values$nascar_data$Full_Name[!is.na(values$nascar_data$Full_Name)]))
        
        incProgress(0.7, detail = "Processing tracks...")
        tracks <- sort(unique(values$nascar_data$track_name[!is.na(values$nascar_data$track_name)]))
        
        incProgress(0.8, detail = "Processing teams...")
        teams <- sort(unique(values$nascar_data$team_name[!is.na(values$nascar_data$team_name)]))
        
        # Initialize filter choices
        updateSelectizeInput(session, "season_filter", choices = seasons)
        updateSelectizeInput(session, "driver_filter", choices = drivers)
        updateSelectizeInput(session, "track_filter", choices = tracks)
        updateSelectizeInput(session, "team_filter", choices = teams)
        
        # Initialize profile builder track choices
        updateSelectizeInput(session, "target_track", choices = tracks)
        
        incProgress(1.0, detail = "Complete!")
      }
    })
  })
  
  # Reactive data for filter updates (cascading filters)
  filter_base_data <- reactive({
    req(values$nascar_data)
    data <- values$nascar_data
    
    # Apply season filter first
    if (!is.null(input$season_filter) && length(input$season_filter) > 0) {
      data <- data %>% filter(race_season %in% input$season_filter)
    }
    
    # Apply series filter
    if (!is.null(input$series_filter) && length(input$series_filter) > 0) {
      data <- data %>% filter(series_id %in% as.numeric(input$series_filter))
    }
    
    return(data)
  })
  
  # Update driver choices based on other filters
  observe({
    base_data <- filter_base_data()
    if (!is.null(base_data)) {
      # Further filter by track and team if selected
      if (!is.null(input$track_filter) && length(input$track_filter) > 0) {
        base_data <- base_data %>% filter(track_name %in% input$track_filter)
      }
      if (!is.null(input$team_filter) && length(input$team_filter) > 0) {
        base_data <- base_data %>% filter(team_name %in% input$team_filter)
      }
      
      available_drivers <- sort(unique(base_data$Full_Name[!is.na(base_data$Full_Name)]))
      updateSelectizeInput(session, "driver_filter", choices = available_drivers, 
                           selected = input$driver_filter[input$driver_filter %in% available_drivers])
    }
  })
  
  # Update track choices based on other filters
  observe({
    base_data <- filter_base_data()
    if (!is.null(base_data)) {
      # Further filter by driver and team if selected
      if (!is.null(input$driver_filter) && length(input$driver_filter) > 0) {
        base_data <- base_data %>% filter(Full_Name %in% input$driver_filter)
      }
      if (!is.null(input$team_filter) && length(input$team_filter) > 0) {
        base_data <- base_data %>% filter(team_name %in% input$team_filter)
      }
      
      available_tracks <- sort(unique(base_data$track_name[!is.na(base_data$track_name)]))
      updateSelectizeInput(session, "track_filter", choices = available_tracks,
                           selected = input$track_filter[input$track_filter %in% available_tracks])
    }
  })
  
  # Update team choices based on other filters
  observe({
    base_data <- filter_base_data()
    if (!is.null(base_data)) {
      # Further filter by driver and track if selected
      if (!is.null(input$driver_filter) && length(input$driver_filter) > 0) {
        base_data <- base_data %>% filter(Full_Name %in% input$driver_filter)
      }
      if (!is.null(input$track_filter) && length(input$track_filter) > 0) {
        base_data <- base_data %>% filter(track_name %in% input$track_filter)
      }
      
      available_teams <- sort(unique(base_data$team_name[!is.na(base_data$team_name)]))
      updateSelectizeInput(session, "team_filter", choices = available_teams,
                           selected = input$team_filter[input$team_filter %in% available_teams])
    }
  })
  
  # Filtered data
  filtered_data <- reactive({
    req(values$nascar_data)
    
    withProgress(message = 'Applying filters...', {
      data <- values$nascar_data
      
      incProgress(0.2, detail = "Season filter...")
      if (!is.null(input$season_filter) && length(input$season_filter) > 0) {
        data <- data %>% filter(race_season %in% input$season_filter)
      }
      
      incProgress(0.4, detail = "Series filter...")
      if (!is.null(input$series_filter) && length(input$series_filter) > 0) {
        data <- data %>% filter(series_id %in% as.numeric(input$series_filter))
      }
      
      incProgress(0.6, detail = "Driver filter...")
      if (!is.null(input$driver_filter) && length(input$driver_filter) > 0) {
        data <- data %>% filter(Full_Name %in% input$driver_filter)
      }
      
      incProgress(0.8, detail = "Track filter...")
      if (!is.null(input$track_filter) && length(input$track_filter) > 0) {
        data <- data %>% filter(track_name %in% input$track_filter)
      }
      
      incProgress(0.9, detail = "Team filter...")
      if (!is.null(input$team_filter) && length(input$team_filter) > 0) {
        data <- data %>% filter(team_name %in% input$team_filter)
      }
      
      incProgress(1.0, detail = "Complete!")
      return(data)
    })
  })
  
  # Main explorer table with smooth gradient and black/gold theme
  output$main_explorer_table <- DT::renderDataTable({
    req(filtered_data())
    
    withProgress(message = 'Rendering table...', {
      # Default columns to display
      default_columns <- c("Full_Name", "start_ps", "ps", "ARP", "SpdRk", "fl", "ll", 
                           "DKSP", "FDSP", "DKDomRank", "FDDomRank", "car_number", 
                           "team_name", "finishing_status", "LapsDown", "race_season", "track_name")
      
      # Filter to only columns that exist in data
      valid_columns <- intersect(default_columns, names(filtered_data()))
      
      # Prepare data with rounding for ARP
      display_data <- filtered_data() %>%
        select(all_of(valid_columns)) %>%
        mutate(
          ARP = if("ARP" %in% names(.)) round(ARP, 1) else ARP
        ) %>%
        rename_with(~ case_when(
          .x == "Full_Name" ~ "Driver Name",
          .x == "start_ps" ~ "Start",
          .x == "ps" ~ "Finish", 
          .x == "fl" ~ "FL",
          .x == "ll" ~ "LL",
          .x == "DKSP" ~ "DK Speed Pts",
          .x == "FDSP" ~ "FD Speed Pts",
          .x == "car_number" ~ "Car Number",
          .x == "team_name" ~ "Team",
          .x == "finishing_status" ~ "Finishing Status",
          .x == "LapsDown" ~ "Laps Down",
          .x == "race_season" ~ "Season",
          .x == "track_name" ~ "Track",
          TRUE ~ .x
        ))
      
      incProgress(0.5, detail = "Applying formatting...")
      
      # Create the datatable with dark theme
      dt <- DT::datatable(
        display_data,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = 'frtip',
          deferRender = TRUE,
          scroller = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          ),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#333333', 'color': '#FFD700'});",
            "}"
          )
        ),
        filter = 'top',
        rownames = FALSE,
        escape = FALSE,
        class = "display nowrap compact"
      )
      
      # Apply smooth gradient formatting for position-related columns
      position_columns <- intersect(c("Start", "Finish", "ARP", "SpdRk"), 
                                    names(display_data))
      
      incProgress(0.8, detail = "Adding gradient formatting...")
      
      for (col in position_columns) {
        if (col %in% names(display_data)) {
          # Get the range of values for this column
          col_values <- display_data[[col]]
          col_values <- col_values[!is.na(col_values)]
          
          if (length(col_values) > 0) {
            min_val <- min(col_values)
            max_val <- max(col_values)
            
            # Create smooth gradient from gold (best) to dark (worst)
            dt <- dt %>% formatStyle(
              col,
              backgroundColor = JS(paste0(
                "function(value, type, row) {",
                "if (type === 'display' && value !== null && value !== '') {",
                "  var min = ", min_val, ";",
                "  var max = ", max_val, ";",
                "  var normalized = (value - min) / (max - min);",
                "  var red = Math.round(255 - (255 - 218) * normalized);",   # From FFD700 to darker
                "  var green = Math.round(215 - (215 - 165) * normalized);", # Gold to darker gold
                "  var blue = Math.round(0 + 32 * normalized);",             # From gold to dark
                "  return 'rgb(' + red + ',' + green + ',' + blue + ')';",
                "} else {",
                "  return '#2d2d2d';", # Default dark background
                "}",
                "}"
              )),
              color = "#000000",
              fontWeight = "bold"
            )
          }
        }
      }
      
      incProgress(1.0, detail = "Complete!")
      return(dt)
    })
  })
  
  # Download CSV handler
  output$download_filtered_csv <- downloadHandler(
    filename = function() {
      paste("nascar_filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(filtered_data())
      
      # Default columns to display
      default_columns <- c("Full_Name", "start_ps", "ps", "ARP", "SpdRk", "fl", "ll", 
                           "DKSP", "FDSP", "DKDomRank", "FDDomRank", "car_number", 
                           "team_name", "finishing_status", "LapsDown", "race_season", "track_name")
      
      valid_columns <- intersect(default_columns, names(filtered_data()))
      
      export_data <- filtered_data() %>% 
        select(all_of(valid_columns)) %>%
        mutate(
          ARP = if("ARP" %in% names(.)) round(ARP, 1) else ARP
        ) %>%
        rename_with(~ case_when(
          .x == "Full_Name" ~ "Driver Name",
          .x == "start_ps" ~ "Start",
          .x == "ps" ~ "Finish", 
          .x == "fl" ~ "FL",
          .x == "ll" ~ "LL",
          .x == "DKSP" ~ "DK Speed Pts",
          .x == "FDSP" ~ "FD Speed Pts",
          .x == "car_number" ~ "Car Number",
          .x == "team_name" ~ "Team",
          .x == "finishing_status" ~ "Finishing Status",
          .x == "LapsDown" ~ "Laps Down",
          .x == "race_season" ~ "Season",
          .x == "track_name" ~ "Track",
          TRUE ~ .x
        ))
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "season_filter", selected = character(0))
    updateSelectizeInput(session, "driver_filter", selected = character(0))
    updateSelectizeInput(session, "track_filter", selected = character(0))
    updateSelectizeInput(session, "team_filter", selected = character(0))
    updateSelectizeInput(session, "series_filter", selected = character(0))
    
    showNotification("All filters have been reset", type = "message", duration = 2)
  })
  
  #--------------------- Profile Builder Logic ---------------------#
  
  # Generate dominator profiles
  observeEvent(input$generate_dominator_profiles, {
    req(input$target_series, input$target_track, input$season_start, input$season_end, values$nascar_data)
    
    if (length(input$target_track) == 0) {
      showNotification("Please select at least one track", type = "warning")
      return()
    }
    
    withProgress(message = 'Generating dominator profiles...', {
      incProgress(0.1, detail = "Filtering race data...")
      
      # Filter data for the selected criteria (now supporting multiple tracks)
      filtered_races <- values$nascar_data %>%
        filter(
          series_id == as.numeric(input$target_series),
          track_name %in% input$target_track,  # Changed to %in% for multiple tracks
          race_season >= input$season_start,
          race_season <= input$season_end
        )
      
      # Filter by race_type_id if column exists
      if ("race_type_id" %in% names(filtered_races)) {
        filtered_races <- filtered_races %>% filter(race_type_id == 1)
      }
      
      if (nrow(filtered_races) == 0) {
        showNotification("No races found matching your criteria", type = "warning")
        return()
      }
      
      incProgress(0.3, detail = "Processing individual races...")
      
      # Get unique races
      unique_races <- filtered_races %>%
        select(race_id, race_season, race_name, track_name) %>%
        distinct() %>%
        arrange(race_season, race_id)
      
      # Generate profiles for each race
      all_profiles <- list()
      race_weights <- data.frame(RaceID = integer(), RaceName = character(), 
                                 Season = integer(), Track = character(), Weight = numeric(), stringsAsFactors = FALSE)
      
      incProgress(0.5, detail = paste("Processing", nrow(unique_races), "races..."))
      
      for(i in 1:nrow(unique_races)) {
        race_info <- unique_races[i,]
        race_data <- filtered_races %>% filter(race_id == race_info$race_id)
        
        if(nrow(race_data) > 0) {
          profile <- create_race_profile(race_data, race_info)
          
          if(nrow(profile) > 0) {
            all_profiles[[paste0("Race_", race_info$race_id)]] <- profile
            
            # Add to weights table
            weight_value <- case_when(
              race_info$race_season == 2024 ~ 0.4,  # Most recent gets highest weight
              race_info$race_season == 2023 ~ 0.35,
              race_info$race_season == 2022 ~ 0.25,
              race_info$race_season == 2021 ~ 0.20,
              TRUE ~ 0.15  # Older races get lower weight
            )
            
            race_weights <- rbind(race_weights, data.frame(
              RaceID = race_info$race_id,
              RaceName = race_info$race_name,
              Season = race_info$race_season,
              Track = race_info$track_name,  # Added track name to weights table
              Weight = weight_value,
              stringsAsFactors = FALSE
            ))
          }
        }
        
        # Update progress
        incProgress(0.4 / nrow(unique_races), detail = paste("Processed race", i, "of", nrow(unique_races)))
      }
      
      incProgress(0.9, detail = "Combining profiles...")
      
      if (length(all_profiles) == 0) {
        showNotification("No dominator profiles could be generated", type = "warning")
        return()
      }
      
      # Combine all profiles
      combined_profiles <- bind_rows(all_profiles)
      
      # Create summary statistics
      race_summary <- combined_profiles %>%
        group_by(RaceID, RaceName, Season) %>%
        summarise(
          Track = first(TrackName),  # Now use TrackName from the profiles data
          Drivers_With_Dom = n(),
          Total_DK_Points = sum(DKDomPoints),
          Total_FD_Points = sum(FDDomPoints),
          Total_Lead_Laps = sum(LeadLaps),
          Total_Fast_Laps = sum(FastLaps),
          Max_DK_Single = max(DKDomPoints),
          Max_FD_Single = max(FDDomPoints),
          .groups = 'drop'
        ) %>%
        left_join(race_weights, by = c("RaceID", "RaceName", "Season", "Track"))
      
      # Store results
      values$dominator_profiles <- combined_profiles
      values$race_weights <- race_weights
      values$race_summary <- race_summary
      
      incProgress(1.0, detail = "Complete!")
      
      selected_tracks <- paste(input$target_track, collapse = ", ")
      showNotification(paste("Generated", nrow(combined_profiles), "dominator profiles from", 
                             length(unique(combined_profiles$RaceID)), "races at:", selected_tracks), 
                       type = "message", duration = 5)
    })
  })
  
  # Render dominator profiles table
  output$dominator_profiles_table <- DT::renderDataTable({
    req(values$dominator_profiles)
    
    display_data <- values$dominator_profiles %>%
      mutate(
        DKDomPoints = round(DKDomPoints, 2),
        FDDomPoints = round(FDDomPoints, 2)
      ) %>%
      arrange(Season, RaceID, desc(FDDomPoints))
    
    DT::datatable(
      display_data,
      options = list(
        scrollX = TRUE,
        pageLength = 20,
        dom = 'frtip',
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#333333', 'color': '#FFD700'});",
          "}"
        )
      ),
      filter = 'top',
      rownames = FALSE,
      class = "display nowrap compact"
    ) %>%
      formatStyle(
        c("DKDomPoints", "FDDomPoints"),
        backgroundColor = styleColorBar(c(0, max(display_data$DKDomPoints, na.rm = TRUE)), "#FFD700"),
        backgroundSize = '80% 50%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        color = "#000000",
        fontWeight = "bold"
      )
  })
  
  # Render race weights table
  output$race_weights_table <- DT::renderDataTable({
    req(values$race_weights)
    
    DT::datatable(
      values$race_weights,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'frtip',
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#333333', 'color': '#FFD700'});",
          "}"
        )
      ),
      rownames = FALSE,
      class = "display nowrap compact"
    ) %>%
      formatStyle(
        "Weight",
        backgroundColor = styleColorBar(c(0, max(values$race_weights$Weight, na.rm = TRUE)), "#FFD700"),
        backgroundSize = '80% 50%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        color = "#000000",
        fontWeight = "bold"
      )
  })
  
  # Profile summary text
  output$profile_summary_text <- renderText({
    req(values$dominator_profiles, values$race_summary)
    
    profiles <- values$dominator_profiles
    summary <- values$race_summary
    
    total_races <- length(unique(profiles$RaceID))
    total_drivers <- nrow(profiles)
    avg_dk_points <- round(mean(profiles$DKDomPoints, na.rm = TRUE), 2)
    avg_fd_points <- round(mean(profiles$FDDomPoints, na.rm = TRUE), 2)
    max_dk_points <- round(max(profiles$DKDomPoints, na.rm = TRUE), 2)
    max_fd_points <- round(max(profiles$FDDomPoints, na.rm = TRUE), 2)
    
    # Track breakdown
    track_breakdown <- profiles %>%
      count(RaceName) %>%
      arrange(desc(n))
    
    # Tier breakdown
    profiles_by_track <- profiles %>%
      group_by(TrackName) %>%
      summarise(
        Total_Profiles = n(),
        Avg_DK = round(mean(DKDomPoints, na.rm = TRUE), 2),
        Avg_FD = round(mean(FDDomPoints, na.rm = TRUE), 2),
        .groups = 'drop'
      )
    
    track_text <- paste(sapply(1:nrow(profiles_by_track), function(i) {
      track <- profiles_by_track$TrackName[i]
      count <- profiles_by_track$Total_Profiles[i]
      avg_dk <- profiles_by_track$Avg_DK[i]
      avg_fd <- profiles_by_track$Avg_FD[i]
      paste(track, ":", count, "profiles (Avg DK:", avg_dk, ", Avg FD:", avg_fd, ")")
    }), collapse = "\n")
    
    # Show selected tracks
    selected_tracks <- paste(input$target_track, collapse = ", ")
    
    paste(
      paste("Profile Generation Summary"),
      paste("Track(s):", selected_tracks),
      paste("Series:", case_when(
        input$target_series == "1" ~ "Cup Series",
        input$target_series == "2" ~ "Xfinity Series",
        input$target_series == "3" ~ "Truck Series"
      )),
      paste("Season Range:", input$season_start, "-", input$season_end),
      "",
      paste("Total Races Analyzed:", total_races),
      paste("Total Driver Profiles:", total_drivers),
      "",
      "Fantasy Points Analysis:",
      paste("Avg DK Dom Points:", avg_dk_points),
      paste("Avg FD Dom Points:", avg_fd_points),
      paste("Max DK Dom Points:", max_dk_points),
      paste("Max FD Dom Points:", max_fd_points),
      "",
      "Dominator Tier Breakdown:",
      tier_text,
      sep = "\n"
    )
  })
  
  # Download profiles Excel handler
  output$download_profiles_xlsx <- downloadHandler(
    filename = function() {
      # Handle multiple tracks in filename
      if (length(input$target_track) > 1) {
        track_name <- "Multiple_Tracks"
      } else {
        track_name <- gsub("[^A-Za-z0-9]", "_", input$target_track[1])
      }
      
      series_name <- case_when(
        input$target_series == "1" ~ "Cup",
        input$target_series == "2" ~ "Xfinity",
        input$target_series == "3" ~ "Truck"
      )
      paste0(track_name, "_", series_name, "_Dominator_Profiles_", 
             input$season_start, "-", input$season_end, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(values$dominator_profiles, values$race_weights, values$race_summary)
      
      # Create Excel workbook with multiple sheets
      excel_data <- list(
        "Dominator_Profiles" = values$dominator_profiles,
        "Race_Weights" = values$race_weights,
        "Race_Summary" = values$race_summary
      )
      
      write_xlsx(excel_data, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)