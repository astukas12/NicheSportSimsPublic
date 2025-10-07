# NASCAR Race Analysis App - REWRITTEN VERSION
# Vertical navigation with comprehensive analysis sections

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
library(plotly)
library(ggplot2)
library(jsonlite)
library(tidyr)

#--------------------- Helper Functions ---------------------#

# Load NASCAR database
load_nascar_database <- function() {
  if (file.exists("NascarDatabase.csv")) {
    data <- read_csv("NascarDatabase.csv", show_col_types = FALSE)
    
    if (file.exists("RaceIDs.xlsx")) {
      race_ids <- read_excel("RaceIDs.xlsx")
      if ("race_type_id" %in% names(race_ids)) {
        regular_race_ids <- race_ids %>% 
          filter(race_type_id == 1) %>% 
          pull(race_id)
        data <- data %>% filter(race_id %in% regular_race_ids)
      }
    }
    return(data)
  } else {
    return(NULL)
  }
}

# Load race list
load_race_list <- function() {
  if (file.exists("RaceIDs.xlsx")) {
    race_data <- read_excel("RaceIDs.xlsx")
    filtered_data <- race_data %>% filter(Historical == "N")
    if ("race_type_id" %in% names(filtered_data)) {
      return(filtered_data %>% filter(race_type_id == 1))
    } else {
      return(filtered_data)
    }
  } else {
    return(NULL)
  }
}

# Load entry list from NASCAR API
load_entry_list <- function(race_season, series_id, race_id) {
  tryCatch({
    url <- paste0(
      'https://cf.nascar.com/cacher/',
      race_season, '/', series_id, '/', race_id,
      '/weekend-feed.json'
    )
    
    json_data <- fromJSON(url)
    
    entry_list <- json_data$weekend_race %>%
      unnest(results, names_sep = "_") %>%
      select(
        results_driver_fullname,
        results_car_number,
        results_team_name,
        results_crew_chief_fullname,
        results_car_make,
        results_sponsor
      ) %>%
      rename(
        Name = results_driver_fullname,
        Car = results_car_number,
        Team = results_team_name,
        Make = results_car_make,
        CC = results_crew_chief_fullname,
        Sponsor = results_sponsor
      ) %>%
      mutate(Car = as.integer(Car)) %>%
      arrange(Car)
    
    return(entry_list)
  }, error = function(e) {
    return(data.frame(Name = character(), Car = integer(), Team = character(), 
                      CC = character(), Make = character(), Sponsor = character()))
  })
}

# Calculate dominator points available
calc_dom_points <- function(total_laps, green_laps) {
  dk_points <- (0.45 * green_laps) + (0.25 * total_laps)
  fd_points <- 0.1 * total_laps
  return(list(dk = round(dk_points, 1), fd = round(fd_points, 1)))
}

#--------------------- UI Definition ---------------------#

ui <- fluidPage(
  useShinyjs(),
  
  # Custom CSS - Black and Gold Theme
  tags$head(
    tags$style(HTML("
      /* Global styles */
      body {
        background-color: #1a1a1a !important;
        color: #ffffff !important;
        font-family: 'Helvetica Neue', Arial, sans-serif;
      }
      
      /* Header */
      .app-header {
        background-color: #000000;
        padding: 15px 30px;
        border-bottom: 3px solid #FFD700;
        display: flex;
        align-items: center;
        margin-bottom: 0;
      }
      
      .app-logo {
        height: 40px;
        margin-right: 15px;
      }
      
      .app-title {
        color: #FFD700;
        font-size: 24px;
        font-weight: bold;
        margin: 0;
      }
      
   /* Horizontal tabs */
      .nav-tabs {
        background-color: #000000;
        border-bottom: 3px solid #FFD700;
        padding-left: 20px;
        margin-bottom: 20px;
      }
      
      .nav-tabs > li > a {
        color: #FFD700 !important;
        background-color: #000000 !important;
        border: none !important;
        padding: 12px 25px;
        margin-right: 2px;
        font-weight: 500;
        transition: all 0.3s;
      }
      
      .nav-tabs > li > a:hover {
        background-color: #333333 !important;
        color: #FFD700 !important;
      }
      
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: #FFD700 !important;
        color: #000000 !important;
        border: none !important;
        font-weight: bold;
      }
      
      /* Main content */
      .container-fluid {
        padding: 20px 30px;
      }
      
      /* Boxes */
      .box {
        background-color: #2d2d2d !important;
        border: 1px solid #444444 !important;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,.3);
        margin-bottom: 20px;
        color: #ffffff !important;
      }
      
      .box-header {
        background-color: #333333 !important;
        color: #FFD700 !important;
        border-bottom: 2px solid #FFD700 !important;
        padding: 15px;
        border-radius: 5px 5px 0 0;
      }
      
      .box-title {
        color: #FFD700 !important;
        font-size: 18px;
        font-weight: bold;
      }
      
      .box-body {
        padding: 20px;
      }
      
      /* Buttons */
      .btn-primary {
        background-color: #FFD700 !important;
        border-color: #DAA520 !important;
        color: #000000 !important;
        font-weight: bold;
        transition: all 0.3s;
      }
      
      .btn-primary:hover, .btn-primary:focus {
        background-color: #DAA520 !important;
        border-color: #B8860B !important;
        color: #000000 !important;
        box-shadow: 0 0 10px rgba(255, 215, 0, 0.5);
      }
      
      .btn-success {
        background-color: #FFD700 !important;
        border-color: #DAA520 !important;
        color: #000000 !important;
        font-weight: bold;
      }
      
      .btn-success:hover {
        background-color: #DAA520 !important;
        border-color: #B8860B !important;
      }
      
      .btn-warning {
        background-color: #DAA520 !important;
        border-color: #B8860B !important;
        color: #000000 !important;
        font-weight: bold;
      }
      
      .btn-info {
        background-color: #555555 !important;
        border-color: #666666 !important;
        color: #FFD700 !important;
        font-weight: bold;
      }
      
      /* Form controls */
      .form-control, .selectize-input {
        background-color: #404040 !important;
        border: 1px solid #666666 !important;
        color: #ffffff !important;
      }
      
      .form-control:focus, .selectize-input.focus {
        border-color: #FFD700 !important;
        box-shadow: 0 0 0 0.2rem rgba(255, 215, 0, 0.25) !important;
      }
      
     .selectize-dropdown {
        background-color: #333333 !important;
        border: 1px solid #666666 !important;
        color: #ffffff !important;
      }
      
      .selectize-dropdown-content .option {
        color: #ffffff !important;
        background-color: #333333 !important;
      }
      
      .selectize-dropdown-content .option:hover,
      .selectize-dropdown-content .option.active {
        background-color: #FFD700 !important;
        color: #000000 !important;
      }
      
      .selectize-dropdown-content .optgroup-header {
        color: #FFD700 !important;
        background-color: #222222 !important;
      }
      
      /* Labels */
      label {
        color: #ffffff !important;
        font-weight: 500;
      }
      
      /* DataTables */
      .dataTables_wrapper {
        color: #ffffff !important;
      }
      
      .dataTable thead th {
        background-color: #333333 !important;
        color: #FFD700 !important;
        border-bottom: 2px solid #FFD700 !important;
        font-weight: bold;
        padding: 12px 8px;
      }
      
      .dataTable tbody td {
        background-color: #2d2d2d !important;
        color: #ffffff !important;
        border-bottom: 1px solid #444444 !important;
        padding: 10px 8px;
      }
      
      .dataTable tbody tr:hover {
        background-color: #404040 !important;
      }
      
      .dataTables_info, .dataTables_length label, .dataTables_filter label {
        color: #ffffff !important;
      }
      
      .dataTables_paginate .paginate_button {
        background-color: #333333 !important;
        color: #FFD700 !important;
        border: 1px solid #555555 !important;
        margin: 0 2px;
      }
      
      .dataTables_paginate .paginate_button:hover {
        background-color: #FFD700 !important;
        color: #000000 !important;
      }
      
      .dataTables_paginate .paginate_button.current {
        background-color: #FFD700 !important;
        color: #000000 !important;
        font-weight: bold;
      }
      
      /* Checkboxes */
      input[type='checkbox'] {
        cursor: pointer;
        width: 18px;
        height: 18px;
      }
      
      /* Section headers */
      .section-header {
        color: #FFD700;
        font-size: 20px;
        font-weight: bold;
        margin-bottom: 15px;
        padding-bottom: 10px;
        border-bottom: 2px solid #FFD700;
      }
      
      /* Info boxes */
      .info-box {
        background-color: #333333;
        border-left: 4px solid #FFD700;
        padding: 15px;
        margin-bottom: 15px;
        border-radius: 3px;
      }
      
      /* Plotly */
      .js-plotly-plot .plotly .modebar {
        background-color: #2d2d2d !important;
      }
      
      /* Toggle buttons */
      .btn-group-toggle label {
        background-color: #404040 !important;
        border-color: #666666 !important;
        color: #FFD700 !important;
      }
      
      .btn-group-toggle label.active {
        background-color: #FFD700 !important;
        color: #000000 !important;
      }
      
      /* Slider */
      .irs--shiny .irs-bar {
        background: #FFD700 !important;
        border-top: 1px solid #FFD700 !important;
        border-bottom: 1px solid #FFD700 !important;
      }
      
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
        background: #FFD700 !important;
        color: #000000 !important;
      }
      
      .irs--shiny .irs-handle {
        background: #FFD700 !important;
        border: 2px solid #DAA520 !important;
      }
      
      .irs--shiny .irs-min, .irs--shiny .irs-max {
        color: #FFD700 !important;
        background: #333333 !important;
      }
      
      .irs--shiny .irs-line {
        background: #555555 !important;
      }
    "))
  ),
  
  # Header
  div(class = "app-header",
      img(src = "logo.jpg", class = "app-logo"),
      h1("Golden Ticket Research", class = "app-title")
  ),
  
  # Navigation Tabs
  navbarPage(
    title = NULL,
    id = "main_tabs",
    windowTitle = "Golden Ticket Research",
    
    # Race Selection Tab
    tabPanel(
      "Race Selection",
      value = "race_selection",
      
      fluidRow(
        column(12,
               div(class = "box",
                   div(class = "box-header",
                       h3("Race Selection Configuration", class = "box-title")
                   ),
                   div(class = "box-body",
                       fluidRow(
                         column(3,
                                selectizeInput("analysis_series", "Series:",
                                               choices = c("Cup Series" = 1, "Xfinity Series" = 2, "Truck Series" = 3),
                                               selected = 1,
                                               options = list(placeholder = "Select Series"))
                         ),
                         column(3,
                                selectizeInput("analysis_primary_track", "Primary Track:",
                                               choices = NULL,
                                               selected = NULL,
                                               options = list(placeholder = "Select Track"))
                         ),
                         column(3,
                                selectizeInput("analysis_similar_tracks", "Similar Tracks (Optional):",
                                               choices = NULL,
                                               multiple = TRUE,
                                               options = list(placeholder = "Select Track(s)"))
                         ),
                         column(3,
                                selectizeInput("analysis_race_id", "Upcoming Race:",
                                               choices = NULL,
                                               selected = NULL,
                                               options = list(placeholder = "Select Race"))
                         )
                       ),
                       fluidRow(
                         column(3,
                                numericInput("analysis_start_year", "Start Year:", 
                                             value = 2019, min = 2000, max = 2025, step = 1)
                         ),
                         column(3,
                                numericInput("analysis_end_year", "End Year:", 
                                             value = 2025, min = 2000, max = 2025, step = 1)
                         ),
                         column(6,
                                div(style = "margin-top: 25px;",
                                    actionButton("confirm_analysis_filters", "Load Races", 
                                                 class = "btn-primary", 
                                                 style = "width: 100%; padding: 10px; font-size: 16px;"))
                         )
                       )
                   )
               )
        )
      ),
      
      # Races Table with Lap Filter
      conditionalPanel(
        condition = "output.filters_confirmed",
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Races Available for Analysis", class = "box-title")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(12,
                                  div(class = "info-box",
                                      p(style = "margin: 0; color: #ffffff;",
                                        strong("Dominator Lap Filter: "), 
                                        "Races with fewer laps can skew dominator analysis. Use the slider below to set minimum lap requirement for dominator analysis only. Other sections use all races."
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(12,
                                  div(class = "info-box",
                                      p(style = "margin: 0; color: #ffffff;",
                                        strong("Dominator Lap Filter: "), 
                                        "Races with fewer laps can skew dominator analysis. Set minimum and maximum lap requirements for dominator analysis only. Other sections use all races."
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(4,
                                  sliderInput("min_laps_dominator", 
                                              "Minimum Laps:",
                                              min = 0, max = 600, value = 0, step = 10)
                           ),
                           column(4,
                                  sliderInput("max_laps_dominator", 
                                              "Maximum Laps:",
                                              min = 0, max = 600, value = 600, step = 10)
                           ),
                           column(4,
                                  uiOutput("lap_filter_summary")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  withSpinner(DT::dataTableOutput("races_selection_table"))
                           )
                         )
                     )
                 )
          )
        )
      )
    ),
    
    # Entry List Tab
    tabPanel(
      "Entry List",
      value = "entry_list",
      
      conditionalPanel(
        condition = "output.filters_confirmed",
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("NASCAR Entry List", class = "box-title")
                     ),
                     div(class = "box-body",
                         withSpinner(DT::dataTableOutput("entry_list_table"))
                     )
                 )
          )
        )
      ),
      
      conditionalPanel(
        condition = "!output.filters_confirmed",
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("No Races Loaded", class = "box-title")
                     ),
                     div(class = "box-body",
                         p(style = "color: #ffffff; text-align: center; padding: 40px; font-size: 16px;",
                           "Please go to the Race Selection tab and load races first.")
                     )
                 )
          )
        )
      )
    ),
    
    # Dominator Tab
    tabPanel(
      "Dominator",
      value = "dominator",
      
      conditionalPanel(
        condition = "output.filters_confirmed",
        
        # Data Table Section
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Dominator Data", class = "box-title")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(12,
                                  downloadButton("download_dominator_csv", 
                                                 "Download CSV", 
                                                 class = "btn-success", 
                                                 style = "margin-bottom: 15px;")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  withSpinner(DT::dataTableOutput("dominator_data_table"))
                           )
                         )
                     )
                 )
          )
        ),
        
        # Visual Section
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Dominator Visualizations", class = "box-title")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(6,
                                  selectInput("dom_visual_type", "Select Visualization:",
                                              choices = c(
                                                "Score Distribution by Dom Rank" = "score_dist",
                                                "Dom Rank Finish Ranges" = "rank_finish",
                                                "Dom Pts by Finish Position" = "pts_by_finish",
                                                "Dom Pts by Starting Position" = "dom_pts_start",
                                                "Dom Rank by Starting Position" = "dom_rank_start",
                                                "Laps Led by Finish Position" = "laps_led",
                                                "Laps Led by Starting Position" = "laps_led_start",
                                                "Fast Laps by Finish Position" = "fast_laps",
                                                "Fast Laps by Starting Position" = "fast_laps_start",
                                                "Driver Dominator Boxplots" = "driver_boxplot",
                                                "Team Dominator Boxplots" = "team_boxplot"
                                              ),
                                              selected = "score_dist")
                           ),
                           column(6,
                                  radioButtons("dom_platform", "Platform:",
                                               choices = c("DraftKings" = "DK", "FanDuel" = "FD"),
                                               selected = "DK",
                                               inline = TRUE)
                           )
                         ),
                         fluidRow(
                           column(12,
                                  withSpinner(plotlyOutput("dominator_plot", height = "700px"))
                           )
                         )
                     )
                 )
          )
        )
      ),
      
      conditionalPanel(
        condition = "!output.filters_confirmed",
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("No Races Loaded", class = "box-title")
                     ),
                     div(class = "box-body",
                         p(style = "color: #ffffff; text-align: center; padding: 40px; font-size: 16px;",
                           "Please go to the Race Selection tab and load races first.")
                     )
                 )
          )
        )
      )
    ),
    
    # Place Differential Tab
    tabPanel(
      "Place Differential",
      value = "place_differential",
      
      conditionalPanel(
        condition = "output.filters_confirmed",
        
        # Data Table Section
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Place Differential Data", class = "box-title")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(12,
                                  downloadButton("download_pd_csv", 
                                                 "Download CSV", 
                                                 class = "btn-success", 
                                                 style = "margin-bottom: 15px;")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  withSpinner(DT::dataTableOutput("pd_data_table"))
                           )
                         )
                     )
                 )
          )
        ),
        
        # Visual Section
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Place Differential Visualizations", class = "box-title")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(12,
                                  selectInput("pd_visual_type", "Select Visualization:",
                                              choices = c(
                                                "Starting vs Finishing Position" = "scatter",
                                                "Position Change Distribution" = "histogram",
                                                "PD by Starting Position" = "boxplot_start",
                                                "PD by Finishing Position" = "boxplot_finish"
                                              ),
                                              selected = "scatter")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  withSpinner(plotlyOutput("pd_plot", height = "700px"))
                           )
                         )
                     )
                 )
          )
        )
      ),
      
      conditionalPanel(
        condition = "!output.filters_confirmed",
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("No Races Loaded", class = "box-title")
                     ),
                     div(class = "box-body",
                         p(style = "color: #ffffff; text-align: center; padding: 40px; font-size: 16px;",
                           "Please go to the Race Selection tab and load races first.")
                     )
                 )
          )
        )
      )
    ),
    
    # Performance Tab
    tabPanel(
      "Performance",
      value = "performance",
      
      conditionalPanel(
        condition = "output.filters_confirmed",
        
        # Data Table Section
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Performance Data", class = "box-title")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(6,
                                  radioButtons("perf_time_filter", "Time Period:",
                                               choices = c("Full History" = "all", "2025 Only" = "2025"),
                                               selected = "all",
                                               inline = TRUE)
                           ),
                           column(6,
                                  downloadButton("download_performance_csv", 
                                                 "Download CSV", 
                                                 class = "btn-success", 
                                                 style = "margin-top: 0px;")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  withSpinner(DT::dataTableOutput("performance_data_table"))
                           )
                         )
                     )
                 )
          )
        ),
        
        # Visual Section
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Performance Visualizations", class = "box-title")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(6,
                                  selectInput("perf_visual_type", "Select Visualization:",
                                              choices = c(
                                                "Driver Speed Rank Distribution" = "driver_speed",
                                                "Team Speed Rank Distribution" = "team_speed",
                                                "Driver Finish Distribution" = "driver_finish",
                                                "Team Finish Distribution" = "team_finish",
                                                "Driver ARP Distribution" = "driver_arp",
                                                "Team ARP Distribution" = "team_arp"
                                              ),
                                              selected = "driver_speed")
                           ),
                           column(6,
                                  radioButtons("perf_visual_time", "Time Period:",
                                               choices = c("Full History" = "all", "2025 Only" = "2025"),
                                               selected = "all",
                                               inline = TRUE)
                           )
                         ),
                         fluidRow(
                           column(12,
                                  withSpinner(plotlyOutput("performance_plot", height = "800px"))
                           )
                         )
                     )
                 )
          )
        )
      ),
      
      conditionalPanel(
        condition = "!output.filters_confirmed",
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("No Races Loaded", class = "box-title")
                     ),
                     div(class = "box-body",
                         p(style = "color: #ffffff; text-align: center; padding: 40px; font-size: 16px;",
                           "Please go to the Race Selection tab and load races first.")
                     )
                 )
          )
        )
      )
    ),
    
    # Fantasy Scoring Tab
    tabPanel(
      "Fantasy Scoring",
      value = "fantasy_scoring",
      
      conditionalPanel(
        condition = "output.filters_confirmed",
        
        # Data Table Section
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Fantasy Scoring Data", class = "box-title")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(6,
                                  radioButtons("fs_platform", "Platform:",
                                               choices = c("DraftKings" = "DK", "FanDuel" = "FD"),
                                               selected = "DK",
                                               inline = TRUE)
                           ),
                           column(6,
                                  downloadButton("download_fantasy_csv", 
                                                 "Download CSV", 
                                                 class = "btn-success", 
                                                 style = "margin-top: 0px;")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  withSpinner(DT::dataTableOutput("fantasy_data_table"))
                           )
                         )
                     )
                 )
          )
        ),
        
        # Visual Section
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("Fantasy Scoring Visualizations", class = "box-title")
                     ),
                     div(class = "box-body",
                         fluidRow(
                           column(6,
                                  selectInput("fs_visual_type", "Select Visualization:",
                                              choices = c(
                                                "Score Distribution by Rank" = "score_dist",
                                                "Scoring Components Breakdown" = "components",
                                                "Score Distribution by Start Position" = "score_by_start",
                                                "Score Distribution by Finish Position" = "score_by_finish",
                                                "Components by Start Position" = "components_start",
                                                "Components by Finish Position" = "components_finish"
                                              ),
                                              selected = "score_dist")
                           ),
                           column(6,
                                  radioButtons("fs_visual_platform", "Platform:",
                                               choices = c("DraftKings" = "DK", "FanDuel" = "FD"),
                                               selected = "DK",
                                               inline = TRUE)
                           )
                         ),
                         fluidRow(
                           column(12,
                                  withSpinner(plotlyOutput("fantasy_plot", height = "700px"))
                           )
                         )
                     )
                 )
          )
        )
      ),
      
      conditionalPanel(
        condition = "!output.filters_confirmed",
        fluidRow(
          column(12,
                 div(class = "box",
                     div(class = "box-header",
                         h3("No Races Loaded", class = "box-title")
                     ),
                     div(class = "box-body",
                         p(style = "color: #ffffff; text-align: center; padding: 40px; font-size: 16px;",
                           "Please go to the Race Selection tab and load races first.")
                     )
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
    
    # Race Selection
    analysis_filtered_data = NULL,
    analysis_entry_list = NULL,
    analysis_races_available = NULL,
    filters_confirmed = FALSE,
    
    # Section-specific race selections
    pd_race_ids = NULL,
    performance_race_ids = NULL,
    fantasy_race_ids = NULL
  )
  
  # Load initial data
  observe({
    withProgress(message = 'Loading Golden Ticket Database...', {
      values$nascar_data <- load_nascar_database()
      values$race_list <- load_race_list()
      
      if (!is.null(values$nascar_data)) {
        incProgress(0.3, detail = "Processing seasons...")
        seasons <- sort(unique(values$nascar_data$race_season), decreasing = TRUE)
        
        incProgress(0.5, detail = "Processing tracks...")
        all_race_data <- if(file.exists("RaceIDs.xlsx")) read_excel("RaceIDs.xlsx") else NULL
        if (!is.null(all_race_data)) {
          all_tracks <- sort(unique(all_race_data$track_name[!is.na(all_race_data$track_name)]))
        } else {
          all_tracks <- sort(unique(values$nascar_data$track_name[!is.na(values$nascar_data$track_name)]))
        }
        
        # Initialize track choices
        updateSelectizeInput(session, "analysis_primary_track", choices = all_tracks)
        updateSelectizeInput(session, "analysis_similar_tracks", choices = all_tracks)
        
        incProgress(1.0, detail = "Complete!")
      }
    })
  })
  
  # Update race ID choices when track/series/years change
  observe({
    req(input$analysis_series, input$analysis_primary_track, 
        input$analysis_start_year, input$analysis_end_year)
    
    all_races <- if(file.exists("RaceIDs.xlsx")) read_excel("RaceIDs.xlsx") else NULL
    
    if (!is.null(all_races)) {
      available_races <- all_races %>%
        filter(
          series_id == as.numeric(input$analysis_series),
          track_name == input$analysis_primary_track,
          race_season >= input$analysis_start_year,
          race_season <= input$analysis_end_year,
          Historical == "N"
        ) %>%
        arrange(desc(race_season)) %>%
        mutate(race_label = paste0(race_season, " - ", race_name))
      
      race_choices <- setNames(available_races$race_id, available_races$race_label)
      
      updateSelectizeInput(session, "analysis_race_id", choices = race_choices,
                           selected = if(length(race_choices) > 0) race_choices[1] else NULL)
    }
  })
  
  # Confirm filters and load data
  observeEvent(input$confirm_analysis_filters, {
    req(input$analysis_series, input$analysis_primary_track, 
        input$analysis_start_year, input$analysis_end_year, input$analysis_race_id)
    
    withProgress(message = 'Loading races...', {
      incProgress(0.2, detail = "Filtering races...")
      
      all_races <- if(file.exists("RaceIDs.xlsx")) read_excel("RaceIDs.xlsx") else values$race_list
      
      # Determine which tracks to include
      tracks_to_include <- input$analysis_primary_track
      if (!is.null(input$analysis_similar_tracks) && length(input$analysis_similar_tracks) > 0) {
        tracks_to_include <- c(tracks_to_include, input$analysis_similar_tracks)
      }
      
      # Filter race list - ONLY HISTORICAL RACES for analysis
      filtered_race_list <- all_races %>%
        filter(
          series_id == as.numeric(input$analysis_series),
          track_name %in% tracks_to_include,
          race_season >= input$analysis_start_year,
          race_season <= input$analysis_end_year,
          Historical == "Y"
        )
      
      if ("race_type_id" %in% names(filtered_race_list)) {
        filtered_race_list <- filtered_race_list %>% filter(race_type_id == 1)
      }
      
      incProgress(0.4, detail = "Loading race data...")
      
      # Filter main NASCAR data
      filtered_nascar <- values$nascar_data %>%
        filter(race_id %in% filtered_race_list$race_id)
      
      incProgress(0.6, detail = "Loading entry list...")
      
      # Load entry list from the SELECTED UPCOMING RACE
      race_info <- all_races %>%
        filter(race_id == as.numeric(input$analysis_race_id)) %>%
        slice(1)
      
      entry_list <- load_entry_list(
        race_info$race_season,
        as.numeric(input$analysis_series),
        as.numeric(input$analysis_race_id)
      )
      
      incProgress(0.8, detail = "Preparing race summary...")
      
      # Prepare races available data with summary stats
      races_available <- filtered_race_list %>%
        left_join(
          filtered_nascar %>%
            group_by(race_id) %>%
            summarize(
              total_laps = if("actual_laps" %in% names(cur_data())) first(actual_laps) 
              else if("TotalLaps" %in% names(cur_data())) first(TotalLaps)
              else NA_real_,
              caution_laps = if("number_of_caution_laps" %in% names(cur_data())) first(number_of_caution_laps)
              else if("CautionLaps" %in% names(cur_data())) first(CautionLaps)
              else 0,
              lead_lap = sum(LapsDown == 0, na.rm = TRUE),
              crash_dnfs = sum(finishing_status %in% c("Accident", "DVP", "Damage"), na.rm = TRUE),
              mech_dnfs = sum(!finishing_status %in% c("Running", "Accident", "DVP", "Damage"), na.rm = TRUE),
              .groups = 'drop'
            ),
          by = "race_id"
        ) %>%
        mutate(
          total_laps = if_else(is.na(total_laps), scheduled_laps, total_laps),
          green_flag_laps = total_laps - caution_laps
        ) %>%
        rowwise() %>%
        mutate(
          dom_points = list(calc_dom_points(total_laps, green_flag_laps)),
          DK_Dom_Available = dom_points$dk,
          FD_Dom_Available = dom_points$fd
        ) %>%
        select(-dom_points) %>%
        ungroup()
      
      # Store in reactive values
      values$analysis_filtered_data <- filtered_nascar
      values$analysis_entry_list <- entry_list
      values$analysis_races_available <- races_available
      
      # Initialize all races selected for non-dominator sections
      values$pd_race_ids <- races_available$race_id
      values$performance_race_ids <- races_available$race_id
      values$fantasy_race_ids <- races_available$race_id
      
      values$filters_confirmed <- TRUE
      
      incProgress(1.0, detail = "Complete!")
      
      showNotification(
        paste("Loaded", nrow(filtered_race_list), "historical races. Entry list from:", race_info$race_name),
        type = "message",
        duration = 5
      )
    })
  })
  
  # Output to show/hide sections
  output$filters_confirmed <- reactive({
    return(values$filters_confirmed)
  })
  outputOptions(output, "filters_confirmed", suspendWhenHidden = FALSE)
  
  # Races Selection Table (simplified - no checkboxes)
  output$races_selection_table <- DT::renderDataTable({
    req(values$analysis_races_available)
    
    race_selection_data <- values$analysis_races_available %>%
      select(
        Season = race_season,
        Track = track_name,
        Race = race_name,
        Cars = number_of_cars_in_field,
        Qualifying,
        Leaders = number_of_leaders,
        Cautions = number_of_cautions,
        `Total Laps` = total_laps,
        `GF Laps` = green_flag_laps,
        `DK Dom Avail` = DK_Dom_Available,
        `FD Dom Avail` = FD_Dom_Available,
        `Lead Lap` = lead_lap,
        `Crash DNFs` = crash_dnfs,
        `Mech DNFs` = mech_dnfs
      ) %>%
      arrange(desc(Season))
    
    DT::datatable(
      race_selection_data,
      selection = 'none',
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'tip',
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      rownames = FALSE,
      class = "display nowrap compact"
    )
  })
  
  output$lap_filter_summary <- renderUI({
    req(values$analysis_races_available, input$min_laps_dominator, input$max_laps_dominator)
    
    filtered_count <- values$analysis_races_available %>%
      filter(total_laps >= input$min_laps_dominator, 
             total_laps <= input$max_laps_dominator) %>%
      nrow()
    
    total_count <- nrow(values$analysis_races_available)
    
    tagList(
      h4(style = "color: #FFD700; margin-top: 30px;", 
         paste(filtered_count, "of", total_count, "races meet lap requirements"))
    )
  })
  
  # Dominator race IDs based on lap filter
  dominator_filtered_races <- reactive({
    req(values$analysis_races_available, input$min_laps_dominator, input$max_laps_dominator)
    
    values$analysis_races_available %>%
      filter(total_laps >= input$min_laps_dominator,
             total_laps <= input$max_laps_dominator) %>%
      pull(race_id)
  })
  
  #--------------------- Entry List Output ---------------------#
  
  output$entry_list_table <- DT::renderDataTable({
    req(values$analysis_entry_list)
    
    DT::datatable(
      values$analysis_entry_list,
      caption = "Current Entry List",
      options = list(
        pageLength = 40,
        scrollX = TRUE,
        dom = 'tip',
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      rownames = FALSE,
      class = "display nowrap compact"
    )
  })
  
  #--------------------- Dominator Section ---------------------#
  
  # Dominator filtered data
  dominator_data <- reactive({
    req(values$analysis_filtered_data, dominator_filtered_races())
    
    data <- values$analysis_filtered_data %>%
      filter(race_id %in% dominator_filtered_races())
    
    # Calculate dominator points if not present
    if (!"DKSP" %in% names(data) || all(is.na(data$DKSP))) {
      data <- data %>%
        group_by(race_id) %>%
        mutate(
          DKSP = (fast_laps * 0.45) + (lead_laps * 0.25),
          DKDomRank = dense_rank(desc(DKSP))
        ) %>%
        ungroup()
    }
    
    if (!"FDSP" %in% names(data) || all(is.na(data$FDSP))) {
      data <- data %>%
        group_by(race_id) %>%
        mutate(
          FDSP = lead_laps * 0.1,
          FDDomRank = dense_rank(desc(FDSP))
        ) %>%
        ungroup()
    }
    
    return(data)
  })
  
  # Dominator Data Table
  output$dominator_data_table <- DT::renderDataTable({
    req(dominator_data())
    
    display_data <- dominator_data() %>%
      filter(DKSP > 0 | FDSP > 0) %>%
      select(
        Driver = Full_Name,
        Start = start_ps,
        Finish = ps,
        Qualifying,
        Team = team_name,
        `Laps Led` = lead_laps,
        `Fast Laps` = fast_laps,
        `DK Dom Pts` = DKSP,
        `DK Dom Rank` = DKDomRank,
        `FD Dom Pts` = FDSP,
        `FD Dom Rank` = FDDomRank,
        Season = race_season,
        Race = race_name,
        Track = track_name
      ) %>%
      mutate(
        `DK Dom Pts` = round(`DK Dom Pts`, 1),
        `FD Dom Pts` = round(`FD Dom Pts`, 1)
      ) %>%
      arrange(desc(`DK Dom Pts`))
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'frtip',
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      rownames = FALSE,
      filter = "top",
      class = "display nowrap compact"
    )
  })
  
  # Download Dominator CSV
  output$download_dominator_csv <- downloadHandler(
    filename = function() {
      paste("dominator_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(dominator_data())
      
      export_data <- dominator_data() %>%
        filter(DKSP > 0 | FDSP > 0) %>%
        select(
          Driver = Full_Name,
          Start = start_ps,
          Finish = ps,
          Qualifying,
          Team = team_name,
          `Laps Led` = lead_laps,
          `Fast Laps` = fast_laps,
          `DK Dom Pts` = DKSP,
          `DK Dom Rank` = DKDomRank,
          `FD Dom Pts` = FDSP,
          `FD Dom Rank` = FDDomRank,
          Season = race_season,
          Race = race_name,
          Track = track_name
        ) %>%
        mutate(
          `DK Dom Pts` = round(`DK Dom Pts`, 1),
          `FD Dom Pts` = round(`FD Dom Pts`, 1)
        )
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  

  
  #--------------------- Dominator Visualizations ---------------------#
  
  output$dominator_plot <- renderPlotly({
    req(dominator_data(), input$dom_visual_type, input$dom_platform)
    
    plot_data <- dominator_data()
    platform <- input$dom_platform
    
    # Set platform-specific variables
    if (platform == "DK") {
      dom_pts_col <- "DKSP"
      dom_rank_col <- "DKDomRank"
      platform_name <- "DraftKings"
    } else {
      dom_pts_col <- "FDSP"
      dom_rank_col <- "FDDomRank"
      platform_name <- "FanDuel"
    }
    
    # Create visualization based on selected type
    if (input$dom_visual_type == "score_dist") {
      # Score Distribution by Dom Rank
      viz_data <- plot_data %>%
        filter(!!sym(dom_rank_col) <= 10, !!sym(dom_pts_col) > 0)
      
      p <- ggplot(viz_data, aes(x = factor(!!sym(dom_rank_col)), y = !!sym(dom_pts_col))) +
        geom_boxplot(aes(text = sprintf(
          "Dom Rank: %d\nDominator Points: %.1f\nDriver: %s\nTrack: %s",
          !!sym(dom_rank_col), !!sym(dom_pts_col), Full_Name, track_name
        )), fill = "forestgreen", alpha = 0.7) +
        geom_smooth(aes(x = as.numeric(!!sym(dom_rank_col)), y = !!sym(dom_pts_col)), 
                    method = "loess", se = FALSE, color = "#FFD700", size = 1.5) +
        labs(
          title = paste(platform_name, "Dominator Points Distribution by Dom Rank (Top 10)"),
          x = "Dominator Rank",
          y = "Dominator Points"
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:10)) +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
      
    } else if (input$dom_visual_type == "rank_finish") {
      # Dom Rank Finish Ranges
      viz_data <- plot_data %>%
        filter(!!sym(dom_rank_col) <= 10, !!sym(dom_pts_col) > 0)
      
      p <- ggplot(viz_data, aes(x = factor(!!sym(dom_rank_col)), y = ps)) +
        geom_boxplot(aes(text = sprintf(
          "Dom Rank: %d\nFinish Position: %d\nDriver: %s\nTrack: %s",
          !!sym(dom_rank_col), ps, Full_Name, track_name
        )), fill = "darkgreen", alpha = 0.7) +
        geom_smooth(aes(x = as.numeric(!!sym(dom_rank_col)), y = ps), 
                    method = "loess", se = FALSE, color = "#FFD700", size = 1.5) +
        labs(
          title = paste("Where Have the Top", platform_name, "Dominators Finished"),
          x = "Dominator Rank",
          y = "Finish Position"
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:10)) +
        scale_y_continuous(breaks = seq(0, 40, 5)) +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
      
    } else if (input$dom_visual_type == "pts_by_finish") {
      # Dom Pts by Finish Position
      viz_data <- plot_data %>%
        filter(ps <= 40, !is.na(ps), !is.na(!!sym(dom_pts_col)))
      
      p <- ggplot(viz_data, aes(x = factor(ps), y = !!sym(dom_pts_col))) +
        geom_boxplot(aes(text = sprintf(
          "Position: %d\nDominator Points: %.1f\nDriver: %s\nTrack: %s",
          ps, !!sym(dom_pts_col), Full_Name, track_name
        )), fill = "darkgreen", alpha = 0.7) +
        geom_smooth(aes(x = ps, y = !!sym(dom_pts_col)), 
                    method = "loess", se = FALSE, color = "#FFD700", size = 1.5) +
        labs(
          title = paste(platform_name, "Dominator Points by Finish Position"),
          x = "Finish Position",
          y = "Dominator Points"
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:40)) +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
      
    } else if (input$dom_visual_type == "dom_pts_start") {
      # Dom Pts by Starting Position
      viz_data <- plot_data %>%
        filter(start_ps <= 40, !is.na(start_ps), !is.na(!!sym(dom_pts_col)))
      
      p <- ggplot(viz_data, aes(x = factor(start_ps), y = !!sym(dom_pts_col))) +
        geom_boxplot(aes(text = sprintf(
          "Start Position: %d\nDominator Points: %.1f\nDriver: %s\nTrack: %s",
          start_ps, !!sym(dom_pts_col), Full_Name, track_name
        )), fill = "darkgreen", alpha = 0.7) +
        geom_smooth(aes(x = start_ps, y = !!sym(dom_pts_col)), 
                    method = "loess", se = FALSE, color = "#FFD700", size = 1.5) +
        labs(
          title = paste(platform_name, "Dominator Points by Starting Position"),
          x = "Starting Position",
          y = "Dominator Points"
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:40)) +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
      
    } else if (input$dom_visual_type == "dom_rank_start") {
      # Dom Rank by Starting Position
      viz_data <- plot_data %>%
        filter(start_ps <= 40, !is.na(start_ps), !is.na(!!sym(dom_rank_col)), !!sym(dom_pts_col) > 0)
      
      p <- ggplot(viz_data, aes(x = factor(start_ps), y = !!sym(dom_rank_col))) +
        geom_boxplot(aes(text = sprintf(
          "Start Position: %d\nDom Rank: %d\nDriver: %s\nTrack: %s",
          start_ps, !!sym(dom_rank_col), Full_Name, track_name
        )), fill = "forestgreen", alpha = 0.7) +
        geom_smooth(aes(x = start_ps, y = !!sym(dom_rank_col)), 
                    method = "loess", se = FALSE, color = "#FFD700", size = 1.5) +
        labs(
          title = paste(platform_name, "Dominator Rank by Starting Position"),
          x = "Starting Position",
          y = "Dominator Rank"
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:40)) +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
      
    } else if (input$dom_visual_type == "laps_led") {
      # Laps Led by Finish Position
      viz_data <- plot_data %>%
        filter(ps <= 40, !is.na(ps), !is.na(lead_laps))
      
      p <- ggplot(viz_data, aes(x = factor(ps), y = lead_laps)) +
        geom_boxplot(aes(text = sprintf(
          "Position: %d\nLaps Led: %d\nDriver: %s\nTrack: %s",
          ps, lead_laps, Full_Name, track_name
        )), fill = "lightgreen", alpha = 0.7) +
        geom_smooth(aes(x = ps, y = lead_laps), 
                    method = "loess", se = FALSE, color = "#FFD700", size = 1.5) +
        labs(
          title = "Laps Led by Finish Position",
          x = "Finish Position",
          y = "Laps Led"
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:40)) +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
      
    } else if (input$dom_visual_type == "laps_led_start") {
      # Laps Led by Starting Position
      viz_data <- plot_data %>%
        filter(start_ps <= 40, !is.na(start_ps), !is.na(lead_laps))
      
      p <- ggplot(viz_data, aes(x = factor(start_ps), y = lead_laps)) +
        geom_boxplot(aes(text = sprintf(
          "Start Position: %d\nLaps Led: %d\nDriver: %s\nTrack: %s",
          start_ps, lead_laps, Full_Name, track_name
        )), fill = "lightgreen", alpha = 0.7) +
        geom_smooth(aes(x = start_ps, y = lead_laps), 
                    method = "loess", se = FALSE, color = "#FFD700", size = 1.5) +
        labs(
          title = "Laps Led by Starting Position",
          x = "Starting Position",
          y = "Laps Led"
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:40)) +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
      
    } else if (input$dom_visual_type == "fast_laps") {
      # Fast Laps by Finish Position
      viz_data <- plot_data %>%
        filter(ps <= 40, !is.na(ps), !is.na(fast_laps))
      
      p <- ggplot(viz_data, aes(x = factor(ps), y = fast_laps)) +
        geom_boxplot(aes(text = sprintf(
          "Position: %d\nFast Laps: %d\nDriver: %s\nTrack: %s",
          ps, fast_laps, Full_Name, track_name
        )), fill = "green", alpha = 0.7) +
        geom_smooth(aes(x = ps, y = fast_laps), 
                    method = "loess", se = FALSE, color = "#FFD700", size = 1.5) +
        labs(
          title = "Fast Laps by Finish Position",
          x = "Finish Position",
          y = "Fast Laps"
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:40)) +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
      
    } else if (input$dom_visual_type == "fast_laps_start") {
      # Fast Laps by Starting Position
      viz_data <- plot_data %>%
        filter(start_ps <= 40, !is.na(start_ps), !is.na(fast_laps))
      
      p <- ggplot(viz_data, aes(x = factor(start_ps), y = fast_laps)) +
        geom_boxplot(aes(text = sprintf(
          "Start Position: %d\nFast Laps: %d\nDriver: %s\nTrack: %s",
          start_ps, fast_laps, Full_Name, track_name
        )), fill = "green", alpha = 0.7) +
        geom_smooth(aes(x = start_ps, y = fast_laps), 
                    method = "loess", se = FALSE, color = "#FFD700", size = 1.5) +
        labs(
          title = "Fast Laps by Starting Position",
          x = "Starting Position",
          y = "Fast Laps"
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:40)) +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
      
    } else if (input$dom_visual_type == "driver_boxplot") {
      # Driver Dominator Boxplots
      req(values$analysis_entry_list)
      
      entry_drivers <- values$analysis_entry_list$Name
      
      driver_data <- plot_data %>%
        filter(Full_Name %in% entry_drivers) %>%
        group_by(Full_Name) %>%
        summarize(
          avg_dom = mean(!!sym(dom_pts_col), na.rm = TRUE),
          race_count = n(),
          .groups = 'drop'
        ) %>%
        filter(avg_dom > 0) %>%
        arrange(avg_dom)
      
      driver_order <- driver_data$Full_Name
      
      viz_data <- plot_data %>%
        filter(Full_Name %in% driver_order) %>%
        mutate(Driver = factor(Full_Name, levels = driver_order))
      
      plot_ly(
        type = "box",
        y = ~Driver,
        x = as.formula(paste0("~", dom_pts_col)),
        data = viz_data,
        orientation = "h",
        marker = list(color = "#FFD700", opacity = 0.6),
        line = list(color = "#DAA520"),
        fillcolor = "rgba(255, 215, 0, 0.3)",
        hoverinfo = "text",
        text = ~paste("Driver:", Full_Name, 
                      "<br>Dom Points:", round(!!sym(dom_pts_col), 1),
                      "<br>Race:", race_name)
      ) %>%
        layout(
          title = list(
            text = paste(platform_name, "Dominator Points Distribution by Driver"),
            font = list(size = 18, color = "#FFD700")
          ),
          xaxis = list(
            title = "Dominator Points", 
            color = "#ffffff",
            gridcolor = "#404040",
            zerolinecolor = "#666666"
          ),
          yaxis = list(
            title = "", 
            color = "#ffffff",
            gridcolor = "#404040"
          ),
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          height = 900,
          margin = list(l = 150),
          hovermode = "closest"
        )
      
    } else if (input$dom_visual_type == "team_boxplot") {
      # Team Dominator Boxplots
      req(values$analysis_entry_list)
      
      entry_teams <- unique(values$analysis_entry_list$Team)
      
      team_data <- plot_data %>%
        filter(team_name %in% entry_teams) %>%
        group_by(team_name) %>%
        summarize(
          median_dom = median(!!sym(dom_pts_col), na.rm = TRUE),
          count = n(),
          .groups = 'drop'
        ) %>%
        arrange(median_dom)
      
      team_order <- team_data$team_name
      
      viz_data <- plot_data %>%
        filter(team_name %in% team_order) %>%
        mutate(Team = factor(team_name, levels = team_order))
      
      plot_ly(
        type = "box",
        y = ~Team,
        x = as.formula(paste0("~", dom_pts_col)),
        data = viz_data,
        orientation = "h",
        marker = list(color = "#FFD700", opacity = 0.6),
        line = list(color = "#DAA520"),
        fillcolor = "rgba(255, 215, 0, 0.3)",
        hoverinfo = "text",
        text = ~paste("Team:", team_name, 
                      "<br>Dom Points:", round(!!sym(dom_pts_col), 1),
                      "<br>Driver:", Full_Name,
                      "<br>Track:", track_name)
      ) %>%
        layout(
          title = list(
            text = paste("Team", platform_name, "Dominator Points Distribution"),
            font = list(size = 18, color = "#FFD700")
          ),
          xaxis = list(
            title = "Dominator Points", 
            color = "#ffffff",
            gridcolor = "#404040",
            zerolinecolor = "#666666"
          ),
          yaxis = list(
            title = "", 
            color = "#ffffff",
            gridcolor = "#404040"
          ),
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          height = 900,
          margin = list(l = 150),
          hovermode = "closest"
        )
    }
  })
  
  #--------------------- Place Differential Section ---------------------#
  
  # Place Differential filtered data
  pd_data <- reactive({
    req(values$analysis_filtered_data, values$pd_race_ids)
    
    data <- values$analysis_filtered_data %>%
      filter(race_id %in% values$pd_race_ids, !is.na(start_ps), !is.na(ps)) %>%
      mutate(PD = start_ps - ps)
    
    return(data)
  })
  
  # Place Differential Data Table
  output$pd_data_table <- DT::renderDataTable({
    req(pd_data())
    
    display_data <- pd_data() %>%
      select(
        Driver = Full_Name,
        Team = team_name,
        Start = start_ps,
        Finish = ps,
        PD,
        Qualifying,
        Season = race_season,
        Race = race_name,
        Track = track_name
      ) %>%
      arrange(desc(PD))
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'frtip',
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      rownames = FALSE,
      filter = "top",
      class = "display nowrap compact"
    ) %>%
      formatStyle(
        'PD',
        backgroundColor = styleInterval(
          cuts = c(-10, -5, 0, 5, 10),
          values = c('#8B0000', '#CD5C5C', '#F0E68C', '#90EE90', '#228B22', '#006400')
        ),
        color = "#000000",
        fontWeight = "bold"
      )
  })
  
  # Download Place Differential CSV
  output$download_pd_csv <- downloadHandler(
    filename = function() {
      paste("place_differential_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(pd_data())
      
      export_data <- pd_data() %>%
        select(
          Driver = Full_Name,
          Team = team_name,
          Start = start_ps,
          Finish = ps,
          PD,
          Qualifying,
          Season = race_season,
          Race = race_name,
          Track = track_name
        )
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  # Performance Visualizations
  output$performance_plot <- renderPlotly({
    req(values$analysis_filtered_data, values$performance_race_ids, 
        input$perf_visual_type, input$perf_visual_time)
    
    # Apply time filter
    viz_data <- values$analysis_filtered_data %>%
      filter(race_id %in% values$performance_race_ids)
    
    if (input$perf_visual_time == "2025") {
      viz_data <- viz_data %>% filter(race_season == 2025)
    }
    
    time_label <- ifelse(input$perf_visual_time == "2025", "2025", "Full History")
    
    if (input$perf_visual_type == "driver_speed") {
      # Driver Speed Rank Distribution
      req(values$analysis_entry_list)
      
      entry_drivers <- values$analysis_entry_list$Name
      
      driver_data <- viz_data %>%
        filter(Full_Name %in% entry_drivers, !is.na(SpdRk)) %>%
        group_by(Full_Name) %>%
        summarize(
          avg_spdrk = mean(SpdRk, na.rm = TRUE),
          race_count = n(),
          .groups = 'drop'
        ) %>%
        filter(race_count > 0) %>%
        arrange(desc(avg_spdrk))
      
      driver_order <- driver_data$Full_Name
      
      plot_data <- viz_data %>%
        filter(Full_Name %in% driver_order, !is.na(SpdRk)) %>%
        mutate(Driver = factor(Full_Name, levels = driver_order))
      
      plot_ly(
        type = "box",
        y = ~Driver,
        x = ~SpdRk,
        data = plot_data,
        orientation = "h",
        marker = list(color = "#FFD700", opacity = 0.6),
        line = list(color = "#DAA520"),
        fillcolor = "rgba(255, 215, 0, 0.3)",
        hoverinfo = "text",
        text = ~paste("Driver:", Full_Name, 
                      "<br>Speed Rank:", round(SpdRk, 1),
                      "<br>Race:", race_name,
                      "<br>Track:", track_name)
      ) %>%
        layout(
          title = list(
            text = paste("Speed Rank Distribution by Driver -", time_label),
            font = list(size = 18, color = "#FFD700")
          ),
          xaxis = list(
            title = "Speed Rank", 
            color = "#ffffff",
            gridcolor = "#404040",
            zerolinecolor = "#666666"
          ),
          yaxis = list(
            title = "", 
            color = "#ffffff",
            gridcolor = "#404040"
          ),
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          height = 900,
          margin = list(l = 150),
          hovermode = "closest"
        )
      
    } else if (input$perf_visual_type == "team_speed") {
      # Team Speed Rank Distribution
      req(values$analysis_entry_list)
      
      entry_teams <- unique(values$analysis_entry_list$Team)
      
      team_data <- viz_data %>%
        filter(team_name %in% entry_teams, !is.na(SpdRk)) %>%
        group_by(team_name) %>%
        summarize(
          median_spdrk = median(SpdRk, na.rm = TRUE),
          count = n(),
          .groups = 'drop'
        ) %>%
        filter(count > 0) %>%
        arrange(desc(median_spdrk))
      
      team_order <- team_data$team_name
      
      plot_data <- viz_data %>%
        filter(team_name %in% team_order, !is.na(SpdRk)) %>%
        mutate(Team = factor(team_name, levels = team_order))
      
      plot_ly(
        type = "box",
        y = ~Team,
        x = ~SpdRk,
        data = plot_data,
        orientation = "h",
        marker = list(color = "#FFD700", opacity = 0.6),
        line = list(color = "#DAA520"),
        fillcolor = "rgba(255, 215, 0, 0.3)",
        hoverinfo = "text",
        text = ~paste("Team:", team_name, 
                      "<br>Speed Rank:", round(SpdRk, 1),
                      "<br>Driver:", Full_Name,
                      "<br>Track:", track_name)
      ) %>%
        layout(
          title = list(
            text = paste("Team Speed Rank Distribution -", time_label),
            font = list(size = 18, color = "#FFD700")
          ),
          xaxis = list(
            title = "Speed Rank", 
            color = "#ffffff",
            gridcolor = "#404040",
            zerolinecolor = "#666666"
          ),
          yaxis = list(
            title = "", 
            color = "#ffffff",
            gridcolor = "#404040"
          ),
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          height = 900,
          margin = list(l = 150),
          hovermode = "closest"
        )
      
    } else if (input$perf_visual_type == "driver_finish") {
      # Driver Finish Distribution
      req(values$analysis_entry_list)
      
      entry_drivers <- values$analysis_entry_list$Name
      
      driver_data <- viz_data %>%
        filter(Full_Name %in% entry_drivers, !is.na(ps)) %>%
        group_by(Full_Name) %>%
        summarize(
          avg_finish = mean(ps, na.rm = TRUE),
          race_count = n(),
          .groups = 'drop'
        ) %>%
        filter(race_count > 0) %>%
        arrange(desc(avg_finish))
      
      driver_order <- driver_data$Full_Name
      
      plot_data <- viz_data %>%
        filter(Full_Name %in% driver_order, !is.na(ps)) %>%
        mutate(Driver = factor(Full_Name, levels = driver_order))
      
      plot_ly(
        type = "box",
        y = ~Driver,
        x = ~ps,
        data = plot_data,
        orientation = "h",
        marker = list(color = "#FFD700", opacity = 0.6),
        line = list(color = "#DAA520"),
        fillcolor = "rgba(255, 215, 0, 0.3)",
        hoverinfo = "text",
        text = ~paste("Driver:", Full_Name, 
                      "<br>Finish:", ps,
                      "<br>Race:", race_name,
                      "<br>Track:", track_name)
      ) %>%
        layout(
          title = list(
            text = paste("Finish Position Distribution by Driver -", time_label),
            font = list(size = 18, color = "#FFD700")
          ),
          xaxis = list(
            title = "Finish Position", 
            color = "#ffffff",
            gridcolor = "#404040",
            zerolinecolor = "#666666"
          ),
          yaxis = list(
            title = "", 
            color = "#ffffff",
            gridcolor = "#404040"
          ),
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          height = 900,
          margin = list(l = 150),
          hovermode = "closest"
        )
      
    } else if (input$perf_visual_type == "team_finish") {
      # Team Finish Distribution
      req(values$analysis_entry_list)
      
      entry_teams <- unique(values$analysis_entry_list$Team)
      
      team_data <- viz_data %>%
        filter(team_name %in% entry_teams, !is.na(ps)) %>%
        group_by(team_name) %>%
        summarize(
          median_finish = median(ps, na.rm = TRUE),
          count = n(),
          .groups = 'drop'
        ) %>%
        filter(count > 0) %>%
        arrange(desc(median_finish))
      
      team_order <- team_data$team_name
      
      plot_data <- viz_data %>%
        filter(team_name %in% team_order, !is.na(ps)) %>%
        mutate(Team = factor(team_name, levels = team_order))
      
      plot_ly(
        type = "box",
        y = ~Team,
        x = ~ps,
        data = plot_data,
        orientation = "h",
        marker = list(color = "#FFD700", opacity = 0.6),
        line = list(color = "#DAA520"),
        fillcolor = "rgba(255, 215, 0, 0.3)",
        hoverinfo = "text",
        text = ~paste("Team:", team_name, 
                      "<br>Finish:", ps,
                      "<br>Driver:", Full_Name,
                      "<br>Track:", track_name)
      ) %>%
        layout(
          title = list(
            text = paste("Team Finish Position Distribution -", time_label),
            font = list(size = 18, color = "#FFD700")
          ),
          xaxis = list(
            title = "Finish Position", 
            color = "#ffffff",
            gridcolor = "#404040",
            zerolinecolor = "#666666"
          ),
          yaxis = list(
            title = "", 
            color = "#ffffff",
            gridcolor = "#404040"
          ),
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          height = 900,
          margin = list(l = 150),
          hovermode = "closest"
        )
      
    } else if (input$perf_visual_type == "driver_arp") {
      # Driver ARP Distribution
      req(values$analysis_entry_list)
      
      entry_drivers <- values$analysis_entry_list$Name
      
      driver_data <- viz_data %>%
        filter(Full_Name %in% entry_drivers, !is.na(ARP)) %>%
        group_by(Full_Name) %>%
        summarize(
          avg_arp = mean(ARP, na.rm = TRUE),
          race_count = n(),
          .groups = 'drop'
        ) %>%
        filter(race_count > 0) %>%
        arrange(desc(avg_arp))
      
      driver_order <- driver_data$Full_Name
      
      plot_data <- viz_data %>%
        filter(Full_Name %in% driver_order, !is.na(ARP)) %>%
        mutate(Driver = factor(Full_Name, levels = driver_order))
      
      plot_ly(
        type = "box",
        y = ~Driver,
        x = ~ARP,
        data = plot_data,
        orientation = "h",
        marker = list(color = "#FFD700", opacity = 0.6),
        line = list(color = "#DAA520"),
        fillcolor = "rgba(255, 215, 0, 0.3)",
        hoverinfo = "text",
        text = ~paste("Driver:", Full_Name, 
                      "<br>ARP:", round(ARP, 1),
                      "<br>Race:", race_name,
                      "<br>Track:", track_name)
      ) %>%
        layout(
          title = list(
            text = paste("Average Running Position Distribution by Driver -", time_label),
            font = list(size = 18, color = "#FFD700")
          ),
          xaxis = list(
            title = "Average Running Position", 
            color = "#ffffff",
            gridcolor = "#404040",
            zerolinecolor = "#666666"
          ),
          yaxis = list(
            title = "", 
            color = "#ffffff",
            gridcolor = "#404040"
          ),
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          height = 900,
          margin = list(l = 150),
          hovermode = "closest"
        )
      
    } else if (input$perf_visual_type == "team_arp") {
      # Team ARP Distribution
      req(values$analysis_entry_list)
      
      entry_teams <- unique(values$analysis_entry_list$Team)
      
      team_data <- viz_data %>%
        filter(team_name %in% entry_teams, !is.na(ARP)) %>%
        group_by(team_name) %>%
        summarize(
          median_arp = median(ARP, na.rm = TRUE),
          count = n(),
          .groups = 'drop'
        ) %>%
        filter(count > 0) %>%
        arrange(desc(median_arp))
      
      team_order <- team_data$team_name
      
      plot_data <- viz_data %>%
        filter(team_name %in% team_order, !is.na(ARP)) %>%
        mutate(Team = factor(team_name, levels = team_order))
      
      plot_ly(
        type = "box",
        y = ~Team,
        x = ~ARP,
        data = plot_data,
        orientation = "h",
        marker = list(color = "#FFD700", opacity = 0.6),
        line = list(color = "#DAA520"),
        fillcolor = "rgba(255, 215, 0, 0.3)",
        hoverinfo = "text",
        text = ~paste("Team:", team_name, 
                      "<br>ARP:", round(ARP, 1),
                      "<br>Driver:", Full_Name,
                      "<br>Track:", track_name)
      ) %>%
        layout(
          title = list(
            text = paste("Team Average Running Position Distribution -", time_label),
            font = list(size = 18, color = "#FFD700")
          ),
          xaxis = list(
            title = "Average Running Position", 
            color = "#ffffff",
            gridcolor = "#404040",
            zerolinecolor = "#666666"
          ),
          yaxis = list(
            title = "", 
            color = "#ffffff",
            gridcolor = "#404040"
          ),
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          height = 900,
          margin = list(l = 150),
          hovermode = "closest"
        )
    }
  })
  
  #--------------------- Performance Section ---------------------#
  
  # Performance filtered data
  performance_data <- reactive({
    req(values$analysis_filtered_data, values$performance_race_ids, input$perf_time_filter)
    
    data <- values$analysis_filtered_data %>%
      filter(race_id %in% values$performance_race_ids)
    
    # Apply time filter
    if (input$perf_time_filter == "2025") {
      data <- data %>% filter(race_season == 2025)
    }
    
    return(data)
  })
  
  # Performance Data Table
  output$performance_data_table <- DT::renderDataTable({
    req(performance_data())
    
    # Select relevant columns similar to Data Explorer
    default_columns <- c("Full_Name", "start_ps", "ps", "ARP", "SpdRk", "fl", "ll", 
                         "DKSP", "FDSP", "DKDomRank", "FDDomRank", "DKPoints", "FDPoints",
                         "car_number", "team_name", "race_season", "track_name", 
                         "finishing_status", "LapsDown", "Qualifying")
    
    valid_columns <- intersect(default_columns, names(performance_data()))
    
    display_data <- performance_data() %>%
      select(all_of(valid_columns)) %>%
      mutate(
        ARP = if("ARP" %in% names(.)) round(ARP, 1) else ARP,
        DKSP = if("DKSP" %in% names(.)) round(DKSP, 1) else DKSP,
        FDSP = if("FDSP" %in% names(.)) round(FDSP, 1) else FDSP,
        DKPoints = if("DKPoints" %in% names(.)) round(DKPoints, 1) else DKPoints,
        FDPoints = if("FDPoints" %in% names(.)) round(FDPoints, 1) else FDPoints
      ) %>%
      rename_with(~ case_when(
        .x == "Full_Name" ~ "Driver Name",
        .x == "start_ps" ~ "Start",
        .x == "ps" ~ "Finish", 
        .x == "fl" ~ "FL",
        .x == "ll" ~ "LL",
        .x == "DKSP" ~ "DK Dom Pts",
        .x == "FDSP" ~ "FD Dom Pts",
        .x == "DKPoints" ~ "DK Points",
        .x == "FDPoints" ~ "FD Points",
        .x == "DKDomRank" ~ "DK Dom Rank",
        .x == "FDDomRank" ~ "FD Dom Rank",
        .x == "car_number" ~ "Car",
        .x == "team_name" ~ "Team",
        .x == "race_season" ~ "Season",
        .x == "track_name" ~ "Track",
        .x == "finishing_status" ~ "Status",
        .x == "LapsDown" ~ "Laps Down",
        TRUE ~ .x
      ))
    
    dt <- DT::datatable(
      display_data,
      options = list(
        scrollX = TRUE,
        pageLength = 25,
        dom = 'frtip',
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      filter = 'top',
      rownames = FALSE,
      escape = FALSE,
      class = "display nowrap compact"
    )
    
    # Add gradient formatting for position columns
    position_columns <- intersect(c("Start", "Finish", "ARP", "SpdRk"), 
                                  names(display_data))
    
    for (col in position_columns) {
      if (col %in% names(display_data)) {
        col_values <- display_data[[col]]
        col_values <- col_values[!is.na(col_values)]
        
        if (length(col_values) > 0) {
          min_val <- min(col_values)
          max_val <- max(col_values)
          
          dt <- dt %>% formatStyle(
            col,
            backgroundColor = JS(paste0(
              "function(value, type, row) {",
              "if (type === 'display' && value !== null && value !== '') {",
              "  var min = ", min_val, ";",
              "  var max = ", max_val, ";",
              "  var normalized = (value - min) / (max - min);",
              "  var red = Math.round(255 - (255 - 218) * normalized);",
              "  var green = Math.round(215 - (215 - 165) * normalized);",
              "  var blue = Math.round(0 + 32 * normalized);",
              "  return 'rgb(' + red + ',' + green + ',' + blue + ')';",
              "} else {",
              "  return '#2d2d2d';",
              "}",
              "}"
            )),
            color = "#000000",
            fontWeight = "bold"
          )
        }
      }
    }
    
    return(dt)
  })
  
  # Download Performance CSV
  output$download_performance_csv <- downloadHandler(
    filename = function() {
      time_label <- ifelse(input$perf_time_filter == "2025", "2025", "full_history")
      paste("performance_data_", time_label, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(performance_data())
      
      default_columns <- c("Full_Name", "start_ps", "ps", "ARP", "SpdRk", "fl", "ll", 
                           "DKSP", "FDSP", "DKDomRank", "FDDomRank", "DKPoints", "FDPoints",
                           "car_number", "team_name", "race_season", "track_name", 
                           "finishing_status", "LapsDown", "Qualifying")
      
      valid_columns <- intersect(default_columns, names(performance_data()))
      
      export_data <- performance_data() %>%
        select(all_of(valid_columns)) %>%
        mutate(
          ARP = if("ARP" %in% names(.)) round(ARP, 1) else ARP,
          DKSP = if("DKSP" %in% names(.)) round(DKSP, 1) else DKSP,
          FDSP = if("FDSP" %in% names(.)) round(FDSP, 1) else FDSP
        ) %>%
        rename_with(~ case_when(
          .x == "Full_Name" ~ "Driver Name",
          .x == "start_ps" ~ "Start",
          .x == "ps" ~ "Finish", 
          .x == "fl" ~ "FL",
          .x == "ll" ~ "LL",
          .x == "DKSP" ~ "DK Dom Pts",
          .x == "FDSP" ~ "FD Dom Pts",
          .x == "car_number" ~ "Car",
          .x == "team_name" ~ "Team",
          .x == "race_season" ~ "Season",
          .x == "track_name" ~ "Track",
          .x == "finishing_status" ~ "Status",
          .x == "LapsDown" ~ "Laps Down",
          TRUE ~ .x
        ))
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  

  # Fantasy Scoring Visualizations
  output$fantasy_plot <- renderPlotly({
    req(fantasy_data(), input$fs_visual_type, input$fs_visual_platform)
    
    plot_data <- fantasy_data()
    platform <- input$fs_visual_platform
    
    # Set platform-specific variables
    if (platform == "DK") {
      points_col <- "DKPoints"
      rank_col <- "DKRank"
      fp_col <- "DKFP"
      pd_col <- "DKPD"
      sp_col <- "DKSP"
      platform_name <- "DraftKings"
    } else {
      points_col <- "FDPoints"
      rank_col <- "FDRank"
      fp_col <- "FDFP"
      pd_col <- "FDPD"
      sp_col <- "FDSP"
      platform_name <- "FanDuel"
    }
    
    if (input$fs_visual_type == "score_dist") {
      # Score Distribution by Rank
      viz_data <- plot_data %>%
        filter(!!sym(rank_col) <= 15)
      
      p <- ggplot(viz_data, aes(x = factor(!!sym(rank_col)), y = !!sym(points_col))) +
        geom_boxplot(aes(text = sprintf(
          "Rank: %d\nPoints: %.1f\nDriver: %s\nTrack: %s",
          !!sym(rank_col), !!sym(points_col), Full_Name, track_name
        )), fill = "dodgerblue", alpha = 0.7) +
        geom_smooth(aes(x = as.numeric(!!sym(rank_col)), y = !!sym(points_col), group = 1), 
                    method = "loess", se = FALSE, color = "#FFD700", size = 1.5) +
        labs(
          title = paste(platform_name, "Points Distribution by Fantasy Rank"),
          x = paste(platform_name, "Rank"),
          y = paste(platform_name, "Points")
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:15)) +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
      
    } else if (input$fs_visual_type == "components") {
      # Scoring Components Breakdown
      if (platform == "DK") {
        point_composition <- plot_data %>%
          filter(!!sym(rank_col) <= 15) %>%
          mutate(
            Finish_Pct = round(DKFP / DKPoints * 100, 1),
            PD_Pct = round(DKPD / DKPoints * 100, 1),
            Dominator_Pct = round(DKSP / DKPoints * 100, 1)
          )
      } else {
        point_composition <- plot_data %>%
          filter(!!sym(rank_col) <= 15) %>%
          mutate(
            Finish_Pct = round(FDFP / (FDPoints - FDLP) * 100, 1),
            PD_Pct = round(FDPD / (FDPoints - FDLP) * 100, 1),
            Dominator_Pct = round(FDSP / (FDPoints - FDLP) * 100, 1)
          )
      }
      
      comp_data <- point_composition %>%
        group_by(!!sym(rank_col)) %>%
        summarize(
          Finish_Pct = mean(Finish_Pct, na.rm = TRUE),
          PD_Pct = mean(PD_Pct, na.rm = TRUE),
          Dominator_Pct = mean(Dominator_Pct, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        pivot_longer(
          cols = c(Finish_Pct, PD_Pct, Dominator_Pct),
          names_to = "Point_Type",
          values_to = "Percentage"
        ) %>%
        mutate(
          Point_Type = case_when(
            Point_Type == "Finish_Pct" ~ "Finish Position",
            Point_Type == "PD_Pct" ~ "Place Differential",
            Point_Type == "Dominator_Pct" ~ "Dominator Points",
            TRUE ~ Point_Type
          )
        )
      
      p <- ggplot(comp_data, aes(x = factor(!!sym(rank_col)), y = Percentage, fill = Point_Type)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
                  position = position_stack(vjust = 0.5), 
                  color = "white", fontface = "bold", size = 4) +
        scale_fill_manual(values = c("Finish Position" = "#3406cc", 
                                     "Place Differential" = "#33cc33", 
                                     "Dominator Points" = "#ff9900")) +
        labs(
          title = paste(platform_name, "Scoring Components Breakdown"),
          subtitle = "What Type of Points are Driving Top Scores",
          x = paste(platform_name, "Rank"),
          y = "Points Composition (%)",
          fill = "Point Type"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          plot.subtitle = element_text(size = 14, color = "#ffffff"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          legend.title = element_text(size = 14, color = "#FFD700"),
          legend.text = element_text(size = 12, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333"),
          legend.background = element_rect(fill = "#2d2d2d"),
          legend.key = element_rect(fill = "#2d2d2d")
        )
      
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
      
    } else if (input$fs_visual_type == "score_by_start") {
      # Score Distribution by Start Position
      viz_data <- plot_data %>%
        filter(start_ps <= 40, !is.na(start_ps), !is.na(!!sym(points_col)))
      
      p <- ggplot(viz_data, aes(x = factor(start_ps), y = !!sym(points_col))) +
        geom_boxplot(aes(text = sprintf(
          "Start: %d\nPoints: %.1f\nDriver: %s\nTrack: %s",
          start_ps, !!sym(points_col), Full_Name, track_name
        )), fill = "purple", alpha = 0.6) +
        geom_smooth(aes(x = start_ps, y = !!sym(points_col)), 
                    method = "loess", se = FALSE, color = "#FFD700", size = 1.5) +
        labs(
          title = paste(platform_name, "Fantasy Points by Starting Position"),
          x = "Starting Position",
          y = paste(platform_name, "Points")
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:40)) +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
      
    } else if (input$fs_visual_type == "score_by_finish") {
      # Score Distribution by Finish Position
      viz_data <- plot_data %>%
        filter(ps <= 40, !is.na(ps), !is.na(!!sym(points_col)))
      
      p <- ggplot(viz_data, aes(x = factor(ps), y = !!sym(points_col))) +
        geom_boxplot(aes(text = sprintf(
          "Finish: %d\nPoints: %.1f\nDriver: %s\nTrack: %s",
          ps, !!sym(points_col), Full_Name, track_name
        )), fill = "orange", alpha = 0.6) +
        geom_smooth(aes(x = ps, y = !!sym(points_col)), 
                    method = "loess", se = FALSE, color = "#FFD700", size = 1.5) +
        labs(
          title = paste(platform_name, "Fantasy Points by Finish Position"),
          x = "Finish Position",
          y = paste(platform_name, "Points")
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:40)) +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
      
    } else if (input$fs_visual_type == "components_start") {
      # Components by Start Position
      if (platform == "DK") {
        viz_data <- plot_data %>%
          filter(start_ps <= 20, !is.na(start_ps)) %>%
          mutate(
            Finish_Pct = round(DKFP / DKPoints * 100, 1),
            PD_Pct = round(DKPD / DKPoints * 100, 1),
            Dominator_Pct = round(DKSP / DKPoints * 100, 1)
          )
      } else {
        viz_data <- plot_data %>%
          filter(start_ps <= 20, !is.na(start_ps)) %>%
          mutate(
            Finish_Pct = round(FDFP / (FDPoints - FDLP) * 100, 1),
            PD_Pct = round(FDPD / (FDPoints - FDLP) * 100, 1),
            Dominator_Pct = round(FDSP / (FDPoints - FDLP) * 100, 1)
          )
      }
      
      comp_data <- viz_data %>%
        group_by(start_ps) %>%
        summarize(
          Finish_Pct = mean(Finish_Pct, na.rm = TRUE),
          PD_Pct = mean(PD_Pct, na.rm = TRUE),
          Dominator_Pct = mean(Dominator_Pct, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        pivot_longer(
          cols = c(Finish_Pct, PD_Pct, Dominator_Pct),
          names_to = "Point_Type",
          values_to = "Percentage"
        ) %>%
        mutate(
          Point_Type = case_when(
            Point_Type == "Finish_Pct" ~ "Finish Position",
            Point_Type == "PD_Pct" ~ "Place Differential",
            Point_Type == "Dominator_Pct" ~ "Dominator Points",
            TRUE ~ Point_Type
          )
        )
      
      p <- ggplot(comp_data, aes(x = factor(start_ps), y = Percentage, fill = Point_Type)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = sprintf("%.0f%%", Percentage)), 
                  position = position_stack(vjust = 0.5), 
                  color = "white", fontface = "bold", size = 3) +
        scale_fill_manual(values = c("Finish Position" = "#3406cc", 
                                     "Place Differential" = "#33cc33", 
                                     "Dominator Points" = "#ff9900")) +
        labs(
          title = paste(platform_name, "Scoring Components by Starting Position"),
          subtitle = "Top 20 Starting Positions",
          x = "Starting Position",
          y = "Points Composition (%)",
          fill = "Point Type"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          plot.subtitle = element_text(size = 14, color = "#ffffff"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          legend.title = element_text(size = 14, color = "#FFD700"),
          legend.text = element_text(size = 12, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333"),
          legend.background = element_rect(fill = "#2d2d2d"),
          legend.key = element_rect(fill = "#2d2d2d")
        )
      
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
      
    } else if (input$fs_visual_type == "components_finish") {
      # Components by Finish Position
      if (platform == "DK") {
        viz_data <- plot_data %>%
          filter(ps <= 20, !is.na(ps)) %>%
          mutate(
            Finish_Pct = round(DKFP / DKPoints * 100, 1),
            PD_Pct = round(DKPD / DKPoints * 100, 1),
            Dominator_Pct = round(DKSP / DKPoints * 100, 1)
          )
      } else {
        viz_data <- plot_data %>%
          filter(ps <= 20, !is.na(ps)) %>%
          mutate(
            Finish_Pct = round(FDFP / (FDPoints - FDLP) * 100, 1),
            PD_Pct = round(FDPD / (FDPoints - FDLP) * 100, 1),
            Dominator_Pct = round(FDSP / (FDPoints - FDLP) * 100, 1)
          )
      }
      
      comp_data <- viz_data %>%
        group_by(ps) %>%
        summarize(
          Finish_Pct = mean(Finish_Pct, na.rm = TRUE),
          PD_Pct = mean(PD_Pct, na.rm = TRUE),
          Dominator_Pct = mean(Dominator_Pct, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        pivot_longer(
          cols = c(Finish_Pct, PD_Pct, Dominator_Pct),
          names_to = "Point_Type",
          values_to = "Percentage"
        ) %>%
        mutate(
          Point_Type = case_when(
            Point_Type == "Finish_Pct" ~ "Finish Position",
            Point_Type == "PD_Pct" ~ "Place Differential",
            Point_Type == "Dominator_Pct" ~ "Dominator Points",
            TRUE ~ Point_Type
          )
        )
      
      p <- ggplot(comp_data, aes(x = factor(ps), y = Percentage, fill = Point_Type)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = sprintf("%.0f%%", Percentage)), 
                  position = position_stack(vjust = 0.5), 
                  color = "white", fontface = "bold", size = 3) +
        scale_fill_manual(values = c("Finish Position" = "#3406cc", 
                                     "Place Differential" = "#33cc33", 
                                     "Dominator Points" = "#ff9900")) +
        labs(
          title = paste(platform_name, "Scoring Components by Finish Position"),
          subtitle = "Top 20 Finishing Positions",
          x = "Finish Position",
          y = "Points Composition (%)",
          fill = "Point Type"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#FFD700"),
          plot.subtitle = element_text(size = 14, color = "#ffffff"),
          axis.title = element_text(size = 16, color = "#ffffff"),
          axis.text = element_text(size = 14, color = "#ffffff"),
          legend.title = element_text(size = 14, color = "#FFD700"),
          legend.text = element_text(size = 12, color = "#ffffff"),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333"),
          legend.background = element_rect(fill = "#2d2d2d"),
          legend.key = element_rect(fill = "#2d2d2d")
        )
      
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
          height = 700
        )
    }
  })
  
  #--------------------- Fantasy Scoring Section ---------------------#
  
  # Fantasy Scoring filtered data
  fantasy_data <- reactive({
    req(values$analysis_filtered_data, values$fantasy_race_ids)
    
    data <- values$analysis_filtered_data %>%
      filter(race_id %in% values$fantasy_race_ids)
    
    return(data)
  })
  
  # Fantasy Scoring Data Table
  output$fantasy_data_table <- DT::renderDataTable({
    req(fantasy_data(), input$fs_platform)
    
    if (input$fs_platform == "DK") {
      display_data <- fantasy_data() %>%
        filter(DKRank <= 25) %>%
        select(
          Driver = Full_Name,
          Rank = DKRank,
          `Total Pts` = DKPoints,
          `Finish Pts` = DKFP,
          `PD Pts` = DKPD,
          `Dom Pts` = DKSP,
          Finish = ps,
          Start = start_ps,
          `Laps Led` = lead_laps,
          `Fast Laps` = fast_laps,
          Race = race_name,
          Track = track_name,
          Season = race_season
        ) %>%
        mutate(
          `Total Pts` = round(`Total Pts`, 1),
          `Finish Pts` = round(`Finish Pts`, 1),
          `PD Pts` = round(`PD Pts`, 1),
          `Dom Pts` = round(`Dom Pts`, 1),
          `Finish %` = round(`Finish Pts` / `Total Pts` * 100, 1),
          `PD %` = round(`PD Pts` / `Total Pts` * 100, 1),
          `Dom %` = round(`Dom Pts` / `Total Pts` * 100, 1)
        ) %>%
        arrange(desc(`Total Pts`)) %>%
        slice_head(n = 25)
    } else {
      display_data <- fantasy_data() %>%
        filter(FDRank <= 25) %>%
        select(
          Driver = Full_Name,
          Rank = FDRank,
          `Total Pts` = FDPoints,
          `Finish Pts` = FDFP,
          `PD Pts` = FDPD,
          `Dom Pts` = FDSP,
          `Lap Pts` = FDLP,
          Finish = ps,
          Start = start_ps,
          `Laps Led` = lead_laps,
          Race = race_name,
          Track = track_name,
          Season = race_season
        ) %>%
        mutate(
          `Total Pts` = round(`Total Pts`, 1),
          `Finish Pts` = round(`Finish Pts`, 1),
          `PD Pts` = round(`PD Pts`, 1),
          `Dom Pts` = round(`Dom Pts`, 1),
          `Lap Pts` = round(`Lap Pts`, 1),
          `Finish %` = round(`Finish Pts` / `Total Pts` * 100, 1),
          `PD %` = round(`PD Pts` / `Total Pts` * 100, 1),
          `Dom %` = round(`Dom Pts` / `Total Pts` * 100, 1)
        ) %>%
        arrange(desc(`Total Pts`)) %>%
        slice_head(n = 25)
    }
    
    DT::datatable(
      display_data,
      caption = paste("Top 25", ifelse(input$fs_platform == "DK", "DraftKings", "FanDuel"), "Fantasy Scores"),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'fti',
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      rownames = FALSE,
      class = "display nowrap compact"
    )
  })
  
  # Download Fantasy CSV
  output$download_fantasy_csv <- downloadHandler(
    filename = function() {
      platform_label <- ifelse(input$fs_platform == "DK", "DraftKings", "FanDuel")
      paste("fantasy_scoring_", platform_label, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(fantasy_data(), input$fs_platform)
      
      if (input$fs_platform == "DK") {
        export_data <- fantasy_data() %>%
          select(
            Driver = Full_Name,
            Rank = DKRank,
            `Total Pts` = DKPoints,
            `Finish Pts` = DKFP,
            `PD Pts` = DKPD,
            `Dom Pts` = DKSP,
            Finish = ps,
            Start = start_ps,
            `Laps Led` = lead_laps,
            `Fast Laps` = fast_laps,
            Race = race_name,
            Track = track_name,
            Season = race_season
          ) %>%
          mutate(
            `Total Pts` = round(`Total Pts`, 1),
            `Finish Pts` = round(`Finish Pts`, 1),
            `PD Pts` = round(`PD Pts`, 1),
            `Dom Pts` = round(`Dom Pts`, 1)
          )
      } else {
        export_data <- fantasy_data() %>%
          select(
            Driver = Full_Name,
            Rank = FDRank,
            `Total Pts` = FDPoints,
            `Finish Pts` = FDFP,
            `PD Pts` = FDPD,
            `Dom Pts` = FDSP,
            `Lap Pts` = FDLP,
            Finish = ps,
            Start = start_ps,
            `Laps Led` = lead_laps,
            Race = race_name,
            Track = track_name,
            Season = race_season
          ) %>%
          mutate(
            `Total Pts` = round(`Total Pts`, 1),
            `Finish Pts` = round(`Finish Pts`, 1),
            `PD Pts` = round(`PD Pts`, 1),
            `Dom Pts` = round(`Dom Pts`, 1),
            `Lap Pts` = round(`Lap Pts`, 1)
          )
      }
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  # Fantasy Scoring Visualizations
  output$fantasy_plot <- renderPlotly({
    req(fantasy_data(), input$fs_visual_type, input$fs_visual_platform)
    
    plot_data <- fantasy_data()
    platform <- input$fs_visual_platform
    
    # Set platform-specific variables
    if (platform == "DK") {
      points_col <- "DKPoints"
      rank_col <- "DKRank"
      fp_col <- "DKFP"
      pd_col <- "DKPD"
      sp_col <- "DKSP"
      platform_name <- "DraftKings"
    } else {
      points_col <- "FDPoints"
      rank_col <- "FDRank"
      fp_col <- "FDFP"
      pd_col <- "FDPD"
      sp_col <- "FDSP"
      platform_name <- "FanDuel"
    }
    
    if (input$fs_visual_type == "score_dist") {
      # Score Distribution by Rank
      viz_data <- plot_data %>%
        filter(!!sym(rank_col) <= 15)
      
      p <- ggplot(viz_data, aes(x = factor(!!sym(rank_col)), y = !!sym(points_col))) +
        geom_boxplot(aes(text = sprintf(
          "Rank: %d\nPoints: %.1f\nDriver: %s\nTrack: %s",
          !!sym(rank_col), !!sym(points_col), Full_Name, track_name
        )), fill = "dodgerblue", alpha = 0.7) +
        geom_smooth(aes(x = as.numeric(!!sym(rank_col)), y = !!sym(points_col), group = 1), 
                    method = "loess", se = FALSE, color = "red", size = 1.2) +
        labs(
          title = paste(platform_name, "Points Distribution by Fantasy Rank"),
          x = paste(platform_name, "Rank"),
          y = paste(platform_name, "Points")
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:15)) +
        theme(
          plot.title = element_text(size = 18, face = "bold"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333"),
          text = element_text(color = "#ffffff"),
          axis.text.x = element_text(color = "#ffffff"),
          axis.text.y = element_text(color = "#ffffff")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          height = 700
        )
      
    } else if (input$fs_visual_type == "components") {
      # Scoring Components Breakdown
      if (platform == "DK") {
        point_composition <- plot_data %>%
          filter(!!sym(rank_col) <= 15) %>%
          mutate(
            Finish_Pct = round(DKFP / DKPoints * 100, 1),
            PD_Pct = round(DKPD / DKPoints * 100, 1),
            Dominator_Pct = round(DKSP / DKPoints * 100, 1)
          )
      } else {
        point_composition <- plot_data %>%
          filter(!!sym(rank_col) <= 15) %>%
          mutate(
            Finish_Pct = round(FDFP / (FDPoints - FDLP) * 100, 1),
            PD_Pct = round(FDPD / (FDPoints - FDLP) * 100, 1),
            Dominator_Pct = round(FDSP / (FDPoints - FDLP) * 100, 1)
          )
      }
      
      comp_data <- point_composition %>%
        group_by(!!sym(rank_col)) %>%
        summarize(
          Finish_Pct = mean(Finish_Pct, na.rm = TRUE),
          PD_Pct = mean(PD_Pct, na.rm = TRUE),
          Dominator_Pct = mean(Dominator_Pct, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        pivot_longer(
          cols = c(Finish_Pct, PD_Pct, Dominator_Pct),
          names_to = "Point_Type",
          values_to = "Percentage"
        ) %>%
        mutate(
          Point_Type = case_when(
            Point_Type == "Finish_Pct" ~ "Finish Position",
            Point_Type == "PD_Pct" ~ "Place Differential",
            Point_Type == "Dominator_Pct" ~ "Dominator Points",
            TRUE ~ Point_Type
          )
        )
      
      p <- ggplot(comp_data, aes(x = factor(!!sym(rank_col)), y = Percentage, fill = Point_Type)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
                  position = position_stack(vjust = 0.5), 
                  color = "white", fontface = "bold", size = 4) +
        scale_fill_manual(values = c("Finish Position" = "#3406cc", 
                                     "Place Differential" = "#33cc33", 
                                     "Dominator Points" = "#ff9900")) +
        labs(
          title = paste(platform_name, "Scoring Components Breakdown"),
          subtitle = "What Type of Points are Driving Top Scores",
          x = paste(platform_name, "Rank"),
          y = "Points Composition (%)",
          fill = "Point Type"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333"),
          text = element_text(color = "#ffffff"),
          axis.text.x = element_text(color = "#ffffff"),
          axis.text.y = element_text(color = "#ffffff"),
          legend.background = element_rect(fill = "#2d2d2d"),
          legend.key = element_rect(fill = "#2d2d2d")
        )
      
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          height = 700
        )
      
    } else if (input$fs_visual_type == "score_by_start") {
      # Score Distribution by Start Position
      viz_data <- plot_data %>%
        filter(start_ps <= 40, !is.na(start_ps), !is.na(!!sym(points_col)))
      
      p <- ggplot(viz_data, aes(x = factor(start_ps), y = !!sym(points_col))) +
        geom_boxplot(aes(text = sprintf(
          "Start: %d\nPoints: %.1f\nDriver: %s\nTrack: %s",
          start_ps, !!sym(points_col), Full_Name, track_name
        )), fill = "purple", alpha = 0.6) +
        geom_smooth(aes(x = start_ps, y = !!sym(points_col)), 
                    method = "loess", se = FALSE, color = "yellow", size = 1.2) +
        labs(
          title = paste(platform_name, "Fantasy Points by Starting Position"),
          x = "Starting Position",
          y = paste(platform_name, "Points")
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:40)) +
        theme(
          plot.title = element_text(size = 18, face = "bold"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333"),
          text = element_text(color = "#ffffff"),
          axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1),
          axis.text.y = element_text(color = "#ffffff")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          height = 700
        )
      
    } else if (input$fs_visual_type == "score_by_finish") {
      # Score Distribution by Finish Position
      viz_data <- plot_data %>%
        filter(ps <= 40, !is.na(ps), !is.na(!!sym(points_col)))
      
      p <- ggplot(viz_data, aes(x = factor(ps), y = !!sym(points_col))) +
        geom_boxplot(aes(text = sprintf(
          "Finish: %d\nPoints: %.1f\nDriver: %s\nTrack: %s",
          ps, !!sym(points_col), Full_Name, track_name
        )), fill = "orange", alpha = 0.6) +
        geom_smooth(aes(x = ps, y = !!sym(points_col)), 
                    method = "loess", se = FALSE, color = "blue", size = 1.2) +
        labs(
          title = paste(platform_name, "Fantasy Points by Finish Position"),
          x = "Finish Position",
          y = paste(platform_name, "Points")
        ) +
        theme_minimal() +
        scale_x_discrete(limits = factor(1:40)) +
        theme(
          plot.title = element_text(size = 18, face = "bold"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333"),
          text = element_text(color = "#ffffff"),
          axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1),
          axis.text.y = element_text(color = "#ffffff")
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          height = 700
        )
      
    } else if (input$fs_visual_type == "components_start") {
      # Components by Start Position
      if (platform == "DK") {
        viz_data <- plot_data %>%
          filter(start_ps <= 20, !is.na(start_ps)) %>%
          mutate(
            Finish_Pct = round(DKFP / DKPoints * 100, 1),
            PD_Pct = round(DKPD / DKPoints * 100, 1),
            Dominator_Pct = round(DKSP / DKPoints * 100, 1)
          )
      } else {
        viz_data <- plot_data %>%
          filter(start_ps <= 20, !is.na(start_ps)) %>%
          mutate(
            Finish_Pct = round(FDFP / (FDPoints - FDLP) * 100, 1),
            PD_Pct = round(FDPD / (FDPoints - FDLP) * 100, 1),
            Dominator_Pct = round(FDSP / (FDPoints - FDLP) * 100, 1)
          )
      }
      
      comp_data <- viz_data %>%
        group_by(start_ps) %>%
        summarize(
          Finish_Pct = mean(Finish_Pct, na.rm = TRUE),
          PD_Pct = mean(PD_Pct, na.rm = TRUE),
          Dominator_Pct = mean(Dominator_Pct, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        pivot_longer(
          cols = c(Finish_Pct, PD_Pct, Dominator_Pct),
          names_to = "Point_Type",
          values_to = "Percentage"
        ) %>%
        mutate(
          Point_Type = case_when(
            Point_Type == "Finish_Pct" ~ "Finish Position",
            Point_Type == "PD_Pct" ~ "Place Differential",
            Point_Type == "Dominator_Pct" ~ "Dominator Points",
            TRUE ~ Point_Type
          )
        )
      
      p <- ggplot(comp_data, aes(x = factor(start_ps), y = Percentage, fill = Point_Type)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = sprintf("%.0f%%", Percentage)), 
                  position = position_stack(vjust = 0.5), 
                  color = "white", fontface = "bold", size = 3) +
        scale_fill_manual(values = c("Finish Position" = "#3406cc", 
                                     "Place Differential" = "#33cc33", 
                                     "Dominator Points" = "#ff9900")) +
        labs(
          title = paste(platform_name, "Scoring Components by Starting Position"),
          subtitle = "Top 20 Starting Positions",
          x = "Starting Position",
          y = "Points Composition (%)",
          fill = "Point Type"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333"),
          text = element_text(color = "#ffffff"),
          axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1),
          axis.text.y = element_text(color = "#ffffff"),
          legend.background = element_rect(fill = "#2d2d2d"),
          legend.key = element_rect(fill = "#2d2d2d")
        )
      
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          height = 700
        )
      
    } else if (input$fs_visual_type == "components_finish") {
      # Components by Finish Position
      if (platform == "DK") {
        viz_data <- plot_data %>%
          filter(ps <= 20, !is.na(ps)) %>%
          mutate(
            Finish_Pct = round(DKFP / DKPoints * 100, 1),
            PD_Pct = round(DKPD / DKPoints * 100, 1),
            Dominator_Pct = round(DKSP / DKPoints * 100, 1)
          )
      } else {
        viz_data <- plot_data %>%
          filter(ps <= 20, !is.na(ps)) %>%
          mutate(
            Finish_Pct = round(FDFP / (FDPoints - FDLP) * 100, 1),
            PD_Pct = round(FDPD / (FDPoints - FDLP) * 100, 1),
            Dominator_Pct = round(FDSP / (FDPoints - FDLP) * 100, 1)
          )
      }
      
      comp_data <- viz_data %>%
        group_by(ps) %>%
        summarize(
          Finish_Pct = mean(Finish_Pct, na.rm = TRUE),
          PD_Pct = mean(PD_Pct, na.rm = TRUE),
          Dominator_Pct = mean(Dominator_Pct, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        pivot_longer(
          cols = c(Finish_Pct, PD_Pct, Dominator_Pct),
          names_to = "Point_Type",
          values_to = "Percentage"
        ) %>%
        mutate(
          Point_Type = case_when(
            Point_Type == "Finish_Pct" ~ "Finish Position",
            Point_Type == "PD_Pct" ~ "Place Differential",
            Point_Type == "Dominator_Pct" ~ "Dominator Points",
            TRUE ~ Point_Type
          )
        )
      
      p <- ggplot(comp_data, aes(x = factor(ps), y = Percentage, fill = Point_Type)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = sprintf("%.0f%%", Percentage)), 
                  position = position_stack(vjust = 0.5), 
                  color = "white", fontface = "bold", size = 3) +
        scale_fill_manual(values = c("Finish Position" = "#3406cc", 
                                     "Place Differential" = "#33cc33", 
                                     "Dominator Points" = "#ff9900")) +
        labs(
          title = paste(platform_name, "Scoring Components by Finish Position"),
          subtitle = "Top 20 Finishing Positions",
          x = "Finish Position",
          y = "Points Composition (%)",
          fill = "Point Type"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 14),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.background = element_rect(fill = "#2d2d2d"),
          plot.background = element_rect(fill = "#2d2d2d"),
          panel.grid.major = element_line(color = "#404040"),
          panel.grid.minor = element_line(color = "#333333"),
          text = element_text(color = "#ffffff"),
          axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1),
          axis.text.y = element_text(color = "#ffffff"),
          legend.background = element_rect(fill = "#2d2d2d"),
          legend.key = element_rect(fill = "#2d2d2d")
        )
      
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(
          paper_bgcolor = "#2d2d2d",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "#ffffff"),
          height = 700
        )
    }
  })
  
} 

#--------------------- Run the Application ---------------------#

shinyApp(ui = ui, server = server)
