# NASCAR Race Analysis App
# Enhanced version with flexible race filtering

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
    # Default to non-historical races (Historical == "N")
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

# Load entry list from NASCAR API
load_entry_list <- function(race_season, series_id, race_id) {
  tryCatch({
    url <- paste0(
      'https://cf.nascar.com/cacher/',
      race_season,
      '/',
      series_id,
      '/',
      race_id,
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

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = div(
      style = "display: flex; align-items: center; padding: 0;",
      img(src = "logo.jpg", height = "35px", style = "margin-right: 10px;"),
      span(
        style = "color: #FFD700; font-weight: bold; font-size: 18px; white-space: nowrap;",
        "Golden Ticket Research"
      )
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Race Selection", tabName = "race_analysis", icon = icon("filter")),
      menuItem("Race Context", tabName = "race_context", icon = icon("info-circle")),
      menuItem("DK Dominator", tabName = "dk_dominator", icon = icon("tachometer-alt")),
      menuItem("FD Dominator", tabName = "fd_dominator", icon = icon("tachometer-alt")),
      menuItem("Place Differential", tabName = "place_differential", icon = icon("exchange-alt")),
      menuItem("Data Explorer", tabName = "data_explorer", icon = icon("table")),
      menuItem("Profile Builder", tabName = "profile_builder", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    
    # Custom CSS - Black and Gold Theme
    tags$head(
      tags$script(src = "custom-handlers.js"),
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
        
        /* Labels */
        label {
          color: #ffffff !important;
        }
      "))
    ),
    
    tabItems(
      # Race Selection Tab (formerly Race Analysis)
      tabItem(tabName = "race_analysis",
              # Master Filter Configuration
              fluidRow(
                box(
                  title = "Race Selection Configuration", status = "primary", solidHeader = TRUE, 
                  width = 12, collapsible = TRUE,
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
                                            class = "btn-primary", style = "width: 100%; padding: 10px;"))
                    )
                  )
                )
              ),
              
              # Races Included section
              conditionalPanel(
                condition = "output.filters_confirmed",
                fluidRow(
                  box(
                    title = "Races Included", status = "success", solidHeader = TRUE, 
                    width = 12, collapsible = TRUE,
                    fluidRow(
                      column(12,
                             p(style = "color: #ffffff; margin-bottom: 15px;",
                               "Select which races to include in analysis. Uncheck races to exclude them from ",
                               strong("ALL"), " analysis tabs.")
                      )
                    ),
                    fluidRow(
                      column(12,
                             withSpinner(DT::dataTableOutput("races_selection_table"))
                      )
                    ),
                    fluidRow(
                      column(9,
                             uiOutput("races_selection_summary")
                      ),
                      column(3,
                             actionButton("apply_race_selection", "Apply Selection to All Tabs", 
                                          class = "btn-success", style = "width: 100%; margin-top: 10px;")
                      )
                    )
                  )
                )
              )
      ),
      
      # Data Explorer Tab (your existing code)
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
                  title = "Golden Ticket Data Explorer", status = "primary", solidHeader = TRUE, width = 12,
                  withSpinner(DT::dataTableOutput("main_explorer_table"))
                )
              )
      ),
      
      # Profile Builder Tab (your existing code)
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
      ),
      
      # Race Context Tab (NEW)
      tabItem(tabName = "race_context",
              conditionalPanel(
                condition = "output.filters_confirmed",
                fluidRow(
                  box(
                    title = "Entry List", status = "primary", solidHeader = TRUE, 
                    width = 12, collapsible = TRUE,
                    withSpinner(DT::dataTableOutput("entry_list_table"))
                  )
                ),
                fluidRow(
                  box(
                    title = "Race Summary Statistics", status = "info", solidHeader = TRUE, 
                    width = 12, collapsible = TRUE,
                    withSpinner(DT::dataTableOutput("races_summary_stats_table"))
                  )
                )
              ),
              conditionalPanel(
                condition = "!output.filters_confirmed",
                fluidRow(
                  box(
                    title = "No Races Loaded", status = "warning", solidHeader = TRUE, width = 12,
                    p(style = "color: #ffffff; text-align: center; padding: 20px;",
                      "Please go to the Race Selection tab and load races first.")
                  )
                )
              )
      ),
      
      # DK Dominator Analysis Tab
      tabItem(tabName = "dk_dominator",
              conditionalPanel(
                condition = "output.filters_confirmed",
                fluidRow(
                  box(
                    title = "DK Dominator Analysis", status = "primary", solidHeader = TRUE, 
                    width = 12,
                    fluidRow(
                      column(12,
                             p(style = "color: #ffffff;",
                               "Analysis uses races selected in the Race Selection tab. ",
                               "Go back to Race Selection to change which races are included.")
                      )
                    ),
                    fluidRow(
                      column(9,
                             uiOutput("dk_analysis_info")
                      ),
                      column(3,
                             actionButton("generate_dk_analysis", "Generate DK Dominator Analysis", 
                                          class = "btn-primary", style = "width: 100%; margin-top: 10px;")
                      )
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "output.dk_analysis_ready",
                  fluidRow(
                    box(
                      title = "DK Dominator Analysis Results", status = "success", 
                      solidHeader = TRUE, width = 12,
                      tabBox(
                        width = 12,
                        tabPanel("Top Dominators",
                                 withSpinner(DT::dataTableOutput("dk_top_doms_table"))
                        ),
                        tabPanel("Score Distribution",
                                 withSpinner(plotlyOutput("dk_score_dist_plot", height = "600px"))
                        ),
                        tabPanel("Dom Rank Finishes",
                                 withSpinner(plotlyOutput("dk_rank_finish_plot", height = "600px"))
                        ),
                        tabPanel("Dom Pts by Finish",
                                 withSpinner(plotlyOutput("dk_pts_by_finish_plot", height = "600px"))
                        ),
                        tabPanel("Laps Led",
                                 withSpinner(plotlyOutput("dk_laps_led_plot", height = "600px"))
                        ),
                        tabPanel("Fast Laps",
                                 withSpinner(plotlyOutput("dk_fast_laps_plot", height = "600px"))
                        )
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "!output.filters_confirmed",
                fluidRow(
                  box(
                    title = "No Races Loaded", status = "warning", solidHeader = TRUE, width = 12,
                    p(style = "color: #ffffff; text-align: center; padding: 20px;",
                      "Please go to the Race Selection tab and load races first.")
                  )
                )
              )
      ),
      
      # FD Dominator Analysis Tab
      tabItem(tabName = "fd_dominator",
              conditionalPanel(
                condition = "output.filters_confirmed",
                fluidRow(
                  box(
                    title = "FD Dominator Analysis", status = "primary", solidHeader = TRUE, 
                    width = 12,
                    fluidRow(
                      column(12,
                             p(style = "color: #ffffff;",
                               "Analysis uses races selected in the Race Selection tab. ",
                               "Go back to Race Selection to change which races are included.")
                      )
                    ),
                    fluidRow(
                      column(9,
                             uiOutput("fd_analysis_info")
                      ),
                      column(3,
                             actionButton("generate_fd_analysis", "Generate FD Dominator Analysis", 
                                          class = "btn-primary", style = "width: 100%; margin-top: 10px;")
                      )
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "output.fd_analysis_ready",
                  fluidRow(
                    box(
                      title = "FD Dominator Analysis Results", status = "success", 
                      solidHeader = TRUE, width = 12,
                      tabBox(
                        width = 12,
                        tabPanel("Top Dominators",
                                 withSpinner(DT::dataTableOutput("fd_top_doms_table"))
                        ),
                        tabPanel("Score Distribution",
                                 withSpinner(plotlyOutput("fd_score_dist_plot", height = "600px"))
                        ),
                        tabPanel("Dom Rank Finishes",
                                 withSpinner(plotlyOutput("fd_rank_finish_plot", height = "600px"))
                        ),
                        tabPanel("Dom Pts by Finish",
                                 withSpinner(plotlyOutput("fd_pts_by_finish_plot", height = "600px"))
                        ),
                        tabPanel("Laps Led",
                                 withSpinner(plotlyOutput("fd_laps_led_plot", height = "600px"))
                        )
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "!output.filters_confirmed",
                fluidRow(
                  box(
                    title = "No Races Loaded", status = "warning", solidHeader = TRUE, width = 12,
                    p(style = "color: #ffffff; text-align: center; padding: 20px;",
                      "Please go to the Race Selection tab and load races first.")
                  )
                )
              )
      ),
      
      # Place Differential Tab
      tabItem(tabName = "place_differential",
              conditionalPanel(
                condition = "output.filters_confirmed",
                fluidRow(
                  column(12,
                         actionButton("generate_pd_analysis", "Generate Place Differential Analysis", 
                                      class = "btn-primary", style = "width: 100%; margin-bottom: 15px;")
                  )
                ),
                
                conditionalPanel(
                  condition = "output.pd_analysis_ready",
                  fluidRow(
                    box(
                      title = "Place Differential Analysis", status = "success", 
                      solidHeader = TRUE, width = 12,
                      tabBox(
                        width = 12,
                        tabPanel("Starting vs Finishing",
                                 withSpinner(plotlyOutput("pd_scatter_plot", height = "700px"))
                        ),
                        tabPanel("Change Distribution",
                                 withSpinner(plotlyOutput("pd_histogram_plot", height = "700px"))
                        ),
                        tabPanel("PD by Start Position",
                                 withSpinner(plotlyOutput("pd_by_start_plot", height = "700px"))
                        ),
                        tabPanel("Biggest Movers",
                                 withSpinner(DT::dataTableOutput("pd_movers_table"))
                        )
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "!output.filters_confirmed",
                fluidRow(
                  box(
                    title = "No Races Loaded", status = "warning", solidHeader = TRUE, width = 12,
                    p(style = "color: #ffffff; text-align: center; padding: 20px;",
                      "Please go to the Race Selection tab and load races first.")
                  )
                )
              )
      )
    )
  )
)

#--------------------- Server Function ---------------------#

server <- function(input, output, session) {
  
  # Reactive values for all tabs
  values <- reactiveValues(
    nascar_data = NULL,
    race_list = NULL,
    dominator_profiles = NULL,
    race_weights = NULL,
    race_summary = NULL,
    
    # Race Selection specific values
    analysis_filtered_data = NULL,
    analysis_entry_list = NULL,
    analysis_races_available = NULL,
    filters_confirmed = FALSE,
    selected_race_ids = NULL,  # NEW - tracks selected races across all tabs
    
    # Analysis ready flags
    dk_analysis_data = NULL,
    dk_analysis_ready = FALSE,
    fd_analysis_data = NULL,
    fd_analysis_ready = FALSE,
    pd_analysis_data = NULL,
    pd_analysis_ready = FALSE
  )
  
  # Load initial data
  observe({
    withProgress(message = 'Loading Golden Ticket Database...', {
      values$nascar_data <- load_nascar_database()
      values$race_list <- load_race_list()
      
      if (!is.null(values$nascar_data)) {
        incProgress(0.3, detail = "Processing seasons...")
        seasons <- sort(unique(values$nascar_data$race_season), decreasing = TRUE)
        
        incProgress(0.5, detail = "Processing drivers...")
        drivers <- sort(unique(values$nascar_data$Full_Name[!is.na(values$nascar_data$Full_Name)]))
        
        incProgress(0.7, detail = "Processing tracks...")
        # Get ALL tracks from the full race list (both historical and non-historical)
        all_race_data <- if(file.exists("RaceIDs.xlsx")) read_excel("RaceIDs.xlsx") else NULL
        if (!is.null(all_race_data)) {
          all_tracks <- sort(unique(all_race_data$track_name[!is.na(all_race_data$track_name)]))
        } else {
          all_tracks <- sort(unique(values$nascar_data$track_name[!is.na(values$nascar_data$track_name)]))
        }
        
        incProgress(0.8, detail = "Processing teams...")
        teams <- sort(unique(values$nascar_data$team_name[!is.na(values$nascar_data$team_name)]))
        
        # Initialize filter choices for Data Explorer
        updateSelectizeInput(session, "season_filter", choices = seasons)
        updateSelectizeInput(session, "driver_filter", choices = drivers)
        updateSelectizeInput(session, "track_filter", choices = all_tracks)
        updateSelectizeInput(session, "team_filter", choices = teams)
        
        # Initialize profile builder track choices
        updateSelectizeInput(session, "target_track", choices = all_tracks)
        
        # Initialize race analysis track choices (ALL TRACKS)
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
    
    # Load full race list (including non-historical)
    all_races <- if(file.exists("RaceIDs.xlsx")) read_excel("RaceIDs.xlsx") else NULL
    
    if (!is.null(all_races)) {
      available_races <- all_races %>%
        filter(
          series_id == as.numeric(input$analysis_series),
          track_name == input$analysis_primary_track,
          race_season >= input$analysis_start_year,
          race_season <= input$analysis_end_year,
          Historical == "N"  # Only show non-historical for entry list
        ) %>%
        arrange(desc(race_season)) %>%
        mutate(race_label = paste0(race_season, " - ", race_name, " *"))  # Add asterisk
      
      race_choices <- setNames(available_races$race_id, available_races$race_label)
      
      updateSelectizeInput(session, "analysis_race_id", choices = race_choices,
                           selected = if(length(race_choices) > 0) race_choices[1] else NULL)
    }
  })
  
  #--------------------- Data Explorer Tab Logic (UNCHANGED) ---------------------#
  
  filter_base_data <- reactive({
    req(values$nascar_data)
    data <- values$nascar_data
    
    if (!is.null(input$season_filter) && length(input$season_filter) > 0) {
      data <- data %>% filter(race_season %in% input$season_filter)
    }
    
    if (!is.null(input$series_filter) && length(input$series_filter) > 0) {
      data <- data %>% filter(series_id %in% as.numeric(input$series_filter))
    }
    
    return(data)
  })
  
  observe({
    base_data <- filter_base_data()
    if (!is.null(base_data)) {
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
  
  observe({
    base_data <- filter_base_data()
    if (!is.null(base_data)) {
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
  
  observe({
    base_data <- filter_base_data()
    if (!is.null(base_data)) {
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
  
  output$main_explorer_table <- DT::renderDataTable({
    req(filtered_data())
    
    withProgress(message = 'Rendering table...', {
      default_columns <- c("Full_Name", "start_ps", "ps", "ARP", "SpdRk", "fl", "ll", 
                           "DKSP", "FDSP", "DKDomRank", "FDDomRank", "car_number", 
                           "team_name", "race_season", "track_name", "finishing_status", "LapsDown")
      
      valid_columns <- intersect(default_columns, names(filtered_data()))
      
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
          .x == "race_season" ~ "Season",
          .x == "track_name" ~ "Track",
          .x == "finishing_status" ~ "Finishing Status",
          .x == "LapsDown" ~ "Laps Down",
          TRUE ~ .x
        ))
      
      incProgress(0.5, detail = "Applying formatting...")
      
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
      
      position_columns <- intersect(c("Start", "Finish", "ARP", "SpdRk"), 
                                    names(display_data))
      
      incProgress(0.8, detail = "Adding gradient formatting...")
      
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
      
      incProgress(1.0, detail = "Complete!")
      return(dt)
    })
  })
  
  output$download_filtered_csv <- downloadHandler(
    filename = function() {
      paste("golden_ticket_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(filtered_data())
      
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
  
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "season_filter", selected = character(0))
    updateSelectizeInput(session, "driver_filter", selected = character(0))
    updateSelectizeInput(session, "track_filter", selected = character(0))
    updateSelectizeInput(session, "team_filter", selected = character(0))
    updateSelectizeInput(session, "series_filter", selected = character(0))
    
    showNotification("All filters have been reset", type = "message", duration = 2)
  })
  
  #--------------------- Profile Builder Tab Logic (UNCHANGED) ---------------------#
  
  create_race_profile <- function(race_data, race_info) {
    profile <- race_data %>%
      mutate(
        DKDomPoints = (fast_laps * 0.45) + (lead_laps * 0.25),
        FDDomPoints = lead_laps * 0.1
      ) %>%
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
  
  observeEvent(input$generate_dominator_profiles, {
    req(input$target_series, input$target_track, input$season_start, input$season_end, values$nascar_data)
    
    if (length(input$target_track) == 0) {
      showNotification("Please select at least one track", type = "warning")
      return()
    }
    
    withProgress(message = 'Generating dominator profiles...', {
      incProgress(0.1, detail = "Filtering race data...")
      
      filtered_races <- values$nascar_data %>%
        filter(
          series_id == as.numeric(input$target_series),
          track_name %in% input$target_track,
          race_season >= input$season_start,
          race_season <= input$season_end
        )
      
      if ("race_type_id" %in% names(filtered_races)) {
        filtered_races <- filtered_races %>% filter(race_type_id == 1)
      }
      
      if (nrow(filtered_races) == 0) {
        showNotification("No races found matching your criteria", type = "warning")
        return()
      }
      
      incProgress(0.3, detail = "Processing individual races...")
      
      unique_races <- filtered_races %>%
        select(race_id, race_season, race_name, track_name) %>%
        distinct() %>%
        arrange(race_season, race_id)
      
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
            
            weight_value <- case_when(
              race_info$race_season == 2024 ~ 0.4,
              race_info$race_season == 2023 ~ 0.35,
              race_info$race_season == 2022 ~ 0.25,
              race_info$race_season == 2021 ~ 0.20,
              TRUE ~ 0.15
            )
            
            race_weights <- rbind(race_weights, data.frame(
              RaceID = race_info$race_id,
              RaceName = race_info$race_name,
              Season = race_info$race_season,
              Track = race_info$track_name,
              Weight = weight_value,
              stringsAsFactors = FALSE
            ))
          }
        }
        
        incProgress(0.4 / nrow(unique_races), detail = paste("Processed race", i, "of", nrow(unique_races)))
      }
      
      incProgress(0.9, detail = "Combining profiles...")
      
      if (length(all_profiles) == 0) {
        showNotification("No dominator profiles could be generated", type = "warning")
        return()
      }
      
      combined_profiles <- bind_rows(all_profiles)
      
      race_summary <- combined_profiles %>%
        group_by(RaceID, RaceName, Season) %>%
        summarise(
          Track = first(TrackName),
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
      "Track Profile Breakdown:",
      track_text,
      sep = "\n"
    )
  })
  
  output$download_profiles_xlsx <- downloadHandler(
    filename = function() {
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
      paste0("Golden_Ticket_", track_name, "_", series_name, "_Profiles_", 
             input$season_start, "-", input$season_end, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(values$dominator_profiles, values$race_weights, values$race_summary)
      
      excel_data <- list(
        "Dominator_Profiles" = values$dominator_profiles,
        "Race_Weights" = values$race_weights,
        "Race_Summary" = values$race_summary
      )
      
      write_xlsx(excel_data, file)
    }
  )
  
  #--------------------- Race Selection Tab Logic ---------------------#
  
  # Confirm filters and load data
  observeEvent(input$confirm_analysis_filters, {
    req(input$analysis_series, input$analysis_primary_track, 
        input$analysis_start_year, input$analysis_end_year, input$analysis_race_id)
    
    withProgress(message = 'Loading races...', {
      incProgress(0.2, detail = "Filtering races...")
      
      # Load full race list
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
      
      # Prepare races available data
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
      values$selected_race_ids <- races_available$race_id  # Initialize with all races selected
      values$filters_confirmed <- TRUE
      
      # Reset all analysis states
      values$dk_analysis_ready <- FALSE
      values$fd_analysis_ready <- FALSE
      values$pd_analysis_ready <- FALSE
      
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
  
  # Races Selection Table
  output$races_selection_table <- DT::renderDataTable({
    req(values$analysis_races_available)
    
    race_selection_data <- values$analysis_races_available %>%
      select(
        race_id,
        Season = race_season,
        Track = track_name,
        Race = race_name,
        "Total Laps" = total_laps,
        "GF Laps" = green_flag_laps,
        "DK Dom Avail" = DK_Dom_Available,
        "FD Dom Avail" = FD_Dom_Available,
        Leaders = number_of_leaders,
        Cautions = number_of_cautions
      ) %>%
      arrange(desc(Season))
    
    DT::datatable(
      race_selection_data,
      selection = list(mode = 'none'),
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'frtip',
        columnDefs = list(
          list(
            targets = 0,
            checkboxes = list(selectRow = FALSE),
            render = JS(
              "function(data, type, row, meta) {",
              "  return '<input type=\"checkbox\" class=\"race-selection-checkbox\" value=\"' + data + '\" checked>';",
              "}"
            )
          ),
          list(className = "dt-center", targets = "_all")
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#333333', 'color': '#FFD700'});",
          "}"
        )
      ),
      rownames = FALSE,
      escape = FALSE,
      class = "display nowrap compact"
    )
  })
  
  # JavaScript handler for race selection checkboxes
  observe({
    req(values$analysis_races_available)
    
    session$sendCustomMessage(
      type = 'race_selection_handler',
      message = list(
        js_code = "
          $(document).on('change', '.race-selection-checkbox', function() {
            var checked = $('.race-selection-checkbox:checked').length;
            $('#races_selected_count').text(checked);
            
            var selected = [];
            $('.race-selection-checkbox:checked').each(function() {
              selected.push(parseInt($(this).val()));
            });
            
            Shiny.setInputValue('selected_race_ids_temp', selected, {priority: 'event'});
          });
          
          var initial = [];
          $('.race-selection-checkbox:checked').each(function() {
            initial.push(parseInt($(this).val()));
          });
          Shiny.setInputValue('selected_race_ids_temp', initial, {priority: 'event'});
        "
      )
    )
  })
  
  # Selection summary
  output$races_selection_summary <- renderUI({
    req(values$analysis_races_available)
    
    total_races <- nrow(values$analysis_races_available)
    
    tagList(
      h4(style = "color: #FFD700;", 
         span(id = "races_selected_count", total_races), 
         " of ", total_races, " races selected for analysis")
    )
  })
  
  # Apply race selection button
  observeEvent(input$apply_race_selection, {
    req(input$selected_race_ids_temp)
    
    values$selected_race_ids <- input$selected_race_ids_temp
    
    # Reset analysis states when selection changes
    values$dk_analysis_ready <- FALSE
    values$fd_analysis_ready <- FALSE
    values$pd_analysis_ready <- FALSE
    
    showNotification(
      paste("Race selection applied:", length(values$selected_race_ids), "races selected"),
      type = "message",
      duration = 3
    )
  })
  
  #--------------------- Race Context Tab Logic ---------------------#
  
  # Entry List Table
  output$entry_list_table <- DT::renderDataTable({
    req(values$analysis_entry_list)
    
    DT::datatable(
      values$analysis_entry_list,
      caption = "Current Entry List",
      options = list(
        pageLength = 40,
        scrollX = TRUE,
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
      filter = "top",
      class = "display nowrap compact"
    )
  })
  
  # Race Summary Stats Table
  output$races_summary_stats_table <- DT::renderDataTable({
    req(values$analysis_races_available, values$selected_race_ids)
    
    # Filter to only selected races
    selected_races <- values$analysis_races_available %>%
      filter(race_id %in% values$selected_race_ids)
    
    race_summary <- selected_races %>%
      left_join(
        values$analysis_filtered_data %>%
          group_by(race_id) %>%
          summarize(
            lead_lap = sum(LapsDown == 0, na.rm = TRUE),
            crash_dnfs = sum(finishing_status %in% c("Accident", "DVP", "Damage"), na.rm = TRUE),
            mech_dnfs = sum(!finishing_status %in% c("Running", "Accident", "DVP", "Damage"), na.rm = TRUE),
            .groups = 'drop'
          ),
        by = "race_id"
      ) %>%
      select(
        Season = race_season,
        Track = track_name,
        Race = race_name,
        Cars = number_of_cars_in_field,
        Leaders = number_of_leaders,
        Cautions = number_of_cautions,
        "Total Laps" = total_laps,
        "GF Laps" = green_flag_laps,
        "DK Dom Avail" = DK_Dom_Available,
        "FD Dom Avail" = FD_Dom_Available,
        "Lead Lap" = lead_lap,
        "Crash DNFs" = crash_dnfs,
        "Mech DNFs" = mech_dnfs
      ) %>%
      arrange(desc(Season), Track)
    
    DT::datatable(
      race_summary,
      caption = "Summary Statistics for Selected Races",
      options = list(
        pageLength = 20,
        scrollX = TRUE,
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
    )
  })
  
  #--------------------- DK Dominator Analysis ---------------------#
  
  # DK Analysis Info Display
  output$dk_analysis_info <- renderUI({
    req(values$selected_race_ids)
    
    tagList(
      h4(style = "color: #FFD700;", 
         "Ready to analyze ", length(values$selected_race_ids), " selected races")
    )
  })
  
  # Generate DK Dominator Analysis
  observeEvent(input$generate_dk_analysis, {
    req(values$selected_race_ids, values$analysis_filtered_data)
    
    if (length(values$selected_race_ids) == 0) {
      showNotification("No races selected. Go to Race Selection tab.", type = "warning")
      return()
    }
    
    withProgress(message = 'Generating DK Dominator Analysis...', {
      incProgress(0.2, detail = "Filtering selected races...")
      
      # Filter data to selected races only
      dk_filtered_data <- values$analysis_filtered_data %>%
        filter(race_id %in% values$selected_race_ids)
      
      incProgress(0.5, detail = "Calculating dominator metrics...")
      
      # Calculate DK dominator points and ranks
      dk_analysis <- dk_filtered_data %>%
        group_by(race_id) %>%
        mutate(
          DKSP_calc = (fast_laps * 0.45) + (lead_laps * 0.25),
          DKDomRank_calc = dense_rank(desc(DKSP_calc))
        ) %>%
        ungroup() %>%
        mutate(
          DKSP = if("DKSP" %in% names(.) && !all(is.na(DKSP))) DKSP else DKSP_calc,
          DKDomRank = if("DKDomRank" %in% names(.) && !all(is.na(DKDomRank))) DKDomRank else DKDomRank_calc
        )
      
      # Store analysis data
      values$dk_analysis_data <- dk_analysis
      values$dk_analysis_ready <- TRUE
      
      incProgress(1.0, detail = "Complete!")
      
      showNotification(
        paste("DK Analysis generated for", length(values$selected_race_ids), "races"),
        type = "message",
        duration = 3
      )
    })
  })
  
  # Output to show/hide DK analysis results
  output$dk_analysis_ready <- reactive({
    return(values$dk_analysis_ready)
  })
  outputOptions(output, "dk_analysis_ready", suspendWhenHidden = FALSE)
  
  # DK Top Dominators Table
  output$dk_top_doms_table <- DT::renderDataTable({
    req(values$dk_analysis_data)
    
    top_doms <- values$dk_analysis_data %>%
      filter(DKSP > 0) %>%
      select(
        Driver = Full_Name,
        Start = start_ps,
        Finish = ps,
        Qualifying,
        Team = team_name,
        "Laps Led" = lead_laps,
        "Fast Laps" = fast_laps,
        "Dom Pts" = DKSP,
        "Dom Rank" = DKDomRank,
        Season = race_season,
        Race = race_name,
        Track = track_name
      ) %>%
      mutate(`Dom Pts` = round(`Dom Pts`, 1)) %>%
      arrange(desc(`Dom Pts`)) %>%
      slice_head(n = 25)
    
    DT::datatable(
      top_doms,
      caption = "Top 25 DK Dominators",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'ftip',
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
      filter = "top",
      class = "display nowrap compact"
    )
  })
  
  # DK Score Distribution Plot
  output$dk_score_dist_plot <- renderPlotly({
    req(values$dk_analysis_data)
    
    plot_data <- values$dk_analysis_data %>%
      filter(DKDomRank <= 10, DKSP > 0)
    
    p <- ggplot(plot_data, aes(x = factor(DKDomRank), y = DKSP)) +
      geom_boxplot(fill = "forestgreen", alpha = 0.7) +
      geom_smooth(aes(x = DKDomRank, y = DKSP, group = 1), 
                  method = "loess", se = FALSE, color = "red", size = 1) +
      labs(
        title = "DK Dominator Points Distribution by Dom Rank (Top 10)",
        x = "Dominator Rank",
        y = "Dominator Points"
      ) +
      theme_minimal() +
      scale_x_discrete(limits = factor(1:10)) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = "#2d2d2d"),
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.grid.major = element_line(color = "#404040"),
        panel.grid.minor = element_line(color = "#333333"),
        text = element_text(color = "#ffffff"),
        axis.text.x = element_text(color = "#ffffff"),
        axis.text.y = element_text(color = "#ffffff")
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        paper_bgcolor = "#2d2d2d",
        plot_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff")
      )
  })
  
  # DK Rank Finish Plot
  output$dk_rank_finish_plot <- renderPlotly({
    req(values$dk_analysis_data)
    
    plot_data <- values$dk_analysis_data %>%
      filter(DKDomRank <= 10, DKSP > 0)
    
    p <- ggplot(plot_data, aes(x = factor(DKDomRank), y = ps)) +
      geom_boxplot(fill = "darkgreen", alpha = 0.7) +
      geom_smooth(aes(x = DKDomRank, y = ps, group = 1), 
                  method = "loess", se = FALSE, color = "red", size = 1) +
      labs(
        title = "Finish Position by DK Dom Rank (Top 10)",
        x = "Dominator Rank",
        y = "Finish Position"
      ) +
      theme_minimal() +
      scale_x_discrete(limits = factor(1:10)) +
      scale_y_continuous(breaks = seq(0, 40, 5)) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = "#2d2d2d"),
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.grid.major = element_line(color = "#404040"),
        panel.grid.minor = element_line(color = "#333333"),
        text = element_text(color = "#ffffff"),
        axis.text.x = element_text(color = "#ffffff"),
        axis.text.y = element_text(color = "#ffffff")
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        paper_bgcolor = "#2d2d2d",
        plot_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff")
      )
  })
  
  # DK Points by Finish Plot
  output$dk_pts_by_finish_plot <- renderPlotly({
    req(values$dk_analysis_data)
    
    plot_data <- values$dk_analysis_data %>%
      filter(ps <= 40, !is.na(ps), !is.na(DKSP))
    
    p <- ggplot(plot_data, aes(x = factor(ps), y = DKSP)) +
      geom_boxplot(fill = "darkgreen", alpha = 0.7) +
      geom_smooth(aes(x = ps, y = DKSP, group = 1), 
                  method = "loess", se = FALSE, color = "red", size = 1) +
      labs(
        title = "DK Dominator Points by Finish Position",
        x = "Finish Position",
        y = "Dominator Points"
      ) +
      theme_minimal() +
      scale_x_discrete(limits = factor(1:40)) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = "#2d2d2d"),
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.grid.major = element_line(color = "#404040"),
        panel.grid.minor = element_line(color = "#333333"),
        text = element_text(color = "#ffffff"),
        axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1),
        axis.text.y = element_text(color = "#ffffff")
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        paper_bgcolor = "#2d2d2d",
        plot_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff")
      )
  })
  
  # DK Laps Led Plot
  output$dk_laps_led_plot <- renderPlotly({
    req(values$dk_analysis_data)
    
    plot_data <- values$dk_analysis_data %>%
      filter(ps <= 40, !is.na(ps), !is.na(lead_laps))
    
    p <- ggplot(plot_data, aes(x = factor(ps), y = lead_laps)) +
      geom_boxplot(fill = "lightgreen", alpha = 0.7) +
      geom_smooth(aes(x = ps, y = lead_laps, group = 1), 
                  method = "loess", se = FALSE, color = "red", size = 1) +
      labs(
        title = "Laps Led by Finish Position",
        x = "Finish Position",
        y = "Laps Led"
      ) +
      theme_minimal() +
      scale_x_discrete(limits = factor(1:40)) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = "#2d2d2d"),
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.grid.major = element_line(color = "#404040"),
        panel.grid.minor = element_line(color = "#333333"),
        text = element_text(color = "#ffffff"),
        axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1),
        axis.text.y = element_text(color = "#ffffff")
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        paper_bgcolor = "#2d2d2d",
        plot_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff")
      )
  })
  
  # DK Fast Laps Plot
  output$dk_fast_laps_plot <- renderPlotly({
    req(values$dk_analysis_data)
    
    plot_data <- values$dk_analysis_data %>%
      filter(ps <= 40, !is.na(ps), !is.na(fast_laps))
    
    p <- ggplot(plot_data, aes(x = factor(ps), y = fast_laps)) +
      geom_boxplot(fill = "green", alpha = 0.7) +
      geom_smooth(aes(x = ps, y = fast_laps, group = 1), 
                  method = "loess", se = FALSE, color = "red", size = 1) +
      labs(
        title = "Fast Laps by Finish Position",
        x = "Finish Position",
        y = "Fast Laps"
      ) +
      theme_minimal() +
      scale_x_discrete(limits = factor(1:40)) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = "#2d2d2d"),
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.grid.major = element_line(color = "#404040"),
        panel.grid.minor = element_line(color = "#333333"),
        text = element_text(color = "#ffffff"),
        axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1),
        axis.text.y = element_text(color = "#ffffff")
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        paper_bgcolor = "#2d2d2d",
        plot_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff")
      )
  })
  
  #--------------------- FD Dominator Analysis ---------------------#
  
  # FD Analysis Info Display
  output$fd_analysis_info <- renderUI({
    req(values$selected_race_ids)
    
    tagList(
      h4(style = "color: #FFD700;", 
         "Ready to analyze ", length(values$selected_race_ids), " selected races")
    )
  })
  
  # Generate FD Dominator Analysis
  observeEvent(input$generate_fd_analysis, {
    req(values$selected_race_ids, values$analysis_filtered_data)
    
    if (length(values$selected_race_ids) == 0) {
      showNotification("No races selected. Go to Race Selection tab.", type = "warning")
      return()
    }
    
    withProgress(message = 'Generating FD Dominator Analysis...', {
      incProgress(0.2, detail = "Filtering selected races...")
      
      fd_filtered_data <- values$analysis_filtered_data %>%
        filter(race_id %in% values$selected_race_ids)
      
      incProgress(0.5, detail = "Calculating dominator metrics...")
      
      fd_analysis <- fd_filtered_data %>%
        group_by(race_id) %>%
        mutate(
          FDSP_calc = lead_laps * 0.1,
          FDDomRank_calc = dense_rank(desc(FDSP_calc))
        ) %>%
        ungroup() %>%
        mutate(
          FDSP = if("FDSP" %in% names(.) && !all(is.na(FDSP))) FDSP else FDSP_calc,
          FDDomRank = if("FDDomRank" %in% names(.) && !all(is.na(FDDomRank))) FDDomRank else FDDomRank_calc
        )
      
      values$fd_analysis_data <- fd_analysis
      values$fd_analysis_ready <- TRUE
      
      incProgress(1.0, detail = "Complete!")
      
      showNotification(
        paste("FD Analysis generated for", length(values$selected_race_ids), "races"),
        type = "message",
        duration = 3
      )
    })
  })
  
  # Output to show/hide FD analysis results
  output$fd_analysis_ready <- reactive({
    return(values$fd_analysis_ready)
  })
  outputOptions(output, "fd_analysis_ready", suspendWhenHidden = FALSE)
  
  # FD Top Dominators Table
  output$fd_top_doms_table <- DT::renderDataTable({
    req(values$fd_analysis_data)
    
    top_doms <- values$fd_analysis_data %>%
      filter(FDSP > 0) %>%
      select(
        Driver = Full_Name,
        Start = start_ps,
        Finish = ps,
        Qualifying,
        Team = team_name,
        "Laps Led" = lead_laps,
        "Dom Pts" = FDSP,
        "Dom Rank" = FDDomRank,
        Season = race_season,
        Race = race_name,
        Track = track_name
      ) %>%
      mutate(`Dom Pts` = round(`Dom Pts`, 1)) %>%
      arrange(desc(`Dom Pts`)) %>%
      slice_head(n = 25)
    
    DT::datatable(
      top_doms,
      caption = "Top 25 FD Dominators",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'ftip',
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
      filter = "top",
      class = "display nowrap compact"
    )
  })
  
  # FD Score Distribution Plot
  output$fd_score_dist_plot <- renderPlotly({
    req(values$fd_analysis_data)
    
    plot_data <- values$fd_analysis_data %>%
      filter(FDDomRank <= 10, FDSP > 0)
    
    p <- ggplot(plot_data, aes(x = factor(FDDomRank), y = FDSP)) +
      geom_boxplot(fill = "forestgreen", alpha = 0.7) +
      geom_smooth(aes(x = FDDomRank, y = FDSP, group = 1), 
                  method = "loess", se = FALSE, color = "red", size = 1) +
      labs(
        title = "FD Dominator Points Distribution by Dom Rank (Top 10)",
        x = "Dominator Rank",
        y = "Dominator Points"
      ) +
      theme_minimal() +
      scale_x_discrete(limits = factor(1:10)) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = "#2d2d2d"),
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.grid.major = element_line(color = "#404040"),
        panel.grid.minor = element_line(color = "#333333"),
        text = element_text(color = "#ffffff"),
        axis.text.x = element_text(color = "#ffffff"),
        axis.text.y = element_text(color = "#ffffff")
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        paper_bgcolor = "#2d2d2d",
        plot_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff")
      )
  })
  
  # FD Rank Finish Plot
  output$fd_rank_finish_plot <- renderPlotly({
    req(values$fd_analysis_data)
    
    plot_data <- values$fd_analysis_data %>%
      filter(FDDomRank <= 10, FDSP > 0)
    
    p <- ggplot(plot_data, aes(x = factor(FDDomRank), y = ps)) +
      geom_boxplot(fill = "darkgreen", alpha = 0.7) +
      geom_smooth(aes(x = FDDomRank, y = ps, group = 1), 
                  method = "loess", se = FALSE, color = "red", size = 1) +
      labs(
        title = "Finish Position by FD Dom Rank (Top 10)",
        x = "Dominator Rank",
        y = "Finish Position"
      ) +
      theme_minimal() +
      scale_x_discrete(limits = factor(1:10)) +
      scale_y_continuous(breaks = seq(0, 40, 5)) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = "#2d2d2d"),
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.grid.major = element_line(color = "#404040"),
        panel.grid.minor = element_line(color = "#333333"),
        text = element_text(color = "#ffffff"),
        axis.text.x = element_text(color = "#ffffff"),
        axis.text.y = element_text(color = "#ffffff")
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        paper_bgcolor = "#2d2d2d",
        plot_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff")
      )
  })
  
  # FD Points by Finish Plot
  output$fd_pts_by_finish_plot <- renderPlotly({
    req(values$fd_analysis_data)
    
    plot_data <- values$fd_analysis_data %>%
      filter(ps <= 40, !is.na(ps), !is.na(FDSP))
    
    p <- ggplot(plot_data, aes(x = factor(ps), y = FDSP)) +
      geom_boxplot(fill = "darkgreen", alpha = 0.7) +
      geom_smooth(aes(x = ps, y = FDSP, group = 1), 
                  method = "loess", se = FALSE, color = "red", size = 1) +
      labs(
        title = "FD Dominator Points by Finish Position",
        x = "Finish Position",
        y = "Dominator Points"
      ) +
      theme_minimal() +
      scale_x_discrete(limits = factor(1:40)) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = "#2d2d2d"),
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.grid.major = element_line(color = "#404040"),
        panel.grid.minor = element_line(color = "#333333"),
        text = element_text(color = "#ffffff"),
        axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1),
        axis.text.y = element_text(color = "#ffffff")
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        paper_bgcolor = "#2d2d2d",
        plot_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff")
      )
  })
  
  # FD Laps Led Plot
  output$fd_laps_led_plot <- renderPlotly({
    req(values$fd_analysis_data)
    
    plot_data <- values$fd_analysis_data %>%
      filter(ps <= 40, !is.na(ps), !is.na(lead_laps))
    
    p <- ggplot(plot_data, aes(x = factor(ps), y = lead_laps)) +
      geom_boxplot(fill = "lightgreen", alpha = 0.7) +
      geom_smooth(aes(x = ps, y = lead_laps, group = 1), 
                  method = "loess", se = FALSE, color = "red", size = 1) +
      labs(
        title = "Laps Led by Finish Position",
        x = "Finish Position",
        y = "Laps Led"
      ) +
      theme_minimal() +
      scale_x_discrete(limits = factor(1:40)) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = "#2d2d2d"),
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.grid.major = element_line(color = "#404040"),
        panel.grid.minor = element_line(color = "#333333"),
        text = element_text(color = "#ffffff"),
        axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1),
        axis.text.y = element_text(color = "#ffffff")
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        paper_bgcolor = "#2d2d2d",
        plot_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff")
      )
  })
  
  #--------------------- Place Differential Analysis ---------------------#
  
  # Generate PD Analysis
  observeEvent(input$generate_pd_analysis, {
    req(values$analysis_filtered_data, values$selected_race_ids)
    
    withProgress(message = 'Generating Place Differential Analysis...', {
      incProgress(0.3, detail = "Calculating place differentials...")
      
      pd_data <- values$analysis_filtered_data %>%
        filter(race_id %in% values$selected_race_ids, !is.na(start_ps), !is.na(ps)) %>%
        mutate(PD = start_ps - ps)
      
      values$pd_analysis_data <- pd_data
      values$pd_analysis_ready <- TRUE
      
      incProgress(1.0, detail = "Complete!")
      
      showNotification(
        "Place Differential Analysis generated",
        type = "message",
        duration = 3
      )
    })
  })
  
  # Output to show/hide PD analysis results
  output$pd_analysis_ready <- reactive({
    return(values$pd_analysis_ready)
  })
  outputOptions(output, "pd_analysis_ready", suspendWhenHidden = FALSE)
  
  # PD Scatter Plot
  output$pd_scatter_plot <- renderPlotly({
    req(values$pd_analysis_data)
    
    plot_data <- values$pd_analysis_data %>%
      filter(start_ps <= 40, ps <= 40)
    
    p <- ggplot(plot_data, 
                aes(x = start_ps, y = ps, 
                    size = abs(PD), 
                    color = PD,
                    text = paste0(
                      "Driver: ", Full_Name,
                      "<br>Team: ", team_name,
                      "<br>Start: ", start_ps,
                      "<br>Finish: ", ps,
                      "<br>PD: ", PD,
                      "<br>Season: ", race_season,
                      "<br>Track: ", track_name
                    ))) +
      geom_point(alpha = 0.6) +
      geom_abline(linetype = "dashed", color = "gray50") +
      scale_color_gradient2(
        low = "red",
        mid = "gray80",
        high = "green",
        midpoint = 0
      ) +
      scale_size_continuous(range = c(2, 10)) +
      scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
      scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
      labs(
        title = "Starting vs Finishing Position",
        x = "Starting Position",
        y = "Finishing Position",
        color = "Position\nChange",
        size = "Magnitude"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
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
        font = list(color = "#ffffff")
      )
  })
  
  # PD Histogram
  output$pd_histogram_plot <- renderPlotly({
    req(values$pd_analysis_data)
    
    p <- ggplot(values$pd_analysis_data, aes(x = PD)) +
      geom_histogram(binwidth = 1, fill = "dodgerblue", color = "black", alpha = 0.7) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
      labs(
        title = "Position Change Distribution",
        x = "Place Differential",
        y = "Count"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = "#2d2d2d"),
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.grid.major = element_line(color = "#404040"),
        panel.grid.minor = element_line(color = "#333333"),
        text = element_text(color = "#ffffff"),
        axis.text.x = element_text(color = "#ffffff"),
        axis.text.y = element_text(color = "#ffffff")
      )
    
    ggplotly(p) %>%
      layout(
        paper_bgcolor = "#2d2d2d",
        plot_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff")
      )
  })
  
  # PD by Start Position
  output$pd_by_start_plot <- renderPlotly({
    req(values$pd_analysis_data)
    
    plot_data <- values$pd_analysis_data %>%
      filter(start_ps <= 40)
    
    p <- ggplot(plot_data, aes(x = factor(start_ps), y = PD)) +
      geom_boxplot(fill = "skyblue", alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkblue", size = 1) +
      geom_smooth(aes(x = as.integer(start_ps), y = PD, group = 1), 
                  method = "loess", se = FALSE, color = "red", size = 1.2) +
      labs(
        title = "Place Differential by Starting Position",
        x = "Starting Position",
        y = "Place Differential"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = "#2d2d2d"),
        plot.background = element_rect(fill = "#2d2d2d"),
        panel.grid.major = element_line(color = "#404040"),
        panel.grid.minor = element_line(color = "#333333"),
        text = element_text(color = "#ffffff"),
        axis.text.x = element_text(color = "#ffffff", angle = 45, hjust = 1),
        axis.text.y = element_text(color = "#ffffff")
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        paper_bgcolor = "#2d2d2d",
        plot_bgcolor = "#2d2d2d",
        font = list(color = "#ffffff")
      )
  })
  
  # Biggest Movers Table
  output$pd_movers_table <- DT::renderDataTable({
    req(values$pd_analysis_data)
    
    biggest_movers <- values$pd_analysis_data %>%
      select(
        Driver = Full_Name,
        Team = team_name,
        Start = start_ps,
        Finish = ps,
        PD,
        Season = race_season,
        Race = race_name,
        Track = track_name
      ) %>%
      arrange(desc(PD)) %>%
      slice_head(n = 20)
    
    DT::datatable(
      biggest_movers,
      caption = "Top 20 Biggest Position Gains",
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'ftip',
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
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
  

  
  