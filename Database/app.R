# NASCAR Race Analysis App - UPDATED VERSION
# Changes: compact nav, removed search boxes on Performance/Fantasy,
# added Finish Rates tab, salary upload on Entry List, team tiers, Create Input File button

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)
library(readxl)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(openxlsx)
library(plotly)
library(ggplot2)
library(jsonlite)
library(tidyr)

#--------------------- Helper Functions ---------------------#

load_nascar_database <- function() {
  if (file.exists("NascarDatabase.csv")) {
    data <- read_csv("NascarDatabase.csv", show_col_types = FALSE)
    if (file.exists("RaceIDs.xlsx")) {
      race_ids <- read_excel("RaceIDs.xlsx")
      if ("race_type_id" %in% names(race_ids)) {
        regular_race_ids <- race_ids %>% pull(race_id)
        data <- data %>% filter(race_id %in% regular_race_ids)
      }
    }
    return(data)
  } else {
    return(NULL)
  }
}

load_race_list <- function() {
  if (file.exists("RaceIDs.xlsx")) {
    race_data <- read_excel("RaceIDs.xlsx")
    filtered_data <- race_data %>% filter(Historical == "N")
  } else {
    return(NULL)
  }
}

load_entry_list <- function(race_season, series_id, race_id) {
  tryCatch({
    url <- paste0('https://cf.nascar.com/cacher/', race_season, '/', series_id, '/', race_id, '/weekend-feed.json')
    json_data <- fromJSON(url)
    entry_list <- json_data$weekend_race %>%
      unnest(results, names_sep = "_") %>%
      select(
        results_starting_position, results_driver_fullname, results_car_number,
        results_team_name, results_crew_chief_fullname, results_car_make, results_sponsor
      ) %>%
      rename(
        Start = results_starting_position, Name = results_driver_fullname,
        Car = results_car_number, Team = results_team_name,
        Make = results_car_make, CC = results_crew_chief_fullname, Sponsor = results_sponsor
      ) %>%
      mutate(Car = as.integer(Car), Start = as.integer(Start)) %>%
      arrange(Start)
    return(entry_list)
  }, error = function(e) {
    return(data.frame(Start = integer(), Name = character(), Car = integer(),
                      Team = character(), CC = character(), Make = character(), Sponsor = character()))
  })
}

calc_dom_points <- function(total_laps, green_laps) {
  dk_points <- (0.45 * green_laps) + (0.25 * total_laps)
  fd_points <- 0.1 * total_laps
  return(list(dk = round(dk_points, 1), fd = round(fd_points, 1)))
}

# Calculate finish rates for a dataset
calc_finish_rates <- function(data, group_col, group_label) {
  data %>%
    group_by(!!sym(group_col)) %>%
    summarize(
      Races     = n(),
      Win       = round(mean(ps == 1,  na.rm = TRUE) * 100, 1),
      `Top 3`   = round(mean(ps <= 3,  na.rm = TRUE) * 100, 1),
      `Top 5`   = round(mean(ps <= 5,  na.rm = TRUE) * 100, 1),
      `Top 10`  = round(mean(ps <= 10, na.rm = TRUE) * 100, 1),
      `Top 15`  = round(mean(ps <= 15, na.rm = TRUE) * 100, 1),
      `Top 20`  = round(mean(ps <= 20, na.rm = TRUE) * 100, 1),
      `Top 25`  = round(mean(ps <= 25, na.rm = TRUE) * 100, 1),
      `Top 30`  = round(mean(ps <= 30, na.rm = TRUE) * 100, 1),
      `Avg Finish` = round(mean(ps, na.rm = TRUE), 1),
      .groups = 'drop'
    ) %>%
    rename(!!group_label := !!sym(group_col))
}

#--------------------- UI Definition ---------------------#

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      body { background-color: #1a1a1a !important; color: #ffffff !important; font-family: 'Helvetica Neue', Arial, sans-serif; }

      .app-header { background-color: #000000; padding: 10px 30px; border-bottom: 3px solid #FFD700; display: flex; align-items: center; margin-bottom: 0; }
      .app-logo { height: 35px; margin-right: 15px; }
      .app-title { color: #FFD700; font-size: 20px; font-weight: bold; margin: 0; }

      /* Compact navbar */
      .navbar-default { background-color: #000000 !important; border: none !important; border-bottom: 3px solid #FFD700 !important; border-radius: 0 !important; margin-bottom: 0 !important; min-height: 46px !important; }
      .navbar-default .navbar-nav > li > a { color: #FFD700 !important; background-color: #000000 !important; padding: 13px 22px !important; font-weight: 700 !important; font-size: 13px !important; letter-spacing: 0.5px !important; text-transform: uppercase !important; transition: all 0.3s ease !important; border-right: 1px solid #333333 !important; }
      .navbar-default .navbar-nav > li:last-child > a { border-right: none !important; }
      .navbar-default .navbar-nav > li > a:hover, .navbar-default .navbar-nav > li > a:focus { background-color: #1a1a1a !important; color: #FFD700 !important; }
      .navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:hover, .navbar-default .navbar-nav > .active > a:focus { background-color: #FFD700 !important; color: #000000 !important; font-weight: 900 !important; }
      .navbar-nav { margin: 0 !important; }

      .container-fluid { padding: 15px 25px; }

      .box { background-color: #2d2d2d !important; border: 1px solid #444444 !important; border-radius: 5px; box-shadow: 0 2px 4px rgba(0,0,0,.3); margin-bottom: 15px; color: #ffffff !important; }
      .box-header { background-color: #333333 !important; color: #FFD700 !important; border-bottom: 2px solid #FFD700 !important; padding: 10px 15px; border-radius: 5px 5px 0 0; }
      .box-title { color: #FFD700 !important; font-size: 16px; font-weight: bold; margin: 0; }
      .box-body { padding: 15px; }

      .btn-primary { background-color: #FFD700 !important; border-color: #DAA520 !important; color: #000000 !important; font-weight: bold; }
      .btn-primary:hover { background-color: #DAA520 !important; color: #000000 !important; }
      .btn-success { background-color: #FFD700 !important; border-color: #DAA520 !important; color: #000000 !important; font-weight: bold; }
      .btn-success:hover { background-color: #DAA520 !important; }
      .btn-warning { background-color: #DAA520 !important; border-color: #B8860B !important; color: #000000 !important; font-weight: bold; }
      .btn-info { background-color: #555555 !important; border-color: #666666 !important; color: #FFD700 !important; font-weight: bold; }
      .btn-danger { background-color: #cc3300 !important; border-color: #aa2200 !important; color: #ffffff !important; font-weight: bold; }

      .form-control, .selectize-input { background-color: #404040 !important; border: 1px solid #666666 !important; color: #ffffff !important; }
      .form-control:focus { border-color: #FFD700 !important; box-shadow: 0 0 0 0.2rem rgba(255,215,0,0.25) !important; }
      .selectize-dropdown { background-color: #333333 !important; border: 1px solid #666666 !important; color: #ffffff !important; }
      .selectize-dropdown-content .option { color: #ffffff !important; background-color: #333333 !important; }
      .selectize-dropdown-content .option:hover, .selectize-dropdown-content .option.active { background-color: #FFD700 !important; color: #000000 !important; }
      label { color: #ffffff !important; font-weight: 500; }

      .dataTables_wrapper { color: #ffffff !important; }
      .dataTable thead th { background-color: #333333 !important; color: #FFD700 !important; border-bottom: 2px solid #FFD700 !important; font-weight: bold; padding: 10px 8px; }
      .dataTable tbody td { background-color: #2d2d2d !important; color: #ffffff !important; border-bottom: 1px solid #444444 !important; padding: 8px; }
      .dataTable tbody tr:hover { background-color: #404040 !important; }
      .dataTables_info, .dataTables_length label, .dataTables_filter label { color: #ffffff !important; }
      .dataTables_paginate .paginate_button { background-color: #333333 !important; color: #FFD700 !important; border: 1px solid #555555 !important; margin: 0 2px; }
      .dataTables_paginate .paginate_button:hover { background-color: #FFD700 !important; color: #000000 !important; }
      .dataTables_paginate .paginate_button.current { background-color: #FFD700 !important; color: #000000 !important; font-weight: bold; }
      .dataTables_wrapper input[type=search], .dataTables_wrapper input[type=text] { background-color: #404040 !important; border: 1px solid #FFD700 !important; color: #ffffff !important; padding: 4px 8px !important; border-radius: 3px !important; }
      .dataTables_wrapper select { background-color: #404040 !important; border: 1px solid #FFD700 !important; color: #ffffff !important; padding: 4px !important; border-radius: 3px !important; }

      .info-box { background-color: #333333; border-left: 4px solid #FFD700; padding: 12px; margin-bottom: 12px; border-radius: 3px; }

      .irs--shiny .irs-bar { background: #FFD700 !important; border-top: 1px solid #FFD700 !important; border-bottom: 1px solid #FFD700 !important; }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #FFD700 !important; color: #000000 !important; }
      .irs--shiny .irs-handle { background: #FFD700 !important; border: 2px solid #DAA520 !important; }
      .irs--shiny .irs-min, .irs--shiny .irs-max { color: #FFD700 !important; background: #333333 !important; }
      .irs--shiny .irs-line { background: #555555 !important; }

      .tier-box { background-color: #2a2a2a; border: 1px solid #555; border-radius: 4px; padding: 10px; margin-bottom: 8px; }
      .tier-label { color: #FFD700; font-weight: bold; margin-bottom: 5px; }

      /* File input styling */
      .shiny-input-container input[type=file] { color: #ffffff; }
      .btn-file { background-color: #555555 !important; border-color: #666666 !important; color: #FFD700 !important; }
    "))
  ),
  
  div(class = "app-header",
      img(src = "logo.jpg", class = "app-logo"),
      h1("Golden Ticket Research Center", class = "app-title")
  ),
  
  navbarPage(
    title = NULL,
    id = "main_tabs",
    windowTitle = "Golden Ticket Research Center",
    
    #----- RACE SELECTION TAB -----#
    tabPanel("Race Selection", value = "race_selection",
             fluidRow(column(12,
                             div(class = "box",
                                 div(class = "box-header", h3("Race Selection Configuration", class = "box-title")),
                                 div(class = "box-body",
                                     fluidRow(
                                       column(3, selectizeInput("analysis_series", "Series:", choices = c("Cup Series"=1,"Xfinity Series"=2,"Truck Series"=3), selected=1)),
                                       column(3, selectizeInput("analysis_primary_track", "Primary Track:", choices=NULL, options=list(placeholder="Select Track"))),
                                       column(3, selectizeInput("analysis_similar_tracks", "Similar Tracks (Optional):", choices=NULL, multiple=TRUE, options=list(placeholder="Select Track(s)"))),
                                       column(3, selectizeInput("analysis_race_id", "Upcoming Race:", choices=NULL, options=list(placeholder="Select Race")))
                                     ),
                                     fluidRow(
                                       column(3, numericInput("analysis_start_year", "Start Year:", value=2022, min=2022, max=2026, step=1)),
                                       column(3, numericInput("analysis_end_year", "End Year:", value=2026, min=2022, max=2026, step=1)),
                                       column(6, div(style="margin-top:25px;",
                                                     actionButton("confirm_analysis_filters", "Load Races", class="btn-primary", style="width:100%;padding:10px;font-size:16px;")))
                                     )
                                 )
                             )
             )),
             conditionalPanel(condition = "output.filters_confirmed",
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-header", h3("Filtered Races", class = "box-title")),
                                                  div(class = "box-body",
                                                      fluidRow(
                                                        column(8, sliderInput("lap_range_dominator","Lap Range for Dominator Analysis:", min=0, max=600, value=c(0,600), step=10)),
                                                        column(4, uiOutput("lap_filter_summary"))
                                                      ),
                                                      fluidRow(column(12, withSpinner(DT::dataTableOutput("races_selection_table"))))
                                                  )
                                              )
                              ))
             )
    ),
    
    #----- ENTRY LIST TAB -----#
    tabPanel("Entry List", value = "entry_list",
             conditionalPanel(condition = "output.filters_confirmed",
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-header", style="display:flex;justify-content:space-between;align-items:center;",
                                                      uiOutput("entry_list_title", inline=TRUE),
                                                      div(style="display:flex;gap:8px;align-items:center;",
                                                          downloadButton("download_entry_list_csv", "CSV", class="btn-success", style="margin:0;"),
                                                          downloadButton("download_entry_list_excel", "Excel", class="btn-success", style="margin:0;")
                                                      )
                                                  ),
                                                  div(class = "box-body",
                                                      fluidRow(column(12, withSpinner(DT::dataTableOutput("entry_list_table"))))
                                                  )
                                              )
                              ))
             ),
             conditionalPanel(condition = "!output.filters_confirmed",
                              fluidRow(column(12, div(class="box", div(class="box-header", h3("No Races Loaded", class="box-title")),
                                                      div(class="box-body", p(style="color:#ffffff;text-align:center;padding:40px;font-size:16px;","Please go to the Race Selection tab and load races first.")))))
             )
    ),
    
    #----- FINISH RATES TAB -----#
    tabPanel("Finish Rates", value = "finish_rates",
             conditionalPanel(condition = "output.filters_confirmed",
                              
                              # Controls row
                              fluidRow(column(12,
                                              div(class="box",
                                                  div(class="box-header", style="display:flex;justify-content:space-between;align-items:center;",
                                                      h3("Finish Rate Controls", class="box-title"),
                                                      downloadButton("download_finish_rates", "Download All", class="btn-success", style="margin:0;")
                                                  ),
                                                  div(class="box-body",
                                                      fluidRow(
                                                        column(3,
                                                               radioButtons("fr_view", "View By:",
                                                                            choices=c("Driver"="driver","Car"="car","Team"="team","Tier"="tier"),
                                                                            selected="driver", inline=TRUE)
                                                        ),
                                                        column(3,
                                                               radioButtons("fr_time", "Time Period:",
                                                                            choices=c("Full History"="all","2025 Only"="2025"),
                                                                            selected="2025", inline=TRUE)
                                                        ),
                                                        column(6,
                                                               conditionalPanel(condition="input.fr_view == 'tier'",
                                                                                div(style="background:#222;border:1px solid #555;border-radius:4px;padding:10px;",
                                                                                    p(style="color:#FFD700;font-weight:bold;margin-bottom:8px;", "Team Tier Configuration"),
                                                                                    uiOutput("tier_config_ui"),
                                                                                    div(style="margin-top:8px;display:flex;gap:8px;",
                                                                                        actionButton("add_tier", "+ Add Tier", class="btn-info", style="font-size:12px;padding:4px 10px;"),
                                                                                        actionButton("remove_tier", "- Remove Tier", class="btn-danger", style="font-size:12px;padding:4px 10px;")
                                                                                    )
                                                                                )
                                                               )
                                                        )
                                                      )
                                                  )
                                              )
                              )),
                              
                              # Finish Rates Table
                              fluidRow(column(12,
                                              div(class="box",
                                                  div(class="box-header", h3("Finish Rates (%)", class="box-title")),
                                                  div(class="box-body",
                                                      withSpinner(DT::dataTableOutput("finish_rates_table"))
                                                  )
                                              )
                              ))
             ),
             conditionalPanel(condition="!output.filters_confirmed",
                              fluidRow(column(12, div(class="box", div(class="box-header", h3("No Races Loaded", class="box-title")),
                                                      div(class="box-body", p(style="color:#ffffff;text-align:center;padding:40px;font-size:16px;","Please go to the Race Selection tab and load races first.")))))
             )
    ),
    
    #----- DOMINATOR TAB -----#
    tabPanel("Dominator", value = "dominator",
             conditionalPanel(condition = "output.filters_confirmed",
                              fluidRow(column(12,
                                              div(class="box",
                                                  div(class="box-header", style="display:flex;justify-content:space-between;align-items:center;",
                                                      h3("Dominator Data", class="box-title", style="margin:0;"),
                                                      div(style="display:flex;gap:8px;",
                                                          downloadButton("download_dominator_csv","CSV",class="btn-success",style="margin:0;"),
                                                          downloadButton("download_dominator_profile","Download Profile",class="btn-success",style="margin:0;"),
                                                          downloadButton("download_input_file","Create Input File",class="btn-warning",style="margin:0;")
                                                      )
                                                  ),
                                                  div(class="box-body", withSpinner(DT::dataTableOutput("dominator_data_table")))
                                              )
                              )),
                              fluidRow(column(12,
                                              div(class="box",
                                                  div(class="box-header", h3("Dominator Visualizations", class="box-title")),
                                                  div(class="box-body",
                                                      fluidRow(
                                                        column(6, selectInput("dom_visual_type","Select Visualization:",
                                                                              choices=c("Score Distribution by Dom Rank"="score_dist","Dom Rank Finish Ranges"="rank_finish",
                                                                                        "Dom Pts by Finish Position"="pts_by_finish","Dom Pts by Starting Position"="dom_pts_start",
                                                                                        "Dom Rank by Starting Position"="dom_rank_start","Laps Led by Finish Position"="laps_led",
                                                                                        "Laps Led by Starting Position"="laps_led_start","Fast Laps by Finish Position"="fast_laps",
                                                                                        "Fast Laps by Starting Position"="fast_laps_start","Driver Dominator Boxplots"="driver_boxplot",
                                                                                        "Team Dominator Boxplots"="team_boxplot","Entry Boxplots"="entry_boxplot"), selected="score_dist")),
                                                        column(6, radioButtons("dom_platform","Platform:",choices=c("DraftKings"="DK","FanDuel"="FD"),selected="DK",inline=TRUE))
                                                      ),
                                                      fluidRow(column(12, withSpinner(plotlyOutput("dominator_plot", height="700px"))))
                                                  )
                                              )
                              ))
             ),
             conditionalPanel(condition="!output.filters_confirmed",
                              fluidRow(column(12, div(class="box", div(class="box-header", h3("No Races Loaded",class="box-title")),
                                                      div(class="box-body",p(style="color:#ffffff;text-align:center;padding:40px;font-size:16px;","Please go to the Race Selection tab and load races first.")))))
             )
    ),
    
    #----- PLACE DIFFERENTIAL TAB -----#
    tabPanel("Place Differential", value = "place_differential",
             conditionalPanel(condition="output.filters_confirmed",
                              fluidRow(column(12,
                                              div(class="box",
                                                  div(class="box-header", style="display:flex;justify-content:space-between;align-items:center;",
                                                      h3("Place Differential Data",class="box-title",style="margin:0;"),
                                                      downloadButton("download_pd_csv","CSV",class="btn-success",style="margin:0;")
                                                  ),
                                                  div(class="box-body", withSpinner(DT::dataTableOutput("pd_data_table")))
                                              )
                              )),
                              fluidRow(column(12,
                                              div(class="box",
                                                  div(class="box-header", h3("Place Differential Visualizations",class="box-title")),
                                                  div(class="box-body",
                                                      fluidRow(column(4, selectInput("pd_visual_type","Visualization Type:",
                                                                                     choices=c("Start vs Finish Scatter"="scatter","Position Change Distribution"="histogram",
                                                                                               "PD by Start Position"="boxplot_start","PD by Finish Position"="boxplot_finish")))),
                                                      fluidRow(column(12, withSpinner(plotlyOutput("pd_plot",height="700px"))))
                                                  )
                                              )
                              ))
             ),
             conditionalPanel(condition="!output.filters_confirmed",
                              fluidRow(column(12, div(class="box", div(class="box-header", h3("No Races Loaded",class="box-title")),
                                                      div(class="box-body",p(style="color:#ffffff;text-align:center;padding:40px;font-size:16px;","Please go to the Race Selection tab and load races first.")))))
             )
    ),
    
    #----- PERFORMANCE TAB -----#
    tabPanel("Performance", value = "performance",
             conditionalPanel(condition="output.filters_confirmed",
                              fluidRow(column(12,
                                              div(class="box",
                                                  div(class="box-header", h3("Performance Data",class="box-title")),
                                                  div(class="box-body",
                                                      fluidRow(
                                                        column(6, radioButtons("perf_time_filter","Time Period:",choices=c("Full History"="all","2025 Only"="2025"),selected="all",inline=TRUE)),
                                                        column(6, downloadButton("download_performance_csv","Download CSV",class="btn-success",style="margin-top:0px;"))
                                                      ),
                                                      fluidRow(column(12, withSpinner(DT::dataTableOutput("performance_data_table"))))
                                                  )
                                              )
                              )),
                              fluidRow(column(12,
                                              div(class="box",
                                                  div(class="box-header", h3("Performance Visualizations",class="box-title")),
                                                  div(class="box-body",
                                                      fluidRow(
                                                        column(6, selectInput("perf_visual_type","Select Visualization:",
                                                                              choices=c("Driver Speed Rank Distribution"="driver_speed","Team Speed Rank Distribution"="team_speed",
                                                                                        "Driver Finish Distribution"="driver_finish","Team Finish Distribution"="team_finish",
                                                                                        "Driver ARP Distribution"="driver_arp","Team ARP Distribution"="team_arp"), selected="driver_speed")),
                                                        column(6, radioButtons("perf_visual_time","Time Period:",choices=c("Full History"="all","2025 Only"="2025"),selected="all",inline=TRUE))
                                                      ),
                                                      fluidRow(column(12, withSpinner(plotlyOutput("performance_plot",height="800px"))))
                                                  )
                                              )
                              ))
             ),
             conditionalPanel(condition="!output.filters_confirmed",
                              fluidRow(column(12, div(class="box", div(class="box-header", h3("No Races Loaded",class="box-title")),
                                                      div(class="box-body",p(style="color:#ffffff;text-align:center;padding:40px;font-size:16px;","Please go to the Race Selection tab and load races first.")))))
             )
    ),
    
    #----- FANTASY SCORING TAB -----#
    tabPanel("Fantasy Scoring", value = "fantasy_scoring",
             conditionalPanel(condition="output.filters_confirmed",
                              fluidRow(column(12,
                                              div(class="box",
                                                  div(class="box-header", h3("Fantasy Scoring Data",class="box-title")),
                                                  div(class="box-body",
                                                      fluidRow(
                                                        column(6, radioButtons("fs_platform","Platform:",choices=c("DraftKings"="DK","FanDuel"="FD"),selected="DK",inline=TRUE)),
                                                        column(6, downloadButton("download_fantasy_csv","Download CSV",class="btn-success",style="margin-top:0px;"))
                                                      ),
                                                      fluidRow(column(12, withSpinner(DT::dataTableOutput("fantasy_data_table"))))
                                                  )
                                              )
                              )),
                              fluidRow(column(12,
                                              div(class="box",
                                                  div(class="box-header", h3("Fantasy Scoring Visualizations",class="box-title")),
                                                  div(class="box-body",
                                                      fluidRow(
                                                        column(6, selectInput("fs_visual_type","Select Visualization:",
                                                                              choices=c("Score Distribution by Rank"="score_dist","Scoring Components Breakdown"="components",
                                                                                        "Score Distribution by Start Position"="score_by_start","Score Distribution by Finish Position"="score_by_finish",
                                                                                        "Components by Start Position"="components_start","Components by Finish Position"="components_finish"), selected="score_dist")),
                                                        column(6, radioButtons("fs_visual_platform","Platform:",choices=c("DraftKings"="DK","FanDuel"="FD"),selected="DK",inline=TRUE))
                                                      ),
                                                      fluidRow(column(12, withSpinner(plotlyOutput("fantasy_plot",height="700px"))))
                                                  )
                                              )
                              ))
             ),
             conditionalPanel(condition="!output.filters_confirmed",
                              fluidRow(column(12, div(class="box", div(class="box-header", h3("No Races Loaded",class="box-title")),
                                                      div(class="box-body",p(style="color:#ffffff;text-align:center;padding:40px;font-size:16px;","Please go to the Race Selection tab and load races first.")))))
             )
    )
  )
)

#--------------------- Server Function ---------------------#

server <- function(input, output, session) {
  
  values <- reactiveValues(
    nascar_data = NULL,
    race_list = NULL,
    analysis_filtered_data = NULL,
    analysis_entry_list = NULL,
    analysis_races_available = NULL,
    filters_confirmed = FALSE,
    pd_race_ids = NULL,
    performance_race_ids = NULL,
    num_tiers = 3
  )
  
  # Load initial data
  observe({
    withProgress(message='Loading Golden Ticket Database...', {
      values$nascar_data <- load_nascar_database()
      values$race_list <- load_race_list()
      if (!is.null(values$nascar_data)) {
        incProgress(0.3, detail="Processing tracks...")
        all_race_data <- if(file.exists("RaceIDs.xlsx")) read_excel("RaceIDs.xlsx") else NULL
        if (!is.null(all_race_data)) {
          all_tracks <- sort(unique(all_race_data$track_name[!is.na(all_race_data$track_name)]))
        } else {
          all_tracks <- sort(unique(values$nascar_data$track_name[!is.na(values$nascar_data$track_name)]))
        }
        updateSelectizeInput(session,"analysis_primary_track",choices=all_tracks)
        updateSelectizeInput(session,"analysis_similar_tracks",choices=all_tracks)
        incProgress(1.0)
      }
    })
  })
  
  observe({
    req(input$analysis_series, input$analysis_primary_track, input$analysis_start_year, input$analysis_end_year)
    all_races <- if(file.exists("RaceIDs.xlsx")) read_excel("RaceIDs.xlsx") else NULL
    if (!is.null(all_races)) {
      available_races <- all_races %>%
        filter(series_id==as.numeric(input$analysis_series), track_name==input$analysis_primary_track,
               race_season>=input$analysis_start_year, race_season<=input$analysis_end_year, Historical=="N") %>%
        arrange(desc(race_season)) %>%
        mutate(race_label=paste0(race_season," - ",race_name))
      race_choices <- setNames(available_races$race_id, available_races$race_label)
      updateSelectizeInput(session,"analysis_race_id",choices=race_choices,
                           selected=if(length(race_choices)>0) race_choices[1] else NULL)
    }
  })
  
  observeEvent(input$confirm_analysis_filters, {
    req(input$analysis_series, input$analysis_primary_track, input$analysis_start_year, input$analysis_end_year, input$analysis_race_id)
    withProgress(message='Loading races...', {
      incProgress(0.2)
      all_races <- if(file.exists("RaceIDs.xlsx")) read_excel("RaceIDs.xlsx") else values$race_list
      tracks_to_include <- input$analysis_primary_track
      if (!is.null(input$analysis_similar_tracks) && length(input$analysis_similar_tracks)>0)
        tracks_to_include <- c(tracks_to_include, input$analysis_similar_tracks)
      
      filtered_race_list <- all_races %>%
        filter(series_id==as.numeric(input$analysis_series), track_name %in% tracks_to_include,
               race_season>=input$analysis_start_year, race_season<=input$analysis_end_year, Historical=="Y")
      
      incProgress(0.4)
      filtered_nascar <- values$nascar_data %>% filter(race_id %in% filtered_race_list$race_id)
      incProgress(0.6)
      
      race_info <- all_races %>% filter(race_id==as.numeric(input$analysis_race_id)) %>% slice(1)
      entry_list <- load_entry_list(race_info$race_season, as.numeric(input$analysis_series), as.numeric(input$analysis_race_id))
      
      incProgress(0.8)
      races_available <- filtered_race_list %>%
        left_join(
          filtered_nascar %>%
            group_by(race_id) %>%
            summarize(
              total_laps = if("actual_laps" %in% names(cur_data())) first(actual_laps) else if("TotalLaps" %in% names(cur_data())) first(TotalLaps) else NA_real_,
              caution_laps = if("number_of_caution_laps" %in% names(cur_data())) first(number_of_caution_laps) else if("CautionLaps" %in% names(cur_data())) first(CautionLaps) else 0,
              lead_lap = sum(LapsDown==0, na.rm=TRUE),
              crash_dnfs = sum(finishing_status %in% c("Accident","DVP","Damage"), na.rm=TRUE),
              mech_dnfs = sum(!finishing_status %in% c("Running","Accident","DVP","Damage"), na.rm=TRUE),
              .groups='drop'
            ), by="race_id"
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
      
      values$analysis_filtered_data <- filtered_nascar
      values$analysis_entry_list <- entry_list
      values$analysis_races_available <- races_available
      values$pd_race_ids <- races_available$race_id
      values$performance_race_ids <- races_available$race_id
      values$filters_confirmed <- TRUE
      incProgress(1.0)
      showNotification(paste("Loaded", nrow(filtered_race_list), "historical races."), type="message", duration=5)
    })
  })
  
  output$filters_confirmed <- reactive({ return(values$filters_confirmed) })
  outputOptions(output, "filters_confirmed", suspendWhenHidden=FALSE)
  
  output$races_selection_table <- DT::renderDataTable({
    req(values$analysis_races_available)
    race_selection_data <- values$analysis_races_available %>%
      select(Season=race_season, Track=track_name, Race=race_name, Cars=number_of_cars_in_field,
             Qualifying, Leaders=number_of_leaders, Cautions=number_of_cautions,
             `Scheduled Laps`=scheduled_laps, `Actual Laps`=total_laps,
             `DK Dom Avail`=DK_Dom_Available, `FD Dom Avail`=FD_Dom_Available,
             `Lead Lap`=lead_lap, `Crash DNFs`=crash_dnfs, `Mech DNFs`=mech_dnfs) %>%
      arrange(desc(Season))
    DT::datatable(race_selection_data, selection='none',
                  options=list(pageLength=20,scrollX=TRUE,dom='tip',columnDefs=list(list(className="dt-center",targets="_all"))),
                  rownames=FALSE, class="display nowrap compact")
  })
  
  output$lap_filter_summary <- renderUI({
    req(values$analysis_races_available, input$lap_range_dominator)
    filtered_count <- values$analysis_races_available %>%
      filter(total_laps>=input$lap_range_dominator[1], total_laps<=input$lap_range_dominator[2]) %>% nrow()
    total_count <- nrow(values$analysis_races_available)
    tagList(h4(style="color:#FFD700;margin-top:30px;", paste(filtered_count,"of",total_count,"races meet lap requirements")))
  })
  
  dominator_filtered_races <- reactive({
    req(values$analysis_races_available, input$lap_range_dominator)
    values$analysis_races_available %>%
      filter(total_laps>=input$lap_range_dominator[1], total_laps<=input$lap_range_dominator[2]) %>%
      pull(race_id)
  })
  
  #----- SALARY (auto-loaded from working directory) -----#
  # Loads DKSalaries.csv and FDSalaries.csv from the app working directory
  # DK format: Name, ID, Salary columns
  # FD format: First Name, Last Name, Id, Salary columns
  
  load_dk_salaries <- function() {
    for (f in c("DKSalaries.csv","DKSalaries.xlsx")) {
      if (file.exists(f)) {
        sal <- if(grepl(".csv",f)) read_csv(f,show_col_types=FALSE) else read_excel(f)
        cols <- tolower(names(sal))
        name_col <- names(sal)[cols == "name"][1]
        id_col   <- names(sal)[cols == "id"][1]
        sal_col  <- names(sal)[cols == "salary"][1]
        if (!is.na(name_col) && !is.na(id_col)) {
          return(sal %>% mutate(
            FullName = trimws(.data[[name_col]]),
            DKName   = FullName,
            DKID     = as.character(.data[[id_col]]),
            DKSalary = if(!is.na(sal_col)) as.numeric(.data[[sal_col]]) else NA_real_
          ) %>% select(FullName, DKName, DKID, DKSalary))
        }
      }
    }
    return(NULL)
  }
  
  load_fd_salaries <- function() {
    for (f in c("FDSalaries.csv","FDSalaries.xlsx")) {
      if (file.exists(f)) {
        sal <- if(grepl(".csv",f)) read_csv(f,show_col_types=FALSE) else read_excel(f)
        cols_lower <- tolower(names(sal))
        cols       <- names(sal)
        if ("first name" %in% cols_lower && "last name" %in% cols_lower) {
          fn_col  <- cols[cols_lower == "first name"][1]
          ln_col  <- cols[cols_lower == "last name"][1]
          id_col  <- cols[cols_lower == "id"][1]
          sal_col <- cols[cols_lower == "salary"][1]
          return(sal %>% mutate(
            FullName = trimws(paste(.data[[fn_col]], .data[[ln_col]])),
            FDName   = FullName,
            FDID     = if(!is.na(id_col)) as.character(.data[[id_col]]) else NA_character_,
            FDSalary = if(!is.na(sal_col)) as.numeric(.data[[sal_col]]) else NA_real_
          ) %>% select(FullName, FDName, FDID, FDSalary))
        }
      }
    }
    return(NULL)
  }
  
  # Entry list with both DK and FD salaries merged in for display
  entry_list_with_salaries <- reactive({
    req(values$analysis_entry_list)
    el <- values$analysis_entry_list
    if (nrow(el) == 0) return(el)
    dk <- load_dk_salaries()
    fd <- load_fd_salaries()
    if (!is.null(dk)) el <- el %>% left_join(dk %>% select(FullName, DKSalary), by=c("Name"="FullName"))
    if (!is.null(fd)) el <- el %>% left_join(fd %>% select(FullName, FDSalary), by=c("Name"="FullName"))
    el
  })
  
  #----- ENTRY LIST OUTPUT -----#
  output$entry_list_table <- DT::renderDataTable({
    req(entry_list_with_salaries())
    DT::datatable(entry_list_with_salaries(),
                  options=list(pageLength=40,scrollX=TRUE,dom='tip',columnDefs=list(list(className="dt-center",targets="_all"))),
                  rownames=FALSE, class="display nowrap compact")
  })
  
  output$entry_list_title <- renderUI({
    req(values$analysis_races_available, input$analysis_race_id)
    race_info <- values$analysis_races_available %>% filter(race_id==as.numeric(input$analysis_race_id)) %>% slice(1)
    if (nrow(race_info)==0) {
      all_races <- if(file.exists("RaceIDs.xlsx")) read_excel("RaceIDs.xlsx") else values$race_list
      race_info <- all_races %>% filter(race_id==as.numeric(input$analysis_race_id)) %>% slice(1)
    }
    race_name <- if(nrow(race_info)>0) race_info$race_name else "Entry List"
    h3(paste(race_name,"Entry List"), class="box-title")
  })
  
  output$download_entry_list_csv <- downloadHandler(
    filename=function() paste0("Entry_List_",Sys.Date(),".csv"),
    content=function(file) { req(entry_list_with_salaries()); write.csv(entry_list_with_salaries(),file,row.names=FALSE) }
  )
  
  output$download_entry_list_excel <- downloadHandler(
    filename=function() paste0("Starting_Grid_",Sys.Date(),".xlsx"),
    content=function(file) {
      req(entry_list_with_salaries())
      wb <- createWorkbook(); addWorksheet(wb,"Starting Grid")
      writeData(wb,"Starting Grid",entry_list_with_salaries(),startRow=1,startCol=1,rowNames=FALSE)
      headerStyle <- createStyle(fontSize=12,fontColour="#000000",fgFill="#FFD700",halign="center",valign="center",
                                 textDecoration="bold",border="TopBottomLeftRight",borderColour="#000000")
      addStyle(wb,"Starting Grid",headerStyle,rows=1,cols=1:ncol(entry_list_with_salaries()),gridExpand=TRUE)
      saveWorkbook(wb,file,overwrite=TRUE)
    }
  )
  
  #----- FINISH RATES -----#
  
  # Tier configuration UI
  output$tier_config_ui <- renderUI({
    req(values$analysis_entry_list)
    n <- values$num_tiers
    all_teams <- if(!is.null(values$analysis_filtered_data)) {
      sort(unique(values$analysis_filtered_data$team_name))
    } else character(0)
    
    # Render all dropdowns with full choices - filtering handled by observer below
    tier_inputs <- lapply(1:n, function(i) {
      div(class="tier-box",
          div(class="tier-label", paste("Tier", i)),
          fluidRow(
            column(5, textInput(paste0("tier_name_",i), NULL, value=paste0("Tier ",i), placeholder="Tier name")),
            column(7, selectizeInput(paste0("tier_teams_",i), NULL, choices=all_teams, multiple=TRUE,
                                     options=list(placeholder=paste("Assign teams to Tier",i))))
          )
      )
    })
    tagList(tier_inputs)
  })
  
  # Whenever any tier selection changes, update all other tiers to exclude already-selected teams
  observe({
    req(values$analysis_entry_list, values$num_tiers)
    n <- values$num_tiers
    all_teams <- if(!is.null(values$analysis_filtered_data)) {
      sort(unique(values$analysis_filtered_data$team_name))
    } else character(0)
    
    # Collect current selections for each tier
    selections <- lapply(1:n, function(i) input[[paste0("tier_teams_",i)]])
    
    # For each tier, available choices = all teams minus teams selected in OTHER tiers
    for (i in 1:n) {
      other_selected <- unlist(selections[-i])
      available <- setdiff(all_teams, other_selected)
      # Keep current selection even if it would otherwise be excluded (already in this tier)
      current <- selections[[i]]
      updateSelectizeInput(session, paste0("tier_teams_",i),
                           choices = available,
                           selected = current)
    }
  })
  
  observeEvent(input$add_tier, {
    values$num_tiers <- min(values$num_tiers + 1, 8)
  })
  observeEvent(input$remove_tier, {
    values$num_tiers <- max(values$num_tiers - 1, 1)
  })
  
  # Build finish rates data
  finish_rates_data <- reactive({
    req(values$analysis_filtered_data, input$fr_view, input$fr_time)
    data <- values$analysis_filtered_data
    
    if (input$fr_time == "2025") data <- data %>% filter(race_season == 2025)
    
    # Filter to entry list if available
    has_entry <- !is.null(values$analysis_entry_list) && nrow(values$analysis_entry_list) > 0
    if (has_entry && input$fr_view == "driver") {
      data <- data %>% filter(Full_Name %in% values$analysis_entry_list$Name)
    }
    if (has_entry && input$fr_view %in% c("car", "team")) {
      data <- data %>% filter(team_name %in% unique(values$analysis_entry_list$Team))
    }
    
    if (nrow(data) == 0) return(NULL)
    
    if (input$fr_view == "driver") {
      result <- calc_finish_rates(data, "Full_Name", "Driver")
      result <- result %>% arrange(`Avg Finish`)
    } else if (input$fr_view == "car") {
      result <- data %>%
        mutate(car_entry = paste0("#", car_number, " (", team_name, ")"))
      result <- calc_finish_rates(result, "car_entry", "Car")
      result <- result %>% arrange(`Avg Finish`)
    } else if (input$fr_view == "team") {
      result <- calc_finish_rates(data, "team_name", "Team")
      result <- result %>% arrange(`Avg Finish`)
    } else if (input$fr_view == "tier") {
      n <- values$num_tiers
      tier_assignments <- lapply(1:n, function(i) {
        teams <- input[[paste0("tier_teams_",i)]]
        name  <- input[[paste0("tier_name_",i)]]
        if (is.null(teams) || length(teams)==0) return(NULL)
        data.frame(team_name=teams, Tier=name, stringsAsFactors=FALSE)
      })
      tier_df <- bind_rows(tier_assignments)
      if (is.null(tier_df) || nrow(tier_df)==0) return(NULL)
      data_tiered <- data %>% inner_join(tier_df, by="team_name")
      result <- calc_finish_rates(data_tiered, "Tier", "Tier")
      result <- result %>% arrange(`Avg Finish`)
    }
    result
  })
  
  output$finish_rates_table <- DT::renderDataTable({
    req(finish_rates_data())
    dt <- DT::datatable(finish_rates_data(),
                        options=list(pageLength=50, scrollX=TRUE, dom='ti',
                                     columnDefs=list(list(className="dt-center",targets="_all"))),
                        rownames=FALSE, class="display nowrap compact")
    
    # Color the percentage columns green-to-yellow gradient
    pct_cols <- c("Win","Top 3","Top 5","Top 10","Top 15","Top 20","Top 25","Top 30")
    pct_cols_present <- intersect(pct_cols, names(finish_rates_data()))
    for (col in pct_cols_present) {
      dt <- dt %>% formatStyle(col,
                               backgroundColor = styleInterval(c(0,5,10,20,40,60),
                                                               c('#2d2d2d','#1a3a1a','#1a5a1a','#228b22','#90ee90','#ffd700','#ffd700')),
                               color = styleInterval(c(40), c('#ffffff','#000000')))
    }
    dt
  })
  
  output$download_finish_rates <- downloadHandler(
    filename=function() paste0("finish_rates_",input$fr_view,"_",Sys.Date(),".csv"),
    content=function(file) {
      req(finish_rates_data())
      write.csv(finish_rates_data(), file, row.names=FALSE)
    }
  )
  
  #----- DOMINATOR SECTION -----#
  dominator_data <- reactive({
    req(values$analysis_filtered_data, dominator_filtered_races())
    data <- values$analysis_filtered_data %>% filter(race_id %in% dominator_filtered_races())
    if (!"DKSP" %in% names(data) || all(is.na(data$DKSP))) {
      data <- data %>% group_by(race_id) %>%
        mutate(DKSP=fast_laps*0.45+lead_laps*0.25, DKDomRank=dense_rank(desc(DKSP))) %>% ungroup()
    }
    if (!"FDSP" %in% names(data) || all(is.na(data$FDSP))) {
      data <- data %>% group_by(race_id) %>%
        mutate(FDSP=lead_laps*0.1, FDDomRank=dense_rank(desc(FDSP))) %>% ungroup()
    }
    data
  })
  
  output$dominator_data_table <- DT::renderDataTable({
    req(dominator_data())
    display_data <- dominator_data() %>%
      filter(DKSP>0|FDSP>0) %>%
      select(Driver=Full_Name,Start=start_ps,Finish=ps,Qualifying,Team=team_name,
             `Laps Led`=lead_laps,`Fast Laps`=fast_laps,`DK Dom Pts`=DKSP,`DK Dom Rank`=DKDomRank,
             `FD Dom Pts`=FDSP,`FD Dom Rank`=FDDomRank,Season=race_season,Race=race_name,Track=track_name) %>%
      mutate(`DK Dom Pts`=round(`DK Dom Pts`,1),`FD Dom Pts`=round(`FD Dom Pts`,1)) %>%
      arrange(desc(`DK Dom Pts`))
    DT::datatable(display_data,
                  options=list(pageLength=25,scrollX=TRUE,dom='tip',columnDefs=list(list(className="dt-center",targets="_all"))),
                  rownames=FALSE,filter="top",class="display nowrap compact")
  })
  
  output$download_dominator_csv <- downloadHandler(
    filename=function() paste0("dominator_data_",Sys.Date(),".csv"),
    content=function(file) {
      req(dominator_data())
      export_data <- dominator_data() %>% filter(DKSP>0|FDSP>0) %>%
        select(Driver=Full_Name,Start=start_ps,Finish=ps,Qualifying,Team=team_name,
               `Laps Led`=lead_laps,`Fast Laps`=fast_laps,`DK Dom Pts`=DKSP,`DK Dom Rank`=DKDomRank,
               `FD Dom Pts`=FDSP,`FD Dom Rank`=FDDomRank,Season=race_season,Race=race_name,Track=track_name) %>%
        mutate(`DK Dom Pts`=round(`DK Dom Pts`,1),`FD Dom Pts`=round(`FD Dom Pts`,1))
      write.csv(export_data,file,row.names=FALSE)
    }
  )
  
  output$download_dominator_profile <- downloadHandler(
    filename=function() paste0("dominator_profile_",Sys.Date(),".xlsx"),
    content=function(file) {
      req(dominator_data(), values$analysis_races_available)
      fd_laps <- dominator_data() %>% filter(!is.na(ps),!is.na(FDLP)) %>%
        group_by(ps) %>% summarize(Pt=round(mean(FDLP,na.rm=TRUE),1),.groups='drop') %>%
        arrange(ps) %>% rename(PS=ps)
      race_weights <- values$analysis_races_available %>%
        filter(race_id %in% dominator_filtered_races()) %>%
        select(RaceID=race_id,RaceName=race_name,Season=race_season,Track=track_name) %>%
        mutate(Weight=round(1/n(),4)) %>% arrange(Season,Track)
      race_profiles <- dominator_data() %>%
        select(RaceID=race_id,StartPos=start_ps,FinPos=ps,LeadLaps=lead_laps,FastLaps=fast_laps,
               DKDomPoints=DKSP,FDDomPoints=FDSP,TrackName=track_name,Driver=Full_Name,Team=team_name) %>%
        mutate(DKDomPoints=round(DKDomPoints,1),FDDomPoints=round(FDDomPoints,1)) %>%
        filter(DKDomPoints>0) %>% arrange(desc(FDDomPoints))
      wb <- createWorkbook()
      addWorksheet(wb,"FDLaps"); writeData(wb,"FDLaps",fd_laps)
      addWorksheet(wb,"Race_Weights"); writeData(wb,"Race_Weights",race_weights)
      addWorksheet(wb,"Race_Profiles"); writeData(wb,"Race_Profiles",race_profiles)
      saveWorkbook(wb,file,overwrite=TRUE)
    }
  )
  
  # ---- CREATE INPUT FILE ----#
  output$download_input_file <- downloadHandler(
    filename=function() paste0("NASCAR_Sim_Input_",Sys.Date(),".xlsx"),
    content=function(file) {
      req(dominator_data(), values$analysis_races_available, values$analysis_entry_list)
      
      # --- Build Driver sheet ---
      # Start with entry list
      el <- entry_list_with_salaries()
      entry_drivers <- values$analysis_entry_list$Name
      if (length(entry_drivers) > 0)
        fr_data <- fr_data %>% filter(Full_Name %in% entry_drivers)
      
      fr_sheet <- fr_data %>%
        group_by(Full_Name) %>%
        summarize(
          W   = round(mean(ps == 1,  na.rm=TRUE) * 100, 1),
          T3  = round(mean(ps <= 3,  na.rm=TRUE) * 100, 1),
          T5  = round(mean(ps <= 5,  na.rm=TRUE) * 100, 1),
          T10 = round(mean(ps <= 10, na.rm=TRUE) * 100, 1),
          T15 = round(mean(ps <= 15, na.rm=TRUE) * 100, 1),
          T20 = round(mean(ps <= 20, na.rm=TRUE) * 100, 1),
          T25 = round(mean(ps <= 25, na.rm=TRUE) * 100, 1),
          T30 = round(mean(ps <= 30, na.rm=TRUE) * 100, 1),
          .groups = 'drop'
        )
      
      # Build the Driver sheet with exact column order:
      # FDName DKName Name DKID FDID car team DKSalary FDSalary Starting DKOP FDOP W T3 T5 T10 T15 T20 T25 T30 DKMax FDMax
      driver_sheet <- el %>%
        select(Name, car=Car, team=Team, Starting=Start) %>%
        mutate(
          FDName   = NA_character_,
          DKName   = NA_character_,
          DKID     = NA_character_,
          FDID     = NA_character_,
          DKSalary = NA_real_,
          FDSalary = NA_real_,
          DKOP     = NA_real_,
          FDOP     = NA_real_,
          DKMax    = NA_real_,
          FDMax    = NA_real_
        )
      
      # Merge DK and FD salaries from working directory files
      dk <- load_dk_salaries()
      fd <- load_fd_salaries()
      if (!is.null(dk)) {
        driver_sheet <- driver_sheet %>%
          left_join(dk, by=c("Name"="FullName")) %>%
          mutate(
            DKName   = coalesce(DKName,   DKName.y),
            DKID     = coalesce(DKID,     DKID.y),
            DKSalary = coalesce(DKSalary, DKSalary.y)
          ) %>% select(-any_of(c("DKName.y","DKID.y","DKSalary.y")))
      }
      if (!is.null(fd)) {
        driver_sheet <- driver_sheet %>%
          left_join(fd, by=c("Name"="FullName")) %>%
          mutate(
            FDName   = coalesce(FDName,   FDName.y),
            FDID     = coalesce(FDID,     FDID.y),
            FDSalary = coalesce(FDSalary, FDSalary.y)
          ) %>% select(-any_of(c("FDName.y","FDID.y","FDSalary.y")))
      }
      
      # Fall back: if DKName/FDName still blank after merge, use Name
      driver_sheet <- driver_sheet %>%
        mutate(
          DKName = if_else(is.na(DKName), Name, DKName),
          FDName = if_else(is.na(FDName), Name, FDName)
        )
      
      
      # Merge finish rates
      driver_sheet <- driver_sheet %>%
        left_join(fr_sheet, by=c("Name"="Full_Name"))
      
      # Final column order
      driver_sheet <- driver_sheet %>%
        select(FDName, DKName, Name, DKID, FDID, car, team,
               DKSalary, FDSalary, Starting, DKOP, FDOP,
               W, T3, T5, T10, T15, T20, T25, T30, DKMax, FDMax)
      
      # --- Sheet 2: Race_Profiles ---
      race_profiles <- dominator_data() %>%
        select(RaceID=race_id, StartPos=start_ps, FinPos=ps, LeadLaps=lead_laps, FastLaps=fast_laps,
               DKDomPoints=DKSP, FDDomPoints=FDSP, TrackName=track_name, Driver=Full_Name, Team=team_name) %>%
        mutate(DKDomPoints=round(DKDomPoints,1), FDDomPoints=round(FDDomPoints,1)) %>%
        filter(DKDomPoints > 0) %>% arrange(desc(DKDomPoints))
      
      # --- Sheet 3: Race_Weights ---
      race_weights <- values$analysis_races_available %>%
        filter(race_id %in% dominator_filtered_races()) %>%
        select(RaceID=race_id, RaceName=race_name, Season=race_season, Track=track_name) %>%
        mutate(Weight=round(1/n(),4)) %>% arrange(Season, Track)
      
      # --- Sheet 4: FDLaps ---
      fd_laps <- dominator_data() %>%
        filter(!is.na(ps), !is.na(FDLP)) %>%
        group_by(ps) %>% summarize(Pt=round(mean(FDLP,na.rm=TRUE),1),.groups='drop') %>%
        arrange(ps) %>% rename(PS=ps)
      
      # --- Write workbook ---
      wb <- createWorkbook()
      gold_header <- createStyle(fontSize=11, fontColour="#000000", fgFill="#FFD700",
                                 halign="center", valign="center", textDecoration="bold",
                                 border="TopBottomLeftRight", borderColour="#000000")
      
      sheets <- list(Driver=driver_sheet, Race_Profiles=race_profiles,
                     Race_Weights=race_weights, FDLaps=fd_laps)
      
      for (sname in names(sheets)) {
        addWorksheet(wb, sname)
        writeData(wb, sname, sheets[[sname]])
        addStyle(wb, sname, gold_header, rows=1, cols=1:ncol(sheets[[sname]]), gridExpand=TRUE)
        setColWidths(wb, sname, cols=1:ncol(sheets[[sname]]), widths="auto")
      }
      
      saveWorkbook(wb, file, overwrite=TRUE)
    }
  )
  
  #----- DOMINATOR VISUALIZATIONS -----#
  output$dominator_plot <- renderPlotly({
    req(dominator_data(), input$dom_visual_type, input$dom_platform)
    plot_data <- dominator_data()
    platform <- input$dom_platform
    dom_pts_col <- if(platform=="DK") "DKSP" else "FDSP"
    dom_rank_col <- if(platform=="DK") "DKDomRank" else "FDDomRank"
    platform_name <- if(platform=="DK") "DraftKings" else "FanDuel"
    
    dark_theme <- theme_minimal() + theme(
      plot.title=element_text(size=18,face="bold",color="#FFD700"),
      axis.title=element_text(size=16,color="#ffffff"), axis.text=element_text(size=14,color="#ffffff"),
      panel.background=element_rect(fill="#2d2d2d"), plot.background=element_rect(fill="#2d2d2d"),
      panel.grid.major=element_line(color="#404040"), panel.grid.minor=element_line(color="#333333"))
    
    # dark_layout inlined below
    
    if (input$dom_visual_type == "score_dist") {
      viz_data <- plot_data %>% filter(!!sym(dom_rank_col)<=10, !!sym(dom_pts_col)>0)
      p <- ggplot(viz_data,aes(x=factor(!!sym(dom_rank_col)),y=!!sym(dom_pts_col))) +
        geom_boxplot(aes(text=sprintf("Dom Rank: %d
Dom Pts: %.1f
Driver: %s
Track: %s",
                                      !!sym(dom_rank_col),!!sym(dom_pts_col),Full_Name,track_name)),fill="forestgreen",alpha=0.7) +
        labs(title=paste(platform_name,"Dom Points by Dom Rank (Top 10)"),x="Dom Rank",y="Dom Points") +
        coord_flip() + scale_x_discrete(limits=factor(10:1)) + dark_theme
      ggplotly(p,tooltip="text",height=700) %>% layout(paper_bgcolor="#2d2d2d", plot_bgcolor="#2d2d2d", font=list(color="#ffffff"), xaxis=list(gridcolor="#404040", zerolinecolor="#666666"), yaxis=list(gridcolor="#404040", zerolinecolor="#666666"))
      
    } else if (input$dom_visual_type == "rank_finish") {
      viz_data <- plot_data %>% filter(!!sym(dom_rank_col)<=10, !!sym(dom_pts_col)>0)
      p <- ggplot(viz_data,aes(x=factor(!!sym(dom_rank_col)),y=ps)) +
        geom_boxplot(aes(text=sprintf("Dom Rank: %d
Finish: %d
Driver: %s
Track: %s",
                                      !!sym(dom_rank_col),ps,Full_Name,track_name)),fill="darkgreen",alpha=0.7) +
        geom_smooth(aes(x=as.numeric(!!sym(dom_rank_col)),y=ps),method="loess",se=FALSE,color="#FFD700",linewidth=1.5) +
        labs(title=paste("Where Have Top",platform_name,"Dominators Finished"),x="Dom Rank",y="Finish Position") +
        scale_x_discrete(limits=factor(1:10)) + scale_y_continuous(breaks=seq(0,40,5)) + dark_theme
      ggplotly(p,tooltip="text",height=700) %>% layout(paper_bgcolor="#2d2d2d", plot_bgcolor="#2d2d2d", font=list(color="#ffffff"), xaxis=list(gridcolor="#404040", zerolinecolor="#666666"), yaxis=list(gridcolor="#404040", zerolinecolor="#666666"))
      
    } else if (input$dom_visual_type %in% c("pts_by_finish","dom_pts_start","dom_rank_start","laps_led","laps_led_start","fast_laps","fast_laps_start")) {
      # Generic horizontal boxplot handler
      cfg <- list(
        pts_by_finish  = list(x="ps",       y=dom_pts_col,  fill="darkgreen",  xt="Finish Position",    yt="Dom Points",    ti=paste(platform_name,"Dom Pts by Finish")),
        dom_pts_start  = list(x="start_ps", y=dom_pts_col,  fill="darkgreen",  xt="Starting Position",  yt="Dom Points",    ti=paste(platform_name,"Dom Pts by Start")),
        dom_rank_start = list(x="start_ps", y=dom_rank_col, fill="forestgreen",xt="Starting Position",  yt="Dom Rank",      ti=paste(platform_name,"Dom Rank by Start")),
        laps_led       = list(x="ps",       y="lead_laps",  fill="lightgreen", xt="Finish Position",    yt="Laps Led",      ti="Laps Led by Finish"),
        laps_led_start = list(x="start_ps", y="lead_laps",  fill="lightgreen", xt="Starting Position",  yt="Laps Led",      ti="Laps Led by Start"),
        fast_laps      = list(x="ps",       y="fast_laps",  fill="green",      xt="Finish Position",    yt="Fast Laps",     ti="Fast Laps by Finish"),
        fast_laps_start= list(x="start_ps", y="fast_laps",  fill="green",      xt="Starting Position",  yt="Fast Laps",     ti="Fast Laps by Start")
      )
      cc <- cfg[[input$dom_visual_type]]
      filt_col <- if(cc$x=="ps") "ps" else "start_ps"
      viz_data <- plot_data %>% filter(!!sym(filt_col)<=40, !is.na(!!sym(filt_col)), !is.na(!!sym(cc$y)))
      p <- ggplot(viz_data,aes(x=factor(!!sym(cc$x)),y=!!sym(cc$y))) +
        geom_boxplot(aes(text=sprintf("%s: %%d
%s: %s
Driver: %%s
Track: %%s" |>
                                        sprintf(cc$xt,cc$yt,if(grepl("laps|pts|points",tolower(cc$yt)))"%.1f" else "%d") |>
                                        identity(), !!sym(cc$x), !!sym(cc$y), Full_Name, track_name)), fill=cc$fill, alpha=0.7) +
        labs(title=cc$ti, x=cc$xt, y=cc$yt) +
        coord_flip() + scale_x_discrete(limits=factor(40:1)) + dark_theme
      ggplotly(p,tooltip="text",height=900) %>% layout(paper_bgcolor="#2d2d2d", plot_bgcolor="#2d2d2d", font=list(color="#ffffff"), xaxis=list(gridcolor="#404040", zerolinecolor="#666666"), yaxis=list(gridcolor="#404040", zerolinecolor="#666666"))
      
    } else if (input$dom_visual_type %in% c("driver_boxplot","team_boxplot","entry_boxplot")) {
      req(values$analysis_entry_list)
      if (input$dom_visual_type == "driver_boxplot") {
        entry_drivers <- values$analysis_entry_list$Name
        grp_data <- plot_data %>% filter(Full_Name %in% entry_drivers) %>%
          group_by(Full_Name) %>% summarize(avg=mean(.data[[dom_pts_col]],na.rm=TRUE),.groups='drop') %>%
          filter(avg>0) %>% arrange(avg)
        grp_order <- grp_data$Full_Name
        viz_data <- plot_data %>% filter(Full_Name %in% grp_order) %>% mutate(Grp=factor(Full_Name,levels=grp_order))
        plot_ly(data=viz_data,type="box",y=~Grp,x=~get(dom_pts_col),orientation="h",
                marker=list(color="#FFD700",opacity=0.6),line=list(color="#DAA520"),
                fillcolor="rgba(255,215,0,0.3)",showlegend=FALSE) %>%
          layout(title=list(text=paste(platform_name,"Dom Points by Driver"),font=list(size=18,color="#FFD700")),
                 xaxis=list(title="Dom Points",color="#ffffff",gridcolor="#404040"),
                 yaxis=list(title="",color="#ffffff",gridcolor="#404040",categoryorder="array",categoryarray=grp_order),
                 paper_bgcolor="#2d2d2d",plot_bgcolor="#2d2d2d",font=list(color="#ffffff"),height=900,margin=list(l=150))
        
      } else if (input$dom_visual_type == "team_boxplot") {
        entry_teams <- unique(values$analysis_entry_list$Team)
        grp_data <- plot_data %>% filter(team_name %in% entry_teams) %>%
          group_by(team_name) %>% summarize(med=median(.data[[dom_pts_col]],na.rm=TRUE),.groups='drop') %>%
          arrange(med)
        grp_order <- grp_data$team_name
        viz_data <- plot_data %>% filter(team_name %in% grp_order) %>% mutate(Grp=factor(team_name,levels=grp_order))
        plot_ly(data=viz_data,type="box",y=~Grp,x=~get(dom_pts_col),orientation="h",
                marker=list(color="#FFD700",opacity=0.6),line=list(color="#DAA520"),
                fillcolor="rgba(255,215,0,0.3)",showlegend=FALSE) %>%
          layout(title=list(text=paste("Team",platform_name,"Dom Points"),font=list(size=18,color="#FFD700")),
                 xaxis=list(title="Dom Points",color="#ffffff",gridcolor="#404040"),
                 yaxis=list(title="",color="#ffffff",categoryorder="array",categoryarray=grp_order),
                 paper_bgcolor="#2d2d2d",plot_bgcolor="#2d2d2d",font=list(color="#ffffff"),height=900,margin=list(l=150))
        
      } else {
        entry_labels <- values$analysis_entry_list %>%
          mutate(Entry_Label=paste0(Team," #",Car," (",Name,")")) %>% select(Name,Entry_Label)
        grp_data <- plot_data %>% inner_join(entry_labels,by=c("Full_Name"="Name")) %>%
          group_by(Entry_Label) %>% summarize(avg=mean(.data[[dom_pts_col]],na.rm=TRUE),.groups='drop') %>%
          filter(avg>0) %>% arrange(avg)
        grp_order <- grp_data$Entry_Label
        viz_data <- plot_data %>% inner_join(entry_labels,by=c("Full_Name"="Name")) %>%
          filter(Entry_Label %in% grp_order) %>% mutate(Grp=factor(Entry_Label,levels=grp_order))
        plot_ly(data=viz_data,type="box",y=~Grp,x=~get(dom_pts_col),orientation="h",
                marker=list(color="#FFD700",opacity=0.6),line=list(color="#DAA520"),
                fillcolor="rgba(255,215,0,0.3)",showlegend=FALSE) %>%
          layout(title=list(text=paste(platform_name,"Dom Points by Entry"),font=list(size=18,color="#FFD700")),
                 xaxis=list(title="Dom Points",color="#ffffff",gridcolor="#404040"),
                 yaxis=list(title="",color="#ffffff",categoryorder="array",categoryarray=grp_order),
                 paper_bgcolor="#2d2d2d",plot_bgcolor="#2d2d2d",font=list(color="#ffffff"),height=900,margin=list(l=200))
      }
    }
  })
  
  #----- PLACE DIFFERENTIAL SECTION -----#
  pd_data <- reactive({
    req(values$analysis_filtered_data, values$pd_race_ids)
    values$analysis_filtered_data %>%
      filter(race_id %in% values$pd_race_ids, !is.na(start_ps), !is.na(ps)) %>%
      mutate(PD=start_ps-ps)
  })
  
  output$pd_data_table <- DT::renderDataTable({
    req(pd_data())
    display_data <- pd_data() %>%
      select(Driver=Full_Name,Start=start_ps,Finish=ps,PD,Qualifying,Team=team_name,ARP,
             Season=race_season,Race=race_name,Track=track_name) %>%
      mutate(ARP=round(ARP,1)) %>% arrange(desc(PD))
    DT::datatable(display_data,
                  options=list(pageLength=25,scrollX=TRUE,dom='tip',columnDefs=list(list(className="dt-center",targets="_all"))),
                  rownames=FALSE,filter="top",class="display nowrap compact")
  })
  
  output$download_pd_csv <- downloadHandler(
    filename=function() paste0("place_differential_",Sys.Date(),".csv"),
    content=function(file) {
      req(pd_data())
      write.csv(pd_data() %>% select(Driver=Full_Name,Start=start_ps,Finish=ps,PD,Qualifying,
                                     Team=team_name,ARP,Season=race_season,Race=race_name,Track=track_name) %>% mutate(ARP=round(ARP,1)), file, row.names=FALSE)
    }
  )
  
  output$pd_plot <- renderPlotly({
    req(pd_data(), input$pd_visual_type)
    plot_data <- pd_data()
    # dark_layout inlined below
    dark_theme <- theme_minimal()+theme(plot.title=element_text(size=20,face="bold",color="#FFD700"),
                                        plot.subtitle=element_text(size=14,color="#ffffff"),axis.title=element_text(size=16,color="#ffffff"),
                                        axis.text=element_text(size=14,color="#ffffff"),panel.background=element_rect(fill="#2d2d2d"),
                                        plot.background=element_rect(fill="#2d2d2d"),panel.grid.major=element_line(color="#404040"),
                                        panel.grid.minor=element_line(color="#333333"))
    
    if (input$pd_visual_type=="scatter") {
      viz_data <- plot_data %>% filter(start_ps<=40,ps<=40)
      p <- ggplot(viz_data,aes(x=start_ps,y=ps,size=abs(PD),color=PD,
                               text=sprintf("Driver: %s
Start: %d
Finish: %d
PD: %d
Track: %s",Full_Name,start_ps,ps,PD,track_name)))+
        geom_point(alpha=0.6)+geom_abline(linetype="dashed",color="#FFD700",linewidth=1.2)+
        scale_color_gradient2(low="#DC143C",mid="#cccccc",high="#32CD32",midpoint=0,name="PD")+
        scale_size_continuous(range=c(2,12))+
        scale_x_continuous(limits=c(0,40),breaks=seq(0,40,5))+scale_y_continuous(limits=c(0,40),breaks=seq(0,40,5))+
        labs(title="Starting vs Finishing Position",x="Starting Position",y="Finishing Position")+dark_theme
      ggplotly(p,tooltip="text",height=700) %>% layout(paper_bgcolor="#2d2d2d", plot_bgcolor="#2d2d2d", font=list(color="#ffffff"), xaxis=list(gridcolor="#404040", zerolinecolor="#666666"), yaxis=list(gridcolor="#404040", zerolinecolor="#666666"))
    } else if (input$pd_visual_type=="histogram") {
      p <- ggplot(plot_data,aes(x=PD))+geom_histogram(binwidth=1,fill="#FFD700",color="#000000",alpha=0.8)+
        geom_vline(xintercept=0,linetype="dashed",color="#FF0000",linewidth=1.5)+
        labs(title="Position Change Distribution",x="Place Differential",y="Count")+dark_theme
      ggplotly(p,height=700) %>% layout(paper_bgcolor="#2d2d2d", plot_bgcolor="#2d2d2d", font=list(color="#ffffff"), xaxis=list(gridcolor="#404040", zerolinecolor="#666666"), yaxis=list(gridcolor="#404040", zerolinecolor="#666666"))
    } else if (input$pd_visual_type=="boxplot_start") {
      viz_data <- plot_data %>% filter(start_ps<=40)
      p <- ggplot(viz_data,aes(x=factor(start_ps),y=PD,
                               text=sprintf("Start: %d
PD: %d
Driver: %s
Track: %s",start_ps,PD,Full_Name,track_name)))+
        geom_boxplot(fill="skyblue",alpha=0.7)+geom_hline(yintercept=0,linetype="dashed",color="#FFD700",linewidth=1.2)+
        labs(title="Place Differential by Starting Position",x="Starting Position",y="Place Differential")+
        coord_flip()+scale_x_discrete(limits=factor(40:1))+dark_theme
      ggplotly(p,tooltip="text",height=900) %>% layout(paper_bgcolor="#2d2d2d", plot_bgcolor="#2d2d2d", font=list(color="#ffffff"), xaxis=list(gridcolor="#404040", zerolinecolor="#666666"), yaxis=list(gridcolor="#404040", zerolinecolor="#666666"))
    } else {
      viz_data <- plot_data %>% filter(ps<=40)
      p <- ggplot(viz_data,aes(x=factor(ps),y=PD,
                               text=sprintf("Finish: %d
PD: %d
Driver: %s
Track: %s",ps,PD,Full_Name,track_name)))+
        geom_boxplot(fill="lightcoral",alpha=0.7)+geom_hline(yintercept=0,linetype="dashed",color="#FFD700",linewidth=1.2)+
        labs(title="Place Differential by Finishing Position",x="Finishing Position",y="Place Differential")+
        coord_flip()+scale_x_discrete(limits=factor(40:1))+dark_theme
      ggplotly(p,tooltip="text",height=900) %>% layout(paper_bgcolor="#2d2d2d", plot_bgcolor="#2d2d2d", font=list(color="#ffffff"), xaxis=list(gridcolor="#404040", zerolinecolor="#666666"), yaxis=list(gridcolor="#404040", zerolinecolor="#666666"))
    }
  })
  
  #----- PERFORMANCE SECTION -----#
  performance_data <- reactive({
    req(values$analysis_filtered_data, values$performance_race_ids, input$perf_time_filter)
    data <- values$analysis_filtered_data %>% filter(race_id %in% values$performance_race_ids)
    if (input$perf_time_filter=="2025") data <- data %>% filter(race_season==2025)
    data
  })
  
  output$performance_data_table <- DT::renderDataTable({
    req(performance_data())
    default_columns <- c("Full_Name","start_ps","ps","ARP","SpdRk","fl","ll","DKSP","FDSP",
                         "DKDomRank","FDDomRank","DKPoints","FDPoints","car_number","team_name",
                         "race_season","track_name","finishing_status","LapsDown","Qualifying")
    valid_columns <- intersect(default_columns, names(performance_data()))
    display_data <- performance_data() %>% select(all_of(valid_columns)) %>%
      mutate(across(any_of(c("ARP","DKSP","FDSP","DKPoints","FDPoints")), ~round(.,1))) %>%
      rename_with(~case_when(
        .x=="Full_Name"~"Driver",.x=="start_ps"~"Start",.x=="ps"~"Finish",
        .x=="fl"~"FL",.x=="ll"~"LL",.x=="DKSP"~"DK Dom Pts",.x=="FDSP"~"FD Dom Pts",
        .x=="DKPoints"~"DK Pts",.x=="FDPoints"~"FD Pts",.x=="DKDomRank"~"DK Dom Rank",
        .x=="FDDomRank"~"FD Dom Rank",.x=="car_number"~"Car",.x=="team_name"~"Team",
        .x=="race_season"~"Season",.x=="track_name"~"Track",.x=="finishing_status"~"Status",
        .x=="LapsDown"~"Laps Down",TRUE~.x))
    DT::datatable(display_data,
                  options=list(scrollX=TRUE,pageLength=25,dom='rtip',  # no 'f' = no search box
                               columnDefs=list(list(className="dt-center",targets="_all"))),
                  filter='top',rownames=FALSE,class="display nowrap compact")
  })
  
  output$download_performance_csv <- downloadHandler(
    filename=function() paste0("performance_",Sys.Date(),".csv"),
    content=function(file) { req(performance_data()); write.csv(performance_data(),file,row.names=FALSE) }
  )
  
  output$performance_plot <- renderPlotly({
    req(values$analysis_filtered_data, values$performance_race_ids, input$perf_visual_type, input$perf_visual_time)
    viz_data <- values$analysis_filtered_data %>% filter(race_id %in% values$performance_race_ids)
    if (input$perf_visual_time=="2025") viz_data <- viz_data %>% filter(race_season==2025)
    time_label <- ifelse(input$perf_visual_time=="2025","2025","Full History")
    req(values$analysis_entry_list)
    
    make_perf_boxplot <- function(data, grp_col, val_col, order_dir="desc", title_txt, x_label, margin_l=150) {
      grp_data <- data %>% group_by(!!sym(grp_col)) %>%
        summarize(metric=if(order_dir=="desc") mean(!!sym(val_col),na.rm=TRUE) else mean(!!sym(val_col),na.rm=TRUE),
                  n=n(),.groups='drop') %>% filter(n>0)
      if (order_dir=="desc") grp_data <- grp_data %>% arrange(desc(metric)) else grp_data <- grp_data %>% arrange(metric)
      grp_order <- grp_data[[grp_col]]
      viz <- data %>% filter(!!sym(grp_col) %in% grp_order, !is.na(!!sym(val_col))) %>%
        mutate(Grp=factor(!!sym(grp_col),levels=grp_order))
      plot_ly(data=viz,type="box",y=~Grp,x=~get(val_col),orientation="h",
              marker=list(color="#FFD700",opacity=0.6),line=list(color="#DAA520"),
              fillcolor="rgba(255,215,0,0.3)",showlegend=FALSE) %>%
        layout(title=list(text=paste(title_txt,"-",time_label),font=list(size=18,color="#FFD700")),
               xaxis=list(title=x_label,color="#ffffff",gridcolor="#404040"),
               yaxis=list(title="",color="#ffffff",categoryorder="array",categoryarray=grp_order),
               paper_bgcolor="#2d2d2d",plot_bgcolor="#2d2d2d",font=list(color="#ffffff"),
               height=900,margin=list(l=margin_l))
    }
    
    entry_drivers <- values$analysis_entry_list$Name
    entry_teams <- unique(values$analysis_entry_list$Team)
    
    if (input$perf_visual_type=="driver_speed") {
      make_perf_boxplot(viz_data %>% filter(Full_Name %in% entry_drivers,!is.na(SpdRk)),"Full_Name","SpdRk","desc","Speed Rank by Driver","Speed Rank")
    } else if (input$perf_visual_type=="team_speed") {
      make_perf_boxplot(viz_data %>% filter(team_name %in% entry_teams,!is.na(SpdRk)),"team_name","SpdRk","desc","Team Speed Rank","Speed Rank")
    } else if (input$perf_visual_type=="driver_finish") {
      make_perf_boxplot(viz_data %>% filter(Full_Name %in% entry_drivers,!is.na(ps)),"Full_Name","ps","desc","Finish Distribution by Driver","Finish Position")
    } else if (input$perf_visual_type=="team_finish") {
      make_perf_boxplot(viz_data %>% filter(team_name %in% entry_teams,!is.na(ps)),"team_name","ps","desc","Team Finish Distribution","Finish Position")
    } else if (input$perf_visual_type=="driver_arp") {
      make_perf_boxplot(viz_data %>% filter(Full_Name %in% entry_drivers,!is.na(ARP)),"Full_Name","ARP","desc","ARP by Driver","Avg Running Position")
    } else {
      make_perf_boxplot(viz_data %>% filter(team_name %in% entry_teams,!is.na(ARP)),"team_name","ARP","desc","Team ARP Distribution","Avg Running Position")
    }
  })
  
  #----- FANTASY SCORING SECTION -----#
  fantasy_data <- reactive({
    req(values$analysis_filtered_data, dominator_filtered_races())
    values$analysis_filtered_data %>% filter(race_id %in% dominator_filtered_races())
  })
  
  output$fantasy_data_table <- DT::renderDataTable({
    req(fantasy_data(), input$fs_platform)
    if (input$fs_platform=="DK") {
      display_data <- fantasy_data() %>% filter(DKRank<=25) %>%
        select(Driver=Full_Name,Rank=DKRank,`Total Pts`=DKPoints,`Finish Pts`=DKFP,
               `PD Pts`=DKPD,`Dom Pts`=DKSP,Finish=ps,Start=start_ps,
               `Laps Led`=lead_laps,`Fast Laps`=fast_laps,Race=race_name,Track=track_name,Season=race_season) %>%
        mutate(across(c(`Total Pts`,`Finish Pts`,`PD Pts`,`Dom Pts`),~round(.,1))) %>%
        arrange(desc(`Total Pts`))
    } else {
      display_data <- fantasy_data() %>% filter(FDRank<=25) %>%
        select(Driver=Full_Name,Rank=FDRank,`Total Pts`=FDPoints,`Finish Pts`=FDFP,
               `PD Pts`=FDPD,`Dom Pts`=FDSP,`Lap Pts`=FDLP,Finish=ps,Start=start_ps,
               `Laps Led`=lead_laps,Race=race_name,Track=track_name,Season=race_season) %>%
        mutate(across(c(`Total Pts`,`Finish Pts`,`PD Pts`,`Dom Pts`,`Lap Pts`),~round(.,1))) %>%
        arrange(desc(`Total Pts`))
    }
    DT::datatable(display_data,
                  options=list(pageLength=25,scrollX=TRUE,dom='tip',  # no 'f' = no search box
                               columnDefs=list(list(className="dt-center",targets="_all"))),
                  rownames=FALSE,class="display nowrap compact")
  })
  
  output$download_fantasy_csv <- downloadHandler(
    filename=function() paste0("fantasy_",input$fs_platform,"_",Sys.Date(),".csv"),
    content=function(file) { req(fantasy_data()); write.csv(fantasy_data(),file,row.names=FALSE) }
  )
  
  output$fantasy_plot <- renderPlotly({
    req(fantasy_data(), input$fs_visual_type, input$fs_visual_platform)
    plot_data <- fantasy_data()
    platform <- input$fs_visual_platform
    points_col <- if(platform=="DK") "DKPoints" else "FDPoints"
    rank_col   <- if(platform=="DK") "DKRank"   else "FDRank"
    platform_name <- if(platform=="DK") "DraftKings" else "FanDuel"
    
    # dark_layout inlined below
    dark_theme <- theme_minimal()+theme(
      plot.title=element_text(size=18,face="bold",color="#FFD700"),
      axis.title=element_text(size=16,color="#ffffff"),axis.text=element_text(size=14,color="#ffffff"),
      panel.background=element_rect(fill="#2d2d2d"),plot.background=element_rect(fill="#2d2d2d"),
      panel.grid.major=element_line(color="#404040"),panel.grid.minor=element_line(color="#333333"),
      legend.background=element_rect(fill="#2d2d2d"),legend.key=element_rect(fill="#2d2d2d"),
      legend.text=element_text(color="#ffffff"),legend.title=element_text(color="#FFD700"))
    
    if (input$fs_visual_type=="score_dist") {
      viz_data <- plot_data %>% filter(!!sym(rank_col)<=15)
      p <- ggplot(viz_data,aes(x=factor(!!sym(rank_col)),y=!!sym(points_col)))+
        geom_boxplot(aes(text=sprintf("Rank: %d
Pts: %.1f
Driver: %s
Track: %s",!!sym(rank_col),!!sym(points_col),Full_Name,track_name)),fill="dodgerblue",alpha=0.7)+
        geom_smooth(aes(x=as.numeric(!!sym(rank_col)),y=!!sym(points_col),group=1),method="loess",se=FALSE,color="#FFD700",linewidth=1.5)+
        labs(title=paste(platform_name,"Points by Fantasy Rank"),x="Rank",y="Points")+
        scale_x_discrete(limits=factor(1:15))+dark_theme
      ggplotly(p,tooltip="text",height=700) %>% layout(paper_bgcolor="#2d2d2d", plot_bgcolor="#2d2d2d", font=list(color="#ffffff"), xaxis=list(gridcolor="#404040", zerolinecolor="#666666"), yaxis=list(gridcolor="#404040", zerolinecolor="#666666"))
      
    } else if (input$fs_visual_type %in% c("components","components_start","components_finish")) {
      make_comp <- function(d) {
        if(platform=="DK") d %>% mutate(Finish_Pct=round(DKFP/DKPoints*100,1),PD_Pct=round(DKPD/DKPoints*100,1),Dom_Pct=round(DKSP/DKPoints*100,1))
        else d %>% mutate(Finish_Pct=round(FDFP/(FDPoints-FDLP)*100,1),PD_Pct=round(FDPD/(FDPoints-FDLP)*100,1),Dom_Pct=round(FDSP/(FDPoints-FDLP)*100,1))
      }
      if (input$fs_visual_type=="components") {
        grp_col <- rank_col; filt <- plot_data %>% filter(!!sym(rank_col)<=15)
      } else if (input$fs_visual_type=="components_start") {
        grp_col <- "start_ps"; filt <- plot_data %>% filter(start_ps<=20,!is.na(start_ps))
      } else {
        grp_col <- "ps"; filt <- plot_data %>% filter(ps<=20,!is.na(ps))
      }
      comp_data <- make_comp(filt) %>% group_by(!!sym(grp_col)) %>%
        summarize(FP=mean(Finish_Pct,na.rm=TRUE),PD=mean(PD_Pct,na.rm=TRUE),Dom=mean(Dom_Pct,na.rm=TRUE),.groups='drop') %>%
        pivot_longer(cols=c(FP,PD,Dom),names_to="Type",values_to="Pct") %>%
        mutate(Type=case_when(Type=="FP"~"Finish Position",Type=="PD"~"Place Differential",TRUE~"Dominator Points"))
      p <- ggplot(comp_data,aes(x=factor(!!sym(grp_col)),y=Pct,fill=Type))+
        geom_bar(stat="identity",position="stack")+
        geom_text(aes(label=sprintf("%.0f%%",Pct)),position=position_stack(vjust=0.5),color="white",fontface="bold",size=3)+
        scale_fill_manual(values=c("Finish Position"="#3406cc","Place Differential"="#33cc33","Dominator Points"="#ff9900"))+
        labs(title=paste(platform_name,"Scoring Components"),x=grp_col,y="%",fill="Type")+dark_theme
      ggplotly(p,tooltip=c("x","y","fill"),height=700) %>% layout(paper_bgcolor="#2d2d2d", plot_bgcolor="#2d2d2d", font=list(color="#ffffff"), xaxis=list(gridcolor="#404040", zerolinecolor="#666666"), yaxis=list(gridcolor="#404040", zerolinecolor="#666666"))
      
    } else {
      filt_col <- if(input$fs_visual_type=="score_by_start") "start_ps" else "ps"
      fill_col <- if(input$fs_visual_type=="score_by_start") "purple" else "orange"
      viz_data <- plot_data %>% filter(!!sym(filt_col)<=40,!is.na(!!sym(filt_col)),!is.na(!!sym(points_col)))
      p <- ggplot(viz_data,aes(x=factor(!!sym(filt_col)),y=!!sym(points_col)))+
        geom_boxplot(aes(text=sprintf("%s: %%d
Pts: %%.1f
Driver: %%s
Track: %%s" |>
                                        sprintf(if(filt_col=="start_ps")"Start" else "Finish"), !!sym(filt_col),!!sym(points_col),Full_Name,track_name)),
                     fill=fill_col,alpha=0.6)+
        labs(title=paste(platform_name,"Points by",if(filt_col=="start_ps")"Start" else "Finish"),
             x=if(filt_col=="start_ps")"Starting Position" else "Finish Position",y="Points")+
        scale_x_discrete(limits=factor(1:40))+dark_theme+
        theme(axis.text.x=element_text(angle=45,hjust=1,color="#ffffff"))
      ggplotly(p,tooltip="text",height=700) %>% layout(paper_bgcolor="#2d2d2d", plot_bgcolor="#2d2d2d", font=list(color="#ffffff"), xaxis=list(gridcolor="#404040", zerolinecolor="#666666"), yaxis=list(gridcolor="#404040", zerolinecolor="#666666"))
    }
  })
}

shinyApp(ui = ui, server = server)