# =============================================================================
# app.R
# Golden Ticket Research Center — NASCAR Research App
# =============================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)
library(readxl)
library(openxlsx)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(ggplot2)
library(jsonlite)
library(tidyr)
library(stringr)

# =============================================================================
# CONSTANTS — computed once at startup, used throughout
# =============================================================================
CURRENT_YEAR <- as.integer(format(Sys.Date(), "%Y"))
DATA_FILE    <- "NascarData.xlsx"

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

safe_trimws <- function(x) {
  tryCatch({
    x_clean <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
    trimws(x_clean)
  }, error = function(e) x)
}

# Load the Results sheet from NascarData.xlsx
load_nascar_database <- function() {
  if (!file.exists(DATA_FILE)) return(NULL)
  tryCatch({
    read_xlsx(DATA_FILE, sheet = "Results")
  }, error = function(e) {
    message("Error loading Results sheet: ", e$message)
    NULL
  })
}

# Load the Races sheet from NascarData.xlsx
load_races_sheet <- function() {
  if (!file.exists(DATA_FILE)) return(NULL)
  tryCatch({
    read_xlsx(DATA_FILE, sheet = "Races")
  }, error = function(e) {
    message("Error loading Races sheet: ", e$message)
    NULL
  })
}

# Load live entry list from NASCAR API for the upcoming race
load_entry_list <- function(race_season, series_id, race_id) {
  tryCatch({
    url <- sprintf("https://cf.nascar.com/cacher/%d/%d/%d/weekend-feed.json",
                   race_season, series_id, race_id)
    json_data <- fromJSON(url)
    json_data$weekend_race %>%
      unnest(results, names_sep = "_") %>%
      select(
        Start   = results_starting_position,
        Name    = results_driver_fullname,
        Car     = results_car_number,
        Team    = results_team_name,
        CC      = results_crew_chief_fullname,
        Make    = results_car_make,
        Sponsor = results_sponsor
      ) %>%
      mutate(
        Car     = as.integer(Car),
        Start   = as.integer(Start),
        across(c(Name, Team, CC, Sponsor, Make),
               ~iconv(., from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""))
      ) %>%
      arrange(Start)
  }, error = function(e) {
    data.frame(Start = integer(), Name = character(), Car = integer(),
               Team = character(), CC = character(),
               Make = character(), Sponsor = character())
  })
}

calc_dom_points <- function(total_laps, green_laps) {
  list(
    dk = round((0.45 * green_laps) + (0.25 * total_laps), 1),
    fd = round(0.1 * total_laps, 1)
  )
}

calc_finish_rates <- function(data, group_col, group_label) {
  data %>%
    group_by(!!sym(group_col)) %>%
    summarize(
      Races       = n(),
      Win         = round(mean(ps == 1,  na.rm = TRUE) * 100, 1),
      `Top 3`     = round(mean(ps <= 3,  na.rm = TRUE) * 100, 1),
      `Top 5`     = round(mean(ps <= 5,  na.rm = TRUE) * 100, 1),
      `Top 10`    = round(mean(ps <= 10, na.rm = TRUE) * 100, 1),
      `Top 15`    = round(mean(ps <= 15, na.rm = TRUE) * 100, 1),
      `Top 20`    = round(mean(ps <= 20, na.rm = TRUE) * 100, 1),
      `Top 25`    = round(mean(ps <= 25, na.rm = TRUE) * 100, 1),
      `Top 30`    = round(mean(ps <= 30, na.rm = TRUE) * 100, 1),
      `Avg Finish`= round(mean(ps,       na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    rename(!!group_label := !!sym(group_col))
}

# Series id -> display label
series_label <- function(id) {
  switch(as.character(id),
         "1" = "Cup Series",
         "2" = "OReilly Series",
         "3" = "Truck Series",
         "Cup Series"
  )
}

# Series id -> salary file prefix  (DKCup.csv, DKOReilly.csv, DKTrucks.csv)
series_salary_prefix <- function(id) {
  switch(as.character(id),
         "1" = "Cup",
         "2" = "OReilly",
         "3" = "Trucks",
         "Cup"
  )
}

# =============================================================================
# UI
# =============================================================================

# Shared empty-state panel shown on tabs before races are loaded
no_races_panel <- function() {
  fluidRow(column(12,
                  div(class = "box",
                      div(class = "box-header", h3("No Races Loaded", class = "box-title")),
                      div(class = "box-body",
                          div(class = "empty-state",
                              div(class = "empty-state-icon", "🏁"),
                              p(class = "empty-state-text",
                                "Races will load automatically — or use Race Selection to change filters.")
                          )
                      )
                  )
  ))
}

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    # External CSS — all styles live in gts_theme.css
    tags$link(rel = "stylesheet", type = "text/css", href = "gts_theme.css"),
    
    # Page title
    tags$title("Golden Ticket Research Center")
  ),
  
  # ---- HEADER ----
  div(class = "app-header",
      div(class = "app-header-left",
          img(src = "logo.jpg", class = "app-logo"),
          h1("Golden Ticket Research Center", class = "app-title")
      )
  ),
  
  navbarPage(
    title       = NULL,
    id          = "main_tabs",
    windowTitle = "Golden Ticket Research Center",
    
    # =========================================================================
    # RACE SELECTION TAB
    # =========================================================================
    tabPanel("Race Selection", value = "race_selection",
             
             fluidRow(column(12,
                             div(class = "box",
                                 div(class = "box-header", h3("Race Selection", class = "box-title")),
                                 div(class = "box-body",
                                     fluidRow(
                                       column(4, selectizeInput("analysis_series", "Series:",
                                                                choices  = c("Cup Series" = 1, "OReilly Series" = 2, "Truck Series" = 3),
                                                                selected = 1)),
                                       column(4, selectizeInput("analysis_primary_track", "Track:",
                                                                choices = NULL,
                                                                options = list(placeholder = "Select Track"))),
                                       column(4, selectizeInput("analysis_race_id", "Race:",
                                                                choices = NULL,
                                                                options = list(placeholder = "Select Race")))
                                     )
                                 )
                             )
             )),
             
             conditionalPanel(condition = "output.filters_confirmed",
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-header",
                                                      div(style = "display:flex;justify-content:space-between;align-items:center;",
                                                          h3("Race Pools", class = "box-title"),
                                                          uiOutput("races_loaded_badge", inline = TRUE)
                                                      )
                                                  ),
                                                  div(class = "box-body",
                                                      
                                                      # Row 1: Dom slider + counts
                                                      fluidRow(
                                                        column(8, uiOutput("dom_points_slider_ui")),
                                                        column(4, uiOutput("dom_points_summary"))
                                                      ),
                                                      
                                                      hr(style = "border-color:#444444;margin:8px 0 12px;"),
                                                      
                                                      # Row 2: Perf pool bulk controls
                                                      fluidRow(
                                                        column(12,
                                                               div(style = "display:flex;align-items:flex-start;gap:24px;flex-wrap:wrap;",
                                                                   
                                                                   # Same-track toggle
                                                                   div(
                                                                     p(style = "color:#aaaaaa;font-size:12px;font-weight:600;margin:0 0 6px;text-transform:uppercase;letter-spacing:0.5px;",
                                                                       "Perf Pool"),
                                                                     uiOutput("same_track_toggle_ui")
                                                                   ),
                                                                   
                                                                   # Track type pills
                                                                   div(style = "flex:1;",
                                                                       p(style = "color:#aaaaaa;font-size:12px;font-weight:600;margin:0 0 6px;text-transform:uppercase;letter-spacing:0.5px;",
                                                                         "Track Types"),
                                                                       uiOutput("track_type_pills_ui")
                                                                   ),
                                                                   
                                                                   # Season from
                                                                   div(style = "min-width:130px;",
                                                                       uiOutput("perf_season_from_ui")
                                                                   )
                                                               )
                                                        )
                                                      ),
                                                      
                                                      div(style = "margin:10px 0 6px;color:#666666;font-size:11px;",
                                                          HTML("<b style='color:#FFE500;'>Dom</b> — sim inputs &nbsp;|&nbsp;
                      <b style='color:#FFE500;'>Perf</b> — finish rates &amp; performance &nbsp;|&nbsp;
                      Checkboxes override individual rows. Reload resets all.")
                                                      ),
                                                      
                                                      fluidRow(column(12, withSpinner(DT::dataTableOutput("races_selection_table"))))
                                                  )
                                              )
                              ))
             )
    ),
    
    # =========================================================================
    # ENTRY LIST TAB
    # =========================================================================
    tabPanel("Entry List", value = "entry_list",
             
             conditionalPanel(condition = "output.filters_confirmed",
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-header",
                                                      style = "display:flex;justify-content:space-between;align-items:center;",
                                                      uiOutput("entry_list_title", inline = TRUE),
                                                      div(style = "display:flex;gap:8px;align-items:center;",
                                                          downloadButton("download_entry_list_csv",   "CSV",              class = "btn-success", style = "margin:0;"),
                                                          downloadButton("download_entry_list_excel", "Excel",            class = "btn-success", style = "margin:0;"),
                                                          downloadButton("download_input_file",       "Create Input File",class = "btn-warning", style = "margin:0;")
                                                      )
                                                  ),
                                                  div(class = "box-body",
                                                      fluidRow(column(12, withSpinner(DT::dataTableOutput("entry_list_table"))))
                                                  )
                                              )
                              ))
             ),
             
             conditionalPanel(condition = "!output.filters_confirmed", no_races_panel())
    ),
    
    # =========================================================================
    # FINISH RATES TAB
    # =========================================================================
    tabPanel("Finish Rates", value = "finish_rates",
             
             conditionalPanel(condition = "output.filters_confirmed",
                              
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-header",
                                                      style = "display:flex;justify-content:space-between;align-items:center;",
                                                      h3("Finish Rate Controls", class = "box-title"),
                                                      downloadButton("download_finish_rates", "Download", class = "btn-success", style = "margin:0;")
                                                  ),
                                                  div(class = "box-body",
                                                      
                                                      # View pills
                                                      fluidRow(column(12,
                                                                      div(style = "display:flex;align-items:center;gap:12px;flex-wrap:wrap;margin-bottom:10px;",
                                                                          span(style = "color:#aaa;font-size:12px;font-weight:600;text-transform:uppercase;letter-spacing:0.5px;",
                                                                               "View By:"),
                                                                          uiOutput("fr_view_pills_ui")
                                                                      )
                                                      )),
                                                      
                                                      # Time period pills
                                                      fluidRow(column(12,
                                                                      div(style = "display:flex;align-items:center;gap:12px;flex-wrap:wrap;margin-bottom:10px;",
                                                                          span(style = "color:#aaa;font-size:12px;font-weight:600;text-transform:uppercase;letter-spacing:0.5px;",
                                                                               "Seasons:"),
                                                                          uiOutput("fr_time_pills_ui")
                                                                      )
                                                      )),
                                                      
                                                      # Tier config (only when tier view active)
                                                      conditionalPanel(condition = "output.fr_view_is_tier",
                                                                       div(style = "background:#222;border:1px solid #555;border-radius:4px;padding:10px;margin-top:4px;",
                                                                           p(style = "color:#FFE500;font-weight:bold;margin-bottom:8px;",
                                                                             "Team Tier Configuration"),
                                                                           uiOutput("tier_config_ui"),
                                                                           div(style = "margin-top:8px;display:flex;gap:8px;",
                                                                               actionButton("add_tier",    "+ Add Tier",    class = "btn-info",   style = "font-size:12px;padding:4px 10px;"),
                                                                               actionButton("remove_tier", "- Remove Tier", class = "btn-danger", style = "font-size:12px;padding:4px 10px;")
                                                                           )
                                                                       )
                                                      )
                                                  )
                                              )
                              )),
                              
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-header", h3("Finish Rates (%)", class = "box-title")),
                                                  div(class = "box-body",
                                                      withSpinner(DT::dataTableOutput("finish_rates_table"))
                                                  )
                                              )
                              ))
             ),
             
             conditionalPanel(condition = "!output.filters_confirmed", no_races_panel())
    ),
    
    # =========================================================================
    # DOMINATOR TAB
    # =========================================================================
    tabPanel("Dominator", value = "dominator",
             
             conditionalPanel(condition = "output.filters_confirmed",
                              
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-header",
                                                      style = "display:flex;justify-content:space-between;align-items:center;",
                                                      h3("Dominator Data", class = "box-title", style = "margin:0;"),
                                                      div(style = "display:flex;gap:8px;",
                                                          downloadButton("download_dominator_csv",     "CSV",              class = "btn-success", style = "margin:0;"),
                                                          downloadButton("download_dominator_profile", "Download Profile", class = "btn-success", style = "margin:0;")
                                                      )
                                                  ),
                                                  div(class = "box-body",
                                                      withSpinner(DT::dataTableOutput("dominator_data_table"))
                                                  )
                                              )
                              )),
                              
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-header", h3("Dominator Visualizations", class = "box-title")),
                                                  div(class = "box-body",
                                                      fluidRow(
                                                        column(6, selectInput("dom_visual_type", "Select Visualization:",
                                                                              choices = c(
                                                                                "Score Distribution by Dom Rank"  = "score_dist",
                                                                                "Dom Rank Finish Ranges"           = "rank_finish",
                                                                                "Dom Pts by Finish Position"       = "pts_by_finish",
                                                                                "Dom Pts by Starting Position"     = "dom_pts_start",
                                                                                "Dom Rank by Starting Position"    = "dom_rank_start",
                                                                                "Laps Led by Finish Position"      = "laps_led",
                                                                                "Laps Led by Starting Position"    = "laps_led_start",
                                                                                "Fast Laps by Finish Position"     = "fast_laps",
                                                                                "Fast Laps by Starting Position"   = "fast_laps_start",
                                                                                "Driver Dominator Boxplots"        = "driver_boxplot",
                                                                                "Team Dominator Boxplots"          = "team_boxplot"
                                                                              ),
                                                                              selected = "score_dist"
                                                        )),
                                                        column(6, radioButtons("dom_platform", "Platform:",
                                                                               choices = c("DraftKings" = "DK", "FanDuel" = "FD"),
                                                                               selected = "DK", inline = TRUE))
                                                      ),
                                                      fluidRow(
                                                        column(4, selectInput("dom_color_by", "Color By:", choices = c("None" = "none", "Track Type" = "track_type", "Season" = "race_season", "Track" = "track_name"), selected = "none"))
                                                      ),
                                                      fluidRow(column(12, withSpinner(plotlyOutput("dominator_plot", height = "700px"))))
                                                  )
                                              )
                              ))
             ),
             
             conditionalPanel(condition = "!output.filters_confirmed", no_races_panel())
    ),
    
    # =========================================================================
    # PLACE DIFFERENTIAL TAB
    # =========================================================================
    tabPanel("Place Differential", value = "place_differential",
             
             conditionalPanel(condition = "output.filters_confirmed",
                              
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-header",
                                                      style = "display:flex;justify-content:space-between;align-items:center;",
                                                      h3("Place Differential Data", class = "box-title", style = "margin:0;"),
                                                      downloadButton("download_pd_csv", "CSV", class = "btn-success", style = "margin:0;")
                                                  ),
                                                  div(class = "box-body",
                                                      withSpinner(DT::dataTableOutput("pd_data_table"))
                                                  )
                                              )
                              )),
                              
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-header", h3("Place Differential Visualizations", class = "box-title")),
                                                  div(class = "box-body",
                                                      fluidRow(
                                                        column(4, selectInput("pd_visual_type", "Visualization Type:",
                                                                              choices = c(
                                                                                "Start vs Finish Scatter"    = "scatter",
                                                                                "Position Change Distribution" = "histogram",
                                                                                "PD by Start Position"        = "boxplot_start",
                                                                                "PD by Finish Position"       = "boxplot_finish"
                                                                              )
                                                        ))
                                                      ),
                                                      fluidRow(
                                                        column(4, selectInput("pd_color_by", "Color By:", choices = c("None" = "none", "Track Type" = "track_type", "Season" = "race_season", "Track" = "track_name"), selected = "none"))
                                                      ),
                                                      fluidRow(column(12, withSpinner(plotlyOutput("pd_plot", height = "700px"))))
                                                  )
                                              )
                              ))
             ),
             
             conditionalPanel(condition = "!output.filters_confirmed", no_races_panel())
    ),
    
    # =========================================================================
    # PERFORMANCE TAB
    # =========================================================================
    tabPanel("Performance", value = "performance",
             
             conditionalPanel(condition = "output.filters_confirmed",
                              
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-header", h3("Performance Data", class = "box-title")),
                                                  div(class = "box-body",
                                                      fluidRow(
                                                        column(6, uiOutput("perf_time_ui")),
                                                        column(6, downloadButton("download_performance_csv", "Download CSV",
                                                                                 class = "btn-success", style = "margin-top:0px;"))
                                                      ),
                                                      fluidRow(column(12, withSpinner(DT::dataTableOutput("performance_data_table"))))
                                                  )
                                              )
                              )),
                              
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-header", h3("Performance Visualizations", class = "box-title")),
                                                  div(class = "box-body",
                                                      fluidRow(
                                                        column(6, selectInput("perf_visual_type", "Select Visualization:",
                                                                              choices = c(
                                                                                "Driver Speed Rank Distribution" = "driver_speed",
                                                                                "Team Speed Rank Distribution"   = "team_speed",
                                                                                "Driver Finish Distribution"     = "driver_finish",
                                                                                "Team Finish Distribution"       = "team_finish",
                                                                                "Driver ARP Distribution"        = "driver_arp",
                                                                                "Team ARP Distribution"          = "team_arp"
                                                                              ),
                                                                              selected = "driver_speed"
                                                        )),
                                                        column(6, uiOutput("perf_visual_time_ui"))
                                                      ),
                                                      fluidRow(
                                                        column(4, selectInput("perf_color_by", "Color By:", choices = c("None" = "none", "Track Type" = "track_type", "Season" = "race_season", "Track" = "track_name"), selected = "none"))
                                                      ),
                                                      fluidRow(column(12, withSpinner(plotlyOutput("performance_plot", height = "800px"))))
                                                  )
                                              )
                              ))
             ),
             
             conditionalPanel(condition = "!output.filters_confirmed", no_races_panel())
    ),
    
    # =========================================================================
    # FANTASY SCORING TAB
    # =========================================================================
    tabPanel("Fantasy Scoring", value = "fantasy_scoring",
             
             conditionalPanel(condition = "output.filters_confirmed",
                              
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-header", h3("Fantasy Scoring Data", class = "box-title")),
                                                  div(class = "box-body",
                                                      fluidRow(
                                                        column(6, radioButtons("fs_platform", "Platform:",
                                                                               choices = c("DraftKings" = "DK", "FanDuel" = "FD"),
                                                                               selected = "DK", inline = TRUE)),
                                                        column(6, downloadButton("download_fantasy_csv", "Download CSV",
                                                                                 class = "btn-success", style = "margin-top:0px;"))
                                                      ),
                                                      fluidRow(column(12, withSpinner(DT::dataTableOutput("fantasy_data_table"))))
                                                  )
                                              )
                              )),
                              
                              fluidRow(column(12,
                                              div(class = "box",
                                                  div(class = "box-header", h3("Fantasy Scoring Visualizations", class = "box-title")),
                                                  div(class = "box-body",
                                                      fluidRow(
                                                        column(6, selectInput("fs_visual_type", "Select Visualization:",
                                                                              choices = c(
                                                                                "Score Distribution by Rank"        = "score_dist",
                                                                                "Scoring Components Breakdown"      = "components",
                                                                                "Score Distribution by Start"       = "score_by_start",
                                                                                "Score Distribution by Finish"      = "score_by_finish"
                                                                              ),
                                                                              selected = "score_dist"
                                                        )),
                                                        column(6, radioButtons("fs_visual_platform", "Platform:",
                                                                               choices = c("DraftKings" = "DK", "FanDuel" = "FD"),
                                                                               selected = "DK", inline = TRUE))
                                                      ),
                                                      fluidRow(
                                                        column(4, selectInput("fs_color_by", "Color By:", choices = c("None" = "none", "Track Type" = "track_type", "Season" = "race_season", "Track" = "track_name"), selected = "none"))
                                                      ),
                                                      fluidRow(column(12, withSpinner(plotlyOutput("fantasy_plot", height = "700px"))))
                                                  )
                                              )
                              ))
             ),
             
             conditionalPanel(condition = "!output.filters_confirmed", no_races_panel())
    )
    
  ) # end navbarPage
) # end fluidPage

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # Null-coalescing operator
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
  
  # ---------------------------------------------------------------------------
  # REACTIVE VALUES
  # ---------------------------------------------------------------------------
  values <- reactiveValues(
    nascar_data              = NULL,
    races_sheet              = NULL,
    analysis_filtered_data   = NULL,
    analysis_entry_list      = NULL,
    analysis_races_available = NULL,
    filters_confirmed        = FALSE,
    pool_state               = NULL,   # race_id, dom, perf, dom_manual, perf_manual
    # Perf pool bulk controls
    perf_same_track_only     = FALSE,
    perf_track_types         = character(0),  # track types currently included
    perf_season_from         = 2022L,
    perf_same_track_toggle   = 0L,            # toggle counter for button
    num_tiers                = 3,
    fr_view_sel              = "driver",    # active view pill
    fr_seasons_sel           = "all"        # active season pills ("all" or char vec of years)
  )
  
  # ---------------------------------------------------------------------------
  # STARTUP: load data, populate dropdowns, auto-trigger filter load
  # ---------------------------------------------------------------------------
  observe({
    withProgress(message = "Loading Golden Ticket Database...", {
      
      incProgress(0.1, detail = "Reading database...")
      values$nascar_data <- load_nascar_database()
      values$races_sheet <- load_races_sheet()
      
      req(values$races_sheet)
      incProgress(0.3, detail = "Processing tracks...")
      
      # Only show tracks that have data in the Results sheet
      tracks_with_data <- unique(values$nascar_data$track_name[!is.na(values$nascar_data$track_name)])
      all_tracks <- sort(unique(values$races_sheet$track_name[
        values$races_sheet$track_name %in% tracks_with_data &
          !is.na(values$races_sheet$track_name)]))
      
      # Find next upcoming race for Cup Series (series 1) and pre-select
      upcoming <- values$races_sheet %>%
        filter(!is.na(race_date),
               as.Date(substr(race_date, 1, 10)) >= Sys.Date(),
               series_id == 1) %>%
        arrange(race_date)
      
      if (nrow(upcoming) > 0) {
        next_race <- upcoming[1, ]
        # Pre-select series, track — the series observer will fire and
        # populate the race dropdown, which triggers the pool load
        updateSelectizeInput(session, "analysis_series",
                             selected = as.character(next_race$series_id))
        updateSelectizeInput(session, "analysis_primary_track",
                             choices = all_tracks, selected = next_race$track_name)
      } else {
        updateSelectizeInput(session, "analysis_primary_track", choices = all_tracks)
      }
      
      incProgress(0.8, detail = "Auto-loading races...")
    })
    
  })
  
  # ---------------------------------------------------------------------------
  # DYNAMIC YEAR RADIO BUTTONS
  # Replaces all hardcoded "2025 Only" labels throughout the app
  # ---------------------------------------------------------------------------
  year_radio_choices <- reactive({
    c("Full History" = "all", setNames(as.character(CURRENT_YEAR), paste(CURRENT_YEAR, "Only")))
  })
  
  output$fr_time_ui <- renderUI({
    radioButtons("fr_time", "Time Period:",
                 choices = year_radio_choices(), selected = as.character(CURRENT_YEAR), inline = TRUE)
  })
  
  # FR view pills
  output$fr_view_pills_ui <- renderUI({
    req(values$analysis_races_available)
    views <- c("Driver" = "driver", "Car" = "car", "Team" = "team",
               "Start Pos" = "start_pos", "Tier" = "tier")
    cur <- if (!is.null(values$fr_view_sel)) values$fr_view_sel else "driver"
    pill_btn <- function(lbl, val) {
      actionButton(paste0("frview_", val), lbl,
                   style = paste0(
                     "margin:2px;padding:4px 12px;font-size:12px;font-weight:600;border-radius:20px;",
                     if (cur == val)
                       "background:#FFE500!important;color:#000!important;border:2px solid #FFE500!important;"
                     else
                       "background:#2a2a2a!important;color:#888!important;border:2px solid #444!important;"))
    }
    div(style = "display:flex;flex-wrap:wrap;gap:4px;",
        mapply(pill_btn, names(views), views, SIMPLIFY = FALSE))
  })
  
  # FR time pills — multi-select seasons + "All"
  output$fr_time_pills_ui <- renderUI({
    req(values$analysis_races_available)
    seasons <- sort(unique(values$analysis_races_available$race_season))
    cur     <- if (!is.null(values$fr_seasons_sel)) values$fr_seasons_sel else "all"
    make_pill <- function(lbl, val) {
      is_active <- val %in% cur
      actionButton(paste0("frtime_", val), lbl,
                   style = paste0(
                     "margin:2px;padding:4px 12px;font-size:12px;font-weight:600;border-radius:20px;",
                     if (is_active)
                       "background:#FFE500!important;color:#000!important;border:2px solid #FFE500!important;"
                     else
                       "background:#2a2a2a!important;color:#888!important;border:2px solid #444!important;"))
    }
    all_pills <- c(
      list(make_pill("All", "all")),
      lapply(seasons, function(s) make_pill(as.character(s), as.character(s)))
    )
    div(style = "display:flex;flex-wrap:wrap;gap:4px;", tagList(all_pills))
  })
  
  # Expose fr_view for conditionalPanel
  output$fr_view_is_tier <- reactive({
    isTRUE(values$fr_view_sel == "tier")
  })
  outputOptions(output, "fr_view_is_tier", suspendWhenHidden = FALSE)
  
  output$perf_time_ui <- renderUI({
    radioButtons("perf_time_filter", "Time Period:",
                 choices = year_radio_choices(), selected = "all", inline = TRUE)
  })
  
  output$perf_visual_time_ui <- renderUI({
    radioButtons("perf_visual_time", "Time Period:",
                 choices = year_radio_choices(), selected = "all", inline = TRUE)
  })
  
  # ---------------------------------------------------------------------------
  # SERIES CHANGE: update track list AND pre-select next race for that series
  # ---------------------------------------------------------------------------
  observeEvent(input$analysis_series, {
    req(input$analysis_series, values$races_sheet, values$nascar_data)
    sid <- as.numeric(input$analysis_series)
    
    tracks_with_data <- values$nascar_data %>%
      filter(series_id == sid) %>%
      pull(track_name) %>%
      unique()
    all_tracks <- sort(unique(values$races_sheet$track_name[
      values$races_sheet$track_name %in% tracks_with_data]))
    
    # Find next upcoming race for this series and pre-select its track
    next_up <- values$races_sheet %>%
      filter(series_id == sid,
             !is.na(race_date),
             as.Date(substr(race_date, 1, 10)) >= Sys.Date()) %>%
      arrange(race_date) %>%
      slice(1)
    
    if (nrow(next_up) > 0) {
      updateSelectizeInput(session, "analysis_primary_track",
                           choices = all_tracks, selected = next_up$track_name)
    } else {
      updateSelectizeInput(session, "analysis_primary_track", choices = all_tracks)
    }
  }, ignoreInit = TRUE)
  
  # ---------------------------------------------------------------------------
  # RACE DROPDOWN: upcoming races at selected series + track
  # ---------------------------------------------------------------------------
  observe({
    req(input$analysis_series, input$analysis_primary_track, values$races_sheet)
    
    available <- values$races_sheet %>%
      filter(
        series_id  == as.numeric(input$analysis_series),
        track_name == input$analysis_primary_track,
        !is.na(race_date),
        as.Date(substr(race_date, 1, 10)) >= Sys.Date()
      ) %>%
      arrange(race_date) %>%
      mutate(race_label = paste0(race_season, " — ", race_name))
    
    choices <- setNames(available$race_id, available$race_label)
    updateSelectizeInput(session, "analysis_race_id",
                         choices  = choices,
                         selected = if (length(choices) > 0) choices[1] else NULL)
  })
  
  # ---------------------------------------------------------------------------
  # LOAD RACES: main filter handler (also triggered automatically on startup)
  # ---------------------------------------------------------------------------
  # Fires on series or track change (no button needed)
  observeEvent(list(input$analysis_series, input$analysis_primary_track), {
    req(input$analysis_series, input$analysis_primary_track,
        values$races_sheet, values$nascar_data)
    
    withProgress(message = "Loading races...", {
      incProgress(0.2)
      
      selected_series <- as.numeric(input$analysis_series)
      selected_track  <- input$analysis_primary_track
      
      # Get selected track type for perf pool pre-population
      selected_track_type <- values$races_sheet %>%
        filter(track_name == selected_track) %>%
        pull(track_type) %>%
        first()
      
      # All historical races across the series — every series/track combo
      # that has data is eligible; filter to Historical only
      all_historical <- values$races_sheet %>%
        filter(Historical == "Y",
               series_id  == selected_series)
      
      incProgress(0.4)
      
      # Join race aggregates from Results sheet
      race_aggs <- values$nascar_data %>%
        filter(series_id == selected_series) %>%
        group_by(race_id) %>%
        summarise(
          total_laps   = first(act_laps),
          lead_lap     = sum(LapsDown == 0, na.rm = TRUE),
          crash_dnfs   = sum(finishing_status %in%
                               c("Accident", "DVP", "Damage"), na.rm = TRUE),
          mech_dnfs    = sum(!finishing_status %in%
                               c("Running", "Accident", "DVP", "Damage") &
                               !is.na(finishing_status), na.rm = TRUE),
          DK_Dom_Total = round(sum(DKSP, na.rm = TRUE), 1),
          FD_Dom_Total = round(sum(FDSP, na.rm = TRUE), 1),
          .groups = "drop"
        )
      
      races_available <- all_historical %>%
        inner_join(race_aggs, by = "race_id") %>%
        mutate(
          total_laps    = if_else(is.na(total_laps), scheduled_laps, total_laps),
          is_same_track = track_name == selected_track,
          is_same_type  = !is.na(track_type) & !is.na(selected_track_type) &
            track_type == selected_track_type
        ) %>%
        arrange(desc(is_same_track), desc(race_season))
      
      incProgress(0.6)
      
      # Auto dom pool: ±25% of median DK dom total at this track+series
      # If no same-track history, use full series median
      same_track_dom <- races_available %>%
        filter(is_same_track, DK_Dom_Total > 0) %>%
        pull(DK_Dom_Total)
      # Default range: ±25% of median, but always wide enough to include
      # all same-track races so the selected track's history is in Dom pool
      ref_median <- if (length(same_track_dom) >= 2) {
        median(same_track_dom, na.rm = TRUE)
      } else {
        median(races_available$DK_Dom_Total[races_available$DK_Dom_Total > 0],
               na.rm = TRUE)
      }
      dom_lo <- if (length(same_track_dom) > 0) {
        min(floor(ref_median * 0.75), floor(min(same_track_dom) * 0.95))
      } else {
        floor(ref_median * 0.75)
      }
      dom_hi <- if (length(same_track_dom) > 0) {
        max(ceiling(ref_median * 1.25), ceiling(max(same_track_dom) * 1.05))
      } else {
        ceiling(ref_median * 1.25)
      }
      
      # Default perf pill state: tracks of same type as selected track
      default_perf_tracks <- if (!is.na(selected_track_type)) {
        unique(races_available$track_name[
          races_available$track_type == selected_track_type &
            !is.na(races_available$track_name)])
      } else {
        unique(races_available$track_name[!is.na(races_available$track_name)])
      }
      
      # Build initial pool_state: auto-assign dom and perf flags
      pool_state <- races_available %>%
        transmute(
          race_id,
          dom        = DK_Dom_Total >= dom_lo & DK_Dom_Total <= dom_hi,
          perf       = track_name %in% default_perf_tracks,
          dom_manual = FALSE,
          perf_manual= FALSE
        )
      
      # Store perf control state
      values$perf_same_track_only   <- FALSE
      values$perf_track_types       <- default_perf_tracks
      values$perf_season_from       <- min(races_available$race_season, na.rm = TRUE)
      values$perf_same_track_toggle <- 0L
      
      incProgress(0.8)
      
      filtered_nascar <- values$nascar_data %>%
        filter(race_id %in% races_available$race_id)
      
      values$analysis_filtered_data   <- filtered_nascar
      values$analysis_entry_list      <- NULL   # reset; entry list observer reloads it
      values$analysis_races_available <- races_available
      values$pool_state               <- pool_state
      values$filters_confirmed        <- TRUE
      
      # Store auto-range for slider
      values$dom_lo <- dom_lo
      values$dom_hi <- dom_hi
      
      incProgress(1.0)
      n_dom  <- sum(pool_state$dom)
      n_perf <- sum(pool_state$perf)
      showNotification(
        sprintf("Loaded %d races. Dom pool: %d | Perf pool: %d",
                nrow(races_available), n_dom, n_perf),
        type = "message", duration = 5)
    })
  })
  
  # ---------------------------------------------------------------------------
  # PERF POOL BULK CONTROLS — pills, toggle, season
  # ---------------------------------------------------------------------------
  
  # Same-track-only toggle button
  output$same_track_toggle_ui <- renderUI({
    req(values$analysis_races_available)
    is_on <- isTRUE(values$perf_same_track_only)
    actionButton("perf_same_track_btn",
                 label  = if (is_on) "★ Same Track Only" else "★ Same Track Only",
                 class  = if (is_on) "btn-primary" else "btn-default",
                 style  = paste0(
                   "font-size:12px;padding:5px 12px;font-weight:600;",
                   if (is_on) "background:#FFE500!important;color:#000!important;border-color:#FFE500!important;"
                   else "background:#333!important;color:#aaa!important;border-color:#555!important;"
                 )
    )
  })
  
  observeEvent(input$perf_same_track_btn, {
    values$perf_same_track_only <- !isTRUE(values$perf_same_track_only)
  })
  
  # Track pills — grouped by type. Clicking a type header toggles all tracks
  # in that type; clicking an individual track pill toggles just that track.
  # Perf pool track type pills — JS onclick fires single shared inputs
  # 'perf_type_click' carries the track_type value
  # 'perf_track_click' carries the track_name value
  output$track_type_pills_ui <- renderUI({
    req(values$analysis_races_available)
    ra    <- values$analysis_races_available
    active_tracks <- values$perf_track_types
    
    type_labels <- c(
      short_track   = "Short Track",
      intermediate  = "Intermediate",
      superspeedway = "Superspeedway",
      road_course   = "Road Course",
      atlanta       = "Atlanta",
      dirt          = "Dirt",
      other         = "Other"
    )
    
    types  <- sort(unique(ra$track_type[!is.na(ra$track_type)]))
    
    groups <- lapply(types, function(tt) {
      tracks_in_type <- sort(unique(ra$track_name[ra$track_type == tt & !is.na(ra$track_name)]))
      all_active <- all(tracks_in_type %in% active_tracks)
      type_label <- if (tt %in% names(type_labels)) type_labels[[tt]] else tt
      
      type_js <- sprintf(
        "Shiny.setInputValue('perf_type_click', '%s', {priority:'event'})", tt)
      type_pill <- tags$span(
        onClick = type_js,
        style = paste0(
          "display:inline-block;cursor:pointer;user-select:none;",
          "margin:2px;padding:4px 14px;font-size:12px;font-weight:700;border-radius:20px;",
          if (all_active)
            "background:#FFE500;color:#000;border:2px solid #FFE500;"
          else
            "background:#2a2a2a;color:#888;border:2px solid #555;"
        ),
        type_label
      )
      
      track_pills <- lapply(tracks_in_type, function(tn) {
        is_active <- tn %in% active_tracks
        # JSON-encode track name so special chars are safe in JS string
        tn_safe <- gsub("'", "\\'", tn)
        track_js <- sprintf(
          "Shiny.setInputValue('perf_track_click', '%s', {priority:'event'})", tn_safe)
        tags$span(
          onClick = track_js,
          style = paste0(
            "display:inline-block;cursor:pointer;user-select:none;",
            "margin:2px;padding:3px 10px;font-size:11px;font-weight:500;border-radius:20px;",
            if (is_active)
              "background:rgba(255,229,0,0.2);color:#FFE500;border:1px solid #FFE500;"
            else
              "background:#1e1e1e;color:#666;border:1px solid #3a3a3a;"
          ),
          tn
        )
      })
      
      div(style = "margin-bottom:8px;",
          div(style = "display:flex;flex-wrap:wrap;gap:3px;align-items:center;",
              type_pill,
              tags$span(style = "color:#444;margin:0 4px;font-size:14px;", "▸"),
              tagList(track_pills)
          )
      )
    })
    
    tagList(groups)
  })
  
  # Single observer for type header clicks
  observeEvent(input$perf_type_click, {
    tt <- input$perf_type_click
    req(tt, values$analysis_races_available)
    tracks_in_type <- unique(values$analysis_races_available$track_name[
      values$analysis_races_available$track_type == tt])
    current <- values$perf_track_types
    if (all(tracks_in_type %in% current)) {
      values$perf_track_types <- setdiff(current, tracks_in_type)
    } else {
      values$perf_track_types <- union(current, tracks_in_type)
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Single observer for individual track clicks
  observeEvent(input$perf_track_click, {
    tn <- input$perf_track_click
    req(tn)
    current <- values$perf_track_types
    if (tn %in% current) {
      values$perf_track_types <- setdiff(current, tn)
    } else {
      values$perf_track_types <- c(current, tn)
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Season from selector
  output$perf_season_from_ui <- renderUI({
    req(values$analysis_races_available)
    seasons <- sort(unique(values$analysis_races_available$race_season))
    selectInput("perf_season_from", "Perf From Season:",
                choices  = seasons,
                selected = values$perf_season_from,
                width    = "130px")
  })
  
  observeEvent(input$perf_season_from, {
    req(input$perf_season_from)
    values$perf_season_from <- as.integer(input$perf_season_from)
  }, ignoreInit = TRUE)
  
  # Recompute pool_state$perf whenever any bulk control changes
  # Respects perf_manual overrides — those rows are immune
  observe({
    req(values$pool_state, values$analysis_races_available)
    same_only   <- isTRUE(values$perf_same_track_only)
    incl_types  <- values$perf_track_types
    season_from <- values$perf_season_from %||% 2022L
    
    ra <- values$analysis_races_available %>%
      select(race_id, track_type, track_name, race_season, is_same_track)
    
    ps <- values$pool_state %>%
      left_join(ra, by = "race_id") %>%
      mutate(
        auto_perf = if (same_only) {
          is_same_track & race_season >= season_from
        } else {
          track_name %in% incl_types & race_season >= season_from
        },
        perf = if_else(!perf_manual, auto_perf, perf)
      ) %>%
      select(-track_type, -track_name, -race_season, -is_same_track, -auto_perf)
    
    values$pool_state <- ps
  })
  
  # ---------------------------------------------------------------------------
  # DOM POINTS SLIDER — rendered after load so range reflects actual data
  # ---------------------------------------------------------------------------
  output$dom_points_slider_ui <- renderUI({
    req(values$analysis_races_available, values$dom_lo, values$dom_hi)
    dom_vals <- values$analysis_races_available$DK_Dom_Total
    dom_vals <- dom_vals[!is.na(dom_vals) & dom_vals > 0]
    sliderInput("dom_points_range",
                "Dom Points Range (DK) — races within this range auto-assigned to Dom pool:",
                min   = floor(min(dom_vals)),
                max   = ceiling(max(dom_vals)),
                value = c(values$dom_lo, values$dom_hi),
                step  = 5)
  })
  
  output$dom_points_summary <- renderUI({
    req(values$pool_state)
    n_dom  <- sum(values$pool_state$dom,  na.rm = TRUE)
    n_perf <- sum(values$pool_state$perf, na.rm = TRUE)
    tagList(
      h4(style = "color:#FFE500;margin-top:30px;",
         sprintf("Dom: %d race%s", n_dom,  if (n_dom  == 1) "" else "s")),
      h4(style = "color:#aaaaaa;margin-top:5px;",
         sprintf("Perf: %d race%s", n_perf, if (n_perf == 1) "" else "s"))
    )
  })
  
  # When slider moves: update dom flags for NON-manually-overridden rows only
  observeEvent(input$dom_points_range, {
    req(values$pool_state, values$analysis_races_available)
    lo <- input$dom_points_range[1]
    hi <- input$dom_points_range[2]
    ra <- values$analysis_races_available %>% select(race_id, DK_Dom_Total)
    ps <- values$pool_state
    ps <- ps %>%
      left_join(ra, by = "race_id") %>%
      mutate(
        dom = if_else(!dom_manual,
                      DK_Dom_Total >= lo & DK_Dom_Total <= hi,
                      dom)
      ) %>%
      select(-DK_Dom_Total)
    values$pool_state <- ps
  }, ignoreInit = TRUE)
  
  # Handle individual checkbox toggles from JS
  observeEvent(input$race_pool_toggle, {
    req(values$pool_state)
    toggle  <- input$race_pool_toggle
    rid     <- as.integer(toggle$race_id)
    pool    <- toggle$pool
    checked <- toggle$checked
    ps      <- values$pool_state
    if (pool == "dom") {
      ps$dom[ps$race_id == rid]        <- checked
      ps$dom_manual[ps$race_id == rid] <- TRUE
    } else {
      ps$perf[ps$race_id == rid]         <- checked
      ps$perf_manual[ps$race_id == rid]  <- TRUE
    }
    values$pool_state <- ps
  })
  
  # ---------------------------------------------------------------------------
  # FILTER STATE
  # ---------------------------------------------------------------------------
  output$filters_confirmed <- reactive({ values$filters_confirmed })
  outputOptions(output, "filters_confirmed", suspendWhenHidden = FALSE)
  
  output$races_loaded_badge <- renderUI({
    req(values$analysis_races_available)
    n <- nrow(values$analysis_races_available)
    span(class = "status-badge status-badge-ok",
         sprintf("%d race%s", n, if (n == 1) "" else "s"))
  })
  
  # ---------------------------------------------------------------------------
  # RACE SELECTION TABLE — with inline Dom/Perf checkboxes
  # ---------------------------------------------------------------------------
  output$races_selection_table <- DT::renderDataTable({
    req(values$analysis_races_available, values$pool_state)
    
    ra <- values$analysis_races_available %>%
      left_join(values$pool_state %>% select(race_id, dom, perf), by = "race_id")
    
    # Build checkbox HTML for each row
    make_cb <- function(pool, race_ids, checked_vec) {
      mapply(function(rid, chk) {
        sprintf(
          "<input type='checkbox' id='%s_%s' onclick='Shiny.setInputValue(%s, {pool:%s, race_id:%s, checked:this.checked}, {priority:%s})' %s>",
          pool, rid,
          '"race_pool_toggle"', paste0('"', pool, '"'), paste0('"', rid, '"'), '"event"',
          if (isTRUE(chk)) "checked" else "")
      }, race_ids, checked_vec, SIMPLIFY = TRUE)
    }
    
    display <- ra %>%
      transmute(
        `Same Track` = if_else(is_same_track, "★", ""),
        Season       = race_season,
        Track        = track_name,
        `Track Type` = track_type,
        Race         = race_name,
        `Sched Laps` = scheduled_laps,
        `DK Dom`     = DK_Dom_Total,
        `FD Dom`     = FD_Dom_Total,
        `Lead Lap`   = lead_lap,
        `Crash DNFs` = crash_dnfs,
        `Mech DNFs`  = mech_dnfs,
        Dom          = make_cb("dom",  race_id, dom),
        Perf         = make_cb("perf", race_id, perf)
      )
    
    DT::datatable(display,
                  escape    = FALSE,       # render checkbox HTML
                  selection = "none",
                  rownames  = FALSE,
                  filter    = "top",       # per-column filters
                  class     = "display nowrap compact",
                  options   = list(
                    pageLength = 25, scrollX = TRUE, dom = "ltp",
                    columnDefs = list(
                      list(className = "dt-center", targets = "_all"),
                      list(orderable = FALSE, searchable = FALSE, targets = c(11, 12))
                    )
                  )
    )
  })
  
  # ---------------------------------------------------------------------------
  # ENTRY LIST: fires when race dropdown changes — separate from pool load
  # This ensures race_id is settled before we call the API
  # ---------------------------------------------------------------------------
  observeEvent(input$analysis_race_id, {
    req(input$analysis_race_id, input$analysis_series, values$races_sheet)
    upcoming_race_id <- as.numeric(input$analysis_race_id)
    if (is.na(upcoming_race_id) || length(upcoming_race_id) == 0) {
      values$analysis_entry_list <- data.frame(
        Start = integer(), Name = character(), Car = integer(),
        Team = character(), CC = character(),
        Make = character(), Sponsor = character())
      return()
    }
    race_info <- values$races_sheet %>%
      filter(race_id == upcoming_race_id) %>%
      slice(1)
    if (nrow(race_info) == 0) {
      values$analysis_entry_list <- data.frame(
        Start = integer(), Name = character(), Car = integer(),
        Team = character(), CC = character(),
        Make = character(), Sponsor = character())
      return()
    }
    withProgress(message = "Loading entry list...", {
      values$analysis_entry_list <- load_entry_list(
        race_info$race_season,
        as.numeric(input$analysis_series),
        upcoming_race_id)
    })
  }, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  # ---------------------------------------------------------------------------
  # POOL REACTIVES — downstream tabs read from these
  # ---------------------------------------------------------------------------
  dominator_filtered_races <- reactive({
    req(values$pool_state)
    values$pool_state %>% filter(dom == TRUE) %>% pull(race_id)
  })
  
  performance_race_ids_reactive <- reactive({
    req(values$pool_state)
    values$pool_state %>% filter(perf == TRUE) %>% pull(race_id)
  })
  
  # ---------------------------------------------------------------------------
  # SALARY AUTO-LOADING
  # ---------------------------------------------------------------------------
  entry_list_with_salaries <- reactive({
    req(values$analysis_entry_list, input$analysis_series)
    entry_list  <- values$analysis_entry_list
    prefix      <- series_salary_prefix(input$analysis_series)
    
    load_salary <- function(platforms, col_name, id_name) {
      for (pat in c(paste0(platforms, prefix, ".csv"),
                    paste0(platforms, "Salaries", prefix, ".csv"),
                    paste0(tolower(platforms), tolower(prefix), ".csv"))) {
        if (file.exists(pat)) {
          tryCatch({
            sal <- read_csv(pat, show_col_types = FALSE,
                            locale = locale(encoding = "UTF-8"))
            name_col <- if ("Nickname" %in% names(sal)) "Nickname" else "Name"
            id_col   <- if (platforms == "DK") "ID" else "Id"
            out <- sal %>% select(Name = all_of(name_col),
                                  !!col_name := Salary) %>%
              mutate(Name = safe_trimws(Name))
            if (id_col %in% names(sal))
              out[[id_name]] <- sal[[id_col]]
            return(out)
          }, error = function(e) NULL)
        }
      }
      NULL
    }
    
    dk_sal <- load_salary("DK", "DK_Salary", "DKID")
    fd_sal <- load_salary("FD", "FD_Salary", "FDID")
    
    entry_list <- entry_list %>% mutate(Name_Clean = safe_trimws(Name))
    
    if (!is.null(dk_sal)) {
      dk_sal <- dk_sal %>% mutate(Name_Clean = safe_trimws(Name))
      entry_list <- left_join(entry_list,
                              dk_sal %>% select(-Name),
                              by = "Name_Clean")
    }
    
    if (!is.null(fd_sal)) {
      fd_sal <- fd_sal %>% mutate(Name_Clean = safe_trimws(Name))
      entry_list <- left_join(entry_list,
                              fd_sal %>% select(-Name),
                              by = "Name_Clean")
    }
    
    entry_list <- entry_list %>% select(-Name_Clean)
    
    # Column order: base info, then salaries
    base_cols   <- c("Start", "Name", "Car", "Team", "Make", "CC", "Sponsor")
    salary_cols <- intersect(c("DK_Salary", "FD_Salary", "DKID", "FDID"), names(entry_list))
    entry_list %>% select(all_of(c(base_cols, salary_cols)))
  })
  
  # ---------------------------------------------------------------------------
  # ENTRY LIST OUTPUTS
  # ---------------------------------------------------------------------------
  output$entry_list_title <- renderUI({
    req(values$races_sheet, input$analysis_race_id, input$analysis_series)
    race_info <- values$races_sheet %>%
      filter(race_id == as.numeric(input$analysis_race_id)) %>%
      slice(1)
    race_name <- if (nrow(race_info) > 0) race_info$race_name else "Entry List"
    prefix    <- series_salary_prefix(input$analysis_series)
    dk_loaded <- file.exists(paste0("DK", prefix, ".csv"))
    fd_loaded <- file.exists(paste0("FD", prefix, ".csv"))
    suffix <- case_when(
      dk_loaded & fd_loaded ~ " — DK + FD Salaries Loaded",
      dk_loaded             ~ " — DK Salaries Loaded",
      fd_loaded             ~ " — FD Salaries Loaded",
      TRUE                  ~ ""
    )
    h3(paste0(race_name, " Entry List", suffix), class = "box-title")
  })
  
  output$entry_list_table <- DT::renderDataTable({
    req(entry_list_with_salaries())
    DT::datatable(entry_list_with_salaries(),
                  rownames = FALSE, class = "display nowrap compact",
                  options  = list(
                    pageLength = 40, scrollX = TRUE, dom = "tip",
                    columnDefs = list(list(className = "dt-center", targets = "_all"))
                  )
    )
  })
  
  output$download_entry_list_csv <- downloadHandler(
    filename = function() paste0("Entry_List_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(entry_list_with_salaries())
      write.csv(entry_list_with_salaries(), file, row.names = FALSE)
    }
  )
  
  output$download_entry_list_excel <- downloadHandler(
    filename    = function() paste0("Starting_Grid_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content     = function(file) {
      req(entry_list_with_salaries())
      el <- entry_list_with_salaries()
      wb <- createWorkbook()
      addWorksheet(wb, "Starting Grid")
      writeData(wb, "Starting Grid", el, rowNames = FALSE)
      addStyle(wb, "Starting Grid",
               createStyle(fontSize = 12, fontColour = "#000000", fgFill = "#FFE500",
                           halign = "center", valign = "center", textDecoration = "bold",
                           border = "TopBottomLeftRight", borderColour = "#000000"),
               rows = 1, cols = 1:ncol(el), gridExpand = TRUE)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ---------------------------------------------------------------------------
  # FINISH RATES
  # ---------------------------------------------------------------------------
  output$tier_config_ui <- renderUI({
    req(values$analysis_filtered_data)
    n         <- values$num_tiers
    all_teams <- sort(unique(values$analysis_filtered_data$team_name[
      !is.na(values$analysis_filtered_data$team_name)]))
    tagList(lapply(seq_len(n), function(i) {
      div(class = "tier-box",
          div(class = "tier-label", paste("Tier", i)),
          fluidRow(
            column(5, textInput(paste0("tier_name_", i), NULL,
                                value = paste0("Tier ", i), placeholder = "Tier name")),
            column(7, selectizeInput(paste0("tier_teams_", i), NULL,
                                     choices  = all_teams, multiple = TRUE,
                                     options  = list(placeholder = paste("Assign teams to Tier", i))))
          )
      )
    }))
  })
  
  observe({
    req(values$analysis_filtered_data, values$num_tiers)
    n         <- values$num_tiers
    all_teams <- sort(unique(values$analysis_filtered_data$team_name[
      !is.na(values$analysis_filtered_data$team_name)]))
    selections <- lapply(seq_len(n), function(i) input[[paste0("tier_teams_", i)]])
    for (i in seq_len(n)) {
      other_selected <- unlist(selections[-i])
      updateSelectizeInput(session, paste0("tier_teams_", i),
                           choices  = setdiff(all_teams, other_selected),
                           selected = selections[[i]])
    }
  })
  
  # FR view pill observers
  observe({
    for (val in c("driver", "car", "team", "start_pos", "tier")) {
      local({
        v <- val
        observeEvent(input[[paste0("frview_", v)]], {
          values$fr_view_sel <- v
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # FR time pill observers — "all" clears multi-select; years toggle
  observe({
    req(values$analysis_races_available)
    seasons <- as.character(sort(unique(values$analysis_races_available$race_season)))
    observeEvent(input[["frtime_all"]], {
      values$fr_seasons_sel <- "all"
    }, ignoreInit = TRUE)
    lapply(seasons, function(s) {
      observeEvent(input[[paste0("frtime_", s)]], {
        cur <- values$fr_seasons_sel
        if (identical(cur, "all")) {
          values$fr_seasons_sel <- s
        } else if (s %in% cur) {
          remaining <- setdiff(cur, s)
          values$fr_seasons_sel <- if (length(remaining) == 0) "all" else remaining
        } else {
          values$fr_seasons_sel <- c(cur, s)
        }
      }, ignoreInit = TRUE)
    })
  })
  
  observeEvent(input$add_tier,    { values$num_tiers <- min(values$num_tiers + 1, 8) })
  observeEvent(input$remove_tier, { values$num_tiers <- max(values$num_tiers - 1, 1) })
  
  finish_rates_data <- reactive({
    req(values$analysis_filtered_data)
    view_sel    <- values$fr_view_sel    %||% "driver"
    seasons_sel <- values$fr_seasons_sel %||% "all"
    
    data <- values$analysis_filtered_data
    
    # Season filter — multi-select pills
    if (!identical(seasons_sel, "all"))
      data <- data %>% filter(race_season %in% as.integer(seasons_sel))
    
    if (nrow(data) == 0) return(NULL)
    
    has_entry <- !is.null(values$analysis_entry_list) &&
      nrow(values$analysis_entry_list) > 0
    
    if (view_sel == "driver") {
      if (has_entry)
        data <- data %>% filter(Full_Name %in% values$analysis_entry_list$Name)
      calc_finish_rates(data, "Full_Name", "Driver") %>% arrange(`Avg Finish`)
      
    } else if (view_sel == "car") {
      data %>%
        mutate(car_entry = paste0("#", car_number, " (", team_name, ")")) %>%
        calc_finish_rates("car_entry", "Car") %>%
        arrange(`Avg Finish`)
      
    } else if (view_sel == "team") {
      calc_finish_rates(data, "team_name", "Team") %>% arrange(`Avg Finish`)
      
    } else if (view_sel == "start_pos") {
      # Group starting positions in buckets of 5
      data %>%
        filter(!is.na(start_ps)) %>%
        mutate(
          start_group = case_when(
            start_ps <=  5 ~ "P1-5",
            start_ps <= 10 ~ "P6-10",
            start_ps <= 15 ~ "P11-15",
            start_ps <= 20 ~ "P16-20",
            start_ps <= 25 ~ "P21-25",
            start_ps <= 30 ~ "P26-30",
            start_ps <= 35 ~ "P31-35",
            TRUE           ~ "P36+"
          )
        ) %>%
        calc_finish_rates("start_group", "Starting Position") %>%
        arrange(factor(`Starting Position`,
                       levels = c("P1-5","P6-10","P11-15","P16-20",
                                  "P21-25","P26-30","P31-35","P36+")))
      
    } else if (view_sel == "tier") {
      n   <- values$num_tiers
      tier_df <- bind_rows(lapply(seq_len(n), function(i) {
        teams <- input[[paste0("tier_teams_", i)]]
        name  <- input[[paste0("tier_name_",  i)]]
        if (is.null(teams) || length(teams) == 0) return(NULL)
        data.frame(team_name = teams, Tier = name, stringsAsFactors = FALSE)
      }))
      if (is.null(tier_df) || nrow(tier_df) == 0) return(NULL)
      data %>%
        inner_join(tier_df, by = "team_name") %>%
        calc_finish_rates("Tier", "Tier") %>%
        arrange(`Avg Finish`)
    }
  })
  
  output$finish_rates_table <- DT::renderDataTable({
    req(finish_rates_data())
    DT::datatable(finish_rates_data(),
                  rownames = FALSE,
                  filter   = "top",
                  class    = "display nowrap compact",
                  options  = list(
                    pageLength = 50, scrollX = TRUE, dom = "ltp",
                    columnDefs = list(list(className = "dt-center", targets = "_all"))
                  )
    )
  })
  
  output$download_finish_rates <- downloadHandler(
    filename = function() paste0("finish_rates_", values$fr_view_sel %||% "driver", "_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(finish_rates_data())
      write.csv(finish_rates_data(), file, row.names = FALSE)
    }
  )
  
  # ---------------------------------------------------------------------------
  # DOMINATOR
  # ---------------------------------------------------------------------------
  dominator_data <- reactive({
    req(values$analysis_filtered_data, dominator_filtered_races())
    data <- values$analysis_filtered_data %>%
      filter(race_id %in% dominator_filtered_races())
    # Fall back to computing from raw laps if DKSP/FDSP missing
    if (!"DKSP" %in% names(data) || all(is.na(data$DKSP))) {
      data <- data %>% group_by(race_id) %>%
        mutate(DKSP = fast_laps * 0.45 + lead_laps * 0.25,
               DKDomRank = dense_rank(desc(DKSP))) %>% ungroup()
    }
    if (!"FDSP" %in% names(data) || all(is.na(data$FDSP))) {
      data <- data %>% group_by(race_id) %>%
        mutate(FDSP = lead_laps * 0.1,
               FDDomRank = dense_rank(desc(FDSP))) %>% ungroup()
    }
    data
  })
  
  output$dominator_data_table <- DT::renderDataTable({
    req(dominator_data())
    dominator_data() %>%
      filter(DKSP > 0 | FDSP > 0) %>%
      transmute(
        Driver       = Full_Name,
        Start        = start_ps,
        Finish       = ps,
        Team         = team_name,
        `Laps Led`   = lead_laps,
        `Fast Laps`  = fast_laps,
        `DK Dom Pts` = round(DKSP, 1),
        `DK Dom Rank`= DKDomRank,
        `FD Dom Pts` = round(FDSP, 1),
        `FD Dom Rank`= FDDomRank,
        Season       = race_season,
        Race         = race_name,
        Track        = track_name
      ) %>%
      arrange(desc(`DK Dom Pts`)) %>%
      DT::datatable(
        rownames = FALSE, filter = "top",
        class    = "display nowrap compact",
        options  = list(
          pageLength = 25, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      )
  })
  
  output$download_dominator_csv <- downloadHandler(
    filename = function() paste0("dominator_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(dominator_data())
      dominator_data() %>%
        filter(DKSP > 0 | FDSP > 0) %>%
        transmute(Driver = Full_Name, Start = start_ps, Finish = ps,
                  Team = team_name, `Laps Led` = lead_laps,
                  `Fast Laps` = fast_laps,
                  `DK Dom Pts` = round(DKSP, 1), `DK Dom Rank` = DKDomRank,
                  `FD Dom Pts` = round(FDSP, 1), `FD Dom Rank` = FDDomRank,
                  Season = race_season, Race = race_name, Track = track_name) %>%
        write.csv(file, row.names = FALSE)
    }
  )
  
  output$download_dominator_profile <- downloadHandler(
    filename    = function() paste0("dominator_profile_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content     = function(file) {
      req(dominator_data(), values$analysis_races_available)
      n_races <- length(dominator_filtered_races())
      fd_laps <- dominator_data() %>%
        filter(!is.na(ps), !is.na(FDLP)) %>%
        group_by(ps) %>%
        summarise(Pt = round(mean(FDLP, na.rm = TRUE), 1), .groups = "drop") %>%
        arrange(ps) %>% rename(PS = ps)
      race_weights <- values$analysis_races_available %>%
        filter(race_id %in% dominator_filtered_races()) %>%
        transmute(RaceID = race_id, RaceName = race_name,
                  Season = race_season, Track = track_name,
                  Weight = round(1 / n_races, 4)) %>%
        arrange(Season, Track)
      race_profiles <- dominator_data() %>%
        filter(DKSP > 0) %>%
        transmute(RaceID = race_id, StartPos = start_ps, FinPos = ps,
                  LeadLaps = lead_laps, FastLaps = fast_laps,
                  DKDomPoints = round(DKSP, 1), FDDomPoints = round(FDSP, 1),
                  TrackName = track_name, Driver = Full_Name, Team = team_name) %>%
        arrange(desc(DKDomPoints))
      wb <- createWorkbook()
      gold_hdr <- createStyle(fontSize = 11, fontColour = "#000000",
                              fgFill = "#FFE500", halign = "center", valign = "center",
                              textDecoration = "bold", border = "TopBottomLeftRight",
                              borderColour = "#000000")
      for (sname in c("FDLaps", "Race_Weights", "Race_Profiles")) {
        df <- list(FDLaps = fd_laps, Race_Weights = race_weights,
                   Race_Profiles = race_profiles)[[sname]]
        addWorksheet(wb, sname)
        writeData(wb, sname, df)
        addStyle(wb, sname, gold_hdr, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
        setColWidths(wb, sname, cols = 1:ncol(df), widths = "auto")
      }
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_input_file <- downloadHandler(
    filename    = function() paste0("NASCAR_Sim_Input_", Sys.Date(), ".xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content     = function(file) {
      req(dominator_data(), values$analysis_races_available,
          values$analysis_entry_list)
      el <- entry_list_with_salaries()
      # Filter to drivers with at least one salary
      sal_cols <- intersect(c("DK_Salary", "FD_Salary"), names(el))
      if (length(sal_cols) > 0)
        el <- el %>% filter(if_any(all_of(sal_cols), ~!is.na(.)))
      
      driver_sheet <- el %>%
        mutate(FDName = Name, DKName = Name)
      if (!"DKID"      %in% names(driver_sheet)) driver_sheet$DKID      <- NA_character_
      if (!"FDID"      %in% names(driver_sheet)) driver_sheet$FDID      <- NA_character_
      if (!"DK_Salary" %in% names(driver_sheet)) driver_sheet$DKSalary  <- NA_real_ else
        driver_sheet <- driver_sheet %>% rename(DKSalary = DK_Salary)
      if (!"FD_Salary" %in% names(driver_sheet)) driver_sheet$FDSalary  <- NA_real_ else
        driver_sheet <- driver_sheet %>% rename(FDSalary = FD_Salary)
      driver_sheet <- driver_sheet %>%
        mutate(DKOP = NA_real_, FDOP = NA_real_) %>%
        select(FDName, DKName, Name, DKID, FDID, Car, Team,
               DKSalary, FDSalary, Start, DKOP, FDOP)
      
      n_races      <- length(dominator_filtered_races())
      race_profiles <- dominator_data() %>%
        filter(DKSP > 0) %>%
        transmute(RaceID = race_id, StartPos = start_ps, FinPos = ps,
                  LeadLaps = lead_laps, FastLaps = fast_laps,
                  DKDomPoints = round(DKSP, 1), FDDomPoints = round(FDSP, 1),
                  TrackName = track_name, Driver = Full_Name, Team = team_name) %>%
        arrange(desc(DKDomPoints))
      race_weights <- values$analysis_races_available %>%
        filter(race_id %in% dominator_filtered_races()) %>%
        transmute(RaceID = race_id, RaceName = race_name,
                  Season = race_season, Track = track_name,
                  Weight = round(1 / n_races, 4)) %>%
        arrange(Season, Track)
      fd_laps <- dominator_data() %>%
        filter(!is.na(ps), !is.na(FDLP)) %>%
        group_by(ps) %>%
        summarise(Pt = round(mean(FDLP, na.rm = TRUE), 1), .groups = "drop") %>%
        arrange(ps) %>% rename(PS = ps)
      
      wb      <- createWorkbook()
      gold_hdr <- createStyle(fontSize = 11, fontColour = "#000000",
                              fgFill = "#FFE500", halign = "center", valign = "center",
                              textDecoration = "bold", border = "TopBottomLeftRight",
                              borderColour = "#000000")
      sheets <- list(Driver = driver_sheet, Race_Profiles = race_profiles,
                     Race_Weights = race_weights, FDLaps = fd_laps)
      for (sname in names(sheets)) {
        df <- sheets[[sname]]
        addWorksheet(wb, sname)
        writeData(wb, sname, df)
        addStyle(wb, sname, gold_hdr, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
        setColWidths(wb, sname, cols = 1:ncol(df), widths = "auto")
      }
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ---------------------------------------------------------------------------
  # DOMINATOR VISUALIZATIONS
  # ---------------------------------------------------------------------------
  # Shared color grouping helpers for all charts
  group_palette <- c(
    "#FFE500", "#4caf50", "#2196f3", "#f44336",
    "#ff9800", "#9c27b0", "#00bcd4", "#e91e63",
    "#8bc34a", "#ff5722", "#607d8b", "#795548"
  )
  
  # Helper: apply color grouping to a plotly horizontal box chart
  # data must have a Grp factor column and the x value column
  # color_by: "none" | "track_type" | "race_season" | "track_name"
  make_colored_box <- function(data, x_col, title_txt, x_label,
                               color_by = "none", margin_l = 150,
                               grp_order = NULL) {
    if (is.null(grp_order))
      grp_order <- levels(data$Grp)
    
    if (color_by == "none") {
      p <- plot_ly(data = data, type = "box",
                   y = ~Grp, x = ~get(x_col), orientation = "h",
                   marker    = list(color = "#FFE500", opacity = 0.6),
                   line      = list(color = "#D4B000"),
                   fillcolor = "rgba(255,229,0,0.25)",
                   showlegend = FALSE)
    } else {
      grp_vals  <- sort(unique(as.character(data[[color_by]])))
      color_map <- setNames(
        group_palette[seq_along(grp_vals) %% length(group_palette) + 1],
        grp_vals)
      traces <- lapply(grp_vals, function(gv) {
        sub <- data %>% filter(as.character(.data[[color_by]]) == gv)
        hex <- color_map[[gv]]
        # convert hex to rgba for fill
        r <- strtoi(substr(hex,2,3),16)
        g <- strtoi(substr(hex,4,5),16)
        b <- strtoi(substr(hex,6,7),16)
        fill_rgba <- sprintf("rgba(%d,%d,%d,0.25)", r, g, b)
        plot_ly(data = sub, type = "box",
                y = ~Grp, x = ~get(x_col), orientation = "h",
                name      = gv,
                marker    = list(color = hex, opacity = 0.7),
                line      = list(color = hex),
                fillcolor = fill_rgba)
      })
      p <- do.call(subplot, c(traces, list(shareX = TRUE, shareY = TRUE, nrows = 1)))
      # rebuild as overlay — subplot doesn't give us what we want; use add_trace
      p <- plot_ly()
      for (gv in grp_vals) {
        sub <- data %>% filter(as.character(.data[[color_by]]) == gv)
        hex <- color_map[[gv]]
        r <- strtoi(substr(hex,2,3),16); g <- strtoi(substr(hex,4,5),16); b <- strtoi(substr(hex,6,7),16)
        fill_rgba <- sprintf("rgba(%d,%d,%d,0.25)", r, g, b)
        p <- p %>% add_trace(data = sub, type = "box",
                             y = ~Grp, x = ~get(x_col), orientation = "h",
                             name      = gv,
                             marker    = list(color = hex, opacity = 0.7),
                             line      = list(color = hex),
                             fillcolor = fill_rgba)
      }
    }
    
    col_label <- switch(color_by,
                        track_type   = "Track Type",
                        race_season  = "Season",
                        track_name   = "Track",
                        NULL)
    
    p %>% layout(
      title       = list(text = title_txt, font = list(size = 18, color = "#FFE500")),
      boxmode     = "overlay",
      xaxis       = list(title = x_label, color = "#ffffff", gridcolor = "#404040",
                         zerolinecolor = "#555555"),
      yaxis       = list(title = "", color = "#ffffff",
                         categoryorder = "array", categoryarray = rev(grp_order)),
      legend      = list(title = list(text = col_label),
                         font = list(color = "#ffffff"),
                         bgcolor = "rgba(0,0,0,0)"),
      paper_bgcolor = "#1e1e1e", plot_bgcolor = "#1e1e1e",
      font        = list(color = "#ffffff"),
      height      = 900,
      margin      = list(l = margin_l, r = 20, t = 50, b = 40))
  }
  
  output$dominator_plot <- renderPlotly({
    req(dominator_data(), input$dom_visual_type, input$dom_platform)
    plot_data     <- dominator_data()
    platform      <- input$dom_platform
    dom_pts_col   <- if (platform == "DK") "DKSP"      else "FDSP"
    dom_rank_col  <- if (platform == "DK") "DKDomRank" else "FDDomRank"
    platform_name <- if (platform == "DK") "DraftKings" else "FanDuel"
    
    dark_theme <- theme_minimal() + theme(
      plot.title       = element_text(size = 18, face = "bold", color = "#FFE500"),
      axis.title       = element_text(size = 16, color = "#ffffff"),
      axis.text        = element_text(size = 14, color = "#ffffff"),
      panel.background = element_rect(fill = "#1e1e1e"),
      plot.background  = element_rect(fill = "#1e1e1e"),
      panel.grid.major = element_line(color = "#404040"),
      panel.grid.minor = element_line(color = "#333333"))
    
    dark_layout <- function(p) {
      p %>% layout(paper_bgcolor = "#2d2d2d", plot_bgcolor = "#2d2d2d",
                   font   = list(color = "#ffffff"),
                   xaxis  = list(gridcolor = "#404040", zerolinecolor = "#666666"),
                   yaxis  = list(gridcolor = "#404040", zerolinecolor = "#666666"))
    }
    
    vt <- input$dom_visual_type
    
    if (vt == "score_dist") {
      viz <- plot_data %>% filter(!!sym(dom_rank_col) <= 10, !!sym(dom_pts_col) > 0)
      p <- ggplot(viz, aes(x = factor(!!sym(dom_rank_col)), y = !!sym(dom_pts_col))) +
        geom_boxplot(aes(text = sprintf("Dom Rank: %d\nDom Pts: %.1f\nDriver: %s\nTrack: %s",
                                        !!sym(dom_rank_col), !!sym(dom_pts_col), Full_Name, track_name)),
                     fill = "#FFE500", alpha = 0.7) +
        labs(title = paste(platform_name, "Dom Points by Dom Rank (Top 10)"),
             x = "Dom Rank", y = "Dom Points") +
        coord_flip() + scale_x_discrete(limits = factor(10:1)) + dark_theme
      ggplotly(p, tooltip = "text", height = 700) %>% dark_layout()
      
    } else if (vt == "rank_finish") {
      viz <- plot_data %>% filter(!!sym(dom_rank_col) <= 10, !!sym(dom_pts_col) > 0)
      p <- ggplot(viz, aes(x = factor(!!sym(dom_rank_col)), y = ps)) +
        geom_boxplot(aes(text = sprintf("Dom Rank: %d\nFinish: %d\nDriver: %s\nTrack: %s",
                                        !!sym(dom_rank_col), ps, Full_Name, track_name)),
                     fill = "#FFE500", alpha = 0.7) +
        geom_smooth(aes(x = as.numeric(!!sym(dom_rank_col)), y = ps, group = 1),
                    method = "loess", se = FALSE, color = "#FFE500", linewidth = 1.5) +
        labs(title = paste("Where Have Top", platform_name, "Dominators Finished"),
             x = "Dom Rank", y = "Finish Position") +
        scale_x_discrete(limits = factor(1:10)) +
        scale_y_continuous(breaks = seq(0, 40, 5)) + dark_theme
      ggplotly(p, tooltip = "text", height = 700) %>% dark_layout()
      
    } else if (vt %in% c("pts_by_finish", "dom_pts_start", "dom_rank_start",
                         "laps_led", "laps_led_start", "fast_laps", "fast_laps_start")) {
      cfg <- list(
        pts_by_finish   = list(x = "ps",       y = dom_pts_col,  fill = "#FFE500", xt = "Finish Position",   yt = "Dom Points",  ti = paste(platform_name, "Dom Pts by Finish")),
        dom_pts_start   = list(x = "start_ps", y = dom_pts_col,  fill = "#FFE500", xt = "Starting Position", yt = "Dom Points",  ti = paste(platform_name, "Dom Pts by Start")),
        dom_rank_start  = list(x = "start_ps", y = dom_rank_col, fill = "#FFE500", xt = "Starting Position", yt = "Dom Rank",    ti = paste(platform_name, "Dom Rank by Start")),
        laps_led        = list(x = "ps",       y = "lead_laps",  fill = "#DAA520", xt = "Finish Position",   yt = "Laps Led",    ti = "Laps Led by Finish"),
        laps_led_start  = list(x = "start_ps", y = "lead_laps",  fill = "#DAA520", xt = "Starting Position", yt = "Laps Led",    ti = "Laps Led by Start"),
        fast_laps       = list(x = "ps",       y = "fast_laps",  fill = "#FFE500", xt = "Finish Position",   yt = "Fast Laps",   ti = "Fast Laps by Finish"),
        fast_laps_start = list(x = "start_ps", y = "fast_laps",  fill = "#FFE500", xt = "Starting Position", yt = "Fast Laps",   ti = "Fast Laps by Start")
      )
      cc  <- cfg[[vt]]
      viz <- plot_data %>%
        filter(!!sym(cc$x) <= 40, !is.na(!!sym(cc$x)), !is.na(!!sym(cc$y)))
      p <- ggplot(viz, aes(x = factor(!!sym(cc$x)), y = !!sym(cc$y))) +
        geom_boxplot(fill = cc$fill, alpha = 0.7) +
        labs(title = cc$ti, x = cc$xt, y = cc$yt) +
        coord_flip() + scale_x_discrete(limits = factor(40:1)) + dark_theme
      ggplotly(p, height = 900) %>% dark_layout()
      
    } else if (vt %in% c("driver_boxplot", "team_boxplot")) {
      req(values$analysis_entry_list)
      entry_drivers <- values$analysis_entry_list$Name
      entry_teams   <- unique(values$analysis_entry_list$Team)
      
      color_by_dom <- input$dom_color_by %||% "none"
      
      make_dom_box <- function(data, grp_col, title_txt, margin_l = 150) {
        grp_order <- data %>%
          filter(!is.na(!!sym(dom_pts_col))) %>%
          group_by(!!sym(grp_col)) %>%
          summarise(med = median(!!sym(dom_pts_col), na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(med)) %>% pull(!!sym(grp_col))
        data %>%
          filter(!!sym(grp_col) %in% grp_order, !is.na(!!sym(dom_pts_col))) %>%
          mutate(Grp = factor(!!sym(grp_col), levels = grp_order)) %>%
          make_colored_box(dom_pts_col, title_txt, "Dom Points",
                           color_by = color_by_dom, margin_l = margin_l,
                           grp_order = grp_order)
      }
      
      if (vt == "driver_boxplot") {
        make_dom_box(plot_data %>% filter(Full_Name %in% entry_drivers),
                     "Full_Name", paste(platform_name, "Dom Points by Driver"))
      } else {
        make_dom_box(plot_data %>% filter(team_name %in% entry_teams),
                     "team_name", paste("Team", platform_name, "Dom Points"), margin_l = 180)
      }
    }
  })
  
  # ---------------------------------------------------------------------------
  # PLACE DIFFERENTIAL
  # ---------------------------------------------------------------------------
  pd_data <- reactive({
    req(values$analysis_filtered_data, performance_race_ids_reactive())
    values$analysis_filtered_data %>%
      filter(race_id %in% performance_race_ids_reactive(),
             !is.na(start_ps), !is.na(ps)) %>%
      mutate(PD = start_ps - ps)
  })
  
  output$pd_data_table <- DT::renderDataTable({
    req(pd_data())
    pd_data() %>%
      transmute(Driver = Full_Name, Start = start_ps, Finish = ps, PD,
                Team = team_name, ARP = round(ARP, 1),
                Season = race_season, Race = race_name, Track = track_name) %>%
      arrange(desc(PD)) %>%
      DT::datatable(
        rownames = FALSE, filter = "top",
        class    = "display nowrap compact",
        options  = list(
          pageLength = 25, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      )
  })
  
  output$download_pd_csv <- downloadHandler(
    filename = function() paste0("place_differential_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(pd_data())
      pd_data() %>%
        transmute(Driver = Full_Name, Start = start_ps, Finish = ps, PD,
                  Team = team_name, ARP = round(ARP, 1),
                  Season = race_season, Race = race_name, Track = track_name) %>%
        write.csv(file, row.names = FALSE)
    }
  )
  
  output$pd_plot <- renderPlotly({
    req(pd_data(), input$pd_visual_type)
    plot_data    <- pd_data()
    color_by_pd  <- input$pd_color_by %||% "none"
    dark_theme <- theme_minimal() + theme(
      plot.title       = element_text(size = 20, face = "bold", color = "#FFE500"),
      axis.title       = element_text(size = 16, color = "#ffffff"),
      axis.text        = element_text(size = 14, color = "#ffffff"),
      panel.background = element_rect(fill = "#1e1e1e"),
      plot.background  = element_rect(fill = "#1e1e1e"),
      panel.grid.major = element_line(color = "#404040"),
      panel.grid.minor = element_line(color = "#333333"))
    dark_layout <- function(p) p %>% layout(
      paper_bgcolor = "#2d2d2d", plot_bgcolor = "#2d2d2d",
      font  = list(color = "#ffffff"),
      xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
      yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"))
    
    if (input$pd_visual_type == "scatter") {
      viz <- plot_data %>% filter(start_ps <= 40, ps <= 40)
      p <- ggplot(viz, aes(x = start_ps, y = ps, size = abs(PD), color = PD,
                           text = sprintf("Driver: %s\nStart: %d\nFinish: %d\nPD: %d\nTrack: %s",
                                          Full_Name, start_ps, ps, PD, track_name))) +
        geom_point(alpha = 0.6) +
        geom_abline(linetype = "dashed", color = "#FFE500", linewidth = 1.2) +
        scale_color_gradient2(low = "#DC143C", mid = "#cccccc", high = "#FFE500",
                              midpoint = 0, name = "PD") +
        scale_size_continuous(range = c(2, 12)) +
        scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
        scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
        labs(title = "Starting vs Finishing Position",
             x = "Starting Position", y = "Finishing Position") + dark_theme
      ggplotly(p, tooltip = "text", height = 700) %>% dark_layout()
      
    } else if (input$pd_visual_type == "histogram") {
      p <- ggplot(plot_data, aes(x = PD)) +
        geom_histogram(binwidth = 1, fill = "#FFE500", color = "#000000", alpha = 0.8) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "#FF0000", linewidth = 1.5) +
        labs(title = "Position Change Distribution",
             x = "Place Differential", y = "Count") + dark_theme
      ggplotly(p, height = 700) %>% dark_layout()
      
    } else {
      x_col   <- if (input$pd_visual_type == "boxplot_start") "start_ps" else "ps"
      x_label <- if (input$pd_visual_type == "boxplot_start") "Starting Position" else "Finishing Position"
      title   <- paste("Place Differential by", x_label)
      viz     <- plot_data %>%
        filter(!!sym(x_col) <= 40, !is.na(!!sym(x_col))) %>%
        mutate(Grp = factor(as.character(!!sym(x_col)),
                            levels = as.character(1:40)))
      grp_ord <- as.character(1:40)
      make_colored_box(viz, "PD", title, "Place Differential",
                       color_by = color_by_pd, margin_l = 80,
                       grp_order = grp_ord)
    }
  })
  
  # ---------------------------------------------------------------------------
  # PERFORMANCE
  # ---------------------------------------------------------------------------
  performance_data <- reactive({
    req(values$analysis_filtered_data, performance_race_ids_reactive())
    req(!is.null(input$perf_time_filter))
    data <- values$analysis_filtered_data %>%
      filter(race_id %in% performance_race_ids_reactive())
    if (input$perf_time_filter != "all")
      data <- data %>% filter(race_season == as.integer(input$perf_time_filter))
    data
  })
  
  output$performance_data_table <- DT::renderDataTable({
    req(performance_data())
    performance_data() %>%
      select(any_of(c("Full_Name", "start_ps", "ps", "ARP", "SpdRk",
                      "fast_laps", "lead_laps", "DKSP", "FDSP",
                      "DKDomRank", "FDDomRank", "DKPoints", "FDPoints",
                      "car_number", "team_name", "race_season",
                      "track_name", "finishing_status", "LapsDown"))) %>%
      mutate(across(any_of(c("ARP", "DKSP", "FDSP", "DKPoints", "FDPoints")),
                    ~round(., 1))) %>%
      rename_with(~recode(.,
                          Full_Name       = "Driver",   start_ps    = "Start",    ps        = "Finish",
                          fast_laps       = "FL",       lead_laps   = "LL",       DKSP      = "DK Dom Pts",
                          FDSP            = "FD Dom Pts", DKPoints  = "DK Pts",   FDPoints  = "FD Pts",
                          DKDomRank       = "DK Dom Rank", FDDomRank = "FD Dom Rank",
                          car_number      = "Car",      team_name   = "Team",     race_season = "Season",
                          track_name      = "Track",    finishing_status = "Status", LapsDown = "Laps Down"
      )) %>%
      DT::datatable(
        rownames = FALSE, filter = "top",
        class    = "display nowrap compact",
        options  = list(
          pageLength = 25, scrollX = TRUE, dom = "rtip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      )
  })
  
  output$download_performance_csv <- downloadHandler(
    filename = function() paste0("performance_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(performance_data())
      write.csv(performance_data(), file, row.names = FALSE)
    }
  )
  
  output$performance_plot <- renderPlotly({
    req(values$analysis_filtered_data, performance_race_ids_reactive(),
        input$perf_visual_type, values$analysis_entry_list)
    req(!is.null(input$perf_visual_time))
    
    viz_data <- values$analysis_filtered_data %>%
      filter(race_id %in% performance_race_ids_reactive())
    if (input$perf_visual_time != "all")
      viz_data <- viz_data %>% filter(race_season == as.integer(input$perf_visual_time))
    
    time_label    <- if (input$perf_visual_time == "all") "Full History" else input$perf_visual_time
    entry_drivers <- values$analysis_entry_list$Name
    entry_teams   <- unique(values$analysis_entry_list$Team)
    
    color_by_perf <- input$perf_color_by %||% "none"
    
    make_perf_box <- function(data, grp_col, val_col, title_txt, x_label,
                              ascending = TRUE, margin_l = 150) {
      grp_order <- data %>%
        filter(!is.na(!!sym(val_col))) %>%
        group_by(!!sym(grp_col)) %>%
        summarise(m = mean(!!sym(val_col), na.rm = TRUE), .groups = "drop") %>%
        { if (ascending) arrange(., desc(m)) else arrange(., m) } %>%
        pull(!!sym(grp_col))
      data %>%
        filter(!!sym(grp_col) %in% grp_order, !is.na(!!sym(val_col))) %>%
        mutate(Grp = factor(!!sym(grp_col), levels = grp_order)) %>%
        make_colored_box(val_col,
                         paste(title_txt, "—", time_label), x_label,
                         color_by = color_by_perf, margin_l = margin_l,
                         grp_order = grp_order)
    }
    
    switch(input$perf_visual_type,
           driver_speed  = make_perf_box(viz_data %>% filter(Full_Name %in% entry_drivers, !is.na(SpdRk)),
                                         "Full_Name", "SpdRk", "Speed Rank by Driver",    "Speed Rank"),
           team_speed    = make_perf_box(viz_data %>% filter(team_name %in% entry_teams,  !is.na(SpdRk)),
                                         "team_name", "SpdRk", "Team Speed Rank",          "Speed Rank"),
           driver_finish = make_perf_box(viz_data %>% filter(Full_Name %in% entry_drivers, !is.na(ps)),
                                         "Full_Name", "ps",    "Finish Distribution by Driver", "Finish Position"),
           team_finish   = make_perf_box(viz_data %>% filter(team_name %in% entry_teams,  !is.na(ps)),
                                         "team_name", "ps",    "Team Finish Distribution", "Finish Position"),
           driver_arp    = make_perf_box(viz_data %>% filter(Full_Name %in% entry_drivers, !is.na(ARP)),
                                         "Full_Name", "ARP",   "ARP by Driver",            "Avg Running Position"),
           team_arp      = make_perf_box(viz_data %>% filter(team_name %in% entry_teams,  !is.na(ARP)),
                                         "team_name", "ARP",   "Team ARP Distribution",    "Avg Running Position")
    )
  })
  
  # ---------------------------------------------------------------------------
  # FANTASY SCORING
  # ---------------------------------------------------------------------------
  fantasy_data <- reactive({
    req(values$analysis_filtered_data, dominator_filtered_races())
    values$analysis_filtered_data %>%
      filter(race_id %in% dominator_filtered_races())
  })
  
  output$fantasy_data_table <- DT::renderDataTable({
    req(fantasy_data(), input$fs_platform)
    if (input$fs_platform == "DK") {
      fantasy_data() %>%
        filter(DKRank <= 25) %>%
        transmute(Driver = Full_Name, Rank = DKRank,
                  `Total Pts` = round(DKPoints, 1), `Finish Pts` = round(DKFP, 1),
                  `PD Pts` = round(DKPD, 1), `Dom Pts` = round(DKSP, 1),
                  Finish = ps, Start = start_ps,
                  `Laps Led` = lead_laps, `Fast Laps` = fast_laps,
                  Race = race_name, Track = track_name, Season = race_season) %>%
        arrange(desc(`Total Pts`))
    } else {
      fantasy_data() %>%
        filter(FDRank <= 25) %>%
        transmute(Driver = Full_Name, Rank = FDRank,
                  `Total Pts` = round(FDPoints, 1), `Finish Pts` = round(FDFP, 1),
                  `PD Pts` = round(FDPD, 1), `Dom Pts` = round(FDSP, 1),
                  `Lap Pts` = round(FDLP, 1),
                  Finish = ps, Start = start_ps, `Laps Led` = lead_laps,
                  Race = race_name, Track = track_name, Season = race_season) %>%
        arrange(desc(`Total Pts`))
    } %>%
      DT::datatable(
        rownames  = FALSE,
        class     = "display nowrap compact",
        options   = list(
          pageLength = 25, scrollX = TRUE, dom = "tip",
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      )
  })
  
  output$download_fantasy_csv <- downloadHandler(
    filename = function() paste0("fantasy_", input$fs_platform, "_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(fantasy_data())
      write.csv(fantasy_data(), file, row.names = FALSE)
    }
  )
  
  output$fantasy_plot <- renderPlotly({
    req(fantasy_data(), input$fs_visual_type, input$fs_visual_platform)
    plot_data     <- fantasy_data()
    color_by_fs   <- input$fs_color_by %||% "none"
    platform      <- input$fs_visual_platform
    points_col    <- if (platform == "DK") "DKPoints" else "FDPoints"
    rank_col      <- if (platform == "DK") "DKRank"   else "FDRank"
    platform_name <- if (platform == "DK") "DraftKings" else "FanDuel"
    
    dark_theme <- theme_minimal() + theme(
      plot.title       = element_text(size = 18, face = "bold", color = "#FFE500"),
      axis.title       = element_text(size = 16, color = "#ffffff"),
      axis.text        = element_text(size = 14, color = "#ffffff"),
      panel.background = element_rect(fill = "#1e1e1e"),
      plot.background  = element_rect(fill = "#1e1e1e"),
      panel.grid.major = element_line(color = "#404040"),
      panel.grid.minor = element_line(color = "#333333"),
      legend.background = element_rect(fill = "#2d2d2d"),
      legend.key        = element_rect(fill = "#2d2d2d"),
      legend.text       = element_text(color = "#ffffff"),
      legend.title      = element_text(color = "#FFE500"))
    dark_layout <- function(p) p %>% layout(
      paper_bgcolor = "#2d2d2d", plot_bgcolor = "#2d2d2d",
      font  = list(color = "#ffffff"),
      xaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"),
      yaxis = list(gridcolor = "#404040", zerolinecolor = "#666666"))
    
    vt <- input$fs_visual_type
    
    if (vt == "score_dist") {
      viz <- plot_data %>% filter(!!sym(rank_col) <= 15)
      p <- ggplot(viz, aes(x = factor(!!sym(rank_col)), y = !!sym(points_col))) +
        geom_boxplot(aes(text = sprintf("Rank: %d\nPts: %.1f\nDriver: %s\nTrack: %s",
                                        !!sym(rank_col), !!sym(points_col), Full_Name, track_name)),
                     fill = "#3a6ea5", alpha = 0.8) +
        geom_smooth(aes(x = as.numeric(!!sym(rank_col)), y = !!sym(points_col), group = 1),
                    method = "loess", se = FALSE, color = "#FFE500", linewidth = 1.5) +
        labs(title = paste(platform_name, "Points by Fantasy Rank"),
             x = "Rank", y = "Points") +
        scale_x_discrete(limits = factor(1:15)) + dark_theme
      ggplotly(p, tooltip = "text", height = 700) %>% dark_layout()
      
    } else if (vt == "components") {
      make_comp <- function(d) {
        if (platform == "DK")
          d %>% mutate(
            Finish_Pct = round(DKFP / DKPoints * 100, 1),
            PD_Pct     = round(DKPD / DKPoints * 100, 1),
            Dom_Pct    = round(DKSP / DKPoints * 100, 1))
        else
          d %>% mutate(
            Finish_Pct = round(FDFP / (FDPoints - FDLP) * 100, 1),
            PD_Pct     = round(FDPD / (FDPoints - FDLP) * 100, 1),
            Dom_Pct    = round(FDSP / (FDPoints - FDLP) * 100, 1))
      }
      grp_col <- rank_col
      filt    <- plot_data %>% filter(!!sym(rank_col) <= 15)
      comp_data <- make_comp(filt) %>%
        group_by(!!sym(grp_col)) %>%
        summarise(FP = mean(Finish_Pct, na.rm = TRUE),
                  PD = mean(PD_Pct,     na.rm = TRUE),
                  Dom= mean(Dom_Pct,    na.rm = TRUE), .groups = "drop") %>%
        pivot_longer(cols = c(FP, PD, Dom), names_to = "Type", values_to = "Pct") %>%
        mutate(Type = recode(Type, FP = "Finish Position",
                             PD = "Place Differential", Dom = "Dominator Points"))
      p <- ggplot(comp_data, aes(x = factor(!!sym(grp_col)), y = Pct, fill = Type)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = sprintf("%.0f%%", Pct)),
                  position = position_stack(vjust = 0.5),
                  color = "white", fontface = "bold", size = 3) +
        scale_fill_manual(values = c(
          "Finish Position"    = "#1a1a1a",
          "Place Differential" = "#DAA520",
          "Dominator Points"   = "#FFE500")) +
        labs(title = paste(platform_name, "Scoring Components"),
             x = grp_col, y = "%", fill = "Type") + dark_theme
      ggplotly(p, tooltip = c("x", "y", "fill"), height = 700) %>% dark_layout()
      
    } else {
      filt_col  <- if (vt == "score_by_start") "start_ps" else "ps"
      fill_col  <- if (vt == "score_by_start") "#4a6fa5" else "#5a9e6f"
      x_label   <- if (vt == "score_by_start") "Starting Position" else "Finish Position"
      viz <- plot_data %>%
        filter(!!sym(filt_col) <= 40, !is.na(!!sym(filt_col)), !is.na(!!sym(points_col)))
      p <- ggplot(viz, aes(x = factor(!!sym(filt_col)), y = !!sym(points_col))) +
        geom_boxplot(fill = fill_col, color = "#FFE500", alpha = 0.7) +
        labs(title = paste(platform_name, "Points by", x_label),
             x = x_label, y = "Points") +
        scale_x_discrete(limits = factor(1:40)) + dark_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#ffffff"))
      ggplotly(p, height = 700) %>% dark_layout()
    }
  })
  
} # end server

shinyApp(ui = ui, server = server)