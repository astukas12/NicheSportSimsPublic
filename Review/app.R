# Golden Ticket Sims - Unified Review App
# Black & Yellow Professional Theme

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

# Scan directories for contest files
scan_contest_files <- function() {
  # Use getwd() to get current working directory where app is running
  base_dir <- getwd()
  optimals_dir <- file.path(base_dir, "optimals")
  contests_dir <- file.path(base_dir, "contests")
  
  if (!dir.exists(optimals_dir) || !dir.exists(contests_dir)) {
    return(data.frame(
      sport = character(),
      date = character(),
      optimal_file = character(),
      contest_file = character(),
      optimal_display = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Get all files
  optimal_files <- list.files(optimals_dir, pattern = "\\.csv$", full.names = FALSE)
  contest_files <- list.files(contests_dir, pattern = "\\.csv$", full.names = FALSE)
  
  # Parse optimal files
  optimal_data <- lapply(optimal_files, function(f) {
    # Extract _SPORT-DATE.csv or _SPORT_DATE.csv from end
    # Handles formats like: _F1-11-9-25.csv or _F1_11-9-25.csv
    match <- str_match(f, "_([A-Z]+)[-_]([\\d\\-]+)\\.csv$")
    if (!is.na(match[1])) {
      return(data.frame(
        sport = match[2],
        date = match[3],
        optimal_file = file.path(optimals_dir, f),
        optimal_display = f,
        stringsAsFactors = FALSE
      ))
    }
    return(NULL)
  })
  optimal_data <- do.call(rbind, optimal_data[!sapply(optimal_data, is.null)])
  
  if (is.null(optimal_data) || nrow(optimal_data) == 0) {
    return(data.frame(
      sport = character(),
      date = character(),
      optimal_file = character(),
      contest_file = character(),
      optimal_display = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Parse contest files
  contest_data <- lapply(contest_files, function(f) {
    # Handles formats like: _F1-11-9-25.csv or _F1_11-9-25.csv
    match <- str_match(f, "_([A-Z]+)[-_]([\\d\\-]+)\\.csv$")
    if (!is.na(match[1])) {
      return(data.frame(
        sport = match[2],
        date = match[3],
        contest_file = file.path(contests_dir, f),
        stringsAsFactors = FALSE
      ))
    }
    return(NULL)
  })
  contest_data <- do.call(rbind, contest_data[!sapply(contest_data, is.null)])
  
  if (is.null(contest_data) || nrow(contest_data) == 0) {
    return(data.frame(
      sport = character(),
      date = character(),
      optimal_file = character(),
      contest_file = character(),
      optimal_display = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Match pairs
  matched <- optimal_data %>%
    inner_join(contest_data, by = c("sport", "date"))
  
  return(matched)
}

# Detect sport from column names
detect_sport <- function(df) {
  cols <- colnames(df)
  if ("Captain" %in% cols) return("F1")
  if ("Fighter1" %in% cols) return("MMA")
  if ("Golfer1" %in% cols) return("GOLF")
  if ("Player1" %in% cols) return("TEN")
  if ("Driver" %in% cols && "Dominator" %in% cols) return("NAS")
  return(NULL)
}

# Get sport-specific configuration
get_sport_config <- function(sport) {
  switch(sport,
         "F1" = list(
           positions = c("Captain", "Driver1", "Driver2", "Driver3", "Driver4", "Constructor"),
           sim_metric = "OptimalCount",
           metric_label = "Optimal Count",
           player_cols = c("Captain", paste0("Driver", 1:4), "Constructor")
         ),
         "MMA" = list(
           positions = paste0("Fighter", 1:6),
           sim_metric = "Top1Count",
           metric_label = "Top 1 Count",
           player_cols = paste0("Fighter", 1:6),
           additional_metrics = c("Top2Count", "Top3Count", "Top5Count")
         ),
         "TEN" = list(
           positions = paste0("Player", 1:6),
           sim_metric = "Top1Count",
           metric_label = "Top 1 Count",
           player_cols = paste0("Player", 1:6)
         ),
         "GOLF" = list(
           positions = paste0("Golfer", 1:6),
           sim_metric = "Top1Count",
           metric_label = "Top 1 Count",
           player_cols = paste0("Golfer", 1:6)
         ),
         "NAS" = list(
           positions = c("Driver", paste0("Driver", 2:6)),
           sim_metric = "Top1Count",
           metric_label = "Top 1 Count",
           player_cols = c("Driver", paste0("Driver", 2:6))
         )
  )
}

# Parse player IDs from name strings
extract_player_id <- function(player_str) {
  if (is.na(player_str) || player_str == "") return(NA)
  id <- str_extract(player_str, "\\([0-9]+\\)$")
  if (is.na(id)) return(NA)
  id <- str_remove_all(id, "[\\(\\)]")
  return(id)
}

extract_player_name <- function(player_str) {
  if (is.na(player_str) || player_str == "") return(NA)
  name <- str_remove(player_str, "\\s*\\([0-9]+\\)$")
  return(name)
}

# Create lineup key for matching
create_lineup_key <- function(df, sport) {
  config <- get_sport_config(sport)
  player_cols <- config$player_cols
  
  keys <- character(nrow(df))
  
  for (i in 1:nrow(df)) {
    if (sport == "F1") {
      captain_id <- extract_player_id(df[i, "Captain"])
      driver_ids <- sapply(paste0("Driver", 1:4), function(col) extract_player_id(df[i, col]))
      driver_ids <- sort(driver_ids[!is.na(driver_ids)])
      cnstr_id <- extract_player_id(df[i, "Constructor"])
      keys[i] <- paste(c(paste0("CPT:", captain_id), driver_ids, paste0("CNSTR:", cnstr_id)), collapse = "|")
    } else {
      # For MMA, TEN, GOLF, NAS - sort all players
      player_ids <- sapply(player_cols, function(col) {
        if (col %in% colnames(df)) extract_player_id(df[i, col]) else NA
      })
      player_ids <- sort(player_ids[!is.na(player_ids)])
      keys[i] <- paste(player_ids, collapse = "|")
    }
  }
  
  return(keys)
}

# Parse contest lineup string
parse_contest_lineup <- function(lineup_str, sport) {
  if (is.na(lineup_str) || lineup_str == "") return(NA)
  
  if (sport == "F1") {
    parts <- str_split(lineup_str, "\\s+")[[1]]
    
    cnstr_idx <- which(parts == "CNSTR")
    if (length(cnstr_idx) == 0) return(NA)
    cnstr_name <- parts[cnstr_idx + 1]
    
    cpt_idx <- which(parts == "CPT")
    if (length(cpt_idx) == 0) return(NA)
    cpt_start <- cpt_idx + 1
    
    driver_indices <- which(parts == "D")
    if (length(driver_indices) == 0) return(NA)
    
    first_d_idx <- driver_indices[1]
    captain_name <- paste(parts[cpt_start:(first_d_idx - 1)], collapse = " ")
    
    drivers <- list()
    for (i in seq_along(driver_indices)) {
      d_idx <- driver_indices[i]
      if (i < length(driver_indices)) {
        next_d_idx <- driver_indices[i + 1]
        driver_name <- paste(parts[(d_idx + 1):(next_d_idx - 1)], collapse = " ")
      } else {
        driver_name <- paste(parts[(d_idx + 1):length(parts)], collapse = " ")
      }
      drivers[[i]] <- driver_name
    }
    
    return(list(captain = captain_name, drivers = drivers, constructor = cnstr_name))
    
  } else if (sport == "MMA") {
    parts <- str_split(lineup_str, "\\s+")[[1]]
    fighter_indices <- which(parts == "F")
    
    if (length(fighter_indices) == 0) return(NA)
    
    fighters <- list()
    for (i in seq_along(fighter_indices)) {
      f_idx <- fighter_indices[i]
      if (i < length(fighter_indices)) {
        next_f_idx <- fighter_indices[i + 1]
        fighter_name <- paste(parts[(f_idx + 1):(next_f_idx - 1)], collapse = " ")
      } else {
        fighter_name <- paste(parts[(f_idx + 1):length(parts)], collapse = " ")
      }
      fighters[[i]] <- fighter_name
    }
    
    return(list(fighters = fighters))
  }
  
  # Generic parser for other sports (TEN, GOLF, NAS)
  return(NA)
}

# Match sim lineups to contest entries
match_lineups <- function(sim_df, contest_df, sport, contest_df_raw = NULL) {
  
  # Pre-create player name lookups for faster matching
  config <- get_sport_config(sport)
  
  # Create lineup keys for sim data
  sim_df$lineup_key <- create_lineup_key(sim_df, sport)
  
  # Create player name to ID lookup
  player_lookup <- list()
  for (col in config$player_cols) {
    unique_players <- unique(sim_df[[col]])
    for (player in unique_players) {
      if (!is.na(player) && player != "") {
        player_name <- extract_player_name(player)
        player_id <- extract_player_id(player)
        if (!is.na(player_name) && !is.na(player_id)) {
          player_lookup[[player_name]] <- player_id
        }
      }
    }
  }
  
  # Create lineup keys for contest data (vectorized where possible)
  contest_keys <- character(nrow(contest_df))
  
  for (i in 1:nrow(contest_df)) {
    lineup_str <- contest_df$Lineup[i]
    parsed <- parse_contest_lineup(lineup_str, sport)
    
    if (length(parsed) == 1 && is.na(parsed)) {
      contest_keys[i] <- NA
      next
    }
    
    if (sport == "F1") {
      captain_id <- player_lookup[[parsed$captain]]
      if (is.null(captain_id)) {
        contest_keys[i] <- NA
        next
      }
      
      driver_ids <- sapply(parsed$drivers, function(d) player_lookup[[d]])
      driver_ids <- sort(driver_ids[!is.na(driver_ids)])
      
      cnstr_id <- player_lookup[[parsed$constructor]]
      if (is.null(cnstr_id)) {
        contest_keys[i] <- NA
        next
      }
      
      contest_keys[i] <- paste(c(paste0("CPT:", captain_id), driver_ids, paste0("CNSTR:", cnstr_id)), collapse = "|")
      
    } else if (sport == "MMA") {
      fighter_ids <- sapply(parsed$fighters, function(f) player_lookup[[f]])
      fighter_ids <- sort(fighter_ids[!is.na(fighter_ids)])
      contest_keys[i] <- paste(fighter_ids, collapse = "|")
    }
  }
  
  contest_df$lineup_key <- contest_keys
  
  sim_df$Played <- sim_df$lineup_key %in% contest_df$lineup_key
  
  sim_df <- sim_df %>%
    left_join(
      contest_df %>% select(lineup_key, ActualScore = Points, Rank),
      by = "lineup_key"
    )
  
  # Extract player scores from contest data (raw format with one row per player)
  player_scores <- list()
  if (!is.null(contest_df_raw) && "Player" %in% colnames(contest_df_raw) && "FPTS" %in% colnames(contest_df_raw)) {
    # Get unique player scores
    player_data <- contest_df_raw %>%
      filter(!is.na(Player) & Player != "" & !is.na(FPTS)) %>%
      select(Player, FPTS) %>%
      distinct(Player, .keep_all = TRUE)
    
    for (i in 1:nrow(player_data)) {
      player_scores[[player_data$Player[i]]] <- as.numeric(player_data$FPTS[i])
    }
  }
  
  # Score unplayed lineups
  if (length(player_scores) > 0) {
    unplayed_indices <- which(!sim_df$Played | is.na(sim_df$ActualScore))
    
    for (i in unplayed_indices) {
      total_score <- 0
      players_found <- 0
      
      for (col in config$player_cols) {
        player_name <- extract_player_name(sim_df[i, col])
        
        if (!is.na(player_name) && player_name %in% names(player_scores)) {
          player_fpts <- player_scores[[player_name]]
          
          # Captain gets 1.5x for F1
          if (sport == "F1" && col == "Captain") {
            total_score <- total_score + (player_fpts * 1.5)
          } else {
            total_score <- total_score + player_fpts
          }
          players_found <- players_found + 1
        }
      }
      
      if (players_found == length(config$player_cols)) {
        sim_df$ActualScore[i] <- total_score
      }
    }
  }
  
  config <- get_sport_config(sport)
  sim_df <- sim_df %>%
    mutate(
      SimPercentile = percent_rank(!!sym(config$sim_metric)) * 100,
      OverUnder = ifelse(!is.na(ActualScore), ActualScore - mean(ActualScore, na.rm = TRUE), NA)
    )
  
  return(sim_df)
}

# ============================================================================
# UI
# ============================================================================

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = span(
      style = "color: #FFFF00; font-weight: bold; font-size: 20px;",
      "Golden Ticket Sim Review"
    )
  ),
  
  dashboardSidebar(
    width = 220,
    div(
      style = "text-align: center; padding: 20px 10px;",
      img(src = "logo.jpg", width = "180px")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Slate Selector", tabName = "load", icon = icon("upload")),
      menuItem("Scored Lineups", tabName = "table", icon = icon("table")),
      menuItem("Contest Lineups", tabName = "contest", icon = icon("users")),
      menuItem("Visualizations", tabName = "viz", icon = icon("chart-bar")),
      menuItem("Pool Analysis", tabName = "pool", icon = icon("filter")),
      menuItem("Exposure Analysis", tabName = "exposure", icon = icon("users")),
      menuItem("Opponent Tracking", tabName = "opponents", icon = icon("trophy")),
      menuItem("Insights", tabName = "insights", icon = icon("lightbulb"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #000000;
        }
        .box {
          background-color: #1a1a1a;
          border: 2px solid #FFFF00;
        }
        .box-header {
          background-color: #FFFF00;
          color: #000000;
          border-bottom: 2px solid #FFFF00;
        }
        .box-body {
          color: #FFFFFF;
        }
        .info-box {
          background-color: #1a1a1a;
          border: 2px solid #FFFF00;
          color: #FFFFFF;
        }
        .info-box-icon {
          background-color: #FFFF00 !important;
          color: #000000 !important;
        }
        .info-box-text {
          color: #FFFFFF;
        }
        .info-box-number {
          color: #FFFF00;
        }
        .small-box {
          background-color: #1a1a1a !important;
          border: 2px solid #FFFF00;
        }
        .small-box h3, .small-box p {
          color: #FFFF00;
        }
        .skin-black .main-header .logo {
          background-color: #000000;
          border-right: 2px solid #FFFF00;
        }
        .skin-black .main-header .navbar {
          background-color: #000000;
          border-bottom: 2px solid #FFFF00;
        }
        .skin-black .main-sidebar {
          background-color: #1a1a1a;
          border-right: 2px solid #FFFF00;
        }
        .skin-black .sidebar-menu > li.active > a {
          border-left-color: #FFFF00;
          background-color: #000000;
          color: #FFFF00;
        }
        .skin-black .sidebar-menu > li > a {
          color: #FFFFFF;
        }
        .skin-black .sidebar-menu > li:hover > a {
          background-color: #000000;
          border-left-color: #FFFF00;
          color: #FFFF00;
        }
        label {
          color: #FFFF00;
          font-weight: bold;
        }
        .selectize-input {
          background-color: #1a1a1a;
          border: 1px solid #FFFF00;
          color: #FFFFFF;
        }
        .selectize-dropdown {
          background-color: #1a1a1a;
          border: 1px solid #FFFF00;
          color: #FFFFFF;
        }
        .dataTables_wrapper {
          color: #FFFFFF;
        }
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_processing,
        .dataTables_wrapper .dataTables_paginate {
          color: #FFFFFF;
        }
        table.dataTable thead {
          background-color: #1a1a1a;
          color: #FFFF00;
        }
        table.dataTable tbody {
          background-color: #000000;
          color: #FFFFFF;
        }
        table.dataTable tbody tr:hover {
          background-color: #1a1a1a !important;
        }
        .btn-warning {
          background-color: #FFFF00 !important;
          border-color: #FFFF00 !important;
          color: #000000 !important;
          font-weight: bold;
        }
        .btn-warning:hover {
          background-color: #FFFF33 !important;
          border-color: #FFFF33 !important;
          color: #000000 !important;
        }
        h3, h4, h5 {
          color: #FFFF00;
        }
        .sport-btn {
          width: 140px;
          height: 80px;
          font-size: 24px;
          font-weight: bold;
          margin: 10px;
          border: 3px solid #FFFF00;
          background-color: #1a1a1a;
          color: #FFFF00;
          border-radius: 8px;
          transition: all 0.3s;
        }
        .sport-btn:hover {
          background-color: #FFFF00;
          color: #000000;
          transform: scale(1.05);
        }
        .sport-btn.active {
          background-color: #FFFF00;
          color: #000000;
        }
        .contest-selector-box {
          background-color: #1a1a1a;
          border: 2px solid #FFFF00;
          padding: 30px;
          border-radius: 8px;
          margin-top: 30px;
        }
      "))
    ),
    
    tabItems(
      # Load Data Tab
      tabItem(
        tabName = "load",
        fluidRow(
          column(6,
                 box(
                   title = "Select Sim Optimal File",
                   status = "warning",
                   solidHeader = TRUE,
                   width = 12,
                   uiOutput("optimal_file_selector")
                 )
          ),
          column(6,
                 box(
                   title = "Select Contest Standings File",
                   status = "warning",
                   solidHeader = TRUE,
                   width = 12,
                   uiOutput("contest_file_selector")
                 )
          )
        ),
        
        fluidRow(
          column(12, align = "center",
                 actionButton("load_files_btn", "Load Selected Files", 
                              class = "btn-warning btn-lg",
                              style = "margin-top: 20px;")
          )
        )
      ),
      
      # Scored Lineups Tab
      tabItem(
        tabName = "table",
        fluidRow(
          column(12,
                 div(style = "background-color: #000000; padding: 20px; border: 2px solid #FFFF00;",
                     fluidRow(
                       column(4,
                              h3(style = "color: #FFFF00; margin-top: 0;", "Scored Lineups")
                       ),
                       column(4,
                              uiOutput("rank_metric_selector")
                       ),
                       column(4, align = "right",
                              downloadButton("download_lineups", "Download Table", 
                                             class = "btn-warning",
                                             style = "margin-top: 10px;")
                       )
                     ),
                     hr(style = "border-color: #FFFF00; margin: 15px 0;"),
                     DTOutput("lineup_table")
                 )
          )
        )
      ),
      
      # Contest Lineups Tab
      tabItem(
        tabName = "contest",
        fluidRow(
          column(12,
                 div(style = "background-color: #000000; padding: 20px; border: 2px solid #FFFF00;",
                     fluidRow(
                       column(6,
                              h3(style = "color: #FFFF00; margin-top: 0;", "Contest Lineups")
                       ),
                       column(6, align = "right",
                              downloadButton("download_contest", "Download Table", 
                                             class = "btn-warning",
                                             style = "margin-top: 10px;")
                       )
                     ),
                     hr(style = "border-color: #FFFF00; margin: 15px 0;"),
                     DTOutput("contest_table")
                 )
          )
        )
      ),
      
      # Visualizations Tab
      tabItem(
        tabName = "viz",
        fluidRow(
          box(
            title = "Grouping & Visualization Controls",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(4,
                     pickerInput(
                       "group_by",
                       "Group Lineups By:",
                       choices = c("Played Status", "Sim Percentile", "Score Buckets"),
                       selected = "Played Status",
                       options = list(style = "btn-warning")
                     )
              ),
              column(4,
                     pickerInput(
                       "viz_type",
                       "Visualization Type:",
                       choices = c("Violin Plot", "Box Plot", "Scatter Plot", "Histogram"),
                       selected = "Violin Plot",
                       options = list(style = "btn-warning")
                     )
              ),
              column(4,
                     pickerInput(
                       "metric_to_plot",
                       "Metric to Plot:",
                       choices = c("ActualScore", "SimPercentile"),
                       selected = "ActualScore",
                       options = list(style = "btn-warning")
                     )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Visualization",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("main_viz", height = "600px")
          )
        )
      ),
      
      # Pool Analysis Tab
      tabItem(
        tabName = "pool",
        fluidRow(
          column(4,
                 box(
                   title = "Pool Filter Settings",
                   status = "warning",
                   solidHeader = TRUE,
                   width = 12,
                   
                   sliderInput("pool_top_pct", "Top X% by Sim Score:", 
                               min = 1, max = 100, value = 10, step = 1),
                   
                   numericInput("pool_min_count", "Min Optimal/Top1 Count:", 
                                value = 0, min = 0),
                   
                   actionButton("apply_pool_filter", "Apply Filter",
                                class = "btn-warning")
                 )
          ),
          
          column(8,
                 box(
                   title = "Pool Performance Metrics",
                   status = "warning",
                   solidHeader = TRUE,
                   width = 12,
                   DTOutput("pool_metrics_table")
                 )
          )
        ),
        
        fluidRow(
          box(
            title = "Pool Comparison Visualization",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("pool_comparison_plot", height = "500px")
          )
        )
      ),
      
      # Exposure Analysis Tab
      tabItem(
        tabName = "exposure",
        fluidRow(
          box(
            title = "Player Exposure Analysis",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("exposure_table")
          )
        ),
        
        fluidRow(
          column(6,
                 box(
                   title = "Your Exposures",
                   status = "warning",
                   solidHeader = TRUE,
                   width = 12,
                   plotlyOutput("exposure_comparison_plot", height = "500px")
                 )
          ),
          column(6,
                 box(
                   title = "Top Exposures",
                   status = "warning",
                   solidHeader = TRUE,
                   width = 12,
                   plotlyOutput("leverage_plot", height = "500px")
                 )
          )
        )
      ),
      
      # Opponent Tracking Tab
      tabItem(
        tabName = "opponents",
        fluidRow(
          box(
            title = "User Performance Leaderboard",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("user_leaderboard")
          )
        )
      ),
      
      # Insights Tab
      tabItem(
        tabName = "insights",
        fluidRow(
          box(
            title = "Key Insights",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            uiOutput("insights_text")
          )
        ),
        
        fluidRow(
          column(6,
                 box(
                   title = "Sim Accuracy",
                   status = "warning",
                   solidHeader = TRUE,
                   width = 12,
                   plotlyOutput("sim_accuracy_plot", height = "400px")
                 )
          ),
          column(6,
                 box(
                   title = "Score Distribution",
                   status = "warning",
                   solidHeader = TRUE,
                   width = 12,
                   plotlyOutput("score_distribution_plot", height = "400px")
                 )
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    sim_data = NULL,
    contest_data = NULL,
    contest_data_raw = NULL,
    matched_data = NULL,
    sport = NULL,
    optimal_files = NULL,
    contest_files = NULL
  )
  
  # Scan for available files on startup
  observe({
    base_dir <- getwd()
    optimals_dir <- file.path(base_dir, "optimals")
    contests_dir <- file.path(base_dir, "contests")
    
    if (dir.exists(optimals_dir)) {
      rv$optimal_files <- list.files(optimals_dir, pattern = "\\.csv$", full.names = TRUE)
    } else {
      rv$optimal_files <- character(0)
    }
    
    if (dir.exists(contests_dir)) {
      rv$contest_files <- list.files(contests_dir, pattern = "\\.csv$", full.names = TRUE)
    } else {
      rv$contest_files <- character(0)
    }
  })
  
  # Optimal file selector
  output$optimal_file_selector <- renderUI({
    if (length(rv$optimal_files) == 0) {
      return(p("No files found in optimals/ directory", style = "color: #FFFFFF;"))
    }
    
    choices <- setNames(rv$optimal_files, basename(rv$optimal_files))
    
    selectInput(
      "selected_optimal",
      "Choose Optimal File:",
      choices = choices,
      width = "100%"
    )
  })
  
  # Contest file selector
  output$contest_file_selector <- renderUI({
    if (length(rv$contest_files) == 0) {
      return(p("No files found in contests/ directory", style = "color: #FFFFFF;"))
    }
    
    choices <- setNames(rv$contest_files, basename(rv$contest_files))
    
    selectInput(
      "selected_contest",
      "Choose Contest File:",
      choices = choices,
      width = "100%"
    )
  })
  
  # Load files button
  observeEvent(input$load_files_btn, {
    req(input$selected_optimal, input$selected_contest)
    
    tryCatch({
      # Read files
      sim_df <- read.csv(input$selected_optimal, stringsAsFactors = FALSE)
      contest_df_raw <- read.csv(input$selected_contest, stringsAsFactors = FALSE)
      
      # Remove BOM if present
      colnames(contest_df_raw)[1] <- str_remove(colnames(contest_df_raw)[1], "^\ufeff")
      
      # Aggregate contest data by EntryId (since each entry has multiple rows for players)
      contest_df <- contest_df_raw %>%
        group_by(EntryId, EntryName, Rank) %>%
        summarise(
          Points = first(Points),
          Lineup = first(Lineup),
          .groups = 'drop'
        )
      
      # Store raw contest data for player scores
      rv$contest_data_raw <- contest_df_raw
      
      # Detect sport
      sport <- detect_sport(sim_df)
      if (is.null(sport)) {
        showNotification("Could not detect sport from file columns", type = "error", duration = 10)
        return()
      }
      
      # Match lineups
      matched_df <- match_lineups(sim_df, contest_df, sport, contest_df_raw)
      
      # Store data
      rv$sim_data <- sim_df
      rv$contest_data <- contest_df
      rv$matched_data <- matched_df
      rv$sport <- sport
      
      showNotification(
        paste("Loaded successfully! Sport:", sport),
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      showNotification(
        paste("Error loading files:", e$message),
        type = "error",
        duration = 10
      )
      print(e)
    })
  })
  
  # Load status box
  output$load_status_box <- renderUI({
    if (!is.null(rv$matched_data)) {
      n_total <- nrow(rv$matched_data)
      n_played <- sum(rv$matched_data$Played, na.rm = TRUE)
      cash_line <- ceiling(nrow(rv$contest_data) * 0.20)
      played_data <- rv$matched_data %>% filter(Played == TRUE)
      n_cashed <- sum(played_data$Rank <= cash_line, na.rm = TRUE)
      cash_rate <- if (n_played > 0) round(n_cashed / n_played * 100, 1) else 0
      best_finish <- if (nrow(played_data) > 0) min(played_data$Rank, na.rm = TRUE) else NA
      
      fluidRow(
        valueBox(
          n_played,
          "Lineups Played",
          icon = icon("play"),
          color = "yellow",
          width = 4
        ),
        valueBox(
          paste0(cash_rate, "%"),
          "Cash Rate",
          icon = icon("dollar-sign"),
          color = "yellow",
          width = 4
        ),
        valueBox(
          ifelse(is.na(best_finish), "N/A", best_finish),
          "Best Finish",
          icon = icon("trophy"),
          color = "yellow",
          width = 4
        )
      )
    }
  })
  
  # Contest lineups data - show each entry with user info and sim metrics
  contest_lineups_data <- reactive({
    req(rv$contest_data, rv$matched_data)
    
    config <- get_sport_config(rv$sport)
    
    # Get all numeric columns (sim metrics)
    numeric_cols <- names(rv$matched_data)[sapply(rv$matched_data, is.numeric)]
    exclude_cols <- c("SimPercentile", "OverUnder", "Played")
    sim_metric_cols <- setdiff(numeric_cols, exclude_cols)
    
    # Create a lookup with sim metrics
    sim_lookup <- rv$matched_data %>%
      select(lineup_key, all_of(config$player_cols), all_of(sim_metric_cols)) %>%
      distinct(lineup_key, .keep_all = TRUE)
    
    # Calculate SimRank based on selected metric (or default)
    rank_metric <- if (!is.null(input$rank_metric)) input$rank_metric else config$sim_metric
    if (rank_metric %in% colnames(sim_lookup)) {
      sim_lookup <- sim_lookup %>%
        arrange(desc(!!sym(rank_metric))) %>%
        mutate(SimRank = row_number())
    } else {
      sim_lookup$SimRank <- NA
    }
    
    # rv$contest_data already has lineup_key from match_lineups function
    contest_with_sims <- rv$contest_data %>%
      left_join(sim_lookup, by = "lineup_key") %>%
      select(EntryName, Rank, Points, everything(), -lineup_key) %>%
      arrange(Rank)
    
    return(contest_with_sims)
  })
  
  # Contest table output
  output$contest_table <- renderDT({
    req(contest_lineups_data())
    
    config <- get_sport_config(rv$sport)
    
    # Get all numeric columns from the data
    all_cols <- colnames(contest_lineups_data())
    
    # Build display columns: EntryName, Rank, Points, Players, then all sim metrics, then SimRank
    display_cols <- c("EntryName", "Rank", "Points")
    
    # Add player columns
    for (col in config$player_cols) {
      if (col %in% all_cols) {
        display_cols <- c(display_cols, col)
      }
    }
    
    # Add all sim metric columns (numeric columns except Rank and Points)
    numeric_cols <- all_cols[sapply(contest_lineups_data(), is.numeric)]
    sim_metrics <- setdiff(numeric_cols, c("Rank", "Points"))
    display_cols <- c(display_cols, sim_metrics)
    
    # Make sure SimRank is at the end if it exists
    if ("SimRank" %in% display_cols) {
      display_cols <- c(setdiff(display_cols, "SimRank"), "SimRank")
    }
    
    df_display <- contest_lineups_data() %>%
      select(any_of(display_cols)) %>%
      mutate(
        Points = round(Points, 2)
      )
    
    # Clean player names
    for (col in config$player_cols) {
      if (col %in% colnames(df_display)) {
        df_display[[col]] <- sapply(df_display[[col]], extract_player_name)
      }
    }
    
    datatable(
      df_display,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        dom = 't<"bottom"lip>',
        ordering = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Download handler for contest table
  output$download_contest <- downloadHandler(
    filename = function() {
      paste0("contest_lineups_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(contest_lineups_data())
      config <- get_sport_config(rv$sport)
      
      # Get all columns
      all_cols <- colnames(contest_lineups_data())
      display_cols <- c("EntryName", "Rank", "Points")
      
      for (col in config$player_cols) {
        if (col %in% all_cols) {
          display_cols <- c(display_cols, col)
        }
      }
      
      # Add all numeric sim metrics
      numeric_cols <- all_cols[sapply(contest_lineups_data(), is.numeric)]
      sim_metrics <- setdiff(numeric_cols, c("Rank", "Points"))
      display_cols <- c(display_cols, sim_metrics)
      
      df_download <- contest_lineups_data() %>%
        select(any_of(display_cols)) %>%
        mutate(
          Points = round(Points, 2)
        )
      
      # Clean player names
      for (col in config$player_cols) {
        if (col %in% colnames(df_download)) {
          df_download[[col]] <- sapply(df_download[[col]], extract_player_name)
        }
      }
      
      write.csv(df_download, file, row.names = FALSE)
    }
  )
  
  # Filtered data for table - aggregate by unique lineups
  filtered_data <- reactive({
    req(rv$matched_data, rv$contest_data)
    
    df <- rv$matched_data
    config <- get_sport_config(rv$sport)
    
    # Group by lineup_key to get unique lineups
    unique_lineups <- df %>%
      group_by(lineup_key) %>%
      summarise(
        # Keep first instance of player columns
        across(all_of(config$player_cols), first),
        # Get ALL numeric columns from sim data (all metrics) EXCEPT the ones we calculate separately
        across(where(is.numeric) & !any_of(c("Played", "Rank")), first, .names = "{.col}"),
        # Contest info
        TimesPlayed = sum(Played, na.rm = TRUE),
        ContestRank = first(Rank),
        .groups = 'drop'
      )
    
    # Calculate sim rank based on selected ranking metric
    rank_metric <- if (!is.null(input$rank_metric)) input$rank_metric else config$sim_metric
    if (rank_metric %in% colnames(unique_lineups)) {
      unique_lineups <- unique_lineups %>%
        arrange(desc(!!sym(rank_metric))) %>%
        mutate(SimRank = row_number())
    } else {
      unique_lineups$SimRank <- NA
    }
    
    # Sort by ActualScore for display (NAs last)
    unique_lineups <- unique_lineups %>%
      arrange(desc(ActualScore))
    
    return(unique_lineups)
  })
  
  # Rank metric selector
  output$rank_metric_selector <- renderUI({
    req(rv$matched_data)
    
    # Get all numeric column names from the matched data (these are sim metrics)
    numeric_cols <- names(rv$matched_data)[sapply(rv$matched_data, is.numeric)]
    
    # Remove system columns
    exclude_cols <- c("SimPercentile", "OverUnder", "Rank", "TimesPlayed", "ContestRank", "SimRank", "Played")
    metric_choices <- setdiff(numeric_cols, exclude_cols)
    
    # Use column names as both names and values
    choices <- setNames(metric_choices, metric_choices)
    
    config <- get_sport_config(rv$sport)
    
    selectInput(
      "rank_metric",
      "Rank By:",
      choices = choices,
      selected = if (config$sim_metric %in% metric_choices) config$sim_metric else metric_choices[1],
      width = "100%"
    )
  })
  
  # Download handler for lineup table
  output$download_lineups <- downloadHandler(
    filename = function() {
      paste0("scored_lineups_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(filtered_data())
      config <- get_sport_config(rv$sport)
      
      display_cols <- c(config$player_cols, config$sim_metric)
      
      if (rv$sport == "MMA" && !is.null(config$additional_metrics)) {
        display_cols <- c(display_cols, config$additional_metrics)
      }
      
      display_cols <- c(display_cols, "ActualScore", "SimRank", "TimesPlayed", "ContestRank")
      
      df_download <- filtered_data() %>%
        select(any_of(display_cols)) %>%
        mutate(
          ActualScore = ifelse(!is.na(ActualScore), round(ActualScore, 2), NA)
        )
      
      # Clean player names
      for (col in config$player_cols) {
        if (col %in% colnames(df_download)) {
          df_download[[col]] <- sapply(df_download[[col]], extract_player_name)
        }
      }
      
      write.csv(df_download, file, row.names = FALSE)
    }
  )
  
  # Lineup table output
  output$lineup_table <- renderDT({
    req(filtered_data())
    
    config <- get_sport_config(rv$sport)
    
    display_cols <- c(config$player_cols, config$sim_metric)
    
    if (rv$sport == "MMA" && !is.null(config$additional_metrics)) {
      display_cols <- c(display_cols, config$additional_metrics)
    }
    
    display_cols <- c(display_cols, "ActualScore", "SimRank", "TimesPlayed", "ContestRank")
    
    df_display <- filtered_data() %>%
      select(any_of(display_cols)) %>%
      mutate(
        ActualScore = ifelse(!is.na(ActualScore), round(ActualScore, 2), NA)
      )
    
    # Clean player names
    for (col in config$player_cols) {
      if (col %in% colnames(df_display)) {
        df_display[[col]] <- sapply(df_display[[col]], extract_player_name)
      }
    }
    
    datatable(
      df_display,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        dom = 't<"bottom"lip>',
        ordering = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Main visualization
  output$main_viz <- renderPlotly({
    req(filtered_data())
    
    df <- filtered_data() %>% filter(!is.na(ActualScore))
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data to display"))
    }
    
    if (input$group_by == "Played Status") {
      df$Group <- ifelse(df$Played, "Played", "Not Played")
    } else if (input$group_by == "Sim Percentile") {
      df$Group <- cut(df$SimPercentile, 
                      breaks = c(0, 25, 50, 75, 90, 95, 100),
                      labels = c("0-25%", "25-50%", "50-75%", "75-90%", "90-95%", "95-100%"))
    } else {
      df$Group <- "All"
    }
    
    metric <- input$metric_to_plot
    yellow_colors <- c("#FFFF00", "#FFFF33", "#FFFF66", "#FFFF99", "#FFFFCC", "#FFFFE6")
    
    if (input$viz_type == "Violin Plot") {
      plot_ly(df, y = ~get(metric), x = ~Group, type = 'violin',
              color = ~Group, colors = yellow_colors) %>%
        layout(
          paper_bgcolor = '#000000',
          plot_bgcolor = '#1a1a1a',
          font = list(color = '#FFFFFF'),
          xaxis = list(title = "Group", gridcolor = '#FFFF00'),
          yaxis = list(title = metric, gridcolor = '#FFFF00')
        )
    } else if (input$viz_type == "Box Plot") {
      plot_ly(df, y = ~get(metric), x = ~Group, type = 'box',
              color = ~Group, colors = yellow_colors) %>%
        layout(
          paper_bgcolor = '#000000',
          plot_bgcolor = '#1a1a1a',
          font = list(color = '#FFFFFF'),
          xaxis = list(title = "Group", gridcolor = '#FFFF00'),
          yaxis = list(title = metric, gridcolor = '#FFFF00')
        )
    } else if (input$viz_type == "Scatter Plot") {
      config <- get_sport_config(rv$sport)
      plot_ly(df, x = ~get(config$sim_metric), y = ~ActualScore, 
              color = ~Group, colors = yellow_colors,
              text = ~paste("Rank:", Rank),
              type = 'scatter', mode = 'markers') %>%
        layout(
          paper_bgcolor = '#000000',
          plot_bgcolor = '#1a1a1a',
          font = list(color = '#FFFFFF'),
          xaxis = list(title = config$metric_label, gridcolor = '#FFFF00'),
          yaxis = list(title = "Actual Score", gridcolor = '#FFFF00')
        )
    } else {
      plot_ly(df, x = ~get(metric), type = 'histogram',
              marker = list(color = '#FFFF00', line = list(color = '#000000', width = 1))) %>%
        layout(
          paper_bgcolor = '#000000',
          plot_bgcolor = '#1a1a1a',
          font = list(color = '#FFFFFF'),
          xaxis = list(title = metric, gridcolor = '#FFFF00'),
          yaxis = list(title = "Count", gridcolor = '#FFFF00')
        )
    }
  })
  
  # Pool metrics table
  output$pool_metrics_table <- renderDT({
    req(rv$matched_data)
    
    df <- rv$matched_data
    config <- get_sport_config(rv$sport)
    cash_line <- ceiling(nrow(rv$contest_data) * 0.20)
    
    pools <- list(
      "All Sims" = df,
      "Top 10%" = df %>% filter(SimPercentile >= 90),
      "Top 5%" = df %>% filter(SimPercentile >= 95),
      "Top 1%" = df %>% filter(SimPercentile >= 99)
    )
    
    results <- lapply(names(pools), function(pool_name) {
      pool_df <- pools[[pool_name]]
      played_df <- pool_df %>% filter(Played == TRUE)
      
      n_total <- nrow(pool_df)
      n_played <- nrow(played_df)
      n_cashed <- sum(played_df$Rank <= cash_line, na.rm = TRUE)
      cash_rate <- if (n_played > 0) round(n_cashed / n_played * 100, 1) else 0
      avg_finish <- if (n_played > 0) round(mean(played_df$Rank, na.rm = TRUE), 0) else NA
      
      data.frame(
        Pool = pool_name,
        Total_Lineups = n_total,
        Played = n_played,
        Cashed = n_cashed,
        Cash_Rate = paste0(cash_rate, "%"),
        Avg_Finish = ifelse(is.na(avg_finish), "N/A", avg_finish)
      )
    })
    
    results_df <- do.call(rbind, results)
    
    datatable(
      results_df,
      options = list(
        dom = 't',
        pageLength = 10
      ),
      rownames = FALSE
    )
  })
  
  # Pool comparison plot
  output$pool_comparison_plot <- renderPlotly({
    req(rv$matched_data)
    
    df <- rv$matched_data
    cash_line <- ceiling(nrow(rv$contest_data) * 0.20)
    
    pools <- list(
      "All Sims" = df,
      "Top 10%" = df %>% filter(SimPercentile >= 90),
      "Top 5%" = df %>% filter(SimPercentile >= 95),
      "Top 1%" = df %>% filter(SimPercentile >= 99)
    )
    
    cash_rates <- sapply(names(pools), function(pool_name) {
      pool_df <- pools[[pool_name]]
      played_df <- pool_df %>% filter(Played == TRUE)
      
      n_played <- nrow(played_df)
      n_cashed <- sum(played_df$Rank <= cash_line, na.rm = TRUE)
      if (n_played > 0) round(n_cashed / n_played * 100, 1) else 0
    })
    
    plot_ly(x = names(cash_rates), y = cash_rates, type = 'bar',
            marker = list(color = '#FFFF00', line = list(color = '#000000', width = 2))) %>%
      layout(
        title = "Cash Rate by Pool",
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        xaxis = list(title = "Pool", gridcolor = '#FFFF00'),
        yaxis = list(title = "Cash Rate (%)", gridcolor = '#FFFF00')
      )
  })
  
  # Exposure table
  output$exposure_table <- renderDT({
    req(rv$matched_data, rv$contest_data)
    
    config <- get_sport_config(rv$sport)
    
    played_df <- rv$matched_data %>% filter(Played == TRUE)
    n_played <- nrow(played_df)
    
    if (n_played == 0) {
      return(datatable(data.frame(Message = "No played lineups found")))
    }
    
    exposure_list <- list()
    
    for (col in config$player_cols) {
      players <- played_df[[col]]
      player_names <- sapply(players, extract_player_name)
      player_counts <- table(player_names)
      
      for (player in names(player_counts)) {
        if (is.na(player) || player == "") next
        exposure_list[[player]] <- as.numeric(player_counts[player]) / n_played * 100
      }
    }
    
    exposure_df <- data.frame(
      Player = names(exposure_list),
      Your_Exposure = round(unlist(exposure_list), 1)
    )
    
    exposure_df <- exposure_df %>% arrange(desc(Your_Exposure))
    
    datatable(
      exposure_df,
      options = list(
        pageLength = 25,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv')
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Your_Exposure',
        background = styleColorBar(range(exposure_df$Your_Exposure), '#FFFF00'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Exposure comparison plot
  output$exposure_comparison_plot <- renderPlotly({
    req(rv$matched_data)
    
    config <- get_sport_config(rv$sport)
    played_df <- rv$matched_data %>% filter(Played == TRUE)
    n_played <- nrow(played_df)
    
    if (n_played == 0) {
      return(plot_ly() %>% layout(title = "No played lineups"))
    }
    
    exposure_list <- list()
    
    for (col in config$player_cols) {
      players <- played_df[[col]]
      player_names <- sapply(players, extract_player_name)
      player_counts <- table(player_names)
      
      for (player in names(player_counts)) {
        if (is.na(player) || player == "") next
        exposure_list[[player]] <- as.numeric(player_counts[player]) / n_played * 100
      }
    }
    
    exposure_df <- data.frame(
      Player = names(exposure_list),
      Exposure = unlist(exposure_list)
    ) %>%
      arrange(desc(Exposure)) %>%
      head(15)
    
    plot_ly(exposure_df, x = ~Exposure, y = ~reorder(Player, Exposure), type = 'bar',
            orientation = 'h',
            marker = list(color = '#FFFF00', line = list(color = '#000000', width = 1))) %>%
      layout(
        title = "Top 15 Player Exposures",
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        xaxis = list(title = "Exposure %", gridcolor = '#FFFF00'),
        yaxis = list(title = "", gridcolor = '#FFFF00')
      )
  })
  
  # Leverage plot
  output$leverage_plot <- renderPlotly({
    req(rv$matched_data)
    
    config <- get_sport_config(rv$sport)
    played_df <- rv$matched_data %>% filter(Played == TRUE)
    n_played <- nrow(played_df)
    
    if (n_played == 0) {
      return(plot_ly() %>% layout(title = "No played lineups"))
    }
    
    exposure_list <- list()
    
    for (col in config$player_cols) {
      players <- played_df[[col]]
      player_names <- sapply(players, extract_player_name)
      player_counts <- table(player_names)
      
      for (player in names(player_counts)) {
        if (is.na(player) || player == "") next
        exposure_list[[player]] <- as.numeric(player_counts[player]) / n_played * 100
      }
    }
    
    exposure_df <- data.frame(
      Player = names(exposure_list),
      Exposure = unlist(exposure_list)
    ) %>%
      arrange(desc(Exposure)) %>%
      head(20)
    
    plot_ly(exposure_df, labels = ~Player, values = ~Exposure, type = 'pie',
            marker = list(colors = c('#FFFF00', '#FFFF33', '#FFFF66', '#FFFF99', '#FFFFCC', '#FFFFE6',
                                     '#FFFF00', '#FFFF33', '#FFFF66', '#FFFF99'))) %>%
      layout(
        title = "Exposure Distribution (Top 20)",
        paper_bgcolor = '#000000',
        font = list(color = '#FFFFFF')
      )
  })
  
  # User leaderboard
  output$user_leaderboard <- renderDT({
    req(rv$contest_data)
    
    user_stats <- rv$contest_data %>%
      group_by(EntryName) %>%
      summarise(
        Entries = n(),
        Best_Finish = min(Rank, na.rm = TRUE),
        Avg_Finish = round(mean(Rank, na.rm = TRUE), 1),
        Total_Points = round(sum(Points, na.rm = TRUE), 1),
        .groups = 'drop'
      ) %>%
      arrange(Best_Finish)
    
    datatable(
      user_stats,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv')
      ),
      rownames = FALSE
    )
  })
  
  # Insights text
  output$insights_text <- renderUI({
    req(rv$matched_data)
    
    df <- rv$matched_data
    config <- get_sport_config(rv$sport)
    cash_line <- ceiling(nrow(rv$contest_data) * 0.20)
    
    top5_df <- df %>% filter(SimPercentile >= 95, Played == TRUE)
    top5_cash_rate <- if (nrow(top5_df) > 0) {
      round(sum(top5_df$Rank <= cash_line, na.rm = TRUE) / nrow(top5_df) * 100, 1)
    } else 0
    
    played_df <- df %>% filter(Played == TRUE)
    overall_cash_rate <- if (nrow(played_df) > 0) {
      round(sum(played_df$Rank <= cash_line, na.rm = TRUE) / nrow(played_df) * 100, 1)
    } else 0
    
    cor_val <- cor(df[[config$sim_metric]], df$ActualScore, use = "complete.obs")
    
    best_sim <- df %>% arrange(desc(!!sym(config$sim_metric))) %>% head(1)
    best_sim_score <- if (nrow(best_sim) > 0 && !is.na(best_sim$ActualScore[1])) {
      round(best_sim$ActualScore[1], 1)
    } else "N/A"
    
    tagList(
      h3(style = "color: #FFFF00;", "???? Key Findings:"),
      tags$ul(
        style = "color: #FFFFFF; font-size: 16px;",
        tags$li(paste("Top 5% of sims had a", top5_cash_rate, "% cash rate")),
        tags$li(paste("Overall cash rate:", overall_cash_rate, "%")),
        tags$li(paste("Sim accuracy (correlation):", round(cor_val, 3))),
        tags$li(paste("Best sim lineup scored:", best_sim_score, "points"))
      )
    )
  })
  
  # Sim accuracy plot
  output$sim_accuracy_plot <- renderPlotly({
    req(rv$matched_data)
    
    config <- get_sport_config(rv$sport)
    df <- rv$matched_data %>% filter(!is.na(ActualScore))
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }
    
    plot_ly(df, x = ~get(config$sim_metric), y = ~ActualScore,
            type = 'scatter', mode = 'markers',
            marker = list(color = '#FFFF00', size = 6, 
                          line = list(color = '#000000', width = 1))) %>%
      layout(
        title = paste(config$metric_label, "vs Actual Score"),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        xaxis = list(title = config$metric_label, gridcolor = '#FFFF00'),
        yaxis = list(title = "Actual Score", gridcolor = '#FFFF00')
      )
  })
  
  # Score distribution plot
  output$score_distribution_plot <- renderPlotly({
    req(rv$matched_data)
    
    df <- rv$matched_data %>% filter(!is.na(ActualScore))
    
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }
    
    plot_ly(df, x = ~ActualScore, type = 'histogram',
            marker = list(color = '#FFFF00', line = list(color = '#000000', width = 1))) %>%
      layout(
        title = "Actual Score Distribution",
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        xaxis = list(title = "Actual Score", gridcolor = '#FFFF00'),
        yaxis = list(title = "Count", gridcolor = '#FFFF00')
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)