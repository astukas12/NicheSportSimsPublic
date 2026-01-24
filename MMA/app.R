# MMA Simulation Shiny App
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(plotly)
library(data.table)
library(lpSolve)
library(memoise)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)

# Global constants
DK_ROSTER_SIZE <- 6
FD_ROSTER_SIZE <- 6  
DK_SALARY_CAP <- 50000
FD_SALARY_CAP <- 100  

SD_ROSTER_SIZE <- 6  # 1 Captain + 5 Fighters  
SD_SALARY_CAP <- 50000


# Win bonus lookup tables
DK_WIN_BONUSES <- list(
  "QuickWin_R1" = 115,
  "R1" = 90,
  "R2" = 70,
  "R3" = 45,
  "R4" = 40,
  "R5" = 40,
  "Decision_R3" = 30,
  "Decision_R5" = 30,
  "Decision" = 30
)

FD_WIN_BONUSES <- list(
  "QuickWin_R1" = 100,
  "R1" = 100,
  "R2" = 75,
  "R3" = 50,
  "R4" = 35,
  "R5" = 25,
  "Decision_R3" = 20,
  "Decision_R5" = 20,
  "Decision" = 20
)

#' Get win bonus for a given outcome and platform
#' @param outcome The outcome string (e.g., "R1", "R2", "Decision_R3")
#' @param platform "DK" or "FD"
#' @return Win bonus value
get_win_bonus <- function(outcome, platform = "DK") {
  bonuses <- if(platform == "DK") DK_WIN_BONUSES else FD_WIN_BONUSES
  
  # Try exact match first
  if(outcome %in% names(bonuses)) {
    return(bonuses[[outcome]])
  }
  
  # Default to 0 if no match (for losses or unknown outcomes)
  return(0)
}

# Set up custom CSS for black and red theme
custom_css <- "
  /* Override dashboard header colors */
  .skin-blue .main-header {
    background-color: #000000;
  }
  .skin-blue .main-header .logo {
    background-color: #000000;
    color: #FFD700; 
  }
  .skin-blue .main-header .logo:hover {
    background-color: #000000;
  }
  .skin-blue .main-header .navbar {
    background-color: #000000;
  }
  
  /* Override dashboard sidebar colors */
  .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
    background-color: #222222;
  }
  .skin-blue .sidebar a {
    color: #FFD700; 
  }
  .skin-blue .sidebar-menu > li.active > a, 
  .skin-blue .sidebar-menu > li:hover > a {
    color: #ffffff;
    background: #333333;
    border-left-color: #FFD700;
  }
  
  /* Customize box headers */
  .box.box-primary .box-header {
    background-color: #333333;
    color: #FFD700;
  }
  
  /* Style buttons */
  .btn-primary {
    background-color: #FFD700;
    border-color: #DAA520;
    color: #000000;
  }
  .btn-primary:hover, .btn-primary:focus {
    background-color: #DAA520;
    border-color: #B8860B;
    color: #000000;
  }
  
  /* Style tabs */
  .nav-tabs-custom > .nav-tabs > li.active {
    border-top-color: #FFD700;
  }
  
  /* Additional styles for gold accents */
  .pagination > .active > a, 
  .pagination > .active > span, 
  .pagination > .active > a:hover, 
  .pagination > .active > span:hover, 
  .pagination > .active > a:focus, 
  .pagination > .active > span:focus {
    background-color: #FFD700;
    border-color: #DAA520;
    color: #000000;
  }
  
  /* Style for sliders and other inputs */
  .irs-bar,
  .irs-bar-edge,
  .irs-single,
  .irs-from,
  .irs-to {
    background: #FFD700;
    border-color: #DAA520;
    color: #000000;
  }
  
  /* Style for checkboxes and radio buttons */
  input[type='checkbox']:checked, 
  input[type='radio']:checked {
    background-color: #FFD700;
    border-color: #DAA520;
  }
  
  /* Style loader spinners */
  .shiny-spinner .load-container .loader {
    border-top-color: #FFD700;
  }
"

# Read input file function
read_input_file <- function(file_path) {
  tryCatch({
    # Read sheets needed for both platforms
    sheets <- list(
      Fights = read_excel(file_path, sheet = "Fights"),
      Scores = read_excel(file_path, sheet = "Scores")
    )
    
    # Identify available platforms based on sheets
    has_dk <- "DKSalary" %in% colnames(sheets$Fights) && "CPTID" %in% colnames(sheets$Fights) && "SDID" %in% colnames(sheets$Fights) && "SDSal" %in% colnames(sheets$Fights) && "Winner_DK_Base_P50" %in% colnames(sheets$Scores)
    has_fd <- "FDSalary" %in% colnames(sheets$Fights) && "Winner_FD_Base_P50" %in% colnames(sheets$Scores)
    
    # Create platform info
    platform_info <- list(
      has_draftkings = has_dk,
      has_fanduel = has_fd
    )
    
    list(
      sheets = sheets,
      platform_info = platform_info
    )
  }, error = function(e) {
    stop(paste("Error reading Excel file:", e$message))
  })
}

# Process input data efficiently
process_input_data <- function(input_data) {
  # Extract data components
  fights_data <- input_data$sheets$Fights
  scores_data <- input_data$sheets$Scores
  
  # Process fights data
  processed_fights <- as.data.table(fights_data)
  
  # Process scores data
  processed_scores <- as.data.table(scores_data)
  
  # Convert relevant numeric columns in fights data
  numeric_cols_fights <- c(
    "DKSalary", "FDSalary", "DKOwn", "FDOwn", "CPTID", "SDID", "SDSal", "OriginalML", 
    "DeViggedProb", "R1", "QuickWin_R1", "R2", "R3", "R4", "R5", "Decision"
  )
  
  for(col in numeric_cols_fights) {
    if(col %in% names(processed_fights)) {
      processed_fights[, (col) := as.numeric(get(col))]
    }
  }
  
  # Convert relevant numeric columns in scores data
  numeric_cols_scores <- c(
    "Winner_Prob", 
    "Winner_DK_Base_P5", "Winner_DK_Base_P10", "Winner_DK_Base_P25", "Winner_DK_Base_P50", 
    "Winner_DK_Base_P75", "Winner_DK_Base_P90", "Winner_DK_Base_P95",
    "Winner_FD_Base_P5", "Winner_FD_Base_P10", "Winner_FD_Base_P25", "Winner_FD_Base_P50", 
    "Winner_FD_Base_P75", "Winner_FD_Base_P90", "Winner_FD_Base_P95",
    "Loser_DK_Base_P5", "Loser_DK_Base_P10", "Loser_DK_Base_P25", "Loser_DK_Base_P50", 
    "Loser_DK_Base_P75", "Loser_DK_Base_P90", "Loser_DK_Base_P95",
    "Loser_FD_Base_P5", "Loser_FD_Base_P10", "Loser_FD_Base_P25", "Loser_FD_Base_P50", 
    "Loser_FD_Base_P75", "Loser_FD_Base_P90", "Loser_FD_Base_P95"
  )
  
  for(col in numeric_cols_scores) {
    if(col %in% names(processed_scores)) {
      processed_scores[, (col) := as.numeric(get(col))]
    }
  }
  
  # Create fight pairs (matching fighters against opponents)
  fight_pairs <- create_fight_pairs(processed_fights)
  
  # Return processed data
  list(
    fights = processed_fights,
    scores = processed_scores,
    fight_pairs = fight_pairs
  )
}

# Create fight pairs
create_fight_pairs <- function(fights_data) {
  # Create a unique identifier for each fight
  fighter_pairs <- data.table()
  
  # Get unique fighters from the data
  unique_fighters <- unique(fights_data$Name)
  
  # Iterate through fighters to find matching pairs
  for(fighter in unique_fighters) {
    # Find the fighter's row
    fighter_row <- fights_data[Name == fighter]
    
    # Skip if no opponent is listed
    if(is.null(fighter_row$Opponent) || is.na(fighter_row$Opponent)) next
    
    # Check if opponent exists and this pair hasn't been added yet
    opponent <- fighter_row$Opponent
    
    # Only add the pair once (when fighter name comes alphabetically before opponent)
    if(fighter < opponent) {
      # Find opponent row
      opponent_row <- fights_data[Name == opponent]
      
      # Verify the opponent has the fighter listed as their opponent
      if(nrow(opponent_row) > 0 && opponent_row$Opponent == fighter) {
        # Get additional fight info
        weight_class <- fighter_row$WeightClass
        rounds <- fighter_row$Rounds
        
        # Create a fight pair entry
        pair <- data.table(
          Fighter1 = fighter,
          Fighter2 = opponent,
          WeightClass = weight_class,
          Rounds = rounds,
          FightID = paste(fighter, "vs", opponent)
        )
        
        # Add to pairs table
        fighter_pairs <- rbind(fighter_pairs, pair)
      }
    }
  }
  
  return(fighter_pairs)
}

generate_chalk_field <- function(optimal_lineups, n_field = 50, platform = "DK") {
  setDT(optimal_lineups)
  roster_size <- if (platform == "DK") DK_ROSTER_SIZE else FD_ROSTER_SIZE
  
  if (!"CumulativeOwnership" %in% names(optimal_lineups)) {
    return(data.table())
  }
  
  setorder(optimal_lineups, -CumulativeOwnership)
  chalk_lineups <- head(optimal_lineups, min(n_field, nrow(optimal_lineups)))
  
  fighter_cols <- paste0("Fighter", 1:roster_size)
  keep_cols <- c(fighter_cols, "TotalSalary", "CumulativeOwnership", "GeometricMeanOwnership")
  
  if (platform == "FD" && "MVP" %in% names(chalk_lineups)) {
    keep_cols <- c("MVP", keep_cols)
  }
  
  keep_cols <- intersect(keep_cols, names(chalk_lineups))
  chalk_lineups <- chalk_lineups[, ..keep_cols]
  
  return(chalk_lineups)
}

# Pre-compute score matrix - OPTIMIZED VERSION
create_score_matrix <- function(simulation_results, platform = "DK") {
  fantasy_col <- if (platform == "DK") "DKScore" else "FDScore"
  
  setDT(simulation_results)
  
  # Get unique fighters and sim IDs
  fighters <- unique(simulation_results$Name)
  sim_ids <- unique(simulation_results$SimID)
  
  cat("Creating score matrix:", length(fighters), "fighters x", length(sim_ids), "sims\n")
  
  # Use data.table's dcast for much faster matrix creation
  score_matrix <- dcast(
    simulation_results[, c("SimID", "Name", fantasy_col), with = FALSE],
    SimID ~ Name,
    value.var = fantasy_col,
    fill = 0
  )
  
  # Extract the matrix part (remove SimID column)
  sim_id_col <- score_matrix$SimID
  score_matrix[, SimID := NULL]
  score_matrix <- as.matrix(score_matrix)
  rownames(score_matrix) <- sim_id_col
  
  cat("Score matrix created successfully\n")
  return(score_matrix)
}

# Calculate lineup score
calculate_lineup_score_fast <- function(fighters, mvp, score_matrix, sim_id_idx, platform = "DK") {
  fighter_scores <- score_matrix[sim_id_idx, fighters]
  
  if (platform == "FD" && !is.null(mvp) && mvp %in% fighters) {
    mvp_score <- score_matrix[sim_id_idx, mvp]
    non_mvp_scores <- fighter_scores[fighters != mvp]
    total_score <- sum(non_mvp_scores, na.rm = TRUE) + (mvp_score * 1.5)
  } else {
    total_score <- sum(fighter_scores, na.rm = TRUE)
  }
  
  return(total_score)
}

# Pre-calculate ALL chalk scores for ALL simulations - VECTORIZED
precalculate_all_chalk_scores <- function(chalk_field, score_matrix, platform = "DK") {
  roster_size <- if (platform == "DK") DK_ROSTER_SIZE else FD_ROSTER_SIZE
  n_chalk <- nrow(chalk_field)
  n_sims <- nrow(score_matrix)
  
  # Pre-allocate result matrix: rows = sims, cols = chalk lineups
  chalk_scores_matrix <- matrix(0, nrow = n_sims, ncol = n_chalk)
  
  # Calculate scores for all simulations at once for each chalk lineup
  for (i in 1:n_chalk) {
    fighters <- unlist(chalk_field[i, paste0("Fighter", 1:roster_size), with = FALSE])
    
    # Vectorized: get scores for all sims at once
    lineup_scores <- rowSums(score_matrix[, fighters, drop = FALSE])
    
    # Handle FanDuel MVP multiplier if needed
    if (platform == "FD" && "MVP" %in% names(chalk_field)) {
      mvp <- chalk_field$MVP[i]
      if (!is.null(mvp) && mvp %in% fighters) {
        mvp_scores <- score_matrix[, mvp]
        # Add 0.5x multiplier to MVP (already counted once, add 0.5 more)
        lineup_scores <- lineup_scores + (mvp_scores * 0.5)
      }
    }
    
    chalk_scores_matrix[, i] <- lineup_scores
  }
  
  return(chalk_scores_matrix)
}

# Main contest simulator - USES ALL AVAILABLE SIMS
run_contest_simulator_optimized <- function(optimal_lineups, simulation_results, filters,
                                            platform = "DK", n_field = 50) {
  
  cat("\n=== Contest Simulator Started ===\n")
  cat("Generating chalk field with", n_field, "highest ownership lineups...\n")
  
  chalk_field <- generate_chalk_field(optimal_lineups, n_field, platform)
  
  if (nrow(chalk_field) == 0) {
    return(list(error = "No chalk field lineups generated"))
  }
  cat("??? Chalk field generated:", nrow(chalk_field), "lineups\n\n")
  
  cat("Applying filters to optimal lineups...\n")
  filtered_lineups <- filter_contest_lineups(optimal_lineups, filters, platform)
  
  if (nrow(filtered_lineups) == 0) {
    return(list(error = "No lineups match the current filters"))
  }
  
  
  cat("Testing", nrow(filtered_lineups), "lineups...\n\n")
  
  cat("Pre-computing fighter score matrix...\n")
  score_matrix <- create_score_matrix(simulation_results, platform)
  sim_ids <- rownames(score_matrix)
  n_sims <- length(sim_ids)
  
  cat("??? Using ALL", n_sims, "available simulations\n\n")
  
  roster_size <- if (platform == "DK") DK_ROSTER_SIZE else FD_ROSTER_SIZE
  contest_size <- nrow(chalk_field) + 1
  
  thresholds <- list(
    DoubleUp = ceiling(contest_size * 0.45),
    FiveX = ceiling(contest_size * 0.20),
    TenX = ceiling(contest_size * 0.10)
  )
  
  cat("Contest details:\n")
  cat("  - Contest size:", contest_size, "entries\n")
  cat("  - DoubleUp cash line: Top", thresholds$DoubleUp, "(45%)\n")
  cat("  - 5x cash line: Top", thresholds$FiveX, "(20%)\n")
  cat("  - 10x cash line: Top", thresholds$TenX, "(10%)\n\n")
  
  # OPTIMIZATION: Pre-calculate ALL chalk scores for ALL simulations ONCE
  cat("Pre-calculating chalk field scores for all simulations...\n")
  chalk_scores_matrix <- precalculate_all_chalk_scores(chalk_field, score_matrix, platform)
  cat("b Chalk scores pre-calculated\n\n")
  
  cat("=== Starting Contest Simulations ===\n")
  
  results <- data.table()
  n_test <- nrow(filtered_lineups)
  
  for (lineup_idx in 1:n_test) {
    if (lineup_idx == 1 || lineup_idx %% 10 == 0) {
      cat("Processing lineup", lineup_idx, "of", n_test, "...\n")
    }
    
    user_lineup <- filtered_lineups[lineup_idx]
    user_fighters <- unlist(user_lineup[, paste0("Fighter", 1:roster_size), with = FALSE])
    user_mvp <- if (platform == "FD" && "MVP" %in% names(user_lineup)) {
      user_lineup$MVP
    } else {
      NULL
    }
    
    # OPTIMIZATION: Vectorized user score calculation for all sims at once
    user_scores_all_sims <- rowSums(score_matrix[, user_fighters, drop = FALSE])
    
    # Handle FanDuel MVP multiplier
    if (platform == "FD" && !is.null(user_mvp) && user_mvp %in% user_fighters) {
      mvp_scores <- score_matrix[, user_mvp]
      user_scores_all_sims <- user_scores_all_sims + (mvp_scores * 0.5)
    }
    
    # OPTIMIZATION: Vectorized ranking for all simulations at once
    # For each simulation, combine user score with chalk scores and rank
    cash_counts <- list(DoubleUp = 0, FiveX = 0, TenX = 0)
    
    for (sim_idx in 1:n_sims) {
      user_score <- user_scores_all_sims[sim_idx]
      chalk_scores <- chalk_scores_matrix[sim_idx, ]
      
      # Count how many chalk lineups beat the user
      n_better <- sum(chalk_scores > user_score)
      user_rank <- n_better + 1
      
      if (user_rank <= thresholds$DoubleUp) cash_counts$DoubleUp <- cash_counts$DoubleUp + 1
      if (user_rank <= thresholds$FiveX) cash_counts$FiveX <- cash_counts$FiveX + 1
      if (user_rank <= thresholds$TenX) cash_counts$TenX <- cash_counts$TenX + 1
    }
    
    result_row <- data.table(
      LineupIndex = lineup_idx,
      DoubleUpRate = round((cash_counts$DoubleUp / n_sims) * 100, 2),
      FiveXRate = round((cash_counts$FiveX / n_sims) * 100, 2),
      TenXRate = round((cash_counts$TenX / n_sims) * 100, 2)
    )
    
    if (platform == "FD" && "MVP" %in% names(user_lineup)) {
      result_row$MVP <- user_lineup$MVP
    }
    
    for (i in 1:roster_size) {
      result_row[[paste0("Fighter", i)]] <- user_lineup[[paste0("Fighter", i)]]
    }
    
    if ("TotalSalary" %in% names(user_lineup)) {
      result_row$TotalSalary <- user_lineup$TotalSalary
    }
    if ("CumulativeOwnership" %in% names(user_lineup)) {
      result_row$CumulativeOwnership <- user_lineup$CumulativeOwnership
    }
    if ("GeometricMeanOwnership" %in% names(user_lineup)) {
      result_row$GeometricMeanOwnership <- user_lineup$GeometricMeanOwnership
    }
    
    results <- rbind(results, result_row)
  }
  
  setorder(results, -DoubleUpRate)
  
  cat("\n=== Contest Simulation Complete ===\n")
  cat("??? Tested", nrow(results), "lineups\n")
  cat("??? Average DoubleUp rate:", round(mean(results$DoubleUpRate), 1), "%\n")
  cat("??? Average 5x rate:", round(mean(results$FiveXRate), 1), "%\n")
  cat("??? Average 10x rate:", round(mean(results$TenXRate), 1), "%\n\n")
  
  return(list(
    results = results,
    chalk_field_size = nrow(chalk_field),
    n_sims = n_sims,
    lineups_tested = nrow(results)
  ))
}

# Filter lineups (same as before)
filter_contest_lineups <- function(optimal_lineups, filters, platform = "DK") {
  filtered <- copy(optimal_lineups)
  
  if (!is.null(filters$min_top1_count) && "Top1Count" %in% names(filtered)) {
    filtered <- filtered[Top1Count >= filters$min_top1_count]
  }
  if (!is.null(filters$min_top2_count) && "Top2Count" %in% names(filtered)) {
    filtered <- filtered[Top2Count >= filters$min_top2_count]
  }
  if (!is.null(filters$min_top3_count) && "Top3Count" %in% names(filtered)) {
    filtered <- filtered[Top3Count >= filters$min_top3_count]
  }
  if (!is.null(filters$min_top5_count) && "Top5Count" %in% names(filtered)) {
    filtered <- filtered[Top5Count >= filters$min_top5_count]
  }
  
  if (!is.null(filters$min_cumulative_ownership) && "CumulativeOwnership" %in% names(filtered)) {
    filtered <- filtered[CumulativeOwnership >= filters$min_cumulative_ownership & 
                           CumulativeOwnership <= filters$max_cumulative_ownership]
  }
  
  if (!is.null(filters$min_geometric_mean) && "GeometricMeanOwnership" %in% names(filtered)) {
    filtered <- filtered[GeometricMeanOwnership >= filters$min_geometric_mean & 
                           GeometricMeanOwnership <= filters$max_geometric_mean]
  }
  
  if (!is.null(filters$excluded_fighters) && length(filters$excluded_fighters) > 0) {
    roster_size <- if (platform == "DK") DK_ROSTER_SIZE else FD_ROSTER_SIZE
    for (i in 1:roster_size) {
      fighter_col <- paste0("Fighter", i)
      if (fighter_col %in% names(filtered)) {
        filtered <- filtered[!(get(fighter_col) %in% filters$excluded_fighters)]
      }
    }
    
    if (platform == "FD" && "MVP" %in% names(filtered)) {
      filtered <- filtered[!(MVP %in% filters$excluded_fighters)]
    }
  }
  
  return(filtered)
}

# COMPACT filter UI - stacked vertically
generate_contest_filter_ui <- function(platform, optimal_data) {
  if (is.null(optimal_data) || nrow(optimal_data) == 0) {
    return(p("No optimal lineups available. Generate optimal lineups first."))
  }
  
  ownership_range <- if ("CumulativeOwnership" %in% names(optimal_data)) {
    c(floor(min(optimal_data$CumulativeOwnership, na.rm = TRUE)), 
      ceiling(max(optimal_data$CumulativeOwnership, na.rm = TRUE)))
  } else {
    c(100, 300)
  }
  
  geometric_range <- if ("GeometricMeanOwnership" %in% names(optimal_data)) {
    c(floor(min(optimal_data$GeometricMeanOwnership, na.rm = TRUE)), 
      ceiling(max(optimal_data$GeometricMeanOwnership, na.rm = TRUE)))
  } else {
    c(15, 35)
  }
  
  roster_size <- if (platform == "DK") DK_ROSTER_SIZE else FD_ROSTER_SIZE
  all_fighters <- c()
  for (i in 1:roster_size) {
    col_name <- paste0("Fighter", i)
    if (col_name %in% names(optimal_data)) {
      all_fighters <- c(all_fighters, unique(optimal_data[[col_name]]))
    }
  }
  if (platform == "FD" && "MVP" %in% names(optimal_data)) {
    all_fighters <- c(all_fighters, unique(optimal_data$MVP))
  }
  all_fighters <- sort(unique(all_fighters))
  
  prefix <- "contest_sim"
  
  tagList(
    # Top counts in one row
    fluidRow(
      column(width = 3, numericInput(paste0(prefix, "_top1_count"), "Min Top 1:", value = 0, min = 0)),
      column(width = 3, numericInput(paste0(prefix, "_top2_count"), "Min Top 2:", value = 0, min = 0)),
      column(width = 3, numericInput(paste0(prefix, "_top3_count"), "Min Top 3:", value = 0, min = 0)),
      column(width = 3, numericInput(paste0(prefix, "_top5_count"), "Min Top 5:", value = 0, min = 0))
    ),
    
    # Ownership sliders in one row
    fluidRow(
      column(width = 6, sliderInput(paste0(prefix, "_ownership_range"), "Cumulative Ownership:",
                                    min = ownership_range[1], max = ownership_range[2], 
                                    value = ownership_range, step = 1, post = "%")),
      column(width = 6, sliderInput(paste0(prefix, "_geometric_range"), "Geometric Mean Ownership:",
                                    min = geometric_range[1], max = geometric_range[2], 
                                    value = geometric_range, step = 1, post = "%"))
    ),
    
    # Exclusions
    fluidRow(
      column(width = 12, selectizeInput(paste0(prefix, "_excluded_fighters"), "Exclude Fighters:",
                                        choices = all_fighters, multiple = TRUE,
                                        options = list(plugins = list('remove_button'),
                                                       placeholder = 'Select fighters to exclude')))
    )
  )
}

# COMPACT UI - tighter layout
contest_simulator_tab_ui <- function() {
  tabItem(
    tabName = "contest_sim",
    
    fluidRow(
      box(
        title = "Contest Simulator",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        
        # Settings row - compact
        fluidRow(
          column(width = 3, 
                 selectInput("contest_sim_platform", "Platform:",
                             choices = c("DraftKings" = "DK", "FanDuel" = "FD"), 
                             selected = "DK")),
          column(width = 3, 
                 numericInput("contest_chalk_field_size", "Chalk Field Size:",
                              value = 50, min = 10, max = 100, step = 10)),
          column(width = 6,
                 div(style = "margin-top: 25px;",
                     p(style = "margin: 0; font-size: 13px;",
                       strong("Payouts:"), " Double-Up (top 45%), 5x (top 20%), 10x (top 10%)"),
                     p(style = "margin: 0; font-size: 12px; color: #666;",
                       "Uses all available simulations for maximum accuracy")))
        ),
        
        hr(style = "margin: 10px 0;"),
        
        h5("Lineup Filters", style = "margin: 5px 0 10px 0;"),
        
        # Dynamic filters
        uiOutput("contest_sim_filters_ui"),
        
        # Filtered pool count
        fluidRow(
          column(width = 12,
                 div(style = "background-color: #f5f5f5; padding: 8px; border-radius: 3px; margin: 10px 0;",
                     textOutput("contest_sim_filtered_pool_size")))
        ),
        
        # Run button - centered, compact
        fluidRow(
          column(width = 12, align = "center",
                 actionButton("run_contest_sim", "Run Contest Simulator",
                              class = "btn-primary btn-lg", icon = icon("trophy"),
                              style = "margin: 5px 0;"))
        )
      )
    ),
    
    # Chalk field preview
    fluidRow(
      box(
        title = "Chalk Field Preview",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        
        withSpinner(DTOutput("contest_chalk_field_table"), type = 4, color = "#FFD700")
      )
    ),
    
    # Results
    fluidRow(
      box(
        title = "Contest Simulation Results",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        
        uiOutput("contest_sim_summary"),
        br(),
        withSpinner(DTOutput("contest_sim_results_table"), type = 4, color = "#FFD700"),
        br(),
        downloadButton("download_contest_sim_results", "Download Results", class = "btn-primary")
      )
    )
  )
}


# Helper function to sample from percentile distribution
sample_from_base_percentiles <- function(p5, p10, p25, p50, p75, p90, p95) {
  percentile <- runif(1, 0, 1)
  
  if(percentile <= 0.05) {
    return(p5)
  } else if(percentile <= 0.1) {
    return(p5 + (p10 - p5) * (percentile - 0.05) / 0.05)
  } else if(percentile <= 0.25) {
    return(p10 + (p25 - p10) * (percentile - 0.1) / 0.15)
  } else if(percentile <= 0.5) {
    return(p25 + (p50 - p25) * (percentile - 0.25) / 0.25)
  } else if(percentile <= 0.75) {
    return(p50 + (p75 - p50) * (percentile - 0.5) / 0.25)
  } else if(percentile <= 0.9) {
    return(p75 + (p90 - p75) * (percentile - 0.75) / 0.15)
  } else if(percentile <= 0.95) {
    return(p90 + (p95 - p90) * (percentile - 0.9) / 0.05)
  } else {
    return(p95)
  }
}

# Function to simulate a single iteration of all fights
simulate_all_fights <- function(fights_data, scores_data, fight_pairs) {
  # Create results table
  fight_results <- data.table()
  
  # Iterate through each fight pair
  for(i in 1:nrow(fight_pairs)) {
    fighter1 <- fight_pairs$Fighter1[i]
    fighter2 <- fight_pairs$Fighter2[i]
    weight_class <- fight_pairs$WeightClass[i]
    rounds <- fight_pairs$Rounds[i]
    
    # Get the outcome probabilities for this fight
    fight_outcomes <- scores_data[
      (Winner == fighter1 & Loser == fighter2) | 
        (Winner == fighter2 & Loser == fighter1)
    ]
    
    # Skip if no outcome data for this fight
    if(nrow(fight_outcomes) == 0) next
    
    # Normalize probabilities to ensure they sum to 1
    fight_outcomes[, Winner_Prob := ifelse(is.na(Winner_Prob), 0, Winner_Prob)]
    total_prob <- sum(fight_outcomes$Winner_Prob)
    if(total_prob == 0) next
    fight_outcomes[, Winner_Prob := Winner_Prob / total_prob]
    
    # Sample an outcome based on probabilities
    outcome_idx <- sample(1:nrow(fight_outcomes), 1, prob = fight_outcomes$Winner_Prob)
    selected_outcome <- fight_outcomes[outcome_idx]
    
    # Determine winner and loser
    winner <- selected_outcome$Winner
    loser <- selected_outcome$Loser
    outcome <- selected_outcome$Outcome
    
    # Generate BASE fantasy scores (without win bonuses)
    # For winner DK base score
    dk_winner_base <- sample_from_base_percentiles(
      selected_outcome$Winner_DK_Base_P5,
      selected_outcome$Winner_DK_Base_P10,
      selected_outcome$Winner_DK_Base_P25,
      selected_outcome$Winner_DK_Base_P50,
      selected_outcome$Winner_DK_Base_P75,
      selected_outcome$Winner_DK_Base_P90,
      selected_outcome$Winner_DK_Base_P95
    )
    
    # For winner FD base score
    fd_winner_base <- sample_from_base_percentiles(
      selected_outcome$Winner_FD_Base_P5,
      selected_outcome$Winner_FD_Base_P10,
      selected_outcome$Winner_FD_Base_P25,
      selected_outcome$Winner_FD_Base_P50,
      selected_outcome$Winner_FD_Base_P75,
      selected_outcome$Winner_FD_Base_P90,
      selected_outcome$Winner_FD_Base_P95
    )
    
    # For loser DK base score
    dk_loser_base <- sample_from_base_percentiles(
      selected_outcome$Loser_DK_Base_P5,
      selected_outcome$Loser_DK_Base_P10,
      selected_outcome$Loser_DK_Base_P25,
      selected_outcome$Loser_DK_Base_P50,
      selected_outcome$Loser_DK_Base_P75,
      selected_outcome$Loser_DK_Base_P90,
      selected_outcome$Loser_DK_Base_P95
    )
    
    # For loser FD base score
    fd_loser_base <- sample_from_base_percentiles(
      selected_outcome$Loser_FD_Base_P5,
      selected_outcome$Loser_FD_Base_P10,
      selected_outcome$Loser_FD_Base_P25,
      selected_outcome$Loser_FD_Base_P50,
      selected_outcome$Loser_FD_Base_P75,
      selected_outcome$Loser_FD_Base_P90,
      selected_outcome$Loser_FD_Base_P95
    )
    
    # ADD WIN BONUSES TO WINNER SCORES ONLY
    dk_winner_bonus <- get_win_bonus(outcome, "DK")
    fd_winner_bonus <- get_win_bonus(outcome, "FD")
    
    dk_winner_score <- dk_winner_base + dk_winner_bonus
    fd_winner_score <- fd_winner_base + fd_winner_bonus
    
    # Loser scores remain base only (no win bonus)
    dk_loser_score <- dk_loser_base
    fd_loser_score <- fd_loser_base
    
    # Add result for winner
    winner_result <- data.table(
      Name = winner,
      Opponent = loser,
      Result = "Win",
      Outcome = outcome,
      DKScore = dk_winner_score,
      FDScore = fd_winner_score
    )
    
    # Add result for loser
    loser_result <- data.table(
      Name = loser,
      Opponent = winner,
      Result = "Loss",
      Outcome = outcome,
      DKScore = dk_loser_score,
      FDScore = fd_loser_score
    )
    
    # Add to results table
    fight_results <- rbind(fight_results, winner_result, loser_result)
  }
  
  return(fight_results)
}

# Main simulation function
run_mma_simulations <- function(input_data, n_sims = 1000, batch_size = 100) {
  # Extract necessary data
  fights_data <- input_data$fights
  scores_data <- input_data$scores
  fight_pairs <- input_data$fight_pairs
  
  # Determine which platforms are active
  has_dk <- "DKSalary" %in% names(fights_data)
  has_fd <- "FDSalary" %in% names(fights_data)
  
  # Pre-allocate results for all simulations
  all_results <- vector("list", n_sims)
  
  # Run simulations
  for(sim in 1:n_sims) {
    all_results[[sim]] <- simulate_all_fights(fights_data, scores_data, fight_pairs)
    all_results[[sim]][, SimID := sim]
    
    # Progress reporting
    if(sim %% 100 == 0) {
      cat(sprintf("Completed %d/%d simulations (%.1f%%)\n", 
                  sim, n_sims, sim/n_sims*100))
    }
  }
  
  # Combine all results (use fill=TRUE to handle missing columns)
  combined_results <- rbindlist(all_results, fill = TRUE)
  
  # Add fighter information to results
  combined_results <- merge(
    combined_results,
    fights_data[, .(Name, DKID, FDID, DKSalary, FDSalary, DKOwn, FDOwn)],
    by = "Name",
    all.x = TRUE
  )
  
  # Add showdown columns (CPTID, SDID, SDSal)
  combined_results <- merge(
    combined_results,
    fights_data[, .(Name, CPTID, SDID, SDSal)],
    by = "Name",
    all.x = TRUE
  )
  
  # Add captain score column (1.5x DK score)
  combined_results[, captain_score := DKScore * 1.5]
  
  # Calculate captain salary
  combined_results[, CPTSal := ifelse(!is.na(SDSal), SDSal * 1.5, NA)]
  
  # Return results and platform availability
  return(list(
    results = combined_results,
    has_dk = has_dk,
    has_fd = has_fd
  ))
}

# Analysis functions for fight outcomes - focused on input file metrics
analyze_fight_outcomes <- function(sim_results) {
  # Use data.table for faster aggregation
  setDT(sim_results)
  
  # Pre-calculate all metrics in one pass to reduce memory operations
  outcomes <- sim_results[, .(
    Win_Rate = mean(Result == "Win", na.rm = TRUE) * 100,
    Loss_Rate = mean(Result == "Loss", na.rm = TRUE) * 100
  ), by = .(Name)]
  
  # Add fighter's salary and ownership information
  fighter_info <- unique(sim_results[, .(Name, DKSalary, FDSalary, DKOwn, FDOwn)])
  outcomes <- merge(outcomes, fighter_info, by = "Name", all.x = TRUE)
  
  # Ensure all columns are numeric before rounding
  numeric_cols <- setdiff(names(outcomes), c("Name", "WeightClass"))
  for (col in numeric_cols) {
    if(!is.null(outcomes[[col]])) {
      outcomes[, (col) := round(as.numeric(get(col)), 1)]
    }
  }
  
  # Sort by Win Rate in descending order
  setorder(outcomes, -Win_Rate)
  
  return(outcomes)
}

# Simplified fantasy scoring analysis
analyze_fantasy_scoring <- function(sim_results) {
  # Ensure data.table
  setDT(sim_results)
  
  # Create results containers for both platforms
  dk_results <- data.table()
  fd_results <- data.table()
  
  # Process DraftKings scores
  if("DKScore" %in% names(sim_results)) {
    # Calculate total fights, wins and win rate
    dk_stats <- sim_results[, .(
      TotalFights = .N,
      Wins = sum(Result == "Win"),
      DKSalary = first(DKSalary),
      DKOwn = first(DKOwn)
    ), by = Name]
    
    dk_stats[, WinRate := (Wins / TotalFights)]
    
    # Calculate overall median and average
    dk_stats[, `:=`(
      Median_DKScore = sim_results[Name == .BY$Name, median(DKScore, na.rm = TRUE)],
      Avg_DKScore = sim_results[Name == .BY$Name, mean(DKScore, na.rm = TRUE)]
    ), by = Name]
    
    # Calculate win-only median and average
    dk_stats[, `:=`(
      Win_Median = sim_results[Name == .BY$Name & Result == "Win", median(DKScore, na.rm = TRUE)]
    ), by = Name]
    
    # Calculate PPD based on median
    dk_stats[, DKPPD := Median_DKScore / (DKSalary/1000)]
    
    # Round numeric columns
    numeric_cols <- setdiff(names(dk_stats), c("Name", "DKOwn", "WinRate"))
    for (col in numeric_cols) {
      dk_stats[, (col) := round(get(col), 1)]
    }
    
    # Sort by median score
    setorder(dk_stats, -Median_DKScore)
    
    dk_results <- dk_stats %>% 
      select(-TotalFights, -Wins)
  }
  
  # Process FanDuel scores
  if("FDScore" %in% names(sim_results)) {
    # Calculate total fights, wins and win rate
    fd_stats <- sim_results[, .(
      TotalFights = .N,
      Wins = sum(Result == "Win"),
      FDSalary = first(FDSalary),
      FDOwn = first(FDOwn)
    ), by = Name]
    
    fd_stats[, WinRate := (Wins / TotalFights)]
    
    # Calculate overall median and average
    fd_stats[, `:=`(
      Median_FDScore = sim_results[Name == .BY$Name, median(FDScore, na.rm = TRUE)],
      Avg_FDScore = sim_results[Name == .BY$Name, mean(FDScore, na.rm = TRUE)]
    ), by = Name]
    
    # Calculate win-only median and average
    fd_stats[, `:=`(
      Win_Median = sim_results[Name == .BY$Name & Result == "Win", median(FDScore, na.rm = TRUE)]
    ), by = Name]
    
    # Calculate PPD based on median
    fd_stats[, FDPPD := Median_FDScore / (FDSalary)]
    
    # Round numeric columns
    numeric_cols <- setdiff(names(fd_stats), c("Name", "FDOwn", "WinRate"))
    for (col in numeric_cols) {
      fd_stats[, (col) := round(get(col), 1)]
    }
    
    # Sort by median score
    setorder(fd_stats, -Median_FDScore)
    
    fd_results <- fd_stats %>% 
      select(-TotalFights, -Wins)
  }
  
  # Return a list with both platforms' results
  return(list(
    dk = dk_results,
    fd = fd_results
  ))
}

# Simulation accuracy analysis focusing on outcome probabilities
analyze_simulation_accuracy <- function(sim_results, scores_data) {
  # Ensure data.table format
  setDT(sim_results)
  setDT(scores_data)
  
  # Create a data.table to store accuracy metrics
  accuracy <- data.table()
  
  # Get unique fighter pairs
  fight_pairs <- unique(scores_data[, .(Winner, Loser, Outcome, Winner_Prob)])
  
  # Pre-allocate results
  accuracy_rows <- nrow(fight_pairs)
  accuracy <- data.table(
    Winner = character(accuracy_rows),
    Loser = character(accuracy_rows),
    Outcome = character(accuracy_rows),
    Expected = numeric(accuracy_rows),
    Observed = numeric(accuracy_rows),
    Difference = numeric(accuracy_rows)
  )
  
  # Iterate through each outcome
  for (i in 1:nrow(fight_pairs)) {
    winner <- fight_pairs$Winner[i]
    loser <- fight_pairs$Loser[i]
    outcome <- fight_pairs$Outcome[i]
    expected_prob <- fight_pairs$Winner_Prob[i]
    
    # Calculate observed probability from simulations - only count fights where this fighter won
    # First find all fights between these fighters
    fight_matches <- sim_results$Name == winner & sim_results$Opponent == loser |
      sim_results$Name == loser & sim_results$Opponent == winner
    
    # Total fights between these fighters
    total_fights <- sum(fight_matches) / 2  # Divide by 2 as each fight appears twice in data
    
    if (total_fights > 0) {
      # Count fights specifically where this fighter won by this outcome
      observed_fights <- sum(sim_results$Name == winner & 
                               sim_results$Opponent == loser & 
                               sim_results$Result == "Win" &
                               sim_results$Outcome == outcome)
      
      observed_prob <- observed_fights / total_fights
    } else {
      observed_prob <- 0
    }
    
    # Calculate difference
    difference <- abs(expected_prob - observed_prob)
    
    # Add to accuracy table
    accuracy$Winner[i] <- winner
    accuracy$Loser[i] <- loser
    accuracy$Outcome[i] <- outcome
    accuracy$Expected[i] <- expected_prob
    accuracy$Observed[i] <- observed_prob
    accuracy$Difference[i] <- difference
  }
  
  # Remove rows with NA values
  accuracy <- accuracy[!is.na(Winner) & !is.na(Loser)]
  
  # Calculate summary metrics by fight
  fight_accuracy <- accuracy[, .(
    Avg_Difference = mean(Difference, na.rm = TRUE),
    Max_Difference = max(Difference, na.rm = TRUE),
    Outcomes_Count = .N
  ), by = .(Winner, Loser)]
  
  # Calculate overall accuracy
  overall_accuracy <- data.table(
    Avg_Difference = mean(accuracy$Difference, na.rm = TRUE),
    Max_Difference = max(accuracy$Difference, na.rm = TRUE),
    Total_Outcomes = nrow(accuracy)
  )
  
  # Round numeric columns
  numeric_cols <- c("Expected", "Observed", "Difference")
  for (col in numeric_cols) {
    accuracy[, (col) := round(get(col), 3)]
  }
  
  numeric_cols <- c("Avg_Difference", "Max_Difference")
  for (col in numeric_cols) {
    fight_accuracy[, (col) := round(get(col), 3)]
    overall_accuracy[, (col) := round(get(col), 3)]
  }
  
  # Sort by average difference
  setorder(fight_accuracy, Avg_Difference)
  
  return(list(
    detailed = accuracy,
    fight_summary = fight_accuracy,
    overall = overall_accuracy
  ))
}

# Function to analyze performance by specific outcomes from your input data
analyze_performance_by_outcome <- function(sim_results) {
  # Ensure data.table format
  setDT(sim_results)
  
  # Create separate result sets for DK and FD
  dk_outcome_stats <- NULL
  fd_outcome_stats <- NULL
  
  # Process DraftKings scores if available
  if("DKScore" %in% names(sim_results)) {
    dk_outcome_stats <- sim_results[Result == "Win", .(
      DK_Avg_Score = mean(DKScore, na.rm = TRUE),
      DK_Median_Score = median(DKScore, na.rm = TRUE),
      DK_Min_Score = min(DKScore, na.rm = TRUE),
      DK_Max_Score = max(DKScore, na.rm = TRUE),
      DK_Count = .N
    ), by = .(Outcome)]
    
    # Round numeric columns
    numeric_cols <- c("DK_Avg_Score", "DK_Median_Score", "DK_Min_Score", "DK_Max_Score")
    for(col in numeric_cols) {
      dk_outcome_stats[, (col) := round(get(col), 1)]
    }
    
    # Sort by average score
    setorder(dk_outcome_stats, -DK_Avg_Score)
  }
  
  # Process FanDuel scores if available
  if("FDScore" %in% names(sim_results)) {
    fd_outcome_stats <- sim_results[Result == "Win", .(
      FD_Avg_Score = mean(FDScore, na.rm = TRUE),
      FD_Median_Score = median(FDScore, na.rm = TRUE),
      FD_Min_Score = min(FDScore, na.rm = TRUE),
      FD_Max_Score = max(FDScore, na.rm = TRUE),
      FD_Count = .N
    ), by = .(Outcome)]
    
    # Round numeric columns
    numeric_cols <- c("FD_Avg_Score", "FD_Median_Score", "FD_Min_Score", "FD_Max_Score")
    for(col in numeric_cols) {
      fd_outcome_stats[, (col) := round(get(col), 1)]
    }
    
    # Sort by average score
    setorder(fd_outcome_stats, -FD_Avg_Score)
  }
  
  # Merge DK and FD stats if both are available
  if(!is.null(dk_outcome_stats) && !is.null(fd_outcome_stats)) {
    merged_stats <- merge(dk_outcome_stats, fd_outcome_stats, 
                          by = "Outcome", all = TRUE)
    return(merged_stats)
  } else if(!is.null(dk_outcome_stats)) {
    return(dk_outcome_stats)
  } else if(!is.null(fd_outcome_stats)) {
    return(fd_outcome_stats)
  } else {
    return(NULL)
  }
}


# Function to calculate filtered pool rates for fighter exposure
calculate_filtered_pool_rates <- function(optimal_lineups, filters, platform = "dk") {
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame())
  }
  
  # Apply filters to get filtered lineups
  filtered_lineups <- as.data.table(optimal_lineups)
  
  # Apply Top Count filters with NA safety
  if (!is.null(filters$min_top1_count) && filters$min_top1_count > 0 && "Top1Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(Top1Count) & Top1Count >= filters$min_top1_count]
  }
  
  if (!is.null(filters$min_top2_count) && filters$min_top2_count > 0 && "Top2Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(Top2Count) & Top2Count >= filters$min_top2_count]
  }
  
  if (!is.null(filters$min_top3_count) && filters$min_top3_count > 0 && "Top3Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(Top3Count) & Top3Count >= filters$min_top3_count]
  }
  
  if (!is.null(filters$min_top5_count) && filters$min_top5_count > 0 && "Top5Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(Top5Count) & Top5Count >= filters$min_top5_count]
  }
  
  if(platform == "dk") {
    if (!is.null(filters$cumulative_ownership_range) && length(filters$cumulative_ownership_range) == 2 && 
        "CumulativeOwnership" %in% names(filtered_lineups)) {
      cum_vals <- filtered_lineups$CumulativeOwnership[!is.na(filtered_lineups$CumulativeOwnership)]
      if(length(cum_vals) > 0) {
        data_min <- min(cum_vals)
        data_max <- max(cum_vals)
        filter_min <- filters$cumulative_ownership_range[1]
        filter_max <- filters$cumulative_ownership_range[2]
        
        if(filter_max >= data_min && filter_min <= data_max) {
          filtered_lineups <- filtered_lineups[!is.na(CumulativeOwnership) & 
                                                 CumulativeOwnership >= filter_min & 
                                                 CumulativeOwnership <= filter_max]
        }
      }
    }
    
    if (!is.null(filters$geometric_mean_range) && length(filters$geometric_mean_range) == 2 && 
        "GeometricMeanOwnership" %in% names(filtered_lineups)) {
      geom_vals <- filtered_lineups$GeometricMeanOwnership[!is.na(filtered_lineups$GeometricMeanOwnership)]
      if(length(geom_vals) > 0) {
        data_min <- min(geom_vals)
        data_max <- max(geom_vals)
        filter_min <- filters$geometric_mean_range[1]
        filter_max <- filters$geometric_mean_range[2]
        
        if(filter_max >= data_min && filter_min <= data_max) {
          filtered_lineups <- filtered_lineups[!is.na(GeometricMeanOwnership) & 
                                                 GeometricMeanOwnership >= filter_min & 
                                                 GeometricMeanOwnership <= filter_max]
        }
      }
    }
  }
  
  # Apply fighter exclusion filter with NA safety
  if (!is.null(filters$excluded_fighters) && length(filters$excluded_fighters) > 0) {
    if(platform == "dk") {
      fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
    } else if(platform == "sd") {
      fighter_cols <- c("Captain", paste0("Fighter", 1:5))
    } else {
      fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
    }
    
    if(length(fighter_cols) > 0) {
      to_exclude <- rep(FALSE, nrow(filtered_lineups))
      
      for(col in fighter_cols) {
        if(col %in% names(filtered_lineups)) {
          # Add NA safety here too
          col_values <- filtered_lineups[[col]]
          to_exclude <- to_exclude | (!is.na(col_values) & col_values %in% filters$excluded_fighters)
        }
      }
      
      # For FD, also check MVP column
      if(platform == "fd" && "MVP" %in% names(filtered_lineups)) {
        mvp_values <- filtered_lineups$MVP
        to_exclude <- to_exclude | (!is.na(mvp_values) & mvp_values %in% filters$excluded_fighters)
      }
      
      filtered_lineups <- filtered_lineups[!to_exclude]
    }
  }
  
  # Calculate filtered pool rates
  if(nrow(filtered_lineups) == 0) {
    return(data.frame())
  }
  
  # Get all fighters from filtered lineups
  if(platform == "dk") {
    fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
    all_fighters <- unique(unlist(filtered_lineups[, ..fighter_cols]))
  } else if(platform == "sd") {
    all_fighters <- unique(c(
      filtered_lineups$Captain,
      unlist(filtered_lineups[, paste0("Fighter", 1:5)])
    ))
  } else {
    fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
    all_fighters <- unique(unlist(filtered_lineups[, ..fighter_cols]))
    # For FD, also include MVP fighters
    if("MVP" %in% names(filtered_lineups)) {
      mvp_fighters <- unique(filtered_lineups$MVP)
      all_fighters <- unique(c(all_fighters, mvp_fighters))
    }
  }
  
  # Calculate filtered pool rates
  filtered_rates <- data.table(
    Name = all_fighters,
    FilteredPoolRate = 0
  )
  
  # Set fighter_cols for counting based on platform
  if(platform == "dk") {
    count_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
  } else if(platform == "sd") {
    count_cols <- c("Captain", paste0("Fighter", 1:5))
  } else {
    count_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
  }
  
  for(fighter in all_fighters) {
    # Count appearances in filtered lineups
    fighter_appears <- logical(nrow(filtered_lineups))
    for(col in count_cols) {
      if(col %in% names(filtered_lineups)) {
        fighter_appears <- fighter_appears | (filtered_lineups[[col]] == fighter)
      }
    }
    
    # For FD, also check MVP column
    if(platform == "fd" && "MVP" %in% names(filtered_lineups)) {
      fighter_appears <- fighter_appears | (filtered_lineups$MVP == fighter)
    }
    
    filtered_rates[Name == fighter, FilteredPoolRate := (sum(fighter_appears) / nrow(filtered_lineups)) * 100]
  }
  
  return(as.data.frame(filtered_rates))
}

# Function to calculate cumulative ownership and geometric mean for lineups
calculate_lineup_ownership_stats <- function(lineup_data, fighter_ownership_map, platform = "dk") {
  if(is.null(lineup_data) || nrow(lineup_data) == 0) return(lineup_data)
  
  # Determine roster size and fighter columns based on platform
  if(platform == "dk") {
    roster_size <- DK_ROSTER_SIZE
    fighter_cols <- paste0("Fighter", 1:roster_size)
    ownership_col <- "DKOwn"
  } else {
    roster_size <- FD_ROSTER_SIZE
    fighter_cols <- paste0("Fighter", 1:roster_size)
    ownership_col <- "FDOwn"
    # For FD, also include MVP in ownership calculation
    if("MVP" %in% names(lineup_data)) {
      fighter_cols <- c("MVP", fighter_cols)
    }
  }
  
  # Initialize new columns
  lineup_data$CumulativeOwnership <- 0
  lineup_data$GeometricMeanOwnership <- 0
  
  # Calculate for each lineup
  for(i in 1:nrow(lineup_data)) {
    ownership_values <- c()
    
    # Get ownership for each fighter in the lineup
    for(col in fighter_cols) {
      if(col %in% names(lineup_data)) {
        fighter_name <- lineup_data[[col]][i]
        if(!is.na(fighter_name) && fighter_name %in% names(fighter_ownership_map)) {
          ownership <- fighter_ownership_map[fighter_name]
          if(!is.na(ownership)) {
            # Convert to percentage if it's in decimal format (0.17 -> 17%)
            if(ownership < 1) {
              ownership <- ownership * 100
            }
            ownership_values <- c(ownership_values, ownership)
          }
        }
      }
    }
    
    # Calculate cumulative ownership (sum)
    if(length(ownership_values) > 0) {
      lineup_data$CumulativeOwnership[i] <- sum(ownership_values)
      
      # Calculate geometric mean (ownership is already in percentage format)
      ownership_decimals <- ownership_values / 100
      geometric_mean <- exp(mean(log(ownership_decimals[ownership_decimals > 0])))
      lineup_data$GeometricMeanOwnership[i] <- geometric_mean * 100
    }
  }
  
  return(lineup_data)
}

# Optimized DraftKings lineup finder
find_dk_optimal_lineups <- function(sim_data, k = 5) {
  # Ensure data.table
  setDT(sim_data)
  
  # Pre-filter top candidates using a single vectorized operation
  sim_data[, PPD := DKScore / (DKSalary/1000)]
  
  # Get top candidates by both absolute points and value (points per dollar)
  top_points_idx <- order(-sim_data$DKScore)[1:min(20, nrow(sim_data))]
  top_ppd_idx <- order(-sim_data$PPD)[1:min(20, nrow(sim_data))]
  
  # Combine candidates more efficiently
  candidate_idx <- unique(c(top_points_idx, top_ppd_idx))
  candidates <- sim_data[candidate_idx, .(Name, DKSalary, DKScore)]
  
  n <- nrow(candidates)
  if(n < DK_ROSTER_SIZE) return(NULL)
  
  # Pre-allocate constraint matrix for reuse - more efficient
  base_const_mat <- matrix(0, nrow = 2, ncol = n)
  base_const_mat[1, ] <- candidates$DKSalary  # Salary cap
  base_const_mat[2, ] <- 1                    # Roster size
  
  base_const_dir <- c("<=", "==")
  base_const_rhs <- c(DK_SALARY_CAP, DK_ROSTER_SIZE)
  
  # Pre-allocate results with exact size
  lineup_results <- data.frame(
    Lineup = character(k),
    Rank = integer(k),
    stringsAsFactors = FALSE
  )
  
  # Track excluded pairs for diversity
  excluded_pairs <- list()
  
  # Find k lineups with more efficient LP setup
  lineup_count <- 0
  for(i in 1:k) {
    # Create constraint matrix - only regenerate what's needed
    if(length(excluded_pairs) > 0) {
      const_rows <- 2 + length(excluded_pairs)
      const.mat <- matrix(0, nrow = const_rows, ncol = n)
      const.mat[1:2, ] <- base_const_mat
      
      const.dir <- c(base_const_dir, rep("<=", length(excluded_pairs)))
      const.rhs <- c(base_const_rhs, rep(DK_ROSTER_SIZE-1, length(excluded_pairs)))
      
      for(j in 1:length(excluded_pairs)) {
        const.mat[2+j, excluded_pairs[[j]]] <- 1
      }
    } else {
      const.mat <- base_const_mat
      const.dir <- base_const_dir
      const.rhs <- base_const_rhs
    }
    
    # Solve with minimal options for speed
    result <- tryCatch({
      suppressWarnings(
        lp("max", candidates$DKScore, const.mat, const.dir, const.rhs, 
           all.bin = TRUE, presolve = 0, compute.sens = 0)
      )
    }, error = function(e) {
      NULL
    })
    
    if(is.null(result) || result$status != 0) break
    
    # Get selected fighters
    selected_indices <- which(result$solution > 0.9)
    
    if(length(selected_indices) != DK_ROSTER_SIZE) break
    
    # Create lineup string
    selected_fighters <- sort(candidates$Name[selected_indices])
    lineup_str <- paste(selected_fighters, collapse = "|")
    
    # Add to results
    lineup_count <- lineup_count + 1
    lineup_results$Lineup[lineup_count] <- lineup_str
    lineup_results$Rank[lineup_count] <- i
    
    # Track for diversity
    excluded_pairs[[length(excluded_pairs) + 1]] <- selected_indices
    
    # Free memory immediately
    rm(result)
  }
  
  # Return only valid results
  if(lineup_count == 0) return(NULL)
  if(lineup_count < k) {
    lineup_results <- lineup_results[1:lineup_count, , drop = FALSE]
  }
  
  return(lineup_results)
}


find_fd_optimal_lineups <- function(sim_data, k = 5) {
  # Ensure data.table
  setDT(sim_data)
  
  # Filter out fighters with zero salary and ensure non-NA values
  sim_data <- sim_data[FDSalary > 0 & !is.na(FDScore)]
  
  # Pre-filter top candidates using a single vectorized operation
  sim_data[, PPD := FDScore / FDSalary]
  
  # Get top candidates by both absolute points and value (points per dollar)
  top_points_idx <- order(-sim_data$FDScore)[1:min(10, nrow(sim_data))]
  top_ppd_idx <- order(-sim_data$PPD)[1:min(12, nrow(sim_data))]
  
  # Combine candidates more efficiently
  candidate_idx <- unique(c(top_points_idx, top_ppd_idx))
  candidates <- sim_data[candidate_idx, .(Name, FDSalary, FDScore)]
  
  n <- nrow(candidates)
  if(n < FD_ROSTER_SIZE) return(NULL)
  
  # Pre-allocate constraint matrix for reuse - more efficient
  base_const_mat <- matrix(0, nrow = 2, ncol = n)
  base_const_mat[1, ] <- candidates$FDSalary  # Salary cap
  base_const_mat[2, ] <- 1                    # Roster size
  
  base_const_dir <- c("<=", "==")
  base_const_rhs <- c(FD_SALARY_CAP, FD_ROSTER_SIZE)
  
  # Pre-allocate results with exact size
  lineup_results <- data.frame(
    Lineup = character(k),
    Rank = integer(k),
    stringsAsFactors = FALSE
  )
  
  # Track excluded pairs for diversity
  excluded_pairs <- list()
  
  # Find k lineups with more efficient LP setup
  lineup_count <- 0
  for(i in 1:k) {
    # Create constraint matrix - only regenerate what's needed
    if(length(excluded_pairs) > 0) {
      const_rows <- 2 + length(excluded_pairs)
      const.mat <- matrix(0, nrow = const_rows, ncol = n)
      const.mat[1:2, ] <- base_const_mat
      
      const.dir <- c(base_const_dir, rep("<=", length(excluded_pairs)))
      const.rhs <- c(base_const_rhs, rep(FD_ROSTER_SIZE-1, length(excluded_pairs)))
      
      for(j in 1:length(excluded_pairs)) {
        const.mat[2+j, excluded_pairs[[j]]] <- 1
      }
    } else {
      const.mat <- base_const_mat
      const.dir <- base_const_dir
      const.rhs <- base_const_rhs
    }
    
    # Solve with minimal options for speed
    result <- tryCatch({
      suppressWarnings(
        lp("max", candidates$FDScore, const.mat, const.dir, const.rhs, 
           all.bin = TRUE, presolve = 0, compute.sens = 0)
      )
    }, error = function(e) {
      NULL
    })
    
    if(is.null(result) || result$status != 0) break
    
    # Get selected fighters
    selected_indices <- which(result$solution > 0.9)
    
    if(length(selected_indices) != FD_ROSTER_SIZE) break
    
    # Create lineup string - use sorted names for consistent identification
    selected_fighters <- sort(candidates$Name[selected_indices])
    lineup_str <- paste(selected_fighters, collapse = "|")
    
    # Add to results
    lineup_count <- lineup_count + 1
    lineup_results$Lineup[lineup_count] <- lineup_str
    lineup_results$Rank[lineup_count] <- i
    
    # Track for diversity
    excluded_pairs[[length(excluded_pairs) + 1]] <- selected_indices
    
    # Free memory immediately
    rm(result)
  }
  
  # Return only valid results
  if(lineup_count == 0) return(NULL)
  if(lineup_count < k) {
    lineup_results <- lineup_results[1:lineup_count, , drop = FALSE]
  }
  
  return(lineup_results)
}


# Complete DraftKings optimal lineup function
count_dk_optimal_lineups <- function(sim_results) {
  # Always use top_k=5
  top_k <- 5
  
  # Get roster size and salary cap
  dk_roster_size <- 6
  dk_salary_cap <- 50000
  
  # Create data.table for better performance
  sim_results_dt <- as.data.table(sim_results)
  
  # Extract only necessary columns
  sim_results_dt <- sim_results_dt[, .(
    SimID, Name, DKSalary, DKScore
  )]
  
  # Split by simulation ID
  all_sim_ids <- unique(sim_results_dt$SimID)
  n_sims <- length(all_sim_ids)
  
  # Initialize lineup storage
  all_lineups <- vector("list", n_sims)
  
  # Process in chunks for memory efficiency
  chunk_size <- 50
  chunks <- ceiling(n_sims / chunk_size)
  
  for(chunk in 1:chunks) {
    start_idx <- (chunk-1) * chunk_size + 1
    end_idx <- min(chunk * chunk_size, n_sims)
    chunk_sim_ids <- all_sim_ids[start_idx:end_idx]
    
    # Process only the current chunk of simulations
    message(sprintf("Processing chunk %d/%d (simulations %d to %d)", 
                    chunk, chunks, start_idx, end_idx))
    
    # Get data for just this chunk of simulations
    chunk_data <- sim_results_dt[SimID %in% chunk_sim_ids]
    
    # Split this chunk by simulation ID
    chunk_sim_list <- split(chunk_data, by = "SimID")
    
    # Process each simulation in this chunk
    for(i in 1:length(chunk_sim_list)) {
      sim_idx <- start_idx + i - 1
      if(sim_idx <= n_sims) {
        sim_data <- chunk_sim_list[[i]]
        
        # Calculate points per dollar for filtering
        sim_data[, PPD := DKScore / (DKSalary/1000)]
        
        # Get top candidates by points and PPD
        top_points_idx <- order(-sim_data$DKScore)[1:min(15, nrow(sim_data))]
        top_ppd_idx <- order(-sim_data$PPD)[1:min(15, nrow(sim_data))]
        
        # Combine indices without redundant copies
        candidate_idx <- unique(c(top_points_idx, top_ppd_idx))
        
        # Create candidates data frame
        candidates <- sim_data[candidate_idx, .(Name, DKSalary, DKScore)]
        
        n <- nrow(candidates)
        if(n < dk_roster_size) {
          all_lineups[[sim_idx]] <- NULL
          next
        }
        
        # Initialize results
        lineup_results <- data.frame(
          Lineup = character(top_k),
          Rank = integer(top_k),
          stringsAsFactors = FALSE
        )
        
        # Base constraint matrix that doesn't change
        base_const_mat <- matrix(0, nrow = 2, ncol = n)
        base_const_mat[1, ] <- candidates$DKSalary  # Salary cap
        base_const_mat[2, ] <- 1                    # Roster size
        
        base_const_dir <- c("<=", "==")
        base_const_rhs <- c(dk_salary_cap, dk_roster_size)
        
        # Track excluded pairs for diversity
        excluded_pairs <- list()
        
        # Find k lineups
        lineup_count <- 0
        for(j in 1:top_k) {
          if(length(excluded_pairs) > 0) {
            const_rows <- 2 + length(excluded_pairs)
            const.mat <- matrix(0, nrow = const_rows, ncol = n)
            const.mat[1:2, ] <- base_const_mat
            
            const.dir <- c(base_const_dir, rep("<=", length(excluded_pairs)))
            const.rhs <- c(base_const_rhs, rep(dk_roster_size-1, length(excluded_pairs)))
            
            for(k in 1:length(excluded_pairs)) {
              const.mat[2+k, excluded_pairs[[k]]] <- 1
            }
          } else {
            const.mat <- base_const_mat
            const.dir <- base_const_dir
            const.rhs <- base_const_rhs
          }
          
          # Solve with minimal options for speed
          result <- tryCatch({
            suppressWarnings(
              lp("max", candidates$DKScore, const.mat, const.dir, const.rhs, 
                 all.bin = TRUE, presolve = 0, compute.sens = 0)
            )
          }, error = function(e) {
            NULL
          })
          
          if(is.null(result) || result$status != 0) break
          
          # Get selected fighters
          selected_indices <- which(result$solution > 0.9)
          
          if(length(selected_indices) != dk_roster_size) break
          
          # Create lineup string
          selected_fighters <- sort(candidates$Name[selected_indices])
          lineup_str <- paste(selected_fighters, collapse = "|")
          
          # Add to results
          lineup_count <- lineup_count + 1
          lineup_results$Lineup[lineup_count] <- lineup_str
          lineup_results$Rank[lineup_count] <- j
          
          # Track for diversity
          excluded_pairs[[length(excluded_pairs) + 1]] <- selected_indices
          
          # Free memory
          rm(result)
        }
        
        # Trim results if needed
        if(lineup_count == 0) {
          all_lineups[[sim_idx]] <- NULL
        } else if(lineup_count < top_k) {
          all_lineups[[sim_idx]] <- lineup_results[1:lineup_count, , drop = FALSE]
        } else {
          all_lineups[[sim_idx]] <- lineup_results
        }
      }
    }
    
    # Clean up chunk variables
    rm(chunk_data, chunk_sim_list)
    gc(verbose = FALSE, full = TRUE)
    
    # Progress reporting
    cat(sprintf("Processed %d/%d simulations (%.1f%%)\n", 
                min(end_idx, n_sims), n_sims, 
                min(end_idx, n_sims) / n_sims * 100))
  }
  
  # Filter out NULL results
  valid_lineups <- all_lineups[!sapply(all_lineups, is.null)]
  
  # Return NULL if no valid lineups
  if(length(valid_lineups) == 0) return(NULL)
  
  # Combine results
  combined_lineups <- do.call(rbind, valid_lineups)
  
  # Count lineup appearances by rank
  lineup_table <- table(combined_lineups$Lineup, combined_lineups$Rank)
  
  # Create result dataframe
  lineup_data <- data.frame(
    Lineup = rownames(lineup_table),
    stringsAsFactors = FALSE
  )
  
  # Add individual rank counts
  for (i in 1:top_k) {
    col_name <- paste0("Rank", i, "Count")
    lineup_data[[col_name]] <- if(as.character(i) %in% colnames(lineup_table)) {
      lineup_table[, as.character(i)]
    } else {
      0
    }
  }
  
  # Add cumulative counts
  lineup_data$Top1Count <- lineup_data$Rank1Count
  lineup_data$Top2Count <- lineup_data$Rank1Count + lineup_data$Rank2Count
  lineup_data$Top3Count <- lineup_data$Rank1Count + lineup_data$Rank2Count + lineup_data$Rank3Count
  lineup_data$Top5Count <- rowSums(lineup_data[, paste0("Rank", 1:5, "Count")])
  
  # Create salary and ownership lookup tables
  first_sim <- sim_results_dt[SimID == all_sim_ids[1]]
  
  # Get salary data
  salary_lookup <- unique(sim_results[!duplicated(sim_results$Name), c("Name", "DKSalary")])
  
  # Calculate total salary efficiently
  lineup_data$TotalSalary <- sapply(lineup_data$Lineup, function(lineup_str) {
    fighters <- strsplit(lineup_str, "\\|")[[1]]
    salaries <- sapply(fighters, function(f) {
      match_row <- which(salary_lookup$Name == f)
      if(length(match_row) > 0) salary_lookup$DKSalary[match_row[1]] else 0
    })
    sum(salaries, na.rm = TRUE)
  })
  
  # Get ownership data if available
  ownership_map <- NULL
  if("DKOwn" %in% names(sim_results)) {
    ownership_data <- unique(sim_results[!is.na(sim_results$DKOwn), c("Name", "DKOwn")])
    if(nrow(ownership_data) > 0) {
      ownership_map <- setNames(ownership_data$DKOwn, ownership_data$Name)
    }
  }
  
  # Sort by Top1Count
  lineup_data <- lineup_data[order(-lineup_data$Top1Count), ]
  
  # Split fighter columns for display
  fighter_cols <- do.call(rbind, strsplit(lineup_data$Lineup, "\\|"))
  
  # Validate column count
  if(ncol(fighter_cols) != dk_roster_size) {
    warning(paste("Expected", dk_roster_size, "fighter columns, got", ncol(fighter_cols)))
    return(NULL)
  }
  
  colnames(fighter_cols) <- paste0("Fighter", 1:dk_roster_size)
  
  # Create final result
  result <- cbind(
    as.data.frame(fighter_cols),
    lineup_data[, grep("Count$|Salary$", names(lineup_data), value = TRUE), drop = FALSE]
  )
  
  # Calculate ownership statistics if ownership data is available
  if(!is.null(ownership_map) && length(ownership_map) > 0) {
    result <- calculate_lineup_ownership_stats(result, ownership_map, "dk")
  } else {
    # Add default ownership columns if no ownership data
    result$CumulativeOwnership <- 0
    result$GeometricMeanOwnership <- 0
  }
  
  # Clean up to free memory
  rm(lineup_data, fighter_cols, combined_lineups, lineup_table, salary_lookup)
  gc(verbose = FALSE, full = TRUE)
  
  return(result)
}

# Complete FanDuel optimal lineup function
count_fd_optimal_lineups <- function(sim_results) {
  # Always use top_k=5
  top_k <- 5
  
  # Get roster size and salary cap
  fd_roster_size <- 6  # Including 1 MVP
  fd_salary_cap <- 100
  
  # Create data.table for better performance
  sim_results_dt <- as.data.table(sim_results)
  
  # Filter out fighters with zero FD salary
  sim_results_dt <- sim_results_dt[FDSalary > 0]
  
  # Extract only necessary columns
  sim_results_dt <- sim_results_dt[, .(
    SimID, Name, FDSalary, FDScore
  )]
  
  # Split by simulation ID
  all_sim_ids <- unique(sim_results_dt$SimID)
  n_sims <- length(all_sim_ids)
  
  # Initialize lineup storage
  all_lineups <- vector("list", n_sims)
  
  # Process in chunks for memory efficiency
  chunk_size <- 50
  chunks <- ceiling(n_sims / chunk_size)
  
  for(chunk in 1:chunks) {
    start_idx <- (chunk-1) * chunk_size + 1
    end_idx <- min(chunk * chunk_size, n_sims)
    chunk_sim_ids <- all_sim_ids[start_idx:end_idx]
    
    # Process only the current chunk of simulations
    message(sprintf("Processing chunk %d/%d (simulations %d to %d)", 
                    chunk, chunks, start_idx, end_idx))
    
    # Get data for just this chunk of simulations
    chunk_data <- sim_results_dt[SimID %in% chunk_sim_ids]
    
    # Split this chunk by simulation ID
    chunk_sim_list <- split(chunk_data, by = "SimID")
    
    # Process each simulation in this chunk
    for(i in 1:length(chunk_sim_list)) {
      sim_idx <- start_idx + i - 1
      if(sim_idx <= n_sims) {
        sim_data <- chunk_sim_list[[i]]
        
        # Calculate points per dollar for filtering
        sim_data[, PPD := FDScore / FDSalary]
        
        # Get top candidates by points and PPD
        top_points_idx <- order(-sim_data$FDScore)[1:min(15, nrow(sim_data))]
        top_ppd_idx <- order(-sim_data$PPD)[1:min(15, nrow(sim_data))]
        
        # Combine indices without redundant copies
        candidate_idx <- unique(c(top_points_idx, top_ppd_idx))
        
        # Create candidates data frame
        candidates <- sim_data[candidate_idx, .(Name, FDSalary, FDScore)]
        
        n <- nrow(candidates)
        if(n < fd_roster_size) {
          all_lineups[[sim_idx]] <- NULL
          next
        }
        
        # Initialize results
        lineup_results <- data.frame(
          Lineup = character(top_k),
          Rank = integer(top_k),
          stringsAsFactors = FALSE
        )
        
        # Base constraint matrix that doesn't change
        base_const_mat <- matrix(0, nrow = 2, ncol = n)
        base_const_mat[1, ] <- candidates$FDSalary  # Salary cap
        base_const_mat[2, ] <- 1                    # Roster size
        
        base_const_dir <- c("<=", "==")
        base_const_rhs <- c(fd_salary_cap, fd_roster_size)
        
        # Track excluded pairs for diversity
        excluded_pairs <- list()
        
        # Find k lineups
        lineup_count <- 0
        for(j in 1:top_k) {
          if(length(excluded_pairs) > 0) {
            const_rows <- 2 + length(excluded_pairs)
            const.mat <- matrix(0, nrow = const_rows, ncol = n)
            const.mat[1:2, ] <- base_const_mat
            
            const.dir <- c(base_const_dir, rep("<=", length(excluded_pairs)))
            const.rhs <- c(base_const_rhs, rep(fd_roster_size-1, length(excluded_pairs)))
            
            for(k in 1:length(excluded_pairs)) {
              const.mat[2+k, excluded_pairs[[k]]] <- 1
            }
          } else {
            const.mat <- base_const_mat
            const.dir <- base_const_dir
            const.rhs <- base_const_rhs
          }
          
          # Solve with minimal options for speed
          result <- tryCatch({
            suppressWarnings(
              lp("max", candidates$FDScore, const.mat, const.dir, const.rhs, 
                 all.bin = TRUE, presolve = 0, compute.sens = 0)
            )
          }, error = function(e) {
            NULL
          })
          
          if(is.null(result) || result$status != 0) break
          
          # Get selected fighters
          selected_indices <- which(result$solution > 0.9)
          
          if(length(selected_indices) != fd_roster_size) break
          
          # Create lineup string
          selected_fighters <- sort(candidates$Name[selected_indices])
          lineup_str <- paste(selected_fighters, collapse = "|")
          
          # Add to results
          lineup_count <- lineup_count + 1
          lineup_results$Lineup[lineup_count] <- lineup_str
          lineup_results$Rank[lineup_count] <- j
          
          # Track for diversity
          excluded_pairs[[length(excluded_pairs) + 1]] <- selected_indices
          
          # Free memory
          rm(result)
        }
        
        # Identify MVP for each lineup
        if(lineup_count > 0) {
          # Trim results if needed
          if(lineup_count < top_k) {
            lineup_results <- lineup_results[1:lineup_count, , drop = FALSE]
          }
          
          # Add MVP column
          lineup_results$MVP <- character(nrow(lineup_results))
          
          # For each lineup, find the MVP (highest FDScore)
          for(l in 1:nrow(lineup_results)) {
            # Get fighters in this lineup
            lineup_fighters <- unlist(strsplit(lineup_results$Lineup[l], "\\|"))
            
            # Get their scores
            fighter_scores <- sim_data[Name %in% lineup_fighters, .(Name, FDScore)]
            
            # Sort by score descending
            fighter_scores <- fighter_scores[order(-FDScore)]
            
            # MVP is the highest scoring fighter
            if(nrow(fighter_scores) > 0) {
              lineup_results$MVP[l] <- fighter_scores$Name[1]
            }
          }
          
          all_lineups[[sim_idx]] <- lineup_results
        } else {
          all_lineups[[sim_idx]] <- NULL
        }
      }
    }
    
    # Clean up chunk variables
    rm(chunk_data, chunk_sim_list)
    gc(verbose = FALSE, full = TRUE)
    
    # Progress reporting
    cat(sprintf("Processed %d/%d simulations (%.1f%%)\n", 
                min(end_idx, n_sims), n_sims, 
                min(end_idx, n_sims) / n_sims * 100))
  }
  
  # Filter out NULL results
  valid_lineups <- all_lineups[!sapply(all_lineups, is.null)]
  
  # Return NULL if no valid lineups
  if(length(valid_lineups) == 0) return(NULL)
  
  # Combine results
  combined_lineups <- do.call(rbind, valid_lineups)
  
  # Count lineup appearances by rank
  lineup_table <- table(combined_lineups$Lineup, combined_lineups$Rank)
  
  # Count MVP appearances
  mvp_table <- table(combined_lineups$Lineup, combined_lineups$MVP)
  
  # Create result dataframe
  lineup_data <- data.frame(
    Lineup = rownames(lineup_table),
    stringsAsFactors = FALSE
  )
  
  # Add individual rank counts
  for (i in 1:top_k) {
    col_name <- paste0("Rank", i, "Count")
    lineup_data[[col_name]] <- if(as.character(i) %in% colnames(lineup_table)) {
      lineup_table[, as.character(i)]
    } else {
      0
    }
  }
  
  # Add cumulative counts
  lineup_data$Top1Count <- lineup_data$Rank1Count
  lineup_data$Top2Count <- lineup_data$Rank1Count + lineup_data$Rank2Count
  lineup_data$Top3Count <- lineup_data$Rank1Count + lineup_data$Rank2Count + lineup_data$Rank3Count
  lineup_data$Top5Count <- rowSums(lineup_data[, paste0("Rank", 1:5, "Count")])
  
  # Get salary data
  salary_lookup <- unique(sim_results[!duplicated(sim_results$Name), c("Name", "FDSalary")])
  
  # Calculate total salary efficiently
  lineup_data$TotalSalary <- sapply(lineup_data$Lineup, function(lineup_str) {
    fighters <- strsplit(lineup_str, "\\|")[[1]]
    salaries <- sapply(fighters, function(f) {
      match_row <- which(salary_lookup$Name == f)
      if(length(match_row) > 0) salary_lookup$FDSalary[match_row[1]] else 0
    })
    sum(salaries, na.rm = TRUE)
  })
  
  # Get ownership data if available
  ownership_map <- NULL
  if("FDOwn" %in% names(sim_results)) {
    ownership_data <- unique(sim_results[!is.na(sim_results$FDOwn), c("Name", "FDOwn")])
    if(nrow(ownership_data) > 0) {
      ownership_map <- setNames(ownership_data$FDOwn, ownership_data$Name)
    }
  }
  
  # Find most common MVP for each lineup
  lineup_data$MVP <- character(nrow(lineup_data))
  for(i in 1:nrow(lineup_data)) {
    lineup_str <- lineup_data$Lineup[i]
    if(lineup_str %in% rownames(mvp_table)) {
      mvp_counts <- mvp_table[lineup_str, ]
      # Get the MVP with the highest count
      most_common_mvp <- names(mvp_counts)[which.max(mvp_counts)]
      lineup_data$MVP[i] <- most_common_mvp
    }
  }
  
  # Sort by Top1Count
  lineup_data <- lineup_data[order(-lineup_data$Top1Count), ]
  
  # Split fighter columns for display
  fighter_cols <- do.call(rbind, strsplit(lineup_data$Lineup, "\\|"))
  
  # Validate column count
  if(ncol(fighter_cols) != fd_roster_size) {
    warning(paste("Expected", fd_roster_size, "fighter columns, got", ncol(fighter_cols)))
    return(NULL)
  }
  
  colnames(fighter_cols) <- paste0("Fighter", 1:fd_roster_size)
  
  # Create final result
  result <- cbind(
    as.data.frame(fighter_cols),
    lineup_data[, c("MVP", grep("Count$|Salary$", names(lineup_data), value = TRUE)), drop = FALSE]
  )
  
  # Calculate ownership statistics if ownership data is available
  if(!is.null(ownership_map) && length(ownership_map) > 0) {
    result <- calculate_lineup_ownership_stats(result, ownership_map, "fd")
  } else {
    # Add default ownership columns if no ownership data
    result$CumulativeOwnership <- 0
    result$GeometricMeanOwnership <- 0
  }
  
  # Clean up to free memory
  rm(lineup_data, fighter_cols, combined_lineups, lineup_table, salary_lookup)
  gc(verbose = FALSE, full = TRUE)
  
  return(result)
}

# ============================================================================
# DK SHOWDOWN OPTIMAL LINEUP FUNCTIONS
# ============================================================================

# Complete DK Showdown optimal lineup function
count_sd_optimal_lineups <- function(sim_results) {
  top_k <- 5
  
  # Create data.table
  sim_results_dt <- as.data.table(sim_results)
  
  cat("\n=== DK SHOWDOWN OPTIMIZATION DEBUG ===\n")
  cat("Total simulation rows:", nrow(sim_results_dt), "\n")
  cat("Columns present:", paste(names(sim_results_dt), collapse=", "), "\n")
  cat("SDID values:", paste(unique(sim_results_dt$SDID)[1:5], collapse=", "), "...\n")
  cat("SDSal range:", min(sim_results_dt$SDSal, na.rm=TRUE), "to", max(sim_results_dt$SDSal, na.rm=TRUE), "\n")
  
  # Filter to showdown-eligible fighters
  before_filter <- nrow(sim_results_dt)
  sim_results_dt <- sim_results_dt[!is.na(SDID) & SDID > 0 & !is.na(SDSal) & SDSal > 0]
  after_filter <- nrow(sim_results_dt)
  
  cat("After filtering (SDID>0 & SDSal>0):", after_filter, "rows (", 
      length(unique(sim_results_dt$Name)), "unique fighters)\n")
  
  if(nrow(sim_results_dt) == 0) {
    cat("??? NO ELIGIBLE FIGHTERS - all SDID or SDSal are 0 or NA\n")
    cat("Check your input file: SDID and SDSal must be > 0\n")
    message("No showdown-eligible fighters found")
    return(NULL)
  }
  
  cat("Eligible fighters:", paste(unique(sim_results_dt$Name), collapse=", "), "\n")
  cat("Salary range: $", min(sim_results_dt$SDSal), "to $", max(sim_results_dt$SDSal), "\n")
  
  # Ensure captain_score and CPTSal columns exist
  if(!"captain_score" %in% names(sim_results_dt)) {
    cat("Creating captain_score column (DKScore * 1.5)\n")
    sim_results_dt[, captain_score := DKScore * 1.5]
  }
  if(!"CPTSal" %in% names(sim_results_dt)) {
    cat("Creating CPTSal column (SDSal * 1.5)\n")
    sim_results_dt[, CPTSal := SDSal * 1.5]
  }
  
  cat("Captain score range:", min(sim_results_dt$captain_score, na.rm=TRUE), "to", 
      max(sim_results_dt$captain_score, na.rm=TRUE), "\n")
  cat("Captain salary range: $", min(sim_results_dt$CPTSal, na.rm=TRUE), "to $", 
      max(sim_results_dt$CPTSal, na.rm=TRUE), "\n")
  
  # Check salary feasibility
  min_cpt_sal <- min(sim_results_dt$CPTSal, na.rm=TRUE)
  min_5_fighters <- sum(sort(unique(sim_results_dt$SDSal))[1:5])
  min_lineup <- min_cpt_sal + min_5_fighters
  cat("Minimum possible lineup: $", min_lineup, "(feasible:", min_lineup <= 50000, ")\n")
  cat("=====================================\n\n")
  
  # Extract necessary columns
  sim_results_dt <- sim_results_dt[, .(
    SimID, Name, CPTID, SDID, SDSal, CPTSal, DKScore, captain_score
  )]
  
  all_sim_ids <- unique(sim_results_dt$SimID)
  n_sims <- length(all_sim_ids)
  
  all_lineups <- vector("list", n_sims)
  
  chunk_size <- 50
  chunks <- ceiling(n_sims / chunk_size)
  
  for(chunk in 1:chunks) {
    start_idx <- (chunk-1) * chunk_size + 1
    end_idx <- min(chunk * chunk_size, n_sims)
    chunk_sim_ids <- all_sim_ids[start_idx:end_idx]
    
    message(sprintf("Processing SD chunk %d/%d (sims %d to %d)", 
                    chunk, chunks, start_idx, end_idx))
    
    chunk_data <- sim_results_dt[SimID %in% chunk_sim_ids]
    chunk_sim_list <- split(chunk_data, by = "SimID")
    
    for(i in 1:length(chunk_sim_list)) {
      sim_idx <- start_idx + i - 1
      if(sim_idx <= n_sims) {
        sim_data <- chunk_sim_list[[i]]
        
        # Get candidates
        sim_data[, CPT_PPD := captain_score / (CPTSal/1000)]
        sim_data[, FLEX_PPD := DKScore / (SDSal/1000)]
        
        top_cpt_score <- order(-sim_data$captain_score)[1:min(12, nrow(sim_data))]
        top_cpt_ppd <- order(-sim_data$CPT_PPD)[1:min(12, nrow(sim_data))]
        top_flex_score <- order(-sim_data$DKScore)[1:min(15, nrow(sim_data))]
        top_flex_ppd <- order(-sim_data$FLEX_PPD)[1:min(15, nrow(sim_data))]
        
        candidate_idx <- unique(c(top_cpt_score, top_cpt_ppd, top_flex_score, top_flex_ppd))
        candidates <- sim_data[candidate_idx, .(Name, CPTID, SDID, SDSal, CPTSal, DKScore, captain_score)]
        
        n <- nrow(candidates)
        if(n < SD_ROSTER_SIZE) {
          if(sim_idx == 1) {
            cat("??????  Sim", sim_idx, "has only", n, "candidates (need", SD_ROSTER_SIZE, ")\n")
          }
          all_lineups[[sim_idx]] <- NULL
          next
        }
        
        lineup_results <- data.frame(
          Lineup = character(top_k),
          Captain = character(top_k),
          Rank = integer(top_k),
          stringsAsFactors = FALSE
        )
        
        # Create constraint matrix (2n variables: n captains + n flex)
        obj <- c(candidates$captain_score, candidates$DKScore)
        
        base_const_mat <- matrix(0, nrow = 3 + n, ncol = 2*n)
        base_const_mat[1, 1:n] <- candidates$CPTSal
        base_const_mat[1, (n+1):(2*n)] <- candidates$SDSal
        base_const_mat[2, 1:n] <- 1
        base_const_mat[3, (n+1):(2*n)] <- 1
        for(j in 1:n) {
          base_const_mat[3+j, j] <- 1
          base_const_mat[3+j, n+j] <- 1
        }
        
        base_const_dir <- c("<=", "==", "==", rep("<=", n))
        base_const_rhs <- c(SD_SALARY_CAP, 1, 5, rep(1, n))
        
        excluded_lineups <- list()
        lineup_count <- 0
        
        for(j in 1:top_k) {
          if(length(excluded_lineups) > 0) {
            const_rows <- 3 + n + length(excluded_lineups)
            const.mat <- matrix(0, nrow = const_rows, ncol = 2*n)
            const.mat[1:(3+n), ] <- base_const_mat
            
            const.dir <- c(base_const_dir, rep("<=", length(excluded_lineups)))
            const.rhs <- c(base_const_rhs, rep(SD_ROSTER_SIZE-1, length(excluded_lineups)))
            
            for(ex_idx in 1:length(excluded_lineups)) {
              const.mat[3+n+ex_idx, excluded_lineups[[ex_idx]]] <- 1
            }
          } else {
            const.mat <- base_const_mat
            const.dir <- base_const_dir
            const.rhs <- base_const_rhs
          }
          
          result <- tryCatch({
            suppressWarnings(
              lp("max", obj, const.mat, const.dir, const.rhs, 
                 all.bin = TRUE, presolve = 0, compute.sens = 0)
            )
          }, error = function(e) {
            if(sim_idx == 1 && j == 1) {
              cat("??? LP solve error in sim 1:", e$message, "\n")
            }
            NULL
          })
          
          if(is.null(result) || result$status != 0) {
            if(sim_idx == 1 && j == 1) {
              cat("??? LP solve failed: status =", ifelse(is.null(result), "NULL", result$status), "\n")
            }
            break
          }
          
          solution <- result$solution
          captain_idx <- which(solution[1:n] > 0.9)
          flex_idx <- which(solution[(n+1):(2*n)] > 0.9)
          
          if(length(captain_idx) != 1 || length(flex_idx) != 5) break
          
          captain_name <- candidates$Name[captain_idx]
          flex_names <- sort(candidates$Name[flex_idx])
          lineup_str <- paste(c(captain_name, flex_names), collapse = "|")
          
          lineup_count <- lineup_count + 1
          lineup_results$Lineup[lineup_count] <- lineup_str
          lineup_results$Captain[lineup_count] <- captain_name
          lineup_results$Rank[lineup_count] <- j
          
          selected_indices <- c(captain_idx, n + flex_idx)
          excluded_lineups[[length(excluded_lineups) + 1]] <- selected_indices
          
          rm(result)
        }
        
        if(lineup_count == 0) {
          all_lineups[[sim_idx]] <- NULL
        } else if(lineup_count < top_k) {
          all_lineups[[sim_idx]] <- lineup_results[1:lineup_count, , drop = FALSE]
        } else {
          all_lineups[[sim_idx]] <- lineup_results
        }
      }
    }
    
    rm(chunk_data, chunk_sim_list)
    gc(verbose = FALSE, full = TRUE)
    
    cat(sprintf("Processed %d/%d simulations (%.1f%%)\n", 
                min(end_idx, n_sims), n_sims, 
                min(end_idx, n_sims) / n_sims * 100))
  }
  
  valid_lineups <- all_lineups[!sapply(all_lineups, is.null)]
  cat("\n=== LINEUP GENERATION COMPLETE ===\n")
  cat("Simulations processed:", n_sims, "\n")
  cat("Simulations with lineups:", length(valid_lineups), "\n")
  cat("Simulations with NO lineups:", n_sims - length(valid_lineups), "\n")
  
  if(length(valid_lineups) == 0) {
    cat("??? ALL SIMULATIONS FAILED - LP could not find valid lineups\n")
    cat("This usually means salaries are too high or constraints too tight\n")
    return(NULL)
  }
  
  cat("??? Successfully generated lineups from", length(valid_lineups), "simulations\n")
  cat("=====================================\n\n")
  
  combined_lineups <- do.call(rbind, valid_lineups)
  lineup_table <- table(combined_lineups$Lineup, combined_lineups$Rank)
  
  lineup_data <- data.frame(
    Lineup = rownames(lineup_table),
    stringsAsFactors = FALSE
  )
  
  # Add rank counts
  for (i in 1:top_k) {
    col_name <- paste0("Rank", i, "Count")
    lineup_data[[col_name]] <- if(as.character(i) %in% colnames(lineup_table)) {
      lineup_table[, as.character(i)]
    } else {
      0
    }
  }
  
  # Add cumulative counts
  lineup_data$Top1Count <- lineup_data$Rank1Count
  lineup_data$Top2Count <- lineup_data$Rank1Count + lineup_data$Rank2Count
  lineup_data$Top3Count <- lineup_data$Rank1Count + lineup_data$Rank2Count + lineup_data$Rank3Count
  lineup_data$Top5Count <- rowSums(lineup_data[, paste0("Rank", 1:5, "Count")])
  
  # Get captain for each lineup (most common)
  captain_table <- table(combined_lineups$Lineup, combined_lineups$Captain)
  lineup_data$Captain <- character(nrow(lineup_data))
  for(i in 1:nrow(lineup_data)) {
    lineup_str <- lineup_data$Lineup[i]
    if(lineup_str %in% rownames(captain_table)) {
      captain_counts <- captain_table[lineup_str, ]
      lineup_data$Captain[i] <- names(captain_counts)[which.max(captain_counts)]
    }
  }
  
  # Calculate total salary
  salary_lookup <- unique(sim_results[!duplicated(sim_results$Name), c("Name", "SDSal")])
  salary_lookup$CPTSal <- salary_lookup$SDSal * 1.5
  
  lineup_data$TotalSalary <- sapply(lineup_data$Lineup, function(lineup_str) {
    parts <- strsplit(lineup_str, "\\|")[[1]]
    captain <- parts[1]
    flex_fighters <- parts[-1]
    
    # Captain salary
    cpt_match <- which(salary_lookup$Name == captain)
    cpt_sal <- if(length(cpt_match) > 0) salary_lookup$CPTSal[cpt_match[1]] else 0
    
    # Flex salaries
    flex_sals <- sapply(flex_fighters, function(f) {
      match_idx <- which(salary_lookup$Name == f)
      if(length(match_idx) > 0) salary_lookup$SDSal[match_idx[1]] else 0
    })
    
    cpt_sal + sum(flex_sals, na.rm = TRUE)
  })
  
  # Sort by Top1Count
  lineup_data <- lineup_data[order(-lineup_data$Top1Count), ]
  
  # Split into columns
  lineup_parts <- do.call(rbind, strsplit(lineup_data$Lineup, "\\|"))
  
  if(ncol(lineup_parts) != SD_ROSTER_SIZE) {
    warning(paste("Expected", SD_ROSTER_SIZE, "columns, got", ncol(lineup_parts)))
    return(NULL)
  }
  
  # First column is captain, rest are fighters
  result_df <- data.frame(
    Captain = lineup_parts[, 1],
    stringsAsFactors = FALSE
  )
  
  for(i in 2:SD_ROSTER_SIZE) {
    result_df[[paste0("Fighter", i-1)]] <- lineup_parts[, i]
  }
  
  # Add count and salary columns
  result <- cbind(
    result_df,
    lineup_data[, grep("Count$|Salary$", names(lineup_data), value = TRUE), drop = FALSE]
  )
  
  # No ownership stats for showdown
  result$CumulativeOwnership <- 0
  result$GeometricMeanOwnership <- 0
  
  rm(lineup_data, combined_lineups, lineup_table, salary_lookup)
  gc(verbose = FALSE, full = TRUE)
  
  return(result)
}



# Calculate SD fighter exposure
calculate_sd_fighter_exposure <- function(optimal_lineups, fighter_info, random_lineups = NULL) {
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame())
  }
  
  # Determine which lineups to use for exposure calculation
  lineups_for_exposure <- if(!is.null(random_lineups) && nrow(random_lineups) > 0) {
    random_lineups
  } else {
    optimal_lineups
  }
  
  # Get all fighters
  all_fighters <- unique(c(
    optimal_lineups$Captain,
    unlist(optimal_lineups[, paste0("Fighter", 1:5)])
  ))
  
  n_lineups <- nrow(lineups_for_exposure)
  
  # Calculate exposures
  exposure_data <- data.frame(
    Name = all_fighters,
    CaptainExp = 0,
    FighterExp = 0,
    TotalExp = 0,
    stringsAsFactors = FALSE
  )
  
  for(i in 1:nrow(exposure_data)) {
    fighter <- exposure_data$Name[i]
    
    # Captain exposure
    captain_count <- sum(lineups_for_exposure$Captain == fighter, na.rm = TRUE)
    exposure_data$CaptainExp[i] <- (captain_count / n_lineups) * 100
    
    # Fighter exposure (in flex spots)
    fighter_count <- 0
    for(col in paste0("Fighter", 1:5)) {
      fighter_count <- fighter_count + sum(lineups_for_exposure[[col]] == fighter, na.rm = TRUE)
    }
    exposure_data$FighterExp[i] <- (fighter_count / n_lineups) * 100
    
    # Total exposure
    exposure_data$TotalExp[i] <- exposure_data$CaptainExp[i] + exposure_data$FighterExp[i]
  }
  
  # Add fighter info if provided
  if(!is.null(fighter_info) && nrow(fighter_info) > 0) {
    for(i in 1:nrow(exposure_data)) {
      match_idx <- which(fighter_info$Name == exposure_data$Name[i])
      if(length(match_idx) > 0) {
        if("SDSal" %in% names(fighter_info)) {
          exposure_data$SDSal[i] <- fighter_info$SDSal[match_idx[1]]
        }
        if("Median_DKScore" %in% names(fighter_info)) {
          exposure_data$Proj[i] <- fighter_info$Median_DKScore[match_idx[1]]
        }
        if("Proj" %in% names(fighter_info)) {
          exposure_data$Proj[i] <- fighter_info$Proj[match_idx[1]]
        }
      }
    }
  }
  
  # Sort by total exposure
  exposure_data <- exposure_data[order(-exposure_data$TotalExp), ]
  
  return(exposure_data)
}

# SD filtered pool stats
calculate_sd_filtered_pool_stats <- function(optimal_lineups, filters) {
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(list(count = 0))
  }
  
  filtered_lineups <- as.data.table(optimal_lineups)
  
  # Apply Top Count filters
  if (!is.null(filters$min_top1_count) && filters$min_top1_count > 0 && "Top1Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
  }
  
  if (!is.null(filters$min_top2_count) && filters$min_top2_count > 0 && "Top2Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top2Count >= filters$min_top2_count]
  }
  
  if (!is.null(filters$min_top3_count) && filters$min_top3_count > 0 && "Top3Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top3Count >= filters$min_top3_count]
  }
  
  if (!is.null(filters$min_top5_count) && filters$min_top5_count > 0 && "Top5Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top5Count >= filters$min_top5_count]
  }
  
  # Apply fighter exclusion filter
  if (!is.null(filters$excluded_fighters) && length(filters$excluded_fighters) > 0) {
    fighter_cols <- c("Captain", paste0("Fighter", 1:5))
    to_exclude <- rep(FALSE, nrow(filtered_lineups))
    
    for(col in fighter_cols) {
      if(col %in% names(filtered_lineups)) {
        to_exclude <- to_exclude | filtered_lineups[[col]] %in% filters$excluded_fighters
      }
    }
    
    filtered_lineups <- filtered_lineups[!to_exclude]
  }
  
  return(list(count = nrow(filtered_lineups)))
}




calculate_fd_filtered_pool_stats <- function(optimal_lineups, filters) {
  # Validate inputs before proceeding
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  # Ensure we're working with a data.table
  filtered_lineups <- tryCatch({
    as.data.table(optimal_lineups)
  }, error = function(e) {
    # If conversion fails, return empty result
    return(NULL)
  })
  
  if(is.null(filtered_lineups) || nrow(filtered_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  # Apply Top1Count filter safely
  if (!is.null(filters$min_top1_count) && filters$min_top1_count > 0 && "Top1Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
  }
  
  # Apply Top2Count filter safely
  if (!is.null(filters$min_top2_count) && filters$min_top2_count > 0 && "Top2Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top2Count >= filters$min_top2_count]
  }
  
  # Apply Top3Count filter safely
  if (!is.null(filters$min_top3_count) && filters$min_top3_count > 0 && "Top3Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top3Count >= filters$min_top3_count]
  }
  
  # Apply Top5Count filter safely
  if (!is.null(filters$min_top5_count) && filters$min_top5_count > 0 && "Top5Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top5Count >= filters$min_top5_count]
  }
  
  # Apply fighter exclusion filter safely
  if (!is.null(filters$excluded_fighters) && length(filters$excluded_fighters) > 0) {
    # Get fighter columns
    fighter_cols <- grep("^Fighter", names(filtered_lineups), value = TRUE)
    
    if(length(fighter_cols) > 0) {
      # Initialize vector for tracking which rows to exclude
      to_exclude <- rep(FALSE, nrow(filtered_lineups))
      
      # Check each fighter column
      for(col in fighter_cols) {
        # Update the exclusion vector if any excluded fighter is found
        to_exclude <- to_exclude | filtered_lineups[[col]] %in% filters$excluded_fighters
      }
      
      # Also check MVP column if it exists
      if("MVP" %in% names(filtered_lineups)) {
        to_exclude <- to_exclude | filtered_lineups$MVP %in% filters$excluded_fighters
      }
      
      # Keep only the rows that don't contain excluded fighters
      filtered_lineups <- filtered_lineups[!to_exclude]
    }
  }
  
  # Return early with count 0 if no lineups match the filters
  if(nrow(filtered_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  # Calculate thresholds for display
  thresholds <- list()
  threshold_columns <- c("Top1Count", "Top2Count", "Top3Count", "Top5Count")
  
  for (col in threshold_columns) {
    if (col %in% names(filtered_lineups) && !all(is.na(filtered_lineups[[col]]))) {
      min_val <- min(filtered_lineups[[col]], na.rm = TRUE)
      max_val <- max(filtered_lineups[[col]], na.rm = TRUE)
      
      # Use proper naming convention
      min_name <- paste0("min_", sub("Count", "", tolower(col)))
      max_name <- paste0("max_", sub("Count", "", tolower(col)))
      
      thresholds[[min_name]] <- min_val
      thresholds[[max_name]] <- max_val
    }
  }
  
  return(list(
    count = nrow(filtered_lineups),
    thresholds = thresholds
  ))
}



calculate_dk_filtered_pool_stats <- function(optimal_lineups, filters) {
  # Validate inputs before proceeding
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  # Ensure we're working with a data.table
  filtered_lineups <- tryCatch({
    as.data.table(optimal_lineups)
  }, error = function(e) {
    return(NULL)
  })
  
  if(is.null(filtered_lineups) || nrow(filtered_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  # Apply Top Count filters safely
  if (!is.null(filters$min_top1_count) && filters$min_top1_count > 0 && "Top1Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
  }
  
  if (!is.null(filters$min_top2_count) && filters$min_top2_count > 0 && "Top2Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top2Count >= filters$min_top2_count]
  }
  
  if (!is.null(filters$min_top3_count) && filters$min_top3_count > 0 && "Top3Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top3Count >= filters$min_top3_count]
  }
  
  if (!is.null(filters$min_top5_count) && filters$min_top5_count > 0 && "Top5Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top5Count >= filters$min_top5_count]
  }
  
  # Apply ownership range filters ONLY if the columns exist and ranges are valid
  if (!is.null(filters$cumulative_ownership_range) && length(filters$cumulative_ownership_range) == 2 && 
      "CumulativeOwnership" %in% names(filtered_lineups)) {
    # Only apply filter if the range makes sense
    cum_vals <- filtered_lineups$CumulativeOwnership[!is.na(filtered_lineups$CumulativeOwnership)]
    if(length(cum_vals) > 0) {
      data_min <- min(cum_vals)
      data_max <- max(cum_vals)
      filter_min <- filters$cumulative_ownership_range[1]
      filter_max <- filters$cumulative_ownership_range[2]
      
      # Only apply if filter range overlaps with data range
      if(filter_max >= data_min && filter_min <= data_max) {
        filtered_lineups <- filtered_lineups[CumulativeOwnership >= filter_min & CumulativeOwnership <= filter_max]
      }
    }
  }
  
  if (!is.null(filters$geometric_mean_range) && length(filters$geometric_mean_range) == 2 && 
      "GeometricMeanOwnership" %in% names(filtered_lineups)) {
    # Only apply filter if the range makes sense
    geom_vals <- filtered_lineups$GeometricMeanOwnership[!is.na(filtered_lineups$GeometricMeanOwnership)]
    if(length(geom_vals) > 0) {
      data_min <- min(geom_vals)
      data_max <- max(geom_vals)
      filter_min <- filters$geometric_mean_range[1]
      filter_max <- filters$geometric_mean_range[2]
      
      # Only apply if filter range overlaps with data range
      if(filter_max >= data_min && filter_min <= data_max) {
        filtered_lineups <- filtered_lineups[GeometricMeanOwnership >= filter_min & GeometricMeanOwnership <= filter_max]
      }
    }
  }
  
  # Apply fighter exclusion filter
  if (!is.null(filters$excluded_fighters) && length(filters$excluded_fighters) > 0) {
    fighter_cols <- grep("^Fighter", names(filtered_lineups), value = TRUE)
    
    if(length(fighter_cols) > 0) {
      to_exclude <- rep(FALSE, nrow(filtered_lineups))
      
      for(col in fighter_cols) {
        to_exclude <- to_exclude | filtered_lineups[[col]] %in% filters$excluded_fighters
      }
      
      filtered_lineups <- filtered_lineups[!to_exclude]
    }
  }
  
  # Return count and thresholds
  return(list(
    count = nrow(filtered_lineups),
    thresholds = NULL
  ))
}



calculate_dk_fighter_exposure <- function(optimal_lineups, fantasy_analysis, random_lineups = NULL) {
  # Quick validation
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame(Message = "No optimal lineups available."))
  }
  
  # Convert inputs to data.table for better performance
  setDT(optimal_lineups)
  if(!is.null(fantasy_analysis)) setDT(fantasy_analysis)
  if(!is.null(random_lineups)) setDT(random_lineups)
  
  # Get all fighters from the optimal lineups
  fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
  all_fighters <- unique(unlist(optimal_lineups[, ..fighter_cols]))
  
  # Initialize metrics data frame with all needed columns (including Exposure)
  metrics_data <- data.table(
    Name = all_fighters,
    DKSalary = NA_real_,
    DKOwn = NA_real_,
    OptimalRate = 0,
    FilteredPoolRate = 0,
    Exposure = 0,  # Always initialize this column
    Leverage = 0   # Always initialize this column
  )
  
  # Match with fantasy analysis data
  if(!is.null(fantasy_analysis) && nrow(fantasy_analysis) > 0) {
    lookup_data <- fantasy_analysis[, .(Name, DKSalary, DKOwn)]
    setkey(lookup_data, Name)
    
    for(i in 1:nrow(metrics_data)) {
      fighter_name <- metrics_data$Name[i]
      matches <- lookup_data[Name == fighter_name]
      
      if(nrow(matches) > 0) {
        metrics_data[i, DKSalary := matches$DKSalary[1]]
        metrics_data[i, DKOwn := matches$DKOwn[1]]
      }
    }
  }
  
  # Calculate OptimalRate
  total_top1 <- sum(optimal_lineups$Top1Count, na.rm = TRUE)
  if(total_top1 > 0) {
    for(fighter in all_fighters) {
      fighter_appears <- logical(nrow(optimal_lineups))
      for(col in fighter_cols) {
        fighter_appears <- fighter_appears | (optimal_lineups[[col]] == fighter)
      }
      fighter_matches <- which(fighter_appears)
      
      fighter_total <- sum(optimal_lineups$Top1Count[fighter_matches], na.rm = TRUE)
      metrics_data[Name == fighter, OptimalRate := (fighter_total / total_top1) * 100]
    }
  }
  
  # Calculate Exposure from random lineups (only if provided)
  if(!is.null(random_lineups) && nrow(random_lineups) > 0) {
    random_fighter_cols <- grep("^Fighter", names(random_lineups), value = TRUE)
    if(length(random_fighter_cols) > 0) {
      for(fighter in all_fighters) {
        fighter_appears <- logical(nrow(random_lineups))
        for(col in random_fighter_cols) {
          fighter_appears <- fighter_appears | (random_lineups[[col]] == fighter)
        }
        metrics_data[Name == fighter, Exposure := (sum(fighter_appears) / nrow(random_lineups)) * 100]
      }
    }
  }
  # If no random lineups, Exposure remains 0 (initialized above)
  
  # Calculate leverage (only if we have ownership data and exposure)
  metrics_data[!is.na(DKOwn), Leverage := Exposure - (DKOwn*100)]
  
  # Sort by OptimalRate
  setorder(metrics_data, -OptimalRate)
  
  return(as.data.frame(metrics_data))
}

calculate_fd_fighter_exposure <- function(optimal_lineups, fantasy_analysis, random_lineups = NULL) {
  # Quick validation
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame(Message = "No optimal lineups available."))
  }
  
  # Use data.table operations
  setDT(optimal_lineups)
  if (!is.null(fantasy_analysis)) setDT(fantasy_analysis)
  if (!is.null(random_lineups)) setDT(random_lineups)
  
  # Fighter columns
  fighter_cols <- grep("^Fighter", names(optimal_lineups), value = TRUE)
  if(length(fighter_cols) == 0) {
    return(data.frame(Message = "No fighter columns found in lineups."))
  }
  
  # Get all unique fighters
  all_fighters <- unique(unlist(optimal_lineups[, ..fighter_cols]))
  
  # Initialize metrics data frame with only needed columns
  metrics_data <- data.table(
    Name = all_fighters,
    FDSalary = NA_real_,
    FDOwn = NA_real_,
    OptimalRate = 0,
    FilteredPoolRate = 0,
    MVPRate = 0,
    Exposure = 0,
    MVPExposure = 0,
    Leverage = 0,
    FDID = NA_character_
  )
  
  # Match with fantasy analysis data
  if(!is.null(fantasy_analysis) && nrow(fantasy_analysis) > 0) {
    lookup_data <- fantasy_analysis[, .(Name, FDSalary, FDOwn)]
    setkey(lookup_data, Name)
    
    for(i in 1:nrow(metrics_data)) {
      fighter_name <- metrics_data$Name[i]
      matches <- lookup_data[Name == fighter_name]
      
      if(nrow(matches) > 0) {
        metrics_data[i, FDSalary := matches$FDSalary[1]]
        metrics_data[i, FDOwn := matches$FDOwn[1]]
      }
    }
  }
  
  # Calculate OptimalRate
  total_top1 <- sum(optimal_lineups$Top1Count, na.rm = TRUE)
  if(total_top1 > 0) {
    for(fighter in all_fighters) {
      fighter_appears <- logical(nrow(optimal_lineups))
      for(col in fighter_cols) {
        fighter_appears <- fighter_appears | (optimal_lineups[[col]] == fighter)
      }
      fighter_matches <- which(fighter_appears)
      
      fighter_total <- sum(optimal_lineups$Top1Count[fighter_matches], na.rm = TRUE)
      metrics_data[Name == fighter, OptimalRate := (fighter_total / total_top1) * 100]
    }
  }
  
  # Calculate MVPRate
  if("MVP" %in% names(optimal_lineups) && total_top1 > 0) {
    for(fighter in all_fighters) {
      mvp_matches <- which(optimal_lineups$MVP == fighter)
      mvp_total <- sum(optimal_lineups$Top1Count[mvp_matches], na.rm = TRUE)
      metrics_data[Name == fighter, MVPRate := (mvp_total / total_top1) * 100]
    }
  }
  
  # Calculate Exposure and MVPExposure from random lineups
  if(!is.null(random_lineups) && nrow(random_lineups) > 0) {
    random_fighter_cols <- grep("^Fighter", names(random_lineups), value = TRUE)
    if(length(random_fighter_cols) > 0) {
      for(fighter in all_fighters) {
        fighter_appears <- logical(nrow(random_lineups))
        for(col in random_fighter_cols) {
          fighter_appears <- fighter_appears | (random_lineups[[col]] == fighter)
        }
        metrics_data[Name == fighter, Exposure := (sum(fighter_appears) / nrow(random_lineups)) * 100]
        
        # Calculate MVP exposure if applicable
        if("MVP" %in% names(random_lineups)) {
          mvp_appears <- random_lineups$MVP == fighter
          metrics_data[Name == fighter, MVPExposure := (sum(mvp_appears) / nrow(random_lineups)) * 100]
        }
      }
    }
    
    # Use the MVP exposure from the attributes if available
    mvp_exposure_attr <- attr(random_lineups, "mvp_exposure")
    if(!is.null(mvp_exposure_attr)) {
      for(fighter in names(mvp_exposure_attr)) {
        metrics_data[Name == fighter, MVPExposure := mvp_exposure_attr[fighter]]
      }
    }
  }
  
  # Calculate leverage
  metrics_data[!is.na(FDOwn) & !is.na(Exposure), Leverage := Exposure - (FDOwn*100)]
  
  # Sort by OptimalRate
  setorder(metrics_data, -OptimalRate)
  
  return(as.data.frame(metrics_data))
}

# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(title = "MMA Fantasy Sims"),
  
  # Dashboard sidebar
  dashboardSidebar(
    useShinyjs(),
    div(
      style = "text-align: center; padding: 10px; margin-bottom: 5px;",
      tags$img(src = "logo.jpg", height = "200px", width = "auto", 
               style = "border: 2px solid #FFD700; border-radius: 10px;")
    ),
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Input Check", tabName = "upload", icon = icon("upload")),
      menuItem("Fight Analysis", tabName = "fight_analysis", icon = icon("chart-line")),
      menuItem("Fantasy Projections", tabName = "fantasy", icon = icon("calculator")),
      menuItem("Optimal Lineups", tabName = "optimal_lineups", icon = icon("trophy")),
      menuItem("Lineup Builder", tabName = "lineup_builder", icon = icon("percentage")),
      menuItem("Contest Simulator", tabName = "contest_sim", icon = icon("trophy"))
    ),
    br(),
    fileInput("excel_file", "Upload Excel File", accept = c(".xlsx")),
    numericInput("n_sims", "Number of Simulations:", value = 10000, min = 100, max = 50000),
    actionButton("run_sim", "Run Simulation", class = "btn-primary", style = "margin: 15px; width: 90%"),
    div(id = "sim_status", class = "text-center", style = "margin-top: 10px;")
  ),
  
  # Dashboard body
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css))
    ),
    
    tabItems(
      # Upload Tab
      tabItem(tabName = "upload",
              uiOutput("upload_content")
      ),
      
      # Fight Analysis Tab
      tabItem(tabName = "fight_analysis",
              fluidRow(
                div(style = "text-align: right; margin: 10px 15px;",
                    downloadButton('downloadResults', 'Download Full Simulation Results', 
                                   style = "margin-top: 5px;"))
              ),
              fluidRow(
                box(width = 12,
                    title = "Outcome Distribution by Fighter",
                    plotlyOutput("outcome_dist_plot", height = "1100px") %>% withSpinner(color = "#FFD700")
                )
              )
      ),
      
      # Fantasy Projections Tab
      tabItem(tabName = "fantasy",
              uiOutput("fantasy_ui")
      ),
      
      # Optimal Lineups Tab
      tabItem(tabName = "optimal_lineups",
              fluidRow(
                box(
                  width = 12, 
                  title = "Run Lineup Optimization",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  # Radio buttons to select platform - only one at a time
                  radioButtons(
                    "platform_selection", 
                    "Select Fantasy Platform:",
                    choices = list(
                      "DraftKings" = "dk",
                      "FanDuel" = "fd",
                      "DK Showdown" = "sd"
                    ),
                    selected = "dk",
                    inline = TRUE
                  ),
                  
                  # Conditional panel for DraftKings options
                  conditionalPanel(
                    condition = "input.platform_selection == 'dk' && output.has_draftkings == 'true'",
                    fluidRow(
                      column(6,
                             actionButton("run_dk_optimization", "Calculate DraftKings Lineups",
                                          class = "btn-primary", 
                                          style = "width: 100%; margin-top: 10px;")
                      ),
                      column(6,
                             div(
                               style = "margin-top: 10px;",
                               uiOutput("dk_optimization_status")
                             )
                      )
                    )
                  ),
                  
                  # Conditional panel for FanDuel options
                  conditionalPanel(
                    condition = "input.platform_selection == 'fd' && output.has_fanduel == 'true'",
                    fluidRow(
                      column(6,
                             actionButton("run_fd_optimization", "Calculate FanDuel Lineups",
                                          class = "btn-primary", 
                                          style = "width: 100%; margin-top: 10px;")
                      ),
                      column(6,
                             div(
                               style = "margin-top: 10px;",
                               uiOutput("fd_optimization_status")
                             )
                      )
                    )
                  ),
                  
                  # Conditional panel for DK Showdown options
                  conditionalPanel(
                    condition = "input.platform_selection == 'sd' && output.has_draftkings == 'true'",
                    fluidRow(
                      column(6,
                             actionButton("run_sd_optimization", "Calculate DK Showdown Lineups",
                                          class = "btn-primary", 
                                          style = "width: 100%; margin-top: 10px;")
                      ),
                      column(6,
                             div(
                               style = "margin-top: 10px;",
                               uiOutput("sd_optimization_status")
                             )
                      )
                    )
                  ),
                  
                  # Show warning if platform not available
                  conditionalPanel(
                    condition = "(input.platform_selection == 'dk' && output.has_draftkings != 'true') || 
                     (input.platform_selection == 'fd' && output.has_fanduel != 'true') ||
                     (input.platform_selection == 'sd' && output.has_draftkings != 'true')",
                    div(
                      class = "alert alert-warning",
                      style = "margin-top: 10px;",
                      "The selected platform data is not available in the input file. Please check your file or select another platform."
                    )
                  )
                )
              ),
              
              # DraftKings results section - only shown when DK selected and lineups available
              conditionalPanel(
                condition = "input.platform_selection == 'dk' && output.has_dk_lineups == 'true'",
                fluidRow(
                  box(
                    width = 12,
                    title = "DraftKings Optimal Lineups",
                    div(
                      style = "text-align: right; margin-bottom: 10px;",
                      downloadButton('download_dk_optimal_lineups', 'Download All DK Lineups',
                                     style = "margin-top: 10px;")
                    ),
                    DTOutput("dk_optimal_lineups_table")
                  )
                )
              ),
              
              # FanDuel results section - only shown when FD selected and lineups available
              conditionalPanel(
                condition = "input.platform_selection == 'fd' && output.has_fd_lineups == 'true'",
                fluidRow(
                  box(
                    width = 12,
                    title = "FanDuel Optimal Lineups",
                    div(
                      style = "text-align: right; margin-bottom: 10px;",
                      downloadButton('download_fd_optimal_lineups', 'Download All FD Lineups',
                                     style = "margin-top: 10px;")
                    ),
                    DTOutput("fd_optimal_lineups_table")
                  )
                )
              ),
              
              # DK Showdown results section - only shown when SD selected and lineups available
              conditionalPanel(
                condition = "input.platform_selection == 'sd' && output.has_sd_lineups == 'true'",
                fluidRow(
                  box(
                    width = 12,
                    title = "DK Showdown Optimal Lineups",
                    div(
                      style = "text-align: right; margin-bottom: 10px;",
                      downloadButton('download_sd_optimal_lineups', 'Download All SD Lineups',
                                     style = "margin-top: 10px;")
                    ),
                    DTOutput("sd_optimal_lineups_table")
                  )
                )
              )
      ),
      
      # Lineup Builder Tab
      tabItem(tabName = "lineup_builder",
              uiOutput("lineup_builder_ui")
      ),
      tabItem(
        tabName = "contest_sim",
        
        fluidRow(
          box(
            title = "Contest Simulator",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            
            # Settings row - compact
            fluidRow(
              column(width = 3, 
                     selectInput("contest_sim_platform", "Platform:",
                                 choices = c("DraftKings" = "DK", "FanDuel" = "FD"), 
                                 selected = "DK")),
              column(width = 3, 
                     numericInput("contest_chalk_field_size", "Chalk Field Size:",
                                  value = 50, min = 10, max = 100, step = 10)),
              column(width = 6,
                     div(style = "margin-top: 25px;",
                         p(style = "margin: 0; font-size: 13px;",
                           strong("Payouts:"), " Double-Up (top 45%), 5x (top 20%), 10x (top 10%)"),
                         p(style = "margin: 0; font-size: 12px; color: #666;",
                           "Uses all available simulations for maximum accuracy")))
            ),
            
            hr(style = "margin: 10px 0;"),
            
            h5("Lineup Filters", style = "margin: 5px 0 10px 0;"),
            
            # Dynamic filters
            uiOutput("contest_sim_filters_ui"),
            
            # Filtered pool count
            fluidRow(
              column(width = 12,
                     div(style = "background-color: #f5f5f5; padding: 8px; border-radius: 3px; margin: 10px 0;",
                         textOutput("contest_sim_filtered_pool_size")))
            ),
            
            # Run button - centered, compact
            fluidRow(
              column(width = 12, align = "center",
                     actionButton("run_contest_sim", "Run Contest Simulator",
                                  class = "btn-primary btn-lg", icon = icon("trophy"),
                                  style = "margin: 5px 0;"))
            )
          )
        ),
        
        # Chalk field preview
        fluidRow(
          box(
            title = "Chalk Field Preview",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            
            withSpinner(DTOutput("contest_chalk_field_table"), type = 4, color = "#FFD700")
          )
        ),
        
        # Results
        fluidRow(
          box(
            title = "Contest Simulation Results",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            
            uiOutput("contest_sim_summary"),
            br(),
            withSpinner(DTOutput("contest_sim_results_table"), type = 4, color = "#FFD700"),
            br(),
            downloadButton("download_contest_sim_results", "Download Results", class = "btn-primary")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store data
  rv <- reactiveValues(
    input_data = NULL,
    processed_data = NULL,
    simulation_results = NULL,
    has_draftkings = FALSE,
    has_fanduel = FALSE,
    fight_outcomes = NULL,
    outcome_analysis = NULL,
    dk_fantasy_analysis = NULL,
    fd_fantasy_analysis = NULL,
    dk_optimal_lineups = NULL,
    fd_optimal_lineups = NULL,
    sd_optimal_lineups = NULL,
    dk_fighter_exposure = NULL,
    fd_fighter_exposure = NULL,
    sd_fighter_exposure = NULL,
    dk_random_lineups = NULL,
    fd_random_lineups = NULL,
    sd_random_lineups = NULL,
    file_uploaded = FALSE,
    simulation_complete = FALSE,
    contest_sim_results = NULL,
    contest_chalk_field = NULL,
    dk_lineup_builds = list(),
    fd_lineup_builds = list(),
    sd_lineup_builds = list()
  )
  
  
  
  output$has_draftkings <- reactive({
    # Convert boolean TRUE/FALSE to lowercase string "true"/"false"
    result <- tolower(as.character(rv$has_draftkings))
    return(result)
  })
  outputOptions(output, "has_draftkings", suspendWhenHidden = FALSE)
  
  output$has_fanduel <- reactive({
    # Convert boolean TRUE/FALSE to lowercase string "true"/"false"
    result <- tolower(as.character(rv$has_fanduel))
    return(result)
  })
  outputOptions(output, "has_fanduel", suspendWhenHidden = FALSE)
  
  output$has_dk_lineups <- reactive({
    # Convert boolean TRUE/FALSE to lowercase string "true"/"false"
    result <- tolower(as.character(!is.null(rv$dk_optimal_lineups) && nrow(rv$dk_optimal_lineups) > 0))
    return(result)
  })
  outputOptions(output, "has_dk_lineups", suspendWhenHidden = FALSE)
  
  output$has_fd_lineups <- reactive({
    # Convert boolean TRUE/FALSE to lowercase string "true"/"false"
    result <- tolower(as.character(!is.null(rv$fd_optimal_lineups) && nrow(rv$fd_optimal_lineups) > 0))
    return(result)
  })
  outputOptions(output, "has_fd_lineups", suspendWhenHidden = FALSE)
  
  output$has_sd_lineups <- reactive({
    # Convert boolean TRUE/FALSE to lowercase string "true"/"false"
    result <- tolower(as.character(!is.null(rv$sd_optimal_lineups) && nrow(rv$sd_optimal_lineups) > 0))
    return(result)
  })
  outputOptions(output, "has_sd_lineups", suspendWhenHidden = FALSE)
  
  # File upload handler
  observeEvent(input$excel_file, {
    req(input$excel_file)
    
    # Show progress notification
    withProgress(message = 'Reading file...', value = 0, {
      tryCatch({
        # Read the input file
        rv$input_data <- read_input_file(input$excel_file$datapath)
        rv$file_uploaded <- TRUE
        rv$simulation_complete <- FALSE
        
        # Reset all other values
        rv$processed_data <- NULL
        rv$simulation_results <- NULL
        rv$fight_outcomes <- NULL
        rv$outcome_analysis <- NULL
        rv$dk_fantasy_analysis <- NULL
        rv$fd_fantasy_analysis <- NULL
        rv$dk_optimal_lineups <- NULL
        rv$fd_optimal_lineups <- NULL
        rv$sd_optimal_lineups <- NULL
        rv$sd_fighter_exposure <- NULL
        rv$dk_fighter_exposure <- NULL
        rv$fd_fighter_exposure <- NULL
        rv$dk_random_lineups <- NULL
        rv$fd_random_lineups <- NULL
        rv$simulation_complete <- FALSE
        
        # Store platform availability
        rv$has_draftkings <- rv$input_data$platform_info$has_draftkings
        rv$has_fanduel <- rv$input_data$platform_info$has_fanduel
        
        # Process the data
        incProgress(0.5, detail = "Processing data...")
        rv$processed_data <- process_input_data(rv$input_data)
        
        # Update the data preview
        output$data_preview <- renderDT({
          # Create a filtered version of the data for display
          display_data <- rv$input_data$sheets$Fights %>% 
            select(Name, Opponent, DKSalary, FDSalary, DeViggedProb, QuickWin_R1, R1, R2, R3, R4, R5, Decision) %>% 
            rename(DKSal = DKSalary,
                   FDSal = FDSalary, 
                   Win = DeViggedProb,
                   QuickWin = QuickWin_R1)
          
          # Create the datatable with better column alignment
          dt <- datatable(
            display_data,
            options = list(
              scrollX = TRUE, 
              pageLength = -1,  # Show all rows
              autoWidth = FALSE,  # Don't use autoWidth for better control
              dom = "t",  # Only show table ('t'), no search/pagination
              ordering = TRUE,  # Allow sorting
              columnDefs = list(
                list(className = 'dt-center', targets = "_all")  # Center-align all columns
              ),
              scrollCollapse = TRUE,
              fixedColumns = TRUE
            ),
            class = 'cell-border stripe display compact',  # Added compact class for tighter spacing
            rownames = FALSE,
            width = "100%",  # Use full width
            height = "auto"
          )
          
          # Formatting for various columns
          if("DKOwn" %in% colnames(display_data)) {
            dt <- dt %>% formatPercentage("DKOwn", digits = 1)
          }
          
          if("FDOwn" %in% colnames(display_data)) {
            dt <- dt %>% formatPercentage("FDOwn", digits = 1)
          }
          
          
          # Format probability columns as percentages
          prob_cols <- c("Win", "R1", "QuickWin", "R2", "R3", "R4", "R5", "Decision")
          for(col in prob_cols) {
            if(col %in% colnames(display_data)) {
              dt <- dt %>% formatPercentage(col, digits = 1)
            }
          }
          
          # Format salary columns as currency
          if("DKSal" %in% colnames(display_data)) {
            dt <- dt %>% formatCurrency("DKSal", currency = "$", interval = 3, mark = ",", digits = 0)
          }
          
          if("FDSal" %in% colnames(display_data)) {
            dt <- dt %>% formatCurrency("FDSal", currency = "$", digits = 0)
          }
          
          return(dt)
        })
        
        
        # Switch to upload tab
        updateTabItems(session, "sidebar_menu", selected = "upload")
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Error reading file:", e$message),
          easyClose = TRUE
        ))
        rv$file_uploaded <- FALSE
      })
    })
  })
  
  
  # Run simulation button handler
  observeEvent(input$run_sim, {
    req(rv$processed_data)
    
    # Clear previous results and force garbage collection
    rv$simulation_results <- NULL
    rv$fight_outcomes <- NULL
    rv$outcome_analysis <- NULL
    rv$dk_fantasy_analysis <- NULL
    rv$fd_fantasy_analysis <- NULL
    rv$dk_optimal_lineups <- NULL
    rv$fd_optimal_lineups <- NULL
    rv$sd_optimal_lineups <- NULL
    rv$sd_fighter_exposure <- NULL
    rv$dk_fighter_exposure <- NULL
    rv$fd_fighter_exposure <- NULL
    rv$dk_random_lineups <- NULL
    rv$fd_random_lineups <- NULL
    
    # Force garbage collection before running new simulation
    gc(verbose = FALSE, full = TRUE)
    
    # Show progress dialog
    withProgress(message = 'Running simulations...', value = 0, {
      # Run the simulations
      setProgress(0.1, detail = "Initializing simulation...")
      
      simulation_results <- run_mma_simulations(
        rv$processed_data, 
        n_sims = input$n_sims,
        batch_size = 100
      )
      
      # Store results
      rv$simulation_results <- simulation_results$results
      
      # Update platform availability
      rv$has_draftkings <- "DKScore" %in% names(rv$simulation_results)
      rv$has_fanduel <- "FDScore" %in% names(rv$simulation_results)
      
      # Force garbage collection
      gc(verbose = FALSE, full = TRUE)
      
      # Process fight outcomes analysis
      setProgress(0.7, detail = "Analyzing fight outcomes...")
      rv$fight_outcomes <- analyze_fight_outcomes(rv$simulation_results)
      rv$outcome_analysis <- analyze_performance_by_outcome(rv$simulation_results)
      
      # Process fantasy scoring analysis
      setProgress(0.9, detail = "Analyzing fantasy scores...")
      fantasy_analysis <- analyze_fantasy_scoring(rv$simulation_results)
      
      if(rv$has_draftkings) {
        rv$dk_fantasy_analysis <- fantasy_analysis$dk
      }
      
      if(rv$has_fanduel) {
        rv$fd_fantasy_analysis <- fantasy_analysis$fd
      }
      
      # Mark simulation as complete
      rv$simulation_complete <- TRUE
      
      # Switch to fight analysis tab
      updateTabItems(session, "sidebar_menu", selected = "fight_analysis")
      
      # Show success message
      showModal(modalDialog(
        title = "Success",
        "Simulation completed successfully! Review the fight outcomes and projections or move to optimal lineup creation.",
        easyClose = TRUE
      ))
      
      # Final cleanup
      gc(verbose = FALSE, full = TRUE)
    })
  })
  
  
  # Upload content UI
  output$upload_content <- renderUI({
    if(rv$simulation_complete) {
      # Show accuracy analysis after simulation is complete
      tagList(
        fluidRow(
          box(width = 12,
              title = "Simulation Accuracy Analysis",
              DTOutput("accuracy_analysis") %>% withSpinner(color = "#FFD700"),
              downloadButton('downloadAccuracy', 'Download Accuracy Analysis')
          )
        )
      )
    } else {
      # Show input data before simulation is run
      tagList(
        fluidRow(
          box(width = 12,
              title = "Fighters Data",
              DTOutput("data_preview") %>% withSpinner(color = "#FFD700")
          )
        ),
        uiOutput("available_platforms")
      )
    }
  })
  
  # Accuracy analysis output
  output$accuracy_analysis <- renderDT({
    req(rv$simulation_results, rv$processed_data$scores)
    
    # Calculate accuracy metrics
    accuracy_data <- analyze_simulation_accuracy(rv$simulation_results, rv$processed_data$scores)
    
    # Create an informative summary view
    datatable(
      accuracy_data$detailed,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        order = list(list(5, 'desc')), # Sort by difference in descending order
        columnDefs = list(
          list(targets = c(3, 4, 5), className = 'dt-right')
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatPercentage(c('Expected', 'Observed'), digits = 2) %>%
      formatPercentage('Difference', digits = 2) %>%
      formatStyle(
        'Difference',
        background = styleColorBar(c(0, max(accuracy_data$detailed$Difference)), 'rgba(255, 0, 0, 0.5)'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  
  # Outcome distribution plot
  output$outcome_dist_plot <- renderPlotly({
    req(rv$simulation_results)
    
    # Get all simulation results - both wins and losses for proper win rate calculation
    all_outcomes <- rv$simulation_results
    
    # Get total fights per fighter to calculate win rates
    total_fights <- all_outcomes %>%
      group_by(Name) %>%
      summarize(TotalFights = n())
    
    # Get all outcomes for each fighter where they won
    fighter_outcomes <- all_outcomes[all_outcomes$Result == "Win", ]
    
    # Add a simplified outcome category for visualization
    fighter_outcomes$VisOutcome <- case_when(
      grepl("Decision", fighter_outcomes$Outcome) ~ "Decision",
      grepl("QuickWin", fighter_outcomes$Outcome) ~ "QuickWin_R1",
      grepl("R1", fighter_outcomes$Outcome) ~ "R1 Finish",
      grepl("R2", fighter_outcomes$Outcome) ~ "R2 Finish",
      grepl("R3", fighter_outcomes$Outcome) ~ "R3 Finish",
      grepl("R4", fighter_outcomes$Outcome) ~ "R4 Finish",
      grepl("R5", fighter_outcomes$Outcome) ~ "R5 Finish",
      TRUE ~ as.character(fighter_outcomes$Outcome)
    )
    
    # Get all fighters with salary info to sort by DK salary
    fighter_salaries <- fighter_outcomes %>%
      group_by(Name) %>%
      summarize(
        DKSalary = first(DKSalary),
        FDSalary = first(FDSalary),
        WinCount = n()
      ) %>%
      filter(!is.na(DKSalary)) %>%
      arrange(DKSalary)  # Sort by DK salary ASCENDING for reverse order
    
    # Order fighter names by DK salary (ascending for reversed order)
    ordered_names <- fighter_salaries$Name
    
    # Calculate raw count for each outcome type per fighter
    outcome_counts <- fighter_outcomes %>%
      group_by(Name, VisOutcome) %>%
      summarize(Count = n(), .groups = 'drop')
    
    # Join with total fights to calculate actual win percentages
    outcome_percentages <- outcome_counts %>%
      left_join(total_fights, by = "Name") %>%
      mutate(WinPercentage = Count / TotalFights * 100)
    
    # Ensure fighters are in salary order
    outcome_percentages$Name <- factor(outcome_percentages$Name, levels = ordered_names)
    
    # Define the exact order for outcome types
    outcome_order <- c( "Decision", "R5 Finish", "R4 Finish", "R3 Finish", "R2 Finish", "R1 Finish", "QuickWin_R1")
    outcome_percentages$VisOutcome <- factor(outcome_percentages$VisOutcome, levels = outcome_order)
    
    
    # Create custom labels with Name, DK Salary and FD Salary
    fighter_labels <- fighter_salaries %>%
      left_join(total_fights, by = "Name") %>%
      mutate(
        WinRate = WinCount / TotalFights * 100,
        Label = sprintf("%s ($%s | $%s) - Win: %.1f%%", 
                        Name, 
                        format(DKSalary, big.mark=","), 
                        format(FDSalary, big.mark=","),
                        WinRate)
      )
    
    # Map names to labels
    name_to_label <- setNames(fighter_labels$Label, fighter_labels$Name)
    outcome_percentages$Label <- name_to_label[outcome_percentages$Name]
    
    # Define a better color palette for win types
    win_colors <- c(
      "QuickWin_R1" = "#9932CC",  
      "R1 Finish" = "#1E90FF",    
      "R2 Finish" = "#32CD32",    
      "R3 Finish" = "#FF8C00",    
      "R4 Finish" = "maroon",    
      "R5 Finish" = "#FFFF00",    
      "Decision" = "#DC143C"      
    )
    
    # Create the plot - use WinPercentage instead of Percentage for y-axis
    p <- ggplot(outcome_percentages, aes(x = Name, y = WinPercentage, fill = VisOutcome)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = win_colors, name = "Win Method", breaks = outcome_order)  +
      labs(title = "Win Method Distribution",
           x = "Fighter",
           y = "Win Percentage") +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 16),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom"
      )
    
    # Use the custom label for y-axis
    p <- p + scale_x_discrete(labels = name_to_label)
    
    # Convert to plotly with custom hover text
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(height = 1100,  # Increased height for all fighters
             margin = list(l = 250, r = 20, b = 100, t = 80),  # Increased left margin for labels
             legend = list(orientation = "h", y = -0.2))
  })
  
  observeEvent(rv$dk_optimal_lineups, {
    if(!is.null(rv$dk_optimal_lineups) && nrow(rv$dk_optimal_lineups) > 0) {
      
      # Get all unique fighters from optimal lineups
      fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
      all_fighters <- c()
      
      for(col in fighter_cols) {
        if(col %in% names(rv$dk_optimal_lineups)) {
          all_fighters <- c(all_fighters, rv$dk_optimal_lineups[[col]])
        }
      }
      
      all_fighters <- unique(all_fighters)
      all_fighters <- all_fighters[!is.na(all_fighters) & all_fighters != ""]
      all_fighters <- sort(all_fighters)
      
      # Update lock dropdown
      updateSelectizeInput(session, "dk_locked_fighters",
                           choices = all_fighters,
                           server = FALSE)
      
      # Update exclude dropdown
      updateSelectizeInput(session, "dk_excluded_fighters",
                           choices = all_fighters,
                           server = FALSE)
      
      cat("??? DK dropdowns updated with", length(all_fighters), "fighters\n")
    }
  })
  

  
  # Generate dynamic fantasy UI based on available platforms
  output$fantasy_ui <- renderUI({
    req(rv$simulation_results)
    
    # Create appropriate UI based on available platforms
    if(rv$has_draftkings && rv$has_fanduel) {
      # Both platforms available - use tabs
      tabsetPanel(
        id = "fantasy_tabs",
        tabPanel(
          "DraftKings",
          fluidRow(
            box(width = 12,
                title = "DraftKings Fantasy Point Projections",
                DTOutput("dk_fantasy_projections") %>% withSpinner(color = "#FFD700"),
                downloadButton('download_dk_fantasy_projections', 'Download Projections')
            )
          ),
          fluidRow(
            box(width = 12,
                title = "DraftKings Fantasy Points Distribution",
                plotlyOutput("dk_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
            )
          )
        ),
        tabPanel(
          "FanDuel",
          fluidRow(
            box(width = 12,
                title = "FanDuel Fantasy Point Projections",
                DTOutput("fd_fantasy_projections") %>% withSpinner(color = "#FFD700"),
                downloadButton('download_fd_fantasy_projections', 'Download Projections')
            )
          ),
          fluidRow(
            box(width = 12,
                title = "FanDuel Fantasy Points Distribution",
                plotlyOutput("fd_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
            )
          )
        )
      )
    } else if(rv$has_draftkings) {
      # Only DraftKings available
      tagList(
        fluidRow(
          box(width = 12,
              title = "DraftKings Fantasy Point Projections",
              DTOutput("dk_fantasy_projections") %>% withSpinner(color = "#FFD700"),
              downloadButton('download_dk_fantasy_projections', 'Download Projections')
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Fantasy Points vs Salary",
              plotlyOutput("dk_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#FFD700")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Fantasy Points Distribution",
              plotlyOutput("dk_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
          )
        )
      )
    } else if(rv$has_fanduel) {
      # Only FanDuel available
      tagList(
        fluidRow(
          box(width = 12,
              title = "FanDuel Fantasy Point Projections",
              DTOutput("fd_fantasy_projections") %>% withSpinner(color = "#FFD700"),
              downloadButton('download_fd_fantasy_projections', 'Download Projections')
          )
        ),
        fluidRow(
          box(width = 12,
              title = "FanDuel Fantasy Points vs Salary",
              plotlyOutput("fd_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#FFD700")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "FanDuel Fantasy Points Distribution",
              plotlyOutput("fd_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
          )
        )
      )
    } else {
      # No platforms available
      fluidRow(
        box(
          width = 12,
          status = "warning",
          solidHeader = TRUE,
          title = "No Fantasy Platforms Available",
          "No fantasy platform data was detected in your input file. Please ensure your file contains the necessary DraftKings or FanDuel columns."
        )
      )
    }
  })
  
  
  # ============================================================================
  # COMPLETE LINEUP BUILDER UI REPLACEMENT
  # Replace lines 4163-4415 with this entire block
  # ============================================================================
  
  output$lineup_builder_ui <- renderUI({
    # Check if any optimal lineups exist
    has_dk_lineups <- !is.null(rv$dk_optimal_lineups) && nrow(rv$dk_optimal_lineups) > 0
    has_fd_lineups <- !is.null(rv$fd_optimal_lineups) && nrow(rv$fd_optimal_lineups) > 0
    has_sd_lineups <- !is.null(rv$sd_optimal_lineups) && nrow(rv$sd_optimal_lineups) > 0
    
    # If neither platform has lineups, show a message
    if(!has_dk_lineups && !has_fd_lineups && !has_sd_lineups) {
      return(
        box(
          width = 12,
          status = "warning",
          title = "No Optimal Lineups Available",
          "Please calculate optimal lineups in the Optimal Lineups tab first."
        )
      )
    }
    
    # Calculate actual ranges for DK sliders if we have data
    dk_cum_range <- c(100, 300)  # Default range for cumulative ownership
    dk_geom_range <- c(15, 35)   # Default range for geometric mean
    
    if(has_dk_lineups && "CumulativeOwnership" %in% names(rv$dk_optimal_lineups)) {
      cum_vals <- rv$dk_optimal_lineups$CumulativeOwnership[!is.na(rv$dk_optimal_lineups$CumulativeOwnership)]
      if(length(cum_vals) > 0) {
        # CumulativeOwnership should already be in percentage format (sum of percentages)
        dk_cum_range <- c(floor(min(cum_vals)), ceiling(max(cum_vals)))
      }
    }
    
    if(has_dk_lineups && "GeometricMeanOwnership" %in% names(rv$dk_optimal_lineups)) {
      geom_vals <- rv$dk_optimal_lineups$GeometricMeanOwnership[!is.na(rv$dk_optimal_lineups$GeometricMeanOwnership)]
      if(length(geom_vals) > 0) {
        # GeometricMeanOwnership should already be in percentage format
        dk_geom_range <- c(floor(min(geom_vals)), ceiling(max(geom_vals)))
      }
    }
    
    # Generate UI for both platforms
    tagList(
      # Platform selection
      fluidRow(
        box(
          width = 12,
          title = "Select Fantasy Platform",
          status = "primary",
          solidHeader = TRUE,
          conditionalPanel(
            condition = "output.has_dk_lineups == 'true' && output.has_fd_lineups == 'true' && output.has_sd_lineups == 'true'",
            radioButtons(
              "lineup_builder_platform", 
              "Platform:",
              choices = list(
                "DraftKings" = "dk",
                "FanDuel" = "fd",
                "DK Showdown" = "sd"
              ),
              selected = "dk",
              inline = TRUE
            )
          ),
          conditionalPanel(
            condition = "output.has_dk_lineups == 'true' && output.has_fd_lineups == 'true' && output.has_sd_lineups != 'true'",
            radioButtons(
              "lineup_builder_platform", 
              "Platform:",
              choices = list(
                "DraftKings" = "dk",
                "FanDuel" = "fd"
              ),
              selected = "dk",
              inline = TRUE
            )
          ),
          conditionalPanel(
            condition = "output.has_dk_lineups == 'true' && output.has_fd_lineups != 'true' && output.has_sd_lineups == 'true'",
            radioButtons(
              "lineup_builder_platform", 
              "Platform:",
              choices = list(
                "DraftKings" = "dk",
                "DK Showdown" = "sd"
              ),
              selected = "dk",
              inline = TRUE
            )
          ),
          conditionalPanel(
            condition = "output.has_dk_lineups == 'true' && output.has_fd_lineups != 'true' && output.has_sd_lineups != 'true'",
            radioButtons(
              "lineup_builder_platform", 
              "Platform:",
              choices = list("DraftKings" = "dk"),
              selected = "dk",
              inline = TRUE
            ),
            h4("Using DraftKings lineups")
          ),
          conditionalPanel(
            condition = "output.has_dk_lineups != 'true' && output.has_fd_lineups == 'true' && output.has_sd_lineups == 'true'",
            radioButtons(
              "lineup_builder_platform", 
              "Platform:",
              choices = list(
                "FanDuel" = "fd",
                "DK Showdown" = "sd"
              ),
              selected = "fd",
              inline = TRUE
            )
          ),
          conditionalPanel(
            condition = "output.has_dk_lineups != 'true' && output.has_fd_lineups == 'true' && output.has_sd_lineups != 'true'",
            radioButtons(
              "lineup_builder_platform", 
              "Platform:",
              choices = list("FanDuel" = "fd"),
              selected = "fd",
              inline = TRUE
            ),
            h4("Using FanDuel lineups")
          ),
          conditionalPanel(
            condition = "output.has_sd_lineups == 'true' && output.has_dk_lineups != 'true' && output.has_fd_lineups != 'true'",
            radioButtons(
              "lineup_builder_platform", 
              "Platform:",
              choices = list("DK Showdown" = "sd"),
              selected = "sd",
              inline = TRUE
            ),
            h4("Using DK Showdown lineups")
          )
        )
      ),
      
      # ========== DRAFTKINGS UI ==========
      conditionalPanel(
        condition = "(input.lineup_builder_platform == 'dk' || (input.lineup_builder_platform == null && output.has_dk_lineups == 'true')) && output.has_dk_lineups == 'true'",
        
        # Filters Box
        fluidRow(
          box(width = 12,
              title = "DraftKings Lineup Filters",
              status = "primary",
              solidHeader = TRUE,
              
              # Top count filters
              fluidRow(
                column(3, numericInput("dk_min_top1_count", "Min Top 1 Count:", value = 0, min = 0)),
                column(3, numericInput("dk_min_top2_count", "Min Top 2 Count:", value = 0, min = 0)),
                column(3, numericInput("dk_min_top3_count", "Min Top 3 Count:", value = 0, min = 0)),
                column(3, numericInput("dk_min_top5_count", "Min Top 5 Count:", value = 0, min = 0))
              ),
              
              # Ownership filters
              fluidRow(
                column(6, sliderInput("dk_cumulative_ownership_range", "Cumulative Ownership Range:",
                                      min = dk_cum_range[1], max = dk_cum_range[2], 
                                      value = dk_cum_range, step = 1, post = "%")),
                column(6, sliderInput("dk_geometric_mean_range", "Geometric Mean Ownership Range:",
                                      min = dk_geom_range[1], max = dk_geom_range[2], 
                                      value = dk_geom_range, step = 1, post = "%"))
              ),
              
              # Lock/Exclude/Label/Count
              fluidRow(
                column(3, selectizeInput("dk_locked_fighters", "Lock Fighters:", 
                                         choices = if(!is.null(rv$dk_optimal_lineups)) {
                                           fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
                                           all_fighters <- unique(unlist(rv$dk_optimal_lineups[,..fighter_cols]))
                                           sort(all_fighters[!is.na(all_fighters) & all_fighters != ""])
                                         } else NULL,
                                         multiple = TRUE, selected = character(0),
                                         options = list(plugins = list('remove_button'), 
                                                        placeholder = 'Lock fighters (optional)',
                                                        maxItems = 5))),
                column(3, selectizeInput("dk_excluded_fighters", "Exclude Fighters:", 
                                         choices = if(!is.null(rv$dk_optimal_lineups)) {
                                           fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
                                           all_fighters <- unique(unlist(rv$dk_optimal_lineups[,..fighter_cols]))
                                           sort(all_fighters[!is.na(all_fighters) & all_fighters != ""])
                                         } else NULL,
                                         multiple = TRUE,
                                         options = list(plugins = list('remove_button'), 
                                                        placeholder = 'Exclude fighters'))),
                column(3, textInput("dk_build_label", "Build Label:", 
                                    value = "", 
                                    placeholder = "e.g., 'Chalk', 'Contrarian'")),
                column(3, numericInput("dk_num_random_lineups", "Number of Lineups:", 
                                       value = 20, min = 1, max = 150))
              ),
              
              # Filtered pool display and buttons
              fluidRow(
                column(6, div(
                  class = "well well-sm", 
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center;",
                    h4("Filtered Pool:", style = "margin: 0;"),
                    h4(textOutput("dk_filtered_pool_count", inline = TRUE), 
                       style = "margin: 0; font-weight: bold; color: #000000;")
                  ),
                  textOutput("dk_filtered_pool_size")
                )),
                column(6, div(style = "margin-top: 20px;",
                              actionButton("generate_dk_lineups", "Add Lineups to Portfolio", 
                                           class = "btn-primary btn-lg", style = "width: 100%;"),
                              br(), br(),
                              actionButton("clear_dk_builds", "Clear All Builds", 
                                           class = "btn-warning", style = "width: 100%;"),
                              br(), br(),
                              downloadButton("download_dk_random_lineups", "Download Portfolio", 
                                             style = "width: 100%;")))
              )
          )
        ),
        
        # Filtered Pool Statistics Table
        fluidRow(
          box(
            width = 12, 
            title = div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              span("Filtered Pool Fighter Statistics"),
              span(textOutput("dk_filtered_pool_stats_count", inline = TRUE), 
                   style = "font-size: 20px; font-weight: bold; color: #000000;")
            ),
            status = "info",
            solidHeader = TRUE,
            DTOutput("dk_filtered_pool_stats_table") %>% withSpinner(color = "#FFD700")
          )
        ),
        
        # Build Summary Table
        fluidRow(
          box(
            width = 12,
            title = "Lineup Builds Summary",
            status = "warning",
            solidHeader = TRUE,
            DTOutput("dk_builds_summary_table") %>% withSpinner(color = "#FFD700")
          )
        ),
        
        # Fighter Exposure Analysis (All Builds)
        fluidRow(
          box(width = 12, 
              title = "Fighter Exposure Analysis (All Builds)",
              status = "info",
              solidHeader = TRUE,
              DTOutput("dk_fighter_exposure_table") %>% withSpinner(color = "#FFD700"))
        ),
        
        # All Generated Lineups
        fluidRow(
          box(width = 12, 
              title = "All Generated Lineups",
              status = "success",
              solidHeader = TRUE,
              DTOutput("dk_random_lineups_table") %>% withSpinner(color = "#FFD700"))
        )
      ),
      
      # ========== FANDUEL UI ==========
      conditionalPanel(
        condition = "input.lineup_builder_platform == 'fd' && output.has_fd_lineups == 'true'",
        
        # Filters Box
        fluidRow(
          box(width = 12,
              title = "FanDuel Lineup Filters",
              status = "primary",
              solidHeader = TRUE,
              
              # Top count filters
              fluidRow(
                column(3, numericInput("fd_min_top1_count", "Min Top 1 Count:", value = 0, min = 0)),
                column(3, numericInput("fd_min_top2_count", "Min Top 2 Count:", value = 0, min = 0)),
                column(3, numericInput("fd_min_top3_count", "Min Top 3 Count:", value = 0, min = 0)),
                column(3, numericInput("fd_min_top5_count", "Min Top 5 Count:", value = 0, min = 0))
              ),
              
              # Lock/Exclude/Label/Count (no ownership filters for FanDuel)
              fluidRow(
                column(3, selectizeInput("fd_locked_fighters", "Lock Fighters:", 
                                         choices = NULL, multiple = TRUE, selected = character(0),
                                         options = list(plugins = list('remove_button'), 
                                                        placeholder = 'Lock fighters (optional)',
                                                        maxItems = 5))),
                column(3, selectizeInput("fd_excluded_fighters", "Exclude Fighters:", 
                                         choices = NULL, multiple = TRUE,
                                         options = list(plugins = list('remove_button'), 
                                                        placeholder = 'Exclude fighters'))),
                column(3, textInput("fd_build_label", "Build Label:", 
                                    value = "", 
                                    placeholder = "e.g., 'Chalk', 'Contrarian'")),
                column(3, numericInput("fd_num_random_lineups", "Number of Lineups:", 
                                       value = 20, min = 1, max = 150))
              ),
              
              # Filtered pool display and buttons
              fluidRow(
                column(6, div(
                  class = "well well-sm", 
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center;",
                    h4("Filtered Pool:", style = "margin: 0;"),
                    h4(textOutput("fd_filtered_pool_count", inline = TRUE), 
                       style = "margin: 0; font-weight: bold; color: #000000;")
                  ),
                  textOutput("fd_filtered_pool_size")
                )),
                column(6, div(style = "margin-top: 20px;",
                              actionButton("generate_fd_lineups", "Add Lineups to Portfolio", 
                                           class = "btn-primary btn-lg", style = "width: 100%;"),
                              br(), br(),
                              actionButton("clear_fd_builds", "Clear All Builds", 
                                           class = "btn-warning", style = "width: 100%;"),
                              br(), br(),
                              downloadButton("download_fd_random_lineups", "Download Portfolio", 
                                             style = "width: 100%;")))
              )
          )
        ),
        
        # Filtered Pool Statistics Table
        fluidRow(
          box(
            width = 12, 
            title = div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              span("Filtered Pool Fighter Statistics"),
              span(textOutput("fd_filtered_pool_stats_count", inline = TRUE), 
                   style = "font-size: 20px; font-weight: bold; color: #000000;")
            ),
            status = "info",
            solidHeader = TRUE,
            DTOutput("fd_filtered_pool_stats_table") %>% withSpinner(color = "#FFD700")
          )
        ),
        
        # Build Summary Table
        fluidRow(
          box(
            width = 12,
            title = "Lineup Builds Summary",
            status = "warning",
            solidHeader = TRUE,
            DTOutput("fd_builds_summary_table") %>% withSpinner(color = "#FFD700")
          )
        ),
        
        # Fighter Exposure Analysis (All Builds)
        fluidRow(
          box(width = 12, 
              title = "Fighter Exposure Analysis (All Builds)",
              status = "info",
              solidHeader = TRUE,
              DTOutput("fd_fighter_exposure_table") %>% withSpinner(color = "#FFD700"))
        ),
        
        # All Generated Lineups
        fluidRow(
          box(width = 12, 
              title = "All Generated Lineups",
              status = "success",
              solidHeader = TRUE,
              DTOutput("fd_random_lineups_table") %>% withSpinner(color = "#FFD700"))
        )
      ),
      
      # ========== DK SHOWDOWN UI ==========
      conditionalPanel(
        condition = "input.lineup_builder_platform == 'sd' && output.has_sd_lineups == 'true'",
        
        # Filters Box
        fluidRow(
          box(width = 12,
              title = "DK Showdown Lineup Filters",
              status = "primary",
              solidHeader = TRUE,
              
              # Top count filters
              fluidRow(
                column(3, numericInput("sd_min_top1_count", "Min Top 1 Count:", value = 0, min = 0)),
                column(3, numericInput("sd_min_top2_count", "Min Top 2 Count:", value = 0, min = 0)),
                column(3, numericInput("sd_min_top3_count", "Min Top 3 Count:", value = 0, min = 0)),
                column(3, numericInput("sd_min_top5_count", "Min Top 5 Count:", value = 0, min = 0))
              ),
              
              # Lock/Exclude/Label/Count
              fluidRow(
                column(3, selectizeInput("sd_locked_captain", "Lock Captain:", 
                                         choices = if(!is.null(rv$sd_optimal_lineups)) {
                                           if("Captain" %in% names(rv$sd_optimal_lineups)) {
                                             captains <- unique(rv$sd_optimal_lineups$Captain)
                                             captains <- captains[!is.na(captains) & captains != ""]
                                             c("None" = "", sort(captains))
                                           } else NULL
                                         } else NULL,
                                         multiple = FALSE, selected = "",
                                         options = list(placeholder = 'Lock captain (optional)'))),
                
                column(3, selectizeInput("sd_locked_fighters", "Lock Fighters (non-captain):", 
                                         choices = if(!is.null(rv$sd_optimal_lineups)) {
                                           all_fighters <- c()
                                           for(i in 1:5) {
                                             col <- paste0("Fighter", i)
                                             if(col %in% names(rv$sd_optimal_lineups)) {
                                               all_fighters <- c(all_fighters, rv$sd_optimal_lineups[[col]])
                                             }
                                           }
                                           all_fighters <- unique(all_fighters)
                                           sort(all_fighters[!is.na(all_fighters) & all_fighters != ""])
                                         } else NULL,
                                         multiple = TRUE, selected = character(0),
                                         options = list(plugins = list('remove_button'), 
                                                        placeholder = 'Lock fighters (optional)',
                                                        maxItems = 5))),
                
                column(3, selectizeInput("sd_excluded_captain", "Exclude Captain:", 
                                         choices = if(!is.null(rv$sd_optimal_lineups)) {
                                           if("Captain" %in% names(rv$sd_optimal_lineups)) {
                                             captains <- unique(rv$sd_optimal_lineups$Captain)
                                             captains <- captains[!is.na(captains) & captains != ""]
                                             sort(captains)
                                           } else NULL
                                         } else NULL,
                                         multiple = TRUE,
                                         options = list(plugins = list('remove_button'), 
                                                        placeholder = 'Exclude captains'))),
                
                column(3, selectizeInput("sd_excluded_fighters", "Exclude Fighters (non-captain):", 
                                         choices = if(!is.null(rv$sd_optimal_lineups)) {
                                           all_fighters <- c()
                                           for(i in 1:5) {
                                             col <- paste0("Fighter", i)
                                             if(col %in% names(rv$sd_optimal_lineups)) {
                                               all_fighters <- c(all_fighters, rv$sd_optimal_lineups[[col]])
                                             }
                                           }
                                           all_fighters <- unique(all_fighters)
                                           sort(all_fighters[!is.na(all_fighters) & all_fighters != ""])
                                         } else NULL,
                                         multiple = TRUE,
                                         options = list(plugins = list('remove_button'), 
                                                        placeholder = 'Exclude fighters')))
              ),
              
              # Build label and lineup count on second row
              fluidRow(
                column(3, textInput("sd_build_label", "Build Label:", 
                                    value = "", 
                                    placeholder = "e.g., 'Chalk', 'Contrarian'")),
                column(3, numericInput("sd_num_random_lineups", "Number of Lineups:", 
                                       value = 20, min = 1, max = 150)),
                column(6, "")  # Empty columns
              ),
                column(3, numericInput("sd_num_random_lineups", "Number of Lineups:", 
                                       value = 20, min = 1, max = 150))
              ),
              
              # Filtered pool display and buttons
              fluidRow(
                column(6, div(
                  class = "well well-sm", 
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center;",
                    h4("Filtered Pool:", style = "margin: 0;"),
                    h4(textOutput("sd_filtered_pool_count", inline = TRUE), 
                       style = "margin: 0; font-weight: bold; color: #000000;")
                  ),
                  textOutput("sd_filtered_pool_size")
                )),
                column(6, div(style = "margin-top: 20px;",
                              actionButton("generate_sd_lineups", "Add Lineups to Portfolio", 
                                           class = "btn-primary btn-lg", style = "width: 100%;"),
                              br(), br(),
                              actionButton("clear_sd_builds", "Clear All Builds", 
                                           class = "btn-warning", style = "width: 100%;"),
                              br(), br(),
                              downloadButton("download_sd_random_lineups", "Download Portfolio", 
                                             style = "width: 100%;")))
              )
          )
        ),
        
        # Filtered Pool Statistics Table
        fluidRow(
          box(
            width = 12, 
            title = div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              span("Filtered Pool Fighter Statistics"),
              span(textOutput("sd_filtered_pool_stats_count", inline = TRUE), 
                   style = "font-size: 20px; font-weight: bold; color: #000000;")
            ),
            status = "info",
            solidHeader = TRUE,
            DTOutput("sd_filtered_pool_stats_table") %>% withSpinner(color = "#FFD700")
          )
        ),
        
        # Build Summary Table
        fluidRow(
          box(
            width = 12,
            title = "Lineup Builds Summary",
            status = "warning",
            solidHeader = TRUE,
            DTOutput("sd_builds_summary_table") %>% withSpinner(color = "#FFD700")
          )
        ),
        
        # Fighter Exposure Analysis (All Builds)
        fluidRow(
          box(width = 12, 
              title = "Fighter Exposure Analysis (All Builds)",
              status = "info",
              solidHeader = TRUE,
              DTOutput("sd_fighter_exposure_table") %>% withSpinner(color = "#FFD700"))
        ),
        
        # All Generated Lineups
        fluidRow(
          box(width = 12, 
              title = "All Generated Lineups",
              status = "success",
              solidHeader = TRUE,
              DTOutput("sd_random_lineups_table") %>% withSpinner(color = "#FFD700"))
        )
    )
  })
  
  # DraftKings fantasy projections
  output$dk_fantasy_projections <- renderDT({
    req(rv$dk_fantasy_analysis)
    
    dt <- datatable(
      rv$dk_fantasy_analysis,
      options = list(
        scrollX = TRUE, 
        pageLength = -1,  # Show all rows
        dom = "t",  # Only show table ('t'), no search/pagination
        order = list(list(4, 'desc')),  # Sort by Median_DKScore
        columnDefs = list(
          list(
            targets = "_all",
            className = 'dt-center'
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency('DKSalary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatPercentage(c('DKOwn', 'WinRate'), digits = 1) %>%
      formatRound(c('Median_DKScore', 'Avg_DKScore', 
                    'Win_Median',
                    'DKPPD'), 
                  digits = 1)
    
    dt
  })
  
  # FanDuel fantasy projections
  output$fd_fantasy_projections <- renderDT({
    req(rv$fd_fantasy_analysis)
    
    dt <- datatable(
      rv$fd_fantasy_analysis,
      options = list(
        scrollX = TRUE, 
        pageLength = -1,  # Show all rows
        dom = "t",  # Only show table ('t'), no search/pagination
        order = list(list(4, 'desc')),  # Sort by Median_FDScore
        columnDefs = list(
          list(
            targets = "_all",
            className = 'dt-center'
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency('FDSalary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatPercentage(c('FDOwn', 'WinRate'), digits = 1) %>%
      formatRound(c('Median_FDScore', 'Avg_FDScore', 
                    'Win_Median',
                    'FDPPD'), 
                  digits = 1)
    
    dt
  })
  
  
  # DraftKings Fantasy Points Distribution
  output$dk_fantasy_points_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Focus on win results only since they're more relevant
    win_results <- rv$simulation_results[rv$simulation_results$Result == "Win", ]
    
    # Get unique fighter salary info
    fighter_salaries <- win_results %>%
      group_by(Name) %>%
      summarize(DKSalary = first(DKSalary),
                Avg_Score = mean(DKScore, na.rm = TRUE)) %>%
      arrange(DKSalary)
    
    
    # Order fighter names by average score
    ordered_names <- fighter_salaries$Name
    
    plot_data <- win_results %>%
      filter(Name %in% ordered_names)
    
    p <- ggplot(plot_data, aes(x = factor(Name, levels = ordered_names),
                               y = DKScore,
                               fill = Name)) +
      geom_boxplot(outlier.alpha = 0.25) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Fighter", y = "DK Fantasy Points (Wins Only)") +
      theme(legend.position = "none")  # Hide legend to avoid clutter
    
    ggplotly(p, height = 700, tooltip = c("x", "y")) %>%
      layout(
        margin = list(l = 120, r = 20, b = 50, t = 50),
        yaxis = list(title = "DK Fantasy Points")
      )
  })
  
  # FanDuel Fantasy Points Distribution
  output$fd_fantasy_points_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Focus on win results only since they're more relevant
    win_results <- rv$simulation_results[rv$simulation_results$Result == "Win", ]
    
    # Get unique fighter salary info
    fighter_salaries <- win_results %>%
      group_by(Name) %>%
      summarize(FDSalary = first(FDSalary),
                Avg_Score = mean(FDScore, na.rm = TRUE)) %>%
      arrange(FDSalary)
    
    # Order fighter names by average score
    ordered_names <- fighter_salaries$Name
    
    plot_data <- win_results %>%
      filter(Name %in% ordered_names)
    
    p <- ggplot(plot_data, aes(x = factor(Name, levels = ordered_names),
                               y = FDScore,
                               fill = Name)) +
      geom_boxplot(outlier.alpha = 0.25) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Fighter", y = "FD Fantasy Points (Wins Only)") +
      theme(legend.position = "none")  # Hide legend to avoid clutter
    
    ggplotly(p, height = 700, tooltip = c("x", "y")) %>%
      layout(
        margin = list(l = 120, r = 20, b = 50, t = 50),
        yaxis = list(title = "FD Fantasy Points")
      )
  })
  
  observeEvent(input$run_dk_optimization, {
    req(rv$simulation_results, rv$has_draftkings)
    
    # Clear previous analysis results but keep simulation data
    rv$dk_optimal_lineups <- NULL
    rv$dk_fighter_exposure <- NULL
    rv$dk_random_lineups <- NULL
    
    gc(verbose = FALSE, full = TRUE)
    
    # Show progress dialog
    withProgress(message = 'Calculating DraftKings optimal lineups...', value = 0, {
      # Show a specific modal
      showModal(modalDialog(
        title = "Processing DraftKings Optimal Lineups",
        "Finding optimal lineups using all simulations. This may take a few minutes.",
        footer = NULL,
        easyClose = FALSE
      ))
      
      # Calculate optimal lineups
      setProgress(0.2, detail = "Finding optimal lineups...")
      rv$dk_optimal_lineups <- tryCatch({
        count_dk_optimal_lineups(rv$simulation_results)
      }, error = function(e) {
        message("Error finding optimal lineups: ", e$message)
        removeModal()
        showModal(modalDialog(
          title = "Error Finding Optimal Lineups",
          paste("There was an error:", e$message),
          easyClose = TRUE
        ))
        NULL
      })
      
      gc(verbose = FALSE, full = TRUE)
      
      # Create a fighter mapping from the simulation results
      setProgress(0.7, detail = "Creating fighter mapping...")
      fighter_mapping <- NULL
      if(!is.null(rv$dk_optimal_lineups)) {
        # Get all unique fighters from optimal lineups
        fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
        
        # Use a more generic approach that works with both data.frame and data.table
        lineup_fighters <- c()
        for(col in fighter_cols) {
          if(col %in% names(rv$dk_optimal_lineups)) {
            lineup_fighters <- c(lineup_fighters, rv$dk_optimal_lineups[[col]])
          }
        }
        lineup_fighters <- unique(lineup_fighters)
        
        fighter_mapping <- data.frame(
          Name = lineup_fighters,  
          DKSalary = NA_real_,
          DKOwn = NA_real_
        )
        
        # Get mapping from simulation results
        unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                     c("Name", "DKSalary", "DKOwn")]
        
        # Match each fighter from lineups to the simulation results
        for(i in 1:nrow(fighter_mapping)) {
          name <- fighter_mapping$Name[i]
          # Try to find this fighter in the simulation results
          matches <- which(unique_sim_fighters$Name == name)
          if(length(matches) > 0) {
            match_idx <- matches[1]
            fighter_mapping$Name[i] <- unique_sim_fighters$Name[match_idx]
            fighter_mapping$DKSalary[i] <- unique_sim_fighters$DKSalary[match_idx]
            fighter_mapping$DKOwn[i] <- unique_sim_fighters$DKOwn[match_idx]
          }
        }
        
        # UPDATE SLIDERS IMMEDIATELY WITH ACTUAL DATA
        if("CumulativeOwnership" %in% names(rv$dk_optimal_lineups)) {
          cum_values <- rv$dk_optimal_lineups$CumulativeOwnership[!is.na(rv$dk_optimal_lineups$CumulativeOwnership)]
          if(length(cum_values) > 0) {
            cum_min <- floor(min(cum_values))
            cum_max <- ceiling(max(cum_values))
            
            updateSliderInput(session, "dk_cumulative_ownership_range",
                              min = cum_min,
                              max = cum_max,
                              value = c(cum_min, cum_max))
          }
        }
        
        if("GeometricMeanOwnership" %in% names(rv$dk_optimal_lineups)) {
          geom_values <- rv$dk_optimal_lineups$GeometricMeanOwnership[!is.na(rv$dk_optimal_lineups$GeometricMeanOwnership)]
          if(length(geom_values) > 0) {
            geom_min <- floor(min(geom_values))
            geom_max <- ceiling(max(geom_values))
            
            updateSliderInput(session, "dk_geometric_mean_range",
                              min = geom_min,
                              max = geom_max,
                              value = c(geom_min, geom_max))
          }
        }
      }
      
      # Calculate initial fighter exposure with the mapping
      setProgress(0.8, detail = "Calculating fighter exposure...")
      if(!is.null(rv$dk_optimal_lineups)) {
        if(!is.null(fighter_mapping) && nrow(fighter_mapping) > 0) {
          rv$dk_fighter_exposure <- calculate_dk_fighter_exposure(
            rv$dk_optimal_lineups, 
            fighter_mapping
          )
        } else {
          rv$dk_fighter_exposure <- calculate_dk_fighter_exposure(
            rv$dk_optimal_lineups, 
            rv$dk_fantasy_analysis
          )
        }
        
        # CRITICAL: Initialize filtered pool rates immediately
        if(!is.null(rv$dk_fighter_exposure)) {
          # Set initial filtered pool rates to optimal rates
          rv$dk_fighter_exposure$FilteredPoolRate <- rv$dk_fighter_exposure$OptimalRate
          
          # Update fighter choices for exclusion dropdown
          fighter_data <- rv$dk_fighter_exposure
          fighter_names <- fighter_data$Name
          fighter_labels <- paste0(fighter_names, " (", round(fighter_data$OptimalRate, 1), "%)")
          fighter_choices <- setNames(fighter_names, fighter_labels)
          
    
          # FORCE TABLE TO APPEAR - Use isolate to prevent infinite loops
          isolate({
            # Trigger a reactive dependency by modifying and restoring the data
            temp_exposure <- rv$dk_fighter_exposure
            rv$dk_fighter_exposure <- NULL
            rv$dk_fighter_exposure <- temp_exposure
          })
        }
      }
      
      # Remove the processing modal
      removeModal()
      
      # Show success message with delay to ensure data is processed
      Sys.sleep(0.1)
      showModal(modalDialog(
        title = "Success",
        HTML(sprintf(
          "Successfully generated <b>%d</b> optimal lineups for DraftKings!<br><br>
        You can now go to the <b>Lineup Builder</b> tab to filter and select lineups from this pool.",
          if(!is.null(rv$dk_optimal_lineups)) nrow(rv$dk_optimal_lineups) else 0
        )),
        easyClose = TRUE
      ))
    })
  })
  
 
  observeEvent(input$run_fd_optimization, {
    req(rv$simulation_results, rv$has_fanduel)
    
    # Clear previous analysis results but keep simulation data
    rv$fd_optimal_lineups <- NULL
    rv$sd_optimal_lineups <- NULL
    rv$sd_fighter_exposure <- NULL
    rv$fd_fighter_exposure <- NULL
    rv$fd_random_lineups <- NULL
    
    gc(verbose = FALSE, full = TRUE)
    
    # Show progress dialog
    withProgress(message = 'Calculating FanDuel optimal lineups...', value = 0, {
      # Show a specific modal
      showModal(modalDialog(
        title = "Processing FanDuel Optimal Lineups",
        "Finding optimal lineups using all simulations. This may take a few minutes.",
        footer = NULL,
        easyClose = FALSE
      ))
      
      # Calculate optimal lineups
      setProgress(0.2, detail = "Finding optimal lineups...")
      rv$fd_optimal_lineups <- tryCatch({
        count_fd_optimal_lineups(rv$simulation_results)
      }, error = function(e) {
        message("Error finding optimal lineups: ", e$message)
        removeModal()
        showModal(modalDialog(
          title = "Error Finding Optimal Lineups",
          paste("There was an error:", e$message),
          easyClose = TRUE
        ))
        NULL
      })
      
      gc(verbose = FALSE, full = TRUE)
      
      # Create a fighter mapping from the simulation results
      setProgress(0.7, detail = "Creating fighter mapping...")
      fighter_mapping <- NULL
      if(!is.null(rv$fd_optimal_lineups)) {
        # Get all unique fighters from optimal lineups
        fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
        
        # Use a generic approach that works with both data.frame and data.table
        lineup_fighters <- c()
        for(col in fighter_cols) {
          if(col %in% names(rv$fd_optimal_lineups)) {
            lineup_fighters <- c(lineup_fighters, rv$fd_optimal_lineups[[col]])
          }
        }
        
        # Also include MVP fighters if present
        if("MVP" %in% names(rv$fd_optimal_lineups)) {
          lineup_fighters <- c(lineup_fighters, rv$fd_optimal_lineups$MVP)
        }
        
        lineup_fighters <- unique(lineup_fighters)
        
        fighter_mapping <- data.frame(
          Name = lineup_fighters,
          FDSalary = NA_real_,
          FDOwn = NA_real_,
          FDID = NA_character_
        )
        
        # Get mapping from simulation results
        unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                     c("Name", "FDSalary", "FDOwn", "FDID")]
        
        # Match each fighter from lineups
        for(i in 1:nrow(fighter_mapping)) {
          name <- fighter_mapping$Name[i]
          # Try to find this fighter in the simulation results
          matches <- which(unique_sim_fighters$Name == name)
          if(length(matches) > 0) {
            match_idx <- matches[1]
            fighter_mapping$Name[i] <- unique_sim_fighters$Name[match_idx]
            fighter_mapping$FDSalary[i] <- unique_sim_fighters$FDSalary[match_idx]
            fighter_mapping$FDOwn[i] <- unique_sim_fighters$FDOwn[match_idx]
            fighter_mapping$FDID[i] <- unique_sim_fighters$FDID[match_idx]
            
          }
        }
      }
      
      # Calculate initial fighter exposure with the mapping
      setProgress(0.8, detail = "Calculating fighter exposure...")
      if(!is.null(rv$fd_optimal_lineups)) {
        if(!is.null(fighter_mapping) && nrow(fighter_mapping) > 0) {
          rv$fd_fighter_exposure <- calculate_fd_fighter_exposure(
            rv$fd_optimal_lineups, 
            fighter_mapping
          )
        } else {
          rv$fd_fighter_exposure <- calculate_fd_fighter_exposure(
            rv$fd_optimal_lineups, 
            rv$fd_fantasy_analysis
          )
        }
      }
      
      if(!is.null(rv$fd_fighter_exposure)) {
        # Initialize filtered pool rates
        rv$fd_fighter_exposure$FilteredPoolRate <- rv$fd_fighter_exposure$OptimalRate
        
        # Get all fighters from the fighter exposure data
        fighter_data <- rv$fd_fighter_exposure
        fighter_names <- fighter_data$Name
        fighter_labels <- paste0(fighter_names, " (Opt: ", round(fighter_data$OptimalRate, 1), 
                                 "%, MVP: ", round(fighter_data$MVPRate, 1), "%)")
        fighter_choices <- setNames(fighter_names, fighter_labels)
        
        updateSelectizeInput(
          session = session,
          inputId = "fd_excluded_fighters",
          choices = fighter_choices
        )
        
        # FORCE REACTIVE UPDATE
        session$sendCustomMessage("shiny:updateinput", list(
          inputId = "fd_fighter_exposure_table"
        ))
      }
      
      # Remove the processing modal
      removeModal()
    })
    
    
    # Update excluded fighters selection for the lineup builder
    if(!is.null(rv$fd_optimal_lineups) && !is.null(rv$fd_fighter_exposure)) {
      # Get all fighters from the fighter exposure data
      fighter_data <- rv$fd_fighter_exposure
      
      # Create basic named vector for choices
      fighter_names <- fighter_data$Name
      fighter_ids <- fighter_data$FDID
      
      # Create simple labels with name and optimal rate
      fighter_labels <- paste0(fighter_names, " (", round(fighter_data$OptimalRate, 1), "%)")
      
      # If we have MVPRate, include it in the label
      if("MVPRate" %in% names(fighter_data)) {
        fighter_labels <- paste0(fighter_names, " (Opt: ", round(fighter_data$OptimalRate, 1), 
                                 "%, MVP: ", round(fighter_data$MVPRate, 1), "%)")
      }
      
      # Create choices with names
      fighter_choices <- setNames(fighter_ids, fighter_labels)
      
      # Show success message
      showModal(modalDialog(
        title = "Success",
        HTML(sprintf(
          "Successfully generated <b>%d</b> optimal lineups for FanDuel with MVP!<br><br>
        You can now go to the <b>Lineup Builder</b> tab to filter and select lineups from this pool.",
          nrow(rv$fd_optimal_lineups)
        )),
        easyClose = TRUE
      ))
    }
  })
  
  # DraftKings optimal lineups table
  output$dk_optimal_lineups_table <- renderDT({
    req(rv$dk_optimal_lineups)
    
    # Clone lineups for display (fighter names are already used in the lineup)
    display_data <- as.data.table(rv$dk_optimal_lineups)
    
    # No need to convert from DKID to Name since we're already using Names
    
    # Keep only necessary columns
    cols_to_keep <- c(paste0("Fighter", 1:DK_ROSTER_SIZE), 
                      grep("^Top[0-9]+Count$", names(display_data), value = TRUE),
                      "TotalSalary", "CumulativeOwnership", "GeometricMeanOwnership")
    cols_to_keep <- intersect(cols_to_keep, names(display_data))
    
    # Use the correct data.table syntax
    display_data <- display_data[, ..cols_to_keep]
    
    # Sort the data by Top1Count, then Top5Count (both descending)
    if("Top1Count" %in% names(display_data) && "Top5Count" %in% names(display_data)) {
      setorder(display_data, -Top1Count, -Top5Count)
    } else if("Top1Count" %in% names(display_data)) {
      setorder(display_data, -Top1Count)
    }
    
    # Find TopXCount column indices for ordering
    top1_idx <- which(names(display_data) == "Top1Count") - 1  # 0-based index for JS
    top5_idx <- which(names(display_data) == "Top5Count") - 1  # 0-based index for JS
    
    # Create the datatable with pagination but no length control
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        rownames = FALSE,
        dom = "ftp",
        ordering = TRUE,
        order = if(length(top1_idx) > 0 && length(top5_idx) > 0) {
          list(list(top1_idx, 'desc'), list(top5_idx, 'desc'))
        } else {
          list()
        },
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Apply formatting to TotalSalary column
    if("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    if("CumulativeOwnership" %in% names(display_data)) {
      dt <- dt %>% formatRound('CumulativeOwnership', digits = 1) %>%
        formatStyle('CumulativeOwnership', 
                    `text-align` = 'center',
                    `background-color` = '#f8f9fa')
    }
    if("GeometricMeanOwnership" %in% names(display_data)) {
      dt <- dt %>% formatRound('GeometricMeanOwnership', digits = 2) %>%
        formatStyle('GeometricMeanOwnership', 
                    `text-align` = 'center',
                    `background-color` = '#f8f9fa')
    }
    
    # Apply formatting to count columns
    count_cols <- grep("^Top[0-9]+Count$", names(display_data), value = TRUE)
    for(col in count_cols) {
      if(any(!is.na(display_data[[col]]))) {
        max_count <- max(display_data[[col]], na.rm = TRUE)
        if(is.finite(max_count) && max_count > 0) {
          dt <- dt %>% formatStyle(
            col,
            background = styleColorBar(c(0, max_count), 'lightblue'),
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
        }
      }
    }
    
    dt
  })
  
  output$fd_optimal_lineups_table <- renderDT({
    req(rv$fd_optimal_lineups)
    
    # Clone lineups for display
    display_data <- as.data.table(rv$fd_optimal_lineups)
    
    # Format fighter columns to show names
    if(!is.null(rv$fd_fighter_exposure)) {
      for(i in 1:FD_ROSTER_SIZE) {
        col <- paste0("Fighter", i)
        if(col %in% names(display_data)) {
          display_data[[col]] <- sapply(display_data[[col]], function(id) {
            match_idx <- which(rv$fd_fighter_exposure$Name == id)
            if(length(match_idx) > 0) {
              # Already a name
              id
            } else {
              # Look up by FDID
              match_idx <- which(rv$fd_fighter_exposure$FDID == id)
              if(length(match_idx) > 0) {
                rv$fd_fighter_exposure$Name[match_idx[1]]
              } else {
                id
              }
            }
          })
        }
      }
      
      # Also format MVP column if it exists
      if("MVP" %in% names(display_data)) {
        display_data$MVP <- sapply(display_data$MVP, function(id) {
          match_idx <- which(rv$fd_fighter_exposure$Name == id)
          if(length(match_idx) > 0) {
            # Already a name
            id
          } else {
            # Look up by FDID
            match_idx <- which(rv$fd_fighter_exposure$FDID == id)
            if(length(match_idx) > 0) {
              rv$fd_fighter_exposure$Name[match_idx[1]]
            } else {
              id
            }
          }
        })
      }
    }
    
    # Create a new data structure with exactly 1 MVP and 5 fighters
    new_display_data <- data.table()
    
    # Process each lineup
    for(i in 1:nrow(display_data)) {
      # Get the MVP (ensuring there is one)
      mvp_name <- NA
      if("MVP" %in% names(display_data) && !is.na(display_data$MVP[i])) {
        mvp_name <- display_data$MVP[i]
      } else {
        # If no MVP is specified, use the highest scoring fighter as MVP
        # (this shouldn't happen with proper optimization, but as a fallback)
        if("Lineup" %in% names(display_data)) {
          all_fighters <- unlist(strsplit(display_data$Lineup[i], "\\|"))
          if(length(all_fighters) > 0) {
            mvp_name <- all_fighters[1]  # Use first fighter as MVP if none specified
          }
        } else {
          fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
          fighter_cols <- intersect(fighter_cols, names(display_data))
          if(length(fighter_cols) > 0) {
            fighters_in_row <- unlist(display_data[i, ..fighter_cols])
            fighters_in_row <- fighters_in_row[!is.na(fighters_in_row)]
            if(length(fighters_in_row) > 0) {
              mvp_name <- fighters_in_row[1]  # Use first non-NA fighter as MVP if none specified
            }
          }
        }
      }
      
      # Get all other fighters (excluding the MVP)
      fighter_list <- c()
      
      if("Lineup" %in% names(display_data)) {
        # If Lineup column exists, use it
        all_fighters <- unlist(strsplit(display_data$Lineup[i], "\\|"))
        fighter_list <- all_fighters[all_fighters != mvp_name]
      } else {
        # Otherwise use Fighter columns
        fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
        fighter_cols <- intersect(fighter_cols, names(display_data))
        if(length(fighter_cols) > 0) {
          all_fighters <- unlist(display_data[i, ..fighter_cols])
          all_fighters <- all_fighters[!is.na(all_fighters)]
          fighter_list <- all_fighters[all_fighters != mvp_name]
        }
      }
      
      # Ensure we have exactly 5 fighters plus the MVP
      if(length(fighter_list) > 5) {
        # If more than 5 non-MVP fighters, keep only the first 5
        fighter_list <- fighter_list[1:5]
      } else if(length(fighter_list) < 5) {
        # If fewer than 5, pad with placeholder values
        while(length(fighter_list) < 5) {
          fighter_list <- c(fighter_list, paste("Fighter", length(fighter_list) + 1))
        }
      }
      
      # Create a new row with MVP and exactly 5 fighters
      new_row <- data.table(
        MVP = mvp_name
      )
      
      # Add exactly 5 fighter columns
      for(j in 1:5) {
        new_row[[paste0("Fighter", j)]] <- fighter_list[j]
      }
      
      # Copy over count columns and TotalSalary
      count_cols <- grep("^Top[0-9]+Count$", names(display_data), value = TRUE)
      for(col in c(count_cols, "TotalSalary")) {
        if(col %in% names(display_data)) {
          new_row[[col]] <- display_data[[col]][i]
        }
      }
      
      # Add to the new display data
      new_display_data <- rbind(new_display_data, new_row)
    }
    
    # Replace the display data with our standardized version
    display_data <- new_display_data
    
    # Sort the data by Top1Count, then Top5Count (both descending)
    if("Top1Count" %in% names(display_data) && "Top5Count" %in% names(display_data)) {
      setorder(display_data, -Top1Count, -Top5Count)
    } else if("Top1Count" %in% names(display_data)) {
      setorder(display_data, -Top1Count)
    }
    
    # Find TopXCount column indices for ordering
    top1_idx <- which(names(display_data) == "Top1Count") - 1  # 0-based index for JS
    top5_idx <- which(names(display_data) == "Top5Count") - 1  # 0-based index for JS
    
    # Create the datatable with pagination
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        rownames = FALSE,
        dom = "ftp",          # Only show table and pagination
        ordering = TRUE,
        order = if(length(top1_idx) > 0 && length(top5_idx) > 0) {
          list(list(top1_idx, 'desc'), list(top5_idx, 'desc'))
        } else {
          list()
        },
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Apply formatting to TotalSalary column
    if("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    # Apply formatting to count columns
    count_cols <- grep("^Top[0-9]+Count$", names(display_data), value = TRUE)
    for(col in count_cols) {
      if(any(!is.na(display_data[[col]]))) {
        max_count <- max(display_data[[col]], na.rm = TRUE)
        if(is.finite(max_count) && max_count > 0) {
          dt <- dt %>% formatStyle(
            col,
            background = styleColorBar(c(0, max_count), 'lightblue'),
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
        }
      }
    }
    
    # Highlight MVP column
    if("MVP" %in% names(display_data)) {
      dt <- dt %>% formatStyle(
        'MVP',
        backgroundColor = '#FFD700',  # Gold background
        fontWeight = 'bold',          # Bold text
        color = '#000000'             # Black text for better contrast
      )
    }
    
    dt
  })
  
  
  

  
  # DraftKings filtered pool reactive
  dk_filtered_optimal_lineups <- reactive({
    req(rv$dk_optimal_lineups)
    
    optimal <- rv$dk_optimal_lineups
    
    # Apply Top Count filters
    filtered <- optimal %>%
      filter(
        Top1Count >= input$dk_min_top1_count,
        Top2Count >= input$dk_min_top2_count,
        Top3Count >= input$dk_min_top3_count,
        Top5Count >= input$dk_min_top5_count
      )
    
    # Apply ownership filters if columns exist
    if("CumulativeOwnership" %in% names(filtered)) {
      filtered <- filtered %>%
        filter(
          CumulativeOwnership >= input$dk_cumulative_ownership_range[1],
          CumulativeOwnership <= input$dk_cumulative_ownership_range[2]
        )
    }
    
    if("GeometricMeanOwnership" %in% names(filtered)) {
      filtered <- filtered %>%
        filter(
          GeometricMeanOwnership >= input$dk_geometric_mean_range[1],
          GeometricMeanOwnership <= input$dk_geometric_mean_range[2]
        )
    }
    
    # Apply exclude filter
    if(!is.null(input$dk_excluded_fighters) && length(input$dk_excluded_fighters) > 0) {
      fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
      
      for(excluded_fighter in input$dk_excluded_fighters) {
        for(col in fighter_cols) {
          if(col %in% names(filtered)) {
            filtered <- filtered[filtered[[col]] != excluded_fighter, ]
          }
        }
      }
    }
    
    # Apply lock filter
    if(!is.null(input$dk_locked_fighters) && length(input$dk_locked_fighters) > 0) {
      fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
      
      for(locked_fighter in input$dk_locked_fighters) {
        # Keep only lineups that contain this fighter
        has_fighter <- rep(FALSE, nrow(filtered))
        for(col in fighter_cols) {
          if(col %in% names(filtered)) {
            has_fighter <- has_fighter | (filtered[[col]] == locked_fighter)
          }
        }
        filtered <- filtered[has_fighter, ]
      }
    }
    
    return(filtered)
  })
  
  # DK filtered pool size display
  output$dk_filtered_pool_size <- renderText({
    filtered <- dk_filtered_optimal_lineups()
    
    if(is.null(filtered) || nrow(filtered) == 0) {
      return("No lineups match current filters")
    }
    
    paste0(
      nrow(filtered), " lineups in filtered pool ",
      "(", round(nrow(filtered) / nrow(rv$dk_optimal_lineups) * 100, 1), "% of total)"
    )
  })
  
  # DK filtered pool count (for title display)
  output$dk_filtered_pool_count <- renderText({
    filtered <- dk_filtered_optimal_lineups()
    if(is.null(filtered) || nrow(filtered) == 0) return("0")
    return(format(nrow(filtered), big.mark = ","))
  })
  
  # DK filtered pool stats count (for stats table title)
  output$dk_filtered_pool_stats_count <- renderText({
    filtered <- dk_filtered_optimal_lineups()
    if(is.null(filtered) || nrow(filtered) == 0) return("0 lineups")
    return(paste0(format(nrow(filtered), big.mark = ","), " lineups"))
  })
  
  observeEvent(input$generate_dk_lineups, {
    req(rv$dk_optimal_lineups)
    
    filtered_pool <- dk_filtered_optimal_lineups()
    
    if(is.null(filtered_pool) || nrow(filtered_pool) == 0) {
      showModal(modalDialog(
        title = "Error",
        "No lineups match the selected filters. Try adjusting your criteria.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Get number of lineups to generate
    num_lineups <- input$dk_num_random_lineups
    
    # Get build label (or generate default)
    build_label <- if(!is.null(input$dk_build_label) && nchar(trimws(input$dk_build_label)) > 0) {
      trimws(input$dk_build_label)
    } else {
      paste0("Build ", length(rv$dk_lineup_builds) + 1)
    }
    
    # Show progress
    withProgress(message = 'Generating lineups...', value = 0, {
      # Sample from filtered pool
      if(nrow(filtered_pool) <= num_lineups) {
        # If filtered pool is smaller, take all
        sampled_lineups <- filtered_pool
      } else {
        # Weight by Top1Count
        weights <- filtered_pool$Top1Count + 1  # Add 1 to avoid zero weights
        sampled_indices <- sample(1:nrow(filtered_pool), size = num_lineups, 
                                  prob = weights, replace = FALSE)
        sampled_lineups <- filtered_pool[sampled_indices, ]
      }
      
      # Add build metadata
      sampled_lineups$BuildLabel <- build_label
      sampled_lineups$LineupNum <- 1:nrow(sampled_lineups)
      
      # Store this build
      build_data <- list(
        label = build_label,
        timestamp = Sys.time(),
        filters = list(
          min_top1 = input$dk_min_top1_count,
          min_top2 = input$dk_min_top2_count,
          min_top3 = input$dk_min_top3_count,
          min_top5 = input$dk_min_top5_count,
          locked = input$dk_locked_fighters,
          excluded = input$dk_excluded_fighters,
          cum_own = input$dk_cumulative_ownership_range,
          geom_own = input$dk_geometric_mean_range
        ),
        lineups = sampled_lineups
      )
      
      # Add to builds list
      rv$dk_lineup_builds[[length(rv$dk_lineup_builds) + 1]] <- build_data
      
      # Combine all builds for display
      all_lineups <- do.call(rbind, lapply(rv$dk_lineup_builds, function(b) b$lineups))
      rv$dk_random_lineups <- all_lineups
      
      # Update fighter exposure
      if(!is.null(rv$dk_random_lineups)) {
        existing_mapping <- NULL
        if(!is.null(rv$dk_fighter_exposure)) {
          existing_mapping <- rv$dk_fighter_exposure[, c("Name", "DKSalary", "DKOwn")]
        }
        
        if(is.null(existing_mapping) || nrow(existing_mapping) == 0) {
          fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
          all_fighters <- c()
          
          for(col in fighter_cols) {
            if(col %in% names(rv$dk_optimal_lineups)) {
              all_fighters <- c(all_fighters, rv$dk_optimal_lineups[[col]])
            }
          }
          
          for(col in fighter_cols) {
            if(col %in% names(rv$dk_random_lineups)) {
              all_fighters <- c(all_fighters, rv$dk_random_lineups[[col]])
            }
          }
          all_fighters <- unique(all_fighters)
          
          fighter_mapping <- data.frame(
            Name = all_fighters,
            DKSalary = NA_real_,
            DKOwn = NA_real_
          )
          
          unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                       c("Name", "DKSalary", "DKOwn")]
          
          for(i in 1:nrow(fighter_mapping)) {
            name <- fighter_mapping$Name[i]
            matches <- which(unique_sim_fighters$Name == name)
            if(length(matches) > 0) {
              match_idx <- matches[1]
              fighter_mapping$Name[i] <- unique_sim_fighters$Name[match_idx]
              fighter_mapping$DKSalary[i] <- unique_sim_fighters$DKSalary[match_idx]
              fighter_mapping$DKOwn[i] <- unique_sim_fighters$DKOwn[match_idx]
            }
          }
        } else {
          fighter_mapping <- existing_mapping
        }
        
        rv$dk_fighter_exposure <- calculate_dk_fighter_exposure(
          rv$dk_optimal_lineups, 
          fighter_mapping, 
          rv$dk_random_lineups
        )
      }
      
      showModal(modalDialog(
        title = "Success",
        sprintf("Added %d lineups to build '%s'! Total portfolio: %d lineups across %d builds.", 
                nrow(sampled_lineups), build_label, nrow(rv$dk_random_lineups), 
                length(rv$dk_lineup_builds)),
        easyClose = TRUE
      ))
    })
  })
  
  # Clear DK builds
  observeEvent(input$clear_dk_builds, {
    rv$dk_lineup_builds <- list()
    rv$dk_random_lineups <- NULL
    
    # Recalculate exposure with just optimal lineups
    if(!is.null(rv$dk_optimal_lineups) && !is.null(rv$simulation_results)) {
      fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
      lineup_fighters <- c()
      for(col in fighter_cols) {
        if(col %in% names(rv$dk_optimal_lineups)) {
          lineup_fighters <- c(lineup_fighters, rv$dk_optimal_lineups[[col]])
        }
      }
      lineup_fighters <- unique(lineup_fighters)
      
      fighter_mapping <- data.frame(
        Name = lineup_fighters,  
        DKSalary = NA_real_,
        DKOwn = NA_real_
      )
      
      unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                   c("Name", "DKSalary", "DKOwn")]
      
      for(i in 1:nrow(fighter_mapping)) {
        name <- fighter_mapping$Name[i]
        matches <- which(unique_sim_fighters$Name == name)
        if(length(matches) > 0) {
          match_idx <- matches[1]
          fighter_mapping$Name[i] <- unique_sim_fighters$Name[match_idx]
          fighter_mapping$DKSalary[i] <- unique_sim_fighters$DKSalary[match_idx]
          fighter_mapping$DKOwn[i] <- unique_sim_fighters$DKOwn[match_idx]
        }
      }
      
      rv$dk_fighter_exposure <- calculate_dk_fighter_exposure(
        rv$dk_optimal_lineups, 
        fighter_mapping
      )
      
      if(!is.null(rv$dk_fighter_exposure)) {
        rv$dk_fighter_exposure$FilteredPoolRate <- rv$dk_fighter_exposure$OptimalRate
      }
    }
    
    showModal(modalDialog(
      title = "Portfolio Cleared",
      "All DraftKings builds have been cleared.",
      easyClose = TRUE
    ))
  })
  
  output$dk_filtered_pool_stats_table <- renderDT({
    req(rv$simulation_complete)
    
    filtered_pool <- dk_filtered_optimal_lineups()
    
    if(is.null(filtered_pool) || nrow(filtered_pool) == 0) {
      return(datatable(data.frame(Message = "No lineups in filtered pool"),
                       options = list(dom = 't'), rownames = FALSE))
    }
    
    tryCatch({
      # Get fighter columns
      fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
      
      # Extract all fighters from filtered pool
      all_fighters <- c()
      for(col in fighter_cols) {
        if(col %in% names(filtered_pool)) {
          all_fighters <- c(all_fighters, filtered_pool[[col]])
        }
      }
      
      # Count appearances
      fighter_counts <- as.data.frame(table(all_fighters))
      names(fighter_counts) <- c("Name", "Appearances")
      
      # Calculate FilteredPoolRate
      fighter_counts$FilteredPoolRate <- round(
        (fighter_counts$Appearances / nrow(filtered_pool)) * 100, 1
      )
      
      # Get fighter info from simulation results (salary and ownership)
      unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                   c("Name", "DKSalary", "DKOwn")]
      
      # Merge with fighter info
      fighter_stats <- merge(fighter_counts, unique_sim_fighters, by = "Name", all.x = TRUE)
      
      # Convert DKOwn to percentage and calculate Leverage
      fighter_stats$Own <- fighter_stats$DKOwn * 100
      fighter_stats$Leverage <- round(fighter_stats$FilteredPoolRate - fighter_stats$Own, 1)
      
      # Select and order columns (NO OptimalRate)
      fighter_stats <- fighter_stats[, c("Name", "DKSalary", "FilteredPoolRate", "Own", "Leverage")]
      fighter_stats <- fighter_stats[order(-fighter_stats$FilteredPoolRate), ]
      
      # Create caption
      caption_text <- sprintf("Filtered Pool: %s lineups", 
                              format(nrow(filtered_pool), big.mark = ","))
      
      # Create display
      dt <- datatable(
        fighter_stats,
        caption = caption_text,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = 'ftp',
          order = list(list(2, 'desc')),  # Sort by FilteredPoolRate
          columnDefs = list(
            list(targets = "_all", className = 'dt-center')
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe compact'
      )
      
      # Format columns
      if("DKSalary" %in% names(fighter_stats)) {
        dt <- dt %>% formatCurrency('DKSalary', currency = "$", interval = 3, 
                                    mark = ",", digits = 0)
      }
      
      # Format numeric columns
      numeric_cols <- intersect(c('FilteredPoolRate', 'Own', 'Leverage'), 
                                names(fighter_stats))
      if(length(numeric_cols) > 0) {
        dt <- dt %>% formatRound(numeric_cols, digits = 1)
      }
      
      # Add color bar to FilteredPoolRate
      dt <- dt %>% formatStyle(
        'FilteredPoolRate',
        background = styleColorBar(range(fighter_stats$FilteredPoolRate, na.rm = TRUE), '#32CD32'),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
      
      return(dt)
      
    }, error = function(e) {
      return(datatable(data.frame(Error = paste("Error:", e$message)),
                       options = list(dom = 't'), rownames = FALSE))
    })
  })
  
  # DK Builds Summary Table
  output$dk_builds_summary_table <- renderDT({
    req(length(rv$dk_lineup_builds) > 0)
    
    # Create summary of each build
    build_summary <- data.frame(
      BuildLabel = character(),
      NumLineups = integer(),
      LockedFighters = character(),
      ExcludedFighters = character(),
      MinTop1 = integer(),
      AvgTop1Count = numeric(),
      Timestamp = character(),
      stringsAsFactors = FALSE
    )
    
    for(i in 1:length(rv$dk_lineup_builds)) {
      build <- rv$dk_lineup_builds[[i]]
      
      locked_str <- if(length(build$filters$locked) > 0) {
        paste(build$filters$locked, collapse = ", ")
      } else {
        "None"
      }
      
      excluded_str <- if(length(build$filters$excluded) > 0) {
        paste(build$filters$excluded, collapse = ", ")
      } else {
        "None"
      }
      
      build_summary <- rbind(build_summary, data.frame(
        BuildLabel = build$label,
        NumLineups = nrow(build$lineups),
        LockedFighters = locked_str,
        ExcludedFighters = excluded_str,
        MinTop1 = build$filters$min_top1,
        AvgTop1Count = round(mean(build$lineups$Top1Count, na.rm = TRUE), 1),
        Timestamp = format(build$timestamp, "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      ))
    }
    
    # Add totals row
    totals_row <- data.frame(
      BuildLabel = "TOTAL PORTFOLIO",
      NumLineups = sum(build_summary$NumLineups),
      LockedFighters = "",
      ExcludedFighters = "",
      MinTop1 = NA,
      AvgTop1Count = round(mean(rv$dk_random_lineups$Top1Count, na.rm = TRUE), 1),
      Timestamp = "",
      stringsAsFactors = FALSE
    )
    
    build_summary <- rbind(build_summary, totals_row)
    
    dt <- datatable(
      build_summary,
      options = list(
        scrollX = TRUE,
        pageLength = -1,
        dom = 't',
        columnDefs = list(
          list(targets = "_all", className = 'dt-center')
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'BuildLabel',
        target = 'row',
        fontWeight = styleEqual('TOTAL PORTFOLIO', 'bold'),
        backgroundColor = styleEqual('TOTAL PORTFOLIO', '#ffffcc')
      )
    
    return(dt)
  })
  
  
  # Dedicated reactive for DraftKings fighter exposure - ensures it loads immediately
  observe({
    if(!is.null(rv$dk_optimal_lineups) && !is.null(rv$simulation_results) && is.null(rv$dk_fighter_exposure)) {
      
      # Create fighter mapping
      fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
      lineup_fighters <- c()
      for(col in fighter_cols) {
        if(col %in% names(rv$dk_optimal_lineups)) {
          lineup_fighters <- c(lineup_fighters, rv$dk_optimal_lineups[[col]])
        }
      }
      lineup_fighters <- unique(lineup_fighters)
      
      fighter_mapping <- data.frame(
        Name = lineup_fighters,  
        DKSalary = NA_real_,
        DKOwn = NA_real_
      )
      
      # Get mapping from simulation results
      unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                   c("Name", "DKSalary", "DKOwn")]
      
      # Match each fighter
      for(i in 1:nrow(fighter_mapping)) {
        name <- fighter_mapping$Name[i]
        matches <- which(unique_sim_fighters$Name == name)
        if(length(matches) > 0) {
          match_idx <- matches[1]
          fighter_mapping$Name[i] <- unique_sim_fighters$Name[match_idx]
          fighter_mapping$DKSalary[i] <- unique_sim_fighters$DKSalary[match_idx]
          fighter_mapping$DKOwn[i] <- unique_sim_fighters$DKOwn[match_idx]
        }
      }
      
      # Calculate fighter exposure
      rv$dk_fighter_exposure <- calculate_dk_fighter_exposure(
        rv$dk_optimal_lineups, 
        fighter_mapping
      )
      
      # Initialize filtered pool rates
      if(!is.null(rv$dk_fighter_exposure)) {
        rv$dk_fighter_exposure$FilteredPoolRate <- rv$dk_fighter_exposure$OptimalRate
        
        # Update fighter choices for exclusion dropdown
        fighter_data <- rv$dk_fighter_exposure
        fighter_names <- fighter_data$Name
        fighter_labels <- paste0(fighter_names, " (", round(fighter_data$OptimalRate, 1), "%)")
        fighter_choices <- setNames(fighter_names, fighter_labels)
        
            }
    }
  })
  
  # FanDuel filtered pool reactive
  fd_filtered_optimal_lineups <- reactive({
    req(rv$fd_optimal_lineups)
    
    optimal <- rv$fd_optimal_lineups
    
    # Apply Top Count filters
    filtered <- optimal %>%
      filter(
        Top1Count >= input$fd_min_top1_count,
        Top2Count >= input$fd_min_top2_count,
        Top3Count >= input$fd_min_top3_count,
        Top5Count >= input$fd_min_top5_count
      )
    
    # Apply exclude filter
    if(!is.null(input$fd_excluded_fighters) && length(input$fd_excluded_fighters) > 0) {
      fighter_cols <- c("MVP", paste0("Fighter", 1:(FD_ROSTER_SIZE-1)))
      
      for(excluded_fighter in input$fd_excluded_fighters) {
        for(col in fighter_cols) {
          if(col %in% names(filtered)) {
            filtered <- filtered[filtered[[col]] != excluded_fighter, ]
          }
        }
      }
    }
    
    # Apply lock filter
    if(!is.null(input$fd_locked_fighters) && length(input$fd_locked_fighters) > 0) {
      fighter_cols <- c("MVP", paste0("Fighter", 1:(FD_ROSTER_SIZE-1)))
      
      for(locked_fighter in input$fd_locked_fighters) {
        # Keep only lineups that contain this fighter
        has_fighter <- rep(FALSE, nrow(filtered))
        for(col in fighter_cols) {
          if(col %in% names(filtered)) {
            has_fighter <- has_fighter | (filtered[[col]] == locked_fighter)
          }
        }
        filtered <- filtered[has_fighter, ]
      }
    }
    
    return(filtered)
  })
  
  # FD filtered pool size display
  output$fd_filtered_pool_size <- renderText({
    filtered <- fd_filtered_optimal_lineups()
    
    if(is.null(filtered) || nrow(filtered) == 0) {
      return("No lineups match current filters")
    }
    
    paste0(
      nrow(filtered), " lineups in filtered pool ",
      "(", round(nrow(filtered) / nrow(rv$fd_optimal_lineups) * 100, 1), "% of total)"
    )
  })
  
  # FD filtered pool count (for title display)
  output$fd_filtered_pool_count <- renderText({
    filtered <- fd_filtered_optimal_lineups()
    if(is.null(filtered) || nrow(filtered) == 0) return("0")
    return(format(nrow(filtered), big.mark = ","))
  })
  
  # FD filtered pool stats count (for stats table title)
  output$fd_filtered_pool_stats_count <- renderText({
    filtered <- fd_filtered_optimal_lineups()
    if(is.null(filtered) || nrow(filtered) == 0) return("0 lineups")
    return(paste0(format(nrow(filtered), big.mark = ","), " lineups"))
  })
  
  # ===== FD LINEUP GENERATION HANDLER =====
  # REPLACE observeEvent(input$generate_fd_lineups, { ... })
  
  observeEvent(input$generate_fd_lineups, {
    req(rv$fd_optimal_lineups)
    
    filtered_pool <- fd_filtered_optimal_lineups()
    
    if(is.null(filtered_pool) || nrow(filtered_pool) == 0) {
      showModal(modalDialog(
        title = "Error",
        "No lineups match the selected filters. Try adjusting your criteria.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Get number of lineups to generate
    num_lineups <- input$fd_num_random_lineups
    
    # Get build label (or generate default)
    build_label <- if(!is.null(input$fd_build_label) && nchar(trimws(input$fd_build_label)) > 0) {
      trimws(input$fd_build_label)
    } else {
      paste0("Build ", length(rv$fd_lineup_builds) + 1)
    }
    
    # Show progress
    withProgress(message = 'Generating lineups...', value = 0, {
      # Sample from filtered pool
      if(nrow(filtered_pool) <= num_lineups) {
        sampled_lineups <- filtered_pool
      } else {
        weights <- filtered_pool$Top1Count + 1
        sampled_indices <- sample(1:nrow(filtered_pool), size = num_lineups, 
                                  prob = weights, replace = FALSE)
        sampled_lineups <- filtered_pool[sampled_indices, ]
      }
      
      # Add build metadata
      sampled_lineups$BuildLabel <- build_label
      sampled_lineups$LineupNum <- 1:nrow(sampled_lineups)
      
      # Store this build
      build_data <- list(
        label = build_label,
        timestamp = Sys.time(),
        filters = list(
          min_top1 = input$fd_min_top1_count,
          min_top2 = input$fd_min_top2_count,
          min_top3 = input$fd_min_top3_count,
          min_top5 = input$fd_min_top5_count,
          locked = input$fd_locked_fighters,
          excluded = input$fd_excluded_fighters
        ),
        lineups = sampled_lineups
      )
      
      # Add to builds list
      rv$fd_lineup_builds[[length(rv$fd_lineup_builds) + 1]] <- build_data
      
      # Combine all builds for display
      all_lineups <- do.call(rbind, lapply(rv$fd_lineup_builds, function(b) b$lineups))
      rv$fd_random_lineups <- all_lineups
      
      # Update fighter exposure
      if(!is.null(rv$fd_random_lineups)) {
        existing_mapping <- NULL
        if(!is.null(rv$fd_fighter_exposure)) {
          existing_mapping <- rv$fd_fighter_exposure[, c("Name", "FDSalary", "FDOwn")]
        }
        
        if(is.null(existing_mapping) || nrow(existing_mapping) == 0) {
          fighter_cols <- c("MVP", paste0("Fighter", 1:(FD_ROSTER_SIZE-1)))
          all_fighters <- c()
          
          for(col in fighter_cols) {
            if(col %in% names(rv$fd_optimal_lineups)) {
              all_fighters <- c(all_fighters, rv$fd_optimal_lineups[[col]])
            }
          }
          
          for(col in fighter_cols) {
            if(col %in% names(rv$fd_random_lineups)) {
              all_fighters <- c(all_fighters, rv$fd_random_lineups[[col]])
            }
          }
          all_fighters <- unique(all_fighters)
          
          fighter_mapping <- data.frame(
            Name = all_fighters,
            FDSalary = NA_real_,
            FDOwn = NA_real_
          )
          
          unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                       c("Name", "FDSalary", "FDOwn")]
          
          for(i in 1:nrow(fighter_mapping)) {
            name <- fighter_mapping$Name[i]
            matches <- which(unique_sim_fighters$Name == name)
            if(length(matches) > 0) {
              match_idx <- matches[1]
              fighter_mapping$Name[i] <- unique_sim_fighters$Name[match_idx]
              fighter_mapping$FDSalary[i] <- unique_sim_fighters$FDSalary[match_idx]
              fighter_mapping$FDOwn[i] <- unique_sim_fighters$FDOwn[match_idx]
            }
          }
        } else {
          fighter_mapping <- existing_mapping
        }
        
        rv$fd_fighter_exposure <- calculate_fd_fighter_exposure(
          rv$fd_optimal_lineups, 
          fighter_mapping, 
          rv$fd_random_lineups
        )
      }
      
      showModal(modalDialog(
        title = "Success",
        sprintf("Added %d lineups to build '%s'! Total portfolio: %d lineups across %d builds.", 
                nrow(sampled_lineups), build_label, nrow(rv$fd_random_lineups), 
                length(rv$fd_lineup_builds)),
        easyClose = TRUE
      ))
    })
  })
  
  # ===== FD CLEAR BUILDS HANDLER =====
  # Add this new handler
  
  observeEvent(input$clear_fd_builds, {
    rv$fd_lineup_builds <- list()
    rv$fd_random_lineups <- NULL
    
    # Recalculate exposure with just optimal lineups
    if(!is.null(rv$fd_optimal_lineups) && !is.null(rv$simulation_results)) {
      fighter_cols <- c("MVP", paste0("Fighter", 1:(FD_ROSTER_SIZE-1)))
      lineup_fighters <- c()
      for(col in fighter_cols) {
        if(col %in% names(rv$fd_optimal_lineups)) {
          lineup_fighters <- c(lineup_fighters, rv$fd_optimal_lineups[[col]])
        }
      }
      lineup_fighters <- unique(lineup_fighters)
      
      fighter_mapping <- data.frame(
        Name = lineup_fighters,  
        FDSalary = NA_real_,
        FDOwn = NA_real_
      )
      
      unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                   c("Name", "FDSalary", "FDOwn")]
      
      for(i in 1:nrow(fighter_mapping)) {
        name <- fighter_mapping$Name[i]
        matches <- which(unique_sim_fighters$Name == name)
        if(length(matches) > 0) {
          match_idx <- matches[1]
          fighter_mapping$Name[i] <- unique_sim_fighters$Name[match_idx]
          fighter_mapping$FDSalary[i] <- unique_sim_fighters$FDSalary[match_idx]
          fighter_mapping$FDOwn[i] <- unique_sim_fighters$FDOwn[match_idx]
        }
      }
      
      rv$fd_fighter_exposure <- calculate_fd_fighter_exposure(
        rv$fd_optimal_lineups, 
        fighter_mapping
      )
      
      if(!is.null(rv$fd_fighter_exposure)) {
        rv$fd_fighter_exposure$FilteredPoolRate <- rv$fd_fighter_exposure$OptimalRate
      }
    }
    
    showModal(modalDialog(
      title = "Portfolio Cleared",
      "All FanDuel builds have been cleared.",
      easyClose = TRUE
    ))
  })
  
  # ===== FD NEW TABLE OUTPUTS =====
  
  # FD Filtered Pool Statistics Table
  output$fd_filtered_pool_stats_table <- renderDT({
    req(rv$simulation_complete)
    
    filtered_pool <- fd_filtered_optimal_lineups()
    
    if(is.null(filtered_pool) || nrow(filtered_pool) == 0) {
      return(datatable(data.frame(Message = "No lineups in filtered pool"),
                       options = list(dom = 't'), rownames = FALSE))
    }
    
    tryCatch({
      fighter_cols <- c("MVP", paste0("Fighter", 1:(FD_ROSTER_SIZE-1)))
      
      all_fighters <- c()
      for(col in fighter_cols) {
        if(col %in% names(filtered_pool)) {
          all_fighters <- c(all_fighters, filtered_pool[[col]])
        }
      }
      
      fighter_counts <- as.data.frame(table(all_fighters))
      names(fighter_counts) <- c("Name", "Appearances")
      
      fighter_counts$FilteredPoolRate <- round(
        (fighter_counts$Appearances / nrow(filtered_pool)) * 100, 2
      )
      
      unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                   c("Name", "FDSalary", "FDOwn")]
      
      fighter_stats <- merge(fighter_counts, unique_sim_fighters, by = "Name", all.x = TRUE)
      
      if(!is.null(rv$fd_fighter_exposure)) {
        optimal_rates <- rv$fd_fighter_exposure[, c("Name", "OptimalRate")]
        fighter_stats <- merge(fighter_stats, optimal_rates, by = "Name", all.x = TRUE)
      }
      
      fighter_stats <- fighter_stats[order(-fighter_stats$FilteredPoolRate), ]
      
      dt <- datatable(
        fighter_stats,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = 'tip',
          order = list(list(2, 'desc')),
          columnDefs = list(
            list(targets = "_all", className = 'dt-center')
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      )
      
      if("FDSalary" %in% names(fighter_stats)) {
        dt <- dt %>% formatCurrency('FDSalary', currency = "$", interval = 3, 
                                    mark = ",", digits = 0)
      }
      if("FDOwn" %in% names(fighter_stats)) {
        dt <- dt %>% formatPercentage('FDOwn', digits = 1)
      }
      dt <- dt %>% formatRound(c('FilteredPoolRate', 'OptimalRate'), digits = 2)
      
      return(dt)
      
    }, error = function(e) {
      return(datatable(data.frame(Error = paste("Error:", e$message)),
                       options = list(dom = 't'), rownames = FALSE))
    })
  })
  
  # FD Builds Summary Table
  output$fd_builds_summary_table <- renderDT({
    req(length(rv$fd_lineup_builds) > 0)
    
    build_summary <- data.frame(
      BuildLabel = character(),
      NumLineups = integer(),
      LockedFighters = character(),
      ExcludedFighters = character(),
      MinTop1 = integer(),
      AvgTop1Count = numeric(),
      Timestamp = character(),
      stringsAsFactors = FALSE
    )
    
    for(i in 1:length(rv$fd_lineup_builds)) {
      build <- rv$fd_lineup_builds[[i]]
      
      locked_str <- if(length(build$filters$locked) > 0) {
        paste(build$filters$locked, collapse = ", ")
      } else {
        "None"
      }
      
      excluded_str <- if(length(build$filters$excluded) > 0) {
        paste(build$filters$excluded, collapse = ", ")
      } else {
        "None"
      }
      
      build_summary <- rbind(build_summary, data.frame(
        BuildLabel = build$label,
        NumLineups = nrow(build$lineups),
        LockedFighters = locked_str,
        ExcludedFighters = excluded_str,
        MinTop1 = build$filters$min_top1,
        AvgTop1Count = round(mean(build$lineups$Top1Count, na.rm = TRUE), 1),
        Timestamp = format(build$timestamp, "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      ))
    }
    
    totals_row <- data.frame(
      BuildLabel = "TOTAL PORTFOLIO",
      NumLineups = sum(build_summary$NumLineups),
      LockedFighters = "",
      ExcludedFighters = "",
      MinTop1 = NA,
      AvgTop1Count = round(mean(rv$fd_random_lineups$Top1Count, na.rm = TRUE), 1),
      Timestamp = "",
      stringsAsFactors = FALSE
    )
    
    build_summary <- rbind(build_summary, totals_row)
    
    dt <- datatable(
      build_summary,
      options = list(
        scrollX = TRUE,
        pageLength = -1,
        dom = 't',
        columnDefs = list(
          list(targets = "_all", className = 'dt-center')
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'BuildLabel',
        target = 'row',
        fontWeight = styleEqual('TOTAL PORTFOLIO', 'bold'),
        backgroundColor = styleEqual('TOTAL PORTFOLIO', '#ffffcc')
      )
    
    return(dt)
  })
  
  # DK filter change observer for real-time updates
  observeEvent(c(input$dk_min_top1_count, input$dk_min_top2_count, input$dk_min_top3_count, 
                 input$dk_min_top5_count, input$dk_cumulative_ownership_range, 
                 input$dk_geometric_mean_range, input$dk_excluded_fighters), {
                   if(!is.null(rv$dk_optimal_lineups) && !is.null(rv$dk_fighter_exposure)) {
                     
                     filters <- list(
                       min_top1_count = if(!is.null(input$dk_min_top1_count)) input$dk_min_top1_count else 0,
                       min_top2_count = if(!is.null(input$dk_min_top2_count)) input$dk_min_top2_count else 0,
                       min_top3_count = if(!is.null(input$dk_min_top3_count)) input$dk_min_top3_count else 0,
                       min_top5_count = if(!is.null(input$dk_min_top5_count)) input$dk_min_top5_count else 0,
                       cumulative_ownership_range = if(!is.null(input$dk_cumulative_ownership_range)) {
                         input$dk_cumulative_ownership_range
                       } else {
                         # Default to actual data range
                         if("CumulativeOwnership" %in% names(rv$dk_optimal_lineups)) {
                           cum_vals <- rv$dk_optimal_lineups$CumulativeOwnership[!is.na(rv$dk_optimal_lineups$CumulativeOwnership)]
                           if(length(cum_vals) > 0) c(min(cum_vals), max(cum_vals)) else c(0, 100)
                         } else c(0, 100)
                       },
                       geometric_mean_range = if(!is.null(input$dk_geometric_mean_range)) {
                         input$dk_geometric_mean_range
                       } else {
                         # Default to actual data range
                         if("GeometricMeanOwnership" %in% names(rv$dk_optimal_lineups)) {
                           geom_vals <- rv$dk_optimal_lineups$GeometricMeanOwnership[!is.na(rv$dk_optimal_lineups$GeometricMeanOwnership)]
                           if(length(geom_vals) > 0) c(min(geom_vals), max(geom_vals)) else c(0, 100)
                         } else c(0, 100)
                       },
                       excluded_fighters = if(!is.null(input$dk_excluded_fighters)) input$dk_excluded_fighters else character(0)
                     )
                     
                     # Calculate filtered pool rates using the new function
                     filtered_rates <- calculate_filtered_pool_rates(rv$dk_optimal_lineups, filters, "dk")
                     
                     # Update the FilteredPoolRate column in fighter exposure
                     if(!is.null(filtered_rates) && nrow(filtered_rates) > 0) {
                       # Create a copy of the fighter exposure data to modify
                       updated_exposure <- rv$dk_fighter_exposure
                       
                       # Update FilteredPoolRate for each fighter
                       for(i in 1:nrow(updated_exposure)) {
                         fighter_name <- updated_exposure$Name[i]
                         rate_match <- filtered_rates[filtered_rates$Name == fighter_name, ]
                         if(nrow(rate_match) > 0) {
                           updated_exposure$FilteredPoolRate[i] <- rate_match$FilteredPoolRate[1]
                         } else {
                           updated_exposure$FilteredPoolRate[i] <- 0
                         }
                       }
                       
                       # Update the reactive value
                       rv$dk_fighter_exposure <- updated_exposure
                     }
                   }
                 }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # FD filter change observer for real-time updates
  observeEvent(c(input$fd_min_top1_count, input$fd_min_top2_count, input$fd_min_top3_count, 
                 input$fd_min_top5_count, input$fd_excluded_fighters), {
                   if(!is.null(rv$fd_optimal_lineups) && !is.null(rv$fd_fighter_exposure)) {
                     filters <- list(
                       min_top1_count = if(!is.null(input$fd_min_top1_count)) input$fd_min_top1_count else 0,
                       min_top2_count = if(!is.null(input$fd_min_top2_count)) input$fd_min_top2_count else 0,
                       min_top3_count = if(!is.null(input$fd_min_top3_count)) input$fd_min_top3_count else 0,
                       min_top5_count = if(!is.null(input$fd_min_top5_count)) input$fd_min_top5_count else 0,
                       excluded_fighters = if(!is.null(input$fd_excluded_fighters)) input$fd_excluded_fighters else character(0)
                     )
                     
                     # Calculate filtered pool rates
                     filtered_rates <- calculate_filtered_pool_rates(rv$fd_optimal_lineups, filters, "fd")
                     
                     # Update the FilteredPoolRate column in fighter exposure
                     if(nrow(filtered_rates) > 0) {
                       for(i in 1:nrow(rv$fd_fighter_exposure)) {
                         fighter_name <- rv$fd_fighter_exposure$Name[i]
                         rate_match <- filtered_rates[filtered_rates$Name == fighter_name, ]
                         if(nrow(rate_match) > 0) {
                           rv$fd_fighter_exposure$FilteredPoolRate[i] <- rate_match$FilteredPoolRate[1]
                         } else {
                           rv$fd_fighter_exposure$FilteredPoolRate[i] <- 0
                         }
                       }
                     }
                   }
                 })
  
  
  
  output$fd_random_lineups_table <- renderDT({
    req(rv$fd_random_lineups)
    
    # Clone the random lineups for display
    original_data <- as.data.frame(rv$fd_random_lineups)
    
    # Create a new data structure with exactly 1 MVP and 5 fighters
    new_display_data <- data.table()
    
    # Process each lineup
    for(i in 1:nrow(original_data)) {
      # Get the MVP (ensuring there is one)
      mvp_name <- NA
      if("MVP" %in% names(original_data) && !is.na(original_data$MVP[i])) {
        mvp_name <- original_data$MVP[i]
      } else {
        # If no MVP is specified, use the highest scoring fighter as MVP
        # (this shouldn't happen with proper optimization, but as a fallback)
        if("Lineup" %in% names(original_data)) {
          all_fighters <- unlist(strsplit(original_data$Lineup[i], "\\|"))
          if(length(all_fighters) > 0) {
            mvp_name <- all_fighters[1]  # Use first fighter as MVP if none specified
          }
        } else {
          fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
          fighter_cols <- intersect(fighter_cols, names(original_data))
          if(length(fighter_cols) > 0) {
            fighters_in_row <- unlist(original_data[i, fighter_cols])
            fighters_in_row <- fighters_in_row[!is.na(fighters_in_row)]
            if(length(fighters_in_row) > 0) {
              mvp_name <- fighters_in_row[1]  # Use first non-NA fighter as MVP if none specified
            }
          }
        }
      }
      
      # Get all other fighters (excluding the MVP)
      fighter_list <- c()
      
      if("Lineup" %in% names(original_data)) {
        # If Lineup column exists, use it
        all_fighters <- unlist(strsplit(original_data$Lineup[i], "\\|"))
        fighter_list <- all_fighters[all_fighters != mvp_name]
      } else {
        # Otherwise use Fighter columns
        fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
        fighter_cols <- intersect(fighter_cols, names(original_data))
        if(length(fighter_cols) > 0) {
          all_fighters <- unlist(original_data[i, fighter_cols])
          all_fighters <- all_fighters[!is.na(all_fighters)]
          fighter_list <- all_fighters[all_fighters != mvp_name]
        }
      }
      
      # Ensure we have exactly 5 fighters plus the MVP
      if(length(fighter_list) > 5) {
        # If more than 5 non-MVP fighters, keep only the first 5
        fighter_list <- fighter_list[1:5]
      } else if(length(fighter_list) < 5) {
        # If fewer than 5, pad with placeholder values
        while(length(fighter_list) < 5) {
          fighter_list <- c(fighter_list, paste("Fighter", length(fighter_list) + 1))
        }
      }
      
      # Create a new row with MVP and exactly 5 fighters
      new_row <- data.table(
        MVP = mvp_name
      )
      
      # Add exactly 5 fighter columns
      for(j in 1:5) {
        new_row[[paste0("Fighter", j)]] <- fighter_list[j]
      }
      
      # Copy over count columns and TotalSalary
      count_cols <- grep("^Top[0-9]+Count$", names(original_data), value = TRUE)
      for(col in c(count_cols, "TotalSalary")) {
        if(col %in% names(original_data)) {
          new_row[[col]] <- original_data[[col]][i]
        }
      }
      
      # Add to the new display data
      new_display_data <- rbind(new_display_data, new_row)
    }
    
    # Use the standardized version for display
    display_data <- new_display_data
    
    # Sort by Top1Count if available, otherwise use Top5Count
    if("Top1Count" %in% names(display_data) && "Top5Count" %in% names(display_data)) {
      setorder(display_data, -Top1Count, -Top5Count)
    } else if("Top1Count" %in% names(display_data)) {
      setorder(display_data, -Top1Count)
    } else if("Top5Count" %in% names(display_data)) {
      setorder(display_data, -Top5Count)
    }
    
    # Create the datatable with pagination
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        rownames = FALSE,
        dom = "ftp",          # Only show table and pagination
        ordering = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Apply formatting to TotalSalary column
    if("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    # Apply formatting to count columns
    count_cols <- grep("^Top[0-9]+Count$", names(display_data), value = TRUE)
    for(col in count_cols) {
      if(any(!is.na(display_data[[col]]))) {
        max_count <- max(display_data[[col]], na.rm = TRUE)
        if(is.finite(max_count) && max_count > 0) {
          dt <- dt %>% formatStyle(
            col,
            background = styleColorBar(c(0, max_count), 'lightblue'),
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
        }
      }
    }
    
    # Highlight MVP column
    if("MVP" %in% names(display_data)) {
      dt <- dt %>% formatStyle(
        'MVP',
        backgroundColor = '#FFD700',  # Gold background
        fontWeight = 'bold',          # Bold text
        color = '#000000'             # Black text for better contrast
      )
    }
    
    dt
  })
  
  output$sd_fighter_exposure_table <- renderDT({
    req(rv$sd_random_lineups)
    req(length(rv$sd_lineup_builds) > 0)
    
    tryCatch({
      # Combine all builds
      all_lineups <- rv$sd_random_lineups
      total_lineups <- nrow(all_lineups)
      
      # Get all unique fighters from portfolio
      # IMPORTANT: Uses Captain + Fighter1-5 (NOT Player1-5!)
      all_fighters <- c()
      
      if("Captain" %in% names(all_lineups)) {
        all_fighters <- c(all_fighters, all_lineups$Captain)
      }
      
      for(i in 1:5) {
        col <- paste0("Fighter", i)
        if(col %in% names(all_lineups)) {
          all_fighters <- c(all_fighters, all_lineups[[col]])
        }
      }
      
      all_fighters <- unique(all_fighters)
      
      # Create exposure stats
      display_data <- data.frame(
        Name = all_fighters,
        CPT_Exposure = 0,
        Flex_Exposure = 0,
        stringsAsFactors = FALSE
      )
      
      # Calculate captain and flex exposure for each fighter
      for(i in 1:nrow(display_data)) {
        fighter <- display_data$Name[i]
        
        # Captain exposure
        if("Captain" %in% names(all_lineups)) {
          cpt_count <- sum(all_lineups$Captain == fighter, na.rm = TRUE)
          display_data$CPT_Exposure[i] <- round((cpt_count / total_lineups) * 100, 1)
        }
        
        # Flex exposure (Fighter1-5)
        flex_count <- 0
        for(j in 1:5) {
          col <- paste0("Fighter", j)
          if(col %in% names(all_lineups)) {
            flex_count <- flex_count + sum(all_lineups[[col]] == fighter, na.rm = TRUE)
          }
        }
        display_data$Flex_Exposure[i] <- round((flex_count / total_lineups) * 100, 1)
      }
      
      # Total exposure
      display_data$Total_Exposure <- display_data$CPT_Exposure + display_data$Flex_Exposure
      
      # Get fighter info from simulation results
      unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                   c("Name", "SDSal")]
      
      # Merge with fighter info
      display_data <- merge(display_data, unique_sim_fighters, by = "Name", all.x = TRUE)
      
      # Showdown doesn't have ownership, hardcode to 0
      display_data$Own <- 0
      
      # Calculate leverage for each category
      display_data$CPT_Leverage <- round(display_data$CPT_Exposure - display_data$Own, 1)
      display_data$Flex_Leverage <- round(display_data$Flex_Exposure - display_data$Own, 1)
      display_data$Total_Leverage <- round(display_data$Total_Exposure - display_data$Own, 1)
      
      # Filter to only fighters with exposure > 0 and select columns
      display_data <- display_data[display_data$Total_Exposure > 0, ]
      display_data <- display_data[, c("Name", "SDSal", 
                                       "CPT_Exposure", "Flex_Exposure", "Total_Exposure", "Own",
                                       "CPT_Leverage", "Flex_Leverage", "Total_Leverage")]
      display_data <- display_data[order(-display_data$Total_Exposure), ]
      
      # Create caption
      caption_text <- sprintf("Portfolio Exposure (%s lineups across %s builds)", 
                              format(total_lineups, big.mark = ","),
                              length(rv$sd_lineup_builds))
      
      # Create datatable
      dt <- datatable(
        display_data,
        caption = caption_text,
        options = list(
          pageLength = 25,
          dom = "ftp",
          scrollX = TRUE,
          order = list(list(4, 'desc')),  # Sort by Total_Exposure
          columnDefs = list(
            list(targets = "_all", className = 'dt-center')
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe compact'
      )
      
      # Format columns
      if("SDSal" %in% names(display_data)) {
        dt <- dt %>% formatCurrency('SDSal', currency = "$", interval = 3, 
                                    mark = ",", digits = 0)
      }
      
      # Format numeric columns
      numeric_cols <- intersect(
        c('CPT_Exposure', 'Flex_Exposure', 'Total_Exposure', 'Own',
          'CPT_Leverage', 'Flex_Leverage', 'Total_Leverage'), 
        names(display_data)
      )
      if(length(numeric_cols) > 0) {
        dt <- dt %>% formatRound(numeric_cols, digits = 1)
      }
      
      # Add gold color bar to Total_Exposure
      dt <- dt %>% formatStyle(
        'Total_Exposure',
        background = styleColorBar(range(display_data$Total_Exposure, na.rm = TRUE), '#FFD700'),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
      
      return(dt)
      
    }, error = function(e) {
      return(datatable(data.frame(Error = paste("Error:", e$message)),
                       options = list(dom = 't'), rownames = FALSE))
    })
  })
  
  
  
  output$dk_fighter_exposure_table <- renderDT({
    req(rv$dk_random_lineups)
    req(length(rv$dk_lineup_builds) > 0)
    
    tryCatch({
      # Combine all builds
      all_lineups <- rv$dk_random_lineups
      total_lineups <- nrow(all_lineups)
      
      # Get all unique fighters from portfolio
      fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
      
      # Extract all fighters safely
      all_fighters <- c()
      for(col in fighter_cols) {
        if(col %in% names(all_lineups)) {
          all_fighters <- c(all_fighters, all_lineups[[col]])
        }
      }
      all_fighters <- unique(all_fighters)
      
      # Create exposure stats
      display_data <- data.frame(
        Name = all_fighters,
        Exposure = 0,
        stringsAsFactors = FALSE
      )
      
      # Calculate exposure for each fighter
      for(i in 1:nrow(display_data)) {
        fighter <- display_data$Name[i]
        
        # Count appearances across all fighter columns
        count <- 0
        for(col in fighter_cols) {
          if(col %in% names(all_lineups)) {
            count <- count + sum(all_lineups[[col]] == fighter, na.rm = TRUE)
          }
        }
        display_data$Exposure[i] <- round((count / total_lineups) * 100, 1)
      }
      
      # Get fighter info from simulation results (salary and ownership)
      unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                   c("Name", "DKSalary", "DKOwn")]
      
      # Merge with fighter info
      display_data <- merge(display_data, unique_sim_fighters, by = "Name", all.x = TRUE)
      
      # Convert DKOwn to percentage
      display_data$Own <- display_data$DKOwn * 100
      
      # Calculate Leverage
      display_data$Leverage <- round(display_data$Exposure - display_data$Own, 1)
      
      # Filter to only fighters with exposure > 0 and select columns
      display_data <- display_data[display_data$Exposure > 0, ]
      display_data <- display_data[, c("Name", "DKSalary", "Exposure", "Own", "Leverage")]
      display_data <- display_data[order(-display_data$Exposure), ]
      
      # Create caption
      caption_text <- sprintf("Portfolio Exposure (%s lineups across %s builds)", 
                              format(total_lineups, big.mark = ","),
                              length(rv$dk_lineup_builds))
      
      # Create datatable
      dt <- datatable(
        display_data,
        caption = caption_text,
        options = list(
          pageLength = 25,
          dom = "ftp",
          scrollX = TRUE,
          order = list(list(2, 'desc')),  # Sort by Exposure
          columnDefs = list(
            list(targets = "_all", className = 'dt-center')
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe compact'
      )
      
      # Format columns
      if("DKSalary" %in% names(display_data)) {
        dt <- dt %>% formatCurrency('DKSalary', currency = "$", interval = 3, 
                                    mark = ",", digits = 0)
      }
      
      # Format numeric columns
      numeric_cols <- intersect(c('Exposure', 'Own', 'Leverage'), 
                                names(display_data))
      if(length(numeric_cols) > 0) {
        dt <- dt %>% formatRound(numeric_cols, digits = 1)
      }
      
      # Add gold color bar to Exposure
      dt <- dt %>% formatStyle(
        'Exposure',
        background = styleColorBar(range(display_data$Exposure, na.rm = TRUE), '#FFD700'),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
      
      return(dt)
      
    }, error = function(e) {
      return(datatable(data.frame(Error = paste("Error:", e$message)),
                       options = list(dom = 't'), rownames = FALSE))
    })
  })
  
  
  # ===== FD EXPOSURE TABLE =====
  # Replace output$fd_fighter_exposure_table
  
  output$fd_fighter_exposure_table <- renderDT({
    req(rv$fd_random_lineups)
    req(length(rv$fd_lineup_builds) > 0)
    
    tryCatch({
      # Combine all builds
      all_lineups <- rv$fd_random_lineups
      total_lineups <- nrow(all_lineups)
      
      # Get all unique fighters from portfolio (MVP + Fighter1-5)
      fighter_cols <- c("MVP", paste0("Fighter", 1:(FD_ROSTER_SIZE-1)))
      all_fighters <- unique(unlist(all_lineups[, fighter_cols]))
      
      # Create exposure stats
      display_data <- data.frame(
        Name = all_fighters,
        Exposure = 0,
        stringsAsFactors = FALSE
      )
      
      # Calculate exposure for each fighter
      for(i in 1:nrow(display_data)) {
        fighter <- display_data$Name[i]
        
        # Count appearances across all fighter columns
        count <- 0
        for(col in fighter_cols) {
          if(col %in% names(all_lineups)) {
            count <- count + sum(all_lineups[[col]] == fighter, na.rm = TRUE)
          }
        }
        display_data$Exposure[i] <- round((count / total_lineups) * 100, 1)
      }
      
      # Get fighter info from simulation results (salary and ownership)
      unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                   c("Name", "FDSalary", "FDOwn")]
      
      # Merge with fighter info
      display_data <- merge(display_data, unique_sim_fighters, by = "Name", all.x = TRUE)
      
      # Convert FDOwn to percentage
      display_data$Own <- display_data$FDOwn * 100
      
      # Calculate Leverage
      display_data$Leverage <- round(display_data$Exposure - display_data$Own, 1)
      
      # Filter to only fighters with exposure > 0
      display_data <- display_data %>%
        filter(Exposure > 0) %>%
        select(Name, FDSalary, Exposure, Own, Leverage) %>%
        arrange(desc(Exposure))
      
      # Create caption
      caption_text <- sprintf("Portfolio Exposure (%s lineups across %s builds)", 
                              format(total_lineups, big.mark = ","),
                              length(rv$fd_lineup_builds))
      
      # Create datatable
      dt <- datatable(
        display_data,
        caption = caption_text,
        options = list(
          pageLength = 25,
          dom = "ftp",
          scrollX = TRUE,
          order = list(list(2, 'desc')),  # Sort by Exposure
          columnDefs = list(
            list(targets = "_all", className = 'dt-center')
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe compact'
      )
      
      # Format columns
      if("FDSalary" %in% names(display_data)) {
        dt <- dt %>% formatCurrency('FDSalary', currency = "$", interval = 3, 
                                    mark = ",", digits = 0)
      }
      
      # Format numeric columns
      numeric_cols <- intersect(c('Exposure', 'Own', 'Leverage'), 
                                names(display_data))
      if(length(numeric_cols) > 0) {
        dt <- dt %>% formatRound(numeric_cols, digits = 1)
      }
      
      # Add gold color bar to Exposure
      dt <- dt %>% formatStyle(
        'Exposure',
        background = styleColorBar(range(display_data$Exposure, na.rm = TRUE), '#FFD700'),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
      
      return(dt)
      
    }, error = function(e) {
      return(datatable(data.frame(Error = paste("Error:", e$message)),
                       options = list(dom = 't'), rownames = FALSE))
    })
  })
  
  
  output$dk_random_lineups_table <- renderDT({
    req(rv$dk_random_lineups)
    
    # Convert to data.frame if it's not already
    display_data <- as.data.frame(rv$dk_random_lineups)
    
    # Format fighter columns to show names if we have fighter exposure data
    if(!is.null(rv$dk_fighter_exposure)) {
      for(i in 1:DK_ROSTER_SIZE) {
        col <- paste0("Fighter", i)
        if(col %in% names(display_data)) {
          display_data[[col]] <- sapply(display_data[[col]], function(id) {
            match_idx <- which(rv$dk_fighter_exposure$Name == id)
            if(length(match_idx) > 0) {
              rv$dk_fighter_exposure$Name[match_idx[1]]
            } else {
              id
            }
          })
        }
      }
    }
    
    # Create datatable with styling
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        rownames = FALSE,
        dom = "tp",
        ordering = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Format TotalSalary
    if("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    # Format count columns
    count_cols <- c("Top1Count", "Top2Count", "Top3Count", "Top5Count")
    count_cols <- intersect(count_cols, names(display_data))
    for(col in count_cols) {
      if(any(!is.na(display_data[[col]]))) {
        dt <- dt %>% formatStyle(
          col,
          background = styleColorBar(c(0, max(display_data[[col]], na.rm = TRUE)), 'lightblue'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      }
    }
    
    dt
  })
  
  output$fd_random_lineups_table <- renderDT({
    req(rv$fd_random_lineups)
    
    # Clone the random lineups for display
    original_data <- as.data.frame(rv$fd_random_lineups)
    
    # Create a new data structure with exactly 1 MVP and 5 fighters
    new_display_data <- data.table()
    
    # Process each lineup
    for(i in 1:nrow(original_data)) {
      # Get the MVP (ensuring there is one)
      mvp_name <- NA
      if("MVP" %in% names(original_data) && !is.na(original_data$MVP[i])) {
        mvp_name <- original_data$MVP[i]
      } else {
        # If no MVP is specified, use the highest scoring fighter as MVP
        # (this shouldn't happen with proper optimization, but as a fallback)
        if("Lineup" %in% names(original_data)) {
          all_fighters <- unlist(strsplit(original_data$Lineup[i], "\\|"))
          if(length(all_fighters) > 0) {
            mvp_name <- all_fighters[1]  # Use first fighter as MVP if none specified
          }
        } else {
          fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
          fighter_cols <- intersect(fighter_cols, names(original_data))
          if(length(fighter_cols) > 0) {
            fighters_in_row <- unlist(original_data[i, fighter_cols])
            fighters_in_row <- fighters_in_row[!is.na(fighters_in_row)]
            if(length(fighters_in_row) > 0) {
              mvp_name <- fighters_in_row[1]  # Use first non-NA fighter as MVP if none specified
            }
          }
        }
      }
      
      # Get all other fighters (excluding the MVP)
      fighter_list <- c()
      
      if("Lineup" %in% names(original_data)) {
        # If Lineup column exists, use it
        all_fighters <- unlist(strsplit(original_data$Lineup[i], "\\|"))
        fighter_list <- all_fighters[all_fighters != mvp_name]
      } else {
        # Otherwise use Fighter columns
        fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
        fighter_cols <- intersect(fighter_cols, names(original_data))
        if(length(fighter_cols) > 0) {
          all_fighters <- unlist(original_data[i, fighter_cols])
          all_fighters <- all_fighters[!is.na(all_fighters)]
          fighter_list <- all_fighters[all_fighters != mvp_name]
        }
      }
      
      # Ensure we have exactly 5 fighters plus the MVP
      if(length(fighter_list) > 5) {
        # If more than 5 non-MVP fighters, keep only the first 5
        fighter_list <- fighter_list[1:5]
      } else if(length(fighter_list) < 5) {
        # If fewer than 5, pad with placeholder values
        while(length(fighter_list) < 5) {
          fighter_list <- c(fighter_list, paste("Fighter", length(fighter_list) + 1))
        }
      }
      
      # Create a new row with MVP and exactly 5 fighters
      new_row <- data.table(
        MVP = mvp_name
      )
      
      # Add exactly 5 fighter columns
      for(j in 1:5) {
        new_row[[paste0("Fighter", j)]] <- fighter_list[j]
      }
      
      # Copy over count columns and TotalSalary
      count_cols <- grep("^Top[0-9]+Count$", names(original_data), value = TRUE)
      for(col in c(count_cols, "TotalSalary")) {
        if(col %in% names(original_data)) {
          new_row[[col]] <- original_data[[col]][i]
        }
      }
      
      # Add to the new display data
      new_display_data <- rbind(new_display_data, new_row)
    }
    
    # Use the standardized version for display
    display_data <- new_display_data
    
    # Sort by Top1Count if available, otherwise use Top5Count
    if("Top1Count" %in% names(display_data) && "Top5Count" %in% names(display_data)) {
      setorder(display_data, -Top1Count, -Top5Count)
    } else if("Top1Count" %in% names(display_data)) {
      setorder(display_data, -Top1Count)
    } else if("Top5Count" %in% names(display_data)) {
      setorder(display_data, -Top5Count)
    }
    
    # Create the datatable with pagination
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        rownames = FALSE,
        dom = "ftp",          # Only show table and pagination
        ordering = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Apply formatting to TotalSalary column
    if("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    # Apply formatting to count columns
    count_cols <- grep("^Top[0-9]+Count$", names(display_data), value = TRUE)
    for(col in count_cols) {
      if(any(!is.na(display_data[[col]]))) {
        max_count <- max(display_data[[col]], na.rm = TRUE)
        if(is.finite(max_count) && max_count > 0) {
          dt <- dt %>% formatStyle(
            col,
            background = styleColorBar(c(0, max_count), 'lightblue'),
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
        }
      }
    }
    
    # Highlight MVP column
    if("MVP" %in% names(display_data)) {
      dt <- dt %>% formatStyle(
        'MVP',
        backgroundColor = '#FFD700',  # Gold background
        fontWeight = 'bold',          # Bold text
        color = '#000000'             # Black text for better contrast
      )
    }
    
    dt
  })
  
  
  # Download handlers
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("mma_simulation_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(rv$simulation_results, file, row.names = FALSE)
    }
  )
  
  output$downloadAccuracy <- downloadHandler(
    filename = function() {
      paste("simulation_accuracy_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      accuracy_data <- analyze_simulation_accuracy(rv$simulation_results, rv$processed_data$scores)
      write.csv(accuracy_data$detailed, file, row.names = FALSE)
    }
  )
  
  output$download_dk_fantasy_projections <- downloadHandler(
    filename = function() {
      paste("dk_fantasy_projections_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(rv$dk_fantasy_analysis, file, row.names = FALSE)
    }
  )
  
  output$download_fd_fantasy_projections <- downloadHandler(
    filename = function() {
      paste("fd_fantasy_projections_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(rv$fd_fantasy_analysis, file, row.names = FALSE)
    }
  )
  
  output$download_dk_optimal_lineups <- downloadHandler(
    filename = function() {
      paste("dk_optimal_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      # Get the optimal lineups with fighter names
      display_data <- as.data.frame(rv$dk_optimal_lineups)
      
      # Create a name-to-DKID mapping from the simulation results
      name_to_id_map <- unique(rv$simulation_results[, c("Name", "DKID")])
      
      # Create a copy of the data for download that will use DKIDs
      download_data <- display_data
      
      # Replace fighter names with DKIDs in the download data
      for(i in 1:DK_ROSTER_SIZE) {
        col <- paste0("Fighter", i)
        if(col %in% names(download_data)) {
          download_data[[col]] <- sapply(download_data[[col]], function(name) {
            match_idx <- which(name_to_id_map$Name == name)
            if(length(match_idx) > 0) {
              name_to_id_map$DKID[match_idx[1]]
            } else {
              name  # Fallback to name if ID not found
            }
          })
        }
      }
      
      # Keep fighter columns, TopX Count columns, TotalSalary, and ownership metrics
      cols_to_keep <- c(paste0("Fighter", 1:DK_ROSTER_SIZE), 
                        grep("^Top[0-9]+Count$", names(download_data), value = TRUE),
                        "TotalSalary", "CumulativeOwnership", "GeometricMeanOwnership")
      cols_to_keep <- intersect(cols_to_keep, names(download_data))
      download_data <- download_data[, cols_to_keep, drop = FALSE]
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"  # Explicitly set MIME type for CSV
  )
  
  
  output$download_dk_random_lineups <- downloadHandler(
    filename = function() {
      paste0("MMA_DK_Portfolio_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(length(rv$dk_lineup_builds) > 0)
      
      # Combine all builds
      all_lineups <- do.call(rbind, lapply(rv$dk_lineup_builds, function(b) b$lineups))
      
      # Randomize the order
      all_lineups <- all_lineups[sample(nrow(all_lineups)), ]
      
      # Create a copy for downloading
      download_data <- as.data.frame(all_lineups)
      
      # Create a name-to-DKID mapping from the simulation results
      name_to_id_map <- unique(rv$simulation_results[, c("Name", "DKID")])
      
      # Replace fighter names with "Name (ID)" format
      for(i in 1:DK_ROSTER_SIZE) {
        col <- paste0("Fighter", i)
        if(col %in% names(download_data)) {
          download_data[[col]] <- sapply(download_data[[col]], function(name) {
            match_idx <- which(name_to_id_map$Name == name)
            if(length(match_idx) > 0) {
              paste0(name, " (", name_to_id_map$DKID[match_idx[1]], ")")
            } else {
              name  # Fallback to name if ID not found
            }
          })
        }
      }
      
      # Keep all columns including BuildLabel, LineupNum, and all metrics
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  

  
  output$download_fd_optimal_lineups <- downloadHandler(
    filename = function() {
      paste("fd_optimal_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      # Check if optimal lineups exist
      if(is.null(rv$fd_optimal_lineups) || nrow(rv$fd_optimal_lineups) == 0) {
        # Create an empty dataframe with appropriate columns if no lineups exist
        empty_data <- data.frame(
          MVP = character(0),
          Fighter1 = character(0),
          Fighter2 = character(0),
          Fighter3 = character(0), 
          Fighter4 = character(0),
          Fighter5 = character(0),
          Top1Count = integer(0),
          Top2Count = integer(0),
          Top3Count = integer(0),
          Top5Count = integer(0),
          TotalSalary = integer(0),
          stringsAsFactors = FALSE
        )
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      # Get the original lineup data
      original_data <- as.data.frame(rv$fd_optimal_lineups)
      
      # Create a name-to-FDID mapping
      name_to_id_map <- NULL
      if(!is.null(rv$simulation_results)) {
        # Create a direct mapping from fighter names to FDID
        name_map <- rv$simulation_results[!duplicated(rv$simulation_results$Name), c("Name", "FDID")]
        # Remove rows with NA in either column
        name_map <- name_map[!is.na(name_map$Name) & !is.na(name_map$FDID), ]
        if(nrow(name_map) > 0) {
          name_to_id_map <- setNames(name_map$FDID, name_map$Name)
        }
      }
      
      # Prepare final data with only required columns
      download_data <- data.frame(
        MVP = character(nrow(original_data)),
        Fighter1 = character(nrow(original_data)),
        Fighter2 = character(nrow(original_data)),
        Fighter3 = character(nrow(original_data)),
        Fighter4 = character(nrow(original_data)),
        Fighter5 = character(nrow(original_data)),
        stringsAsFactors = FALSE
      )
      
      # Add the count and salary columns if they exist
      for(col in c("Top1Count", "Top2Count", "Top3Count", "Top5Count", "TotalSalary")) {
        if(col %in% names(original_data)) {
          download_data[[col]] <- original_data[[col]]
        } else {
          download_data[[col]] <- NA
        }
      }
      
      # Process each row
      for(i in 1:nrow(original_data)) {
        # Get MVP
        mvp_name <- NA_character_
        if("MVP" %in% names(original_data)) {
          mvp_name <- as.character(original_data$MVP[i])
        }
        
        # Get all fighters for this lineup
        fighters_list <- c()
        for(j in 1:6) {
          col_name <- paste0("Fighter", j)
          if(col_name %in% names(original_data) && !is.na(original_data[[col_name]][i])) {
            fighter_name <- as.character(original_data[[col_name]][i])
            if(fighter_name != mvp_name) {
              fighters_list <- c(fighters_list, fighter_name)
            }
          }
        }
        
        # Ensure we have exactly 5 non-MVP fighters
        while(length(fighters_list) < 5) {
          fighters_list <- c(fighters_list, NA_character_)
        }
        if(length(fighters_list) > 5) {
          fighters_list <- fighters_list[1:5]
        }
        
        # Assign to download data
        download_data$MVP[i] <- mvp_name
        for(j in 1:5) {
          download_data[[paste0("Fighter", j)]][i] <- fighters_list[j]
        }
      }
      
      # Replace names with FDIDs if mapping is available
      if(!is.null(name_to_id_map)) {
        # Convert MVP column
        download_data$MVP <- sapply(download_data$MVP, function(name) {
          if(is.na(name)) return(NA_character_)
          if(name %in% names(name_to_id_map)) {
            return(name_to_id_map[name])
          } else {
            return(name)  # Fallback to name if ID not found
          }
        })
        
        # Convert Fighter columns
        for(i in 1:5) {
          col <- paste0("Fighter", i)
          download_data[[col]] <- sapply(download_data[[col]], function(name) {
            if(is.na(name)) return(NA_character_)
            if(name %in% names(name_to_id_map)) {
              return(name_to_id_map[name])
            } else {
              return(name)  # Fallback to name if ID not found
            }
          })
        }
      }
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"  # Explicitly set MIME type for CSV
  )
  
  
  output$download_fd_random_lineups <- downloadHandler(
    filename = function() {
      paste0("MMA_FD_Portfolio_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(length(rv$fd_lineup_builds) > 0)
      
      # Combine all builds
      all_lineups <- do.call(rbind, lapply(rv$fd_lineup_builds, function(b) b$lineups))
      
      # FanDuel doesn't use IDs, just keep names as-is
      write.csv(all_lineups, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # Memory cleanup functions
  observe({
    invalidateLater(180000) # 3 minutes
    gc(verbose = FALSE, full = TRUE)
  })
  
  output$contest_sim_filters_ui <- renderUI({
    req(input$contest_sim_platform)
    
    platform <- input$contest_sim_platform
    optimal_data <- if (platform == "DK") rv$dk_optimal_lineups else rv$fd_optimal_lineups
    
    if (is.null(optimal_data)) {
      return(p("No optimal lineups available. Generate optimal lineups first.",
               style = "color: #999; padding: 20px;"))
    }
    
    generate_contest_filter_ui(platform, optimal_data)
  })
  
  # Display filtered pool size
  output$contest_sim_filtered_pool_size <- renderText({
    req(input$contest_sim_platform)
    
    platform <- input$contest_sim_platform
    optimal_lineups <- if (platform == "DK") {
      req(rv$dk_optimal_lineups)
      rv$dk_optimal_lineups
    } else {
      req(rv$fd_optimal_lineups)
      rv$fd_optimal_lineups
    }
    
    top1 <- input$contest_sim_top1_count
    top2 <- input$contest_sim_top2_count
    top3 <- input$contest_sim_top3_count
    top5 <- input$contest_sim_top5_count
    ownership_range <- input$contest_sim_ownership_range
    geometric_range <- input$contest_sim_geometric_range
    excluded <- input$contest_sim_excluded_fighters
    
    req(ownership_range, geometric_range)
    
    filters <- list(
      min_top1_count = if(is.null(top1)) 0 else top1,
      min_top2_count = if(is.null(top2)) 0 else top2,
      min_top3_count = if(is.null(top3)) 0 else top3,
      min_top5_count = if(is.null(top5)) 0 else top5,
      min_cumulative_ownership = ownership_range[1],
      max_cumulative_ownership = ownership_range[2],
      min_geometric_mean = geometric_range[1],
      max_geometric_mean = geometric_range[2],
      excluded_fighters = excluded
    )
    
    filtered <- filter_contest_lineups(optimal_lineups, filters, platform)
    paste("Filtered Pool:", nrow(filtered), "lineups")
  })
  
  # Run contest simulator - NO SIMS INPUT, USES ALL AVAILABLE
  observeEvent(input$run_contest_sim, {
    req(rv$simulation_results)
    
    platform <- input$contest_sim_platform
    
    optimal_lineups <- if (platform == "DK") {
      req(rv$dk_optimal_lineups)
      rv$dk_optimal_lineups
    } else {
      req(rv$fd_optimal_lineups)
      rv$fd_optimal_lineups
    }
    
    top1 <- input$contest_sim_top1_count
    top2 <- input$contest_sim_top2_count
    top3 <- input$contest_sim_top3_count
    top5 <- input$contest_sim_top5_count
    ownership_range <- input$contest_sim_ownership_range
    geometric_range <- input$contest_sim_geometric_range
    excluded <- input$contest_sim_excluded_fighters
    
    req(ownership_range, geometric_range)
    
    filters <- list(
      min_top1_count = if(is.null(top1)) 0 else top1,
      min_top2_count = if(is.null(top2)) 0 else top2,
      min_top3_count = if(is.null(top3)) 0 else top3,
      min_top5_count = if(is.null(top5)) 0 else top5,
      min_cumulative_ownership = ownership_range[1],
      max_cumulative_ownership = ownership_range[2],
      min_geometric_mean = geometric_range[1],
      max_geometric_mean = geometric_range[2],
      excluded_fighters = excluded
    )
    
    showNotification("Starting contest simulator... Check R console for progress", 
                     type = "message", duration = 3)
    
    # Run simulator - NO n_sims parameter (uses all available)
    sim_results <- tryCatch({
      run_contest_simulator_optimized(
        optimal_lineups = optimal_lineups,
        simulation_results = rv$simulation_results,
        filters = filters,
        platform = platform,
        n_field = input$contest_chalk_field_size
      )
    }, error = function(e) {
      cat("\nERROR:", e$message, "\n")
      list(error = paste("Error:", e$message))
    })
    
    if (!is.null(sim_results$error)) {
      showNotification(sim_results$error, type = "error", duration = 10)
      return()
    }
    
    # Store results
    rv$contest_sim_results <- sim_results$results
    rv$contest_chalk_field <- generate_chalk_field(
      optimal_lineups, 
      input$contest_chalk_field_size, 
      platform
    )
    
    showNotification(
      paste("??? Complete!", sim_results$lineups_tested, "lineups tested using", 
            sim_results$n_sims, "simulations"),
      type = "message",
      duration = 5
    )
  })
  
  # Display summary
  output$contest_sim_summary <- renderUI({
    req(rv$contest_sim_results)
    
    results <- rv$contest_sim_results
    
    tagList(
      fluidRow(
        column(width = 4,
               div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; text-align: center;",
                   h4("Double-Up", style = "margin-top: 0;"),
                   h2(paste0(round(mean(results$DoubleUpRate), 1), "%"), 
                      style = "color: #FFD700; margin: 10px 0;"),
                   p(paste("Range:", round(min(results$DoubleUpRate), 1), "-", 
                           round(max(results$DoubleUpRate), 1), "%"), style = "margin: 0;"))),
        column(width = 4,
               div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; text-align: center;",
                   h4("5x Multiplier", style = "margin-top: 0;"),
                   h2(paste0(round(mean(results$FiveXRate), 1), "%"), 
                      style = "color: #FFD700; margin: 10px 0;"),
                   p(paste("Range:", round(min(results$FiveXRate), 1), "-", 
                           round(max(results$FiveXRate), 1), "%"), style = "margin: 0;"))),
        column(width = 4,
               div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; text-align: center;",
                   h4("10x Multiplier", style = "margin-top: 0;"),
                   h2(paste0(round(mean(results$TenXRate), 1), "%"), 
                      style = "color: #FFD700; margin: 10px 0;"),
                   p(paste("Range:", round(min(results$TenXRate), 1), "-", 
                           round(max(results$TenXRate), 1), "%"), style = "margin: 0;")))
      ),
      br(),
      p(class = "text-muted", align = "center",
        paste("Tested", nrow(results), "lineups against", 
              ifelse(!is.null(rv$contest_chalk_field), nrow(rv$contest_chalk_field), 0),
              "chalk opponents"))
    )
  })
  
  # Display results table
  output$contest_sim_results_table <- renderDT({
    req(rv$contest_sim_results)
    
    display_data <- copy(rv$contest_sim_results)
    platform <- input$contest_sim_platform
    roster_size <- if (platform == "DK") DK_ROSTER_SIZE else FD_ROSTER_SIZE
    
    cols_to_show <- c(
      if(platform == "FD" && "MVP" %in% names(display_data)) "MVP" else NULL,
      paste0("Fighter", 1:roster_size),
      "DoubleUpRate", "FiveXRate", "TenXRate",
      "TotalSalary", "CumulativeOwnership", "GeometricMeanOwnership"
    )
    cols_to_show <- intersect(cols_to_show, names(display_data))
    display_data <- display_data[, ..cols_to_show]
    
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "tip",
        ordering = TRUE,
        order = list(list(which(cols_to_show == "DoubleUpRate") - 1, 'desc')),
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    ) %>%
      formatRound(c("DoubleUpRate", "FiveXRate", "TenXRate"), digits = 1) %>%
      formatStyle(
        c("DoubleUpRate", "FiveXRate", "TenXRate"),
        background = styleColorBar(c(0, 100), '#FFD700'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    
    if ("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    if ("CumulativeOwnership" %in% names(display_data)) {
      dt <- dt %>% formatRound('CumulativeOwnership', digits = 1)
    }
    if ("GeometricMeanOwnership" %in% names(display_data)) {
      dt <- dt %>% formatRound('GeometricMeanOwnership', digits = 1)
    }
    
    dt
  })
  
  # Display chalk field
  output$contest_chalk_field_table <- renderDT({
    req(rv$contest_chalk_field)
    
    display_data <- copy(rv$contest_chalk_field)
    
    datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "tip",
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    ) %>%
      formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(c('CumulativeOwnership', 'GeometricMeanOwnership'), digits = 1)
  })
  
  # Download handler
  output$download_contest_sim_results <- downloadHandler(
    filename = function() {
      paste0("contest_sim_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(rv$contest_sim_results)
      write.csv(rv$contest_sim_results, file, row.names = FALSE)
    }
  )
  
  
  
  
  
  # ==================================================================
  # DK SHOWDOWN SERVER HANDLERS
  # ==================================================================
  
  # SD optimization button handler  
  observeEvent(input$run_sd_optimization, {
    req(rv$simulation_results, rv$has_draftkings)
    
    rv$sd_optimal_lineups <- NULL
    rv$sd_fighter_exposure <- NULL
    gc(verbose = FALSE, full = TRUE)
    
    withProgress(message = 'Calculating DK Showdown optimal lineups...', value = 0, {
      showModal(modalDialog(
        title = "Processing DK Showdown Optimal Lineups",
        "Finding optimal lineups using all simulations. This may take a few minutes.",
        footer = NULL,
        easyClose = FALSE
      ))
      
      setProgress(0.2, detail = "Finding optimal lineups...")
      rv$sd_optimal_lineups <- tryCatch({
        count_sd_optimal_lineups(rv$simulation_results)
      }, error = function(e) {
        message("Error finding SD optimal lineups: ", e$message)
        removeModal()
        showModal(modalDialog(
          title = "Error Finding Optimal Lineups",
          paste("There was an error:", e$message),
          easyClose = TRUE
        ))
        NULL
      })
      
      cat("\n=== SD OPTIMAL LINEUPS STORED ===\n")
      cat("rv$sd_optimal_lineups is NULL:", is.null(rv$sd_optimal_lineups), "\n")
      if(!is.null(rv$sd_optimal_lineups)) {
        cat("Number of rows:", nrow(rv$sd_optimal_lineups), "\n")
        cat("Number of cols:", ncol(rv$sd_optimal_lineups), "\n")
        cat("Column names:", paste(names(rv$sd_optimal_lineups), collapse=", "), "\n")
      }
      cat("=================================\n\n")
      
      gc(verbose = FALSE, full = TRUE)
      
      setProgress(0.7, detail = "Creating fighter mapping...")
      fighter_mapping <- NULL
      if(!is.null(rv$sd_optimal_lineups)) {
        lineup_fighters <- c(
          rv$sd_optimal_lineups$Captain,
          unlist(rv$sd_optimal_lineups[, paste0("Fighter", 1:5)])
        )
        lineup_fighters <- unique(lineup_fighters)
        
        fighter_mapping <- data.frame(
          Name = lineup_fighters,
          SDSal = NA_real_,
          Median_DKScore = NA_real_,
          stringsAsFactors = FALSE
        )
        
        unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                     c("Name", "SDSal")]
        
        if(!is.null(rv$dk_fantasy_analysis)) {
          median_scores <- rv$dk_fantasy_analysis[, c("Name", "Median_DKScore")]
        }
        
        for(i in 1:nrow(fighter_mapping)) {
          name <- fighter_mapping$Name[i]
          matches <- which(unique_sim_fighters$Name == name)
          if(length(matches) > 0) {
            fighter_mapping$SDSal[i] <- unique_sim_fighters$SDSal[matches[1]]
          }
          if(exists("median_scores")) {
            score_match <- which(median_scores$Name == name)
            if(length(score_match) > 0) {
              fighter_mapping$Median_DKScore[i] <- median_scores$Median_DKScore[score_match[1]]
            }
          }
        }
      }
      
      setProgress(0.8, detail = "Calculating fighter exposure...")
      if(!is.null(rv$sd_optimal_lineups)) {
        rv$sd_fighter_exposure <- calculate_sd_fighter_exposure(
          rv$sd_optimal_lineups,
          fighter_mapping
        )
      }
      
      removeModal()
      
      Sys.sleep(0.1)
      showModal(modalDialog(
        title = "Success",
        HTML(sprintf(
          "Successfully generated <b>%d</b> optimal lineups for DK Showdown!<br><br>
          You can now go to the <b>Lineup Builder</b> tab to view fighter exposures.",
          if(!is.null(rv$sd_optimal_lineups)) nrow(rv$sd_optimal_lineups) else 0
        )),
        easyClose = TRUE
      ))
    })
  })
  
  # SD optimal lineups table
  output$sd_optimal_lineups_table <- renderDT({
    req(rv$sd_optimal_lineups)
    
    display_data <- copy(as.data.table(rv$sd_optimal_lineups))
    
    display_cols <- c("Captain", paste0("Fighter", 1:5), 
                      "Top1Count", "Top2Count", "Top3Count", "Top5Count", 
                      "TotalSalary")
    display_cols <- intersect(display_cols, names(display_data))
    display_data <- display_data[, ..display_cols]
    
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "tip",
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        order = list(list(which(display_cols == "Top1Count") - 1, 'desc'))
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    ) %>%
      formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    
    for(col in c("Top1Count", "Top2Count", "Top3Count", "Top5Count")) {
      if(col %in% names(display_data)) {
        dt <- dt %>% formatStyle(
          col,
          background = styleColorBar(range(display_data[[col]], na.rm = TRUE), '#FFD700'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      }
    }
    
    dt
  })
  

  output$sd_filtered_pool_stats_table <- renderDT({
    req(rv$simulation_complete)
    
    filtered_pool <- as.data.frame(sd_filtered_optimal_lineups())
    
    if(is.null(filtered_pool) || nrow(filtered_pool) == 0) {
      return(datatable(data.frame(Message = "No lineups in filtered pool"),
                       options = list(dom = 't'), rownames = FALSE))
    }
    
    tryCatch({
      # Get all unique fighters from filtered pool
      # IMPORTANT: Showdown uses Captain + Fighter1-5 in YOUR data
      all_fighters <- c()
      
      # Add captains
      if("Captain" %in% names(filtered_pool)) {
        all_fighters <- c(all_fighters, filtered_pool$Captain)
      }
      
      # Add fighters 1-5 (NOT Player1-5!)
      for(i in 1:5) {
        col <- paste0("Fighter", i)
        if(col %in% names(filtered_pool)) {
          all_fighters <- c(all_fighters, filtered_pool[[col]])
        }
      }
      
      all_fighters <- unique(all_fighters)
      
      # Create stats dataframe
      fighter_stats <- data.frame(
        Name = all_fighters,
        CPT_Count = 0,
        Flex_Count = 0,
        stringsAsFactors = FALSE
      )
      
      # Count captain appearances
      for(i in 1:nrow(fighter_stats)) {
        fighter <- fighter_stats$Name[i]
        
        # Captain count
        if("Captain" %in% names(filtered_pool)) {
          fighter_stats$CPT_Count[i] <- sum(filtered_pool$Captain == fighter, na.rm = TRUE)
        }
        
        # Flex count (Fighter1-5)
        flex_count <- 0
        for(j in 1:5) {
          col <- paste0("Fighter", j)
          if(col %in% names(filtered_pool)) {
            flex_count <- flex_count + sum(filtered_pool[[col]] == fighter, na.rm = TRUE)
          }
        }
        fighter_stats$Flex_Count[i] <- flex_count
      }
      
      # Calculate rates
      fighter_stats$CPT_Rate <- round((fighter_stats$CPT_Count / nrow(filtered_pool)) * 100, 1)
      fighter_stats$Flex_Rate <- round((fighter_stats$Flex_Count / nrow(filtered_pool)) * 100, 1)
      fighter_stats$TotalRate <- fighter_stats$CPT_Rate + fighter_stats$Flex_Rate
      
      # Get fighter info from simulation results
      unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                   c("Name", "SDSal")]
      
      # Merge with fighter info
      fighter_stats <- merge(fighter_stats, unique_sim_fighters, by = "Name", all.x = TRUE)
      
      # Showdown doesn't have ownership, hardcode to 0
      fighter_stats$Own <- 0
      
      # Calculate leverage
      fighter_stats$CPT_Leverage <- round(fighter_stats$CPT_Rate - fighter_stats$Own, 1)
      fighter_stats$Flex_Leverage <- round(fighter_stats$Flex_Rate - fighter_stats$Own, 1)
      fighter_stats$Total_Leverage <- round(fighter_stats$TotalRate - fighter_stats$Own, 1)
      
      # Select and order columns
      fighter_stats <- fighter_stats[, c("Name", "SDSal", "CPT_Rate", "Flex_Rate", "TotalRate", 
                                         "Own", "CPT_Leverage", "Flex_Leverage", "Total_Leverage")]
      fighter_stats <- fighter_stats[order(-fighter_stats$TotalRate), ]
      
      # Create caption
      caption_text <- sprintf("Filtered Pool: %s lineups", 
                              format(nrow(filtered_pool), big.mark = ","))
      
      # Create display
      dt <- datatable(
        fighter_stats,
        caption = caption_text,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = 'ftp',
          order = list(list(4, 'desc')),  # Sort by TotalRate
          columnDefs = list(
            list(targets = "_all", className = 'dt-center')
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe compact'
      )
      
      # Format columns
      if("SDSal" %in% names(fighter_stats)) {
        dt <- dt %>% formatCurrency('SDSal', currency = "$", interval = 3, 
                                    mark = ",", digits = 0)
      }
      
      # Format numeric columns
      numeric_cols <- intersect(c('CPT_Rate', 'Flex_Rate', 'TotalRate', 'Own', 
                                  'CPT_Leverage', 'Flex_Leverage', 'Total_Leverage'), 
                                names(fighter_stats))
      if(length(numeric_cols) > 0) {
        dt <- dt %>% formatRound(numeric_cols, digits = 1)
      }
      
      # Add color bar to TotalRate
      dt <- dt %>% formatStyle(
        'TotalRate',
        background = styleColorBar(range(fighter_stats$TotalRate, na.rm = TRUE), '#32CD32'),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
      
      return(dt)
      
    }, error = function(e) {
      return(datatable(data.frame(Error = paste("Error:", e$message)),
                       options = list(dom = 't'), rownames = FALSE))
    })
  })  

  # SD download handler
  output$download_sd_optimal_lineups <- downloadHandler(
    filename = function() {
      paste("sd_optimal_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      req(rv$sd_optimal_lineups)
      
      display_data <- as.data.frame(rv$sd_optimal_lineups)
      
      name_to_cptid_map <- unique(rv$simulation_results[, c("Name", "CPTID")])
      name_to_sdid_map <- unique(rv$simulation_results[, c("Name", "SDID")])
      
      download_data <- display_data
      
      if("Captain" %in% names(download_data)) {
        download_data$Captain <- sapply(download_data$Captain, function(name) {
          match_idx <- which(name_to_cptid_map$Name == name)
          if(length(match_idx) > 0) {
            paste0(name, " (", name_to_cptid_map$CPTID[match_idx[1]], ")")
          } else {
            name
          }
        })
      }
      
      for(i in 1:5) {
        col <- paste0("Fighter", i)
        if(col %in% names(download_data)) {
          download_data[[col]] <- sapply(download_data[[col]], function(name) {
            match_idx <- which(name_to_sdid_map$Name == name)
            if(length(match_idx) > 0) {
              paste0(name, " (", name_to_sdid_map$SDID[match_idx[1]], ")")
            } else {
              name
            }
          })
        }
      }
      
      cols_to_keep <- c("Captain", paste0("Fighter", 1:5),
                        grep("^Top[0-9]+Count$", names(download_data), value = TRUE),
                        "TotalSalary")
      cols_to_keep <- intersect(cols_to_keep, names(download_data))
      download_data <- download_data[, cols_to_keep, drop = FALSE]
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # ==================================================================
  # DK SHOWDOWN LINEUP BUILDER HANDLERS
  # ==================================================================
  
  
  sd_filtered_optimal_lineups <- reactive({
    req(rv$sd_optimal_lineups)
    
    optimal <- rv$sd_optimal_lineups
    
    # Apply Top Count filters
    filtered <- optimal %>%
      filter(
        Top1Count >= input$sd_min_top1_count,
        Top2Count >= input$sd_min_top2_count,
        Top3Count >= input$sd_min_top3_count,
        Top5Count >= input$sd_min_top5_count
      )
    
    cat("After Top filters:", nrow(filtered), "lineups\n")
    
    # Apply CAPTAIN LOCK (single captain)
    if(!is.null(input$sd_locked_captain) && input$sd_locked_captain != "") {
      if("Captain" %in% names(filtered)) {
        before <- nrow(filtered)
        filtered <- filtered[filtered$Captain == input$sd_locked_captain, ]
        cat("After captain lock (", input$sd_locked_captain, "):", nrow(filtered), 
            "(removed", before - nrow(filtered), ")\n")
      }
    }
    
    # Apply CAPTAIN EXCLUDE (multiple captains)
    if(!is.null(input$sd_excluded_captain) && length(input$sd_excluded_captain) > 0) {
      if("Captain" %in% names(filtered)) {
        before <- nrow(filtered)
        for(excluded_captain in input$sd_excluded_captain) {
          filtered <- filtered[filtered$Captain != excluded_captain, ]
        }
        cat("After captain exclude:", nrow(filtered), "(removed", before - nrow(filtered), ")\n")
      }
    }
    
    # Apply FIGHTER LOCK (non-captain positions: Fighter1-5)
    if(!is.null(input$sd_locked_fighters) && length(input$sd_locked_fighters) > 0) {
      fighter_cols <- paste0("Fighter", 1:5)
      
      for(locked_fighter in input$sd_locked_fighters) {
        before <- nrow(filtered)
        # Keep only lineups that contain this fighter in Fighter1-5 positions
        has_fighter <- rep(FALSE, nrow(filtered))
        for(col in fighter_cols) {
          if(col %in% names(filtered)) {
            has_fighter <- has_fighter | (filtered[[col]] == locked_fighter)
          }
        }
        filtered <- filtered[has_fighter, ]
        cat("After fighter lock (", locked_fighter, "):", nrow(filtered), 
            "(removed", before - nrow(filtered), ")\n")
      }
    }
    
    # Apply FIGHTER EXCLUDE (non-captain positions: Fighter1-5)
    if(!is.null(input$sd_excluded_fighters) && length(input$sd_excluded_fighters) > 0) {
      fighter_cols <- paste0("Fighter", 1:5)
      
      before <- nrow(filtered)
      for(excluded_fighter in input$sd_excluded_fighters) {
        for(col in fighter_cols) {
          if(col %in% names(filtered)) {
            filtered <- filtered[filtered[[col]] != excluded_fighter, ]
          }
        }
      }
      cat("After fighter exclude:", nrow(filtered), "(removed", before - nrow(filtered), ")\n")
    }
    
    cat("Final filtered pool:", nrow(filtered), "lineups\n\n")
    
    return(filtered)
  })
  
  # SD filtered pool size display
  output$sd_filtered_pool_size <- renderText({
    filtered <- sd_filtered_optimal_lineups()
    
    if(is.null(filtered) || nrow(filtered) == 0) {
      return("No lineups match current filters")
    }
    
    paste0(
      nrow(filtered), " lineups in filtered pool ",
      "(", round(nrow(filtered) / nrow(rv$sd_optimal_lineups) * 100, 1), "% of total)"
    )
  })
  
  # SD filtered pool count (for title display)
  output$sd_filtered_pool_count <- renderText({
    filtered <- sd_filtered_optimal_lineups()
    if(is.null(filtered) || nrow(filtered) == 0) return("0")
    return(format(nrow(filtered), big.mark = ","))
  })
  

  
  observeEvent(input$generate_sd_lineups, {
    req(rv$sd_optimal_lineups)
    
    filtered_pool <- sd_filtered_optimal_lineups()
    
    if(is.null(filtered_pool) || nrow(filtered_pool) == 0) {
      showModal(modalDialog(
        title = "Error",
        "No lineups match the selected filters. Try adjusting your criteria.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Get number of lineups to generate
    num_lineups <- input$sd_num_random_lineups
    
    # Get build label (or generate default)
    build_label <- if(!is.null(input$sd_build_label) && nchar(trimws(input$sd_build_label)) > 0) {
      trimws(input$sd_build_label)
    } else {
      paste0("Build ", length(rv$sd_lineup_builds) + 1)
    }
    
    # Show progress
    withProgress(message = 'Generating lineups...', value = 0, {
      # Sample from filtered pool
      if(nrow(filtered_pool) <= num_lineups) {
        sampled_lineups <- filtered_pool
      } else {
        weights <- filtered_pool$Top1Count + 1
        sampled_indices <- sample(1:nrow(filtered_pool), size = num_lineups, 
                                  prob = weights, replace = FALSE)
        sampled_lineups <- filtered_pool[sampled_indices, ]
      }
      
      # Add build metadata
      sampled_lineups$BuildLabel <- build_label
      sampled_lineups$LineupNum <- 1:nrow(sampled_lineups)
      
      # Store this build
      build_data <- list(
        label = build_label,
        timestamp = Sys.time(),
        filters = list(
          min_top1 = input$sd_min_top1_count,
          min_top2 = input$sd_min_top2_count,
          min_top3 = input$sd_min_top3_count,
          min_top5 = input$sd_min_top5_count,
          locked = input$sd_locked_fighters,
          excluded = input$sd_excluded_fighters
        ),
        lineups = sampled_lineups
      )
      
      # Add to builds list
      rv$sd_lineup_builds[[length(rv$sd_lineup_builds) + 1]] <- build_data
      
      # Combine all builds for display
      all_lineups <- do.call(rbind, lapply(rv$sd_lineup_builds, function(b) b$lineups))
      rv$sd_random_lineups <- all_lineups
      
      # Update fighter exposure
      if(!is.null(rv$sd_random_lineups)) {
        existing_mapping <- NULL
        if(!is.null(rv$sd_fighter_exposure)) {
          existing_mapping <- rv$sd_fighter_exposure[, c("Name", "SDSal", "Proj")]
        }
        
        if(is.null(existing_mapping) || nrow(existing_mapping) == 0) {
          all_fighters <- unique(c(
            rv$sd_optimal_lineups$Captain,
            unlist(rv$sd_optimal_lineups[, paste0("Player", 1:5)])
          ))
          
          fighter_mapping <- data.frame(
            Name = all_fighters,
            SDSal = NA_real_,
            Proj = NA_real_,
            stringsAsFactors = FALSE
          )
          
          unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                       c("Name", "SDSal")]
          
          if(!is.null(rv$dk_fantasy_analysis)) {
            median_scores <- rv$dk_fantasy_analysis[, c("Name", "Median_DKScore")]
          }
          
          for(i in 1:nrow(fighter_mapping)) {
            name <- fighter_mapping$Name[i]
            matches <- which(unique_sim_fighters$Name == name)
            
            if(length(matches) > 0) {
              fighter_mapping$SDSal[i] <- unique_sim_fighters$SDSal[matches[1]]
            }
            
            if(!is.null(rv$dk_fantasy_analysis)) {
              score_match <- which(median_scores$Name == name)
              if(length(score_match) > 0) {
                fighter_mapping$Proj[i] <- median_scores$Median_DKScore[score_match[1]]
              }
            }
          }
        } else {
          fighter_mapping <- existing_mapping
        }
        
        rv$sd_fighter_exposure <- calculate_sd_fighter_exposure(
          rv$sd_optimal_lineups, 
          fighter_mapping, 
          rv$sd_random_lineups
        )
      }
      
      showModal(modalDialog(
        title = "Success",
        sprintf("Added %d lineups to build '%s'! Total portfolio: %d lineups across %d builds.", 
                nrow(sampled_lineups), build_label, nrow(rv$sd_random_lineups), 
                length(rv$sd_lineup_builds)),
        easyClose = TRUE
      ))
    })
  })
  
  # ===== SD CLEAR BUILDS HANDLER =====
  # Add this new handler
  
  observeEvent(input$clear_sd_builds, {
    rv$sd_lineup_builds <- list()
    rv$sd_random_lineups <- NULL
    
    # Recalculate exposure with just optimal lineups
    if(!is.null(rv$sd_optimal_lineups) && !is.null(rv$simulation_results)) {
      all_fighters <- unique(c(
        rv$sd_optimal_lineups$Captain,
        unlist(rv$sd_optimal_lineups[, paste0("Player", 1:5)])
      ))
      
      fighter_mapping <- data.frame(
        Name = all_fighters,
        SDSal = NA_real_,
        Proj = NA_real_,
        stringsAsFactors = FALSE
      )
      
      unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                   c("Name", "SDSal")]
      
      if(!is.null(rv$dk_fantasy_analysis)) {
        median_scores <- rv$dk_fantasy_analysis[, c("Name", "Median_DKScore")]
      }
      
      for(i in 1:nrow(fighter_mapping)) {
        name <- fighter_mapping$Name[i]
        matches <- which(unique_sim_fighters$Name == name)
        
        if(length(matches) > 0) {
          fighter_mapping$SDSal[i] <- unique_sim_fighters$SDSal[matches[1]]
        }
        
        if(!is.null(rv$dk_fantasy_analysis)) {
          score_match <- which(median_scores$Name == name)
          if(length(score_match) > 0) {
            fighter_mapping$Proj[i] <- median_scores$Median_DKScore[score_match[1]]
          }
        }
      }
      
      rv$sd_fighter_exposure <- calculate_sd_fighter_exposure(
        rv$sd_optimal_lineups, 
        fighter_mapping
      )
      
      if(!is.null(rv$sd_fighter_exposure)) {
        rv$sd_fighter_exposure$FilteredPoolRate <- rv$sd_fighter_exposure$OptimalRate
      }
    }
    
    showModal(modalDialog(
      title = "Portfolio Cleared",
      "All Showdown builds have been cleared.",
      easyClose = TRUE
    ))
  })
  
  # ===== SD NEW TABLE OUTPUTS =====
  
  output$sd_filtered_pool_stats_table <- renderDT({
    req(rv$simulation_complete)
    
    filtered_pool <- sd_filtered_optimal_lineups()
    
    if(is.null(filtered_pool) || nrow(filtered_pool) == 0) {
      return(datatable(data.frame(Message = "No lineups in filtered pool"),
                       options = list(dom = 't'), rownames = FALSE))
    }
    
    tryCatch({
      # Get all unique fighters from filtered pool
      # IMPORTANT: Showdown uses Captain + Player1-5, NOT Fighter columns
      all_fighters <- c()
      
      # Add captains
      if("Captain" %in% names(filtered_pool)) {
        all_fighters <- c(all_fighters, filtered_pool$Captain)
      }
      
      # Add players 1-5
      for(i in 1:5) {
        col <- paste0("Player", i)
        if(col %in% names(filtered_pool)) {
          all_fighters <- c(all_fighters, filtered_pool[[col]])
        }
      }
      
      all_fighters <- unique(all_fighters)
      
      # Create stats dataframe
      fighter_stats <- data.frame(
        Name = all_fighters,
        CPT_Count = 0,
        Flex_Count = 0,
        stringsAsFactors = FALSE
      )
      
      # Count captain appearances
      for(i in 1:nrow(fighter_stats)) {
        fighter <- fighter_stats$Name[i]
        
        # Captain count
        if("Captain" %in% names(filtered_pool)) {
          fighter_stats$CPT_Count[i] <- sum(filtered_pool$Captain == fighter, na.rm = TRUE)
        }
        
        # Flex count (Player1-5)
        flex_count <- 0
        for(j in 1:5) {
          col <- paste0("Player", j)
          if(col %in% names(filtered_pool)) {
            flex_count <- flex_count + sum(filtered_pool[[col]] == fighter, na.rm = TRUE)
          }
        }
        fighter_stats$Flex_Count[i] <- flex_count
      }
      
      # Calculate rates
      fighter_stats$CPT_Rate <- round((fighter_stats$CPT_Count / nrow(filtered_pool)) * 100, 1)
      fighter_stats$Flex_Rate <- round((fighter_stats$Flex_Count / nrow(filtered_pool)) * 100, 1)
      fighter_stats$TotalRate <- fighter_stats$CPT_Rate + fighter_stats$Flex_Rate
      
      # Get fighter info from simulation results
      unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                   c("Name", "SDSal")]
      
      # Merge with fighter info
      fighter_stats <- merge(fighter_stats, unique_sim_fighters, by = "Name", all.x = TRUE)
      
      # Showdown doesn't have ownership, hardcode to 0
      fighter_stats$Own <- 0
      
      # Calculate leverage
      fighter_stats$CPT_Leverage <- round(fighter_stats$CPT_Rate - fighter_stats$Own, 1)
      fighter_stats$Flex_Leverage <- round(fighter_stats$Flex_Rate - fighter_stats$Own, 1)
      fighter_stats$Total_Leverage <- round(fighter_stats$TotalRate - fighter_stats$Own, 1)
      
      # Select and order columns
      fighter_stats <- fighter_stats[, c("Name", "SDSal", "CPT_Rate", "Flex_Rate", "TotalRate", 
                                         "Own", "CPT_Leverage", "Flex_Leverage", "Total_Leverage")]
      fighter_stats <- fighter_stats[order(-fighter_stats$TotalRate), ]
      
      # Create caption
      caption_text <- sprintf("Filtered Pool: %s lineups", 
                              format(nrow(filtered_pool), big.mark = ","))
      
      # Create display
      dt <- datatable(
        fighter_stats,
        caption = caption_text,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = 'ftp',
          order = list(list(4, 'desc')),  # Sort by TotalRate
          columnDefs = list(
            list(targets = "_all", className = 'dt-center')
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe compact'
      )
      
      # Format columns
      if("SDSal" %in% names(fighter_stats)) {
        dt <- dt %>% formatCurrency('SDSal', currency = "$", interval = 3, 
                                    mark = ",", digits = 0)
      }
      
      # Format numeric columns
      numeric_cols <- intersect(c('CPT_Rate', 'Flex_Rate', 'TotalRate', 'Own', 
                                  'CPT_Leverage', 'Flex_Leverage', 'Total_Leverage'), 
                                names(fighter_stats))
      if(length(numeric_cols) > 0) {
        dt <- dt %>% formatRound(numeric_cols, digits = 1)
      }
      
      # Add color bar to TotalRate
      dt <- dt %>% formatStyle(
        'TotalRate',
        background = styleColorBar(range(fighter_stats$TotalRate, na.rm = TRUE), '#32CD32'),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
      
      return(dt)
      
    }, error = function(e) {
      return(datatable(data.frame(Error = paste("Error:", e$message)),
                       options = list(dom = 't'), rownames = FALSE))
    })
  })
  
  
  # SD Builds Summary Table
  output$sd_builds_summary_table <- renderDT({
    req(length(rv$sd_lineup_builds) > 0)
    
    build_summary <- data.frame(
      BuildLabel = character(),
      NumLineups = integer(),
      LockedFighters = character(),
      ExcludedFighters = character(),
      MinTop1 = integer(),
      AvgTop1Count = numeric(),
      Timestamp = character(),
      stringsAsFactors = FALSE
    )
    
    for(i in 1:length(rv$sd_lineup_builds)) {
      build <- rv$sd_lineup_builds[[i]]
      
      locked_str <- if(length(build$filters$locked) > 0) {
        paste(build$filters$locked, collapse = ", ")
      } else {
        "None"
      }
      
      excluded_str <- if(length(build$filters$excluded) > 0) {
        paste(build$filters$excluded, collapse = ", ")
      } else {
        "None"
      }
      
      build_summary <- rbind(build_summary, data.frame(
        BuildLabel = build$label,
        NumLineups = nrow(build$lineups),
        LockedFighters = locked_str,
        ExcludedFighters = excluded_str,
        MinTop1 = build$filters$min_top1,
        AvgTop1Count = round(mean(build$lineups$Top1Count, na.rm = TRUE), 1),
        Timestamp = format(build$timestamp, "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      ))
    }
    
    totals_row <- data.frame(
      BuildLabel = "TOTAL PORTFOLIO",
      NumLineups = sum(build_summary$NumLineups),
      LockedFighters = "",
      ExcludedFighters = "",
      MinTop1 = NA,
      AvgTop1Count = round(mean(rv$sd_random_lineups$Top1Count, na.rm = TRUE), 1),
      Timestamp = "",
      stringsAsFactors = FALSE
    )
    
    build_summary <- rbind(build_summary, totals_row)
    
    dt <- datatable(
      build_summary,
      options = list(
        scrollX = TRUE,
        pageLength = -1,
        dom = 't',
        columnDefs = list(
          list(targets = "_all", className = 'dt-center')
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'BuildLabel',
        target = 'row',
        fontWeight = styleEqual('TOTAL PORTFOLIO', 'bold'),
        backgroundColor = styleEqual('TOTAL PORTFOLIO', '#ffffcc')
      )
    
    return(dt)
  })
  
  
  # SD filter change observer for real-time pool size updates
  observeEvent(c(input$sd_min_top1_count, input$sd_min_top2_count, input$sd_min_top3_count, 
                 input$sd_min_top5_count, input$sd_excluded_fighters), {
                   if(!is.null(rv$sd_optimal_lineups) && !is.null(rv$sd_fighter_exposure)) {
                     
                     filters <- list(
                       min_top1_count = if(!is.null(input$sd_min_top1_count)) input$sd_min_top1_count else 0,
                       min_top2_count = if(!is.null(input$sd_min_top2_count)) input$sd_min_top2_count else 0,
                       min_top3_count = if(!is.null(input$sd_min_top3_count)) input$sd_min_top3_count else 0,
                       min_top5_count = if(!is.null(input$sd_min_top5_count)) input$sd_min_top5_count else 0,
                       excluded_fighters = if(!is.null(input$sd_excluded_fighters)) input$sd_excluded_fighters else character(0)
                     )
                     
                     pool_stats <- calculate_sd_filtered_pool_stats(rv$sd_optimal_lineups, filters)
                     
                     output$sd_filtered_pool_size <- renderText({
                       sprintf("Filtered Pool: %s lineups available", 
                               format(pool_stats$count, big.mark = ","))
                     })
                     
                     # Update fighter exposure filtered pool rates
                     filtered_rates <- calculate_filtered_pool_rates(rv$sd_optimal_lineups, filters, platform = "sd")
                     
                     if(!is.null(filtered_rates) && nrow(filtered_rates) > 0 && !is.null(rv$sd_fighter_exposure)) {
                       rv$sd_fighter_exposure <- merge(
                         rv$sd_fighter_exposure[, setdiff(names(rv$sd_fighter_exposure), "FilteredPoolRate"), drop = FALSE],
                         filtered_rates[, c("Name", "FilteredPoolRate")],
                         by = "Name",
                         all.x = TRUE
                       )
                     }
                   }
                 }, ignoreNULL = FALSE)
  
  # SD excluded fighters choices
  observe({
    if(!is.null(rv$sd_optimal_lineups)) {
      fighter_cols <- c("Captain", paste0("Fighter", 1:5))
      all_fighters <- unique(unlist(rv$sd_optimal_lineups[, fighter_cols]))
      all_fighters <- sort(all_fighters[!is.na(all_fighters)])
      
      updateSelectizeInput(session, "sd_excluded_fighters", 
                           choices = all_fighters,
                           server = TRUE)
    }
  })
  
  # SD random lineups table
  output$sd_random_lineups_table <- renderDT({
    req(rv$sd_random_lineups)
    
    display_data <- as.data.frame(rv$sd_random_lineups)
    
    # Reorder columns for display
    display_cols <- c("Captain", paste0("Fighter", 1:5), 
                      "Top1Count", "Top2Count", "Top3Count", "Top5Count", 
                      "TotalSalary")
    display_cols <- intersect(display_cols, names(display_data))
    display_data <- display_data[, display_cols, drop = FALSE]
    
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "tip",
        order = list(list(0, 'asc')),
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Format TotalSalary as currency
    if("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    # Add color bars to count columns
    for(col in c("Top1Count", "Top2Count", "Top3Count", "Top5Count")) {
      if(col %in% names(display_data)) {
        dt <- dt %>% formatStyle(
          col,
          background = styleColorBar(range(display_data[[col]], na.rm = TRUE), '#FFD700'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      }
    }
    
    dt
  })
  
  output$download_sd_random_lineups <- downloadHandler(
    filename = function() {
      paste0("MMA_SD_Portfolio_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(length(rv$sd_lineup_builds) > 0)
      
      # Combine all builds
      all_lineups <- do.call(rbind, lapply(rv$sd_lineup_builds, function(b) b$lineups))
      
      # Randomize the order
      all_lineups <- all_lineups[sample(nrow(all_lineups)), ]
      
       # Create a copy for downloading
      download_data <- as.data.frame(all_lineups)
      
      # Create a name-to-SDID mapping from the simulation results
      name_to_id_map <- unique(rv$simulation_results[, c("Name", "SDID")])
      
      # Replace fighter names with "Name (ID)" format for Captain
      if("Captain" %in% names(download_data)) {
        download_data$Captain <- sapply(download_data$Captain, function(name) {
          match_idx <- which(name_to_id_map$Name == name)
          if(length(match_idx) > 0) {
            paste0(name, " (", name_to_id_map$SDID[match_idx[1]], ")")
          } else {
            name
          }
        })
      }
      
      # Replace fighter names with "Name (ID)" format for Players 1-5
      for(i in 1:5) {
        col <- paste0("Player", i)
        if(col %in% names(download_data)) {
          download_data[[col]] <- sapply(download_data[[col]], function(name) {
            match_idx <- which(name_to_id_map$Name == name)
            if(length(match_idx) > 0) {
              paste0(name, " (", name_to_id_map$SDID[match_idx[1]], ")")
            } else {
              name
            }
          })
        }
      }
      
      # Keep all columns including BuildLabel, LineupNum, and all metrics
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  
  # Clean up on session end
  session$onSessionEnded(function() {
    gc(verbose = FALSE, full = TRUE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)