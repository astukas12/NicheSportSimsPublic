# Load required packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(readxl)
library(ggplot2)
library(DT)
library(plotly)
library(lpSolve)
library(memoise)
library(shinycssloaders)
library(shinyjs)
library(parallel)
library(doParallel)
library(furrr)

options(datatable.optimize = Inf) 

# Set up custom CSS for app theme with black and gold color scheme
custom_css <- "
  /* Override dashboard header colors */
  .skin-blue .main-header {
+    background-color: #000000;
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
    border-left-color: #FFD700; /* Changed from #ff6600 to #FFD700 */
  }
  
  /* Customize box headers */
  .box.box-primary .box-header {
    background-color: #333333;
    color: #FFD700; /* Changed from #ff6600 to #FFD700 */
  }
  
  /* Style buttons */
  .btn-primary {
    background-color: #FFD700; /* Changed from #ff6600 to #FFD700 */
    border-color: #DAA520; /* Changed from #e65c00 to #DAA520 (darker gold) */
    color: #000000; /* Changed text color to black for better contrast on gold */
  }
  .btn-primary:hover, .btn-primary:focus {
    background-color: #DAA520; /* Changed from #e65c00 to #DAA520 */
    border-color: #B8860B; /* Changed from #cc5200 to #B8860B (darkgoldenrod) */
    color: #000000; /* Keep text black for contrast */
  }
  
  /* Style tabs */
  .nav-tabs-custom > .nav-tabs > li.active {
    border-top-color: #FFD700; /* Changed from #ff6600 to #FFD700 */
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

# Load historical score data (from the repository)
load_historical_data <- function() {
  # Attempt to load data from the package directory
  score_history_path <- "ScoreHistory.csv"
  
  if (file.exists(score_history_path)) {
    score_history <- read.csv(score_history_path)
    
    # Convert data types as needed
    score_history <- score_history %>%
      mutate(
        Tour = as.factor(Tour),
        surface = as.factor(surface),
        best_of = as.integer(best_of),
        w_dk_score = as.numeric(w_dk_score),
        l_dk_score = as.numeric(l_dk_score),
        WIO = as.numeric(WIO),
        LIO = as.numeric(LIO),
        straight_sets = as.integer(straight_sets)
      )
    
    return(score_history)
  } else {
    # If file doesn't exist, return NULL and app will prompt user to upload
    message("Historical score data not found in package. User will need to upload.")
    return(NULL)
  }
}

# Load historical data
SCORE_HISTORY <- load_historical_data()

# Global constants
DK_ROSTER_SIZE <- 6
DK_SALARY_CAP <- 50000



apply_lineup_filters <- function(optimal_lineups, filters) {
  # Convert to data.table for efficiency
  filtered_lineups <- as.data.table(copy(optimal_lineups))
  
  # Apply all the same filters as in generate_random_lineups
  if (!is.null(filters$min_top1_count) && filters$min_top1_count > 0) {
    filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
  }
  
  if (!is.null(filters$min_top2_count) && filters$min_top2_count > 0) {
    filtered_lineups <- filtered_lineups[Top2Count >= filters$min_top2_count]
  }
  
  if (!is.null(filters$min_top3_count) && filters$min_top3_count > 0) {
    filtered_lineups <- filtered_lineups[Top3Count >= filters$min_top3_count]
  }
  
  if (!is.null(filters$min_top5_count) && filters$min_top5_count > 0) {
    filtered_lineups <- filtered_lineups[Top5Count >= filters$min_top5_count]
  }
  
  if (!is.null(filters$min_total_ew) && filters$min_total_ew > 0) {
    filtered_lineups <- filtered_lineups[TotalEW >= filters$min_total_ew]
  }
  
  if (!is.null(filters$min_win6_pct) && filters$min_win6_pct > 0) {
    filtered_lineups <- filtered_lineups[Win6Pct >= filters$min_win6_pct]
  }
  
  if (!is.null(filters$min_win5plus_pct) && filters$min_win5plus_pct > 0) {
    filtered_lineups <- filtered_lineups[Win5PlusPct >= filters$min_win5plus_pct]
  }
  
  if (!is.null(filters$min_median_score) && filters$min_median_score > 0) {
    filtered_lineups <- filtered_lineups[MedianScore >= filters$min_median_score]
  }
  
  if (!is.null(filters$min_score80th) && filters$min_score80th > 0) {
    filtered_lineups <- filtered_lineups[Score80th >= filters$min_score80th]
  }
  
  # Apply player exclusion filter
  if (!is.null(filters$excluded_players) && length(filters$excluded_players) > 0) {
    player_cols <- if(any(grepl("^Name[1-6]$", names(filtered_lineups)))) {
      grep("^Name[1-6]$", names(filtered_lineups), value = TRUE)
    } else {
      grep("^Player[1-6]$", names(filtered_lineups), value = TRUE)
    }
    
    if(length(player_cols) > 0) {
      for(excluded_player in filters$excluded_players) {
        exclude_condition <- rep(TRUE, nrow(filtered_lineups))
        for(col in player_cols) {
          exclude_condition <- exclude_condition & (filtered_lineups[[col]] != excluded_player)
        }
        filtered_lineups <- filtered_lineups[exclude_condition]
      }
    }
  }
  
  return(as.data.frame(filtered_lineups))
}

# Helper functions for odds conversion
odds_to_probability <- function(odds) {
  if (odds > 0) {
    return(100 / (odds + 100))
  } else {
    return(abs(odds) / (abs(odds) + 100))
  }
}

probability_to_odds <- function(prob) {
  if (prob >= 0.5) {
    return(-(prob * 100) / (1 - prob))
  } else {
    return((100 - prob * 100) / prob)
  }
}

# Memory management functions
cleanup_memory <- function(verbose = FALSE) {
  gc(verbose = verbose, full = TRUE, reset = TRUE)
  
  if (.Platform$OS.type == "unix") {
    try(system("sync"), silent = TRUE)
  }
}


calculate_lineup_ranks <- function(lineups_df) {
  # Create a copy to avoid modifying original data
  ranked_lineups <- as.data.frame(lineups_df)
  
  # Define columns that should be ranked (higher values = better ranks)
  rank_columns <- c("TotalEW", "MedianScore", "Score80th", "Win6Pct", "Win5PlusPct", 
                    "Top1Count", "Top2Count", "Top3Count", "Top5Count")
  
  # Only rank columns that exist in the data
  available_rank_cols <- intersect(rank_columns, names(ranked_lineups))
  
  # Calculate ranks for each column (1 = best, higher numbers = worse)
  for (col in available_rank_cols) {
    rank_col_name <- paste0(col, "_Rank")
    if (!all(is.na(ranked_lineups[[col]]))) {
      ranked_lineups[[rank_col_name]] <- rank(-ranked_lineups[[col]], ties.method = "min")
    } else {
      ranked_lineups[[rank_col_name]] <- NA
    }
  }
  
  return(ranked_lineups)
}

calculate_weighted_rank <- function(lineups_df, weights) {
  # Available rank columns based on what exists in the data
  available_rank_cols <- names(lineups_df)[grepl("_Rank$", names(lineups_df))]
  
  # Initialize weighted rank as 0
  lineups_df$WeightedRank <- 0
  lineups_df$WeightedScore <- 0
  
  # Calculate weighted rank score (lower is better since ranks are 1=best)
  total_weight <- 0
  
  for (rank_col in available_rank_cols) {
    # Extract the base metric name (remove "_Rank" suffix)
    base_metric <- gsub("_Rank$", "", rank_col)
    
    # Check if we have a weight for this metric
    weight_key <- switch(base_metric,
                         "TotalEW" = "weight_totalew",
                         "MedianScore" = "weight_medianscore", 
                         "Score80th" = "weight_score80th",
                         "Win6Pct" = "weight_win6pct",
                         "Win5PlusPct" = "weight_win5pluspct",
                         "Top1Count" = "weight_top1count",
                         "Top2Count" = "weight_top2count", 
                         "Top3Count" = "weight_top3count",
                         "Top5Count" = "weight_top5count",
                         NULL)
    
    if (!is.null(weight_key) && !is.null(weights[[weight_key]]) && weights[[weight_key]] > 0) {
      weight_value <- weights[[weight_key]]
      
      # Add weighted rank to the total (multiply rank by weight)
      lineups_df$WeightedScore <- lineups_df$WeightedScore + 
        (lineups_df[[rank_col]] * weight_value)
      
      total_weight <- total_weight + weight_value
    }
  }
  
  # Normalize by total weight if any weights were applied
  if (total_weight > 0) {
    lineups_df$WeightedScore <- lineups_df$WeightedScore / total_weight
  }
  
  # Calculate final weighted rank (1 = best weighted score)
  lineups_df$WeightedRank <- rank(lineups_df$WeightedScore, ties.method = "min")
  
  return(lineups_df)
}


# Updated calculate_ew_metrics function that accounts for same-match constraints
calculate_ew_metrics <- function(simulation_results, n_simulations, dk_data = NULL) {
  cat("Calculating EW metrics from simulation results...\n")
  
  # Convert to data.table for performance
  sim_dt <- as.data.table(simulation_results)
  
  # Calculate win counts per iteration for each player
  win_counts <- sim_dt[Result == "Winner" & Outcome != "WO", .N, by = .(Iteration, Player)]
  
  # Create a complete grid of all iterations and players (including 0 wins)
  all_iterations <- unique(sim_dt$Iteration)
  all_players <- unique(sim_dt$Player)
  
  complete_grid <- CJ(Iteration = all_iterations, Player = all_players)
  
  # Merge with actual win counts (NAs become 0)
  win_counts_complete <- merge(complete_grid, win_counts, 
                               by = c("Iteration", "Player"), all.x = TRUE)
  win_counts_complete[is.na(N), N := 0]
  
  # Calculate basic metrics for each player (individual EW without constraints)
  individual_ew_metrics <- win_counts_complete[, .(
    Individual_EW = mean(N, na.rm = TRUE),  # Individual Expected Wins
    Win6Plus = mean(N >= 6, na.rm = TRUE) * 100,  # 6+ win %
    Win5Plus = mean(N >= 5, na.rm = TRUE) * 100   # 5+ win %
  ), by = Player]
  
  # If we have dk_data, create match mapping for constraint-aware EW calculation
  if (!is.null(dk_data)) {
    cat("Creating match mapping for constraint-aware EW calculation...\n")
    
    # Create player to match mapping
    dk_dt <- as.data.table(dk_data)
    player_match_map <- setNames(dk_dt$`Game Info`, dk_dt$Name)
    
    # Function to calculate constraint-aware EW for a lineup
    calculate_lineup_constraint_ew <- function(player_names) {
      # Get matches for each player
      player_matches <- player_match_map[player_names]
      
      # Group players by match
      match_groups <- split(player_names, player_matches)
      
      total_ew <- 0
      
      for (match_players in match_groups) {
        if (length(match_players) == 1) {
          # Single player from this match - use individual EW
          player_ew <- individual_ew_metrics[Player == match_players[1], Individual_EW]
          if (length(player_ew) > 0) {
            total_ew <- total_ew + player_ew
          }
        } else if (length(match_players) == 2) {
          # Two players from same match - EW is exactly 1.0
          total_ew <- total_ew + 1.0
        } else {
          # This shouldn't happen in tennis (more than 2 players from same match)
          warning("More than 2 players from same match detected: ", paste(match_players, collapse = ", "))
          total_ew <- total_ew + 1.0
        }
      }
      
      return(total_ew)
    }
    
    # Add constraint-aware EW to the metrics
    individual_ew_metrics[, Constraint_EW := Individual_EW]  # Default to individual EW
    
    # For display purposes, we'll use Individual_EW but note that lineup-level 
    # calculations should use the constraint-aware method
    cat("EW metrics calculated with match constraint awareness\n")
    
    # Add a helper function to the global environment for lineup calculations
    assign("calculate_lineup_constraint_ew", calculate_lineup_constraint_ew, envir = .GlobalEnv)
    assign("player_match_map", player_match_map, envir = .GlobalEnv)
    assign("individual_ew_metrics", individual_ew_metrics, envir = .GlobalEnv)
  } else {
    cat("No DK data provided - using individual EW only\n")
  }
  
  # Return individual metrics (constraint-aware calculations happen at lineup level)
  final_metrics <- individual_ew_metrics[, .(
    Player = Player,
    EW = Individual_EW,  # This represents individual player EW
    Win6Plus = Win6Plus,
    Win5Plus = Win5Plus
  )]
  
  cat("EW metrics calculated for", nrow(final_metrics), "players\n")
  return(final_metrics)
}

# Updated function to calculate constraint-aware TotalEW for lineups
calculate_lineup_total_ew <- function(lineup_players, individual_ew_metrics, player_match_map) {
  # Get matches for each player
  player_matches <- player_match_map[lineup_players]
  
  # Group players by match
  match_groups <- split(lineup_players, player_matches)
  
  total_ew <- 0
  
  for (match_players in match_groups) {
    if (length(match_players) == 1) {
      # Single player from this match - use individual EW
      player_ew <- individual_ew_metrics[Player == match_players[1], EW]
      if (length(player_ew) > 0) {
        total_ew <- total_ew + player_ew
      }
    } else if (length(match_players) == 2) {
      # Two players from same match - EW is exactly 1.0
      total_ew <- total_ew + 1.0
    } else {
      # This shouldn't happen in tennis
      warning("More than 2 players from same match detected: ", paste(match_players, collapse = ", "))
      total_ew <- total_ew + 1.0
    }
  }
  
  return(total_ew)
}

calculate_lineup_total_ew <- function(lineup_players, individual_ew_metrics, player_match_map) {
  # Get matches for each player
  player_matches <- player_match_map[lineup_players]
  
  # Group players by match
  match_groups <- split(lineup_players, player_matches)
  
  total_ew <- 0
  
  for (match_players in match_groups) {
    if (length(match_players) == 1) {
      # Single player from this match - use individual EW
      player_ew <- individual_ew_metrics[Player == match_players[1], EW]
      if (length(player_ew) > 0) {
        total_ew <- total_ew + player_ew
      }
    } else if (length(match_players) == 2) {
      # Two players from same match - EW is exactly 1.0
      total_ew <- total_ew + 1.0
    } else {
      # This shouldn't happen in tennis
      warning("More than 2 players from same match detected: ", paste(match_players, collapse = ", "))
      total_ew <- total_ew + 1.0
    }
  }
  
  return(total_ew)
}


calculate_weighted_pool_stats <- function(optimal_lineups_ranked, top_x, excluded_players = NULL) {
  # Validate inputs
  if(is.null(optimal_lineups_ranked) || nrow(optimal_lineups_ranked) == 0) {
    return(list(count = 0, rank_range = "N/A", ew_range = "N/A"))
  }
  
  # Convert to data.table for efficiency
  lineups_dt <- as.data.table(optimal_lineups_ranked)
  
  # Sort by WeightedRank if available, otherwise by TotalEW
  if("WeightedRank" %in% names(lineups_dt)) {
    setorder(lineups_dt, WeightedRank)
  } else if("TotalEW" %in% names(lineups_dt)) {
    setorder(lineups_dt, -TotalEW)
  }
  
  # Take top X lineups
  top_lineups <- head(lineups_dt, min(top_x, nrow(lineups_dt)))
  
  # Apply player exclusion filter if specified
  if (!is.null(excluded_players) && length(excluded_players) > 0) {
    # Determine player column pattern
    player_cols <- if(any(grepl("^Name[1-6]$", names(top_lineups)))) {
      grep("^Name[1-6]$", names(top_lineups), value = TRUE)
    } else {
      grep("^Player[1-6]$", names(top_lineups), value = TRUE)
    }
    
    if(length(player_cols) > 0) {
      for(excluded_player in excluded_players) {
        # Create condition to exclude lineups containing this player
        exclude_condition <- rep(TRUE, nrow(top_lineups))
        for(col in player_cols) {
          exclude_condition <- exclude_condition & (top_lineups[[col]] != excluded_player)
        }
        top_lineups <- top_lineups[exclude_condition]
      }
    }
  }
  
  # Calculate statistics
  final_count <- nrow(top_lineups)
  
  if(final_count == 0) {
    return(list(count = 0, rank_range = "N/A", ew_range = "N/A"))
  }
  
  # Rank range
  if("WeightedRank" %in% names(top_lineups)) {
    min_rank <- min(top_lineups$WeightedRank, na.rm = TRUE)
    max_rank <- max(top_lineups$WeightedRank, na.rm = TRUE)
    rank_range <- paste0("#", min_rank, " - #", max_rank)
  } else {
    rank_range <- "N/A"
  }
  
  # EW range
  if("TotalEW" %in% names(top_lineups)) {
    min_ew <- min(top_lineups$TotalEW, na.rm = TRUE)
    max_ew <- max(top_lineups$TotalEW, na.rm = TRUE)
    ew_range <- paste0(round(min_ew, 2), " - ", round(max_ew, 2))
  } else {
    ew_range <- "N/A"
  }
  
  return(list(
    count = final_count,
    rank_range = rank_range,
    ew_range = ew_range
  ))
}

run_batch_simulation <- function(dk_data, historical_data, n_simulations = 50000) {
  # Start timing
  overall_start_time <- Sys.time()
  cat("\n=== BATCH SIMULATION STARTED ===\n")
  cat("Starting batch simulation with", format(n_simulations, big.mark = ","), "iterations\n")
  cat("Timestamp:", format(overall_start_time, "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # Convert to data.table for performance
  dk_dt <- as.data.table(dk_data)
  hist_dt <- as.data.table(historical_data)
  
  # Create indexes on historical data
  setkey(hist_dt, Tour, best_of, straight_sets)
  
  # Get all unique matches
  matches <- unique(dk_dt$`Game Info`)
  total_matches <- length(matches)
  
  cat("Processing", total_matches, "matches:\n")
  for(i in seq_along(matches)) {
    cat(sprintf("  %d. %s\n", i, matches[i]))
  }
  cat("\n")
  
  # Create storage for match outcomes
  match_results <- list()
  
  # Process each match with detailed timing
  for (match_idx in seq_along(matches)) {
    match_start_time <- Sys.time()
    match_name <- matches[match_idx]
    
    cat(sprintf("[%s] Processing match %d/%d: %s", 
                format(match_start_time, "%H:%M:%S"), 
                match_idx, total_matches, match_name))
    
    # Get players in this match
    match_players <- dk_dt[`Game Info` == match_name]
    
    if (nrow(match_players) != 2) {
      cat(" - SKIPPED (invalid player count)\n")
      warning("Skipping match with invalid number of players:", match_name)
      next
    }
    
    # Extract player info
    p1 <- match_players[1]
    p2 <- match_players[2]
    
    cat(sprintf(" (%s vs %s)", p1$Name, p2$Name))
    
    # Check for walkover scenario
    p1_tour <- p1$Tour
    p2_tour <- p2$Tour
    
    # Check if this is a walkover match
    is_walkover <- any(c(p1_tour, p2_tour) %in% c("WD", "WO"))
    
    if (is_walkover) {
      cat(" - WALKOVER DETECTED")
      
      # Determine who withdraws and who gets the walkover win
      if (p1_tour == "WD" && p2_tour == "WO") {
        winner_name <- p2$Name
        loser_name <- p1$Name
        winner_score <- 30
        loser_score <- 0
      } else if (p1_tour == "WO" && p2_tour == "WD") {
        winner_name <- p1$Name
        loser_name <- p2$Name
        winner_score <- 30
        loser_score <- 0
      } else if (p1_tour == "WD") {
        winner_name <- p2$Name
        loser_name <- p1$Name
        winner_score <- 30
        loser_score <- 0
      } else if (p2_tour == "WD") {
        winner_name <- p1$Name
        loser_name <- p2$Name
        winner_score <- 30
        loser_score <- 0
      } else if (p1_tour == "WO") {
        winner_name <- p1$Name
        loser_name <- p2$Name
        winner_score <- 30
        loser_score <- 0
      } else if (p2_tour == "WO") {
        winner_name <- p2$Name
        loser_name <- p1$Name
        winner_score <- 30
        loser_score <- 0
      } else {
        cat(" - ERROR: Invalid walkover configuration")
        next
      }
      
      # Create walkover results for all iterations
      match_output <- data.table(
        iteration = 1:n_simulations,
        winner = rep(winner_name, n_simulations),
        loser = rep(loser_name, n_simulations),
        outcome = rep("WO", n_simulations),
        winner_prob = rep(1.0, n_simulations),
        loser_prob = rep(0.0, n_simulations),
        winner_score = rep(winner_score, n_simulations),
        loser_score = rep(loser_score, n_simulations)
      )
      
      cat(sprintf(" (%s withdraws, %s gets walkover)", loser_name, winner_name))
      
    } else {
      # NORMAL MATCH SIMULATION
      
      # Calculate ML probabilities
      p1_ml <- odds_to_probability(as.numeric(p1$ML))
      p2_ml <- odds_to_probability(as.numeric(p2$ML))
      
      # Normalize to sum to 1
      total_ml_prob <- p1_ml + p2_ml
      p1_ml_prob <- p1_ml / total_ml_prob
      p2_ml_prob <- p2_ml / total_ml_prob
      
      # Calculate SS probabilities
      p1_ss <- odds_to_probability(as.numeric(p1$SS))
      p2_ss <- odds_to_probability(as.numeric(p2$SS))
      
      p1_nss <- p1_ml - p1_ss
      p2_nss <- p2_ml - p2_ss
      
      # Normalize to 1
      p1_ss_prob <- p1_ss / total_ml_prob
      p2_ss_prob <- p2_ss / total_ml_prob
      p1_nss_prob <- p1_nss / total_ml_prob
      p2_nss_prob <- p2_nss / total_ml_prob
      
      # Create probability bins for sampling
      cum_probs <- c(0, p1_ss_prob, p1_ss_prob + p1_nss_prob, 
                     p1_ss_prob + p1_nss_prob + p2_ss_prob, 1)
      
      # Generate ALL simulation outcomes at once for this match
      random_values <- runif(n_simulations)
      
      # Pre-allocate results for this match
      match_output <- data.table(
        iteration = 1:n_simulations,
        winner = character(n_simulations),
        loser = character(n_simulations),
        outcome = character(n_simulations),
        winner_prob = numeric(n_simulations),
        loser_prob = numeric(n_simulations),
        winner_score = numeric(n_simulations),
        loser_score = numeric(n_simulations)
      )
      
      # Figure out which bin each random value falls into
      outcome_bins <- findInterval(random_values, cum_probs)
      
      # Assign winners, losers and outcome types based on bins
      match_output[outcome_bins == 1, `:=`(
        winner = p1$Name, loser = p2$Name, outcome = "SS",
        winner_prob = p1_ml_prob, loser_prob = p2_ml_prob
      )]
      
      match_output[outcome_bins == 2, `:=`(
        winner = p1$Name, loser = p2$Name, outcome = "NSS",
        winner_prob = p1_ml_prob, loser_prob = p2_ml_prob
      )]
      
      match_output[outcome_bins == 3, `:=`(
        winner = p2$Name, loser = p1$Name, outcome = "SS",
        winner_prob = p2_ml_prob, loser_prob = p1_ml_prob
      )]
      
      match_output[outcome_bins == 4, `:=`(
        winner = p2$Name, loser = p1$Name, outcome = "NSS",
        winner_prob = p2_ml_prob, loser_prob = p1_ml_prob
      )]
      
      # Now assign scores based on outcome types
      outcome_groups <- match_output[, .N, by = .(winner, loser, outcome)]
      
      score_assignment_start <- Sys.time()
      
      # For each distinct outcome configuration
      for (j in 1:nrow(outcome_groups)) {
        winner <- outcome_groups$winner[j]
        loser <- outcome_groups$loser[j]
        outcome_type <- outcome_groups$outcome[j]
        count <- outcome_groups$N[j]
        
        # Get rows matching this outcome
        outcome_indices <- which(
          match_output$winner == winner & 
            match_output$loser == loser & 
            match_output$outcome == outcome_type
        )
        
        # Get one example row to extract probabilities
        example_row <- match_output[outcome_indices[1]]
        winner_prob <- example_row$winner_prob
        loser_prob <- example_row$loser_prob
        
        # Create match info for finding similar historical matches
        tour <- match_players$Tour[1]
        is_straight_sets <- as.integer(outcome_type == "SS")
        #best_of_value <- ifelse(tour == "ATP", 5, 3)
        best_of_value <- 3
        
        # Find similar historical matches
        similar_matches <- hist_dt[
          Tour == tour & 
            best_of == best_of_value &
            straight_sets == is_straight_sets
        ]
        
        # If not enough matches, relax constraints
        if (nrow(similar_matches) < 10) {
          similar_matches <- hist_dt[Tour == tour & best_of == best_of_value]
        }
        if (nrow(similar_matches) < 10) {
          similar_matches <- hist_dt[Tour == tour]
        }
        
        # Calculate similarity scores and assign scores
        if (nrow(similar_matches) > 0) {
          similar_matches[, odds_diff := abs(WIO - winner_prob) + abs(LIO - loser_prob)]
          setorder(similar_matches, odds_diff)
          
          n_similar <- min(40, nrow(similar_matches))
          top_matches <- similar_matches[1:n_similar]
          
          sample_indices <- sample(1:n_similar, length(outcome_indices), replace = TRUE)
          
          match_output[outcome_indices, `:=`(
            winner_score = top_matches$w_dk_score[sample_indices],
            loser_score = top_matches$l_dk_score[sample_indices]
          )]
        } else {
          match_output[outcome_indices, `:=`(
            winner_score = runif(length(outcome_indices), 50, 70),
            loser_score = runif(length(outcome_indices), 20, 40)
          )]
        }
      }
      
      # Randomly shuffle the iterations
      shuffled_indices <- sample(1:n_simulations)
      match_output <- match_output[shuffled_indices]
      match_output[, iteration := 1:n_simulations]
    }
    
    # Store this match's results
    match_results[[match_name]] <- match_output
    
    # Calculate and display timing
    match_end_time <- Sys.time()
    match_elapsed <- difftime(match_end_time, match_start_time, units = "secs")
    
    cat(sprintf(" - COMPLETED in %.2f seconds\n", as.numeric(match_elapsed)))
    
    # Force garbage collection after each match
    if (match_idx %% 3 == 0) {
      gc(full = TRUE)
    }
  }
  
  cat("\n=== MATCH PROCESSING COMPLETED ===\n")
  cat("Now converting match results to final format...\n")
  
  # Convert match-by-match results to final format with progress tracking
  conversion_start_time <- Sys.time()
  n_matches <- length(match_results)
  total_rows <- n_simulations * n_matches * 2
  
  cat(sprintf("Converting %s simulation results (%s total rows)\n", 
              format(n_simulations, big.mark = ","),
              format(total_rows, big.mark = ",")))
  
  final_results <- data.table(
    Iteration = integer(total_rows),
    Match = character(total_rows),
    Player = character(total_rows),
    Result = character(total_rows),
    Outcome = character(total_rows),
    Score = numeric(total_rows)
  )
  
  row_idx <- 1
  
  # Track progress during conversion
  progress_interval <- max(1000, n_simulations %/% 20)  # Show progress 20 times
  
  for (iter in 1:n_simulations) {
    for (match_name in names(match_results)) {
      match_data <- match_results[[match_name]]
      iter_data <- match_data[iteration == iter]
      
      if (nrow(iter_data) == 0) next
      
      # Add winner
      final_results[row_idx, `:=`(
        Iteration = iter, Match = match_name, Player = iter_data$winner,
        Result = "Winner", Outcome = iter_data$outcome, Score = iter_data$winner_score
      )]
      row_idx <- row_idx + 1
      
      # Add loser
      final_results[row_idx, `:=`(
        Iteration = iter, Match = match_name, Player = iter_data$loser,
        Result = "Loser", Outcome = iter_data$outcome, Score = iter_data$loser_score
      )]
      row_idx <- row_idx + 1
    }
    
    # Show progress
    if (iter %% progress_interval == 0) {
      elapsed_conversion <- difftime(Sys.time(), conversion_start_time, units = "secs")
      pct_complete <- (iter / n_simulations) * 100
      estimated_total <- elapsed_conversion * (n_simulations / iter)
      estimated_remaining <- estimated_total - elapsed_conversion
      
      cat(sprintf("[%s] Conversion progress: %d/%s iterations (%.1f%%) - %.1fs elapsed, ~%.1fs remaining\n",
                  format(Sys.time(), "%H:%M:%S"),
                  iter, format(n_simulations, big.mark = ","),
                  pct_complete, as.numeric(elapsed_conversion), as.numeric(estimated_remaining)))
    }
  }
  
  # Trim unused rows
  final_results <- final_results[1:(row_idx-1)]
  
  # Report final timing
  overall_end_time <- Sys.time()
  total_elapsed <- difftime(overall_end_time, overall_start_time, units = "mins")
  conversion_elapsed <- difftime(Sys.time(), conversion_start_time, units = "secs")
  
  cat("\n=== BATCH SIMULATION COMPLETED ===\n")
  cat(sprintf("Total time: %.2f minutes\n", as.numeric(total_elapsed)))
  cat(sprintf("Final conversion time: %.2f seconds\n", as.numeric(conversion_elapsed)))
  cat(sprintf("Generated %s simulation results\n", format(nrow(final_results), big.mark = ",")))
  cat("Timestamp:", format(overall_end_time, "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  return(final_results)
}


calculate_player_exposure <- function(optimal_lineups, player_stats, random_lineups = NULL) {
  # Quick validation
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame(Message = "No optimal lineups available."))
  }
  
  # Convert inputs to data.table for better performance
  setDT(optimal_lineups)
  if(!is.null(player_stats)) setDT(player_stats)
  if(!is.null(random_lineups)) setDT(random_lineups)
  
  # Determine player column naming pattern - handle both "Name1" and "Player1" formats
  name_pattern <- if(any(grepl("^Name[1-6]$", names(optimal_lineups)))) {
    "^Name[1-6]$"
  } else if(any(grepl("^Player[1-6]$", names(optimal_lineups)))) {
    "^Player[1-6]$"
  } else {
    stop("Could not find player columns in optimal lineups")
  }
  
  player_cols <- grep(name_pattern, names(optimal_lineups), value = TRUE)
  
  # Get all players from both optimal and random lineups
  all_players_optimal <- unique(unlist(optimal_lineups[, ..player_cols]))
  
  if(!is.null(random_lineups)) {
    # Determine player column pattern in random lineups
    random_pattern <- if(any(grepl("^Name[1-6]$", names(random_lineups)))) {
      "^Name[1-6]$"
    } else if(any(grepl("^Player[1-6]$", names(random_lineups)))) {
      "^Player[1-6]$"
    } else {
      NULL
    }
    
    if(!is.null(random_pattern)) {
      random_player_cols <- grep(random_pattern, names(random_lineups), value = TRUE)
      all_players_random <- unique(unlist(random_lineups[, ..random_player_cols]))
      all_players <- unique(c(all_players_optimal, all_players_random))
    } else {
      all_players <- all_players_optimal
    }
  } else {
    all_players <- all_players_optimal
  }
  
  # Start with all players from player_stats to ensure complete list
  if(!is.null(player_stats) && nrow(player_stats) > 0) {
    # Get all unique players from player_stats
    stats_players <- if("Player" %in% names(player_stats)) {
      unique(player_stats$Player)
    } else if("Name" %in% names(player_stats)) {
      unique(player_stats$Name)
    } else {
      character(0)
    }
    all_players <- unique(c(all_players, stats_players))
  }
  
  # Initialize metrics data frame
  metrics_data <- data.table(
    Player = all_players,
    Salary = NA_real_,
    DKOwn = NA_real_,
    Pool_Exposure = 0,
    Randomized_Exposure = 0,
    Pool_Leverage = 0,
    Randomized_Leverage = 0
  )
  
  # Match with player stats using direct matching
  if(!is.null(player_stats) && nrow(player_stats) > 0) {
    for(i in 1:nrow(metrics_data)) {
      player_name <- metrics_data$Player[i]
      
      # Try to find the player using different column names
      matches <- if("Player" %in% names(player_stats)) {
        player_stats[Player == player_name]
      } else if("Name" %in% names(player_stats)) {
        player_stats[Name == player_name]
      } else {
        data.table()
      }
      
      if(nrow(matches) > 0) {
        # Update values directly
        if("Salary" %in% names(matches)) {
          metrics_data[i, Salary := matches$Salary[1]]
        }
        if("DKOwn" %in% names(matches)) {
          metrics_data[i, DKOwn := matches$DKOwn[1]]
        }
      }
    }
  }
  
  # Calculate Pool Exposure (from optimal lineups)
  if(nrow(optimal_lineups) > 0) {
    for(player in all_players) {
      # Count appearances in optimal lineups
      player_appears <- logical(nrow(optimal_lineups))
      for(col in player_cols) {
        player_appears <- player_appears | (optimal_lineups[[col]] == player)
      }
      metrics_data[Player == player, Pool_Exposure := (sum(player_appears) / nrow(optimal_lineups)) * 100]
    }
  }
  
  # Calculate Randomized Exposure from random lineups
  if(!is.null(random_lineups) && nrow(random_lineups) > 0) {
    # Determine player column pattern in random lineups
    random_pattern <- if(any(grepl("^Name[1-6]$", names(random_lineups)))) {
      "^Name[1-6]$"
    } else if(any(grepl("^Player[1-6]$", names(random_lineups)))) {
      "^Player[1-6]$"
    } else {
      NULL
    }
    
    if(!is.null(random_pattern)) {
      random_player_cols <- grep(random_pattern, names(random_lineups), value = TRUE)
      
      if(length(random_player_cols) > 0) {
        for(player in all_players) {
          player_appears <- logical(nrow(random_lineups))
          for(col in random_player_cols) {
            player_appears <- player_appears | (random_lineups[[col]] == player)
          }
          metrics_data[Player == player, Randomized_Exposure := (sum(player_appears) / nrow(random_lineups)) * 100]
        }
      }
    }
  }
  
  # Calculate leverage (only where we have ownership projections)
  metrics_data[!is.na(DKOwn), Pool_Leverage := Pool_Exposure - (DKOwn * 100)]
  metrics_data[!is.na(DKOwn), Randomized_Leverage := Randomized_Exposure - (DKOwn * 100)]
  
  # Sort by Pool Exposure descending
  setorder(metrics_data, -Pool_Exposure)
  
  return(as.data.frame(metrics_data))
}

generate_weighted_lineups <- function(optimal_lineups_ranked, top_x, num_lineups, excluded_players = NULL) {
  # Convert to data.table for efficiency
  lineups_dt <- as.data.table(optimal_lineups_ranked)
  
  # Sort by WeightedRank if available, otherwise by TotalEW
  if("WeightedRank" %in% names(lineups_dt)) {
    setorder(lineups_dt, WeightedRank)
  } else if("TotalEW" %in% names(lineups_dt)) {
    setorder(lineups_dt, -TotalEW)
  }
  
  # Take top X lineups to form the pool
  pool_lineups <- head(lineups_dt, min(top_x, nrow(lineups_dt)))
  
  # Determine player column pattern
  player_cols <- if(any(grepl("^Name[1-6]$", names(pool_lineups)))) {
    grep("^Name[1-6]$", names(pool_lineups), value = TRUE)
  } else {
    grep("^Player[1-6]$", names(pool_lineups), value = TRUE)
  }
  
  if(length(player_cols) == 0) {
    stop("Could not find player columns in the lineup data")
  }
  
  # Apply player exclusion filter
  if (!is.null(excluded_players) && length(excluded_players) > 0) {
    for(excluded_player in excluded_players) {
      # Create condition to exclude lineups containing this player
      exclude_condition <- rep(TRUE, nrow(pool_lineups))
      for(col in player_cols) {
        exclude_condition <- exclude_condition & (pool_lineups[[col]] != excluded_player)
      }
      pool_lineups <- pool_lineups[exclude_condition]
    }
  }
  
  # Check if any lineups remain after filtering
  if (nrow(pool_lineups) == 0) {
    return(NULL)
  }
  
  # Sample lineups from the filtered pool
  # Use inverse rank as weights (better ranks = higher probability)
  if("WeightedRank" %in% names(pool_lineups)) {
    # Convert ranks to weights (lower rank = higher weight)
    max_rank <- max(pool_lineups$WeightedRank, na.rm = TRUE)
    weights <- (max_rank + 1) - pool_lineups$WeightedRank
  } else {
    # Use TotalEW as weights directly
    weights <- pool_lineups$TotalEW
  }
  
  # Ensure weights are positive
  weights <- pmax(weights, 0.1)
  
  # Sample lineups
  sample_size <- min(num_lineups, nrow(pool_lineups))
  selected_indices <- sample(1:nrow(pool_lineups), sample_size, replace = FALSE, prob = weights)
  selected_lineups <- pool_lineups[selected_indices]
  
  # Prepare for exposure tracking
  all_players <- unique(unlist(selected_lineups[, ..player_cols]))
  player_counts <- setNames(numeric(length(all_players)), all_players)
  
  # Calculate player exposure
  for(i in 1:nrow(selected_lineups)) {
    lineup_players <- unlist(selected_lineups[i, ..player_cols])
    player_counts[lineup_players] <- player_counts[lineup_players] + 1
  }
  
  # Calculate exposure percentage
  final_exposure <- (player_counts / nrow(selected_lineups)) * 100
  attr(selected_lineups, "exposure") <- final_exposure
  
  # Keep relevant columns for display
  keep_cols <- c(player_cols, "WeightedRank", "TotalEW", "MedianScore", "Score80th", 
                 "Win6Pct", "Win5PlusPct", "Top1Count", "TotalSalary")
  keep_cols <- intersect(keep_cols, names(selected_lineups))
  
  return(as.data.frame(selected_lineups[, ..keep_cols]))
}


find_similar_matches <- function(current_match, historical_data, n_matches = 50, cache = NULL) {
  # Create an efficient static cache if none provided
  if (is.null(cache)) {
    if (!exists("MATCH_CACHE", envir = .GlobalEnv)) {
      assign("MATCH_CACHE", new.env(hash = TRUE), envir = .GlobalEnv)
    }
    cache <- get("MATCH_CACHE", envir = .GlobalEnv)
  }
  
  # Determine best_of based on tour
  #best_of_value <- ifelse(current_match$Tour == "ATP", 5, 3)
  best_of_value <- 3
  
  # Create cache key including best_of
  cache_key <- paste(
    current_match$Tour,
    best_of_value,
    current_match$straight_sets,
    round(current_match$WIO, 2),  # Round to reduce key variations
    round(current_match$LIO, 2),
    sep = "_"
  )
  
  # Check if result is in cache using exists() for faster lookup
  if (exists(cache_key, envir = cache)) {
    return(get(cache_key, envir = cache))
  }
  
  # Convert to data.table if not already
  if (!is.data.table(historical_data)) {
    hist_dt <- as.data.table(historical_data)
  } else {
    hist_dt <- historical_data
  }
  
  # Filter historical data efficiently - now including best_of
  filtered_data <- hist_dt[
    Tour == current_match$Tour & 
      best_of == best_of_value &
      straight_sets == current_match$straight_sets
  ]
  
  # If not enough matches, relax straight_sets constraint but keep best_of
  if (nrow(filtered_data) < 10) {
    filtered_data <- hist_dt[
      Tour == current_match$Tour &
        best_of == best_of_value
    ]
  }
  
  # If still not enough matches, relax best_of constraint
  if (nrow(filtered_data) < 10) {
    filtered_data <- hist_dt[
      Tour == current_match$Tour
    ]
  }
  
  # Calculate similarity score efficiently
  filtered_data[, odds_diff := abs(WIO - current_match$WIO) + abs(LIO - current_match$LIO)]
  setorder(filtered_data, odds_diff)
  
  # Return top N most similar matches
  result <- head(filtered_data, n_matches)
  
  # Store in cache
  assign(cache_key, result, envir = cache)
  
  return(result)
}


generate_match_score <- function(current_match, historical_data, cache = NULL) {
  # Find similar matches
  similar_matches <- find_similar_matches(current_match, historical_data)
  
  # If no similar matches found, return default scores
  if (nrow(similar_matches) == 0) {
    result <- list(
      winner_score = 60,  # Default scores
      loser_score = 30
    )
  } else {
    # Randomly sample one match
    selected_match <- similar_matches[sample(nrow(similar_matches), 1), ]
    
    # Return the winner and loser scores
    result <- list(
      winner_score = selected_match$w_dk_score,
      loser_score = selected_match$l_dk_score
    )
  }
  
  return(result)
}

pre_simulate_matches <- function(dk_data, historical_data, n_samples = 10000) {
  # Convert to data.table for performance
  dk_dt <- as.data.table(dk_data)
  hist_dt <- as.data.table(historical_data)
  
  # Create indexes on historical data for faster lookups - now including best_of
  setkey(hist_dt, Tour, best_of, straight_sets)
  
  # Get all unique matches
  matches <- unique(dk_dt$`Game Info`)
  
  # Create result storage
  all_match_outcomes <- list()
  
  # Process each match
  for (match_name in matches) {
    cat("Pre-simulating outcomes for match:", match_name, "\n")
    
    # Get players in this match
    match_players <- dk_dt[`Game Info` == match_name]
    
    if (nrow(match_players) != 2) {
      warning("Skipping match with invalid number of players:", match_name)
      next
    }
    
    # Extract player info
    p1 <- match_players[1]
    p2 <- match_players[2]
    
    # Check for walkover scenario
    p1_tour <- p1$Tour
    p2_tour <- p2$Tour
    
    # Check if this is a walkover match
    is_walkover <- any(c(p1_tour, p2_tour) %in% c("WD", "WO"))
    
    if (is_walkover) {
      cat(" - WALKOVER DETECTED")
      
      # Determine who withdraws and who gets the walkover win
      if (p1_tour == "WD" && p2_tour == "WO") {
        winner_name <- p2$Name
        loser_name <- p1$Name
        winner_score <- 30
        loser_score <- 0
      } else if (p1_tour == "WO" && p2_tour == "WD") {
        winner_name <- p1$Name
        loser_name <- p2$Name
        winner_score <- 30
        loser_score <- 0
      } else if (p1_tour == "WD") {
        winner_name <- p2$Name
        loser_name <- p1$Name
        winner_score <- 30
        loser_score <- 0
      } else if (p2_tour == "WD") {
        winner_name <- p1$Name
        loser_name <- p2$Name
        winner_score <- 30
        loser_score <- 0
      } else if (p1_tour == "WO") {
        winner_name <- p1$Name
        loser_name <- p2$Name
        winner_score <- 30
        loser_score <- 0
      } else if (p2_tour == "WO") {
        winner_name <- p2$Name
        loser_name <- p1$Name
        winner_score <- 30
        loser_score <- 0
      } else {
        cat(" - ERROR: Invalid walkover configuration")
        next
      }
      
      # Create walkover outcome (single outcome with probability 1.0)
      outcome_scores <- data.table(
        outcome_id = paste(winner_name, "WO", loser_name, sep = "_"),
        winner_name = winner_name,
        loser_name = loser_name,
        outcome_type = "WO",
        probability = 1.0,
        winner_score = rep(winner_score, n_samples),
        loser_score = rep(loser_score, n_samples)
      )
      
      all_match_outcomes[[match_name]] <- outcome_scores
      
      cat(sprintf(" (%s withdraws, %s gets walkover)", loser_name, winner_name))
      
    } else {
      # NORMAL MATCH LOGIC
      
      # Calculate ML probabilities
      p1_ml_prob <- odds_to_probability(as.numeric(p1$ML))
      p2_ml_prob <- odds_to_probability(as.numeric(p2$ML))
      
      # Normalize to sum to 1
      total_ml_prob <- p1_ml_prob + p2_ml_prob
      p1_ml_prob <- p1_ml_prob / total_ml_prob
      p2_ml_prob <- p2_ml_prob / total_ml_prob
      
      # Calculate SS probabilities
      p1_ss_prob <- odds_to_probability(as.numeric(p1$SS))
      p2_ss_prob <- odds_to_probability(as.numeric(p2$SS))
      
      # Ensure SS probability doesn't exceed win probability
      p1_ss_prob <- min(p1_ss_prob, p1_ml_prob)
      p2_ss_prob <- min(p2_ss_prob, p2_ml_prob)
      
      # Calculate NSS probabilities
      p1_nss_prob <- p1_ml_prob - p1_ss_prob
      p2_nss_prob <- p2_ml_prob - p2_ss_prob
      
      # Create four possible outcomes with their probabilities
      outcomes <- list(
        list(winner = p1$Name, loser = p2$Name, outcome = "SS", prob = p1_ss_prob),
        list(winner = p1$Name, loser = p2$Name, outcome = "NSS", prob = p1_nss_prob),
        list(winner = p2$Name, loser = p1$Name, outcome = "SS", prob = p2_ss_prob),
        list(winner = p2$Name, loser = p1$Name, outcome = "NSS", prob = p2_nss_prob)
      )
      
      # Pre-generate scores for each outcome
      match_scores <- list()
      
      for (outcome_info in outcomes) {
        # Extract outcome details
        winner <- outcome_info$winner
        loser <- outcome_info$loser
        outcome <- outcome_info$outcome
        probability <- outcome_info$prob
        winner_prob <- ifelse(winner == p1$Name, p1_ml_prob, p2_ml_prob)
        loser_prob <- ifelse(loser == p1$Name, p1_ml_prob, p2_ml_prob)
        
        # Only generate samples if probability > 0
        if (probability > 0) {
          # Create match info for similarity matching
          tour <- p1$Tour
          #best_of_value <- ifelse(tour == "ATP", 5, 3)
          best_of_value <- 3
          
          match_info <- list(
            Tour = tour,
            best_of = best_of_value,
            straight_sets = ifelse(outcome == "SS", 1, 0),
            WIO = winner_prob,
            LIO = loser_prob
          )
          
          # Find similar historical matches - now including best_of
          similar_matches <- hist_dt[
            Tour == match_info$Tour & 
              best_of == match_info$best_of &
              straight_sets == match_info$straight_sets
          ]
          
          # If not enough matches, relax straight_sets constraint but keep best_of
          if (nrow(similar_matches) < 10) {
            similar_matches <- hist_dt[
              Tour == match_info$Tour &
                best_of == match_info$best_of
            ]
          }
          
          # If still not enough matches, relax best_of constraint
          if (nrow(similar_matches) < 10) {
            similar_matches <- hist_dt[Tour == match_info$Tour]
          }
          
          # Generate n_samples scores
          outcome_scores <- data.table(
            outcome_id = paste(winner, outcome, loser, sep = "_"),
            winner_name = winner,
            loser_name = loser,
            outcome_type = outcome,
            probability = probability,
            winner_score = numeric(n_samples),
            loser_score = numeric(n_samples)
          )
          
          if (nrow(similar_matches) > 0) {
            # Calculate similarity scores
            similar_matches[, odds_diff := abs(WIO - winner_prob) + abs(LIO - loser_prob)]
            setorder(similar_matches, odds_diff)
            
            # Take top 50 most similar matches
            sample_size <- min(50, nrow(similar_matches))
            top_matches <- similar_matches[1:sample_size]
            
            # Sample n_samples scores with replacement
            sample_indices <- sample(1:sample_size, n_samples, replace = TRUE)
            
            for (i in 1:n_samples) {
              idx <- sample_indices[i]
              outcome_scores$winner_score[i] <- top_matches$w_dk_score[idx]
              outcome_scores$loser_score[i] <- top_matches$l_dk_score[idx]
            }
          } else {
            # Default scores if no similar matches
            outcome_scores$winner_score <- runif(n_samples, 50, 70)
            outcome_scores$loser_score <- runif(n_samples, 20, 40)
          }
          
          # Add to match scores
          match_scores[[length(match_scores) + 1]] <- outcome_scores
        }
      }
      
      # Combine all outcomes for this match
      if (length(match_scores) > 0) {
        all_match_outcomes[[match_name]] <- rbindlist(match_scores)
      }
    }
  }
  
  return(all_match_outcomes)
}

# Fast simulation using pre-generated scores
run_fast_simulation <- function(dk_data, pre_simulated_outcomes, n_simulations = 10000) {
  # Get all matches
  matches <- names(pre_simulated_outcomes)
  
  # Create result storage with pre-allocated memory
  estimated_rows <- n_simulations * length(matches) * 2
  results <- data.table(
    Iteration = integer(estimated_rows),
    Match = character(estimated_rows),
    Player = character(estimated_rows),
    Result = character(estimated_rows),
    Outcome = character(estimated_rows),
    Score = numeric(estimated_rows)
  )
  
  # Track current position in results
  row_idx <- 1
  
  # Run the simulations
  for (iter in 1:n_simulations) {
    if (iter %% 1000 == 0) {
      cat("Running simulation", iter, "of", n_simulations, "\n")
    }
    
    # For each match
    for (match_name in matches) {
      match_outcomes <- pre_simulated_outcomes[[match_name]]
      
      if (is.null(match_outcomes) || nrow(match_outcomes) == 0) {
        next
      }
      
      # Sample an outcome based on probabilities
      outcome_probs <- match_outcomes$probability
      outcome_probs <- outcome_probs / sum(outcome_probs)  # Ensure they sum to 1
      
      selected_outcome_idx <- sample(1:nrow(match_outcomes), 1, prob = outcome_probs)
      selected_outcome <- match_outcomes[selected_outcome_idx]
      
      # Sample a random score from this outcome type
      same_outcome_samples <- match_outcomes[outcome_id == selected_outcome$outcome_id]
      score_idx <- sample(1:nrow(same_outcome_samples), 1)
      
      winner <- selected_outcome$winner_name
      loser <- selected_outcome$loser_name
      outcome_type <- selected_outcome$outcome_type
      winner_score <- same_outcome_samples$winner_score[score_idx]
      loser_score <- same_outcome_samples$loser_score[score_idx]
      
      # Add winner result
      results[row_idx, `:=`(
        Iteration = iter,
        Match = match_name,
        Player = winner,
        Result = "Winner",
        Outcome = outcome_type,
        Score = winner_score
      )]
      row_idx <- row_idx + 1
      
      # Add loser result
      results[row_idx, `:=`(
        Iteration = iter,
        Match = match_name,
        Player = loser,
        Result = "Loser",
        Outcome = outcome_type,
        Score = loser_score
      )]
      row_idx <- row_idx + 1
    }
  }
  
  # Trim unfilled rows
  results <- results[1:(row_idx-1)]
  
  return(results)
}

# Main simulation function to replace your current run_simulation
run_optimized_simulation <- function(dk_data, historical_data, n_simulations = 10000) {
  # Step 1: Pre-simulate all match outcomes once
  pre_simulated <- pre_simulate_matches(
    dk_data, 
    historical_data, 
    n_samples = max(1000, n_simulations / 10)  # Generate enough samples
  )
  
  # Step 2: Run the fast simulation using pre-simulated outcomes
  results <- run_fast_simulation(
    dk_data,
    pre_simulated,
    n_simulations
  )
  
  return(results)
}


# Function to analyze player scores
analyze_player_scores <- function(simulation_results, dk_data) {
  # Check if we have the expected column structure
  if (!"Player" %in% names(simulation_results) || !"Score" %in% names(simulation_results)) {
    stop("Simulation results must include Player and Score columns")
  }
  
  # Analyze scores by player
  player_stats <- simulation_results %>%
    group_by(Player) %>%
    summarize(
      AvgScore = mean(Score, na.rm = TRUE),
      MedianScore = median(Score, na.rm = TRUE),
      StdDev = sd(Score, na.rm = TRUE),
      Min = min(Score, na.rm = TRUE),
      Max = max(Score, na.rm = TRUE),
      n_samples = n(),
      WinPct = mean(Result == "Winner", na.rm = TRUE),
      StraightSetsPct = mean(Result == "Winner" & Outcome == "SS", na.rm = TRUE)
    )
  
  # Add salary info
  player_stats <- left_join(
    player_stats,
    dk_data[, c("Name", "ID", "DKOwn","Salary", "ML")],
    by = c("Player" = "Name")
  )
  
  return(player_stats)
}

# Function to find optimal lineup
find_optimal_lineup <- function(player_scores, salary_cap = 50000, roster_size = 6) {
  # Ensure player_scores has required columns
  if (!all(c("Player", "Score", "Salary") %in% colnames(player_scores))) {
    stop("player_scores must contain Player, Score, and Salary columns")
  }
  
  # Set up lpSolve
  obj <- player_scores$Score
  
  # Constraints
  const.mat <- matrix(c(
    player_scores$Salary,  # Salary constraint
    rep(1, nrow(player_scores))  # Roster size constraint
  ), nrow = 2, byrow = TRUE)
  
  const.dir <- c("<=", "==")
  const.rhs <- c(salary_cap, roster_size)
  
  # Solve
  result <- lp("max", obj, const.mat, const.dir, const.rhs, all.bin = TRUE)
  
  if (result$status != 0) {
    warning("No feasible lineup found")
    return(NULL)
  }
  
  # Extract solution
  selected_indices <- which(result$solution > 0.5)
  optimal_lineup <- player_scores[selected_indices, ]
  
  return(optimal_lineup)
}

find_all_optimal_lineups <- function(simulation_results, player_data, dk_data = NULL) {
  overall_start_time <- Sys.time()
  cat("\n=== LINEUP OPTIMIZATION STARTED ===\n")
  cat("Timestamp:", format(overall_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Make sure we're working with data.tables
  sim_dt <- as.data.table(simulation_results)
  player_dt <- as.data.table(player_data)
  
  # Get unique iterations
  iterations <- unique(sim_dt$Iteration)
  total_iterations <- length(iterations)
  
  cat(sprintf("Processing %s iterations to find optimal lineups\n", format(total_iterations, big.mark = ",")))
  
  # Use smaller batch sizes for better progress reporting
  batch_size <- min(500, total_iterations)
  n_batches <- ceiling(total_iterations / batch_size)
  
  cat(sprintf("Using batch size of %d iterations (%d batches total)\n\n", batch_size, n_batches))
  
  # Create a dictionary to track lineups more efficiently
  lineup_dict <- new.env(hash = TRUE, parent = emptyenv())
  
  # Create a salary lookup map for faster access
  name_col <- "Name"
  if (!"Name" %in% names(player_dt)) {
    if ("Player" %in% names(player_dt)) {
      name_col <- "Player"
    } else {
      stop("Player data must contain either 'Name' or 'Player' column")
    }
  }
  
  player_salary_map <- setNames(player_dt$Salary, player_dt[[name_col]])
  
  # LP solver cache for avoiding redundant problems
  lp_cache <- new.env(hash = TRUE, parent = emptyenv())
  
  # Process iterations in batches with detailed timing
  for (batch in 1:n_batches) {
    batch_start_time <- Sys.time()
    
    # Calculate batch start and end indices
    start_iter <- (batch - 1) * batch_size + 1
    end_iter <- min(batch * batch_size, total_iterations)
    actual_batch_size <- end_iter - start_iter + 1
    
    cat(sprintf("[%s] Processing batch %d/%d (iterations %s-%s, %d iterations)",
                format(batch_start_time, "%H:%M:%S"),
                batch, n_batches, 
                format(start_iter, big.mark = ","), 
                format(end_iter, big.mark = ","),
                actual_batch_size))
    
    # Get iterations for this batch
    batch_iterations <- iterations[start_iter:end_iter]
    
    # Get all batch data at once to reduce filtering operations
    batch_results <- sim_dt[Iteration %in% batch_iterations]
    
    # Track lineups found in this batch
    batch_lineups_found <- 0
    
    # Process each iteration in the batch
    for (iter in batch_iterations) {
      # Get results for this iteration more efficiently
      iter_results <- batch_results[Iteration == iter]
      
      # Create player scores for this iteration
      player_scores <- iter_results[, .(Score = sum(Score)), by = Player]
      
      # Add salary info using the map
      player_scores[, Salary := player_salary_map[Player]]
      
      # Remove players with NA salaries
      player_scores <- player_scores[!is.na(Salary)]
      
      # Skip if not enough players
      if (nrow(player_scores) < 6) next
      
      # Calculate win counts for this iteration for later lineup analysis
      win_counts <- iter_results[Result == "Winner" & Outcome != "WO", .N, by = Player]
      setnames(win_counts, "N", "wins")
      
      # Find top 5 lineups for this iteration
      top_lineups <- list()
      players_used <- character()
      
      for (i in 1:5) {
        # Skip if we've used too many players already
        if (length(players_used) > nrow(player_scores) - 6) break
        
        # Filter out already used players
        available_players <- player_scores[!Player %in% players_used]
        if (nrow(available_players) < 6) break
        
        # Check LP cache first
        lp_key <- paste(
          paste(sort(available_players$Player), collapse="|"),
          paste(available_players$Salary, collapse="|"),
          paste(available_players$Score, collapse="|"),
          sep="||"
        )
        
        if (exists(lp_key, envir = lp_cache)) {
          optimal <- get(lp_key, envir = lp_cache)
        } else {
          # Find optimal lineup among available players
          result <- lp("max", 
                       available_players$Score, 
                       rbind(available_players$Salary, rep(1, nrow(available_players))), 
                       c("<=", "=="), 
                       c(50000, 6), 
                       all.bin = TRUE)
          
          if (result$status != 0) break
          
          optimal <- available_players[result$solution > 0.5]
          assign(lp_key, optimal, envir = lp_cache)
        }
        
        if (nrow(optimal) == 6) {
          # Create lineup ID
          lineup_id <- paste(sort(optimal$Player), collapse = "|")
          
          # Calculate total wins for this lineup in this iteration
          lineup_players <- optimal$Player
          total_wins <- sum(win_counts[Player %in% lineup_players, wins], na.rm = TRUE)
          
          # Add to tracking with win count information
          top_lineups[[i]] <- list(
            id = lineup_id, 
            rank = i, 
            total_wins = total_wins,
            players = lineup_players
          )
          
          # Mark these players as used
          players_used <- c(players_used, optimal$Player)
          batch_lineups_found <- batch_lineups_found + 1
        }
      }
      
      # Update the counters for each lineup
      for (lineup in top_lineups) {
        lineup_id <- lineup$id
        rank <- lineup$rank
        total_wins <- lineup$total_wins
        
        # Check if lineup exists in dictionary
        if (exists(lineup_id, envir = lineup_dict)) {
          # Get current counts
          current_counts <- get(lineup_id, envir = lineup_dict)
          
          # Update counts
          current_counts$Count <- current_counts$Count + 1
          
          # Update top counts based on rank
          if (rank == 1) {
            current_counts$Top1Count <- current_counts$Top1Count + 1
            current_counts$Top2Count <- current_counts$Top2Count + 1
            current_counts$Top3Count <- current_counts$Top3Count + 1
            current_counts$Top5Count <- current_counts$Top5Count + 1
          } else if (rank == 2) {
            current_counts$Top2Count <- current_counts$Top2Count + 1
            current_counts$Top3Count <- current_counts$Top3Count + 1
            current_counts$Top5Count <- current_counts$Top5Count + 1
          } else if (rank == 3) {
            current_counts$Top3Count <- current_counts$Top3Count + 1
            current_counts$Top5Count <- current_counts$Top5Count + 1
          } else if (rank <= 5) {
            current_counts$Top5Count <- current_counts$Top5Count + 1
          }
          
          # Update in dictionary
          assign(lineup_id, current_counts, envir = lineup_dict)
        } else {
          # Create new entry
          new_counts <- list(
            Count = 1,
            Top1Count = ifelse(rank == 1, 1, 0),
            Top2Count = ifelse(rank <= 2, 1, 0),
            Top3Count = ifelse(rank <= 3, 1, 0),
            Top5Count = ifelse(rank <= 5, 1, 0),
            players = lineup$players  # Store player names for later score calculation
          )
          
          # Add to dictionary
          assign(lineup_id, new_counts, envir = lineup_dict)
        }
      }
    }
    
    # Calculate and display batch timing
    batch_end_time <- Sys.time()
    batch_elapsed <- difftime(batch_end_time, batch_start_time, units = "secs")
    
    # Calculate estimated time remaining
    if (batch > 1) {
      avg_batch_time <- difftime(batch_end_time, overall_start_time, units = "secs") / batch
      estimated_remaining <- avg_batch_time * (n_batches - batch)
      
      cat(sprintf(" - COMPLETED in %.2f seconds (~%.1f sec remaining, %d lineups found)\n",
                  as.numeric(batch_elapsed), as.numeric(estimated_remaining), batch_lineups_found))
    } else {
      cat(sprintf(" - COMPLETED in %.2f seconds (%d lineups found)\n",
                  as.numeric(batch_elapsed), batch_lineups_found))
    }
    
    # Force garbage collection after each batch
    if (batch %% 3 == 0) {
      cleanup_memory(verbose = FALSE)
    }
  }
  
  cat("\n=== CONVERTING LINEUP DICTIONARY TO TABLE ===\n")
  conversion_start_time <- Sys.time()
  
  # Convert dictionary to data.table efficiently
  lineup_ids <- ls(lineup_dict)
  total_unique_lineups <- length(lineup_ids)
  
  cat(sprintf("Found %s unique lineups, converting to data table...\n", 
              format(total_unique_lineups, big.mark = ",")))
  
  # Create final data.table with score statistics columns
  lineup_counter <- data.table(
    LineupID = lineup_ids,
    Count = integer(length(lineup_ids)),
    Top1Count = integer(length(lineup_ids)),
    Top2Count = integer(length(lineup_ids)),
    Top3Count = integer(length(lineup_ids)),
    Top5Count = integer(length(lineup_ids)),
    TotalEW = numeric(length(lineup_ids)),
    MedianScore = numeric(length(lineup_ids)),
    Score80th = numeric(length(lineup_ids)),
    Win6Pct = numeric(length(lineup_ids)),
    Win5PlusPct = numeric(length(lineup_ids))
  )
  
  # Fill the basic counts first
  for (i in seq_along(lineup_ids)) {
    lineup_id <- lineup_ids[i]
    counts <- get(lineup_id, envir = lineup_dict)
    
    set(lineup_counter, i, 2L, counts$Count)
    set(lineup_counter, i, 3L, counts$Top1Count)
    set(lineup_counter, i, 4L, counts$Top2Count)
    set(lineup_counter, i, 5L, counts$Top3Count)
    set(lineup_counter, i, 6L, counts$Top5Count)
  }
  
  cat("\n=== CALCULATING EXPECTED WINS (EW) FOR ALL LINEUPS ===\n")
  ew_calc_start_time <- Sys.time()
  
  # Calculate EW for each lineup using constraint-aware method
  cat("Calculating EW metrics from simulation results...\n")
  ew_metrics <- tryCatch({
    calculate_ew_metrics(sim_dt, total_iterations, dk_data)  # Use the passed dk_data
  }, error = function(e) {
    cat("Error calculating EW metrics:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(ew_metrics)) {
    # Create EW lookup for individual players
    ew_lookup <- setNames(ew_metrics$EW, ew_metrics$Player)
    cat(sprintf("EW metrics calculated for %d players\n", length(ew_lookup)))
    
    # Create player-to-match mapping if dk_data is available
    if (!is.null(dk_data)) {
      dk_dt <- as.data.table(dk_data)
      player_match_map <- setNames(dk_dt$`Game Info`, dk_dt$Name)
      cat("Using constraint-aware EW calculation (accounts for same-match pairs)\n")
    } else {
      player_match_map <- NULL
      cat("Using simple additive EW calculation\n")
    }
    
    # Calculate TotalEW for each lineup
    for (i in seq_along(lineup_ids)) {
      lineup_id <- lineup_ids[i]
      counts <- get(lineup_id, envir = lineup_dict)
      
      # Get the players in this lineup
      if (!is.null(counts$players)) {
        lineup_players <- counts$players
      } else {
        lineup_players <- unlist(strsplit(lineup_id, "\\|"))
      }
      
      # Calculate total EW using constraint-aware method if mapping available
      if (!is.null(player_match_map)) {
        total_ew <- calculate_lineup_total_ew(lineup_players, ew_metrics, player_match_map)
      } else {
        # Fallback to simple additive method
        player_ew <- ew_lookup[lineup_players]
        player_ew[is.na(player_ew)] <- 0
        total_ew <- sum(player_ew, na.rm = TRUE)
      }
      
      set(lineup_counter, i, 7L, total_ew)  # TotalEW
      
      # Show progress
      if (i %% 1000 == 0) {
        cat(sprintf("Calculated EW for %s/%s lineups (%.1f%%)\n", 
                    format(i, big.mark = ","), 
                    format(total_unique_lineups, big.mark = ","),
                    (i / total_unique_lineups) * 100))
      }
    }
    
    ew_calc_elapsed <- difftime(Sys.time(), ew_calc_start_time, units = "secs")
    cat(sprintf("EW calculation completed in %.1f seconds\n", as.numeric(ew_calc_elapsed)))
  } else {
    cat("WARNING: Could not calculate EW metrics, setting all TotalEW to 0\n")
    lineup_counter[, TotalEW := 0]
  }
  
  cat("\n=== FILTERING TO TOP 10,000 LINEUPS BY EW ===\n")
  # Sort by TotalEW and keep only top 10k
  setorder(lineup_counter, -TotalEW, -Top1Count)
  
  original_count <- nrow(lineup_counter)
  lineup_counter <- lineup_counter[1:min(25000, nrow(lineup_counter))]
  filtered_count <- nrow(lineup_counter)
  
  cat(sprintf("Filtered from %s to %s lineups (top 10k by EW)\n", 
              format(original_count, big.mark = ","),
              format(filtered_count, big.mark = ",")))
  cat(sprintf("EW range: %.3f to %.3f\n", 
              min(lineup_counter$TotalEW, na.rm = TRUE),
              max(lineup_counter$TotalEW, na.rm = TRUE)))
  
  cat("\n=== CALCULATING SCORE STATISTICS ACROSS ALL ITERATIONS ===\n")
  score_calc_start_time <- Sys.time()
  cat(sprintf("Calculating scores for TOP %s lineups (filtered by EW) across %s iterations\n", 
              format(nrow(lineup_counter), big.mark = ","),
              format(total_iterations, big.mark = ",")))
  
  # OPTIMIZATION: Pre-calculate all player scores for all iterations once
  cat("Pre-calculating player scores for all iterations (this may take a moment)...\n")
  
  # Create a matrix: rows = iterations, cols = players
  all_players <- unique(sim_dt$Player)
  player_scores_matrix <- matrix(0, nrow = total_iterations, ncol = length(all_players))
  colnames(player_scores_matrix) <- all_players
  
  # Fill the matrix efficiently
  for (iter_idx in seq_along(iterations)) {
    iter <- iterations[iter_idx]
    iter_results <- sim_dt[Iteration == iter]
    
    # Calculate player scores for this iteration
    player_iter_scores <- iter_results[, .(Score = sum(Score)), by = Player]
    
    # Fill the matrix row
    for (j in 1:nrow(player_iter_scores)) {
      player_name <- player_iter_scores$Player[j]
      if (player_name %in% all_players) {
        player_scores_matrix[iter_idx, player_name] <- player_iter_scores$Score[j]
      }
    }
    
    # Show progress for pre-calculation
    if (iter_idx %% 1000 == 0) {
      cat(sprintf("Pre-calculated scores for %s/%s iterations (%.1f%%)\n", 
                  format(iter_idx, big.mark = ","), 
                  format(total_iterations, big.mark = ","),
                  (iter_idx / total_iterations) * 100))
    }
  }
  
  # Pre-calculate win counts matrix too
  cat("Pre-calculating win counts for all iterations...\n")
  win_counts_matrix <- matrix(0, nrow = total_iterations, ncol = length(all_players))
  colnames(win_counts_matrix) <- all_players
  
  for (iter_idx in seq_along(iterations)) {
    iter <- iterations[iter_idx]
    iter_results <- sim_dt[Iteration == iter]
    
    # Calculate win counts for this iteration
    win_counts <- iter_results[Result == "Winner" & Outcome != "WO", .N, by = Player]
    
    # Fill the matrix row
    for (j in 1:nrow(win_counts)) {
      player_name <- win_counts$Player[j]
      if (player_name %in% all_players) {
        win_counts_matrix[iter_idx, player_name] <- win_counts$N[j]
      }
    }
    
    # Show progress
    if (iter_idx %% 1000 == 0) {
      cat(sprintf("Pre-calculated wins for %s/%s iterations (%.1f%%)\n", 
                  format(iter_idx, big.mark = ","), 
                  format(total_iterations, big.mark = ","),
                  (iter_idx / total_iterations) * 100))
    }
  }
  
  cat("Pre-calculation complete! Now calculating lineup statistics...\n\n")
  
  # NOW: Calculate lineup statistics using matrix operations (MUCH faster)
  for (i in 1:nrow(lineup_counter)) {
    lineup_id <- lineup_counter$LineupID[i]
    counts <- get(lineup_id, envir = lineup_dict)
    
    # Get the players in this lineup
    if (!is.null(counts$players)) {
      lineup_players <- counts$players
    } else {
      # Fallback: parse from lineup_id
      lineup_players <- unlist(strsplit(lineup_id, "\\|"))
    }
    
    # Filter to players that exist in our matrices
    valid_players <- lineup_players[lineup_players %in% all_players]
    
    if (length(valid_players) >= 6) {
      # Use only first 6 if more than 6
      valid_players <- valid_players[1:6]
      
      # Calculate lineup scores across all iterations using matrix operations
      lineup_scores <- rowSums(player_scores_matrix[, valid_players, drop = FALSE])
      lineup_wins <- rowSums(win_counts_matrix[, valid_players, drop = FALSE])
      
      # Calculate score statistics (median and 80th percentile only)
      set(lineup_counter, i, 8L, median(lineup_scores, na.rm = TRUE))      # MedianScore
      set(lineup_counter, i, 9L, quantile(lineup_scores, 0.8, na.rm = TRUE)) # Score80th
      
      # Calculate win percentages across ALL iterations
      win6_pct <- (sum(lineup_wins >= 6) / total_iterations) * 100
      win5plus_pct <- (sum(lineup_wins >= 5) / total_iterations) * 100
      
      set(lineup_counter, i, 10L, win6_pct)      # Win6Pct
      set(lineup_counter, i, 11L, win5plus_pct)  # Win5PlusPct
    } else {
      # Set default values if not enough valid players
      set(lineup_counter, i, 8L, 0)   # MedianScore
      set(lineup_counter, i, 9L, 0)   # Score80th
      set(lineup_counter, i, 10L, 0)  # Win6Pct
      set(lineup_counter, i, 11L, 0)  # Win5PlusPct
    }
    
    # Show progress with timing estimates (more frequent updates)
    if (i %% 1000 == 0 || i == nrow(lineup_counter)) {
      current_time <- Sys.time()
      elapsed_time <- difftime(current_time, score_calc_start_time, units = "secs")
      
      if (i > 0) {
        avg_time_per_lineup <- as.numeric(elapsed_time) / i
        estimated_total_time <- avg_time_per_lineup * nrow(lineup_counter)
        estimated_remaining <- estimated_total_time - as.numeric(elapsed_time)
        
        cat(sprintf("[%s] Calculated scores for %s/%s lineups (%.1f%%) - %.1fs elapsed, ~%.1fs remaining\n", 
                    format(current_time, "%H:%M:%S"),
                    format(i, big.mark = ","), 
                    format(nrow(lineup_counter), big.mark = ","),
                    (i / nrow(lineup_counter)) * 100,
                    as.numeric(elapsed_time),
                    estimated_remaining))
      }
    }
  }
  
  # Calculate frequencies and percentages correctly
  lineup_counter[, Frequency := Count / total_iterations * 100]
  
  # Sort by TotalEW (already sorted, but re-sort in case)
  setorder(lineup_counter, -TotalEW, -Top1Count)
  
  # Report final timing
  overall_end_time <- Sys.time()
  total_elapsed <- difftime(overall_end_time, overall_start_time, units = "mins")
  conversion_elapsed <- difftime(overall_end_time, conversion_start_time, units = "secs")
  
  cat("\n=== LINEUP OPTIMIZATION COMPLETED ===\n")
  cat(sprintf("Total optimization time: %.2f minutes\n", as.numeric(total_elapsed)))
  cat(sprintf("Final conversion time: %.2f seconds\n", as.numeric(conversion_elapsed)))
  cat(sprintf("Generated %s unique optimal lineups\n", format(nrow(lineup_counter), big.mark = ",")))
  cat("Timestamp:", format(overall_end_time, "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  return(lineup_counter)
}

expand_lineup_details <- function(lineup_stats, player_data, ew_metrics = NULL) {
  start_time <- Sys.time()
  cat("\n=== EXPANDING LINEUP DETAILS ===\n")
  cat("Timestamp:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Convert to regular data.frame to avoid data.table issues
  lineup_df <- as.data.frame(lineup_stats)
  player_df <- as.data.frame(player_data)
  
  total_lineups <- nrow(lineup_df)
  cat(sprintf("Expanding details for %s lineups\n", format(total_lineups, big.mark = ",")))
  
  # Make sure we have the right name column in player data
  name_col <- "Name"
  if (!"Name" %in% names(player_df)) {
    if ("Player" %in% names(player_df)) {
      name_col <- "Player"
    } else {
      stop("Player data must contain either 'Name' or 'Player' column")
    }
  }
  
  # Create a fast player name to salary lookup
  player_salary_map <- setNames(player_df$Salary, player_df[[name_col]])
  
  # Start with the existing lineup data and add new columns
  expanded_lineups <- lineup_df
  
  # Add TotalSalary column if it doesn't exist
  if (!"TotalSalary" %in% names(expanded_lineups)) {
    expanded_lineups$TotalSalary <- numeric(nrow(expanded_lineups))
  }
  
  # Add player name and salary columns if they don't exist
  for (j in 1:6) {
    name_col_name <- paste0("Name", j)
    salary_col_name <- paste0("Salary", j)
    
    if (!name_col_name %in% names(expanded_lineups)) {
      expanded_lineups[[name_col_name]] <- character(nrow(expanded_lineups))
    }
    if (!salary_col_name %in% names(expanded_lineups)) {
      expanded_lineups[[salary_col_name]] <- numeric(nrow(expanded_lineups))
    }
  }
  
  # Use smaller batch sizes for more frequent progress updates
  batch_size <- min(250, nrow(lineup_df))
  n_batches <- ceiling(nrow(lineup_df) / batch_size)
  
  cat(sprintf("Using batch size of %d lineups (%d batches total)\n\n", batch_size, n_batches))
  
  # Process each lineup with batch reporting
  for (batch in 1:n_batches) {
    batch_start_time <- Sys.time()
    
    # Calculate batch range
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, nrow(lineup_df))
    actual_batch_size <- end_idx - start_idx + 1
    
    cat(sprintf("[%s] Processing expansion batch %d/%d (lineups %s-%s, %d lineups)",
                format(batch_start_time, "%H:%M:%S"),
                batch, n_batches,
                format(start_idx, big.mark = ","),
                format(end_idx, big.mark = ","),
                actual_batch_size))
    
    # Process each lineup in this batch
    for (i in start_idx:end_idx) {
      # Split the lineup ID to get player names
      player_names <- unlist(strsplit(lineup_df$LineupID[i], "\\|"))
      
      # Get salaries directly from the map
      player_salaries <- player_salary_map[player_names]
      player_salaries[is.na(player_salaries)] <- 0
      
      # Calculate total salary
      total_salary <- sum(player_salaries, na.rm = TRUE)
      expanded_lineups$TotalSalary[i] <- total_salary
      
      # Sort players by salary (descending)
      sorted_indices <- order(player_salaries, decreasing = TRUE)
      sorted_players <- player_names[sorted_indices]
      sorted_salaries <- player_salaries[sorted_indices]
      
      # Add player details to the lineup
      for (j in 1:min(length(sorted_players), 6)) {
        name_col <- paste0("Name", j)
        salary_col <- paste0("Salary", j)
        
        expanded_lineups[[name_col]][i] <- sorted_players[j]
        expanded_lineups[[salary_col]][i] <- sorted_salaries[j]
      }
    }
    
    # Calculate and display batch timing
    batch_end_time <- Sys.time()
    batch_elapsed <- difftime(batch_end_time, batch_start_time, units = "secs")
    
    # Calculate estimated time remaining
    if (batch > 1) {
      avg_batch_time <- difftime(batch_end_time, start_time, units = "secs") / batch
      estimated_remaining <- avg_batch_time * (n_batches - batch)
      
      cat(sprintf(" - COMPLETED in %.2f seconds (~%.1f sec remaining)\n",
                  as.numeric(batch_elapsed), as.numeric(estimated_remaining)))
    } else {
      cat(sprintf(" - COMPLETED in %.2f seconds\n", as.numeric(batch_elapsed)))
    }
    
    # Clean up memory occasionally
    if (batch %% 5 == 0) {
      cleanup_memory(verbose = FALSE)
    }
  }
  
  # Report final timing
  end_time <- Sys.time()
  total_elapsed <- difftime(end_time, start_time, units = "secs")
  
  cat("\n=== LINEUP DETAIL EXPANSION COMPLETED ===\n")
  cat(sprintf("Total expansion time: %.2f seconds\n", as.numeric(total_elapsed)))
  cat(sprintf("Processed %s lineups\n", format(nrow(expanded_lineups), big.mark = ",")))
  
  # Final debug output
  cat("Final columns in expanded_lineups:", paste(names(expanded_lineups), collapse = ", "), "\n")
  if ("TotalEW" %in% names(expanded_lineups)) {
    cat("TotalEW column exists\n")
    cat("TotalEW range:", min(expanded_lineups$TotalEW, na.rm=TRUE), "to", max(expanded_lineups$TotalEW, na.rm=TRUE), "\n")
  }
  
  # Check for score columns
  score_cols <- c("MedianScore", "Score80th", "Win6Pct", "Win5PlusPct")
  existing_score_cols <- intersect(score_cols, names(expanded_lineups))
  cat("Score columns preserved:", paste(existing_score_cols, collapse = ", "), "\n")
  
  cat("Timestamp:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  return(expanded_lineups)
}
# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(title = "Tennis DFS Simulator"),
  
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
      menuItem("Match Analysis", tabName = "match_analysis", icon = icon("chart-line")),
      menuItem("Fantasy Projections", tabName = "player_projections", icon = icon("calculator")),
      menuItem("Optimal Lineups", tabName = "optimal_lineups", icon = icon("trophy")),
      menuItem("Lineup Builder", tabName = "lineup_builder", icon = icon("percentage"))
    ),
    br(),
    fileInput("dk_file", "Upload DraftKings File", accept = c(".xlsx")),
    conditionalPanel(
      condition = "window.SCORE_HISTORY_LOADED === false",
      fileInput("score_history", "Upload Score History", accept = c(".csv"))
    ),
    numericInput("n_sims", "Number of Simulations:", value = 10000, min = 100, max = 500000),
    actionButton("run_sim", "Run Simulation", class = "btn-primary", style = "margin: 15px; width: 90%"),
    div(id = "sim_status", class = "text-center", style = "margin-top: 10px;")
  ),
  
  # Dashboard body
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css)),
      # Pass variable to JavaScript to check if historical data is loaded
      tags$script(HTML(paste0(
        "window.SCORE_HISTORY_LOADED = ", !is.null(SCORE_HISTORY), ";"
      )))
    ),
    
    tabItems(
      # Input Data Tab
      tabItem(tabName = "upload",
              fluidRow(
                box(width = 12,
                    title = "DraftKings Data Preview",
                    DTOutput("dk_data_preview") %>% 
                      withSpinner(color = "#ff6600")
                )
              )
      ),
      
      # Match Analysis Tab
      tabItem(tabName = "match_analysis",
              fluidRow(
                box(width = 12,
                    title = "Simulation vs Implied Probabilities",
                    DTOutput("sim_vs_implied_table") %>% 
                      withSpinner(color = "#FFD700")
                )
              )
      ),
      
      # Player Projections Tab
      tabItem(tabName = "player_projections",
              fluidRow(
                box(width = 12,
                    title = "Player Fantasy Point Projections",
                    DTOutput("player_projections") %>% 
                      withSpinner(color = "#FFD700"),
                    downloadButton('download_projections', 'Download Projections')
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Fantasy Points vs Salary",
                    plotlyOutput("points_salary_plot") %>% 
                      withSpinner(color = "#FFD700")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Player Score Distribution",
                    plotlyOutput("player_score_dist") %>% 
                      withSpinner(color = "#FFD700")
                )
              )
      ),
      
      # Optimal Lineups Tab
      tabItem(tabName = "optimal_lineups",
              fluidRow(
                box(
                  width = 12, 
                  title = "Calculate Optimal Lineups",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  fluidRow(
                    column(6,
                           actionButton("run_dk_optimization", "Calculate Optimal Lineups",
                                        class = "btn-primary", 
                                        style = "width: 100%; margin-top: 10px; padding: 15px; font-size: 16px;")
                    )
                  )
                )
              ),
              
              # Weighted Ranking Controls - only shown when lineups are available
              conditionalPanel(
                condition = "output.optimization_complete === 'true'",
                fluidRow(
                  box(
                    width = 12,
                    title = "Weighted Ranking Controls",
                    status = "info",
                    solidHeader = TRUE,
                    
                    h4("Adjust weights for each metric (0 = ignore, higher = more important)"),
                    
                    fluidRow(
                      column(3,
                             sliderInput("weight_totalew", "TotalEW Weight:", 
                                         min = 0, max = 10, value = 2,  step = 1)
                      ),
                      column(3,
                             sliderInput("weight_medianscore", "Median Score Weight:", 
                                         min = 0, max = 10, value = 0,  step = 1)
                      ),
                      column(3,
                             sliderInput("weight_score80th", "80th %ile Score Weight:", 
                                         min = 0, max = 10, value = 0,  step = 1)
                      ),
                      column(3,
                             sliderInput("weight_win6pct", "6-Win % Weight:", 
                                         min = 0, max = 10, value = 1,  step = 1)
                      )
                    ),
                    
                    fluidRow(
                      column(3,
                             sliderInput("weight_win5pluspct", "5+ Win % Weight:", 
                                         min = 0, max = 10, value = 1,  step = 1)
                      ),
                      column(3,
                             sliderInput("weight_top1count", "Top 1 Count Weight:", 
                                         min = 0, max = 10, value = 1,  step = 1)
                      ),
                      column(3,
                             sliderInput("weight_top2count", "Top 2 Count Weight:", 
                                         min = 0, max = 10, value = 0,  step = 1)
                      ),
                      column(3,
                             sliderInput("weight_top3count", "Top 3 Count Weight:", 
                                         min = 0, max = 10, value = 0,  step = 1)
                      )
                    ),
                    
                    fluidRow(
                      column(3,
                             sliderInput("weight_top5count", "Top 5 Count Weight:", 
                                         min = 0, max = 10, value = 0, step = 1)  
                      ),
                      column(3,
                             actionButton("apply_weights", "Apply Weights & Re-rank", 
                                          class = "btn-info", 
                                          style = "margin-top: 25px; width: 100%;")
                      ),
                      column(3,
                             actionButton("reset_weights", "Reset to Defaults", 
                                          class = "btn-warning", 
                                          style = "margin-top: 25px; width: 100%;")
                      )
                    ),
                    
                    div(style = "margin-top: 15px;",
                        verbatimTextOutput("weight_summary")
                    )
                  )
                )
              ),
              
              # Optimal lineups results section - only shown when lineups are available
              conditionalPanel(
                condition = "output.optimization_complete === 'true'",
                fluidRow(
                  box(
                    width = 12,
                    title = "Optimal Lineups",
                    div(
                      style = "text-align: right; margin-bottom: 10px;",
                      downloadButton('download_optimal_lineups', 'Download All Lineups',
                                     style = "margin-top: 10px;")
                    ),
                    DTOutput("optimal_lineups_table") %>% withSpinner(color = "#FFD700")
                  )
                )
              )
      ),
      
      # Lineup Builder Tab
      tabItem(tabName = "lineup_builder",
              conditionalPanel(
                condition = "!output.optimization_complete",
                fluidRow(
                  box(
                    width = 12,
                    status = "warning",
                    title = "No Optimal Lineups Available",
                    "Please calculate optimal lineups in the Optimal Lineups tab first."
                  )
                )
              ),
              conditionalPanel(
                condition = "output.optimization_complete === 'true'",
                fluidRow(
                  box(width = 12,
                      title = "Lineup Selection Controls",
                      status = "primary",
                      solidHeader = TRUE,
                      
                      h4("Select lineups from the top-ranked pool using your weighted ranking"),
                      
                      fluidRow(
                        column(4,
                               numericInput("top_lineups_pool", 
                                            "Top X Lineups to Select From:", 
                                            value = 500, 
                                            min = 10, 
                                            max = 10000, 
                                            step = 10)
                        ),
                        column(4,
                               numericInput("num_lineups", 
                                            "Number of Lineups to Generate:", 
                                            value = 20, 
                                            min = 1, 
                                            max = 150)
                        ),
                        column(4,
                               selectizeInput("excluded_players", "Exclude Players:",
                                              choices = NULL,
                                              multiple = TRUE,
                                              options = list(plugins = list('remove_button')))
                        )
                      ),
                      
                      fluidRow(
                        column(6,
                               div(class = "well well-sm",
                                   h4("Pool Statistics:"),
                                   textOutput("weighted_pool_stats")
                               )
                        ),
                        column(6,
                               div(style = "margin-top: 20px;",
                                   actionButton("generate_lineups", "Generate Lineups", 
                                                class = "btn-primary btn-lg", 
                                                style = "width: 100%;"),
                                   br(), br(),
                                   downloadButton("download_generated_lineups", "Download Lineups", 
                                                  style = "width: 100%;")
                               )
                        )
                      ),
                      
                      br(),
                      div(class = "alert alert-info",
                          HTML("<strong>How it works:</strong> This system uses your weighted ranking from the Optimal Lineups tab. 
                         Adjust the weights there first, then come here to select lineups from the top-ranked pool. 
                         The higher the 'Top X Lineups' number, the more variety in your generated lineups."))
                  )
                ),
                fluidRow(
                  box(width = 12,
                      title = "Player Exposure Analysis",
                      DTOutput("player_exposure_table") %>% withSpinner(color = "#FFD700")
                  )
                ),
                fluidRow(
                  box(width = 12,
                      title = "Generated Lineups",
                      DTOutput("generated_lineups_table") %>% withSpinner(color = "#FFD700")
                  )
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
    dk_data = NULL,
    score_history = SCORE_HISTORY, # Use global data if available
    simulation_results = NULL,
    player_projections = NULL,
    dk_optimal_lineups = NULL,
    dk_optimal_lineups_ranked = NULL,
    dk_player_exposure = NULL,
    dk_random_lineups = NULL,
    file_uploaded = FALSE,
    simulation_complete = FALSE,
    optimization_complete = FALSE  # Explicitly initialize to FALSE
  )
  
  
  # Expose reactive values as outputs
  output$simulation_complete <- reactive({
    return(rv$simulation_complete)
  })
  outputOptions(output, "simulation_complete", suspendWhenHidden = FALSE)
  
  
  output$optimization_complete <- reactive({
    # Convert boolean TRUE/FALSE to lowercase string "true"/"false"
    result <- tolower(as.character(!is.null(rv$dk_optimal_lineups) && nrow(rv$dk_optimal_lineups) > 0))
    return(result)
  })
  outputOptions(output, "optimization_complete", suspendWhenHidden = FALSE)
  
  # Handle score history file upload if needed
  observeEvent(input$score_history, {
    req(input$score_history)
    
    tryCatch({
      rv$score_history <- read.csv(input$score_history$datapath)
      
      # Convert data types
      rv$score_history <- rv$score_history %>%
        mutate(
          Tour = as.factor(Tour),
          best_of = as.integer(best_of),
          w_dk_score = as.numeric(w_dk_score),
          l_dk_score = as.numeric(l_dk_score),
          WIO = as.numeric(WIO),
          LIO = as.numeric(LIO),
          straight_sets = as.integer(straight_sets)
        )
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Error reading score history file:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Handle DraftKings file upload
  observeEvent(input$dk_file, {
    req(input$dk_file)
    
    tryCatch({
      # Read DraftKings data from Excel
      dk_data <- read_excel(input$dk_file$datapath)
      
      # Check if file has required columns
      required_cols <- c("Name", "Game Info", "ML", "SS")
      if (!all(required_cols %in% colnames(dk_data))) {
        showModal(modalDialog(
          title = "Invalid File Format",
          "DraftKings file must contain columns: Name, Game Info, ML, SS",
          easyClose = TRUE
        ))
        return(NULL)
      }
      
      # Add Tour and Surface columns if not present
      if (!"Tour" %in% colnames(dk_data)) {
        tour_input <- showModal(modalDialog(
          title = "Tour Information",
          "Please select the tour for this slate:",
          radioButtons("tour_input", NULL, 
                       choices = c("ATP", "WTA"),
                       selected = "ATP"),
          footer = tagList(
            actionButton("confirm_tour", "Confirm")
          ),
          easyClose = FALSE
        ))
        
        # Wait for user to confirm
        observeEvent(input$confirm_tour, {
          dk_data$Tour <- input$tour_input
          removeModal()
        })
      }
      
      if (!"Surface" %in% colnames(dk_data)) {
        surface_input <- showModal(modalDialog(
          title = "Surface Information",
          "Please select the court surface for this slate:",
          radioButtons("surface_input", NULL, 
                       choices = c("Hard", "Clay", "Grass"),
                       selected = "Hard"),
          footer = tagList(
            actionButton("confirm_surface", "Confirm")
          ),
          easyClose = FALSE
        ))
        
        # Wait for user to confirm
        observeEvent(input$confirm_surface, {
          dk_data$Surface <- input$surface_input
          removeModal()
        })
      }
      
      # Store data
      rv$dk_data <- dk_data
      rv$file_uploaded <- TRUE
      
      # Reset simulation flags
      rv$simulation_complete <- FALSE
      rv$optimization_complete <- FALSE
      
      # Update match selection dropdown
      updateSelectInput(session, "selected_match", 
                        choices = unique(dk_data$`Game Info`))
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Error reading DraftKings file:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # DraftKings data preview with probabilities
  output$dk_data_preview <- renderDT({
    req(rv$dk_data)
    
    # Create a copy of the data to avoid modifying the original
    display_data <- as.data.table(copy(rv$dk_data))
    
    # Group by match to process each match separately
    matches <- unique(display_data$`Game Info`)
    
    # Initialize columns for probabilities
    display_data[, `:=`(
      ML_Prob = 0,   # Devigged win probability
      SS_Prob = 0,   # Straight sets probability
      NSS_Prob = 0   # Non-straight sets probability
    )]
    
    # Process each match
    for (match in matches) {
      # Get players in this match
      match_players <- display_data[`Game Info` == match]
      
      if (nrow(match_players) != 2) {
        next  # Skip invalid matches
      }
      
      # Convert ML odds to raw probabilities
      p1_ml <- odds_to_probability(match_players$ML[1])
      p2_ml <- odds_to_probability(match_players$ML[2])
      
      # Devig ML probabilities
      total_ml <- p1_ml + p2_ml
      p1_ml_devig <- p1_ml / total_ml
      p2_ml_devig <- p2_ml / total_ml
      
      # Convert SS odds to probabilities
      p1_ss <- odds_to_probability(match_players$SS[1])
      p2_ss <- odds_to_probability(match_players$SS[2])
      
      # Ensure SS probability doesn't exceed devigged ML probability
      p1_ss <- min(p1_ss, p1_ml)
      p2_ss <- min(p2_ss, p2_ml)
      
      # Devig SS probabilities proportionally to maintain the same ratio
      total_ss <- p1_ss + p2_ss
      if (total_ss > 0) {
        # Scale so their sum matches the sum of devigged ML probabilities
        ss_scale_factor <- (p1_ml_devig + p2_ml_devig) / (p1_ss + p2_ss)
        p1_ss_devig <- p1_ss * ss_scale_factor * (p1_ml_devig / (p1_ml_devig + p2_ml_devig))
        p2_ss_devig <- p2_ss * ss_scale_factor * (p2_ml_devig / (p1_ml_devig + p2_ml_devig))
        
        # Make final adjustment to ensure SS probability doesn't exceed ML probability
        p1_ss_devig <- min(p1_ss_devig, p1_ml_devig)
        p2_ss_devig <- min(p2_ss_devig, p2_ml_devig)
      } else {
        p1_ss_devig <- 0
        p2_ss_devig <- 0
      }
      
      # Calculate NSS probabilities
      p1_nss_devig <- p1_ml_devig - p1_ss_devig
      p2_nss_devig <- p2_ml_devig - p2_ss_devig
      
      # Update probabilities in the data
      display_data[`Game Info` == match & Name == match_players$Name[1], 
                   `:=`(ML_Prob = p1_ml_devig, 
                        SS_Prob = p1_ss_devig, 
                        NSS_Prob = p1_nss_devig)]
      
      display_data[`Game Info` == match & Name == match_players$Name[2], 
                   `:=`(ML_Prob = p2_ml_devig, 
                        SS_Prob = p2_ss_devig, 
                        NSS_Prob = p2_nss_devig)]
    }
    
    # Select columns for display, removing the original odds
    display_cols <- c("Name", "Game Info", "Surface", "Tour", "Salary", 
                      "ML_Prob", "SS_Prob", "NSS_Prob")
    
    # Only include columns that actually exist
    display_cols <- intersect(display_cols, names(display_data))
    
    # Create the datatable
    datatable(
      display_data[, ..display_cols],
      options = list(
        paging = FALSE,       # No pagination
        searching = FALSE,    # No search box
        info = FALSE,         # No info
        scrollY = "600px",    # Vertical scrolling
        scrollX = TRUE,       # Horizontal scrolling
        dom = "t"             # Only table, no controls
      ),
      rownames = FALSE
    ) %>%
      formatPercentage(c("ML_Prob", "SS_Prob", "NSS_Prob"), 1)  # Format as percentages
  })
  
  observeEvent(input$run_sim, {
    req(rv$dk_data, rv$score_history)
    
    # Validate data
    if (is.null(rv$dk_data) || nrow(rv$dk_data) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Please upload valid DraftKings data",
        easyClose = TRUE
      ))
      return()
    }
    
    if (is.null(rv$score_history) || nrow(rv$score_history) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Historical score data is required for simulation",
        easyClose = TRUE
      ))
      return()
    }
    
    # Reset previous results
    rv$simulation_results <- NULL
    rv$player_projections <- NULL
    rv$dk_optimal_lineups <- NULL
    rv$dk_player_exposure <- NULL
    rv$dk_random_lineups <- NULL
    rv$simulation_complete <- FALSE
    rv$optimization_complete <- FALSE
    
    # Force garbage collection
    gc(full = TRUE)
    
    # Run simulation with progress indicator
    withProgress(message = 'Running simulation...', value = 0, {
      
      # Step 1: Run match simulations using the new batch method
      incProgress(0.1, detail = "Simulating matches...")
      
      simulation_results <- run_batch_simulation(rv$dk_data, rv$score_history, input$n_sims)
      
      # Step 2: Analyze player projections
      incProgress(0.5, detail = "Calculating player projections...")
      player_projections <- analyze_player_scores(simulation_results, rv$dk_data)
      
      # Store results
      rv$simulation_results <- simulation_results
      rv$player_projections <- player_projections
      rv$simulation_complete <- TRUE
      
      # Update player selection dropdown
      updateSelectizeInput(
        session,
        "selected_players",
        choices = unique(player_projections$Player),
        selected = head(unique(player_projections$Player), 5)
      )
      
      # Show success message
      showModal(modalDialog(
        title = "Success",
        "Simulation completed successfully! Now you can find optimal lineups or explore the player projections.",
        easyClose = TRUE
      ))
    })
    
    # Switch to match analysis tab
    updateTabItems(session, "sidebar_menu", selected = "match_analysis")
    
    # Final cleanup
    gc(full = TRUE)
  })
  
  observeEvent(input$run_dk_optimization, {
    req(rv$simulation_results, rv$player_projections)
    
    # Clear previous lineup results
    rv$dk_optimal_lineups <- NULL
    rv$dk_player_exposure <- NULL
    rv$dk_random_lineups <- NULL
    rv$optimization_complete <- FALSE
    
    # Force garbage collection
    cleanup_memory()
    
    # Show progress dialog
    withProgress(message = 'Finding optimal lineups...', value = 0, {
      showModal(modalDialog(
        title = "Processing Optimal Lineups",
        "Finding optimal lineups using all simulations. This may take a few minutes.",
        footer = NULL,
        easyClose = FALSE
      ))
      
      lp_cache <- new.env(hash = TRUE, parent = emptyenv())
      
      # Step 1: Calculate EW metrics
      incProgress(0.1, detail = "Calculating EW metrics...")
      
      cat("\n=== STARTING EW CALCULATION ===\n")
      cat("Simulation results available:", !is.null(rv$simulation_results), "\n")
      cat("Number of simulation rows:", ifelse(!is.null(rv$simulation_results), nrow(rv$simulation_results), 0), "\n")
      
      ew_metrics <- tryCatch({
        calculate_ew_metrics(rv$simulation_results, input$n_sims, rv$dk_data)  # Add rv$dk_data here
      }, error = function(e) {
        cat("Error calculating EW metrics:", e$message, "\n")
        return(NULL)
      })
      
      # DEBUG: Check EW metrics
      cat("\n=== EW METRICS DEBUG ===\n")
      cat("EW metrics calculated successfully:", !is.null(ew_metrics), "\n")
      if (!is.null(ew_metrics)) {
        cat("Number of players with EW:", nrow(ew_metrics), "\n")
        cat("EW columns:", paste(names(ew_metrics), collapse = ", "), "\n")
        if (nrow(ew_metrics) > 0) {
          cat("Sample EW values:\n")
          print(head(ew_metrics, 5))
        }
      } else {
        cat("EW metrics calculation failed or returned NULL\n")
      }
      cat("========================\n")
      
      # Step 2: Find optimal lineups
      incProgress(0.3, detail = "Finding optimal lineups...")
      optimal_lineups <- find_all_optimal_lineups(
        rv$simulation_results, 
        rv$dk_data,
        rv$dk_data  # Pass dk_data as third parameter
      )
      
      # Step 3: Expand lineup details with EW metrics
      incProgress(0.6, detail = "Processing lineup details...")
      
      # DEBUG: Check if we're passing EW metrics correctly
      cat("About to call expand_lineup_details:\n")
      cat("  - optimal_lineups is null:", is.null(optimal_lineups), "\n")
      cat("  - rv$dk_data is null:", is.null(rv$dk_data), "\n")
      cat("  - ew_metrics is null:", is.null(ew_metrics), "\n")
      
      expanded_lineups <- expand_lineup_details(
        optimal_lineups,
        rv$dk_data,
        ew_metrics  # This should now work
      )
      
      # DEBUG: Check if TotalEW was added
      cat("expand_lineup_details completed successfully\n")
      cat("TotalEW column exists in expanded lineups:", "TotalEW" %in% names(expanded_lineups), "\n")
      if ("TotalEW" %in% names(expanded_lineups)) {
        cat("TotalEW range:", min(expanded_lineups$TotalEW, na.rm=TRUE), "to", max(expanded_lineups$TotalEW, na.rm=TRUE), "\n")
      }
      
      # Store results
      rv$dk_optimal_lineups <- expanded_lineups
      
      # Calculate ranks for all metrics
      rv$dk_optimal_lineups_ranked <- calculate_lineup_ranks(expanded_lineups)
      
      # Apply initial weighted ranking with default weights
      default_weights <- list(
        weight_totalew = 2,           
        weight_medianscore = 0,       
        weight_score80th = 0,         
        weight_win6pct = 1,           
        weight_win5pluspct = 1,       
        weight_top1count = 1,        
        weight_top2count = 0,         
        weight_top3count = 0,         
        weight_top5count = 0      
      )
      
      rv$dk_optimal_lineups_ranked <- calculate_weighted_rank(rv$dk_optimal_lineups_ranked, default_weights)
      
      rv$optimization_complete <- TRUE
      
      # Calculate initial player exposure
      incProgress(0.8, detail = "Calculating player exposure...")
      if(!is.null(rv$dk_optimal_lineups)) {
        rv$dk_player_exposure <- calculate_player_exposure(
          rv$dk_optimal_lineups, 
          rv$player_projections
        )
      }
      
      if (!is.null(expanded_lineups)) {
        # Get unique players from all lineups
        player_cols <- grep("^Name[1-6]$", names(expanded_lineups), value = TRUE)
        all_players <- c()
        for(col in player_cols) {
          all_players <- c(all_players, expanded_lineups[[col]])
        }
        all_players <- unique(all_players[!is.na(all_players)])  # Remove NA values
        
        # Calculate pool exposure for each player
        pool_exposure <- sapply(all_players, function(player) {
          # Count appearances in optimal lineups
          player_appears <- logical(nrow(expanded_lineups))
          for(col in player_cols) {
            player_appears <- player_appears | (expanded_lineups[[col]] == player)
          }
          exposure_pct <- (sum(player_appears) / nrow(expanded_lineups)) * 100
          return(exposure_pct)
        })
        
        # Create labels with pool exposure percentages
        player_labels <- sapply(all_players, function(player) {
          exposure <- pool_exposure[player]
          sprintf("%s (%.1f%%)", player, exposure)
        })
        
        # Sort by exposure descending
        sorted_indices <- order(pool_exposure, decreasing = TRUE)
        sorted_players <- all_players[sorted_indices]
        sorted_labels <- player_labels[sorted_indices]
        
        # Update the selectInput
        updateSelectizeInput(
          session, 
          "excluded_players",
          choices = setNames(sorted_players, sorted_labels),
          selected = NULL
        )
      }
      

      
      # Remove the processing modal
      removeModal()
      
      # Show success message
      if(!is.null(rv$dk_optimal_lineups)) {
        success_msg <- sprintf(
          "Successfully generated <b>%d</b> optimal lineups!<br><br>",
          nrow(rv$dk_optimal_lineups)
        )
        
        if ("TotalEW" %in% names(rv$dk_optimal_lineups)) {
          success_msg <- paste0(success_msg, "✅ Expected Wins (EW) metrics included<br><br>")
        } else {
          success_msg <- paste0(success_msg, "⚠️ EW metrics not available<br><br>")
        }
        
        success_msg <- paste0(success_msg, "You can now go to the <b>Lineup Builder</b> tab to filter and select lineups from this pool.")
        
        showModal(modalDialog(
          title = "Success",
          HTML(success_msg),
          easyClose = TRUE
        ))
      }
    })
    
    # Switch to optimal lineups tab
    updateTabItems(session, "sidebar_menu", selected = "optimal_lineups")
    
    # Final cleanup
    cleanup_memory()
  })
  
  # Also trigger lineup optimization from alternative button
  observeEvent(input$run_optimization_alt, {
    # Trigger the main lineup optimization handler
    if(rv$simulation_complete) {
      session$sendCustomMessage(type = "click", message = list(id = "run_dk_optimization"))
    }
  })
  
  # Optimization status display
  output$dk_optimization_status <- renderUI({
    if(rv$optimization_complete) {
      div(
        class = "alert alert-success",
        HTML(sprintf("<b>%d</b> optimal lineups generated", nrow(rv$dk_optimal_lineups)))
      )
    } else if(rv$simulation_complete) {
      div(
        class = "alert alert-info",
        "Ready to find optimal lineups"
      )
    } else {
      div(
        class = "alert alert-warning",
        "Run simulations first"
      )
    }
  })
  
  # Simulation vs Implied table
  output$sim_vs_implied_table <- renderDT({
    req(rv$simulation_results, rv$dk_data)
    
    # Wrap everything in tryCatch to handle errors gracefully
    tryCatch({
      # Process DK data to get implied probabilities
      dk_processed <- as.data.frame(rv$dk_data)
      
      # Apply odds_to_probability to each row individually (safer than vectorized approach)
      dk_processed$ImpliedWin <- sapply(dk_processed$ML, function(odds) {
        if (is.numeric(odds)) {
          odds_to_probability(odds)
        } else {
          NA
        }
      })
      
      dk_processed$ImpliedSS <- sapply(dk_processed$SS, function(odds) {
        if (is.numeric(odds)) {
          odds_to_probability(odds)
        } else {
          NA
        }
      })
      
      # Calculate non-straight sets probability
      dk_processed$ImpliedNSS <- pmax(0, dk_processed$ImpliedWin - dk_processed$ImpliedSS)
      
      # Get simulation results in the right format
      sim_results <- as.data.frame(rv$simulation_results)
      
      # Calculate player win rates from simulation
      sim_outcomes <- sim_results %>%
        group_by(Player) %>%
        summarize(
          TotalMatches = n_distinct(paste0(Iteration, Match)),
          TotalAppearances = n(),
          Wins = sum(Result == "Winner"),
          StraightSets = sum(Result == "Winner" & Outcome == "SS"),
          NonStraightSets = sum(Result == "Winner" & Outcome == "NSS"),
          .groups = "drop"
        ) %>%
        mutate(
          WinRate = Wins / TotalMatches,
          SSRate = StraightSets / TotalMatches,
          NSSRate = NonStraightSets / TotalMatches
        )
      
      # Join with DK data to get implied probabilities and salary
      dk_select <- dk_processed %>% 
        select(Name, Salary, ImpliedWin, ImpliedSS, ImpliedNSS)
      
      comparison <- sim_outcomes %>%
        left_join(dk_select, by = c("Player" = "Name")) %>%
        mutate(
          WinDiff = (WinRate - ImpliedWin) * 100,  # Convert to percentage points
          SSDiff = (SSRate - ImpliedSS) * 100,
          NSSDiff = (NSSRate - ImpliedNSS) * 100
        ) %>%
        select(Player, Salary, 
               WinRate, ImpliedWin, WinDiff,
               SSRate, ImpliedSS, SSDiff,
               NSSRate, ImpliedNSS, NSSDiff)
      
      # Create datatable
      datatable(
        comparison,
        options = list(
          paging = FALSE,       # No pagination
          searching = FALSE,    # No search box
          info = FALSE,         # No info
          scrollY = "400px",    # Vertical scrolling
          scrollX = TRUE,       # Horizontal scrolling
          dom = "t",            # Only table, no controls
          order = list(list(4, 'desc'))  # Sort by WinDiff
        ),
        rownames = FALSE
      ) %>%
        formatCurrency("Salary", "$", digits = 0) %>%
        formatPercentage(c("WinRate", "ImpliedWin"), 1) %>%
        formatPercentage(c("SSRate", "ImpliedSS"), 1) %>%
        formatPercentage(c("NSSRate", "ImpliedNSS"), 1) %>%
        formatRound(c("WinDiff", "SSDiff", "NSSDiff"), 1) %>%
        formatStyle(
          c("WinDiff", "SSDiff", "NSSDiff"),
          color = styleInterval(c(-1, 1), c("red", "black", "green")),
          fontWeight = styleInterval(c(-3, 3), c("normal", "normal", "bold"))
        )
    }, error = function(e) {
      # Log the error
      cat("Error in simulation comparison table:", e$message, "\n")
      
      # Return empty datatable with message
      datatable(
        data.frame(Message = paste("Error generating comparison table:", e$message)),
        options = list(dom = "t", ordering = FALSE),
        rownames = FALSE
      )
    })
  })
  
  # Player projections table
  output$player_projections <- renderDT({
    req(rv$player_projections)
    
    # Create PPD (Points Per Dollar) column
    projections <- rv$player_projections %>%
      mutate(PPD = MedianScore / (Salary / 1000))
    
    # Select and rename columns for display
    display_data <- projections %>%
      select(
        Player, Salary, WinPct, StraightSetsPct, AvgScore, MedianScore, PPD
      ) %>%
      rename(
        "Avg Points" = AvgScore,
        "Median Points" = MedianScore,
        "Win %" = WinPct,
        "Straight Sets %" = StraightSetsPct
      )
    
    datatable(
      display_data,
      options = list(
        scrollX = TRUE,
        pageLength = -1,
        dom = "t",  # Only show table ('t'), no search/pagination
        order = list(list(2, 'desc'))  # Sort by Median Points by default
      ),
      rownames = FALSE
    ) %>%
      formatCurrency("Salary", "$", digits = 0) %>%
      formatRound(c("Avg Points", "Median Points", "PPD"), 2) %>%
      formatPercentage(c("Win %", "Straight Sets %"), 1)
  })
  
  
  # Points vs Salary plot
  output$points_salary_plot <- renderPlotly({
    req(rv$player_projections)
    
    # Create plot
    p <- ggplot(rv$player_projections, aes(x = Salary, y = MedianScore, text = Player)) +
      geom_point(aes(size = WinPct, color = StraightSetsPct)) +
      geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
      scale_color_gradient(low = "#4169E1", high = "#FFD700", name = "Straight Sets %") +
      scale_size(name = "Win %") +
      labs(title = "Fantasy Points vs Salary",
           x = "Salary ($)",
           y = "Median Fantasy Points") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("text", "x", "y", "size", "color"))
  })
  
  
  
  
  # Player score distribution - box and whisker for winning scores only
  output$player_score_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Focus on win results only since they're more relevant
    win_results <- rv$simulation_results[rv$simulation_results$Result == "Winner", ]
    
    # Get win rate for sorting
    win_rates <- rv$simulation_results %>%
      group_by(Player) %>%
      summarize(
        WinRate = mean(Result == "Winner", na.rm = TRUE),
        .groups = "drop"
      ) %>% 
      arrange(WinRate)
    
    ordered_names <- win_rates$Player
    
    plot_data <- win_results %>%
      filter(Player %in% ordered_names)
    
    p <- ggplot(plot_data, aes(x = factor(Player, levels = ordered_names),
                               y = Score,
                               fill = Player)) +
      geom_boxplot(outlier.alpha = 0.25) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Player", y = "DK Fantasy Points (Wins Only)") +
      theme(legend.position = "none")  # Hide legend to avoid clutter
    
    ggplotly(p, height = 700, tooltip = c("x", "y")) %>%
      layout(
        margin = list(l = 120, r = 20, b = 50, t = 50),
        yaxis = list(title = "DK Fantasy Points")
      )
    
    # Convert to plotly for interactivity
    ggplotly(p) %>%
      layout(margin = list(l = 120)) # Add left margin for player names
  })
  
  
  output$optimal_lineups_table <- renderDT({
    # Use ranked data if available, otherwise fall back to unranked
    lineup_data <- if (!is.null(rv$dk_optimal_lineups_ranked)) {
      rv$dk_optimal_lineups_ranked
    } else {
      rv$dk_optimal_lineups
    }
    
    # Validate required data
    if (is.null(lineup_data) || nrow(lineup_data) == 0) {
      return(datatable(
        data.frame(Message = "No optimal lineups available. Click 'Calculate Optimal Lineups' button to generate lineups."),
        options = list(dom = "t", ordering = FALSE),
        rownames = FALSE
      ))
    }
    
    # Get lineup data and ensure it's a data.frame
    lineups_df <- tryCatch({
      as.data.frame(lineup_data)
    }, error = function(e) {
      return(data.frame(Message = paste("Error converting lineup data:", e$message)))
    })
    
    # Check if conversion failed
    if (!"LineupID" %in% names(lineups_df)) {
      return(datatable(
        data.frame(Message = "Invalid lineup data structure"),
        options = list(dom = "t", ordering = FALSE),
        rownames = FALSE
      ))
    }
    
    # Determine the player column naming pattern used
    player_cols <- character(0)
    if (any(grepl("^Name[1-6]$", names(lineups_df)))) {
      player_cols <- grep("^Name[1-6]$", names(lineups_df), value = TRUE)
    } else if (any(grepl("^Player[1-6]$", names(lineups_df)))) {
      player_cols <- grep("^Player[1-6]$", names(lineups_df), value = TRUE)
    }
    
    # Select columns for display - include both values and ranks
    base_metric_cols <- c("TotalEW", "MedianScore", "Score80th", "Win6Pct", "Win5PlusPct",
                          "Top1Count", "Top2Count", "Top3Count", "Top5Count")
    
    # Build display columns: players, then alternating metric and rank
    display_cols <- player_cols
    
    # Add WeightedRank first if it exists
    if ("WeightedRank" %in% names(lineups_df)) {
      display_cols <- c(display_cols, "WeightedRank")
    }
    
    # Add metric and rank pairs
    for (metric in base_metric_cols) {
      if (metric %in% names(lineups_df)) {
        display_cols <- c(display_cols, metric)
        rank_col <- paste0(metric, "_Rank")
        if (rank_col %in% names(lineups_df)) {
          display_cols <- c(display_cols, rank_col)
        }
      }
    }
    
    # Add TotalSalary at the end
    if ("TotalSalary" %in% names(lineups_df)) {
      display_cols <- c(display_cols, "TotalSalary")
    }
    
    # Only use columns that actually exist in the data
    display_cols <- intersect(display_cols, names(lineups_df))
    
    # Ensure we have some columns to display
    if (length(display_cols) == 0) {
      return(datatable(
        data.frame(Message = "No valid columns found for display"),
        options = list(dom = "t", ordering = FALSE),
        rownames = FALSE
      ))
    }
    
    # Use the selected columns from the data frame
    display_data <- lineups_df[, display_cols, drop = FALSE]
    
    # Sort by WeightedRank if available, otherwise by TotalEW
    tryCatch({
      if ("WeightedRank" %in% names(display_data)) {
        display_data <- display_data[order(display_data$WeightedRank), ]
      } else if ("TotalEW" %in% names(display_data) && "Top1Count" %in% names(display_data)) {
        display_data <- display_data[order(-display_data$TotalEW, -display_data$Top1Count), ]
      } else if ("Top1Count" %in% names(display_data)) {
        display_data <- display_data[order(-display_data$Top1Count), ]
      }
    }, error = function(e) {
      cat("Sorting error:", e$message, "\n")
    })
    
    # Create datatable
    dt <- datatable(
      display_data,
      options = list(
        scrollX = TRUE,
        pageLength = 50,
        columnDefs = list(
          list(className = 'dt-body-nowrap', targets = "_all")
        )
      ),
      rownames = FALSE
    )
    
    # Apply formatting for regular metrics (same as before)
    totalew_exists <- "TotalEW" %in% names(display_data)
    if (totalew_exists) {
      dt <- tryCatch({
        dt %>% 
          formatRound("TotalEW", 3) %>%
          formatStyle(
            "TotalEW",
            backgroundColor = styleInterval(
              c(2.0, 2.5, 3.0, 3.5), 
              c("white", "#e8f5e8", "#d4e6d4", "#c0d7c0", "#8bc34a")
            ),
            fontWeight = styleInterval(3.0, c("normal", "bold"))
          )
      }, error = function(e) {
        dt
      })
    }
    
    # Format rank columns with different styling
    rank_cols_available <- grep("_Rank$", names(display_data), value = TRUE)
    if (length(rank_cols_available) > 0) {
      dt <- tryCatch({
        dt %>% 
          formatStyle(
            rank_cols_available,
            backgroundColor = styleInterval(
              c(10, 25, 50, 100), 
              c("#d4edd4", "#e8f5e8", "#f5f5f5", "#ffe6e6", "#ffcccc")
            ),
            color = styleInterval(
              c(10, 50), 
              c("#2e7d32", "#666666", "#c62828")
            ),
            fontWeight = styleInterval(25, c("bold", "normal"))
          )
      }, error = function(e) {
        dt
      })
    }
    
    # Special formatting for WeightedRank
    if ("WeightedRank" %in% names(display_data)) {
      dt <- tryCatch({
        dt %>% 
          formatStyle(
            "WeightedRank",
            backgroundColor = styleInterval(
              c(5, 15, 30, 60), 
              c("#ffd700", "#ffeb3b", "#fff9c4", "#f5f5f5", "#ffffff")
            ),
            fontWeight = "bold",
            color = "#1976d2"
          )
      }, error = function(e) {
        dt
      })
    }
    
    # Apply other formatting (scores, percentages, salary)
    score_cols_available <- intersect(c("MedianScore", "Score80th"), names(display_data))
    if (length(score_cols_available) > 0) {
      dt <- tryCatch({
        dt %>% formatRound(score_cols_available, 1)
      }, error = function(e) {
        dt
      })
    }
    
    win_pct_cols_available <- intersect(c("Win6Pct", "Win5PlusPct"), names(display_data))
    if (length(win_pct_cols_available) > 0) {
      dt <- tryCatch({
        dt %>% formatRound(win_pct_cols_available, 2)
      }, error = function(e) {
        dt
      })
    }
    
    salary_exists <- "TotalSalary" %in% names(display_data)
    if (salary_exists) {
      dt <- tryCatch({
        dt %>% formatCurrency("TotalSalary", "$", digits = 0)
      }, error = function(e) {
        dt
      })
    }
    
    return(dt)
  })
  
  

  # Observer for applying weights
  observeEvent(input$apply_weights, {
    req(rv$dk_optimal_lineups_ranked)
    
    # Create weights list from input values
    weights <- list(
      weight_totalew = input$weight_totalew,
      weight_medianscore = input$weight_medianscore,
      weight_score80th = input$weight_score80th,
      weight_win6pct = input$weight_win6pct,
      weight_win5pluspct = input$weight_win5pluspct,
      weight_top1count = input$weight_top1count,
      weight_top2count = input$weight_top2count,
      weight_top3count = input$weight_top3count,
      weight_top5count = input$weight_top5count
    )
    
    # Calculate weighted rank
    rv$dk_optimal_lineups_ranked <- calculate_weighted_rank(rv$dk_optimal_lineups_ranked, weights)
    
    showNotification("Weighted ranking applied successfully!", type = "message")
  })
  

  observeEvent(input$reset_weights, {
    updateSliderInput(session, "weight_totalew", value = 2)      # Changed from 3 to 2
    updateSliderInput(session, "weight_medianscore", value = 0)  # Changed from 2 to 0
    updateSliderInput(session, "weight_score80th", value = 0)    # Changed from 2 to 0
    updateSliderInput(session, "weight_win6pct", value = 1)      # Keep at 1
    updateSliderInput(session, "weight_win5pluspct", value = 1)  # Keep at 1
    updateSliderInput(session, "weight_top1count", value = 1)    # Changed from 2 to 1
    updateSliderInput(session, "weight_top2count", value = 0)    # Changed from 1 to 0
    updateSliderInput(session, "weight_top3count", value = 0)    # Changed from 1 to 0
    updateSliderInput(session, "weight_top5count", value = 0)    # Changed from 0.5 to 0
    
    showNotification("Weights reset to defaults", type = "message")
  })
  
  
  # Add weight summary output
  output$weight_summary <- renderText({
    total_weight <- input$weight_totalew + input$weight_medianscore + input$weight_score80th + 
      input$weight_win6pct + input$weight_win5pluspct + input$weight_top1count + 
      input$weight_top2count + input$weight_top3count + input$weight_top5count
    
    paste("Total Weight:", round(total_weight, 1), 
          "| TotalEW:", round((input$weight_totalew/total_weight)*100, 1), "%",
          "| Scores:", round(((input$weight_medianscore + input$weight_score80th)/total_weight)*100, 1), "%",
          "| Wins:", round(((input$weight_win6pct + input$weight_win5pluspct)/total_weight)*100, 1), "%",
          "| Counts:", round(((input$weight_top1count + input$weight_top2count + input$weight_top3count + input$weight_top5count)/total_weight)*100, 1), "%")
  })
  
  # Lineup count thresholds
  output$lineup_count_thresholds <- renderDT({
    req(rv$dk_optimal_lineups)
    
    # Define thresholds to check
    thresholds <- c(1, 2, 5, 10, 15, 20)
    
    # Calculate counts for each threshold
    counts <- data.frame(
      Threshold = thresholds,
      Top1Count = sapply(thresholds, function(t) sum(rv$dk_optimal_lineups$Top1Count >= t)),
      Top2Count = sapply(thresholds, function(t) sum(rv$dk_optimal_lineups$Top2Count >= t)),
      Top3Count = sapply(thresholds, function(t) sum(rv$dk_optimal_lineups$Top3Count >= t)),
      Top5Count = sapply(thresholds, function(t) sum(rv$dk_optimal_lineups$Top5Count >= t))
    )
    
    datatable(
      counts,
      options = list(
        dom = 't',  # Only show table (no pagination)
        ordering = FALSE
      ),
      rownames = FALSE,
      caption = "Number of lineups with at least this many appearances"
    )
  })
  
  output$weighted_pool_stats <- renderText({
    req(rv$dk_optimal_lineups_ranked)
    
    stats <- calculate_weighted_pool_stats(
      rv$dk_optimal_lineups_ranked, 
      input$top_lineups_pool,
      input$excluded_players
    )
    
    paste0("Pool Size: ", stats$count, " lineups | ",
           "Rank Range: ", stats$rank_range, " | ",
           "EW Range: ", stats$ew_range)
  })
  

  observeEvent(rv$dk_optimal_lineups_ranked, {
    req(rv$dk_optimal_lineups_ranked)
    
    if (!is.null(rv$dk_optimal_lineups_ranked)) {
      # Get unique players from all lineups
      player_cols <- grep("^Name[1-6]$", names(rv$dk_optimal_lineups_ranked), value = TRUE)
      if(length(player_cols) == 0) {
        player_cols <- grep("^Player[1-6]$", names(rv$dk_optimal_lineups_ranked), value = TRUE)
      }
      
      all_players <- c()
      for(col in player_cols) {
        all_players <- c(all_players, rv$dk_optimal_lineups_ranked[[col]])
      }
      all_players <- unique(all_players[!is.na(all_players)])
      
      # Calculate pool exposure for each player (from top 1000 lineups)
      top_pool <- head(rv$dk_optimal_lineups_ranked, 1000)
      pool_exposure <- sapply(all_players, function(player) {
        player_appears <- logical(nrow(top_pool))
        for(col in player_cols) {
          player_appears <- player_appears | (top_pool[[col]] == player)
        }
        exposure_pct <- (sum(player_appears) / nrow(top_pool)) * 100
        return(exposure_pct)
      })
      
      # Create labels with pool exposure percentages
      player_labels <- sapply(all_players, function(player) {
        exposure <- pool_exposure[player]
        sprintf("%s (%.1f%%)", player, exposure)
      })
      
      # Sort by exposure descending
      sorted_indices <- order(pool_exposure, decreasing = TRUE)
      sorted_players <- all_players[sorted_indices]
      sorted_labels <- player_labels[sorted_indices]
      
      # Update the selectInput
      updateSelectizeInput(
        session, 
        "excluded_players",
        choices = setNames(sorted_players, sorted_labels),
        selected = NULL
      )
      
      # Update the max value for top_lineups_pool
      updateNumericInput(
        session,
        "top_lineups_pool",
        max = nrow(rv$dk_optimal_lineups_ranked),
        value = min(500, nrow(rv$dk_optimal_lineups_ranked))
      )
    }
  })
  
  observe({
    req(rv$dk_optimal_lineups_ranked)
    
    # This will trigger pool stats update when top_lineups_pool or excluded_players change
    input$top_lineups_pool
    input$excluded_players
    
    # The output$weighted_pool_stats will automatically update due to this dependency
  })
  
  observeEvent(input$generate_lineups, {
    req(rv$dk_optimal_lineups_ranked)
    
    # Show progress
    withProgress(message = 'Generating lineups from weighted pool...', value = 0, {
      
      # Generate lineups using weighted selection
      rv$dk_random_lineups <- generate_weighted_lineups(
        rv$dk_optimal_lineups_ranked,
        input$top_lineups_pool,
        input$num_lineups,
        input$excluded_players
      )
      
      # Update player exposure data
      if(!is.null(rv$dk_random_lineups)) {
        # Get the filtered pool for Pool_Exposure calculation
        pool_stats <- calculate_weighted_pool_stats(
          rv$dk_optimal_lineups_ranked, 
          input$top_lineups_pool,
          input$excluded_players
        )
        
        # Calculate exposure from the actual top X pool (before player exclusions)
        pool_lineups <- head(rv$dk_optimal_lineups_ranked, input$top_lineups_pool)
        
        rv$dk_player_exposure <- calculate_player_exposure(
          pool_lineups,  # Use the full top X pool for Pool_Exposure
          rv$player_projections,
          rv$dk_random_lineups  # Generated lineups for Randomized_Exposure
        )
        
        showModal(modalDialog(
          title = "Success",
          sprintf("Generated %d lineups from top %d ranked lineups!", 
                  nrow(rv$dk_random_lineups), input$top_lineups_pool),
          easyClose = TRUE
        ))
      } else {
        showModal(modalDialog(
          title = "Error",
          "No lineups available after applying player exclusions. Try reducing exclusions or increasing the pool size.",
          easyClose = TRUE
        ))
      }
    })
  })
  
  observe({
    req(rv$dk_optimal_lineups_ranked)
    
    # This will trigger whenever top_lineups_pool or excluded_players change
    input$top_lineups_pool
    input$excluded_players
    
    # Calculate exposure from the current top X pool (before player exclusions)
    pool_lineups <- head(rv$dk_optimal_lineups_ranked, input$top_lineups_pool)
    
    # Apply player exclusion filter to the pool if needed
    if (!is.null(input$excluded_players) && length(input$excluded_players) > 0) {
      # Determine player column pattern
      player_cols <- if(any(grepl("^Name[1-6]$", names(pool_lineups)))) {
        grep("^Name[1-6]$", names(pool_lineups), value = TRUE)
      } else {
        grep("^Player[1-6]$", names(pool_lineups), value = TRUE)
      }
      
      if(length(player_cols) > 0) {
        for(excluded_player in input$excluded_players) {
          exclude_condition <- rep(TRUE, nrow(pool_lineups))
          for(col in player_cols) {
            exclude_condition <- exclude_condition & (pool_lineups[[col]] != excluded_player)
          }
          pool_lineups <- pool_lineups[exclude_condition, ]
        }
      }
    }
    
    # Update player exposure using the current pool
    rv$dk_player_exposure <- calculate_player_exposure(
      pool_lineups,             # Use current top X pool (with exclusions) for Pool_Exposure
      rv$player_projections,
      rv$dk_random_lineups      # Keep existing generated lineups (may be NULL)
    )
  })
  
  output$player_exposure_table <- renderDT({
    req(rv$dk_player_exposure)
    
    # Get the exposure data
    display_data <- rv$dk_player_exposure
    
    # Reorder columns and sort by Pool Exposure descending
    display_data <- display_data[, c("Player", "Salary", "Randomized_Exposure", "Pool_Exposure", "DKOwn", "Randomized_Leverage", "Pool_Leverage")]
    display_data <- display_data[order(display_data$Pool_Exposure, decreasing = TRUE), ]
    
    datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "tip",
        ordering = TRUE
      ),
      rownames = FALSE,
      colnames = c("Player", "Salary", "Randomized %", "Pool %", "DKOwn %", "Randomized Leverage", "Pool Leverage")
    ) %>%
      formatCurrency("Salary", "$", digits = 0) %>%
      formatRound(c("Randomized_Exposure", "Pool_Exposure"), 1) %>%
      formatPercentage("DKOwn", 1) %>%
      formatRound(c("Randomized_Leverage", "Pool_Leverage"), 1)
  })
  
  # Generated lineups table
  output$generated_lineups_table <- renderDT({
    req(rv$dk_random_lineups)
    
    # Convert to data.frame if it's not already
    display_data <- as.data.frame(rv$dk_random_lineups)
    
    # Determine the player column naming pattern used
    if(any(grepl("^Name[1-6]$", names(display_data)))) {
      player_cols <- grep("^Name[1-6]$", names(display_data), value = TRUE)
    } else if(any(grepl("^Player[1-6]$", names(display_data)))) {
      player_cols <- grep("^Player[1-6]$", names(display_data), value = TRUE)
    } else {
      player_cols <- character(0)
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
          background = styleColorBar(c(0, max(display_data[[col]], na.rm = TRUE)), '#FFD700'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      }
    }
    
    dt
  })
  
  # Download handlers
  output$download_projections <- downloadHandler(
    filename = function() {
      paste("tennis_projections_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$player_projections, file, row.names = FALSE)
    }
  )
  
  output$download_optimal_lineups <- downloadHandler(
    filename = function() {
      paste("tennis_optimal_lineups_with_ranks_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      # Use ranked data if available
      lineup_data <- if (!is.null(rv$dk_optimal_lineups_ranked)) {
        rv$dk_optimal_lineups_ranked
      } else {
        rv$dk_optimal_lineups
      }
      
      if(is.null(lineup_data) || nrow(lineup_data) == 0) {
        empty_data <- data.frame(matrix(ncol = DK_ROSTER_SIZE + 15, nrow = 0))
        colnames(empty_data) <- c(paste0("Player", 1:DK_ROSTER_SIZE), "WeightedRank", "TotalEW", "TotalEW_Rank", 
                                  "MedianScore", "MedianScore_Rank", "Score80th", "Score80th_Rank", 
                                  "Win6Pct", "Win6Pct_Rank", "Top1Count", "Top1Count_Rank", "TotalSalary")
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      lineups_df <- as.data.frame(lineup_data)
      
      # Determine player column pattern
      player_pattern <- if(any(grepl("^Name[1-6]$", names(lineups_df)))) {
        "^Name[1-6]$"
      } else if(any(grepl("^Player[1-6]$", names(lineups_df)))) {
        "^Player[1-6]$"
      } else {
        stop("Could not find player columns in lineup data")
      }
      
      player_cols <- grep(player_pattern, names(lineups_df), value = TRUE)
      
      # Include all metric and rank columns for download
      metric_cols <- c("TotalEW", "MedianScore", "Score80th", "Win6Pct", "Win5PlusPct",
                       "Top1Count", "Top2Count", "Top3Count", "Top5Count")
      rank_cols <- paste0(metric_cols, "_Rank")
      
      download_cols <- c(player_cols, "WeightedRank", metric_cols, rank_cols, "TotalSalary")
      download_cols <- intersect(download_cols, names(lineups_df))
      
      lineups_for_export <- lineups_df[, download_cols, drop = FALSE]
      
      # Rename Name columns to Player columns for DraftKings format
      if(any(grepl("^Name[1-6]$", names(lineups_for_export)))) {
        for(i in 1:6) {
          old_name <- paste0("Name", i)
          new_name <- paste0("Player", i)
          if(old_name %in% names(lineups_for_export)) {
            names(lineups_for_export)[names(lineups_for_export) == old_name] <- new_name
          }
        }
      }
      
      # Add player IDs (existing logic)
      get_player_id <- function(name) {
        player_info <- rv$dk_data[rv$dk_data$Name == name, ]
        if (nrow(player_info) > 0 && "ID" %in% names(player_info)) {
          return(paste0(name, " (", player_info$ID[1], ")"))
        } else {
          return(name)
        }
      }
      
      player_cols <- grep("^Player[1-6]$", names(lineups_for_export), value = TRUE)
      for (col in player_cols) {
        if (col %in% names(lineups_for_export)) {
          lineups_for_export[[col]] <- sapply(lineups_for_export[[col]], get_player_id)
        }
      }
      
      # Sort by WeightedRank if available
      if("WeightedRank" %in% names(lineups_for_export)) {
        lineups_for_export <- lineups_for_export[order(lineups_for_export$WeightedRank), ]
      } else if("TotalEW" %in% names(lineups_for_export)) {
        lineups_for_export <- lineups_for_export[order(-lineups_for_export$TotalEW), ]
      }
      
      write.csv(lineups_for_export, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # Fixed download handler for generated lineups
  output$download_generated_lineups <- downloadHandler(
    filename = function() {
      paste("tennis_generated_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      if(is.null(rv$dk_random_lineups) || nrow(rv$dk_random_lineups) == 0) {
        # Create an empty dataframe with appropriate columns
        empty_data <- data.frame(matrix(ncol = DK_ROSTER_SIZE + 4, nrow = 0))
        colnames(empty_data) <- c(paste0("Player", 1:DK_ROSTER_SIZE), "Top1Count", "Top3Count", "Top5Count", "TotalSalary")
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      # Get generated lineup data
      lineups_df <- as.data.frame(rv$dk_random_lineups)
      
      # Determine the naming pattern used in the data
      player_pattern <- if(any(grepl("^Name[1-6]$", names(lineups_df)))) {
        "^Name[1-6]$"
      } else if(any(grepl("^Player[1-6]$", names(lineups_df)))) {
        "^Player[1-6]$"
      } else {
        stop("Could not find player columns in lineup data")
      }
      
      # Find player columns
      player_cols <- grep(player_pattern, names(lineups_df), value = TRUE)
      
      # Create a copy for exporting
      lineups_for_export <- lineups_df
      
      # Select columns for download, including all available columns
      display_cols <- c(
        player_cols,
        "Top1Count", "Top2Count", "Top3Count", "Top5Count", "TotalSalary"
      )
      
      # Intersect with available columns
      display_cols <- intersect(display_cols, names(lineups_df))
      lineups_for_export <- lineups_df[, display_cols, drop = FALSE]
      
      # Rename Name columns to Player columns for DraftKings format
      if(any(grepl("^Name[1-6]$", names(lineups_for_export)))) {
        for(i in 1:6) {
          old_name <- paste0("Name", i)
          new_name <- paste0("Player", i)
          if(old_name %in% names(lineups_for_export)) {
            names(lineups_for_export)[names(lineups_for_export) == old_name] <- new_name
          }
        }
      }
      
      # ID formatting function - retrieve player ID from dk_data based on name
      get_player_id <- function(name) {
        player_info <- rv$dk_data[rv$dk_data$Name == name, ]
        if (nrow(player_info) > 0 && "ID" %in% names(player_info)) {
          return(paste0(name, " (", player_info$ID[1], ")"))
        } else {
          return(name)  # Return just the name if ID not found
        }
      }
      
      # Append IDs to player names
      player_cols <- grep("^Player[1-6]$", names(lineups_for_export), value = TRUE)
      for (col in player_cols) {
        if (col %in% names(lineups_for_export)) {
          # Create a new column with the name + ID format
          lineups_for_export[[col]] <- sapply(lineups_for_export[[col]], get_player_id)
        }
      }
      
      # Write to CSV file
      write.csv(lineups_for_export, file, row.names = FALSE)
    },
    contentType = "text/csv"  # Explicitly set the content type to CSV
  )
  
  # Also add a function to analyze match outcomes (similar to fight_outcomes in MMA app)
  analyze_match_outcomes <- function(simulation_results) {
    setDT(simulation_results)
    
    # Calculate win rates and result distributions
    match_outcomes <- simulation_results[, .(
      Win_Rate = mean(Result == "Winner", na.rm = TRUE) * 100,
      SS_Rate = mean(Result == "Winner" & Outcome == "SS", na.rm = TRUE) * 100,
      NSS_Rate = mean(Result == "Winner" & Outcome == "NSS", na.rm = TRUE) * 100
    ), by = .(Player)]
    
    # Add player's salary and other information from the simulation data
    player_info <- unique(simulation_results[, .(Player, Salary)])
    match_outcomes <- merge(match_outcomes, player_info, by = "Player", all.x = TRUE)
    
    # Ensure all columns are numeric before rounding
    numeric_cols <- setdiff(names(match_outcomes), c("Player", "Match"))
    for (col in numeric_cols) {
      if(!is.null(match_outcomes[[col]])) {
        match_outcomes[, (col) := round(as.numeric(get(col)), 1)]
      }
    }
    
    # Sort by Win Rate in descending order
    setorder(match_outcomes, -Win_Rate)
    
    return(match_outcomes)
  }
  
  # Memory cleanup functions
  observe({
    invalidateLater(180000) # 3 minutes
    gc(verbose = FALSE, full = TRUE)
  })
  
  # Clean up on session end
  session$onSessionEnded(function() {
    gc(verbose = FALSE, full = TRUE)
  })
}


# Run the application
shinyApp(ui = ui, server = server)