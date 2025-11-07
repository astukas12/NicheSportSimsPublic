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

# Detect contest format from data
detect_contest_format <- function(data) {
  has_showdown_cols <- all(c("CID", "AID", "PID") %in% names(data))
  if (has_showdown_cols) {
    return("showdown")
  } else {
    return("classic")
  }
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
                    "Top1Count")
  
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

# Convert ranks to 0-100 scores based on percentile with gap awareness
calculate_lineup_scores <- function(lineups_df) {
  # Create a copy to avoid modifying original data
  scored_lineups <- as.data.frame(lineups_df)
  
  # Define columns that should be scored (higher values = better scores)
  score_columns <- c("TotalEW", "MedianScore", "Score80th", "Win6Pct", "Win5PlusPct", "Top1Count")
  
  # Only score columns that exist in the data
  available_score_cols <- intersect(score_columns, names(scored_lineups))
  
  # Calculate scores for each column (0-100 scale, higher = better)
  for (col in available_score_cols) {
    score_col_name <- paste0(col, "_Score")
    if (!all(is.na(scored_lineups[[col]]))) {
      # Use percentile ranking with slight curve to emphasize top performers
      percentile_ranks <- rank(scored_lineups[[col]], ties.method = "min") / length(scored_lineups[[col]])
      # Apply curve: (percentile ^ 1.2) * 100
      scored_lineups[[score_col_name]] <- (percentile_ranks ^ 1.2) * 100
    } else {
      scored_lineups[[score_col_name]] <- NA
    }
  }
  
  return(scored_lineups)
}

# Calculate ownership metrics for lineups
calculate_ownership_metrics <- function(lineups_df, dk_data) {
  # Check if we have ownership data
  if (is.null(dk_data) || !"DKOwn" %in% names(dk_data)) {
    cat("No ownership data available - skipping ownership calculations\n")
    return(lineups_df)
  }
  
  cat("Calculating ownership metrics...\n")
  
  # Create ownership lookup
  ownership_lookup <- setNames(dk_data$DKOwn, dk_data$Name)
  
  # Determine player column pattern
  player_cols <- if(any(grepl("^Name[1-6]$", names(lineups_df)))) {
    grep("^Name[1-6]$", names(lineups_df), value = TRUE)
  } else {
    # Parse from LineupID if needed
    NULL
  }
  
  # Add ownership columns
  lineups_df$LineupOwnership <- numeric(nrow(lineups_df))
  lineups_df$ContrarianScore <- numeric(nrow(lineups_df))
  
  for (i in 1:nrow(lineups_df)) {
    # Get player names for this lineup
    if (!is.null(player_cols)) {
      lineup_players <- unlist(lineups_df[i, player_cols])
    } else {
      # Parse from LineupID
      lineup_players <- unlist(strsplit(lineups_df$LineupID[i], "\\|"))
    }
    
    # Get ownership for each player
    player_ownership <- ownership_lookup[lineup_players]
    player_ownership[is.na(player_ownership)] <- 0.05  # Default 5% for missing data
    
    # NEW GRANULAR OWNERSHIP CALCULATION
    # This creates a more sensitive rating that spreads lineups across a wider range
    
    # Method 1: Multiplicative with scaling factor
    # This heavily penalizes high-ownership stacks
    geometric_mean <- prod(player_ownership)^(1/length(player_ownership))
    
    # Method 2: Weighted by ownership variance 
    # Higher variance = more contrarian build
    ownership_variance <- var(player_ownership)
    variance_bonus <- min(0.3, ownership_variance * 2)  # Cap the bonus
    
    # Method 3: Penalize high-ownership players more
    # Create exponential penalty for popular players
    high_own_penalty <- sum((player_ownership^1.5)) / length(player_ownership)
    
    # Combined ownership rating (0 to ~1 scale, but more granular)
    lineup_ownership <- (geometric_mean * 0.4) +           # Base ownership
      (high_own_penalty * 0.5) +          # Penalty for popular players  
      (mean(player_ownership) * 0.1) -    # Small average component
      (variance_bonus)                     # Bonus for contrarian builds
    
    # Ensure it stays in reasonable range
    lineup_ownership <- max(0.001, min(1.0, lineup_ownership))
    
    lineups_df$LineupOwnership[i] <- lineup_ownership
    
    # Calculate ContrarianScore: WeightedScore / LineupOwnership  
    # Higher WeightedScore + Lower Ownership = Higher Contrarian Score
    if ("WeightedScore" %in% names(lineups_df) && lineup_ownership > 0) {
      lineups_df$ContrarianScore[i] <- lineups_df$WeightedScore[i] / lineup_ownership
    }
  }
  
  cat(sprintf("Ownership metrics calculated for %d lineups\n", nrow(lineups_df)))
  
  # Report ownership distribution for debugging
  ownership_summary <- summary(lineups_df$LineupOwnership)
  cat("Ownership distribution:", paste(names(ownership_summary), "=", round(ownership_summary, 4), collapse = ", "), "\n")
  
  return(lineups_df)
}

# Updated weighted scoring function using scores instead of ranks
calculate_weighted_score <- function(lineups_df, weights) {
  # Available score columns based on what exists in the data
  available_score_cols <- names(lineups_df)[grepl("_Score$", names(lineups_df))]
  
  # Initialize weighted score as 0
  lineups_df$WeightedScore <- 0
  
  # Calculate weighted score (higher is better since scores are 0-100)
  total_weight <- 0
  
  for (score_col in available_score_cols) {
    # Extract the base metric name (remove "_Score" suffix)
    base_metric <- gsub("_Score$", "", score_col)
    
    # Check if we have a weight for this metric
    weight_key <- switch(base_metric,
                         "TotalEW" = "weight_totalew",
                         "MedianScore" = "weight_medianscore", 
                         "Score80th" = "weight_score80th",
                         "Win6Pct" = "weight_win6pct",
                         "Win5PlusPct" = "weight_win5pluspct",
                         "Top1Count" = "weight_top1count",
                         NULL)
    
    if (!is.null(weight_key) && !is.null(weights[[weight_key]]) && weights[[weight_key]] > 0) {
      weight_value <- weights[[weight_key]]
      
      # Add weighted score to the total (multiply score by weight)
      lineups_df$WeightedScore <- lineups_df$WeightedScore + 
        (lineups_df[[score_col]] * weight_value)
      
      total_weight <- total_weight + weight_value
    }
  }
  
  # Normalize by total weight if any weights were applied
  if (total_weight > 0) {
    lineups_df$WeightedScore <- lineups_df$WeightedScore / total_weight
  }
  
  # Calculate final weighted rank (1 = best weighted score)
  lineups_df$WeightedRank <- rank(-lineups_df$WeightedScore, ties.method = "min")
  
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

run_batch_simulation <- function(dk_data, historical_data, n_simulations = 50000, contest_format = "classic") {
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
  
  # === PRE-COMPUTATION PHASE ===
  cat("=== PRE-COMPUTING MATCH PROBABILITIES AND SCORES ===\n")
  precomp_start_time <- Sys.time()
  
  match_prob_cache <- list()
  
  for (match_idx in seq_along(matches)) {
    match_name <- matches[match_idx]
    
    cat(sprintf("[%s] Pre-computing match %d/%d: %s", 
                format(Sys.time(), "%H:%M:%S"), 
                match_idx, total_matches, match_name))
    
    match_players <- dk_dt[`Game Info` == match_name]
    
    if (nrow(match_players) != 2) {
      cat(" - SKIPPED (invalid player count)\n")
      next
    }
    
    p1 <- match_players[1]
    p2 <- match_players[2]
    
    cat(sprintf(" (%s vs %s)", p1$Name, p2$Name))
    
    # Check for walkover scenario
    p1_tour <- p1$Tour
    p2_tour <- p2$Tour
    is_walkover <- any(c(p1_tour, p2_tour) %in% c("WD", "WO"))
    
    if (is_walkover) {
      cat(" - WALKOVER")
      
      # Determine walkover winner/loser
      if (p1_tour == "WD" && p2_tour == "WO") {
        winner_name <- p2$Name; loser_name <- p1$Name
      } else if (p1_tour == "WO" && p2_tour == "WD") {
        winner_name <- p1$Name; loser_name <- p2$Name
      } else if (p1_tour == "WD") {
        winner_name <- p2$Name; loser_name <- p1$Name
      } else if (p2_tour == "WD") {
        winner_name <- p1$Name; loser_name <- p2$Name
      } else if (p1_tour == "WO") {
        winner_name <- p1$Name; loser_name <- p2$Name
      } else if (p2_tour == "WO") {
        winner_name <- p2$Name; loser_name <- p1$Name
      } else {
        cat(" - ERROR: Invalid walkover configuration")
        next
      }
      
      match_prob_cache[[match_name]] <- list(
        type = "walkover",
        winner = winner_name,
        loser = loser_name,
        winner_score = 30,
        loser_score = 0
      )
      
    } else {
      # NORMAL MATCH - Pre-compute all probabilities and score samples
      
      # Calculate ML probabilities
      p1_ml <- odds_to_probability(as.numeric(p1$ML))
      p2_ml <- odds_to_probability(as.numeric(p2$ML))
      total_ml_prob <- p1_ml + p2_ml
      p1_ml_prob <- p1_ml / total_ml_prob
      p2_ml_prob <- p2_ml / total_ml_prob
      
      # Calculate SS probabilities
      p1_ss <- odds_to_probability(as.numeric(p1$SS))
      p2_ss <- odds_to_probability(as.numeric(p2$SS))
      p1_nss <- p1_ml - p1_ss
      p2_nss <- p2_ml - p2_ss
      
      # Normalize probabilities
      p1_ss_prob <- p1_ss / total_ml_prob
      p2_ss_prob <- p2_ss / total_ml_prob
      p1_nss_prob <- p1_nss / total_ml_prob
      p2_nss_prob <- p2_nss / total_ml_prob
      
      # Create cumulative probability intervals for fast sampling
      cum_probs <- c(0, p1_ss_prob, p1_ss_prob + p1_nss_prob, 
                     p1_ss_prob + p1_nss_prob + p2_ss_prob, 1)
      
      # Pre-compute score samples for each possible outcome
      outcomes <- list(
        list(winner = p1$Name, loser = p2$Name, outcome = "SS", prob = p1_ss_prob),
        list(winner = p1$Name, loser = p2$Name, outcome = "NSS", prob = p1_nss_prob),
        list(winner = p2$Name, loser = p1$Name, outcome = "SS", prob = p2_ss_prob),
        list(winner = p2$Name, loser = p1$Name, outcome = "NSS", prob = p2_nss_prob)
      )
      
      score_samples <- list()
      for (outcome_info in outcomes) {
        if (outcome_info$prob > 0) {
          winner_prob <- ifelse(outcome_info$winner == p1$Name, p1_ml_prob, p2_ml_prob)
          loser_prob <- ifelse(outcome_info$loser == p1$Name, p1_ml_prob, p2_ml_prob)
          
          # Find similar historical matches
          tour <- p1$Tour
          is_straight_sets <- as.integer(outcome_info$outcome == "SS")
          #best_of_value <- ifelse(tour == "WTA", 3, 5)
          best_of_value <- 3
          
          similar_matches <- hist_dt[
            Tour == tour & best_of == best_of_value & straight_sets == is_straight_sets
          ]
          
          if (nrow(similar_matches) < 10) {
            similar_matches <- hist_dt[Tour == tour & best_of == best_of_value]
          }
          if (nrow(similar_matches) < 10) {
            similar_matches <- hist_dt[Tour == tour]
          }
          
          if (nrow(similar_matches) > 0) {
            # Calculate similarity and get top matches
            similar_matches[, odds_diff := abs(WIO - winner_prob) + abs(LIO - loser_prob)]
            setorder(similar_matches, odds_diff)
            
            n_similar <- min(50, nrow(similar_matches))
            top_matches <- similar_matches[1:n_similar]
            
            # Pre-sample scores (generate enough for all simulations)
            sample_size <- min(20000, n_simulations * 2)  # Extra buffer
            sample_indices <- sample(1:n_similar, sample_size, replace = TRUE)
            
            score_samples[[length(score_samples) + 1]] <- list(
              outcome_id = paste(outcome_info$winner, outcome_info$outcome, outcome_info$loser, sep = "_"),
              winner = outcome_info$winner,
              loser = outcome_info$loser,
              outcome = outcome_info$outcome,
              prob = outcome_info$prob,
              winner_scores = top_matches$w_dk_score[sample_indices],
              loser_scores = top_matches$l_dk_score[sample_indices],
              sample_counter = 1  # Track which sample to use next
            )
          } else {
            # Default scores if no historical matches
            score_samples[[length(score_samples) + 1]] <- list(
              outcome_id = paste(outcome_info$winner, outcome_info$outcome, outcome_info$loser, sep = "_"),
              winner = outcome_info$winner,
              loser = outcome_info$loser,
              outcome = outcome_info$outcome,
              prob = outcome_info$prob,
              winner_scores = rep(60, n_simulations),
              loser_scores = rep(30, n_simulations),
              sample_counter = 1
            )
          }
        }
      }
      
      match_prob_cache[[match_name]] <- list(
        type = "normal",
        p1_name = p1$Name,
        p2_name = p2$Name,
        cum_probs = cum_probs,
        score_samples = score_samples
      )
    }
    
    cat(" - COMPLETED\n")
  }
  
  precomp_elapsed <- difftime(Sys.time(), precomp_start_time, units = "secs")
  cat(sprintf("Pre-computation completed in %.2f seconds\n\n", as.numeric(precomp_elapsed)))
  
  # === VECTORIZED SIMULATION PHASE ===
  cat("=== RUNNING VECTORIZED SIMULATIONS ===\n")
  sim_start_time <- Sys.time()
  
  # Pre-allocate final results table
  estimated_rows <- n_simulations * length(match_prob_cache) * 2
  final_results <- data.table(
    Iteration = integer(estimated_rows),
    Match = character(estimated_rows),
    Player = character(estimated_rows),
    Result = character(estimated_rows),
    Outcome = character(estimated_rows),
    Score = numeric(estimated_rows)
  )
  
  row_idx <- 1
  progress_interval <- max(1000, n_simulations %/% 20)
  
  # Vectorized simulation loop
  for (iter in 1:n_simulations) {
    for (match_name in names(match_prob_cache)) {
      match_info <- match_prob_cache[[match_name]]
      
      if (match_info$type == "walkover") {
        # Walkover - deterministic outcome
        final_results[row_idx, `:=`(
          Iteration = iter, Match = match_name, Player = match_info$winner,
          Result = "Winner", Outcome = "WO", Score = match_info$winner_score
        )]
        row_idx <- row_idx + 1
        
        final_results[row_idx, `:=`(
          Iteration = iter, Match = match_name, Player = match_info$loser,
          Result = "Loser", Outcome = "WO", Score = match_info$loser_score
        )]
        row_idx <- row_idx + 1
        
      } else {
        # Normal match - sample outcome using pre-computed probabilities
        random_val <- runif(1)
        outcome_bin <- findInterval(random_val, match_info$cum_probs)
        
        # Map bin to outcome details
        if (outcome_bin == 1) {
          # P1 wins straight sets
          outcome_type <- "SS"
          winner <- match_info$p1_name
          loser <- match_info$p2_name
        } else if (outcome_bin == 2) {
          # P1 wins non-straight sets
          outcome_type <- "NSS"
          winner <- match_info$p1_name
          loser <- match_info$p2_name
        } else if (outcome_bin == 3) {
          # P2 wins straight sets
          outcome_type <- "SS"
          winner <- match_info$p2_name
          loser <- match_info$p1_name
        } else {
          # P2 wins non-straight sets
          outcome_type <- "NSS"
          winner <- match_info$p2_name
          loser <- match_info$p1_name
        }
        
        # Find matching pre-computed score sample
        target_id <- paste(winner, outcome_type, loser, sep = "_")
        score_sample <- NULL
        sample_idx_in_list <- NULL
        
        for (i in seq_along(match_info$score_samples)) {
          if (match_info$score_samples[[i]]$outcome_id == target_id) {
            score_sample <- match_info$score_samples[[i]]
            sample_idx_in_list <- i
            break
          }
        }
        
        if (!is.null(score_sample)) {
          # Get next pre-computed score
          current_counter <- score_sample$sample_counter
          if (current_counter <= length(score_sample$winner_scores)) {
            winner_score <- score_sample$winner_scores[current_counter]
            loser_score <- score_sample$loser_scores[current_counter]
            
            # Increment counter for next use (with wrap-around)
            match_prob_cache[[match_name]]$score_samples[[sample_idx_in_list]]$sample_counter <- 
              (current_counter %% length(score_sample$winner_scores)) + 1
          } else {
            # Fallback if we somehow run out of samples
            winner_score <- runif(1, 50, 70)
            loser_score <- runif(1, 20, 40)
          }
        } else {
          # Fallback if no sample found
          winner_score <- runif(1, 50, 70)
          loser_score <- runif(1, 20, 40)
        }
        
        # Add winner result
        final_results[row_idx, `:=`(
          Iteration = iter, Match = match_name, Player = winner,
          Result = "Winner", Outcome = outcome_type, Score = winner_score
        )]
        row_idx <- row_idx + 1
        
        # Add loser result
        final_results[row_idx, `:=`(
          Iteration = iter, Match = match_name, Player = loser,
          Result = "Loser", Outcome = outcome_type, Score = loser_score
        )]
        row_idx <- row_idx + 1
      }
    }
    
    # Progress reporting
    if (iter %% progress_interval == 0) {
      elapsed_time <- difftime(Sys.time(), sim_start_time, units = "secs")
      pct_complete <- (iter / n_simulations) * 100
      estimated_total <- elapsed_time * (n_simulations / iter)
      estimated_remaining <- estimated_total - elapsed_time
      
      cat(sprintf("[%s] Simulation progress: %d/%s iterations (%.1f%%) - %.1fs elapsed, ~%.1fs remaining\n",
                  format(Sys.time(), "%H:%M:%S"),
                  iter, format(n_simulations, big.mark = ","),
                  pct_complete, as.numeric(elapsed_time), as.numeric(estimated_remaining)))
    }
  }
  
  # Trim unused rows
  final_results <- final_results[1:(row_idx-1)]
  
  # Report final timing
  overall_end_time <- Sys.time()
  total_elapsed <- difftime(overall_end_time, overall_start_time, units = "mins")
  sim_elapsed <- difftime(overall_end_time, sim_start_time, units = "secs")
  
  cat("\n=== BATCH SIMULATION COMPLETED ===\n")
  cat(sprintf("Pre-computation time: %.2f seconds\n", as.numeric(precomp_elapsed)))
  cat(sprintf("Simulation time: %.2f seconds\n", as.numeric(sim_elapsed)))
  cat(sprintf("Total time: %.2f minutes\n", as.numeric(total_elapsed)))
  cat(sprintf("Generated %s simulation results\n", format(nrow(final_results), big.mark = ",")))
  cat("Timestamp:", format(overall_end_time, "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  # If showdown format, expand results with position multipliers
  if (contest_format == "showdown") {
    cat("=== EXPANDING FOR SHOWDOWN FORMAT ===\n")
    showdown_start <- Sys.time()
    
    final_results <- final_results %>%
      crossing(Position = c("P", "ACPT", "CPT")) %>%
      mutate(
        Multiplier = case_when(
          Position == "P" ~ 1.0,
          Position == "ACPT" ~ 1.25,
          Position == "CPT" ~ 1.5
        ),
        OriginalScore = Score,
        Score = Score * Multiplier,
        PlayerPosition = paste0(Player, "_", Position)
      )
    
    showdown_elapsed <- difftime(Sys.time(), showdown_start, units = "secs")
    cat(sprintf("Showdown expansion completed in %.2f seconds\n", as.numeric(showdown_elapsed)))
    cat(sprintf("Expanded to %s position-specific results\n", format(nrow(final_results), big.mark = ",")))
  }
  
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
  
  # Determine player column naming pattern - handle classic and showdown formats
  if(any(c("CPT", "ACPT", "P") %in% names(optimal_lineups))) {
    # Showdown format
    player_cols <- c("CPT", "ACPT", "P")
  } else if(any(grepl("^Name[1-6]$", names(optimal_lineups)))) {
    # Classic with Name columns
    player_cols <- grep("^Name[1-6]$", names(optimal_lineups), value = TRUE)
  } else if(any(grepl("^Player[1-6]$", names(optimal_lineups)))) {
    # Classic with Player columns
    player_cols <- grep("^Player[1-6]$", names(optimal_lineups), value = TRUE)
  } else {
    stop("Could not find player columns in optimal lineups")
  }
  
  # Get all players from both optimal and random lineups
  all_players_optimal <- unique(unlist(optimal_lineups[, ..player_cols]))
  
  if(!is.null(random_lineups)) {
    # Determine player column pattern in random lineups
    if(any(c("CPT", "ACPT", "P") %in% names(random_lineups))) {
      random_player_cols <- c("CPT", "ACPT", "P")
    } else if(any(grepl("^Name[1-6]$", names(random_lineups)))) {
      random_player_cols <- grep("^Name[1-6]$", names(random_lineups), value = TRUE)
    } else if(any(grepl("^Player[1-6]$", names(random_lineups)))) {
      random_player_cols <- grep("^Player[1-6]$", names(random_lineups), value = TRUE)
    } else {
      random_player_cols <- NULL
    }
    
    if(!is.null(random_player_cols)) {
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
    cat(sprintf("Applied player exclusions: %d lineups remain\n", nrow(pool_lineups)))
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
  
  # For showdown, filter to base position only to avoid triple-counting
  if ("Position" %in% names(simulation_results)) {
    # Showdown format - use only P (1x) scores for analysis
    analysis_data <- simulation_results %>%
      filter(Position == "P")
  } else {
    # Classic format
    analysis_data <- simulation_results
  }
  
  # Analyze scores by player
  player_stats <- analysis_data %>%
    group_by(Player) %>%
    summarize(
      AvgScore = mean(Score, na.rm = TRUE),
      MedianScore = median(Score, na.rm = TRUE),
      StdDev = sd(Score, na.rm = TRUE),
      Min = min(Score, na.rm = TRUE),
      Max = max(Score, na.rm = TRUE),
      n_samples = n(),
      WinPct = mean(Result == "Winner", na.rm = TRUE),
      StraightSetsPct = mean(Result == "Winner" & Outcome == "SS", na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Add salary info - only use columns that exist
  dk_cols <- c("Name", "Salary", "ML")
  
  # Add optional columns if they exist
  if ("ID" %in% names(dk_data)) dk_cols <- c(dk_cols, "ID")
  if ("DKOwn" %in% names(dk_data)) dk_cols <- c(dk_cols, "DKOwn")
  
  player_stats <- left_join(
    player_stats,
    dk_data[, dk_cols],
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
  
  # Get slate size for early termination threshold
  slate_size <- length(unique(sim_dt$Match))
  ew_threshold <- if (slate_size <= 10) 2.8 else 3.0
  cat(sprintf("Slate size: %d matches, EW threshold: %.1f\n", slate_size, ew_threshold))
  
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
  name_col <- if ("Name" %in% names(player_dt)) "Name" else "Player"
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
      
      # Find ONLY the #1 optimal lineup for this iteration
      # Check LP cache first
      lp_key <- paste(
        paste(sort(player_scores$Player), collapse="|"),
        paste(player_scores$Salary, collapse="|"),
        paste(player_scores$Score, collapse="|"),
        sep="||"
      )
      
      if (exists(lp_key, envir = lp_cache)) {
        optimal <- get(lp_key, envir = lp_cache)
      } else {
        # Find optimal lineup
        result <- lp("max", 
                     player_scores$Score, 
                     rbind(player_scores$Salary, rep(1, nrow(player_scores))), 
                     c("<=", "=="), 
                     c(50000, 6), 
                     all.bin = TRUE)
        
        if (result$status != 0) next
        
        optimal <- player_scores[result$solution > 0.5]
        assign(lp_key, optimal, envir = lp_cache)
      }
      
      if (nrow(optimal) == 6) {
        # Create lineup ID
        lineup_id <- paste(sort(optimal$Player), collapse = "|")
        
        # Calculate total wins for this lineup in this iteration
        lineup_players <- optimal$Player
        total_wins <- sum(win_counts[Player %in% lineup_players, wins], na.rm = TRUE)
        
        batch_lineups_found <- batch_lineups_found + 1
        
        # Check if lineup exists in dictionary
        if (exists(lineup_id, envir = lineup_dict)) {
          # Get current counts and increment Top1Count
          current_counts <- get(lineup_id, envir = lineup_dict)
          current_counts$Top1Count <- current_counts$Top1Count + 1
          assign(lineup_id, current_counts, envir = lineup_dict)
        } else {
          # Create new entry
          new_counts <- list(
            Top1Count = 1,
            players = lineup_players
          )
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
    Top1Count = integer(length(lineup_ids)),
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
    
    lineup_counter[i, Top1Count := counts$Top1Count]
  }
  
  cat("\n=== CALCULATING EXPECTED WINS (EW) FOR ALL LINEUPS ===\n")
  ew_calc_start_time <- Sys.time()
  
  # Calculate EW for each lineup using constraint-aware method
  cat("Calculating EW metrics from simulation results...\n")
  ew_metrics <- tryCatch({
    calculate_ew_metrics(sim_dt, total_iterations, dk_data)
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
      
      lineup_counter[i, TotalEW := total_ew]
      
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
  
  # EARLY TERMINATION: Filter by EW threshold before expensive calculations
  cat("\n=== APPLYING EARLY TERMINATION FILTER ===\n")
  original_count <- nrow(lineup_counter)
  
  lineup_counter <- lineup_counter[TotalEW >= ew_threshold]
  
  filtered_count <- nrow(lineup_counter)
  cat(sprintf("Early termination: %s → %s lineups (%.1f%% reduction)\n", 
              format(original_count, big.mark = ","),
              format(filtered_count, big.mark = ","),
              ((original_count - filtered_count) / original_count) * 100))
  
  # Skip detailed calculations if no lineups pass
  if (filtered_count == 0) {
    cat("No lineups meet EW threshold - stopping calculation\n")
    return(lineup_counter)
  }
  
  cat("\n=== CALCULATING SCORE STATISTICS FOR QUALIFYING LINEUPS ===\n")
  score_calc_start_time <- Sys.time()
  cat(sprintf("Calculating scores for %s lineups (EW >= %.1f) across %s iterations\n", 
              format(nrow(lineup_counter), big.mark = ","),
              ew_threshold,
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
      lineup_counter[i, MedianScore := median(lineup_scores, na.rm = TRUE)]
      lineup_counter[i, Score80th := quantile(lineup_scores, 0.8, na.rm = TRUE)]
      
      # Calculate win percentages across ALL iterations
      win6_pct <- (sum(lineup_wins >= 6) / total_iterations) * 100
      win5plus_pct <- (sum(lineup_wins >= 5) / total_iterations) * 100
      
      lineup_counter[i, Win6Pct := win6_pct]
      lineup_counter[i, Win5PlusPct := win5plus_pct]
    } else {
      # Set default values if not enough valid players
      lineup_counter[i, MedianScore := 0]
      lineup_counter[i, Score80th := 0]
      lineup_counter[i, Win6Pct := 0]
      lineup_counter[i, Win5PlusPct := 0]
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
  lineup_counter[, Frequency := Top1Count / total_iterations * 100]
  
  # Sort by TotalEW
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

# Showdown-specific functions
optimize_showdown_lineups <- function(sim_dt, dk_data, n_optimal = 5000) {
  
  cat("\n=== SHOWDOWN LINEUP OPTIMIZATION ===\n")
  start_time <- Sys.time()
  
  # Get unique players
  players <- unique(dk_data$Name)
  n_players <- length(players)
  
  cat(sprintf("Optimizing showdown lineups from %d players\n", n_players))
  
  # Calculate average fantasy points per player from simulations (use base 1x scores)
  player_avg <- sim_dt %>%
    filter(Position == "P") %>%
    group_by(Player) %>%
    summarise(AvgPoints = mean(Score), .groups = 'drop')
  
  # Get salaries
  player_salaries <- dk_data %>%
    select(Name, Salary) %>%
    distinct()
  
  # Combine
  player_data <- left_join(player_avg, player_salaries, by = c("Player" = "Name"))
  
  # Generate ALL possible showdown combinations
  cat("Enumerating all valid showdown lineups...\n")
  
  all_valid_lineups <- list()
  
  # For each possible CPT
  for(i in 1:n_players) {
    cpt_player <- players[i]
    base_salary <- player_data$Salary[player_data$Player == cpt_player]
    cpt_salary <- round((base_salary * 1.5) / 100) * 100  # Round to nearest 100
    cpt_points <- player_data$AvgPoints[player_data$Player == cpt_player] * 1.5
    
    # For each possible ACPT (excluding CPT)
    for(j in 1:n_players) {
      if(i == j) next  # Same player
      
      acpt_player <- players[j]
      base_salary <- player_data$Salary[player_data$Player == acpt_player]
      acpt_salary <- round((base_salary * 1.25) / 100) * 100  # Round to nearest 100
      acpt_points <- player_data$AvgPoints[player_data$Player == acpt_player] * 1.25
      
      # For each possible P (excluding CPT and ACPT)
      for(k in 1:n_players) {
        if(k == i || k == j) next  # Same as CPT or ACPT
        
        p_player <- players[k]
        p_salary <- player_data$Salary[player_data$Player == p_player] * 1.0
        p_points <- player_data$AvgPoints[player_data$Player == p_player] * 1.0
        
        # Check salary constraint
        total_salary <- cpt_salary + acpt_salary + p_salary
        
        if(total_salary <= DK_SALARY_CAP) {
          total_points <- cpt_points + acpt_points + p_points
          
          lineup_id <- paste(cpt_player, acpt_player, p_player, sep = "|")
          
          all_valid_lineups[[lineup_id]] <- list(
            CPT = cpt_player,
            ACPT = acpt_player,
            P = p_player,
            TotalSalary = total_salary,
            TotalPoints = total_points
          )
        }
      }
    }
  }
  
  cat(sprintf("Found %d valid showdown lineups under salary cap\n", length(all_valid_lineups)))
  
  # Sort by total points and take top n_optimal
  lineups_df <- do.call(rbind, lapply(all_valid_lineups, function(x) {
    data.frame(
      CPT = x$CPT,
      ACPT = x$ACPT,
      P = x$P,
      TotalSalary = x$TotalSalary,
      TotalPoints = x$TotalPoints,
      stringsAsFactors = FALSE
    )
  }))
  
  lineups_df <- lineups_df[order(-lineups_df$TotalPoints), ]
  
  # Take top n_optimal or all if fewer exist
  n_to_keep <- min(n_optimal, nrow(lineups_df))
  lineups_df <- lineups_df[1:n_to_keep, ]
  
  # Convert to data.table and add required columns
  lineups_dt <- as.data.table(lineups_df)
  lineups_dt[, LineupID := paste(CPT, ACPT, P, sep = "|")]
  lineups_dt[, Top1Count := 0L]
  lineups_dt[, Top3Count := 0L]
  lineups_dt[, Top5Count := 0L]
  
  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  cat(sprintf("Showdown optimization completed in %.1f seconds\n", as.numeric(elapsed)))
  cat(sprintf("Kept top %d lineups for analysis\n", nrow(lineups_dt)))
  
  return(lineups_dt)
}

analyze_showdown_lineups <- function(lineups_dt, sim_dt) {
  
  cat("\n=== ANALYZING SHOWDOWN LINEUPS ===\n")
  start_time <- Sys.time()
  
  total_lineups <- nrow(lineups_dt)
  total_iterations <- max(sim_dt$Iteration)
  
  cat(sprintf("Analyzing %d lineups across %d iterations\n", total_lineups, total_iterations))
  
  # Create a matrix of lineup scores for all iterations
  lineup_scores_matrix <- matrix(0, nrow = total_lineups, ncol = total_iterations)
  
  # Pre-fetch all player-position scores for efficiency
  sim_dt_wide <- sim_dt %>%
    select(Iteration, Player, Position, Score) %>%
    pivot_wider(names_from = c(Player, Position), 
                values_from = Score,
                names_sep = "_",
                values_fill = 0)
  
  # For each lineup, calculate total score across all iterations
  for(i in 1:nrow(lineups_dt)) {
    lineup <- lineups_dt[i,]
    
    cpt_col <- paste0(lineup$CPT, "_CPT")
    acpt_col <- paste0(lineup$ACPT, "_ACPT")
    p_col <- paste0(lineup$P, "_P")
    
    if(all(c(cpt_col, acpt_col, p_col) %in% names(sim_dt_wide))) {
      lineup_scores_matrix[i, ] <- sim_dt_wide[[cpt_col]] + 
        sim_dt_wide[[acpt_col]] + 
        sim_dt_wide[[p_col]]
    }
    
    if(i %% 500 == 0) {
      cat(sprintf("Calculated scores for %d/%d lineups (%.1f%%)\n", 
                  i, total_lineups, (i/total_lineups)*100))
    }
  }
  
  cat("\nRanking lineups in each iteration...\n")
  
  # For each iteration, rank lineups and count top finishes
  for(iter_idx in 1:total_iterations) {
    iter_scores <- lineup_scores_matrix[, iter_idx]
    ranks <- rank(-iter_scores, ties.method = "min")
    
    lineups_dt[ranks == 1, Top1Count := Top1Count + 1]
    lineups_dt[ranks <= 3, Top3Count := Top3Count + 1]
    lineups_dt[ranks <= 5, Top5Count := Top5Count + 1]
    
    if(iter_idx %% 1000 == 0) {
      cat(sprintf("Ranked iteration %d/%d (%.1f%%)\n", 
                  iter_idx, total_iterations, (iter_idx/total_iterations)*100))
    }
  }
  
  # Sort by Top1Count, then Top3Count, then Top5Count
  setorder(lineups_dt, -Top1Count, -Top3Count, -Top5Count)
  
  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  cat(sprintf("Analysis completed in %.1f seconds\n", as.numeric(elapsed)))
  
  return(lineups_dt)
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
              # Main results section - only shown when analysis is complete
              conditionalPanel(
                condition = "output.optimization_complete === 'true'",
                
                # Weighting box - ONLY for classic format
                conditionalPanel(
                  condition = "output.is_classic_format",
                  fluidRow(
                    box(
                      width = 12,
                      title = "Strategy Selection & Weighted Scoring",
                      status = "info",
                      solidHeader = TRUE,
                      
                      # Strategy preset buttons
                      fluidRow(
                        column(12,
                               h4("Quick Strategy Presets:"),
                               div(style = "margin-bottom: 15px;",
                                   actionButton("preset_cash", "Cash Game", class = "btn-success", style = "margin-right: 10px;"),
                                   actionButton("preset_gpp", "GPP", class = "btn-warning", style = "margin-right: 10px;"),
                                   actionButton("preset_se", "Single Entry", class = "btn-info", style = "margin-right: 10px;"),
                                   actionButton("preset_custom", "Custom", class = "btn-secondary")
                               )
                        )
                      ),
                      
                      h4("Manual Weight Adjustment (0-100 scoring system):"),
                      fluidRow(
                        column(2, sliderInput("weight_totalew", "TotalEW Weight:", min = 0, max = 10, value = 2, step = 1)),
                        column(2, sliderInput("weight_medianscore", "Median Score Weight:", min = 0, max = 10, value = 0, step = 1)),
                        column(2, sliderInput("weight_score80th", "80th %ile Score Weight:", min = 0, max = 10, value = 0, step = 1)),
                        column(2, sliderInput("weight_win6pct", "6-Win % Weight:", min = 0, max = 10, value = 1, step = 1)),
                        column(2, sliderInput("weight_win5pluspct", "5+ Win % Weight:", min = 0, max = 10, value = 1, step = 1)),
                        column(2, sliderInput("weight_top1count", "Top 1 Count Weight:", min = 0, max = 10, value = 1, step = 1))
                      ),
                      
                      fluidRow(
                        column(4, actionButton("apply_weights", "Apply Weights & Re-score", class = "btn-primary", style = "width: 100%;")),
                        column(4, actionButton("reset_weights", "Reset to Defaults", class = "btn-warning", style = "width: 100%;"))
                      )
                    )
                  )
                ),
                
                # Random lineup selector - ONLY for showdown format
                conditionalPanel(
                  condition = "!output.is_classic_format",
                  fluidRow(
                    box(
                      width = 12,
                      title = "Random Lineup Selection",
                      status = "info",
                      solidHeader = TRUE,
                      fluidRow(
                        column(4,
                               numericInput("showdown_top_n", "Select from Top X Lineups:", 
                                            value = 100, min = 1, max = 1000, step = 10)
                        ),
                        column(4,
                               numericInput("showdown_num_lineups", "Number of Lineups to Generate:", 
                                            value = 20, min = 1, max = 150, step = 1)
                        ),
                        column(4,
                               div(style = "margin-top: 25px;",
                                   actionButton("showdown_generate", "Generate Random Lineups", 
                                                class = "btn-primary", style = "width: 100%;")
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               div(style = "margin-top: 10px;",
                                   downloadButton("download_showdown_random", "Download Random Lineups",
                                                  style = "width: 100%;")
                               )
                        )
                      )
                    )
                  )
                ),
                
                # Optimal lineups results table
                fluidRow(
                  box(
                    width = 12,
                    title = "Optimal Lineups (Scored & Ranked)",
                    div(
                      style = "text-align: right; margin-bottom: 10px;",
                      downloadButton('download_optimal_lineups', 'Download All Lineups',
                                     style = "margin-top: 10px;")
                    ),
                    DTOutput("optimal_lineups_table") %>% withSpinner(color = "#FFD700")
                  )
                )
              ),
              
              # Show this when analysis hasn't been run yet
              conditionalPanel(
                condition = "!output.simulation_complete",
                fluidRow(
                  box(
                    width = 12,
                    status = "warning",
                    title = "No Analysis Complete",
                    HTML("Please run the simulation first using the <strong>Run Simulation</strong> button in the sidebar. 
                   This will automatically generate optimal lineups with the new scoring system.")
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
                    "Please run the simulation first using the Run Simulation button in the sidebar. This will automatically generate optimal lineups."
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
                               div(class = "well well-sm", style = "margin-top: 5px;",
                                   h5("Pool Statistics:"),
                                   textOutput("filtered_pool_stats")
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
                      )
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
    optimization_complete = FALSE,  # Explicitly initialize to FALSE
    contest_format = "classic"  # Track contest format (classic or showdown)
  )
  
  
  # Expose reactive values as outputs
  output$simulation_complete <- reactive({
    return(rv$simulation_complete)
  })
  outputOptions(output, "simulation_complete", suspendWhenHidden = FALSE)
  
  output$filtered_pool_stats <- renderText({
    req(rv$dk_optimal_lineups_ranked)
    req(input$top_lineups_pool)
    
    # Get initial pool
    pool_lineups <- head(rv$dk_optimal_lineups_ranked, input$top_lineups_pool)
    initial_count <- nrow(pool_lineups)
    
    # Apply player exclusions only
    if (!is.null(input$excluded_players) && length(input$excluded_players) > 0) {
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
    
    final_count <- nrow(pool_lineups)
    
    paste0("Filtered Pool: ", final_count, " / ", initial_count, " lineups")
  })
  
  output$optimization_complete <- reactive({
    # Convert boolean TRUE/FALSE to lowercase string "true"/"false"
    result <- tolower(as.character(!is.null(rv$dk_optimal_lineups) && nrow(rv$dk_optimal_lineups) > 0))
    return(result)
  })
  outputOptions(output, "optimization_complete", suspendWhenHidden = FALSE)
  
  output$is_classic_format <- reactive({
    return(is.null(rv$contest_format) || rv$contest_format == "classic")
  })
  outputOptions(output, "is_classic_format", suspendWhenHidden = FALSE)
  
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
      
      # Default surface to Hard (no dialog)
      if (!"Surface" %in% colnames(dk_data)) {
        dk_data$Surface <- "Hard"
      }
      
      # Store data
      rv$dk_data <- dk_data
      rv$file_uploaded <- TRUE
      
      # Detect contest format
      rv$contest_format <- detect_contest_format(dk_data)
      
      # Show notification with format
      showNotification(
        paste0("Data loaded successfully! Format: ", toupper(rv$contest_format)),
        type = "message",
        duration = 5
      )
      
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
    
    # Run complete process with progress indicator
    withProgress(message = 'Running complete analysis...', value = 0, {
      
      # Step 1: Run match simulations
      incProgress(0.1, detail = "Simulating matches...")
      simulation_results <- run_batch_simulation(rv$dk_data, rv$score_history, input$n_sims, rv$contest_format)
      
      # Step 2: Analyze player projections
      incProgress(0.3, detail = "Calculating player projections...")
      player_projections <- analyze_player_scores(simulation_results, rv$dk_data)
      
      # Store intermediate results
      rv$simulation_results <- simulation_results
      rv$player_projections <- player_projections
      rv$simulation_complete <- TRUE
      
      # Step 3: Find optimal lineups (format-specific)
      incProgress(0.5, detail = "Finding optimal lineups...")
      
      if(rv$contest_format == "showdown") {
        # Showdown optimization
        optimal_lineups <- optimize_showdown_lineups(
          sim_dt = simulation_results,
          dk_data = rv$dk_data,
          n_optimal = 5000
        )
        
        # Analyze showdown lineups
        incProgress(0.7, detail = "Analyzing showdown lineups...")
        optimal_lineups <- analyze_showdown_lineups(
          lineups_dt = optimal_lineups,
          sim_dt = simulation_results
        )
        
        # For showdown, scored and ranked are the same (no additional scoring needed)
        rv$dk_optimal_lineups <- optimal_lineups
        rv$dk_optimal_lineups_scored <- optimal_lineups
        rv$dk_optimal_lineups_ranked <- optimal_lineups
        rv$optimization_complete <- TRUE
        
      } else {
        # Classic optimization
        optimal_lineups <- find_all_optimal_lineups(
          rv$simulation_results, 
          rv$dk_data,
          rv$dk_data
        )
        
        # Step 4: Expand lineup details
        incProgress(0.7, detail = "Processing lineup details...")
        expanded_lineups <- expand_lineup_details(
          optimal_lineups,
          rv$dk_data,
          NULL
        )
        
        # Step 5: Calculate scores and rankings
        incProgress(0.8, detail = "Calculating scores and rankings...")
        rv$dk_optimal_lineups <- expanded_lineups
        
        # Calculate 0-100 scores instead of simple ranks
        scored_lineups <- calculate_lineup_scores(expanded_lineups)
        
        # Calculate ownership metrics if available
        scored_lineups <- calculate_ownership_metrics(scored_lineups, rv$dk_data)
        
        # Store the scored version
        rv$dk_optimal_lineups_scored <- scored_lineups
        
        # Apply initial weighted scoring with default weights
        default_weights <- list(
          weight_totalew = 2,           
          weight_medianscore = 0,       
          weight_score80th = 0,         
          weight_win6pct = 1,           
          weight_win5pluspct = 1,       
          weight_top1count = 1        
        )
        
        rv$dk_optimal_lineups_ranked <- calculate_weighted_score(scored_lineups, default_weights)
        rv$optimization_complete <- TRUE
      }
      
      # Step 6: Calculate initial player exposure
      incProgress(0.9, detail = "Calculating player exposure...")
      if(!is.null(rv$dk_optimal_lineups)) {
        rv$dk_player_exposure <- calculate_player_exposure(
          rv$dk_optimal_lineups, 
          rv$player_projections
        )
      }
      
      # Update player selection dropdown and excluded players
      if (!is.null(rv$dk_optimal_lineups)) {
        # Update player selection dropdown
        updateSelectizeInput(
          session,
          "selected_players",
          choices = unique(player_projections$Player),
          selected = head(unique(player_projections$Player), 5)
        )
        
        # Get unique players from all lineups for exclusion dropdown
        if(rv$contest_format == "showdown") {
          # Showdown format
          player_cols <- c("CPT", "ACPT", "P")
        } else {
          # Classic format
          player_cols <- grep("^Name[1-6]$", names(rv$dk_optimal_lineups), value = TRUE)
        }
        
        all_players <- c()
        for(col in player_cols) {
          if(col %in% names(rv$dk_optimal_lineups)) {
            all_players <- c(all_players, rv$dk_optimal_lineups[[col]])
          }
        }
        all_players <- unique(all_players[!is.na(all_players)])
        
        # Calculate pool exposure for each player
        pool_exposure <- sapply(all_players, function(player) {
          player_appears <- logical(nrow(rv$dk_optimal_lineups))
          for(col in player_cols) {
            if(col %in% names(rv$dk_optimal_lineups)) {
              player_appears <- player_appears | (rv$dk_optimal_lineups[[col]] == player)
            }
          }
          exposure_pct <- (sum(player_appears) / nrow(rv$dk_optimal_lineups)) * 100
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
      
      incProgress(1.0, detail = "Complete!")
      
      # Show success message
      lineup_count <- if(!is.null(rv$dk_optimal_lineups)) nrow(rv$dk_optimal_lineups) else 0
      
      showModal(modalDialog(
        title = "Analysis Complete!",
        HTML(sprintf(
          "Successfully completed full analysis!<br><br>
        ✅ <b>%s</b> simulations run<br>
        ✅ <b>%d</b> optimal lineups generated<br>
        ✅ Expected Wins (EW) metrics calculated<br>
        ✅ Weighted ranking applied<br><br>",
          format(input$n_sims, big.mark = ","),
          lineup_count
        )),
        easyClose = TRUE
      ))
    })
    
    # Switch to optimal lineups tab to show results
    updateTabItems(session, "sidebar_menu", selected = "optimal_lineups")
    
    # Final cleanup
    gc(full = TRUE)
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
    # Check for showdown format
    if (!is.null(rv$contest_format) && rv$contest_format == "showdown") {
      # SHOWDOWN DISPLAY
      lineup_data <- if (!is.null(rv$dk_optimal_lineups_ranked)) {
        rv$dk_optimal_lineups_ranked
      } else {
        rv$dk_optimal_lineups
      }
      
      if (is.null(lineup_data) || nrow(lineup_data) == 0) {
        return(datatable(
          data.frame(Message = "No showdown lineups available."),
          options = list(dom = "t", ordering = FALSE),
          rownames = FALSE
        ))
      }
      
      display_data <- as.data.frame(lineup_data) %>%
        select(CPT, ACPT, P, Top1Count, Top3Count, Top5Count, TotalSalary) %>%
        mutate(
          Top1Count = as.integer(Top1Count),
          Top3Count = as.integer(Top3Count),
          Top5Count = as.integer(Top5Count)
        )
      
      dt <- datatable(
        display_data,
        options = list(
          scrollX = TRUE,
          pageLength = 50,
          order = list(list(3, 'desc'))  # Sort by Top1Count
        ),
        colnames = c("Captain", "Asst Captain", "Player", "Top 1", "Top 3", "Top 5", "Salary"),
        class = 'cell-border stripe',
        rownames = FALSE
      )
      
      # Style count columns with gold bars
      for(col in c("Top1Count", "Top3Count", "Top5Count")) {
        if(col %in% names(display_data)) {
          dt <- dt %>% formatStyle(
            col,
            background = styleColorBar(c(0, max(display_data[[col]], na.rm = TRUE)), '#FFD700'),
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
        }
      }
      
      return(dt %>% formatCurrency('TotalSalary', currency = "$", digits = 0))
    }
    
    # CLASSIC DISPLAY (original code follows)
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
                          "Top1Count")
    
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
    
    req(rv$dk_optimal_lineups_scored)
    
    # Create weights list from input values
    weights <- list(
      weight_totalew = input$weight_totalew,
      weight_medianscore = input$weight_medianscore,
      weight_score80th = input$weight_score80th,
      weight_win6pct = input$weight_win6pct,
      weight_win5pluspct = input$weight_win5pluspct,
      weight_top1count = input$weight_top1count
    )
    
    # Calculate weighted score
    rv$dk_optimal_lineups_ranked <- calculate_weighted_score(rv$dk_optimal_lineups_scored, weights)
    
    showNotification("Weighted scoring applied successfully!", type = "message")
  })
  
  
  observeEvent(input$reset_weights, {
    updateSliderInput(session, "weight_totalew", value = 2)      # Changed from 3 to 2
    updateSliderInput(session, "weight_medianscore", value = 0)  # Changed from 2 to 0
    updateSliderInput(session, "weight_score80th", value = 0)    # Changed from 2 to 0
    updateSliderInput(session, "weight_win6pct", value = 1)      # Keep at 1
    updateSliderInput(session, "weight_win5pluspct", value = 1)  # Keep at 1
    updateSliderInput(session, "weight_top1count", value = 1)    # Changed from 2 to 1
    
    showNotification("Weights reset to defaults", type = "message")
  })
  
  
  # Add weight summary output
  output$weight_summary <- renderText({
    total_weight <- input$weight_totalew + input$weight_medianscore + input$weight_score80th + 
      input$weight_win6pct + input$weight_win5pluspct + input$weight_top1count
    
    if (total_weight == 0) return("No weights applied")
    
    paste("Total Weight:", round(total_weight, 1), 
          "| TotalEW:", round((input$weight_totalew/total_weight)*100, 1), "%",
          "| Scores:", round(((input$weight_medianscore + input$weight_score80th)/total_weight)*100, 1), "%",
          "| Wins:", round(((input$weight_win6pct + input$weight_win5pluspct)/total_weight)*100, 1), "%",
          "| Top1:", round((input$weight_top1count/total_weight)*100, 1), "%")
  })
  
  
  output$weighted_pool_stats <- renderText({
    req(rv$dk_optimal_lineups_ranked)
    req(input$top_lineups_pool)
    
    # Validate input before calling function
    if (is.null(input$top_lineups_pool) || is.na(input$top_lineups_pool) || input$top_lineups_pool <= 0) {
      return("Invalid pool size")
    }
    
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
      # Get unique players from all lineups - handle both formats
      if(rv$contest_format == "showdown") {
        player_cols <- c("CPT", "ACPT", "P")
      } else {
        player_cols <- grep("^Name[1-6]$", names(rv$dk_optimal_lineups_ranked), value = TRUE)
        if(length(player_cols) == 0) {
          player_cols <- grep("^Player[1-6]$", names(rv$dk_optimal_lineups_ranked), value = TRUE)
        }
      }
      
      all_players <- c()
      for(col in player_cols) {
        if(col %in% names(rv$dk_optimal_lineups_ranked)) {
          all_players <- c(all_players, rv$dk_optimal_lineups_ranked[[col]])
        }
      }
      all_players <- unique(all_players[!is.na(all_players)])
      
      # Calculate pool exposure for each player (from top 1000 lineups)
      top_pool <- head(rv$dk_optimal_lineups_ranked, 1000)
      pool_exposure <- sapply(all_players, function(player) {
        player_appears <- logical(nrow(top_pool))
        for(col in player_cols) {
          if(col %in% names(top_pool)) {
            player_appears <- player_appears | (top_pool[[col]] == player)
          }
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
  
  # Showdown random lineup generation
  observeEvent(input$showdown_generate, {
    req(rv$dk_optimal_lineups_ranked)
    req(rv$contest_format == "showdown")
    
    top_n <- min(input$showdown_top_n, nrow(rv$dk_optimal_lineups_ranked))
    num_lineups <- min(input$showdown_num_lineups, top_n)
    
    # Sample randomly from top N
    top_lineups <- head(rv$dk_optimal_lineups_ranked, top_n)
    sampled_indices <- sample(1:nrow(top_lineups), num_lineups, replace = FALSE)
    
    rv$dk_random_lineups <- top_lineups[sampled_indices, ]
    
    showNotification(
      sprintf("Generated %d random lineups from top %d!", num_lineups, top_n),
      type = "message"
    )
  })
  
  # Download handler for showdown random lineups
  output$download_showdown_random <- downloadHandler(
    filename = function() {
      paste0("tennis_showdown_random_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      if(is.null(rv$dk_random_lineups) || nrow(rv$dk_random_lineups) == 0) {
        write.csv(data.frame(), file, row.names = FALSE)
        return()
      }
      
      lineups_df <- as.data.frame(rv$dk_random_lineups)
      
      # Add IDs based on position
      get_showdown_id <- function(name, position) {
        player_info <- rv$dk_data[rv$dk_data$Name == name, ]
        if (nrow(player_info) > 0) {
          id_col <- switch(position,
                           "CPT" = "CID",
                           "ACPT" = "AID",
                           "P" = "PID")
          if (id_col %in% names(player_info)) {
            return(paste0(name, " (", player_info[[id_col]][1], ")"))
          }
        }
        return(name)
      }
      
      lineups_df$CPT <- sapply(lineups_df$CPT, function(x) get_showdown_id(x, "CPT"))
      lineups_df$ACPT <- sapply(lineups_df$ACPT, function(x) get_showdown_id(x, "ACPT"))
      lineups_df$P <- sapply(lineups_df$P, function(x) get_showdown_id(x, "P"))
      
      download_data <- lineups_df %>%
        select(CPT, ACPT, P, Top1Count, Top3Count, Top5Count, TotalSalary)
      
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
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
      
      # Update player exposure data - FIXED VERSION
      if(!is.null(rv$dk_random_lineups)) {
        # Calculate Pool_Exposure from the FILTERED pool (after exclusions)
        pool_lineups <- head(rv$dk_optimal_lineups_ranked, input$top_lineups_pool)
        
        # Apply the same player exclusions to the pool
        if (!is.null(input$excluded_players) && length(input$excluded_players) > 0) {
          # Determine player columns based on format
          if(!is.null(rv$contest_format) && rv$contest_format == "showdown") {
            player_cols <- c("CPT", "ACPT", "P")
          } else {
            player_cols <- if(any(grepl("^Name[1-6]$", names(pool_lineups)))) {
              grep("^Name[1-6]$", names(pool_lineups), value = TRUE)
            } else {
              grep("^Player[1-6]$", names(pool_lineups), value = TRUE)
            }
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
        
        rv$dk_player_exposure <- calculate_player_exposure(
          pool_lineups,         # Use the FILTERED pool for Pool_Exposure
          rv$player_projections,
          rv$dk_random_lineups  # Generated lineups for Randomized_Exposure
        )
        
        showModal(modalDialog(
          title = "Success",
          sprintf("Generated %d lineups from filtered pool of %d lineups!", 
                  nrow(rv$dk_random_lineups), nrow(pool_lineups)),
          easyClose = TRUE
        ))
      } else {
        showModal(modalDialog(
          title = "Error",
          "No lineups available after applying filters. Try reducing exclusions or increasing the pool size.",
          easyClose = TRUE
        ))
      }
    })
  })
  
  # Preset strategy observers
  observeEvent(input$preset_cash, {
    slate_size <- if (!is.null(rv$dk_data)) length(unique(rv$dk_data$`Game Info`)) else 12
    
    if (slate_size <= 10) {
      # 8-10 matches cash
      updateSliderInput(session, "weight_totalew", value = 3)
      updateSliderInput(session, "weight_win6pct", value = 0)
      updateSliderInput(session, "weight_win5pluspct", value = 3)
      updateSliderInput(session, "weight_top1count", value = 2)
      updateSliderInput(session, "weight_score80th", value = 0)
      updateSliderInput(session, "weight_medianscore", value = 2)
    } else if (slate_size <= 16) {
      # 11-16 matches cash
      updateSliderInput(session, "weight_totalew", value = 3)
      updateSliderInput(session, "weight_win6pct", value = 0)
      updateSliderInput(session, "weight_win5pluspct", value = 3)
      updateSliderInput(session, "weight_top1count", value = 1)
      updateSliderInput(session, "weight_score80th", value = 1)
      updateSliderInput(session, "weight_medianscore", value = 2)
    } else {
      # 16+ matches cash
      updateSliderInput(session, "weight_totalew", value = 2)
      updateSliderInput(session, "weight_win6pct", value = 3)
      updateSliderInput(session, "weight_win5pluspct", value = 1)
      updateSliderInput(session, "weight_top1count", value = 0)
      updateSliderInput(session, "weight_score80th", value = 2)
      updateSliderInput(session, "weight_medianscore", value = 2)
    }
    showNotification("Cash game strategy applied!", type = "message")
  })
  
  observeEvent(input$preset_gpp, {
    slate_size <- if (!is.null(rv$dk_data)) length(unique(rv$dk_data$`Game Info`)) else 12
    
    if (slate_size <= 10) {
      # 8-10 matches GPP
      updateSliderInput(session, "weight_totalew", value = 0)
      updateSliderInput(session, "weight_win6pct", value = 0)
      updateSliderInput(session, "weight_win5pluspct", value = 3)
      updateSliderInput(session, "weight_top1count", value = 6)
      updateSliderInput(session, "weight_score80th", value = 1)
      updateSliderInput(session, "weight_medianscore", value = 0)
    } else if (slate_size <= 16) {
      # 11-16 matches GPP
      updateSliderInput(session, "weight_totalew", value = 0)
      updateSliderInput(session, "weight_win6pct", value = 5)
      updateSliderInput(session, "weight_win5pluspct", value = 2)
      updateSliderInput(session, "weight_top1count", value = 2)
      updateSliderInput(session, "weight_score80th", value = 1)
      updateSliderInput(session, "weight_medianscore", value = 0)
    } else {
      # 16+ matches GPP
      updateSliderInput(session, "weight_totalew", value = 0)
      updateSliderInput(session, "weight_win6pct", value = 7)
      updateSliderInput(session, "weight_win5pluspct", value = 1)
      updateSliderInput(session, "weight_top1count", value = 1)
      updateSliderInput(session, "weight_score80th", value = 1)
      updateSliderInput(session, "weight_medianscore", value = 0)
    }
    showNotification("GPP strategy applied!", type = "message")
  })
  
  observeEvent(input$preset_se, {
    slate_size <- if (!is.null(rv$dk_data)) length(unique(rv$dk_data$`Game Info`)) else 12
    
    if (slate_size <= 10) {
      # 8-10 matches SE
      updateSliderInput(session, "weight_totalew", value = 0)
      updateSliderInput(session, "weight_win6pct", value = 0)
      updateSliderInput(session, "weight_win5pluspct", value = 5)
      updateSliderInput(session, "weight_top1count", value = 3)
      updateSliderInput(session, "weight_score80th", value = 2)
      updateSliderInput(session, "weight_medianscore", value = 0)
    } else if (slate_size <= 16) {
      # 11-16 matches SE
      updateSliderInput(session, "weight_totalew", value = 1)
      updateSliderInput(session, "weight_win6pct", value = 3)
      updateSliderInput(session, "weight_win5pluspct", value = 3)
      updateSliderInput(session, "weight_top1count", value = 2)
      updateSliderInput(session, "weight_score80th", value = 1)
      updateSliderInput(session, "weight_medianscore", value = 0)
    } else {
      # 16+ matches SE
      updateSliderInput(session, "weight_totalew", value = 1)
      updateSliderInput(session, "weight_win6pct", value = 5)
      updateSliderInput(session, "weight_win5pluspct", value = 2)
      updateSliderInput(session, "weight_top1count", value = 1)
      updateSliderInput(session, "weight_score80th", value = 1)
      updateSliderInput(session, "weight_medianscore", value = 0)
    }
    showNotification("Single Entry strategy applied!", type = "message")
  })
  
  observeEvent(input$preset_custom, {
    showNotification("Custom mode - adjust sliders manually", type = "message")
  })
  
  observe({
    req(rv$dk_optimal_lineups_ranked)
    
    # This will trigger whenever top_lineups_pool or excluded_players change
    input$top_lineups_pool
    input$excluded_players 
    
    
    # Validate top_lineups_pool value
    if (is.null(input$top_lineups_pool) || is.na(input$top_lineups_pool) || input$top_lineups_pool <= 0) {
      return()
    }
    
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
    count_cols <- c("Top1Count")
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
      format_label <- if(!is.null(rv$contest_format) && rv$contest_format == "showdown") "showdown" else "classic"
      paste0("tennis_optimal_lineups_", format_label, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      # Use ranked data if available
      lineup_data <- if (!is.null(rv$dk_optimal_lineups_ranked)) {
        rv$dk_optimal_lineups_ranked
      } else {
        rv$dk_optimal_lineups
      }
      
      if(is.null(lineup_data) || nrow(lineup_data) == 0) {
        write.csv(data.frame(), file, row.names = FALSE)
        return()
      }
      
      lineups_df <- as.data.frame(lineup_data)
      
      # Handle showdown format differently
      if(!is.null(rv$contest_format) && rv$contest_format == "showdown") {
        # Showdown columns - add IDs based on position
        get_showdown_id <- function(name, position) {
          player_info <- rv$dk_data[rv$dk_data$Name == name, ]
          if (nrow(player_info) > 0) {
            id_col <- switch(position,
                             "CPT" = "CID",
                             "ACPT" = "AID", 
                             "P" = "PID")
            if (id_col %in% names(player_info)) {
              return(paste0(name, " (", player_info[[id_col]][1], ")"))
            }
          }
          return(name)
        }
        
        # Add IDs to each position
        lineups_df$CPT <- sapply(lineups_df$CPT, function(x) get_showdown_id(x, "CPT"))
        lineups_df$ACPT <- sapply(lineups_df$ACPT, function(x) get_showdown_id(x, "ACPT"))
        lineups_df$P <- sapply(lineups_df$P, function(x) get_showdown_id(x, "P"))
        
        download_data <- lineups_df %>%
          select(CPT, ACPT, P, Top1Count, Top3Count, Top5Count, TotalSalary)
        write.csv(download_data, file, row.names = FALSE)
        return()
      }
      
      # Classic format handling (original code)
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
                       "Top1Count")
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
      format_label <- if(!is.null(rv$contest_format) && rv$contest_format == "showdown") "showdown" else "classic"
      paste0("tennis_generated_lineups_", format_label, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      if(is.null(rv$dk_random_lineups) || nrow(rv$dk_random_lineups) == 0) {
        write.csv(data.frame(), file, row.names = FALSE)
        return()
      }
      
      # Get generated lineup data
      lineups_df <- as.data.frame(rv$dk_random_lineups)
      
      # Handle showdown format
      if(!is.null(rv$contest_format) && rv$contest_format == "showdown") {
        download_data <- lineups_df %>%
          select(CPT, ACPT, P, Top1Count, Top3Count, Top5Count, TotalSalary)
        write.csv(download_data, file, row.names = FALSE)
        return()
      }
      
      # Classic format handling (original code)
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
        "Top1Count", "TotalSalary"
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