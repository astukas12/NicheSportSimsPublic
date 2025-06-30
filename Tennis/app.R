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

# Calculate Expected Wins and high-win percentages from simulation results
calculate_ew_metrics <- function(simulation_results, n_simulations) {
  cat("Calculating EW metrics from simulation results...\n")
  
  # Convert to data.table for performance
  sim_dt <- as.data.table(simulation_results)
  
  # Calculate win counts per iteration for each player
  win_counts <- sim_dt[Result == "Winner", .N, by = .(Iteration, Player)]
  
  # Create a complete grid of all iterations and players (including 0 wins)
  all_iterations <- unique(sim_dt$Iteration)
  all_players <- unique(sim_dt$Player)
  
  complete_grid <- CJ(Iteration = all_iterations, Player = all_players)
  
  # Merge with actual win counts (NAs become 0)
  win_counts_complete <- merge(complete_grid, win_counts, 
                               by = c("Iteration", "Player"), all.x = TRUE)
  win_counts_complete[is.na(N), N := 0]
  
  # Calculate metrics for each player
  ew_metrics <- win_counts_complete[, .(
    EW = mean(N, na.rm = TRUE),  # Expected Wins
    Win6Plus = mean(N >= 6, na.rm = TRUE) * 100,  # 6+ win %
    Win5Plus = mean(N >= 5, na.rm = TRUE) * 100   # 5+ win %
  ), by = Player]
  
  cat("EW metrics calculated for", nrow(ew_metrics), "players\n")
  return(ew_metrics)
}


# Calculate filtered pool stats
calculate_filtered_pool_stats <- function(optimal_lineups, filters) {
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
  
  # Apply filters
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
  
  if (!is.null(filters$min_total_ew) && filters$min_total_ew > 0 && "TotalEW" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[TotalEW >= filters$min_total_ew]
  }
  if (!is.null(filters$min_win6_pct) && filters$min_win6_pct > 0 && "Win6Pct" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Win6Pct >= filters$min_win6_pct]
  }
  
  if (!is.null(filters$min_win5plus_pct) && filters$min_win5plus_pct > 0 && "Win5PlusPct" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Win5PlusPct >= filters$min_win5plus_pct]
  }
  
  if (!is.null(filters$min_win4plus_pct) && filters$min_win4plus_pct > 0 && "Win4PlusPct" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Win4PlusPct >= filters$min_win4plus_pct]
  }
  
  # Apply player exclusion filter safely
  if (!is.null(filters$excluded_players) && length(filters$excluded_players) > 0) {
    # Determine the player column naming pattern - check both formats
    player_pattern <- if(any(grepl("^Name[1-6]$", names(filtered_lineups)))) {
      "^Name[1-6]$"
    } else if(any(grepl("^Player[1-6]$", names(filtered_lineups)))) {
      "^Player[1-6]$"
    } else {
      NULL
    }
    
    if(!is.null(player_pattern)) {
      # Get player columns
      player_cols <- grep(player_pattern, names(filtered_lineups), value = TRUE)
      
      if(length(player_cols) > 0) {
        # Initialize vector for tracking which rows to exclude
        to_exclude <- rep(FALSE, nrow(filtered_lineups))
        
        # Check each player column
        for(col in player_cols) {
          # Update the exclusion vector if any excluded player is found
          to_exclude <- to_exclude | filtered_lineups[[col]] %in% filters$excluded_players
        }
        
        # Keep only the rows that don't contain excluded players
        filtered_lineups <- filtered_lineups[!to_exclude]
      }
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
      best_of_value <- ifelse(tour == "ATP", 5, 3)
      
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



# Calculate player exposure
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
  
  # Get all players from the optimal lineups
  all_players <- unique(unlist(optimal_lineups[, ..player_cols]))
  
  # Initialize metrics data frame
  metrics_data <- data.table(
    Player = all_players,
    Salary = NA_real_,
    DKOwn = NA_real_,
    OptimalRate = 0,
    EliteRate = 0,
    FloorRate = 0,
    AppearanceRate = 0,
    Exposure = 0,
    Leverage = 0
  )
  
  # Match with fantasy analysis data using direct matching
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
  
  # Calculate OptimalRate (percentage of Top1Count lineups with this player)
  total_top1 <- sum(optimal_lineups$Top1Count, na.rm = TRUE)
  if(total_top1 > 0) {
    for(player in all_players) {
      # Find lineups with this player
      player_appears <- logical(nrow(optimal_lineups))
      for(col in player_cols) {
        player_appears <- player_appears | (optimal_lineups[[col]] == player)
      }
      player_matches <- which(player_appears)
      
      # Calculate optimal rate percentage
      player_total <- sum(optimal_lineups$Top1Count[player_matches], na.rm = TRUE)
      metrics_data[Player == player, OptimalRate := (player_total / total_top1) * 100]
    }
  }
  
  # Calculate EliteRate (top 10% of lineups by Top1Count)
  if(nrow(optimal_lineups) >= 10) {
    elite_lineups <- copy(optimal_lineups)
    elite_lineups <- elite_lineups[order(-Top1Count, -Top5Count)]
    
    n_elite <- max(1, round(nrow(elite_lineups) * 0.1))
    elite_lineups <- elite_lineups[1:n_elite]
    
    for(player in all_players) {
      # Count appearances in elite lineups
      player_appears <- logical(nrow(elite_lineups))
      for(col in player_cols) {
        player_appears <- player_appears | (elite_lineups[[col]] == player)
      }
      player_elite_count <- sum(player_appears)
      
      # Calculate elite rate
      metrics_data[Player == player, EliteRate := (player_elite_count / n_elite) * 100]
    }
  }
  
  # Calculate FloorRate (top 20% of lineups by Top5Count)
  if(nrow(optimal_lineups) >= 5) {
    floor_lineups <- copy(optimal_lineups)
    floor_lineups <- floor_lineups[order(-Top5Count, -Top1Count)]
    
    n_floor <- max(1, round(nrow(floor_lineups) * 0.2))
    floor_lineups <- floor_lineups[1:n_floor]
    
    for(player in all_players) {
      # Count appearances in floor lineups
      player_appears <- logical(nrow(floor_lineups))
      for(col in player_cols) {
        player_appears <- player_appears | (floor_lineups[[col]] == player)
      }
      player_floor_count <- sum(player_appears)
      
      # Calculate floor rate
      metrics_data[Player == player, FloorRate := (player_floor_count / n_floor) * 100]
    }
  }
  
  # Calculate AppearanceRate (percentage of all lineups with this player)
  if(nrow(optimal_lineups) > 0) {
    for(player in all_players) {
      # Count appearances in all lineups
      player_appears <- logical(nrow(optimal_lineups))
      for(col in player_cols) {
        player_appears <- player_appears | (optimal_lineups[[col]] == player)
      }
      player_appearance_count <- sum(player_appears)
      
      # Calculate appearance rate
      metrics_data[Player == player, AppearanceRate := (player_appearance_count / nrow(optimal_lineups)) * 100]
    }
  }
  
  # Calculate Exposure from random lineups
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
          metrics_data[Player == player, Exposure := (sum(player_appears) / nrow(random_lineups)) * 100]
        }
      }
    }
  }
  
  # Calculate leverage
  metrics_data[!is.na(DKOwn), Leverage := Exposure - (DKOwn*100)]
  
  # Sort by OptimalRate
  setorder(metrics_data, -OptimalRate)
  
  return(as.data.frame(metrics_data))
}


# Generate random lineups
generate_random_lineups <- function(optimal_lineups, filters) {
  # Convert to data.table for efficiency
  setDT(optimal_lineups)
  filtered_lineups <- copy(optimal_lineups)
  
  # Apply filters
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
  
  # Check if any lineups match filters
  if (nrow(filtered_lineups) == 0) {
    return(NULL)
  }
  
  # Prepare for tracking
  # Check if columns are named "Name1", "Name2", etc. or "Player1", "Player2", etc.
  player_cols <- if(any(grepl("^Name[1-6]$", names(filtered_lineups)))) {
    grep("^Name[1-6]$", names(filtered_lineups), value = TRUE)
  } else {
    grep("^Player[1-6]$", names(filtered_lineups), value = TRUE)
  }
  
  if(length(player_cols) == 0) {
    stop("Could not find player columns in the lineup data")
  }
  
  all_players <- unique(unlist(filtered_lineups[, ..player_cols]))
  player_counts <- setNames(numeric(length(all_players)), all_players)
  
  # Sample lineups using Top1Count as weight
  weight_col <- "Top1Count"
  selected_lineups <- data.table()
  selected_indices <- integer(0)
  
  # Sampling with minimal attempt tracking
  attempts <- 0
  max_attempts <- filters$num_lineups * 10
  
  while (nrow(selected_lineups) < filters$num_lineups && attempts < max_attempts) {
    attempts <- attempts + 1
    
    # Available lineups
    available_indices <- setdiff(1:nrow(filtered_lineups), selected_indices)
    if (length(available_indices) == 0) break
    
    # Sample based on weights
    weights <- filtered_lineups[[weight_col]][available_indices]
    if (sum(weights) == 0) weights <- rep(1, length(available_indices))
    
    selected_idx <- tryCatch({
      sample(available_indices, 1, prob = weights)
    }, error = function(e) {
      sample(available_indices, 1)
    })
    
    # Add lineup immediately
    selected_lineups <- rbind(selected_lineups, filtered_lineups[selected_idx])
    selected_indices <- c(selected_indices, selected_idx)
    
    # Update player counts for exposure calculation
    candidates_players <- unlist(filtered_lineups[selected_idx, ..player_cols])
    player_counts[candidates_players] <- player_counts[candidates_players] + 1
  }
  
  if (nrow(selected_lineups) == 0) return(NULL)
  
  # Calculate exposure for attribute
  final_exposure <- (player_counts / nrow(selected_lineups)) * 100
  attr(selected_lineups, "exposure") <- final_exposure
  
  # Keep only needed columns
  keep_cols <- c(player_cols, "Top1Count", "Top2Count", "Top3Count", "Top5Count", "TotalSalary")
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
  best_of_value <- ifelse(current_match$Tour == "ATP", 5, 3)
  
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
        best_of_value <- ifelse(tour == "ATP", 5, 3)
        
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

find_all_optimal_lineups <- function(simulation_results, player_data) {
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
      win_counts <- iter_results[Result == "Winner", .N, by = Player]
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
          
          # Update win count tracking
          if (total_wins == 6) current_counts$Win6Count <- current_counts$Win6Count + 1
          if (total_wins >= 5) current_counts$Win5PlusCount <- current_counts$Win5PlusCount + 1
          if (total_wins >= 4) current_counts$Win4PlusCount <- current_counts$Win4PlusCount + 1
          
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
            Win6Count = ifelse(total_wins == 6, 1, 0),
            Win5PlusCount = ifelse(total_wins >= 5, 1, 0),
            Win4PlusCount = ifelse(total_wins >= 4, 1, 0)
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
  
  # Create final data.table with correct size preallocated
  lineup_counter <- data.table(
    LineupID = lineup_ids,
    Count = integer(length(lineup_ids)),
    Top1Count = integer(length(lineup_ids)),
    Top2Count = integer(length(lineup_ids)),
    Top3Count = integer(length(lineup_ids)),
    Top5Count = integer(length(lineup_ids)),
    Win6Count = integer(length(lineup_ids)),
    Win5PlusCount = integer(length(lineup_ids)),
    Win4PlusCount = integer(length(lineup_ids))
  )
  
  # Fill the data.table efficiently using set()
  for (i in seq_along(lineup_ids)) {
    lineup_id <- lineup_ids[i]
    counts <- get(lineup_id, envir = lineup_dict)
    
    set(lineup_counter, i, 2L, counts$Count)
    set(lineup_counter, i, 3L, counts$Top1Count)
    set(lineup_counter, i, 4L, counts$Top2Count)
    set(lineup_counter, i, 5L, counts$Top3Count)
    set(lineup_counter, i, 6L, counts$Top5Count)
    set(lineup_counter, i, 7L, counts$Win6Count)
    set(lineup_counter, i, 8L, counts$Win5PlusCount)
    set(lineup_counter, i, 9L, counts$Win4PlusCount)
    
    # Show progress for conversion too
    if (i %% 1000 == 0) {
      cat(sprintf("Converted %s/%s lineups (%.1f%%)\n", 
                  format(i, big.mark = ","), 
                  format(total_unique_lineups, big.mark = ","),
                  (i / total_unique_lineups) * 100))
    }
  }
  
  # Calculate frequencies and percentages
  lineup_counter[, Frequency := Count / total_iterations * 100]
  lineup_counter[, Win6Pct := (Win6Count / Count) * 100]
  lineup_counter[, Win5PlusPct := (Win5PlusCount / Count) * 100]
  lineup_counter[, Win4PlusPct := (Win4PlusCount / Count) * 100]
  
  # Sort by Top1Count
  setorder(lineup_counter, -Top1Count)
  
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

expand_lineup_details <- function(lineup_stats, player_data) {
  start_time <- Sys.time()
  cat("\n=== EXPANDING LINEUP DETAILS ===\n")
  cat("Timestamp:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Make sure we're working with data.tables
  lineup_dt <- as.data.table(lineup_stats)
  player_dt <- as.data.table(player_data)
  
  total_lineups <- nrow(lineup_dt)
  cat(sprintf("Expanding details for %s lineups\n", format(total_lineups, big.mark = ",")))
  
  # Make sure we have the right name column in player data
  name_col <- "Name"
  if (!"Name" %in% names(player_dt)) {
    if ("Player" %in% names(player_dt)) {
      name_col <- "Player"
    } else {
      stop("Player data must contain either 'Name' or 'Player' column")
    }
  }
  
  # Create a fast player name to salary lookup
  player_salary_map <- setNames(player_dt$Salary, player_dt[[name_col]])
  
  # Pre-allocate the results data.table for better performance
  expanded_lineups <- data.table(
    LineupID = lineup_dt$LineupID,
    Count = lineup_dt$Count,
    Top1Count = lineup_dt$Top1Count,
    Top2Count = lineup_dt$Top2Count,
    Top3Count = lineup_dt$Top3Count,
    Top5Count = lineup_dt$Top5Count,
    Win6Count = lineup_dt$Win6Count,
    Win5PlusCount = lineup_dt$Win5PlusCount,
    Win4PlusCount = lineup_dt$Win4PlusCount,
    Win6Pct = lineup_dt$Win6Pct,
    Win5PlusPct = lineup_dt$Win5PlusPct,
    Win4PlusPct = lineup_dt$Win4PlusPct,
    TotalSalary = numeric(nrow(lineup_dt))
  )
  
  # Add player name and salary columns
  for (j in 1:6) {
    expanded_lineups[[paste0("Name", j)]] <- character(nrow(lineup_dt))
    expanded_lineups[[paste0("Salary", j)]] <- numeric(nrow(lineup_dt))
  }
  
  # Use smaller batch sizes for more frequent progress updates
  batch_size <- min(250, nrow(lineup_dt))
  n_batches <- ceiling(nrow(lineup_dt) / batch_size)
  
  cat(sprintf("Using batch size of %d lineups (%d batches total)\n\n", batch_size, n_batches))
  
  # Process each lineup with batch reporting
  for (batch in 1:n_batches) {
    batch_start_time <- Sys.time()
    
    # Calculate batch range
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, nrow(lineup_dt))
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
      player_names <- unlist(strsplit(lineup_dt$LineupID[i], "\\|"))
      
      # Get salaries directly from the map
      player_salaries <- player_salary_map[player_names]
      player_salaries[is.na(player_salaries)] <- 0
      
      # Calculate total salary
      total_salary <- sum(player_salaries, na.rm = TRUE)
      set(expanded_lineups, i, "TotalSalary", total_salary)
      
      # Sort players by salary (descending)
      sorted_indices <- order(player_salaries, decreasing = TRUE)
      sorted_players <- player_names[sorted_indices]
      sorted_salaries <- player_salaries[sorted_indices]
      
      # Add player details to the lineup using set()
      for (j in 1:min(length(sorted_players), 6)) {
        name_col <- paste0("Name", j)
        salary_col <- paste0("Salary", j)
        
        set(expanded_lineups, i, name_col, sorted_players[j])
        set(expanded_lineups, i, salary_col, sorted_salaries[j])
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
                  
                  # Always show a clear, prominent button regardless of simulation status
                  fluidRow(
                    column(6,
                           actionButton("run_dk_optimization", "Calculate Optimal Lineups",
                                        class = "btn-primary", 
                                        style = "width: 100%; margin-top: 10px; padding: 15px; font-size: 16px;")
                    )
                  ),
                  
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
                      title = "Lineup Count Thresholds",
                      DTOutput("lineup_count_thresholds") %>% withSpinner(color = "#FFD700")
                  )
                ),
                fluidRow(
                  box(width = 12,
                      title = "Lineup Filters",
                      fluidRow(
                        column(3,
                               numericInput("min_top1_count", "Min Top 1 Count:", 
                                            value = 0, min = 0)
                        ),
                        column(3,
                               numericInput("min_top2_count", "Min Top 2 Count:", 
                                            value = 0, min = 0)
                        ),
                        column(3,
                               numericInput("min_top3_count", "Min Top 3 Count:", 
                                            value = 0, min = 0)
                        ),
                        column(3,
                               numericInput("min_top5_count", "Min Top 5 Count:", 
                                            value = 0, min = 0)
                        )
                      ),
                        fluidRow(
                          column(4,
                                 numericInput("min_total_ew", "Min Total EW:", 
                                              value = 0, min = 0, step = 0.1)
                          ),
                          column(4,
                                 numericInput("min_win6_pct", "Min 6-Win %:", 
                                              value = 0, min = 0, max = 100, step = 0.1)
                          ),
                          column(4,
                                 numericInput("min_win5plus_pct", "Min 5+ Win %:", 
                                              value = 0, min = 0, max = 100, step = 0.1)
                          ),
                          column(4,
                                 numericInput("min_win4plus_pct", "Min 4+ Win %:", 
                                              value = 0, min = 0, max = 100, step = 0.1)
                          )
                        ),
                      fluidRow(
                        column(6,
                               selectizeInput("excluded_players", "Exclude Players:",
                                              choices = NULL,
                                              multiple = TRUE,
                                              options = list(plugins = list('remove_button')))
                        ),
                        column(6,
                               numericInput("num_lineups", "Number of Lineups to Generate:", 
                                            value = 20, min = 1, max = 150)
                        )
                      ),
                      fluidRow(
                        column(6,
                               div(class = "well well-sm",
                                   h4("Filtered Pool Statistics:"),
                                   textOutput("filtered_pool_size")
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
  
  # Run lineup optimization
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
      # Show a specific modal
      showModal(modalDialog(
        title = "Processing Optimal Lineups",
        "Finding optimal lineups using all simulations. This may take a few minutes.",
        footer = NULL,
        easyClose = FALSE
      ))
      
      lp_cache <- new.env(hash = TRUE, parent = emptyenv())
      
      # Step 1: Calculate EW metrics
      incProgress(0.1, detail = "Calculating EW metrics...")
      ew_metrics <- calculate_ew_metrics(rv$simulation_results, input$n_sims)
      
      # Step 2: Find optimal lineups
      incProgress(0.3, detail = "Finding optimal lineups...")
      optimal_lineups <- find_all_optimal_lineups(
        rv$simulation_results, 
        rv$dk_data
      )
      
      # Step 3: Expand lineup details with EW metrics
      incProgress(0.6, detail = "Processing lineup details...")
      expanded_lineups <- expand_lineup_details(
        optimal_lineups,
        rv$dk_data,
        ew_metrics  # ADD THIS PARAMETER
      )
      
      # Store results
      rv$dk_optimal_lineups <- expanded_lineups
      rv$optimization_complete <- TRUE
      
      # Calculate initial player exposure
      incProgress(0.8, detail = "Calculating player exposure...")
      if(!is.null(rv$dk_optimal_lineups)) {
        rv$dk_player_exposure <- calculate_player_exposure(
          rv$dk_optimal_lineups, 
          rv$player_projections
        )
      }
      
      # Update excluded players selectInput
      if (!is.null(expanded_lineups)) {
        # Get unique players from all lineups
        player_cols <- grep("^Name[1-6]$", names(expanded_lineups), value = TRUE)
        all_players <- c()
        for(col in player_cols) {
          all_players <- c(all_players, expanded_lineups[[col]])
        }
        all_players <- unique(all_players)
        
        # Create labels with ownership percentages
        player_labels <- sapply(all_players, function(player) {
          gto <- rv$dk_player_exposure$OptimalRate[rv$dk_player_exposure$Player == player]
          sprintf("%s (%.1f%%)", player, gto)
        })
        
        # Update the selectInput
        updateSelectizeInput(
          session, 
          "excluded_players",
          choices = setNames(all_players, player_labels),
          selected = NULL
        )
      }
      
      # Remove the processing modal
      removeModal()
      
      # Show success message
      if(!is.null(rv$dk_optimal_lineups)) {
        showModal(modalDialog(
          title = "Success",
          HTML(sprintf(
            "Successfully generated <b>%d</b> optimal lineups!<br><br>
            You can now go to the <b>Lineup Builder</b> tab to filter and select lineups from this pool.",
            nrow(rv$dk_optimal_lineups)
          )),
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
  
  # Correct the optimal_lineups_table output
  # Updated optimal_lineups_table output with improved sorting
  output$optimal_lineups_table <- renderDT({
    # Force validation and show diagnostic info
    cat("Optimization complete:", rv$optimization_complete, "\n")
    
    # Validate required data
    if (is.null(rv$dk_optimal_lineups) || nrow(rv$dk_optimal_lineups) == 0) {
      return(datatable(
        data.frame(Message = "No optimal lineups available. Click 'Calculate Optimal Lineups' button to generate lineups."),
        options = list(dom = "t", ordering = FALSE),
        rownames = FALSE
      ))
    }
    
    # Get lineup data
    lineups_df <- as.data.frame(rv$dk_optimal_lineups)
    
    # Determine the player column naming pattern used
    if(any(grepl("^Name[1-6]$", names(lineups_df)))) {
      player_cols <- grep("^Name[1-6]$", names(lineups_df), value = TRUE)
    } else if(any(grepl("^Player[1-6]$", names(lineups_df)))) {
      player_cols <- grep("^Player[1-6]$", names(lineups_df), value = TRUE)
    } else {
      player_cols <- character(0)
    }
    
    # Select columns for display
    display_cols <- c(
      player_cols,
      "TotalSalary", "Win6Pct", "Win5PlusPct", "Win4PlusPct",  # NEW WIN COLUMNS
      "Top1Count", "Top2Count", "Top3Count", "Top5Count"
    )
    
    # Intersect with available columns
    display_cols <- intersect(display_cols, names(lineups_df))
    
    # Use the selected columns from the data frame
    display_data <- lineups_df[, display_cols, drop = FALSE]
    
    # Sort the data by Top1Count (desc) then by Top5Count (desc)
    if("Top1Count" %in% names(display_data) && "Top5Count" %in% names(display_data)) {
      display_data <- display_data[order(-display_data$Top1Count, -display_data$Top5Count), ]
    } else if("Top1Count" %in% names(display_data)) {
      display_data <- display_data[order(-display_data$Top1Count), ]
    }
    
    # Find the column indices for Top1Count and Top5Count for the datatable ordering
    top1_col_idx <- which(names(display_data) == "Top1Count") - 1  # DT uses 0-based indexing
    top5_col_idx <- which(names(display_data) == "Top5Count") - 1  # DT uses 0-based indexing
    
    # Create the ordering specification
    # If both columns exist, sort by Top1Count desc, then Top5Count desc
    if(length(top1_col_idx) > 0 && length(top5_col_idx) > 0) {
      order_spec <- list(
        list(top1_col_idx, 'desc'),
        list(top5_col_idx, 'desc')
      )
    } else if(length(top1_col_idx) > 0) {
      order_spec <- list(list(top1_col_idx, 'desc'))
    } else {
      order_spec <- list(list(0, 'desc'))  # Default to first column
    }
    
    # Create datatable
    dt <- datatable(
      display_data,
      options = list(
        scrollX = TRUE,
        pageLength = 50,
        order = order_spec,  # Use the multi-column sorting
        columnDefs = list(
          # Make the count columns more prominent with center alignment
          list(className = 'dt-center', targets = c(top1_col_idx, top5_col_idx)),
          # Optional: make the top count columns have a different background
          list(className = 'dt-body-nowrap', targets = "_all")
        )
      ),
      rownames = FALSE
    ) %>%
      # Add conditional formatting to highlight high-performing lineups
      formatStyle(
        "Top1Count",
        backgroundColor = styleInterval(
          c(1, 5, 10, 20), 
          c("white", "#fff3cd", "#ffeaa7", "#fdcb6e", "#e17055")
        ),
        fontWeight = styleInterval(10, c("normal", "bold"))
      ) %>%
      formatStyle(
        "Top5Count", 
        backgroundColor = styleInterval(
          c(5, 15, 30, 50), 
          c("white", "#f8f9fa", "#e9ecef", "#dee2e6", "#ced4da")
        )
      )
    
    # Format salary
    if("TotalSalary" %in% display_cols) {
      dt <- dt %>% formatCurrency("TotalSalary", "$", digits = 0)
    }
    
    if("TotalEW" %in% display_cols) {
      dt <- dt %>% formatRound("TotalEW", 2)
    }
    if("Win6Pct" %in% display_cols) {
      dt <- dt %>% formatRound("Win6Pct", 1)
    }
    if("Win5PlusPct" %in% display_cols) {
      dt <- dt %>% formatRound("Win5PlusPct", 1)
    }
    if("Win4PlusPct" %in% display_cols) {
      dt <- dt %>% formatRound("Win4PlusPct", 1)
    }
    
    dt
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
  
  # Filtered pool size
  output$filtered_pool_size <- renderText({
    req(rv$dk_optimal_lineups)
    
    # Create filters
    filters <- list(
      min_top1_count = input$min_top1_count,
      min_top2_count = input$min_top2_count,
      min_top3_count = input$min_top3_count,
      min_top5_count = input$min_top5_count,
      min_total_ew = input$min_total_ew,        # ADD THIS
      min_avg_6plus = input$min_avg_6plus,      # ADD THIS
      min_avg_5plus = input$min_avg_5plus, 
      excluded_players = input$excluded_players
    )
    
    # Calculate stats
    stats <- calculate_filtered_pool_stats(rv$dk_optimal_lineups, filters)
    
    paste("Number of lineups in filtered pool:", stats$count)
  })
  
  # Generate lineups
  observeEvent(input$generate_lineups, {
    req(rv$dk_optimal_lineups)
    
    # Create filters for lineup generation
    filters <- list(
      min_top1_count = input$min_top1_count,
      min_top2_count = input$min_top2_count,
      min_top3_count = input$min_top3_count,
      min_top5_count = input$min_top5_count,
      excluded_players = input$excluded_players,
      num_lineups = input$num_lineups
    )
    
    # Show progress
    withProgress(message = 'Generating lineups...', value = 0, {
      # Generate random lineups
      rv$dk_random_lineups <- generate_random_lineups(rv$dk_optimal_lineups, filters)
      
      # Update player exposure data
      if(!is.null(rv$dk_random_lineups)) {
        rv$dk_player_exposure <- calculate_player_exposure(
          rv$dk_optimal_lineups, 
          rv$player_projections, 
          rv$dk_random_lineups
        )
      }
      
      # Show message based on result
      if(is.null(rv$dk_random_lineups)) {
        showModal(modalDialog(
          title = "Error",
          "No lineups match the selected filters. Try adjusting your criteria.",
          easyClose = TRUE
        ))
      } else {
        showModal(modalDialog(
          title = "Success",
          sprintf("Generated %d lineups successfully!", nrow(rv$dk_random_lineups)),
          easyClose = TRUE
        ))
      }
    })
  })
  
  # Player exposure table
  output$player_exposure_table <- renderDT({
    req(rv$dk_player_exposure)
    
    # Reorganize data for display
    display_data <- rv$dk_player_exposure
    
    datatable(
      display_data,
      options = list(
        pageLength = -1,
        dom = "t",
        scrollX = TRUE,
        order = list(list(3, 'desc')),  # Sort by OptimalRate by default
        rownames = FALSE  # Remove row numbers
      ),
      rownames = FALSE
    ) %>%
      formatCurrency('Salary', currency = "$", digits = 0) %>%
      formatRound(c('OptimalRate', 'EliteRate', 'FloorRate', 'AppearanceRate', 'Exposure', 'Leverage'), digits = 1)
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
  
  # Fixed download handler for optimal lineups
  output$download_optimal_lineups <- downloadHandler(
    filename = function() {
      paste("tennis_optimal_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      if(is.null(rv$dk_optimal_lineups) || nrow(rv$dk_optimal_lineups) == 0) {
        # Create an empty dataframe with appropriate columns
        empty_data <- data.frame(matrix(ncol = DK_ROSTER_SIZE + 4, nrow = 0))
        colnames(empty_data) <- c(paste0("Player", 1:DK_ROSTER_SIZE), "Top1Count", "Top2Count", "Top3Count", "Top5Count", "TotalSalary")
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      # Get optimal lineup data
      lineups_df <- as.data.frame(rv$dk_optimal_lineups)
      
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
      lineups_for_export <- lineups_df %>% 
        filter(Top1Count > 0)
      
      # Select columns for download
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