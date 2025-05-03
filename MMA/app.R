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
library(webshot)
library(parallel)
library(foreach)
library(doParallel)
library(shinyjs)
library(shinycssloaders)

# Global constants
DK_ROSTER_SIZE <- 6
FD_ROSTER_SIZE <- 6  
DK_SALARY_CAP <- 50000
FD_SALARY_CAP <- 100  

# Set up custom CSS for black and red theme
custom_css <- "
  /* Override dashboard header colors */
  .skin-blue .main-header {
    background-color: #000000;
  }
  .skin-blue .main-header .logo {
    background-color: #000000;
    color: #ff0000;
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
    color: #ff0000;
  }
  .skin-blue .sidebar-menu > li.active > a, 
  .skin-blue .sidebar-menu > li:hover > a {
    color: #ffffff;
    background: #333333;
    border-left-color: #ff0000;
  }
  
  /* Customize box headers */
  .box.box-primary .box-header {
    background-color: #333333;
    color: #ff0000;
  }
  
  /* Style buttons */
  .btn-primary {
    background-color: #ff0000;
    border-color: #cc0000;
    color: #ffffff;
  }
  .btn-primary:hover, .btn-primary:focus {
    background-color: #cc0000;
    border-color: #990000;
  }
  
  /* Style tabs */
  .nav-tabs-custom > .nav-tabs > li.active {
    border-top-color: #ff0000;
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
    has_dk <- "DKSalary" %in% colnames(sheets$Fights) && "Winner_DK_P50" %in% colnames(sheets$Scores)
    has_fd <- "FDSalary" %in% colnames(sheets$Fights) && "Winner_FD_P50" %in% colnames(sheets$Scores)
    
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
    "DKSalary", "FDSalary", "DKOwn", "FDOwn", "OriginalML", 
    "DeViggedProb", "R1", "QuickWin_R1", "R2", "R3", "R4", "R5", "Decision"
  )
  
  for(col in numeric_cols_fights) {
    if(col %in% names(processed_fights)) {
      processed_fights[, (col) := as.numeric(get(col))]
    }
  }
  
  # Convert relevant numeric columns in scores data
  numeric_cols_scores <- c(
    "Winner_Prob", "Winner_DK_P5","Winner_FD_P5","Loser_FD_P5","Loser_FD_P5",
    "Winner_DK_P10", "Winner_DK_P25", "Winner_DK_P50", "Winner_DK_P75", "Winner_DK_P90","Winner_DK_P95",
    "Winner_FD_P10", "Winner_FD_P25", "Winner_FD_P50", "Winner_FD_P75", "Winner_FD_P90","Winner_FD_P95",
    "Loser_DK_P10", "Loser_DK_P25", "Loser_DK_P50", "Loser_DK_P75", "Loser_DK_P90","Loser_DK_P95",
    "Loser_FD_P10", "Loser_FD_P25", "Loser_FD_P50", "Loser_FD_P75", "Loser_FD_P90", "Loser_FD_P95"
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
    
    # Generate fantasy scores based on percentiles
    # For winner DK score - sample between percentiles
    dk_winner_percentile <- runif(1, 0, 1)
    if(dk_winner_percentile <= 0.05) {
      # Use P5 as minimum
      dk_winner_score <- selected_outcome$Winner_DK_P5
    } else if(dk_winner_percentile <= 0.1) {
      # Interpolate between P5 and P10
      p5 <- selected_outcome$Winner_DK_P5
      p10 <- selected_outcome$Winner_DK_P10
      dk_winner_score <- p5 + (p10 - p5) * (dk_winner_percentile - 0.05) / 0.05
    } else if(dk_winner_percentile <= 0.25) {
      p10 <- selected_outcome$Winner_DK_P10
      p25 <- selected_outcome$Winner_DK_P25
      dk_winner_score <- p10 + (p25 - p10) * (dk_winner_percentile - 0.1) / 0.15
    } else if(dk_winner_percentile <= 0.5) {
      p25 <- selected_outcome$Winner_DK_P25
      p50 <- selected_outcome$Winner_DK_P50
      dk_winner_score <- p25 + (p50 - p25) * (dk_winner_percentile - 0.25) / 0.25
    } else if(dk_winner_percentile <= 0.75) {
      p50 <- selected_outcome$Winner_DK_P50
      p75 <- selected_outcome$Winner_DK_P75
      dk_winner_score <- p50 + (p75 - p50) * (dk_winner_percentile - 0.5) / 0.25
    } else if(dk_winner_percentile <= 0.9) {
      p75 <- selected_outcome$Winner_DK_P75
      p90 <- selected_outcome$Winner_DK_P90
      dk_winner_score <- p75 + (p90 - p75) * (dk_winner_percentile - 0.75) / 0.15
    } else if(dk_winner_percentile <= 0.95) {
      # Interpolate between P90 and P95
      p90 <- selected_outcome$Winner_DK_P90
      p95 <- selected_outcome$Winner_DK_P95
      dk_winner_score <- p90 + (p95 - p90) * (dk_winner_percentile - 0.9) / 0.05
    } else {
      # Use P95 as maximum
      dk_winner_score <- selected_outcome$Winner_DK_P95
    }
    
    # For winner FD score - sample between percentiles
    fd_winner_percentile <- runif(1, 0, 1)
    if(fd_winner_percentile <= 0.05) {
      # Use P5 as minimum
      fd_winner_score <- selected_outcome$Winner_FD_P5
    } else if(fd_winner_percentile <= 0.1) {
      # Interpolate between P5 and P10
      p5 <- selected_outcome$Winner_FD_P5
      p10 <- selected_outcome$Winner_FD_P10
      fd_winner_score <- p5 + (p10 - p5) * (fd_winner_percentile - 0.05) / 0.05
    } else if(fd_winner_percentile <= 0.25) {
      p10 <- selected_outcome$Winner_FD_P10
      p25 <- selected_outcome$Winner_FD_P25
      fd_winner_score <- p10 + (p25 - p10) * (fd_winner_percentile - 0.1) / 0.15
    } else if(fd_winner_percentile <= 0.5) {
      p25 <- selected_outcome$Winner_FD_P25
      p50 <- selected_outcome$Winner_FD_P50
      fd_winner_score <- p25 + (p50 - p25) * (fd_winner_percentile - 0.25) / 0.25
    } else if(fd_winner_percentile <= 0.75) {
      p50 <- selected_outcome$Winner_FD_P50
      p75 <- selected_outcome$Winner_FD_P75
      fd_winner_score <- p50 + (p75 - p50) * (fd_winner_percentile - 0.5) / 0.25
    } else if(fd_winner_percentile <= 0.9) {
      p75 <- selected_outcome$Winner_FD_P75
      p90 <- selected_outcome$Winner_FD_P90
      fd_winner_score <- p75 + (p90 - p75) * (fd_winner_percentile - 0.75) / 0.15
    } else if(fd_winner_percentile <= 0.95) {
      # Interpolate between P90 and P95
      p90 <- selected_outcome$Winner_FD_P90
      p95 <- selected_outcome$Winner_FD_P95
      fd_winner_score <- p90 + (p95 - p90) * (fd_winner_percentile - 0.9) / 0.05
    } else {
      # Use P95 as maximum
      fd_winner_score <- selected_outcome$Winner_FD_P95
    }
    
    
    # For loser DK score - sample between percentiles
    dk_loser_percentile <- runif(1, 0, 1)
    if(dk_loser_percentile <= 0.05) {
      # Use P5 as minimum
      dk_loser_score <- selected_outcome$Loser_DK_P5
    } else if(dk_loser_percentile <= 0.1) {
      # Interpolate between P5 and P10
      p5 <- selected_outcome$Loser_DK_P5
      p10 <- selected_outcome$Loser_DK_P10
      dk_loser_score <- p5 + (p10 - p5) * (dk_loser_percentile - 0.05) / 0.05
    } else if(dk_loser_percentile <= 0.25) {
      p10 <- selected_outcome$Loser_DK_P10
      p25 <- selected_outcome$Loser_DK_P25
      dk_loser_score <- p10 + (p25 - p10) * (dk_loser_percentile - 0.1) / 0.15
    } else if(dk_loser_percentile <= 0.5) {
      p25 <- selected_outcome$Loser_DK_P25
      p50 <- selected_outcome$Loser_DK_P50
      dk_loser_score <- p25 + (p50 - p25) * (dk_loser_percentile - 0.25) / 0.25
    } else if(dk_loser_percentile <= 0.75) {
      p50 <- selected_outcome$Loser_DK_P50
      p75 <- selected_outcome$Loser_DK_P75
      dk_loser_score <- p50 + (p75 - p50) * (dk_loser_percentile - 0.5) / 0.25
    } else if(dk_loser_percentile <= 0.9) {
      p75 <- selected_outcome$Loser_DK_P75
      p90 <- selected_outcome$Loser_DK_P90
      dk_loser_score <- p75 + (p90 - p75) * (dk_loser_percentile - 0.75) / 0.15
    } else if(dk_loser_percentile <= 0.95) {
      # Interpolate between P90 and P95
      p90 <- selected_outcome$Loser_DK_P90
      p95 <- selected_outcome$Loser_DK_P95
      dk_loser_score <- p90 + (p95 - p90) * (dk_loser_percentile - 0.9) / 0.05
    } else {
      # Use P95 as maximum
      dk_loser_score <- selected_outcome$Loser_DK_P95
    }
    
    # For loser FD score - sample between percentiles
    fd_loser_percentile <- runif(1, 0, 1)
    if(fd_loser_percentile <= 0.05) {
      # Use P5 as minimum
      fd_loser_score <- selected_outcome$Loser_FD_P5
    } else if(fd_loser_percentile <= 0.1) {
      # Interpolate between P5 and P10
      p5 <- selected_outcome$Loser_FD_P5
      p10 <- selected_outcome$Loser_FD_P10
      fd_loser_score <- p5 + (p10 - p5) * (fd_loser_percentile - 0.05) / 0.05
    } else if(fd_loser_percentile <= 0.25) {
      p10 <- selected_outcome$Loser_FD_P10
      p25 <- selected_outcome$Loser_FD_P25
      fd_loser_score <- p10 + (p25 - p10) * (fd_loser_percentile - 0.1) / 0.15
    } else if(fd_loser_percentile <= 0.5) {
      p25 <- selected_outcome$Loser_FD_P25
      p50 <- selected_outcome$Loser_FD_P50
      fd_loser_score <- p25 + (p50 - p25) * (fd_loser_percentile - 0.25) / 0.25
    } else if(fd_loser_percentile <= 0.75) {
      p50 <- selected_outcome$Loser_FD_P50
      p75 <- selected_outcome$Loser_FD_P75
      fd_loser_score <- p50 + (p75 - p50) * (fd_loser_percentile - 0.5) / 0.25
    } else if(fd_loser_percentile <= 0.9) {
      p75 <- selected_outcome$Loser_FD_P75
      p90 <- selected_outcome$Loser_FD_P90
      fd_loser_score <- p75 + (p90 - p75) * (fd_loser_percentile - 0.75) / 0.15
    }  else if(fd_loser_percentile <= 0.95) {
      # Interpolate between P90 and P95
      p90 <- selected_outcome$Loser_FD_P90
      p95 <- selected_outcome$Loser_FD_P95
      fd_loser_score <- p90 + (p95 - p90) * (fd_loser_percentile - 0.9) / 0.05
    } else {
      # Use P95 as maximum
      fd_loser_score <- selected_outcome$Loser_FD_P95
    }
    
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
  
  # Combine all results
  combined_results <- rbindlist(all_results)
  
  # Add fighter information to results
  combined_results <- merge(
    combined_results,
    fights_data[, .(Name, DKID, FDID, DKSalary, FDSalary, DKOwn, FDOwn)],
    by = "Name",
    all.x = TRUE
  )
  
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


# Optimized function without excluded fighters functionality
count_dk_optimal_lineups <- function(sim_results) {
  # Always use top_k=5
  top_k <- 5
  
  # Get roster size and salary cap from the global environment or hardcode
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
  
  # Create salary lookup table
  first_sim <- sim_results_dt[SimID == all_sim_ids[1]]
  salary_lookup <- unique(first_sim[, .(Name, DKSalary)])
  setkey(salary_lookup, Name)
  
  # Calculate total salary efficiently
  lineup_data$TotalSalary <- sapply(lineup_data$Lineup, function(lineup_str) {
    fighters <- strsplit(lineup_str, "\\|")[[1]]
    salaries <- salary_lookup[fighters, on = "Name", nomatch = 0]$DKSalary
    sum(salaries, na.rm = TRUE)
  })
  
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
  
  # Clean up to free memory
  rm(lineup_data, fighter_cols, combined_lineups, lineup_table, salary_lookup)
  gc(verbose = FALSE, full = TRUE)
  
  return(result)
}

find_fd_optimal_lineups <- function(sim_data, k = 5) {
  # Ensure data.table
  setDT(sim_data)
  
  # Use FDScore instead of FDFantasyPoints
  fd_fantasy_points <- sim_data$FDScore  
  fd_salary <- sim_data$FDSalary
  ppd_values <- fd_fantasy_points / (fd_salary/1000)
  
  # Find indices of top candidates
  top_points_idx <- order(-fd_fantasy_points)[1:min(15, length(fd_fantasy_points))]
  top_ppd_idx <- order(-ppd_values)[1:min(15, length(ppd_values))]
  
  # Combine indices without redundant copies
  candidate_idx <- unique(c(top_points_idx, top_ppd_idx))
  
  # Use FDScore instead of FDFantasyPoints
  candidates <- sim_data[candidate_idx, .(Name, FDSalary, FDScore)]
  
  n <- nrow(candidates)
  if(n < FD_ROSTER_SIZE) return(NULL)
  
  # Initialize results with exact size to prevent reallocations
  lineup_results <- data.frame(
    Lineup = character(k),
    Rank = integer(k),
    stringsAsFactors = FALSE
  )
  
  # Track excluded pairs for diversity
  excluded_pairs <- list()
  
  # Find k lineups
  lineup_count <- 0
  for(i in 1:k) {
    # Create constraint matrix
    const_rows <- 2 + length(excluded_pairs)
    const.mat <- matrix(0, nrow = const_rows, ncol = n)
    
    # Basic constraints
    const.mat[1, ] <- candidates$FDSalary  # Salary cap
    const.mat[2, ] <- 1                    # Roster size
    
    # Add previous lineup exclusions
    if(length(excluded_pairs) > 0) {
      for(j in 1:length(excluded_pairs)) {
        const.mat[2+j, excluded_pairs[[j]]] <- 1
      }
    }
    
    const.dir <- c("<=", "==", rep("<=", length(excluded_pairs)))
    const.rhs <- c(FD_SALARY_CAP, FD_ROSTER_SIZE, rep(FD_ROSTER_SIZE-1, length(excluded_pairs)))
    
    # Use FDScore instead of FDFantasyPoints
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
    
    # Create lineup string using fighter Names
    selected_fighters <- sort(candidates$Name[selected_indices])
    lineup_str <- paste(selected_fighters, collapse = "|")
    
    # Add to results
    lineup_count <- lineup_count + 1
    lineup_results$Lineup[lineup_count] <- lineup_str
    lineup_results$Rank[lineup_count] <- i
    
    # Track for diversity
    excluded_pairs[[length(excluded_pairs) + 1]] <- selected_indices
    
    # Free memory
    rm(result)
    gc(verbose = FALSE, full = TRUE)
  }
  
  # Return only valid results
  if(lineup_count == 0) return(NULL)
  if(lineup_count < k) {
    lineup_results <- lineup_results[1:lineup_count, , drop = FALSE]
  }
  
  return(lineup_results)
}

count_fd_optimal_lineups <- function(sim_results) {
  # Always use top_k=5
  top_k <- 5
  
  # Create data.table for better performance
  sim_results_dt <- as.data.table(sim_results)
  
  # Use FDScore instead of FDFantasyPoints
  sim_results_dt <- sim_results_dt[, .(
    SimID, Name, FDSalary, FDScore
  )]
  
  # Split by simulation ID
  all_sim_ids <- unique(sim_results_dt$SimID)
  n_sims <- length(all_sim_ids)
  
  # Initialize results storage
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
        all_lineups[[sim_idx]] <- find_fd_optimal_lineups(chunk_sim_list[[i]], k = top_k)
      }
    }
    
    # Clean up chunk variables to free memory
    rm(chunk_data, chunk_sim_list)
    
    # Garbage collection every chunk
    gc(verbose = FALSE, full = TRUE)
    
    # Progress reporting
    cat(sprintf("Processed %d/%d simulations (%.1f%%)\n", 
                min(end_idx, n_sims), n_sims, 
                min(end_idx, n_sims) / n_sims * 100))
  }
  
  # Combine and process results more efficiently
  valid_indices <- which(!sapply(all_lineups, is.null))
  valid_lineups <- all_lineups[valid_indices]
  
  # Free memory
  rm(all_lineups)
  gc(verbose = FALSE, full = TRUE)
  
  # Return NULL if no valid lineups
  if(length(valid_lineups) == 0) return(NULL)
  
  combined_lineups <- do.call(rbind, valid_lineups)
  
  # Free more memory
  rm(valid_lineups)
  gc(verbose = FALSE, full = TRUE)
  
  # Return NULL if no valid lineups
  if(is.null(combined_lineups) || nrow(combined_lineups) == 0) return(NULL)
  
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
  
  # Pre-compute a salary lookup based on Name
  unique_fighters <- unique(unlist(strsplit(lineup_data$Lineup, "\\|")))
  salary_lookup <- sim_results_dt[, .SD[1], by = Name, .SDcols = "FDSalary"]
  setkey(salary_lookup, Name)
  
  # Calculate total salary more efficiently
  lineup_data$TotalSalary <- sapply(lineup_data$Lineup, function(lineup_str) {
    fighters <- strsplit(lineup_str, "\\|")[[1]]
    salaries <- salary_lookup[fighters, on = "Name", nomatch = 0]$FDSalary
    sum(salaries, na.rm = TRUE)
  })
  
  # Sort by Top1Count
  lineup_data <- lineup_data[order(-lineup_data$Top1Count), ]
  
  # Split fighter columns for display
  driver_cols <- do.call(rbind, strsplit(lineup_data$Lineup, "\\|"))
  
  # Validate column count
  if(ncol(driver_cols) != FD_ROSTER_SIZE) {
    warning(paste("Expected", FD_ROSTER_SIZE, "fighter columns, got", ncol(driver_cols)))
    return(NULL)
  }
  
  colnames(driver_cols) <- paste0("Fighter", 1:FD_ROSTER_SIZE)
  
  # Create final result - directly combining without copying
  result <- cbind(
    as.data.frame(driver_cols),
    lineup_data[, grep("Count$|Salary$", names(lineup_data), value = TRUE), drop = FALSE]
  )
  
  # Clean up to free memory
  rm(lineup_data, driver_cols, combined_lineups, lineup_table, salary_lookup)
  gc(verbose = FALSE, full = TRUE)
  
  return(result)
}

# Calculate filtered pool stats for DraftKings
calculate_dk_filtered_pool_stats <- function(optimal_lineups, filters) {
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  setDT(optimal_lineups)
  filtered_lineups <- copy(optimal_lineups)
  
  # Apply Top1Count filter
  if (!is.null(filters$min_top1_count) && "Top1Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
  }
  
  # Apply Top2Count filter
  if (!is.null(filters$min_top2_count) && "Top2Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top2Count >= filters$min_top2_count]
  }
  
  # Apply Top3Count filter
  if (!is.null(filters$min_top3_count) && "Top3Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top3Count >= filters$min_top3_count]
  }
  
  # Apply Top5Count filter
  if (!is.null(filters$min_top5_count) && "Top5Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top5Count >= filters$min_top5_count]
  }
  
  # Apply fighter exclusion filter
  if (!is.null(filters$excluded_fighters) && length(filters$excluded_fighters) > 0) {
    fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
    
    # More efficient fighter exclusion
    filtered_lineups <- filtered_lineups[!rowSums(sapply(fighter_cols, function(col) {
      filtered_lineups[[col]] %in% filters$excluded_fighters
    })) > 0]
  }
  
  # Return early if no lineups match the filters
  if(nrow(filtered_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  # Calculate thresholds for display
  thresholds <- list()
  threshold_columns <- c("Top1Count", "Top2Count", "Top3Count", "Top5Count")
  
  for (col in threshold_columns) {
    if (col %in% names(filtered_lineups)) {
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

# Calculate filtered pool stats for FanDuel
calculate_fd_filtered_pool_stats <- function(optimal_lineups, filters) {
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  setDT(optimal_lineups)
  filtered_lineups <- copy(optimal_lineups)
  
  # Apply Top1Count filter
  if (!is.null(filters$min_top1_count) && "Top1Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
  }
  
  # Apply Top2Count filter
  if (!is.null(filters$min_top2_count) && "Top2Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top2Count >= filters$min_top2_count]
  }
  
  # Apply Top3Count filter
  if (!is.null(filters$min_top3_count) && "Top3Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top3Count >= filters$min_top3_count]
  }
  
  # Apply Top5Count filter
  if (!is.null(filters$min_top5_count) && "Top5Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top5Count >= filters$min_top5_count]
  }
  
  # Apply fighter exclusion filter
  if (!is.null(filters$excluded_fighters) && length(filters$excluded_fighters) > 0) {
    fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
    
    # More efficient fighter exclusion
    filtered_lineups <- filtered_lineups[!rowSums(sapply(fighter_cols, function(col) {
      filtered_lineups[[col]] %in% filters$excluded_fighters
    })) > 0]
  }
  
  # Return early if no lineups match the filters
  if(nrow(filtered_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  # Calculate thresholds for display
  thresholds <- list()
  threshold_columns <- c("Top1Count", "Top2Count", "Top3Count", "Top5Count")
  
  for (col in threshold_columns) {
    if (col %in% names(filtered_lineups)) {
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

# Simplified DraftKings random lineup generation
generate_random_dk_lineups <- function(optimal_lineups, filters) {
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
  fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
  all_fighters <- unique(unlist(filtered_lineups[, ..fighter_cols]))
  fighter_counts <- setNames(numeric(length(all_fighters)), all_fighters)
  
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
    
    # Update fighter counts for exposure calculation
    candidates_fighters <- unlist(filtered_lineups[selected_idx, ..fighter_cols])
    fighter_counts[candidates_fighters] <- fighter_counts[candidates_fighters] + 1
  }
  
  if (nrow(selected_lineups) == 0) return(NULL)
  
  # Calculate exposure for attribute
  final_exposure <- (fighter_counts / nrow(selected_lineups)) * 100
  attr(selected_lineups, "exposure") <- final_exposure
  
  # Keep only needed columns
  keep_cols <- c(fighter_cols, "Top1Count", "Top2Count", "Top3Count", "Top5Count", "TotalSalary")
  keep_cols <- intersect(keep_cols, names(selected_lineups))
  
  return(as.data.frame(selected_lineups[, ..keep_cols]))
}

# Simplified FanDuel random lineup generation
generate_random_fd_lineups <- function(optimal_lineups, filters) {
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
  
  # Apply fighter exclusions
  if (!is.null(filters$excluded_fighters) && length(filters$excluded_fighters) > 0) {
    fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
    to_exclude <- logical(nrow(filtered_lineups))
    
    for(col in fighter_cols) {
      to_exclude <- to_exclude | filtered_lineups[[col]] %in% filters$excluded_fighters
    }
    
    filtered_lineups <- filtered_lineups[!to_exclude]
  }
  
  # Check if any lineups match filters
  if (nrow(filtered_lineups) == 0) {
    return(NULL)
  }
  
  # Prepare for tracking
  fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
  all_fighters <- unique(unlist(filtered_lineups[, ..fighter_cols]))
  fighter_counts <- setNames(numeric(length(all_fighters)), all_fighters)
  
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
    
    # Update fighter counts for exposure calculation
    candidates_fighters <- unlist(filtered_lineups[selected_idx, ..fighter_cols])
    fighter_counts[candidates_fighters] <- fighter_counts[candidates_fighters] + 1
  }
  
  if (nrow(selected_lineups) == 0) return(NULL)
  
  # Calculate exposure for attribute
  final_exposure <- (fighter_counts / nrow(selected_lineups)) * 100
  attr(selected_lineups, "exposure") <- final_exposure
  
  # Keep only needed columns
  keep_cols <- c(fighter_cols, "Top1Count", "Top2Count", "Top3Count", "Top5Count", "TotalSalary")
  keep_cols <- intersect(keep_cols, names(selected_lineups))
  
  return(as.data.frame(selected_lineups[, ..keep_cols]))
}

# Generate random FanDuel lineups from filtered pool
generate_random_fd_lineups <- function(optimal_lineups, filters) {
  filtered_lineups <- optimal_lineups
  
  # Apply filters with early return if empty
  if(is.null(filtered_lineups) || nrow(filtered_lineups) == 0) {
    return(NULL)
  }
  
  # Convert to data.table for better performance
  setDT(filtered_lineups)
  
  # Apply min threshold filters
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
  
  # Exclude specific fighters
  if (!is.null(filters$excluded_fighters) && length(filters$excluded_fighters) > 0) {
    fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
    to_exclude <- logical(nrow(filtered_lineups))
    
    for(col in fighter_cols) {
      to_exclude <- to_exclude | filtered_lineups[[col]] %in% filters$excluded_fighters
    }
    
    filtered_lineups <- filtered_lineups[!to_exclude]
  }
  
  if (nrow(filtered_lineups) == 0) {
    return(NULL)
  }
  
  # Pre-allocate for tracking
  fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
  all_fighters <- unique(unlist(filtered_lineups[, ..fighter_cols]))
  fighter_counts <- setNames(numeric(length(all_fighters)), all_fighters)
  
  # Use the selected lineups data.table
  selected_lineups <- data.table()
  selected_indices <- integer(0)
  
  # Use Top1Count as weight for sampling
  weight_col <- "Top1Count"
  
  # Sampling loop with max attempts limit
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
    
    candidate_lineup <- filtered_lineups[selected_idx]
    
    # Get fighters from this lineup
    candidate_fighters <- unlist(candidate_lineup[, ..fighter_cols])
    
    # Update fighter counts
    fighter_counts <- fighter_counts + table(factor(candidate_fighters, levels = names(fighter_counts)))
    
    # Add lineup unconditionally (no max exposure check)
    selected_lineups <- rbind(selected_lineups, candidate_lineup)
    selected_indices <- c(selected_indices, selected_idx)
    
    # Periodically clean up
    if (attempts %% 100 == 0) gc(verbose = FALSE)
  }
  
  if (nrow(selected_lineups) == 0) return(NULL)
  
  # Calculate exposure for attribute
  final_exposure <- (fighter_counts / nrow(selected_lineups)) * 100
  attr(selected_lineups, "exposure") <- final_exposure
  
  return(as.data.frame(selected_lineups))
}

# Calculate fighter exposure for DraftKings lineups
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
  
  # Initialize metrics data frame
  metrics_data <- data.table(
    Name = all_fighters,
    DKSalary = NA_real_,
    DKOwn = NA_real_,
    OptimalRate = 0,
    EliteRate = 0,
    FloorRate = 0,
    AppearanceRate = 0,
    Exposure = 0,
    Leverage = 0
  )
  
  # Match with fantasy analysis data using direct matching
  if(!is.null(fantasy_analysis) && nrow(fantasy_analysis) > 0) {
    
      # Prepare lookup table
      lookup_data <- fantasy_analysis[, .(Name, DKSalary, DKOwn)]
      setkey(lookup_data, Name)
      
      # Update all metrics data at once for efficiency
      for(i in 1:nrow(metrics_data)) {
        fighter_name <- metrics_data$Name[i]
        
        # Find matching fighter data
        matches <- lookup_data[Name == fighter_name]
        
        if(nrow(matches) > 0) {
          # Update values directly
          metrics_data[i, DKSalary := matches$DKSalary[1]]
          metrics_data[i, DKOwn := matches$DKOwn[1]]
        }
      }
  }
  
  # Calculate OptimalRate (percentage of Top1Count lineups with this fighter)
  total_top1 <- sum(optimal_lineups$Top1Count, na.rm = TRUE)
  if(total_top1 > 0) {
    for(fighter in all_fighters) {
      # Find lineups with this fighter
      fighter_appears <- logical(nrow(optimal_lineups))
      for(col in fighter_cols) {
        fighter_appears <- fighter_appears | (optimal_lineups[[col]] == fighter)
      }
      driver_matches <- which(fighter_appears)
      
      # Calculate optimal rate percentage
      fighter_total <- sum(optimal_lineups$Top1Count[driver_matches], na.rm = TRUE)
      metrics_data[Name == fighter, OptimalRate := (fighter_total / total_top1) * 100]
    }
  }
  
  # Calculate EliteRate (top 10% of lineups by Top1Count)
  if(nrow(optimal_lineups) >= 10) {
    elite_lineups <- copy(optimal_lineups)
    elite_lineups <- elite_lineups[order(-Top1Count, -Top5Count)]
    
    n_elite <- max(1, round(nrow(elite_lineups) * 0.1))
    elite_lineups <- elite_lineups[1:n_elite]
    
    for(fighter in all_fighters) {
      # Count appearances in elite lineups
      fighter_appears <- logical(nrow(elite_lineups))
      for(col in fighter_cols) {
        fighter_appears <- fighter_appears | (elite_lineups[[col]] == fighter)
      }
      fighter_elite_count <- sum(fighter_appears)
      
      # Calculate elite rate
      metrics_data[Name == fighter, EliteRate := (fighter_elite_count / n_elite) * 100]
    }
  }
  
  # Calculate FloorRate (top 20% of lineups by Top5Count)
  if(nrow(optimal_lineups) >= 5) {
    floor_lineups <- copy(optimal_lineups)
    floor_lineups <- floor_lineups[order(-Top5Count, -Top1Count)]
    
    n_floor <- max(1, round(nrow(floor_lineups) * 0.2))
    floor_lineups <- floor_lineups[1:n_floor]
    
    for(fighter in all_fighters) {
      # Count appearances in floor lineups
      fighter_appears <- logical(nrow(floor_lineups))
      for(col in fighter_cols) {
        fighter_appears <- fighter_appears | (floor_lineups[[col]] == fighter)
      }
      fighter_floor_count <- sum(fighter_appears)
      
      # Calculate floor rate
      metrics_data[Name == fighter, FloorRate := (fighter_floor_count / n_floor) * 100]
    }
  }
  
  # Calculate AppearanceRate (percentage of all lineups with this fighter)
  if(nrow(optimal_lineups) > 0) {
    for(fighter in all_fighters) {
      # Count appearances in all lineups
      fighter_appears <- logical(nrow(optimal_lineups))
      for(col in fighter_cols) {
        fighter_appears <- fighter_appears | (optimal_lineups[[col]] == fighter)
      }
      fighter_appearance_count <- sum(fighter_appears)
      
      # Calculate appearance rate
      metrics_data[Name == fighter, AppearanceRate := (fighter_appearance_count / nrow(optimal_lineups)) * 100]
    }
  }
  
  # Calculate Exposure from random lineups
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
  
  
  # Calculate leverage
  metrics_data[!is.na(DKOwn) & !is.na(Exposure), Leverage := Exposure - DKOwn]
  
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
  
  # Get all unique fighters (these are Names now instead of FDIDs)
  all_fighters <- unique(unlist(optimal_lineups[, ..fighter_cols]))
  
  # Initialize metrics data frame - use Name as primary identifier now
  metrics_data <- data.table(
    Name = all_fighters,  # This is the fighter name directly
    FDSalary = NA_real_,
    FDOP = NA_real_,
    OptimalRate = 0,
    EliteRate = 0,
    FloorRate = 0,
    AppearanceRate = 0,
    Exposure = 0,
    Leverage = 0,
    Starting = NA_real_,
    Proj = NA_real_,
    FDID = NA_character_  # We'll fill this in later if needed
  )
  
  # Calculate OptimalRate (percentage of Top1Count lineups with this fighter)
  total_top1 <- sum(optimal_lineups$Top1Count, na.rm = TRUE)
  if(total_top1 > 0) {
    for(fighter in all_fighters) {
      # Find lineups with this fighter
      fighter_appears <- logical(nrow(optimal_lineups))
      for(col in fighter_cols) {
        fighter_appears <- fighter_appears | (optimal_lineups[[col]] == fighter)
      }
      fighter_matches <- which(fighter_appears)
      
      # Calculate optimal rate percentage
      fighter_total <- sum(optimal_lineups$Top1Count[fighter_matches], na.rm = TRUE)
      metrics_data[Name == fighter, OptimalRate := (fighter_total / total_top1) * 100]
    }
  }
  
  # Calculate EliteRate (top 10% of lineups by Top1Count)
  if(nrow(optimal_lineups) >= 10) {
    elite_lineups <- copy(optimal_lineups)
    elite_lineups <- elite_lineups[order(-Top1Count, -Top5Count)]
    
    n_elite <- max(1, round(nrow(elite_lineups) * 0.1))
    elite_lineups <- elite_lineups[1:n_elite]
    
    for(fighter in all_fighters) {
      # Count appearances in elite lineups
      fighter_appears <- logical(nrow(elite_lineups))
      for(col in fighter_cols) {
        fighter_appears <- fighter_appears | (elite_lineups[[col]] == fighter)
      }
      fighter_elite_count <- sum(fighter_appears)
      
      # Calculate elite rate
      metrics_data[Name == fighter, EliteRate := (fighter_elite_count / n_elite) * 100]
    }
  }
  
  # Calculate FloorRate (top 20% of lineups by Top5Count)
  if(nrow(optimal_lineups) >= 5) {
    floor_lineups <- copy(optimal_lineups)
    floor_lineups <- floor_lineups[order(-Top5Count, -Top1Count)]
    
    n_floor <- max(1, round(nrow(floor_lineups) * 0.2))
    floor_lineups <- floor_lineups[1:n_floor]
    
    for(fighter in all_fighters) {
      # Count appearances in floor lineups
      fighter_appears <- logical(nrow(floor_lineups))
      for(col in fighter_cols) {
        fighter_appears <- fighter_appears | (floor_lineups[[col]] == fighter)
      }
      fighter_floor_count <- sum(fighter_appears)
      
      # Calculate floor rate
      metrics_data[Name == fighter, FloorRate := (fighter_floor_count / n_floor) * 100]
    }
  }
  
  # Calculate AppearanceRate (percentage of all lineups with this fighter)
  if(nrow(optimal_lineups) > 0) {
    for(fighter in all_fighters) {
      # Count appearances in all lineups
      fighter_appears <- logical(nrow(optimal_lineups))
      for(col in fighter_cols) {
        fighter_appears <- fighter_appears | (optimal_lineups[[col]] == fighter)
      }
      fighter_appearance_count <- sum(fighter_appears)
      
      # Calculate appearance rate
      metrics_data[Name == fighter, AppearanceRate := (fighter_appearance_count / nrow(optimal_lineups)) * 100]
    }
  }
  
  # Calculate Exposure from random lineups
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
  
  # Add fighter information from fantasy analysis - now we match directly on Name
  if(!is.null(fantasy_analysis) && nrow(fantasy_analysis) > 0) {
    for(i in 1:nrow(metrics_data)) {
      fighter_name <- metrics_data$Name[i]
      match_idx <- which(fantasy_analysis$Name == fighter_name)
      
      if(length(match_idx) > 0) {
        idx <- match_idx[1]
        
        # Copy over all relevant fields
        metrics_data$FDSalary[i] <- fantasy_analysis$FDSalary[idx]
        metrics_data$FDOP[i] <- fantasy_analysis$FDOP[idx]
        metrics_data$Starting[i] <- fantasy_analysis$Starting[idx]
        
        # Also copy FDID if needed for reference
        if("FDID" %in% names(fantasy_analysis)) {
          metrics_data$FDID[i] <- fantasy_analysis$FDID[idx]
        }
        
        # Get projection if available
        if("Median_Fantasy_Pts" %in% names(fantasy_analysis)) {
          metrics_data$Proj[i] <- fantasy_analysis$Median_Fantasy_Pts[idx]
        } else if("Proj" %in% names(fantasy_analysis)) {
          metrics_data$Proj[i] <- fantasy_analysis$Proj[idx]
        }
      }
    }
  }
  
  # Convert ownership to percentage format if needed
  if(!is.null(metrics_data$FDOP) && !all(is.na(metrics_data$FDOP))) {
    if(max(metrics_data$FDOP, na.rm = TRUE) <= 1) {
      # Convert to percentage for consistent leverage calculation
      metrics_data[, FDOP := FDOP * 100]
    }
  }
  
  # Calculate leverage
  metrics_data[!is.na(FDOP) & !is.na(Exposure), Leverage := Exposure - FDOP]
  
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
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Input Check", tabName = "upload", icon = icon("upload")),
      menuItem("Fight Analysis", tabName = "fight_analysis", icon = icon("chart-line")),
      menuItem("Fantasy Projections", tabName = "fantasy", icon = icon("calculator")),
      menuItem("Optimal Lineups", tabName = "optimal_lineups", icon = icon("trophy")),
      menuItem("Lineup Builder", tabName = "lineup_builder", icon = icon("percentage"))
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
                    plotlyOutput("outcome_dist_plot", height = "1100px") %>% withSpinner(color = "#ff0000")
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
                  height = "100px",
                  title = "Run Lineup Optimization",
                  status = "primary",
                  solidHeader = TRUE,
                  div(
                    style = "display: flex; gap: 10px; margin: 5px 0;",
                    conditionalPanel(
                      condition = "output.has_draftkings === 'true'",
                      actionButton("run_dk_optimization", "Calculate DraftKings Lineups",
                                   class = "btn-primary", 
                                   style = "flex: 1; padding: 3px; font-size: 12px;")
                    ),
                    conditionalPanel(
                      condition = "output.has_fanduel === 'true'",
                      actionButton("run_fd_optimization", "Calculate FanDuel Lineups",
                                   class = "btn-primary", 
                                   style = "flex: 1; padding: 3px; font-size: 12px;")
                    )
                  ),
                  conditionalPanel(
                    condition = "output.has_draftkings != 'true' && output.has_fanduel != 'true'",
                    div(
                      class = "alert alert-warning",
                      style = "margin: 0; padding: 5px; font-size: 11px;",
                      "No fantasy platform data detected. Check your input file."
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_dk_lineups === 'true'",
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
              conditionalPanel(
                condition = "output.has_fd_lineups === 'true'",
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
              )
      ),
      
      # Lineup Builder Tab
      tabItem(tabName = "lineup_builder",
              uiOutput("lineup_builder_ui")
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
    dk_fighter_exposure = NULL,
    fd_fighter_exposure = NULL,
    dk_random_lineups = NULL,
    fd_random_lineups = NULL,
    file_uploaded = FALSE,
    simulation_complete = FALSE
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
              DTOutput("accuracy_analysis") %>% withSpinner(color = "#ff0000"),
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
              DTOutput("data_preview") %>% withSpinner(color = "#ff0000")
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
                DTOutput("dk_fantasy_projections") %>% withSpinner(color = "#ff0000"),
                downloadButton('download_dk_fantasy_projections', 'Download Projections')
            )
          ),
          fluidRow(
            box(width = 12,
                title = "DraftKings Fantasy Points Distribution",
                plotlyOutput("dk_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#ff0000")
            )
          )
        ),
        tabPanel(
          "FanDuel",
          fluidRow(
            box(width = 12,
                title = "FanDuel Fantasy Point Projections",
                DTOutput("fd_fantasy_projections") %>% withSpinner(color = "#ff0000"),
                downloadButton('download_fd_fantasy_projections', 'Download Projections')
            )
          ),
          fluidRow(
            box(width = 12,
                title = "FanDuel Fantasy Points Distribution",
                plotlyOutput("fd_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#ff0000")
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
              DTOutput("dk_fantasy_projections") %>% withSpinner(color = "#ff0000"),
              downloadButton('download_dk_fantasy_projections', 'Download Projections')
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Fantasy Points vs Salary",
              plotlyOutput("dk_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#ff0000")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Fantasy Points Distribution",
              plotlyOutput("dk_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#ff0000")
          )
        )
      )
    } else if(rv$has_fanduel) {
      # Only FanDuel available
      tagList(
        fluidRow(
          box(width = 12,
              title = "FanDuel Fantasy Point Projections",
              DTOutput("fd_fantasy_projections") %>% withSpinner(color = "#ff0000"),
              downloadButton('download_fd_fantasy_projections', 'Download Projections')
          )
        ),
        fluidRow(
          box(width = 12,
              title = "FanDuel Fantasy Points vs Salary",
              plotlyOutput("fd_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#ff0000")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "FanDuel Fantasy Points Distribution",
              plotlyOutput("fd_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#ff0000")
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
  
  # Lineup Builder UI
  output$lineup_builder_ui <- renderUI({
    if(is.null(rv$dk_optimal_lineups) && is.null(rv$fd_optimal_lineups)) {
      return(
        box(
          width = 12,
          status = "warning",
          title = "No Optimal Lineups Available",
          "Please calculate optimal lineups in the Optimal Lineups tab first."
        )
      )
    }
    
    tagList(
      # DraftKings lineup builder UI 
      conditionalPanel(
        condition = "output.has_dk_lineups == 'true'",
        fluidRow(
          box(width = 12,
              title = "DraftKings Lineup Count Thresholds",
              DTOutput("dk_lineup_count_thresholds") %>% withSpinner(color = "#ff0000")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Lineup Filters",
              fluidRow(
                column(3,
                       numericInput("dk_min_top1_count", "Min Top 1 Count:", 
                                    value = 0, min = 0)
                ),
                column(3,
                       numericInput("dk_min_top2_count", "Min Top 2 Count:", 
                                    value = 0, min = 0)
                ),
                column(3,
                       numericInput("dk_min_top3_count", "Min Top 3 Count:", 
                                    value = 0, min = 0)
                ),
                column(3,
                       numericInput("dk_min_top5_count", "Min Top 5 Count:", 
                                    value = 0, min = 0)
                )
              ),
              fluidRow(
                column(6,
                       numericInput("dk_num_random_lineups", "Number of Lineups to Generate:", 
                                    value = 20, min = 1, max = 150)
                )
              ),
              fluidRow(
                column(6,
                       div(class = "well well-sm",
                           h4("Filtered Pool Statistics:"),
                           textOutput("dk_filtered_pool_size")
                       )
                ),
                column(6,
                       div(style = "margin-top: 20px;",
                           actionButton("generate_dk_lineups", "Randomize DraftKings Lineups", 
                                        class = "btn-primary btn-lg", 
                                        style = "width: 100%;"),
                           br(), br(),
                           downloadButton("download_dk_random_lineups", "Download Selected Lineups", 
                                          style = "width: 100%;")
                       )
                )
              )
          )
        ),
        box(
          width = 12,
          title = "Understanding Fighter Rates",
          status = "info",
          solidHeader = TRUE,
          p("These rate statistics help you understand each fighter's presence in lineups:"),
          tags$ul(
            tags$li(tags$strong("OptimalRate:"), "How often fighters appear in lineups that finished 1st (optimal) in at least one simulation"),
            tags$li(tags$strong("EliteRate:"), "How often fighters appear in the top 10% of lineups when ranked by Top1Count"),
            tags$li(tags$strong("FloorRate:"), "How often fighters appear in the top 20% of lineups when ranked by Top5Count"),
            tags$li(tags$strong("AppearanceRate:"), "How often fighters appear across all lineups in the full pool"),
            tags$li(tags$strong("Exposure:"), "How often fighters appear in your selected randomized lineups"),
            tags$li(tags$strong("Leverage:"), "The difference between a fighter's exposure in your lineups and their projected ownership")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Fighter Exposure Analysis",
              DTOutput("dk_fighter_exposure_table") %>% withSpinner(color = "#ff0000")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "Generated DraftKings Lineups",
              DTOutput("dk_random_lineups_table") %>% withSpinner(color = "#ff0000")
          )
        )
      ),
      
      # FanDuel lineup builder UI
      conditionalPanel(
        condition = "output.has_fd_lineups == 'true'",
        fluidRow(
          box(width = 12,
              title = "FanDuel Lineup Count Thresholds",
              DTOutput("fd_lineup_count_thresholds") %>% withSpinner(color = "#ff0000")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "FanDuel Lineup Filters",
              fluidRow(
                column(3,
                       numericInput("fd_min_top1_count", "Min Top 1 Count:", 
                                    value = 0, min = 0)
                ),
                column(3,
                       numericInput("fd_min_top2_count", "Min Top 2 Count:", 
                                    value = 0, min = 0)
                ),
                column(3,
                       numericInput("fd_min_top3_count", "Min Top 3 Count:", 
                                    value = 0, min = 0)
                ),
                column(3,
                       numericInput("fd_min_top5_count", "Min Top 5 Count:", 
                                    value = 0, min = 0)
                )
              ),
              fluidRow(
                column(6,
                       selectizeInput("fd_excluded_fighters", "Exclude Fighters:",
                                      choices = NULL,
                                      multiple = TRUE,
                                      options = list(
                                        plugins = list('remove_button'),
                                        placeholder = 'Click to select fighters to exclude'
                                      ))
                ),
                column(6,
                       numericInput("fd_num_random_lineups", "Number of Lineups to Generate:", 
                                    value = 20, min = 1, max = 150)
                )
              ),
              fluidRow(
                column(6,
                       div(class = "well well-sm",
                           h4("Filtered Pool Statistics:"),
                           textOutput("fd_filtered_pool_size")
                       )
                ),
                column(6,
                       div(style = "margin-top: 20px;",
                           actionButton("generate_fd_lineups", "Randomize FanDuel Lineups", 
                                        class = "btn-primary btn-lg", 
                                        style = "width: 100%;"),
                           br(), br(),
                           downloadButton("download_fd_random_lineups", "Download Selected Lineups", 
                                          style = "width: 100%;")
                       )
                )
              )
          )
        ),
        box(
          width = 12,
          title = "Understanding Fighter Rates",
          status = "info",
          solidHeader = TRUE,
          p("These rate statistics help you understand each fighter's presence in lineups:"),
          tags$ul(
            tags$li(tags$strong("OptimalRate:"), "How often fighters appear in lineups that finished 1st (optimal) in at least one simulation"),
            tags$li(tags$strong("EliteRate:"), "How often fighters appear in the top 10% of lineups when ranked by Top1Count"),
            tags$li(tags$strong("FloorRate:"), "How often fighters appear in the top 20% of lineups when ranked by Top5Count"),
            tags$li(tags$strong("AppearanceRate:"), "How often fighters appear across all lineups in the full pool"),
            tags$li(tags$strong("Exposure:"), "How often fighters appear in your selected randomized lineups"),
            tags$li(tags$strong("Leverage:"), "The difference between a fighter's exposure in your lineups and their projected ownership")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "FanDuel Fighter Exposure Analysis",
              DTOutput("fd_fighter_exposure_table") %>% withSpinner(color = "#ff0000")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "Generated FanDuel Lineups",
              DTOutput("fd_random_lineups_table") %>% withSpinner(color = "#ff0000")
          )
        )
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
  
  # Calculate DraftKings optimal lineup
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
      }
      
      # Remove the processing modal
      removeModal()
    })
  })
  
  # Calculate FanDuel optimal lineup
  observeEvent(input$run_fd_optimization, {
    req(rv$simulation_results, rv$has_fanduel)
    
    # Clear previous analysis results but keep simulation data
    rv$fd_optimal_lineups <- NULL
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
        lineup_fighters <- unique(lineup_fighters)
        
        fighter_mapping <- data.frame(
          FDID = lineup_fighters,  # Using FDID as primary key
          Name = NA_character_,
          FDSalary = NA_real_,
          FDOwn = NA_real_,
          Proj = NA_real_
        )
        
        # Get mapping from simulation results
        unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$FDID), 
                                                     c("FDID", "Name", "FDSalary", "FDOwn")]
        
        # Match each fighter from lineups to the simulation results
        for(i in 1:nrow(fighter_mapping)) {
          fd_id <- fighter_mapping$FDID[i]
          # Try to find this fighter in the simulation results
          matches <- which(unique_sim_fighters$FDID == fd_id)
          if(length(matches) > 0) {
            match_idx <- matches[1]
            fighter_mapping$Name[i] <- unique_sim_fighters$Name[match_idx]
            fighter_mapping$FDSalary[i] <- unique_sim_fighters$FDSalary[match_idx]
            fighter_mapping$FDOwn[i] <- unique_sim_fighters$FDOwn[match_idx]
            
            # Get projection from fantasy analysis
            if(!is.null(rv$fd_fantasy_analysis)) {
              name_match <- which(rv$fd_fantasy_analysis$Name == unique_sim_fighters$Name[match_idx])
              if(length(name_match) > 0) {
                fighter_mapping$Proj[i] <- rv$fd_fantasy_analysis$Win_Median_FDScore[name_match[1]]
              }
            }
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
      
      # Create choices with names
      fighter_choices <- setNames(fighter_ids, fighter_labels)
      
      # Update the select input with choices
      updateSelectizeInput(
        session = session,
        inputId = "fd_excluded_fighters",
        choices = fighter_choices,
        selected = character(0)  # Empty selection initially
      )
      
      # Show success message
      showModal(modalDialog(
        title = "Success",
        HTML(sprintf(
          "Successfully generated <b>%d</b> optimal lineups for FanDuel!<br><br>
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
                      "TotalSalary")
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
  
  # FanDuel Optimal Lineups Table
  output$fd_optimal_lineups_table <- renderDT({
    req(rv$fd_optimal_lineups)
    
    # Clone lineups for display
    display_data <- as.data.table(rv$fd_optimal_lineups)
    
    # Format fighter columns to show names
    if(!is.null(rv$fd_fighter_exposure)) {
      for(i in 1:FD_ROSTER_SIZE) {
        col <- paste0("Fighter", i)
        display_data[[col]] <- sapply(display_data[[col]], function(id) {
          match_idx <- which(rv$fd_fighter_exposure$FDID == id)
          if(length(match_idx) > 0) {
            rv$fd_fighter_exposure$Name[match_idx[1]]
          } else {
            id
          }
        })
      }
    }
    
    # Remove Rank columns, keep only TopX Count columns
    cols_to_keep <- c(paste0("Fighter", 1:FD_ROSTER_SIZE), 
                      grep("^Top[0-9]+Count$", names(display_data), value = TRUE),
                      "TotalSalary")
    cols_to_keep <- intersect(cols_to_keep, names(display_data))
    
    # Use the correct data.table syntax with ..cols_to_keep
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
        rownames = FALSE,    # No row numbers
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
      rownames = FALSE      # Explicitly set rownames to FALSE again at the table level
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
    
    dt
  })
  
  # DraftKings Lineup Count Thresholds
  output$dk_lineup_count_thresholds <- renderDT({
    req(rv$dk_optimal_lineups)
    
    # Create threshold values to check
    threshold_values <- c(1, 2, 3, 5, 10, 15, 20, 25)
    
    # Initialize a data frame to hold the counts
    count_data <- data.frame(
      Threshold = threshold_values,
      Top1Count = numeric(length(threshold_values)),
      Top2Count = numeric(length(threshold_values)),
      Top3Count = numeric(length(threshold_values)),
      Top5Count = numeric(length(threshold_values))
    )
    
    # Calculate counts for each threshold and column
    for (i in seq_along(threshold_values)) {
      threshold <- threshold_values[i]
      
      # Count lineups with each column at or above the threshold
      if ("Top1Count" %in% names(rv$dk_optimal_lineups))
        count_data$Top1Count[i] <- sum(rv$dk_optimal_lineups$Top1Count >= threshold)
      
      if ("Top2Count" %in% names(rv$dk_optimal_lineups))
        count_data$Top2Count[i] <- sum(rv$dk_optimal_lineups$Top2Count >= threshold)
      
      if ("Top3Count" %in% names(rv$dk_optimal_lineups))
        count_data$Top3Count[i] <- sum(rv$dk_optimal_lineups$Top3Count >= threshold)
      
      if ("Top5Count" %in% names(rv$dk_optimal_lineups))
        count_data$Top5Count[i] <- sum(rv$dk_optimal_lineups$Top5Count >= threshold)
    }
    datatable(
      count_data,
      options = list(
        pageLength = 8,       # Show all the rows
        dom = 't',            # Only show the table, no search or pagination
        ordering = FALSE,     # No sorting
        rownames = FALSE      # No row names
      ),
      rownames = FALSE,       # Add this explicit parameter at the datatable level
      caption = "Number of lineups with at least this many appearances"
    )
  })
  
  # FanDuel Lineup Count Thresholds
  output$fd_lineup_count_thresholds <- renderDT({
    req(rv$fd_optimal_lineups)
    
    # Create threshold values to check
    threshold_values <- c(1, 2, 3, 5, 10, 15, 20, 25)
    
    # Initialize a data frame to hold the counts
    count_data <- data.frame(
      Threshold = threshold_values,
      Top1Count = numeric(length(threshold_values)),
      Top2Count = numeric(length(threshold_values)),
      Top3Count = numeric(length(threshold_values)),
      Top5Count = numeric(length(threshold_values))
    )
    
    # Calculate counts for each threshold and column
    for (i in seq_along(threshold_values)) {
      threshold <- threshold_values[i]
      
      # Count lineups with each column at or above the threshold
      if ("Top1Count" %in% names(rv$fd_optimal_lineups))
        count_data$Top1Count[i] <- sum(rv$fd_optimal_lineups$Top1Count >= threshold)
      
      if ("Top2Count" %in% names(rv$fd_optimal_lineups))
        count_data$Top2Count[i] <- sum(rv$fd_optimal_lineups$Top2Count >= threshold)
      
      if ("Top3Count" %in% names(rv$fd_optimal_lineups))
        count_data$Top3Count[i] <- sum(rv$fd_optimal_lineups$Top3Count >= threshold)
      
      if ("Top5Count" %in% names(rv$fd_optimal_lineups))
        count_data$Top5Count[i] <- sum(rv$fd_optimal_lineups$Top5Count >= threshold)
    }
    
    datatable(
      count_data,
      options = list(
        pageLength = 8,       # Show all the rows
        dom = 't',            # Only show the table, no search or pagination
        ordering = FALSE,     # No sorting
        rownames = FALSE      # No row names
      ),
      rownames = FALSE,       # Add this explicit parameter at the datatable level
      caption = "Number of lineups with at least this many appearances"
    )
  })
  
  # DraftKings filtered pool stats
  output$dk_filtered_pool_size <- renderText({
    req(rv$dk_optimal_lineups)
    
    filters <- list(
      min_top1_count = input$dk_min_top1_count,
      min_top2_count = input$dk_min_top2_count,
      min_top3_count = input$dk_min_top3_count,
      min_top5_count = input$dk_min_top5_count
    )
    
    stats <- calculate_dk_filtered_pool_stats(rv$dk_optimal_lineups, filters)
    paste("Number of lineups in filtered pool:", stats$count)
  })
  
  # FanDuel filtered pool stats
  output$fd_filtered_pool_size <- renderText({
    req(rv$fd_optimal_lineups)
    
    filters <- list(
      min_top1_count = input$fd_min_top1_count,
      min_top2_count = input$fd_min_top2_count,
      min_top3_count = input$fd_min_top3_count,
      min_top5_count = input$fd_min_top5_count,
      excluded_fighters = input$fd_excluded_fighters
    )
    
    stats <- calculate_fd_filtered_pool_stats(rv$fd_optimal_lineups, filters)
    paste("Number of lineups in filtered pool:", stats$count)
  })
  
  # Generate random DraftKings lineups
  observeEvent(input$generate_dk_lineups, {
    req(rv$dk_optimal_lineups)
    
    # Create filters for lineup generation
    filters <- list(
      min_top1_count = input$dk_min_top1_count,
      min_top2_count = input$dk_min_top2_count,
      min_top3_count = input$dk_min_top3_count,
      min_top5_count = input$dk_min_top5_count,
      num_lineups = input$dk_num_random_lineups
    )
    
    # Show progress
    withProgress(message = 'Generating lineups...', value = 0, {
      # Generate random lineups
      rv$dk_random_lineups <- generate_random_dk_lineups(rv$dk_optimal_lineups, filters)
      
      # Update fighter exposure data using the same mapping approach from optimization
      if(!is.null(rv$dk_random_lineups)) {
        # First, let's preserve the existing fighter mapping data
        existing_mapping <- NULL
        
        # Check if the current fighter exposure table has mapping data
        if(!is.null(rv$dk_fighter_exposure)) {
          # Extract the mapping columns from the exposure data
          existing_mapping <- rv$dk_fighter_exposure[, c( "Name", "DKSalary", "DKOwn")]
        }
        
        # If we don't have existing mapping, create a new one
        if(is.null(existing_mapping) || nrow(existing_mapping) == 0) {
          # Get all unique fighters from optimal lineups and random lineups
          fighter_cols <- paste0("Fighter", 1:DK_ROSTER_SIZE)
          
          # Get fighters from both lineup sets
          all_fighters <- c()
          
          # From optimal lineups
          for(col in fighter_cols) {
            if(col %in% names(rv$dk_optimal_lineups)) {
              all_fighters <- c(all_fighters, rv$dk_optimal_lineups[[col]])
            }
          }
          
          # From random lineups
          for(col in fighter_cols) {
            if(col %in% names(rv$dk_random_lineups)) {
              all_fighters <- c(all_fighters, rv$dk_random_lineups[[col]])
            }
          }
          all_fighters <- unique(all_fighters)
          
          # Create a fighter mapping from simulation results
          fighter_mapping <- data.frame(
            Name = all_fighters,
            DKSalary = NA_real_,
            DKOwn = NA_real_
          )
          
          # Get mapping from simulation results
          unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$Name), 
                                                       c("Name", "DKSalary", "DKOwn")]
          
          # Match each fighter
          for(i in 1:nrow(fighter_mapping)) {
            dk_id <- fighter_mapping$DKID[i]
            matches <- which(unique_sim_fighters$DKID == dk_id)
            
            if(length(matches) > 0) {
              match_idx <- matches[1]
              fighter_mapping$Name[i] <- unique_sim_fighters$Name[match_idx]
              fighter_mapping$DKSalary[i] <- unique_sim_fighters$DKSalary[match_idx]
              fighter_mapping$DKOwn[i] <- unique_sim_fighters$DKOwn[match_idx]
              
              # Get projection from fantasy analysis
              if(!is.null(rv$dk_fantasy_analysis)) {
                name_match <- which(rv$dk_fantasy_analysis$Name == unique_sim_fighters$Name[match_idx])
                if(length(name_match) > 0) {
                  fighter_mapping$Proj[i] <- rv$dk_fantasy_analysis$Win_Median_DKScore[name_match[1]]
                }
              }
            }
          }
        } else {
          # Use the existing mapping
          fighter_mapping <- existing_mapping
        }
        
        # Calculate fighter exposure with the mapping and random lineups
        rv$dk_fighter_exposure <- calculate_dk_fighter_exposure(
          rv$dk_optimal_lineups, 
          fighter_mapping, 
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
          sprintf("Generated %d DraftKings lineups successfully!", nrow(rv$dk_random_lineups)),
          easyClose = TRUE
        ))
      }
    })
  })
  
  # Generate random FanDuel lineups
  observeEvent(input$generate_fd_lineups, {
    req(rv$fd_optimal_lineups)
    
    # Create filters for lineup generation
    filters <- list(
      min_top1_count = input$fd_min_top1_count,
      min_top2_count = input$fd_min_top2_count,
      min_top3_count = input$fd_min_top3_count,
      min_top5_count = input$fd_min_top5_count,
      excluded_fighters = input$fd_excluded_fighters,
      num_lineups = input$fd_num_random_lineups
    )
    
    # Show progress
    withProgress(message = 'Generating lineups...', value = 0, {
      # Generate random lineups
      rv$fd_random_lineups <- generate_random_fd_lineups(rv$fd_optimal_lineups, filters)
      
      # Update fighter exposure data using the same mapping approach from optimization
      if(!is.null(rv$fd_random_lineups)) {
        # First, let's preserve the existing fighter mapping data
        existing_mapping <- NULL
        
        # Check if the current fighter exposure table has mapping data
        if(!is.null(rv$fd_fighter_exposure)) {
          # Extract the mapping columns from the exposure data
          existing_mapping <- rv$fd_fighter_exposure[, c("FDID", "Name", "FDSalary", "FDOwn", "Proj")]
        }
        
        # If we don't have existing mapping, create a new one
        if(is.null(existing_mapping) || nrow(existing_mapping) == 0) {
          # Get all unique fighters from optimal lineups and random lineups
          fighter_cols <- paste0("Fighter", 1:FD_ROSTER_SIZE)
          
          # Get fighters from both lineup sets
          all_fighters <- c()
          
          # From optimal lineups
          for(col in fighter_cols) {
            if(col %in% names(rv$fd_optimal_lineups)) {
              all_fighters <- c(all_fighters, rv$fd_optimal_lineups[[col]])
            }
          }
          
          # From random lineups
          for(col in fighter_cols) {
            if(col %in% names(rv$fd_random_lineups)) {
              all_fighters <- c(all_fighters, rv$fd_random_lineups[[col]])
            }
          }
          all_fighters <- unique(all_fighters)
          
          # Create a fighter mapping from simulation results
          fighter_mapping <- data.frame(
            FDID = all_fighters,
            Name = NA_character_,
            FDSalary = NA_real_,
            FDOwn = NA_real_,
            Proj = NA_real_
          )
          
          # Get mapping from simulation results
          unique_sim_fighters <- rv$simulation_results[!duplicated(rv$simulation_results$FDID), 
                                                       c("FDID", "Name", "FDSalary", "FDOwn")]
          
          # Match each fighter
          for(i in 1:nrow(fighter_mapping)) {
            fd_id <- fighter_mapping$FDID[i]
            matches <- which(unique_sim_fighters$FDID == fd_id)
            
            if(length(matches) > 0) {
              match_idx <- matches[1]
              fighter_mapping$Name[i] <- unique_sim_fighters$Name[match_idx]
              fighter_mapping$FDSalary[i] <- unique_sim_fighters$FDSalary[match_idx]
              fighter_mapping$FDOwn[i] <- unique_sim_fighters$FDOwn[match_idx]
              
              # Get projection from fantasy analysis
              if(!is.null(rv$fd_fantasy_analysis)) {
                name_match <- which(rv$fd_fantasy_analysis$Name == unique_sim_fighters$Name[match_idx])
                if(length(name_match) > 0) {
                  fighter_mapping$Proj[i] <- rv$fd_fantasy_analysis$Win_Median_FDScore[name_match[1]]
                }
              }
            }
          }
        } else {
          # Use the existing mapping
          fighter_mapping <- existing_mapping
        }
        
        # Calculate fighter exposure with the mapping and random lineups
        rv$fd_fighter_exposure <- calculate_fd_fighter_exposure(
          rv$fd_optimal_lineups, 
          fighter_mapping, 
          rv$fd_random_lineups
        )
      }
      
      # Show message based on result
      if(is.null(rv$fd_random_lineups)) {
        showModal(modalDialog(
          title = "Error",
          "No lineups match the selected filters. Try adjusting your criteria.",
          easyClose = TRUE
        ))
      } else {
        showModal(modalDialog(
          title = "Success",
          sprintf("Generated %d FanDuel lineups successfully!", nrow(rv$fd_random_lineups)),
          easyClose = TRUE
        ))
      }
    })
  })
  
  # DraftKings fighter exposure table
  output$dk_fighter_exposure_table <- renderDT({
    req(rv$dk_fighter_exposure)
    
    # Clone the data for display
    display_data <- rv$dk_fighter_exposure
    
    # If random lineups don't exist, remove the Exposure and Leverage columns
    if(is.null(rv$dk_random_lineups) || nrow(rv$dk_random_lineups) == 0) {
      # Make sure these columns don't appear if they somehow exist
      display_data$Exposure <- NULL
      display_data$Leverage <- NULL
    }
    
    # Hide DKID column
    display_data$DKID <- NULL
    
    # Reorder columns to put metrics in a logical order
    col_order <- c("Name", "Proj", "DKSalary", "DKOwn", "OptimalRate", "EliteRate", 
                   "FloorRate", "AppearanceRate")
    
    # Add Exposure and Leverage if they exist
    if("Exposure" %in% names(display_data)) {
      col_order <- c(col_order, "Exposure")
    }
    if("Leverage" %in% names(display_data)) {
      col_order <- c(col_order, "Leverage")
    }
    
    # Add remaining columns
    remaining_cols <- setdiff(names(display_data), col_order)
    col_order <- c(col_order, remaining_cols)
    
    # Reorder columns (only if they exist)
    display_data <- display_data[, intersect(col_order, names(display_data))]
    
    # Create the datatable
    dt <- datatable(
      display_data,
      options = list(
        pageLength = -1,
        dom = "t",
        scrollX = TRUE,
        order = list(list(6, 'desc')),  # Sort by OptimalRate by default (adjusted for new column order)
        rownames = FALSE  # Remove row numbers
      ),
      rownames = FALSE
    )
    
    # Apply formatting
    if("DKSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('DKSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    # Format DKOwn as percentage if present
    if("DKOwn" %in% names(display_data)) {
      dt <- dt %>% formatPercentage('DKOwn', digits = 1)
    }
    
    # Format numeric columns with 1 decimal place
    numeric_cols <- intersect(
      c('OptimalRate', 'EliteRate', 'FloorRate', 'AppearanceRate', 'Exposure', 'Leverage', 'Proj'),
      names(display_data)
    )
    
    if(length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(numeric_cols, digits = 1)
    }
    
    return(dt)
  })
  
  # FanDuel fighter exposure table
  output$fd_fighter_exposure_table <- renderDT({
    req(rv$fd_fighter_exposure)
    
    # Clone the data for display
    display_data <- rv$fd_fighter_exposure
    
    # If random lineups don't exist, remove the Exposure and Leverage columns
    if(is.null(rv$fd_random_lineups) || nrow(rv$fd_random_lineups) == 0) {
      # Make sure these columns don't appear if they somehow exist
      display_data$Exposure <- NULL
      display_data$Leverage <- NULL
    }
    
    # Hide FDID column
    display_data$FDID <- NULL
    
    # Reorder columns to put metrics in a logical order
    col_order <- c("Name", "Proj", "FDSalary", "FDOwn", "OptimalRate", "EliteRate", 
                   "FloorRate", "AppearanceRate")
    
    # Add Exposure and Leverage if they exist
    if("Exposure" %in% names(display_data)) {
      col_order <- c(col_order, "Exposure")
    }
    if("Leverage" %in% names(display_data)) {
      col_order <- c(col_order, "Leverage")
    }
    
    # Add remaining columns
    remaining_cols <- setdiff(names(display_data), col_order)
    col_order <- c(col_order, remaining_cols)
    
    # Reorder columns (only if they exist)
    display_data <- display_data[, intersect(col_order, names(display_data))]
    
    # Create the datatable
    dt <- datatable(
      display_data,
      options = list(
        pageLength = -1,
        dom = "t",
        scrollX = TRUE,
        order = list(list(6, 'desc')),  # Sort by OptimalRate by default (adjusted for new column order)
        rownames = FALSE  # Remove row numbers
      ),
      rownames = FALSE
    )
    
    # Apply formatting
    if("FDSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('FDSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    # Format FDOwn as percentage if present
    if("FDOwn" %in% names(display_data)) {
      dt <- dt %>% formatPercentage('FDOwn', digits = 1)
    }
    
    # Format numeric columns with 1 decimal place
    numeric_cols <- intersect(
      c('OptimalRate', 'EliteRate', 'FloorRate', 'AppearanceRate', 'Exposure', 'Leverage', 'Proj'),
      names(display_data)
    )
    
    if(length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(numeric_cols, digits = 1)
    }
    
    return(dt)
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
  
  # FanDuel random lineups table
  output$fd_random_lineups_table <- renderDT({
    req(rv$fd_random_lineups)
    
    # Clone for display
    display_data <- as.data.frame(rv$fd_random_lineups)  # Convert to data.frame to avoid data.table issues
    
    # Format fighter columns to show names
    if(!is.null(rv$fd_fighter_exposure)) {
      for(i in 1:FD_ROSTER_SIZE) {
        col <- paste0("Fighter", i)
        if(col %in% names(display_data)) {
          display_data[[col]] <- sapply(display_data[[col]], function(id) {
            match_idx <- which(rv$fd_fighter_exposure$FDID == id)
            if(length(match_idx) > 0) {
              rv$fd_fighter_exposure$Name[match_idx[1]]
            } else {
              id
            }
          })
        }
      }
    }
    
    # Keep only fighter columns and TopXCount columns
    cols_to_keep <- c(paste0("Fighter", 1:FD_ROSTER_SIZE), 
                      grep("^Top[0-9]+Count$", names(display_data), value = TRUE),
                      "TotalSalary")
    cols_to_keep <- intersect(cols_to_keep, names(display_data))
    
    # Use standard data.frame subsetting instead of data.table syntax
    display_data <- display_data[, cols_to_keep, drop = FALSE]
    
    # Create datatable with styling matching the optimal lineups table
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        rownames = FALSE,    # No row numbers
        dom = "tp",          # Only show table and pagination (no search)
        ordering = TRUE,     # Allow sorting
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
      
      # Keep only fighter columns, TopX Count columns, and TotalSalary
      cols_to_keep <- c(paste0("Fighter", 1:DK_ROSTER_SIZE), 
                        grep("^Top[0-9]+Count$", names(download_data), value = TRUE),
                        "TotalSalary")
      cols_to_keep <- intersect(cols_to_keep, names(download_data))
      download_data <- download_data[, cols_to_keep, drop = FALSE]
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"  # Explicitly set MIME type for CSV
  )
  
  output$download_fd_optimal_lineups <- downloadHandler(
    filename = function() {
      paste("fd_optimal_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      # Get the optimal lineups with fighter names
      display_data <- as.data.frame(rv$fd_optimal_lineups)
      
      # Create a name-to-FDID mapping from the simulation results
      name_to_id_map <- unique(rv$simulation_results[, c("Name", "FDID")])
      
      # Create a copy of the data for download that will use FDIDs
      download_data <- display_data
      
      # Replace fighter names with FDIDs in the download data
      for(i in 1:FD_ROSTER_SIZE) {
        col <- paste0("Fighter", i)
        if(col %in% names(download_data)) {
          download_data[[col]] <- sapply(download_data[[col]], function(name) {
            match_idx <- which(name_to_id_map$Name == name)
            if(length(match_idx) > 0) {
              name_to_id_map$FDID[match_idx[1]]
            } else {
              name  # Fallback to name if ID not found
            }
          })
        }
      }
      
      # Keep only fighter columns, TopX Count columns, and TotalSalary
      cols_to_keep <- c(paste0("Fighter", 1:FD_ROSTER_SIZE), 
                        grep("^Top[0-9]+Count$", names(download_data), value = TRUE),
                        "TotalSalary")
      cols_to_keep <- intersect(cols_to_keep, names(download_data))
      download_data <- download_data[, cols_to_keep, drop = FALSE]
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"  # Explicitly set MIME type for CSV
  )
  
  output$download_dk_random_lineups <- downloadHandler(
    filename = function() {
      paste("dk_random_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      # Check if random lineups exist
      if(is.null(rv$dk_random_lineups) || nrow(rv$dk_random_lineups) == 0) {
        # Create an empty dataframe with appropriate columns if no lineups exist
        empty_data <- data.frame(matrix(ncol = DK_ROSTER_SIZE, nrow = 0))
        colnames(empty_data) <- paste0("Fighter", 1:DK_ROSTER_SIZE)
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      # Create a copy for downloading
      download_data <- as.data.frame(rv$dk_random_lineups)
      
      # Create a name-to-DKID mapping from the simulation results
      name_to_id_map <- unique(rv$simulation_results[, c("Name", "DKID")])
      
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
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_fd_random_lineups <- downloadHandler(
    filename = function() {
      paste("fd_random_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      # Check if random lineups exist
      if(is.null(rv$fd_random_lineups) || nrow(rv$fd_random_lineups) == 0) {
        # Create an empty dataframe with appropriate columns if no lineups exist
        empty_data <- data.frame(matrix(ncol = FD_ROSTER_SIZE, nrow = 0))
        colnames(empty_data) <- paste0("Fighter", 1:FD_ROSTER_SIZE)
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      # Format data for download (convert to data.frame first to avoid data.table issues)
      download_data <- as.data.frame(rv$fd_random_lineups)
      
      # Keep only fighter columns, TopX Count columns, and TotalSalary
      cols_to_keep <- c(paste0("Fighter", 1:FD_ROSTER_SIZE), 
                        grep("^Top[0-9]+Count$", names(download_data), value = TRUE),
                        "TotalSalary")
      cols_to_keep <- intersect(cols_to_keep, names(download_data))
      download_data <- download_data[, cols_to_keep, drop = FALSE]
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"  # Explicitly set MIME type for CSV
  )
  
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