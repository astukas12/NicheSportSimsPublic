library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(lpSolve)
library(data.table)
library(readxl)
library(dplyr)

# Your helper functions
setDTthreads(2)

read_f1_input_file <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("File does not exist:", file_path))
  }
  
  # Try to read all required sheets
  required_sheets <- c("Drivers", "FL", "LL", "Salaries", "Classification")
  sheets <- list()
  
  for (sheet_name in required_sheets) {
    tryCatch({
      sheets[[sheet_name]] <- read_excel(file_path, sheet = sheet_name)
      
      # Check if sheet is empty
      if (nrow(sheets[[sheet_name]]) == 0) {
        stop(paste("Sheet", sheet_name, "is empty"))
      }
    }, error = function(e) {
      stop(paste("Error reading sheet", sheet_name, ":", e$message))
    })
  }
  
  # Convert sheets to data.tables
  driver_data <- as.data.table(sheets$Drivers)
  salaries_data <- as.data.table(sheets$Salaries)
  fl_data <- as.data.table(sheets$FL)
  ll_data <- as.data.table(sheets$LL)
  classification_data <- as.data.table(sheets$Classification)
  
  # Validate driver data
  required_driver_cols <- c("Name", "Team")
  for (col in required_driver_cols) {
    if (!(col %in% names(driver_data))) {
      stop(paste("Required column", col, "missing in Drivers sheet"))
    }
  }
  
  # Check for position probability columns (1-20)
  pos_cols <- as.character(1:20)
  missing_pos_cols <- pos_cols[!(pos_cols %in% names(driver_data))]
  if (length(missing_pos_cols) > 0) {
    stop(paste(
      "Missing position probability columns in Drivers sheet:",
      paste(missing_pos_cols, collapse = ", ")
    ))
  }
  
  # Validate salaries data
  required_salary_cols <- c("DFSID", "Name", "Position", "Salary")
  for (col in required_salary_cols) {
    if (!(col %in% names(salaries_data))) {
      stop(paste("Required column", col, "missing in Salaries sheet"))
    }
  }
  
  # Check for required salary positions
  required_positions <- c("Driver", "Captain", "Constructor")
  for (pos in required_positions) {
    if (sum(salaries_data$Position == pos) == 0) {
      stop(paste("No entries with Position =", pos, "found in Salaries sheet"))
    }
  }
  
  # Validate FL data
  required_fl_cols <- c("Finish", "Prob")
  for (col in required_fl_cols) {
    if (!(col %in% names(fl_data))) {
      stop(paste("Required column", col, "missing in FL sheet"))
    }
  }
  
  # Validate LL data
  required_ll_cols <- c("WinnerGrid", "Leaders", "Prob")
  for (col in required_ll_cols) {
    if (!(col %in% names(ll_data))) {
      stop(paste("Required column", col, "missing in LL sheet"))
    }
  }
  
  # Validate Classification data
  required_class_cols <- c("NumClassified", "Probability")
  for (col in required_class_cols) {
    if (!(col %in% names(classification_data))) {
      stop(paste("Required column", col, "missing in Classification sheet"))
    }
  }
  # Process data based on validation
  
  # Extract constructors from Salaries
  constructor_data <- salaries_data[Position == "Constructor", ]
  
  # Check if any constructors found
  if (nrow(constructor_data) == 0) {
    stop("No constructors found in Salaries sheet (Position = 'Constructor')")
  }
  
  # Select only needed columns for constructors
  constructor_data <- constructor_data[, .(DFSID, Name, Salary)]
  
  # Extract driver and captain salaries
  driver_salaries <- salaries_data[Position == "Driver", .(DFSID, Name, Salary)]
  captain_salaries <- salaries_data[Position == "Captain", .(Name, Salary)]
  
  # Check for potential errors
  if (length(unique(driver_data$Name)) != nrow(driver_data)) {
    stop("Duplicate driver names found in Drivers sheet")
  }
  
  # Check for missing teams
  if (any(is.na(driver_data$Team)) || any(driver_data$Team == "")) {
    stop("Missing team information for some drivers")
  }
  
  # Ensure Start column exists
  if (!"Start" %in% names(driver_data)) {
    if ("Grid" %in% names(driver_data)) {
      driver_data[, Start := Grid]
      message("Note: Using 'Grid' column as 'Start' position")
    } else {
      stop("Missing 'Start' or 'Grid' column in Drivers sheet")
    }
  }
  
  # Add DFSID to driver data from salaries data
  driver_data[, DFSID := NA]
  driver_data[, DriverSal := NA]
  driver_data[, CptSal := NA]
  
  # Update driver information with salary data
  matched_drivers <- 0
  for (i in 1:nrow(driver_data)) {
    driver_name <- driver_data$Name[i]
    
    # Find matching driver and captain in salary data
    driver_idx <- which(driver_salaries$Name == driver_name)
    captain_idx <- which(captain_salaries$Name == driver_name)
    
    if (length(driver_idx) > 0) {
      driver_data$DFSID[i] <- driver_salaries$DFSID[driver_idx[1]]
      driver_data$DriverSal[i] <- driver_salaries$Salary[driver_idx[1]]
      matched_drivers <- matched_drivers + 1
    }
    
    if (length(captain_idx) > 0) {
      driver_data$CptSal[i] <- captain_salaries$Salary[captain_idx[1]]
    }
  }
  
  # Check if all drivers have been matched
  if (matched_drivers < nrow(driver_data)) {
    stop(
      paste(
        "Only",
        matched_drivers,
        "out of",
        nrow(driver_data),
        "drivers matched with salary data. Check for name mismatches."
      )
    )
  }
  
  # Return processed data
  list(
    drivers = driver_data,
    constructors = constructor_data,
    fl_probs = fl_data,
    ll_data = ll_data,
    classification_data = classification_data
  )
}


assign_laps_led_optimized <- function(race_result, total_race_laps, ll_data = NULL) {
  # Initialize all drivers with 0 laps led
  race_result[, LapsLed := 0]
  
  # Find the winner - regardless of classification status
  winner_idx <- which(race_result$FinishPosition == 1)[1]
  
  if (length(winner_idx) == 0) {
    return(race_result)
  }
  
  # Get winner's starting grid position
  winner_grid <- min(20, max(1, race_result$Starting[winner_idx]))
  
  # Default to 2 leaders for grid positions beyond what we have data for
  n_leaders <- 2
  
  # If we have LL data, use it to determine number of leaders
  if (!is.null(ll_data) && nrow(ll_data) > 0) {
    # Find matching grid positions
    grid_matches <- ll_data[ll_data$WinnerGrid == winner_grid, ]
    
    if (nrow(grid_matches) > 0) {
      # Sample number of leaders based on probability
      n_leaders <- sample(grid_matches$Leaders, 1, prob = grid_matches$Prob)
    } else if (winner_grid > 7) {
      # For grid positions > 7, use the data for position 7
      grid_7_matches <- ll_data[ll_data$WinnerGrid == 7, ]
      if (nrow(grid_7_matches) > 0) {
        n_leaders <- sample(grid_7_matches$Leaders, 1, prob = grid_7_matches$Prob)
      }
    }
  }
  
  # If only 1 leader (the winner leads all laps)
  if (n_leaders == 1) {
    race_result[winner_idx, LapsLed := total_race_laps]
    return(race_result)
  }
  
  # For multiple leaders, allocate laps led
  # Winner's percentage based on grid position and number of leaders
  winner_pct <- 0
  if (winner_grid == 1) {
    winner_pct <- runif(1, 0.65, 0.85)  # Pole winner leads most laps
  } else if (winner_grid <= 3) {
    winner_pct <- runif(1, 0.50, 0.70)  # Front row leads substantial laps
  } else if (winner_grid <= 6) {
    winner_pct <- runif(1, 0.35, 0.55)  # Mid-grid leads moderate laps
  } else {
    winner_pct <- runif(1, 0.25, 0.45)  # Back leads fewer laps
  }
  
  # Adjust percentage based on number of leaders
  if (n_leaders == 3) {
    winner_pct <- winner_pct * 0.9  # Reduce winner's share with more leaders
  }
  
  # Assign winner laps
  winner_laps <- round(total_race_laps * winner_pct)
  race_result[winner_idx, LapsLed := winner_laps]
  
  # Calculate remaining laps
  remaining_laps <- total_race_laps - winner_laps
  
  if (remaining_laps <= 0) {
    return(race_result)
  }
  
  # Find additional leaders - focus on top finishers and front-row starters
  # REMOVED CLASSIFICATION CHECK - Focus on finishing position and starting grid only
  potential_leaders <- unique(c(
    which(race_result$FinishPosition > 1 & race_result$FinishPosition <= 5),
    which(race_result$Starting <= 3 & race_result$FinishPosition != 1)
  ))
  
  # If we don't have enough potential leaders, just use top finishers
  if (length(potential_leaders) < (n_leaders - 1)) {
    potential_leaders <- which(race_result$FinishPosition > 1)
  }
  
  # Select the additional leaders based on a simple score
  if (length(potential_leaders) > 0) {
    # Calculate scores (lower is better)
    scores <- numeric(length(potential_leaders))
    for (i in 1:length(potential_leaders)) {
      driver_idx <- potential_leaders[i]
      grid_score <- race_result$Starting[driver_idx] * 0.3  # 30% weight to grid
      finish_score <- race_result$FinishPosition[driver_idx] * 0.7  # 70% weight to finish
      scores[i] <- grid_score + finish_score + runif(1, 0, 1)  # Add small randomness
    }
    
    # Take the best potential leaders
    n_additional <- min(n_leaders - 1, length(potential_leaders))
    additional_leaders <- potential_leaders[order(scores)][1:n_additional]
    
    # Distribute remaining laps based on finishing position
    weights <- numeric(length(additional_leaders))
    for (i in 1:length(additional_leaders)) {
      # Better finishing position gets more weight
      pos <- race_result$FinishPosition[additional_leaders[i]]
      weights[i] <- 1 / pos  # Simple inverse relationship
    }
    weights <- weights / sum(weights)  # Normalize
    
    # Allocate laps
    laps_allocation <- floor(weights * remaining_laps)
    
    # Handle any remainder
    remainder <- remaining_laps - sum(laps_allocation)
    if (remainder > 0 && length(laps_allocation) > 0) {
      laps_allocation[1] <- laps_allocation[1] + remainder
    }
    
    # Assign laps to additional leaders
    for (i in 1:length(additional_leaders)) {
      race_result[additional_leaders[i], LapsLed := laps_allocation[i]]
    }
  }
  
  return(race_result)
}
simulate_f1_races <- function(driver_data,
                              constructor_data,
                              fl_probs,
                              ll_data,
                              classification_data,
                              total_race_laps = 60,
                              n_sims = 1000) {
  # Ensure we're working with data.tables
  if (!is.data.table(driver_data))
    driver_data <- as.data.table(driver_data)
  if (!is.data.table(constructor_data))
    constructor_data <- as.data.table(constructor_data)
  if (!is.null(fl_probs) &&
      !is.data.table(fl_probs))
    fl_probs <- as.data.table(fl_probs)
  if (!is.null(ll_data) &&
      !is.data.table(ll_data))
    ll_data <- as.data.table(ll_data)
  
  
  n_drivers <- nrow(driver_data)
  n_constructors <- nrow(constructor_data)
  
  # Extract marginal probability columns (for positions 1-20)
  pos_cols <- as.character(1:20)
  if (!all(pos_cols %in% names(driver_data))) {
    stop("Required marginal probability columns (1-20) not found")
  }
  
  # Initialize results lists
  all_driver_results <- vector("list", n_sims)
  all_constructor_results <- vector("list", n_sims)
  
  # Position points mapping for fantasy scoring
  position_points <- c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1, rep(0, 10))
  
  # Process in batches for better memory management
  batch_size <- min(200, max(50, ceiling(5000 / n_drivers)))
  n_batches <- ceiling(n_sims / batch_size)
  
  cat("Starting simulation with", n_sims, "races...\n")
  
  for (batch in 1:n_batches) {
    batch_start_time <- Sys.time()
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, n_sims)
    batch_sims <- end_idx - start_idx + 1
    
    cat(
      "Processing batch",
      batch,
      "of",
      n_batches,
      "(simulations",
      start_idx,
      "to",
      end_idx,
      ")\n"
    )
    
    # Process each simulation in this batch
    for (sim_offset in 1:batch_sims) {
      sim_id <- start_idx + sim_offset - 1
      
      # PERFORMANCE-BASED SIMULATION METHOD
      # Generate performance scores for each driver
      performance_scores <- numeric(n_drivers)
      
      for (driver in 1:n_drivers) {
        # Extract driver's probability distribution for finishing positions
        driver_probs <- as.numeric(driver_data[driver, pos_cols, with = FALSE])
        
        # Sample a position according to probabilities
        sampled_pos <- sample(1:20, 1, prob = driver_probs)
        
        # Add small random noise to break ties
        performance_scores[driver] <- sampled_pos + runif(1, 0, 0.1)
      }
      
      # Rank the drivers (lower score is better)
      finish_positions <- rank(performance_scores, ties.method = "random")
      
      # Create race result
      race_result <- data.table(
        DFSID = driver_data$DFSID,
        Name = driver_data$Name,
        Team = driver_data$Team,
        FinishPosition = finish_positions,
        Starting = if ("Start" %in% names(driver_data))
          driver_data$Start
        else
          1:n_drivers,
        DriverSalary = if ("DriverSal" %in% names(driver_data))
          driver_data$DriverSal
        else if ("Salary" %in% names(driver_data))
          driver_data$Salary
        else
          rep(5000, n_drivers),
        CaptainSalary = if ("CptSal" %in% names(driver_data))
          driver_data$CptSal
        else if ("Salary" %in% names(driver_data))
          driver_data$Salary * 1.5
        else
          rep(7500, n_drivers),
        SimID = sim_id
      )
      
      # Initialize additional columns
      race_result[, FastestLap := FALSE]
      race_result[, LapsLed := 0]
      race_result[, IsClassified := TRUE]
      race_result[, DefeatedTeammate := FALSE]
      
      # Apply fastest lap logic based on FL probabilities
      if (!is.null(fl_probs) && nrow(fl_probs) > 0) {
        # Ensure we have Finish and Prob columns
        if ("Finish" %in% names(fl_probs) &&
            "Prob" %in% names(fl_probs)) {
          # Create a mapping of positions to probabilities
          fl_position_probs <- numeric(20)
          
          # Fill in probabilities from the FL sheet
          for (i in 1:nrow(fl_probs)) {
            pos <- fl_probs$Finish[i]
            if (pos >= 1 && pos <= 20) {
              fl_position_probs[pos] <- fl_probs$Prob[i]
            }
          }
          
          # Ensure we have valid probabilities
          if (sum(fl_position_probs) > 0) {
            # Normalize probabilities if they don't sum to 1
            if (abs(sum(fl_position_probs) - 1) > 0.001) {
              fl_position_probs <- fl_position_probs / sum(fl_position_probs)
            }
            
            # Assign probabilities to drivers based on finish position
            fl_driver_probs <- numeric(n_drivers)
            for (d in 1:n_drivers) {
              pos <- race_result$FinishPosition[d]
              if (pos >= 1 && pos <= 20) {
                fl_driver_probs[d] <- fl_position_probs[pos]
              }
            }
            
            # Ensure we have valid probabilities
            if (sum(fl_driver_probs) > 0) {
              # Normalize again just to be sure
              fl_driver_probs <- fl_driver_probs / sum(fl_driver_probs)
              
              # Sample one driver for fastest lap
              fl_driver <- sample(1:n_drivers, 1, prob = fl_driver_probs)
              race_result[fl_driver, FastestLap := TRUE]
            } else {
              # Fallback: assign to random driver in top 8
              top8 <- which(race_result$FinishPosition <= 8)
              if (length(top8) > 0) {
                fl_driver <- sample(top8, 1)
                race_result[fl_driver, FastestLap := TRUE]
              } else {
                # Extreme fallback: just pick a random driver
                race_result[sample(1:n_drivers, 1), FastestLap := TRUE]
              }
            }
          } else {
            # Fallback: assign to random driver in top 8
            top8 <- which(race_result$FinishPosition <= 8)
            if (length(top8) > 0) {
              fl_driver <- sample(top8, 1)
              race_result[fl_driver, FastestLap := TRUE]
            } else {
              race_result[sample(1:n_drivers, 1), FastestLap := TRUE]
            }
          }
        } else {
          # Fallback if Finish or Prob columns missing: assign to random driver in top 8
          top8 <- which(race_result$FinishPosition <= 8)
          if (length(top8) > 0) {
            fl_driver <- sample(top8, 1)
            race_result[fl_driver, FastestLap := TRUE]
          } else {
            race_result[sample(1:n_drivers, 1), FastestLap := TRUE]
          }
        }
      } else {
        # Fallback if fl_probs is NULL: assign to random driver in top 8
        top8 <- which(race_result$FinishPosition <= 8)
        if (length(top8) > 0) {
          fl_driver <- sample(top8, 1)
          race_result[fl_driver, FastestLap := TRUE]
        } else {
          race_result[sample(1:n_drivers, 1), FastestLap := TRUE]
        }
      }
      
      race_result <- assign_laps_led_optimized(race_result, total_race_laps, ll_data)
      
      if (!is.null(classification_data) && nrow(classification_data) > 0) {
        # First determine how many drivers will be classified in this race
        total_drivers <- nrow(race_result)
        
        # Default all to classified initially
        race_result[, IsClassified := TRUE]
        
        # Get probabilities for different classification counts
        prob_vec <- classification_data$Probability
        count_vec <- classification_data$NumClassified
        
        # Sample the number of classified drivers based on probability
        n_classified <- sample(count_vec, 1, prob = prob_vec)
        
        # Limit to total drivers
        n_classified <- min(n_classified, total_drivers)
        
        # If not all drivers are classified
        if (n_classified < total_drivers) {
          # Determine which drivers are not classified
          # Use finish position as proxy - those at the back are less likely to be classified
          non_classified_indices <- order(race_result$FinishPosition, decreasing = TRUE)[1:(total_drivers - n_classified)]
          
          # Mark these drivers as not classified
          race_result[non_classified_indices, IsClassified := FALSE]
        }
      }
      
      team_list <- unique(race_result$Team)
      for (team in team_list) {
        # Get all drivers for this team regardless of classification status
        team_drivers <- which(race_result$Team == team)
        
        if (length(team_drivers) > 1) {
          # Get finishing positions for all drivers on this team
          finish_positions <- race_result$FinishPosition[team_drivers]
          
          # Find the best (lowest) finishing position
          best_pos <- min(finish_positions)
          
          # Find driver(s) with the best position
          best_drivers <- team_drivers[finish_positions == best_pos]
          
          # Mark these drivers as defeating their teammates
          race_result[best_drivers, DefeatedTeammate := TRUE]
        }
      }
      
      # Calculate driver fantasy points
      
      # Position points
      race_result[, PositionPoints := 0]
      for (i in 1:nrow(race_result)) {
        pos <- race_result$FinishPosition[i]
        if (pos >= 1 && pos <= 20) {
          race_result$PositionPoints[i] <- position_points[pos]
        }
      }
      
      # Position differential
      race_result[, PosDiff := Starting - FinishPosition]
      
      # Position differential points
      race_result[, DiffPoints := 0]
      for (i in 1:nrow(race_result)) {
        diff <- race_result$PosDiff[i]
        if (diff >= 10)
          race_result$DiffPoints[i] <- 5
        else if (diff >= 5)
          race_result$DiffPoints[i] <- 3
        else if (diff >= 3)
          race_result$DiffPoints[i] <- 2
        else if (diff <= -10)
          race_result$DiffPoints[i] <- -5
        else if (diff <= -5)
          race_result$DiffPoints[i] <- -3
        else if (diff <= -3)
          race_result$DiffPoints[i] <- -2
      }
      
      # Fastest lap points
      race_result[, FastestLapPoints := ifelse(FastestLap, 3, 0)]
      
      # Laps led points (0.1 points per lap led)
      race_result[, LapsLedPoints := LapsLed * 0.1]
      
      # Points for defeating teammate
      race_result[, TeammatePoints := ifelse(DefeatedTeammate, 5, 0)]
      
      # Points for being classified at finish
      race_result[, ClassifiedPoints := ifelse(IsClassified, 1, 0)]
      
      # Calculate total fantasy points
      race_result[, FantasyPoints := PositionPoints +
                    DiffPoints +
                    FastestLapPoints +
                    LapsLedPoints +
                    TeammatePoints +
                    ClassifiedPoints]
      
      # Store driver result
      all_driver_results[[sim_id]] <- race_result
      
      # CONSTRUCTOR SCORING
      # Create a constructor results table for this simulation
      constructor_results <- data.table(
        DFSID = constructor_data$DFSID,
        Name = constructor_data$Name,
        Salary = constructor_data$Salary,
        SimID = sim_id
      )
      
      # Calculate team performance metrics for each constructor
      constructor_results[, HasFastestLap := FALSE]
      constructor_results[, PositionPoints := 0]
      constructor_results[, BothFinished := FALSE]
      constructor_results[, BothInPoints := FALSE]
      constructor_results[, BothOnPodium := FALSE]
      constructor_results[, LapsLedPoints := 0]
      constructor_results[, BonusPoints := 0]
      constructor_results[, FLPoints := 0]
      
      # Process each constructor
      for (i in 1:nrow(constructor_results)) {
        constructor_name <- constructor_results$Name[i]
        
        # Find matching team in driver results
        team_drivers <- race_result[Team == constructor_name]
        
        if (nrow(team_drivers) > 0) {
          # Sum position points
          constructor_results$PositionPoints[i] <- sum(team_drivers$PositionPoints)
          
          # Check if any driver has fastest lap
          constructor_results$HasFastestLap[i] <- any(team_drivers$FastestLap)
          
          # Sum laps led points
          constructor_results$LapsLedPoints[i] <- sum(team_drivers$LapsLed) * 0.1
          
          # Check both drivers conditions
          if (nrow(team_drivers) >= 2) {
            # Both finished
            constructor_results$BothFinished[i] <- all(team_drivers$IsClassified)
            
            # Both in points (top 10)
            constructor_results$BothInPoints[i] <- all(team_drivers$FinishPosition <= 10)
            
            # Both on podium (top 3)
            constructor_results$BothOnPodium[i] <- all(team_drivers$FinishPosition <= 3)
          }
        }
        
        # Calculate bonus points
        bonus_points <- 0
        if (constructor_results$BothFinished[i])
          bonus_points <- bonus_points + 2
        if (constructor_results$BothInPoints[i])
          bonus_points <- bonus_points + 5
        if (constructor_results$BothOnPodium[i])
          bonus_points <- bonus_points + 3
        
        constructor_results$BonusPoints[i] <- bonus_points
        
        # Calculate fastest lap points separately (3 points for FL)
        constructor_results$FLPoints[i] <- if(constructor_results$HasFastestLap[i]) 3 else 0
        
        # Calculate total fantasy points
        constructor_results[i, FantasyPoints := PositionPoints + BonusPoints + LapsLedPoints + FLPoints]
      }
      
      # Store constructor results
      all_constructor_results[[sim_id]] <- constructor_results
    }
    
    batch_end_time <- Sys.time()
    batch_duration <- as.numeric(difftime(batch_end_time, batch_start_time, units = "secs"))
    cat("Completed batch",
        batch,
        "in",
        round(batch_duration, 2),
        "seconds\n")
    
    # Free memory after each batch
    gc(verbose = FALSE)
  }
  
  # Combine all results - handle potential NULL entries
  valid_driver_idx <- which(!sapply(all_driver_results, is.null))
  valid_constructor_idx <- which(!sapply(all_constructor_results, is.null))
  
  if (length(valid_driver_idx) == 0) {
    stop("No valid driver results generated")
  }
  
  if (length(valid_constructor_idx) == 0) {
    stop("No valid constructor results generated")
  }
  
  # Combine all results
  combined_driver_results <- rbindlist(all_driver_results[valid_driver_idx])
  combined_constructor_results <- rbindlist(all_constructor_results[valid_constructor_idx])
  
  # Return results
  list(
    driver_results = combined_driver_results,
    constructor_results = combined_constructor_results,
    successful_sims = length(valid_driver_idx)
  )
}

# Optimized LP solver function - finds the single best lineup
find_optimal_lineup_lp <- function(all_entries, salary_cap, exclusion_constraints = NULL) {
  # Store original entries for reference
  original_entries <- all_entries
  n_entries <- nrow(all_entries)
  
  # Identify different entry types
  driver_indices <- which(all_entries$Type == "Driver")
  captain_indices <- which(all_entries$Type == "Captain")
  constructor_indices <- which(all_entries$Type == "Constructor")
  
  # Calculate points per dollar for all entries
  all_entries$PPD <- 0
  
  all_entries$PPD[driver_indices] <- all_entries$FantasyPoints[driver_indices] / 
    all_entries$DriverSalary[driver_indices]
  all_entries$PPD[captain_indices] <- all_entries$FantasyPoints[captain_indices] / 
    all_entries$CaptainSalary[captain_indices]
  all_entries$PPD[constructor_indices] <- all_entries$FantasyPoints[constructor_indices] / 
    all_entries$Salary[constructor_indices]
  
  # Now set up the LP problem
  n_entries <- nrow(all_entries)
  
  # Objective: maximize fantasy points
  obj <- all_entries$FantasyPoints
  
  # Base constraints
  # Start with 4 base constraints + 1 for exactly 4 regular drivers
  n_constraints <- 5
  
  # Add driver-captain exclusivity constraints
  driver_ids <- unique(all_entries$DFSID[all_entries$Type %in% c("Driver", "Captain")])
  n_driver_constraints <- 0
  
  for (id in driver_ids) {
    if (any(all_entries$DFSID == id & all_entries$Type == "Driver") &&
        any(all_entries$DFSID == id & all_entries$Type == "Captain")) {
      n_driver_constraints <- n_driver_constraints + 1
    }
  }
  
  # Add team constraints (can't have 2+ drivers from same team AND constructor)
  n_team_constraints <- 0
  for (constructor_id in unique(all_entries$DFSID[all_entries$IsConstructor])) {
    constructor_idx <- which(all_entries$DFSID == constructor_id &
                               all_entries$IsConstructor)
    if (length(constructor_idx) == 0)
      next
    
    constructor_name <- all_entries$Name[constructor_idx][1]
    team_drivers <- which(all_entries$Team == constructor_name &
                            !all_entries$IsConstructor)
    
    if (length(team_drivers) >= 2) {
      # For each pair of drivers from this team
      driver_combos <- combn(team_drivers, 2)
      n_team_constraints <- n_team_constraints + ncol(driver_combos)
    }
  }
  
  # Add exclusion constraints
  n_exclusion_constraints <- 0
  if (!is.null(exclusion_constraints)) {
    n_exclusion_constraints <- length(exclusion_constraints)
  }
  
  # Total number of constraints
  total_constraints <- n_constraints + n_driver_constraints + n_team_constraints + n_exclusion_constraints
  
  # Create constraint matrix
  const.mat <- matrix(0, nrow = total_constraints, ncol = n_entries)
  const.dir <- character(total_constraints)
  const.rhs <- numeric(total_constraints)
  
  # Basic constraints
  # 1. Salary cap
  const.mat[1, driver_indices] <- all_entries$DriverSalary[driver_indices]
  const.mat[1, captain_indices] <- all_entries$CaptainSalary[captain_indices]
  const.mat[1, constructor_indices] <- all_entries$Salary[constructor_indices]
  const.dir[1] <- "<="
  const.rhs[1] <- salary_cap
  
  # 2. Exactly 6 selections (4 drivers + 1 captain + 1 constructor)
  const.mat[2, ] <- 1
  const.dir[2] <- "=="
  const.rhs[2] <- 6
  
  # 3. Exactly 1 captain
  const.mat[3, captain_indices] <- 1
  const.dir[3] <- "=="
  const.rhs[3] <- 1
  
  # 4. Exactly 1 constructor
  const.mat[4, constructor_indices] <- 1
  const.dir[4] <- "=="
  const.rhs[4] <- 1
  
  # 5. Exactly 4 regular drivers
  const.mat[5, driver_indices] <- 1
  const.dir[5] <- "=="
  const.rhs[5] <- 4
  
  # Current constraint index
  curr_row <- 6
  
  # Add driver-captain exclusivity constraints
  for (id in driver_ids) {
    driver_idx <- which(all_entries$DFSID == id &
                          all_entries$Type == "Driver")
    captain_idx <- which(all_entries$DFSID == id &
                           all_entries$Type == "Captain")
    
    if (length(driver_idx) > 0 && length(captain_idx) > 0) {
      const.mat[curr_row, c(driver_idx, captain_idx)] <- 1
      const.dir[curr_row] <- "<="
      const.rhs[curr_row] <- 1
      
      curr_row <- curr_row + 1
    }
  }
  
  # Add team constraints
  for (constructor_id in unique(all_entries$DFSID[all_entries$IsConstructor])) {
    constructor_idx <- which(all_entries$DFSID == constructor_id &
                               all_entries$IsConstructor)
    if (length(constructor_idx) == 0)
      next
    
    constructor_name <- all_entries$Name[constructor_idx][1]
    team_drivers <- which(all_entries$Team == constructor_name &
                            !all_entries$IsConstructor)
    
    if (length(team_drivers) >= 2) {
      # For each pair of drivers from this team
      driver_combos <- combn(team_drivers, 2)
      
      for (c in 1:ncol(driver_combos)) {
        driver1 <- driver_combos[1, c]
        driver2 <- driver_combos[2, c]
        
        # Can't have both drivers and constructor (driver1 + driver2 + constructor <= 2)
        const.mat[curr_row, c(driver1, driver2, constructor_idx)] <- 1
        const.dir[curr_row] <- "<="
        const.rhs[curr_row] <- 2
        
        curr_row <- curr_row + 1
      }
    }
  }
  
  # Add exclusion constraints
  if (!is.null(exclusion_constraints)) {
    for (i in 1:length(exclusion_constraints)) {
      if (length(exclusion_constraints[[i]]) != n_entries) {
        # Size mismatch - need to map constraint to current entries
        if (length(exclusion_constraints[[i]]) == nrow(original_entries)) {
          # Create new constraint vector
          new_constraint <- numeric(n_entries)
          
          for (j in 1:n_entries) {
            # Find this entry in the original entries
            curr_entry <- all_entries[j,]
            orig_idx <- which(
              original_entries$DFSID == curr_entry$DFSID & 
                original_entries$Type == curr_entry$Type
            )
            
            if (length(orig_idx) > 0) {
              new_constraint[j] <- exclusion_constraints[[i]][orig_idx[1]]
            }
          }
          
          # Use the mapped constraint
          const.mat[curr_row, ] <- new_constraint
        } else {
          # Can't properly map, use a default
          const.mat[curr_row, ] <- 1
        }
      } else {
        # Sizes match, use as is
        const.mat[curr_row, ] <- exclusion_constraints[[i]]
      }
      
      const.dir[curr_row] <- "<="
      const.rhs[curr_row] <- 5  # At most 5 elements from previous lineup
      
      curr_row <- curr_row + 1
    }
  }
  
  # Use lpSolve with a shorter timeout
  tryCatch({
    result <- lpSolve::lp("max", obj, const.mat, const.dir, const.rhs, all.bin = TRUE, timeout = 2)
    
    # Check if solution found
    if (result$status != 0) {
      return(NULL)  # No valid solution
    }
    
    # Extract solution
    selected_indices <- which(result$solution > 0.5)
    selected <- all_entries[selected_indices, ]
    
    # Verify lineup structure
    driver_indices <- which(selected$Type == "Driver")
    captain_indices <- which(selected$Type == "Captain")
    constructor_indices <- which(selected$Type == "Constructor")
    
    if (length(driver_indices) != 4 ||
        length(captain_indices) != 1 ||
        length(constructor_indices) != 1) {
      return(NULL)  # Invalid lineup structure
    }
    
    # Create lineup object
    lineup <- list(
      sim_id = if ("SimID" %in% names(selected))
        selected$SimID[1]
      else
        NA,
      drivers = selected$Name[driver_indices],
      captain = selected$Name[captain_indices][1],
      constructor = selected$Name[constructor_indices][1],
      total_salary = sum(
        sum(selected$DriverSalary[driver_indices], na.rm = TRUE),
        sum(selected$CaptainSalary[captain_indices], na.rm = TRUE),
        sum(selected$Salary[constructor_indices], na.rm = TRUE)
      ),
      total_points = sum(selected$FantasyPoints),
      selection_vector = result$solution  # Store for exclusion constraints
    )
    
    return(lineup)
    
  }, error = function(e) {
    # Handle timeout or other errors quietly
    return(NULL)
  })
}

# Optimized function to find the optimal lineup
find_top_lineups <- function(sim_drivers,
                             sim_constructors,
                             n_top = 1, # Default to 1 now
                             salary_cap = 50000) {
  # Early constraint check - if we don't have enough data
  if (nrow(sim_drivers) < 5 || nrow(sim_constructors) < 1) {
    return(NULL)
  }
  
  # Convert to data.table for efficiency
  if (!is.data.table(sim_drivers)) {
    sim_drivers <- as.data.table(sim_drivers)
  }
  if (!is.data.table(sim_constructors)) {
    sim_constructors <- as.data.table(sim_constructors)
  }
  
  # Check for required columns
  required_driver_cols <- c("DFSID",
                            "Name",
                            "Team",
                            "DriverSalary",
                            "CaptainSalary",
                            "FantasyPoints")
  required_constructor_cols <- c("DFSID", "Name", "Salary", "FantasyPoints")
  
  for (col in required_driver_cols) {
    if (!(col %in% names(sim_drivers))) {
      stop(paste("Required column", col, "missing in driver data"))
    }
  }
  
  for (col in required_constructor_cols) {
    if (!(col %in% names(sim_constructors))) {
      stop(paste("Required column", col, "missing in constructor data"))
    }
  }
  
  # Check if any valid lineup is even possible
  min_driver_cost <- sum(sort(sim_drivers$DriverSalary)[1:4])  # 4 cheapest drivers
  min_captain_cost <- min(sim_drivers$CaptainSalary)           # Cheapest captain
  min_constructor_cost <- min(sim_constructors$Salary)         # Cheapest constructor
  
  min_possible_cost <- min_driver_cost + min_captain_cost + min_constructor_cost
  
  if (min_possible_cost > salary_cap) {
    return(NULL)  # No valid lineup possible under salary cap
  }
  
  
  
    # Linear programming approach for finding optimal lineup
  
  # Create entries for all possible choices
  # Regular drivers - with filtering
  reg_drivers <- copy(sim_drivers)
  
  # Calculate PPD for regular drivers
  reg_drivers[, PPD := FantasyPoints / DriverSalary]
  
  # Find salary thresholds (keep all cheap players, filter expensive ones)
  driver_salary_threshold <- quantile(reg_drivers$DriverSalary, 0.6, na.rm = TRUE)  # Top 40% most expensive
  
  # For expensive drivers, only keep if they're good scorers OR good value
  expensive_drivers <- reg_drivers[DriverSalary >= driver_salary_threshold]
  cheap_drivers <- reg_drivers[DriverSalary < driver_salary_threshold]
  
  if(nrow(expensive_drivers) > 0) {
    # For expensive drivers, keep top 6 by points OR top 4 by PPD
    top_points_expensive <- expensive_drivers[order(-FantasyPoints)][1:min(4, nrow(expensive_drivers))]$Name
    top_ppd_expensive <- expensive_drivers[order(-PPD)][1:min(4, nrow(expensive_drivers))]$Name
    keep_expensive <- unique(c(top_points_expensive, top_ppd_expensive))
    
    # Filter expensive drivers
    expensive_drivers_filtered <- expensive_drivers[Name %in% keep_expensive]
    
    # Combine cheap (all) + expensive (filtered)
    reg_drivers <- rbindlist(list(cheap_drivers, expensive_drivers_filtered))
  } else {
    # If no expensive drivers, keep all
    reg_drivers <- cheap_drivers
  }
  
  reg_drivers[, `:=`(Type = "Driver",
                     IsCaptain = FALSE,
                     IsConstructor = FALSE)]
  
  # Captain entries (with 1.5x points) - with filtering
  captains <- copy(sim_drivers)
  captains[, `:=`(FantasyPoints = FantasyPoints * 1.5)]  # Apply captain multiplier first
  
  # Calculate PPD for captains (using captain salary and multiplied points)
  captains[, PPD := FantasyPoints / CaptainSalary]
  
  # Find salary thresholds for captains
  captain_salary_threshold <- quantile(captains$CaptainSalary, 0.6, na.rm = TRUE)  # Top 40% most expensive
  
  # For expensive captains, only keep if they're good scorers OR good value
  expensive_captains <- captains[CaptainSalary >= captain_salary_threshold]
  cheap_captains <- captains[CaptainSalary < captain_salary_threshold]
  
  if(nrow(expensive_captains) > 0) {
    # For expensive captains, keep top 6 by points OR top 4 by PPD
    top_points_expensive_capt <- expensive_captains[order(-FantasyPoints)][1:min(4, nrow(expensive_captains))]$Name
    top_ppd_expensive_capt <- expensive_captains[order(-PPD)][1:min(4, nrow(expensive_captains))]$Name
    keep_expensive_capt <- unique(c(top_points_expensive_capt, top_ppd_expensive_capt))
    
    # Filter expensive captains
    expensive_captains_filtered <- expensive_captains[Name %in% keep_expensive_capt]
    
    # Combine cheap (all) + expensive (filtered)
    captains <- rbindlist(list(cheap_captains, expensive_captains_filtered))
  } else {
    # If no expensive captains, keep all
    captains <- cheap_captains
  }
  
  captains[, `:=`(
    Type = "Captain",
    IsCaptain = TRUE,
    IsConstructor = FALSE,
    DriverSalary = NA_real_  # Not used for captains
  )]
  
  # Constructor filtering (existing code)
  constructors <- copy(sim_constructors)
  
  # Calculate points per dollar for constructors
  constructors[, PPD := FantasyPoints / Salary]
  
  # Get top 4 by raw fantasy points
  top_4_points <- constructors[order(-FantasyPoints)][1:min(4, nrow(constructors))]$Name
  
  # Get top 3 by points per dollar
  top_3_ppd <- constructors[order(-PPD)][1:min(3, nrow(constructors))]$Name
  
  # Keep constructors that are in either top list
  keep_constructors <- unique(c(top_4_points, top_3_ppd))
  constructors <- constructors[Name %in% keep_constructors]
  
  # Add the required columns
  constructors[, `:=`(Type = "Constructor",
                      IsCaptain = FALSE,
                      IsConstructor = TRUE)]
  
  # Combine all entries
  all_entries <- rbindlist(list(reg_drivers, captains, constructors),
                           fill = TRUE,
                           use.names = TRUE)
  
  # For Top 1 only, we just need to find the optimal lineup
  result <- find_optimal_lineup_lp(all_entries, salary_cap)
  
  # Clean up memory
  rm(reg_drivers, captains, constructors, all_entries)
  gc(verbose = FALSE)
  
  # Return the result as a list for consistency with original function
  if (!is.null(result)) {
    return(list(result))
  } else {
    return(NULL)
  }
}

analyze_top_lineups <- function(driver_results,
                                constructor_results,
                                max_sims = NULL) {
  # Add a global time limit for safety
  start_time <- Sys.time()
  max_runtime <- 25 * 60  # 25 minutes max
  
  # Force garbage collection before starting
  gc(verbose = FALSE)
  
  # Get all simulation IDs
  sim_ids <- unique(driver_results$SimID)
  
  # Sample simulations if max_sims is specified
  if (!is.null(max_sims) && length(sim_ids) > max_sims) {
    sim_ids <- sample(sim_ids, max_sims)
  }
  
  # OPTIMIZATION 1: Use lists instead of growing vectors
  all_lineups <- vector("list", length(sim_ids))  # Pre-allocate
  lineup_keys <- vector("list", length(sim_ids))  # Pre-allocate
  
  # Get number of drivers for batch size calculation  
  n_drivers <- length(unique(driver_results$Name))
  
  # OPTIMIZATION 2: Larger batch sizes to reduce overhead
  batch_size <- min(500, max(100, ceiling(length(sim_ids) / 10)))
  n_batches <- ceiling(length(sim_ids) / batch_size)
  
  cat("Starting to analyze top lineups across",
      length(sim_ids),
      "simulations...\n")
  
  successful_sims <- 0
  total_unique_lineups <- 0
  
  for (batch in 1:n_batches) {
    batch_start_time <- Sys.time()
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, length(sim_ids))
    
    cat("Processing batch",
        batch,
        "of",
        n_batches,
        "(simulations",
        start_idx,
        "to",
        end_idx,
        ")\n")
    
    # OPTIMIZATION 3: Process batch of simulations at once
    batch_sims <- sim_ids[start_idx:end_idx]
    
    # Pre-filter data for this batch to reduce memory usage
    batch_drivers <- driver_results[SimID %in% batch_sims]
    batch_constructors <- constructor_results[SimID %in% batch_sims]
    
    # Process each simulation in this batch
    for (i in 1:length(batch_sims)) {
      # Check for timeout periodically
      if (i %% 50 == 0) {
        current_runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        if (current_runtime > max_runtime) {
          cat("Time limit reached. Stopping analysis.\n")
          break
        }
      }
      
      sim_id <- batch_sims[i]
      
      # Get data for this simulation using optimized filtering
      sim_drivers <- batch_drivers[SimID == sim_id]
      sim_constructors <- batch_constructors[SimID == sim_id]
      
      # Find top lineups for this simulation using your original function
      tryCatch({
        # OPTIMIZATION 4: Use shorter timeout but keep your original logic
        top_sim_lineups <- find_top_lineups(sim_drivers, sim_constructors, n_top = 1)
        
        if (!is.null(top_sim_lineups) && length(top_sim_lineups) > 0) {
          lineup <- top_sim_lineups[[1]]
          
          # Create a unique key for this lineup (same as your original)
          lineup_key <- paste(paste(sort(c(lineup$drivers, lineup$captain)), collapse = "|"),
                              lineup$constructor, sep = "||")
          
          # Store in pre-allocated lists
          idx <- start_idx + i - 1
          all_lineups[[idx]] <- lineup
          lineup_keys[[idx]] <- lineup_key
          successful_sims <- successful_sims + 1
        }
      }, error = function(e) {
        # Skip this simulation if error occurs (same as original)
      })
    }
    
    batch_end_time <- Sys.time()
    batch_duration <- as.numeric(difftime(batch_end_time, batch_start_time, units = "secs"))
    cat("Completed batch",
        batch,
        "in",
        round(batch_duration, 2),
        "seconds\n")
    
    # OPTIMIZATION 5: Aggressive garbage collection after each batch
    rm(batch_drivers, batch_constructors)
    gc(verbose = FALSE)
  }
  
  # OPTIMIZATION 6: More efficient lineup consolidation
  # Remove NULL entries
  valid_indices <- which(!sapply(all_lineups, is.null))
  all_lineups <- all_lineups[valid_indices]
  lineup_keys <- unlist(lineup_keys[valid_indices])
  
  # If no valid lineups found, return NULL
  if (length(lineup_keys) == 0) {
    return(NULL)
  }
  
  # OPTIMIZATION 7: Use table() for faster counting instead of manual loops
  lineup_counts <- table(lineup_keys)
  unique_keys <- names(lineup_counts)
  counts <- as.numeric(lineup_counts)
  
  # Create mapping from keys back to lineup objects
  key_to_lineup <- setNames(all_lineups, lineup_keys)
  
  # OPTIMIZATION 8: Sort once using order() instead of multiple sorts
  sort_order <- order(counts, decreasing = TRUE)
  sorted_keys <- unique_keys[sort_order]
  sorted_counts <- counts[sort_order]
  
  # OPTIMIZATION 9: Vectorized result creation
  results <- data.frame(
    OptimalCount = sorted_counts,
    stringsAsFactors = FALSE
  )
  
  # Pre-allocate result columns
  results$Captain <- character(nrow(results))
  results$Constructor <- character(nrow(results)) 
  results$TotalSalary <- numeric(nrow(results))
  
  # Pre-allocate driver columns
  for (j in 1:4) {
    results[[paste0("Driver", j)]] <- character(nrow(results))
  }
  
  # Fill in lineup details efficiently
  for (i in 1:length(sorted_keys)) {
    key <- sorted_keys[i]
    lineup <- key_to_lineup[[key]]
    
    if (!is.null(lineup)) {
      results$Captain[i] <- lineup$captain
      results$Constructor[i] <- lineup$constructor
      results$TotalSalary[i] <- lineup$total_salary
      
      # Fill in driver columns
      for (j in 1:min(length(lineup$drivers), 4)) {
        results[[paste0("Driver", j)]][i] <- lineup$drivers[j]
      }
    }
  }
  
  cat("Analysis completed. Found", length(unique_keys), "unique lineups from", successful_sims, "successful simulations.\n")
  
  return(list(lineups = results, total_processed = successful_sims))
}

# Analysis functions
analyze_f1_drivers <- function(driver_results) {
  # Check if required columns exist
  required_cols <- c(
    "Name",
    "Team",
    "DFSID",
    "Starting",
    "DriverSalary",
    "CaptainSalary",
    "FinishPosition",
    "FastestLap",
    "LapsLed",
    "FantasyPoints",
    "DefeatedTeammate",
    "IsClassified"
  )
  
  
  # Now calculate driver performance metrics including the new fields
  analysis <- driver_results[, .(
    Starting = first(Starting),
    Salary = first(DriverSalary),
    CptSal = first(CaptainSalary),
    Median_Finish = median(FinishPosition),
    DefeatedTeammate = mean(DefeatedTeammate) * 100,
    Win_Rate = mean(FinishPosition == 1) * 100,
    Podium_Rate = mean(FinishPosition <= 3) * 100,
    Points_Rate = mean(FinishPosition <= 10) * 100,
    ClassifiedRate = mean(IsClassified) * 100,
    FL_Rate = mean(FastestLap) * 100,
    Avg_LL = mean(LapsLed),
    Median_Proj = median(FantasyPoints)
  ), by = .(Name, Team, DFSID)]
  
  
  # Round numeric columns
  numeric_cols <- c(
    "Median_Finish",
    "Win_Rate",
    "Podium_Rate",
    "Points_Rate",
    "ClassifiedRate",
    "DefeatedTeammate",
    "FL_Rate",
    "Avg_LL",
    "Median_Proj"
  )
  
  analysis[, (numeric_cols) := lapply(.SD, round, 2), .SDcols = numeric_cols]
  
  
  # Sort by average fantasy points
  setorder(analysis, Median_Finish)
  
  return(analysis)
}

analyze_f1_constructors <- function(constructor_results) {
  # Calculate constructor performance metrics
  analysis <- constructor_results[, .(
    Salary = first(Salary),
    Avg_Pos_Pts = mean(PositionPoints),
    Avg_Bonus = mean(BonusPoints),
    Avg_LL = mean(LapsLedPoints),
    FL_Rate = mean(HasFastestLap) * 100,  # Add FL rate as percentage
    Avg_FLPoints = mean(FLPoints),        # Add average FL points
    ClassBonus = mean(BothFinished) * 100,
    PointsBonus = mean(BothInPoints) * 100,
    PodiumBonus = mean(BothOnPodium) * 100,
    Median_Proj = median(FantasyPoints)
  ), by = .(DFSID, Name)]
  
  # Round numeric columns
  numeric_cols <- c(
    "Avg_Pos_Pts", "Avg_Bonus", "Avg_LL", "FL_Rate", "Avg_FLPoints",
    "ClassBonus", "PointsBonus", "PodiumBonus",
    "Median_Proj"
  )
  
  analysis[, (numeric_cols) := lapply(.SD, round, 2), .SDcols = numeric_cols]
  
  # Sort by average fantasy points
  setorder(analysis, -Median_Proj)
  
  return(analysis)
}



analyze_top_lineups <- function(driver_results,
                                constructor_results,
                                max_sims = NULL) {
  # Get all simulation IDs
  sim_ids <- unique(driver_results$SimID)
  
  # Sample simulations if max_sims is specified
  if (!is.null(max_sims) && length(sim_ids) > max_sims) {
    sim_ids <- sample(sim_ids, max_sims)
  }
  
  # Initialize storage
  all_lineups <- list()
  lineup_keys <- character()
  lineup_top1_counts <- integer()

  
  # Get number of drivers for batch size calculation
  n_drivers <- length(unique(driver_results$Name))
  
  # Process simulations in batches for better memory management
  batch_size <- min(200, max(50, ceiling(5000 / max(n_drivers, 1))))
  n_batches <- ceiling(length(sim_ids) / batch_size)
  
  cat("Starting to analyze top lineups across",
      length(sim_ids),
      "simulations...\n")
  
  for (batch in 1:n_batches) {
    batch_start_time <- Sys.time()
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, length(sim_ids))
    batch_sims <- end_idx - start_idx + 1
    
    cat(
      "Processing batch",
      batch,
      "of",
      n_batches,
      "(simulations",
      start_idx,
      "to",
      end_idx,
      ")\n"
    )
    
    # Process each simulation in this batch
    for (sim_offset in 1:batch_sims) {
      sim_id <- start_idx + sim_offset - 1
      
      # Get data for this simulation
      sim_drivers <- driver_results[SimID == sim_id]
      sim_constructors <- constructor_results[SimID == sim_id]
      
      # Find top lineups for this simulation
      tryCatch({
        top_sim_lineups <- find_top_lineups(sim_drivers, sim_constructors, n_top = 3)
        
        if (!is.null(top_sim_lineups) &&
            length(top_sim_lineups) > 0) {
          
          for (i in 1:length(top_sim_lineups)) {
            lineup <- top_sim_lineups[[i]]
            rank <- i  # This will be 1 for top lineup, 2 for second, etc.
            
            # Create a unique key for this lineup
            lineup_key <- paste(paste(sort(
              c(lineup$drivers, lineup$captain)
            ), collapse = "|"),
            lineup$constructor,
            sep = "||")
            
            # Check if we already have this lineup
            key_match <- match(lineup_key, lineup_keys)
            
            if (!is.na(key_match)) {
              # Lineup already exists, increment appropriate counts
              if (rank == 1) lineup_top1_counts[key_match] <- lineup_top1_counts[key_match] + 1
            } else {
              # New unique lineup
              lineup_keys <- c(lineup_keys, lineup_key)
              
              # Initialize counts
              lineup_top1_counts <- c(lineup_top1_counts, if(rank == 1) 1 else 0)
           
              
              all_lineups[[length(lineup_keys)]] <- lineup
            }
          }
        }
      }, error = function(e) {
        # Skip this simulation if error occurs
        cat("Error processing simulation",
            sim_id,
            ":",
            e$message,
            "\n")
      })
    }
    
    batch_end_time <- Sys.time()
    batch_duration <- as.numeric(difftime(batch_end_time, batch_start_time, units = "secs"))
    cat("Completed batch",
        batch,
        "in",
        round(batch_duration, 2),
        "seconds\n")
    
    # Free memory after each batch
    gc(verbose = FALSE)
  }
  
  # If no valid lineups found, return NULL
  if (length(lineup_keys) == 0) {
    return(NULL)
  }
  
  # Create data frame of results sorted by Top1 Count then Top3 Count
  lineup_order <- order(lineup_top1_counts, decreasing = TRUE)
  top_lineup_keys <- lineup_keys[lineup_order]
  top_lineup_top1_counts <- lineup_top1_counts[lineup_order]
  
  
  # Create results data frame
  results <- data.frame(
    OptimalCount = top_lineup_top1_counts,
    stringsAsFactors = FALSE
  )
  
  # Maximum number of drivers across all lineups
  max_drivers <- max(sapply(all_lineups, function(x)
    length(x$drivers)))
  
  # Add columns for drivers, captain, constructor
  results$Captain <- character(nrow(results))
  results$Constructor <- character(nrow(results))
  results$TotalSalary <- numeric(nrow(results))
  
  for (j in 1:max_drivers) {
    results[[paste0("Driver", j)]] <- character(nrow(results))
  }
  
  # Fill in lineup details
  for (i in 1:length(top_lineup_keys)) {
    key <- top_lineup_keys[i]
    lineup_idx <- match(key, lineup_keys)
    
    if (!is.na(lineup_idx)) {
      lineup <- all_lineups[[lineup_idx]]
      
      results$Captain[i] <- lineup$captain
      results$Constructor[i] <- lineup$constructor
      results$TotalSalary[i] <- lineup$total_salary
      
      # Fill in driver columns
      for (j in 1:length(lineup$drivers)) {
        if (j <= max_drivers) {
          results[[paste0("Driver", j)]][i] <- lineup$drivers[j]
        }
      }
    }
  }
  
  return(list(lineups = results, total_processed = length(sim_ids)))
}




calculate_filtered_pool_stats <- function(optimal_lineups, filters) {
  if (is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  filtered_lineups <- optimal_lineups
  
  # Convert to data.table for faster operations
  if (!is.data.table(filtered_lineups)) {
    filtered_dt <- as.data.table(filtered_lineups)
  } else {
    filtered_dt <- copy(filtered_lineups)
  }
  
  # Apply OptimalCount filter
  if (!is.null(filters$min_top1_count) && filters$min_top1_count > 0) {
    if ("OptimalCount" %in% names(filtered_dt)) {
      filtered_dt <- filtered_dt[OptimalCount >= filters$min_top1_count]
    }
  }
  
  
  # Apply driver exclusion filter
  if (!is.null(filters$excluded_drivers) && length(filters$excluded_drivers) > 0) {
    # Get all driver columns
    driver_cols <- grep("^Driver", names(filtered_dt), value = TRUE)
    
    # For each driver column, exclude rows with excluded drivers
    for (col in driver_cols) {
      filtered_dt <- filtered_dt[!(filtered_dt[[col]] %in% filters$excluded_drivers)]
    }
  }
  
  # Apply captain exclusion filter
  if (!is.null(filters$excluded_captains) && length(filters$excluded_captains) > 0) {
    if ("Captain" %in% names(filtered_dt)) {
      filtered_dt <- filtered_dt[!(Captain %in% filters$excluded_captains)]
    }
  }
  
  # Apply constructor exclusion filter
  if (!is.null(filters$excluded_constructors) && length(filters$excluded_constructors) > 0) {
    if ("Constructor" %in% names(filtered_dt)) {
      filtered_dt <- filtered_dt[!(Constructor %in% filters$excluded_constructors)]
    }
  }
  
  # Return early if no lineups match the filters
  if (nrow(filtered_dt) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  # Calculate thresholds for display
  thresholds <- list()
  
  # Add Count thresholds based on available columns
  for (col in c("OptimalCount")) {
    if (col %in% names(filtered_dt)) {
      prefix <- gsub("Count", "", col)
      min_name <- paste0("min_", tolower(prefix), "count")
      max_name <- paste0("max_", tolower(prefix), "count")
      
      thresholds[[min_name]] <- min(filtered_dt[[col]], na.rm = TRUE)
      thresholds[[max_name]] <- max(filtered_dt[[col]], na.rm = TRUE)
    }
  }
  
  return(list(count = nrow(filtered_dt), thresholds = thresholds))
}

generate_random_lineups <- function(optimal_lineups, filters) {
  # Convert to data.table for faster operations
  if (!is.data.table(optimal_lineups)) {
    filtered_dt <- as.data.table(optimal_lineups)
  } else {
    filtered_dt <- copy(optimal_lineups)
  }
  
  # Apply OptimalCount filter
  if (!is.null(filters$min_top1_count) && filters$min_top1_count > 0) {
    if ("OptimalCount" %in% names(filtered_dt)) {
      filtered_dt <- filtered_dt[OptimalCount >= filters$min_top1_count]
    }
  }
  
  
  # More efficient driver exclusion filter
  if (!is.null(filters$excluded_drivers) && length(filters$excluded_drivers) > 0) {
    # Get all driver columns
    driver_cols <- grep("^Driver", names(filtered_dt), value = TRUE)
    
    # Filter rows where any driver is in the exclusion list
    for (col in driver_cols) {
      filtered_dt <- filtered_dt[!(filtered_dt[[col]] %in% filters$excluded_drivers)]
    }
  }
  
  # Apply captain exclusion filter
  if (!is.null(filters$excluded_captains) && length(filters$excluded_captains) > 0) {
    if ("Captain" %in% names(filtered_dt)) {
      filtered_dt <- filtered_dt[!(Captain %in% filters$excluded_captains)]
    }
  }
  
  # Apply constructor exclusion filter
  if (!is.null(filters$excluded_constructors) && length(filters$excluded_constructors) > 0) {
    if ("Constructor" %in% names(filtered_dt)) {
      filtered_dt <- filtered_dt[!(Constructor %in% filters$excluded_constructors)]
    }
  }
  
  # Return early if no lineups match the filters
  if (nrow(filtered_dt) == 0) {
    return(NULL)
  }
  
  # Setup for lineup generation
  n_lineups <- filters$num_lineups
  
  # Handle case where we have fewer lineups than requested
  if (nrow(filtered_dt) <= n_lineups) {
    return(filtered_dt)
  }
  
  # Extract weights for sampling - use OptimalCount as weight 
  weights <- filtered_dt$OptimalCount
  
  # Sample with weights
  selected_indices <- sample(1:nrow(filtered_dt), n_lineups, replace = FALSE, prob = weights)
  
  # Extract selected lineups
  selected_lineups <- filtered_dt[selected_indices]
  
  return(selected_lineups)
}

# Function to calculate driver exposure for the lineup builder
calculate_driver_exposure <- function(optimal_lineups,
                                      driver_results,
                                      random_lineups) {
  # Convert inputs to data.table for efficiency
  if (!is.data.table(optimal_lineups))
    setDT(optimal_lineups)
  if (!is.data.table(driver_results))
    setDT(driver_results)
  if (!is.null(random_lineups) &&
      !is.data.table(random_lineups))
    setDT(random_lineups)
  
  # Guard against NULL input
  if (is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame(Message = "No optimal lineups available."))
  }
  
  # Get all driver columns
  driver_cols <- c(grep("^Driver", names(optimal_lineups), value = TRUE), "Captain")
  
  # Extract all unique drivers more efficiently
  all_drivers <- unique(unlist(optimal_lineups[, driver_cols, with = FALSE]))
  all_drivers <- all_drivers[!is.na(all_drivers)]
  
  # Pre-allocate result data frame
  exposure_data <- data.frame(
    Driver = all_drivers,
    Optimal = 0,
    Exposure = 0,
    stringsAsFactors = FALSE
  )
  
  # Calculate Optimal frequency in one pass
  if ("Count" %in% names(optimal_lineups)) {
    total_count <- sum(optimal_lineups$Count, na.rm = TRUE)
    
    for (i in seq_along(all_drivers)) {
      driver <- all_drivers[i]
      
      # Find lineups containing this driver
      driver_lineups <- optimal_lineups[sapply(1:nrow(optimal_lineups), function(row) {
        any(unlist(optimal_lineups[row, driver_cols, with = FALSE]) == driver)
      })]
      
      if (nrow(driver_lineups) > 0) {
        # Calculate percentage
        driver_count <- sum(driver_lineups$Count, na.rm = TRUE)
        exposure_data$Optimal[i] <- (driver_count / total_count) * 100
      }
    }
  }
  
  # Calculate Exposure from random lineups if available
  if (!is.null(random_lineups) && nrow(random_lineups) > 0) {
    # Get driver columns from random lineups
    random_driver_cols <- c(grep("^Driver", names(random_lineups), value = TRUE), "Captain")
    
    if (length(random_driver_cols) > 0) {
      for (i in seq_along(all_drivers)) {
        driver <- all_drivers[i]
        
        # Count lineups containing this driver
        count <- sum(sapply(1:nrow(random_lineups), function(row) {
          driver_values <- unlist(lapply(random_driver_cols, function(col)
            random_lineups[row, col, with = FALSE]))
          any(driver_values == driver)
        }))
        
        exposure_data$Exposure[i] <- (count / nrow(random_lineups)) * 100
      }
    }
  }
  
  # Add driver data if available
  if (!is.null(driver_results) && nrow(driver_results) > 0) {
    # Find driver identification column
    id_col <- "Name"  # Default
    if (!id_col %in% names(driver_results)) {
      # Try to find another suitable column
      for (col in c("Driver", "DriverName")) {
        if (col %in% names(driver_results)) {
          id_col <- col
          break
        }
      }
    }
    
    # If we have a valid ID column, add driver data
    if (id_col %in% names(driver_results)) {
      # Create efficient summary data
      driver_summary <- driver_results[, .(
        DriverSalary = first(DriverSalary),
        CaptainSalary = first(CaptainSalary),
        Starting = first(Starting),
        Proj = mean(FantasyPoints, na.rm = TRUE)
      ), by = eval(id_col)]
      
      # Convert to data.frame for compatibility
      driver_summary <- as.data.frame(driver_summary)
      names(driver_summary)[1] <- "Driver"
      
      # Merge data
      exposure_data <- merge(exposure_data,
                             driver_summary,
                             by = "Driver",
                             all.x = TRUE)
      
      # Fill NA values with 0
      for (col in names(exposure_data)) {
        if (any(is.na(exposure_data[[col]]))) {
          exposure_data[[col]][is.na(exposure_data[[col]])] <- 0
        }
      }
    }
  }
  
  # Sort by Optimal (descending)
  exposure_data <- exposure_data[order(-exposure_data$Optimal), ]
  
  return(exposure_data)
}



# UI Definition
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "F1 Fantasy Simulator"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Input Check", tabName = "upload", icon = icon("upload")),
      menuItem("Driver Analysis", tabName = "driver_analysis", icon = icon("car")),
      menuItem( "Constructor Analysis", tabName = "constructor_analysis",icon = icon("university")),
      menuItem("Optimal Lineups", tabName = "optimal_lineups", icon = icon("trophy")),
      menuItem("Lineup Selection", tabName = "lineup_builder",icon = icon("users"))),
    
    # Input controls
    br(),
    fileInput("excel_file", "Upload Excel File", accept = c(".xlsx")),
    numericInput(
      "n_sims",
      "Number of Simulations:",
      value = 15000,
      min = 100,
      max = 25000
    ),
    numericInput(
      "total_laps",
      "Race Laps:",
      value = 78
    ),
    actionButton(
      "run_sim",
      "Run Simulation",
      class = "btn-primary",
      style = "margin: 15px;"
    ),
    textOutput("upload_status")
  ),
  
  dashboardBody(
    tabItems(
      # Upload Tab
      tabItem(tabName = "upload", box(
        width = 12, title = "Input Data", DTOutput("data_preview")
      )),
      
      
      # Driver Analysis Tab
      tabItem(tabName = "driver_analysis", fluidRow(
        box(width = 12, title = "Driver Performance Analysis", 
            DTOutput("driver_stats"),
            br(),
            downloadButton("downloadDriverAnalysis", "Download Driver Analysis", class = "btn-success")
        )
      ), fluidRow(
        box(
          width = 12,
          title = "Position Distribution",
          plotlyOutput("position_dist", height = "600px")
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "Fantasy Points Distribution",
          plotlyOutput("fp_dist", height = "600px")
        )
      )
      ),
      
      # Constructor Analysis Tab
      tabItem(tabName = "constructor_analysis", fluidRow(
        box(
          width = 12,
          title = "Constructor Performance Analysis",
          DTOutput("constructor_stats"),
          br(),
          downloadButton("downloadConstructorAnalysis", "Download Constructor Analysis", class = "btn-success")
        )
      ), fluidRow(
        box(
          width = 12,
          title = "Constructor Points Distribution",
          plotlyOutput("constructor_dist")
        )
      )),
      
      
      # Optimal Lineups Tab
      tabItem(
        tabName = "optimal_lineups",
        fluidRow(
          box(
            width = 12,
            title = "Optimal Lineups",
            DTOutput("optimal_lineup_frequency"),
            br(),
            downloadButton("downloadOptimalFrequency", "Download Optimal Lineups", class = "btn-success")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Driver Frequency",
            plotlyOutput("driver_freq_plot")
          ),
          box(
            width = 6,
            title = "Constructor Frequency",
            plotlyOutput("constructor_freq_plot")
          )
        )
      ),
      
      # Lineup Builder Tab
      tabItem(
        tabName = "lineup_builder",
        fluidRow(
          box(width = 12,
              title = "Lineup Count Thresholds",
              DTOutput("lineup_count_thresholds")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Lineup Filters",
            status = "primary",
            solidHeader = TRUE,
            fluidRow(
              # Left column - Min counts
              column(
                width = 4,
                h4("Filter By Optimal Count"),
                numericInput("min_top1_count", "Min Optimal Count:", value = 0, min = 0)
              ),
              # Middle column - Exclusions
              column(
                width = 4,
                h4("Exclusion Filters"),
                selectizeInput(
                  "excluded_drivers",
                  "Exclude Drivers:",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(plugins = list("remove_button"))
                ),
                selectizeInput(
                  "excluded_captains",
                  "Exclude as Captains:",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(plugins = list("remove_button"))
                ),
                selectizeInput(
                  "excluded_constructors",
                  "Exclude Constructors:",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(plugins = list("remove_button"))
                )
              ),
              # Right column - Lineup selection
              column(
                width = 4,
                h4("Lineup Selection"),
                numericInput(
                  "num_random_lineups",
                  "Number of Lineups to Pick:",
                  value = 20,
                  min = 1
                ),
                div(
                  class = "well well-sm",
                  style = "margin-top: 15px;",
                  h4("Filtered Pool:"),
                  textOutput("filtered_pool_size")
                ),
                div(
                  style = "margin-top: 20px;",
                  actionButton(
                    "generate_lineups",
                    "Randomly Select Lineups",
                    class = "btn-primary",
                    style = "width: 100%; margin-bottom: 10px;"
                  ),
                  downloadButton(
                    "download_random_lineups",
                    "Download Selected Lineups",
                    class = "btn-success",
                    style = "width: 100%;"
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Driver Exposure Analysis",
            DTOutput("driver_exposure_table")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Constructor Exposure Analysis",
            DTOutput("constructor_exposure_table")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Randomly Selected Lineups",
            DTOutput("random_lineups_table")
          )
        )
      )
    )
  )
)


# Server Logic
server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    input_data = NULL,
    simulation_results = NULL,
    driver_analysis = NULL,
    constructor_analysis = NULL,
    file_uploaded = FALSE,
    simulation_run = FALSE
  )
  
  # File upload handling
  observeEvent(input$excel_file, {
    req(input$excel_file)
    
    tryCatch({
      rv$input_data <- read_f1_input_file(input$excel_file$datapath)
      rv$file_uploaded <- TRUE
      rv$simulation_run <- FALSE
      
      output$data_preview <- renderDT({
        # Get the driver data
        driver_data <- as.data.frame(rv$input_data$drivers)
        
        # Round all numeric columns to 2 decimal places
        numeric_cols <- sapply(driver_data, is.numeric)
        driver_data[, numeric_cols] <- round(driver_data[, numeric_cols], 2)
        
        # Create the datatable with all rows visible
        datatable(
          driver_data,
          options = list(
            scrollX = TRUE,
            paging = FALSE,
            # Disable pagination
            searching = FALSE,
            # Optional: disable search
            info = FALSE        # Optional: hide "Showing X of Y entries" text
          )
        )
      })
      
      updateTabItems(session, "sidebar", selected = "upload")
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Error reading file:", e$message),
        easyClose = TRUE
      ))
      rv$file_uploaded <- FALSE
    })
  })
  
  # Upload status
  output$upload_status <- renderText({
    if (is.null(input$excel_file)) {
      return("Please upload an Excel file")
    } else if (!rv$simulation_run) {
      return("File uploaded! Click 'Run Simulation' to begin")
    } else {
      return("Simulation complete!")
    }
  })
  
  # Run simulation
  observeEvent(input$run_sim, {
    req(rv$input_data)
    
    if (!rv$file_uploaded) {
      showModal(modalDialog(
        title = "Warning",
        "Please upload a file first",
        easyClose = TRUE
      ))
      return()
    }
    
    # Clear previous simulation results from memory
    rv$simulation_results <- NULL
    rv$driver_analysis <- NULL
    rv$constructor_analysis <- NULL
    rv$optimal_lineups <- NULL 
    rv$driver_exposure <- NULL
    rv$random_lineups <- NULL
    
    # Force aggressive garbage collection
    gc(verbose = FALSE, full = TRUE)
    
    withProgress(message = 'Running simulations...', value = 0, {
      # Run simulations
      incProgress(0.2, detail = "Simulating races")
      sim_results <- simulate_f1_races(
        rv$input_data$drivers,
        rv$input_data$constructors,
        rv$input_data$fl_probs,
        rv$input_data$ll_data,
        rv$input_data$classification_data,
        input$total_laps,
        input$n_sims
      )
      
      # Store results
      rv$simulation_results <- sim_results
      
      # Perform analysis
      incProgress(0.6, detail = "Analyzing results")
      rv$driver_analysis <- analyze_f1_drivers(sim_results$driver_results)
      rv$constructor_analysis <- analyze_f1_constructors(sim_results$constructor_results)
      
      # Set flag
      rv$simulation_run <- TRUE
      
      # Update UI
      updateTabItems(session, "sidebar", selected = "driver_analysis")
      
      # Update dropdown options for lineup builder
      updateSelectizeInput(session,
                           "selected_drivers",
                           choices = setNames(
                             rv$input_data$drivers$Name,
                             paste0(
                               rv$input_data$drivers$Name,
                               " ($",
                               rv$input_data$drivers$DriverSal,
                               ")"
                             )
                           ))
      
      updateSelectizeInput(session,
                           "captain_driver",
                           choices = setNames(
                             rv$input_data$drivers$Name,
                             paste0(
                               rv$input_data$drivers$Name,
                               " ($",
                               rv$input_data$drivers$CptSal,
                               ")"
                             )
                           ))
      
      updateSelectizeInput(
        session,
        "selected_constructor",
        choices = setNames(
          rv$input_data$constructors$Name,
          paste0(
            rv$input_data$constructors$Name,
            " ($",
            rv$input_data$constructors$Salary,
            ")"
          )
        )
      )
    })
  })
  
  
  ### Driver Analysis Tab
  
  
  # Analysis outputs
  output$driver_stats <- renderDT({
    req(rv$driver_analysis)
    
    # Create a copy of the data to modify
    display_data <- copy(rv$driver_analysis)
    
    # Remove unwanted columns
    display_data[, DFSID := NULL]
    
    round_cols <- c(
      "Median_Finish",
      "DefeatedTeammate",
      "Win_Rate",
      "Podium_Rate",
      "Points_Rate",
      "ClassifiedRate",
      "FL_Rate",
      "Avg_LL",
      "Median_Proj"
    )
    
    # Round specified columns to 1 decimal place
    display_data[, (round_cols) := lapply(.SD, round, 1), .SDcols = round_cols]
    
    # Reorder columns
    col_order <- c(
      "Name",
      "Team",
      "Starting",
      "Salary",
      "CptSal",
      "Median_Finish",
      "DefeatedTeammate",
      "Win_Rate",
      "Podium_Rate",
      "Points_Rate",
      "ClassifiedRate",
      "FL_Rate",
      "Avg_LL",
      "Median_Proj"
    )
    
    # Only select columns that actually exist in the data
    col_order <- intersect(col_order, names(display_data))
    
    # Select columns using the correct data.table syntax
    display_data <- display_data[, ..col_order]
    
    # Create datatable with specified options
    datatable(
      display_data,
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        paging = FALSE,
        searching = FALSE,
        info = FALSE
      )
    ) %>%
      formatCurrency(c('Salary', 'CptSal'), '$', digits = 0)
  })
  
  output$position_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Extract the raw finishing positions
    driver_finishes <- rv$simulation_results$driver_results
    
    # Create box plot
    p <- ggplot(driver_finishes,
                aes(
                  x = reorder(Name, FinishPosition, median),
                  y = FinishPosition,
                  fill = Name
                )) +
      geom_boxplot(alpha = 0.7) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Driver", y = "Finishing Position", title = "Finishing Position Distribution") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  
  output$fp_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Extract the driver results data
    driver_fp <- rv$simulation_results$driver_results
    
    # Get unique driver salaries for sorting
    driver_salaries <- driver_fp[, .(Salary = first(DriverSalary)), by = Name]
    
    # Create box plot with sorting by salary
    p <- ggplot(driver_fp,
                aes(
                  x = factor(Name, levels = driver_salaries[order(Salary)]$Name),
                  y = FantasyPoints,
                  fill = Name
                )) +
      geom_boxplot(alpha = 0.7) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Driver", y = "Fantasy Points", title = "Fantasy Points Distribution") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  
  ### Constructor Analysis Tab
  
  output$constructor_stats <- renderDT({
    req(rv$constructor_analysis)
    
    # Create a copy of the data to modify
    display_data <- copy(rv$constructor_analysis)
    
    # Remove unwanted columns
    display_data[, DFSID := NULL]
    
    round_cols <- c(
      "Avg_Pos_Pts",
      "Avg_Bonus",
      "Avg_LL",
      "ClassBonus",
      "PointsBonus",
      "PodiumBonus",
      "Median_Proj"
    )
    
    
    
    # Round specified columns to 1 decimal place
    display_data[, (round_cols) := lapply(.SD, round, 1), .SDcols = round_cols]
    
    # Reorder columns
    col_order <- c(
      "Name",
      "Salary",
      "Avg_Pos_Pts",
      "Avg_Bonus",
      "Avg_LL",
      "FL_Rate",
      "ClassBonus",
      "PointsBonus",
      "PodiumBonus",
      "Median_Proj"
    )
    
    # Only select columns that actually exist in the data
    col_order <- intersect(col_order, names(display_data))
    
    # Select columns using the correct data.table syntax
    display_data <- display_data[, ..col_order]
    
    # Create datatable with specified options
    datatable(
      display_data,
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        paging = FALSE,
        searching = FALSE,
        info = FALSE
      )
    ) %>%
      formatCurrency(c('Salary'), '$', digits = 0)
  })
  
  
  
  # Constructor points distribution visualization
  output$constructor_dist <- renderPlotly({
    req(rv$simulation_results)
    
    constructor_points <- rv$simulation_results$constructor_results
    
    
    # Create box plot
    p <- ggplot(constructor_points,
                aes(
                  x = reorder(Name, FantasyPoints, median),
                  y = FantasyPoints,
                  fill = Name
                )) +
      geom_boxplot(alpha = 0.7) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Constructor", y = "Fantasy Points", title = "Fantasy Points Distribution") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  
  
  ### Optimal Lineups tab
  
  # After running simulations, analyze optimal lineups
  observeEvent(rv$simulation_results, {
    req(
      rv$simulation_results$driver_results,
      rv$simulation_results$constructor_results
    )
    
    # Only run if not already processed
    if (is.null(rv$simulation_results$optimal_lineup_frequency) ||
        !is.data.frame(rv$simulation_results$optimal_lineup_frequency) ||
        nrow(rv$simulation_results$optimal_lineup_frequency) == 0) {
      # Show modal dialog
      showModal(
        modalDialog(
          title = "Processing Optimal Lineups",
          "Analyzing optimal lineups across simulations...",
          footer = NULL,
          easyClose = FALSE
        )
      )
      
      # Run lineup analysis in the background
      try({
        withProgress(message = 'Analyzing optimal lineups...', value = 0, {
          incProgress(0.1, detail = paste("Processing simulations"))
          
          lineup_analysis <- analyze_top_lineups(
            rv$simulation_results$driver_results,
            rv$simulation_results$constructor_results
          )
          
          incProgress(0.9, detail = "Finishing analysis")
          
          # Update results
          if (!is.null(lineup_analysis) &&
              is.data.frame(lineup_analysis$lineups)) {
            rv$simulation_results$optimal_lineup_frequency <- lineup_analysis$lineups
            rv$simulation_results$optimal_lineup_details <- lineup_analysis
          }
        })
      })
      
      # Remove the modal
      removeModal()
    }
  })
  
  
  
  # Optimal lineup frequency table
  output$optimal_lineup_frequency <- renderDT({
    req(rv$simulation_results$optimal_lineup_frequency)
    
    # Make sure we're working with a data.frame, not a data.table
    lineup_data <- rv$simulation_results$optimal_lineup_frequency
    if (is.data.table(lineup_data)) {
      lineup_data <- as.data.frame(lineup_data)
    }
    
    # Make sure we have the needed columns for F1 lineups (4 regular drivers)
    for (i in 1:4) {
      driver_col <- paste0("Driver", i)
      if (!(driver_col %in% names(lineup_data))) {
        lineup_data[[driver_col]] <- NA
      }
    }
    
    # Define the column order to display
    keep_cols <- c("Captain", "Driver1", "Driver2", "Driver3", "Driver4", 
                   "Constructor", "TotalSalary", "OptimalCount")
    
    # Only keep columns that exist in the data
    keep_cols <- intersect(keep_cols, names(lineup_data))
    
    # Create datatable with the columns in the right order
    dt <- datatable(
      lineup_data[, keep_cols],
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        searching = FALSE,
        order = list(list(7, 'desc')),  # Using numeric indices instead of column names
        columnDefs = list(list(
          className = 'dt-center', targets = "_all"
        )),
        dom = 'tp'
      ),
      rownames = FALSE,
      selection = 'none'
    )
    
    # Format TotalSalary column if it exists
    if ("TotalSalary" %in% keep_cols) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", digits = 0)
    }
    
    # Format count columns with color bars if they exist
    count_cols <- c("OptimalCount")
    for (col in count_cols) {
      if (col %in% keep_cols) {
        max_val <- max(lineup_data[[col]], na.rm = TRUE)
        if (!is.finite(max_val) || max_val == 0) max_val <- 1  # Prevent division by zero
        
        dt <- dt %>% formatStyle(
          col,
          background = styleColorBar(c(0, max_val), 'lightgreen'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      }
    }
    
    return(dt)
  })
  
  output$downloadOptimalFrequency <- downloadHandler(
    filename = function() {
      paste("f1_optimal_lineup_frequency_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      # Use tryCatch for error handling
      tryCatch({
        # Get the exact table data from optimal_lineup_frequency
        lineup_data <- rv$simulation_results$optimal_lineup_frequency
        
        # Clone the data
        download_data <- lineup_data
        
        # Get DFSID mappings
        driver_ids <- NULL
        constructor_ids <- NULL
        
        # Get driver IDs if available
        if (!is.null(rv$simulation_results$driver_results)) {
          driver_ids <- unique(rv$simulation_results$driver_results[, c("Name", "DFSID")])
        }
        
        # Get constructor IDs if available
        if (!is.null(rv$simulation_results$constructor_results)) {
          constructor_ids <- unique(rv$simulation_results$constructor_results[, c("Name", "DFSID")])
        }
        
        # Add DFSID to Captain - with +20 to driver DFSID
        if ("Captain" %in% names(download_data) && !is.null(driver_ids)) {
          for (i in 1:nrow(download_data)) {
            name <- download_data$Captain[i]
            if (!is.na(name)) {
              match_idx <- which(driver_ids$Name == name)
              if (length(match_idx) > 0) {
                driver_dfsid <- driver_ids$DFSID[match_idx[1]]
                captain_dfsid <- driver_dfsid + 20  # Add 20 to get captain DFSID
                download_data$Captain[i] <- paste0(name, " (", captain_dfsid, ")")
              }
            }
          }
        }
        
        # Add DFSID to Drivers
        driver_cols <- grep("^Driver", names(download_data), value = TRUE)
        if (length(driver_cols) > 0 && !is.null(driver_ids)) {
          for (col in driver_cols) {
            for (i in 1:nrow(download_data)) {
              name <- download_data[[col]][i]
              if (!is.na(name)) {
                match_idx <- which(driver_ids$Name == name)
                if (length(match_idx) > 0) {
                  download_data[[col]][i] <- paste0(name, " (", driver_ids$DFSID[match_idx[1]], ")")
                }
              }
            }
          }
        }
        
        # Add DFSID to Constructor
        if ("Constructor" %in% names(download_data) && !is.null(constructor_ids)) {
          for (i in 1:nrow(download_data)) {
            name <- download_data$Constructor[i]
            if (!is.na(name)) {
              match_idx <- which(constructor_ids$Name == name)
              if (length(match_idx) > 0) {
                download_data$Constructor[i] <- paste0(name, " (", constructor_ids$DFSID[match_idx[1]], ")")
              }
            }
          }
        }
        
        # Reorder columns to match the requested order
        ordered_cols <- c()
        
        # 1. Captain
        if ("Captain" %in% names(download_data)) {
          ordered_cols <- c(ordered_cols, "Captain")
        }
        
        # 2. Driver1-Driver4 in order
        driver_cols <- grep("^Driver[1-4]$", names(download_data), value = TRUE)
        driver_cols <- sort(driver_cols)  # Ensure numerical order
        ordered_cols <- c(ordered_cols, driver_cols)
        
        # 3. Constructor
        if ("Constructor" %in% names(download_data)) {
          ordered_cols <- c(ordered_cols, "Constructor")
        }
        
        # 4. TotalSalary
        if ("TotalSalary" %in% names(download_data)) {
          ordered_cols <- c(ordered_cols, "TotalSalary")
        }
        
        # 5. Count columns
        count_cols <- c("OptimalCount")
        for (col in count_cols) {
          if (col %in% names(download_data)) {
            ordered_cols <- c(ordered_cols, col)
          }
        }
        
        # Only keep columns that actually exist
        ordered_cols <- intersect(ordered_cols, names(download_data))
        
        # Final data with columns in the requested order
        final_data <- download_data[, ordered_cols]
        
        # Write to CSV file
        write.csv(final_data, file, row.names = FALSE)
        
      }, error = function(e) {
        # Create an error file and print detailed error
        print(e)
        cat("Error in download handler:", as.character(e), "\n")
        write.csv(data.frame(Error = paste("Error:", e$message)), file, row.names = FALSE)
      })
    }
  )
  
  output$driver_freq_plot <- renderPlotly({
    req(rv$simulation_results$optimal_lineup_frequency)
    
    # Extract the top 500 lineups sorted by OptimalCount
    lineup_data <- rv$simulation_results$optimal_lineup_frequency
    
    # Make sure we have the required columns for sorting
    if ("OptimalCount" %in% names(lineup_data)) {
      lineup_data <- lineup_data[order(-lineup_data$OptimalCount),]
    } else if ("Count" %in% names(lineup_data)) {
      # Fall back to Count if we don't have the new columns
      lineup_data <- lineup_data[order(-lineup_data$Count),]
    }
    
    # Take only the top 500 or all if less than 500
    lineup_data <- head(lineup_data, min(500, nrow(lineup_data)))
    
    # Get all driver columns
    driver_cols <- grep("^Driver", names(lineup_data), value = TRUE)
    
    # Get weight column for calculations
    weight_col <- if ("OptimalCount" %in% names(lineup_data)) "OptimalCount" else "Count"
    
    # Calculate total weight for percentage calculation
    total_weight <- sum(lineup_data[[weight_col]])
    
    # Initialize data frame for driver and captain percentages
    driver_data <- data.frame(
      Driver = character(),
      AsDriver = numeric(),
      AsCaptain = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Get unique drivers from all driver columns and captain
    all_drivers <- unique(c(
      unlist(lineup_data[, driver_cols, drop = FALSE]), 
      lineup_data$Captain
    ))
    all_drivers <- all_drivers[!is.na(all_drivers) & all_drivers != ""]
    
    # Calculate percentages for each driver
    for (driver in all_drivers) {
      # Count as regular driver (across all driver columns)
      driver_count <- 0
      for (col in driver_cols) {
        indices <- which(lineup_data[[col]] == driver)
        if (length(indices) > 0) {
          driver_count <- driver_count + sum(lineup_data[[weight_col]][indices])
        }
      }
      
      # Count as captain
      captain_count <- 0
      captain_indices <- which(lineup_data$Captain == driver)
      if (length(captain_indices) > 0) {
        captain_count <- sum(lineup_data[[weight_col]][captain_indices])
      }
      
      # Convert to percentages
      driver_pct <- (driver_count / total_weight) * 100
      captain_pct <- (captain_count / total_weight) * 100
      
      # Add to data frame
      driver_data <- rbind(driver_data, data.frame(
        Driver = driver,
        AsDriver = driver_pct,
        AsCaptain = captain_pct,
        stringsAsFactors = FALSE
      ))
    }
    
    # Calculate total percentage
    driver_data$Total <- driver_data$AsDriver + driver_data$AsCaptain
    
    # Sort by total percentage
    driver_data <- driver_data[order(-driver_data$Total), ]
    
    # Create data for stacked bar plot
    plot_data <- data.frame(
      Driver = rep(driver_data$Driver, 2),
      Position = c(rep("Driver", nrow(driver_data)), rep("Captain", nrow(driver_data))),
      Percentage = c(driver_data$AsDriver, driver_data$AsCaptain),
      Total = rep(driver_data$Total, 2),
      stringsAsFactors = FALSE
    )
    
    # Create stacked bar plot
    p <- ggplot(plot_data, aes(x = reorder(Driver, Total), y = Percentage, fill = Position)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Drivers in Top 500 Lineups",
        y = "Percentage",
        fill = "Position"
      ) +
      scale_fill_manual(values = c("Driver" = "pink", "Captain" = "red"))
    
    ggplotly(p)
  })
  
  
  output$constructor_freq_plot <- renderPlotly({
    req(rv$simulation_results$optimal_lineup_frequency)
    
    # Extract the top 500 lineups sorted by OptimalCount
    lineup_data <- rv$simulation_results$optimal_lineup_frequency
    
    # Make sure we have the required columns for sorting
    if ("OptimalCount" %in% names(lineup_data)) {
      # Sort by OptimalCount
      lineup_data <- lineup_data[order(-lineup_data$OptimalCount),]
    } else if ("Count" %in% names(lineup_data)) {
      # Fall back to Count if we don't have the new columns
      lineup_data <- lineup_data[order(-lineup_data$Count),]
    }
    
    # Take only the top 500 or all if less than 500
    lineup_data <- head(lineup_data, min(500, nrow(lineup_data)))
    
    # Get weight column for calculations
    weight_col <- if ("OptimalCount" %in% names(lineup_data)) "OptimalCount" else "Count"
    
    # Calculate total weight for percentage calculation
    total_weight <- sum(lineup_data[[weight_col]])
    
    # Count constructor occurrences and convert to percentages
    constructor_table <- table(rep(lineup_data$Constructor, lineup_data[[weight_col]]))
    constructor_pct <- (as.numeric(constructor_table) / total_weight) * 100
    
    # Create data frame
    constructor_data <- data.frame(
      Constructor = names(constructor_table),
      Percentage = constructor_pct,
      stringsAsFactors = FALSE
    )
    
    # Sort by percentage
    constructor_data <- constructor_data[order(-constructor_data$Percentage), ]
    
    # Create horizontal bar plot with simplified colors and no y-axis label
    p <- ggplot(constructor_data, aes(x = reorder(Constructor, Percentage), y = Percentage)) +
      geom_bar(stat = "identity", fill = "darkgreen") +  # Simplified to a single green color
      coord_flip() +
      theme_minimal() +
      theme(axis.title.y = element_blank()) +  # Remove y-axis label
      labs(
        title = "Constructors in Top 500 Lineups"
        # Removed y-axis label
      )
    
    ggplotly(p)
  })
  
  
  
  
  ### Lineup Builder 
  
  
  filter_inputs <- reactive({
    list(
      min_top1_count = input$min_top1_count,
      excluded_drivers = input$excluded_drivers,
      excluded_captains = input$excluded_captains,
      excluded_constructors = input$excluded_constructors,
      num_lineups = input$num_random_lineups
    )
  })
  
  output$lineup_count_thresholds <- renderDT({
    req(rv$simulation_results$optimal_lineup_frequency)
    
    # Create threshold values to check
    threshold_values <- c(1, 5, 10, 25, 50, 75, 100)
    
    # Get the lineup data
    lineup_data <- rv$simulation_results$optimal_lineup_frequency
    
    # Create a data frame with columns for each Top Count threshold
    count_data <- data.frame(Threshold = threshold_values)
    
    # Add columns for each type of count
    count_types <- c("OptimalCount")
    
    for (count_type in count_types) {
      if (count_type %in% names(lineup_data)) {
        # Calculate counts for each threshold for this count type
        count_column <- sapply(threshold_values, function(threshold) {
          sum(lineup_data[[count_type]] >= threshold)
        })
        
        # Add to the results data frame
        count_data[[count_type]] <- count_column
      }
    }
    
    # Format column names to be more readable
    col_names <- names(count_data)
    col_names <- gsub("OptimalCount", "Optimal", col_names)
    
    names(count_data) <- col_names
    
    datatable(
      count_data,
      options = list(
        pageLength = 7,
        dom = 't',
        ordering = FALSE
      ),
      rownames = FALSE,
      caption = "Number of lineups with at least this many appearances"
    )
  })
  
  # Filtered pool size output
  output$filtered_pool_size <- renderText({
    req(rv$simulation_results$optimal_lineup_frequency)
    
    filters <- filter_inputs()
    
    stats <- calculate_filtered_pool_stats(rv$simulation_results$optimal_lineup_frequency, filters)
    paste("Number of lineups in filtered pool:", stats$count)
  })
  
  
  
  # Update excluded_drivers select input with drivers from optimal lineups
  observe({
    req(rv$simulation_results$optimal_lineup_frequency)
    
    # Get the lineup data
    lineup_data <- rv$simulation_results$optimal_lineup_frequency
    
    # For drivers
    driver_cols <- grep("^Driver",
                        names(lineup_data),
                        value = TRUE)
    if (length(driver_cols) > 0) {
      all_drivers <- unique(unlist(lineup_data[, driver_cols, drop = FALSE]))
      all_drivers <- all_drivers[!is.na(all_drivers)]
      
      updateSelectizeInput(session,
                           "excluded_drivers",
                           choices = all_drivers,
                           selected = NULL)
    }
    
    # For captains
    if ("Captain" %in% names(lineup_data)) {
      unique_captains <- unique(lineup_data$Captain)
      unique_captains <- unique_captains[!is.na(unique_captains)]
      
      updateSelectizeInput(session,
                           "excluded_captains",
                           choices = unique_captains,
                           selected = NULL)
    }
    
    # For constructors
    if ("Constructor" %in% names(lineup_data)) {
      unique_constructors <- unique(lineup_data$Constructor)
      unique_constructors <- unique_constructors[!is.na(unique_constructors)]
      
      updateSelectizeInput(session,
                           "excluded_constructors",
                           choices = unique_constructors,
                           selected = NULL)
    }
  })
  
  
  # Generate lineups button handler
  observeEvent(input$generate_lineups, {
    req(rv$simulation_results$optimal_lineup_frequency)
    
    filters <- filter_inputs()
    
    # Show progress indicator
    withProgress(message = 'Generating lineups...', value = 0, {
      # Use optimized function for lineup generation
      rv$random_lineups <- generate_random_lineups(rv$simulation_results$optimal_lineup_frequency,
                                                   filters)
      
      # Force garbage collection after generation
      gc(verbose = FALSE)
      
      if (is.null(rv$random_lineups) ||
          nrow(rv$random_lineups) == 0) {
        showModal(
          modalDialog(
            title = "Error",
            "No lineups match the selected filters",
            easyClose = TRUE
          )
        )
      }
    })
  })
  
  # Download handler for driver analysis
  output$downloadDriverAnalysis <- downloadHandler(
    filename = function() {
      paste("f1_driver_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      tryCatch({
        # Get driver analysis data
        download_data <- copy(rv$driver_analysis)
        
        # Write to CSV file
        write.csv(download_data, file, row.names = FALSE)
      }, error = function(e) {
        # Handle errors
        write.csv(data.frame(Error = paste("Error:", e$message)), file, row.names = FALSE)
      })
    }
  )
  
  # Download handler for constructor analysis
  output$downloadConstructorAnalysis <- downloadHandler(
    filename = function() {
      paste("f1_constructor_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      tryCatch({
        # Get constructor analysis data
        download_data <- copy(rv$constructor_analysis)
        
        # Write to CSV file
        write.csv(download_data, file, row.names = FALSE)
      }, error = function(e) {
        # Handle errors
        write.csv(data.frame(Error = paste("Error:", e$message)), file, row.names = FALSE)
      })
    }
  )
  
  output$random_lineups_table <- renderDT({
    req(rv$random_lineups)
    
    # Convert to data.frame for consistent handling
    lineups <- rv$random_lineups
    if (is.data.table(lineups)) {
      lineups <- as.data.frame(lineups)
    }
    
    # Make sure we have the needed columns for F1 lineups (4 regular drivers)
    for (i in 1:4) {
      driver_col <- paste0("Driver", i)
      if (!(driver_col %in% names(lineups))) {
        lineups[[driver_col]] <- NA
      }
    }
    
    # Define the column order to match optimal lineups table
    keep_cols <- c("Captain", "Driver1", "Driver2", "Driver3", "Driver4", 
                   "Constructor", "TotalSalary", "OptimalCount")
    
    # Only keep columns that exist in the data
    keep_cols <- intersect(keep_cols, names(lineups))
    
    # Create datatable with the columns in the right order
    dt <- datatable(
      lineups[, keep_cols],
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        searching = FALSE,
        dom = 'tp'  # Simplified options to avoid potential issues
      ),
      rownames = FALSE,
      selection = 'none'
    )
    
    # Format TotalSalary column if it exists
    if ("TotalSalary" %in% keep_cols) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", digits = 0)
    }
    
    # Format count columns with color bars if they exist
    count_cols <- c("OptimalCount")
    for (col in count_cols) {
      if (col %in% keep_cols) {
        max_val <- max(lineups[[col]], na.rm = TRUE)
        if (!is.finite(max_val) || max_val == 0) max_val <- 1  # Prevent division by zero
        
        dt <- dt %>% formatStyle(
          col,
          background = styleColorBar(c(0, max_val), 'lightgreen'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      }
    }
    
    return(dt)
  })
  
  output$download_random_lineups <- downloadHandler(
    filename = function() {
      paste("f1_random_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      # Use tryCatch for error handling
      tryCatch({
        # Check if we have valid data
        if (is.null(rv$random_lineups) || nrow(rv$random_lineups) == 0) {
          write.csv(data.frame(
            Message = "No random lineups available. Please generate lineups first."
          ), file, row.names = FALSE)
          return()
        }
        
        # Get a copy of the data
        download_data <- rv$random_lineups
        if (is.data.table(download_data)) {
          download_data <- as.data.frame(download_data)
        }
        
        # Get DFSID mappings
        driver_ids <- NULL
        constructor_ids <- NULL
        
        # Get driver IDs if available
        if (!is.null(rv$simulation_results$driver_results)) {
          driver_ids <- unique(rv$simulation_results$driver_results[, c("Name", "DFSID")])
        }
        
        # Get constructor IDs if available
        if (!is.null(rv$simulation_results$constructor_results)) {
          constructor_ids <- unique(rv$simulation_results$constructor_results[, c("Name", "DFSID")])
        }
        
        # Add DFSID to Captain - with +20 to driver DFSID
        if ("Captain" %in% names(download_data) && !is.null(driver_ids)) {
          for (i in 1:nrow(download_data)) {
            name <- download_data$Captain[i]
            if (!is.na(name)) {
              match_idx <- which(driver_ids$Name == name)
              if (length(match_idx) > 0) {
                driver_dfsid <- driver_ids$DFSID[match_idx[1]]
                captain_dfsid <- driver_dfsid + 20  # Add 20 to get captain DFSID
                download_data$Captain[i] <- paste0(name, " (", captain_dfsid, ")")
              }
            }
          }
        }
        
        # Add DFSID to Drivers
        driver_cols <- grep("^Driver", names(download_data), value = TRUE)
        if (length(driver_cols) > 0 && !is.null(driver_ids)) {
          for (col in driver_cols) {
            for (i in 1:nrow(download_data)) {
              name <- download_data[[col]][i]
              if (!is.na(name)) {
                match_idx <- which(driver_ids$Name == name)
                if (length(match_idx) > 0) {
                  download_data[[col]][i] <- paste0(name, " (", driver_ids$DFSID[match_idx[1]], ")")
                }
              }
            }
          }
        }
        
        # Add DFSID to Constructor
        if ("Constructor" %in% names(download_data) && !is.null(constructor_ids)) {
          for (i in 1:nrow(download_data)) {
            name <- download_data$Constructor[i]
            if (!is.na(name)) {
              match_idx <- which(constructor_ids$Name == name)
              if (length(match_idx) > 0) {
                download_data$Constructor[i] <- paste0(name, " (", constructor_ids$DFSID[match_idx[1]], ")")
              }
            }
          }
        }
        
        # Reorder columns to match the requested order - identical to optimal lineups
        ordered_cols <- c()
        
        # 1. Captain
        if ("Captain" %in% names(download_data)) {
          ordered_cols <- c(ordered_cols, "Captain")
        }
        
        # 2. Driver1-Driver4 in order
        driver_cols <- grep("^Driver[1-4]$", names(download_data), value = TRUE)
        driver_cols <- sort(driver_cols)  # Ensure numerical order
        ordered_cols <- c(ordered_cols, driver_cols)
        
        # 3. Constructor
        if ("Constructor" %in% names(download_data)) {
          ordered_cols <- c(ordered_cols, "Constructor")
        }
        
        # 4. TotalSalary
        if ("TotalSalary" %in% names(download_data)) {
          ordered_cols <- c(ordered_cols, "TotalSalary")
        }
        
        # 5. Count columns
        count_cols <- c("OptimalCount")
        for (col in count_cols) {
          if (col %in% names(download_data)) {
            ordered_cols <- c(ordered_cols, col)
          }
        }
        
        # Only keep columns that actually exist
        ordered_cols <- intersect(ordered_cols, names(download_data))
        
        # Final data with columns in the requested order
        final_data <- download_data[, ordered_cols]
        
        # Write to CSV file
        write.csv(final_data, file, row.names = FALSE)
        
      }, error = function(e) {
        # Create an error file and print detailed error
        print(e)
        cat("Error in download handler:", as.character(e), "\n")
        write.csv(data.frame(Error = paste("Error:", e$message)), file, row.names = FALSE)
      })
    }
  )
  
  
  output$driver_exposure_table <- renderDT({
    req(rv$simulation_results$optimal_lineup_frequency)
    
    # Extract unique driver names from optimal lineups
    lineup_data <- rv$simulation_results$optimal_lineup_frequency
    
    # Get all driver columns
    driver_cols <- grep("^Driver", names(lineup_data), value = TRUE)
    captain_col <- if ("Captain" %in% names(lineup_data))
      "Captain"
    else
      NULL
    constructor_col <- if ("Constructor" %in% names(lineup_data))
      "Constructor"
    else
      NULL
    
    # Convert to data.frame to avoid data.table syntax issues
    ol_df <- rv$simulation_results$optimal_lineup_frequency
    if (is.data.table(ol_df)) {
      ol_df <- as.data.frame(ol_df)
    }
    
    # Extract driver names from driver columns
    driver_entries <- unique(unlist(ol_df[, driver_cols, drop = FALSE]))
    driver_entries <- driver_entries[!is.na(driver_entries)]
    
    # Extract captain names separately
    captain_entries <- c()
    if (!is.null(captain_col)) {
      captain_entries <- unique(ol_df[[captain_col]])
      captain_entries <- captain_entries[!is.na(captain_entries)]
    }
    
    # Extract constructor names separately
    constructor_entries <- c()
    if (!is.null(constructor_col)) {
      constructor_entries <- unique(ol_df[[constructor_col]])
      constructor_entries <- constructor_entries[!is.na(constructor_entries)]
    }
    
    # Combine driver names (excluding constructors)
    all_driver_entries <- unique(c(driver_entries, captain_entries))
    all_driver_entries <- setdiff(all_driver_entries, constructor_entries)
    
    # Create exposure data frame with the correct column structure
    exposure_data <- data.frame(
      Driver = all_driver_entries,
      DriverSalary = 0,
      CaptainSalary = 0,
      Optimal = 0,
      Exposure = 0,
      DriverOptimal = 0,
      DriverExposure = 0,
      CaptainOptimal = 0,
      CaptainExposure = 0,
      stringsAsFactors = FALSE
    )
    
    # Calculate Optimal exposure percentages with role breakdown
    if ("OptimalCount" %in% names(ol_df)) {
      total_count <- sum(ol_df$OptimalCount, na.rm = TRUE)
    } else if ("Count" %in% names(ol_df)) {
      total_count <- sum(ol_df$Count, na.rm = TRUE)
    } else {
      total_count <- nrow(ol_df)
    }
    
    for (entry in all_driver_entries) {
      # As regular driver
      if (length(driver_cols) > 0) {
        driver_lineups <- ol_df[apply(ol_df[, driver_cols, drop = FALSE], 1, function(row)
          entry %in% row), , drop = FALSE]
        
        if (nrow(driver_lineups) > 0) {
          driver_count <- 0
          if ("OptimalCount" %in% names(driver_lineups)) {
            driver_count <- sum(driver_lineups$OptimalCount, na.rm = TRUE)
          } else if ("Count" %in% names(driver_lineups)) {
            driver_count <- sum(driver_lineups$Count, na.rm = TRUE)
          } else {
            driver_count <- nrow(driver_lineups)
          }
          
          driver_optimal_pct <- (driver_count / total_count) * 100
          
          # Update driver optimal percentage
          exposure_data$DriverOptimal[exposure_data$Driver == entry] <- driver_optimal_pct
        }
      }
      
      # As captain
      if (!is.null(captain_col)) {
        captain_lineups <- ol_df[ol_df[[captain_col]] == entry, , drop = FALSE]
        
        if (nrow(captain_lineups) > 0) {
          captain_count <- 0
          if ("OptimalCount" %in% names(captain_lineups)) {
            captain_count <- sum(captain_lineups$OptimalCount, na.rm = TRUE)
          } else if ("Count" %in% names(captain_lineups)) {
            captain_count <- sum(captain_lineups$Count, na.rm = TRUE)
          } else {
            captain_count <- nrow(captain_lineups)
          }
          
          captain_optimal_pct <- (captain_count / total_count) * 100
          
          # Update captain optimal percentage
          exposure_data$CaptainOptimal[exposure_data$Driver == entry] <- captain_optimal_pct
        }
      }
      
      # Calculate total optimal percentage
      exposure_data$Optimal[exposure_data$Driver == entry] <-
        exposure_data$DriverOptimal[exposure_data$Driver == entry] +
        exposure_data$CaptainOptimal[exposure_data$Driver == entry]
    }
    
    # Calculate Exposure from random lineups if available
    if (!is.null(rv$random_lineups) && nrow(rv$random_lineups) > 0) {
      random_df <- rv$random_lineups
      if (is.data.table(random_df)) {
        random_df <- as.data.frame(random_df)
      }
      
      random_driver_cols <- grep("^Driver", names(random_df), value = TRUE)
      random_captain_col <- if ("Captain" %in% names(random_df))
        "Captain"
      else
        NULL
      
      total_random_lineups <- nrow(random_df)
      
      for (entry in all_driver_entries) {
        # Process exposure for each entry type
        
        # As regular driver
        if (length(random_driver_cols) > 0) {
          # Count lineups where this entry appears as a driver
          random_driver_count <- sum(sapply(1:nrow(random_df), function(i) {
            driver_values <- unlist(random_df[i, random_driver_cols, drop = FALSE])
            return(any(driver_values == entry))
          }))
          
          if (random_driver_count > 0) {
            driver_exposure_pct <- (random_driver_count / total_random_lineups) * 100
            exposure_data$DriverExposure[exposure_data$Driver == entry] <- driver_exposure_pct
          }
        }
        
        # As captain
        if (!is.null(random_captain_col)) {
          # Count lineups where this entry appears as captain
          random_captain_count <- sum(random_df[[random_captain_col]] == entry, na.rm = TRUE)
          
          if (random_captain_count > 0) {
            captain_exposure_pct <- (random_captain_count / total_random_lineups) * 100
            exposure_data$CaptainExposure[exposure_data$Driver == entry] <- captain_exposure_pct
          }
        }
        
        # Calculate total exposure percentage
        exposure_data$Exposure[exposure_data$Driver == entry] <-
          exposure_data$DriverExposure[exposure_data$Driver == entry] +
          exposure_data$CaptainExposure[exposure_data$Driver == entry]
      }
    }
    
    # Add player info
    for (i in 1:nrow(exposure_data)) {
      entry <- exposure_data$Driver[i]
      
      # Get driver info if available
      if (!is.null(rv$simulation_results$driver_results)) {
        driver_idx <- which(rv$simulation_results$driver_results$Name == entry)
        
        if (length(driver_idx) > 0) {
          # Get first matching record
          driver_info <- rv$simulation_results$driver_results[driver_idx[1]]
          
          # Update driver info
          if ("DriverSalary" %in% names(driver_info))
            exposure_data$DriverSalary[i] <- driver_info$DriverSalary
          
          if ("CaptainSalary" %in% names(driver_info))
            exposure_data$CaptainSalary[i] <- driver_info$CaptainSalary
        }
      }
    }
    
    # Sort by Optimal exposure (descending)
    exposure_data <- exposure_data[order(-exposure_data$Optimal), ]
    
    # Choose columns to display in the requested order
    display_cols <- c(
      "Driver",
      "DriverSalary",
      "CaptainSalary",
      "Optimal",
      "Exposure",
      "DriverOptimal",
      "DriverExposure",
      "CaptainOptimal",
      "CaptainExposure"
    )
    
    display_data <- exposure_data[, intersect(display_cols, names(exposure_data))]
    
    # Create the datatable with requested options (no pagination, no row names)
    dt <- datatable(display_data,
                    options = list(
                      paging = FALSE,         # No pagination
                      scrollX = TRUE,         # Horizontal scrolling
                      scrollY = "800px",      # Fixed height with vertical scrolling
                      dom = 't',              # Only show the table (no search, pagination, etc.)
                      ordering = TRUE,        # Allow column sorting
                      rownames = FALSE        # No row names/numbers
                    ),
                    rownames = FALSE) %>%     # No row names/numbers
      formatCurrency(
        c('DriverSalary', 'CaptainSalary'),
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      ) %>%
      formatRound(
        c(
          'Optimal',
          'Exposure',
          'DriverOptimal',
          'DriverExposure',
          'CaptainOptimal',
          'CaptainExposure'
        ),
        digits = 1
      )
    
    return(dt)
  })
  
  # New table for constructor exposure
  output$constructor_exposure_table <- renderDT({
    req(rv$simulation_results$optimal_lineup_frequency)
    
    # Extract constructor names from optimal lineups
    lineup_data <- rv$simulation_results$optimal_lineup_frequency
    
    constructor_col <- if ("Constructor" %in% names(lineup_data))
      "Constructor"
    else
      NULL
    
    # Early exit if no constructor column found
    if (is.null(constructor_col)) {
      return(datatable(data.frame(Message = "No constructor data available")))
    }
    
    # Convert to data.frame to avoid data.table syntax issues
    ol_df <- rv$simulation_results$optimal_lineup_frequency
    if (is.data.table(ol_df)) {
      ol_df <- as.data.frame(ol_df)
    }
    
    # Extract constructor names
    constructor_entries <- unique(ol_df[[constructor_col]])
    constructor_entries <- constructor_entries[!is.na(constructor_entries)]
    
    # Create exposure data frame with the correct column structure
    exposure_data <- data.frame(
      Constructor = constructor_entries,
      Salary = 0,
      Optimal = 0,
      Exposure = 0,
      stringsAsFactors = FALSE
    )
    
    # Calculate Optimal exposure percentages
    if ("OptimalCount" %in% names(ol_df)) {
      total_count <- sum(ol_df$OptimalCount, na.rm = TRUE)
    } else if ("Count" %in% names(ol_df)) {
      total_count <- sum(ol_df$Count, na.rm = TRUE)
    } else {
      total_count <- nrow(ol_df)
    }
    
    for (entry in constructor_entries) {
      constructor_lineups <- ol_df[ol_df[[constructor_col]] == entry, , drop = FALSE]
      
      if (nrow(constructor_lineups) > 0) {
        constructor_count <- 0
        if ("OptimalCount" %in% names(constructor_lineups)) {
          constructor_count <- sum(constructor_lineups$OptimalCount, na.rm = TRUE)
        } else if ("Count" %in% names(constructor_lineups)) {
          constructor_count <- sum(constructor_lineups$Count, na.rm = TRUE)
        } else {
          constructor_count <- nrow(constructor_lineups)
        }
        
        constructor_optimal_pct <- (constructor_count / total_count) * 100
        
        # Update optimal percentage
        exposure_data$Optimal[exposure_data$Constructor == entry] <- constructor_optimal_pct
      }
    }
    
    # Calculate Exposure from random lineups if available
    if (!is.null(rv$random_lineups) && nrow(rv$random_lineups) > 0) {
      random_df <- rv$random_lineups
      if (is.data.table(random_df)) {
        random_df <- as.data.frame(random_df)
      }
      
      random_constructor_col <- if ("Constructor" %in% names(random_df))
        "Constructor"
      else
        NULL
      
      if (!is.null(random_constructor_col)) {
        total_random_lineups <- nrow(random_df)
        
        for (entry in constructor_entries) {
          # Count lineups where this entry appears as constructor
          random_constructor_count <- sum(random_df[[random_constructor_col]] == entry, na.rm = TRUE)
          
          if (random_constructor_count > 0) {
            constructor_exposure_pct <- (random_constructor_count / total_random_lineups) * 100
            exposure_data$Exposure[exposure_data$Constructor == entry] <- constructor_exposure_pct
          }
        }
      }
    }
    
    # Add constructor salary info
    for (i in 1:nrow(exposure_data)) {
      entry <- exposure_data$Constructor[i]
      
      # Get constructor info if available
      if (!is.null(rv$simulation_results$constructor_results)) {
        constructor_idx <- which(rv$simulation_results$constructor_results$Name == entry)
        
        if (length(constructor_idx) > 0) {
          # Get first matching constructor record
          constructor_info <- rv$simulation_results$constructor_results[constructor_idx[1]]
          
          # Update constructor info
          if ("Salary" %in% names(constructor_info))
            exposure_data$Salary[i] <- constructor_info$Salary
        }
      }
    }
    
    # Sort by Optimal exposure (descending)
    exposure_data <- exposure_data[order(-exposure_data$Optimal), ]
    
    # Create the datatable with requested options (no pagination, no row names)
    dt <- datatable(exposure_data,
                    options = list(
                      paging = FALSE,         # No pagination
                      scrollX = TRUE,         # Horizontal scrolling
                      scrollY = "400px",      # Fixed height with vertical scrolling
                      dom = 't',              # Only show the table (no search, pagination, etc.)
                      ordering = TRUE,        # Allow column sorting
                      rownames = FALSE        # No row names/numbers
                    ),
                    rownames = FALSE) %>%     # No row names/numbers
      formatCurrency(
        c('Salary'),
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      ) %>%
      formatRound(
        c('Optimal', 'Exposure'),
        digits = 1
      )
    
    return(dt)
  })
  
  onSessionEnded(function() {
    gc(full = TRUE, verbose = FALSE)
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)