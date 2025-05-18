# app.R
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

# Set up custom CSS for black and orange theme
custom_css <- "
  /* Override dashboard header colors */
  .skin-blue .main-header {
    background-color: #000000;
  }
  .skin-blue .main-header .logo {
    background-color: #000000;
    color: #ff6600;
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
    color: #ff6600;
  }
  .skin-blue .sidebar-menu > li.active > a, 
  .skin-blue .sidebar-menu > li:hover > a {
    color: #ffffff;
    background: #333333;
    border-left-color: #ff6600;
  }
  
  /* Customize box headers */
  .box.box-primary .box-header {
    background-color: #333333;
    color: #ff6600;
  }
  
  /* Style buttons */
  .btn-primary {
    background-color: #ff6600;
    border-color: #e65c00;
    color: #ffffff;
  }
  .btn-primary:hover, .btn-primary:focus {
    background-color: #e65c00;
    border-color: #cc5200;
  }
  
  /* Style tabs */
  .nav-tabs-custom > .nav-tabs > li.active {
    border-top-color: #ff6600;
  }
"


# Global constants
DK_ROSTER_SIZE <- 6
FD_ROSTER_SIZE <- 5
DK_SALARY_CAP <- 50000
FD_SALARY_CAP <- 50000



read_input_file <- function(file_path) {
  tryCatch({
    # Read sheets needed by both platforms
    sheets <- list(
      Driver = read_excel(file_path, sheet = "Driver"),
      Race = read_excel(file_path, sheet = "Race")
    )
    
    # Try to read DraftKings specific sheet with the new name
    sheets$DKDom <- tryCatch(
      read_excel(file_path, sheet = "DKDom"),
      error = function(e) NULL
    )
    
    # Try to read optional FanDuel specific sheets
    sheets$FDDom <- tryCatch(
      read_excel(file_path, sheet = "FDDom"),
      error = function(e) NULL
    )
    
    sheets$FDLaps <- tryCatch(
      read_excel(file_path, sheet = "FDLaps"),
      error = function(e) NULL
    )
    
    # Identify available platforms based on sheets
    has_dk <- !is.null(sheets$DKDom) && "DKSalary" %in% colnames(sheets$Driver)
    has_fd <- (!is.null(sheets$FDDom) || !is.null(sheets$FDLaps)) && "FDSalary" %in% colnames(sheets$Driver)
    
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

# Process input data efficiently with data.table
process_input_data <- function(input_data) {
  # Extract data components
  driver_data <- input_data$sheets$Driver
  race_data <- input_data$sheets$Race
  dk_dom_data <- input_data$sheets$DKDom
  fd_dom_data <- input_data$sheets$FDDom
  fd_laps_data <- input_data$sheets$FDLaps
  
  # Process driver data
  processed_drivers <- as.data.table(driver_data)
  
  # Convert relevant numeric columns efficiently
  numeric_cols <- c("W", "T3", "T5", "T10", "T15", "T20", "T25", "T30", 
                    "DKSalary", "FDSalary", "DKOP", "FDOP", "Starting", 
                    "DKDomTier", "FDDomTier")
  
  for(col in numeric_cols) {
    if(col %in% names(processed_drivers)) {
      processed_drivers[, (col) := as.numeric(get(col))]
    }
  }
  
  # Process race data
  processed_race <- as.data.table(race_data)
  
  # Process DK dominator data if available
  processed_dk_dominator <- if(!is.null(dk_dom_data)) {
    dom_dt <- as.data.table(dk_dom_data)
    dom_dt <- dom_dt[!(is.na(PtLow) & is.na(PtHigh) & is.na(FinLow) & is.na(FinHigh) & 
                         is.na(OR) & is.na(TierMin) & is.na(TierMax))]
    
    dom_dt[, ProcessedRank := {
      sapply(Rank, function(x) {
        if(x == "Strategy") return(0)
        if(x == "DomDead") return(999)
        # Try to convert to numeric, return 999 if it fails
        num <- suppressWarnings(as.numeric(as.character(x)))
        if(is.na(num)) return(999) else return(num)
      })
    }]
  
    
    dom_dt[, OriginalRank := Rank]
    setorder(dom_dt, ProcessedRank)
    dom_dt
  } else data.table()
  

  # Process FD dominator data if available
  processed_fd_dominator <- if(!is.null(fd_dom_data)) {
    fd_dom_dt <- as.data.table(fd_dom_data)
    fd_dom_dt <- fd_dom_dt[!is.na(PtLow) | !is.na(PtHigh)]
    
    # Calculate ProcessedRank safely without warnings
    fd_dom_dt[, ProcessedRank := {
      sapply(Rank, function(x) {
        if(x == "Strategy") return(0)
        if(x == "DomDead") return(999)
        # Try to convert to numeric, return 999 if it fails
        num <- suppressWarnings(as.numeric(as.character(x)))
        if(is.na(num)) return(999) else return(num)
      })
    }]
    
    fd_dom_dt[, OriginalRank := Rank]
    
    # Ensure all required columns exist
    if(!"OR" %in% names(fd_dom_dt)) fd_dom_dt[, OR := "R"]
    if(!"TierMin" %in% names(fd_dom_dt)) fd_dom_dt[, TierMin := 1]
    if(!"TierMax" %in% names(fd_dom_dt)) fd_dom_dt[, TierMax := 3]
    if(!"FinLow" %in% names(fd_dom_dt)) fd_dom_dt[, FinLow := 1]
    if(!"FinHigh" %in% names(fd_dom_dt)) fd_dom_dt[, FinHigh := 35]
    
    setorder(fd_dom_dt, ProcessedRank)
    fd_dom_dt
  } else data.table()
  
  # Process FD laps data if available
  processed_fd_laps <- if(!is.null(fd_laps_data)) {
    fd_laps_dt <- as.data.table(fd_laps_data)
    
    # Only handle Pt column (no more high/low)
    fd_laps_dt <- fd_laps_dt[!is.na(Pt)]
    
    # Convert columns to numeric
    for(col in c("ps", "Pt")) {
      if(col %in% names(fd_laps_dt)) {
        fd_laps_dt[, (col) := as.numeric(get(col))]
      }
    }
    
    # Use position (ps) as finish position
    fd_laps_dt[, FinishLow := ps]
    fd_laps_dt[, FinishHigh := ps]
    
    setorder(fd_laps_dt, ps)
    fd_laps_dt
  } else data.table()
  
  # Extract dominator points values for both platforms
  dk_dom_points <- if("DKDom" %in% names(processed_race)) {
    processed_race$DKDom
  } else numeric(0)
  
  fd_dom_points <- if("FDDom" %in% names(processed_race)) {
    processed_race$FDDom
  } else numeric(0)
  
  # Return processed data
  list(
    drivers = processed_drivers,
    race = processed_race,
    dk_dominator = processed_dk_dominator,
    fd_dominator = processed_fd_dominator,
    fd_laps = processed_fd_laps,
    dk_dom_points = dk_dom_points,
    fd_dom_points = fd_dom_points
  )
}


# Optimized race simulation function
simulate_finishing_positions <- function(drivers_dt) {
  n_drivers <- nrow(drivers_dt)
  
  # Extract cumulative probabilities for each driver
  prob_cols <- c("W", "T3", "T5", "T10", "T15", "T20", "T25", "T30")
  
  # Generate performance scores for each driver
  performance_scores <- numeric(n_drivers)
  
  for (i in 1:n_drivers) {
    # Convert cumulative probabilities to marginal probabilities
    cum_probs <- c(
      drivers_dt$W[i],
      drivers_dt$T3[i],
      drivers_dt$T5[i],
      drivers_dt$T10[i],
      drivers_dt$T15[i],
      drivers_dt$T20[i],
      drivers_dt$T25[i],
      drivers_dt$T30[i],
      1.0  # Ensure probabilities sum to 1
    )
    
    # Handle NA values
    cum_probs[is.na(cum_probs)] <- 0
    
    # Convert to marginal probabilities
    pos_ranges <- list(
      1,        # W: Position 1
      2:3,      # T3: Positions 2-3
      4:5,      # T5: Positions 4-5
      6:10,     # T10: Positions 6-10
      11:15,    # T15: Positions 11-15
      16:20,    # T20: Positions 16-20
      21:25,    # T25: Positions 21-25
      26:30,    # T30: Positions 26-30
      31:n_drivers  # Beyond T30
    )
    
    # Calculate marginal probabilities
    marg_probs <- numeric(length(pos_ranges))
    prev_prob <- 0
    for (j in 1:length(cum_probs)) {
      marg_probs[j] <- max(0, cum_probs[j] - prev_prob)  # Ensure non-negative
      prev_prob <- cum_probs[j]
    }
    
    # Normalize to ensure they sum to 1
    if (sum(marg_probs) > 0) {
      marg_probs <- marg_probs / sum(marg_probs)
    } else {
      # If all zero (should be rare), use uniform distribution
      marg_probs <- rep(1/length(marg_probs), length(marg_probs))
    }
    
    # Sample a position range based on marginal probabilities
    pos_range_idx <- sample(1:length(pos_ranges), 1, prob = marg_probs)
    pos_range <- pos_ranges[[pos_range_idx]]
    
    # Sample a specific position within the range
    if (length(pos_range) > 1) {
      sampled_pos <- sample(pos_range, 1)
    } else {
      sampled_pos <- pos_range
    }
    
    # Add small random noise to break ties
    performance_scores[i] <- sampled_pos + runif(1, 0, 0.1)
  }
  
  # Rank the drivers (lower score is better)
  finish_positions <- rank(performance_scores)
  
  # Check for duplicates (extremely rare but possible)
  if (anyDuplicated(finish_positions)) {
    # Add a tiny bit more randomness to resolve ties
    finish_positions <- rank(performance_scores + runif(n_drivers, 0, 0.001))
  }
  
  return(finish_positions)
}

# Platform-specific dominator point assignment for DraftKings
assign_dk_dominator_points <- function(race_results, dominator_data, total_dom_points) {
  setDT(race_results)
  setDT(dominator_data)
  
  # Pre-allocate and pre-process
  race_results[, DKDominatorPoints := 0]
  remaining_points <- total_dom_points
  
  # Add safety check for empty dominator_data
  if(nrow(dominator_data) == 0) {
    # Fallback distribution if no rules but points exist
    if(remaining_points > 0 && nrow(race_results) > 0) {
      # Distribute points to top finishers, prioritizing higher tier drivers
      top_finishers <- race_results[order(DKDomTier, FinishPosition)][1:min(5, .N)]
      if(nrow(top_finishers) > 0) {
        points_per_driver <- round(remaining_points / nrow(top_finishers), 2)
        for(i in 1:nrow(top_finishers)) {
          race_results[Name == top_finishers$Name[i], 
                       DKDominatorPoints := DKDominatorPoints + points_per_driver]
        }
      }
    }
    return(race_results)
  }
  
  # Track which drivers have received points
  drivers_with_points <- character(0)
  
  # Apply rules in order of rank
  if(remaining_points > 0) {
    # First Pass: Rank-Based Point Assignment
    for(i in 1:nrow(dominator_data)) {
      if(remaining_points <= 0) break
      
      rule <- dominator_data[i]
      
      # Skip invalid rules
      if(is.null(rule) || length(rule) == 0) next
      
      # Extract rule parameters with defaults
      fin_low <- ifelse(is.na(rule$FinLow), 1, rule$FinLow)
      fin_high <- ifelse(is.na(rule$FinHigh), Inf, rule$FinHigh)
      tier_min <- ifelse(is.na(rule$TierMin), 1, rule$TierMin)
      tier_max <- ifelse(is.na(rule$TierMax), Inf, rule$TierMax)
      
      # Find eligible drivers
      eligible <- race_results[
        !(Name %in% drivers_with_points) &
          DKDominatorPoints == 0 & 
          FinishPosition >= fin_low & 
          FinishPosition <= fin_high & 
          DKDomTier >= tier_min & 
          DKDomTier <= tier_max
      ]
      
      # Try less strict criteria if no eligible drivers
      if(nrow(eligible) == 0) {
        fin_high_expanded <- ifelse(is.infinite(fin_high), Inf, fin_high + 2)
        
        eligible <- race_results[
          !(Name %in% drivers_with_points) &
            DKDominatorPoints == 0 & 
            FinishPosition >= fin_low & 
            FinishPosition <= fin_high_expanded & 
            DKDomTier >= tier_min & 
            DKDomTier <= tier_max
        ]
      }
      
      # Try even more relaxed criteria if still no eligible drivers
      if(nrow(eligible) == 0) {
        fin_high_wider <- ifelse(is.infinite(fin_high), Inf, fin_high + 4)
        
        eligible <- race_results[
          !(Name %in% drivers_with_points) &
            DKDominatorPoints == 0 & 
            FinishPosition >= fin_low & 
            FinishPosition <= fin_high_wider & 
            DKDomTier >= tier_min & 
            DKDomTier <= tier_max
        ]
      }
      
      # Tier-based fallback as last resort
      if(nrow(eligible) == 0) {
        for(tier in 1:3) {
          if(tier > tier_max) break
          
          tier_drivers <- race_results[
            !(Name %in% drivers_with_points) &
              DKDominatorPoints == 0 & 
              DKDomTier == tier
          ]
          
          if(nrow(tier_drivers) > 0) {
            eligible <- tier_drivers[order(FinishPosition)][1]
            break
          }
        }
      }
      
      # Skip if still no eligible drivers
      if(nrow(eligible) == 0) next
      
      # Determine point range from rule
      points_low <- max(0, ifelse(is.na(rule$PtLow), 0, rule$PtLow))
      points_high <- min(
        ifelse(is.na(rule$PtHigh), 5, rule$PtHigh),
        remaining_points
      )
      
      # Skip if no points available
      if(points_low > points_high) next
      
      # Selection logic (ordered vs random)
      has_or_value <- !is.null(rule$OR) && !is.na(rule$OR) && length(rule$OR) > 0
      
      selected_driver <- if(has_or_value) {
        if(rule$OR == "O") {
          eligible[order(FinishPosition)][1]
        } else if(rule$OR == "R") {
          eligible[sample.int(nrow(eligible), 1)]
        } else {
          eligible[order(FinishPosition)][1]
        }
      } else {
        eligible[order(FinishPosition)][1]
      }
      
      # Determine points to assign
      points_to_assign <- if(i == nrow(dominator_data)) {
        if(selected_driver$DKDomTier <= 2) {
          min(remaining_points, total_dom_points * 0.4)
        } else {
          min(remaining_points, 5.0)
        }
      } else {
        min(
          round(runif(1, points_low, points_high), 2),
          remaining_points
        )
      }
      
      # Ensure non-negative points
      points_to_assign <- max(0, points_to_assign)
      
      # Assign points
      if(points_to_assign > 0) {
        race_results[Name == selected_driver$Name, 
                     DKDominatorPoints := DKDominatorPoints + points_to_assign]
        
        # Track this driver and update remaining points
        drivers_with_points <- c(drivers_with_points, selected_driver$Name)
        remaining_points <- remaining_points - points_to_assign
      }
    }
  }
  
  # Handle remaining points using "dominated but wrecked" logic
  if(remaining_points > 0) {
    if(remaining_points >= (total_dom_points * 0.20)) {
      # Look for Tier 1 drivers who finished poorly
      potential_dominators <- race_results[DKDomTier == 1 & FinishPosition > 15 & DKDominatorPoints == 0]
      
      # If no Tier 1 drivers meet criteria, check Tier 2
      if(nrow(potential_dominators) == 0) {
        potential_dominators <- race_results[DKDomTier == 2 & FinishPosition > 15 & DKDominatorPoints == 0]
      }
      
      if(nrow(potential_dominators) > 0) {
        # Select one with worse finish position (likely crashed/had issues)
        potential_dominators <- potential_dominators[order(-FinishPosition)]
        selected_dominator <- potential_dominators[1]
        
        # Assign a large chunk of remaining points
        points_to_assign <- round(remaining_points * runif(1, 0.6, 0.9), 2)
        
        race_results[Name == selected_dominator$Name, 
                     DKDominatorPoints := DKDominatorPoints + points_to_assign]
        
        remaining_points <- remaining_points - points_to_assign
      }
    }
    
    # Distribute any remaining points to drivers with no points yet
    remaining_drivers <- race_results[DKDominatorPoints == 0]
    
    if(nrow(remaining_drivers) > 0) {
      # Process each tier in sequence
      for(tier in 1:5) {
        # Get drivers in this tier
        tier_drivers <- remaining_drivers[DKDomTier == tier]
        
        if(nrow(tier_drivers) > 0) {
          # Different allocation strategy for different tiers
          if(tier <= 3) {
            # For tiers 1-3, allocate with exponential decay
            tier_weights <- c(0.65, 0.25, 0.1, 0, 0)
            tier_points <- remaining_points * tier_weights[tier]
            
            if(tier_points <= 0) next
            
            # Sort by finish position
            tier_drivers <- tier_drivers[order(FinishPosition)]
            
            # Create exponential decay weights
            position_factor <- 0.7 + (0.05 * tier)
            weights <- exp(-position_factor * (1:nrow(tier_drivers) - 1))
            weights <- weights / sum(weights)
            
            # Calculate points per driver
            points_allocation <- round(tier_points * weights, 2)
            
            # Assign points
            points_assigned <- FALSE
            for(i in 1:nrow(tier_drivers)) {
              if(points_allocation[i] > 0) {
                race_results[Name == tier_drivers$Name[i], 
                             DKDominatorPoints := DKDominatorPoints + points_allocation[i]]
                
                remaining_points <- remaining_points - points_allocation[i]
                points_assigned <- TRUE
              }
            }
            
            # Stop after assigning points to a tier
            if(points_assigned) break
          } else {
            # For tiers 4-5, assign minimal points
            if(remaining_points > 0.1) {
              tier_drivers <- tier_drivers[order(FinishPosition)]
              max_per_driver <- min(0.5, remaining_points / nrow(tier_drivers))
              
              for(i in 1:min(3, nrow(tier_drivers))) {
                if(remaining_points < 0.1) break
                
                points_to_add <- min(max_per_driver, remaining_points)
                
                race_results[Name == tier_drivers$Name[i], 
                             DKDominatorPoints := DKDominatorPoints + points_to_add]
                
                remaining_points <- remaining_points - points_to_add
              }
            }
          }
        }
      }
    }
  }
  
  # Validation - cap Tier 4/5 drivers with excessive points
  unusual_allocations <- race_results[DKDomTier >= 4 & DKDominatorPoints > 5]
  if(nrow(unusual_allocations) > 0) {
    for(i in 1:nrow(unusual_allocations)) {
      driver_name <- unusual_allocations$Name[i]
      original_points <- unusual_allocations$DKDominatorPoints[i]
      excess_points <- original_points - 5
      
      race_results[Name == driver_name, DKDominatorPoints := 5]
      
      # Redistribute excess points to top Tier 1 finishers
      if(excess_points > 0) {
        top_t1_drivers <- race_results[DKDomTier == 1][order(FinishPosition)][1:3]
        if(nrow(top_t1_drivers) > 0) {
          points_per_driver <- round(excess_points / nrow(top_t1_drivers), 2)
          for(j in 1:nrow(top_t1_drivers)) {
            race_results[Name == top_t1_drivers$Name[j], 
                         DKDominatorPoints := DKDominatorPoints + points_per_driver]
          }
        }
      }
    }
  }
  
  # Final check - ensure total is not higher than the race total
  total_allocated <- sum(race_results$DKDominatorPoints, na.rm = TRUE)
  if(total_allocated > total_dom_points) {
    # Scale everyone's points down proportionally
    scale_factor <- total_dom_points / total_allocated
    race_results[, DKDominatorPoints := round(DKDominatorPoints * scale_factor, 2)]
  }
  
  return(race_results)
}

# Platform-specific dominator point assignment for FanDuel
assign_fd_dominator_points <- function(race_results, fd_dominator_data, total_dom_points) {
  setDT(race_results)
  setDT(fd_dominator_data)
  
  # Create copy to avoid modifying the original
  result <- copy(race_results)
  
  # Initialize with zeros
  result[, FDDominatorPoints := 0]
  remaining_points <- total_dom_points
  
  # Safety check for empty dominator data
  if(nrow(fd_dominator_data) == 0) {
    # Fallback distribution if no rules but points exist
    if(remaining_points > 0 && nrow(result) > 0) {
      # Distribute based on finish position and tier
      top_drivers <- result[order(FDDomTier, FinishPosition)][1:min(5, .N)]
      if(nrow(top_drivers) > 0) {
        points_per_driver <- round(remaining_points / nrow(top_drivers), 2)
        for(i in 1:nrow(top_drivers)) {
          result[Name == top_drivers$Name[i], 
                 FDDominatorPoints := FDDominatorPoints + points_per_driver]
        }
      }
    }
    return(result)
  }
  
  # Process rank field for sorting
  if("Rank" %in% names(fd_dominator_data)) {
    fd_dominator_data[, ProcessedRank := {
      ifelse(Rank == "Strategy", 0,
             ifelse(Rank == "DomDead", 999,
                    as.numeric(Rank)))
    }]
    fd_dominator_data[, OriginalRank := Rank]
    setorder(fd_dominator_data, ProcessedRank)
  }
  
  # Track drivers who have received points
  drivers_with_points <- character(0)
  
  # Apply each rule in order
  for(i in 1:nrow(fd_dominator_data)) {
    if(remaining_points <= 0) break
    
    rule <- fd_dominator_data[i]
    if(is.null(rule) || length(rule) == 0) next
    
    # Extract rule parameters with defaults
    fin_low <- ifelse(is.na(rule$FinLow), 1, rule$FinLow)
    fin_high <- ifelse(is.na(rule$FinHigh), Inf, rule$FinHigh)
    tier_min <- ifelse(is.na(rule$TierMin), 1, rule$TierMin)
    tier_max <- ifelse(is.na(rule$TierMax), Inf, rule$TierMax)
    pt_low <- max(0, ifelse(is.na(rule$PtLow), 0, rule$PtLow))
    pt_high <- min(ifelse(is.na(rule$PtHigh), 5, rule$PtHigh), remaining_points)
    
    # Skip if no points available
    if(pt_low > pt_high) next
    
    # Find eligible drivers
    eligible <- result[
      !(Name %in% drivers_with_points) &
        FinishPosition >= fin_low & 
        FinishPosition <= fin_high & 
        FDDomTier >= tier_min & 
        FDDomTier <= tier_max
    ]
    
    if(nrow(eligible) == 0) next
    
    # Selection method - Ordered vs Random
    has_or_value <- !is.null(rule$OR) && !is.na(rule$OR) && length(rule$OR) > 0
    
    selected_driver <- if(has_or_value && rule$OR == "R") {
      # Random selection
      eligible[sample.int(nrow(eligible), 1)]
    } else {
      # Default to ordered by position
      eligible[order(FinishPosition)][1]
    }
    
    # Generate random points within range
    points_to_assign <- round(runif(1, pt_low, pt_high), 2)
    points_to_assign <- min(points_to_assign, remaining_points)
    
    # Assign points
    if(points_to_assign > 0) {
      result[Name == selected_driver$Name, 
             FDDominatorPoints := FDDominatorPoints + points_to_assign]
      
      # Track and update remaining points
      drivers_with_points <- c(drivers_with_points, selected_driver$Name)
      remaining_points <- remaining_points - points_to_assign
    }
  }
  
  # Handle dominated-but-wrecked scenario
  if(remaining_points > (total_dom_points * 0.1)) {
    potential_dominators <- result[
      !(Name %in% drivers_with_points) &
        FDDomTier <= 2 &
        FinishPosition > 10
    ]
    
    if(nrow(potential_dominators) > 0) {
      # Select one with worse finish position
      selected <- potential_dominators[order(-FinishPosition)][1]
      
      # Assign significant portion of remaining points
      points_to_assign <- round(remaining_points * runif(1, 0.6, 0.9), 2)
      
      result[Name == selected$Name, 
             FDDominatorPoints := FDDominatorPoints + points_to_assign]
      
      remaining_points <- remaining_points - points_to_assign
    }
  }
  
  # Distribute remaining points to drivers who haven't received points
  if(remaining_points > 0.5) {
    potential_recipients <- result[
      !(Name %in% drivers_with_points) &
        FinishPosition <= 15 &
        FDDomTier <= 3
    ]
    
    if(nrow(potential_recipients) > 0) {
      # Sort by tier and finish position
      potential_recipients <- potential_recipients[order(FDDomTier, FinishPosition)]
      
      # Distribute to top finishers
      n_recipients <- min(3, nrow(potential_recipients))
      
      # Create exponentially decaying weights
      weights <- exp(-0.7 * (1:n_recipients - 1))
      weights <- weights / sum(weights)
      
      # Distribute points
      for(i in 1:n_recipients) {
        points_to_add <- round(remaining_points * weights[i], 2)
        
        if(points_to_add < 0.1) next
        
        result[Name == potential_recipients$Name[i], 
               FDDominatorPoints := FDDominatorPoints + points_to_add]
        
        remaining_points <- remaining_points - points_to_add
      }
    }
  }
  
  # Ensure total matches expected
  total_allocated <- sum(result$FDDominatorPoints, na.rm = TRUE)
  
  if(total_allocated > total_dom_points) {
    # Scale down proportionally
    scale_factor <- total_dom_points / total_allocated
    result[, FDDominatorPoints := round(FDDominatorPoints * scale_factor, 2)]
  } else if(total_allocated < total_dom_points && (total_dom_points - total_allocated) >= 0.5) {
    # Add remaining points to top dominator
    remainder <- total_dom_points - total_allocated
    top_dominator <- result[order(-FDDominatorPoints)][1]
    
    result[Name == top_dominator$Name, 
           FDDominatorPoints := FDDominatorPoints + remainder]
  }
  
  # Final rounding for consistency
  result[, FDDominatorPoints := round(FDDominatorPoints, 2)]
  
  return(result)
}

# FanDuel lap points assignment
assign_fd_lap_points <- function(race_results, fd_laps_data) {
  # Ensure both are data.tables
  setDT(race_results)
  setDT(fd_laps_data)
  
  # Create a copy to avoid modifying the original
  result <- copy(race_results)
  
  # Initialize lap points without affecting other columns
  result[, FDLapPoints := 0]
  
  # If no lap data, return early
  if(nrow(fd_laps_data) == 0) return(result)
  
  # Ensure necessary columns exist
  if(!"ps" %in% names(fd_laps_data)) {
    warning("No 'ps' column in lap data, returning zeros for lap points")
    return(result)
  }
  
  if(!"Pt" %in% names(fd_laps_data) && !all(c("PtLow", "PtHigh") %in% names(fd_laps_data))) {
    warning("No point columns found in lap data, returning zeros for lap points")
    return(result)
  }
  
  # Determine which column to use
  point_col <- if("Pt" %in% names(fd_laps_data)) "Pt" else "PtLow"
  
  # Ensure ps is numeric
  fd_laps_data[, ps := as.numeric(ps)]
  
  # Remove any NA rows
  fd_laps_data <- fd_laps_data[!is.na(ps) & !is.na(get(point_col))]
  
  # If no valid data after filtering, return early
  if(nrow(fd_laps_data) == 0) return(result)
  
  # Build a complete positions lookup table from 1 to max position
  max_pos <- max(result$FinishPosition, na.rm = TRUE)
  max_ps <- max(fd_laps_data$ps, na.rm = TRUE)
  
  # Create a lookup table for all possible positions
  all_positions <- data.table(
    position = 1:max(max_pos, max_ps)
  )
  
  # For each position in the lookup table, find the points value
  for(i in 1:nrow(all_positions)) {
    pos <- all_positions$position[i]
    
    # Try exact match first
    exact_match <- fd_laps_data[ps == pos]
    
    if(nrow(exact_match) > 0) {
      # Use exact match
      all_positions$points[i] <- exact_match[[point_col]][1]
    } else {
      # Find nearest lower value
      lower_match <- fd_laps_data[ps < pos][order(-ps)]
      if(nrow(lower_match) > 0) {
        # Use the nearest lower value
        all_positions$points[i] <- lower_match[[point_col]][1]
      } else {
        # No lower value, use the nearest higher
        higher_match <- fd_laps_data[ps > pos][order(ps)]
        if(nrow(higher_match) > 0) {
          all_positions$points[i] <- higher_match[[point_col]][1]
        } else {
          # No match at all, default to 0
          all_positions$points[i] <- 0
        }
      }
    }
  }
  
  # Enforce the non-increasing property
  for(i in 2:nrow(all_positions)) {
    if(all_positions$points[i] > all_positions$points[i-1]) {
      all_positions$points[i] <- all_positions$points[i-1]
    }
  }
  
  # Apply lap points to each driver based on finish position
  for(i in 1:nrow(result)) {
    if(!is.na(result$FinishPosition[i])) {
      pos <- result$FinishPosition[i]
      if(pos <= nrow(all_positions)) {
        result$FDLapPoints[i] <- all_positions$points[pos]
      } else {
        # Position out of range, use the last available value
        result$FDLapPoints[i] <- all_positions$points[nrow(all_positions)]
      }
    } else {
      # NA finish position gets 0 points
      result$FDLapPoints[i] <- 0
    }
  }
  
  # Ensure consistent precision
  result[, FDLapPoints := round(FDLapPoints, 1)]
  
  return(result)
}

# Main simulation function (optimized for memory)
run_integrated_simulations <- function(input_data, n_sims = 1000, batch_size = 100) {
  # Extract necessary data
  drivers_dt <- as.data.table(input_data$drivers)
  n_drivers <- nrow(drivers_dt)
  
  # Determine which platforms are active
  has_dk <- "DKSalary" %in% names(drivers_dt) && length(input_data$dk_dom_points) > 0
  has_fd <- "FDSalary" %in% names(drivers_dt) && length(input_data$fd_dom_points) > 0
  
  # Pre-calculate scoring vectors
  # DraftKings finish points
  dk_finish_points <- c(45, 42, 41, 40, 39, 38, 37, 36, 35, 34,
                        32, 31, 30, 29, 28, 27, 26, 25, 24, 23,
                        21, 20, 19, 18, 17, 16, 15, 14, 13, 12,
                        10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
  
  # FanDuel finish points
  fd_finish_points <- c(43, 40, 38, 37, 36, 35, 34, 33, 32, 31,
                        30, 29, 28, 27, 26, 25, 24, 23, 22, 21,
                        20, 19, 18, 17, 16, 15, 14, 13, 12, 11,
                        10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
  
  # Create a common base result structure
  results_columns <- c(
    "SimID", "Name", "Starting", "FinishPosition"
  )
  
  # Add platform-specific columns
  if(has_dk) {
    results_columns <- c(
      results_columns, 
      "DKSalary", "DKOP", "DKName", "DKDominatorPoints", "DKFantasyPoints"
    )
  }
  
  if(has_fd) {
    results_columns <- c(
      results_columns, 
      "FDSalary", "FDOP", "FDName", "FDDominatorPoints", "FDLapPoints", "FDFantasyPoints"
    )
  }
  
  # Pre-allocate results data.table with proper types and sizes
  results <- data.table(matrix(NA, nrow = n_drivers * n_sims, ncol = length(results_columns)))
  setnames(results, results_columns)
  
  # Set column types
  results[, SimID := integer(n_drivers * n_sims)]
  results[, Name := character(n_drivers * n_sims)]
  results[, Starting := numeric(n_drivers * n_sims)]
  results[, FinishPosition := integer(n_drivers * n_sims)]
  
  if(has_dk) {
    results[, DKSalary := numeric(n_drivers * n_sims)]
    results[, DKOP := numeric(n_drivers * n_sims)]
    results[, DKName := character(n_drivers * n_sims)]
    results[, DKDominatorPoints := numeric(n_drivers * n_sims)]
    results[, DKFantasyPoints := numeric(n_drivers * n_sims)]
  }
  
  if(has_fd) {
    results[, FDSalary := numeric(n_drivers * n_sims)]
    results[, FDOP := numeric(n_drivers * n_sims)]
    results[, FDName := character(n_drivers * n_sims)]
    results[, FDDominatorPoints := numeric(n_drivers * n_sims)]
    results[, FDLapPoints := numeric(n_drivers * n_sims)]
    results[, FDFantasyPoints := numeric(n_drivers * n_sims)]
  }
  
  # Pre-fill static data
  results[, SimID := rep(1:n_sims, each = n_drivers)]
  results[, Name := rep(drivers_dt$Name, n_sims)]
  results[, Starting := rep(drivers_dt$Starting, n_sims)]
  
  if(has_dk) {
    results[, DKSalary := rep(drivers_dt$DKSalary, n_sims)]
    results[, DKOP := rep(drivers_dt$DKOP, n_sims)]
    results[, DKName := rep(drivers_dt$DKName, n_sims)]
  }
  
  if(has_fd) {
    results[, FDSalary := rep(drivers_dt$FDSalary, n_sims)]
    results[, FDOP := rep(drivers_dt$FDOP, n_sims)]
    results[, FDName := rep(drivers_dt$FDName, n_sims)]
  }
  
  # Calculate batch size based on number of drivers
  batch_size <- min(batch_size, max(50, ceiling(5000 / n_drivers)))
  n_batches <- ceiling(n_sims / batch_size)
  
  cat("Starting simulation with", n_sims, "races...\n")
  
  for(batch in 1:n_batches) {
    batch_start <- Sys.time()
    
    start_sim <- (batch - 1) * batch_size + 1
    end_sim <- min(batch * batch_size, n_sims)
    current_batch_size <- end_sim - start_sim + 1
    
    # Calculate row indices for this batch
    start_idx <- (start_sim - 1) * n_drivers + 1
    end_idx <- start_idx + (current_batch_size * n_drivers) - 1
    
    cat("Processing batch", batch, "of", n_batches, "(simulations", start_sim, "to", end_sim, ")\n")
    
    # Process each simulation in the batch
    for(sim_offset in 1:current_batch_size) {
      sim <- start_sim + sim_offset - 1
      sim_start_idx <- start_idx + (sim_offset - 1) * n_drivers
      sim_end_idx <- sim_start_idx + n_drivers - 1
      
      # Simulate finishing positions for this race using the improved function
      finish_positions <- simulate_finishing_positions(drivers_dt)
      
      # Store finish positions
      results[sim_start_idx:sim_end_idx, FinishPosition := finish_positions]
      
      # Create single race result for assigning dominator points
      race_result <- data.table(
        Name = drivers_dt$Name,
        FinishPosition = finish_positions
      )
      
      # If DraftKings is active, calculate DK dominator points and fantasy points
      if(has_dk) {
        race_result$DKDomTier <- drivers_dt$DKDomTier
        
        # Assign dominator points
        dk_dom_result <- assign_dk_dominator_points(
          race_result, 
          input_data$dk_dominator,
          input_data$dk_dom_points[1]
        )
        
        # Store DK dominator points
        results[sim_start_idx:sim_end_idx, DKDominatorPoints := dk_dom_result$DKDominatorPoints]
        
        # Calculate DK fantasy points
        dk_fantasy_points <- dk_finish_points[pmin(finish_positions, length(dk_finish_points))] + 
          (drivers_dt$Starting - finish_positions) + 
          dk_dom_result$DKDominatorPoints
        
        # Store DK fantasy points
        results[sim_start_idx:sim_end_idx, DKFantasyPoints := dk_fantasy_points]
      }
      
      # If FanDuel is active, calculate FD dominator points, lap points, and fantasy points
      if(has_fd) {
        race_result$FDDomTier <- drivers_dt$FDDomTier
        
        # Assign FD dominator points
        fd_dom_result <- assign_fd_dominator_points(
          race_result, 
          input_data$fd_dominator,
          input_data$fd_dom_points[1]
        )
        
        # Store FD dominator points
        results[sim_start_idx:sim_end_idx, FDDominatorPoints := fd_dom_result$FDDominatorPoints]
        
        # Assign FD lap points
        fd_lap_result <- assign_fd_lap_points(fd_dom_result, input_data$fd_laps)
        
        # Store FD lap points
        results[sim_start_idx:sim_end_idx, FDLapPoints := fd_lap_result$FDLapPoints]
        
        # Calculate FD fantasy points (FanDuel place differential is * 0.5)
        fd_fantasy_points <- fd_finish_points[pmin(finish_positions, length(fd_finish_points))] + 
          ((drivers_dt$Starting - finish_positions) * 0.5) + 
          fd_dom_result$FDDominatorPoints +
          fd_lap_result$FDLapPoints
        
        # Store FD fantasy points
        results[sim_start_idx:sim_end_idx, FDFantasyPoints := fd_fantasy_points]
      }
    }
    
    # Report progress and clean up
    batch_end <- Sys.time()
    batch_time <- difftime(batch_end, batch_start, units = "secs")
    cat(sprintf("Batch %d/%d completed in %.1f seconds\n", 
                batch, n_batches, as.numeric(batch_time)))
    
    # Force garbage collection every batch to manage memory
    gc(verbose = FALSE, full = TRUE)
  }
  
  # Set key for better performance in subsequent operations
  setkey(results, SimID)
  
  # Return the results and platform availability info
  list(
    results = results,
    has_dk = has_dk,
    has_fd = has_fd
  )
}

# Analysis Functions (optimized with data.table)
analyze_finishing_positions <- function(sim_results) {
  setDT(sim_results)
  
  results <- sim_results[, .(
    Win_Rate = mean(FinishPosition == 1, na.rm = TRUE) * 100,
    T3_Rate = mean(FinishPosition <= 3, na.rm = TRUE) * 100,
    T5_Rate = mean(FinishPosition <= 5, na.rm = TRUE) * 100,
    T10_Rate = mean(FinishPosition <= 10, na.rm = TRUE) * 100,
    T15_Rate = mean(FinishPosition <= 15, na.rm = TRUE) * 100,
    T20_Rate = mean(FinishPosition <= 20, na.rm = TRUE) * 100,
    T25_Rate = mean(FinishPosition <= 25, na.rm = TRUE) * 100,
    T30_Rate = mean(FinishPosition <= 30, na.rm = TRUE) * 100,
    Avg_Finish = mean(FinishPosition, na.rm = TRUE),
    Median = median(FinishPosition, na.rm = TRUE)
  ), by = Name]
  
  results <- results[order(Avg_Finish)]
  
  for (col in setdiff(names(results), "Name")) {
    results[, (col) := round(get(col), 1)]
  }
  
  return(results)
}

analyze_dk_dominator_points <- function(sim_results) {
  setDT(sim_results)
  
  # Calculate dominator rank for each simulation
  sim_results[, DKDominatorRank := frank(-DKDominatorPoints, ties.method = "min"), by = SimID]
  
  results <- sim_results[, .(
    Starting = first(Starting),
    DKSalary = first(DKSalary),
    Avg_Dom = mean(DKDominatorPoints),
    Median_Dom = median(DKDominatorPoints),
    Max_Dom = max(DKDominatorPoints),
    Avg_DomRank = mean(DKDominatorRank),
    Median_DomRank = median(DKDominatorRank),
    Top_DomRate = mean(DKDominatorRank == 1) * 100,
    Top3_DomRate = mean(DKDominatorRank <= 3) * 100,
    Top5_DomRate = mean(DKDominatorRank <= 5) * 100,
    Top10_DomRate = mean(DKDominatorRank <= 10) * 100
  ), by = Name]
  
  # Ensure numeric columns before rounding
  numeric_cols <- c("Starting", "DKSalary", "Avg_Dom", "Median_Dom", "Max_Dom", 
                    "Avg_DomRank", "Median_DomRank", 
                    "Top_DomRate", "Top3_DomRate", "Top5_DomRate", "Top10_DomRate")
  
  # Convert to numeric and round to 1 decimal place
  for (col in numeric_cols) {
    results[, (col) := round(as.numeric(get(col)), 1)]
  }
  
  # Sort by Average Dominator Points in descending order
  setorder(results, -Avg_Dom)
  
  return(results)
}

analyze_fd_dominator_points <- function(sim_results) {
  setDT(sim_results)
  
  # Calculate dominator rank for each simulation
  sim_results[, FDDominatorRank := frank(-FDDominatorPoints, ties.method = "min"), by = SimID]
  
  results <- sim_results[, .(
    Starting = first(Starting),
    FDSalary = first(FDSalary),
    Avg_Dom = mean(FDDominatorPoints),
    Median_Dom = median(FDDominatorPoints),
    Max_Dom = max(FDDominatorPoints),
    Avg_DomRank = mean(FDDominatorRank),
    Median_DomRank = median(FDDominatorRank),
    Top_DomRate = mean(FDDominatorRank == 1) * 100,
    Top3_DomRate = mean(FDDominatorRank <= 3) * 100,
    Top5_DomRate = mean(FDDominatorRank <= 5) * 100
  ), by = Name]
  
  # Ensure numeric columns before rounding
  numeric_cols <- c("Starting", "FDSalary", "Avg_Dom", "Median_Dom", "Max_Dom", 
                    "Avg_DomRank", "Median_DomRank", 
                    "Top_DomRate", "Top3_DomRate", "Top5_DomRate")
  
  # Convert to numeric and round to 1 decimal place
  for (col in numeric_cols) {
    results[, (col) := round(as.numeric(get(col)), 1)]
  }
  
  # Sort by Average Dominator Points in descending order
  setorder(results, -Avg_Dom)
  
  return(results)
}

analyze_fd_lap_points <- function(sim_results) {
  setDT(sim_results)
  
  results <- sim_results[, .(
    Starting = first(Starting),
    FDSalary = first(FDSalary),
    Avg_Lap = mean(FDLapPoints),
    Median_Lap = median(FDLapPoints),
    Max_Lap = max(FDLapPoints),
    Min_Lap = min(FDLapPoints)
  ), by = Name]
  
  # Round numeric columns
  numeric_cols <- c("Starting", "FDSalary", "Avg_Lap", "Median_Lap", "Max_Lap", "Min_Lap")
  for (col in numeric_cols) {
    results[, (col) := round(as.numeric(get(col)), 1)]
  }
  
  # Sort by Average Lap Points in descending order
  setorder(results, -Avg_Lap)
  
  return(results)
}

# DraftKings fantasy point projections
analyze_dk_fantasy_points <- function(sim_results) {
  setDT(sim_results)
  
  results <- sim_results[, .(
    DKSalary = first(DKSalary),
    Starting = first(Starting),
    DKOP = first(DKOP),
    Median_Fantasy_Pts = round(median(DKFantasyPoints), 1),
    FP_90thPct = round(quantile(DKFantasyPoints, 0.9), 1)
  ), by = .(Name)]
  
  # Add PPD calculation
  results[, PPD := round(Median_Fantasy_Pts / (DKSalary/1000), 1)]
  
  # Multiply DKOP by 100 to convert to percentage if it's a proportion
  if(max(results$DKOP, na.rm = TRUE) <= 1) {
    results[, DKOP := round(as.numeric(DKOP) * 100, 1)]
  }
  
  return(results)
}

# FanDuel fantasy point projections
analyze_fd_fantasy_points <- function(sim_results) {
  setDT(sim_results)
  
  results <- sim_results[, .(
    FDSalary = first(FDSalary),
    Starting = first(Starting),
    FDOP = first(FDOP),
    Median_Fantasy_Pts = round(median(FDFantasyPoints), 1),
    FP_90thPct = round(quantile(FDFantasyPoints, 0.9), 1)
  ), by = .(Name)]
  
  # Add PPD calculation
  results[, PPD := round(Median_Fantasy_Pts / (FDSalary/1000), 1)]
  
  # Multiply FDOP by 100 to convert to percentage if it's a proportion
  if(max(results$FDOP, na.rm = TRUE) <= 1) {
    results[, FDOP := round(as.numeric(FDOP) * 100, 1)]
  }
  
  return(results)
}

# Accuracy analysis
analyze_simulation_accuracy <- function(sim_results, input_data) {
  # Make sure we're working with data.tables
  setDT(sim_results)
  setDT(input_data)
  
  # Get unique drivers
  drivers <- unique(input_data$Name)
  metrics <- c("W", "T3", "T5", "T10", "T15", "T20", "T25", "T30")
  
  results <- data.table()
  
  for(driver in drivers) {
    # Get simulation results for this driver
    driver_sims <- sim_results[Name == driver]
    driver_input <- input_data[Name == driver]
    
    total_sims <- nrow(driver_sims)
    if(total_sims == 0) next
    
    # Calculate simulated percentages
    sim_pcts <- c(
      W = sum(driver_sims$FinishPosition == 1) / total_sims,
      T3 = sum(driver_sims$FinishPosition <= 3) / total_sims,
      T5 = sum(driver_sims$FinishPosition <= 5) / total_sims,
      T10 = sum(driver_sims$FinishPosition <= 10) / total_sims,
      T15 = sum(driver_sims$FinishPosition <= 15) / total_sims,
      T20 = sum(driver_sims$FinishPosition <= 20) / total_sims,
      T25 = sum(driver_sims$FinishPosition <= 25) / total_sims,
      T30 = sum(driver_sims$FinishPosition <= 30) / total_sims
    )
    
    for(metric in metrics) {
      input_value <- driver_input[[metric]]
      sim_value <- sim_pcts[metric]
      
      if(!is.na(input_value)) {
        results <- rbind(results, data.table(
          Driver = driver,
          Metric = metric,
          Input = input_value * 100, # Convert to percentage
          Simulated = sim_value * 100, # Convert to percentage
          Difference = abs(input_value * 100 - sim_value * 100) # Convert to percentage
        ))
      }
    }
  }
  
  # Sort by driver and metric
  setorder(results, Driver, Metric)
  
  return(results)
}

# Lineup optimization functions
# DraftKings optimal lineup finder with memory optimizations
find_dk_optimal_lineups <- function(sim_data, k = 5) {
  # Ensure data.table
  setDT(sim_data)
  
  # Calculate PPD directly instead of adding a column to save memory
  # Use vector operations instead of adding columns 
  dk_fantasy_points <- sim_data$DKFantasyPoints
  dk_salary <- sim_data$DKSalary
  ppd_values <- dk_fantasy_points / (dk_salary/1000)
  
  # Find indices of top candidates directly without creating intermediate data frames
  top_points_idx <- order(-dk_fantasy_points)[1:min(15, length(dk_fantasy_points))]
  top_ppd_idx <- order(-ppd_values)[1:min(15, length(ppd_values))]
  
  # Combine indices without redundant copies
  candidate_idx <- unique(c(top_points_idx, top_ppd_idx))
  
  # Create candidates data frame directly from indices - only needed columns
  candidates <- sim_data[candidate_idx, .(DKName, DKSalary, DKFantasyPoints)]
  
  n <- nrow(candidates)
  if(n < DK_ROSTER_SIZE) return(NULL)
  
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
    const.mat[1, ] <- candidates$DKSalary  # Salary cap
    const.mat[2, ] <- 1                    # Roster size
    
    # Add previous lineup exclusions
    if(length(excluded_pairs) > 0) {
      for(j in 1:length(excluded_pairs)) {
        const.mat[2+j, excluded_pairs[[j]]] <- 1
      }
    }
    
    const.dir <- c("<=", "==", rep("<=", length(excluded_pairs)))
    const.rhs <- c(DK_SALARY_CAP, DK_ROSTER_SIZE, rep(DK_ROSTER_SIZE-1, length(excluded_pairs)))
    
    # Solve with error handling - minimal options for lower memory usage
    result <- tryCatch({
      suppressWarnings(
        lp("max", candidates$DKFantasyPoints, const.mat, const.dir, const.rhs, 
           all.bin = TRUE, presolve = 0, compute.sens = 0)
      )
    }, error = function(e) {
      NULL
    })
    
    if(is.null(result) || result$status != 0) break
    
    # Get selected drivers - avoid creating unnecessary vectors
    selected_indices <- which(result$solution > 0.9)
    
    if(length(selected_indices) != DK_ROSTER_SIZE) break
    
    # Create lineup string 
    selected_drivers <- sort(candidates$DKName[selected_indices])
    lineup_str <- paste(selected_drivers, collapse = "|")
    
    # Add to results
    lineup_count <- lineup_count + 1
    lineup_results$Lineup[lineup_count] <- lineup_str
    lineup_results$Rank[lineup_count] <- i
    
    # Track for diversity
    excluded_pairs[[length(excluded_pairs) + 1]] <- selected_indices
    
    # Free memory
    rm(result)
  }
  
  # Return only the valid results
  if(lineup_count == 0) return(NULL)
  if(lineup_count < k) {
    lineup_results <- lineup_results[1:lineup_count, , drop = FALSE]
  }
  
  return(lineup_results)
}

# FanDuel optimal lineup finder
# FanDuel optimal lineup finder with memory optimizations
find_fd_optimal_lineups <- function(sim_data, k = 5) {
  # Ensure data.table
  setDT(sim_data) %>% 
    filter(FDSalary > 0)
  
  # Calculate PPD directly instead of adding a column to save memory
  # Use vector operations instead of adding columns 
  fd_fantasy_points <- sim_data$FDFantasyPoints
  fd_salary <- sim_data$FDSalary
  ppd_values <- fd_fantasy_points / (fd_salary/1000)
  
  # Find indices of top candidates directly without creating intermediate data frames
  top_points_idx <- order(-fd_fantasy_points)[1:min(15, length(fd_fantasy_points))]
  top_ppd_idx <- order(-ppd_values)[1:min(15, length(ppd_values))]
  
  # Combine indices without redundant copies
  candidate_idx <- unique(c(top_points_idx, top_ppd_idx))
  
  # Create candidates data frame directly from indices - only needed columns
  candidates <- sim_data[candidate_idx, .(FDName, FDSalary, FDFantasyPoints)]
  
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
    
    # Solve with error handling
    result <- tryCatch({
      suppressWarnings(
        lp("max", candidates$FDFantasyPoints, const.mat, const.dir, const.rhs, 
           all.bin = TRUE, presolve = 0, compute.sens = 0)
      )
    }, error = function(e) {
      NULL
    })
    
    if(is.null(result) || result$status != 0) break
    
    # Get selected drivers
    selected_indices <- which(result$solution > 0.9)
    
    if(length(selected_indices) != FD_ROSTER_SIZE) break
    
    # Create lineup string (sorted IDs for consistent identification)
    selected_drivers <- sort(candidates$FDName[selected_indices])
    lineup_str <- paste(selected_drivers, collapse = "|")
    
    # Add to results
    lineup_count <- lineup_count + 1
    lineup_results$Lineup[lineup_count] <- lineup_str
    lineup_results$Rank[lineup_count] <- i
    
    # Track for diversity
    excluded_pairs[[length(excluded_pairs) + 1]] <- selected_indices
  }
  
  # Return only the valid results
  if(lineup_count == 0) return(NULL)
  if(lineup_count < k) {
    lineup_results <- lineup_results[1:lineup_count, , drop = FALSE]
  }
  
  return(lineup_results)
}

# Optimized count optimal lineups function - DraftKings
count_dk_optimal_lineups <- function(sim_results) {
  # Always use top_k=5
  top_k <- 5
  
  # Create data.table for better performance
  sim_results_dt <- as.data.table(sim_results)
  
  # Extract only necessary columns to reduce memory usage
  sim_results_dt <- sim_results_dt[, .(
    SimID, Name, DKName, DKSalary, DKFantasyPoints
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
        all_lineups[[sim_idx]] <- find_dk_optimal_lineups(chunk_sim_list[[i]], k = top_k)
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
  
  # Pre-compute a salary lookup for efficiency
  unique_drivers <- unique(sim_results_dt$DKName)
  salary_lookup <- sim_results_dt[, .SD[1], by = DKName, .SDcols = "DKSalary"]
  setkey(salary_lookup, DKName)
  
  # Calculate total salary more efficiently
  lineup_data$TotalSalary <- sapply(lineup_data$Lineup, function(lineup_str) {
    drivers <- strsplit(lineup_str, "\\|")[[1]]
    salaries <- salary_lookup[drivers, on = "DKName", nomatch = 0]$DKSalary
    sum(salaries, na.rm = TRUE)
  })
  
  # Sort by Top1Count
  lineup_data <- lineup_data[order(-lineup_data$Top1Count), ]
  
  # Split driver columns for display
  driver_cols <- do.call(rbind, strsplit(lineup_data$Lineup, "\\|"))
  
  # Validate column count
  if(ncol(driver_cols) != DK_ROSTER_SIZE) {
    warning(paste("Expected", DK_ROSTER_SIZE, "driver columns, got", ncol(driver_cols)))
    return(NULL)
  }
  
  colnames(driver_cols) <- paste0("Driver", 1:DK_ROSTER_SIZE)
  
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

# Optimized count optimal lineups function - FanDuel
# Optimized count optimal lineups function - FanDuel
count_fd_optimal_lineups <- function(sim_results) {
  # Always use top_k=5
  top_k <- 5
  
  # Create data.table for better performance
  sim_results_dt <- as.data.table(sim_results)
  
  # Extract only necessary columns to reduce memory usage
  sim_results_dt <- sim_results_dt[, .(
    SimID, Name, FDName, FDSalary, FDFantasyPoints
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
  
  # Pre-compute a salary lookup for efficiency
  unique_drivers <- unique(sim_results_dt$FDName)
  salary_lookup <- sim_results_dt[, .SD[1], by = FDName, .SDcols = "FDSalary"]
  setkey(salary_lookup, FDName)
  
  # Calculate total salary more efficiently
  lineup_data$TotalSalary <- sapply(lineup_data$Lineup, function(lineup_str) {
    drivers <- strsplit(lineup_str, "\\|")[[1]]
    salaries <- salary_lookup[drivers, on = "FDName", nomatch = 0]$FDSalary
    sum(salaries, na.rm = TRUE)
  })
  
  # Sort by Top1Count
  lineup_data <- lineup_data[order(-lineup_data$Top1Count), ]
  
  # Split driver columns for display
  driver_cols <- do.call(rbind, strsplit(lineup_data$Lineup, "\\|"))
  
  # Validate column count
  if(ncol(driver_cols) != FD_ROSTER_SIZE) {
    warning(paste("Expected", FD_ROSTER_SIZE, "driver columns, got", ncol(driver_cols)))
    return(NULL)
  }
  
  colnames(driver_cols) <- paste0("Driver", 1:FD_ROSTER_SIZE)
  
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
  
  # Apply driver exclusion filter
  if (!is.null(filters$excluded_drivers) && length(filters$excluded_drivers) > 0) {
    driver_cols <- paste0("Driver", 1:DK_ROSTER_SIZE)
    
    # More efficient driver exclusion
    filtered_lineups <- filtered_lineups[!rowSums(sapply(driver_cols, function(col) {
      filtered_lineups[[col]] %in% filters$excluded_drivers
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
  
  # Apply driver exclusion filter
  if (!is.null(filters$excluded_drivers) && length(filters$excluded_drivers) > 0) {
    driver_cols <- paste0("Driver", 1:FD_ROSTER_SIZE)
    
    # More efficient driver exclusion
    filtered_lineups <- filtered_lineups[!rowSums(sapply(driver_cols, function(col) {
      filtered_lineups[[col]] %in% filters$excluded_drivers
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

generate_random_dk_lineups <- function(optimal_lineups, filters) {
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
  
  # Exclude specific drivers
  if (!is.null(filters$excluded_drivers) && length(filters$excluded_drivers) > 0) {
    driver_cols <- paste0("Driver", 1:DK_ROSTER_SIZE)
    to_exclude <- logical(nrow(filtered_lineups))
    
    for(col in driver_cols) {
      to_exclude <- to_exclude | filtered_lineups[[col]] %in% filters$excluded_drivers
    }
    
    filtered_lineups <- filtered_lineups[!to_exclude]
  }
  
  if (nrow(filtered_lineups) == 0) {
    return(NULL)
  }
  
  # Pre-allocate for tracking
  driver_cols <- paste0("Driver", 1:DK_ROSTER_SIZE)
  all_drivers <- unique(unlist(filtered_lineups[, ..driver_cols]))
  driver_counts <- setNames(numeric(length(all_drivers)), all_drivers)
  
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
    
    # Get drivers from this lineup
    candidate_drivers <- unlist(candidate_lineup[, ..driver_cols])
    
    # Update driver counts - no exposure limit check
    driver_counts <- driver_counts + table(factor(candidate_drivers, levels = names(driver_counts)))
    
    # Add lineup unconditionally (no max exposure check)
    selected_lineups <- rbind(selected_lineups, candidate_lineup)
    selected_indices <- c(selected_indices, selected_idx)
    
    # Periodically clean up
    if (attempts %% 100 == 0) gc(verbose = FALSE)
  }
  
  if (nrow(selected_lineups) == 0) return(NULL)
  
  # Calculate exposure for attribute
  final_exposure <- (driver_counts / nrow(selected_lineups)) * 100
  attr(selected_lineups, "exposure") <- final_exposure
  
  return(as.data.frame(selected_lineups))
}


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
  
  # Exclude specific drivers
  if (!is.null(filters$excluded_drivers) && length(filters$excluded_drivers) > 0) {
    driver_cols <- paste0("Driver", 1:FD_ROSTER_SIZE)
    to_exclude <- logical(nrow(filtered_lineups))
    
    for(col in driver_cols) {
      to_exclude <- to_exclude | filtered_lineups[[col]] %in% filters$excluded_drivers
    }
    
    filtered_lineups <- filtered_lineups[!to_exclude]
  }
  
  if (nrow(filtered_lineups) == 0) {
    return(NULL)
  }
  
  # Pre-allocate for tracking
  driver_cols <- paste0("Driver", 1:FD_ROSTER_SIZE)
  all_drivers <- unique(unlist(filtered_lineups[, ..driver_cols]))
  driver_counts <- setNames(numeric(length(all_drivers)), all_drivers)
  
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
    
    # Get drivers from this lineup
    candidate_drivers <- unlist(candidate_lineup[, ..driver_cols])
    
    # Update driver counts - no exposure limit check
    driver_counts <- driver_counts + table(factor(candidate_drivers, levels = names(driver_counts)))
    
    # Add lineup unconditionally (no max exposure check)
    selected_lineups <- rbind(selected_lineups, candidate_lineup)
    selected_indices <- c(selected_indices, selected_idx)
    
    # Periodically clean up
    if (attempts %% 100 == 0) gc(verbose = FALSE)
  }
  
  if (nrow(selected_lineups) == 0) return(NULL)
  
  # Calculate exposure for attribute
  final_exposure <- (driver_counts / nrow(selected_lineups)) * 100
  attr(selected_lineups, "exposure") <- final_exposure
  
  return(as.data.frame(selected_lineups))
}

calculate_dk_driver_exposure <- function(optimal_lineups, fantasy_analysis, random_lineups = NULL) {
  # Quick validation
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame(Message = "No optimal lineups available."))
  }
  
  # Use data.table operations
  setDT(optimal_lineups)
  if (!is.null(fantasy_analysis)) setDT(fantasy_analysis)
  if (!is.null(random_lineups)) setDT(random_lineups)
  
  # Driver columns
  driver_cols <- grep("^Driver", names(optimal_lineups), value = TRUE)
  if(length(driver_cols) == 0) {
    return(data.frame(Message = "No driver columns found in lineups."))
  }
  
  # Get all unique drivers (these are DKNames)
  all_drivers <- unique(unlist(optimal_lineups[, ..driver_cols]))
  
  # Initialize metrics data frame - use DKName as primary identifier
  metrics_data <- data.table(
    DKName = all_drivers,  # This is the driver ID from lineups
    Name = NA_character_,  # This will be the actual driver name
    DKSalary = NA_real_,
    DKOP = NA_real_,
    OptimalRate = 0,
    EliteRate = 0,
    FloorRate = 0,
    AppearanceRate = 0,
    Exposure = 0,
    Leverage = 0,
    Starting = NA_real_,
    Proj = NA_real_
  )
  
  # Calculate all rate metrics using DKName as the identifier
  # Calculate OptimalRate (percentage of Top1Count lineups with this driver)
  total_top1 <- sum(optimal_lineups$Top1Count, na.rm = TRUE)
  if(total_top1 > 0) {
    for(driver in all_drivers) {
      # Find lineups with this driver
      driver_appears <- logical(nrow(optimal_lineups))
      for(col in driver_cols) {
        driver_appears <- driver_appears | (optimal_lineups[[col]] == driver)
      }
      driver_matches <- which(driver_appears)
      
      # Calculate optimal rate percentage
      driver_total <- sum(optimal_lineups$Top1Count[driver_matches], na.rm = TRUE)
      metrics_data[DKName == driver, OptimalRate := (driver_total / total_top1) * 100]
    }
  }
  
  # Calculate EliteRate (top 10% of lineups by Top1Count)
  if(nrow(optimal_lineups) >= 10) {
    elite_lineups <- copy(optimal_lineups)
    elite_lineups <- elite_lineups[order(-Top1Count, -Top5Count)]
    
    n_elite <- max(1, round(nrow(elite_lineups) * 0.1))
    elite_lineups <- elite_lineups[1:n_elite]
    
    for(driver in all_drivers) {
      # Count appearances in elite lineups
      driver_appears <- logical(nrow(elite_lineups))
      for(col in driver_cols) {
        driver_appears <- driver_appears | (elite_lineups[[col]] == driver)
      }
      driver_elite_count <- sum(driver_appears)
      
      # Calculate elite rate
      metrics_data[DKName == driver, EliteRate := (driver_elite_count / n_elite) * 100]
    }
  }
  
  # Calculate FloorRate (top 20% of lineups by Top5Count)
  if(nrow(optimal_lineups) >= 5) {
    floor_lineups <- copy(optimal_lineups)
    floor_lineups <- floor_lineups[order(-Top5Count, -Top1Count)]
    
    n_floor <- max(1, round(nrow(floor_lineups) * 0.2))
    floor_lineups <- floor_lineups[1:n_floor]
    
    for(driver in all_drivers) {
      # Count appearances in floor lineups
      driver_appears <- logical(nrow(floor_lineups))
      for(col in driver_cols) {
        driver_appears <- driver_appears | (floor_lineups[[col]] == driver)
      }
      driver_floor_count <- sum(driver_appears)
      
      # Calculate floor rate
      metrics_data[DKName == driver, FloorRate := (driver_floor_count / n_floor) * 100]
    }
  }
  
  # Calculate AppearanceRate (percentage of all lineups with this driver)
  if(nrow(optimal_lineups) > 0) {
    for(driver in all_drivers) {
      # Count appearances in all lineups
      driver_appears <- logical(nrow(optimal_lineups))
      for(col in driver_cols) {
        driver_appears <- driver_appears | (optimal_lineups[[col]] == driver)
      }
      driver_appearance_count <- sum(driver_appears)
      
      # Calculate appearance rate
      metrics_data[DKName == driver, AppearanceRate := (driver_appearance_count / nrow(optimal_lineups)) * 100]
    }
  }
  
  # Calculate Exposure from random lineups
  if(!is.null(random_lineups) && nrow(random_lineups) > 0) {
    random_driver_cols <- grep("^Driver", names(random_lineups), value = TRUE)
    if(length(random_driver_cols) > 0) {
      for(driver in all_drivers) {
        driver_appears <- logical(nrow(random_lineups))
        for(col in random_driver_cols) {
          driver_appears <- driver_appears | (random_lineups[[col]] == driver)
        }
        metrics_data[DKName == driver, Exposure := (sum(driver_appears) / nrow(random_lineups)) * 100]
      }
    }
  }
  
  # Add driver information from fantasy analysis - now we match based on DKName
  if(!is.null(fantasy_analysis) && nrow(fantasy_analysis) > 0) {
    # Check if this is our custom mapping format
    is_mapping_format <- "DKName" %in% names(fantasy_analysis) && "Name" %in% names(fantasy_analysis)
    
    if(is_mapping_format) {
      # This is our custom mapping with DKName as key
      for(i in 1:nrow(metrics_data)) {
        dk_name <- metrics_data$DKName[i]
        match_idx <- which(fantasy_analysis$DKName == dk_name)
        
        if(length(match_idx) > 0) {
          idx <- match_idx[1]
          metrics_data$Name[i] <- fantasy_analysis$Name[idx]
          metrics_data$DKSalary[i] <- fantasy_analysis$DKSalary[idx]
          metrics_data$DKOP[i] <- fantasy_analysis$DKOP[idx]
          metrics_data$Starting[i] <- fantasy_analysis$Starting[idx]
          metrics_data$Proj[i] <- fantasy_analysis$Proj[idx]
        }
      }
    } else if("DKName" %in% names(fantasy_analysis)) {
      # This is regular fantasy analysis data with DKName
      for(i in 1:nrow(metrics_data)) {
        dk_name <- metrics_data$DKName[i]
        match_idx <- which(fantasy_analysis$DKName == dk_name)
        
        if(length(match_idx) > 0) {
          idx <- match_idx[1]
          metrics_data$Name[i] <- fantasy_analysis$Name[idx]
          metrics_data$DKSalary[i] <- fantasy_analysis$DKSalary[idx]
          metrics_data$DKOP[i] <- fantasy_analysis$DKOP[idx]
          metrics_data$Starting[i] <- fantasy_analysis$Starting[idx]
          if("Median_Fantasy_Pts" %in% names(fantasy_analysis))
            metrics_data$Proj[i] <- fantasy_analysis$Median_Fantasy_Pts[idx]
        }
      }
    }
  }
  
  # Check if DKOP is already in percentage format (0-100)
  if(!is.null(metrics_data$DKOP) && !all(is.na(metrics_data$DKOP))) {
    if(max(metrics_data$DKOP, na.rm = TRUE) <= 1) {
      # Convert to percentage for consistent leverage calculation
      metrics_data[, DKOP := DKOP * 100]
    }
  }
  
  # Calculate leverage
  metrics_data[!is.na(DKOP) & !is.na(Exposure), Leverage := Exposure - DKOP]
  
  # Sort by OptimalRate
  setorder(metrics_data, -OptimalRate)
  
  return(as.data.frame(metrics_data))
}

# Fixed FanDuel driver exposure calculation
calculate_fd_driver_exposure <- function(optimal_lineups, fantasy_analysis, random_lineups = NULL) {
  # Quick validation
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame(Message = "No optimal lineups available."))
  }
  
  # Use data.table operations
  setDT(optimal_lineups)
  if (!is.null(fantasy_analysis)) setDT(fantasy_analysis)
  if (!is.null(random_lineups)) setDT(random_lineups)
  
  # Driver columns
  driver_cols <- grep("^Driver", names(optimal_lineups), value = TRUE)
  if(length(driver_cols) == 0) {
    return(data.frame(Message = "No driver columns found in lineups."))
  }
  
  # Get all unique drivers (these are FDNames)
  all_drivers <- unique(unlist(optimal_lineups[, ..driver_cols]))
  
  # Initialize metrics data frame - use FDName as primary identifier
  metrics_data <- data.table(
    FDName = all_drivers,  # This is the driver ID from lineups
    Name = NA_character_,  # This will be the actual driver name
    FDSalary = NA_real_,
    FDOP = NA_real_,
    OptimalRate = 0,
    EliteRate = 0,
    FloorRate = 0,
    AppearanceRate = 0,
    Exposure = 0,
    Leverage = 0,
    Starting = NA_real_,
    Proj = NA_real_
  )
  
  # Calculate OptimalRate (percentage of Top1Count lineups with this driver)
  total_top1 <- sum(optimal_lineups$Top1Count, na.rm = TRUE)
  if(total_top1 > 0) {
    for(driver in all_drivers) {
      # Find lineups with this driver
      driver_appears <- logical(nrow(optimal_lineups))
      for(col in driver_cols) {
        driver_appears <- driver_appears | (optimal_lineups[[col]] == driver)
      }
      driver_matches <- which(driver_appears)
      
      # Calculate optimal rate percentage
      driver_total <- sum(optimal_lineups$Top1Count[driver_matches], na.rm = TRUE)
      metrics_data[FDName == driver, OptimalRate := (driver_total / total_top1) * 100]
    }
  }
  
  # Calculate EliteRate (top 10% of lineups by Top1Count)
  if(nrow(optimal_lineups) >= 10) {
    elite_lineups <- copy(optimal_lineups)
    elite_lineups <- elite_lineups[order(-Top1Count, -Top5Count)]
    
    n_elite <- max(1, round(nrow(elite_lineups) * 0.1))
    elite_lineups <- elite_lineups[1:n_elite]
    
    for(driver in all_drivers) {
      # Count appearances in elite lineups
      driver_appears <- logical(nrow(elite_lineups))
      for(col in driver_cols) {
        driver_appears <- driver_appears | (elite_lineups[[col]] == driver)
      }
      driver_elite_count <- sum(driver_appears)
      
      # Calculate elite rate
      metrics_data[FDName == driver, EliteRate := (driver_elite_count / n_elite) * 100]
    }
  }
  
  # Calculate FloorRate (top 20% of lineups by Top5Count)
  if(nrow(optimal_lineups) >= 5) {
    floor_lineups <- copy(optimal_lineups)
    floor_lineups <- floor_lineups[order(-Top5Count, -Top1Count)]
    
    n_floor <- max(1, round(nrow(floor_lineups) * 0.2))
    floor_lineups <- floor_lineups[1:n_floor]
    
    for(driver in all_drivers) {
      # Count appearances in floor lineups
      driver_appears <- logical(nrow(floor_lineups))
      for(col in driver_cols) {
        driver_appears <- driver_appears | (floor_lineups[[col]] == driver)
      }
      driver_floor_count <- sum(driver_appears)
      
      # Calculate floor rate
      metrics_data[FDName == driver, FloorRate := (driver_floor_count / n_floor) * 100]
    }
  }
  
  # Calculate AppearanceRate (percentage of all lineups with this driver)
  if(nrow(optimal_lineups) > 0) {
    for(driver in all_drivers) {
      # Count appearances in all lineups
      driver_appears <- logical(nrow(optimal_lineups))
      for(col in driver_cols) {
        driver_appears <- driver_appears | (optimal_lineups[[col]] == driver)
      }
      driver_appearance_count <- sum(driver_appears)
      
      # Calculate appearance rate
      metrics_data[FDName == driver, AppearanceRate := (driver_appearance_count / nrow(optimal_lineups)) * 100]
    }
  }
  
  # Calculate Exposure from random lineups
  if(!is.null(random_lineups) && nrow(random_lineups) > 0) {
    random_driver_cols <- grep("^Driver", names(random_lineups), value = TRUE)
    if(length(random_driver_cols) > 0) {
      for(driver in all_drivers) {
        driver_appears <- logical(nrow(random_lineups))
        for(col in random_driver_cols) {
          driver_appears <- driver_appears | (random_lineups[[col]] == driver)
        }
        metrics_data[FDName == driver, Exposure := (sum(driver_appears) / nrow(random_lineups)) * 100]
      }
    }
  }
  
  # Add driver information from fantasy analysis - now we match based on FDName
  if(!is.null(fantasy_analysis) && nrow(fantasy_analysis) > 0) {
    # Check if this is our custom mapping format
    is_mapping_format <- "FDName" %in% names(fantasy_analysis) && "Name" %in% names(fantasy_analysis)
    
    if(is_mapping_format) {
      # This is our custom mapping with FDName as key
      for(i in 1:nrow(metrics_data)) {
        fd_name <- metrics_data$FDName[i]
        match_idx <- which(fantasy_analysis$FDName == fd_name)
        
        if(length(match_idx) > 0) {
          idx <- match_idx[1]
          metrics_data$Name[i] <- fantasy_analysis$Name[idx]
          metrics_data$FDSalary[i] <- fantasy_analysis$FDSalary[idx]
          metrics_data$FDOP[i] <- fantasy_analysis$FDOP[idx]
          metrics_data$Starting[i] <- fantasy_analysis$Starting[idx]
          metrics_data$Proj[i] <- fantasy_analysis$Proj[idx]
        }
      }
    } else if("FDName" %in% names(fantasy_analysis)) {
      # This is regular fantasy analysis data with FDName
      for(i in 1:nrow(metrics_data)) {
        fd_name <- metrics_data$FDName[i]
        match_idx <- which(fantasy_analysis$FDName == fd_name)
        
        if(length(match_idx) > 0) {
          idx <- match_idx[1]
          metrics_data$Name[i] <- fantasy_analysis$Name[idx]
          metrics_data$FDSalary[i] <- fantasy_analysis$FDSalary[idx]
          metrics_data$FDOP[i] <- fantasy_analysis$FDOP[idx]
          metrics_data$Starting[i] <- fantasy_analysis$Starting[idx]
          if("Median_Fantasy_Pts" %in% names(fantasy_analysis))
            metrics_data$Proj[i] <- fantasy_analysis$Median_Fantasy_Pts[idx]
        }
      }
    }
  }
  
  # Check if FDOP is already in percentage format (0-100)
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
# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(title = "NASCAR Fantasy Sims"),
  
  # Dashboard sidebar
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Input Check", tabName = "upload", icon = icon("upload")),
      menuItem("Finish Analysis", tabName = "finish_analysis", icon = icon("chart-line")),
      menuItem("Dominator Analysis", tabName = "dominator", icon = icon("trophy")),
      menuItem("Fantasy Projections", tabName = "fantasy", icon = icon("calculator")),
      menuItem("Optimal Lineups", tabName = "optimal_lineups", icon = icon("trophy")),
      menuItem("Lineup Builder", tabName = "lineup_builder", icon = icon("percentage"))
    ),
    br(),
    fileInput("excel_file", "Upload Excel File", accept = c(".xlsx")),
    numericInput("n_sims", "Number of Simulations:", value = 10000, min = 100, max = 25000),
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
      
      # Finish Analysis Tab
      tabItem(tabName = "finish_analysis",
              fluidRow(
                div(style = "text-align: right; margin: 10px 15px;",
                    downloadButton('downloadResults', 'Download Full Simulation Results', 
                                   style = "margin-top: 5px;"))
              ),
              
              fluidRow(
                box(width = 12,
                    title = "Simulated Finishing Results",
                    DTOutput("driver_stats") %>% withSpinner(color = "#ff6600")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Finish Position Boxplot",
                    plotlyOutput("position_box", height = "1000px") %>% withSpinner(color = "#ff6600")
                )
              )
      ),
      
      # Dominator Analysis Tab
      tabItem(tabName = "dominator",
              uiOutput("dominator_ui")
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
                  height = "100px", # Set a specific height to make it thinner
                  title = "Run Lineup Optimization",
                  status = "primary",
                  solidHeader = TRUE,
                  div(
                    style = "display: flex; gap: 10px; margin: 5px 0;", # Reduced margins
                    conditionalPanel(
                      condition = "output.has_draftkings === 'true'",
                      actionButton("run_dk_optimization", "Calculate DraftKings Lineups", # Shortened text
                                   class = "btn-primary", 
                                   style = "flex: 1; padding: 3px; font-size: 12px;") # Smaller padding and font
                    ),
                    conditionalPanel(
                      condition = "output.has_fanduel === 'true'",
                      actionButton("run_fd_optimization", "Calculate FanDuel Lineups", # Shortened text
                                   class = "btn-primary", 
                                   style = "flex: 1; padding: 3px; font-size: 12px;") # Smaller padding and font
                    )
                  ),
                  conditionalPanel(
                    condition = "output.has_draftkings != 'true' && output.has_fanduel != 'true'",
                    div(
                      class = "alert alert-warning",
                      style = "margin: 0; padding: 5px; font-size: 11px;", # Smaller alert
                      "No fantasy platform data detected. Check your input file."
                    )
                  )
                )
              ),
              # Add additional fluidRows here for showing the results
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
    finishing_analysis = NULL,
    dk_dominator_analysis = NULL,
    fd_dominator_analysis = NULL,
    fd_lap_analysis = NULL,
    dk_fantasy_analysis = NULL,
    fd_fantasy_analysis = NULL,
    dk_optimal_lineups = NULL,
    fd_optimal_lineups = NULL,
    dk_driver_exposure = NULL,
    fd_driver_exposure = NULL,
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
        rv$finishing_analysis <- NULL
        rv$dk_dominator_analysis <- NULL
        rv$fd_dominator_analysis <- NULL
        rv$fd_lap_analysis <- NULL
        rv$dk_fantasy_analysis <- NULL
        rv$fd_fantasy_analysis <- NULL
        rv$dk_optimal_lineups <- NULL
        rv$fd_optimal_lineups <- NULL
        rv$dk_driver_exposure <- NULL
        rv$fd_driver_exposure <- NULL
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
          display_data <- rv$input_data$sheets$Driver
          
          # Remove the specified columns if they exist
          columns_to_remove <- c("DKName", "FDName", "DKID", "FDName", "FDID")
          for(col in columns_to_remove) {
            if(col %in% colnames(display_data)) {
              display_data[[col]] <- NULL
            }
          }
          
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
          if("DKOP" %in% colnames(display_data)) {
            dt <- dt %>% formatRound("DKOP", digits = 2)
          }
          
          if("FDOP" %in% colnames(display_data)) {
            dt <- dt %>% formatRound("FDOP", digits = 2)
          }
          
          # Format W, T3, T5, etc. columns to 2 decimal places
          numeric_cols <- c("W", "T3", "T5", "T10", "T15", "T20", "T25", "T30")
          for(col in numeric_cols) {
            if(col %in% colnames(display_data)) {
              dt <- dt %>% formatRound(col, digits = 2)
            }
          }
          
          # Format salary columns as currency
          if("DKSalary" %in% colnames(display_data)) {
            dt <- dt %>% formatCurrency("DKSalary", currency = "$", digits = 0)
          }
          
          if("FDSalary" %in% colnames(display_data)) {
            dt <- dt %>% formatCurrency("FDSalary", currency = "$", digits = 0)
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
    rv$finishing_analysis <- NULL
    rv$dk_dominator_analysis <- NULL
    rv$fd_dominator_analysis <- NULL
    rv$fd_lap_analysis <- NULL
    rv$dk_fantasy_analysis <- NULL
    rv$fd_fantasy_analysis <- NULL
    rv$dk_optimal_lineups <- NULL
    rv$fd_optimal_lineups <- NULL
    rv$dk_driver_exposure <- NULL
    rv$fd_driver_exposure <- NULL
    rv$dk_random_lineups <- NULL
    rv$fd_random_lineups <- NULL
    
    # Force garbage collection before running new simulation
    gc(verbose = FALSE, full = TRUE)
    
    # Show progress dialog
    withProgress(message = 'Running simulations...', value = 0, {
      # Run the simulations
      setProgress(0.1, detail = "Initializing simulation...")
      
      simulation_results <- run_integrated_simulations(
        rv$processed_data, 
        n_sims = input$n_sims,
        batch_size = 100
      )
      
      # Store results
      rv$simulation_results <- simulation_results$results
      
      # Remove dominator rank columns to save memory
      if("DKDominatorRank" %in% names(rv$simulation_results)) {
        rv$simulation_results$DKDominatorRank <- NULL
      }
      if("FDDominatorRank" %in% names(rv$simulation_results)) {
        rv$simulation_results$FDDominatorRank <- NULL
      }
      
      # Update platform availability
      rv$has_draftkings <- "DKFantasyPoints" %in% names(rv$simulation_results)
      rv$has_fanduel <- "FDFantasyPoints" %in% names(rv$simulation_results)
      
      # Force garbage collection
      gc(verbose = FALSE, full = TRUE)
      
      # Process finish position analysis
      setProgress(0.7, detail = "Analyzing finishing positions...")
      rv$finishing_analysis <- analyze_finishing_positions(rv$simulation_results)
      
      # Process dominator points analysis for each platform
      setProgress(0.8, detail = "Analyzing dominator points...")
      if(rv$has_draftkings) {
        rv$dk_dominator_analysis <- analyze_dk_dominator_points(rv$simulation_results)
      }
      
      if(rv$has_fanduel) {
        rv$fd_dominator_analysis <- analyze_fd_dominator_points(rv$simulation_results)
        rv$fd_lap_analysis <- analyze_fd_lap_points(rv$simulation_results)
      }
      
      # Process fantasy points analysis for each platform
      setProgress(0.9, detail = "Analyzing fantasy points...")
      if(rv$has_draftkings) {
        rv$dk_fantasy_analysis <- analyze_dk_fantasy_points(rv$simulation_results)
      }
      
      if(rv$has_fanduel) {
        rv$fd_fantasy_analysis <- analyze_fd_fantasy_points(rv$simulation_results)
      }
      
      # Mark simulation as complete
      rv$simulation_complete <- TRUE
      
      # Switch to finish analysis tab
      updateTabItems(session, "sidebar_menu", selected = "upload")
      
      # Show success message
      showModal(modalDialog(
        title = "Success",
        "Simulation completed successfully! Review the accuracy analysis and projections or move to optimal lineup creation.",
        easyClose = TRUE
      ))
      
      # Final cleanup
      gc(verbose = FALSE, full = TRUE)
    })
  })
  

  
  output$upload_content <- renderUI({
    if(rv$simulation_complete) {
      # Show accuracy analysis after simulation is complete
      tagList(
        fluidRow(
          box(width = 12,
              title = "Simulation Accuracy Analysis",
              DTOutput("accuracy_analysis") %>% withSpinner(color = "#ff6600"),
              downloadButton('downloadAccuracy', 'Download Accuracy Analysis')
          )
        )
      )
    } else {
      # Show input data before simulation is run
      tagList(
        fluidRow(
          box(width = 12,
              title = "Input Data",
              DTOutput("data_preview") %>% withSpinner(color = "#ff6600")
          )
        ),
        uiOutput("available_platforms")
      )
    }
  })
  
  # Accuracy analysis output
  output$accuracy_analysis <- renderDT({
    req(rv$simulation_results, rv$processed_data$drivers)
    
    # Calculate accuracy metrics
    accuracy_data <- analyze_simulation_accuracy(rv$simulation_results, rv$processed_data$drivers)
    
    # Create an informative summary view
    datatable(
      accuracy_data,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        order = list(list(4, 'desc')), # Sort by difference in descending order
        columnDefs = list(
          list(targets = c(2, 3, 4), className = 'dt-right')
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatRound(c('Input', 'Simulated', 'Difference'), digits = 2) %>%
      formatStyle(
        'Difference',
        background = styleColorBar(c(0, max(accuracy_data$Difference)), 'rgba(255, 102, 0, 0.5)'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Driver stats output
  output$driver_stats <- renderDT({
    req(rv$finishing_analysis)
    
    analysis_data <- rv$finishing_analysis
    
    # Add Starting position and salary data from input data
    if (!is.null(rv$processed_data$drivers)) {
      drivers_data <- as.data.table(rv$processed_data$drivers)
      
      # Identify available columns to join
      join_cols <- c("Name", "Starting")
      
      # Add salary columns based on available platforms
      if(rv$has_draftkings && "DKSalary" %in% names(drivers_data)) {
        join_cols <- c(join_cols, "DKSalary")
      }
      
      if(rv$has_fanduel && "FDSalary" %in% names(drivers_data)) {
        join_cols <- c(join_cols, "FDSalary")
      }
      
      # Join data with available columns
      if(length(join_cols) > 1) {
        # Filter to only include columns that exist
        join_cols <- intersect(join_cols, names(drivers_data))
        
        # Join with available columns
        analysis_data <- merge(
          analysis_data,
          drivers_data[, ..join_cols],
          by = "Name",
          all.x = TRUE
        )
        
        # Create dynamic column order based on available columns
        col_order <- c("Name", "Starting")
        
        # Add salary columns if available
        if("DKSalary" %in% names(analysis_data)) {
          col_order <- c(col_order, "DKSalary")
        }
        if("FDSalary" %in% names(analysis_data)) {
          col_order <- c(col_order, "FDSalary")
        }
        
        # Add remaining columns
        col_order <- c(
          col_order,
          "Avg_Finish", "Median", 
          "Win_Rate", "T3_Rate", "T5_Rate", "T10_Rate", 
          "T15_Rate", "T20_Rate", "T25_Rate", "T30_Rate"
        )
        
        # Ensure all columns exist
        col_order <- intersect(col_order, names(analysis_data))
        
        # Use proper data.table syntax
        analysis_data <- analysis_data[, ..col_order]
      }
    }
    
    # Prepare the table with formatting
    dt <- datatable(
      analysis_data,
      options = list(
        scrollX = TRUE, 
        pageLength = -1,  # Show all rows
        dom = "t",  # Only show table ('t'), no search/pagination
        order = list(list(4, 'asc')),  # Sort by Avg_Finish in ascending order
        columnDefs = list(
          list(
            targets = "_all",
            className = 'dt-center'
          )
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) 
    
    # Format Salary columns
    if ("DKSalary" %in% names(analysis_data)) {
      dt <- dt %>% formatCurrency('DKSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    if ("FDSalary" %in% names(analysis_data)) {
      dt <- dt %>% formatCurrency('FDSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    dt
  })
  
  # Generate dynamic dominator UI based on available platforms
  output$dominator_ui <- renderUI({
    req(rv$simulation_results)
    
    # Create appropriate UI based on available platforms
    if(rv$has_draftkings && rv$has_fanduel) {
      # Both platforms available - use tabs
      tabsetPanel(
        id = "dominator_tabs",
        tabPanel(
          "DraftKings",
          fluidRow(
            box(width = 12,
                title = "DraftKings Dominator Points Analysis",
                DTOutput("dk_dominator_stats") %>% withSpinner(color = "#ff6600")
            )
          ),
          
          fluidRow(
            box(width = 12,
                title = "DraftKings Dominator Points Distribution",
                plotlyOutput("dk_dominator_dist", height = "1000px") %>% withSpinner(color = "#ff6600")
            )
          ),
          fluidRow(
            box(width = 12,
                title = "DraftKings Dominator Points by Position",
                plotlyOutput("dk_points_by_position", height = "700px") %>% withSpinner(color = "#ff6600")
            )
          )
        ),
        tabPanel(
          "FanDuel",
          fluidRow(
            box(width = 12,
                title = "FanDuel Dominator Points Analysis",
                DTOutput("fd_dominator_stats") %>% withSpinner(color = "#ff6600")
            )
          ),
          fluidRow(
            box(width = 12,
                title = "FanDuel Dominator Points Distribution",
                plotlyOutput("fd_dominator_dist", height = "800px") %>% withSpinner(color = "#ff6600")
            )
          ),
          fluidRow(
            box(width = 12,
                title = "FanDuel Dominator Points by Position",
                plotlyOutput("fd_points_by_position", height = "700px") %>% withSpinner(color = "#ff6600")
            )
          ),
          fluidRow(
            box(width = 12,
                title = "FanDuel Lap Points by Position",
                plotlyOutput("fd_lap_points_by_position") %>% withSpinner(color = "#ff6600")
            )
          )
        )
      )
    } else if(rv$has_draftkings) {
      # Only DraftKings available
      tagList(
        fluidRow(
          box(width = 12,
              title = "DraftKings Dominator Points Analysis",
              DTOutput("dk_dominator_stats") %>% withSpinner(color = "#ff6600")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Dominator Points Distribution",
              plotlyOutput("dk_dominator_dist", height = "1000px") %>% withSpinner(color = "#ff6600")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Dominator Points by Position",
              plotlyOutput("dk_points_by_position", height = "700px") %>% withSpinner(color = "#ff6600")
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
  
  # Update the fantasy_ui output function to remove the optimization section
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
                DTOutput("dk_fantasy_projections") %>% withSpinner(color = "#ff6600"),
                downloadButton('download_dk_fantasy_projections', 'Download Projections')
            )
          ),
          fluidRow(
            box(width = 12,
                title = "DraftKings Fantasy Points vs Salary",
                plotlyOutput("dk_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#ff6600")
            )
          ),
          fluidRow(
            box(width = 12,
                title = "DraftKings Fantasy Points Distribution",
                plotlyOutput("dk_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#ff6600")
            )
          )
        ),
        tabPanel(
          "FanDuel",
          fluidRow(
            box(width = 12,
                title = "FanDuel Fantasy Point Projections",
                DTOutput("fd_fantasy_projections") %>% withSpinner(color = "#ff6600"),
                downloadButton('download_fd_fantasy_projections', 'Download Projections')
            )
          ),
          fluidRow(
            box(width = 12,
                title = "FanDuel Fantasy Points vs Salary",
                plotlyOutput("fd_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#ff6600")
            )
          ),
          fluidRow(
            box(width = 12,
                title = "FanDuel Fantasy Points Distribution",
                plotlyOutput("fd_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#ff6600")
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
              DTOutput("dk_fantasy_projections") %>% withSpinner(color = "#ff6600"),
              downloadButton('download_dk_fantasy_projections', 'Download Projections')
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Fantasy Points vs Salary",
              plotlyOutput("dk_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#ff6600")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Fantasy Points Distribution",
              plotlyOutput("dk_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#ff6600")
          )
        )
      )
    } else if(rv$has_fanduel) {
      # Only FanDuel available
      tagList(
        fluidRow(
          box(width = 12,
              title = "FanDuel Fantasy Point Projections",
              DTOutput("fd_fantasy_projections") %>% withSpinner(color = "#ff6600"),
              downloadButton('download_fd_fantasy_projections', 'Download Projections')
          )
        ),
        fluidRow(
          box(width = 12,
              title = "FanDuel Fantasy Points vs Salary",
              plotlyOutput("fd_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#ff6600")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "FanDuel Fantasy Points Distribution",
              plotlyOutput("fd_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#ff6600")
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
              DTOutput("dk_lineup_count_thresholds") %>% withSpinner(color = "#ff6600")
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
                       selectizeInput("dk_excluded_drivers", "Exclude Drivers:",
                                      choices = NULL,  # This will be populated when lineups are calculated
                                      multiple = TRUE,
                                      options = list(
                                        plugins = list('remove_button'),
                                        placeholder = 'Click to select drivers to exclude'
                                      ))
                ),
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
          title = "Understanding Driver Rates",
          status = "info",
          solidHeader = TRUE,
          p("These rate statistics help you understand each driver's presence in lineups:"),
          tags$ul(
            tags$li(tags$strong("OptimalRate:"), "How often drivers appear in lineups that finished 1st (optimal) in at least one simulation"),
            tags$li(tags$strong("EliteRate:"), "How often drivers appear in the top 200 lineups when ranked by Top1Count (with Top5Count as tiebreaker)"),
            tags$li(tags$strong("FloorRate:"), "How often drivers appear in the top 1000 lineups when ranked by Top5Count"),
            tags$li(tags$strong("AppearanceRate:"), "How often drivers appear across all lineups in the full pool"),
            tags$li(tags$strong("Exposure:"), "How often drivers appear in your selected randomized lineups")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "DraftKings Driver Exposure Analysis",
              DTOutput("dk_driver_exposure_table") %>% withSpinner(color = "#ff6600")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "Generated DraftKings Lineups",
              DTOutput("dk_random_lineups_table") %>% withSpinner(color = "#ff6600")
          )
        )
      ),
      
      # FanDuel lineup builder UI
      conditionalPanel(
        condition = "output.has_fd_lineups == 'true'",
        fluidRow(
          box(width = 12,
              title = "FanDuel Lineup Count Thresholds",
              DTOutput("fd_lineup_count_thresholds") %>% withSpinner(color = "#ff6600")
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
                selectizeInput("fd_excluded_drivers", "Exclude Drivers:",
                               choices = NULL,  # This will be populated when lineups are calculated
                               multiple = TRUE,
                               options = list(
                                 plugins = list('remove_button'),
                                 placeholder = 'Click to select drivers to exclude'
                               ))
              ,
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
          title = "Understanding Driver Rates",
          status = "info",
          solidHeader = TRUE,
          p("These rate statistics help you understand each driver's presence in lineups:"),
          tags$ul(
            tags$li(tags$strong("OptimalRate:"), "How often drivers appear in lineups that finished 1st (optimal) in at least one simulation"),
            tags$li(tags$strong("EliteRate:"), "How often drivers appear in the top 200 lineups when ranked by Top1Count (with Top5Count as tiebreaker)"),
            tags$li(tags$strong("FloorRate:"), "How often drivers appear in the top 1000 lineups when ranked by Top5Count"),
            tags$li(tags$strong("AppearanceRate:"), "How often drivers appear across all lineups in the full pool"),
            tags$li(tags$strong("Exposure:"), "How often drivers appear in your selected randomized lineups")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "FanDuel Driver Exposure Analysis",
              DTOutput("fd_driver_exposure_table") %>% withSpinner(color = "#ff6600")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "Generated FanDuel Lineups",
              DTOutput("fd_random_lineups_table") %>% withSpinner(color = "#ff6600")
          )
        )
      )
    )
  })
  
  # Create position boxplot
  output$position_box <- renderPlotly({
    req(rv$simulation_results)
    
    # Get all unique drivers and starting positions
    drivers_info <- unique(rv$simulation_results[, c("Name", "Starting")])
    
    # Order by starting position
    drivers_info <- drivers_info[order(drivers_info$Starting), ]
    
    # Get ordered list of driver names
    ordered_drivers <- drivers_info$Name
    
    # Plot with all drivers, ordered by starting position
    p <- ggplot(rv$simulation_results, aes(x = factor(Name, levels = ordered_drivers), 
                                           y = FinishPosition,
                                           fill = Name)) +
      geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 2) +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
        legend.position = "none"  # Hide legend to reduce clutter
      ) +
      labs(
        x = "Driver", 
        y = "Finish Position",
        title = NULL
      )
    
    ggplotly(p, height = 1000) %>%  
      layout(
        showlegend = FALSE,
        margin = list(
          l = 150,
          r = 40,
          b = 60,
          t = 30,
          pad = 5
        ),
        font = list(
          family = "Arial",
          size = 12
        )
      )
  })
  
  # DraftKings Dominator Stats
  output$dk_dominator_stats <- renderDT({
    req(rv$dk_dominator_analysis)
    
    dt <- datatable(
      rv$dk_dominator_analysis,
      options = list(
        scrollX = TRUE, 
        pageLength = -1,  # Show all rows
        dom = "t",  # Only show table ('t'), no search/pagination
        order = list(list(3, 'desc')),  # Sort by Avg_Dom
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
      formatRound(c('Avg_Dom', 'Median_Dom', 'Max_Dom', 
                    'Avg_DomRank', 'Median_DomRank', 
                    'Top_DomRate', 'Top3_DomRate', 'Top5_DomRate',
                    'Top10_DomRate'), 
                  digits = 1)
    
    dt
  })
  
  # FanDuel Dominator Stats
  output$fd_dominator_stats <- renderDT({
    req(rv$fd_dominator_analysis)
    
    dt <- datatable(
      rv$fd_dominator_analysis,
      options = list(
        scrollX = TRUE, 
        pageLength = -1,  # Show all rows
        dom = "t",  # Only show table ('t'), no search/pagination
        order = list(list(3, 'desc')),  # Sort by Avg_Dom
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
      formatRound(c('Avg_Dom', 'Median_Dom', 'Max_Dom', 
                    'Avg_DomRank', 'Median_DomRank', 
                    'Top_DomRate', 'Top3_DomRate', 'Top5_DomRate'), 
                  digits = 1)
    
    dt
  })
  
  # DraftKings Dominator distribution plot
  output$dk_dominator_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Calculate median DKDominatorPoints for each driver
    driver_medians <- rv$simulation_results %>%
      group_by(Name) %>%
      summarize(median_points = median(DKDominatorPoints, na.rm = TRUE)) %>%
      filter(median_points > 0)
    
    # Filter original data to only include drivers with median > 0
    plot_data <- rv$simulation_results %>%
      filter(Name %in% driver_medians$Name)
    
    # Create box and whisker plot
    p <- ggplot(plot_data, aes(x = reorder(Name, DKDominatorPoints, median), y = DKDominatorPoints, fill = Name)) +
      geom_boxplot(outlier.shape = NA) +  # Hide outliers if too noisy
      coord_flip()+
      theme_minimal() +
      labs(x = "Driver", y = "Dominator Points") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    
    ggplotly(p, height = 1000)
  })
  
  # FanDuel Dominator distribution plot
  output$fd_dominator_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Calculate median DKDominatorPoints for each driver
    driver_means <- rv$simulation_results %>%
      group_by(Name) %>%
      summarize(mean_points = mean(FDDominatorPoints, na.rm = TRUE)) %>%
      filter(mean_points > 0)
    
    # Filter original data to only include drivers with median > 0
    plot_data <- rv$simulation_results %>%
      filter(Name %in% driver_means$Name)
    
    # Create box and whisker plot
    p <- ggplot(plot_data, aes(x = reorder(Name, FDDominatorPoints, mean), y = FDDominatorPoints, fill = Name)) +
      geom_boxplot(outlier.shape = NA) +  # Hide outliers if too noisy
      coord_flip()+
      theme_minimal() +
      labs(x = "Driver", y = "Dominator Points") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
    
    ggplotly(p, height = 800)
  })
  
  # DraftKings Points by Position
  output$dk_points_by_position <- renderPlotly({
    req(rv$simulation_results)
    
    p <- ggplot(rv$simulation_results, 
                aes(x = factor(FinishPosition), y = DKDominatorPoints)) +
      geom_boxplot(fill = "lightblue", color = "darkblue") +
      geom_smooth(method = "lm", color = "red", se = FALSE, aes(group = 1)) +
      theme_minimal() +
      labs(x = "Finish Position", y = "Dominator Points")
    
    ggplotly(p, height = 700)
  })
  
  # FanDuel Points by Position
  output$fd_points_by_position <- renderPlotly({
    req(rv$simulation_results)
    
    p <- ggplot(rv$simulation_results, 
                aes(x = factor(FinishPosition), y = FDDominatorPoints)) +
      geom_boxplot(fill = "lightblue", color = "darkblue") +
      geom_smooth(method = "lm", color = "red", se = FALSE, aes(group = 1)) +
      theme_minimal() +
      labs(x = "Finish Position", y = "Dominator Points")
    
    ggplotly(p, height = 700)
  })
  
  # FanDuel Lap Points by Position
  output$fd_lap_points_by_position <- renderPlotly({
    req(rv$simulation_results)
    
    p <- ggplot(rv$simulation_results, 
                aes(x = factor(FinishPosition), y = FDLapPoints)) +
      geom_boxplot(fill = "lightgreen", color = "darkgreen") +
      theme_minimal() +
      labs(x = "Finish Position", y = "Lap Points")
    
    ggplotly(p)
  })
  
  # DraftKings Fantasy Projections
  output$dk_fantasy_projections <- renderDT({
    req(rv$dk_fantasy_analysis)
    
    dt <- datatable(
      rv$dk_fantasy_analysis,
      options = list(
        scrollX = TRUE, 
        pageLength = -1,  # Show all rows
        dom = "t",  # Only show table ('t'), no search/pagination
        order = list(list(4, 'desc')),  # Sort by median
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
      formatRound(c('Median_Fantasy_Pts', 
                    'FP_90thPct', 'PPD'), 
                  digits = 1)
    
    dt
  })
  
  # FanDuel Fantasy Projections
  output$fd_fantasy_projections <- renderDT({
    req(rv$fd_fantasy_analysis)
    
    dt <- datatable(
      rv$fd_fantasy_analysis,
      options = list(
        scrollX = TRUE, 
        pageLength = -1,  # Show all rows
        dom = "t",  # Only show table ('t'), no search/pagination
        order = list(list(4, 'desc')),  # Sort by median
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
      formatRound(c('Median_Fantasy_Pts', 
                    'FP_90thPct', 'PPD'), 
                  digits = 1)
    
    dt
  })
  
  # DraftKings Fantasy Points vs Salary
  output$dk_fantasy_points_salary <- renderPlotly({
    req(rv$dk_fantasy_analysis)
    
    plot_data <- rv$dk_fantasy_analysis
    
    # Combine Name and Starting into a label
    plot_data$label <- paste0(plot_data$Name, " (Start: ", plot_data$Starting, ")")
    
    p <- ggplot(plot_data, aes(
      x = DKSalary,
      y = Median_Fantasy_Pts,
      size = DKOP,
      text = label
    )) +
      geom_point(alpha = 0.7, color = "#2c7fb8") +
      geom_smooth(aes(x = DKSalary, y = Median_Fantasy_Pts), method = "lm", se = FALSE, color = "darkblue") +
      theme_minimal() +
      labs(x = "DK Salary", y = "Median Fantasy Points", size = "DKOP")
    
    ggplotly(p, height = 800, tooltip = c("text", "x", "y", "size")) %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        hovermode = "closest"
      )
  })
  
  # FanDuel Fantasy Points vs Salary
  output$fd_fantasy_points_salary <- renderPlotly({
    req(rv$fd_fantasy_analysis)
    
    plot_data <- rv$fd_fantasy_analysis
    
    # Combine Name and Starting into a label
    plot_data$label <- paste0(plot_data$Name, " (Start: ", plot_data$Starting, ")")
    
    p <- ggplot(plot_data, aes(
      x = FDSalary,
      y = Median_Fantasy_Pts,
      size = FDOP,
      text = label
    )) +
      geom_point(alpha = 0.7, color = "#2c7fb8") +
      geom_smooth(aes(x = FDSalary, y = Median_Fantasy_Pts), method = "lm", se = FALSE, color = "darkblue") +
      theme_minimal() +
      labs(x = "FD Salary", y = "Median Fantasy Points", size = "FDOP")
    
    ggplotly(p, height = 800, tooltip = c("text", "x", "y", "size")) %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        hovermode = "closest"
      )
  })
  
  # DraftKings Fantasy Points Distribution
  output$dk_fantasy_points_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Get unique driver salary info
    driver_salaries <- rv$simulation_results %>%
      distinct(Name, DKSalary)
    
    # Order driver names by ascending DKSalary
    ordered_names <- driver_salaries %>%
      arrange(DKSalary) %>%
      pull(Name)
    
    plot_data <- rv$simulation_results %>%
      filter(Name %in% ordered_names)
    
    p <- ggplot(plot_data, aes(x = factor(Name, levels = ordered_names),
                               y = DKFantasyPoints,
                               fill = Name)) +
      geom_boxplot(outlier.alpha = 0.25) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Driver", y = "Fantasy Points") +
      theme(legend.position = "none")  # Hide legend to avoid clutter
    
    ggplotly(p, height = 700, tooltip = c("x", "y"))
  })
  
  # FanDuel Fantasy Points Distribution
  output$fd_fantasy_points_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Get unique driver salary info
    driver_salaries <- rv$simulation_results %>%
      distinct(Name, FDSalary)
    
    # Order driver names by ascending DKSalary
    ordered_names <- driver_salaries %>%
      arrange(FDSalary) %>%
      pull(Name)
    
    plot_data <- rv$simulation_results %>%
      filter(Name %in% ordered_names)
    
    p <- ggplot(plot_data, aes(x = factor(Name, levels = ordered_names),
                               y = FDFantasyPoints,
                               fill = Name)) +
      geom_boxplot(outlier.alpha = 0.25) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Driver", y = "Fantasy Points") +
      theme(legend.position = "none")  # Hide legend to avoid clutter
    
    ggplotly(p, height = 700, tooltip = c("x", "y")) 
  })
  
  
  # Calculate DraftKings optimal lineup
  # DraftKings optimization button handler
  observeEvent(input$run_dk_optimization, {
    req(rv$simulation_results, rv$has_draftkings)
    
    # Clear previous analysis results but keep simulation data
    rv$dk_optimal_lineups <- NULL
    rv$dk_driver_exposure <- NULL
    rv$dk_random_lineups <- NULL
    
    # Also clear FanDuel lineups if they exist
    rv$fd_optimal_lineups <- NULL
    rv$fd_driver_exposure <- NULL
    rv$fd_random_lineups <- NULL
    
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
      
      # Create a driver mapping from the simulation results
      setProgress(0.7, detail = "Creating driver mapping...")
      driver_mapping <- NULL
      if(!is.null(rv$dk_optimal_lineups)) {
        # Get all unique drivers from optimal lineups
        driver_cols <- paste0("Driver", 1:DK_ROSTER_SIZE)
        
        # FIXED: Use a more generic approach that works with both data.frame and data.table
        lineup_drivers <- c()
        for(col in driver_cols) {
          if(col %in% names(rv$dk_optimal_lineups)) {
            lineup_drivers <- c(lineup_drivers, rv$dk_optimal_lineups[[col]])
          }
        }
        lineup_drivers <- unique(lineup_drivers)
        
        driver_mapping <- data.frame(
          DKName = lineup_drivers,  # Using DKName as primary key
          Name = NA_character_,
          DKSalary = NA_real_,
          DKOP = NA_real_,
          Starting = NA_real_,
          Proj = NA_real_
        )
        
        # Get mapping from simulation results
        unique_sim_drivers <- rv$simulation_results[!duplicated(rv$simulation_results$DKName), 
                                                    c("DKName", "Name", "DKSalary", "DKOP", "Starting")]
        
        # Match each driver from lineups to the simulation results
        for(i in 1:nrow(driver_mapping)) {
          dk_name <- driver_mapping$DKName[i]
          # Try to find this driver in the simulation results
          matches <- which(unique_sim_drivers$DKName == dk_name)
          if(length(matches) > 0) {
            match_idx <- matches[1]
            driver_mapping$Name[i] <- unique_sim_drivers$Name[match_idx]
            driver_mapping$DKSalary[i] <- unique_sim_drivers$DKSalary[match_idx]
            driver_mapping$DKOP[i] <- unique_sim_drivers$DKOP[match_idx]
            driver_mapping$Starting[i] <- unique_sim_drivers$Starting[match_idx]
            
            # Get projection from fantasy analysis
            if(!is.null(rv$dk_fantasy_analysis)) {
              name_match <- which(rv$dk_fantasy_analysis$Name == unique_sim_drivers$Name[match_idx])
              if(length(name_match) > 0) {
                driver_mapping$Proj[i] <- rv$dk_fantasy_analysis$Median_Fantasy_Pts[name_match[1]]
              }
            }
          }
        }
      }
      
      # Calculate initial driver exposure with the mapping
      setProgress(0.8, detail = "Calculating driver exposure...")
      if(!is.null(rv$dk_optimal_lineups)) {
        if(!is.null(driver_mapping) && nrow(driver_mapping) > 0) {
          rv$dk_driver_exposure <- calculate_dk_driver_exposure(
            rv$dk_optimal_lineups, 
            driver_mapping
          )
        } else {
          rv$dk_driver_exposure <- calculate_dk_driver_exposure(
            rv$dk_optimal_lineups, 
            rv$dk_fantasy_analysis
          )
        }
      }
      
      # Remove the processing modal
      removeModal()
    })
    
    # Update excluded drivers selection for the lineup builder
    if(!is.null(rv$dk_optimal_lineups) && !is.null(rv$dk_driver_exposure)) {
      # Get all drivers from the driver exposure data
      driver_data <- rv$dk_driver_exposure
      
      # Create basic named vector for choices
      driver_names <- driver_data$Name
      driver_ids <- driver_data$DKName
      
      # Create simple labels with name and optimal rate
      driver_labels <- paste0(driver_names, " (", round(driver_data$OptimalRate, 1), "%)")
      
      # Create choices with names
      driver_choices <- setNames(driver_ids, driver_labels)
      
      # Update the select input with choices
      updateSelectizeInput(
        session = session,
        inputId = "dk_excluded_drivers",
        choices = driver_choices,
        selected = character(0)  # Empty selection initially
      )
      
      
      
      
      # Show success message
      showModal(modalDialog(
        title = "Success",
        HTML(sprintf(
          "Successfully generated <b>%d</b> optimal lineups for DraftKings!<br><br>
      You can now go to the <b>Lineup Builder</b> tab to filter and select lineups from this pool.",
          nrow(rv$dk_optimal_lineups)
        )),
        easyClose = TRUE
      ))
    }
  })
  
  
  # Calculate FanDuel optimal lineup
  observeEvent(input$run_fd_optimization, {
    req(rv$simulation_results, rv$has_fanduel)
    
    # Clear previous analysis results but keep simulation data
    rv$fd_optimal_lineups <- NULL
    rv$fd_driver_exposure <- NULL
    rv$fd_random_lineups <- NULL
    
    # Also clear DraftKings lineups if they exist
    rv$dk_optimal_lineups <- NULL
    rv$dk_driver_exposure <- NULL
    rv$dk_random_lineups <- NULL
    
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
      
      # Create a driver mapping from the simulation results
      setProgress(0.7, detail = "Creating driver mapping...")
      driver_mapping <- NULL
      if(!is.null(rv$fd_optimal_lineups)) {
        # Get all unique drivers from optimal lineups
        driver_cols <- paste0("Driver", 1:FD_ROSTER_SIZE)
        
        # Use a generic approach that works with both data.frame and data.table
        lineup_drivers <- c()
        for(col in driver_cols) {
          if(col %in% names(rv$fd_optimal_lineups)) {
            lineup_drivers <- c(lineup_drivers, rv$fd_optimal_lineups[[col]])
          }
        }
        lineup_drivers <- unique(lineup_drivers)
        
        driver_mapping <- data.frame(
          FDName = lineup_drivers,  # Using FDName as primary key
          Name = NA_character_,
          FDSalary = NA_real_,
          FDOP = NA_real_,
          Starting = NA_real_,
          Proj = NA_real_
        )
        
        # Get mapping from simulation results
        unique_sim_drivers <- rv$simulation_results[!duplicated(rv$simulation_results$FDName), 
                                                    c("FDName", "Name", "FDSalary", "FDOP", "Starting")]
        
        # Match each driver from lineups to the simulation results
        for(i in 1:nrow(driver_mapping)) {
          fd_name <- driver_mapping$FDName[i]
          # Try to find this driver in the simulation results
          matches <- which(unique_sim_drivers$FDName == fd_name)
          if(length(matches) > 0) {
            match_idx <- matches[1]
            driver_mapping$Name[i] <- unique_sim_drivers$Name[match_idx]
            driver_mapping$FDSalary[i] <- unique_sim_drivers$FDSalary[match_idx]
            driver_mapping$FDOP[i] <- unique_sim_drivers$FDOP[match_idx]
            driver_mapping$Starting[i] <- unique_sim_drivers$Starting[match_idx]
            
            # Get projection from fantasy analysis
            if(!is.null(rv$fd_fantasy_analysis)) {
              name_match <- which(rv$fd_fantasy_analysis$Name == unique_sim_drivers$Name[match_idx])
              if(length(name_match) > 0) {
                driver_mapping$Proj[i] <- rv$fd_fantasy_analysis$Median_Fantasy_Pts[name_match[1]]
              }
            }
          }
        }
      }
      
      # Calculate initial driver exposure with the mapping
      setProgress(0.8, detail = "Calculating driver exposure...")
      if(!is.null(rv$fd_optimal_lineups)) {
        if(!is.null(driver_mapping) && nrow(driver_mapping) > 0) {
          rv$fd_driver_exposure <- calculate_fd_driver_exposure(
            rv$fd_optimal_lineups, 
            driver_mapping
          )
        } else {
          rv$fd_driver_exposure <- calculate_fd_driver_exposure(
            rv$fd_optimal_lineups, 
            rv$fd_fantasy_analysis
          )
        }
      }
      
      # Remove the processing modal
      removeModal()
    })
    
    # Update excluded drivers selection for the lineup builder
    if(!is.null(rv$fd_optimal_lineups) && !is.null(rv$fd_driver_exposure)) {
      # Get all drivers from the driver exposure data
      driver_data <- rv$fd_driver_exposure
      
      # Create basic named vector for choices
      driver_names <- driver_data$Name
      driver_ids <- driver_data$FDName
      
      # Create simple labels with name and optimal rate
      driver_labels <- paste0(driver_names, " (", round(driver_data$OptimalRate, 1), "%)")
      
      # Create choices with names
      driver_choices <- setNames(driver_ids, driver_labels)
      
      # Update the select input with choices
      updateSelectizeInput(
        session = session,
        inputId = "fd_excluded_drivers",
        choices = driver_choices,
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
  
  output$dk_optimal_lineups_table <- renderDT({
    req(rv$dk_optimal_lineups)
    
    # Clone lineups for display
    display_data <- as.data.table(rv$dk_optimal_lineups)
    
    # Format driver columns to show names
    if(!is.null(rv$dk_fantasy_analysis)) {
      for(i in 1:DK_ROSTER_SIZE) {
        col <- paste0("Driver", i)
        display_data[[col]] <- sapply(display_data[[col]], function(id) {
          match_idx <- which(rv$dk_fantasy_analysis$DKID == id)
          if(length(match_idx) > 0) {
            rv$dk_fantasy_analysis$Name[match_idx[1]]
          } else {
            id
          }
        })
      }
    }
    
    # Remove Rank columns, keep only TopX Count columns
    cols_to_keep <- c(paste0("Driver", 1:DK_ROSTER_SIZE), 
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
    top1_idx <- which(names(display_data) == "Top1Count") -1  # 0-based index for JS
    top5_idx <- which(names(display_data) == "Top5Count") -1   # 0-based index for JS
    
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
  
  output$fd_optimal_lineups_table <- renderDT({
    req(rv$fd_optimal_lineups)
    
    # Clone lineups for display
    display_data <- as.data.table(rv$fd_optimal_lineups)
    
    # Format driver columns to show names
    if(!is.null(rv$fd_fantasy_analysis)) {
      for(i in 1:FD_ROSTER_SIZE) {
        col <- paste0("Driver", i)
        display_data[[col]] <- sapply(display_data[[col]], function(id) {
          match_idx <- which(rv$fd_fantasy_analysis$FDName == id)
          if(length(match_idx) > 0) {
            rv$fd_fantasy_analysis$Name[match_idx[1]]
          } else {
            id
          }
        })
      }
    }
    
    # Remove Rank columns, keep only TopX Count columns
    cols_to_keep <- c(paste0("Driver", 1:FD_ROSTER_SIZE), 
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
      min_top5_count = input$dk_min_top5_count,
      excluded_drivers = input$dk_excluded_drivers
    )
    
    stats <- calculate_dk_filtered_pool_stats(rv$dk_optimal_lineups, filters)
    paste("Number of lineups in filtered pool:", stats$count)
  })
  
  # Download handlers
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("nascar_simulation_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
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
      write.csv(analyze_simulation_accuracy(rv$simulation_results, rv$processed_data$drivers), file, row.names = FALSE)
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
      # Format data for download (convert to data.frame first to avoid data.table issues)
      download_data <- as.data.frame(rv$dk_optimal_lineups)
      
      # Keep only driver columns, TopX Count columns, and TotalSalary
      cols_to_keep <- c(paste0("Driver", 1:DK_ROSTER_SIZE), 
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
      # Format data for download (convert to data.frame first to avoid data.table issues)
      download_data <- as.data.frame(rv$fd_optimal_lineups)
      
      # Keep only driver columns, TopX Count columns, and TotalSalary
      cols_to_keep <- c(paste0("Driver", 1:FD_ROSTER_SIZE), 
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
  
  # Generate random DraftKings lineups
  observeEvent(input$generate_dk_lineups, {
    req(rv$dk_optimal_lineups)
    
    # Create filters for lineup generation
    filters <- list(
      min_top1_count = input$dk_min_top1_count,
      min_top2_count = input$dk_min_top2_count,
      min_top3_count = input$dk_min_top3_count,
      min_top5_count = input$dk_min_top5_count,
      excluded_drivers = input$dk_excluded_drivers,
      num_lineups = input$dk_num_random_lineups
    )
    
    # Show progress
    withProgress(message = 'Generating lineups...', value = 0, {
      # Generate random lineups
      rv$dk_random_lineups <- generate_random_dk_lineups(rv$dk_optimal_lineups, filters)
      
      # Update driver exposure data using the same mapping approach from optimization
      if(!is.null(rv$dk_random_lineups)) {
        # First, let's preserve the existing driver mapping data
        existing_mapping <- NULL
        
        # Check if the current driver exposure table has mapping data
        if(!is.null(rv$dk_driver_exposure)) {
          # Extract the mapping columns from the exposure data
          existing_mapping <- rv$dk_driver_exposure[, c("DKName", "Name", "DKSalary", "DKOP", "Starting", "Proj")]
        }
        
        # If we don't have existing mapping, create a new one
        if(is.null(existing_mapping) || nrow(existing_mapping) == 0) {
          # Get all unique drivers from optimal lineups and random lineups
          driver_cols <- paste0("Driver", 1:DK_ROSTER_SIZE)
          
          # Get drivers from both lineup sets
          all_drivers <- c()
          
          # From optimal lineups
          for(col in driver_cols) {
            if(col %in% names(rv$dk_optimal_lineups)) {
              all_drivers <- c(all_drivers, rv$dk_optimal_lineups[[col]])
            }
          }
          
          # From random lineups
          for(col in driver_cols) {
            if(col %in% names(rv$dk_random_lineups)) {
              all_drivers <- c(all_drivers, rv$dk_random_lineups[[col]])
            }
          }
          all_drivers <- unique(all_drivers)
          
          # Create a driver mapping from simulation results
          driver_mapping <- data.frame(
            DKName = all_drivers,
            Name = NA_character_,
            DKSalary = NA_real_,
            DKOP = NA_real_,
            Starting = NA_real_,
            Proj = NA_real_
          )
          
          # Get mapping from simulation results
          unique_sim_drivers <- rv$simulation_results[!duplicated(rv$simulation_results$DKName), 
                                                      c("DKName", "Name", "DKSalary", "DKOP", "Starting")]
          
          # Match each driver
          for(i in 1:nrow(driver_mapping)) {
            dk_name <- driver_mapping$DKName[i]
            matches <- which(unique_sim_drivers$DKName == dk_name)
            
            if(length(matches) > 0) {
              match_idx <- matches[1]
              driver_mapping$Name[i] <- unique_sim_drivers$Name[match_idx]
              driver_mapping$DKSalary[i] <- unique_sim_drivers$DKSalary[match_idx]
              driver_mapping$DKOP[i] <- unique_sim_drivers$DKOP[match_idx]
              driver_mapping$Starting[i] <- unique_sim_drivers$Starting[match_idx]
              
              # Get projection from fantasy analysis
              if(!is.null(rv$dk_fantasy_analysis)) {
                name_match <- which(rv$dk_fantasy_analysis$Name == unique_sim_drivers$Name[match_idx])
                if(length(name_match) > 0) {
                  driver_mapping$Proj[i] <- rv$dk_fantasy_analysis$Median_Fantasy_Pts[name_match[1]]
                }
              }
            }
          }
        } else {
          # Use the existing mapping
          driver_mapping <- existing_mapping
        }
        
        # Calculate driver exposure with the mapping and random lineups
        rv$dk_driver_exposure <- calculate_dk_driver_exposure(
          rv$dk_optimal_lineups, 
          driver_mapping, 
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
  
  observe({
    # Update DraftKings excluded drivers whenever we visit the lineup builder tab
    if(input$sidebar_menu == "lineup_builder" && !is.null(rv$dk_driver_exposure)) {
      driver_data <- rv$dk_driver_exposure
      driver_names <- driver_data$Name
      driver_ids <- driver_data$DKName
      driver_labels <- paste0(driver_names, " (", round(driver_data$OptimalRate, 1), "%)")
      driver_choices <- setNames(driver_ids, driver_labels)
      
      updateSelectizeInput(
        session = session,
        inputId = "dk_excluded_drivers",
        choices = driver_choices
      )
    }
    
    # Update FanDuel excluded drivers whenever we visit the lineup builder tab
    if(input$sidebar_menu == "lineup_builder" && !is.null(rv$fd_driver_exposure)) {
      driver_data <- rv$fd_driver_exposure
      driver_names <- driver_data$Name
      driver_ids <- driver_data$FDName
      driver_labels <- paste0(driver_names, " (", round(driver_data$OptimalRate, 1), "%)")
      driver_choices <- setNames(driver_ids, driver_labels)
      
      updateSelectizeInput(
        session = session,
        inputId = "fd_excluded_drivers",
        choices = driver_choices
      )
    }
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
      excluded_drivers = input$fd_excluded_drivers,
      num_lineups = input$fd_num_random_lineups
    )
    
    # Show progress
    withProgress(message = 'Generating lineups...', value = 0, {
      # Generate random lineups
      rv$fd_random_lineups <- generate_random_fd_lineups(rv$fd_optimal_lineups, filters)
      
      # Update driver exposure data using the same mapping approach from optimization
      if(!is.null(rv$fd_random_lineups)) {
        # First, let's preserve the existing driver mapping data
        existing_mapping <- NULL
        
        # Check if the current driver exposure table has mapping data
        if(!is.null(rv$fd_driver_exposure)) {
          # Extract the mapping columns from the exposure data
          existing_mapping <- rv$fd_driver_exposure[, c("FDName", "Name", "FDSalary", "FDOP", "Starting", "Proj")]
        }
        
        # If we don't have existing mapping, create a new one
        if(is.null(existing_mapping) || nrow(existing_mapping) == 0) {
          # Get all unique drivers from optimal lineups and random lineups
          driver_cols <- paste0("Driver", 1:FD_ROSTER_SIZE)
          
          # Get drivers from both lineup sets
          all_drivers <- c()
          
          # From optimal lineups
          for(col in driver_cols) {
            if(col %in% names(rv$fd_optimal_lineups)) {
              all_drivers <- c(all_drivers, rv$fd_optimal_lineups[[col]])
            }
          }
          
          # From random lineups
          for(col in driver_cols) {
            if(col %in% names(rv$fd_random_lineups)) {
              all_drivers <- c(all_drivers, rv$fd_random_lineups[[col]])
            }
          }
          all_drivers <- unique(all_drivers)
          
          # Create a driver mapping from simulation results
          driver_mapping <- data.frame(
            FDName = all_drivers,
            Name = NA_character_,
            FDSalary = NA_real_,
            FDOP = NA_real_,
            Starting = NA_real_,
            Proj = NA_real_
          )
          
          # Get mapping from simulation results
          unique_sim_drivers <- rv$simulation_results[!duplicated(rv$simulation_results$FDName), 
                                                      c("FDName", "Name", "FDSalary", "FDOP", "Starting")]
          
          # Match each driver
          for(i in 1:nrow(driver_mapping)) {
            fd_name <- driver_mapping$FDName[i]
            matches <- which(unique_sim_drivers$FDName == fd_name)
            
            if(length(matches) > 0) {
              match_idx <- matches[1]
              driver_mapping$Name[i] <- unique_sim_drivers$Name[match_idx]
              driver_mapping$FDSalary[i] <- unique_sim_drivers$FDSalary[match_idx]
              driver_mapping$FDOP[i] <- unique_sim_drivers$FDOP[match_idx]
              driver_mapping$Starting[i] <- unique_sim_drivers$Starting[match_idx]
              
              # Get projection from fantasy analysis
              if(!is.null(rv$fd_fantasy_analysis)) {
                name_match <- which(rv$fd_fantasy_analysis$Name == unique_sim_drivers$Name[match_idx])
                if(length(name_match) > 0) {
                  driver_mapping$Proj[i] <- rv$fd_fantasy_analysis$Median_Fantasy_Pts[name_match[1]]
                }
              }
            }
          }
        } else {
          # Use the existing mapping
          driver_mapping <- existing_mapping
        }
        
        # Calculate driver exposure with the mapping and random lineups
        rv$fd_driver_exposure <- calculate_fd_driver_exposure(
          rv$fd_optimal_lineups, 
          driver_mapping, 
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
  
  output$dk_driver_exposure_table <- renderDT({
    req(rv$dk_driver_exposure)
    
    # Clone the data for display
    display_data <- rv$dk_driver_exposure
    
    # If random lineups don't exist, remove the Exposure and Leverage columns
    if(is.null(rv$dk_random_lineups) || nrow(rv$dk_random_lineups) == 0) {
      # Make sure these columns don't appear if they somehow exist
      display_data$Exposure <- NULL
      display_data$Leverage <- NULL
    }
    
    # Hide DKName column as requested
    display_data$DKName <- NULL
    
    
    
    # Reorder columns to put metrics in a logical order
    col_order <- c("Name", "Starting", "Proj", "DKSalary", "DKOP", "OptimalRate", "EliteRate", 
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
        order = list(list(5, 'desc')),  # Sort by OptimalRate by default (adjusted for new column order)
        rownames = FALSE  # Remove row numbers
      ),
      rownames = FALSE
    )
    
    # Apply formatting
    if("DKSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('DKSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    # Format numeric columns with 1 decimal place
    numeric_cols <- intersect(
      c('OptimalRate', 'EliteRate', 'FloorRate', 'AppearanceRate', 'Exposure', 'Leverage', 'Proj', 'DKOP'),
      names(display_data)
    )
    
    if(length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(numeric_cols, digits = 1)
    }
    
    return(dt)
  })
  
  output$fd_driver_exposure_table <- renderDT({
    req(rv$fd_driver_exposure)
    
    # Clone the data for display
    display_data <- rv$fd_driver_exposure
    
    # If random lineups don't exist, remove the Exposure and Leverage columns
    if(is.null(rv$fd_random_lineups) || nrow(rv$fd_random_lineups) == 0) {
      # Make sure these columns don't appear if they somehow exist
      display_data$Exposure <- NULL
      display_data$Leverage <- NULL
    }
    
    # Hide FDName column as requested
    display_data$FDName <- NULL
    
    
    # Reorder columns to put metrics in a logical order
    col_order <- c("Name", "Starting", "Proj", "FDSalary", "FDOP", "OptimalRate", "EliteRate", 
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
        order = list(list(5, 'desc')),  # Sort by OptimalRate by default (adjusted for new column order)
        rownames = FALSE  # Remove row numbers
      ),
      rownames = FALSE
    )
    
    # Apply formatting
    if("FDSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('FDSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    # Format numeric columns with 1 decimal place
    numeric_cols <- intersect(
      c('OptimalRate', 'EliteRate', 'FloorRate', 'AppearanceRate', 'Exposure', 'Leverage', 'Proj', 'FDOP'),
      names(display_data)
    )
    
    if(length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(numeric_cols, digits = 1)
    }
    
    return(dt)
  })
  
  # DraftKings random lineups table
  # DraftKings random lineups table
  output$dk_random_lineups_table <- renderDT({
    req(rv$dk_random_lineups)
    
    # Clone for display
    display_data <- as.data.frame(rv$dk_random_lineups)  # Convert to data.frame to avoid data.table issues
    
    # Format driver columns to show names
    if(!is.null(rv$dk_fantasy_analysis)) {
      for(i in 1:DK_ROSTER_SIZE) {
        col <- paste0("Driver", i)
        if(col %in% names(display_data)) {
          display_data[[col]] <- sapply(display_data[[col]], function(id) {
            match_idx <- which(rv$dk_fantasy_analysis$DKName == id)
            if(length(match_idx) > 0) {
              rv$dk_fantasy_analysis$Name[match_idx[1]]
            } else {
              id
            }
          })
        }
      }
    }
    
    # Keep only driver columns and TopXCount columns
    cols_to_keep <- c(paste0("Driver", 1:DK_ROSTER_SIZE), 
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
  
  # FanDuel random lineups table
  output$fd_random_lineups_table <- renderDT({
    req(rv$fd_random_lineups)
    
    # Clone for display
    display_data <- as.data.frame(rv$fd_random_lineups)  # Convert to data.frame to avoid data.table issues
    
    # Format driver columns to show names
    if(!is.null(rv$fd_fantasy_analysis)) {
      for(i in 1:FD_ROSTER_SIZE) {
        col <- paste0("Driver", i)
        if(col %in% names(display_data)) {
          display_data[[col]] <- sapply(display_data[[col]], function(id) {
            match_idx <- which(rv$fd_fantasy_analysis$FDName == id)
            if(length(match_idx) > 0) {
              rv$fd_fantasy_analysis$Name[match_idx[1]]
            } else {
              id
            }
          })
        }
      }
    }
    
    # Keep only driver columns and TopXCount columns
    cols_to_keep <- c(paste0("Driver", 1:FD_ROSTER_SIZE), 
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
  
  # DraftKings exposure info
  output$dk_exposure_info <- renderText({
    req(rv$dk_driver_exposure, rv$dk_random_lineups)
    
    if(is.null(rv$dk_random_lineups) || nrow(rv$dk_random_lineups) == 0) {
      return("No lineups generated yet. Click 'Generate Lineups' to create lineups.")
    }
    
    # Get drivers with highest exposure
    top_exposed <- rv$dk_driver_exposure %>%
      filter(!is.na(Exposure)) %>%
      arrange(desc(Exposure)) %>%
      head(5)
    
    # Format as text
    paste("Top driver exposures in generated lineups:", 
          paste(sapply(1:nrow(top_exposed), function(i) {
            sprintf("\n%s: %.1f%%", top_exposed$Name[i], top_exposed$Exposure[i])
          }), collapse = ""),
          "\n\nNote: Adjust 'Maximum Driver Exposure' to control these values.")
  })
  
  # FanDuel exposure info
  output$fd_exposure_info <- renderText({
    req(rv$fd_driver_exposure, rv$fd_random_lineups)
    
    if(is.null(rv$fd_random_lineups) || nrow(rv$fd_random_lineups) == 0) {
      return("No lineups generated yet. Click 'Generate Lineups' to create lineups.")
    }
    
    # Get drivers with highest exposure
    top_exposed <- rv$fd_driver_exposure %>%
      filter(!is.na(Exposure)) %>%
      arrange(desc(Exposure)) %>%
      head(5)
    
    # Format as text
    paste("Top driver exposures in generated lineups:", 
          paste(sapply(1:nrow(top_exposed), function(i) {
            sprintf("\n%s: %.1f%%", top_exposed$Name[i], top_exposed$Exposure[i])
          }), collapse = ""),
          "\n\nNote: Adjust 'Maximum Driver Exposure' to control these values.")
  })
  
  # Additional file download handlers
  output$download_dk_random_lineups <- downloadHandler(
    filename = function() {
      paste("dk_random_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      # Check if random lineups exist
      if(is.null(rv$dk_random_lineups) || nrow(rv$dk_random_lineups) == 0) {
        # Create an empty dataframe with appropriate columns if no lineups exist
        empty_data <- data.frame(matrix(ncol = DK_ROSTER_SIZE, nrow = 0))
        colnames(empty_data) <- paste0("Driver", 1:DK_ROSTER_SIZE)
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      # Format data for download (convert to data.frame first to avoid data.table issues)
      download_data <- as.data.frame(rv$dk_random_lineups)
      
      # Keep only driver columns, TopX Count columns, and TotalSalary
      cols_to_keep <- c(paste0("Driver", 1:DK_ROSTER_SIZE), 
                        grep("^Top[0-9]+Count$", names(download_data), value = TRUE),
                        "TotalSalary")
      cols_to_keep <- intersect(cols_to_keep, names(download_data))
      download_data <- download_data[, cols_to_keep, drop = FALSE]
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"  # Explicitly set MIME type for CSV
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
        colnames(empty_data) <- paste0("Driver", 1:FD_ROSTER_SIZE)
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      # Format data for download (convert to data.frame first to avoid data.table issues)
      download_data <- as.data.frame(rv$fd_random_lineups)
      
      # Keep only driver columns, TopX Count columns, and TotalSalary
      cols_to_keep <- c(paste0("Driver", 1:FD_ROSTER_SIZE), 
                        grep("^Top[0-9]+Count$", names(download_data), value = TRUE),
                        "TotalSalary")
      cols_to_keep <- intersect(cols_to_keep, names(download_data))
      download_data <- download_data[, cols_to_keep, drop = FALSE]
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"  # Explicitly set MIME type for CSV
  )
  
  # FanDuel filtered pool stats
  output$fd_filtered_pool_size <- renderText({
    req(rv$fd_optimal_lineups)
    
    filters <- list(
      min_top1_count = input$fd_min_top1_count,
      min_top2_count = input$fd_min_top2_count,
      min_top3_count = input$fd_min_top3_count,
      min_top5_count = input$fd_min_top5_count,
      excluded_drivers = input$fd_excluded_drivers
    )
    
    stats <- calculate_fd_filtered_pool_stats(rv$fd_optimal_lineups, filters)
    paste("Number of lineups in filtered pool:", stats$count)
  })
  
  # Clean up on session end
  session$onSessionEnded(function() {
    gc(verbose = FALSE, full = TRUE)
  })
}


# Run the application
shinyApp(ui = ui, server = server)