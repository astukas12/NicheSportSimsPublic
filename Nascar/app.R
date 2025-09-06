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
library(foreach)


custom_css <- "
  /* Override dashboard header colors */
  .skin-blue .main-header {
    background-color: #000000;
  }
  .skin-blue .main-header .logo {
    background-color: #000000;
    color: #FFD700; /* Changed from #FFD700 to #FFD700 */
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
    color: #FFD700; /* Changed from #FFD700 to #FFD700 */
  }
  .skin-blue .sidebar-menu > li.active > a,
  .skin-blue .sidebar-menu > li:hover > a {
    color: #ffffff;
    background: #333333;
    border-left-color: #FFD700; /* Changed from #FFD700 to #FFD700 */
  }

  /* Customize box headers */
  .box.box-primary .box-header {
    background-color: #333333;
    color: #FFD700; /* Changed from #FFD700 to #FFD700 */
  }

  /* Style buttons */
  .btn-primary {
    background-color: #FFD700; /* Changed from #FFD700 to #FFD700 */
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
    border-top-color: #FFD700; /* Changed from #FFD700 to #FFD700 */
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


# Global constants
DK_ROSTER_SIZE <- 6
FD_ROSTER_SIZE <- 5
DK_SALARY_CAP <- 50000
FD_SALARY_CAP <- 50000




read_input_file <- function(file_path) {
  tryCatch({
    # Read sheets needed by both platforms
    sheets <- list(
      Driver = read_excel(file_path, sheet = "Driver")
    )
    
    sheets$Race_Weights <- tryCatch(
      read_excel(file_path, sheet = "Race_Weights"),
      error = function(e) NULL
    )
    
    sheets$Race_Profiles <- tryCatch(
      read_excel(file_path, sheet = "Race_Profiles"),
      error = function(e) NULL
    )
    
    sheets$Dom_Tier <- tryCatch(
      read_excel(file_path, sheet = "Dom_Tier"),
      error = function(e) NULL
    )
    
    # Keep FD Laps (unchanged)
    sheets$FDLaps <- tryCatch(
      read_excel(file_path, sheet = "FDLaps"),
      error = function(e) NULL
    )
    
    # Identify available platforms
    has_race_data <- !is.null(sheets$Race_Weights) && !is.null(sheets$Race_Profiles)
    has_dk <- "DKSalary" %in% colnames(sheets$Driver)
    has_fd <- "FDSalary" %in% colnames(sheets$Driver)
    
    # Create platform info
    platform_info <- list(has_draftkings = has_dk, has_fanduel = has_fd)
    
    
    list(sheets = sheets, platform_info = platform_info)
  }, error = function(e) {
    stop(paste("Error reading Excel file:", e$message))
  })
}

# Process input data efficiently with data.table
process_input_data <- function(input_data) {
  # Extract data components
  driver_data <- input_data$sheets$Driver
  race_data <- input_data$sheets$Race
  fd_laps_data <- input_data$sheets$FDLaps
  race_weights_data <- input_data$sheets$Race_Weights
  race_profiles_data <- input_data$sheets$Race_Profiles
  
  # Process driver data
  processed_drivers <- as.data.table(driver_data)
  
  # Convert relevant numeric columns efficiently
  numeric_cols <- c(
    "W",
    "T3",
    "T5",
    "T10",
    "T15",
    "T20",
    "T25",
    "T30",
    "DKSalary",
    "FDSalary",
    "DKOP",
    "FDOP",
    "Starting",
    "DomTier"
  )
  
  for (col in numeric_cols) {
    if (col %in% names(processed_drivers)) {
      processed_drivers[, (col) := as.numeric(get(col))]
    }
  }
  
  # Process Race Weights data
  processed_race_weights <- if (!is.null(race_weights_data)) {
    weights_dt <- as.data.table(race_weights_data)
    # Ensure required columns exist and are numeric
    if ("RaceID" %in% names(weights_dt) && "Weight" %in% names(weights_dt)) {
      weights_dt[, Weight := as.numeric(Weight)]
      weights_dt[, RaceID := as.character(RaceID)]
      weights_dt <- weights_dt[!is.na(Weight) & Weight > 0]
      weights_dt
    } else {
      data.table()
    }
  } else {
    data.table()
  }
  
  # Process Race Profiles data
  processed_race_profiles <- if (!is.null(race_profiles_data)) {
    profiles_dt <- as.data.table(race_profiles_data)
    # Ensure required columns exist and are numeric
    required_cols <- c("RaceID", "StartPos", "FinPos", "DKDomPoints", "FDDomPoints")
    if (all(required_cols %in% names(profiles_dt))) {
      profiles_dt[, RaceID := as.character(RaceID)]
      profiles_dt[, StartPos := as.numeric(StartPos)]
      profiles_dt[, FinPos := as.numeric(FinPos)]
      profiles_dt[, DKDomPoints := as.numeric(DKDomPoints)]
      profiles_dt[, FDDomPoints := as.numeric(FDDomPoints)]
      # Remove rows with missing required data
      profiles_dt <- profiles_dt[!is.na(StartPos) & !is.na(FinPos) & !is.na(DKDomPoints) & !is.na(FDDomPoints)]
      profiles_dt
    } else {
      data.table()
    }
  } else {
    data.table()
  }
  
  processed_dom_tier <- if (!is.null(input_data$sheets$Dom_Tier)) {
    tier_dt <- as.data.table(input_data$sheets$Dom_Tier)
    if (all(c("DomTier", "DKMax", "FDMax") %in% names(tier_dt))) {
      tier_dt[, DomTier := as.numeric(DomTier)]
      tier_dt[, DKMax := as.numeric(DKMax)]
      tier_dt[, FDMax := as.numeric(FDMax)]
      tier_dt
    } else {
      data.table()
    }
  } else {
    data.table()
  }
  
  # Process FD laps data if available
  processed_fd_laps <- if (!is.null(fd_laps_data)) {
    fd_laps_dt <- as.data.table(fd_laps_data)
    
    # Only handle Pt column (no more high/low)
    fd_laps_dt <- fd_laps_dt[!is.na(Pt)]
    
    # Convert columns to numeric
    for (col in c("ps", "Pt")) {
      if (col %in% names(fd_laps_dt)) {
        fd_laps_dt[, (col) := as.numeric(get(col))]
      }
    }
    
    # Use position (ps) as finish position
    fd_laps_dt[, FinishLow := ps]
    fd_laps_dt[, FinishHigh := ps]
    
    setorder(fd_laps_dt, ps)
    fd_laps_dt
  } else
    data.table()
  
  
  # Return processed data
  list(
    drivers = processed_drivers,
    race_weights = processed_race_weights,
    race_profiles = processed_race_profiles,
    dom_tier = processed_dom_tier,
    fd_laps = processed_fd_laps
  )
}

create_lineup_display_data <- function(full_lineups, max_display = 100) {
  if (is.null(full_lineups) || nrow(full_lineups) == 0) {
    return(list(display = NULL, full = NULL))
  }
  
  # Sort by Top1Count descending, then Top5Count descending
  setDT(full_lineups)
  setorder(full_lineups, -Top1Count, -Top5Count)
  
  # Create display subset (top N lineups)
  display_lineups <- head(full_lineups, max_display)
  
  cat("Created display data: showing top", nrow(display_lineups), "of", nrow(full_lineups), "total lineups\n")
  
  return(list(
    display = as.data.frame(display_lineups),
    full = as.data.frame(full_lineups)
  ))
}

configure_memory_settings <- function() {
  # Configure data.table for better memory usage
  data.table::setDTthreads(0)  # Use all available cores
  options(datatable.optimize = 2)
  
  # More aggressive garbage collection settings
  gcinfo(FALSE)  # Disable verbose GC messages
  
  cat("Memory settings configured:\n")
  cat("- data.table threads:", data.table::getDTthreads(), "\n")
  cat("- data.table optimization level:", getOption("datatable.optimize"), "\n")
}

# Enhanced cleanup with performance monitoring
cleanup_memory <- function(verbose = TRUE) {
  if (verbose) {
    start_mem <- sum(gc()[,2])
    cat("Memory cleanup starting - Used:", round(start_mem, 1), "MB\n")
  }
  
  # Force garbage collection multiple times
  for(i in 1:3) {
    gc(verbose = FALSE, full = TRUE)
    Sys.sleep(0.05)  # Brief pause between collections
  }
  
  if (verbose) {
    end_mem <- sum(gc()[,2])
    freed <- start_mem - end_mem
    cat("Memory cleanup complete - Used:", round(end_mem, 1), "MB")
    if (freed > 0) {
      cat(" (freed", round(freed, 1), "MB)")
    }
    cat("\n")
  }
}




# Efficient memory cleanup
cleanup_memory <- function(verbose = FALSE) {
  if (verbose) cat("Running memory cleanup...\n")
  
  # Force garbage collection
  for(i in 1:3) {
    gc(verbose = FALSE, full = TRUE)
    Sys.sleep(0.1)  # Brief pause between collections
  }
  
  if (verbose) {
    mem_info <- gc()
    cat("Memory after cleanup - Used:", sum(mem_info[,2]), "MB\n")
  }
}

optimize_simulation_storage <- function(sim_results) {
  # Don't remove SimID immediately - let analysis functions handle it
  # Only remove truly unnecessary columns
  columns_to_remove <- c()  # Keep SimID for now
  
  for (col in columns_to_remove) {
    if (col %in% names(sim_results)) {
      sim_results[[col]] <- NULL
    }
  }
  
  return(sim_results)
}


# Pre-calculate expensive lookup data
create_scoring_lookups <- function() {
  list(
    dk_finish_points = c(45, 42, 41, 40, 39, 38, 37, 36, 35, 34, 32, 31, 30, 29, 28, 
                         27, 26, 25, 24, 23, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 
                         10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0),
    fd_finish_points = c(43, 40, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 
                         25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 
                         10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
  )
}

# Initialize memory settings when app starts
configure_memory_settings()

create_driver_lookups <- function(drivers_dt) {
  # Pre-compute salary and name lookups to avoid repeated searches
  dk_lookup <- NULL
  fd_lookup <- NULL
  
  if ("DKSalary" %in% names(drivers_dt)) {
    dk_lookup <- drivers_dt[!duplicated(DKName), .(DKName, Name, DKSalary, DKOP, Starting)]
    setkey(dk_lookup, DKName)
  }
  
  if ("FDSalary" %in% names(drivers_dt)) {
    fd_lookup <- drivers_dt[!duplicated(FDName), .(FDName, Name, FDSalary, FDOP, Starting)]
    setkey(fd_lookup, FDName)
  }
  
  list(dk = dk_lookup, fd = fd_lookup)
}

precompute_driver_distributions <- function(drivers_dt) {
  n_drivers <- nrow(drivers_dt)
  
  # Extract probability columns once
  prob_matrix <- as.matrix(drivers_dt[, .(W, T3, T5, T10, T15, T20, T25, T30)])
  
  # Handle NAs efficiently
  prob_matrix[is.na(prob_matrix)] <- 0
  
  # Pre-compute marginal probabilities for all drivers
  marginal_probs <- array(0, dim = c(n_drivers, 9))
  
  for (i in 1:n_drivers) {
    cum_probs <- c(prob_matrix[i, ], 1.0)
    
    # Calculate marginal probabilities
    marg_probs <- diff(c(0, cum_probs))
    marg_probs[marg_probs < 0] <- 0
    
    # Normalize
    sum_probs <- sum(marg_probs)
    if (sum_probs > 0) {
      marg_probs <- marg_probs / sum_probs
    } else {
      marg_probs <- rep(1/9, 9)
    }
    
    marginal_probs[i, ] <- marg_probs
  }
  
  # Pre-define position ranges
  position_ranges <- list(
    c(1),
    c(2, 3),
    c(4, 5),
    c(6, 7, 8, 9, 10),
    c(11, 12, 13, 14, 15),
    c(16, 17, 18, 19, 20),
    c(21, 22, 23, 24, 25),
    c(26, 27, 28, 29, 30),
    c(31:n_drivers)
  )
  
  return(list(
    marginal_probs = marginal_probs,
    position_ranges = position_ranges,
    n_drivers = n_drivers
  ))
}

# Vectorized finish position simulation
simulate_finish_positions_vectorized <- function(driver_distributions, n_sims) {
  n_drivers <- driver_distributions$n_drivers
  marginal_probs <- driver_distributions$marginal_probs
  position_ranges <- driver_distributions$position_ranges
  
  # Pre-allocate result matrix
  all_positions <- matrix(0, nrow = n_drivers, ncol = n_sims)
  
  # Generate all random numbers at once
  random_matrix <- matrix(runif(n_drivers * n_sims), nrow = n_drivers, ncol = n_sims)
  noise_matrix <- matrix(runif(n_drivers * n_sims, 0, 0.1), nrow = n_drivers, ncol = n_sims)
  
  # Vectorized simulation
  for (sim in 1:n_sims) {
    performance_scores <- numeric(n_drivers)
    
    # Vectorized sampling for all drivers in this simulation
    for (i in 1:n_drivers) {
      # Sample position range using pre-computed probabilities
      random_val <- random_matrix[i, sim]
      cumsum_probs <- cumsum(marginal_probs[i, ])
      pos_range_idx <- which(random_val <= cumsum_probs)[1]
      
      if (is.na(pos_range_idx)) pos_range_idx <- 9
      
      pos_range <- position_ranges[[pos_range_idx]]
      
      # Sample within range
      if (length(pos_range) > 1) {
        range_random <- (random_val * 1000) %% 1
        sampled_pos <- pos_range[ceiling(range_random * length(pos_range))]
      } else {
        sampled_pos <- pos_range[1]
      }
      
      performance_scores[i] <- sampled_pos + noise_matrix[i, sim]
    }
    
    # Rank to get final positions
    all_positions[, sim] <- rank(performance_scores)
  }
  
  return(all_positions)
}

# Pre-compute scoring lookups once
create_scoring_system <- function() {
  list(
    dk_finish_points = c(45, 42, 41, 40, 39, 38, 37, 36, 35, 34, 32, 31, 30, 29, 28, 
                         27, 26, 25, 24, 23, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 
                         10, 9, 8, 7, 6, 5, 4, 3, 2, 1, rep(0, 100)),  # Extend for safety
    fd_finish_points = c(43, 40, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 
                         25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 
                         10, 9, 8, 7, 6, 5, 4, 3, 2, 1, rep(0, 100))   # Extend for safety
  )
}


select_weighted_race <- function(race_weights) {
  if (nrow(race_weights) == 0) return(NULL)
  race_id <- sample(race_weights$RaceID, 1, prob = race_weights$Weight)
  return(race_id)
}

# Calculate match score for driver-profile pairing
calculate_match_score <- function(sim_start, sim_finish, profile_start, profile_finish) {
  finish_diff <- abs(sim_finish - profile_finish)
  start_diff <- abs(sim_start - profile_start)
  
  # Heavy weight on finish position, light weight on start
  score <- (finish_diff * 3) + (start_diff * 1)
  return(score)
}


assign_dominator_points_from_profiles <- function(race_results, race_weights, race_profiles, dom_tier_limits, platform = "DK") {
  setDT(race_results)
  setDT(race_weights)
  setDT(race_profiles)
  setDT(dom_tier_limits)
  
  # Initialize dominator points
  dom_col <- if (platform == "DK") "DKDomPoints" else "FDDomPoints"
  max_col <- if (platform == "DK") "DKMax" else "FDMax"
  result_col <- if (platform == "DK") "DKDominatorPoints" else "FDDominatorPoints"
  
  race_results[, (result_col) := 0]
  
  # Select race (early exit if no data)
  if (nrow(race_weights) == 0) return(race_results)
  selected_race <- select_weighted_race(race_weights)
  if (is.null(selected_race)) return(race_results)
  
  # Get profiles for selected race
  race_profiles_subset <- race_profiles[RaceID == selected_race]
  if (nrow(race_profiles_subset) == 0) return(race_results)
  
  # Sort profiles by dominator points descending (assign highest points first)
  if (platform == "DK") {
    setorder(race_profiles_subset, -DKDomPoints)
  } else {
    setorder(race_profiles_subset, -FDDomPoints)
  }
  
  # Handle missing DomTier
  if (!"DomTier" %in% names(race_results)) {
    race_results[, DomTier := 1]
  }
  
  # Pre-compute tier limits lookup
  tier_limits <- setNames(dom_tier_limits[[max_col]], dom_tier_limits$DomTier)
  
  # Track assigned drivers
  assigned_drivers <- logical(nrow(race_results))
  
  # Assignment loop
  for (i in seq_len(nrow(race_profiles_subset))) {
    profile_points <- race_profiles_subset[[dom_col]][i]
    profile_start <- race_profiles_subset$StartPos[i]
    profile_finish <- race_profiles_subset$FinPos[i]
    
    # Skip if no points to assign
    if (is.na(profile_points) || profile_points == 0) next
    
    # Find drivers whose tier allows this point value AND who haven't been assigned yet
    eligible_drivers_mask <- !assigned_drivers & 
      !is.na(tier_limits[as.character(race_results$DomTier)]) &
      tier_limits[as.character(race_results$DomTier)] >= profile_points
    
    eligible_indices <- which(eligible_drivers_mask)
    
    if (length(eligible_indices) > 0) {
      # Calculate position distances for all eligible drivers
      driver_starts <- race_results$Starting[eligible_indices]
      driver_finishes <- race_results$FinishPosition[eligible_indices]
      
      finish_diffs <- abs(driver_finishes - profile_finish)
      start_diffs <- abs(driver_starts - profile_start)
      distances <- sqrt(finish_diffs^2 + start_diffs^2)
      
      # Find closest match
      best_match_idx <- eligible_indices[which.min(distances)]
      
      # Assign points and mark as assigned
      race_results[best_match_idx, (result_col) := profile_points]
      assigned_drivers[best_match_idx] <- TRUE
    }
    # If no eligible drivers, the points go unassigned (silent skip)
  }
  
  return(race_results)
}


assign_fd_lap_points <- function(race_results, fd_laps_data) {
  setDT(race_results)
  setDT(fd_laps_data)
  
  result <- copy(race_results)
  result[, FDLapPoints := 0]
  
  # Validate required columns
  if (!"ps" %in% names(fd_laps_data)) {
    return(result)
  }
  
  # Determine point column to use
  point_col <- NULL
  if ("Pt" %in% names(fd_laps_data)) {
    point_col <- "Pt"
  } else if ("PtLow" %in% names(fd_laps_data)) {
    point_col <- "PtLow"
  } else if ("PtHigh" %in% names(fd_laps_data)) {
    point_col <- "PtHigh"
  }
  
  if (is.null(point_col)) {
    return(result)
  }
  
  # Clean and prepare lap data
  fd_laps_data[, ps := as.numeric(ps)]
  fd_laps_data[, (point_col) := as.numeric(get(point_col))]
  
  # Remove invalid rows
  fd_laps_data <- fd_laps_data[!is.na(ps) & !is.na(get(point_col)) & ps > 0]
  
  if (nrow(fd_laps_data) == 0) {
    return(result)
  }
  
  # Sort by position for efficient lookup
  setorder(fd_laps_data, ps)
  setkey(fd_laps_data, ps)
  
  # Process each driver's finish position
  for (i in 1:nrow(result)) {
    finish_pos <- result$FinishPosition[i]
    
    if (is.na(finish_pos) || finish_pos <= 0) {
      result$FDLapPoints[i] <- 0
      next
    }
    
    # Find the appropriate lap points for this finish position
    lap_points <- get_lap_points_for_position(finish_pos, fd_laps_data, point_col)
    result$FDLapPoints[i] <- lap_points
  }
  
  # Round to appropriate precision
  result[, FDLapPoints := round(FDLapPoints, 1)]
  
  return(result)
}


# Add these helper functions to your code

create_scoring_system <- function() {
  list(
    dk_finish_points = c(45, 42, 41, 40, 39, 38, 37, 36, 35, 34, 32, 31, 30, 29, 28, 
                         27, 26, 25, 24, 23, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 
                         10, 9, 8, 7, 6, 5, 4, 3, 2, 1, rep(0, 100)),  # Extend for safety
    fd_finish_points = c(43, 40, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 
                         25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 
                         10, 9, 8, 7, 6, 5, 4, 3, 2, 1, rep(0, 100))   # Extend for safety
  )
}



# Helper function for efficient lap point lookup
get_lap_points_for_position <- function(finish_pos, fd_laps_data, point_col) {
  # Try exact match first using data.table's fast binary search
  exact_match <- fd_laps_data[.(finish_pos)]
  if (!is.na(exact_match$ps[1])) {
    return(exact_match[[point_col]][1])
  }
  
  # Find the closest lower position
  lower_positions <- fd_laps_data[ps < finish_pos]
  if (nrow(lower_positions) > 0) {
    closest_lower_idx <- which.max(lower_positions$ps)
    return(lower_positions[[point_col]][closest_lower_idx])
  }
  
  # If no lower position, find the closest higher position
  higher_positions <- fd_laps_data[ps > finish_pos]
  if (nrow(higher_positions) > 0) {
    closest_higher_idx <- which.min(higher_positions$ps)
    return(higher_positions[[point_col]][closest_higher_idx])
  }
  
  # No match found
  return(0)
}


# Helper function for efficient lap point lookup
get_lap_points_for_position <- function(finish_pos, fd_laps_data, point_col) {
  # Try exact match first using data.table's fast binary search
  exact_match <- fd_laps_data[.(finish_pos)]
  if (!is.na(exact_match$ps[1])) {
    return(exact_match[[point_col]][1])
  }
  
  # Find the closest lower position
  lower_positions <- fd_laps_data[ps < finish_pos]
  if (nrow(lower_positions) > 0) {
    # Use the highest position that's still lower than finish_pos
    closest_lower_idx <- which.max(lower_positions$ps)
    return(lower_positions[[point_col]][closest_lower_idx])
  }
  
  # If no lower position, find the closest higher position
  higher_positions <- fd_laps_data[ps > finish_pos]
  if (nrow(higher_positions) > 0) {
    closest_higher_idx <- which.min(higher_positions$ps)
    return(higher_positions[[point_col]][closest_higher_idx])
  }
  
  # No match found - this shouldn't happen with reasonable data
  return(0)
}

run_efficient_simulation <- function(input_data, n_sims = 1000) {
  drivers_dt <- as.data.table(input_data$drivers)
  n_drivers <- nrow(drivers_dt)
  
  # Platform detection
  has_dk <- "DKSalary" %in% names(drivers_dt) && !is.null(input_data$race_weights) && nrow(input_data$race_weights) > 0
  has_fd <- "FDSalary" %in% names(drivers_dt) && (!is.null(input_data$fd_laps) || (!is.null(input_data$race_weights) && nrow(input_data$race_weights) > 0))
  
  cat("=== SIMULATION SETUP ===\n")
  cat("Total simulations:", n_sims, "\n")
  cat("Number of drivers:", n_drivers, "\n")
  cat("Platforms: DraftKings =", has_dk, ", FanDuel =", has_fd, "\n")
  cat("========================\n")
  
  # Pre-compute everything once
  cat("[SETUP] Pre-computing driver distributions...\n")
  driver_distributions <- precompute_driver_distributions(drivers_dt)
  scoring_system <- create_scoring_system()
  
  # Extract static data once
  static_data <- list(
    names = drivers_dt$Name,
    starting = drivers_dt$Starting,
    dom_tier = drivers_dt$DomTier
  )
  
  if (has_dk) {
    static_data$dk <- list(
      salary = drivers_dt$DKSalary,
      op = drivers_dt$DKOP,
      name = drivers_dt$DKName
    )
    cat("[SETUP] DraftKings data loaded\n")
  }
  
  if (has_fd) {
    static_data$fd <- list(
      salary = drivers_dt$FDSalary,
      op = drivers_dt$FDOP,
      name = drivers_dt$FDName
    )
    cat("[SETUP] FanDuel data loaded\n")
  }
  
  # Generate all finish positions at once
  cat("[STEP 1/3] Generating finish positions for", n_sims, "simulations...\n")
  start_time <- Sys.time()
  all_finish_positions <- simulate_finish_positions_vectorized(driver_distributions, n_sims)
  step1_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat("Finish positions completed in", round(step1_time, 1), "seconds\n")
  
  # Vectorized fantasy point calculation - KEEP SimID!
  cat("[STEP 2/3] Calculating fantasy points...\n")
  step2_start <- Sys.time()
  results_list <- vector("list", n_sims)
  
  # Progress tracking for fantasy point calculation
  progress_interval <- max(1, floor(n_sims / 20))  # Report every 5%
  next_progress <- progress_interval
  
  for (sim in 1:n_sims) {
    # Progress reporting
    if (sim == next_progress || sim == n_sims) {
      elapsed <- as.numeric(difftime(Sys.time(), step2_start, units = "secs"))
      rate <- sim / elapsed
      remaining_sims <- n_sims - sim
      eta_seconds <- remaining_sims / rate
      progress_pct <- round((sim / n_sims) * 100, 1)
      
      cat("  Progress:", progress_pct, "% (", sim, "/", n_sims, ") - ", 
          round(rate, 1), " sims/sec - ETA:", round(eta_seconds, 0), "s\n")
      next_progress <- next_progress + progress_interval
    }
    
    sim_result <- data.table(
      SimID = sim,  # KEEP THIS!
      Name = static_data$names,
      Starting = static_data$starting,
      FinishPosition = all_finish_positions[, sim],
      DomTier = static_data$dom_tier
    )
    
    if (has_dk) {
      # Vectorized DK calculations
      dk_finish_pts <- scoring_system$dk_finish_points[pmin(all_finish_positions[, sim], 
                                                            length(scoring_system$dk_finish_points))]
      dk_place_diff <- static_data$starting - all_finish_positions[, sim]
      
      sim_result[, `:=`(
        DKSalary = static_data$dk$salary,
        DKOP = static_data$dk$op,
        DKName = static_data$dk$name,
        DKFantasyPoints = dk_finish_pts + dk_place_diff  # Will add dominator points later
      )]
      
      # Assign DK dominator points using race profiles
      sim_result <- assign_dominator_points_from_profiles(
        sim_result, 
        input_data$race_weights, 
        input_data$race_profiles,
        input_data$dom_tier,
        "DK"
      )
      
      # Update fantasy points with dominator points
      sim_result[, DKFantasyPoints := DKFantasyPoints + DKDominatorPoints]
    }
    
    if (has_fd) {
      # Vectorized FD calculations
      fd_finish_pts <- scoring_system$fd_finish_points[pmin(all_finish_positions[, sim], 
                                                            length(scoring_system$fd_finish_points))]
      fd_place_diff <- (static_data$starting - all_finish_positions[, sim]) * 0.5
      
      sim_result[, `:=`(
        FDSalary = static_data$fd$salary,
        FDOP = static_data$fd$op,
        FDName = static_data$fd$name,
        FDLapPoints = rep(0, n_drivers),  # Will be calculated separately
        FDFantasyPoints = fd_finish_pts + fd_place_diff  # Will add dominator points later
      )]
      
      # Assign FD dominator points using race profiles
      sim_result <- assign_dominator_points_from_profiles(
        sim_result, 
        input_data$race_weights, 
        input_data$race_profiles,
        input_data$dom_tier,
        "FD"
      )
      
      # Calculate FD lap points
      sim_result <- assign_fd_lap_points(sim_result, input_data$fd_laps)
      
      # Update fantasy points with dominator and lap points
      sim_result[, FDFantasyPoints := FDFantasyPoints + FDDominatorPoints + FDLapPoints]
    }
    
    results_list[[sim]] <- sim_result
  }
  
  step2_time <- as.numeric(difftime(Sys.time(), step2_start, units = "secs"))
  cat("Fantasy points completed in", round(step2_time, 1), "seconds\n")
  
  # Combine results
  cat("[STEP 3/3] Combining results...\n")
  step3_start <- Sys.time()
  combined_results <- rbindlist(results_list)
  step3_time <- as.numeric(difftime(Sys.time(), step3_start, units = "secs"))
  cat("Results combined in", round(step3_time, 1), "seconds\n")
  
  total_time <- step1_time + step2_time + step3_time
  cat("=== SIMULATION COMPLETE ===\n")
  cat("Total time:", round(total_time, 1), "seconds\n")
  cat("Average rate:", round(n_sims / total_time, 1), "simulations/second\n")
  cat("Results generated:", nrow(combined_results), "total results\n")
  cat("============================\n")
  
  return(list(
    results = combined_results,
    has_dk = has_dk,
    has_fd = has_fd
  ))
}

# REPLACE the existing analysis functions with these fixed versions

# Fixed DK dominator analysis with proper data type handling
analyze_dk_dominator_points <- function(sim_results, max_sample_size = 1000000) {
  setDT(sim_results)
  
  n_total_results <- nrow(sim_results)

  
  # Sample for performance if dataset is very large
  analysis_data <- sim_results
  if (n_total_results > max_sample_size) {
    sample_size <- max_sample_size
    sample_indices <- sample(n_total_results, sample_size)
    analysis_data <- sim_results[sample_indices]
    cat("Using sample of", sample_size, "results for analysis (", 
        round(sample_size/n_total_results*100, 1), "%)\n")
  }
  
  # Check if SimID exists, if not create it
  if (!"SimID" %in% names(analysis_data)) {
    n_drivers <- length(unique(analysis_data$Name))
    analysis_data[, SimID := rep(1:(nrow(analysis_data) %/% n_drivers + 1), each = n_drivers)[1:nrow(analysis_data)]]
    created_simid <- TRUE
  } else {
    created_simid <- FALSE
  }
  
  # Ensure numeric columns are properly typed
  analysis_data[, Starting := as.numeric(Starting)]
  analysis_data[, DKSalary := as.numeric(DKSalary)]
  analysis_data[, DKDominatorPoints := as.numeric(DKDominatorPoints)]
  
  # Calculate dominator rank for each simulation
  analysis_data[, DKDominatorRank := frank(-DKDominatorPoints, ties.method = "min"), by = SimID]
  
  # Use explicit type conversion in aggregation
  results <- analysis_data[, .(
    Starting = as.numeric(first(Starting)),
    DKSalary = as.numeric(first(DKSalary)),
    Avg_Dom = as.numeric(mean(DKDominatorPoints, na.rm = TRUE)),
    Median_Dom = as.numeric(median(DKDominatorPoints, na.rm = TRUE)),
    Max_Dom = as.numeric(max(DKDominatorPoints, na.rm = TRUE)),
    Avg_DomRank = as.numeric(mean(DKDominatorRank, na.rm = TRUE)),
    Median_DomRank = as.numeric(median(DKDominatorRank, na.rm = TRUE)),
    Top_DomRate = as.numeric(mean(DKDominatorRank == 1, na.rm = TRUE) * 100),
    Top3_DomRate = as.numeric(mean(DKDominatorRank <= 3, na.rm = TRUE) * 100),
    Top5_DomRate = as.numeric(mean(DKDominatorRank <= 5, na.rm = TRUE) * 100),
    Top10_DomRate = as.numeric(mean(DKDominatorRank <= 10, na.rm = TRUE) * 100)
  ), by = Name]
  
  # Clean up temporary columns
  analysis_data[, DKDominatorRank := NULL]
  if (created_simid) {
    analysis_data[, SimID := NULL]
  }
  
  # Round numeric columns
  numeric_cols <- c(
    "Starting", "DKSalary", "Avg_Dom", "Median_Dom", "Max_Dom",
    "Avg_DomRank", "Median_DomRank", "Top_DomRate", "Top3_DomRate", 
    "Top5_DomRate", "Top10_DomRate"
  )
  
  for (col in numeric_cols) {
    if (col %in% names(results)) {
      results[, (col) := round(as.numeric(get(col)), 1)]
    }
  }
  
  # Sort by Average Dominator Points in descending order
  setorder(results, -Avg_Dom)
  
  return(results)
}

# DraftKings contest driver analysis
calculate_dk_contest_driver_analysis <- function(contest_results, fantasy_analysis) {
  if (is.null(contest_results) || nrow(contest_results) == 0) {
    return(data.frame(Message = "No contest results available."))
  }
  
  setDT(contest_results)
  if (!is.null(fantasy_analysis)) setDT(fantasy_analysis)
  
  driver_cols <- paste0("Driver", 1:6)
  all_drivers <- unique(unlist(contest_results[, ..driver_cols]))
  
  metrics_data <- data.table(
    DKName = all_drivers,
    Name = NA_character_,
    DKSalary = NA_real_,
    DKOP = NA_real_,
    ContestWinRate = 0,
    AvgLineupWinRate = 0,
    TopLineupWinRate = 0,
    Starting = NA_real_,
    Proj = NA_real_
  )
  
  # Calculate contest performance metrics for each driver
  for (driver in all_drivers) {
    # Find all lineups containing this driver
    driver_appears <- logical(nrow(contest_results))
    for (col in driver_cols) {
      driver_appears <- driver_appears | (contest_results[[col]] == driver)
    }
    
    if (any(driver_appears)) {
      driver_lineups <- contest_results[driver_appears]
      
      # Calculate metrics
      avg_win_rate <- mean(driver_lineups$WinRate, na.rm = TRUE)
      top_win_rate <- max(driver_lineups$WinRate, na.rm = TRUE)
      contest_win_rate <- (sum(driver_lineups$WinRate >= 50) / nrow(driver_lineups)) * 100
      
      metrics_data[DKName == driver, `:=`(
        ContestWinRate = contest_win_rate,
        AvgLineupWinRate = avg_win_rate,
        TopLineupWinRate = top_win_rate
      )]
    }
  }
  
  # Add driver information from fantasy analysis
  if (!is.null(fantasy_analysis) && nrow(fantasy_analysis) > 0) {
    for (i in 1:nrow(metrics_data)) {
      dk_name <- metrics_data$DKName[i]
      match_idx <- which(fantasy_analysis$DKName == dk_name)
      if (length(match_idx) > 0) {
        idx <- match_idx[1]
        metrics_data$Name[i] <- fantasy_analysis$Name[idx]
        metrics_data$DKSalary[i] <- fantasy_analysis$DKSalary[idx]
        metrics_data$DKOP[i] <- fantasy_analysis$DKOP[idx]
        metrics_data$Starting[i] <- fantasy_analysis$Starting[idx]
        if ("Median_Fantasy_Pts" %in% names(fantasy_analysis)) {
          metrics_data$Proj[i] <- fantasy_analysis$Median_Fantasy_Pts[idx]
        }
      }
    }
  }
  
  # Sort by average lineup win rate
  setorder(metrics_data, -AvgLineupWinRate)
  
  return(as.data.frame(metrics_data))
}

# FanDuel contest driver analysis
calculate_fd_contest_driver_analysis <- function(contest_results, fantasy_analysis) {
  if (is.null(contest_results) || nrow(contest_results) == 0) {
    return(data.frame(Message = "No contest results available."))
  }
  
  setDT(contest_results)
  if (!is.null(fantasy_analysis)) setDT(fantasy_analysis)
  
  driver_cols <- paste0("Driver", 1:5)
  all_drivers <- unique(unlist(contest_results[, ..driver_cols]))
  
  metrics_data <- data.table(
    FDName = all_drivers,
    Name = NA_character_,
    FDSalary = NA_real_,
    FDOP = NA_real_,
    ContestWinRate = 0,
    AvgLineupWinRate = 0,
    TopLineupWinRate = 0,
    Starting = NA_real_,
    Proj = NA_real_
  )
  
  # Calculate contest performance metrics for each driver
  for (driver in all_drivers) {
    # Find all lineups containing this driver
    driver_appears <- logical(nrow(contest_results))
    for (col in driver_cols) {
      driver_appears <- driver_appears | (contest_results[[col]] == driver)
    }
    
    if (any(driver_appears)) {
      driver_lineups <- contest_results[driver_appears]
      
      # Calculate metrics
      avg_win_rate <- mean(driver_lineups$WinRate, na.rm = TRUE)
      top_win_rate <- max(driver_lineups$WinRate, na.rm = TRUE)
      contest_win_rate <- (sum(driver_lineups$WinRate >= 50) / nrow(driver_lineups)) * 100
      
      metrics_data[FDName == driver, `:=`(
        ContestWinRate = contest_win_rate,
        AvgLineupWinRate = avg_win_rate,
        TopLineupWinRate = top_win_rate
      )]
    }
  }
  
  # Add driver information from fantasy analysis
  if (!is.null(fantasy_analysis) && nrow(fantasy_analysis) > 0) {
    for (i in 1:nrow(metrics_data)) {
      fd_name <- metrics_data$FDName[i]
      match_idx <- which(fantasy_analysis$FDName == fd_name)
      if (length(match_idx) > 0) {
        idx <- match_idx[1]
        metrics_data$Name[i] <- fantasy_analysis$Name[idx]
        metrics_data$FDSalary[i] <- fantasy_analysis$FDSalary[idx]
        metrics_data$FDOP[i] <- fantasy_analysis$FDOP[idx]
        metrics_data$Starting[i] <- fantasy_analysis$Starting[idx]
        if ("Median_Fantasy_Pts" %in% names(fantasy_analysis)) {
          metrics_data$Proj[i] <- fantasy_analysis$Median_Fantasy_Pts[idx]
        }
      }
    }
  }
  
  # Check if FDOP is already in percentage format
  if (!is.null(metrics_data$FDOP) && !all(is.na(metrics_data$FDOP))) {
    if (max(metrics_data$FDOP, na.rm = TRUE) <= 1) {
      metrics_data[, FDOP := FDOP * 100]
    }
  }
  
  # Sort by average lineup win rate
  setorder(metrics_data, -AvgLineupWinRate)
  
  return(as.data.frame(metrics_data))
}

# Fixed FD dominator analysis with proper data type handling
analyze_fd_dominator_points <- function(sim_results, max_sample_size = 1000000) {
  setDT(sim_results)
  
  n_total_results <- nrow(sim_results)

  
  # Sample for performance if dataset is very large
  analysis_data <- sim_results
  if (n_total_results > max_sample_size) {
    sample_size <- max_sample_size
    sample_indices <- sample(n_total_results, sample_size)
    analysis_data <- sim_results[sample_indices]
    cat("Using sample of", sample_size, "results for analysis (", 
        round(sample_size/n_total_results*100, 1), "%)\n")
  }
  
  # Check if SimID exists, if not create it
  if (!"SimID" %in% names(analysis_data)) {
    n_drivers <- length(unique(analysis_data$Name))
    analysis_data[, SimID := rep(1:(nrow(analysis_data) %/% n_drivers + 1), each = n_drivers)[1:nrow(analysis_data)]]
    created_simid <- TRUE
  } else {
    created_simid <- FALSE
  }
  
  # Ensure numeric columns are properly typed
  analysis_data[, Starting := as.numeric(Starting)]
  analysis_data[, FDSalary := as.numeric(FDSalary)]
  analysis_data[, FDDominatorPoints := as.numeric(FDDominatorPoints)]
  
  # Calculate dominator rank for each simulation
  analysis_data[, FDDominatorRank := frank(-FDDominatorPoints, ties.method = "min"), by = SimID]
  
  # Use explicit type conversion in aggregation
  results <- analysis_data[, .(
    Starting = as.numeric(first(Starting)),
    FDSalary = as.numeric(first(FDSalary)),
    Avg_Dom = as.numeric(mean(FDDominatorPoints, na.rm = TRUE)),
    Median_Dom = as.numeric(median(FDDominatorPoints, na.rm = TRUE)),
    Max_Dom = as.numeric(max(FDDominatorPoints, na.rm = TRUE)),
    Avg_DomRank = as.numeric(mean(FDDominatorRank, na.rm = TRUE)),
    Median_DomRank = as.numeric(median(FDDominatorRank, na.rm = TRUE)),
    Top_DomRate = as.numeric(mean(FDDominatorRank == 1, na.rm = TRUE) * 100),
    Top3_DomRate = as.numeric(mean(FDDominatorRank <= 3, na.rm = TRUE) * 100),
    Top5_DomRate = as.numeric(mean(FDDominatorRank <= 5, na.rm = TRUE) * 100)
  ), by = Name]
  
  # Clean up temporary columns
  analysis_data[, FDDominatorRank := NULL]
  if (created_simid) {
    analysis_data[, SimID := NULL]
  }
  
  # Round numeric columns
  numeric_cols <- c(
    "Starting", "FDSalary", "Avg_Dom", "Median_Dom", "Max_Dom",
    "Avg_DomRank", "Median_DomRank", "Top_DomRate", "Top3_DomRate", "Top5_DomRate"
  )
  
  for (col in numeric_cols) {
    if (col %in% names(results)) {
      results[, (col) := round(as.numeric(get(col)), 1)]
    }
  }
  
  # Sort by Average Dominator Points in descending order
  setorder(results, -Avg_Dom)
  
  return(results)
}

# Fixed finishing positions analysis with proper data type handling
analyze_finishing_positions <- function(sim_results, max_display_rows = 50) {
  setDT(sim_results)
  
  # For very large datasets, sample for display but keep full analysis
  n_total_results <- nrow(sim_results)
  n_drivers <- length(unique(sim_results$Name))
  n_sims <- n_total_results / n_drivers
  
  cat("Analyzing finishing positions:", n_drivers, "drivers,", n_sims, "simulations\n")
  
  # Ensure FinishPosition is numeric
  sim_results[, FinishPosition := as.numeric(FinishPosition)]
  
  # Always do full analysis for accuracy with explicit type conversion
  results <- sim_results[, .(
    Win_Rate = as.numeric(mean(FinishPosition == 1, na.rm = TRUE) * 100),
    T3_Rate = as.numeric(mean(FinishPosition <= 3, na.rm = TRUE) * 100),
    T5_Rate = as.numeric(mean(FinishPosition <= 5, na.rm = TRUE) * 100),
    T10_Rate = as.numeric(mean(FinishPosition <= 10, na.rm = TRUE) * 100),
    T15_Rate = as.numeric(mean(FinishPosition <= 15, na.rm = TRUE) * 100),
    T20_Rate = as.numeric(mean(FinishPosition <= 20, na.rm = TRUE) * 100),
    T25_Rate = as.numeric(mean(FinishPosition <= 25, na.rm = TRUE) * 100),
    T30_Rate = as.numeric(mean(FinishPosition <= 30, na.rm = TRUE) * 100),
    Avg_Finish = as.numeric(mean(FinishPosition, na.rm = TRUE)),
    Median = as.numeric(median(FinishPosition, na.rm = TRUE))
  ), by = Name]
  
  results <- results[order(Avg_Finish)]
  
  # Round all numeric columns
  for (col in setdiff(names(results), "Name")) {
    results[, (col) := round(as.numeric(get(col)), 1)]
  }
  
  if (nrow(results) > max_display_rows) {
    cat("Note: Showing all", nrow(results), "drivers in finishing analysis\n")
  }
  
  return(results)
}

# Fixed fantasy points analysis with proper data type handling
analyze_dk_fantasy_points <- function(sim_results, max_sample_size = 1000000) {
  setDT(sim_results)
  
  n_total_results <- nrow(sim_results)

  
  # Sample for performance if dataset is very large
  analysis_data <- sim_results
  if (n_total_results > max_sample_size) {
    sample_size <- max_sample_size
    sample_indices <- sample(n_total_results, sample_size)
    analysis_data <- sim_results[sample_indices]
    cat("Using sample of", sample_size, "results for analysis (", 
        round(sample_size/n_total_results*100, 1), "%)\n")
  }
  
  # Ensure numeric columns are properly typed
  analysis_data[, DKSalary := as.numeric(DKSalary)]
  analysis_data[, Starting := as.numeric(Starting)]
  analysis_data[, DKOP := as.numeric(DKOP)]
  analysis_data[, DKFantasyPoints := as.numeric(DKFantasyPoints)]
  
  # Use explicit type conversion in aggregation
  results <- analysis_data[, .(
    DKSalary = as.numeric(first(DKSalary)),
    Starting = as.numeric(first(Starting)),
    DKOP = as.numeric(first(DKOP)),
    Median_Fantasy_Pts = as.numeric(median(DKFantasyPoints, na.rm = TRUE)),
    FP_90thPct = as.numeric(quantile(DKFantasyPoints, 0.9, na.rm = TRUE))
  ), by = .(Name)]
  
  # Add PPD calculation with proper type handling
  results[, PPD := as.numeric(Median_Fantasy_Pts / (DKSalary / 1000))]
  
  # Convert DKOP to percentage if needed
  if (max(results$DKOP, na.rm = TRUE) <= 1) {
    results[, DKOP := as.numeric(DKOP * 100)]
  }
  
  # Round all numeric columns
  numeric_cols <- c("DKSalary", "Starting", "DKOP", "Median_Fantasy_Pts", "FP_90thPct", "PPD")
  for (col in numeric_cols) {
    if (col %in% names(results)) {
      results[, (col) := round(as.numeric(get(col)), 1)]
    }
  }
  
  return(results)
}

# Fixed FD fantasy points analysis
analyze_fd_fantasy_points <- function(sim_results, max_sample_size = 1000000) {
  setDT(sim_results)
  
  n_total_results <- nrow(sim_results)

  
  # Sample for performance if dataset is very large
  analysis_data <- sim_results
  if (n_total_results > max_sample_size) {
    sample_size <- max_sample_size
    sample_indices <- sample(n_total_results, sample_size)
    analysis_data <- sim_results[sample_indices]
    cat("Using sample of", sample_size, "results for analysis (", 
        round(sample_size/n_total_results*100, 1), "%)\n")
  }
  
  # Ensure numeric columns are properly typed
  analysis_data[, FDSalary := as.numeric(FDSalary)]
  analysis_data[, Starting := as.numeric(Starting)]
  analysis_data[, FDOP := as.numeric(FDOP)]
  analysis_data[, FDFantasyPoints := as.numeric(FDFantasyPoints)]
  
  # Use explicit type conversion in aggregation
  results <- analysis_data[, .(
    FDSalary = as.numeric(first(FDSalary)),
    Starting = as.numeric(first(Starting)),
    FDOP = as.numeric(first(FDOP)),
    Median_Fantasy_Pts = as.numeric(median(FDFantasyPoints, na.rm = TRUE)),
    FP_90thPct = as.numeric(quantile(FDFantasyPoints, 0.9, na.rm = TRUE))
  ), by = .(Name)]
  
  # Add PPD calculation with proper type handling
  results[, PPD := as.numeric(Median_Fantasy_Pts / (FDSalary / 1000))]
  
  # Convert FDOP to percentage if needed
  if (max(results$FDOP, na.rm = TRUE) <= 1) {
    results[, FDOP := as.numeric(FDOP * 100)]
  }
  
  # Round all numeric columns
  numeric_cols <- c("FDSalary", "Starting", "FDOP", "Median_Fantasy_Pts", "FP_90thPct", "PPD")
  for (col in numeric_cols) {
    if (col %in% names(results)) {
      results[, (col) := round(as.numeric(get(col)), 1)]
    }
  }
  
  return(results)
}

# Fixed FD lap analysis
analyze_fd_lap_points <- function(sim_results) {
  setDT(sim_results)
  
  # Ensure numeric columns are properly typed
  sim_results[, FDSalary := as.numeric(FDSalary)]
  sim_results[, Starting := as.numeric(Starting)]
  sim_results[, FDLapPoints := as.numeric(FDLapPoints)]
  
  # Use explicit type conversion in aggregation
  results <- sim_results[, .(
    Starting = as.numeric(first(Starting)),
    FDSalary = as.numeric(first(FDSalary)),
    Avg_Lap = as.numeric(mean(FDLapPoints, na.rm = TRUE)),
    Median_Lap = as.numeric(median(FDLapPoints, na.rm = TRUE)),
    Max_Lap = as.numeric(max(FDLapPoints, na.rm = TRUE)),
    Min_Lap = as.numeric(min(FDLapPoints, na.rm = TRUE))
  ), by = Name]
  
  # Round numeric columns
  numeric_cols <- c("Starting", "FDSalary", "Avg_Lap", "Median_Lap", "Max_Lap", "Min_Lap")
  for (col in numeric_cols) {
    if (col %in% names(results)) {
      results[, (col) := round(as.numeric(get(col)), 1)]
    }
  }
  
  # Sort by Average Lap Points in descending order
  setorder(results, -Avg_Lap)
  
  return(results)
}




analyze_simulation_accuracy <- function(sim_results, input_data) {
  # Make sure we're working with data.tables
  setDT(sim_results)
  setDT(input_data)
  
  # Check if SimID exists, if not create it temporarily
  if (!"SimID" %in% names(sim_results)) {
    n_drivers <- length(unique(sim_results$Name))
    sim_results[, SimID := rep(1:(nrow(sim_results) %/% n_drivers + 1), each = n_drivers)[1:nrow(sim_results)]]
    created_simid <- TRUE
  } else {
    created_simid <- FALSE
  }
  
  # Get unique drivers
  drivers <- unique(input_data$Name)
  metrics <- c("W", "T3", "T5", "T10", "T15", "T20", "T25", "T30")
  
  results <- data.table()
  
  for (driver in drivers) {
    # Get simulation results for this driver
    driver_sims <- sim_results[Name == driver]
    driver_input <- input_data[Name == driver]
    
    total_sims <- nrow(driver_sims)
    if (total_sims == 0) next
    
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
    
    for (metric in metrics) {
      input_value <- driver_input[[metric]]
      sim_value <- sim_pcts[metric]
      
      if (!is.na(input_value)) {
        results <- rbind(
          results,
          data.table(
            Driver = driver,
            Metric = metric,
            Input = input_value * 100,
            Simulated = sim_value * 100,
            Difference = abs(input_value * 100 - sim_value * 100)
          )
        )
      }
    }
  }
  
  # Clean up temporary SimID if we created it
  if (created_simid) {
    sim_results[, SimID := NULL]
  }
  
  # Sort by driver and metric
  setorder(results, Driver, Metric)
  
  return(results)
}

# Simplified contest simulation - fixed setkey issue
simulate_double_up_contest <- function(optimal_lineups, simulation_results, platform = "DK", num_contests = 100) {
  cat("Starting vectorized double-up contest simulation...\n")
  
  if (platform == "DK") {
    fantasy_col <- "DKFantasyPoints"
    driver_cols <- paste0("Driver", 1:6)
    name_col <- "DKName"
    roster_size <- 6
  } else {
    fantasy_col <- "FDFantasyPoints"
    driver_cols <- paste0("Driver", 1:5)
    name_col <- "FDName"
    roster_size <- 5
  }
  
  setDT(optimal_lineups)
  setDT(simulation_results)
  
  sim_ids <- unique(simulation_results$SimID)
  n_sims <- length(sim_ids)
  n_lineups <- nrow(optimal_lineups)
  
  cat(sprintf("Processing %d lineups across %d simulations\n", n_lineups, n_sims))
  
  # Pre-calculate all lineup scores efficiently (same as your current code)
  cat("Pre-calculating lineup scores...\n")
  lineup_scores <- matrix(0, nrow = n_lineups, ncol = n_sims)
  
  for (s in 1:n_sims) {
    sim_data <- simulation_results[SimID == sim_ids[s]]
    
    for (i in 1:n_lineups) {
      lineup_drivers <- unlist(optimal_lineups[i, ..driver_cols])
      
      total_score <- 0
      for (d in 1:roster_size) {
        driver_match <- sim_data[get(name_col) == lineup_drivers[d]]
        if (nrow(driver_match) > 0) {
          total_score <- total_score + driver_match[[fantasy_col]][1]
        }
      }
      lineup_scores[i, s] <- total_score
    }
    
    if (s %% 50 == 0) {
      cat(sprintf("Pre-calc progress: %d/%d simulations\n", s, n_sims))
    }
  }
  
  # Field sampling weights
  field_weights <- optimal_lineups$CumulativeOwnership
  field_weights[is.na(field_weights)] <- min(field_weights, na.rm = TRUE)
  field_weights <- field_weights / sum(field_weights)
  
  # VECTORIZED contest simulation - this is the key improvement
  cat("Running vectorized contest simulations...\n")
  
  # Pre-allocate results
  wins <- integer(n_lineups)
  total_contests <- integer(n_lineups)
  
  contests_per_sim <- max(1, num_contests %/% n_sims)
  total_iterations <- n_sims * contests_per_sim
  completed <- 0
  
  for (s in 1:n_sims) {
    sim_scores <- lineup_scores[, s]  # Get all lineup scores for this simulation
    
    for (contest in 1:contests_per_sim) {
      # Generate field (49 lineups)
      field_indices <- sample(1:n_lineups, 49, prob = field_weights, replace = TRUE)
      field_scores <- sim_scores[field_indices]
      
      # VECTORIZED: Calculate cash threshold once
      cash_threshold <- quantile(field_scores, 0.5)  # 50th percentile for double-up
      
      # VECTORIZED: Test all lineups at once against this threshold
      lineup_cashes <- sim_scores >= cash_threshold
      
      # Update counts vectorized
      wins <- wins + as.integer(lineup_cashes)
      total_contests <- total_contests + 1
      
      completed <- completed + 1
      if (completed %% 25 == 0 || completed == total_iterations) {
        progress_pct <- round((completed / total_iterations) * 100, 1)
        cat(sprintf("Contest progress: %s%% (%d/%d contests)\n", progress_pct, completed, total_iterations))
      }
    }
  }
  
  # Calculate results
  win_rates <- (wins / total_contests) * 100
  
  final_results <- cbind(
    optimal_lineups[, ..driver_cols],
    optimal_lineups[, .(CumulativeOwnership, GeometricMean, Top1Count, Top3Count, Top5Count)],
    data.table(WinRate = win_rates, Wins = wins, TotalContests = total_contests)
  )
  
  setorder(final_results, -WinRate)
  
  cat("Vectorized contest simulation completed!\n")
  return(as.data.frame(final_results))
}

# Pre-compute all the static data structures we'll need
prepare_optimization_data <- function(sim_results, platform = "DK") {
  setDT(sim_results)
  
  if (platform == "DK") {
    fantasy_col <- "DKFantasyPoints"
    salary_col <- "DKSalary"
    name_col <- "DKName"
    op_col <- "DKOP"
    roster_size <- 6
    salary_cap <- 50000
  } else {
    fantasy_col <- "FDFantasyPoints"
    salary_col <- "FDSalary" 
    name_col <- "FDName"
    op_col <- "FDOP"
    roster_size <- 5
    salary_cap <- 50000
  }
  
  # Check if required columns exist
  required_cols <- c(fantasy_col, salary_col, name_col, op_col, "Name", "Starting", "SimID")
  missing_cols <- setdiff(required_cols, names(sim_results))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Get unique drivers and create lookup tables - FIXED syntax
  driver_info <- sim_results[, {
    list(
      Salary = first(.SD[[salary_col]]),
      OP = first(.SD[[op_col]]),
      Name = first(Name),
      Starting = first(Starting)
    )
  }, by = name_col]
  
  # Rename the grouping column to DriverID
  setnames(driver_info, name_col, "DriverID")
  setkey(driver_info, DriverID)
  
  # Create fantasy points matrix: drivers x simulations
  sim_ids <- unique(sim_results$SimID)
  driver_ids <- driver_info$DriverID
  
  # Pre-allocate matrix
  fantasy_matrix <- matrix(0, nrow = length(driver_ids), ncol = length(sim_ids))
  rownames(fantasy_matrix) <- driver_ids
  colnames(fantasy_matrix) <- as.character(sim_ids)
  
  # Fill matrix efficiently - FIXED syntax
  for (i in seq_along(driver_ids)) {
    driver_data <- sim_results[get(name_col) == driver_ids[i]]
    if (nrow(driver_data) > 0) {
      sim_indices <- match(driver_data$SimID, sim_ids)
      valid_indices <- !is.na(sim_indices)
      if (any(valid_indices)) {
        fantasy_matrix[i, sim_indices[valid_indices]] <- driver_data[[fantasy_col]][valid_indices]
      }
    }
  }
  
  # Pre-compute constraint matrix (this never changes)
  n_drivers <- length(driver_ids)
  constraint_matrix <- matrix(0, nrow = 2, ncol = n_drivers)
  constraint_matrix[1, ] <- driver_info$Salary  # Salary constraint
  constraint_matrix[2, ] <- 1                   # Roster size constraint
  
  constraint_dirs <- c("<=", "==")
  constraint_rhs <- c(salary_cap, roster_size)
  
  return(list(
    fantasy_matrix = fantasy_matrix,
    driver_info = driver_info,
    constraint_matrix = constraint_matrix,
    constraint_dirs = constraint_dirs,
    constraint_rhs = constraint_rhs,
    sim_ids = sim_ids,
    driver_ids = driver_ids,
    roster_size = roster_size,
    name_col = name_col
  ))
}

# Vectorized lineup finding - processes multiple simulations in parallel batches
find_optimal_lineups_vectorized <- function(opt_data, batch_size = 50, k_per_sim = 3) {
  n_sims <- length(opt_data$sim_ids)
  n_drivers <- length(opt_data$driver_ids)
  
  cat("Processing", n_sims, "simulations in batches of", batch_size, "...\n")
  
  # Store all lineups found
  all_lineups <- list()
  
  # Process simulations in batches for memory efficiency
  n_batches <- ceiling(n_sims / batch_size)
  
  for (batch in 1:n_batches) {
    start_sim <- (batch - 1) * batch_size + 1
    end_sim <- min(batch * batch_size, n_sims)
    batch_sims <- start_sim:end_sim
    
    if (batch %% 10 == 1 || batch == n_batches) {
      cat("Processing batch", batch, "of", n_batches, "(sims", start_sim, "to", end_sim, ")...\n")
    }
    
    # Process each simulation in this batch
    for (sim_idx in batch_sims) {
      sim_id <- opt_data$sim_ids[sim_idx]
      objective_vector <- opt_data$fantasy_matrix[, sim_idx]
      
      # Skip if all zeros (shouldn't happen but safety check)
      if (all(objective_vector == 0)) next
      
      # Find multiple optimal lineups per simulation for diversity
      excluded_combinations <- list()
      
      for (k in 1:k_per_sim) {
        # Create constraint matrix with exclusions
        n_constraints <- 2 + length(excluded_combinations)
        const_matrix <- matrix(0, nrow = n_constraints, ncol = n_drivers)
        
        # Basic constraints
        const_matrix[1:2, ] <- opt_data$constraint_matrix
        
        # Add exclusion constraints
        if (length(excluded_combinations) > 0) {
          for (exc_idx in seq_along(excluded_combinations)) {
            const_matrix[2 + exc_idx, excluded_combinations[[exc_idx]]] <- 1
          }
        }
        
        const_dirs <- c(opt_data$constraint_dirs, rep("<=", length(excluded_combinations)))
        const_rhs <- c(opt_data$constraint_rhs, rep(opt_data$roster_size - 1, length(excluded_combinations)))
        
        # Solve optimization
        result <- tryCatch({
          suppressWarnings(
            lp("max", objective_vector, const_matrix, const_dirs, const_rhs, 
               all.bin = TRUE, presolve = 0, compute.sens = 0)
          )
        }, error = function(e) {
          cat("LP solver error in sim", sim_id, "iteration", k, ":", e$message, "\n")
          NULL
        })
        
        if (!is.null(result) && result$status == 0) {
          selected_indices <- which(result$solution > 0.9)
          
          if (length(selected_indices) == opt_data$roster_size) {
            # Create lineup identifier
            selected_drivers <- sort(opt_data$driver_ids[selected_indices])
            lineup_key <- paste(selected_drivers, collapse = "|")
            
            # Track this lineup
            if (lineup_key %in% names(all_lineups)) {
              all_lineups[[lineup_key]]$sims <- c(all_lineups[[lineup_key]]$sims, sim_id)
              all_lineups[[lineup_key]]$ranks <- c(all_lineups[[lineup_key]]$ranks, k)
            } else {
              all_lineups[[lineup_key]] <- list(
                drivers = selected_drivers,
                sims = sim_id,
                ranks = k,
                total_salary = sum(opt_data$driver_info[selected_drivers]$Salary),
                avg_ownership = mean(opt_data$driver_info[selected_drivers]$OP, na.rm = TRUE)
              )
            }
            
            # Add to exclusions for next iteration
            excluded_combinations[[length(excluded_combinations) + 1]] <- selected_indices
          }
        } else if (!is.null(result)) {
          cat("LP solver failed with status", result$status, "in sim", sim_id, "iteration", k, "\n")
        }
      }
    }
    
    # Periodic cleanup and progress report
    if (batch %% 10 == 0) {
      gc(verbose = FALSE)
      cat("Completed", batch, "batches, found", length(all_lineups), "unique lineups so far...\n")
    }
  }
  
  return(all_lineups)
}

# Convert lineup results to final format with all metrics
process_lineup_results <- function(all_lineups, opt_data) {
  if (length(all_lineups) == 0) return(NULL)
  
  cat("Processing", length(all_lineups), "unique lineups...\n")
  
  # Calculate statistics for each lineup
  lineup_stats <- data.frame(
    Lineup = names(all_lineups),
    stringsAsFactors = FALSE
  )
  
  # Pre-allocate columns
  for (rank in 1:5) {
    lineup_stats[[paste0("Rank", rank, "Count")]] <- 0
  }
  
  # Fill in the rank counts
  for (i in seq_along(all_lineups)) {
    lineup_data <- all_lineups[[i]]
    
    # Count how many times this lineup appeared at each rank
    rank_counts <- table(factor(lineup_data$ranks, levels = 1:5))
    
    for (rank in 1:5) {
      lineup_stats[i, paste0("Rank", rank, "Count")] <- as.numeric(rank_counts[rank])
    }
  }
  
  # Calculate cumulative counts
  lineup_stats$Top1Count <- lineup_stats$Rank1Count
  lineup_stats$Top2Count <- lineup_stats$Rank1Count + lineup_stats$Rank2Count
  lineup_stats$Top3Count <- lineup_stats$Top2Count + lineup_stats$Rank3Count
  lineup_stats$Top5Count <- rowSums(lineup_stats[, paste0("Rank", 1:5, "Count")])
  
  # Add salary and ownership metrics efficiently
  cat("Adding salary and ownership metrics...\n")
  
  lineup_stats$TotalSalary <- sapply(seq_along(all_lineups), function(i) {
    all_lineups[[i]]$total_salary
  })
  
  # Calculate cumulative and geometric ownership
  lineup_stats$CumulativeOwnership <- sapply(lineup_stats$Lineup, function(lineup_str) {
    drivers <- strsplit(lineup_str, "\\|")[[1]]
    
    # Use match to find drivers in the lookup table
    driver_indices <- match(drivers, opt_data$driver_info$DriverID)
    valid_indices <- !is.na(driver_indices)
    
    if (any(valid_indices)) {
      ownerships <- opt_data$driver_info$OP[driver_indices[valid_indices]]
      
      # Convert to percentage if needed
      if (max(ownerships, na.rm = TRUE) <= 1) {
        ownerships <- ownerships * 100
      }
      sum(ownerships, na.rm = TRUE)
    } else {
      0
    }
  })
  
  lineup_stats$GeometricMean <- sapply(lineup_stats$Lineup, function(lineup_str) {
    drivers <- strsplit(lineup_str, "\\|")[[1]]
    
    # Use match to find drivers in the lookup table
    driver_indices <- match(drivers, opt_data$driver_info$DriverID)
    valid_indices <- !is.na(driver_indices)
    
    if (sum(valid_indices) == opt_data$roster_size) {
      ownerships <- opt_data$driver_info$OP[driver_indices[valid_indices]]
      
      # Convert to percentage if needed
      if (max(ownerships, na.rm = TRUE) <= 1) {
        ownerships <- ownerships * 100
      }
      
      if (all(!is.na(ownerships)) && all(ownerships > 0)) {
        exp(mean(log(ownerships)))
      } else {
        NA_real_
      }
    } else {
      NA_real_
    }
  })
  
  # Add starting position metrics efficiently
  lineup_stats$CumulativeStarting <- sapply(lineup_stats$Lineup, function(lineup_str) {
    drivers <- strsplit(lineup_str, "\\|")[[1]]
    
    # Use match to find drivers in the lookup table
    driver_indices <- match(drivers, opt_data$driver_info$DriverID)
    valid_indices <- !is.na(driver_indices)
    
    if (any(valid_indices)) {
      starting_positions <- opt_data$driver_info$Starting[driver_indices[valid_indices]]
      sum(starting_positions, na.rm = TRUE)
    } else {
      0
    }
  })
  
  lineup_stats$GeometricMeanStarting <- sapply(lineup_stats$Lineup, function(lineup_str) {
    drivers <- strsplit(lineup_str, "\\|")[[1]]
    
    # Use match to find drivers in the lookup table
    driver_indices <- match(drivers, opt_data$driver_info$DriverID)
    valid_indices <- !is.na(driver_indices)
    
    if (sum(valid_indices) == opt_data$roster_size) {
      starting_positions <- opt_data$driver_info$Starting[driver_indices[valid_indices]]
      
      if (all(!is.na(starting_positions)) && all(starting_positions > 0)) {
        exp(mean(log(starting_positions)))
      } else {
        NA_real_
      }
    } else {
      NA_real_
    }
  })
  
  # Sort by Top1Count descending
  lineup_stats <- lineup_stats[order(-lineup_stats$Top1Count), ]
  
  # Split into driver columns
  driver_cols <- do.call(rbind, strsplit(lineup_stats$Lineup, "\\|"))
  colnames(driver_cols) <- paste0("Driver", 1:opt_data$roster_size)
  
  # Combine everything
  final_result <- cbind(
    as.data.frame(driver_cols, stringsAsFactors = FALSE),
    lineup_stats[, !names(lineup_stats) %in% "Lineup"]
  )
  
  return(final_result)
}

# Main function that replaces your existing slow functions
count_optimal_lineups_efficient <- function(sim_results, platform = "DK") {
  cat("Starting efficient optimization for", platform, "- processing ALL simulations...\n")
  
  tryCatch({
    # Step 1: Prepare all optimization data structures
    cat("Preparing optimization data structures...\n")
    opt_data <- prepare_optimization_data(sim_results, platform)
    
    # Step 2: Find optimal lineups using vectorized approach
    cat("Finding optimal lineups across all simulations...\n")
    all_lineups <- find_optimal_lineups_vectorized(opt_data, batch_size = 50, k_per_sim = 3)
    
    if (length(all_lineups) == 0) {
      cat("No optimal lineups found.\n")
      return(NULL)
    }
    
    # Step 3: Process results into final format
    cat("Processing results into final format...\n")
    final_result <- process_lineup_results(all_lineups, opt_data)
    
    cat("Efficient optimization completed! Found", nrow(final_result), "unique lineups.\n")
    cat("Each lineup's frequency reflects its true optimality across ALL", length(opt_data$sim_ids), "simulations.\n")
    
    return(final_result)
    
  }, error = function(e) {
    cat("Error in efficient optimization:", e$message, "\n")
    cat("Simulation data columns:", paste(names(sim_results), collapse = ", "), "\n")
    stop(e)
  })
}

# REPLACE your calculate_dk_filtered_pool_stats function with this FIXED version
calculate_dk_filtered_pool_stats <- function(optimal_lineups, filters) {
  if (is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  setDT(optimal_lineups)
  filtered_lineups <- copy(optimal_lineups)
  
  # Apply Top count filters
  if (!is.null(filters$min_top1_count) && "Top1Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
  }
  if (!is.null(filters$min_top2_count) && "Top2Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top2Count >= filters$min_top2_count]
  }
  if (!is.null(filters$min_top3_count) && "Top3Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top3Count >= filters$min_top3_count]
  }
  if (!is.null(filters$min_top5_count) && "Top5Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top5Count >= filters$min_top5_count]
  }
  
  # Apply cumulative ownership filters
  if (!is.null(filters$min_cumulative_ownership) && "CumulativeOwnership" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[CumulativeOwnership >= filters$min_cumulative_ownership]
  }
  if (!is.null(filters$max_cumulative_ownership) && "CumulativeOwnership" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[CumulativeOwnership <= filters$max_cumulative_ownership]
  }
  
  # Apply geometric mean ownership filters
  if (!is.null(filters$min_geometric_mean) && "GeometricMean" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[GeometricMean >= filters$min_geometric_mean]
  }
  if (!is.null(filters$max_geometric_mean) && "GeometricMean" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[GeometricMean <= filters$max_geometric_mean]
  }
  
  # FIXED: Apply cumulative starting position filters
  if (!is.null(filters$min_cumulative_starting) && "CumulativeStarting" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[CumulativeStarting >= filters$min_cumulative_starting]
  }
  if (!is.null(filters$max_cumulative_starting) && "CumulativeStarting" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[CumulativeStarting <= filters$max_cumulative_starting]
  }
  
  # FIXED: Apply geometric mean starting position filters
  if (!is.null(filters$min_geometric_starting) && "GeometricMeanStarting" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting >= filters$min_geometric_starting]
  }
  if (!is.null(filters$max_geometric_starting) && "GeometricMeanStarting" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting <= filters$max_geometric_starting]
  }
  
  # Apply driver exclusion filter
  if (!is.null(filters$excluded_drivers) && length(filters$excluded_drivers) > 0) {
    driver_cols <- paste0("Driver", 1:DK_ROSTER_SIZE)
    filtered_lineups <- filtered_lineups[!rowSums(sapply(driver_cols, function(col) {
      filtered_lineups[[col]] %in% filters$excluded_drivers
    })) > 0]
  }
  
  # Return early if no lineups match the filters
  if (nrow(filtered_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  # Calculate thresholds for display
  thresholds <- list()
  threshold_columns <- c(
    "Top1Count", "Top2Count", "Top3Count", "Top5Count",
    "CumulativeOwnership", "GeometricMean",
    "CumulativeStarting", "GeometricMeanStarting"  # FIXED: Added these
  )
  
  for (col in threshold_columns) {
    if (col %in% names(filtered_lineups)) {
      min_val <- min(filtered_lineups[[col]], na.rm = TRUE)
      max_val <- max(filtered_lineups[[col]], na.rm = TRUE)
      
      # Use proper naming convention
      if (col == "CumulativeOwnership") {
        min_name <- "min_cumulative_ownership"
        max_name <- "max_cumulative_ownership"
      } else if (col == "GeometricMean") {
        min_name <- "min_geometric_mean"
        max_name <- "max_geometric_mean"
      } else if (col == "CumulativeStarting") {
        min_name <- "min_cumulative_starting"
        max_name <- "max_cumulative_starting"
      } else if (col == "GeometricMeanStarting") {
        min_name <- "min_geometric_starting"
        max_name <- "max_geometric_starting"
      } else {
        col_name <- gsub("Count", "", col)
        min_name <- paste0("min_", tolower(col_name))
        max_name <- paste0("max_", tolower(col_name))
      }
      
      thresholds[[min_name]] <- min_val
      thresholds[[max_name]] <- max_val
    }
  }
  
  return(list(count = nrow(filtered_lineups), thresholds = thresholds))
}

calculate_fd_filtered_pool_stats <- function(optimal_lineups, filters) {
  if (is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  setDT(optimal_lineups)
  filtered_lineups <- copy(optimal_lineups)
  
  # Apply Top count filters
  if (!is.null(filters$min_top1_count) && "Top1Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
  }
  if (!is.null(filters$min_top2_count) && "Top2Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top2Count >= filters$min_top2_count]
  }
  if (!is.null(filters$min_top3_count) && "Top3Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top3Count >= filters$min_top3_count]
  }
  if (!is.null(filters$min_top5_count) && "Top5Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[Top5Count >= filters$min_top5_count]
  }
  
  # Apply cumulative ownership filters
  if (!is.null(filters$min_cumulative_ownership) && "CumulativeOwnership" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[CumulativeOwnership >= filters$min_cumulative_ownership]
  }
  if (!is.null(filters$max_cumulative_ownership) && "CumulativeOwnership" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[CumulativeOwnership <= filters$max_cumulative_ownership]
  }
  
  # Apply geometric mean ownership filters
  if (!is.null(filters$min_geometric_mean) && "GeometricMean" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(GeometricMean) & GeometricMean >= filters$min_geometric_mean]
  }
  if (!is.null(filters$max_geometric_mean) && "GeometricMean" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(GeometricMean) & GeometricMean <= filters$max_geometric_mean]
  }
  
  # Apply starting position filters
  if (!is.null(filters$min_cumulative_starting) && "CumulativeStarting" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[CumulativeStarting >= filters$min_cumulative_starting]
  }
  if (!is.null(filters$max_cumulative_starting) && "CumulativeStarting" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[CumulativeStarting <= filters$max_cumulative_starting]
  }
  
  # Apply geometric mean starting position filters
  if (!is.null(filters$min_geometric_starting) && "GeometricMeanStarting" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting >= filters$min_geometric_starting]
  }
  if (!is.null(filters$max_geometric_starting) && "GeometricMeanStarting" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting <= filters$max_geometric_starting]
  }
  
  # Apply driver exclusion filter
  if (!is.null(filters$excluded_drivers) && length(filters$excluded_drivers) > 0) {
    driver_cols <- paste0("Driver", 1:FD_ROSTER_SIZE)
    filtered_lineups <- filtered_lineups[!rowSums(sapply(driver_cols, function(col) {
      filtered_lineups[[col]] %in% filters$excluded_drivers
    })) > 0]
  }
  
  # Return early if no lineups match the filters
  if (nrow(filtered_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  return(list(count = nrow(filtered_lineups), thresholds = NULL))
}



generate_random_dk_lineups <- function(optimal_lineups, filters) {
  filtered_lineups <- optimal_lineups
  
  # Apply filters with early return if empty
  if (is.null(filtered_lineups) || nrow(filtered_lineups) == 0) {
    return(NULL)
  }
  
  # Convert to data.table for better performance
  setDT(filtered_lineups)
  
  # Apply min threshold filters
  if (!is.null(filters$min_top1_count) &&
      filters$min_top1_count > 0) {
    filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
  }
  
  if (!is.null(filters$min_top2_count) &&
      filters$min_top2_count > 0) {
    filtered_lineups <- filtered_lineups[Top2Count >= filters$min_top2_count]
  }
  
  if (!is.null(filters$min_top3_count) &&
      filters$min_top3_count > 0) {
    filtered_lineups <- filtered_lineups[Top3Count >= filters$min_top3_count]
  }
  
  if (!is.null(filters$min_top5_count) &&
      filters$min_top5_count > 0) {
    filtered_lineups <- filtered_lineups[Top5Count >= filters$min_top5_count]
  }
  
  # Apply cumulative ownership filters
  if (!is.null(filters$min_cumulative_ownership) &&
      filters$min_cumulative_ownership > 0) {
    filtered_lineups <- filtered_lineups[CumulativeOwnership >= filters$min_cumulative_ownership]
  }
  
  if (!is.null(filters$max_cumulative_ownership) &&
      filters$max_cumulative_ownership > 0) {
    filtered_lineups <- filtered_lineups[CumulativeOwnership <= filters$max_cumulative_ownership]
  }
  
  # NEW: Apply geometric mean filters
  if (!is.null(filters$min_geometric_mean) &&
      filters$min_geometric_mean > 0) {
    filtered_lineups <- filtered_lineups[GeometricMean >= filters$min_geometric_mean]
  }
  
  if (!is.null(filters$max_geometric_mean) &&
      filters$max_geometric_mean > 0) {
    filtered_lineups <- filtered_lineups[GeometricMean <= filters$max_geometric_mean]
  }
  
  if (!is.null(filters$min_cumulative_starting) && filters$min_cumulative_starting > 0) {
    filtered_lineups <- filtered_lineups[CumulativeStarting >= filters$min_cumulative_starting]
  }
  if (!is.null(filters$max_cumulative_starting) && filters$max_cumulative_starting > 0) {
    filtered_lineups <- filtered_lineups[CumulativeStarting <= filters$max_cumulative_starting]
  }
  
  # NEW: Apply geometric mean starting position filters
  if (!is.null(filters$min_geometric_starting) && filters$min_geometric_starting > 0) {
    filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting >= filters$min_geometric_starting]
  }
  if (!is.null(filters$max_geometric_starting) && filters$max_geometric_starting > 0) {
    filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting <= filters$max_geometric_starting]
  }
  
  # Exclude specific drivers
  if (!is.null(filters$excluded_drivers) &&
      length(filters$excluded_drivers) > 0) {
    driver_cols <- paste0("Driver", 1:DK_ROSTER_SIZE)
    to_exclude <- logical(nrow(filtered_lineups))
    
    for (col in driver_cols) {
      to_exclude <- to_exclude |
        filtered_lineups[[col]] %in% filters$excluded_drivers
    }
    
    filtered_lineups <- filtered_lineups[!to_exclude]
  }
  
  if (nrow(filtered_lineups) == 0) {
    return(NULL)
  }
  
  # Pre-allocate for tracking
  driver_cols <- paste0("Driver", 1:DK_ROSTER_SIZE)
  all_drivers <- unique(unlist(filtered_lineups[, driver_cols, with = FALSE]))
  driver_counts <- setNames(numeric(length(all_drivers)), all_drivers)
  
  # Use the selected lineups data.table
  selected_lineups <- data.table()
  selected_indices <- integer(0)
  
  # Use Top1Count as weight for sampling
  weight_col <- "Top1Count"
  
  # Sampling loop with max attempts limit
  attempts <- 0
  max_attempts <- filters$num_lineups * 10
  
  while (nrow(selected_lineups) < filters$num_lineups &&
         attempts < max_attempts) {
    attempts <- attempts + 1
    
    # Available lineups
    available_indices <- setdiff(1:nrow(filtered_lineups), selected_indices)
    if (length(available_indices) == 0)
      break
    
    # Sample based on weights
    weights <- filtered_lineups[[weight_col]][available_indices]
    if (sum(weights) == 0)
      weights <- rep(1, length(available_indices))
    
    selected_idx <- tryCatch({
      sample(available_indices, 1, prob = weights)
    }, error = function(e) {
      sample(available_indices, 1)
    })
    
    candidate_lineup <- filtered_lineups[selected_idx]
    
    # Get drivers from this lineup
    candidate_drivers <- unlist(candidate_lineup[, driver_cols, with = FALSE])
    
    # Update driver counts - no exposure limit check
    driver_counts <- driver_counts + table(factor(candidate_drivers, levels = names(driver_counts)))
    
    # Add lineup unconditionally (no max exposure check)
    selected_lineups <- rbind(selected_lineups, candidate_lineup)
    selected_indices <- c(selected_indices, selected_idx)
    
    # Periodically clean up
    if (attempts %% 100 == 0)
      gc(verbose = FALSE)
  }
  
  if (nrow(selected_lineups) == 0)
    return(NULL)
  
  # Calculate exposure for attribute
  final_exposure <- (driver_counts / nrow(selected_lineups)) * 100
  attr(selected_lineups, "exposure") <- final_exposure
  
  return(as.data.frame(selected_lineups))
}


generate_random_fd_lineups <- function(optimal_lineups, filters) {
  filtered_lineups <- optimal_lineups
  
  # Apply filters with early return if empty
  if (is.null(filtered_lineups) || nrow(filtered_lineups) == 0) {
    return(NULL)
  }
  
  # Convert to data.table for better performance
  setDT(filtered_lineups)
  
  # Apply min threshold filters
  if (!is.null(filters$min_top1_count) &&
      filters$min_top1_count > 0) {
    filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
  }
  
  
  if (!is.null(filters$min_top2_count) &&
      filters$min_top2_count > 0) {
    filtered_lineups <- filtered_lineups[Top2Count >= filters$min_top2_count]
  }
  
  if (!is.null(filters$min_top3_count) &&
      filters$min_top3_count > 0) {
    filtered_lineups <- filtered_lineups[Top3Count >= filters$min_top3_count]
  }
  
  if (!is.null(filters$min_top5_count) &&
      filters$min_top5_count > 0) {
    filtered_lineups <- filtered_lineups[Top5Count >= filters$min_top5_count]
  }
  
  # Apply cumulative ownership filters
  if (!is.null(filters$min_cumulative_ownership) &&
      filters$min_cumulative_ownership > 0) {
    filtered_lineups <- filtered_lineups[CumulativeOwnership >= filters$min_cumulative_ownership]
  }
  if (!is.null(filters$max_cumulative_ownership) &&
      filters$max_cumulative_ownership > 0) {
    filtered_lineups <- filtered_lineups[CumulativeOwnership <= filters$max_cumulative_ownership]
  }
  
  # Apply geometric mean filters
  if (!is.null(filters$min_geometric_mean) &&
      filters$min_geometric_mean > 0) {
    filtered_lineups <- filtered_lineups[!is.na(GeometricMean) &
                                           GeometricMean >= filters$min_geometric_mean]
  }
  if (!is.null(filters$max_geometric_mean) &&
      filters$max_geometric_mean > 0) {
    filtered_lineups <- filtered_lineups[!is.na(GeometricMean) &
                                           GeometricMean <= filters$max_geometric_mean]
  }
  
  if (!is.null(filters$min_cumulative_starting) && filters$min_cumulative_starting > 0) {
    filtered_lineups <- filtered_lineups[CumulativeStarting >= filters$min_cumulative_starting]
  }
  if (!is.null(filters$max_cumulative_starting) && filters$max_cumulative_starting > 0) {
    filtered_lineups <- filtered_lineups[CumulativeStarting <= filters$max_cumulative_starting]
  }
  
  # NEW: Apply geometric mean starting position filters
  if (!is.null(filters$min_geometric_starting) && filters$min_geometric_starting > 0) {
    filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting >= filters$min_geometric_starting]
  }
  if (!is.null(filters$max_geometric_starting) && filters$max_geometric_starting > 0) {
    filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting <= filters$max_geometric_starting]
  }
  
  # Exclude specific drivers
  if (!is.null(filters$excluded_drivers) &&
      length(filters$excluded_drivers) > 0) {
    driver_cols <- paste0("Driver", 1:FD_ROSTER_SIZE)
    to_exclude <- logical(nrow(filtered_lineups))
    
    for (col in driver_cols) {
      to_exclude <- to_exclude |
        filtered_lineups[[col]] %in% filters$excluded_drivers
    }
    
    filtered_lineups <- filtered_lineups[!to_exclude]
  }
  
  if (nrow(filtered_lineups) == 0) {
    return(NULL)
  }
  
  # Pre-allocate for tracking
  driver_cols <- paste0("Driver", 1:FD_ROSTER_SIZE)
  all_drivers <- unique(unlist(filtered_lineups[, driver_cols, with = FALSE]))
  driver_counts <- setNames(numeric(length(all_drivers)), all_drivers)
  
  # Use the selected lineups data.table
  selected_lineups <- data.table()
  selected_indices <- integer(0)
  
  # Use Top1Count as weight for sampling
  weight_col <- "Top1Count"
  
  # Sampling loop with max attempts limit
  attempts <- 0
  max_attempts <- filters$num_lineups * 10
  
  while (nrow(selected_lineups) < filters$num_lineups &&
         attempts < max_attempts) {
    attempts <- attempts + 1
    
    # Available lineups
    available_indices <- setdiff(1:nrow(filtered_lineups), selected_indices)
    if (length(available_indices) == 0)
      break
    
    # Sample based on weights
    weights <- filtered_lineups[[weight_col]][available_indices]
    if (sum(weights) == 0)
      weights <- rep(1, length(available_indices))
    
    selected_idx <- tryCatch({
      sample(available_indices, 1, prob = weights)
    }, error = function(e) {
      sample(available_indices, 1)
    })
    
    candidate_lineup <- filtered_lineups[selected_idx]
    
    # Get drivers from this lineup
    candidate_drivers <- unlist(candidate_lineup[, driver_cols, with = FALSE])
    
    # Update driver counts - no exposure limit check
    driver_counts <- driver_counts + table(factor(candidate_drivers, levels = names(driver_counts)))
    
    # Add lineup unconditionally (no max exposure check)
    selected_lineups <- rbind(selected_lineups, candidate_lineup)
    selected_indices <- c(selected_indices, selected_idx)
    
    # Periodically clean up
    if (attempts %% 100 == 0)
      gc(verbose = FALSE)
  }
  
  if (nrow(selected_lineups) == 0)
    return(NULL)
  
  # Calculate exposure for attribute
  final_exposure <- (driver_counts / nrow(selected_lineups)) * 100
  attr(selected_lineups, "exposure") <- final_exposure
  
  return(as.data.frame(selected_lineups))
}

# REPLACE your calculate_dk_driver_exposure function with this FIXED version
calculate_dk_driver_exposure <- function(optimal_lineups,
                                         fantasy_analysis,
                                         random_lineups = NULL,
                                         current_filters = NULL) {
  # Quick validation
  if (is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame(Message = "No optimal lineups available."))
  }
  
  # Use data.table operations
  setDT(optimal_lineups)
  if (!is.null(fantasy_analysis))
    setDT(fantasy_analysis)
  if (!is.null(random_lineups))
    setDT(random_lineups)
  
  # Driver columns
  driver_cols <- grep("^Driver", names(optimal_lineups), value = TRUE)
  if (length(driver_cols) == 0) {
    return(data.frame(Message = "No driver columns found in lineups."))
  }
  
  # Get all unique drivers (these are DKNames)
  all_drivers <- unique(unlist(optimal_lineups[, driver_cols, with = FALSE]))
  
  # Initialize metrics data frame - use DKName as primary identifier
  metrics_data <- data.table(
    DKName = all_drivers,
    Name = NA_character_,
    DKSalary = NA_real_,
    DKOP = NA_real_,
    OptimalRate = 0,
    FilteredPoolRate = 0,
    Exposure = 0,
    Leverage = 0,
    Starting = NA_real_,
    Proj = NA_real_
  )
  
  # Calculate OptimalRate (percentage of Top1Count lineups with this driver)
  total_top1 <- sum(optimal_lineups$Top1Count, na.rm = TRUE)
  if (total_top1 > 0) {
    for (driver in all_drivers) {
      driver_appears <- logical(nrow(optimal_lineups))
      for (col in driver_cols) {
        driver_appears <- driver_appears |
          (optimal_lineups[[col]] == driver)
      }
      driver_matches <- which(driver_appears)
      
      driver_total <- sum(optimal_lineups$Top1Count[driver_matches], na.rm = TRUE)
      metrics_data[DKName == driver, OptimalRate := (driver_total / total_top1) * 100]
    }
  }
  
  # Calculate FilteredPoolRate based on current filters
  if (!is.null(current_filters)) {
    # Apply the same filters to get the filtered pool
    filtered_lineups <- copy(optimal_lineups)
    
    # Apply filters
    if (!is.null(current_filters$min_top1_count) &&
        current_filters$min_top1_count > 0) {
      filtered_lineups <- filtered_lineups[Top1Count >= current_filters$min_top1_count]
    }
    if (!is.null(current_filters$min_top2_count) &&
        current_filters$min_top2_count > 0) {
      filtered_lineups <- filtered_lineups[Top2Count >= current_filters$min_top2_count]
    }
    if (!is.null(current_filters$min_top3_count) &&
        current_filters$min_top3_count > 0) {
      filtered_lineups <- filtered_lineups[Top3Count >= current_filters$min_top3_count]
    }
    if (!is.null(current_filters$min_top5_count) &&
        current_filters$min_top5_count > 0) {
      filtered_lineups <- filtered_lineups[Top5Count >= current_filters$min_top5_count]
    }
    if (!is.null(current_filters$min_cumulative_ownership) &&
        current_filters$min_cumulative_ownership > 0) {
      filtered_lineups <- filtered_lineups[CumulativeOwnership >= current_filters$min_cumulative_ownership]
    }
    if (!is.null(current_filters$max_cumulative_ownership) &&
        current_filters$max_cumulative_ownership > 0) {
      filtered_lineups <- filtered_lineups[CumulativeOwnership <= current_filters$max_cumulative_ownership]
    }
    
    # Apply geometric mean filters
    if (!is.null(current_filters$min_geometric_mean) &&
        current_filters$min_geometric_mean > 0) {
      filtered_lineups <- filtered_lineups[GeometricMean >= current_filters$min_geometric_mean]
    }
    
    if (!is.null(current_filters$max_geometric_mean) &&
        current_filters$max_geometric_mean > 0) {
      filtered_lineups <- filtered_lineups[GeometricMean <= current_filters$max_geometric_mean]
    }
    
    # FIXED: Apply starting position filters
    if (!is.null(current_filters$min_cumulative_starting) &&
        current_filters$min_cumulative_starting > 0) {
      filtered_lineups <- filtered_lineups[CumulativeStarting >= current_filters$min_cumulative_starting]
    }
    if (!is.null(current_filters$max_cumulative_starting) &&
        current_filters$max_cumulative_starting > 0) {
      filtered_lineups <- filtered_lineups[CumulativeStarting <= current_filters$max_cumulative_starting]
    }
    if (!is.null(current_filters$min_geometric_starting) &&
        current_filters$min_geometric_starting > 0) {
      filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting >= current_filters$min_geometric_starting]
    }
    if (!is.null(current_filters$max_geometric_starting) &&
        current_filters$max_geometric_starting > 0) {
      filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting <= current_filters$max_geometric_starting]
    }
    
    if (!is.null(current_filters$excluded_drivers) &&
        length(current_filters$excluded_drivers) > 0) {
      to_exclude <- logical(nrow(filtered_lineups))
      for (col in driver_cols) {
        to_exclude <- to_exclude |
          filtered_lineups[[col]] %in% current_filters$excluded_drivers
      }
      filtered_lineups <- filtered_lineups[!to_exclude]
    }
    
    # Calculate FilteredPoolRate
    if (nrow(filtered_lineups) > 0) {
      for (driver in all_drivers) {
        driver_appears <- logical(nrow(filtered_lineups))
        for (col in driver_cols) {
          driver_appears <- driver_appears |
            (filtered_lineups[[col]] == driver)
        }
        metrics_data[DKName == driver, FilteredPoolRate := (sum(driver_appears) / nrow(filtered_lineups)) * 100]
      }
    }
  }
  
  # Calculate Exposure from random lineups
  if (!is.null(random_lineups) && nrow(random_lineups) > 0) {
    random_driver_cols <- grep("^Driver", names(random_lineups), value = TRUE)
    if (length(random_driver_cols) > 0) {
      for (driver in all_drivers) {
        driver_appears <- logical(nrow(random_lineups))
        for (col in random_driver_cols) {
          driver_appears <- driver_appears | (random_lineups[[col]] == driver)
        }
        metrics_data[DKName == driver, Exposure := (sum(driver_appears) / nrow(random_lineups)) * 100]
      }
    }
  }
  
  # Add driver information from fantasy analysis
  if (!is.null(fantasy_analysis) && nrow(fantasy_analysis) > 0) {
    is_mapping_format <- "DKName" %in% names(fantasy_analysis) &&
      "Name" %in% names(fantasy_analysis)
    
    if (is_mapping_format) {
      for (i in 1:nrow(metrics_data)) {
        dk_name <- metrics_data$DKName[i]
        match_idx <- which(fantasy_analysis$DKName == dk_name)
        
        if (length(match_idx) > 0) {
          idx <- match_idx[1]
          metrics_data$Name[i] <- fantasy_analysis$Name[idx]
          metrics_data$DKSalary[i] <- fantasy_analysis$DKSalary[idx]
          metrics_data$DKOP[i] <- fantasy_analysis$DKOP[idx]
          metrics_data$Starting[i] <- fantasy_analysis$Starting[idx]
          metrics_data$Proj[i] <- fantasy_analysis$Proj[idx]
        }
      }
    } else if ("DKName" %in% names(fantasy_analysis)) {
      for (i in 1:nrow(metrics_data)) {
        dk_name <- metrics_data$DKName[i]
        match_idx <- which(fantasy_analysis$DKName == dk_name)
        
        if (length(match_idx) > 0) {
          idx <- match_idx[1]
          metrics_data$Name[i] <- fantasy_analysis$Name[idx]
          metrics_data$DKSalary[i] <- fantasy_analysis$DKSalary[idx]
          metrics_data$DKOP[i] <- fantasy_analysis$DKOP[idx]
          metrics_data$Starting[i] <- fantasy_analysis$Starting[idx]
          if ("Median_Fantasy_Pts" %in% names(fantasy_analysis))
            metrics_data$Proj[i] <- fantasy_analysis$Median_Fantasy_Pts[idx]
        }
      }
    }
  }
  
  # Check if DKOP is already in percentage format
  if (!is.null(metrics_data$DKOP) &&
      !all(is.na(metrics_data$DKOP))) {
    if (max(metrics_data$DKOP, na.rm = TRUE) <= 1) {
      metrics_data[, DKOP := DKOP * 100]
    }
  }
  
  # Calculate leverage
  metrics_data[!is.na(DKOP) &
                 !is.na(Exposure), Leverage := Exposure - DKOP]
  
  # Sort by OptimalRate
  setorder(metrics_data, -OptimalRate)
  
  return(as.data.frame(metrics_data))
}


# REPLACE your calculate_fd_driver_exposure function with this FIXED version
calculate_fd_driver_exposure <- function(optimal_lineups,
                                         fantasy_analysis,
                                         random_lineups = NULL,
                                         current_filters = NULL) {
  # Quick validation
  if (is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame(Message = "No optimal lineups available."))
  }
  
  # Use data.table operations
  setDT(optimal_lineups)
  if (!is.null(fantasy_analysis))
    setDT(fantasy_analysis)
  if (!is.null(random_lineups))
    setDT(random_lineups)
  
  # Driver columns
  driver_cols <- grep("^Driver", names(optimal_lineups), value = TRUE)
  if (length(driver_cols) == 0) {
    return(data.frame(Message = "No driver columns found in lineups."))
  }
  
  # Get all unique drivers (these are FDNames)
  all_drivers <- unique(unlist(optimal_lineups[, driver_cols, with = FALSE]))
  
  # Initialize metrics data frame - use FDName as primary identifier
  metrics_data <- data.table(
    FDName = all_drivers,
    Name = NA_character_,
    FDSalary = NA_real_,
    FDOP = NA_real_,
    OptimalRate = 0,
    FilteredPoolRate = 0,
    Exposure = 0,
    Leverage = 0,
    Starting = NA_real_,
    Proj = NA_real_
  )
  
  # Calculate OptimalRate (percentage of Top1Count lineups with this driver)
  total_top1 <- sum(optimal_lineups$Top1Count, na.rm = TRUE)
  if (total_top1 > 0) {
    for (driver in all_drivers) {
      driver_appears <- logical(nrow(optimal_lineups))
      for (col in driver_cols) {
        driver_appears <- driver_appears |
          (optimal_lineups[[col]] == driver)
      }
      driver_matches <- which(driver_appears)
      
      driver_total <- sum(optimal_lineups$Top1Count[driver_matches], na.rm = TRUE)
      metrics_data[FDName == driver, OptimalRate := (driver_total / total_top1) * 100]
    }
  }
  
  # Calculate FilteredPoolRate based on current filters
  if (!is.null(current_filters)) {
    # Apply the same filters to get the filtered pool
    filtered_lineups <- copy(optimal_lineups)
    
    # Apply filters
    if (!is.null(current_filters$min_top1_count) &&
        current_filters$min_top1_count > 0) {
      filtered_lineups <- filtered_lineups[Top1Count >= current_filters$min_top1_count]
    }
    if (!is.null(current_filters$min_top2_count) &&
        current_filters$min_top2_count > 0) {
      filtered_lineups <- filtered_lineups[Top2Count >= current_filters$min_top2_count]
    }
    if (!is.null(current_filters$min_top3_count) &&
        current_filters$min_top3_count > 0) {
      filtered_lineups <- filtered_lineups[Top3Count >= current_filters$min_top3_count]
    }
    if (!is.null(current_filters$min_top5_count) &&
        current_filters$min_top5_count > 0) {
      filtered_lineups <- filtered_lineups[Top5Count >= current_filters$min_top5_count]
    }
    if (!is.null(current_filters$min_cumulative_ownership) &&
        current_filters$min_cumulative_ownership > 0) {
      filtered_lineups <- filtered_lineups[CumulativeOwnership >= current_filters$min_cumulative_ownership]
    }
    if (!is.null(current_filters$max_cumulative_ownership) &&
        current_filters$max_cumulative_ownership > 0) {
      filtered_lineups <- filtered_lineups[CumulativeOwnership <= current_filters$max_cumulative_ownership]
    }
    
    # Apply geometric mean filters
    if (!is.null(current_filters$min_geometric_mean) &&
        current_filters$min_geometric_mean > 0) {
      filtered_lineups <- filtered_lineups[GeometricMean >= current_filters$min_geometric_mean]
    }
    
    if (!is.null(current_filters$max_geometric_mean) &&
        current_filters$max_geometric_mean > 0) {
      filtered_lineups <- filtered_lineups[GeometricMean <= current_filters$max_geometric_mean]
    }
    
    # FIXED: Apply starting position filters
    if (!is.null(current_filters$min_cumulative_starting) &&
        current_filters$min_cumulative_starting > 0) {
      filtered_lineups <- filtered_lineups[CumulativeStarting >= current_filters$min_cumulative_starting]
    }
    if (!is.null(current_filters$max_cumulative_starting) &&
        current_filters$max_cumulative_starting > 0) {
      filtered_lineups <- filtered_lineups[CumulativeStarting <= current_filters$max_cumulative_starting]
    }
    if (!is.null(current_filters$min_geometric_starting) &&
        current_filters$min_geometric_starting > 0) {
      filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting >= current_filters$min_geometric_starting]
    }
    if (!is.null(current_filters$max_geometric_starting) &&
        current_filters$max_geometric_starting > 0) {
      filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting <= current_filters$max_geometric_starting]
    }
    
    if (!is.null(current_filters$excluded_drivers) &&
        length(current_filters$excluded_drivers) > 0) {
      to_exclude <- logical(nrow(filtered_lineups))
      for (col in driver_cols) {
        to_exclude <- to_exclude |
          filtered_lineups[[col]] %in% current_filters$excluded_drivers
      }
      filtered_lineups <- filtered_lineups[!to_exclude]
    }
    
    # Calculate FilteredPoolRate
    if (nrow(filtered_lineups) > 0) {
      for (driver in all_drivers) {
        driver_appears <- logical(nrow(filtered_lineups))
        for (col in driver_cols) {
          driver_appears <- driver_appears |
            (filtered_lineups[[col]] == driver)
        }
        metrics_data[FDName == driver, FilteredPoolRate := (sum(driver_appears) / nrow(filtered_lineups)) * 100]
      }
    }
  }
  
  # Calculate Exposure from random lineups
  if (!is.null(random_lineups) && nrow(random_lineups) > 0) {
    random_driver_cols <- grep("^Driver", names(random_lineups), value = TRUE)
    if (length(random_driver_cols) > 0) {
      for (driver in all_drivers) {
        driver_appears <- logical(nrow(random_lineups))
        for (col in random_driver_cols) {
          driver_appears <- driver_appears | (random_lineups[[col]] == driver)
        }
        metrics_data[FDName == driver, Exposure := (sum(driver_appears) / nrow(random_lineups)) * 100]
      }
    }
  }
  
  # Add driver information from fantasy analysis
  if (!is.null(fantasy_analysis) && nrow(fantasy_analysis) > 0) {
    is_mapping_format <- "FDName" %in% names(fantasy_analysis) &&
      "Name" %in% names(fantasy_analysis)
    
    if (is_mapping_format) {
      for (i in 1:nrow(metrics_data)) {
        fd_name <- metrics_data$FDName[i]
        match_idx <- which(fantasy_analysis$FDName == fd_name)
        
        if (length(match_idx) > 0) {
          idx <- match_idx[1]
          metrics_data$Name[i] <- fantasy_analysis$Name[idx]
          metrics_data$FDSalary[i] <- fantasy_analysis$FDSalary[idx]
          metrics_data$FDOP[i] <- fantasy_analysis$FDOP[idx]
          metrics_data$Starting[i] <- fantasy_analysis$Starting[idx]
          metrics_data$Proj[i] <- fantasy_analysis$Proj[idx]
        }
      }
    } else if ("FDName" %in% names(fantasy_analysis)) {
      for (i in 1:nrow(metrics_data)) {
        fd_name <- metrics_data$FDName[i]
        match_idx <- which(fantasy_analysis$FDName == fd_name)
        
        if (length(match_idx) > 0) {
          idx <- match_idx[1]
          metrics_data$Name[i] <- fantasy_analysis$Name[idx]
          metrics_data$FDSalary[i] <- fantasy_analysis$FDSalary[idx]
          metrics_data$FDOP[i] <- fantasy_analysis$FDOP[idx]
          metrics_data$Starting[i] <- fantasy_analysis$Starting[idx]
          if ("Median_Fantasy_Pts" %in% names(fantasy_analysis))
            metrics_data$Proj[i] <- fantasy_analysis$Median_Fantasy_Pts[idx]
        }
      }
    }
  }
  
  # Check if FDOP is already in percentage format (0-100)
  if (!is.null(metrics_data$FDOP) &&
      !all(is.na(metrics_data$FDOP))) {
    if (max(metrics_data$FDOP, na.rm = TRUE) <= 1) {
      metrics_data[, FDOP := FDOP * 100]
    }
  }
  
  # Calculate leverage
  metrics_data[!is.na(FDOP) &
                 !is.na(Exposure), Leverage := Exposure - FDOP]
  
  # Sort by OptimalRate
  setorder(metrics_data, -OptimalRate)
  
  return(as.data.frame(metrics_data))
}

# Define UI
# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(
    title = tags$div(style = "display: flex; align-items: center; font-weight: bold;", "NASCAR Fantasy Sims"),
    titleWidth = 350  # Increased from 300 to give more space
  ),
  
  # Dashboard sidebar
  dashboardSidebar(
    useShinyjs(),
    div(
      style = "text-align: center; padding: 10px; margin-bottom: 5px;",
      tags$img(src = "logo.jpg", height = "200px", width = "auto", 
               style = "border: 2px solid #FFD700; border-radius: 10px;")
    ),
    br(),
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Input Check", tabName = "upload", icon = icon("upload")),
      menuItem(
        "Finish Analysis",
        tabName = "finish_analysis",
        icon = icon("chart-line")
      ),
      menuItem(
        "Dominator Analysis",
        tabName = "dominator",
        icon = icon("trophy")
      ),
      menuItem(
        "Fantasy Projections",
        tabName = "fantasy",
        icon = icon("calculator")
      ),
      menuItem(
        "Optimal Lineups",
        tabName = "optimal_lineups",
        icon = icon("trophy")
      ),
      menuItem(
        "Lineup Builder",
        tabName = "lineup_builder",
        icon = icon("percentage")
      ),
      menuItem("Contest Simulator", tabName = "contest_sim", icon = icon("trophy"))
    ),
    br(),
    fileInput("excel_file", "Upload Excel File", accept = c(".xlsx")),
    numericInput(
      "n_sims",
      "Number of Simulations:",
      value = 10000,
      min = 100,
      max = 25000
    ),
    actionButton(
      "run_sim",
      "Run Simulation",
      class = "btn-primary",
      style = "margin: 15px; width: 90%"
    ),
    div(id = "sim_status", class = "text-center", style = "margin-top: 10px;")
  ),
  
  # Dashboard body
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    
    tabItems(
      # Upload Tab
      tabItem(tabName = "upload", uiOutput("upload_content")),
      
      # Finish Analysis Tab
      tabItem(
        tabName = "finish_analysis", 
        fluidRow(
          div(
            style = "text-align: right; margin: 10px 15px;",
            downloadButton('downloadResults', 'Download Full Simulation Results', style = "margin-top: 5px;")
          )
        ), 
        fluidRow(
          box(
            width = 12,
            title = "Simulated Finishing Results",
            DTOutput("driver_stats") %>% withSpinner(color = "#FFD700")
          )
        ), 
        fluidRow(
          box(
            width = 12,
            title = "Finish Position Boxplot",
            plotlyOutput("position_box", height = "1000px") %>% withSpinner(color = "#FFD700")
          )
        )
      ),
      
      # Dominator Analysis Tab
      tabItem(tabName = "dominator", uiOutput("dominator_ui")),
      
      # Fantasy Projections Tab
      tabItem(tabName = "fantasy", uiOutput("fantasy_ui")),
      
      # Optimal Lineups Tab
      tabItem(
        tabName = "optimal_lineups",
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
                actionButton(
                  "run_dk_optimization",
                  "Calculate DraftKings Lineups",
                  class = "btn-primary",
                  style = "flex: 1; padding: 3px; font-size: 12px;"
                )
              ),
              conditionalPanel(
                condition = "output.has_fanduel === 'true'",
                actionButton(
                  "run_fd_optimization",
                  "Calculate FanDuel Lineups",
                  class = "btn-primary",
                  style = "flex: 1; padding: 3px; font-size: 12px;"
                )
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
        # DraftKings optimal lineups results
        conditionalPanel(
          condition = "output.has_dk_lineups === 'true'", 
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Optimal Lineups",
              div(
                style = "text-align: right; margin-bottom: 10px;",
                downloadButton(
                  'download_dk_optimal_lineups',
                  'Download All DK Lineups',
                  style = "margin-top: 10px;"
                )
              ),
              DTOutput("dk_optimal_lineups_table")
            )
          )
        ),
        # FanDuel optimal lineups results
        conditionalPanel(
          condition = "output.has_fd_lineups === 'true'", 
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Optimal Lineups",
              div(
                style = "text-align: right; margin-bottom: 10px;",
                downloadButton(
                  'download_fd_optimal_lineups',
                  'Download All FD Lineups',
                  style = "margin-top: 10px;"
                )
              ),
              DTOutput("fd_optimal_lineups_table")
            )
          )
        )
      ),
      
      # Lineup Builder Tab
      tabItem(tabName = "lineup_builder", uiOutput("lineup_builder_ui")),
      
      # Contest Simulator Tab
      tabItem(
        tabName = "contest_sim",
        
        # DraftKings Contest Simulator
        conditionalPanel(
          condition = "output.has_dk_lineups == 'true'",
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Contest Simulation",
              fluidRow(
                column(4,
                       h4("Lineup Filters"),
                       sliderInput("dk_contest_ownership_range", "Cumulative Ownership Range:",
                                   min = 0, max = 600, value = c(0, 600), step = 5),
                       sliderInput("dk_contest_geometric_range", "Geometric Mean Ownership Range:",
                                   min = 0, max = 100, value = c(0, 100), step = 0.5),
                       sliderInput("dk_contest_starting_range", "Cumulative Starting Position Range:",
                                   min = 6, max = 240, value = c(6, 240), step = 1),
                       sliderInput("dk_contest_starting_geo_range", "Geometric Mean Starting Position Range:",
                                   min = 1, max = 40, value = c(1, 40), step = 0.1),
                       selectizeInput("dk_contest_excluded_drivers", "Exclude Drivers:",
                                      choices = NULL, multiple = TRUE,
                                      options = list(plugins = list('remove_button'),
                                                     placeholder = 'Select drivers to exclude'))
                ),
                column(4,
                       h4("Simulation Settings"),
                       numericInput("dk_contest_num_contests", "Number of Contests:", 
                                    value = 100, min = 50, max = 500, step = 50),
                       div(class = "well well-sm",
                           h4("Filtered Pool:"),
                           textOutput("dk_contest_filtered_count")),
                       actionButton("run_dk_contest_simulation", "Run Contest Simulation", 
                                    class = "btn-primary btn-lg", style = "width: 100%; margin: 15px 0;")
                ),
                column(4,
                       h4("Results Filters"),
                       conditionalPanel(
                         condition = "output.has_dk_contest_results == 'true'",
                         numericInput("dk_contest_min_cash_rate", "Min Cash Rate (%):", 
                                      value = 40, min = 0, max = 100, step = 5),
                         numericInput("dk_contest_show_top", "Show Top X Lineups:", 
                                      value = 20, min = 1, max = 100, step = 5),
                         br(),
                         downloadButton("download_dk_contest_results", "Download All Results", 
                                        style = "width: 100%;")
                       ),
                       conditionalPanel(
                         condition = "output.has_dk_contest_results != 'true'",
                         div(class = "alert alert-info", style = "margin-top: 20px;",
                             "Results filters will appear after running simulation")
                       )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "output.has_dk_contest_results == 'true'",
            fluidRow(
              box(width = 12, title = "DraftKings Driver Contest Analysis",
                  DTOutput("dk_contest_driver_table") %>% withSpinner(color = "#FFD700"))
            ),
            fluidRow(
              box(width = 12, title = "Top Contest Lineups",
                  DTOutput("dk_contest_lineups_table") %>% withSpinner(color = "#FFD700"))
            )
          )
        ),
        
        # FanDuel Contest Simulator
        conditionalPanel(
          condition = "output.has_fd_lineups == 'true'",
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Contest Simulation",
              fluidRow(
                column(4,
                       h4("Lineup Filters"),
                       sliderInput("fd_contest_ownership_range", "Cumulative Ownership Range:",
                                   min = 0, max = 500, value = c(0, 500), step = 5),
                       sliderInput("fd_contest_geometric_range", "Geometric Mean Ownership Range:",
                                   min = 0, max = 100, value = c(0, 100), step = 0.5),
                       sliderInput("fd_contest_starting_range", "Cumulative Starting Position Range:",
                                   min = 5, max = 200, value = c(5, 200), step = 1),
                       sliderInput("fd_contest_starting_geo_range", "Geometric Mean Starting Position Range:",
                                   min = 1, max = 40, value = c(1, 40), step = 0.1),
                       selectizeInput("fd_contest_excluded_drivers", "Exclude Drivers:",
                                      choices = NULL, multiple = TRUE,
                                      options = list(plugins = list('remove_button'),
                                                     placeholder = 'Select drivers to exclude'))
                ),
                column(4,
                       h4("Simulation Settings"),
                       numericInput("fd_contest_num_contests", "Number of Contests:", 
                                    value = 100, min = 50, max = 500, step = 50),
                       div(class = "well well-sm",
                           h4("Filtered Pool:"),
                           textOutput("fd_contest_filtered_count")),
                       actionButton("run_fd_contest_simulation", "Run Contest Simulation", 
                                    class = "btn-primary btn-lg", style = "width: 100%; margin: 15px 0;")
                ),
                column(4,
                       h4("Results Filters"),
                       conditionalPanel(
                         condition = "output.has_fd_contest_results == 'true'",
                         numericInput("fd_contest_min_cash_rate", "Min Cash Rate (%):", 
                                      value = 40, min = 0, max = 100, step = 5),
                         numericInput("fd_contest_show_top", "Show Top X Lineups:", 
                                      value = 20, min = 1, max = 100, step = 5),
                         br(),
                         downloadButton("download_fd_contest_results", "Download All Results", 
                                        style = "width: 100%;")
                       ),
                       conditionalPanel(
                         condition = "output.has_fd_contest_results != 'true'",
                         div(class = "alert alert-info", style = "margin-top: 20px;",
                             "Results filters will appear after running simulation")
                       )
                )
              )
            )
          ),
          conditionalPanel(
            condition = "output.has_fd_contest_results == 'true'",
            fluidRow(
              box(width = 12, title = "FanDuel Driver Contest Analysis",
                  DTOutput("fd_contest_driver_table") %>% withSpinner(color = "#FFD700"))
            ),
            fluidRow(
              box(width = 12, title = "Top Contest Lineups",
                  DTOutput("fd_contest_lineups_table") %>% withSpinner(color = "#FFD700"))
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
    simulation_complete = FALSE,
    field_lineups = NULL,
    dk_contest_results = NULL,
    fd_contest_results = NULL,
    dk_optimal_lineups_display = NULL,
    dk_optimal_lineups_full = NULL,
    fd_optimal_lineups_display = NULL,
    fd_optimal_lineups_full = NULL,
    dk_contest_results_full = NULL,
    fd_contest_results_full = NULL,
    dk_contest_driver_analysis = NULL,
    fd_contest_driver_analysis = NULL,
    updating_sliders = FALSE,
    sliders_initialized = list(dk = FALSE, fd = FALSE),  # NEW: Track initialization
    user_modified_sliders = list(dk = FALSE, fd = FALSE)  # NEW: Track user changes
  )
  
  # Configure memory settings when server starts
  configure_memory_settings()
  
  dk_filters <- reactive({
    list(
      min_top1_count = input$dk_min_top1_count,
      min_top2_count = input$dk_min_top2_count,
      min_top3_count = input$dk_min_top3_count,
      min_top5_count = input$dk_min_top5_count,
      min_cumulative_ownership = input$dk_ownership_range[1],
      max_cumulative_ownership = input$dk_ownership_range[2],
      min_geometric_mean = input$dk_geometric_range[1],
      max_geometric_mean = input$dk_geometric_range[2],
      min_cumulative_starting = input$dk_starting_range[1],    # NEW
      max_cumulative_starting = input$dk_starting_range[2],    # NEW
      min_geometric_starting = input$dk_starting_geo_range[1], # NEW
      max_geometric_starting = input$dk_starting_geo_range[2], # NEW
      excluded_drivers = input$dk_excluded_drivers
    )
  })
  
  fd_filters <- reactive({
    list(
      min_top1_count = input$fd_min_top1_count,
      min_top2_count = input$fd_min_top2_count,
      min_top3_count = input$fd_min_top3_count,
      min_top5_count = input$fd_min_top5_count,
      min_cumulative_ownership = input$fd_ownership_range[1],
      max_cumulative_ownership = input$fd_ownership_range[2],
      min_geometric_mean = input$fd_geometric_range[1],
      max_geometric_mean = input$fd_geometric_range[2],
      min_cumulative_starting = input$fd_starting_range[1],
      max_cumulative_starting = input$fd_starting_range[2],
      min_geometric_starting = input$fd_starting_geo_range[1],
      max_geometric_starting = input$fd_starting_geo_range[2],
      excluded_drivers = input$fd_excluded_drivers
    )
  })
  
  
  # Periodic memory cleanup (every 5 minutes)
  observe({
    invalidateLater(300000, session)  # 5 minutes
    
    # Only clean up if we have significant data
    if (!is.null(rv$simulation_results)) {
      cleanup_memory(verbose = FALSE)
    }
  })
  # REPLACE your existing DraftKings filter observer with this FIXED version
  observeEvent({
    list(
      input$dk_min_top1_count,
      input$dk_min_top2_count, 
      input$dk_min_top3_count,
      input$dk_min_top5_count,
      input$dk_ownership_range,
      input$dk_geometric_range,
      input$dk_starting_range,       
      input$dk_starting_geo_range, 
      input$dk_excluded_drivers
    )
  }, {
    # Only process if:
    # 1. Not currently updating sliders programmatically
    # 2. Sliders have been initialized 
    # 3. We have the necessary data
    if (!isTRUE(rv$updating_sliders) && 
        isTRUE(rv$sliders_initialized$dk) &&
        !is.null(rv$dk_optimal_lineups) && 
        !is.null(rv$dk_driver_exposure)) {
      
      # Mark that user has modified sliders
      rv$user_modified_sliders$dk <- TRUE
      
      # Get existing driver mapping from the current exposure data
      existing_mapping <- rv$dk_driver_exposure[, c("DKName", "Name", "DKSalary", "DKOP", "Starting", "Proj")]
      
      # Calculate updated driver exposure with new filters - FIXED: Added ALL filters
      current_filters <- list(
        min_top1_count = input$dk_min_top1_count,
        min_top2_count = input$dk_min_top2_count,
        min_top3_count = input$dk_min_top3_count,
        min_top5_count = input$dk_min_top5_count,
        min_cumulative_ownership = input$dk_ownership_range[1],
        max_cumulative_ownership = input$dk_ownership_range[2],
        min_geometric_mean = input$dk_geometric_range[1],
        max_geometric_mean = input$dk_geometric_range[2],
        min_cumulative_starting = input$dk_starting_range[1],      # FIXED: These were missing!
        max_cumulative_starting = input$dk_starting_range[2],      # FIXED: These were missing!
        min_geometric_starting = input$dk_starting_geo_range[1],   # FIXED: These were missing!
        max_geometric_starting = input$dk_starting_geo_range[2],   # FIXED: These were missing!
        excluded_drivers = input$dk_excluded_drivers
      )
      
      rv$dk_driver_exposure <- calculate_dk_driver_exposure(
        rv$dk_optimal_lineups,
        existing_mapping,
        rv$dk_random_lineups,
        current_filters
      )
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  observeEvent({
    list(
      input$fd_min_top1_count,
      input$fd_min_top2_count,
      input$fd_min_top3_count, 
      input$fd_min_top5_count,
      input$fd_ownership_range,
      input$fd_geometric_range,
      input$fd_starting_range,    
      input$fd_starting_geo_range,
      input$fd_excluded_drivers
    )
  }, {
    # Only process if we have the necessary data and sliders are initialized
    if (!isTRUE(rv$updating_sliders) && 
        isTRUE(rv$sliders_initialized$fd) &&
        !is.null(rv$fd_optimal_lineups) && 
        !is.null(rv$fd_driver_exposure)) {
      
      # Mark that user has modified sliders
      rv$user_modified_sliders$fd <- TRUE
      
      # Get existing driver mapping from the current exposure data
      existing_mapping <- rv$fd_driver_exposure[, c("FDName", "Name", "FDSalary", "FDOP", "Starting", "Proj")]
      
      # Calculate updated driver exposure with new filters
      current_filters <- list(
        min_top1_count = input$fd_min_top1_count,
        min_top2_count = input$fd_min_top2_count,
        min_top3_count = input$fd_min_top3_count,
        min_top5_count = input$fd_min_top5_count,
        min_cumulative_ownership = input$fd_ownership_range[1],
        max_cumulative_ownership = input$fd_ownership_range[2],
        min_geometric_mean = input$fd_geometric_range[1],
        max_geometric_mean = input$fd_geometric_range[2],
        min_cumulative_starting = input$fd_starting_range[1],
        max_cumulative_starting = input$fd_starting_range[2],
        min_geometric_starting = input$fd_starting_geo_range[1],
        max_geometric_starting = input$fd_starting_geo_range[2],
        excluded_drivers = input$fd_excluded_drivers
      )
      
      rv$fd_driver_exposure <- calculate_fd_driver_exposure(
        rv$fd_optimal_lineups,
        existing_mapping,
        rv$fd_random_lineups,
        current_filters
      )
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # Cleanup when simulation data changes
  observeEvent(rv$simulation_results, {
    # Clean up previous analysis results when new simulation starts
    if (!is.null(rv$simulation_results)) {
      # Remove any large intermediate objects that might be hanging around
      cleanup_memory(verbose = FALSE)
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(rv$sliders_initialized$fd, {
    if (isTRUE(rv$sliders_initialized$fd)) {
      # Force a recalculation of the filtered pool size after a short delay
      observe({
        invalidateLater(500, session)  # Wait 500ms for inputs to stabilize
        if (exists("input") && !is.null(input$fd_ownership_range)) {
          # Trigger the output to recalculate by invalidating it
          outputOptions(output, "fd_filtered_pool_size", suspendWhenHidden = FALSE)
        }
      })
    }
  }, once = TRUE)
  
  
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
  
  output$has_fd_lineups <- reactive({
    # Convert boolean TRUE/FALSE to lowercase string "true"/"false"
    result <- tolower(as.character(
      !is.null(rv$fd_optimal_lineups) && nrow(rv$fd_optimal_lineups) > 0
    ))
    return(result)
  })
  outputOptions(output, "has_fd_lineups", suspendWhenHidden = FALSE)
  
  output$has_dk_lineups <- reactive({
    # Convert boolean TRUE/FALSE to lowercase string "true"/"false"
    result <- tolower(as.character(
      !is.null(rv$dk_optimal_lineups) && nrow(rv$dk_optimal_lineups) > 0
    ))
    return(result)
  })
  outputOptions(output, "has_dk_lineups", suspendWhenHidden = FALSE)
  
  output$has_fd_random_lineups <- reactive({
    result <- tolower(as.character(
      !is.null(rv$fd_random_lineups) && nrow(rv$fd_random_lineups) > 0
    ))
    return(result)
  })
  outputOptions(output, "has_fd_random_lineups", suspendWhenHidden = FALSE)
  
  # Add these around line 1420
  output$has_dk_contest_results <- reactive({
    result <- tolower(as.character(!is.null(rv$dk_contest_results_full)))
    return(result)
  })
  outputOptions(output, "has_dk_contest_results", suspendWhenHidden = FALSE)
  
  output$has_fd_contest_results <- reactive({
    result <- tolower(as.character(!is.null(rv$fd_contest_results_full)))
    return(result)
  })
  outputOptions(output, "has_fd_contest_results", suspendWhenHidden = FALSE)
  
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
        rv$field_lineups <- NULL
        
        
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
          for (col in columns_to_remove) {
            if (col %in% colnames(display_data)) {
              display_data[[col]] <- NULL
            }
          }
          
          # Create the datatable with better column alignment
          dt <- datatable(
            display_data,
            options = list(
              scrollX = TRUE,
              pageLength = -1,
              # Show all rows
              autoWidth = FALSE,
              # Don't use autoWidth for better control
              dom = "t",
              # Only show table ('t'), no search/pagination
              ordering = TRUE,
              # Allow sorting
              columnDefs = list(
                list(className = 'dt-center', targets = "_all")  # Center-align all columns
              ),
              scrollCollapse = TRUE,
              fixedColumns = TRUE
            ),
            class = 'cell-border stripe display compact',
            # Added compact class for tighter spacing
            rownames = FALSE,
            width = "100%",
            # Use full width
            height = "auto"
          )
          
          # Formatting for various columns
          if ("DKOP" %in% colnames(display_data)) {
            dt <- dt %>% formatRound("DKOP", digits = 2)
          }
          
          if ("FDOP" %in% colnames(display_data)) {
            dt <- dt %>% formatRound("FDOP", digits = 2)
          }
          
          # Format W, T3, T5, etc. columns to 2 decimal places
          numeric_cols <- c("W", "T3", "T5", "T10", "T15", "T20", "T25", "T30")
          for (col in numeric_cols) {
            if (col %in% colnames(display_data)) {
              dt <- dt %>% formatRound(col, digits = 2)
            }
          }
          
          # Format salary columns as currency
          if ("DKSalary" %in% colnames(display_data)) {
            dt <- dt %>% formatCurrency("DKSalary",
                                        currency = "$",
                                        digits = 0)
          }
          
          if ("FDSalary" %in% colnames(display_data)) {
            dt <- dt %>% formatCurrency("FDSalary",
                                        currency = "$",
                                        digits = 0)
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
  
  observeEvent(input$run_sim, {
    req(rv$processed_data)
    
    cat("=== STARTING NEW SIMULATION ===\n")
    cat("Simulation parameters:", input$n_sims, "simulations\n")
    
    # Clear previous results and force garbage collection
    cat("Clearing previous results...\n")
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
    cat("Running garbage collection...\n")
    for (i in 1:3) {
      gc(verbose = FALSE, full = TRUE)
      Sys.sleep(0.1)
    }
    
    # Show progress dialog with more detailed steps
    withProgress(message = 'Running simulations...', value = 0, {
      # Update progress
      setProgress(0.05, detail = "Initializing simulation...")
      
      # Run the simulations with console output
      simulation_results <- run_efficient_simulation(rv$processed_data, n_sims = input$n_sims)
      
      # Store results - KEEP SimID! (Don't remove it here)
      setProgress(0.6, detail = "Storing simulation results...")
      rv$simulation_results <- simulation_results$results
      
      # Update platform availability
      rv$has_draftkings <- "DKFantasyPoints" %in% names(rv$simulation_results)
      rv$has_fanduel <- "FDFantasyPoints" %in% names(rv$simulation_results)
      
      cat("Platform detection: DraftKings =", rv$has_draftkings, ", FanDuel =", rv$has_fanduel, "\n")
      
      # Force garbage collection after simulation
      setProgress(0.65, detail = "Memory cleanup...")
      gc(verbose = FALSE, full = TRUE)
      
      # Process finish position analysis
      setProgress(0.7, detail = "Analyzing finishing positions...")
      rv$finishing_analysis <- analyze_finishing_positions(rv$simulation_results)
      
      # Process dominator points analysis for each platform
      if (rv$has_draftkings) {
        setProgress(0.75, detail = "Analyzing DraftKings dominator points...")
        rv$dk_dominator_analysis <- analyze_dk_dominator_points(rv$simulation_results)
      }
      
      if (rv$has_fanduel) {
        setProgress(0.8, detail = "Analyzing FanDuel dominator points...")
        rv$fd_dominator_analysis <- analyze_fd_dominator_points(rv$simulation_results)
        setProgress(0.85, detail = "Analyzing FanDuel lap points...")
        rv$fd_lap_analysis <- analyze_fd_lap_points(rv$simulation_results)
      }
      
      # Process fantasy points analysis for each platform
      if (rv$has_draftkings) {
        setProgress(0.9, detail = "Analyzing DraftKings fantasy points...")
        rv$dk_fantasy_analysis <- analyze_dk_fantasy_points(rv$simulation_results)
      }
      
      if (rv$has_fanduel) {
        setProgress(0.95, detail = "Analyzing FanDuel fantasy points...")
        rv$fd_fantasy_analysis <- analyze_fd_fantasy_points(rv$simulation_results)
      }
      
      # Mark simulation as complete
      setProgress(0.98, detail = "Finalizing...")
      rv$simulation_complete <- TRUE
      
      # Final cleanup
      setProgress(1.0, detail = "Complete!")
      gc(verbose = FALSE, full = TRUE)
      
      cat("=== SIMULATION ANALYSIS COMPLETE ===\n")
      cat("Ready for optimal lineup calculation\n")
      cat("=====================================\n")
    })
    
    # Switch to upload tab to show accuracy analysis
    updateTabItems(session, "sidebar_menu", selected = "upload")
    
    # Show success message with performance info
    n_total_results <- nrow(rv$simulation_results)
    n_drivers <- length(unique(rv$simulation_results$Name))
    
    showModal(
      modalDialog(
        title = "Simulation Complete!",
        HTML(sprintf(
          "<h4>Simulation Results:</h4>
        <ul>
        <li><strong>Simulations:</strong> %s</li>
        <li><strong>Platforms:</strong> %s</li>
        </ul>
        <p>Review the accuracy analysis and projections</p>
        <p>Then move to optimal lineup creation</p>
        <hr>
        <small><em>Note: Large datasets (50k+ sims) use sampling for analysis performance while maintaining accuracy.</em></small>",
          format(input$n_sims, big.mark = ","),
          paste(c(
            if(rv$has_draftkings) "DraftKings" else NULL,
            if(rv$has_fanduel) "FanDuel" else NULL
          ), collapse = " & ")
        )),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Continue"),
          actionButton("go_to_finish_analysis", "View Results", class = "btn-primary")
        )
      )
    )
  })
  
  observeEvent(input$go_to_finish_analysis, {
    removeModal()
    updateTabItems(session, "sidebar_menu", selected = "finish_analysis")
  })
  
  
  output$upload_content <- renderUI({
    if (rv$simulation_complete) {
      # Show accuracy analysis after simulation is complete
      tagList(fluidRow(
        box(
          width = 12,
          title = "Simulation Accuracy Analysis",
          DTOutput("accuracy_analysis") %>% withSpinner(color = "#FFD700"),
          downloadButton('downloadAccuracy', 'Download Accuracy Analysis')
        )
      ))
    } else {
      # Show input data before simulation is run
      tagList(fluidRow(
        box(
          width = 12,
          title = "Input Data",
          DTOutput("data_preview") %>% withSpinner(color = "#FFD700")
        )
      ), uiOutput("available_platforms"))
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
        order = list(list(4, 'desc')),
        # Sort by difference in descending order
        columnDefs = list(list(
          targets = c(2, 3, 4), className = 'dt-right'
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatRound(c('Input', 'Simulated', 'Difference'), digits = 2) %>%
      formatStyle(
        'Difference',
        background = styleColorBar(c(0, max(
          accuracy_data$Difference
        )), 'rgba(255, 102, 0, 0.5)'),
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
      if (rv$has_draftkings &&
          "DKSalary" %in% names(drivers_data)) {
        join_cols <- c(join_cols, "DKSalary")
      }
      
      if (rv$has_fanduel && "FDSalary" %in% names(drivers_data)) {
        join_cols <- c(join_cols, "FDSalary")
      }
      
      # Join data with available columns
      if (length(join_cols) > 1) {
        # Filter to only include columns that exist
        join_cols <- intersect(join_cols, names(drivers_data))
        
        # Join with available columns
        analysis_data <- merge(analysis_data,
                               drivers_data[, ..join_cols],
                               by = "Name",
                               all.x = TRUE)
        
        # Create dynamic column order based on available columns
        col_order <- c("Name", "Starting")
        
        # Add salary columns if available
        if ("DKSalary" %in% names(analysis_data)) {
          col_order <- c(col_order, "DKSalary")
        }
        if ("FDSalary" %in% names(analysis_data)) {
          col_order <- c(col_order, "FDSalary")
        }
        
        # Add remaining columns
        col_order <- c(
          col_order,
          "Avg_Finish",
          "Median",
          "Win_Rate",
          "T3_Rate",
          "T5_Rate",
          "T10_Rate",
          "T15_Rate",
          "T20_Rate",
          "T25_Rate",
          "T30_Rate"
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
        pageLength = -1,
        # Show all rows
        dom = "t",
        # Only show table ('t'), no search/pagination
        order = list(list(4, 'asc')),
        # Sort by Avg_Finish in ascending order
        columnDefs = list(list(
          targets = "_all", className = 'dt-center'
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
    
    # Format Salary columns
    if ("DKSalary" %in% names(analysis_data)) {
      dt <- dt %>% formatCurrency(
        'DKSalary',
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      )
    }
    
    if ("FDSalary" %in% names(analysis_data)) {
      dt <- dt %>% formatCurrency(
        'FDSalary',
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      )
    }
    
    dt
  })
  
  # Generate dynamic dominator UI based on available platforms
  output$dominator_ui <- renderUI({
    req(rv$simulation_results)
    
    # Create appropriate UI based on available platforms
    if (rv$has_draftkings && rv$has_fanduel) {
      # Both platforms available - use tabs
      tabsetPanel(
        id = "dominator_tabs",
        tabPanel(
          "DraftKings",
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Dominator Points Analysis",
              DTOutput("dk_dominator_stats") %>% withSpinner(color = "#FFD700")
            )
          ),
          
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Dominator Points Distribution",
              plotlyOutput("dk_dominator_dist", height = "1000px") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Dominator Points by Position",
              plotlyOutput("dk_points_by_position", height = "700px") %>% withSpinner(color = "#FFD700")
            )
          )
        ),
        tabPanel(
          "FanDuel",
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Dominator Points Analysis",
              DTOutput("fd_dominator_stats") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Dominator Points Distribution",
              plotlyOutput("fd_dominator_dist", height = "800px") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Dominator Points by Position",
              plotlyOutput("fd_points_by_position", height = "700px") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Lap Points by Position",
              plotlyOutput("fd_lap_points_by_position") %>% withSpinner(color = "#FFD700")
            )
          )
        )
      )
    } else if (rv$has_draftkings) {
      # Only DraftKings available
      tagList(fluidRow(
        box(
          width = 12,
          title = "DraftKings Dominator Points Analysis",
          DTOutput("dk_dominator_stats") %>% withSpinner(color = "#FFD700")
        )
      ), fluidRow(
        box(
          width = 12,
          title = "DraftKings Dominator Points Distribution",
          plotlyOutput("dk_dominator_dist", height = "1000px") %>% withSpinner(color = "#FFD700")
        )
      ), fluidRow(
        box(
          width = 12,
          title = "DraftKings Dominator Points by Position",
          plotlyOutput("dk_points_by_position", height = "700px") %>% withSpinner(color = "#FFD700")
        )
      ))
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
    if (rv$has_draftkings && rv$has_fanduel) {
      # Both platforms available - use tabs
      tabsetPanel(
        id = "fantasy_tabs",
        tabPanel(
          "DraftKings",
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Fantasy Point Projections",
              DTOutput("dk_fantasy_projections") %>% withSpinner(color = "#FFD700"),
              downloadButton(
                'download_dk_fantasy_projections',
                'Download Projections'
              )
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Fantasy Points vs Salary",
              plotlyOutput("dk_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "DraftKings Fantasy Points Distribution",
              plotlyOutput("dk_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
            )
          )
        ),
        tabPanel(
          "FanDuel",
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Fantasy Point Projections",
              DTOutput("fd_fantasy_projections") %>% withSpinner(color = "#FFD700"),
              downloadButton(
                'download_fd_fantasy_projections',
                'Download Projections'
              )
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Fantasy Points vs Salary",
              plotlyOutput("fd_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#FFD700")
            )
          ),
          fluidRow(
            box(
              width = 12,
              title = "FanDuel Fantasy Points Distribution",
              plotlyOutput("fd_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
            )
          )
        )
      )
    } else if (rv$has_draftkings) {
      # Only DraftKings available
      tagList(fluidRow(
        box(
          width = 12,
          title = "DraftKings Fantasy Point Projections",
          DTOutput("dk_fantasy_projections") %>% withSpinner(color = "#FFD700"),
          downloadButton(
            'download_dk_fantasy_projections',
            'Download Projections'
          )
        )
      ), fluidRow(
        box(
          width = 12,
          title = "DraftKings Fantasy Points vs Salary",
          plotlyOutput("dk_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#FFD700")
        )
      ), fluidRow(
        box(
          width = 12,
          title = "DraftKings Fantasy Points Distribution",
          plotlyOutput("dk_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
        )
      ))
    } else if (rv$has_fanduel) {
      # Only FanDuel available
      tagList(fluidRow(
        box(
          width = 12,
          title = "FanDuel Fantasy Point Projections",
          DTOutput("fd_fantasy_projections") %>% withSpinner(color = "#FFD700"),
          downloadButton(
            'download_fd_fantasy_projections',
            'Download Projections'
          )
        )
      ), fluidRow(
        box(
          width = 12,
          title = "FanDuel Fantasy Points vs Salary",
          plotlyOutput("fd_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#FFD700")
        )
      ), fluidRow(
        box(
          width = 12,
          title = "FanDuel Fantasy Points Distribution",
          plotlyOutput("fd_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#FFD700")
        )
      ))
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
    if (is.null(rv$dk_optimal_lineups) && is.null(rv$fd_optimal_lineups)) {
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
          box(
            width = 12,
            title = "DraftKings Lineup Filters",
            fluidRow(
              column(3, numericInput("dk_min_top1_count", "Min Top 1 Count:", value = 0, min = 0)),
              column(3, numericInput("dk_min_top2_count", "Min Top 2 Count:", value = 0, min = 0)),
              column(3, numericInput("dk_min_top3_count", "Min Top 3 Count:", value = 0, min = 0)),
              column(3, numericInput("dk_min_top5_count", "Min Top 5 Count:", value = 0, min = 0))
            ),
            fluidRow(
              column(6,
                     sliderInput("dk_ownership_range", "Cumulative Ownership Range:",
                                 min = 0, max = 600, value = c(0, 600), step = 5)
              ),
              column(6,
                     sliderInput("dk_geometric_range", "Geometric Mean Ownership Range:",
                                 min = 0, max = 100, value = c(0, 100), step = 0.5)
              )
            ),
            # NEW: Starting Position Filters
            fluidRow(
              column(6,
                     sliderInput("dk_starting_range", "Cumulative Starting Position Range:",
                                 min = 6, max = 240, value = c(6, 240), step = 1)
              ),
              column(6,
                     sliderInput("dk_starting_geo_range", "Geometric Mean Starting Position Range:",
                                 min = 1, max = 40, value = c(1, 40), step = 0.1)
              )
            ),
            fluidRow(
              column(6,
                     selectizeInput("dk_excluded_drivers", "Exclude Drivers:",
                                    choices = NULL, multiple = TRUE,
                                    options = list(plugins = list('remove_button'),
                                                   placeholder = 'Click to select drivers to exclude'))
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
                         textOutput("dk_filtered_pool_size"))
              ),
              column(6,
                     div(style = "margin-top: 20px;",
                         actionButton("generate_dk_lineups", "Randomize DraftKings Lineups", 
                                      class = "btn-primary btn-lg", style = "width: 100%;"),
                         br(), br(),
                         downloadButton("download_dk_random_lineups", "Download Selected Lineups", 
                                        style = "width: 100%;"))
              )
            )
          )
        ),
        fluidRow(
          box(width = 12, title = "DraftKings Driver Exposure Analysis",
              DTOutput("dk_driver_exposure_table") %>% withSpinner(color = "#FFD700"))
        ),
        fluidRow(
          box(width = 12, title = "Generated DraftKings Lineups",
              DTOutput("dk_random_lineups_table") %>% withSpinner(color = "#FFD700"))
        )
      ),
      # FanDuel lineup builder UI
      conditionalPanel(
        condition = "output.has_fd_lineups == 'true'",
        fluidRow(
          box(width = 12, title = "FanDuel Lineup Filters",
              fluidRow(
                column(3, numericInput("fd_min_top1_count", "Min Top 1 Count:", value = 0, min = 0)),
                column(3, numericInput("fd_min_top2_count", "Min Top 2 Count:", value = 0, min = 0)),
                column(3, numericInput("fd_min_top3_count", "Min Top 3 Count:", value = 0, min = 0)),
                column(3, numericInput("fd_min_top5_count", "Min Top 5 Count:", value = 0, min = 0))
              ),
              fluidRow(
                column(6,
                       sliderInput("fd_ownership_range", "Cumulative Ownership Range:",
                                   min = 0, max = 500, value = c(0, 500), step = 5)
                ),
                column(6,
                       sliderInput("fd_geometric_range", "Geometric Mean Ownership Range:",
                                   min = 0, max = 100, value = c(0, 100), step = 0.5)
                )
              ),
              # NEW: Starting Position Filters for FanDuel
              fluidRow(
                column(6,
                       sliderInput("fd_starting_range", "Cumulative Starting Position Range:",
                                   min = 5, max = 200, value = c(5, 200), step = 1)
                ),
                column(6,
                       sliderInput("fd_starting_geo_range", "Geometric Mean Starting Position Range:",
                                   min = 1, max = 40, value = c(1, 40), step = 0.1)
                )
              ),
              fluidRow(
                column(6,
                       selectizeInput("fd_excluded_drivers", "Exclude Drivers:",
                                      choices = NULL, multiple = TRUE,
                                      options = list(plugins = list('remove_button'),
                                                     placeholder = 'Click to select drivers to exclude'))
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
                           textOutput("fd_filtered_pool_size"))
                ),
                column(6,
                       div(style = "margin-top: 20px;",
                           actionButton("generate_fd_lineups", "Randomize FanDuel Lineups", 
                                        class = "btn-primary btn-lg", style = "width: 100%;"),
                           br(), br(),
                           downloadButton("download_fd_random_lineups", "Download Selected Lineups", 
                                          style = "width: 100%;"))
                )
              )
          )
        ),
        fluidRow(
          box(width = 12, title = "FanDuel Driver Exposure Analysis",
              DTOutput("fd_driver_exposure_table") %>% withSpinner(color = "#FFD700"))
        ),
        fluidRow(
          box(width = 12, title = "Generated FanDuel Lineups",
              DTOutput("fd_random_lineups_table") %>% withSpinner(color = "#FFD700"))
        )
      )
    )
  })
  
  # Create position boxplot
  output$position_box <- renderPlotly({
    req(rv$simulation_results)
    
    n_total_results <- nrow(rv$simulation_results)
    n_drivers <- length(unique(rv$simulation_results$Name))
    n_sims <- n_total_results / n_drivers
    
    cat("Creating position boxplot:", n_drivers, "drivers,", n_sims, "simulations\n")
    
    # Sample data if too large for responsive plotting
    plot_data <- rv$simulation_results
    max_plot_points <- 50000  # Maximum points for responsive plotting
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling", sample_size, "results for plotting (", 
          round(sample_size/n_total_results*100, 1), "%)\n")
    }
    
    # Get all unique drivers and starting positions
    drivers_info <- unique(plot_data[, c("Name", "Starting")])
    
    # Order by starting position
    drivers_info <- drivers_info[order(drivers_info$Starting), ]
    
    # Get ordered list of driver names
    ordered_drivers <- drivers_info$Name
    
    # Plot with all drivers, ordered by starting position
    p <- ggplot(plot_data,
                aes(
                  x = factor(Name, levels = ordered_drivers),
                  y = FinishPosition,
                  fill = Name
                )) +
      geom_boxplot(
        alpha = 0.7,
        outlier.color = "red",
        outlier.size = 1,
        outlier.alpha = 0.5
      ) +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        plot.margin = margin(
          t = 10,
          r = 20,
          b = 10,
          l = 20
        ),
        legend.position = "none"  # Hide legend to reduce clutter
      ) +
      labs(x = "Driver", y = "Finish Position", 
           title = if(n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           })
    
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
        font = list(family = "Arial", size = 12)
      )
  })
  
  # DraftKings Dominator Stats
  output$dk_dominator_stats <- renderDT({
    req(rv$dk_dominator_analysis)
    
    dt <- datatable(
      rv$dk_dominator_analysis,
      options = list(
        scrollX = TRUE,
        pageLength = -1,
        # Show all rows
        dom = "t",
        # Only show table ('t'), no search/pagination
        order = list(list(3, 'desc')),
        # Sort by Avg_Dom
        columnDefs = list(list(
          targets = "_all", className = 'dt-center'
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency(
        'DKSalary',
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      ) %>%
      formatRound(
        c(
          'Avg_Dom',
          'Median_Dom',
          'Max_Dom',
          'Avg_DomRank',
          'Median_DomRank',
          'Top_DomRate',
          'Top3_DomRate',
          'Top5_DomRate',
          'Top10_DomRate'
        ),
        digits = 1
      )
    
    dt
  })
  
  # FanDuel Dominator Stats
  output$fd_dominator_stats <- renderDT({
    req(rv$fd_dominator_analysis)
    
    dt <- datatable(
      rv$fd_dominator_analysis,
      options = list(
        scrollX = TRUE,
        pageLength = -1,
        # Show all rows
        dom = "t",
        # Only show table ('t'), no search/pagination
        order = list(list(3, 'desc')),
        # Sort by Avg_Dom
        columnDefs = list(list(
          targets = "_all", className = 'dt-center'
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency(
        'FDSalary',
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      ) %>%
      formatRound(
        c(
          'Avg_Dom',
          'Median_Dom',
          'Max_Dom',
          'Avg_DomRank',
          'Median_DomRank',
          'Top_DomRate',
          'Top3_DomRate',
          'Top5_DomRate'
        ),
        digits = 1
      )
    
    dt
  })
  
  output$dk_dominator_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Check if we have DK dominator points
    if (!"DKDominatorPoints" %in% names(rv$simulation_results)) {
      return(plotly_empty() %>% layout(title = "No DK dominator data available"))
    }
    
    n_total_results <- nrow(rv$simulation_results)
    cat("Creating DK dominator distribution plot:", n_total_results, "total results\n")
    
    # Sample data for plotting if too large
    max_plot_points <- 50000
    plot_data <- rv$simulation_results
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling", sample_size, "results for DK dominator plot\n")
    }
    
    # Calculate median DKDominatorPoints for each driver
    driver_medians <- plot_data %>%
      group_by(Name) %>%
      summarize(median_points = median(DKDominatorPoints, na.rm = TRUE), .groups = 'drop') %>%
      filter(median_points > 0)
    
    if (nrow(driver_medians) == 0) {
      return(plotly_empty() %>% layout(title = "No drivers with dominator points"))
    }
    
    # Filter original data to only include drivers with median > 0
    plot_data_filtered <- plot_data %>%
      filter(Name %in% driver_medians$Name)
    
    # Create box and whisker plot
    p <- ggplot(plot_data_filtered,
                aes(
                  x = reorder(Name, DKDominatorPoints, median),
                  y = DKDominatorPoints
                )) +
      geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.7) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Driver", y = "Dominator Points",
           title = if(n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           }) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    ggplotly(p, height = 1000)
  })
  
  # Optimized FD dominator distribution plot
  output$fd_dominator_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Check if we have FD dominator points
    if (!"FDDominatorPoints" %in% names(rv$simulation_results)) {
      return(plotly_empty() %>% layout(title = "No FD dominator data available"))
    }
    
    n_total_results <- nrow(rv$simulation_results)
    cat("Creating FD dominator distribution plot:", n_total_results, "total results\n")
    
    # Sample data for plotting if too large
    max_plot_points <- 50000
    plot_data <- rv$simulation_results
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling", sample_size, "results for FD dominator plot\n")
    }
    
    # Calculate median FDDominatorPoints for each driver
    driver_medians <- plot_data %>%
      group_by(Name) %>%
      summarize(median_points = median(FDDominatorPoints, na.rm = TRUE), .groups = 'drop') %>%
      filter(median_points > 0)
    
    if (nrow(driver_medians) == 0) {
      return(plotly_empty() %>% layout(title = "No drivers with dominator points"))
    }
    
    # Filter original data to only include drivers with median > 0
    plot_data_filtered <- plot_data %>%
      filter(Name %in% driver_medians$Name)
    
    # Create box and whisker plot
    p <- ggplot(plot_data_filtered,
                aes(
                  x = reorder(Name, FDDominatorPoints, median),
                  y = FDDominatorPoints
                )) +
      geom_boxplot(outlier.shape = NA, fill = "lightgreen", alpha = 0.7) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Driver", y = "Dominator Points",
           title = if(n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           }) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    ggplotly(p, height = 800)
  })
  
  output$dk_points_by_position <- renderPlotly({
    req(rv$simulation_results)
    
    n_total_results <- nrow(rv$simulation_results)
    cat("Creating DK points by position plot:", n_total_results, "total results\n")
    
    # Sample data for plotting if too large
    max_plot_points <- 25000  # Smaller sample for position plots
    plot_data <- rv$simulation_results
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling", sample_size, "results for DK points by position plot\n")
    }
    
    p <- ggplot(plot_data,
                aes(x = factor(FinishPosition), y = DKDominatorPoints)) +
      geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7, outlier.alpha = 0.3) +
      geom_smooth(
        method = "lm",
        color = "red",
        se = FALSE,
        aes(group = 1)
      ) +
      theme_minimal() +
      labs(x = "Finish Position", y = "Dominator Points",
           title = if(n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           })
    
    ggplotly(p, height = 700)
  })
  
  output$fd_points_by_position <- renderPlotly({
    req(rv$simulation_results)
    
    n_total_results <- nrow(rv$simulation_results)
    cat("Creating FD points by position plot:", n_total_results, "total results\n")
    
    # Sample data for plotting if too large
    max_plot_points <- 25000  # Smaller sample for position plots
    plot_data <- rv$simulation_results
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling", sample_size, "results for FD points by position plot\n")
    }
    
    p <- ggplot(plot_data,
                aes(x = factor(FinishPosition), y = FDDominatorPoints)) +
      geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7, outlier.alpha = 0.3) +
      geom_smooth(
        method = "lm",
        color = "red",
        se = FALSE,
        aes(group = 1)
      ) +
      theme_minimal() +
      labs(x = "Finish Position", y = "Dominator Points",
           title = if(n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           })
    
    ggplotly(p, height = 700)
  })
  
  output$fd_lap_points_by_position <- renderPlotly({
    req(rv$simulation_results)
    
    n_total_results <- nrow(rv$simulation_results)
    cat("Creating FD lap points by position plot:", n_total_results, "total results\n")
    
    # Sample data for plotting if too large
    max_plot_points <- 25000
    plot_data <- rv$simulation_results
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling", sample_size, "results for FD lap points plot\n")
    }
    
    p <- ggplot(plot_data, aes(x = factor(FinishPosition), y = FDLapPoints)) +
      geom_boxplot(fill = "lightgreen", color = "darkgreen", alpha = 0.7, outlier.alpha = 0.3) +
      theme_minimal() +
      labs(x = "Finish Position", y = "Lap Points",
           title = if(n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           })
    
    ggplotly(p)
  })
  
  
  # DraftKings Fantasy Projections
  output$dk_fantasy_projections <- renderDT({
    req(rv$dk_fantasy_analysis)
    
    dt <- datatable(
      rv$dk_fantasy_analysis,
      options = list(
        scrollX = TRUE,
        pageLength = -1,
        # Show all rows
        dom = "t",
        # Only show table ('t'), no search/pagination
        order = list(list(4, 'desc')),
        # Sort by median
        columnDefs = list(list(
          targets = "_all", className = 'dt-center'
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency(
        'DKSalary',
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      ) %>%
      formatRound(c('Median_Fantasy_Pts', 'FP_90thPct', 'PPD'), digits = 1)
    
    dt
  })
  
  # FanDuel Fantasy Projections
  output$fd_fantasy_projections <- renderDT({
    req(rv$fd_fantasy_analysis)
    
    dt <- datatable(
      rv$fd_fantasy_analysis,
      options = list(
        scrollX = TRUE,
        pageLength = -1,
        # Show all rows
        dom = "t",
        # Only show table ('t'), no search/pagination
        order = list(list(4, 'desc')),
        # Sort by median
        columnDefs = list(list(
          targets = "_all", className = 'dt-center'
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency(
        'FDSalary',
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      ) %>%
      formatRound(c('Median_Fantasy_Pts', 'FP_90thPct', 'PPD'), digits = 1)
    
    dt
  })
  
  # DraftKings Fantasy Points vs Salary
  output$dk_fantasy_points_salary <- renderPlotly({
    req(rv$dk_fantasy_analysis)
    
    plot_data <- rv$dk_fantasy_analysis
    
    # Combine Name and Starting into a label
    plot_data$label <- paste0(plot_data$Name, " (Start: ", plot_data$Starting, ")")
    
    p <- ggplot(plot_data,
                aes(
                  x = DKSalary,
                  y = Median_Fantasy_Pts,
                  size = DKOP,
                  text = label
                )) +
      geom_point(alpha = 0.7, color = "#2c7fb8") +
      geom_smooth(
        aes(x = DKSalary, y = Median_Fantasy_Pts),
        method = "lm",
        se = FALSE,
        color = "darkblue"
      ) +
      theme_minimal() +
      labs(x = "DK Salary", y = "Median Fantasy Points", size = "DKOP")
    
    ggplotly(p,
             height = 800,
             tooltip = c("text", "x", "y", "size")) %>%
      layout(hoverlabel = list(bgcolor = "white"),
             hovermode = "closest")
  })
  
  # FanDuel Fantasy Points vs Salary
  output$fd_fantasy_points_salary <- renderPlotly({
    req(rv$fd_fantasy_analysis)
    
    plot_data <- rv$fd_fantasy_analysis
    
    # Combine Name and Starting into a label
    plot_data$label <- paste0(plot_data$Name, " (Start: ", plot_data$Starting, ")")
    
    p <- ggplot(plot_data,
                aes(
                  x = FDSalary,
                  y = Median_Fantasy_Pts,
                  size = FDOP,
                  text = label
                )) +
      geom_point(alpha = 0.7, color = "#2c7fb8") +
      geom_smooth(
        aes(x = FDSalary, y = Median_Fantasy_Pts),
        method = "lm",
        se = FALSE,
        color = "darkblue"
      ) +
      theme_minimal() +
      labs(x = "FD Salary", y = "Median Fantasy Points", size = "FDOP")
    
    ggplotly(p,
             height = 800,
             tooltip = c("text", "x", "y", "size")) %>%
      layout(hoverlabel = list(bgcolor = "white"),
             hovermode = "closest")
  })
  
  output$dk_fantasy_points_dist <- renderPlotly({
    req(rv$simulation_results)
    
    n_total_results <- nrow(rv$simulation_results)
    cat("Creating DK fantasy points distribution:", n_total_results, "total results\n")
    
    # Sample data for plotting if too large
    max_plot_points <- 50000
    plot_data <- rv$simulation_results
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling", sample_size, "results for DK fantasy points plot\n")
    }
    
    # Get unique driver salary info
    driver_salaries <- plot_data %>%
      distinct(Name, DKSalary)
    
    # Order driver names by ascending DKSalary
    ordered_names <- driver_salaries %>%
      arrange(DKSalary) %>%
      pull(Name)
    
    plot_data_filtered <- plot_data %>%
      filter(Name %in% ordered_names)
    
    p <- ggplot(plot_data_filtered, aes(
      x = factor(Name, levels = ordered_names),
      y = DKFantasyPoints,
      fill = Name
    )) +
      geom_boxplot(outlier.alpha = 0.25, alpha = 0.7) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Driver", y = "Fantasy Points",
           title = if(n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           }) +
      theme(legend.position = "none")  # Hide legend to avoid clutter
    
    ggplotly(p, height = 700, tooltip = c("x", "y"))
  })
  
  # Optimized FD fantasy points distribution
  output$fd_fantasy_points_dist <- renderPlotly({
    req(rv$simulation_results)
    
    n_total_results <- nrow(rv$simulation_results)
    cat("Creating FD fantasy points distribution:", n_total_results, "total results\n")
    
    # Sample data for plotting if too large
    max_plot_points <- 50000
    plot_data <- rv$simulation_results
    
    if (n_total_results > max_plot_points) {
      sample_size <- max_plot_points
      sample_indices <- sample(n_total_results, sample_size)
      plot_data <- rv$simulation_results[sample_indices]
      cat("Sampling", sample_size, "results for FD fantasy points plot\n")
    }
    
    # Get unique driver salary info
    driver_salaries <- plot_data %>%
      distinct(Name, FDSalary)
    
    # Order driver names by ascending FDSalary
    ordered_names <- driver_salaries %>%
      arrange(FDSalary) %>%
      pull(Name)
    
    plot_data_filtered <- plot_data %>%
      filter(Name %in% ordered_names)
    
    p <- ggplot(plot_data_filtered, aes(
      x = factor(Name, levels = ordered_names),
      y = FDFantasyPoints,
      fill = Name
    )) +
      geom_boxplot(outlier.alpha = 0.25, alpha = 0.7) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Driver", y = "Fantasy Points",
           title = if(n_total_results > max_plot_points) {
             paste("Sample of", sample_size, "results")
           } else {
             NULL
           }) +
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
      showModal(
        modalDialog(
          title = "Processing DraftKings Optimal Lineups",
          "Finding optimal lineups using all simulations. This may take a few minutes.",
          footer = NULL,
          easyClose = FALSE
        )
      )
      
      # Calculate optimal lineups
      setProgress(0.2, detail = "Finding optimal lineups...")
      dk_lineups_result <- tryCatch({
        count_optimal_lineups_efficient(rv$simulation_results, "DK")
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
      
      if (!is.null(dk_lineups_result)) {
        # Create display and full datasets
        display_data <- create_lineup_display_data(dk_lineups_result, max_display = 100)
        rv$dk_optimal_lineups_display <- display_data$display
        rv$dk_optimal_lineups_full <- display_data$full
        # Keep the old variable for backward compatibility with other functions
        rv$dk_optimal_lineups <- display_data$full
      } else {
        rv$dk_optimal_lineups_display <- NULL
        rv$dk_optimal_lineups_full <- NULL
        rv$dk_optimal_lineups <- NULL
      }
      
      gc(verbose = FALSE, full = TRUE)
      
      # Create a driver mapping from the simulation results
      setProgress(0.7, detail = "Creating driver mapping...")
      driver_mapping <- NULL
      if (!is.null(rv$dk_optimal_lineups)) {
        # Get all unique drivers from optimal lineups
        driver_cols <- paste0("Driver", 1:DK_ROSTER_SIZE)
        
        # FIXED: Use a more generic approach that works with both data.frame and data.table
        lineup_drivers <- c()
        for (col in driver_cols) {
          if (col %in% names(rv$dk_optimal_lineups)) {
            lineup_drivers <- c(lineup_drivers, rv$dk_optimal_lineups[[col]])
          }
        }
        lineup_drivers <- unique(lineup_drivers)
        
        driver_mapping <- data.frame(
          DKName = lineup_drivers,
          # Using DKName as primary key
          Name = NA_character_,
          DKSalary = NA_real_,
          DKOP = NA_real_,
          Starting = NA_real_,
          Proj = NA_real_
        )
        
        # Get mapping from simulation results
        unique_sim_drivers <- rv$simulation_results[!duplicated(rv$simulation_results$DKName), c("DKName", "Name", "DKSalary", "DKOP", "Starting")]
        
        # Match each driver from lineups to the simulation results
        for (i in 1:nrow(driver_mapping)) {
          dk_name <- driver_mapping$DKName[i]
          # Try to find this driver in the simulation results
          matches <- which(unique_sim_drivers$DKName == dk_name)
          if (length(matches) > 0) {
            match_idx <- matches[1]
            driver_mapping$Name[i] <- unique_sim_drivers$Name[match_idx]
            driver_mapping$DKSalary[i] <- unique_sim_drivers$DKSalary[match_idx]
            driver_mapping$DKOP[i] <- unique_sim_drivers$DKOP[match_idx]
            driver_mapping$Starting[i] <- unique_sim_drivers$Starting[match_idx]
            
            # Get projection from fantasy analysis
            if (!is.null(rv$dk_fantasy_analysis)) {
              name_match <- which(rv$dk_fantasy_analysis$Name == unique_sim_drivers$Name[match_idx])
              if (length(name_match) > 0) {
                driver_mapping$Proj[i] <- rv$dk_fantasy_analysis$Median_Fantasy_Pts[name_match[1]]
              }
            }
          }
        }
      }
      
      # Calculate initial driver exposure with the mapping
      setProgress(0.8, detail = "Calculating driver exposure...")
      if (!is.null(rv$dk_optimal_lineups)) {
        if (!is.null(driver_mapping) && nrow(driver_mapping) > 0) {
          rv$dk_driver_exposure <- calculate_dk_driver_exposure(rv$dk_optimal_lineups, driver_mapping)
        } else {
          rv$dk_driver_exposure <- calculate_dk_driver_exposure(rv$dk_optimal_lineups, rv$dk_fantasy_analysis)
        }
      }
      
      # Remove the processing modal
      removeModal()
    })
    
    # Update excluded drivers selection for the lineup builder
    if (!is.null(rv$dk_optimal_lineups) &&
        !is.null(rv$dk_driver_exposure)) {
      # Get all drivers from the driver exposure data
      driver_data <- rv$dk_driver_exposure
      
      # Create basic named vector for choices
      driver_names <- driver_data$Name
      driver_ids <- driver_data$DKName
      
      # Create simple labels with name and optimal rate
      driver_labels <- paste0(driver_names,
                              " (",
                              round(driver_data$OptimalRate, 1),
                              "%)")
      
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
      showModal(modalDialog(title = "Success", HTML(
        sprintf(
          "Successfully generated <b>%d</b> optimal lineups for DraftKings!<br><br>
      You can now go to the <b>Lineup Builder</b> tab to filter and select lineups from this pool.",
          nrow(rv$dk_optimal_lineups)
        )
      ), easyClose = TRUE))
    }
  })
  
  observeEvent(input$run_fd_optimization, {
    req(rv$simulation_results, rv$has_fanduel)
    
    # Clear previous analysis results but keep simulation data
    rv$fd_optimal_lineups <- NULL
    rv$fd_optimal_lineups_display <- NULL
    rv$fd_optimal_lineups_full <- NULL
    rv$fd_driver_exposure <- NULL
    rv$fd_random_lineups <- NULL
    
    # Also clear DraftKings lineups if they exist
    rv$dk_optimal_lineups <- NULL
    rv$dk_optimal_lineups_display <- NULL
    rv$dk_optimal_lineups_full <- NULL
    rv$dk_driver_exposure <- NULL
    rv$dk_random_lineups <- NULL
    
    # Force garbage collection before starting
    cleanup_memory()
    
    # Show progress dialog
    withProgress(message = 'Calculating FanDuel optimal lineups...', value = 0, {
      # Show modal to user
      showModal(
        modalDialog(
          title = "Processing FanDuel Optimal Lineups",
          "Finding optimal lineups using all simulations. This may take a few minutes.",
          footer = NULL,
          easyClose = FALSE
        )
      )
      
      # Calculate optimal lineups
      setProgress(0.2, detail = "Finding optimal lineups...")
      fd_lineups_result <- tryCatch({
        count_optimal_lineups_efficient(rv$simulation_results, "FD")
      }, error = function(e) {
        removeModal()
        showModal(modalDialog(
          title = "Error Finding Optimal Lineups",
          paste("There was an error calculating FanDuel lineups:", e$message),
          easyClose = TRUE
        ))
        return(NULL)
      })
      
      # Process results if successful
      if (!is.null(fd_lineups_result)) {
        setProgress(0.6, detail = "Processing lineup data...")
        
        # Create display and full datasets for performance
        display_data <- create_lineup_display_data(fd_lineups_result, max_display = 100)
        rv$fd_optimal_lineups_display <- display_data$display
        rv$fd_optimal_lineups_full <- display_data$full
        
        # Keep the old variable for backward compatibility with other functions
        rv$fd_optimal_lineups <- display_data$full
        
        # Force cleanup after processing large datasets
        cleanup_memory()
      }
      
      # Create driver mapping from simulation results
      setProgress(0.7, detail = "Creating driver mapping...")
      driver_mapping <- NULL
      
      if (!is.null(rv$fd_optimal_lineups)) {
        # Get all unique drivers from optimal lineups
        driver_cols <- paste0("Driver", 1:FD_ROSTER_SIZE)
        
        # Extract all drivers from lineups
        lineup_drivers <- c()
        for (col in driver_cols) {
          if (col %in% names(rv$fd_optimal_lineups)) {
            lineup_drivers <- c(lineup_drivers, rv$fd_optimal_lineups[[col]])
          }
        }
        lineup_drivers <- unique(lineup_drivers)
        
        # Create mapping dataframe
        driver_mapping <- data.frame(
          FDName = lineup_drivers,
          Name = NA_character_,
          FDSalary = NA_real_,
          FDOP = NA_real_,
          Starting = NA_real_,
          Proj = NA_real_,
          stringsAsFactors = FALSE
        )
        
        # Get mapping from simulation results
        unique_sim_drivers <- rv$simulation_results[!duplicated(rv$simulation_results$FDName), 
                                                    c("FDName", "Name", "FDSalary", "FDOP", "Starting")]
        
        # Match each driver from lineups to the simulation results
        for (i in 1:nrow(driver_mapping)) {
          fd_name <- driver_mapping$FDName[i]
          matches <- which(unique_sim_drivers$FDName == fd_name)
          
          if (length(matches) > 0) {
            match_idx <- matches[1]
            driver_mapping$Name[i] <- unique_sim_drivers$Name[match_idx]
            driver_mapping$FDSalary[i] <- unique_sim_drivers$FDSalary[match_idx]
            driver_mapping$FDOP[i] <- unique_sim_drivers$FDOP[match_idx]
            driver_mapping$Starting[i] <- unique_sim_drivers$Starting[match_idx]
            
            # Get projection from fantasy analysis if available
            if (!is.null(rv$fd_fantasy_analysis)) {
              name_match <- which(rv$fd_fantasy_analysis$Name == unique_sim_drivers$Name[match_idx])
              if (length(name_match) > 0) {
                driver_mapping$Proj[i] <- rv$fd_fantasy_analysis$Median_Fantasy_Pts[name_match[1]]
              }
            }
          }
        }
      }
      
      # Calculate initial driver exposure with the mapping
      setProgress(0.8, detail = "Calculating driver exposure...")
      if (!is.null(rv$fd_optimal_lineups)) {
        rv$fd_driver_exposure <- if (!is.null(driver_mapping) && nrow(driver_mapping) > 0) {
          calculate_fd_driver_exposure(rv$fd_optimal_lineups, driver_mapping)
        } else {
          calculate_fd_driver_exposure(rv$fd_optimal_lineups, rv$fd_fantasy_analysis)
        }
      }
      
      # Remove the processing modal
      removeModal()
    })
    
    # Update UI elements if optimization was successful
    if (!is.null(rv$fd_optimal_lineups) && !is.null(rv$fd_driver_exposure)) {
      # Update excluded drivers dropdown
      driver_data <- rv$fd_driver_exposure
      driver_names <- driver_data$Name
      driver_ids <- driver_data$FDName
      driver_labels <- paste0(driver_names, " (", round(driver_data$OptimalRate, 1), "%)")
      driver_choices <- setNames(driver_ids, driver_labels)
      
      updateSelectizeInput(
        session = session,
        inputId = "fd_excluded_drivers",
        choices = driver_choices,
        selected = character(0)
      )
      
      # Show success message
      showModal(modalDialog(
        title = "Success", 
        HTML(
          sprintf(
            "Successfully generated <b>%d</b> optimal lineups for FanDuel!<br><br>
          <strong>Display:</strong> Showing top 100 lineups in table for performance<br>
          <strong>Download:</strong> All lineups available via download button<br><br>
          You can now go to the <b>Lineup Builder</b> tab to filter and select lineups from this pool.",
            nrow(rv$fd_optimal_lineups)
          )
        ), 
        easyClose = TRUE
      ))
      
    } else {
      # Show error message if optimization failed
      showModal(modalDialog(
        title = "Error",
        "Failed to generate FanDuel optimal lineups. Please check your data and simulation results, then try again.",
        easyClose = TRUE
      ))
    }
    
    # Final cleanup
    cleanup_memory()
  })
  

  # DraftKings contest simulation
  observeEvent(input$run_dk_contest_simulation, {
    req(rv$dk_optimal_lineups, rv$simulation_results)
    
    # Clear previous results
    rv$dk_contest_results_full <- NULL
    rv$dk_contest_driver_analysis <- NULL
    
    withProgress(message = 'Running DraftKings contest simulation...', value = 0, {
      # Apply filters to optimal lineups
      filtered_lineups <- copy(rv$dk_optimal_lineups)
      
      # Apply cumulative ownership filters
      if (!is.null(input$dk_contest_ownership_range)) {
        if (input$dk_contest_ownership_range[1] > 0) {
          filtered_lineups <- filtered_lineups[CumulativeOwnership >= input$dk_contest_ownership_range[1]]
        }
        if (input$dk_contest_ownership_range[2] < 600) {
          filtered_lineups <- filtered_lineups[CumulativeOwnership <= input$dk_contest_ownership_range[2]]
        }
      }
      
      # Apply geometric mean filters
      if (!is.null(input$dk_contest_geometric_range)) {
        if (input$dk_contest_geometric_range[1] > 0) {
          filtered_lineups <- filtered_lineups[!is.na(GeometricMean) & GeometricMean >= input$dk_contest_geometric_range[1]]
        }
        if (input$dk_contest_geometric_range[2] < 100) {
          filtered_lineups <- filtered_lineups[!is.na(GeometricMean) & GeometricMean <= input$dk_contest_geometric_range[2]]
        }
      }
      
      # Apply starting position filters
      if (!is.null(input$dk_contest_starting_range)) {
        if (input$dk_contest_starting_range[1] > 6) {
          filtered_lineups <- filtered_lineups[CumulativeStarting >= input$dk_contest_starting_range[1]]
        }
        if (input$dk_contest_starting_range[2] < 240) {
          filtered_lineups <- filtered_lineups[CumulativeStarting <= input$dk_contest_starting_range[2]]
        }
      }
      
      # Apply geometric mean starting position filters
      if (!is.null(input$dk_contest_starting_geo_range)) {
        if (input$dk_contest_starting_geo_range[1] > 1) {
          filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting >= input$dk_contest_starting_geo_range[1]]
        }
        if (input$dk_contest_starting_geo_range[2] < 40) {
          filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting <= input$dk_contest_starting_geo_range[2]]
        }
      }
      
      # Apply driver exclusions
      if (!is.null(input$dk_contest_excluded_drivers) && length(input$dk_contest_excluded_drivers) > 0) {
        driver_cols <- paste0("Driver", 1:6)
        to_exclude <- logical(nrow(filtered_lineups))
        for (col in driver_cols) {
          to_exclude <- to_exclude | (filtered_lineups[[col]] %in% input$dk_contest_excluded_drivers)
        }
        filtered_lineups <- filtered_lineups[!to_exclude]
      }
      
      if (nrow(filtered_lineups) == 0) {
        showModal(modalDialog(
          title = "Error",
          "No lineups match your filter criteria. Adjust your filters and try again.",
          easyClose = TRUE
        ))
        return()
      }
      
      setProgress(0.3, detail = "Running contest simulation...")
      
      # Run simulation on ALL filtered lineups
      rv$dk_contest_results_full <- simulate_double_up_contest(
        filtered_lineups,
        rv$simulation_results,
        platform = "DK",
        num_contests = input$dk_contest_num_contests
      )
      
      setProgress(0.8, detail = "Calculating driver analysis...")
      
      # Calculate driver analysis from full results
      rv$dk_contest_driver_analysis <- calculate_dk_contest_driver_analysis(
        rv$dk_contest_results_full,
        rv$dk_fantasy_analysis
      )
      
      showModal(modalDialog(
        title = "Contest Simulation Complete",
        sprintf("Analyzed %d filtered lineups across %d contests.",
                nrow(filtered_lineups), input$dk_contest_num_contests),
        easyClose = TRUE
      ))
    })
  })
  
  # FanDuel contest simulation
  observeEvent(input$run_fd_contest_simulation, {
    req(rv$fd_optimal_lineups, rv$simulation_results)
    
    # Clear previous results
    rv$fd_contest_results_full <- NULL
    rv$fd_contest_driver_analysis <- NULL
    
    withProgress(message = 'Running FanDuel contest simulation...', value = 0, {
      # Apply filters to optimal lineups
      filtered_lineups <- copy(rv$fd_optimal_lineups)
      
      # Apply cumulative ownership filters
      if (!is.null(input$fd_contest_ownership_range)) {
        if (input$fd_contest_ownership_range[1] > 0) {
          filtered_lineups <- filtered_lineups[CumulativeOwnership >= input$fd_contest_ownership_range[1]]
        }
        if (input$fd_contest_ownership_range[2] < 500) {
          filtered_lineups <- filtered_lineups[CumulativeOwnership <= input$fd_contest_ownership_range[2]]
        }
      }
      
      # Apply geometric mean filters
      if (!is.null(input$fd_contest_geometric_range)) {
        if (input$fd_contest_geometric_range[1] > 0) {
          filtered_lineups <- filtered_lineups[!is.na(GeometricMean) & GeometricMean >= input$fd_contest_geometric_range[1]]
        }
        if (input$fd_contest_geometric_range[2] < 100) {
          filtered_lineups <- filtered_lineups[!is.na(GeometricMean) & GeometricMean <= input$fd_contest_geometric_range[2]]
        }
      }
      
      # Apply starting position filters
      if (!is.null(input$fd_contest_starting_range)) {
        if (input$fd_contest_starting_range[1] > 5) {
          filtered_lineups <- filtered_lineups[CumulativeStarting >= input$fd_contest_starting_range[1]]
        }
        if (input$fd_contest_starting_range[2] < 200) {
          filtered_lineups <- filtered_lineups[CumulativeStarting <= input$fd_contest_starting_range[2]]
        }
      }
      
      # Apply geometric mean starting position filters
      if (!is.null(input$fd_contest_starting_geo_range)) {
        if (input$fd_contest_starting_geo_range[1] > 1) {
          filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting >= input$fd_contest_starting_geo_range[1]]
        }
        if (input$fd_contest_starting_geo_range[2] < 40) {
          filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting <= input$fd_contest_starting_geo_range[2]]
        }
      }
      
      # Apply driver exclusions
      if (!is.null(input$fd_contest_excluded_drivers) && length(input$fd_contest_excluded_drivers) > 0) {
        driver_cols <- paste0("Driver", 1:5)
        to_exclude <- logical(nrow(filtered_lineups))
        for (col in driver_cols) {
          to_exclude <- to_exclude | (filtered_lineups[[col]] %in% input$fd_contest_excluded_drivers)
        }
        filtered_lineups <- filtered_lineups[!to_exclude]
      }
      
      if (nrow(filtered_lineups) == 0) {
        showModal(modalDialog(
          title = "Error",
          "No lineups match your filter criteria. Adjust your filters and try again.",
          easyClose = TRUE
        ))
        return()
      }
      
      setProgress(0.3, detail = "Running contest simulation...")
      
      # Run simulation on ALL filtered lineups
      rv$fd_contest_results_full <- simulate_double_up_contest(
        filtered_lineups,
        rv$simulation_results,
        platform = "FD",
        num_contests = input$fd_contest_num_contests
      )
      
      setProgress(0.8, detail = "Calculating driver analysis...")
      
      # Calculate driver analysis from full results
      rv$fd_contest_driver_analysis <- calculate_fd_contest_driver_analysis(
        rv$fd_contest_results_full,
        rv$fd_fantasy_analysis
      )
      
      showModal(modalDialog(
        title = "Contest Simulation Complete",
        sprintf("Analyzed %d filtered lineups across %d contests.",
                nrow(filtered_lineups), input$fd_contest_num_contests),
        easyClose = TRUE
      ))
    })
  })
  
  # Reactive for DK display results
  dk_contest_display_results <- reactive({
    req(rv$dk_contest_results_full)
    
    results <- rv$dk_contest_results_full
    
    # Apply min cash rate filter
    if (!is.null(input$dk_contest_min_cash_rate) && input$dk_contest_min_cash_rate > 0) {
      results <- results[results$WinRate >= input$dk_contest_min_cash_rate, ]
    }
    
    # Apply top X filter
    if (!is.null(input$dk_contest_show_top) && input$dk_contest_show_top > 0) {
      results <- head(results, input$dk_contest_show_top)
    }
    
    return(results)
  })
  
  # Reactive for FD display results
  fd_contest_display_results <- reactive({
    req(rv$fd_contest_results_full)
    
    results <- rv$fd_contest_results_full
    
    # Apply min cash rate filter
    if (!is.null(input$fd_contest_min_cash_rate) && input$fd_contest_min_cash_rate > 0) {
      results <- results[results$WinRate >= input$fd_contest_min_cash_rate, ]
    }
    
    # Apply top X filter
    if (!is.null(input$fd_contest_show_top) && input$fd_contest_show_top > 0) {
      results <- head(results, input$fd_contest_show_top)
    }
    
    return(results)
  })
  
  # Filtered pool size displays
  output$dk_contest_filtered_count <- renderText({
    req(rv$dk_optimal_lineups)
    
    filtered_lineups <- copy(rv$dk_optimal_lineups)
    
    # Apply same filters as in simulation
    if (!is.null(input$dk_contest_ownership_range)) {
      if (input$dk_contest_ownership_range[1] > 0) {
        filtered_lineups <- filtered_lineups[CumulativeOwnership >= input$dk_contest_ownership_range[1]]
      }
      if (input$dk_contest_ownership_range[2] < 600) {
        filtered_lineups <- filtered_lineups[CumulativeOwnership <= input$dk_contest_ownership_range[2]]
      }
    }
    
    if (!is.null(input$dk_contest_geometric_range)) {
      if (input$dk_contest_geometric_range[1] > 0) {
        filtered_lineups <- filtered_lineups[!is.na(GeometricMean) & GeometricMean >= input$dk_contest_geometric_range[1]]
      }
      if (input$dk_contest_geometric_range[2] < 100) {
        filtered_lineups <- filtered_lineups[!is.na(GeometricMean) & GeometricMean <= input$dk_contest_geometric_range[2]]
      }
    }
    
    if (!is.null(input$dk_contest_starting_range)) {
      if (input$dk_contest_starting_range[1] > 6) {
        filtered_lineups <- filtered_lineups[CumulativeStarting >= input$dk_contest_starting_range[1]]
      }
      if (input$dk_contest_starting_range[2] < 240) {
        filtered_lineups <- filtered_lineups[CumulativeStarting <= input$dk_contest_starting_range[2]]
      }
    }
    
    if (!is.null(input$dk_contest_starting_geo_range)) {
      if (input$dk_contest_starting_geo_range[1] > 1) {
        filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting >= input$dk_contest_starting_geo_range[1]]
      }
      if (input$dk_contest_starting_geo_range[2] < 40) {
        filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting <= input$dk_contest_starting_geo_range[2]]
      }
    }
    
    if (!is.null(input$dk_contest_excluded_drivers) && length(input$dk_contest_excluded_drivers) > 0) {
      driver_cols <- paste0("Driver", 1:6)
      to_exclude <- logical(nrow(filtered_lineups))
      for (col in driver_cols) {
        to_exclude <- to_exclude | (filtered_lineups[[col]] %in% input$dk_contest_excluded_drivers)
      }
      filtered_lineups <- filtered_lineups[!to_exclude]
    }
    
    paste("Number of lineups:", nrow(filtered_lineups))
  })
  
  output$fd_contest_filtered_count <- renderText({
    req(rv$fd_optimal_lineups)
    
    filtered_lineups <- copy(rv$fd_optimal_lineups)
    
    # Apply same filters as in simulation (similar logic but for FD)
    if (!is.null(input$fd_contest_ownership_range)) {
      if (input$fd_contest_ownership_range[1] > 0) {
        filtered_lineups <- filtered_lineups[CumulativeOwnership >= input$fd_contest_ownership_range[1]]
      }
      if (input$fd_contest_ownership_range[2] < 500) {
        filtered_lineups <- filtered_lineups[CumulativeOwnership <= input$fd_contest_ownership_range[2]]
      }
    }
    
    if (!is.null(input$fd_contest_geometric_range)) {
      if (input$fd_contest_geometric_range[1] > 0) {
        filtered_lineups <- filtered_lineups[!is.na(GeometricMean) & GeometricMean >= input$fd_contest_geometric_range[1]]
      }
      if (input$fd_contest_geometric_range[2] < 100) {
        filtered_lineups <- filtered_lineups[!is.na(GeometricMean) & GeometricMean <= input$fd_contest_geometric_range[2]]
      }
    }
    
    if (!is.null(input$fd_contest_starting_range)) {
      if (input$fd_contest_starting_range[1] > 5) {
        filtered_lineups <- filtered_lineups[CumulativeStarting >= input$fd_contest_starting_range[1]]
      }
      if (input$fd_contest_starting_range[2] < 200) {
        filtered_lineups <- filtered_lineups[CumulativeStarting <= input$fd_contest_starting_range[2]]
      }
    }
    
    if (!is.null(input$fd_contest_starting_geo_range)) {
      if (input$fd_contest_starting_geo_range[1] > 1) {
        filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting >= input$fd_contest_starting_geo_range[1]]
      }
      if (input$fd_contest_starting_geo_range[2] < 40) {
        filtered_lineups <- filtered_lineups[!is.na(GeometricMeanStarting) & GeometricMeanStarting <= input$fd_contest_starting_geo_range[2]]
      }
    }
    
    if (!is.null(input$fd_contest_excluded_drivers) && length(input$fd_contest_excluded_drivers) > 0) {
      driver_cols <- paste0("Driver", 1:5)
      to_exclude <- logical(nrow(filtered_lineups))
      for (col in driver_cols) {
        to_exclude <- to_exclude | (filtered_lineups[[col]] %in% input$fd_contest_excluded_drivers)
      }
      filtered_lineups <- filtered_lineups[!to_exclude]
    }
    
    paste("Number of lineups:", nrow(filtered_lineups))
  })
  
  # Results tables
  output$dk_contest_driver_table <- renderDT({
    req(rv$dk_contest_driver_analysis)
    
    display_data <- rv$dk_contest_driver_analysis
    
    # Hide DKName column
    display_data$DKName <- NULL
    
    # Column order
    col_order <- c("Name", "Starting", "Proj", "DKSalary", "DKOP", 
                   "AvgLineupWinRate", "TopLineupWinRate", "ContestWinRate")
    col_order <- intersect(col_order, names(display_data))
    display_data <- display_data[, col_order]
    
    dt <- datatable(
      display_data,
      options = list(
        pageLength = -1, dom = "t", scrollX = TRUE,
        order = list(list(5, 'desc')),
        rownames = FALSE
      ),
      rownames = FALSE
    )
    
    if ("DKSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('DKSalary', currency = "$", digits = 0)
    }
    
    numeric_cols <- intersect(c('AvgLineupWinRate', 'TopLineupWinRate', 'ContestWinRate', 'Proj', 'DKOP'), 
                              names(display_data))
    if (length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(numeric_cols, digits = 1)
    }
    
    return(dt)
  })
  
  output$fd_contest_driver_table <- renderDT({
    req(rv$fd_contest_driver_analysis)
    
    display_data <- rv$fd_contest_driver_analysis
    
    # Hide FDName column
    display_data$FDName <- NULL
    
    # Column order
    col_order <- c("Name", "Starting", "Proj", "FDSalary", "FDOP", 
                   "AvgLineupWinRate", "TopLineupWinRate", "ContestWinRate")
    col_order <- intersect(col_order, names(display_data))
    display_data <- display_data[, col_order]
    
    dt <- datatable(
      display_data,
      options = list(
        pageLength = -1, dom = "t", scrollX = TRUE,
        order = list(list(5, 'desc')),
        rownames = FALSE
      ),
      rownames = FALSE
    )
    
    if ("FDSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('FDSalary', currency = "$", digits = 0)
    }
    
    numeric_cols <- intersect(c('AvgLineupWinRate', 'TopLineupWinRate', 'ContestWinRate', 'Proj', 'FDOP'), 
                              names(display_data))
    if (length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(numeric_cols, digits = 1)
    }
    
    return(dt)
  })
  
  output$dk_contest_lineups_table <- renderDT({
    display_data <- dk_contest_display_results()
    req(display_data)
    
    if (nrow(display_data) == 0) {
      return(datatable(data.frame(Message = "No lineups meet the current criteria")))
    }
    
    # FIXED: Convert to data.frame to avoid data.table column selection issues
    display_data <- as.data.frame(display_data)
    
    # Format driver names for display
    if (!is.null(rv$dk_fantasy_analysis)) {
      for (i in 1:6) {
        col <- paste0("Driver", i)
        if (col %in% names(display_data)) {
          display_data[[col]] <- sapply(display_data[[col]], function(id) {
            match_idx <- which(rv$dk_fantasy_analysis$DKName == id)
            if (length(match_idx) > 0) {
              rv$dk_fantasy_analysis$Name[match_idx[1]]
            } else {
              id
            }
          })
        }
      }
    }
    
    # FIXED: Use standard R subsetting instead of data.table ..cols_to_keep syntax
    cols_to_keep <- c(paste0("Driver", 1:6), "WinRate", "CumulativeOwnership", "GeometricMean", 
                      "Top1Count", "Top3Count", "Top5Count")
    cols_to_keep <- intersect(cols_to_keep, names(display_data))
    display_data <- display_data[, cols_to_keep, drop = FALSE]
    
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25, scrollX = TRUE, dom = "tp", ordering = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      rownames = FALSE
    ) %>%
      formatRound(c('WinRate', 'CumulativeOwnership', 'GeometricMean'), digits = 1) %>%
      formatStyle('WinRate', 
                  background = styleColorBar(c(0, max(display_data$WinRate)), 'lightgreen'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
    
    return(dt)
  })
  
  # Same fix for FanDuel
  output$fd_contest_lineups_table <- renderDT({
    display_data <- fd_contest_display_results()
    req(display_data)
    
    if (nrow(display_data) == 0) {
      return(datatable(data.frame(Message = "No lineups meet the current criteria")))
    }
    
    # FIXED: Convert to data.frame to avoid data.table column selection issues
    display_data <- as.data.frame(display_data)
    
    # Format driver names for display
    if (!is.null(rv$fd_fantasy_analysis)) {
      for (i in 1:5) {
        col <- paste0("Driver", i)
        if (col %in% names(display_data)) {
          display_data[[col]] <- sapply(display_data[[col]], function(id) {
            match_idx <- which(rv$fd_fantasy_analysis$FDName == id)
            if (length(match_idx) > 0) {
              rv$fd_fantasy_analysis$Name[match_idx[1]]
            } else {
              id
            }
          })
        }
      }
    }
    
    # FIXED: Use standard R subsetting instead of data.table ..cols_to_keep syntax
    cols_to_keep <- c(paste0("Driver", 1:5), "WinRate", "CumulativeOwnership", "GeometricMean", 
                      "Top1Count", "Top3Count", "Top5Count")
    cols_to_keep <- intersect(cols_to_keep, names(display_data))
    display_data <- display_data[, cols_to_keep, drop = FALSE]
    
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25, scrollX = TRUE, dom = "tp", ordering = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      rownames = FALSE
    ) %>%
      formatRound(c('WinRate', 'CumulativeOwnership', 'GeometricMean'), digits = 1) %>%
      formatStyle('WinRate', 
                  background = styleColorBar(c(0, max(display_data$WinRate)), 'lightgreen'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
    
    return(dt)
  })
  
  
  # Download handlers
  output$download_dk_contest_results <- downloadHandler(
    filename = function() {
      paste("dk_contest_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      if (is.null(rv$dk_contest_results_full)) {
        write.csv(data.frame(Message = "No results available"), file, row.names = FALSE)
      } else {
        write.csv(rv$dk_contest_results_full, file, row.names = FALSE)
      }
    },
    contentType = "text/csv"
  )
  
  output$download_fd_contest_results <- downloadHandler(
    filename = function() {
      paste("fd_contest_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      if (is.null(rv$fd_contest_results_full)) {
        write.csv(data.frame(Message = "No results available"), file, row.names = FALSE)
      } else {
        write.csv(rv$fd_contest_results_full, file, row.names = FALSE)
      }
    },
    contentType = "text/csv"
  )
  

  # Update excluded drivers when navigating to contest tab
  observe({
    if(input$sidebar_menu == "contest_sim") {
      
      # Update DraftKings excluded drivers
      if(!is.null(rv$dk_optimal_lineups) && !is.null(rv$dk_driver_exposure)) {
        driver_data <- rv$dk_driver_exposure
        driver_names <- driver_data$Name
        driver_ids <- driver_data$DKName
        driver_labels <- paste0(driver_names, " (", round(driver_data$OptimalRate, 1), "%)")
        driver_choices <- setNames(driver_ids, driver_labels)
        
        updateSelectizeInput(
          session = session,
          inputId = "dk_contest_excluded_drivers",
          choices = driver_choices,
          selected = input$dk_contest_excluded_drivers
        )
      }
      
      # Update FanDuel excluded drivers
      if(!is.null(rv$fd_optimal_lineups) && !is.null(rv$fd_driver_exposure)) {
        driver_data <- rv$fd_driver_exposure
        driver_names <- driver_data$Name
        driver_ids <- driver_data$FDName
        driver_labels <- paste0(driver_names, " (", round(driver_data$OptimalRate, 1), "%)")
        driver_choices <- setNames(driver_ids, driver_labels)
        
        updateSelectizeInput(
          session = session,
          inputId = "fd_contest_excluded_drivers",
          choices = driver_choices,
          selected = input$fd_contest_excluded_drivers
        )
      }
    }
  })
  

  
  output$dk_optimal_lineups_table <- renderDT({
    req(rv$dk_optimal_lineups_display)
    
    # Clone lineups for display
    display_data <- as.data.table(rv$dk_optimal_lineups_display)
    
    # Format driver columns to show names
    if (!is.null(rv$dk_fantasy_analysis)) {
      for (i in 1:DK_ROSTER_SIZE) {
        col <- paste0("Driver", i)
        display_data[[col]] <- sapply(display_data[[col]], function(id) {
          match_idx <- which(rv$dk_fantasy_analysis$DKID == id)
          if (length(match_idx) > 0) {
            rv$dk_fantasy_analysis$Name[match_idx[1]]
          } else {
            id
          }
        })
      }
    }
    
    # Remove Rank columns, keep TopX Count columns and NEW starting position columns
    cols_to_keep <- c(
      paste0("Driver", 1:DK_ROSTER_SIZE),
      grep("^Top[0-9]+Count$", names(display_data), value = TRUE),
      "TotalSalary", "CumulativeOwnership", "GeometricMean",
      "CumulativeStarting", "GeometricMeanStarting"  # NEW
    )
    cols_to_keep <- intersect(cols_to_keep, names(display_data))
    
    # Use the correct data.table syntax
    display_data <- display_data[, ..cols_to_keep]
    
    # Sort the data by Top1Count, then Top5Count (both descending)
    if ("Top1Count" %in% names(display_data) && "Top5Count" %in% names(display_data)) {
      setorder(display_data, -Top1Count, -Top5Count)
    } else if ("Top1Count" %in% names(display_data)) {
      setorder(display_data, -Top1Count)
    }
    
    # Create the datatable
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25, scrollX = TRUE, rownames = FALSE,
        dom = "ftp", ordering = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Apply formatting
    if ("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    if ("CumulativeOwnership" %in% names(display_data)) {
      dt <- dt %>% formatRound('CumulativeOwnership', digits = 1)
    }
    if ("GeometricMean" %in% names(display_data)) {
      dt <- dt %>% formatRound('GeometricMean', digits = 1)
    }
    # NEW: Format starting position columns
    if ("CumulativeStarting" %in% names(display_data)) {
      dt <- dt %>% formatRound('CumulativeStarting', digits = 0)
    }
    if ("GeometricMeanStarting" %in% names(display_data)) {
      dt <- dt %>% formatRound('GeometricMeanStarting', digits = 1)
    }
    
    # Apply color formatting to count columns
    count_cols <- grep("^Top[0-9]+Count$", names(display_data), value = TRUE)
    for (col in count_cols) {
      if (any(!is.na(display_data[[col]]))) {
        max_count <- max(display_data[[col]], na.rm = TRUE)
        if (is.finite(max_count) && max_count > 0) {
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
    req(rv$fd_optimal_lineups_display)
    
    # Clone lineups for display
    display_data <- as.data.table(rv$fd_optimal_lineups_display)
    
    # Format driver columns to show names
    if (!is.null(rv$fd_fantasy_analysis)) {
      for (i in 1:FD_ROSTER_SIZE) {
        col <- paste0("Driver", i)
        display_data[[col]] <- sapply(display_data[[col]], function(id) {
          match_idx <- which(rv$fd_fantasy_analysis$FDName == id)
          if (length(match_idx) > 0) {
            rv$fd_fantasy_analysis$Name[match_idx[1]]
          } else {
            id
          }
        })
      }
    }
    
    # Remove Rank columns, keep TopX Count columns and NEW starting position columns
    cols_to_keep <- c(
      paste0("Driver", 1:FD_ROSTER_SIZE),
      grep("^Top[0-9]+Count$", names(display_data), value = TRUE),
      "TotalSalary", "CumulativeOwnership", "GeometricMean",
      "CumulativeStarting", "GeometricMeanStarting"  # NEW
    )
    cols_to_keep <- intersect(cols_to_keep, names(display_data))
    
    # Use the correct data.table syntax
    display_data <- display_data[, ..cols_to_keep]
    
    # Sort and create datatable (same logic as DK)
    if ("Top1Count" %in% names(display_data) && "Top5Count" %in% names(display_data)) {
      setorder(display_data, -Top1Count, -Top5Count)
    } else if ("Top1Count" %in% names(display_data)) {
      setorder(display_data, -Top1Count)
    }
    
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25, scrollX = TRUE, rownames = FALSE,
        dom = "ftp", ordering = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Apply all formatting including NEW starting position columns
    if ("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    if ("CumulativeOwnership" %in% names(display_data)) {
      dt <- dt %>% formatRound('CumulativeOwnership', digits = 1)
    }
    if ("GeometricMean" %in% names(display_data)) {
      dt <- dt %>% formatRound('GeometricMean', digits = 1)
    }
    # NEW: Format starting position columns
    if ("CumulativeStarting" %in% names(display_data)) {
      dt <- dt %>% formatRound('CumulativeStarting', digits = 0)
    }
    if ("GeometricMeanStarting" %in% names(display_data)) {
      dt <- dt %>% formatRound('GeometricMeanStarting', digits = 1)
    }
    
    # Apply color formatting to count columns
    count_cols <- grep("^Top[0-9]+Count$", names(display_data), value = TRUE)
    for (col in count_cols) {
      if (any(!is.na(display_data[[col]]))) {
        max_count <- max(display_data[[col]], na.rm = TRUE)
        if (is.finite(max_count) && max_count > 0) {
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
  
  output$dk_filtered_pool_size <- renderText({
    req(rv$dk_optimal_lineups)
    
    filters <- list(
      min_top1_count = input$dk_min_top1_count,
      min_top2_count = input$dk_min_top2_count,
      min_top3_count = input$dk_min_top3_count,
      min_top5_count = input$dk_min_top5_count,
      min_cumulative_ownership = input$dk_ownership_range[1],
      max_cumulative_ownership = input$dk_ownership_range[2],
      min_geometric_mean = input$dk_geometric_range[1],
      max_geometric_mean = input$dk_geometric_range[2],
      min_cumulative_starting = input$dk_starting_range[1],        # FIXED: Added
      max_cumulative_starting = input$dk_starting_range[2],        # FIXED: Added
      min_geometric_starting = input$dk_starting_geo_range[1],     # FIXED: Added
      max_geometric_starting = input$dk_starting_geo_range[2],     # FIXED: Added
      excluded_drivers = input$dk_excluded_drivers
    )
    
    stats <- calculate_dk_filtered_pool_stats(rv$dk_optimal_lineups, filters)
    paste("Number of lineups in filtered pool:", stats$count)
  })
  
  
  output$fd_filtered_pool_size <- renderText({
    # Add dependencies to ensure this only runs when everything is ready
    req(rv$fd_optimal_lineups)
    req(rv$sliders_initialized$fd)  # Wait for slider initialization
    
    # Also wait for the actual input values to be available
    req(input$fd_min_top1_count)
    req(input$fd_ownership_range)
    req(input$fd_geometric_range) 
    req(input$fd_starting_range)
    req(input$fd_starting_geo_range)
    
    tryCatch({
      # Create filters list
      filters <- list(
        min_top1_count = input$fd_min_top1_count %||% 0,
        min_top2_count = input$fd_min_top2_count %||% 0,
        min_top3_count = input$fd_min_top3_count %||% 0,
        min_top5_count = input$fd_min_top5_count %||% 0,
        min_cumulative_ownership = input$fd_ownership_range[1] %||% 0,
        max_cumulative_ownership = input$fd_ownership_range[2] %||% 500,
        min_geometric_mean = input$fd_geometric_range[1] %||% 0,
        max_geometric_mean = input$fd_geometric_range[2] %||% 100,
        min_cumulative_starting = input$fd_starting_range[1] %||% 5,
        max_cumulative_starting = input$fd_starting_range[2] %||% 200,
        min_geometric_starting = input$fd_starting_geo_range[1] %||% 1,
        max_geometric_starting = input$fd_starting_geo_range[2] %||% 40,
        excluded_drivers = input$fd_excluded_drivers %||% character(0)
      )
      
      stats <- calculate_fd_filtered_pool_stats(rv$fd_optimal_lineups, filters)
      paste("Number of lineups in filtered pool:", stats$count)
      
    }, error = function(e) {
      cat("Error in fd_filtered_pool_size:", e$message, "\n")
      "Calculating pool size..."
    })
  })
  
  # Download handlers
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("nascar_simulation_results_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(rv$simulation_results, file, row.names = FALSE)
    }
  )
  
  output$downloadAccuracy <- downloadHandler(
    filename = function() {
      paste("simulation_accuracy_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(
        analyze_simulation_accuracy(rv$simulation_results, rv$processed_data$drivers),
        file,
        row.names = FALSE
      )
    }
  )
  
  output$download_dk_fantasy_projections <- downloadHandler(
    filename = function() {
      paste("dk_fantasy_projections_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(rv$dk_fantasy_analysis, file, row.names = FALSE)
    }
  )
  
  output$download_fd_fantasy_projections <- downloadHandler(
    filename = function() {
      paste("fd_fantasy_projections_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(rv$fd_fantasy_analysis, file, row.names = FALSE)
    }
  )
  
  output$download_dk_optimal_lineups <- downloadHandler(
    filename = function() {
      paste("dk_optimal_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      download_data <- as.data.frame(rv$dk_optimal_lineups_full)
      
      # Keep driver columns, TopX Count columns, TotalSalary, and NEW starting position columns
      cols_to_keep <- c(
        paste0("Driver", 1:DK_ROSTER_SIZE),
        grep("^Top[0-9]+Count$", names(download_data), value = TRUE),
        "TotalSalary", "CumulativeOwnership", "GeometricMean",
        "CumulativeStarting", "GeometricMeanStarting"  # NEW
      )
      cols_to_keep <- intersect(cols_to_keep, names(download_data))
      download_data <- download_data[, cols_to_keep, drop = FALSE]
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_fd_optimal_lineups <- downloadHandler(
    filename = function() {
      paste("fd_optimal_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      download_data <- as.data.frame(rv$fd_optimal_lineups_full)
      
      # Keep driver columns, TopX Count columns, TotalSalary, and NEW starting position columns
      cols_to_keep <- c(
        paste0("Driver", 1:FD_ROSTER_SIZE),
        grep("^Top[0-9]+Count$", names(download_data), value = TRUE),
        "TotalSalary", "CumulativeOwnership", "GeometricMean",
        "CumulativeStarting", "GeometricMeanStarting"  # NEW
      )
      cols_to_keep <- intersect(cols_to_keep, names(download_data))
      download_data <- download_data[, cols_to_keep, drop = FALSE]
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  
  # Memory cleanup functions
  observe({
    invalidateLater(180000) # 3 minutes
    gc(verbose = FALSE, full = TRUE)
  })
  
  # Generate random DraftKings lineups
  observeEvent(input$generate_dk_lineups, {
    rv$updating_sliders <- FALSE
    req(rv$dk_optimal_lineups)
    
    # Create filters for lineup generation
    filters <- list(
      min_top1_count = input$dk_min_top1_count,
      min_top2_count = input$dk_min_top2_count,
      min_top3_count = input$dk_min_top3_count,
      min_top5_count = input$dk_min_top5_count,
      min_cumulative_ownership = input$dk_ownership_range[1],
      max_cumulative_ownership = input$dk_ownership_range[2],
      min_geometric_mean = input$dk_geometric_range[1],
      max_geometric_mean = input$dk_geometric_range[2],
      min_cumulative_starting = input$dk_starting_range[1],        # NEW
      max_cumulative_starting = input$dk_starting_range[2],        # NEW
      min_geometric_starting = input$dk_starting_geo_range[1],     # NEW
      max_geometric_starting = input$dk_starting_geo_range[2], 
      excluded_drivers = input$dk_excluded_drivers,
      num_lineups = input$dk_num_random_lineups
    )
    
    # Show progress
    withProgress(message = 'Generating lineups...', value = 0, {
      # Generate random lineups
      rv$dk_random_lineups <- generate_random_dk_lineups(rv$dk_optimal_lineups, filters)
      
      if (!is.null(rv$dk_random_lineups)) {
        # First, let's preserve the existing driver mapping data
        existing_mapping <- NULL
        
        # Check if the current driver exposure table has mapping data
        if (!is.null(rv$dk_driver_exposure)) {
          # Extract the mapping columns from the exposure data
          existing_mapping <- rv$dk_driver_exposure[, c("DKName",
                                                        "Name",
                                                        "DKSalary",
                                                        "DKOP",
                                                        "Starting",
                                                        "Proj")]
        }
        
        # If we don't have existing mapping, create a new one
        if (is.null(existing_mapping) ||
            nrow(existing_mapping) == 0) {
          # Get all unique drivers from optimal lineups and random lineups
          driver_cols <- paste0("Driver", 1:DK_ROSTER_SIZE)
          
          # Get drivers from both lineup sets
          all_drivers <- c()
          
          # From optimal lineups
          for (col in driver_cols) {
            if (col %in% names(rv$dk_optimal_lineups)) {
              all_drivers <- c(all_drivers, rv$dk_optimal_lineups[[col]])
            }
          }
          
          # From random lineups
          for (col in driver_cols) {
            if (col %in% names(rv$dk_random_lineups)) {
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
          unique_sim_drivers <- rv$simulation_results[!duplicated(rv$simulation_results$DKName), c("DKName", "Name", "DKSalary", "DKOP", "Starting")]
          
          # Match each driver
          for (i in 1:nrow(driver_mapping)) {
            dk_name <- driver_mapping$DKName[i]
            matches <- which(unique_sim_drivers$DKName == dk_name)
            
            if (length(matches) > 0) {
              match_idx <- matches[1]
              driver_mapping$Name[i] <- unique_sim_drivers$Name[match_idx]
              driver_mapping$DKSalary[i] <- unique_sim_drivers$DKSalary[match_idx]
              driver_mapping$DKOP[i] <- unique_sim_drivers$DKOP[match_idx]
              driver_mapping$Starting[i] <- unique_sim_drivers$Starting[match_idx]
              
              # Get projection from fantasy analysis
              if (!is.null(rv$dk_fantasy_analysis)) {
                name_match <- which(rv$dk_fantasy_analysis$Name == unique_sim_drivers$Name[match_idx])
                if (length(name_match) > 0) {
                  driver_mapping$Proj[i] <- rv$dk_fantasy_analysis$Median_Fantasy_Pts[name_match[1]]
                }
              }
            }
          }
        } else {
          # Use the existing mapping
          driver_mapping <- existing_mapping
        }
        
        # Get current filters
        current_filters <- list(
          min_top1_count = input$dk_min_top1_count,
          min_top2_count = input$dk_min_top2_count,
          min_top3_count = input$dk_min_top3_count,
          min_top5_count = input$dk_min_top5_count,
          min_cumulative_ownership = input$dk_ownership_range[1],
          max_cumulative_ownership = input$dk_ownership_range[2],
          min_geometric_mean = input$dk_geometric_range[1],
          max_geometric_mean = input$dk_geometric_range[2],
          min_cumulative_starting = input$dk_starting_range[1],        # NEW
          max_cumulative_starting = input$dk_starting_range[2],        # NEW
          min_geometric_starting = input$dk_starting_geo_range[1],     # NEW
          max_geometric_starting = input$dk_starting_geo_range[2],  
          excluded_drivers = input$dk_excluded_drivers
        )
        
        # Calculate driver exposure with the mapping, random lineups, and current filters
        rv$dk_driver_exposure <- calculate_dk_driver_exposure(
          rv$dk_optimal_lineups,
          driver_mapping,
          rv$dk_random_lineups,
          current_filters
        )
      }
      
      # Show message based on result
      if (is.null(rv$dk_random_lineups)) {
        showModal(
          modalDialog(
            title = "Error",
            "No lineups match the selected filters. Try adjusting your criteria.",
            easyClose = TRUE
          )
        )
      } else {
        showModal(modalDialog(
          title = "Success",
          sprintf(
            "Generated %d DraftKings lineups successfully!",
            nrow(rv$dk_random_lineups)
          ),
          easyClose = TRUE
        ))
      }
    })
  })
  
  
  # REPLACE your existing tab observer (around line 3800) with this FIXED version
  observe({
    if(input$sidebar_menu == "lineup_builder") {
      
      # Update DraftKings elements
      if(!is.null(rv$dk_optimal_lineups)) {
        
        # Update excluded drivers dropdown
        if(!is.null(rv$dk_driver_exposure)) {
          driver_data <- rv$dk_driver_exposure
          driver_names <- driver_data$Name
          driver_ids <- driver_data$DKName
          driver_labels <- paste0(driver_names, " (", round(driver_data$OptimalRate, 1), "%)")
          driver_choices <- setNames(driver_ids, driver_labels)
          
          updateSelectizeInput(
            session = session,
            inputId = "dk_excluded_drivers",
            choices = driver_choices,
            selected = input$dk_excluded_drivers
          )
        }
        
        # ONLY update sliders if they haven't been user-modified
        if (!isTRUE(rv$user_modified_sliders$dk)) {
          # Update sliders based on actual data ranges
          if ("CumulativeOwnership" %in% names(rv$dk_optimal_lineups)) {
            ownership_values <- rv$dk_optimal_lineups$CumulativeOwnership
            ownership_values <- ownership_values[!is.na(ownership_values)]
            
            if (length(ownership_values) > 0) {
              min_own <- floor(min(ownership_values))
              max_own <- ceiling(max(ownership_values))
              
              updateSliderInput(session, "dk_ownership_range",
                                min = min_own, max = max_own, value = c(min_own, max_own))
            }
          }
          
          if ("GeometricMean" %in% names(rv$dk_optimal_lineups)) {
            geometric_values <- rv$dk_optimal_lineups$GeometricMean
            geometric_values <- geometric_values[!is.na(geometric_values)]
            
            if (length(geometric_values) > 0) {
              min_geo <- floor(min(geometric_values))
              max_geo <- ceiling(max(geometric_values))
              
              updateSliderInput(session, "dk_geometric_range",
                                min = min_geo, max = max_geo, value = c(min_geo, max_geo))
            }
          }
          
          # NEW: Update starting position sliders
          if ("CumulativeStarting" %in% names(rv$dk_optimal_lineups)) {
            starting_values <- rv$dk_optimal_lineups$CumulativeStarting
            starting_values <- starting_values[!is.na(starting_values)]
            
            if (length(starting_values) > 0) {
              min_start <- floor(min(starting_values))
              max_start <- ceiling(max(starting_values))
              
              updateSliderInput(session, "dk_starting_range",
                                min = min_start, max = max_start, value = c(min_start, max_start))
            }
          }
          
          if ("GeometricMeanStarting" %in% names(rv$dk_optimal_lineups)) {
            geo_starting_values <- rv$dk_optimal_lineups$GeometricMeanStarting
            geo_starting_values <- geo_starting_values[!is.na(geo_starting_values)]
            
            if (length(geo_starting_values) > 0) {
              min_geo_start <- floor(min(geo_starting_values) * 10) / 10
              max_geo_start <- ceiling(max(geo_starting_values) * 10) / 10
              
              updateSliderInput(session, "dk_starting_geo_range",
                                min = min_geo_start, max = max_geo_start, 
                                value = c(min_geo_start, max_geo_start))
            }
          }
        }
      }
      
      # Update FanDuel elements
      if(!is.null(rv$fd_optimal_lineups)) {
        
        # Update excluded drivers dropdown
        if(!is.null(rv$fd_driver_exposure)) {
          driver_data <- rv$fd_driver_exposure
          driver_names <- driver_data$Name
          driver_ids <- driver_data$FDName
          driver_labels <- paste0(driver_names, " (", round(driver_data$OptimalRate, 1), "%)")
          driver_choices <- setNames(driver_ids, driver_labels)
          
          updateSelectizeInput(
            session = session,
            inputId = "fd_excluded_drivers",
            choices = driver_choices,
            selected = input$fd_excluded_drivers
          )
        }
        
        # ONLY update sliders if they haven't been user-modified
        if (!isTRUE(rv$user_modified_sliders$fd)) {
          # Update sliders based on actual data ranges
          if ("CumulativeOwnership" %in% names(rv$fd_optimal_lineups)) {
            ownership_values <- rv$fd_optimal_lineups$CumulativeOwnership
            ownership_values <- ownership_values[!is.na(ownership_values)]
            
            if (length(ownership_values) > 0) {
              min_own <- floor(min(ownership_values))
              max_own <- ceiling(max(ownership_values))
              
              updateSliderInput(session, "fd_ownership_range",
                                min = min_own, max = max_own, value = c(min_own, max_own))
            }
          }
          
          if ("GeometricMean" %in% names(rv$fd_optimal_lineups)) {
            geometric_values <- rv$fd_optimal_lineups$GeometricMean
            geometric_values <- geometric_values[!is.na(geometric_values)]
            
            if (length(geometric_values) > 0) {
              min_geo <- floor(min(geometric_values))
              max_geo <- ceiling(max(geometric_values))
              
              updateSliderInput(session, "fd_geometric_range",
                                min = min_geo, max = max_geo, value = c(min_geo, max_geo))
            }
          }
          
          # NEW: Update starting position sliders for FanDuel
          if ("CumulativeStarting" %in% names(rv$fd_optimal_lineups)) {
            starting_values <- rv$fd_optimal_lineups$CumulativeStarting
            starting_values <- starting_values[!is.na(starting_values)]
            
            if (length(starting_values) > 0) {
              min_start <- floor(min(starting_values))
              max_start <- ceiling(max(starting_values))
              
              updateSliderInput(session, "fd_starting_range",
                                min = min_start, max = max_start, value = c(min_start, max_start))
            }
          }
          
          if ("GeometricMeanStarting" %in% names(rv$fd_optimal_lineups)) {
            geo_starting_values <- rv$fd_optimal_lineups$GeometricMeanStarting
            geo_starting_values <- geo_starting_values[!is.na(geo_starting_values)]
            
            if (length(geo_starting_values) > 0) {
              min_geo_start <- floor(min(geo_starting_values) * 10) / 10
              max_geo_start <- ceiling(max(geo_starting_values) * 10) / 10
              
              updateSliderInput(session, "fd_starting_geo_range",
                                min = min_geo_start, max = max_geo_start, 
                                value = c(min_geo_start, max_geo_start))
            }
          }
        }
      }
    }
  })
  
  
  
  observeEvent(rv$dk_optimal_lineups, {
    if (!is.null(rv$dk_optimal_lineups) && 
        nrow(rv$dk_optimal_lineups) > 0 && 
        !isTRUE(rv$sliders_initialized$dk)) {
      
      cat("Initializing DraftKings sliders...\n")
      
      # Set flag to prevent filter observers from firing
      rv$updating_sliders <- TRUE
      
      # Use isolate to prevent any reactive dependencies during initialization
      isolate({
        # Update Top Count inputs to default to 0
        updateNumericInput(session, "dk_min_top1_count", value = 0)
        updateNumericInput(session, "dk_min_top2_count", value = 0)
        updateNumericInput(session, "dk_min_top3_count", value = 0)
        updateNumericInput(session, "dk_min_top5_count", value = 0)
        
        # Update ownership slider based on actual data range - ONLY if not user-modified
        if (!isTRUE(rv$user_modified_sliders$dk) && "CumulativeOwnership" %in% names(rv$dk_optimal_lineups)) {
          ownership_values <- rv$dk_optimal_lineups$CumulativeOwnership
          ownership_values <- ownership_values[!is.na(ownership_values)]
          
          if (length(ownership_values) > 0) {
            min_own <- floor(min(ownership_values))
            max_own <- ceiling(max(ownership_values))
            
            cat("DK Ownership range:", min_own, "to", max_own, "\n")
            
            updateSliderInput(
              session,
              "dk_ownership_range",
              min = min_own,
              max = max_own,
              value = c(min_own, max_own),
              step = 1
            )
          }
        }
        
        # Update geometric mean slider based on actual data range - ONLY if not user-modified
        if (!isTRUE(rv$user_modified_sliders$dk) && "GeometricMean" %in% names(rv$dk_optimal_lineups)) {
          geometric_values <- rv$dk_optimal_lineups$GeometricMean
          geometric_values <- geometric_values[!is.na(geometric_values)]
          
          if (length(geometric_values) > 0) {
            min_geo <- floor(min(geometric_values))
            max_geo <- ceiling(max(geometric_values))
            
            cat("DK Geometric range:", min_geo, "to", max_geo, "\n")
            
            updateSliderInput(
              session,
              "dk_geometric_range",
              min = min_geo,
              max = max_geo,
              value = c(min_geo, max_geo),
              step = 0.1
            )
          }
        }
        
        # Update starting position sliders - ONLY if not user-modified
        if (!isTRUE(rv$user_modified_sliders$dk) && "CumulativeStarting" %in% names(rv$dk_optimal_lineups)) {
          starting_values <- rv$dk_optimal_lineups$CumulativeStarting
          starting_values <- starting_values[!is.na(starting_values)]
          
          if (length(starting_values) > 0) {
            min_start <- floor(min(starting_values))
            max_start <- ceiling(max(starting_values))
            
            updateSliderInput(session, "dk_starting_range",
                              min = min_start, max = max_start, 
                              value = c(min_start, max_start), step = 1)
          }
        }
        
        if (!isTRUE(rv$user_modified_sliders$dk) && "GeometricMeanStarting" %in% names(rv$dk_optimal_lineups)) {
          geo_starting_values <- rv$dk_optimal_lineups$GeometricMeanStarting
          geo_starting_values <- geo_starting_values[!is.na(geo_starting_values)]
          
          if (length(geo_starting_values) > 0) {
            min_geo_start <- floor(min(geo_starting_values) * 10) / 10
            max_geo_start <- ceiling(max(geo_starting_values) * 10) / 10
            
            updateSliderInput(session, "dk_starting_geo_range",
                              min = min_geo_start, max = max_geo_start, 
                              value = c(min_geo_start, max_geo_start), step = 0.1)
          }
        }
      })
      
      # FIXED: Use a longer delay to ensure all updates complete before resetting flags
      observe({
        invalidateLater(1000, session)  # Wait 1 second instead of immediate
        if (isTRUE(rv$updating_sliders)) {  # Only reset if still updating
          rv$sliders_initialized$dk <- TRUE
          rv$updating_sliders <- FALSE
          # Don't reset user_modified_sliders here - let it stay TRUE if user changed something
          cat("DraftKings sliders initialization complete\n")
        }
      })
    }
  }, once = TRUE)
  
  observeEvent(rv$fd_optimal_lineups, {
    if (!is.null(rv$fd_optimal_lineups) && 
        nrow(rv$fd_optimal_lineups) > 0 && 
        !isTRUE(rv$sliders_initialized$fd)) {
      
      cat("Initializing FanDuel sliders...\n")
      
      # Set flag to prevent filter observers from firing
      rv$updating_sliders <- TRUE
      
      # Use isolate to prevent any reactive dependencies during initialization
      isolate({
        # Update Top Count inputs to default to 0
        updateNumericInput(session, "fd_min_top1_count", value = 0)
        updateNumericInput(session, "fd_min_top2_count", value = 0)
        updateNumericInput(session, "fd_min_top3_count", value = 0)
        updateNumericInput(session, "fd_min_top5_count", value = 0)
        
        # Update ownership slider based on actual data range
        if ("CumulativeOwnership" %in% names(rv$fd_optimal_lineups)) {
          ownership_values <- rv$fd_optimal_lineups$CumulativeOwnership
          ownership_values <- ownership_values[!is.na(ownership_values)]
          
          if (length(ownership_values) > 0) {
            min_own <- floor(min(ownership_values))
            max_own <- ceiling(max(ownership_values))
            
            cat("FD Ownership range:", min_own, "to", max_own, "\n")
            
            updateSliderInput(
              session,
              "fd_ownership_range",
              min = min_own,
              max = max_own,
              value = c(min_own, max_own),
              step = 1
            )
          }
        }
        
        # Update geometric mean slider
        if ("GeometricMean" %in% names(rv$fd_optimal_lineups)) {
          geometric_values <- rv$fd_optimal_lineups$GeometricMean
          geometric_values <- geometric_values[!is.na(geometric_values)]
          
          if (length(geometric_values) > 0) {
            min_geo <- floor(min(geometric_values))
            max_geo <- ceiling(max(geometric_values))
            
            cat("FD Geometric range:", min_geo, "to", max_geo, "\n")
            
            updateSliderInput(
              session,
              "fd_geometric_range",
              min = min_geo,
              max = max_geo,
              value = c(min_geo, max_geo),
              step = 0.1
            )
          }
        }
        
        # Update starting position sliders
        if ("CumulativeStarting" %in% names(rv$fd_optimal_lineups)) {
          starting_values <- rv$fd_optimal_lineups$CumulativeStarting
          starting_values <- starting_values[!is.na(starting_values)]
          
          if (length(starting_values) > 0) {
            min_start <- floor(min(starting_values))
            max_start <- ceiling(max(starting_values))
            
            updateSliderInput(session, "fd_starting_range",
                              min = min_start, max = max_start, 
                              value = c(min_start, max_start), step = 1)
          }
        }
        
        if ("GeometricMeanStarting" %in% names(rv$fd_optimal_lineups)) {
          geo_starting_values <- rv$fd_optimal_lineups$GeometricMeanStarting
          geo_starting_values <- geo_starting_values[!is.na(geo_starting_values)]
          
          if (length(geo_starting_values) > 0) {
            min_geo_start <- floor(min(geo_starting_values) * 10) / 10
            max_geo_start <- ceiling(max(geo_starting_values) * 10) / 10
            
            updateSliderInput(session, "fd_starting_geo_range",
                              min = min_geo_start, max = max_geo_start, 
                              value = c(min_geo_start, max_geo_start), step = 0.1)
          }
        }
      })
      
      # FIXED: Use a more reliable way to mark initialization complete
      observe({
        invalidateLater(1500, session)  # Wait longer for all updates to complete
        if (isTRUE(rv$updating_sliders)) {  
          rv$sliders_initialized$fd <- TRUE
          rv$updating_sliders <- FALSE
          cat("FanDuel sliders initialization complete\n")
          
          # Force the filtered pool size to recalculate after initialization
          observe({
            invalidateLater(200, session)  # Short delay then trigger recalc
            outputOptions(output, "fd_filtered_pool_size", suspendWhenHidden = FALSE)
          })
        }
      })
    }
  }, once = TRUE)
  
  
  # Initialize DraftKings contest sliders based on actual data ranges
  observeEvent(rv$dk_optimal_lineups, {
    if (!is.null(rv$dk_optimal_lineups) && nrow(rv$dk_optimal_lineups) > 0) {
      
      # Update ownership slider based on actual data range
      if ("CumulativeOwnership" %in% names(rv$dk_optimal_lineups)) {
        ownership_values <- rv$dk_optimal_lineups$CumulativeOwnership
        ownership_values <- ownership_values[!is.na(ownership_values)]
        
        if (length(ownership_values) > 0) {
          min_own <- floor(min(ownership_values))
          max_own <- ceiling(max(ownership_values))
          
          updateSliderInput(
            session,
            "dk_contest_ownership_range",
            min = min_own,
            max = max_own,
            value = c(min_own, max_own),
            step = 1
          )
        }
      }
      
      # Update geometric mean slider
      if ("GeometricMean" %in% names(rv$dk_optimal_lineups)) {
        geometric_values <- rv$dk_optimal_lineups$GeometricMean
        geometric_values <- geometric_values[!is.na(geometric_values)]
        
        if (length(geometric_values) > 0) {
          min_geo <- floor(min(geometric_values))
          max_geo <- ceiling(max(geometric_values))
          
          updateSliderInput(
            session,
            "dk_contest_geometric_range",
            min = min_geo,
            max = max_geo,
            value = c(min_geo, max_geo),
            step = 0.1
          )
        }
      }
      
      # Update starting position sliders
      if ("CumulativeStarting" %in% names(rv$dk_optimal_lineups)) {
        starting_values <- rv$dk_optimal_lineups$CumulativeStarting
        starting_values <- starting_values[!is.na(starting_values)]
        
        if (length(starting_values) > 0) {
          min_start <- floor(min(starting_values))
          max_start <- ceiling(max(starting_values))
          
          updateSliderInput(session, "dk_contest_starting_range",
                            min = min_start, max = max_start, 
                            value = c(min_start, max_start), step = 1)
        }
      }
      
      if ("GeometricMeanStarting" %in% names(rv$dk_optimal_lineups)) {
        geo_starting_values <- rv$dk_optimal_lineups$GeometricMeanStarting
        geo_starting_values <- geo_starting_values[!is.na(geo_starting_values)]
        
        if (length(geo_starting_values) > 0) {
          min_geo_start <- floor(min(geo_starting_values) * 10) / 10
          max_geo_start <- ceiling(max(geo_starting_values) * 10) / 10
          
          updateSliderInput(session, "dk_contest_starting_geo_range",
                            min = min_geo_start, max = max_geo_start, 
                            value = c(min_geo_start, max_geo_start), step = 0.1)
        }
      }
    }
  }, once = TRUE)
  
  # Initialize FanDuel contest sliders based on actual data ranges
  observeEvent(rv$fd_optimal_lineups, {
    if (!is.null(rv$fd_optimal_lineups) && nrow(rv$fd_optimal_lineups) > 0) {
      
      # Update ownership slider based on actual data range
      if ("CumulativeOwnership" %in% names(rv$fd_optimal_lineups)) {
        ownership_values <- rv$fd_optimal_lineups$CumulativeOwnership
        ownership_values <- ownership_values[!is.na(ownership_values)]
        
        if (length(ownership_values) > 0) {
          min_own <- floor(min(ownership_values))
          max_own <- ceiling(max(ownership_values))
          
          updateSliderInput(
            session,
            "fd_contest_ownership_range",
            min = min_own,
            max = max_own,
            value = c(min_own, max_own),
            step = 1
          )
        }
      }
      
      # Update geometric mean slider
      if ("GeometricMean" %in% names(rv$fd_optimal_lineups)) {
        geometric_values <- rv$fd_optimal_lineups$GeometricMean
        geometric_values <- geometric_values[!is.na(geometric_values)]
        
        if (length(geometric_values) > 0) {
          min_geo <- floor(min(geometric_values))
          max_geo <- ceiling(max(geometric_values))
          
          updateSliderInput(
            session,
            "fd_contest_geometric_range",
            min = min_geo,
            max = max_geo,
            value = c(min_geo, max_geo),
            step = 0.1
          )
        }
      }
      
      # Update starting position sliders
      if ("CumulativeStarting" %in% names(rv$fd_optimal_lineups)) {
        starting_values <- rv$fd_optimal_lineups$CumulativeStarting
        starting_values <- starting_values[!is.na(starting_values)]
        
        if (length(starting_values) > 0) {
          min_start <- floor(min(starting_values))
          max_start <- ceiling(max(starting_values))
          
          updateSliderInput(session, "fd_contest_starting_range",
                            min = min_start, max = max_start, 
                            value = c(min_start, max_start), step = 1)
        }
      }
      
      if ("GeometricMeanStarting" %in% names(rv$fd_optimal_lineups)) {
        geo_starting_values <- rv$fd_optimal_lineups$GeometricMeanStarting
        geo_starting_values <- geo_starting_values[!is.na(geo_starting_values)]
        
        if (length(geo_starting_values) > 0) {
          min_geo_start <- floor(min(geo_starting_values) * 10) / 10
          max_geo_start <- ceiling(max(geo_starting_values) * 10) / 10
          
          updateSliderInput(session, "fd_contest_starting_geo_range",
                            min = min_geo_start, max = max_geo_start, 
                            value = c(min_geo_start, max_geo_start), step = 0.1)
        }
      }
    }
  }, once = TRUE)
  
  # Generate random FanDuel lineups
  observeEvent(input$generate_fd_lineups, {
    rv$updating_sliders <- FALSE
    req(rv$fd_optimal_lineups)
    
    # Create filters for lineup generation
    filters <- list(
      min_top1_count = input$fd_min_top1_count,
      min_top2_count = input$fd_min_top2_count,
      min_top3_count = input$fd_min_top3_count,
      min_top5_count = input$fd_min_top5_count,
      min_cumulative_ownership = input$fd_ownership_range[1],
      max_cumulative_ownership = input$fd_ownership_range[2],
      min_geometric_mean = input$fd_geometric_range[1],
      max_geometric_mean = input$fd_geometric_range[2],
      min_cumulative_starting = input$fd_starting_range[1],        # NEW
      max_cumulative_starting = input$fd_starting_range[2],        # NEW
      min_geometric_starting = input$fd_starting_geo_range[1],     # NEW
      max_geometric_starting = input$fd_starting_geo_range[2], 
      excluded_drivers = input$fd_excluded_drivers,
      num_lineups = input$fd_num_random_lineups
    )
    
    # Show progress
    withProgress(message = 'Generating lineups...', value = 0, {
      # Generate random lineups
      rv$fd_random_lineups <- generate_random_fd_lineups(rv$fd_optimal_lineups, filters)
      
      # Update driver exposure data using the same mapping approach from optimization
      if (!is.null(rv$fd_random_lineups)) {
        # First, let's preserve the existing driver mapping data
        existing_mapping <- NULL
        
        # Check if the current driver exposure table has mapping data
        if (!is.null(rv$fd_driver_exposure)) {
          # Extract the mapping columns from the exposure data
          existing_mapping <- rv$fd_driver_exposure[, c("FDName",
                                                        "Name",
                                                        "FDSalary",
                                                        "FDOP",
                                                        "Starting",
                                                        "Proj")]
        }
        
        # If we don't have existing mapping, create a new one
        if (is.null(existing_mapping) ||
            nrow(existing_mapping) == 0) {
          # Get all unique drivers from optimal lineups and random lineups
          driver_cols <- paste0("Driver", 1:FD_ROSTER_SIZE)
          
          # Get drivers from both lineup sets
          all_drivers <- c()
          
          # From optimal lineups
          for (col in driver_cols) {
            if (col %in% names(rv$fd_optimal_lineups)) {
              all_drivers <- c(all_drivers, rv$fd_optimal_lineups[[col]])
            }
          }
          
          # From random lineups
          for (col in driver_cols) {
            if (col %in% names(rv$fd_random_lineups)) {
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
          unique_sim_drivers <- rv$simulation_results[!duplicated(rv$simulation_results$FDName), c("FDName", "Name", "FDSalary", "FDOP", "Starting")]
          
          # Match each driver
          for (i in 1:nrow(driver_mapping)) {
            fd_name <- driver_mapping$FDName[i]
            matches <- which(unique_sim_drivers$FDName == fd_name)
            
            if (length(matches) > 0) {
              match_idx <- matches[1]
              driver_mapping$Name[i] <- unique_sim_drivers$Name[match_idx]
              driver_mapping$FDSalary[i] <- unique_sim_drivers$FDSalary[match_idx]
              driver_mapping$FDOP[i] <- unique_sim_drivers$FDOP[match_idx]
              driver_mapping$Starting[i] <- unique_sim_drivers$Starting[match_idx]
              
              # Get projection from fantasy analysis
              if (!is.null(rv$fd_fantasy_analysis)) {
                name_match <- which(rv$fd_fantasy_analysis$Name == unique_sim_drivers$Name[match_idx])
                if (length(name_match) > 0) {
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
        rv$fd_driver_exposure <- calculate_fd_driver_exposure(rv$fd_optimal_lineups,
                                                              driver_mapping,
                                                              rv$fd_random_lineups)
      }
      
      # Show message based on result
      if (is.null(rv$fd_random_lineups)) {
        showModal(
          modalDialog(
            title = "Error",
            "No lineups match the selected filters. Try adjusting your criteria.",
            easyClose = TRUE
          )
        )
      } else {
        showModal(modalDialog(
          title = "Success",
          sprintf(
            "Generated %d FanDuel lineups successfully!",
            nrow(rv$fd_random_lineups)
          ),
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
    if (is.null(rv$dk_random_lineups) ||
        nrow(rv$dk_random_lineups) == 0) {
      # Make sure these columns don't appear if they somehow exist
      display_data$Exposure <- NULL
      display_data$Leverage <- NULL
    }
    
    # Hide DKName column as requested
    display_data$DKName <- NULL
    
    
    
    col_order <- c(
      "Name",
      "Starting",
      "Proj",
      "DKSalary",
      "DKOP",
      "OptimalRate",
      "FilteredPoolRate"
    )
    
    # Add Exposure and Leverage if they exist
    if ("Exposure" %in% names(display_data)) {
      col_order <- c(col_order, "Exposure")
    }
    if ("Leverage" %in% names(display_data)) {
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
        order = list(list(5, 'desc')),
        # Sort by OptimalRate by default (adjusted for new column order)
        rownames = FALSE  # Remove row numbers
      ),
      rownames = FALSE
    )
    
    # Apply formatting
    if ("DKSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency(
        'DKSalary',
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      )
    }
    
    # Format numeric columns with 1 decimal place
    numeric_cols <- intersect(
      c(
        'OptimalRate',
        'FilteredPoolRate',
        'Exposure',
        'Leverage',
        'Proj',
        'DKOP'
      ),
      names(display_data)
    )
    
    if (length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(numeric_cols, digits = 1)
    }
    
    return(dt)
  })
  
  output$fd_driver_exposure_table <- renderDT({
    req(rv$fd_driver_exposure)
    
    # Clone the data for display
    display_data <- rv$fd_driver_exposure
    
    # If random lineups don't exist, remove the Exposure and Leverage columns
    if (is.null(rv$fd_random_lineups) ||
        nrow(rv$fd_random_lineups) == 0) {
      # Make sure these columns don't appear if they somehow exist
      display_data$Exposure <- NULL
      display_data$Leverage <- NULL
    }
    
    # Hide FDName column as requested
    display_data$FDName <- NULL
    
    col_order <- c(
      "Name",
      "Starting",
      "Proj",
      "FDSalary",
      "FDOP",
      "OptimalRate",
      "FilteredPoolRate"
    )
    
    # Add Exposure and Leverage if they exist
    if ("Exposure" %in% names(display_data)) {
      col_order <- c(col_order, "Exposure")
    }
    if ("Leverage" %in% names(display_data)) {
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
        order = list(list(5, 'desc')),
        # Sort by OptimalRate by default (adjusted for new column order)
        rownames = FALSE  # Remove row numbers
      ),
      rownames = FALSE
    )
    
    # Apply formatting
    if ("FDSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency(
        'FDSalary',
        currency = "$",
        interval = 3,
        mark = ",",
        digits = 0
      )
    }
    
    # Format numeric columns with 1 decimal place
    numeric_cols <- intersect(
      c(
        'OptimalRate',
        'FilteredPoolRate',
        'Exposure',
        'Leverage',
        'Proj',
        'FDOP'
      ),
      names(display_data)
    )
    
    if (length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(numeric_cols, digits = 1)
    }
    
    return(dt)
  })
  
  
  
  output$dk_random_lineups_table <- renderDT({
    req(rv$dk_random_lineups)
    
    # Clone for display
    display_data <- as.data.frame(rv$dk_random_lineups)
    
    # Format driver columns to show names
    if (!is.null(rv$dk_fantasy_analysis)) {
      for (i in 1:DK_ROSTER_SIZE) {
        col <- paste0("Driver", i)
        if (col %in% names(display_data)) {
          display_data[[col]] <- sapply(display_data[[col]], function(id) {
            match_idx <- which(rv$dk_fantasy_analysis$DKName == id)
            if (length(match_idx) > 0) {
              rv$dk_fantasy_analysis$Name[match_idx[1]]
            } else {
              id
            }
          })
        }
      }
    }
    
    # Keep driver columns, TopXCount columns, TotalSalary, and NEW starting position columns
    cols_to_keep <- c(
      paste0("Driver", 1:DK_ROSTER_SIZE),
      grep("^Top[0-9]+Count$", names(display_data), value = TRUE),
      "TotalSalary", "CumulativeStarting", "GeometricMeanStarting"  # NEW columns added
    )
    cols_to_keep <- intersect(cols_to_keep, names(display_data))
    
    # Use standard data.frame subsetting
    display_data <- display_data[, cols_to_keep, drop = FALSE]
    
    # Create datatable with styling
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25, scrollX = TRUE, rownames = FALSE,
        dom = "tp", ordering = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Format columns
    if ("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    # NEW: Format starting position columns
    if ("CumulativeStarting" %in% names(display_data)) {
      dt <- dt %>% formatRound('CumulativeStarting', digits = 0)
    }
    if ("GeometricMeanStarting" %in% names(display_data)) {
      dt <- dt %>% formatRound('GeometricMeanStarting', digits = 1)
    }
    
    dt
  })
  
  # Update fd_random_lineups_table output (around line 4100):
  
  output$fd_random_lineups_table <- renderDT({
    req(rv$fd_random_lineups)
    
    # Clone for display
    display_data <- as.data.frame(rv$fd_random_lineups)
    
    # Format driver columns to show names
    if (!is.null(rv$fd_fantasy_analysis)) {
      for (i in 1:FD_ROSTER_SIZE) {
        col <- paste0("Driver", i)
        if (col %in% names(display_data)) {
          display_data[[col]] <- sapply(display_data[[col]], function(id) {
            match_idx <- which(rv$fd_fantasy_analysis$FDName == id)
            if (length(match_idx) > 0) {
              rv$fd_fantasy_analysis$Name[match_idx[1]]
            } else {
              id
            }
          })
        }
      }
    }
    
    # Keep driver columns, TopXCount columns, TotalSalary, and NEW starting position columns
    cols_to_keep <- c(
      paste0("Driver", 1:FD_ROSTER_SIZE),
      grep("^Top[0-9]+Count$", names(display_data), value = TRUE),
      "TotalSalary", "CumulativeStarting", "GeometricMeanStarting"  # NEW columns added
    )
    cols_to_keep <- intersect(cols_to_keep, names(display_data))
    
    # Use standard data.frame subsetting
    display_data <- display_data[, cols_to_keep, drop = FALSE]
    
    # Create datatable with styling
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25, scrollX = TRUE, rownames = FALSE,
        dom = "tp", ordering = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Format columns
    if ("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    # NEW: Format starting position columns
    if ("CumulativeStarting" %in% names(display_data)) {
      dt <- dt %>% formatRound('CumulativeStarting', digits = 0)
    }
    if ("GeometricMeanStarting" %in% names(display_data)) {
      dt <- dt %>% formatRound('GeometricMeanStarting', digits = 1)
    }
    
    dt
  })
  
  
  # DraftKings exposure info
  output$dk_exposure_info <- renderText({
    req(rv$dk_driver_exposure, rv$dk_random_lineups)
    
    if (is.null(rv$dk_random_lineups) ||
        nrow(rv$dk_random_lineups) == 0) {
      return("No lineups generated yet. Click 'Generate Lineups' to create lineups.")
    }
    
    # Get drivers with highest exposure
    top_exposed <- rv$dk_driver_exposure %>%
      filter(!is.na(Exposure)) %>%
      arrange(desc(Exposure)) %>%
      head(5)
    
    # Format as text
    paste(
      "Top driver exposures in generated lineups:",
      paste(sapply(1:nrow(top_exposed), function(i) {
        sprintf("\n%s: %.1f%%",
                top_exposed$Name[i],
                top_exposed$Exposure[i])
      }), collapse = ""),
      "\n\nNote: Adjust 'Maximum Driver Exposure' to control these values."
    )
  })
  
  # FanDuel exposure info
  output$fd_exposure_info <- renderText({
    req(rv$fd_driver_exposure, rv$fd_random_lineups)
    
    if (is.null(rv$fd_random_lineups) ||
        nrow(rv$fd_random_lineups) == 0) {
      return("No lineups generated yet. Click 'Generate Lineups' to create lineups.")
    }
    
    # Get drivers with highest exposure
    top_exposed <- rv$fd_driver_exposure %>%
      filter(!is.na(Exposure)) %>%
      arrange(desc(Exposure)) %>%
      head(5)
    
    # Format as text
    paste(
      "Top driver exposures in generated lineups:",
      paste(sapply(1:nrow(top_exposed), function(i) {
        sprintf("\n%s: %.1f%%",
                top_exposed$Name[i],
                top_exposed$Exposure[i])
      }), collapse = ""),
      "\n\nNote: Adjust 'Maximum Driver Exposure' to control these values."
    )
  })
  
  
  output$download_dk_random_lineups <- downloadHandler(
    filename = function() {
      paste("dk_random_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      if (is.null(rv$dk_random_lineups) || nrow(rv$dk_random_lineups) == 0) {
        empty_data <- data.frame(matrix(ncol = DK_ROSTER_SIZE, nrow = 0))
        colnames(empty_data) <- paste0("Driver", 1:DK_ROSTER_SIZE)
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      download_data <- as.data.frame(rv$dk_random_lineups)
      
      # Keep driver columns, TopX Count columns, TotalSalary, and NEW starting position columns
      cols_to_keep <- c(
        paste0("Driver", 1:DK_ROSTER_SIZE),
        grep("^Top[0-9]+Count$", names(download_data), value = TRUE),
        "TotalSalary", "CumulativeStarting", "GeometricMeanStarting"  # NEW
      )
      cols_to_keep <- intersect(cols_to_keep, names(download_data))
      download_data <- download_data[, cols_to_keep, drop = FALSE]
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # Update download_fd_random_lineups handler (around line 4230):
  
  output$download_fd_random_lineups <- downloadHandler(
    filename = function() {
      paste("fd_random_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      if (is.null(rv$fd_random_lineups) || nrow(rv$fd_random_lineups) == 0) {
        empty_data <- data.frame(matrix(ncol = FD_ROSTER_SIZE, nrow = 0))
        colnames(empty_data) <- paste0("Driver", 1:FD_ROSTER_SIZE)
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      download_data <- as.data.frame(rv$fd_random_lineups)
      
      # Keep driver columns, TopX Count columns, TotalSalary, and NEW starting position columns
      cols_to_keep <- c(
        paste0("Driver", 1:FD_ROSTER_SIZE),
        grep("^Top[0-9]+Count$", names(download_data), value = TRUE),
        "TotalSalary", "CumulativeStarting", "GeometricMeanStarting"  # NEW
      )
      cols_to_keep <- intersect(cols_to_keep, names(download_data))
      download_data <- download_data[, cols_to_keep, drop = FALSE]
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$fd_filtered_pool_size <- renderText({
    req(rv$fd_optimal_lineups)
    
    # Use default values if sliders haven't been initialized yet
    filters <- list(
      min_top1_count = if(is.null(input$fd_min_top1_count)) 0 else input$fd_min_top1_count,
      min_top2_count = if(is.null(input$fd_min_top2_count)) 0 else input$fd_min_top2_count,
      min_top3_count = if(is.null(input$fd_min_top3_count)) 0 else input$fd_min_top3_count,
      min_top5_count = if(is.null(input$fd_min_top5_count)) 0 else input$fd_min_top5_count,
      
      min_cumulative_ownership = if(is.null(input$fd_ownership_range)) {
        if("CumulativeOwnership" %in% names(rv$fd_optimal_lineups)) {
          floor(min(rv$fd_optimal_lineups$CumulativeOwnership, na.rm = TRUE))
        } else 0
      } else input$fd_ownership_range[1],
      
      max_cumulative_ownership = if(is.null(input$fd_ownership_range)) {
        if("CumulativeOwnership" %in% names(rv$fd_optimal_lineups)) {
          ceiling(max(rv$fd_optimal_lineups$CumulativeOwnership, na.rm = TRUE))
        } else 500
      } else input$fd_ownership_range[2],
      
      min_geometric_mean = if(is.null(input$fd_geometric_range)) {
        if("GeometricMean" %in% names(rv$fd_optimal_lineups)) {
          floor(min(rv$fd_optimal_lineups$GeometricMean, na.rm = TRUE))
        } else 0
      } else input$fd_geometric_range[1],
      
      max_geometric_mean = if(is.null(input$fd_geometric_range)) {
        if("GeometricMean" %in% names(rv$fd_optimal_lineups)) {
          ceiling(max(rv$fd_optimal_lineups$GeometricMean, na.rm = TRUE))
        } else 100
      } else input$fd_geometric_range[2],
      
      min_cumulative_starting = if(is.null(input$fd_starting_range)) {
        if("CumulativeStarting" %in% names(rv$fd_optimal_lineups)) {
          floor(min(rv$fd_optimal_lineups$CumulativeStarting, na.rm = TRUE))
        } else 5
      } else input$fd_starting_range[1],
      
      max_cumulative_starting = if(is.null(input$fd_starting_range)) {
        if("CumulativeStarting" %in% names(rv$fd_optimal_lineups)) {
          ceiling(max(rv$fd_optimal_lineups$CumulativeStarting, na.rm = TRUE))
        } else 200
      } else input$fd_starting_range[2],
      
      min_geometric_starting = if(is.null(input$fd_starting_geo_range)) {
        if("GeometricMeanStarting" %in% names(rv$fd_optimal_lineups)) {
          floor(min(rv$fd_optimal_lineups$GeometricMeanStarting, na.rm = TRUE) * 10) / 10
        } else 1
      } else input$fd_starting_geo_range[1],
      
      max_geometric_starting = if(is.null(input$fd_starting_geo_range)) {
        if("GeometricMeanStarting" %in% names(rv$fd_optimal_lineups)) {
          ceiling(max(rv$fd_optimal_lineups$GeometricMeanStarting, na.rm = TRUE) * 10) / 10
        } else 40
      } else input$fd_starting_geo_range[2],
      
      excluded_drivers = if(is.null(input$fd_excluded_drivers)) character(0) else input$fd_excluded_drivers
    )
    
    stats <- calculate_fd_filtered_pool_stats(rv$fd_optimal_lineups, filters)
    paste("Number of lineups in filtered pool:", stats$count)
  })
  
  # Clean up on session end
  session$onSessionEnded(function() {
    gc(verbose = FALSE, full = TRUE)
  })
  
  
  
  observe({
    invalidateLater(240000, session)  # Every 4 minutes
    
    # Only run cleanup if we have simulation data and it's been a while
    if (!is.null(rv$simulation_results) && nrow(rv$simulation_results) > 10000) {
      cleanup_memory(verbose = FALSE)
      
      # Log memory status periodically for large datasets
      if (nrow(rv$simulation_results) > 100000) {
        mem_info <- gc(verbose = FALSE)
        total_used <- sum(mem_info[,2])
        if (total_used > 1000) {  # Only log if using > 1GB
          cat("Periodic memory check - Total used:", round(total_used, 0), "MB\n")
        }
      }
    }
  })
  
  
}


# Run the application
shinyApp(ui = ui, server = server)