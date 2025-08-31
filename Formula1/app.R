library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(data.table)
library(readxl)
library(dplyr)
library(shinycssloaders)
library(shinyjs)

# Your helper functions
setDTthreads(2)


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




# =============================================================================
# PRE-COMPUTATION FUNCTIONS - Add after existing helper functions
# =============================================================================

# Create lookup tables for faster access
create_lookup_tables <- function(driver_data, constructor_data) {
  # Driver lookups
  driver_name_to_id <- setNames(1:nrow(driver_data), driver_data$Name)
  driver_salaries <- setNames(driver_data$DriverSal, driver_data$Name)
  captain_salaries <- setNames(driver_data$CptSal, driver_data$Name)
  
  # Constructor lookups
  constructor_name_to_id <- setNames(1:nrow(constructor_data), constructor_data$Name)
  constructor_salaries <- setNames(constructor_data$Salary, constructor_data$Name)
  
  # Team groupings
  team_groups <- split(1:nrow(driver_data), driver_data$Team)
  
  # Position points lookup
  position_points <- c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1, rep(0, 10))
  
  return(list(
    driver_name_to_id = driver_name_to_id,
    driver_salaries = driver_salaries,
    captain_salaries = captain_salaries,
    constructor_name_to_id = constructor_name_to_id,
    constructor_salaries = constructor_salaries,
    team_groups = team_groups,
    position_points = position_points
  ))
}

precompute_valid_lineups <- function(driver_data, constructor_data, salary_cap = 50000) {
  cat("Pre-computing valid lineups (memory-optimized approach)...\n")
  start_time <- Sys.time()
  
  # Extract driver info
  drivers <- driver_data[, .(Name, DriverSal, CptSal, Team)]
  constructors <- constructor_data[, .(Name, Salary)]
  
  # Create salary lookup tables for faster access
  driver_salaries <- setNames(drivers$DriverSal, drivers$Name)
  captain_salaries <- setNames(drivers$CptSal, drivers$Name)
  constructor_salaries <- setNames(constructors$Salary, constructors$Name)
  driver_teams <- setNames(drivers$Team, drivers$Name)
  
  driver_names <- drivers$Name
  constructor_names <- constructors$Name
  
  # OPTIMIZATION 1: Filter out expensive lineups early (add minimum salary filter)
  min_salary_filter <- 47000  # Only keep lineups >= $47k
  cat("Using minimum salary filter of $", min_salary_filter, "\n")
  
  # OPTIMIZATION 2: Pre-filter expensive drivers/constructors
  # Find cheapest possible lineup to set reasonable bounds
  cheapest_4_drivers <- sum(sort(driver_salaries)[1:4])
  cheapest_captain <- min(captain_salaries)
  cheapest_constructor <- min(constructor_salaries)
  absolute_min_salary <- cheapest_4_drivers + cheapest_captain + cheapest_constructor
  
  cat("Absolute minimum possible salary: $", absolute_min_salary, "\n")
  
  # Step 1: Generate all possible 4-driver "core" combinations
  core_combos <- combn(driver_names, 4, simplify = FALSE)
  cat("Generated", length(core_combos), "possible 4-driver core combinations\n")
  
  # OPTIMIZATION 3: Pre-allocate data.table instead of growing list
  max_possible_lineups <- min(150000, length(core_combos) * 16 * length(constructor_names) / 2)
  
  # Pre-allocate vectors for efficiency
  lineup_ids <- integer(max_possible_lineups)
  captains <- character(max_possible_lineups)
  driver1s <- character(max_possible_lineups)
  driver2s <- character(max_possible_lineups) 
  driver3s <- character(max_possible_lineups)
  driver4s <- character(max_possible_lineups)
  constructors_vec <- character(max_possible_lineups)
  total_salaries <- numeric(max_possible_lineups)
  
  lineup_count <- 0
  
  # OPTIMIZATION 4: Process in smaller batches with garbage collection
  batch_size <- 500
  n_batches <- ceiling(length(core_combos) / batch_size)
  
  for (batch in 1:n_batches) {
    batch_start <- (batch - 1) * batch_size + 1
    batch_end <- min(batch * batch_size, length(core_combos))
    
    if (batch %% 5 == 0) {
      cat("Processing batch", batch, "of", n_batches, "(combinations", batch_start, "to", batch_end, ")\n")
      
      # Periodic garbage collection
      if (batch %% 10 == 0) {
        gc(verbose = FALSE)
      }
    }
    
    # Process batch
    for (i in batch_start:batch_end) {
      core_drivers <- core_combos[[i]]
      core_salary <- sum(driver_salaries[core_drivers])
      
      # OPTIMIZATION 5: Early filtering - skip cores that are too expensive
      max_remaining_budget <- salary_cap - core_salary
      min_captain_plus_constructor <- min(captain_salaries) + min(constructor_salaries)
      
      if (max_remaining_budget < min_captain_plus_constructor) next
      
      # Get remaining drivers that can be captain (excluding the 4 core drivers)
      remaining_drivers <- setdiff(driver_names, core_drivers)
      
      # Step 2: For this core, try all captain + constructor combinations
      for (captain in remaining_drivers) {
        captain_salary <- captain_salaries[captain]
        core_plus_captain_salary <- core_salary + captain_salary
        
        # OPTIMIZATION 6: Early salary check with minimum filter
        if (core_plus_captain_salary > salary_cap) next
        max_constructor_budget <- salary_cap - core_plus_captain_salary
        min_constructor_budget <- min_salary_filter - core_plus_captain_salary
        
        for (constructor in constructor_names) {
          constructor_salary <- constructor_salaries[constructor]
          
          # Check both salary cap and minimum salary filter
          if (constructor_salary > max_constructor_budget) next
          
          total_salary <- core_plus_captain_salary + constructor_salary
          
          if (total_salary > salary_cap || total_salary < min_salary_filter) next
          
          # Check team constraint
          all_drivers <- c(core_drivers, captain)
          all_driver_teams <- driver_teams[all_drivers]
          constructor_team <- constructor
          
          # Count drivers from constructor's team
          drivers_from_constructor_team <- sum(all_driver_teams == constructor_team)
          
          # Only allow if we don't have 2+ drivers + constructor from same team
          if (drivers_from_constructor_team < 2) {
            lineup_count <- lineup_count + 1
            
            # OPTIMIZATION 7: Direct vector assignment instead of list growth
            if (lineup_count <= max_possible_lineups) {
              lineup_ids[lineup_count] <- lineup_count
              captains[lineup_count] <- captain
              # FIXED: Ensure we assign all 4 drivers from the core combination
              driver1s[lineup_count] <- core_drivers[1]
              driver2s[lineup_count] <- core_drivers[2] 
              driver3s[lineup_count] <- core_drivers[3]
              driver4s[lineup_count] <- core_drivers[4]  # THE 4TH DRIVER FROM CORE
              constructors_vec[lineup_count] <- constructor
              total_salaries[lineup_count] <- total_salary
            } else {
              # Safety check - shouldn't happen with good estimation
              cat("Warning: Exceeded pre-allocated space at", lineup_count, "lineups\n")
              break
            }
          }
        }
      }
    }
  }
  
  # OPTIMIZATION 8: Create final data.table from pre-allocated vectors
  if (lineup_count > 0) {
    valid_lineups <- data.table(
      LineupID = lineup_ids[1:lineup_count],
      Captain = captains[1:lineup_count],
      Driver1 = driver1s[1:lineup_count],
      Driver2 = driver2s[1:lineup_count],
      Driver3 = driver3s[1:lineup_count],
      Driver4 = driver4s[1:lineup_count],  # ENSURE Driver4 is included
      Constructor = constructors_vec[1:lineup_count],
      TotalSalary = total_salaries[1:lineup_count]
    )
  } else {
    # Create empty data.table with correct structure
    valid_lineups <- data.table(
      LineupID = integer(0),
      Captain = character(0),
      Driver1 = character(0),
      Driver2 = character(0),
      Driver3 = character(0),
      Driver4 = character(0),  # ENSURE Driver4 is included
      Constructor = character(0),
      TotalSalary = numeric(0)
    )
  }
  
  # Final garbage collection
  gc(verbose = FALSE)
  
  end_time <- Sys.time()
  cat("Pre-computation complete! Found", nrow(valid_lineups), "valid lineups in", 
      round(as.numeric(difftime(end_time, start_time, units = "secs")), 2), "seconds\n")
  
  # Print statistics
  if (nrow(valid_lineups) > 0) {
    avg_salary <- mean(valid_lineups$TotalSalary)
    min_salary <- min(valid_lineups$TotalSalary)
    max_salary <- max(valid_lineups$TotalSalary)
    cat("Salary range: $", min_salary, " to $", max_salary, " (avg: $", round(avg_salary), ")\n", sep = "")
    cat("Memory usage after pre-computation:", get_memory_usage(), "MB\n")
  }
  
  return(valid_lineups)
}

# Fast lineup scoring function (replaces LP solver)
score_lineup_fast <- function(lineup_row, sim_driver_results, sim_constructor_results) {
  total_points <- 0
  
  # Score captain (1.5x multiplier)
  captain_idx <- which(sim_driver_results$Name == lineup_row$Captain)
  if (length(captain_idx) > 0) {
    captain_points <- sim_driver_results$FantasyPoints[captain_idx[1]]
    total_points <- total_points + (captain_points * 1.5)
  }
  
  # Score regular drivers
 driver_names <- c(lineup_row$Driver1, lineup_row$Driver2, lineup_row$Driver3, lineup_row$Driver4)
  for (driver in driver_names) {
    driver_idx <- which(sim_driver_results$Name == driver)
    if (length(driver_idx) > 0) {
      driver_points <- sim_driver_results$FantasyPoints[driver_idx[1]]
      total_points <- total_points + driver_points
    }
  }
  
  # Score constructor
  constructor_idx <- which(sim_constructor_results$Name == lineup_row$Constructor)
  if (length(constructor_idx) > 0) {
    constructor_points <- sim_constructor_results$FantasyPoints[constructor_idx[1]]
    total_points <- total_points + constructor_points
  }
  
  return(total_points)
}

score_all_lineups_vectorized <- function(valid_lineups, sim_driver_results, sim_constructor_results) {
  n_lineups <- nrow(valid_lineups)
  
  # Create lookup vectors for faster access
  driver_points <- setNames(sim_driver_results$FantasyPoints, sim_driver_results$Name)
  constructor_points <- setNames(sim_constructor_results$FantasyPoints, sim_constructor_results$Name)
  
  # OPTIMIZATION: Vectorized lookup instead of loop
  # Extract all names as vectors
  captains <- valid_lineups$Captain
  driver1s <- valid_lineups$Driver1
  driver2s <- valid_lineups$Driver2
  driver3s <- valid_lineups$Driver3
  driver4s <- valid_lineups$Driver4  # INCLUDE Driver4
  constructors <- valid_lineups$Constructor
  
  # Vectorized scoring - all at once instead of loop
  captain_pts <- driver_points[captains] * 1.5
  driver1_pts <- driver_points[driver1s]
  driver2_pts <- driver_points[driver2s]
  driver3_pts <- driver_points[driver3s]
  driver4_pts <- driver_points[driver4s]  # INCLUDE Driver4
  constructor_pts <- constructor_points[constructors]
  
  # Handle NAs (replace with 0)
  captain_pts[is.na(captain_pts)] <- 0
  driver1_pts[is.na(driver1_pts)] <- 0
  driver2_pts[is.na(driver2_pts)] <- 0
  driver3_pts[is.na(driver3_pts)] <- 0
  driver4_pts[is.na(driver4_pts)] <- 0  # INCLUDE Driver4
  constructor_pts[is.na(constructor_pts)] <- 0
  
  # Total scores - vectorized addition (NOW WITH 6 PIECES)
  lineup_scores <- captain_pts + driver1_pts + driver2_pts + driver3_pts + driver4_pts + constructor_pts
  
  return(lineup_scores)
}

# ALSO: Add this progress-aware version for the lineup analysis
score_all_lineups_with_progress <- function(valid_lineups, sim_driver_results, sim_constructor_results, sim_id, total_sims) {
  # Print progress every 50 simulations
  if (sim_id %% 50 == 0) {
    cat("Scoring lineups for simulation", sim_id, "of", total_sims, "\n")
  }
  
  # Use the optimized scoring
  return(score_all_lineups_vectorized(valid_lineups, sim_driver_results, sim_constructor_results))
}

# REPLACE your entire simulate_f1_races function with this optimized version
simulate_f1_races <- function(driver_data,
                              constructor_data,
                              fl_probs,
                              ll_data,
                              classification_data,
                              total_race_laps = 60,
                              n_sims = 1000) {
  
  cat("Starting optimized simulation with", n_sims, "races...\n")
  start_time <- Sys.time()
  
  # Ensure we're working with data.tables
  if (!is.data.table(driver_data))
    driver_data <- as.data.table(driver_data)
  if (!is.data.table(constructor_data))
    constructor_data <- as.data.table(constructor_data)
  if (!is.null(fl_probs) && !is.data.table(fl_probs))
    fl_probs <- as.data.table(fl_probs)
  if (!is.null(ll_data) && !is.data.table(ll_data))
    ll_data <- as.data.table(ll_data)
  
  n_drivers <- nrow(driver_data)
  n_constructors <- nrow(constructor_data)
  
  # Create lookup tables for faster access
  lookup_tables <- create_lookup_tables(driver_data, constructor_data)
  cat("Created lookup tables\n")
  
  # Extract marginal probability columns (for positions 1-20)
  pos_cols <- as.character(1:20)
  if (!all(pos_cols %in% names(driver_data))) {
    stop("Required marginal probability columns (1-20) not found")
  }
  
  # Optimized batch processing
  chunk_size <- min(2000, max(500, ceiling(15000 / max(n_drivers, 1))))
  n_chunks <- ceiling(n_sims / chunk_size)
  
  cat("Processing", n_sims, "simulations in", n_chunks, "chunks of size", chunk_size, "\n")
  
  # Pre-allocate results storage
  all_results_matrices <- vector("list", n_chunks)
  successful_chunks <- 0
  
  for (chunk in 1:n_chunks) {
    chunk_start_time <- Sys.time()
    start_sim <- (chunk - 1) * chunk_size + 1
    end_sim <- min(chunk * chunk_size, n_sims)
    chunk_sims <- end_sim - start_sim + 1
    
    cat("Processing chunk", chunk, "of", n_chunks, "(simulations", start_sim, "to", end_sim, ")\n")
    
    tryCatch({
      # Use new vectorized simulation for this chunk
      chunk_results <- simulate_races_vectorized_chunk(
        driver_data, constructor_data, fl_probs, ll_data, 
        classification_data, total_race_laps, chunk_sims, lookup_tables
      )
      
      all_results_matrices[[chunk]] <- chunk_results
      successful_chunks <- successful_chunks + 1
      
      chunk_end_time <- Sys.time()
      chunk_duration <- as.numeric(difftime(chunk_end_time, chunk_start_time, units = "secs"))
      cat("Completed chunk", chunk, "in", round(chunk_duration, 2), "seconds\n")
      
      # Memory cleanup every few chunks
      if (chunk %% 3 == 0) {
        gc(verbose = FALSE)
      }
      
    }, error = function(e) {
      cat("Error in chunk", chunk, ":", e$message, "\n")
      all_results_matrices[[chunk]] <- NULL
    })
  }
  
  # Combine all chunk results into final matrices
  cat("Combining results from", successful_chunks, "successful chunks...\n")
  
  # Calculate total successful simulations
  total_successful_sims <- successful_chunks * chunk_size
  if (successful_chunks > 0 && n_chunks > 0) {
    # Adjust for last chunk which might be smaller
    last_chunk_size <- n_sims - (n_chunks - 1) * chunk_size
    if (last_chunk_size < chunk_size && successful_chunks == n_chunks) {
      total_successful_sims <- (successful_chunks - 1) * chunk_size + last_chunk_size
    }
  }
  
  # Initialize combined matrices
  combined_driver_matrix <- array(0, dim = c(total_successful_sims, n_drivers, 6))
  combined_constructor_matrix <- array(0, dim = c(total_successful_sims, n_constructors, 3))
  
  # Combine chunk results
  current_sim <- 1
  for (chunk in 1:n_chunks) {
    if (!is.null(all_results_matrices[[chunk]])) {
      chunk_results <- all_results_matrices[[chunk]]
      chunk_sims <- dim(chunk_results$drivers)[1]
      
      sim_range <- current_sim:(current_sim + chunk_sims - 1)
      combined_driver_matrix[sim_range, , ] <- chunk_results$drivers
      
      # Constructor results (simplified for now)
      if (!is.null(chunk_results$constructors)) {
        combined_constructor_matrix[sim_range, , ] <- chunk_results$constructors
      }
      
      current_sim <- current_sim + chunk_sims
    }
  }
  
  # Convert matrices back to data.table format for compatibility
  cat("Converting matrices to data.table format...\n")
  
  sim_ids <- 1:total_successful_sims
  final_results <- convert_matrices_to_display(
    list(drivers = combined_driver_matrix, constructors = combined_constructor_matrix),
    driver_data, constructor_data, sim_ids
  )
  
  cat("Calculating constructor fantasy points with detailed breakdown...\n")
  constructor_results <- final_results$constructor_results
  
  # Add the detailed breakdown columns
  constructor_results[, `:=`(
    PositionPoints = 0,
    BonusPoints = 0, 
    LapsLedPoints = 0,
    HasFastestLap = FALSE,
    FLPoints = 0,
    BothFinished = FALSE,
    BothInPoints = FALSE,
    BothOnPodium = FALSE
  )]
  
  # VECTORIZED approach - calculate all at once using data.table operations
  driver_results <- final_results$driver_results
  
  # Create constructor summary by SimID and Team using data.table aggregation
  constructor_summary <- driver_results[, .(
    PositionPoints = sum(lookup_tables$position_points[pmin(FinishPosition, 20)]),
    HasFastestLap = any(FastestLap),
    LapsLedPoints = sum(LapsLed) * 0.1,
    BothFinished = all(IsClassified),
    BothInPoints = all(FinishPosition <= 10),
    BothOnPodium = all(FinishPosition <= 3),
    DriverCount = .N
  ), by = .(SimID, Team)]
  
  # Calculate bonus points vectorized
  constructor_summary[, BonusPoints := 
                        ifelse(BothFinished, 2, 0) + 
                        ifelse(BothInPoints, 5, 0) + 
                        ifelse(BothOnPodium, 3, 0)]
  
  # Calculate FL points
  constructor_summary[, FLPoints := ifelse(HasFastestLap, 3, 0)]
  
  # Calculate total fantasy points
  constructor_summary[, FantasyPoints := PositionPoints + BonusPoints + LapsLedPoints + FLPoints]
  
  # Update constructor_results using efficient merge
  setkey(constructor_results, SimID, Name)
  setkey(constructor_summary, SimID, Team)
  
  # Match constructor results with constructor summary
  constructor_results[constructor_summary, on = c("SimID", "Name" = "Team"), `:=`(
    PositionPoints = i.PositionPoints,
    BonusPoints = i.BonusPoints,
    LapsLedPoints = i.LapsLedPoints,
    HasFastestLap = i.HasFastestLap,
    FLPoints = i.FLPoints,
    BothFinished = i.BothFinished,
    BothInPoints = i.BothInPoints,
    BothOnPodium = i.BothOnPodium,
    FantasyPoints = i.FantasyPoints
  )]
  
  cat("Constructor scoring completed using vectorized operations\n")
  
  # Final cleanup
  rm(all_results_matrices, combined_driver_matrix, combined_constructor_matrix)
  gc(verbose = FALSE, full = TRUE)
  
  end_time <- Sys.time()
  total_duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  cat("Simulation completed in", round(total_duration, 2), "seconds\n")
  cat("Successfully processed", total_successful_sims, "simulations\n")
  
  # Return results in same format as original function
  return(list(
    driver_results = final_results$driver_results,
    constructor_results = constructor_results,
    successful_sims = total_successful_sims
  ))
}

# =============================================================================
# VECTORIZED SIMULATION FUNCTIONS - Add after simulate_f1_races function
# =============================================================================

# Initialize efficient results matrices
initialize_results_matrices <- function(n_sims, n_drivers, n_constructors) {
  # Driver results matrix: [sim_id, driver_id, metric_id]
  # Metrics: 1=FinishPosition, 2=FastestLap, 3=LapsLed, 4=IsClassified, 5=DefeatedTeammate, 6=FantasyPoints
  driver_results_matrix <- array(0, dim = c(n_sims, n_drivers, 6))
  
  # Constructor results matrix: [sim_id, constructor_id, metric_id]  
  # Metrics: 1=PositionPoints, 2=BonusPoints, 3=FantasyPoints
  constructor_results_matrix <- array(0, dim = c(n_sims, n_constructors, 3))
  
  return(list(
    drivers = driver_results_matrix,
    constructors = constructor_results_matrix
  ))
}

# Vectorized fantasy points calculation
calculate_fantasy_points_vectorized <- function(finish_positions, starting_positions, 
                                                fastest_lap, laps_led, defeated_teammate, 
                                                is_classified, position_points) {
  # Position points
  pos_points <- position_points[pmin(finish_positions, 20)]
  
  # Differential points (vectorized)
  pos_diff <- starting_positions - finish_positions
  diff_points <- ifelse(pos_diff >= 10, 5,
                        ifelse(pos_diff >= 5, 3,
                               ifelse(pos_diff >= 3, 2,
                                      ifelse(pos_diff <= -10, -5,
                                             ifelse(pos_diff <= -5, -3,
                                                    ifelse(pos_diff <= -3, -2, 0))))))
  
  # Bonus points
  fl_points <- fastest_lap * 3
  ll_points <- laps_led * 0.1
  teammate_points <- defeated_teammate * 5
  classified_points <- is_classified * 1
  
  # Total fantasy points
  total_points <- pos_points + diff_points + fl_points + ll_points + teammate_points + classified_points
  
  return(total_points)
}

# Simulate a chunk of races using vectorized operations
simulate_races_vectorized_chunk <- function(driver_data, constructor_data, 
                                            fl_probs, ll_data, classification_data,
                                            total_race_laps, chunk_sims, 
                                            lookup_tables) {
  
  n_drivers <- nrow(driver_data)
  n_constructors <- nrow(constructor_data)
  
  # Extract probability matrix for positions 1-20
  pos_cols <- as.character(1:20)
  prob_matrix <- as.matrix(driver_data[, pos_cols, with = FALSE])
  
  # Initialize results for this chunk
  results <- initialize_results_matrices(chunk_sims, n_drivers, n_constructors)
  
  # VECTORIZED POSITION SIMULATION
  all_positions <- matrix(0, nrow = chunk_sims, ncol = n_drivers)
  
  # Sample positions for each driver across all simulations
  for (driver_idx in 1:n_drivers) {
    driver_probs <- prob_matrix[driver_idx, ]
    
    # Handle NAs and normalize
    if (any(is.na(driver_probs))) driver_probs[is.na(driver_probs)] <- 0
    if (sum(driver_probs) > 0) {
      driver_probs <- driver_probs / sum(driver_probs)
    } else {
      driver_probs <- rep(1/20, 20)
    }
    
    # Sample for all simulations at once
    all_positions[, driver_idx] <- sample(1:20, chunk_sims, replace = TRUE, prob = driver_probs)
  }
  
  # Add noise and rank
  noise_matrix <- matrix(runif(chunk_sims * n_drivers, 0, 0.1), nrow = chunk_sims)
  performance_matrix <- all_positions + noise_matrix
  
  # Get finishing positions for all simulations
  finish_positions <- t(apply(performance_matrix, 1, rank, ties.method = "random"))
  
  # Store finishing positions
  results$drivers[, , 1] <- finish_positions
  
  # VECTORIZED FASTEST LAP ASSIGNMENT
  fastest_lap_matrix <- matrix(FALSE, nrow = chunk_sims, ncol = n_drivers)
  
  if (!is.null(fl_probs) && nrow(fl_probs) > 0) {
    # Pre-compute FL probabilities by position
    fl_position_probs <- numeric(20)
    for (i in 1:nrow(fl_probs)) {
      pos <- fl_probs$Finish[i]
      if (pos >= 1 && pos <= 20) {
        fl_position_probs[pos] <- fl_probs$Prob[i]
      }
    }
    
    if (sum(fl_position_probs) > 0) {
      fl_position_probs <- fl_position_probs / sum(fl_position_probs)
      
      # Assign FL for each simulation
      for (sim in 1:chunk_sims) {
        sim_positions <- finish_positions[sim, ]
        driver_fl_probs <- fl_position_probs[sim_positions]
        driver_fl_probs[is.na(driver_fl_probs)] <- 0
        
        if (sum(driver_fl_probs) > 0) {
          fl_driver <- sample(1:n_drivers, 1, prob = driver_fl_probs)
          fastest_lap_matrix[sim, fl_driver] <- TRUE
        }
      }
    }
  } else {
    # Fallback: random driver in top 8
    for (sim in 1:chunk_sims) {
      top8 <- which(finish_positions[sim, ] <= 8)
      if (length(top8) > 0) {
        fl_driver <- sample(top8, 1)
        fastest_lap_matrix[sim, fl_driver] <- TRUE
      }
    }
  }
  
  # Store fastest lap results
  results$drivers[, , 2] <- as.numeric(fastest_lap_matrix)
  
  # SIMPLIFIED LAPS LED (for speed)
  laps_led_matrix <- matrix(0L, nrow = chunk_sims, ncol = n_drivers)
  
  for (sim in 1:chunk_sims) {
    winner_idx <- which(finish_positions[sim, ] == 1)[1]
    if (length(winner_idx) > 0) {
      # Simplified: winner gets 60-80% of laps, 2nd place gets the rest
      winner_laps <- round(total_race_laps * runif(1, 0.6, 0.8))
      laps_led_matrix[sim, winner_idx] <- winner_laps
      
      # Give remaining laps to 2nd place
      second_idx <- which(finish_positions[sim, ] == 2)[1]
      if (length(second_idx) > 0) {
        laps_led_matrix[sim, second_idx] <- total_race_laps - winner_laps
      }
    }
  }
  
  # Store laps led
  results$drivers[, , 3] <- laps_led_matrix
  
  # CLASSIFICATION (simplified)
  classified_matrix <- matrix(TRUE, nrow = chunk_sims, ncol = n_drivers)
  
  if (!is.null(classification_data) && nrow(classification_data) > 0) {
    for (sim in 1:chunk_sims) {
      n_classified <- sample(classification_data$NumClassified, 1, prob = classification_data$Probability)
      if (n_classified < n_drivers) {
        worst_finishers <- order(finish_positions[sim, ], decreasing = TRUE)[1:(n_drivers - n_classified)]
        classified_matrix[sim, worst_finishers] <- FALSE
      }
    }
  }
  
  # Store classification
  results$drivers[, , 4] <- as.numeric(classified_matrix)
  
  # TEAMMATE DEFEAT (vectorized using lookup tables)
  defeated_teammate_matrix <- matrix(FALSE, nrow = chunk_sims, ncol = n_drivers)
  
  for (sim in 1:chunk_sims) {
    for (team_indices in lookup_tables$team_groups) {
      if (length(team_indices) > 1) {
        team_positions <- finish_positions[sim, team_indices]
        best_pos <- min(team_positions)
        best_drivers <- team_indices[team_positions == best_pos]
        defeated_teammate_matrix[sim, best_drivers] <- TRUE
      }
    }
  }
  
  # Store teammate defeat
  results$drivers[, , 5] <- as.numeric(defeated_teammate_matrix)
  
  # CALCULATE FANTASY POINTS (vectorized)
  starting_positions <- rep(driver_data$Start, each = chunk_sims)
  starting_matrix <- matrix(starting_positions, nrow = chunk_sims, ncol = n_drivers, byrow = TRUE)
  
  fantasy_points_matrix <- matrix(0, nrow = chunk_sims, ncol = n_drivers)
  
  for (sim in 1:chunk_sims) {
    fantasy_points_matrix[sim, ] <- calculate_fantasy_points_vectorized(
      finish_positions[sim, ],
      starting_matrix[sim, ],
      fastest_lap_matrix[sim, ],
      laps_led_matrix[sim, ],
      defeated_teammate_matrix[sim, ],
      classified_matrix[sim, ],
      lookup_tables$position_points
    )
  }
  
  # Store fantasy points
  results$drivers[, , 6] <- fantasy_points_matrix
  
  # CONSTRUCTOR SCORING (simplified for now)
  # This can be optimized further later
  
  return(results)
}



find_top_lineups <- function(sim_drivers,
                             sim_constructors,
                             n_top = 1,
                             salary_cap = 50000,
                             valid_lineups = NULL) {
  
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
  required_driver_cols <- c("DFSID", "Name", "Team", "DriverSalary", "CaptainSalary", "FantasyPoints")
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
  
  # If no pre-computed lineups provided, create them for this simulation
  if (is.null(valid_lineups)) {
    cat("No pre-computed lineups provided. Creating them for this simulation...\n")
    
    # Create temporary data for pre-computation
    temp_drivers <- sim_drivers[, .(Name, DriverSalary, CaptainSalary, Team)]
    setnames(temp_drivers, c("DriverSalary", "CaptainSalary"), c("DriverSal", "CptSal"))
    temp_drivers[, DFSID := 1:.N]
    
    temp_constructors <- sim_constructors[, .(Name, Salary)]
    temp_constructors[, DFSID := 1:.N]
    
    valid_lineups <- precompute_valid_lineups(temp_drivers, temp_constructors, salary_cap)
  }
  
  cat("Using", nrow(valid_lineups), "valid lineups for optimization\n")
  
  # Score all valid lineups for this simulation using fast vectorized scoring
  lineup_scores <- score_all_lineups_vectorized(valid_lineups, sim_drivers, sim_constructors)
  
  # Handle case where scoring failed
  if (length(lineup_scores) == 0 || all(is.na(lineup_scores))) {
    cat("Warning: All lineup scores are NA or empty\n")
    return(NULL)
  }
  
  # Find the top N lineups
  n_top <- min(n_top, length(lineup_scores))
  top_indices <- order(lineup_scores, decreasing = TRUE)[1:n_top]
  
  # Remove any NA indices
  top_indices <- top_indices[!is.na(top_indices)]
  
  if (length(top_indices) == 0) {
    cat("Warning: No valid top lineups found\n")
    return(NULL)
  }
  
  # Create result list
  result_lineups <- list()
  
  for (i in 1:length(top_indices)) {
    lineup_idx <- top_indices[i]
    lineup_row <- valid_lineups[lineup_idx]
    
    # Create lineup object in expected format (NOW WITH 4 DRIVERS)
    lineup <- list(
      sim_id = if ("SimID" %in% names(sim_drivers)) sim_drivers$SimID[1] else NA,
      drivers = c(lineup_row$Driver1, lineup_row$Driver2, lineup_row$Driver3, lineup_row$Driver4),
      captain = lineup_row$Captain,
      constructor = lineup_row$Constructor,
      total_salary = lineup_row$TotalSalary,
      total_points = lineup_scores[lineup_idx]
    )
    
    result_lineups[[i]] <- lineup
  }
  
  return(result_lineups)
}

# Optimized version specifically for single simulation optimization
find_optimal_lineup_fast <- function(sim_drivers, sim_constructors, salary_cap = 50000, valid_lineups = NULL) {
  # This is a simplified version that just finds the single best lineup
  top_lineups <- find_top_lineups(sim_drivers, sim_constructors, n_top = 1, salary_cap, valid_lineups)
  
  if (!is.null(top_lineups) && length(top_lineups) > 0) {
    return(top_lineups[[1]])
  } else {
    return(NULL)
  }
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
                                max_sims = NULL,
                                valid_lineups = NULL) {
  
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
  
  cat("Starting optimized lineup analysis across", length(sim_ids), "simulations...\n")
  
  # Check if we have pre-computed valid lineups
  if (is.null(valid_lineups)) {
    cat("No pre-computed lineups provided. Creating them now...\n")
    
    # Extract unique driver and constructor data for pre-computation
    unique_drivers <- unique(driver_results[, .(Name, DriverSalary, CaptainSalary, Team)])
    setnames(unique_drivers, c("DriverSalary", "CaptainSalary"), c("DriverSal", "CptSal"))
    
    unique_constructors <- unique(constructor_results[, .(Name, Salary)])
    
    # Create temporary DFSID if missing
    if (!"DFSID" %in% names(unique_drivers)) {
      unique_drivers[, DFSID := 1:.N]
    }
    if (!"DFSID" %in% names(unique_constructors)) {
      unique_constructors[, DFSID := 1:.N]
    }
    
    # Pre-compute valid lineups
    valid_lineups <- precompute_valid_lineups(unique_drivers, unique_constructors)
  }
  
  cat("Using", nrow(valid_lineups), "pre-computed valid lineups\n")
  
  # Track which lineup is optimal for each simulation
  optimal_lineup_ids <- integer(length(sim_ids))
  successful_sims <- 0
  
  # Process simulations in batches for memory efficiency
  batch_size <- min(1000, max(200, ceiling(length(sim_ids) / 10)))
  n_batches <- ceiling(length(sim_ids) / batch_size)
  
  cat("Processing in", n_batches, "batches of size", batch_size, "\n")
  
  for (batch in 1:n_batches) {
    batch_start_time <- Sys.time()
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, length(sim_ids))
    batch_sims <- sim_ids[start_idx:end_idx]
    
    cat("Processing batch", batch, "of", n_batches, "(simulations", start_idx, "to", end_idx, ")\n")
    
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
      
      # Find optimal lineup for this simulation using fast scoring
      tryCatch({
        # Use new vectorized scoring instead of LP solver
        lineup_scores <- score_all_lineups_vectorized(valid_lineups, sim_drivers, sim_constructors)
        
        if (length(lineup_scores) > 0 && any(!is.na(lineup_scores))) {
          # Find the best lineup
          best_lineup_idx <- which.max(lineup_scores)
          
          if (length(best_lineup_idx) > 0) {
            optimal_lineup_ids[start_idx + i - 1] <- valid_lineups$LineupID[best_lineup_idx]
            successful_sims <- successful_sims + 1
          }
        }
        
      }, error = function(e) {
        # Skip this simulation if error occurs
        cat("Error processing simulation", sim_id, ":", e$message, "\n")
      })
    }
    
    batch_end_time <- Sys.time()
    batch_duration <- as.numeric(difftime(batch_end_time, batch_start_time, units = "secs"))
    cat("Completed batch", batch, "in", round(batch_duration, 2), "seconds\n")
    
    # Aggressive garbage collection after each batch
    rm(batch_drivers, batch_constructors)
    gc(verbose = FALSE)
  }
  
  # Remove entries where no optimal lineup was found
  valid_indices <- which(optimal_lineup_ids > 0)
  if (length(valid_indices) == 0) {
    cat("No valid optimal lineups found\n")
    return(NULL)
  }
  
  optimal_lineup_ids <- optimal_lineup_ids[valid_indices]
  
  # Count frequency of each optimal lineup
  lineup_counts <- table(optimal_lineup_ids)
  unique_lineup_ids <- as.numeric(names(lineup_counts))
  counts <- as.numeric(lineup_counts)
  
  # Get lineup details for the optimal lineups
  optimal_lineup_details <- valid_lineups[LineupID %in% unique_lineup_ids]
  
  # Add count information
  optimal_lineup_details$OptimalCount <- counts[match(optimal_lineup_details$LineupID, unique_lineup_ids)]
  
  # Sort by count (descending)
  setorder(optimal_lineup_details, -OptimalCount)
  
  # Create results data frame in the expected format
  results <- data.frame(
    OptimalCount = optimal_lineup_details$OptimalCount,
    Captain = optimal_lineup_details$Captain,
    Driver1 = optimal_lineup_details$Driver1,
    Driver2 = optimal_lineup_details$Driver2,
    Driver3 = optimal_lineup_details$Driver3,
    Driver4 = optimal_lineup_details$Driver4,  # ADD THIS LINE
    Constructor = optimal_lineup_details$Constructor,
    TotalSalary = optimal_lineup_details$TotalSalary,
    stringsAsFactors = FALSE
  )
  
  end_time <- Sys.time()
  total_duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  cat("Analysis completed in", round(total_duration, 2), "seconds\n")
  cat("Found", nrow(results), "unique lineups from", successful_sims, "successful simulations\n")
  cat("Average", round(successful_sims / length(sim_ids) * 100, 1), "% success rate\n")
  
  return(list(
    lineups = results, 
    total_processed = successful_sims
  ))
}



# =============================================================================
# MATRIX CONVERSION FUNCTIONS - Add after analysis functions
# =============================================================================

# Convert results matrices back to data.table format for display/analysis
convert_matrices_to_display <- function(results_matrices, driver_data, constructor_data, sim_ids = NULL) {
  
  n_sims <- dim(results_matrices$drivers)[1]
  n_drivers <- dim(results_matrices$drivers)[2]
  
  if (is.null(sim_ids)) {
    sim_ids <- 1:n_sims
  }
  
  # Create driver results data.table
  driver_results_list <- list()
  
  for (sim in 1:n_sims) {
    for (driver in 1:n_drivers) {
      driver_results_list[[length(driver_results_list) + 1]] <- list(
        SimID = sim_ids[sim],
        DFSID = driver_data$DFSID[driver],
        Name = driver_data$Name[driver],
        Team = driver_data$Team[driver],
        FinishPosition = results_matrices$drivers[sim, driver, 1],
        Starting = driver_data$Start[driver],
        DriverSalary = driver_data$DriverSal[driver],
        CaptainSalary = driver_data$CptSal[driver],
        FastestLap = as.logical(results_matrices$drivers[sim, driver, 2]),
        LapsLed = results_matrices$drivers[sim, driver, 3],
        IsClassified = as.logical(results_matrices$drivers[sim, driver, 4]),
        DefeatedTeammate = as.logical(results_matrices$drivers[sim, driver, 5]),
        FantasyPoints = results_matrices$drivers[sim, driver, 6]
      )
    }
  }
  
  # Convert to data.table
  driver_results <- rbindlist(driver_results_list)
  
  # Create constructor results (simplified for now)
  constructor_results <- data.table(
    SimID = rep(sim_ids, each = nrow(constructor_data)),
    DFSID = rep(constructor_data$DFSID, n_sims),
    Name = rep(constructor_data$Name, n_sims),
    Salary = rep(constructor_data$Salary, n_sims),
    FantasyPoints = 0  # Will be calculated properly later
  )
  
  return(list(
    driver_results = driver_results,
    constructor_results = constructor_results
  ))
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

# REPLACE your entire generate_random_lineups function with this optimized version
generate_random_lineups <- function(optimal_lineups, filters) {
  # Convert to data.table for faster operations
  if (!is.data.table(optimal_lineups)) {
    filtered_dt <- as.data.table(optimal_lineups)
  } else {
    filtered_dt <- copy(optimal_lineups)
  }
  
  # Early exit if no data
  if (nrow(filtered_dt) == 0) {
    return(NULL)
  }
  
  # OPTIMIZED FILTERING using data.table operations
  
  # Apply OptimalCount filter
  if (!is.null(filters$min_top1_count) && filters$min_top1_count > 0) {
    if ("OptimalCount" %in% names(filtered_dt)) {
      filtered_dt <- filtered_dt[OptimalCount >= filters$min_top1_count]
    }
  }
  
  # Early exit if filtered out everything
  if (nrow(filtered_dt) == 0) {
    return(NULL)
  }
  
  # OPTIMIZED DRIVER EXCLUSION using vectorized operations
  if (!is.null(filters$excluded_drivers) && length(filters$excluded_drivers) > 0) {
    # Get all driver columns
    driver_cols <- grep("^Driver", names(filtered_dt), value = TRUE)
    
    if (length(driver_cols) > 0) {
      # Create a logical vector for rows to keep
      keep_rows <- rep(TRUE, nrow(filtered_dt))
      
      # For each driver column, mark rows to exclude
      for (col in driver_cols) {
        exclude_mask <- filtered_dt[[col]] %in% filters$excluded_drivers
        keep_rows <- keep_rows & !exclude_mask
      }
      
      # Apply filter
      filtered_dt <- filtered_dt[keep_rows]
    }
  }
  
  # Early exit check
  if (nrow(filtered_dt) == 0) {
    return(NULL)
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
  
  # Final exit check
  if (nrow(filtered_dt) == 0) {
    return(NULL)
  }
  
  # OPTIMIZED LINEUP GENERATION
  n_lineups <- filters$num_lineups
  
  # Handle case where we have fewer lineups than requested
  if (nrow(filtered_dt) <= n_lineups) {
    cat("Returning all", nrow(filtered_dt), "available lineups (requested", n_lineups, ")\n")
    return(filtered_dt)
  }
  
  # EFFICIENT WEIGHTED SAMPLING
  weights <- filtered_dt$OptimalCount
  
  # Handle missing weights
  if (is.null(weights) || all(is.na(weights))) {
    weights <- rep(1, nrow(filtered_dt))
  }
  
  # Ensure weights are positive
  weights[is.na(weights)] <- 0
  weights[weights < 0] <- 0
  
  # If all weights are zero, use uniform sampling
  if (sum(weights) == 0) {
    weights <- rep(1, nrow(filtered_dt))
  }
  
  # Efficient sampling without replacement
  selected_indices <- sample(1:nrow(filtered_dt), n_lineups, replace = FALSE, prob = weights)
  
  # Extract selected lineups efficiently
  selected_lineups <- filtered_dt[selected_indices]
  
  cat("Generated", nrow(selected_lineups), "random lineups from", nrow(filtered_dt), "available lineups\n")
  
  return(selected_lineups)
}


# REPLACE your entire calculate_driver_exposure function with this optimized version
calculate_driver_exposure <- function(optimal_lineups,
                                      driver_results,
                                      random_lineups) {
  
  # Convert inputs to data.table for efficiency
  if (!is.data.table(optimal_lineups))
    setDT(optimal_lineups)
  if (!is.data.table(driver_results))
    setDT(driver_results)
  if (!is.null(random_lineups) && !is.data.table(random_lineups))
    setDT(random_lineups)
  
  # Guard against NULL input
  if (is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame(Message = "No optimal lineups available."))
  }
  
  # Get all driver columns efficiently - ENSURE WE GET ALL 4 DRIVERS
  driver_cols <- grep("^Driver", names(optimal_lineups), value = TRUE)
  captain_col <- if ("Captain" %in% names(optimal_lineups)) "Captain" else NULL
  
  cat("Found driver columns:", paste(driver_cols, collapse = ", "), "\n")
  cat("Expected: Driver1, Driver2, Driver3, Driver4\n")
  
  # Extract all unique drivers using efficient approach
  all_drivers_list <- list()
  
  # Get drivers from driver columns
  if (length(driver_cols) > 0) {
    for (col in driver_cols) {
      all_drivers_list[[col]] <- optimal_lineups[[col]]
    }
  }
  
  # Get captains
  if (!is.null(captain_col)) {
    all_drivers_list[["captains"]] <- optimal_lineups[[captain_col]]
  }
  
  # Combine and clean
  all_drivers <- unique(unlist(all_drivers_list))
  all_drivers <- all_drivers[!is.na(all_drivers) & all_drivers != ""]
  
  if (length(all_drivers) == 0) {
    return(data.frame(Message = "No valid drivers found in lineups."))
  }
  
  # Pre-allocate result data frame for better performance
  exposure_data <- data.frame(
    Driver = all_drivers,
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
  
  # Create lookup tables for faster access
  driver_lookup <- setNames(1:length(all_drivers), all_drivers)
  
  # Calculate total count for optimal percentages
  total_optimal_count <- sum(optimal_lineups$OptimalCount, na.rm = TRUE)
  if (total_optimal_count == 0) {
    total_optimal_count <- nrow(optimal_lineups)
  }
  
  # OPTIMIZED OPTIMAL EXPOSURE CALCULATION
  cat("Calculating optimal exposure for", length(all_drivers), "drivers...\n")
  
  # Pre-create logical matrices for faster lookups
  n_lineups <- nrow(optimal_lineups)
  n_drivers <- length(all_drivers)
  
  # Create matrices for driver and captain appearances
  driver_matrix <- matrix(FALSE, nrow = n_lineups, ncol = n_drivers)
  captain_matrix <- matrix(FALSE, nrow = n_lineups, ncol = n_drivers)
  
  # Fill driver matrix (vectorized approach)
  if (length(driver_cols) > 0) {
    for (col in driver_cols) {
      for (i in 1:n_lineups) {
        driver_name <- optimal_lineups[[col]][i]
        if (!is.na(driver_name) && driver_name %in% all_drivers) {
          driver_idx <- driver_lookup[[driver_name]]
          driver_matrix[i, driver_idx] <- TRUE
        }
      }
    }
  }
  
  # Fill captain matrix
  if (!is.null(captain_col)) {
    for (i in 1:n_lineups) {
      captain_name <- optimal_lineups[[captain_col]][i]
      if (!is.na(captain_name) && captain_name %in% all_drivers) {
        captain_idx <- driver_lookup[[captain_name]]
        captain_matrix[i, captain_idx] <- TRUE
      }
    }
  }
  
  # Calculate exposures using matrix operations
  lineup_weights <- optimal_lineups$OptimalCount
  if (is.null(lineup_weights)) {
    lineup_weights <- rep(1, n_lineups)
  }
  
  # Vectorized exposure calculation
  for (i in 1:n_drivers) {
    driver_name <- all_drivers[i]
    
    # Driver optimal exposure
    driver_appearances <- driver_matrix[, i]
    if (any(driver_appearances)) {
      driver_weight <- sum(lineup_weights[driver_appearances])
      exposure_data$DriverOptimal[i] <- (driver_weight / total_optimal_count) * 100
    }
    
    # Captain optimal exposure
    captain_appearances <- captain_matrix[, i]
    if (any(captain_appearances)) {
      captain_weight <- sum(lineup_weights[captain_appearances])
      exposure_data$CaptainOptimal[i] <- (captain_weight / total_optimal_count) * 100
    }
    
    # Total optimal exposure
    exposure_data$Optimal[i] <- exposure_data$DriverOptimal[i] + exposure_data$CaptainOptimal[i]
  }
  
  # OPTIMIZED RANDOM LINEUPS EXPOSURE
  if (!is.null(random_lineups) && nrow(random_lineups) > 0) {
    cat("Calculating random exposure...\n")
    
    # Get driver columns from random lineups
    random_driver_cols <- grep("^Driver", names(random_lineups), value = TRUE)
    random_captain_col <- if ("Captain" %in% names(random_lineups)) "Captain" else NULL
    
    total_random_lineups <- nrow(random_lineups)
    
    # Create matrices for random lineups
    random_driver_matrix <- matrix(FALSE, nrow = total_random_lineups, ncol = n_drivers)
    random_captain_matrix <- matrix(FALSE, nrow = total_random_lineups, ncol = n_drivers)
    
    # Fill random driver matrix
    if (length(random_driver_cols) > 0) {
      for (col in random_driver_cols) {
        for (i in 1:total_random_lineups) {
          driver_name <- random_lineups[[col]][i]
          if (!is.na(driver_name) && driver_name %in% all_drivers) {
            driver_idx <- driver_lookup[[driver_name]]
            random_driver_matrix[i, driver_idx] <- TRUE
          }
        }
      }
    }
    
    # Fill random captain matrix
    if (!is.null(random_captain_col)) {
      for (i in 1:total_random_lineups) {
        captain_name <- random_lineups[[random_captain_col]][i]
        if (!is.na(captain_name) && captain_name %in% all_drivers) {
          captain_idx <- driver_lookup[[captain_name]]
          random_captain_matrix[i, captain_idx] <- TRUE
        }
      }
    }
    
    # Calculate random exposures
    for (i in 1:n_drivers) {
      # Driver exposure
      driver_count <- sum(random_driver_matrix[, i])
      exposure_data$DriverExposure[i] <- (driver_count / total_random_lineups) * 100
      
      # Captain exposure
      captain_count <- sum(random_captain_matrix[, i])
      exposure_data$CaptainExposure[i] <- (captain_count / total_random_lineups) * 100
      
      # Total exposure
      exposure_data$Exposure[i] <- exposure_data$DriverExposure[i] + exposure_data$CaptainExposure[i]
    }
  }
  
  # ADD DRIVER INFO using efficient lookup
  if (!is.null(driver_results) && nrow(driver_results) > 0) {
    cat("Adding driver salary information...\n")
    
    # Create efficient lookup tables for driver info
    unique_drivers <- unique(driver_results[, .(Name, DriverSalary, CaptainSalary)])
    driver_salary_lookup <- setNames(unique_drivers$DriverSalary, unique_drivers$Name)
    captain_salary_lookup <- setNames(unique_drivers$CaptainSalary, unique_drivers$Name)
    
    # Vectorized salary assignment
    exposure_data$DriverSalary <- driver_salary_lookup[exposure_data$Driver]
    exposure_data$CaptainSalary <- captain_salary_lookup[exposure_data$Driver]
    
    # Handle NAs
    exposure_data$DriverSalary[is.na(exposure_data$DriverSalary)] <- 0
    exposure_data$CaptainSalary[is.na(exposure_data$CaptainSalary)] <- 0
  }
  
  # Sort by Optimal exposure (descending)
  exposure_data <- exposure_data[order(-exposure_data$Optimal), ]
  
  cat("Driver exposure calculation completed for", nrow(exposure_data), "drivers\n")
  
  return(exposure_data)
}


# =============================================================================
# UI PROGRESS FUNCTIONS - ADD THESE after your analysis functions (around line 1000)
# =============================================================================

# Enhanced progress tracking with time estimates
update_progress_detailed <- function(current, total, start_time, detail_msg = "") {
  progress_pct <- current / total
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  if (current > 0) {
    estimated_total_time <- elapsed_time / progress_pct
    remaining_time <- estimated_total_time - elapsed_time
    
    time_msg <- if (remaining_time > 60) {
      paste0("~", round(remaining_time / 60, 1), " min remaining")
    } else {
      paste0("~", round(remaining_time, 0), " sec remaining")
    }
    
    full_detail <- paste0(detail_msg, " (", time_msg, ")")
  } else {
    full_detail <- detail_msg
  }
  
  incProgress(amount = 0, detail = full_detail)
}

# Memory usage monitoring
get_memory_usage <- function() {
  # Get current R memory usage
  mem_info <- gc(verbose = FALSE)
  used_mb <- sum(mem_info[, 2]) # Vcells + Ncells used
  return(round(used_mb, 1))
}

# Progress tracker for simulation phases
create_progress_tracker <- function() {
  list(
    start_time = Sys.time(),
    phase_times = list(),
    memory_snapshots = list()
  )
}

# Log phase completion
log_phase_completion <- function(tracker, phase_name) {
  current_time <- Sys.time()
  elapsed <- as.numeric(difftime(current_time, tracker$start_time, units = "secs"))
  memory_used <- get_memory_usage()
  
  tracker$phase_times[[phase_name]] <- elapsed
  tracker$memory_snapshots[[phase_name]] <- memory_used
  
  cat(sprintf("Phase '%s' completed in %.2f seconds (Memory: %.1f MB)\n", 
              phase_name, elapsed, memory_used))
  
  return(tracker)
}

# Estimate remaining time based on current progress
estimate_remaining_time <- function(current_sim, total_sims, start_time) {
  if (current_sim <= 0) return("Calculating...")
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  rate <- current_sim / elapsed  # sims per second
  remaining_sims <- total_sims - current_sim
  remaining_time <- remaining_sims / rate
  
  if (remaining_time > 3600) {
    return(paste0("~", round(remaining_time / 3600, 1), " hours"))
  } else if (remaining_time > 60) {
    return(paste0("~", round(remaining_time / 60, 1), " minutes"))
  } else {
    return(paste0("~", round(remaining_time, 0), " seconds"))
  }
}


# UI Definition
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "F1 Fantasy Simulator"),
  
  dashboardSidebar(
    useShinyjs(),
    div(
      style = "text-align: center; padding: 10px; margin-bottom: 5px;",
      tags$img(src = "logo.jpg", height = "200px", width = "auto", 
               style = "border: 2px solid #FFD700; border-radius: 10px;")
    ),
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
    valid_lineups = NULL,        # ADD THIS LINE
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
  
  # REPLACE your observeEvent(input$run_sim) section with this optimized version
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
    rv$valid_lineups <- NULL  # Clear previous valid lineups
    
    # Force aggressive garbage collection
    gc(verbose = FALSE, full = TRUE)
    
    # Create progress tracker
    progress_tracker <- create_progress_tracker()
    
    withProgress(message = 'Running optimized simulations...', value = 0, {
      
      # STEP 1: Pre-compute valid lineups (one-time cost)
      incProgress(0.05, detail = "Pre-computing valid lineups...")
      progress_tracker <- log_phase_completion(progress_tracker, "start")
      
      tryCatch({
        rv$valid_lineups <- precompute_valid_lineups(
          rv$input_data$drivers,
          rv$input_data$constructors
        )
        progress_tracker <- log_phase_completion(progress_tracker, "precomputation")
        
        cat("Successfully pre-computed", nrow(rv$valid_lineups), "valid lineups\n")
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Pre-computation Error",
          paste("Error during lineup pre-computation:", e$message),
          easyClose = TRUE
        ))
        return()
      })
      
      # STEP 2: Run simulations
      incProgress(0.25, detail = "Running race simulations...")
      
      tryCatch({
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
        progress_tracker <- log_phase_completion(progress_tracker, "simulation")
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Simulation Error", 
          paste("Error during race simulation:", e$message),
          easyClose = TRUE
        ))
        return()
      })
      
      # STEP 3: Perform driver/constructor analysis
      incProgress(0.65, detail = "Analyzing driver performance...")
      
      tryCatch({
        rv$driver_analysis <- analyze_f1_drivers(sim_results$driver_results)
        rv$constructor_analysis <- analyze_f1_constructors(sim_results$constructor_results)
        progress_tracker <- log_phase_completion(progress_tracker, "basic_analysis")
        
      }, error = function(e) {
        cat("Warning: Error in basic analysis:", e$message, "\n")
        # Continue even if this fails
      })
      
      # STEP 4: Optimal lineup analysis (using pre-computed lineups)
      incProgress(0.85, detail = "Analyzing optimal lineups...")
      
      tryCatch({
        # Use the optimized lineup analysis with pre-computed lineups
        lineup_analysis <- analyze_top_lineups(
          rv$simulation_results$driver_results,
          rv$simulation_results$constructor_results,
          max_sims = NULL,  # Analyze all simulations
          valid_lineups = rv$valid_lineups  # Pass pre-computed lineups
        )
        
        if (!is.null(lineup_analysis) && !is.null(lineup_analysis$lineups)) {
          rv$simulation_results$optimal_lineup_frequency <- lineup_analysis$lineups
          rv$simulation_results$optimal_lineup_details <- lineup_analysis
          progress_tracker <- log_phase_completion(progress_tracker, "lineup_analysis")
        } else {
          cat("Warning: Lineup analysis returned NULL results\n")
        }
        
      }, error = function(e) {
        cat("Warning: Error in lineup analysis:", e$message, "\n")
        showModal(modalDialog(
          title = "Lineup Analysis Warning",
          paste("Lineup analysis encountered an error but simulations completed successfully:", e$message),
          easyClose = TRUE
        ))
      })
      
      # STEP 5: Final cleanup and UI updates
      incProgress(0.98, detail = "Finalizing results...")
      
      # Set completion flags
      rv$simulation_run <- TRUE
      progress_tracker <- log_phase_completion(progress_tracker, "completion")
      
      # Print summary statistics
      total_time <- as.numeric(difftime(Sys.time(), progress_tracker$start_time, units = "secs"))
      cat("\n=== SIMULATION SUMMARY ===\n")
      cat("Total runtime:", round(total_time, 2), "seconds\n")
      cat("Simulations completed:", rv$simulation_results$successful_sims, "of", input$n_sims, "\n")
      cat("Valid lineups found:", nrow(rv$valid_lineups), "\n")
      if (!is.null(rv$simulation_results$optimal_lineup_frequency)) {
        cat("Unique optimal lineups:", nrow(rv$simulation_results$optimal_lineup_frequency), "\n")
      }
      cat("Final memory usage:", get_memory_usage(), "MB\n")
      cat("========================\n")
      
      # Update UI to show results
      updateTabItems(session, "sidebar", selected = "driver_analysis")
      
      # Update dropdown options for lineup builder
      if (!is.null(rv$input_data$drivers)) {
        updateSelectizeInput(session,
                             "excluded_drivers",
                             choices = rv$input_data$drivers$Name)
        
        updateSelectizeInput(session,
                             "excluded_captains", 
                             choices = rv$input_data$drivers$Name)
      }
      
      if (!is.null(rv$input_data$constructors)) {
        updateSelectizeInput(session,
                             "excluded_constructors",
                             choices = rv$input_data$constructors$Name)
      }
      
      # Final progress update
      incProgress(0.02, detail = "Complete!")
      
      # Final garbage collection
      gc(verbose = FALSE)
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
  
  
  output$optimal_lineup_frequency <- renderDT({
    req(rv$simulation_results$optimal_lineup_frequency)
    
    # Make sure we're working with a data.frame, not a data.table
    lineup_data <- rv$simulation_results$optimal_lineup_frequency
    if (is.data.table(lineup_data)) {
      lineup_data <- as.data.frame(lineup_data)
    }
    
    # Ensure all 4 driver columns exist
    for (i in 1:4) {
      driver_col <- paste0("Driver", i)
      if (!(driver_col %in% names(lineup_data))) {
        lineup_data[[driver_col]] <- NA
        cat("Warning: Missing column", driver_col, "- added with NA values\n")
      }
    }
    
    # Define the column order to display (6-piece lineup)
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
        order = list(list(7, 'desc')),  # Sort by OptimalCount
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