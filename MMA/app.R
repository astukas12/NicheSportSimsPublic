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


cached_read_excel <- memoise(read_excel)

# =============================================
# HELPER FUNCTIONS
# =============================================

# Add this function to your code
optimize_datatable <- function(dt, page_length = 25, class = "compact") {
  options <- list(
    pageLength = page_length,
    deferRender = TRUE,      # Only render visible page
    processing = TRUE,       # Show processing indicator
    scrollY = "400px",       # Fixed height scrolling
    scrollX = TRUE,          # Horizontal scrolling
    scroller = TRUE,         # Enable scroller extension
    dom = 'frti',            # Simplified controls
    ordering = TRUE          # Keep ordering capability
  )
  
  # Create a lightweight copy and limit rows for preview
  if (is.data.table(dt)) {
    display_dt <- copy(dt)
  } else {
    display_dt <- as.data.table(dt)
  }
  
  # Limit rows if too large
  if (nrow(display_dt) > 1000) {
    display_dt <- display_dt[1:1000]
  }
  
  datatable(display_dt, options = options, rownames = FALSE, class = class)
}

aggressive_gc <- function(verbose = FALSE) {
  # Free any large objects in global environment
  large_objects <- sapply(ls(envir = .GlobalEnv), function(x) {
    object.size(get(x, envir = .GlobalEnv))
  })
  
  # Run garbage collection multiple times
  for (i in 1:3) {
    gc(verbose = verbose, full = TRUE, reset = TRUE)
  }
  
  # On Linux/Unix systems (like ShinyApps.io), try to force memory release
  if (.Platform$OS.type == "unix") {
    try(system("sync", intern = TRUE), silent = TRUE)
  }
}

# Schedule more frequent memory cleanup
schedule_memory_cleanup <- function(session, interval_seconds = 60) {
  observe({
    invalidateLater(interval_seconds * 1000, session)
    aggressive_gc(verbose = FALSE)
  })
}


# Add this helper function early in your code
create_name_id_mapping <- function(fighter_data) {
  # Convert to data.table if not already
  if (!is.data.table(fighter_data)) setDT(fighter_data)
  
  # Check which ID column to use
  has_nameid <- "NameID" %in% names(fighter_data)
  has_fdnameid <- "FDNameID" %in% names(fighter_data)
  
  # Determine ID column based on what's available
  id_col <- if (has_nameid) "NameID" else if (has_fdnameid) "FDNameID" else NULL
  
  # Create and return mapping - Names to IDs and IDs to Names
  if (!is.null(id_col) && "Name" %in% names(fighter_data)) {
    # Create mapping tables
    name_to_id <- fighter_data[, c("Name", id_col, "Salary", "OP"), with = FALSE]
    setnames(name_to_id, id_col, "FighterID")
    
    # Set keys for faster lookups
    setkey(name_to_id, Name)
    
    return(name_to_id)
  } else {
    warning("Could not create name-ID mapping: missing required columns")
    return(NULL)
  }
}


# Enhanced reactiveValues structure with cleaning functions
initialize_reactive_values <- function() {
  rv <- reactiveValues(
    input_data = NULL,
    simulation_results = NULL,
    fighter_scores = NULL,
    fighter_stats = NULL,
    fight_stats = NULL,
    optimal_lineups = NULL,
    generated_lineups = NULL,
    file_uploaded = FALSE,
    simulation_run = FALSE
  )
  
  # Add cleaning function to reactiveValues
  rv$clean_simulation_data <- function() {
    rv$simulation_results <- NULL
    rv$fighter_scores <- NULL
    rv$fighter_stats <- NULL
    rv$fight_stats <- NULL
    rv$optimal_lineups <- NULL
    rv$generated_lineups <- NULL
    rv$simulation_run <- FALSE
    cleanup_memory()
  }
  
  # Add cleaning function for just analysis results
  rv$clean_analysis_results <- function() {
    rv$fighter_stats <- NULL
    rv$fight_stats <- NULL
    rv$optimal_lineups <- NULL
    rv$generated_lineups <- NULL
    cleanup_memory()
  }
  
  return(rv)
}


# Function to read and validate input file
read_input_file <- function(file_path) {
  tryCatch({
    # Read all sheets at once
    sheets <- list(
      fighterdata = cached_read_excel(file_path, sheet = "FighterData")
    )
    
    # Try to read DraftKings scoring ranges 
    dk_sheet <- tryCatch({
      cached_read_excel(file_path, sheet = "DKScoringRanges")
    }, error = function(e) {
      # Try with the old sheet name for backwards compatibility
      tryCatch({
        cached_read_excel(file_path, sheet = "ScoringRanges")
      }, error = function(e2) {
        stop("Could not find DraftKings scoring ranges (sheet 'DKScoringRanges' or 'ScoringRanges')")
      })
    })
    
    sheets$dk_scoringranges <- dk_sheet
    
    # Try to read FanDuel scoring ranges (optional)
    sheets$fd_scoringranges <- tryCatch({
      cached_read_excel(file_path, sheet = "FDScoringRanges")
    }, error = function(e) {
      # If missing, create a copy of DK ranges as placeholder
      warning("FanDuel scoring ranges (sheet 'FDScoringRanges') not found. Using DraftKings ranges for FanDuel.")
      dk_sheet
    })
    
    # Validate scoring ranges
    required_scoring_cols <- c("Fight", "Winner", "Loser", "Method", "Probability", 
                               "WinnerP10", "WinnerP30", "WinnerP50", "WinnerP70", "WinnerP90",
                               "LoserP10", "LoserP30", "LoserP50", "LoserP70", "LoserP90")
    
    if (!all(required_scoring_cols %in% colnames(sheets$dk_scoringranges))) {
      stop("Missing required columns in DraftKings scoring ranges")
    }
    
    if (!all(required_scoring_cols %in% colnames(sheets$fd_scoringranges))) {
      warning("Missing required columns in FanDuel scoring ranges")
    }
    
    # Process data into data.tables for better performance
    fighters_dt <- as.data.table(sheets$fighterdata)
    dk_scoring_dt <- as.data.table(sheets$dk_scoringranges)
    fd_scoring_dt <- as.data.table(sheets$fd_scoringranges)
    
    # Create processed versions of fighter data for each site
    # First, make copies of the original data
    dk_fighters <- copy(fighters_dt)
    fd_fighters <- copy(fighters_dt)
    
    # For DraftKings: Make sure NameID is present and use DKOwn for OP
    if (!"NameID" %in% names(dk_fighters)) {
      stop("DraftKings data must have a 'NameID' column")
    }
    
    if ("DKOwn" %in% names(dk_fighters)) {
      if (!("OP" %in% names(dk_fighters))) {
        dk_fighters[, OP := as.numeric(DKOwn)]
      }
    }
    
    # For FanDuel: Copy FDNameID to NameID, and FDSal to Salary, FDOwn to OP
    if ("FDNameID" %in% names(fd_fighters)) {
      fd_fighters[, NameID := FDNameID]
    } else {
      stop("FanDuel data must have a 'FDNameID' column")
    }
    
    if ("FDSal" %in% names(fd_fighters)) {
      fd_fighters[, Salary := as.numeric(FDSal)]
    } else {
      stop("FanDuel data must have a 'FDSal' column")
    }
    
    if ("FDOwn" %in% names(fd_fighters)) {
      fd_fighters[, OP := as.numeric(FDOwn)]
    } else {
      stop("FanDuel data must have a 'FDOwn' column")
    }
    
    
    
    # Make sure Salary is numeric
    if (!is.numeric(dk_fighters$Salary)) {
      dk_fighters[, Salary := as.numeric(Salary)]
    }
    
    # Make sure Probability is numeric in scoring data
    dk_scoring_dt[, Probability := as.numeric(Probability)]
    fd_scoring_dt[, Probability := as.numeric(Probability)]
    
    # Convert scoring ranges to numeric
    score_range_cols <- c("WinnerP10", "WinnerP30", "WinnerP50", "WinnerP70", "WinnerP90",
                          "LoserP10", "LoserP30", "LoserP50", "LoserP70", "LoserP90")
    
    for (col in score_range_cols) {
      dk_scoring_dt[, (col) := as.numeric(get(col))]
      fd_scoring_dt[, (col) := as.numeric(get(col))]
    }
    
    # Return processed data with site-specific fighter data
    list(
      fighters_original = fighters_dt,  # Original data
      fighters_dk = dk_fighters,        # Processed for DraftKings
      fighters_fd = fd_fighters,        # Processed for FanDuel
      dk_scoring = dk_scoring_dt,
      fd_scoring = fd_scoring_dt
    )
    
  }, error = function(e) {
    stop(paste("Error reading Excel file:", e$message))
  })
}

# Generate random score based on percentiles
generate_score <- function(min, P30, p50, P70, max) {
  u <- runif(1)
  if (u < 0.25) return(min + (P30 - min) * (u * 4))
  if (u < 0.5) return(P30 + (p50 - P30) * ((u - 0.25) * 4))
  if (u < 0.75) return(p50 + (P70 - p50) * ((u - 0.5) * 4))
  return(P70 + (max - P70) * ((u - 0.75) * 4))
}

cleanup_memory <- function(verbose = FALSE) {
  # Clear any unused memory
  gc(verbose = verbose, full = TRUE, reset = TRUE)
  
  if (.Platform$OS.type == "unix") {
    try(system("sync"), silent = TRUE)
    try(system("echo 3 > /proc/sys/vm/drop_caches"), silent = TRUE)
  }
}

# Calculate filtered pool stats - Move this outside the <- function
calculate_filtered_pool_stats <- function(optimal_lineups, filters) {
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  # Ensure we have a data.table and use efficient reference
  setDT(optimal_lineups)
  
  # More memory efficient: filter directly without copying
  # Create logical vector for each filter
  idx <- rep(TRUE, nrow(optimal_lineups))
  
  # Apply Top1Count filter
  if (!is.null(filters$min_top1_count) && "Top1Count" %in% names(optimal_lineups) && filters$min_top1_count > 0) {
    idx <- idx & optimal_lineups$Top1Count >= filters$min_top1_count
  }
  
  # Apply Top2Count filter
  if (!is.null(filters$min_top2_count) && "Top2Count" %in% names(optimal_lineups) && filters$min_top2_count > 0) {
    idx <- idx & optimal_lineups$Top2Count >= filters$min_top2_count
  }
  
  # Apply Top3Count filter
  if (!is.null(filters$min_top3_count) && "Top3Count" %in% names(optimal_lineups) && filters$min_top3_count > 0) {
    idx <- idx & optimal_lineups$Top3Count >= filters$min_top3_count
  }
  
  # Apply Top5Count filter
  if (!is.null(filters$min_top5_count) && "Top5Count" %in% names(optimal_lineups) && filters$min_top5_count > 0) {
    idx <- idx & optimal_lineups$Top5Count >= filters$min_top5_count
  }
  
  # Apply same fight exclusion filter
  if (!is.null(filters$exclude_same_fight) && filters$exclude_same_fight && "HasSameFight" %in% names(optimal_lineups)) {
    idx <- idx & !optimal_lineups$HasSameFight
  }
  
  # Apply fighter exclusion filter efficiently
  if (!is.null(filters$excluded_fighters) && length(filters$excluded_fighters) > 0) {
    fighter_cols <- grep("^Name[1-6]$", names(optimal_lineups), value = TRUE)
    if (length(fighter_cols) > 0) {
      for (col in fighter_cols) {
        exclude_idx <- optimal_lineups[[col]] %in% filters$excluded_fighters
        idx <- idx & !exclude_idx
      }
    }
  }
  
  # Apply filter - if none match, return early
  filtered_count <- sum(idx)
  if(filtered_count == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  # Calculate thresholds on filtered subset only
  thresholds <- list()
  threshold_columns <- c("Top1Count", "Top2Count", "Top3Count", "Top5Count")
  
  for (col in threshold_columns) {
    if (col %in% names(optimal_lineups)) {
      values <- optimal_lineups[idx, get(col)]
      min_val <- min(values, na.rm = TRUE)
      max_val <- max(values, na.rm = TRUE)
      
      min_name <- paste0("min_", sub("Count", "", tolower(col)))
      max_name <- paste0("max_", sub("Count", "", tolower(col)))
      
      thresholds[[min_name]] <- min_val
      thresholds[[max_name]] <- max_val
    }
  }
  
  return(list(
    count = filtered_count,
    thresholds = thresholds
  ))
}

# Calculate fighter exposure statistics for the analysis table
calculate_fighter_exposure <- function(optimal_lineups, fantasy_analysis, random_lineups) {
  if (is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame(Message = "No optimal lineups available."))
  }
  
  # Use data.table operations throughout
  setDT(optimal_lineups)
  if (!is.null(fantasy_analysis)) setDT(fantasy_analysis)
  if (!is.null(random_lineups)) setDT(random_lineups)
  
  # Pre-calculate fighter columns - use both Name and FighterID columns
  fighter_name_cols <- grep("^Name[1-6]$", names(optimal_lineups), value = TRUE)
  fighter_id_cols <- grep("^FighterID[1-6]$", names(optimal_lineups), value = TRUE)
  
  # Find all fighters once - more efficient extraction
  # Use Name for display but keep track of IDs
  all_fighters <- unique(unlist(optimal_lineups[, ..fighter_name_cols]))
  
  # Initialize data table for the metrics
  metrics_data <- data.table(
    Fighter = all_fighters,
    OptimalRate = 0,
    AppearanceRate = 0,
    EliteRate = 0,
    FloorRate = 0,
    Exposure = 0
  )
  
  # Calculate each metric
  
  # 1. Optimal Rate: Percentage of Top1Count lineups containing the fighter
  if ("Top1Count" %in% names(optimal_lineups)) {
    # First, filter to only optimal lineups (Top1Count > 0)
    optimal_lineups_filtered <- optimal_lineups[Top1Count > 0]
    optimal_count <- nrow(optimal_lineups_filtered)
    
    if (optimal_count > 0) {
      # Calculate for each fighter
      for (i in 1:length(all_fighters)) {
        fighter <- all_fighters[i]
        
        # More direct approach to count appearances - Using Name columns for consistency
        driver_optimal_count <- sum(
          sapply(fighter_name_cols, function(col) optimal_lineups_filtered[[col]] == fighter),
          na.rm = TRUE
        )
        
        # Calculate the rate
        metrics_data[i, OptimalRate := (driver_optimal_count / optimal_count) * 100]
      }
    }
  }
  
  # 2. Appearance Rate: Percentage of all lineups containing the fighter
  total_lineups <- nrow(optimal_lineups)
  if (total_lineups > 0) {
    # For each fighter
    for (i in 1:length(all_fighters)) {
      fighter <- all_fighters[i]
      
      # Direct approach to count appearances - Using Name columns for consistency
      driver_appearances <- sum(
        sapply(fighter_name_cols, function(col) optimal_lineups[[col]] == fighter),
        na.rm = TRUE
      )
      
      # Calculate the rate
      metrics_data[i, AppearanceRate := (driver_appearances / total_lineups) * 100]
    }
  }
  
  # 3. Elite Rate: Top 200 lineups by Top1Count with Top5Count as tiebreaker
  if ("Top1Count" %in% names(optimal_lineups) && "Top5Count" %in% names(optimal_lineups)) {
    # Sort by Top1Count (primary) and Top5Count (secondary) in descending order
    sorted_lineups <- optimal_lineups[order(-Top1Count, -Top5Count)]
    
    # Take exactly the top 200 lineups or all lineups if fewer than 200
    n_elite <- min(200, nrow(optimal_lineups))
    elite_lineups <- sorted_lineups[1:n_elite]
    elite_count <- nrow(elite_lineups)
    
    if (elite_count > 0) {
      # For each fighter
      for (i in 1:length(all_fighters)) {
        fighter <- all_fighters[i]
        
        # Count appearances - Using Name columns for consistency
        driver_elite_count <- sum(
          sapply(fighter_name_cols, function(col) elite_lineups[[col]] == fighter),
          na.rm = TRUE
        )
        
        # Calculate the rate
        metrics_data[i, EliteRate := (driver_elite_count / elite_count) * 100]
      }
    }
  }
  
  # 4. Floor Rate: Top 1000 lineups by Top5Count
  if ("Top5Count" %in% names(optimal_lineups)) {
    # Sort by Top5Count in descending order
    sorted_lineups <- optimal_lineups[order(-Top5Count)]
    
    # Take exactly the top 1000 lineups or all lineups if fewer than 1000
    n_floor <- min(1000, nrow(optimal_lineups))
    floor_lineups <- sorted_lineups[1:n_floor]
    floor_count <- nrow(floor_lineups)
    
    if (floor_count > 0) {
      # For each fighter
      for (i in 1:length(all_fighters)) {
        fighter <- all_fighters[i]
        
        # Count appearances - Using Name columns for consistency
        driver_floor_count <- sum(
          sapply(fighter_name_cols, function(col) floor_lineups[[col]] == fighter),
          na.rm = TRUE
        )
        
        # Calculate the rate
        metrics_data[i, FloorRate := (driver_floor_count / floor_count) * 100]
      }
    }
  }
  
  # Calculate Exposure from random lineups if available
  if (!is.null(random_lineups) && nrow(random_lineups) > 0) {
    # Check for both Name and FighterID columns in random lineups
    rand_fighter_name_cols <- grep("^Name[1-6]$", names(random_lineups), value = TRUE)
    
    if (length(rand_fighter_name_cols) > 0) {
      for (i in 1:length(all_fighters)) {
        fighter <- all_fighters[i]
        
        # Count appearances across all random lineup Name columns
        count <- sum(
          sapply(rand_fighter_name_cols, function(col) random_lineups[[col]] == fighter),
          na.rm = TRUE
        )
        
        # Calculate exposure percentage
        metrics_data[i, Exposure := (count / nrow(random_lineups)) * 100]
      }
    }
  }
  
  # Add fields from fantasy analysis
  if (!is.null(fantasy_analysis)) {
    # For MMA app, we need to handle Name column
    name_col <- "Name"
    if (!"Name" %in% names(fantasy_analysis) && "Name + ID" %in% names(fantasy_analysis)) {
      name_col <- "Name + ID"
    }
    
    # Create lookup data
    if (name_col %in% names(fantasy_analysis)) {
      # Select relevant columns from fantasy analysis
      fantasy_cols <- intersect(c(name_col, "Salary", "Starting", "MedianPoints", "AvgPoints", "OwnProj"), 
                                names(fantasy_analysis))
      
      # Only proceed if we have the name column and at least one more column
      if (length(fantasy_cols) > 1) {
        lookup_data <- fantasy_analysis[, ..fantasy_cols]
        
        # Rename columns as needed
        if ("MedianPoints" %in% names(lookup_data)) {
          setnames(lookup_data, "MedianPoints", "Proj")
        } else if ("AvgPoints" %in% names(lookup_data)) {
          setnames(lookup_data, "AvgPoints", "Proj")
        }
        
        # Rename ownership column if needed
        if ("OwnProj" %in% names(lookup_data)) {
          setnames(lookup_data, "OwnProj", "Ownership")
        } else if ("DKOP" %in% names(lookup_data)) {
          setnames(lookup_data, "DKOP", "Ownership")
        } else if ("OP" %in% names(lookup_data)) {
          setnames(lookup_data, "OP", "Ownership")
        }
        
        # Merge with metrics data
        setnames(lookup_data, name_col, "Fighter")
        metrics_data <- merge(metrics_data, lookup_data, by = "Fighter", all.x = TRUE)
      }
    }
  }
  
  # Calculate Leverage if we have both Exposure and Ownership
  if (all(c("Exposure", "Ownership") %in% names(metrics_data))) {
    metrics_data[, Leverage := round(Exposure - Ownership, 1)]
  }
  
  # Replace NA values with 0
  metrics_data[is.na(metrics_data)] <- 0
  
  # Sort by OptimalRate in descending order
  setorder(metrics_data, -OptimalRate)
  
  return(metrics_data)
}

generate_random_lineups_optimized <- function(optimal_lineups, filters, dfs_site = "dk") {
  # Apply all filters to optimal_lineups first
  filtered_dt <- as.data.table(optimal_lineups)
  
  # Apply Top1Count filter
  if (!is.null(filters$min_top1_count) && "Top1Count" %in% names(filtered_dt)) {
    filtered_dt <- filtered_dt[Top1Count >= filters$min_top1_count]
  }
  
  # Apply Top2Count filter
  if (!is.null(filters$min_top2_count) && "Top2Count" %in% names(filtered_dt)) {
    filtered_dt <- filtered_dt[Top2Count >= filters$min_top2_count]
  }
  
  # Apply Top3Count filter
  if (!is.null(filters$min_top3_count) && "Top3Count" %in% names(filtered_dt)) {
    filtered_dt <- filtered_dt[Top3Count >= filters$min_top3_count]
  }
  
  # Apply Top5Count filter
  if (!is.null(filters$min_top5_count) && "Top5Count" %in% names(filtered_dt)) {
    filtered_dt <- filtered_dt[Top5Count >= filters$min_top5_count]
  }
  
  # Apply same fight exclusion filter
  if (!is.null(filters$exclude_same_fight) && filters$exclude_same_fight && "HasSameFight" %in% names(filtered_dt)) {
    filtered_dt <- filtered_dt[HasSameFight == FALSE]
  }
  
  # Apply fighter exclusion filter
  if (!is.null(filters$excluded_fighters) && length(filters$excluded_fighters) > 0) {
    # First check if we're using Name or FighterID for exclusions
    # Default to use Name columns for backwards compatibility
    fighter_cols <- grep("^Name[1-6]$", names(filtered_dt), value = TRUE)
    
    if (length(fighter_cols) > 0) {
      for (col in fighter_cols) {
        filtered_dt <- filtered_dt[!(get(col) %in% filters$excluded_fighters)]
      }
    }
  }
  
  # Return early if no lineups remain
  if (nrow(filtered_dt) == 0) {
    return(NULL)
  }
  
  # More efficient weights calculation
  weight_col <- NULL
  for (col in c("Top1Count", "Top5Count", "Frequency")) {
    if (col %in% names(filtered_dt)) {
      weight_col <- col
      break
    }
  }
  
  # Pre-calculate weights once
  if (!is.null(weight_col)) {
    weights <- filtered_dt[[weight_col]]
    # Handle zero weights
    if (all(weights == 0)) {
      weights <- rep(1, length(weights))
    }
  } else {
    weights <- rep(1, nrow(filtered_dt))
  }
  
  # Pre-extract fighter info for exposure tracking
  n_lineups <- filters$num_lineups
  
  # Get fighter columns for tracking - use Name columns for consistency with existing code
  # For FanDuel, include IsCaptain columns in tracking
  if (dfs_site == "fd") {
    fighter_cols <- grep("^Name[1-6]$", names(filtered_dt), value = TRUE)
    captain_cols <- grep("^IsCaptain[1-6]$", names(filtered_dt), value = TRUE)
  } else {
    fighter_cols <- grep("^Name[1-6]$", names(filtered_dt), value = TRUE)
  }
  
  all_fighters <- unique(unlist(lapply(fighter_cols, function(col) filtered_dt[[col]])))
  
  # Improved sampling without replacement
  max_attempts <- min(n_lineups * 5, nrow(filtered_dt) * 2)
  selected_lineups <- vector("list", n_lineups)
  selected_indices <- integer(0)
  fighter_counts <- setNames(numeric(length(all_fighters)), all_fighters)
  
  # Track which rows we've tried already (helps avoid repeated failures)
  attempted_indices <- integer(0)
  
  num_selected <- 0
  attempts <- 0
  
  while (num_selected < n_lineups && attempts < max_attempts) {
    attempts <- attempts + 1
    
    # Available indices exclude both selected and ones we already tried 
    # that violated exposure limits
    available_indices <- setdiff(1:nrow(filtered_dt), union(selected_indices, attempted_indices))
    
    # If we've exhausted all options, clear attempted_indices and try again
    if (length(available_indices) == 0) {
      if (length(attempted_indices) > 0) {
        attempted_indices <- integer(0)
        available_indices <- setdiff(1:nrow(filtered_dt), selected_indices)
      } else {
        break # No more lineups available
      }
    }
    
    # Sample a lineup 
    selected_idx <- sample(available_indices, 1, prob = weights[available_indices])
    
    # Extract fighter names from this lineup
    candidate_fighters <- unlist(lapply(fighter_cols, function(col) filtered_dt[[col]][selected_idx]))
    
    # Check exposure limits - this is now vectorized for better performance
    if (!is.null(filters$max_exposure)) {
      # Calculate proposed exposure with this lineup added
      exposure_check <- table(factor(c(names(fighter_counts[fighter_counts > 0]), 
                                       candidate_fighters), 
                                     levels = all_fighters))
      proposed_exposure <- (exposure_check / (num_selected + 1)) * 100
      
      # Check if any fighter would exceed exposure limit
      if (any(proposed_exposure > filters$max_exposure)) {
        # Track this index as attempted but failed exposure check
        attempted_indices <- c(attempted_indices, selected_idx)
        next
      }
    }
    
    # This lineup passes all checks - add it
    num_selected <- num_selected + 1
    selected_lineups[[num_selected]] <- filtered_dt[selected_idx]
    selected_indices <- c(selected_indices, selected_idx)
    
    # Update fighter counts
    for (fighter in candidate_fighters) {
      fighter_counts[fighter] <- fighter_counts[fighter] + 1
    }
    
    # Memory cleanup
    if (attempts %% 1000 == 0) {
      cleanup_memory(verbose = FALSE)
    }
  }
  
  # Combine selected lineups into a single data.table
  if (num_selected == 0) {
    return(NULL)
  }
  
  result <- rbindlist(selected_lineups[1:num_selected])
  
  # Make sure HasSameFight is included in the result
  # If it's missing, calculate it based on the Fight information
  if (!("HasSameFight" %in% names(result)) && "Fight" %in% names(filtered_dt)) {
    # We need to recalculate the HasSameFight flag for each lineup
    # This would require additional logic that depends on how fight information is stored
    # For now, we'll just ensure the flag is carried over if it exists in the source data
    
    # If HasSameFight exists in the source data, it should be carried over
    # If not, we'll add a column with FALSE as default
    if (!("HasSameFight" %in% names(result))) {
      result[, HasSameFight := FALSE]
    }
  }
  
  # Add exposure info as attribute
  attr(result, "exposure") <- (fighter_counts / num_selected) * 100
  
  return(result)
}

# =============================================
# SIMULATION FUNCTIONS
# =============================================

# Simulate a single fight outcome
simulate_single_fight <- function(fight_data) {
  # Ensure we're working with a data.table
  if (!is.data.table(fight_data)) {
    fight_data <- as.data.table(fight_data)
  }
  
  # Get total probability to ensure it sums to 1
  total_prob <- sum(fight_data$Probability)
  
  # Generate a random number to determine outcome
  rand <- runif(1)
  
  # Select outcome based on probability
  cum_prob <- 0
  for (i in 1:nrow(fight_data)) {
    cum_prob <- cum_prob + (fight_data$Probability[i] / total_prob)
    if (rand <= cum_prob) {
      selected_outcome <- fight_data[i]
      break
    }
  }
  
  # Generate scores for winner and loser
  winner_score <- generate_score(
    selected_outcome$WinnerP10,
    selected_outcome$WinnerP30,
    selected_outcome$WinnerP50,
    selected_outcome$WinnerP70,
    selected_outcome$WinnerP90
  )
  
  loser_score <- generate_score(
    selected_outcome$LoserP10,
    selected_outcome$LoserP30,
    selected_outcome$LoserP50,
    selected_outcome$LoserP70,
    selected_outcome$LoserP90
  )
  
  # Return the outcome as a data.table row
  data.table(
    Fight = selected_outcome$Fight,
    Winner = selected_outcome$Winner,
    Loser = selected_outcome$Loser,
    Method = selected_outcome$Method,
    WinnerScore = winner_score,
    LoserScore = loser_score
  )
}


# Simulate all fights for multiple iterations
simulate_fights <- function(scoring_data, n_sims = 1000, batch_size = 50) {
  # Convert to data.table if not already
  setDT(scoring_data)
  
  # Get unique fights
  unique_fights <- unique(scoring_data$Fight)
  n_fights <- length(unique_fights)
  
  # Calculate number of batches
  n_batches <- ceiling(n_sims / batch_size)
  
  # Initialize results with proper structure but empty
  results <- data.table(
    SimID = integer(),
    Fight = character(),
    Winner = character(),
    Loser = character(),
    Method = character(),
    WinnerScore = numeric(),
    LoserScore = numeric()
  )
  
  # Process in batches
  for (batch in 1:n_batches) {
    # Determine batch size
    start_sim <- (batch - 1) * batch_size + 1
    end_sim <- min(batch * batch_size, n_sims)
    current_batch_size <- end_sim - start_sim + 1
    
    # Pre-allocate chunk results for better performance
    chunk_rows <- current_batch_size * n_fights
    chunk_results <- data.table(
      SimID = integer(chunk_rows),
      Fight = character(chunk_rows),
      Winner = character(chunk_rows),
      Loser = character(chunk_rows),
      Method = character(chunk_rows),
      WinnerScore = numeric(chunk_rows),
      LoserScore = numeric(chunk_rows)
    )
    
    row_counter <- 1
    
    # Process each simulation in this batch
    for (sim in start_sim:end_sim) {
      # Process each fight
      for (fight in unique_fights) {
        # Get data for this fight
        fight_data <- scoring_data[Fight == fight]
        
        # Simulate this fight
        fight_result <- simulate_single_fight(fight_data)
        
        # Store the result
        chunk_results[row_counter, `:=`(
          SimID = sim,
          Fight = fight_result$Fight,
          Winner = fight_result$Winner,
          Loser = fight_result$Loser,
          Method = fight_result$Method,
          WinnerScore = fight_result$WinnerScore,
          LoserScore = fight_result$LoserScore
        )]
        
        row_counter <- row_counter + 1
      }
    }
    
    # Bind chunk to results
    results <- rbindlist(list(results, chunk_results[1:(row_counter-1)]), use.names = TRUE)
    
    # Force garbage collection after each chunk
    if (batch %% 2 == 0 || batch == n_batches) {  # Change from %% 3 to %% 2
      cat("Processed batch", batch, "of", n_batches, "\n")
      aggressive_gc()  # Use aggressive_gc instead of cleanup_memory
    }
  }
  
  # Set key for faster operations
  setkey(results, SimID, Fight)
  
  return(results)
}

# Process simulation results to get fighter scores
# Process simulation results to get fighter scores
process_fighter_scores <- function(sim_results, fighter_data, chunk_size = 250) {
  # Ensure we're working with data.tables
  setDT(sim_results)
  setDT(fighter_data)
  
  # Make sure we have the required columns
  required_cols <- c("NameID", "Name", "Salary", "OP")
  for(col in required_cols) {
    if(!(col %in% names(fighter_data))) {
      stop(paste("Missing required column in fighter data:", col))
    }
  }
  
  # Get unique SimIDs
  sim_ids <- unique(sim_results$SimID)
  n_sims <- length(sim_ids)
  
  # Get unique fighters
  fighters <- unique(fighter_data$NameID)
  n_fighters <- length(fighters)
  
  # Create mapping from NameID to Name
  name_mapping <- fighter_data[, .(NameID, Name, Salary, OP)]
  setkey(name_mapping, NameID)
  
  # Initialize results data.table
  fighter_scores <- data.table(
    SimID = integer(),
    NameID = character(),
    Name = character(),
    Salary = numeric(),
    OP = numeric(),
    FantasyPoints = numeric()
  )
  
  # Calculate number of chunks
  n_chunks <- ceiling(n_sims / chunk_size)
  
  # Process in chunks
  for (chunk in 1:n_chunks) {
    start_sim <- (chunk - 1) * chunk_size + 1
    end_sim <- min(chunk * chunk_size, n_sims)
    current_chunk_size <- end_sim - start_sim + 1
    
    # Pre-allocate chunk results
    chunk_rows <- current_chunk_size * n_fighters
    chunk_scores <- data.table(
      SimID = integer(chunk_rows),
      NameID = character(chunk_rows),
      Name = character(chunk_rows),
      Salary = numeric(chunk_rows),
      OP = numeric(chunk_rows),
      FantasyPoints = numeric(chunk_rows)
    )
    
    row_counter <- 1
    
    # Get only the sim data needed for this chunk
    chunk_sim_ids <- sim_ids[start_sim:end_sim]
    chunk_sim_data <- sim_results[SimID %in% chunk_sim_ids]
    
    # Process each simulation in this chunk
    for (sim in chunk_sim_ids) {
      # Get results for this simulation
      sim_data <- chunk_sim_data[SimID == sim]
      
      # Create lookup tables for winners and losers for faster lookups
      winners <- sim_data[, .(Name = Winner, WinnerScore)]
      setkey(winners, Name)
      
      losers <- sim_data[, .(Name = Loser, LoserScore)]
      setkey(losers, Name)
      
      # Process each fighter
      for (i in 1:n_fighters) {
        f_id <- fighters[i]
        fighter <- name_mapping[NameID == f_id]
        
        # Initialize fantasy points
        fantasy_points <- 0
        
        # Check if fighter won 
        winner_match <- winners[fighter$Name]
        if (!is.na(winner_match$WinnerScore)) {
          fantasy_points <- winner_match$WinnerScore
        } else {
          # Check if fighter lost
          loser_match <- losers[fighter$Name]
          if (!is.na(loser_match$LoserScore)) {
            fantasy_points <- loser_match$LoserScore
          }
        }
        
        # Store the result
        chunk_scores[row_counter, `:=`(
          SimID = sim,
          NameID = f_id,
          Name = fighter$Name,
          Salary = fighter$Salary,
          OP = fighter$OP,
          FantasyPoints = fantasy_points
        )]
        
        row_counter <- row_counter + 1
      }
    }
    
    # Bind chunk to results - only bind the rows we actually filled
    fighter_scores <- rbindlist(list(fighter_scores, chunk_scores[1:(row_counter-1)]), use.names = TRUE)
    
    # Garbage collection
    if (chunk %% 3 == 0 || chunk == n_chunks) {
      cat("Processed fighter scores chunk", chunk, "of", n_chunks, "\n")
      cleanup_memory()
    }
  }
  
  # Set key for faster operations
  setkey(fighter_scores, SimID, NameID)
  
  return(fighter_scores)
}


find_optimal_lineup <- function(sim_data, salary_cap, roster_size, dfs_site = "dk") {
  # Ensure we're working with a data.table
  if (!is.data.table(sim_data)) {
    sim_data <- as.data.table(sim_data)
  }
  
  # Get top performers for this simulation
  top_candidates <- sim_data[order(-FantasyPoints)][1:min(20, .N)]
  
  # Continue only if we have enough fighters
  if (nrow(top_candidates) < roster_size) return(NULL)
  
  # Set up for lpSolve
  n <- nrow(top_candidates)
  
  if (dfs_site == "dk") {
    # DraftKings: standard optimization
    obj <- top_candidates$FantasyPoints
    
    # Constraints matrix
    const.mat <- matrix(c(
      top_candidates$Salary,  # Salary constraint
      rep(1, n)               # Roster size constraint
    ), nrow = 2, byrow = TRUE)
    
    const.dir <- c("<=", "==")
    const.rhs <- c(salary_cap, roster_size)
    
  } else {
    # FanDuel: 5 regular fighters + 1 captain (1.5x points)
    
    # Create expanded matrix with regular and captain versions of each fighter
    # First n columns: regular, Next n columns: captain
    
    # Objective function: regular points plus captain points (1.5x)
    obj <- c(top_candidates$FantasyPoints, top_candidates$FantasyPoints * 1.5)
    
    # Constraints:
    # 1. Salary constraint (regular salary + captain salary)
    # 2. Total fighters = roster_size (5 regular + 1 captain)
    # 3. Each fighter can only be used once (either regular or captain)
    # 4. Exactly 1 captain
    
    # Salary constraint: same salary for regular and captain
    salary_constraint <- c(top_candidates$Salary, top_candidates$Salary)
    
    # Regular fighters constraint: 1 for regular, 0 for captain
    regular_constraint <- c(rep(1, n), rep(0, n))
    
    # Captain constraint: 0 for regular, 1 for captain
    captain_constraint <- c(rep(0, n), rep(1, n))
    
    # Identity constraints: each fighter can only be used once
    identity_constraints <- matrix(0, nrow = n, ncol = 2*n)
    for (i in 1:n) {
      identity_constraints[i, i] <- 1        # Regular version
      identity_constraints[i, i + n] <- 1    # Captain version
    }
    
    # Combined constraints matrix
    const.mat <- rbind(
      salary_constraint,           # Salary constraint
      regular_constraint + captain_constraint,  # Total fighters constraint
      identity_constraints,        # Identity constraints
      captain_constraint           # Captain constraint
    )
    
    const.dir <- c("<=", "==", rep("<=", n), "==")
    const.rhs <- c(salary_cap, roster_size, rep(1, n), 1)
  }
  
  # Solve using LP with error handling
  tryCatch({
    result <- lp("max", obj, const.mat, const.dir, const.rhs, all.bin = TRUE)
    
    if (result$status != 0) return(NULL)  # No solution found
    
    if (dfs_site == "dk") {
      # Extract the selected fighters for DraftKings
      selected <- top_candidates[which(result$solution > 0.9)]
      
    } else {
      # For FanDuel, we need to combine regular and captain fighters
      solution_vector <- result$solution
      
      # Regular fighters (first half of solution vector)
      regular_indices <- which(solution_vector[1:n] > 0.9)
      regular_fighters <- top_candidates[regular_indices]
      
      # Captain (second half of solution vector)
      captain_indices <- which(solution_vector[(n+1):(2*n)] > 0.9) 
      captain_fighter <- top_candidates[captain_indices]
      
      # Add a captain flag to the fighter
      if (nrow(captain_fighter) > 0) {
        captain_fighter[, IsCaptain := TRUE]
        captain_fighter[, CaptainPoints := FantasyPoints * 1.5]
      }
      
      # Add regular flag and dummy captain points to regular fighters
      if (nrow(regular_fighters) > 0) {
        regular_fighters[, IsCaptain := FALSE]
        regular_fighters[, CaptainPoints := FantasyPoints]
      }
      
      # Combine the results
      selected <- rbindlist(list(regular_fighters, captain_fighter), use.names = TRUE, fill = TRUE)
    }
    
    # Check we have exactly roster_size fighters
    if (nrow(selected) != roster_size) return(NULL)
    
    # Return the lineup
    return(selected)
  }, error = function(e) {
    cat("LP solver error:", e$message, "\n")
    return(NULL)
  })
}

# Find all valid lineups for a simulation
find_all_valid_lineups <- function(sim_data, salary_cap = 50000, roster_size = 6, max_results = 15, dfs_site = "dk") {
  # Ensure we're working with a data.table
  if (!is.data.table(sim_data)) {
    sim_data <- as.data.table(sim_data)
  }
  
  # Get top performers for this simulation
  top_candidates <- sim_data[order(-FantasyPoints)][1:min(20, .N)]
  
  # If we don't have enough fighters to form a valid lineup, return empty list
  if (nrow(top_candidates) < roster_size) return(list())
  
  # First, find the optimal lineup using LP solver
  optimal_lineup <- find_optimal_lineup(sim_data, salary_cap, roster_size, dfs_site)
  if (is.null(optimal_lineup)) return(list())
  
  # Store valid lineups and their scores
  valid_lineups <- list(optimal_lineup)
  
  # Calculate lineup score based on DFS site
  if (dfs_site == "dk") {
    lineup_scores <- sum(optimal_lineup$FantasyPoints)
  } else {
    # For FanDuel, use CaptainPoints which includes 1.5x for the captain
    lineup_scores <- sum(optimal_lineup$CaptainPoints)
  }
  
  # For FanDuel, we need a different approach to generate more valid lineups
  if (dfs_site == "fd") {
    # We'll use a simple approach: try different fighters as captains
    # Start with the top fighters by fantasy points
    for (i in 1:min(10, nrow(top_candidates))) {
      # Skip if this fighter is already the captain in the optimal lineup
      if (i %in% which(optimal_lineup$IsCaptain)) next
      
      # Try to make this fighter the captain
      captain_fighter <- top_candidates[i]
      captain_id <- captain_fighter$NameID
      
      # Remove the captain from regular fighters
      regular_candidates <- top_candidates[NameID != captain_id]
      
      # If we don't have enough regular fighters left, skip
      if (nrow(regular_candidates) < (roster_size - 1)) next
      
      # Find optimal set of regular fighters given this captain
      # This would be more complex to implement fully - simplified here
      # We'll just take the top regular fighters by points
      regular_fighters <- regular_candidates[order(-FantasyPoints)][1:(roster_size-1)]
      
      # Check if salary cap is respected
      total_salary <- sum(regular_fighters$Salary) + captain_fighter$Salary
      if (total_salary <= salary_cap) {
        # Create lineup with this captain
        captain_fighter[, IsCaptain := TRUE]
        captain_fighter[, CaptainPoints := FantasyPoints * 1.5]
        
        regular_fighters[, IsCaptain := FALSE]
        regular_fighters[, CaptainPoints := FantasyPoints]
        
        # Combine into one lineup
        lineup <- rbindlist(list(regular_fighters, captain_fighter), use.names = TRUE, fill = TRUE)
        
        # Create lineup ID to check for duplicates
        lineup_id <- paste(sort(lineup$NameID), collapse = "|")
        
        # Check if we already have this lineup
        existing_ids <- sapply(valid_lineups, function(l) {
          paste(sort(l$NameID), collapse = "|")
        })
        
        if (!lineup_id %in% existing_ids) {
          valid_lineups[[length(valid_lineups) + 1]] <- lineup
          lineup_scores[length(lineup_scores) + 1] <- sum(lineup$CaptainPoints)
          
          # If we have enough lineups, stop
          if (length(valid_lineups) >= max_results) break
        }
      }
    }
  } else {
    # Original DraftKings approach for generating more lineups
    # Generate all combinations of roster_size fighters from top_candidates
    tryCatch({
      comb_indices <- combn(nrow(top_candidates), roster_size)
      
      # Loop through each combination
      for (i in 1:ncol(comb_indices)) {
        # Get the fighters for this combination
        fighter_indices <- comb_indices[, i]
        lineup <- top_candidates[fighter_indices]
        
        # Check if lineup is valid (under salary cap)
        total_salary <- sum(lineup$Salary)
        if (total_salary <= salary_cap) {
          # Create lineup ID to check for duplicates
          lineup_id <- paste(sort(lineup$NameID), collapse = "|")
          
          # Check if we already have this lineup
          existing_ids <- sapply(valid_lineups, function(l) {
            paste(sort(l$NameID), collapse = "|")
          })
          
          if (!lineup_id %in% existing_ids) {
            valid_lineups[[length(valid_lineups) + 1]] <- lineup
            lineup_scores[length(lineup_scores) + 1] <- sum(lineup$FantasyPoints)
            
            # If we have enough lineups, stop
            if (length(valid_lineups) >= max_results) break
          }
        }
      }
    }, error = function(e) {
      cat("Error in lineup combination generation:", e$message, "\n")
      # Continue with whatever lineups we've found so far
    })
  }
  
  # Sort lineups by score and return top ones
  if (length(valid_lineups) > 1) {
    sorted_idx <- order(lineup_scores, decreasing = TRUE)
    return(list(
      lineups = valid_lineups[sorted_idx],
      scores = lineup_scores[sorted_idx]
    ))
  } else {
    return(list(
      lineups = valid_lineups,
      scores = lineup_scores
    ))
  }
}

# Find top lineups across all simulations
find_top_lineups <- function(fighter_scores, salary_cap = 50000, roster_size = 6, max_lineups = 50000, 
                             chunk_size = 100, dfs_site = "dk") {
  # Get unique simulation IDs
  sim_ids <- unique(fighter_scores$SimID)
  n_sims <- length(sim_ids)
  
  # Initialize the lineup counter table
  lineup_counter <- data.table(
    LineupID = character(),
    Count = integer(),
    Top1Count = integer(),
    Top2Count = integer(),
    Top3Count = integer(),
    Top5Count = integer()
  )
  
  # Add FanDuel-specific columns if needed
  if (dfs_site == "fd") {
    lineup_counter[, CaptainID := character()]
  }
  
  # Calculate number of chunks
  n_chunks <- ceiling(n_sims / chunk_size)
  
  # Process in chunks
  for (chunk in 1:n_chunks) {
    start_sim <- (chunk - 1) * chunk_size + 1
    end_sim <- min(chunk * chunk_size, n_sims)
    
    # Get only the sims we need for this chunk
    chunk_sim_ids <- sim_ids[start_sim:end_sim]
    chunk_fighter_scores <- fighter_scores[SimID %in% chunk_sim_ids]
    
    # Process each simulation in this chunk
    chunk_lineup_counter <- data.table(
      LineupID = character(),
      Count = integer(),
      Top1Count = integer(),
      Top2Count = integer(),
      Top3Count = integer(),
      Top5Count = integer()
    )
    
    # Add FanDuel-specific columns if needed
    if (dfs_site == "fd") {
      chunk_lineup_counter[, CaptainID := character()]
    }
    
    for (i in 1:length(chunk_sim_ids)) {
      sim <- chunk_sim_ids[i]
      
      # Get data for this simulation
      sim_data <- chunk_fighter_scores[SimID == sim]
      
      # Find multiple valid lineups for this simulation with error handling
      tryCatch({
        results <- find_all_valid_lineups(sim_data, salary_cap, roster_size, dfs_site = dfs_site)
        
        # If we have valid lineups, process them
        if (length(results$lineups) > 0) {
          # Get the top 5 lineups (or fewer if less than 5 exist)
          top_count <- min(5, length(results$lineups))
          
          # Process each of the top lineups
          for (rank in 1:top_count) {
            # Get the lineup at this rank
            lineup <- results$lineups[[rank]]
            
            # Create a lineup ID by sorting and concatenating NameIDs
            lineup_id <- paste(sort(lineup$NameID), collapse = "|")
            
            # For FanDuel, track the captain
            captain_id <- NULL
            if (dfs_site == "fd" && "IsCaptain" %in% names(lineup)) {
              captain_id <- lineup[IsCaptain == TRUE, NameID]
              if (length(captain_id) == 0) captain_id <- NULL
            }
            
            # Update the counter table
            if (lineup_id %in% chunk_lineup_counter$LineupID) {
              # Increment existing lineup counts
              chunk_lineup_counter[LineupID == lineup_id, Count := Count + 1]
              
              # Update Top-X counters based on rank
              if (rank == 1) chunk_lineup_counter[LineupID == lineup_id, Top1Count := Top1Count + 1]
              if (rank <= 2) chunk_lineup_counter[LineupID == lineup_id, Top2Count := Top2Count + 1]
              if (rank <= 3) chunk_lineup_counter[LineupID == lineup_id, Top3Count := Top3Count + 1]
              chunk_lineup_counter[LineupID == lineup_id, Top5Count := Top5Count + 1]
              
              # For FanDuel, update captain if this is the top lineup
              if (dfs_site == "fd" && rank == 1 && !is.null(captain_id)) {
                # Count which captain is most common in top lineups
                existing_captain <- chunk_lineup_counter[LineupID == lineup_id, CaptainID]
                if (is.na(existing_captain) || existing_captain == "") {
                  chunk_lineup_counter[LineupID == lineup_id, CaptainID := captain_id]
                }
              }
            } else {
              # Add a new row for this lineup
              new_row <- data.table(
                LineupID = lineup_id,
                Count = 1,
                Top1Count = ifelse(rank == 1, 1, 0),
                Top2Count = ifelse(rank <= 2, 1, 0),
                Top3Count = ifelse(rank <= 3, 1, 0),
                Top5Count = ifelse(rank <= 5, 1, 0)
              )
              
              # For FanDuel, add captain info
              if (dfs_site == "fd") {
                new_row[, CaptainID := ifelse(!is.null(captain_id) && rank == 1, captain_id, NA_character_)]
              }
              
              chunk_lineup_counter <- rbindlist(list(chunk_lineup_counter, new_row))
            }
          }
        }
      }, error = function(e) {
        cat("Error processing simulation", sim, ":", e$message, "\n")
        # Continue with the next simulation
      })
    }
    
    # Merge chunk results with main counter
    if (nrow(chunk_lineup_counter) > 0) {
      if (nrow(lineup_counter) == 0) {
        lineup_counter <- chunk_lineup_counter
      } else {
        # Find existing and new lineup IDs
        existing_ids <- intersect(lineup_counter$LineupID, chunk_lineup_counter$LineupID)
        new_ids <- setdiff(chunk_lineup_counter$LineupID, lineup_counter$LineupID)
        
        # Update existing lineups
        for (id in existing_ids) {
          lineup_counter[LineupID == id, `:=`(
            Count = Count + chunk_lineup_counter[LineupID == id, Count],
            Top1Count = Top1Count + chunk_lineup_counter[LineupID == id, Top1Count],
            Top2Count = Top2Count + chunk_lineup_counter[LineupID == id, Top2Count],
            Top3Count = Top3Count + chunk_lineup_counter[LineupID == id, Top3Count],
            Top5Count = Top5Count + chunk_lineup_counter[LineupID == id, Top5Count]
          )]
          
          # For FanDuel, update captain if needed
          if (dfs_site == "fd") {
            chunk_captain <- chunk_lineup_counter[LineupID == id, CaptainID]
            if (!is.na(chunk_captain) && chunk_captain != "") {
              # Only update if the chunk has more top1 counts for this lineup
              chunk_top1 <- chunk_lineup_counter[LineupID == id, Top1Count]
              current_top1 <- lineup_counter[LineupID == id, Top1Count] - chunk_top1
              
              if (chunk_top1 > current_top1) {
                lineup_counter[LineupID == id, CaptainID := chunk_captain]
              }
            }
          }
        }
        
        # Add new lineups
        if (length(new_ids) > 0) {
          lineup_counter <- rbindlist(list(
            lineup_counter,
            chunk_lineup_counter[LineupID %in% new_ids]
          ))
        }
      }
    }
    
    # Free up memory from chunk processing
    rm(chunk_fighter_scores, chunk_lineup_counter)
    
    # Garbage collection
    if (chunk %% 3 == 0 || chunk == n_chunks) {
      cat("Processed optimal lineups chunk", chunk, "of", n_chunks, "\n")
      cleanup_memory()
    }
  }
  
  # Calculate frequencies
  if (nrow(lineup_counter) > 0) {
    lineup_counter[, Frequency := Count / n_sims * 100]
    
    # Sort by Top1Count (primary) and Top5Count (secondary)
    setorder(lineup_counter, -Top1Count, -Top5Count)
    
    # Return only top lineups
    return(head(lineup_counter, max_lineups))
  } else {
    return(lineup_counter)  # Return empty table with structure intact
  }
}


expand_lineup_details <- function(lineup_stats, fighter_data, dfs_site = "dk") {
  # Ensure we're working with data.tables
  if (!is.data.table(lineup_stats)) setDT(lineup_stats)
  if (!is.data.table(fighter_data)) setDT(fighter_data)
  
  # Create an efficient lookup table for fighter info
  # Use the right ID column based on DFS site
  id_column <- ifelse(dfs_site == "dk", "NameID", "FDNameID")
  
  # Make sure the lookup table uses the right columns
  # First check if columns exist
  required_cols <- c(id_column, "Name", "Salary", "OP", "Fight")
  for (col in required_cols) {
    if (!(col %in% names(fighter_data))) {
      stop(paste("Missing required column in fighter data:", col))
    }
  }
  
  # Create the lookup table with appropriate columns
  fighter_lookup <- fighter_data[, c(id_column, "Name", "Salary", "OP", "Fight"), with = FALSE]
  
  # Rename the ID column to a standard name for processing
  setnames(fighter_lookup, id_column, "FighterID")
  setkey(fighter_lookup, FighterID)
  
  # Process in chunks of 1000 lineups
  chunk_size <- 1000
  n_lineups <- nrow(lineup_stats)
  n_chunks <- ceiling(n_lineups / chunk_size)
  
  # Initialize results
  expanded_lineups <- data.table()
  
  # Determine roster size based on DFS site
  roster_size <- ifelse(dfs_site == "dk", 6, 6)  # 6 for both, but structure differs for FD
  
  for (chunk in 1:n_chunks) {
    start_idx <- (chunk - 1) * chunk_size + 1
    end_idx <- min(chunk * chunk_size, n_lineups)
    
    # Get chunk of lineups
    lineup_chunk <- lineup_stats[start_idx:end_idx]
    
    # Pre-allocate the expanded chunk
    expanded_chunk <- data.table(
      LineupID = character(end_idx - start_idx + 1),
      Count = integer(end_idx - start_idx + 1),
      Frequency = numeric(end_idx - start_idx + 1),
      Top1Count = integer(end_idx - start_idx + 1),
      Top2Count = integer(end_idx - start_idx + 1),
      Top3Count = integer(end_idx - start_idx + 1),
      Top5Count = integer(end_idx - start_idx + 1),
      TotalSalary = numeric(end_idx - start_idx + 1),
      AvgOwnership = numeric(end_idx - start_idx + 1),
      HasSameFight = logical(end_idx - start_idx + 1)
    )
    
    # For FanDuel, track captain
    if (dfs_site == "fd" && "CaptainID" %in% names(lineup_chunk)) {
      expanded_chunk[, CaptainIndex := integer(end_idx - start_idx + 1)]
    }
    
    # Add fighter columns - IMPORTANT: Include BOTH FighterID and Name columns
    for (j in 1:roster_size) {
      expanded_chunk[, paste0("FighterID", j) := character(end_idx - start_idx + 1)]
      expanded_chunk[, paste0("Name", j) := character(end_idx - start_idx + 1)]
      expanded_chunk[, paste0("Salary", j) := numeric(end_idx - start_idx + 1)]
      
      # For FanDuel, add IsCaptain column
      if (dfs_site == "fd") {
        expanded_chunk[, paste0("IsCaptain", j) := logical(end_idx - start_idx + 1)]
      }
    }
    
    # Expand each lineup in the chunk
    for (i in 1:nrow(lineup_chunk)) {
      lineup <- lineup_chunk[i]
      
      # Split the lineup ID to get fighter IDs
      fighter_ids <- strsplit(lineup$LineupID, "\\|")[[1]]
      
      # Get details for each fighter - use FighterID as the key
      fighters <- fighter_lookup[.(fighter_ids)]
      
      # Calculate total salary and ownership
      total_salary <- sum(fighters$Salary)
      avg_ownership <- mean(fighters$OP)
      
      # Check if any fighters are in the same fight
      same_fight <- any(duplicated(fighters$Fight))
      
      # For FanDuel, identify the captain
      captain_index <- NA_integer_
      if (dfs_site == "fd" && "CaptainID" %in% names(lineup)) {
        captain_id <- lineup$CaptainID
        if (!is.na(captain_id) && captain_id != "") {
          captain_index <- which(fighters$FighterID == captain_id)
          if (length(captain_index) == 0) captain_index <- NA_integer_
        }
      }
      
      # Sort fighters by salary
      setorder(fighters, -Salary)
      
      # Populate the expanded chunk row
      expanded_chunk[i, `:=`(
        LineupID = lineup$LineupID,
        Count = lineup$Count,
        Frequency = lineup$Frequency,
        Top1Count = lineup$Top1Count,
        Top2Count = lineup$Top2Count,
        Top3Count = lineup$Top3Count,
        Top5Count = lineup$Top5Count,
        TotalSalary = total_salary,
        AvgOwnership = avg_ownership,
        HasSameFight = same_fight
      )]
      
      # For FanDuel, set captain index
      if (dfs_site == "fd" && "CaptainIndex" %in% names(expanded_chunk)) {
        expanded_chunk[i, CaptainIndex := captain_index]
      }
      
      # Add fighter slots - IMPORTANT: Set both FighterID and Name
      for (j in 1:roster_size) {
        if (j <= nrow(fighters)) {
          expanded_chunk[i, paste0("FighterID", j) := fighters$FighterID[j]]
          expanded_chunk[i, paste0("Name", j) := fighters$Name[j]]
          expanded_chunk[i, paste0("Salary", j) := fighters$Salary[j]]
          
          # For FanDuel, set captain flag
          if (dfs_site == "fd") {
            is_captain <- !is.na(captain_index) && j == captain_index
            expanded_chunk[i, paste0("IsCaptain", j) := is_captain]
          }
        }
      }
    }
    
    # Add the expanded chunk to the results
    expanded_lineups <- rbindlist(list(expanded_lineups, expanded_chunk), fill = TRUE)
    
    # Cleanup memory
    if (chunk %% 5 == 0 || chunk == n_chunks) {
      cat("Processed expanded lineups chunk", chunk, "of", n_chunks, "\n")
      cleanup_memory()
    }
  }
  
  return(expanded_lineups)
}



analyze_fighter_performance <- function(fighter_scores, sim_results) {
  # Ensure we're working with data.tables
  if (!is.data.table(fighter_scores)) setDT(fighter_scores)
  if (!is.data.table(sim_results)) setDT(sim_results)
  
  # Get unique fighters with their basic information - use more efficient syntax
  fighters_base <- unique(fighter_scores[, .(NameID, Name, Salary, OP)])
  
  # Create an efficient mapping of fighters to fights
  fight_mapping <- unique(sim_results[, .(Name = Winner, Fight)])
  fight_mapping <- rbindlist(list(
    fight_mapping,
    unique(sim_results[, .(Name = Loser, Fight)])
  ))
  fight_mapping <- unique(fight_mapping)
  
  # Use efficient join with keys
  setkey(fighters_base, Name)
  setkey(fight_mapping, Name)
  fighters_base <- fighters_base[fight_mapping]
  
  # Calculate overall stats in a single step
  overall_stats <- fighter_scores[, .(
    AvgPoints = mean(FantasyPoints),
    MedianPoints = median(FantasyPoints),
    StdDev = sd(FantasyPoints)
  ), by = NameID]
  
  # Calculate win/loss percentages in a single pass
  n_sims_per_fighter <- uniqueN(sim_results$SimID)
  
  # Process win stats
  win_stats <- sim_results[, .(
    Wins = .N,
    WinPct = .N / n_sims_per_fighter * 100
  ), by = .(Name = Winner)]
  
  # Process loss stats - we'll still calculate this but won't include it in the final output
  loss_stats <- sim_results[, .(
    Losses = .N,
    LossPct = .N / n_sims_per_fighter * 100
  ), by = .(Name = Loser)]
  
  # Merge stats efficiently using keys
  setkey(fighters_base, Name)
  setkey(win_stats, Name)
  setkey(loss_stats, Name)
  
  fighters_with_results <- fighters_base[win_stats]
  fighters_with_results <- fighters_with_results[loss_stats]
  
  # Fill NA values with zeros
  fighters_with_results[is.na(Wins), Wins := 0]
  fighters_with_results[is.na(Losses), Losses := 0]
  fighters_with_results[is.na(WinPct), WinPct := 0]
  fighters_with_results[is.na(LossPct), LossPct := 0]
  
  # More efficient calculation of scores in wins and losses
  fighter_results <- rbindlist(list(
    sim_results[, .(SimID, Name = Winner, Result = "Win")],
    sim_results[, .(SimID, Name = Loser, Result = "Loss")]
  ))
  
  setkey(fighter_scores, SimID, Name)
  setkey(fighter_results, SimID, Name)
  fighter_scores_with_results <- fighter_scores[fighter_results]
  
  # Calculate win/loss stats
  win_loss_stats <- fighter_scores_with_results[, .(
    MedianWinPoints = median(FantasyPoints[Result == "Win"], na.rm = TRUE),
    MedianLossPoints = median(FantasyPoints[Result == "Loss"], na.rm = TRUE)
  ), by = .(NameID)]
  
  # Replace NaN values with NA for cleaner display
  win_loss_stats[is.nan(MedianWinPoints), MedianWinPoints := NA]
  win_loss_stats[is.nan(MedianLossPoints), MedianLossPoints := NA]
  
  # Merge efficiently
  setkey(fighters_with_results, NameID)
  setkey(overall_stats, NameID)
  setkey(win_loss_stats, NameID)
  
  final_stats <- fighters_with_results[overall_stats]
  final_stats <- final_stats[win_loss_stats]
  
  # Select and order columns according to the new requirements
  # Rename OP to OwnProj and keep it as decimal (don't multiply by 100)
  result <- final_stats[, .(
    Name, Fight, Salary, 
    OwnProj = OP, # Rename OP to OwnProj but keep as decimal
    MedianPoints, AvgPoints, # Put these first in projection stats
    WinPct, MedianWinPoints, MedianLossPoints # Win stats after
  )]
  
  # Sort by average points
  setorder(result, -AvgPoints)
  
  return(result)
}


# Fixed analyze_fight_outcomes function
analyze_fight_outcomes <- function(sim_results, name_id_mapping) {
  # Convert to data.table if not already
  if (!is.data.table(sim_results)) setDT(sim_results)
  if (!is.data.table(name_id_mapping)) {
    warning("No name-ID mapping provided, using default values")
    setDT(name_id_mapping)
  }
  
  # Get unique simulation IDs for denominator calculation
  n_sims_per_fighter <- uniqueN(sim_results$SimID)
  
  # Preprocess the Method column to standardize decision and quick finish methods
  sim_results[, ProcessedMethod := Method]
  
  # Handle various Decision formats - map all decision types to "Decision"
  sim_results[grep("Decision", ProcessedMethod), ProcessedMethod := "Decision"]
  
  # Handle Quick R1 Finish - map to "R1 Finish"
  sim_results[grep("Quick R1 Finish", ProcessedMethod), ProcessedMethod := "R1 Finish"]
  
  # Calculate fight stats with the processed method column
  fight_stats <- sim_results[, .(
    TotalFights = .N,
    R1Wins = sum(ProcessedMethod == "R1 Finish"),
    R2Wins = sum(ProcessedMethod == "R2 Finish"),
    R3Wins = sum(ProcessedMethod == "R3 Finish"),
    R4Wins = sum(ProcessedMethod == "R4 Finish"),
    R5Wins = sum(ProcessedMethod == "R5 Finish"),
    DecWins = sum(ProcessedMethod == "Decision")
  ), by = .(Fighter = Winner)]
  
  # If we have a valid mapping, merge with it
  if (nrow(name_id_mapping) > 0 && all(c("Name", "FighterID", "Salary", "OP") %in% names(name_id_mapping))) {
    # Ensure key is set
    if (!haskey(name_id_mapping)) setkey(name_id_mapping, Name)
    
    # Merge with fighter data using Name column
    result <- merge(fight_stats, 
                    name_id_mapping[, .(Name, FighterID, Salary, OP)], 
                    by.x = "Fighter", 
                    by.y = "Name", 
                    all.x = TRUE)
    
    # Handle any fighters that didn't match
    result[is.na(Salary), Salary := 0]
    result[is.na(OP), OP := 0]
    result[is.na(FighterID), FighterID := paste0("UNK_", Fighter)]
    
  } else {
    # If no valid mapping, just add default columns
    result <- copy(fight_stats)
    result[, `:=`(
      FighterID = paste0("UNK_", Fighter),
      Salary = 0,
      OP = 0
    )]
  }
  
  # Calculate percentages
  result[, `:=`(
    WinPct = (TotalFights / n_sims_per_fighter) * 100,
    R1Pct = (R1Wins / n_sims_per_fighter) * 100,
    R2Pct = (R2Wins / n_sims_per_fighter) * 100,
    R3Pct = (R3Wins / n_sims_per_fighter) * 100,
    R4Pct = (R4Wins / n_sims_per_fighter) * 100,
    R5Pct = (R5Wins / n_sims_per_fighter) * 100,
    DecPct = (DecWins / n_sims_per_fighter) * 100,
    OwnProj = OP
  )]
  
  # Remove count columns to save memory
  result[, c("TotalFights", "R1Wins", "R2Wins", "R3Wins", "R4Wins", "R5Wins", "DecWins", "OP") := NULL]
  
  # Order by salary descending
  setorder(result, -Salary)
  
  return(result)
}


# =============================================
# UI DEFINITION
# =============================================

ui <- dashboardPage(
  dashboardHeader(title = "MMA DFS Simulator"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Input Check", tabName = "upload", icon = icon("upload")),
      menuItem("Fight Analysis", tabName = "fight_analysis", icon = icon("hand-rock")),
      menuItem("Fantasy Projections", tabName = "fantasy", icon = icon("calculator")),
      menuItem("Optimal Lineups", tabName = "optimal_lineups", icon = icon("trophy")),
      menuItem("Lineup Builder", tabName = "lineup_builder", icon = icon("tools"))
    ),
    
    # Input controls - always visible
    br(),
    fileInput("excel_file", "Upload Excel File",
              accept = c(".xlsx")),
    numericInput("n_sims", "Number of Simulations:",
                 value = 15000, min = 1000, max = 50000),
    radioButtons("dfs_site", "DFS Site:",
                 choices = c("DraftKings" = "dk", "FanDuel" = "fd"),
                 selected = "dk"),
    actionButton("run_sim", "Run Simulation", 
                 class = "btn-primary",
                 style = "margin: 15px;"),
    # Status indicator
    textOutput("upload_status")
  ),
  
  dashboardBody(
    tabItems(
      # Upload Tab
      tabItem(tabName = "upload",
              box(width = 12,
                  title = "Fighter Data",
                  DTOutput("fighter_data_preview")
              ),
              box(width = 12,
                  title = "Scoring Ranges",
                  DTOutput("scoring_ranges_preview")
              )
      ),
      
      # Fight Analysis Tab
      tabItem(tabName = "fight_analysis",
              fluidRow(
                box(width = 12,
                    title = "Fight Outcome Analysis",
                    DTOutput("fight_outcome_table")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Win Methods",
                    plotlyOutput("win_method_distribution", height = "600px")
                )
              )
      ),
      
      # Fantasy Projections Tab
      tabItem(tabName = "fantasy",
              fluidRow(
                box(width = 12,
                    title = "Fighter Fantasy Point Projections",
                    DTOutput("fantasy_projections"),
                    downloadButton('download_fantasy_projections', 'Download Projections')
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Fantasy Points vs Salary",
                    plotlyOutput("fantasy_points_salary")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Fighter Selection for Visualization",
                    selectizeInput("selected_fighters", "Select Fighters:",
                                   choices = NULL,
                                   selected = NULL,
                                   multiple = TRUE,
                                   options = list(maxItems = 10))
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Fantasy Points Distribution",
                    plotlyOutput("fighter_points_dist")
                )
              )
      ),
      
      # Optimal Lineups Tab
      tabItem(tabName = "optimal_lineups",
              fluidRow(
                box(width = 12,
                    title = "Optimal Lineups",
                    DTOutput("optimal_lineups_table"),
                    downloadButton('downloadOptimalLineups', 'Download Results')
                )
              )
      ),
      
      # Lineup Builder Tab
      tabItem(tabName = "lineup_builder",
              fluidRow(
                box(width = 12,
                    title = "Lineup Count Thresholds",
                    DTOutput("lineup_count_thresholds")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Lineup Filters",
                    fluidRow(
                      column(3,
                             # Top1 and Top2 filters
                             numericInput("min_top1_count", "min Top 1 Count:", 
                                          value = 0, min = 0),
                             numericInput("min_top2_count", "min Top 2 Count:", 
                                          value = 0, min = 0)
                      ),
                      column(3,
                             # Top3 and Top5 filters
                             numericInput("min_top3_count", "min Top 3 Count:", 
                                          value = 0, min = 0),
                             numericInput("min_top5_count", "min Top 5 Count:", 
                                          value = 0, min = 0)
                      ),
                      column(3,
                             # Exposure control and number of lineups
                             numericInput("global_max_exposure", "Maximum Fighter Exposure (%):", 
                                          value = 100, min = 0, max = 100),
                             numericInput("num_random_lineups", "Number of Lineups Needed:", 
                                          value = 20, min = 1, max = 150)
                      ),
                      column(3,
                             # Same fight exclusion option (MMA-specific)
                             checkboxInput("exclude_same_fight", "Exclude Lineups with Fighters from Same Fight", 
                                           value = FALSE)
                      )
                    ),
                    fluidRow(
                      column(12,
                             selectizeInput("excluded_fighters", "Exclude Fighters:",
                                            choices = NULL,
                                            multiple = TRUE,
                                            options = list(plugins = list('remove_button')))
                      )
                    ),
                    # Live updating stats about filtered pool
                    fluidRow(
                      column(6,
                             # Pool size display
                             div(class = "well well-sm",
                                 h4("Filtered Pool Statistics:"),
                                 textOutput("filtered_pool_size")
                             )
                      ),
                      column(6,
                             # Generate button and download
                             div(style = "margin-top: 20px;",
                                 actionButton("generate_lineups", "Randomize Lineups", 
                                              class = "btn-primary btn-lg", 
                                              style = "width: 100%;"),
                                 br(), br(),
                                 downloadButton("download_random_lineups", "Download Selected Lineups", 
                                                style = "width: 100%;")
                             )
                      )
                    )
                )
              ),
              # Add explanation box for fighter rates
              box(
                width = 12,
                title = "Understanding Fighter Rates",
                status = "info",
                solidHeader = TRUE,
                p("These rate statistics help you understand each fighter's presence in lineups:"),
                tags$ul(
                  tags$li(tags$strong("OptimalRate:"), "How often fighters appear in lineups that finished 1st (optimal) in at least one simulation"),
                  tags$li(tags$strong("EliteRate:"), "How often fighters appear in the top 200 lineups when ranked by Top1Count (with Top5Count as tiebreaker)"),
                  tags$li(tags$strong("FloorRate:"), "How often fighters appear in the top 1000 lineups when ranked by Top5Count"),
                  tags$li(tags$strong("AppearanceRate:"), "How often fighters appear across all lineups in the full pool"),
                  tags$li(tags$strong("Exposure:"), "How often fighters appear in your selected randomized lineups")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Fighter Exposure Analysis",
                    DTOutput("driver_exposure_table")
                )
              ),
              fluidRow(
                box(width = 12,
                    title = "Generated Random Lineups",
                    DTOutput("random_lineups_table"),
                    br()
                )
              )
      )
    )
  )
)

# =============================================
# SERVER LOGIC WITH MEMORY OPTIMIZATIONS
# =============================================

server <- function(input, output, session) {
  
  schedule_memory_cleanup(session, interval_seconds = 120)
  # Initialize reactive values with memory management functions
  rv <- initialize_reactive_values()
  
  
  # File upload handling
  observeEvent(input$excel_file, {
    req(input$excel_file)
    
    # Clear previous data to free memory
    rv$clean_simulation_data()
    
    tryCatch({
      rv$input_data <- read_input_file(input$excel_file$datapath)
      rv$file_uploaded <- TRUE
      
      # Render minimal previews (limited rows) to save memory
      output$fighter_data_preview <- renderDT({
        req(rv$input_data)
        
        # Get appropriate fighter data based on selected site
        fighter_data <- if(input$dfs_site == "dk") {
          rv$input_data$fighters_dk
        } else {
          rv$input_data$fighters_fd
        }
        
        rv$name_id_mapping <- create_name_id_mapping(fighter_data)
        
        
        datatable(
          fighter_data,
          options = list(
            scrollX = TRUE, 
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            dom = 't'
          ),
          rownames = FALSE
        )
      })
      
      # Preview scoring ranges with limited rows - show the appropriate site's data
      output$scoring_ranges_preview <- renderDT({
        req(rv$input_data)
        
        if(input$dfs_site == "dk") {
          scoring_data <- rv$input_data$dk_scoring
          rv$current_fighter_data <- rv$input_data$fighters_dk
        } else {
          scoring_data <- rv$input_data$fd_scoring
          rv$current_fighter_data <- rv$input_data$fighters_fd
        }
        
        # Get the appropriate fighter data based on selected site
        fighter_data <- if(input$dfs_site == "dk") {
          rv$input_data$fighters_dk
        } else {
          rv$input_data$fighters_fd
        }
        
        # Limit preview to first 15 rows
        preview_data <- head(scoring_data, 15)
        
        datatable(
          preview_data,
          options = list(
            scrollX = TRUE, 
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            dom = 't'
          ),
          rownames = FALSE
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
    
    # Force cleanup to ensure memory is released
    cleanup_memory()
  })
  
  # Upload status text
  output$upload_status <- renderText({
    if (is.null(input$excel_file)) {
      return("Please upload an Excel file")
    } else if (!rv$simulation_run) {
      return("File uploaded! Click 'Run Simulation' to begin")
    } else {
      return("Simulation complete!")
    }
  })
  
  # Run simulation button - now with memory-optimized functions
  observeEvent(input$run_sim, {
    if (!rv$file_uploaded) {
      showModal(modalDialog(
        title = "Warning",
        "Please upload a file first",
        easyClose = TRUE
      ))
      return()
    }
    
    # Clear previous results before new simulation
    rv$simulation_results <- NULL
    rv$optimal_lineups <- NULL
    rv$lineup_builder <- NULL
    rv$random_lineups <- NULL
    rv$finishing_analysis <- NULL
    rv$dominator_analysis <- NULL
    rv$fantasy_analysis <- NULL
    
    # Force aggressive garbage collection
    aggressive_gc()
    
    req(rv$input_data)
    
    
    # Set appropriate parameters based on selected site
    salary_cap <- ifelse(input$dfs_site == "dk", 50000, 100)
    roster_size <- 6  # 6 for both sites (FD is 5+1 captain)
    
    withProgress(message = 'Running simulations...', value = 0, {
      
      # Step 1: Simulate fight outcomes using chunked approach
      incProgress(0.1, detail = "Simulating fights...")
      
      # Get the appropriate scoring data based on selected site
      scoring_data <- if(input$dfs_site == "dk") {
        rv$input_data$dk_scoring
      } else {
        rv$input_data$fd_scoring
      }
      
      rv$simulation_results <- simulate_fights(
        scoring_data, 
        n_sims = input$n_sims,
        batch_size = 250
      )
      
      # Step 2: Process fighter scores using chunked approach
      incProgress(0.4, detail = "Calculating fighter scores...")
      fighter_data <- if(input$dfs_site == "dk") {
        rv$input_data$fighters_dk
      } else {
        rv$input_data$fighters_fd
      }
      
      rv$fighter_scores <- process_fighter_scores(
        rv$simulation_results, 
        fighter_data
      )
      
      # Step 3: Analyze fighter performance
      incProgress(0.6, detail = "Analyzing fighter performance...")
      rv$fighter_stats <- analyze_fighter_performance(rv$fighter_scores, rv$simulation_results)
      
      # Step 4: Analyze fight outcomes
      incProgress(0.7, detail = "Analyzing fight outcomes...")
      rv$fight_stats <- analyze_fight_outcomes(rv$simulation_results, rv$name_id_mapping)
      
      # Step 5: Find optimal lineups with improved function
      incProgress(0.8, detail = "Finding optimal lineups...")
      top_lineups <- find_top_lineups(
        rv$fighter_scores, 
        salary_cap = salary_cap, 
        roster_size = roster_size,
        dfs_site = input$dfs_site
      )
      
      # Step 6: Expand lineup details with improved function
      incProgress(0.9, detail = "Processing lineup details...")
      rv$optimal_lineups <- expand_lineup_details(
        top_lineups, 
        fighter_data,  # Use the fighter_data we defined above
        dfs_site = input$dfs_site
      )
      
      rv$simulation_run <- TRUE
      
      # Add defensive checks before updating UI elements
      if (!is.null(rv$fighter_stats) && nrow(rv$fighter_stats) > 0 && 
          !is.null(rv$fighter_scores) && nrow(rv$fighter_scores) > 0) {
        
        # Build choices safely - ensure the names exist in both datasets
        fighter_names <- intersect(
          unique(rv$fighter_stats$Name),
          unique(rv$fighter_scores$Name)
        )
        
        # Only proceed if we have valid fighters to show
        if (length(fighter_names) > 0) {
          fighter_choices <- setNames(fighter_names, fighter_names)
          
          # Update fighter selection for visualization
          updateSelectizeInput(
            session, 
            "selected_fighters",
            choices = fighter_choices,
            selected = head(fighter_names, min(5, length(fighter_names)))
          )
        }
      }
      
      # Update excluded fighters selectInput
      if (!is.null(rv$optimal_lineups) && !is.null(rv$fighter_stats)) {
        # Get unique fighters from optimal lineups
        fighter_cols <- grep("^Name[1-6]$", names(rv$optimal_lineups), value = TRUE)
        all_fighters <- unique(unlist(rv$optimal_lineups[, ..fighter_cols]))
        
        # Create labels with ownership percentages if available
        fighter_labels <- sapply(all_fighters, function(fighter) {
          # Look up in fighter stats
          ownership <- rv$fighter_stats$OwnProj[rv$fighter_stats$Name == fighter]
          
          if (length(ownership) > 0 && !is.na(ownership)) {
            sprintf("%s (%.1f%%)", fighter, ownership * 100)
          } else {
            fighter
          }
        })
        
        # Update the selectInput with the new choices
        updateSelectizeInput(session, "excluded_fighters",
                             choices = setNames(all_fighters, fighter_labels),
                             selected = NULL)
      }
      
      # Final memory cleanup
      cleanup_memory()
      
      # Show success message
      showModal(modalDialog(
        title = "Success",
        "Simulation completed successfully!",
        easyClose = TRUE
      ))
    })
  })
  
  # =============================================
  # FIGHT ANALYSIS OUTPUTS
  # =============================================
  
  # Fight outcome table
  output$fight_outcome_table <- renderDT({
    req(rv$fight_stats)
    
    datatable(
      rv$fight_stats[, .(Fighter, Salary, OwnProj, WinPct, R1Pct, R2Pct, R3Pct, R4Pct, R5Pct, DecPct)],
      options = list(
        scrollX = TRUE,
        dom = 't', # Only show the table, no search, pagination, etc.
        ordering = TRUE,
        order = list(list(3, 'desc')), # Order by WinPct descending
        rownames = FALSE, # Remove row numbers
        paging = FALSE,
        searching = FALSE,
        info = FALSE
      ),
      rownames = FALSE # Remove row numbers - redundant but makes sure they're gone
    ) %>%
      formatCurrency('Salary', currency = "$", digits = 0) %>%
      formatRound(c('WinPct', 'R1Pct', 'R2Pct', 'R3Pct', 'R4Pct', 'R5Pct', 'DecPct'), digits = 1)
  })
  
  # Win method distribution plot
  output$win_method_distribution <- renderPlotly({
    req(rv$fight_stats)
    
    # Make a copy of the data for plotting
    plot_data <- as.data.frame(rv$fight_stats)
    
    # Order fighters by WinPct descending
    plot_data <- plot_data[order(-plot_data$WinPct),]
    
    # Get the fighter order for the x-axis
    fighter_order <- plot_data$Fighter
    
    # Create a longer format dataset for ggplot
    library(tidyr)
    
    # Select only the columns we need
    method_cols <- c("Fighter", "R1Pct", "R2Pct", "R3Pct", "R4Pct", "R5Pct", "DecPct")
    plot_data_long <- plot_data[, method_cols]
    
    # Convert to long format for proper stacking
    plot_data_long <- tidyr::pivot_longer(
      plot_data_long,
      cols = c("R1Pct", "R2Pct", "R3Pct", "R4Pct", "R5Pct", "DecPct"),
      names_to = "WinMethod",
      values_to = "Percentage"
    )
    
    # Create readable labels for the methods
    plot_data_long$WinMethod <- factor(
      plot_data_long$WinMethod,
      levels = c("R1Pct", "R2Pct", "R3Pct", "R4Pct", "R5Pct", "DecPct"),
      labels = c("R1 Finish", "R2 Finish", "R3 Finish", "R4 Finish", "R5 Finish", "Decision")
    )
    
    # Ensure Fighter is a factor with the right order
    plot_data_long$Fighter <- factor(plot_data_long$Fighter, levels = fighter_order)
    
    # Color palette that works well
    method_colors <- c(
      "R1 Finish" = "#E41A1C",    # Red
      "R2 Finish" = "#377EB8",    # Blue
      "R3 Finish" = "#4DAF4A",    # Green
      "R4 Finish" = "#984EA3",    # Purple
      "R5 Finish" = "#FF7F00",    # Orange
      "Decision" = "#FFFF33"      # Yellow
    )
    
    # Create base plot for win methods
    p <- ggplot(plot_data_long, aes(x = Fighter, y = Percentage, fill = WinMethod)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = method_colors, name = "Win Method") +
      labs(title = "Win Method Distribution",
           x = "Fighter",
           y = "Win Percentage") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
    
    # Convert to plotly for interactivity
    ggplotly(p, tooltip = c("y", "fill")) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # =============================================
  # FANTASY PROJECTIONS OUTPUTS
  # =============================================
  
  # Fantasy projections table
  output$fantasy_projections <- renderDT({
    req(rv$fighter_stats)
    
    # Select relevant columns for fantasy projections, reordered as requested
    projections <- rv$fighter_stats[, .(
      Name, Fight, Salary, 
      OwnProj,
      MedianPoints, AvgPoints, # Put median and avg points first
      WinPct, MedianWinPoints, MedianLossPoints
    )]
    
    datatable(
      projections,
      options = list(
        scrollX = TRUE,
        dom = 't', # Only show the table, no search, pagination, etc.
        ordering = TRUE,
        order = list(list(5, 'desc')), # Order by AvgPoints descending
        rownames = FALSE, # Remove row numbers
        paging = FALSE,
        searching = FALSE,
        info = FALSE
      ),
      rownames = FALSE # Remove row numbers - redundant but makes sure they're gone
    ) %>%
      formatCurrency('Salary', currency = "$", digits = 0) %>%
      formatRound('OwnProj', digits = 1) %>% 
      formatRound(c('WinPct'), digits = 1) %>%
      formatRound(c('MedianPoints', 'MedianWinPoints', 'MedianLossPoints', 'AvgPoints'), digits = 2)
  })
  
  # Download fantasy projections
  output$download_fantasy_projections <- downloadHandler(
    filename = function() {
      paste("mma_fantasy_projections_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      # Select relevant columns with updated structure
      projections <- rv$fighter_stats[, .(
        Name, Fight, Salary, 
        OwnProj,
        MedianPoints, AvgPoints,
        WinPct, MedianWinPoints, MedianLossPoints
      )]
      write.csv(projections, file, row.names = FALSE)
    }
  )
  
  # Fantasy points vs salary plot
  output$fantasy_points_salary <- renderPlotly({
    req(rv$fighter_stats)
    
    # Create the plot
    p <- ggplot(rv$fighter_stats, aes(x = Salary, y = AvgPoints, text = Name)) +
      geom_point(aes(size = WinPct, color = OwnProj), alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "darkgrey", linetype = "dashed") +
      scale_color_gradient(low = "blue", high = "red", name = "Ownership %") +
      scale_size(name = "Win %") +
      labs(title = "Fantasy Points vs Salary",
           x = "Salary ($)",
           y = "Average Fantasy Points") +
      theme_minimal()
    
    # Convert to plotly for interactivity
    ggplotly(p, tooltip = c("text", "x", "y", "size", "color"))
  })
  
  # Selected fighters for distribution plot
  output$fighter_points_dist <- renderPlotly({
    req(rv$fighter_scores, input$selected_fighters)
    
    # Filter for only selected fighters to improve performance
    selected_fighter_data <- rv$fighter_scores[Name %in% input$selected_fighters]
    
    # Create density plot
    p <- ggplot(selected_fighter_data, aes(x = FantasyPoints, fill = Name)) +
      geom_density(alpha = 0.5) +
      labs(title = "Fantasy Points Distribution",
           x = "Fantasy Points",
           y = "Density") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Convert to plotly
    ggplotly(p) %>%
      layout(legend = list(orientation = "h"))
  })
  
  # =============================================
  # OPTIMAL LINEUPS OUTPUTS
  # =============================================
  
  output$optimal_lineups_table <- renderDT({
    # Guard against NULL data
    if (is.null(rv$optimal_lineups) || nrow(rv$optimal_lineups) == 0) {
      return(datatable(data.frame(Message = "No optimal lineups found.")))
    }
    
    # Check which columns actually exist in the data
    available_cols <- names(rv$optimal_lineups)
    
    # Use only FighterID columns for display (remove Name columns)
    fighter_id_cols <- grep("^FighterID[1-6]$", available_cols, value = TRUE)
    
    # Select fighter columns which should always exist - ONLY INCLUDE IDs
    display_cols <- intersect(fighter_id_cols, available_cols)
    
    # For FanDuel, include captain columns if they exist
    captain_cols <- NULL
    if(input$dfs_site == "fd") {
      captain_cols <- intersect(paste0("IsCaptain", 1:6), names(rv$optimal_lineups))
      if(length(captain_cols) > 0) {
        display_cols <- c(display_cols, captain_cols)
      }
    }
    
    # Add Top1Count through Top5Count columns if they exist
    count_cols <- c("Top1Count", "Top2Count", "Top3Count", "Top5Count")
    count_cols <- intersect(count_cols, available_cols)
    display_cols <- c(display_cols, count_cols)
    
    # Add TotalSalary and other important columns
    if ("TotalSalary" %in% available_cols) {
      display_cols <- c(display_cols, "TotalSalary")
    }
    
    if ("HasSameFight" %in% available_cols) {
      display_cols <- c(display_cols, "HasSameFight")
    }
    
    # Make sure all columns exist before selecting them
    display_cols <- intersect(display_cols, available_cols)
    
    # Start by ensuring lineup data is a data.table
    lineups_data <- as.data.table(rv$optimal_lineups)
    
    # Create display data - Use proper data.table syntax
    display_data <- lineups_data[, .SD, .SDcols = display_cols]
    
    # Define sort column - determine based on available columns
    # Primary sort by Top1Count, secondary by Top5Count
    sort_columns <- list()
    
    if ("Top1Count" %in% names(display_data)) {
      sort_columns[[1]] <- list(which(names(display_data) == "Top1Count") - 1, "desc")
    }
    
    if ("Top5Count" %in% names(display_data)) {
      sort_columns[[length(sort_columns) + 1]] <- list(which(names(display_data) == "Top5Count") - 1, "desc")
    }
    
    if (length(sort_columns) == 0) {
      sort_columns[[1]] <- list(0, "asc")  # Default if no count columns
    }
    
    # Create the datatable with rownames = FALSE to remove row numbers
    dt <- datatable(display_data,
                    options = list(
                      pageLength = 50,
                      order = sort_columns,
                      scrollX = TRUE
                    ),
                    rownames = FALSE,  # This removes the row numbers
                    class = 'cell-border stripe'
    )
    
    # Apply formatting to all salary columns
    salary_cols <- grep("Salary", names(display_data), value = TRUE)
    if (length(salary_cols) > 0) {
      dt <- dt %>% formatCurrency(salary_cols, currency = "$", digits = 0)
    }
    
    # Format count columns - whole numbers (digits = 0)
    numeric_cols <- c("Top1Count", "Top2Count", "Top3Count", "Top5Count")
    numeric_cols <- intersect(numeric_cols, names(display_data))
    if (length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(numeric_cols, digits = 0)
      
      # Add color bars to count columns
      for (col in numeric_cols) {
        dt <- dt %>% formatStyle(
          col,
          background = styleColorBar(c(0, max(display_data[[col]], na.rm = TRUE)), 'lightblue'),
          backgroundSize = '98% 88%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      }
    }
    
    # Special formatting for HasSameFight column if it exists
    if ("HasSameFight" %in% names(display_data)) {
      dt <- dt %>% formatStyle(
        'HasSameFight',
        backgroundColor = styleEqual(c(TRUE), c('yellow'))
      )
    }
    
    # Highlight captains for FanDuel
    if(input$dfs_site == "fd" && length(captain_cols) > 0) {
      # Format captain columns as checkboxes
      dt <- dt %>% formatStyle(
        captain_cols,
        target = 'cell',
        fontWeight = styleEqual(c(TRUE), c('bold')),
        backgroundColor = styleEqual(c(TRUE), c('#FFC107'))
      )
      
      # Also highlight the corresponding ID columns
      for(i in 1:length(captain_cols)) {
        col_num <- as.integer(substr(captain_cols[i], 10, 10))
        id_col <- paste0("FighterID", col_num)
        
        if(id_col %in% display_cols) {
          dt <- dt %>% formatStyle(
            id_col,
            fontWeight = styleEqual(c(TRUE), c('bold')),
            color = styleEqual(c(TRUE), c('#000')),
            valueColumns = captain_cols[i]
          )
        }
      }
    }
    
    # Store the display columns order for use in download handler
    rv$optimal_display_cols <- display_cols
    
    return(dt)
  })
  
  
  
  output$downloadOptimalLineups <- downloadHandler(
    filename = function() {
      site_tag <- ifelse(input$dfs_site == "dk", "dk", "fd")
      paste("mma_optimal_lineups_", site_tag, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      # Convert to data.frame
      lineups_df <- as.data.frame(rv$optimal_lineups)
      
      # If we have display columns to use
      if (!is.null(rv$optimal_display_cols) && length(rv$optimal_display_cols) > 0) {
        # Find which columns actually exist in the data
        existing_cols <- names(lineups_df)
        cols_to_use <- rv$optimal_display_cols[rv$optimal_display_cols %in% existing_cols]
        
        # Select those columns
        if (length(cols_to_use) > 0) {
          result_df <- lineups_df[, cols_to_use, drop = FALSE]
          write.csv(result_df, file, row.names = FALSE)
        } else {
          # Fallback to using all columns
          write.csv(lineups_df, file, row.names = FALSE)
        }
      } else {
        # No display columns set, use all
        write.csv(lineups_df, file, row.names = FALSE)
      }
    }
  )
  
  # =============================================
  # LINEUP BUILDER FUNCTIONS
  # =============================================
  
  # Display thresholds for lineup counts - helps users understand filters
  output$lineup_count_thresholds <- renderDT({
    # Check if optimal lineups exist
    if(is.null(rv$optimal_lineups) || nrow(rv$optimal_lineups) == 0) {
      return(datatable(data.frame(Message = "No optimal lineups available."),
                       options = list(dom = 't')))
    }
    
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
      
      # Count lineups with each Top X Count at or above the threshold
      if ("Top1Count" %in% names(rv$optimal_lineups))
        count_data$Top1Count[i] <- sum(rv$optimal_lineups$Top1Count >= threshold)
      
      if ("Top2Count" %in% names(rv$optimal_lineups))
        count_data$Top2Count[i] <- sum(rv$optimal_lineups$Top2Count >= threshold)
      
      if ("Top3Count" %in% names(rv$optimal_lineups))
        count_data$Top3Count[i] <- sum(rv$optimal_lineups$Top3Count >= threshold)
      
      if ("Top5Count" %in% names(rv$optimal_lineups))
        count_data$Top5Count[i] <- sum(rv$optimal_lineups$Top5Count >= threshold)
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
  
  # Filtered pool size output
  output$filtered_pool_size <- renderText({
    req(rv$optimal_lineups)
    
    filters <- list(
      min_top1_count = input$min_top1_count,
      min_top2_count = input$min_top2_count,
      min_top3_count = input$min_top3_count,
      min_top5_count = input$min_top5_count,
      exclude_same_fight = input$exclude_same_fight,
      excluded_fighters = input$excluded_fighters
    )
    
    stats <- calculate_filtered_pool_stats(rv$optimal_lineups, filters)
    paste("Number of lineups in filtered pool:", stats$count)
  })
  
  # Generate lineups on button click
  observeEvent(input$generate_lineups, {
    req(rv$optimal_lineups, rv$fighter_stats)
    
    filters <- list(
      min_top1_count = input$min_top1_count,
      min_top2_count = input$min_top2_count,
      min_top3_count = input$min_top3_count,
      min_top5_count = input$min_top5_count,
      exclude_same_fight = input$exclude_same_fight,
      excluded_fighters = input$excluded_fighters,
      num_lineups = input$num_random_lineups,
      max_exposure = input$global_max_exposure
    )
    
    # Show progress indicator
    withProgress(message = 'Generating optimized lineups...', value = 0, {
      # Use the improved function for lineup generation with site parameter
      rv$generated_lineups <- generate_random_lineups_optimized(
        rv$optimal_lineups, 
        filters,
        dfs_site = input$dfs_site
      )
      
      # Force garbage collection after generation
      cleanup_memory()
      
      if (is.null(rv$generated_lineups) || nrow(rv$generated_lineups) == 0) {
        showModal(modalDialog(
          title = "Error",
          "No lineups match the selected filters",
          easyClose = TRUE
        ))
      }
    })
  })
  
  # Fighter exposure analysis table
  output$driver_exposure_table <- renderDT({
    req(rv$optimal_lineups, rv$fighter_stats)
    
    exposure_data <- calculate_fighter_exposure(
      rv$optimal_lineups, 
      rv$fighter_stats,
      rv$generated_lineups
    )
    
    # Remove unwanted columns
    if("AvgPoints" %in% names(exposure_data)) exposure_data$AvgPoints <- NULL
    
    # If random lineups don't exist, remove the Exposure column
    if(is.null(rv$generated_lineups) || nrow(rv$generated_lineups) == 0) {
      if("Exposure" %in% names(exposure_data)) exposure_data$Exposure <- NULL
      if("Leverage" %in% names(exposure_data)) exposure_data$Leverage <- NULL
    }
    
    # Create datatable
    dt <- datatable(
      exposure_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(1, 'desc')),  # Sort by OptimalRate by default
        rownames = FALSE
      ),
      rownames = FALSE
    )
    
    # Apply formatting
    if("Salary" %in% names(exposure_data)) {
      dt <- dt %>% formatCurrency('Salary', currency = "$", digits = 0)
    }
    
    # Format numeric columns
    numeric_cols <- intersect(
      c('OptimalRate', 'EliteRate', 'FloorRate', 'AppearanceRate', 'Exposure', 'Leverage', 'Ownership', 'Proj'),
      names(exposure_data)
    )
    
    if(length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(numeric_cols, digits = 1)
    }
    
    return(dt)
  })
  
  output$random_lineups_table <- renderDT({
    req(rv$generated_lineups)
    
    display_data <- rv$generated_lineups
    
    # Explicitly define only fighter ID columns (not names)
    fighter_id_cols <- c("FighterID1", "FighterID2", "FighterID3", 
                         "FighterID4", "FighterID5", "FighterID6")
    
    # Ensure only existing columns are selected
    display_cols <- intersect(fighter_id_cols, names(display_data))
    
    # Add TotalSalary if it exists
    if("TotalSalary" %in% names(display_data)) {
      display_cols <- c(display_cols, "TotalSalary")
    }
    
    # Add HasSameFight flag if it exists
    if("HasSameFight" %in% names(display_data)) {
      display_cols <- c(display_cols, "HasSameFight")
    }
    
    # Add any count columns that exist
    count_cols <- intersect(c("Top1Count", "Top2Count", "Top3Count", "Top5Count"), 
                            names(display_data))
    display_cols <- c(display_cols, count_cols)
    
    # For FanDuel, include captain columns if they exist
    captain_cols <- NULL
    if(input$dfs_site == "fd") {
      captain_cols <- intersect(paste0("IsCaptain", 1:6), names(display_data))
      if(length(captain_cols) > 0) {
        display_cols <- c(display_cols, captain_cols)
      }
    }
    
    # Create the datatable
    dt <- datatable(
      display_data[, ..display_cols],  # Select specific columns using data.table syntax
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        order = list(list(0, 'asc')),  # Sort by first column
        rownames = FALSE
      ),
      rownames = FALSE
    )
    
    # Format TotalSalary
    if("TotalSalary" %in% display_cols) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", digits = 0)
    }
    
    # Format count columns
    if(length(count_cols) > 0) {
      dt <- dt %>% formatRound(count_cols, digits = 0)
    }
    
    # Special formatting for HasSameFight column if it exists
    if ("HasSameFight" %in% display_cols) {
      dt <- dt %>% formatStyle(
        'HasSameFight',
        backgroundColor = styleEqual(c(TRUE), c('yellow'))
      )
    }
    
    # Highlight captains for FanDuel
    if(input$dfs_site == "fd" && length(captain_cols) > 0) {
      # Format captain columns as checkboxes
      dt <- dt %>% formatStyle(
        captain_cols,
        target = 'cell',
        fontWeight = styleEqual(c(TRUE), c('bold')),
        backgroundColor = styleEqual(c(TRUE), c('#FFC107'))
      )
      
      # Also highlight the corresponding ID columns
      for(i in 1:length(captain_cols)) {
        col_num <- as.integer(substr(captain_cols[i], 10, 10))
        id_col <- paste0("FighterID", col_num)
        
        if(id_col %in% display_cols) {
          dt <- dt %>% formatStyle(
            id_col,
            fontWeight = styleEqual(c(TRUE), c('bold')),
            color = styleEqual(c(TRUE), c('#000')),
            valueColumns = captain_cols[i]
          )
        }
      }
    }
    
    # Store the display columns order for use in download handler
    rv$random_display_cols <- display_cols
    
    return(dt)
  })
  
  output$download_random_lineups <- downloadHandler(
    filename = function() {
      site_tag <- ifelse(input$dfs_site == "dk", "dk", "fd")
      paste("mma_random_lineups_", site_tag, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      # Convert to data.frame
      lineups_df <- as.data.frame(rv$generated_lineups)
      
      # If we have display columns to use
      if (!is.null(rv$random_display_cols) && length(rv$random_display_cols) > 0) {
        # Find which columns actually exist in the data
        existing_cols <- names(lineups_df)
        cols_to_use <- rv$random_display_cols[rv$random_display_cols %in% existing_cols]
        
        # Select those columns
        if (length(cols_to_use) > 0) {
          result_df <- lineups_df[, cols_to_use, drop = FALSE]
          write.csv(result_df, file, row.names = FALSE)
        } else {
          # Fallback to using all columns
          write.csv(lineups_df, file, row.names = FALSE)
        }
      } else {
        # No display columns set, use all
        write.csv(lineups_df, file, row.names = FALSE)
      }
    }
  )
  
  
  
  # Clean up resources when session ends
  session$onSessionEnded(function() {
    cleanup_memory()
  })
}


# =============================================
# RUN THE APPLICATION
# =============================================

shinyApp(ui = ui, server = server)