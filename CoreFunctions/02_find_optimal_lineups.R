#' Find Optimal Lineups from Simulation Results
#'
#' Uses linear programming to find the top K optimal lineups from each simulation.
#' Implements hash table tracking, vectorized metrics calculation, and PPD-based
#' candidate filtering for maximum performance.
#'
#' @param sim_results Data frame with columns: SimID, Name, Score, Salary
#' @param player_data Data frame with player info: Name, Salary, Ownership (optional)
#' @param contest_config List with contest configuration
#' @param top_k Integer. Number of optimal lineups to find per simulation (default: 10)
#' @param score_column Character. Column name for fantasy scores (default: "Score")
#' @param batch_size Integer. Process N simulations at a time (default: 50)
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @return Data frame with optimal lineups and metrics
#' @export
#'
#' @examples
#' optimal <- find_optimal_lineups(
#'   sim_results = mma_sims,
#'   player_data = dk_salaries,
#'   contest_config = list(
#'     type = "simple",
#'     roster_size = 6,
#'     salary_cap = 50000,
#'     positions = paste0("Fighter", 1:6),
#'     optimization = list(
#'       candidate_filter = TRUE,
#'       top_by_score = 20,
#'       top_by_ppd = 20
#'     )
#'   )
#' )
find_optimal_lineups <- function(
    sim_results,
    player_data,
    contest_config,
    top_k = 10,
    score_column = "Score",
    batch_size = 50,
    verbose = TRUE
) {
  
  # ============================================================================
  # PHASE 1: VALIDATION AND SETUP
  # ============================================================================
  
  if (verbose) cat("=== OPTIMAL LINEUP FINDER ===\n")
  
  # Validate inputs
  validate_inputs(sim_results, player_data, contest_config, score_column)
  
  # Extract config parameters
  roster_size <- contest_config$roster_size
  salary_cap <- contest_config$salary_cap
  use_filter <- contest_config$optimization$candidate_filter %||% TRUE
  top_by_score <- contest_config$optimization$top_by_score %||% 20
  top_by_ppd <- contest_config$optimization$top_by_ppd %||% 20
  
  # Get simulation IDs
  sim_ids <- unique(sim_results$SimID)
  n_sims <- length(sim_ids)
  
  if (verbose) {
    cat(sprintf("Processing %s simulations\n", format(n_sims, big.mark = ",")))
    cat(sprintf("Finding top %d lineups per simulation\n", top_k))
    cat(sprintf("Roster size: %d | Salary cap: $%s\n", 
                roster_size, format(salary_cap, big.mark = ",")))
    if (use_filter) {
      cat(sprintf("Candidate filtering: ON (top %d by score + top %d by PPD)\n",
                  top_by_score, top_by_ppd))
    } else {
      cat("Candidate filtering: OFF (using all players)\n")
    }
    cat("\n")
  }
  
  # Convert to data.table for performance
  require(data.table)
  sim_dt <- as.data.table(sim_results)
  
  # ============================================================================
  # PHASE 2: LINEUP OPTIMIZATION
  # ============================================================================
  
  if (verbose) cat("Finding optimal lineups...\n")
  
  # Initialize lineup tracker (hash table for O(1) lookup)
  lineup_tracker <- new.env(hash = TRUE, parent = emptyenv())
  
  # Process simulations in batches for memory efficiency
  n_batches <- ceiling(n_sims / batch_size)
  
  for (batch in 1:n_batches) {
    batch_start_time <- Sys.time()
    
    # Define batch range
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, n_sims)
    batch_sim_ids <- sim_ids[start_idx:end_idx]
    
    # Extract data for this batch
    batch_data <- sim_dt[SimID %in% batch_sim_ids]
    
    # Process each simulation in this batch
    for (sim_id in batch_sim_ids) {
      sim_data <- batch_data[SimID == sim_id]
      
      # Apply candidate filtering if enabled
      if (use_filter) {
        candidates <- filter_candidates(
          sim_data, 
          score_column = score_column,
          top_by_score = top_by_score,
          top_by_ppd = top_by_ppd
        )
      } else {
        candidates <- sim_data
      }
      
      # Find top K lineups for this simulation
      find_top_k_lineups(
        candidates = candidates,
        score_column = score_column,
        roster_size = roster_size,
        salary_cap = salary_cap,
        top_k = top_k,
        lineup_tracker = lineup_tracker,
        sim_id = sim_id
      )
    }
    
    # Progress reporting
    if (verbose && (batch %% 10 == 0 || batch == n_batches)) {
      elapsed <- as.numeric(difftime(Sys.time(), batch_start_time, units = "secs"))
      pct_complete <- (end_idx / n_sims) * 100
      lineups_found <- length(ls(lineup_tracker))
      
      cat(sprintf("Batch %d/%d complete (%.1f%%) | %s unique lineups found | %.1f sec\n",
                  batch, n_batches, pct_complete, 
                  format(lineups_found, big.mark = ","), elapsed))
    }
    
    # Periodic garbage collection
    if (batch %% 10 == 0) {
      gc(verbose = FALSE)
    }
  }
  
  if (verbose) {
    cat(sprintf("\nOptimization complete: %s unique lineups found\n\n",
                format(length(ls(lineup_tracker)), big.mark = ",")))
  }
  
  # ============================================================================
  # PHASE 3: EXTRACT UNIQUE LINEUPS (no metrics yet - that comes in Phase 2)
  # ============================================================================
  
  if (verbose) cat("\nExtracting unique lineups...\n")
  
  # Get all lineup keys
  lineup_keys <- ls(lineup_tracker)
  n_lineups <- length(lineup_keys)
  
  if (n_lineups == 0) {
    warning("No lineups found!")
    return(NULL)
  }
  
  # Split into player matrix
  player_matrix <- do.call(rbind, strsplit(lineup_keys, "\\|", fixed = TRUE))
  
  # Set column names (handle mismatches gracefully)
  n_cols <- ncol(player_matrix)
  if(length(contest_config$positions) == n_cols) {
    colnames(player_matrix) <- contest_config$positions
  } else {
    # Generate generic column names if mismatch
    colnames(player_matrix) <- paste0("Player", 1:n_cols)
  }
  
  # Create basic data frame (just players)
  result <- as.data.frame(player_matrix, stringsAsFactors = FALSE)
  
  if (verbose) {
    cat(sprintf("Phase 1 complete: %s unique lineups found\n",
                format(n_lineups, big.mark = ",")))
    cat("Ready for Phase 2 (scoring across all sims)\n\n")
  }
  
  return(result)
}


# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Null coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


#' Validate inputs
#' @keywords internal
validate_inputs <- function(sim_results, player_data, contest_config, score_column) {
  
  # Check sim_results
  if (!is.data.frame(sim_results)) {
    stop("sim_results must be a data frame")
  }
  
  required_cols <- c("SimID", "Name", score_column, "Salary")
  missing_cols <- setdiff(required_cols, names(sim_results))
  if (length(missing_cols) > 0) {
    stop(sprintf("sim_results missing required columns: %s",
                 paste(missing_cols, collapse = ", ")))
  }
  
  # Check player_data
  if (!is.data.frame(player_data)) {
    stop("player_data must be a data frame")
  }
  
  if (!"Name" %in% names(player_data)) {
    stop("player_data must have 'Name' column")
  }
  
  # Check contest_config
  if (!is.list(contest_config)) {
    stop("contest_config must be a list")
  }
  
  required_config <- c("roster_size", "salary_cap", "positions")
  missing_config <- setdiff(required_config, names(contest_config))
  if (length(missing_config) > 0) {
    stop(sprintf("contest_config missing required fields: %s",
                 paste(missing_config, collapse = ", ")))
  }
  
  # Validate roster_size
  if (!is.numeric(contest_config$roster_size) || contest_config$roster_size < 1) {
    stop("contest_config$roster_size must be a positive integer")
  }
  
  # Validate salary_cap
  if (!is.numeric(contest_config$salary_cap) || contest_config$salary_cap <= 0) {
    stop("contest_config$salary_cap must be a positive number")
  }
  
  invisible(TRUE)
}


#' Filter candidates by score and PPD
#' @keywords internal
filter_candidates <- function(sim_data, score_column, top_by_score, top_by_ppd) {
  
  n_players <- nrow(sim_data)
  
  # If fewer players than filter size, return all
  if (n_players <= (top_by_score + top_by_ppd)) {
    return(sim_data)
  }
  
  # Calculate PPD (Points Per $1000)
  sim_data$PPD <- sim_data[[score_column]] / (sim_data$Salary / 1000)
  
  # Get top N by raw score
  top_score_indices <- order(-sim_data[[score_column]])[1:min(top_by_score, n_players)]
  
  # Get top N by PPD
  top_ppd_indices <- order(-sim_data$PPD)[1:min(top_by_ppd, n_players)]
  
  # Combine and remove duplicates
  candidate_indices <- unique(c(top_score_indices, top_ppd_indices))
  
  # Return filtered candidates
  return(sim_data[candidate_indices, ])
}