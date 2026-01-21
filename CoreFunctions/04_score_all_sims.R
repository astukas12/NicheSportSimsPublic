#' Score All Lineups Across All Simulations
#'
#' Takes the unique lineups found in Phase 1 and scores each lineup
#' in every simulation to get full distribution data.
#'
#' @param optimal_lineups Data frame from find_optimal_lineups() with player columns
#' @param sim_results Data frame with SimID, Name, Score columns
#' @param score_column Character. Column name for fantasy scores
#' @param verbose Logical. Print progress
#'
#' @return Data frame with lineup scores for every sim (long format)
#' @export
score_lineups_all_sims <- function(
  optimal_lineups,
  sim_results,
  score_column = "Score",
  verbose = TRUE
) {
  
  if (verbose) cat("=== PHASE 2: SCORING ALL LINEUPS ACROSS ALL SIMS ===\n")
  
  require(data.table)
  
  # Convert to data.table for speed
  sim_dt <- as.data.table(sim_results)
  
  # Get dimensions
  n_lineups <- nrow(optimal_lineups)
  n_players_per_lineup <- ncol(optimal_lineups)
  sim_ids <- unique(sim_dt$SimID)
  n_sims <- length(sim_ids)
  
  if (verbose) {
    cat(sprintf("Lineups to score: %s\n", format(n_lineups, big.mark = ",")))
    cat(sprintf("Simulations: %s\n", format(n_sims, big.mark = ",")))
    cat(sprintf("Total scores to calculate: %s\n", 
                format(n_lineups * n_sims, big.mark = ",")))
    cat("\n")
  }
  
  # ============================================================================
  # STRATEGY: Vectorized scoring using matrix operations
  # ============================================================================
  
  # Pre-allocate result matrix (lineups × sims)
  score_matrix <- matrix(0, nrow = n_lineups, ncol = n_sims)
  
  # Process in batches for memory efficiency
  batch_size <- 1000
  n_batches <- ceiling(n_sims / batch_size)
  
  start_time <- Sys.time()
  
  for (batch in 1:n_batches) {
    
    # Define batch range
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, n_sims)
    batch_sim_ids <- sim_ids[start_idx:end_idx]
    
    # Extract data for this batch
    batch_data <- sim_dt[SimID %in% batch_sim_ids]
    
    # Score all lineups in this batch of sims
    batch_scores <- score_lineups_batch(
      lineups = optimal_lineups,
      sim_data = batch_data,
      sim_ids = batch_sim_ids,
      score_column = score_column
    )
    
    # Store in result matrix
    score_matrix[, start_idx:end_idx] <- batch_scores
    
    # Progress reporting
    if (verbose && (batch %% 5 == 0 || batch == n_batches)) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      pct_complete <- (end_idx / n_sims) * 100
      rate <- end_idx / elapsed
      eta <- (n_sims - end_idx) / rate
      
      cat(sprintf("Batch %d/%d (%.1f%%) | %.1f sims/sec | ETA: %.0f sec\n",
                  batch, n_batches, pct_complete, rate, eta))
    }
    
    # Periodic garbage collection
    if (batch %% 10 == 0) {
      gc(verbose = FALSE)
    }
  }
  
  if (verbose) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cat(sprintf("\nScoring complete: %.1f seconds (%.0f scores/sec)\n\n",
                elapsed, (n_lineups * n_sims) / elapsed))
  }
  
  # Return score matrix with lineup and sim IDs
  result <- list(
    score_matrix = score_matrix,
    lineup_ids = 1:n_lineups,
    sim_ids = sim_ids,
    lineups = optimal_lineups
  )
  
  class(result) <- c("lineup_scores", "list")
  
  return(result)
}


#' Score Lineups in a Batch of Simulations (Vectorized)
#' @keywords internal
score_lineups_batch <- function(lineups, sim_data, sim_ids, score_column) {
  
  n_lineups <- nrow(lineups)
  n_sims <- length(sim_ids)
  n_players <- ncol(lineups)
  
  # Pre-allocate result matrix
  batch_scores <- matrix(0, nrow = n_lineups, ncol = n_sims)
  
  # Process each sim in the batch
  for (sim_idx in 1:n_sims) {
    sim_id <- sim_ids[sim_idx]
    
    # Get player scores for this sim
    sim_scores <- sim_data[SimID == sim_id]
    score_lookup <- setNames(sim_scores[[score_column]], sim_scores$Name)
    
    # For each lineup, sum the player scores (VECTORIZED!)
    for (lineup_idx in 1:n_lineups) {
      players <- as.character(lineups[lineup_idx, ])
      player_scores <- score_lookup[players]
      batch_scores[lineup_idx, sim_idx] <- sum(player_scores, na.rm = TRUE)
    }
  }
  
  return(batch_scores)
}


#' Even Faster Scoring (Full Vectorization)
#' @keywords internal
score_lineups_batch_ultra_fast <- function(lineups, sim_data, sim_ids, score_column) {
  
  n_lineups <- nrow(lineups)
  n_sims <- length(sim_ids)
  n_players <- ncol(lineups)
  
  # Create a 3D lookup: [player, sim] -> score
  # This allows complete vectorization
  
  all_players <- unique(unlist(lineups))
  
  # Build player-sim score matrix
  player_sim_matrix <- matrix(0, 
                               nrow = length(all_players), 
                               ncol = n_sims,
                               dimnames = list(all_players, sim_ids))
  
  # Fill in scores
  for (sim_idx in 1:n_sims) {
    sim_id <- sim_ids[sim_idx]
    sim_scores <- sim_data[SimID == sim_id]
    
    for (i in 1:nrow(sim_scores)) {
      player <- sim_scores$Name[i]
      if (player %in% all_players) {
        player_sim_matrix[player, sim_idx] <- sim_scores[[score_column]][i]
      }
    }
  }
  
  # Now score all lineups at once (FULLY VECTORIZED!)
  batch_scores <- matrix(0, nrow = n_lineups, ncol = n_sims)
  
  for (lineup_idx in 1:n_lineups) {
    players <- as.character(lineups[lineup_idx, ])
    # Sum scores for these players across all sims
    batch_scores[lineup_idx, ] <- colSums(player_sim_matrix[players, , drop = FALSE])
  }
  
  return(batch_scores)
}
