#' Calculate Distribution Metrics from Lineup Scores
#'
#' Takes scored lineups (from Phase 2) and calculates distribution-based
#' metrics: percentiles, win rates, and efficiency.
#'
#' @param lineup_scores Object from score_lineups_all_sims()
#' @param player_data Data frame with Name, Salary, Ownership
#' @param verbose Logical. Print progress
#'
#' @return Data frame with all metrics
#' @export
calculate_distribution_metrics <- function(
  lineup_scores,
  player_data,
  verbose = TRUE
) {
  
  if (verbose) cat("=== PHASE 3: CALCULATING DISTRIBUTION METRICS ===\n")
  
  # Extract components
  score_matrix <- lineup_scores$score_matrix
  lineups <- lineup_scores$lineups
  n_lineups <- nrow(score_matrix)
  n_sims <- ncol(score_matrix)
  
  if (verbose) {
    cat(sprintf("Processing %s lineups across %s simulations\n",
                format(n_lineups, big.mark = ","),
                format(n_sims, big.mark = ",")))
    cat("\n")
  }
  
  # ============================================================================
  # STEP 1: Calculate percentiles (VECTORIZED!)
  # ============================================================================
  
  if (verbose) cat("  - Calculating percentiles...\n")
  
  percentiles <- t(apply(score_matrix, 1, function(scores) {
    quantile(scores, probs = c(0.25, 0.50, 0.75, 0.90), na.rm = TRUE)
  }))
  
  colnames(percentiles) <- c("Pct25", "Pct50", "Pct75", "Pct90")
  
  # ============================================================================
  # STEP 2: Calculate win rates (VECTORIZED!)
  # ============================================================================
  
  if (verbose) cat("  - Calculating win rates...\n")
  
  win_rates <- calculate_win_rates_vectorized(score_matrix, n_sims)
  
  # ============================================================================
  # STEP 3: Calculate salary (VECTORIZED!)
  # ============================================================================
  
  if (verbose) cat("  - Calculating salaries...\n")
  
  total_salary <- calculate_total_salary_vectorized(
    player_matrix = as.matrix(lineups),
    player_data = player_data
  )
  
  # ============================================================================
  # STEP 4: Calculate ownership (VECTORIZED!)
  # ============================================================================
  
  if (verbose) cat("  - Calculating ownership...\n")
  
  ownership_metrics <- calculate_ownership_metrics_vectorized(
    player_matrix = as.matrix(lineups),
    player_data = player_data,
    roster_size = ncol(lineups)
  )
  
  # ============================================================================
  # STEP 5: Calculate efficiency
  # ============================================================================
  
  if (verbose) cat("  - Calculating efficiency...\n")
  
  # Efficiency = Top10Rate / (Ownership / 100)
  efficiency <- calculate_efficiency(
    top10_rate = win_rates$Top10Pct,
    cumulative_ownership = ownership_metrics$CumulativeOwnership
  )
  
  # ============================================================================
  # STEP 6: Combine everything
  # ============================================================================
  
  if (verbose) cat("  - Combining results...\n")
  
  result <- data.frame(
    # Player columns
    lineups,
    
    # Salary
    TotalSalary = total_salary,
    
    # Ownership
    CumulativeOwnership = ownership_metrics$CumulativeOwnership,
    GeometricMean = ownership_metrics$GeometricMean,
    
    # Percentiles
    percentiles,
    
    # Win rates
    WinRate = win_rates$WinRate,
    Top1Pct = win_rates$Top1Pct,
    Top5Pct = win_rates$Top5Pct,
    Top10Pct = win_rates$Top10Pct,
    Top20Pct = win_rates$Top20Pct,
    
    # Efficiency
    Efficiency = efficiency,
    
    stringsAsFactors = FALSE
  )
  
  # ============================================================================
  # STEP 7: Sort by efficiency (best value at top)
  # ============================================================================
  
  result <- result[order(-result$Efficiency), ]
  rownames(result) <- NULL
  
  if (verbose) {
    cat(sprintf("\nPhase 3 complete: %s lineups with %d metrics\n",
                format(nrow(result), big.mark = ","),
                ncol(result) - ncol(lineups)))
    cat("=== ALL PHASES COMPLETE ===\n\n")
  }
  
  return(result)
}


#' Calculate Win Rates (Vectorized)
#' @keywords internal
calculate_win_rates_vectorized <- function(score_matrix, n_sims) {
  
  n_lineups <- nrow(score_matrix)
  
  # For each sim, rank all lineups
  # This creates a rank matrix (lineups × sims)
  # Rank 1 = best lineup in that sim
  
  # Apply rank to each column (sim)
  rank_matrix <- apply(score_matrix, 2, function(sim_scores) {
    rank(-sim_scores, ties.method = "min")
  })
  
  # Calculate cutoffs
  cutoff_1pct <- ceiling(n_lineups * 0.01)
  cutoff_5pct <- ceiling(n_lineups * 0.05)
  cutoff_10pct <- ceiling(n_lineups * 0.10)
  cutoff_20pct <- ceiling(n_lineups * 0.20)
  
  # Count how many times each lineup was in each tier (VECTORIZED!)
  win_rate <- rowSums(rank_matrix == 1) / n_sims * 100
  top1pct <- rowSums(rank_matrix <= cutoff_1pct) / n_sims * 100
  top5pct <- rowSums(rank_matrix <= cutoff_5pct) / n_sims * 100
  top10pct <- rowSums(rank_matrix <= cutoff_10pct) / n_sims * 100
  top20pct <- rowSums(rank_matrix <= cutoff_20pct) / n_sims * 100
  
  return(data.frame(
    WinRate = win_rate,
    Top1Pct = top1pct,
    Top5Pct = top5pct,
    Top10Pct = top10pct,
    Top20Pct = top20pct,
    stringsAsFactors = FALSE
  ))
}


#' Calculate Efficiency Metric
#' @keywords internal
calculate_efficiency <- function(top10_rate, cumulative_ownership) {
  
  # Efficiency = Performance / Cost
  # Top10Rate is already a percentage (0-100)
  # CumulativeOwnership is also a percentage (e.g., 245%)
  
  # Convert ownership to ratio (245% -> 2.45)
  ownership_ratio <- cumulative_ownership / 100
  
  # Efficiency = Top10Rate / OwnershipRatio
  efficiency <- top10_rate / ownership_ratio
  
  # Handle division by zero
  efficiency[is.infinite(efficiency) | is.nan(efficiency)] <- NA_real_
  
  return(efficiency)
}


#' Calculate Total Salary (Vectorized - from metrics_calculation.R)
#' @keywords internal
calculate_total_salary_vectorized <- function(player_matrix, player_data) {
  
  # Create salary lookup table
  salary_lookup <- setNames(player_data$Salary, player_data$Name)
  
  # Look up all salaries at once (vectorized!)
  salary_matrix <- matrix(
    salary_lookup[player_matrix],
    nrow = nrow(player_matrix),
    ncol = ncol(player_matrix)
  )
  
  # Sum across rows (total salary per lineup)
  total_salary <- rowSums(salary_matrix, na.rm = TRUE)
  
  return(total_salary)
}


#' Calculate Ownership Metrics (Vectorized - from metrics_calculation.R)
#' @keywords internal
calculate_ownership_metrics_vectorized <- function(
  player_matrix,
  player_data,
  roster_size
) {
  
  # Check if ownership data exists
  if (!"Ownership" %in% names(player_data)) {
    # Return empty metrics
    return(data.frame(
      CumulativeOwnership = rep(NA_real_, nrow(player_matrix)),
      GeometricMean = rep(NA_real_, nrow(player_matrix)),
      stringsAsFactors = FALSE
    ))
  }
  
  # Create ownership lookup table
  ownership_lookup <- setNames(player_data$Ownership, player_data$Name)
  
  # Look up all ownerships at once (vectorized!)
  ownership_matrix <- matrix(
    ownership_lookup[player_matrix],
    nrow = nrow(player_matrix),
    ncol = ncol(player_matrix)
  )
  
  # Handle percentage conversion (if ownership is 0-1 instead of 0-100)
  if (max(ownership_matrix, na.rm = TRUE) <= 1) {
    ownership_matrix <- ownership_matrix * 100
  }
  
  # Cumulative ownership: sum across rows (FAST!)
  cumulative <- rowSums(ownership_matrix, na.rm = TRUE)
  
  # Geometric mean: exp(mean(log(x))) across rows (FAST!)
  # Handle zeros and NAs
  ownership_matrix[ownership_matrix <= 0 | is.na(ownership_matrix)] <- NA
  
  geometric <- exp(rowMeans(log(ownership_matrix), na.rm = TRUE))
  
  # If all values were NA, geometric will be NaN - convert to NA
  geometric[is.nan(geometric)] <- NA_real_
  
  return(data.frame(
    CumulativeOwnership = cumulative,
    GeometricMean = geometric,
    stringsAsFactors = FALSE
  ))
}
