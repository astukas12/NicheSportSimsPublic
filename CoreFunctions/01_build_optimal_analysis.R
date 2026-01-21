#' Build Complete Optimal Lineup Analysis
#'
#' Master function that runs all three phases:
#' 1. Find optimal lineups using LP
#' 2. Score all lineups across all sims
#' 3. Calculate distribution metrics
#'
#' @param sim_results Data frame with SimID, Name, Score, Salary
#' @param player_data Data frame with Name, Salary, Ownership
#' @param contest_config List with contest configuration
#' @param top_k Integer. Number of optimal lineups to find per sim (default: 10)
#' @param score_column Character. Column name for fantasy scores (default: "Score")
#' @param batch_size Integer. Sims to process at once (default: 50)
#' @param verbose Logical. Print progress (default: TRUE)
#' @param parallel Logical. Use parallel processing (default: TRUE)
#' @param n_cores Integer. Number of cores (default: auto-detect)
#'
#' @return Data frame with lineups and distribution metrics
#' @export
#'
#' @examples
#' \dontrun{
#' result <- build_optimal_analysis(
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
#'   ),
#'   top_k = 10,
#'   score_column = "DKScore"
#' )
#' }
build_optimal_analysis <- function(
  sim_results,
  player_data,
  contest_config,
  top_k = 10,
  score_column = "Score",
  batch_size = 50,
  verbose = TRUE,
  parallel = TRUE,
  n_cores = NULL
) {
  
  total_start_time <- Sys.time()
  
  if (verbose) {
    cat("\n")
    cat("╔════════════════════════════════════════════════════════════╗\n")
    cat("║   OPTIMAL LINEUP BUILDER - FULL DISTRIBUTION ANALYSIS      ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n")
    cat("\n")
  }
  
  # ============================================================================
  # PHASE 1: FIND OPTIMAL LINEUPS
  # ============================================================================
  
  if (verbose) {
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("PHASE 1: Finding Optimal Lineups\n")
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
  }
  
  phase1_start <- Sys.time()
  
  optimal_lineups <- find_optimal_lineups(
    sim_results = sim_results,
    player_data = player_data,
    contest_config = contest_config,
    top_k = top_k,
    score_column = score_column,
    batch_size = batch_size,
    verbose = verbose
  )
  
  phase1_time <- as.numeric(difftime(Sys.time(), phase1_start, units = "secs"))
  
  if (is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    stop("No optimal lineups found in Phase 1")
  }
  
  if (verbose) {
    cat(sprintf("✓ Phase 1 complete: %s unique lineups | %.1f seconds\n\n",
                format(nrow(optimal_lineups), big.mark = ","),
                phase1_time))
  }
  
  # ============================================================================
  # PHASE 2: SCORE ALL LINEUPS ACROSS ALL SIMS
  # ============================================================================
  
  if (verbose) {
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("PHASE 2: Scoring All Lineups Across All Simulations\n")
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
  }
  
  phase2_start <- Sys.time()
  
  lineup_scores <- score_lineups_all_sims(
    optimal_lineups = optimal_lineups,
    sim_results = sim_results,
    score_column = score_column,
    verbose = verbose
  )
  
  phase2_time <- as.numeric(difftime(Sys.time(), phase2_start, units = "secs"))
  
  if (verbose) {
    cat(sprintf("✓ Phase 2 complete: %.1f seconds\n\n", phase2_time))
  }
  
  # ============================================================================
  # PHASE 3: CALCULATE DISTRIBUTION METRICS
  # ============================================================================
  
  if (verbose) {
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    cat("PHASE 3: Calculating Distribution Metrics\n")
    cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
  }
  
  phase3_start <- Sys.time()
  
  final_result <- calculate_distribution_metrics(
    lineup_scores = lineup_scores,
    player_data = player_data,
    verbose = verbose
  )
  
  phase3_time <- as.numeric(difftime(Sys.time(), phase3_start, units = "secs"))
  
  # ============================================================================
  # SUMMARY
  # ============================================================================
  
  total_time <- as.numeric(difftime(Sys.time(), total_start_time, units = "secs"))
  
  if (verbose) {
    cat("\n")
    cat("╔════════════════════════════════════════════════════════════╗\n")
    cat("║                      COMPLETE                              ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n")
    cat("\n")
    cat(sprintf("  Total lineups: %s\n", format(nrow(final_result), big.mark = ",")))
    cat(sprintf("  Total time: %.1f seconds\n", total_time))
    cat(sprintf("    Phase 1 (Find): %.1f sec (%.0f%%)\n", 
                phase1_time, phase1_time/total_time*100))
    cat(sprintf("    Phase 2 (Score): %.1f sec (%.0f%%)\n", 
                phase2_time, phase2_time/total_time*100))
    cat(sprintf("    Phase 3 (Metrics): %.1f sec (%.0f%%)\n", 
                phase3_time, phase3_time/total_time*100))
    cat("\n")
    cat("  Top lineup by efficiency:\n")
    cat(sprintf("    Players: %s\n", paste(final_result[1, 1:contest_config$roster_size], collapse = ", ")))
    cat(sprintf("    Efficiency: %.2f\n", final_result$Efficiency[1]))
    cat(sprintf("    Top 10%% Rate: %.1f%%\n", final_result$Top10Pct[1]))
    cat(sprintf("    Ownership: %.1f%%\n", final_result$CumulativeOwnership[1]))
    cat("\n")
  }
  
  return(final_result)
}
