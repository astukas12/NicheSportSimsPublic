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

# Set up custom CSS for black and gold theme (matching tennis app)
custom_css <- "
  /* Override dashboard header colors */
  .skin-blue .main-header {
    background-color: #000000;
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
    border-left-color: #FFD700;
  }
  
  /* Customize box headers */
  .box.box-primary .box-header {
    background-color: #333333;
    color: #FFD700;
  }
  
  /* Style buttons */
  .btn-primary {
    background-color: #FFD700;
    border-color: #DAA520;
    color: #000000;
  }
  .btn-primary:hover, .btn-primary:focus {
    background-color: #DAA520;
    border-color: #B8860B;
    color: #000000;
  }
  
  /* Style tabs */
  .nav-tabs-custom > .nav-tabs > li.active {
    border-top-color: #FFD700;
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
FD_ROSTER_SIZE <- 6
DK_SALARY_CAP <- 50000
FD_SALARY_CAP <- 60000



read_golf_input_file <- function(file_path) {
  tryCatch({
    # Get all sheet names first
    sheet_names <- readxl::excel_sheets(file_path)
    
    # Always try to read Player sheet
    sheets <- list(
      Player = read_excel(file_path, sheet = "Player")
    )
    
    # Try to read DraftKings points (required)
    if ("DKPts" %in% sheet_names) {
      sheets$DKPts <- read_excel(file_path, sheet = "DKPts")
    } else {
      stop("DKPts sheet is required but not found")
    }
    
    # Try to read FanDuel points (optional)
    if ("FDPts" %in% sheet_names) {
      sheets$FDPts <- read_excel(file_path, sheet = "FDPts")
    } else {
      sheets$FDPts <- NULL
      message("FDPts sheet not found - running DraftKings only")
    }
    
    # Try to read Tiers (optional)
    if ("Tiers" %in% sheet_names) {
      sheets$Tiers <- read_excel(file_path, sheet = "Tiers")
    } else {
      sheets$Tiers <- NULL
      message("Tiers sheet not found - Tiers format not available")
    }
    
    # Identify available platforms based on columns
    has_dk <- "DKSalary" %in% colnames(sheets$Player)
    has_fd <- "FDSalary" %in% colnames(sheets$Player) && !is.null(sheets$FDPts)
    has_tiers <- !is.null(sheets$Tiers)
    
    # Create platform info
    platform_info <- list(
      has_draftkings = has_dk, 
      has_fanduel = has_fd,
      has_tiers = has_tiers
    )
    
    list(sheets = sheets, platform_info = platform_info)
  }, error = function(e) {
    stop(paste("Error reading Excel file:", e$message))
  })
}

process_golf_input_data <- function(input_data) {
  # Extract data components
  player_data <- input_data$sheets$Player
  dk_pts_data <- input_data$sheets$DKPts
  fd_pts_data <- input_data$sheets$FDPts  # This could be NULL now
  
  # Process player data
  processed_players <- as.data.table(player_data)
  
  # Convert relevant numeric columns efficiently - updated with new columns
  numeric_cols <- c("W", "T5", "T10", "T20", "T30", "T40", "Cut", "DKSalary", "FDSalary", "DKOP", "FDOP")
  
  for (col in numeric_cols) {
    if (col %in% names(processed_players)) {
      processed_players[, (col) := as.numeric(get(col))]
    }
  }
  
  # Process scoring tables
  processed_dk_pts <- if (!is.null(dk_pts_data)) {
    dk_dt <- as.data.table(dk_pts_data)
    dk_dt[, Rank := as.numeric(Rank)]
    dk_dt[, Score := as.numeric(Score)]
    dk_dt <- dk_dt[!is.na(Rank) & !is.na(Score)]
    setkey(dk_dt, Rank)
    dk_dt
  } else {
    data.table()
  }
  
  processed_fd_pts <- if (!is.null(fd_pts_data)) {
    fd_dt <- as.data.table(fd_pts_data)
    fd_dt[, Rank := as.numeric(Rank)]
    fd_dt[, Score := as.numeric(Score)]
    fd_dt <- fd_dt[!is.na(Rank) & !is.na(Score)]
    setkey(fd_dt, Rank)
    fd_dt
  } else {
    data.table()  # Empty table if no FD data
  }
  
  # Return processed data
  list(
    players = processed_players,
    dk_points = processed_dk_pts,
    fd_points = processed_fd_pts
  )
}

# Read and process tiers data
read_tiers_data <- function(input_data) {
  if ("Tiers" %in% names(input_data$sheets)) {
    tiers_dt <- as.data.table(input_data$sheets$Tiers)
    
    # Validate required columns
    required_cols <- c("TiersName", "Name", "ID", "Tier")
    if (!all(required_cols %in% names(tiers_dt))) {
      warning("Tiers sheet missing required columns")
      return(NULL)
    }
    
    # Convert Tier to numeric
    tiers_dt[, Tier := as.numeric(Tier)]
    
    # Validate tiers are 1-6
    if (any(tiers_dt$Tier < 1 | tiers_dt$Tier > 6)) {
      warning("Tiers must be between 1 and 6")
      return(NULL)
    }
    
    return(tiers_dt)
  }
  return(NULL)
}

# Memory management and optimization functions
options(datatable.optimize = Inf)

cleanup_memory <- function(verbose = FALSE) {
  gc(verbose = verbose, full = TRUE, reset = TRUE)
  
  if (.Platform$OS.type == "unix") {
    try(system("sync"), silent = TRUE)
  }
}



precompute_golf_distributions <- function(players_dt, cut_line = 65, no_cut = FALSE) {
  n_players <- nrow(players_dt)
  
  # Extract probability columns - now with T30, T40, Cut
  if (no_cut) {
    # No cut tournament - use all columns except Cut
    prob_cols <- c("W", "T5", "T10", "T20", "T30", "T40")
  } else {
    # Tournament with cut - include Cut column
    prob_cols <- c("W", "T5", "T10", "T20", "T30", "T40", "Cut")
  }
  
  # Check which columns actually exist
  available_cols <- intersect(prob_cols, names(players_dt))
  prob_matrix <- as.matrix(players_dt[, ..available_cols])
  
  # Handle NAs efficiently
  prob_matrix[is.na(prob_matrix)] <- 0
  
  # Pre-compute marginal probabilities for all players
  if (no_cut) {
    # No cut: 6 categories + remaining
    marginal_probs <- matrix(0, nrow = n_players, ncol = 7)
    position_ranges <- list(
      c(1),                    # Win
      c(2:5),                  # T5 (2nd-5th) 
      c(6:10),                 # T10 (6th-10th)
      c(11:20),                # T20 (11th-20th)
      c(21:30),                # T30 (21st-30th)
      c(31:40),                # T40 (31st-40th)
      c(41:n_players)          # Remaining positions
    )
  } else {
    # With cut: 6 categories + remaining cut-makers + missed cut
    marginal_probs <- matrix(0, nrow = n_players, ncol = 8)
    position_ranges <- list(
      c(1),                    # Win
      c(2:5),                  # T5 (2nd-5th)
      c(6:10),                 # T10 (6th-10th) 
      c(11:20),                # T20 (11th-20th)
      c(21:30),                # T30 (21st-30th)
      c(31:40),                # T40 (31st-40th)
      c(41:cut_line),          # Remaining cut-makers
      c((cut_line+1):n_players) # Missed cut
    )
  }
  
  # Calculate marginal probabilities
  for (i in 1:n_players) {
    if (no_cut) {
      # No cut calculation
      cum_probs <- c(prob_matrix[i, ], 1.0)
      marg_probs <- diff(c(0, cum_probs))
    } else {
      # With cut calculation
      cut_prob <- if ("Cut" %in% available_cols) prob_matrix[i, "Cut"] else 0.8
      finish_probs <- prob_matrix[i, setdiff(available_cols, "Cut")]
      
      # Scale finish probabilities by cut probability
      scaled_finish_probs <- finish_probs * cut_prob
      cum_finish_probs <- c(scaled_finish_probs, cut_prob)  # Last element is total cut probability
      
      # Calculate marginals for cut-makers + missed cut
      finish_marg <- diff(c(0, cum_finish_probs))
      missed_cut_prob <- 1 - cut_prob
      
      marg_probs <- c(finish_marg, missed_cut_prob)
    }
    
    # Handle edge cases and normalize
    marg_probs[marg_probs < 0] <- 0
    sum_probs <- sum(marg_probs)
    if (sum_probs > 0) {
      marg_probs <- marg_probs / sum_probs
    } else {
      marg_probs <- rep(1/length(marg_probs), length(marg_probs))
    }
    
    marginal_probs[i, ] <- marg_probs
  }
  
  return(list(
    marginal_probs = marginal_probs,
    position_ranges = position_ranges,
    n_players = n_players,
    cut_line = cut_line,
    no_cut = no_cut
  ))
}

simulate_golf_positions_vectorized <- function(player_distributions, n_sims) {
  n_players <- player_distributions$n_players
  marginal_probs <- player_distributions$marginal_probs
  position_ranges <- player_distributions$position_ranges
  cut_line <- player_distributions$cut_line
  no_cut <- player_distributions$no_cut
  
  # Pre-allocate result matrix
  all_positions <- matrix(0, nrow = n_players, ncol = n_sims)
  
  # Generate all random numbers at once
  random_matrix <- matrix(runif(n_players * n_sims), nrow = n_players, ncol = n_sims)
  noise_matrix <- matrix(runif(n_players * n_sims, 0, 0.02), nrow = n_players, ncol = n_sims)
  
  # Pre-compute cumulative probabilities for all players
  cumulative_probs <- t(apply(marginal_probs, 1, cumsum))
  
  # Process simulations in batches
  batch_size <- min(500, n_sims)
  n_batches <- ceiling(n_sims / batch_size)
  
  for (batch in 1:n_batches) {
    start_col <- (batch - 1) * batch_size + 1
    end_col <- min(batch * batch_size, n_sims)
    batch_cols <- start_col:end_col
    
    for (sim in batch_cols) {
      if (no_cut) {
        # No cut - direct position assignment
        performance_scores <- numeric(n_players)
        
        for (i in 1:n_players) {
          random_val <- random_matrix[i, sim]
          pos_range_idx <- findInterval(random_val, c(0, cumulative_probs[i, ])) 
          pos_range_idx <- max(1, min(pos_range_idx, length(position_ranges)))
          
          pos_range <- position_ranges[[pos_range_idx]]
          if (length(pos_range) > 1) {
            sampled_pos <- sample(pos_range, 1)
          } else {
            sampled_pos <- pos_range[1]
          }
          
          performance_scores[i] <- sampled_pos + noise_matrix[i, sim]
        }
        
        all_positions[, sim] <- rank(performance_scores, ties.method = "random")
        
      } else {
        # Two-phase simulation: Cut determination then position assignment
        cut_makers <- logical(n_players)
        cut_maker_scores <- numeric(n_players)
        missed_cut_scores <- numeric(n_players)
        
        # Phase 1: Determine cut makers
        for (i in 1:n_players) {
          random_val <- random_matrix[i, sim]
          pos_range_idx <- findInterval(random_val, c(0, cumulative_probs[i, ]))
          pos_range_idx <- max(1, min(pos_range_idx, ncol(cumulative_probs)))
          
          # Last category is missed cut
          if (pos_range_idx == ncol(cumulative_probs)) {
            cut_makers[i] <- FALSE
            # For missed cut players, use their other probabilities for ordering
            other_score <- sum(marginal_probs[i, 1:(ncol(marginal_probs)-1)]) + noise_matrix[i, sim]
            missed_cut_scores[i] <- other_score
          } else {
            cut_makers[i] <- TRUE
            pos_range <- position_ranges[[pos_range_idx]]
            if (length(pos_range) > 1) {
              sampled_pos <- sample(pos_range, 1)
            } else {
              sampled_pos <- pos_range[1]
            }
            cut_maker_scores[i] <- sampled_pos + noise_matrix[i, sim]
          }
        }
        
        # ENFORCE CUT CONSTRAINTS: Ensure 65-80 players make cut
        n_cut_makers <- sum(cut_makers)
        
        if (n_cut_makers < 65) {
          # Too few cut-makers: promote players on the bubble
          # Get missed cut players sorted by their potential (highest probability first)
          missed_cut_idx <- which(!cut_makers)
          missed_cut_potential <- missed_cut_scores[missed_cut_idx]
          
          # Sort by potential (higher = better)
          sorted_order <- order(missed_cut_potential, decreasing = TRUE)
          players_to_promote <- missed_cut_idx[sorted_order[1:(65 - n_cut_makers)]]
          
          # Promote these players to cut-makers
          cut_makers[players_to_promote] <- TRUE
          # Assign them positions in the "bubble" range
          for (p in players_to_promote) {
            cut_maker_scores[p] <- cut_line - 5 + runif(1, 0, 10)  # Around cut line
          }
          
          n_cut_makers <- 65
        } else if (n_cut_makers > 80) {
          # Too many cut-makers: demote weakest performers
          # Get cut-makers sorted by performance (worst scores = highest position numbers)
          cut_maker_idx <- which(cut_makers)
          cut_maker_perf <- cut_maker_scores[cut_maker_idx]
          
          # Sort by performance (higher score = worse = candidate for demotion)
          sorted_order <- order(cut_maker_perf, decreasing = TRUE)
          players_to_demote <- cut_maker_idx[sorted_order[1:(n_cut_makers - 80)]]
          
          # Demote these players to missed cut
          cut_makers[players_to_demote] <- FALSE
          for (p in players_to_demote) {
            missed_cut_scores[p] <- marginal_probs[p, ncol(marginal_probs)] + runif(1, 0, 0.1)
          }
          
          n_cut_makers <- 80
        }
        
        # Phase 2: Assign final positions
        final_positions <- numeric(n_players)
        
        # Cut makers get positions 1 through n_cut_makers
        if (n_cut_makers > 0) {
          cut_maker_ranks <- rank(cut_maker_scores[cut_makers], ties.method = "random")
          final_positions[cut_makers] <- cut_maker_ranks
        }
        
        # Missed cut players get remaining positions, ordered by their scores
        if (n_cut_makers < n_players) {
          missed_cut_ranks <- rank(-missed_cut_scores[!cut_makers], ties.method = "random")  # Higher score = better = lower final position number
          final_positions[!cut_makers] <- missed_cut_ranks + n_cut_makers
        }
        
        all_positions[, sim] <- final_positions
      }
    }
    
    # Progress indicator
    if (n_sims > 2000 && batch %% max(1, n_batches %/% 10) == 0) {
      cat(sprintf("Simulation progress: %.1f%%\n", (batch / n_batches) * 100))
    }
  }
  
  return(all_positions)
}

# Optimized points lookup with caching
create_points_cache <- function(points_table) {
  if (is.null(points_table) || nrow(points_table) == 0) {
    return(NULL)
  }
  
  setDT(points_table)
  setkey(points_table, Rank)
  
  # Create a fast lookup vector
  max_rank <- max(points_table$Rank, na.rm = TRUE)
  points_lookup <- numeric(max_rank)
  
  for (i in 1:max_rank) {
    # Find closest rank
    closest_rank <- points_table[Rank <= i]
    if (nrow(closest_rank) > 0) {
      points_lookup[i] <- closest_rank$Score[nrow(closest_rank)]
    } else {
      points_lookup[i] <- 0
    }
  }
  
  return(points_lookup)
}

# Fast points assignment using cached lookup
get_points_fast <- function(positions, points_cache) {
  if (is.null(points_cache)) {
    return(rep(0, length(positions)))
  }
  
  # Clamp positions to valid range
  valid_positions <- pmax(1, pmin(positions, length(points_cache)))
  return(points_cache[valid_positions])
}

run_golf_simulation <- function(input_data, n_sims = 1000, cut_line = 65, no_cut = FALSE) {
  players_dt <- as.data.table(input_data$players)
  n_players <- nrow(players_dt)
  
  # Platform detection - updated to handle missing FD
  has_dk <- "DKSalary" %in% names(players_dt) && nrow(input_data$dk_points) > 0
  has_fd <- "FDSalary" %in% names(players_dt) && nrow(input_data$fd_points) > 0
  
  cat("Starting golf simulation with", format(n_sims, big.mark = ","), "iterations\n")
  cat("Players:", n_players, "| DK:", has_dk, "| FD:", has_fd)
  if (!no_cut) cat("| Cut line:", cut_line, "+ ties")
  cat("\n")
  
  # Pre-compute everything once - now with cut configuration
  start_time <- Sys.time()
  cat("Pre-computing player distributions...\n")
  player_distributions <- precompute_golf_distributions(players_dt, cut_line, no_cut)
  
  # Create fast points caches
  dk_points_cache <- NULL
  fd_points_cache <- NULL
  
  if (has_dk) {
    cat("Creating DraftKings points cache...\n")
    dk_points_cache <- create_points_cache(input_data$dk_points)
  }
  
  if (has_fd) {
    cat("Creating FanDuel points cache...\n")
    fd_points_cache <- create_points_cache(input_data$fd_points)
  }
  
  # Extract static data once (updated to handle missing FD)
  static_data <- list(names = players_dt$Name)
  
  if (has_dk) {
    static_data$dk <- list(
      salary = players_dt$DKSalary,
      op = players_dt$DKOP,
      name = if("DKName" %in% names(players_dt)) players_dt$DKName else players_dt$Name
    )
  }
  
  if (has_fd) {
    static_data$fd <- list(
      salary = players_dt$FDSalary,
      op = players_dt$FDOP,
      name = if("FDName" %in% names(players_dt)) players_dt$FDName else players_dt$Name
    )
  }
  
  precomp_time <- difftime(Sys.time(), start_time, units = "secs")
  cat(sprintf("Pre-computation completed in %.2f seconds\n", as.numeric(precomp_time)))
  
  # Generate all finish positions at once - now with new simulation logic
  cat("Generating finish positions...\n")
  sim_start <- Sys.time()
  all_finish_positions <- simulate_golf_positions_vectorized(player_distributions, n_sims)
  sim_time <- difftime(Sys.time(), sim_start, units = "secs")
  cat(sprintf("Position simulation completed in %.2f seconds\n", as.numeric(sim_time)))
  
  # Batch process fantasy points calculation
  cat("Calculating fantasy points in batches...\n")
  points_start <- Sys.time()
  
  # Pre-allocate result lists
  results_list <- vector("list", n_sims)
  
  # Process in batches for better memory management
  batch_size <- min(500, n_sims)
  n_batches <- ceiling(n_sims / batch_size)
  
  for (batch in 1:n_batches) {
    start_sim <- (batch - 1) * batch_size + 1
    end_sim <- min(batch * batch_size, n_sims)
    batch_sims <- start_sim:end_sim
    
    # Process batch
    for (sim_idx in seq_along(batch_sims)) {
      sim <- batch_sims[sim_idx]
      
      sim_result <- data.table(
        SimID = sim,
        Name = static_data$names,
        FinishPosition = all_finish_positions[, sim]
      )
      
      if (has_dk) {
        # Fast DK points calculation using cached lookup
        dk_points <- get_points_fast(all_finish_positions[, sim], dk_points_cache)
        
        sim_result[, `:=`(
          DKSalary = static_data$dk$salary,
          DKOP = static_data$dk$op,
          DKName = static_data$dk$name,
          DKFantasyPoints = dk_points
        )]
      }
      
      if (has_fd) {
        # Fast FD points calculation using cached lookup
        fd_points <- get_points_fast(all_finish_positions[, sim], fd_points_cache)
        
        sim_result[, `:=`(
          FDSalary = static_data$fd$salary,
          FDOP = static_data$fd$op,
          FDName = static_data$fd$name,
          FDFantasyPoints = fd_points
        )]
      }
      
      results_list[[sim]] <- sim_result
    }
    
    # Progress reporting
    if (n_batches > 5 && batch %% max(1, n_batches %/% 10) == 0) {
      cat(sprintf("Processed batch %d/%d (%.1f%%)\n", batch, n_batches, (batch/n_batches)*100))
    }
  }
  
  points_time <- difftime(Sys.time(), points_start, units = "secs")
  cat(sprintf("Fantasy points calculation completed in %.2f seconds\n", as.numeric(points_time)))
  
  # Combine results efficiently
  cat("Combining results...\n")
  combine_start <- Sys.time()
  combined_results <- rbindlist(results_list)
  combine_time <- difftime(Sys.time(), combine_start, units = "secs")
  cat(sprintf("Results combined in %.2f seconds\n", as.numeric(combine_time)))
  
  # Final cleanup
  cleanup_memory(verbose = FALSE)
  
  total_time <- difftime(Sys.time(), start_time, units = "secs")
  cat("\n=== GOLF SIMULATION COMPLETED ===\n")
  cat(sprintf("Total time: %.2f seconds\n", as.numeric(total_time)))
  cat(sprintf("Generated %s results\n", format(nrow(combined_results), big.mark = ",")))
  cat(sprintf("Performance: %.0f simulations/second\n", n_sims / as.numeric(total_time)))
  
  return(list(
    results = combined_results,
    has_dk = has_dk,
    has_fd = has_fd
  ))
}

calculate_cut_distribution <- function(cut_probs) {
  n_players <- length(cut_probs)
  
  # Calculate probability of exactly k players making cut
  exact_probs <- numeric(n_players + 1)
  names(exact_probs) <- paste0("Exactly_", 0:n_players)
  
  # Calculate probability of at least k players making cut
  atleast_probs <- numeric(n_players + 1)
  names(atleast_probs) <- paste0("AtLeast_", 0:n_players)
  
  # Use dynamic programming for efficiency
  # dp[i][j] = probability that exactly j of first i players make cut
  dp <- matrix(0, nrow = n_players + 1, ncol = n_players + 1)
  dp[1, 1] <- 1  # 0 players, 0 cuts
  
  for (i in 1:n_players) {
    for (j in 0:i) {
      if (j == 0) {
        # No cuts made
        dp[i + 1, j + 1] <- dp[i, j + 1] * (1 - cut_probs[i])
      } else if (j == i) {
        # All cuts made
        dp[i + 1, j + 1] <- dp[i, j] * cut_probs[i]
      } else {
        # Some cuts made
        dp[i + 1, j + 1] <- dp[i, j + 1] * (1 - cut_probs[i]) + dp[i, j] * cut_probs[i]
      }
    }
  }
  
  # Extract exact probabilities
  for (k in 0:n_players) {
    exact_probs[k + 1] <- dp[n_players + 1, k + 1]
  }
  
  # Calculate at least probabilities
  for (k in 0:n_players) {
    atleast_probs[k + 1] <- sum(exact_probs[(k + 1):(n_players + 1)])
  }
  
  return(list(exact = exact_probs, atleast = atleast_probs))
}

# -----------------------------------------------------------------------------
# 2. ADD THIS NEW FUNCTION - Add cut metrics to lineups
# -----------------------------------------------------------------------------
add_cut_metrics_to_lineups <- function(lineups, player_data, platform = "dk") {
  if(is.null(lineups) || nrow(lineups) == 0) {
    return(lineups)
  }
  
  # Ensure we have Cut column in player data
  if(!"Cut" %in% names(player_data)) {
    warning("No Cut column found in player data. Skipping cut metrics.")
    lineups$ExpectedCuts <- NA
    lineups$AtLeast6 <- NA
    lineups$AtLeast5 <- NA
    return(lineups)
  }
  
  # Create lookup table for cut probabilities
  cut_lookup <- setNames(player_data$Cut, player_data$Name)
  
  # Determine roster size based on platform
  roster_size <- if(platform == "dk") 6 else 6
  player_cols <- paste0("Player", 1:roster_size)
  
  # Initialize cut metric columns
  lineups$ExpectedCuts <- 0
  lineups$AtLeast6 <- 0
  lineups$AtLeast5 <- 0
  
  # Calculate cut metrics for each lineup
  for(i in 1:nrow(lineups)) {
    # Get player names from lineup
    lineup_players <- unlist(lineups[i, player_cols])
    lineup_players <- lineup_players[!is.na(lineup_players)]
    
    # Get cut probabilities for these players
    cut_probs <- numeric(length(lineup_players))
    for(j in 1:length(lineup_players)) {
      player_name <- lineup_players[j]
      if(player_name %in% names(cut_lookup)) {
        cut_probs[j] <- cut_lookup[player_name]
      } else {
        cut_probs[j] <- 0.80  # Default if not found
      }
    }
    
    # Calculate expected cuts (simple sum)
    lineups$ExpectedCuts[i] <- sum(cut_probs)
    
    # Calculate exact cut distribution probabilities
    if(length(cut_probs) > 0) {
      cut_dist <- calculate_cut_distribution(cut_probs)
      
      # Extract the "at least" probabilities, convert to percentage, and round to 1 decimal
      lineups$AtLeast6[i] <- round(cut_dist$atleast["AtLeast_6"] * 100, 1)
      lineups$AtLeast5[i] <- round(cut_dist$atleast["AtLeast_5"] * 100, 1)
    }
  }
  
  return(lineups)
}

analyze_golf_finishing_positions <- function(sim_results, cut_line = 65, no_cut = FALSE) {
  setDT(sim_results)
  
  # Base analysis
  results <- sim_results[, .(
    Win_Rate = mean(FinishPosition == 1, na.rm = TRUE) * 100,
    T5_Rate = mean(FinishPosition <= 5, na.rm = TRUE) * 100,
    T10_Rate = mean(FinishPosition <= 10, na.rm = TRUE) * 100,
    T20_Rate = mean(FinishPosition <= 20, na.rm = TRUE) * 100,
    T30_Rate = mean(FinishPosition <= 30, na.rm = TRUE) * 100,
    T40_Rate = mean(FinishPosition <= 40, na.rm = TRUE) * 100,
    Avg_Finish = mean(FinishPosition, na.rm = TRUE),
    Median = median(FinishPosition, na.rm = TRUE)
  ), by = Name]
  
  # Add cut analysis if applicable
  if (!no_cut) {
    cut_analysis <- sim_results[, .(
      Cut_Rate = mean(FinishPosition <= cut_line, na.rm = TRUE) * 100,
      MissedCut_Rate = mean(FinishPosition > cut_line, na.rm = TRUE) * 100
    ), by = Name]
    
    results <- merge(results, cut_analysis, by = "Name")
  }
  
  results <- results[order(Avg_Finish)]
  
  # Round all numeric columns
  numeric_cols <- setdiff(names(results), "Name")
  for (col in numeric_cols) {
    results[, (col) := round(get(col), 1)]
  }
  
  return(results)
}


# DraftKings fantasy point projections for golf
analyze_dk_golf_fantasy_points <- function(sim_results, cut_line = 65) {
  setDT(sim_results)
  
  results <- sim_results[, .(
    DKSalary = first(DKSalary),
    DKOP = first(DKOP),
    Median_Fantasy_Pts = round(median(DKFantasyPoints), 1),
    FP_90thPct = round(quantile(DKFantasyPoints, 0.9), 1),
    Made_Cut_Pct = round(mean(FinishPosition <= cut_line, na.rm = TRUE) * 100, 1)
  ), by = .(Name)]
  
  # Add PPD calculation
  results[, PPD := round(Median_Fantasy_Pts / (DKSalary / 1000), 1)]
  
  # Multiply DKOP by 100 to convert to percentage if it's a proportion
  if (max(results$DKOP, na.rm = TRUE) <= 1) {
    results[, DKOP := round(as.numeric(DKOP) * 100, 1)]
  }
  
  return(results)
}

# FanDuel fantasy point projections for golf
analyze_fd_golf_fantasy_points <- function(sim_results, cut_line = 65) {
  setDT(sim_results)
  
  results <- sim_results[, .(
    FDSalary = first(FDSalary),
    FDOP = first(FDOP),
    Median_Fantasy_Pts = round(median(FDFantasyPoints), 1),
    FP_90thPct = round(quantile(FDFantasyPoints, 0.9), 1),
    Made_Cut_Pct = round(mean(FinishPosition <= cut_line, na.rm = TRUE) * 100, 1)
  ), by = .(Name)]
  
  # Add PPD calculation
  results[, PPD := round(Median_Fantasy_Pts / (FDSalary / 1000), 1)]
  
  # Multiply FDOP by 100 to convert to percentage if it's a proportion
  if (max(results$FDOP, na.rm = TRUE) <= 1) {
    results[, FDOP := round(as.numeric(FDOP) * 100, 1)]
  }
  
  return(results)
}

# Generate all possible tier lineups
generate_all_tier_lineups <- function(tiers_data) {
  # Split players by tier
  tier_lists <- list()
  for(tier_num in 1:6) {
    tier_players <- tiers_data[Tier == tier_num]
    if(nrow(tier_players) == 0) {
      stop(paste("No players found in Tier", tier_num))
    }
    tier_lists[[tier_num]] <- tier_players
  }
  
  # Calculate total possible combinations
  n_combinations <- prod(sapply(tier_lists, nrow))
  cat(sprintf("Total possible tier lineups: %s\n", format(n_combinations, big.mark=",")))
  
  if(n_combinations > 100000) {
    warning(sprintf("Large number of combinations (%s). This may take a while.", 
                    format(n_combinations, big.mark=",")))
  }
  
  # Generate all combinations using expand.grid on the indices
  indices <- expand.grid(lapply(tier_lists, function(x) 1:nrow(x)))
  
  # Create lineup data table
  all_lineups <- data.table(
    Tier1 = tier_lists[[1]]$Name[indices[,1]],
    Tier2 = tier_lists[[2]]$Name[indices[,2]],
    Tier3 = tier_lists[[3]]$Name[indices[,3]],
    Tier4 = tier_lists[[4]]$Name[indices[,4]],
    Tier5 = tier_lists[[5]]$Name[indices[,5]],
    Tier6 = tier_lists[[6]]$Name[indices[,6]],
    Tier1Name = tier_lists[[1]]$TiersName[indices[,1]],
    Tier2Name = tier_lists[[2]]$TiersName[indices[,2]],
    Tier3Name = tier_lists[[3]]$TiersName[indices[,3]],
    Tier4Name = tier_lists[[4]]$TiersName[indices[,4]],
    Tier5Name = tier_lists[[5]]$TiersName[indices[,5]],
    Tier6Name = tier_lists[[6]]$TiersName[indices[,6]]
  )
  
  return(all_lineups)
}

# Quick filter tier lineups by average points
quick_filter_tier_lineups <- function(tier_lineups, sim_results, top_n = 10000) {
  setDT(sim_results)
  
  cat(sprintf("Quick filtering %s lineups to top %s...\n", 
              format(nrow(tier_lineups), big.mark=","), format(top_n, big.mark=",")))
  
  # Calculate average DK points for each player
  player_avg <- sim_results[, .(AvgPoints = mean(DKFantasyPoints, na.rm = TRUE)), by = Name]
  
  # Calculate lineup average (sum of 6 player averages)
  tier_lineups[, LineupAvg := {
    players <- c(Tier1, Tier2, Tier3, Tier4, Tier5, Tier6)
    sum(player_avg[match(players, Name), AvgPoints], na.rm = TRUE)
  }]
  
  # Keep top N
  tier_lineups <- tier_lineups[order(-LineupAvg)][1:min(top_n, .N)]
  tier_lineups[, LineupAvg := NULL]
  
  cat(sprintf("Filtered to %s lineups\n", format(nrow(tier_lineups), big.mark=",")))
  
  return(tier_lineups)
}

# Count tier optimal lineups across simulations
count_tier_optimal_lineups <- function(sim_results, tiers_data) {
  sim_results_dt <- as.data.table(sim_results)
  
  # Generate all possible lineups
  cat("Generating all tier combinations...\n")
  all_lineups <- generate_all_tier_lineups(tiers_data)
  
  # Quick filter to top candidates by points if there are too many
  if(nrow(all_lineups) > 10000) {
    cat("Many lineups generated, applying points filter...\n")
    all_lineups <- quick_filter_tier_lineups(all_lineups, sim_results_dt, top_n = 10000)
  }
  
  # Get unique simulation IDs
  all_sim_ids <- unique(sim_results_dt$SimID)
  n_sims <- length(all_sim_ids)
  n_lineups <- nrow(all_lineups)
  
  cat(sprintf("Evaluating %s lineups across %s simulations...\n", 
              format(n_lineups, big.mark=","), format(n_sims, big.mark=",")))
  
  # Initialize results storage using data.table for efficiency
  all_results <- data.table()
  
  # Process simulations in batches
  batch_size <- 100
  chunks <- ceiling(n_sims / batch_size)
  
  for(chunk in 1:chunks) {
    start_idx <- (chunk-1) * batch_size + 1
    end_idx <- min(chunk * batch_size, n_sims)
    chunk_sim_ids <- all_sim_ids[start_idx:end_idx]
    
    if(chunk %% 5 == 0) {
      cat(sprintf("Processing chunk %d/%d (%.1f%%)\n", 
                  chunk, chunks, (chunk/chunks)*100))
    }
    
    # Process all simulations in this chunk
    chunk_results <- rbindlist(lapply(chunk_sim_ids, function(sim_id) {
      sim_data <- sim_results_dt[SimID == sim_id]
      
      # Calculate points for all lineups in this simulation
      lineup_scores <- all_lineups[, .(
        LineupIdx = .I,
        TotalScore = {
          players <- c(Tier1, Tier2, Tier3, Tier4, Tier5, Tier6)
          sum(sim_data[Name %in% players, DKFantasyPoints], na.rm = TRUE)
        }
      )]
      
      # Rank lineups
      lineup_scores[, Rank := frank(-TotalScore, ties.method = "random")]
      lineup_scores[, SimID := sim_id]
      
      return(lineup_scores)
    }))
    
    all_results <- rbind(all_results, chunk_results)
  }
  
  cat(sprintf("\nTotal results collected: %s rows\n", format(nrow(all_results), big.mark=",")))
  
  if(nrow(all_results) == 0) {
    cat("ERROR: No results were generated!\n")
    return(NULL)
  }
  
  # Count lineup appearances by rank - NEW METRICS
  cat("Calculating lineup statistics...\n")
  lineup_counts <- all_results[, .(
    Top1Count = sum(Rank == 1),
    Top10Count = sum(Rank <= 10),
    Top100Count = sum(Rank <= 100),
    Top1000Count = sum(Rank <= 1000)
  ), by = LineupIdx]
  
  # Sort by Top1Count, then Top10Count
  lineup_counts <- lineup_counts[order(-Top1Count, -Top10Count, -Top100Count)]
  
  cat(sprintf("Lineup stats calculated for %s lineups\n", format(nrow(lineup_counts), big.mark=",")))
  
  # Join back with lineup details
  all_lineups[, LineupIdx := .I]
  result <- merge(lineup_counts, all_lineups, by = "LineupIdx")
  
  # Keep only the columns we want in the final output
  keep_cols <- c("Tier1", "Tier2", "Tier3", "Tier4", "Tier5", "Tier6",
                 "Tier1Name", "Tier2Name", "Tier3Name", "Tier4Name", "Tier5Name", "Tier6Name",
                 "Top1Count", "Top10Count", "Top100Count", "Top1000Count")
  
  result <- result[, ..keep_cols]
  
  cat(sprintf("\nGenerated %s tier lineup results\n", format(nrow(result), big.mark=",")))
  if(nrow(result) > 0) {
    cat(sprintf("Top lineup Top1Count: %d\n", result$Top1Count[1]))
  }
  
  return(as.data.frame(result))
}

calculate_dk_golf_player_exposure <- function(optimal_lineups, fantasy_analysis, random_lineups = NULL) {
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame(Message = "No optimal lineups available."))
  }
  
  # Get all players from the optimal lineups
  player_cols <- paste0("Player", 1:DK_ROSTER_SIZE)
  all_players <- c()
  for(col in player_cols) {
    if(col %in% names(optimal_lineups)) {
      all_players <- c(all_players, optimal_lineups[[col]])
    }
  }
  all_players <- unique(all_players[!is.na(all_players)])
  
  if(length(all_players) == 0) {
    return(data.frame(Message = "No players found in lineups."))
  }
  
  # Initialize metrics data frame
  metrics_data <- data.frame(
    Name = all_players,
    DKSalary = NA_real_,
    DKOP = NA_real_,
    OptimalRate = 0,
    FilteredPoolRate = 0,
    Exposure = 0,
    Leverage = 0,
    stringsAsFactors = FALSE
  )
  
  # Match with fantasy analysis data
  if(!is.null(fantasy_analysis) && nrow(fantasy_analysis) > 0) {
    for(i in 1:nrow(metrics_data)) {
      player_name <- metrics_data$Name[i]
      matches <- which(fantasy_analysis$Name == player_name)
      
      if(length(matches) > 0) {
        match_idx <- matches[1]
        if("DKSalary" %in% names(fantasy_analysis)) {
          metrics_data$DKSalary[i] <- fantasy_analysis$DKSalary[match_idx]
        }
        if("DKOP" %in% names(fantasy_analysis)) {
          metrics_data$DKOP[i] <- fantasy_analysis$DKOP[match_idx]
        }
      }
    }
  }
  
  # Calculate OptimalRate
  total_top1 <- sum(optimal_lineups$Top1Count, na.rm = TRUE)
  if(total_top1 > 0) {
    for(player in all_players) {
      player_appears <- logical(nrow(optimal_lineups))
      for(col in player_cols) {
        if(col %in% names(optimal_lineups)) {
          player_appears <- player_appears | (optimal_lineups[[col]] == player)
        }
      }
      player_matches <- which(player_appears)
      
      if(length(player_matches) > 0) {
        player_total <- sum(optimal_lineups$Top1Count[player_matches], na.rm = TRUE)
        metrics_data[metrics_data$Name == player, "OptimalRate"] <- (player_total / total_top1) * 100
      }
    }
  }
  
  # Calculate Exposure from random lineups (only if provided)
  if(!is.null(random_lineups) && nrow(random_lineups) > 0) {
    for(player in all_players) {
      player_appears <- logical(nrow(random_lineups))
      for(col in player_cols) {
        if(col %in% names(random_lineups)) {
          player_appears <- player_appears | (random_lineups[[col]] == player)
        }
      }
      metrics_data[metrics_data$Name == player, "Exposure"] <- (sum(player_appears) / nrow(random_lineups)) * 100
    }
  }
  
  # Calculate leverage (only if we have ownership data and exposure)
  for(i in 1:nrow(metrics_data)) {
    if(!is.na(metrics_data$DKOP[i]) && !is.na(metrics_data$Exposure[i])) {
      # Convert DKOP to percentage if it's a decimal
      ownership_pct <- if(metrics_data$DKOP[i] <= 1) metrics_data$DKOP[i] * 100 else metrics_data$DKOP[i]
      metrics_data$Leverage[i] <- metrics_data$Exposure[i] - ownership_pct
    }
  }
  
  # Sort by OptimalRate
  metrics_data <- metrics_data[order(-metrics_data$OptimalRate), ]
  
  return(metrics_data)
}

# Replace your calculate_fd_golf_player_exposure function with this:
calculate_fd_golf_player_exposure <- function(optimal_lineups, fantasy_analysis, random_lineups = NULL) {
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame(Message = "No optimal lineups available."))
  }
  
  # Get all players from the optimal lineups
  player_cols <- paste0("Player", 1:FD_ROSTER_SIZE)
  all_players <- c()
  for(col in player_cols) {
    if(col %in% names(optimal_lineups)) {
      all_players <- c(all_players, optimal_lineups[[col]])
    }
  }
  all_players <- unique(all_players[!is.na(all_players)])
  
  if(length(all_players) == 0) {
    return(data.frame(Message = "No players found in lineups."))
  }
  
  # Initialize metrics data frame
  metrics_data <- data.frame(
    Name = all_players,
    FDSalary = NA_real_,
    FDOP = NA_real_,
    OptimalRate = 0,
    FilteredPoolRate = 0,
    Exposure = 0,
    Leverage = 0,
    stringsAsFactors = FALSE
  )
  
  # Match with fantasy analysis data
  if(!is.null(fantasy_analysis) && nrow(fantasy_analysis) > 0) {
    for(i in 1:nrow(metrics_data)) {
      player_name <- metrics_data$Name[i]
      matches <- which(fantasy_analysis$Name == player_name)
      
      if(length(matches) > 0) {
        match_idx <- matches[1]
        if("FDSalary" %in% names(fantasy_analysis)) {
          metrics_data$FDSalary[i] <- fantasy_analysis$FDSalary[match_idx]
        }
        if("FDOP" %in% names(fantasy_analysis)) {
          metrics_data$FDOP[i] <- fantasy_analysis$FDOP[match_idx]
        }
      }
    }
  }
  
  # Calculate OptimalRate
  total_top1 <- sum(optimal_lineups$Top1Count, na.rm = TRUE)
  if(total_top1 > 0) {
    for(player in all_players) {
      player_appears <- logical(nrow(optimal_lineups))
      for(col in player_cols) {
        if(col %in% names(optimal_lineups)) {
          player_appears <- player_appears | (optimal_lineups[[col]] == player)
        }
      }
      player_matches <- which(player_appears)
      
      if(length(player_matches) > 0) {
        player_total <- sum(optimal_lineups$Top1Count[player_matches], na.rm = TRUE)
        metrics_data[metrics_data$Name == player, "OptimalRate"] <- (player_total / total_top1) * 100
      }
    }
  }
  
  # Calculate Exposure from random lineups
  if(!is.null(random_lineups) && nrow(random_lineups) > 0) {
    for(player in all_players) {
      player_appears <- logical(nrow(random_lineups))
      for(col in player_cols) {
        if(col %in% names(random_lineups)) {
          player_appears <- player_appears | (random_lineups[[col]] == player)
        }
      }
      metrics_data[metrics_data$Name == player, "Exposure"] <- (sum(player_appears) / nrow(random_lineups)) * 100
    }
  }
  
  # Calculate leverage
  for(i in 1:nrow(metrics_data)) {
    if(!is.na(metrics_data$FDOP[i]) && !is.na(metrics_data$Exposure[i])) {
      # Convert FDOP to percentage if it's a decimal
      ownership_pct <- if(metrics_data$FDOP[i] <= 1) metrics_data$FDOP[i] * 100 else metrics_data$FDOP[i]
      metrics_data$Leverage[i] <- metrics_data$Exposure[i] - ownership_pct
    }
  }
  
  # Sort by OptimalRate
  metrics_data <- metrics_data[order(-metrics_data$OptimalRate), ]
  
  return(metrics_data)
}

# Function to calculate cumulative ownership and geometric mean for lineups
calculate_golf_lineup_ownership_stats <- function(lineup_data, player_ownership_map, platform = "dk") {
  if(is.null(lineup_data) || nrow(lineup_data) == 0) return(lineup_data)
  
  # Determine roster size and player columns based on platform
  if(platform == "dk") {
    roster_size <- DK_ROSTER_SIZE
    player_cols <- paste0("Player", 1:roster_size)
    ownership_col <- "DKOP"
  } else {
    roster_size <- FD_ROSTER_SIZE
    player_cols <- paste0("Player", 1:roster_size)
    ownership_col <- "FDOP"
  }
  
  # Initialize new columns
  lineup_data$CumulativeOwnership <- 0
  lineup_data$GeometricMeanOwnership <- 0
  
  # Calculate for each lineup
  for(i in 1:nrow(lineup_data)) {
    ownership_values <- c()
    
    # Get ownership for each player in the lineup
    for(col in player_cols) {
      if(col %in% names(lineup_data)) {
        player_name <- lineup_data[[col]][i]
        if(!is.na(player_name) && player_name %in% names(player_ownership_map)) {
          ownership <- player_ownership_map[player_name]
          if(!is.na(ownership)) {
            # Convert to percentage if it's in decimal format (0.17 -> 17%)
            if(ownership < 1) {
              ownership <- ownership * 100
            }
            ownership_values <- c(ownership_values, ownership)
          }
        }
      }
    }
    
    # Calculate cumulative ownership (sum)
    if(length(ownership_values) > 0) {
      lineup_data$CumulativeOwnership[i] <- sum(ownership_values)
      
      # Calculate geometric mean (ownership is already in percentage format)
      ownership_decimals <- ownership_values / 100
      geometric_mean <- exp(mean(log(ownership_decimals[ownership_decimals > 0])))
      lineup_data$GeometricMeanOwnership[i] <- geometric_mean * 100
    }
  }
  
  return(lineup_data)
}

# Optimized DraftKings lineup finder for golf
find_dk_golf_optimal_lineups <- function(sim_data, k = 5) {
  # Ensure data.table
  setDT(sim_data)
  
  # Pre-filter top candidates using a single vectorized operation
  sim_data[, PPD := DKFantasyPoints / (DKSalary/1000)]
  
  # Get top candidates by both absolute points and value (points per dollar)
  top_points_idx <- order(-sim_data$DKFantasyPoints)[1:min(20, nrow(sim_data))]
  top_ppd_idx <- order(-sim_data$PPD)[1:min(20, nrow(sim_data))]
  
  # Combine candidates more efficiently
  candidate_idx <- unique(c(top_points_idx, top_ppd_idx))
  candidates <- sim_data[candidate_idx, .(Name, DKSalary, DKFantasyPoints)]
  
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
        lp("max", candidates$DKFantasyPoints, const.mat, const.dir, const.rhs, 
           all.bin = TRUE, presolve = 0, compute.sens = 0)
      )
    }, error = function(e) {
      NULL
    })
    
    if(is.null(result) || result$status != 0) break
    
    # Get selected players
    selected_indices <- which(result$solution > 0.9)
    
    if(length(selected_indices) != DK_ROSTER_SIZE) break
    
    # Create lineup string
    selected_players <- sort(candidates$Name[selected_indices])
    lineup_str <- paste(selected_players, collapse = "|")
    
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

# FanDuel lineup finder for golf
find_fd_golf_optimal_lineups <- function(sim_data, k = 5) {
  # Ensure data.table
  setDT(sim_data)
  
  # Filter out players with zero salary and ensure non-NA values
  sim_data <- sim_data[FDSalary > 0 & !is.na(FDFantasyPoints)]
  
  # Pre-filter top candidates using a single vectorized operation
  sim_data[, PPD := FDFantasyPoints / FDSalary]
  
  # Get top candidates by both absolute points and value (points per dollar)
  top_points_idx <- order(-sim_data$FDFantasyPoints)[1:min(10, nrow(sim_data))]
  top_ppd_idx <- order(-sim_data$PPD)[1:min(12, nrow(sim_data))]
  
  # Combine candidates more efficiently
  candidate_idx <- unique(c(top_points_idx, top_ppd_idx))
  candidates <- sim_data[candidate_idx, .(Name, FDSalary, FDFantasyPoints)]
  
  n <- nrow(candidates)
  if(n < FD_ROSTER_SIZE) return(NULL)
  
  # Pre-allocate constraint matrix for reuse - more efficient
  base_const_mat <- matrix(0, nrow = 2, ncol = n)
  base_const_mat[1, ] <- candidates$FDSalary  # Salary cap
  base_const_mat[2, ] <- 1                    # Roster size
  
  base_const_dir <- c("<=", "==")
  base_const_rhs <- c(FD_SALARY_CAP, FD_ROSTER_SIZE)
  
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
      const.rhs <- c(base_const_rhs, rep(FD_ROSTER_SIZE-1, length(excluded_pairs)))
      
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
        lp("max", candidates$FDFantasyPoints, const.mat, const.dir, const.rhs, 
           all.bin = TRUE, presolve = 0, compute.sens = 0)
      )
    }, error = function(e) {
      NULL
    })
    
    if(is.null(result) || result$status != 0) break
    
    # Get selected players
    selected_indices <- which(result$solution > 0.9)
    
    if(length(selected_indices) != FD_ROSTER_SIZE) break
    
    # Create lineup string - use sorted names for consistent identification
    selected_players <- sort(candidates$Name[selected_indices])
    lineup_str <- paste(selected_players, collapse = "|")
    
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

count_dk_golf_optimal_lineups <- function(sim_results, player_data = NULL) {
  # Create data.table for better performance
  sim_results_dt <- as.data.table(sim_results)
  
  # Extract only necessary columns
  sim_results_dt <- sim_results_dt[, .(
    SimID, Name, DKSalary, DKFantasyPoints, DKOP
  )]
  
  # Split by simulation ID
  all_sim_ids <- unique(sim_results_dt$SimID)
  n_sims <- length(all_sim_ids)
  
  # Initialize lineup storage
  all_lineups <- list()
  
  # Process simulations in smaller batches
  batch_size <- 25
  chunks <- ceiling(n_sims / batch_size)
  
  for(chunk in 1:chunks) {
    start_idx <- (chunk-1) * batch_size + 1
    end_idx <- min(chunk * batch_size, n_sims)
    chunk_sim_ids <- all_sim_ids[start_idx:end_idx]
    
    message(sprintf("Processing chunk %d/%d (simulations %d to %d)", 
                    chunk, chunks, start_idx, end_idx))
    
    for(sim_id in chunk_sim_ids) {
      sim_data <- sim_results_dt[SimID == sim_id]
      
      # Simple optimization: just get top lineups by total score
      sim_data[, PPD := DKFantasyPoints / (DKSalary/1000)]
      
      # Get top candidates
      top_candidates <- sim_data[order(-DKFantasyPoints)][1:min(15, nrow(sim_data))]
      
      if(nrow(top_candidates) >= DK_ROSTER_SIZE) {
        # Generate ALL possible lineups for this simulation (up to 10 per sim)
        for(rank in 1:10) {  # Generate more lineups per simulation
          # Simple greedy selection with some variation
          selected_players <- top_candidates[1:DK_ROSTER_SIZE]
          total_salary <- sum(selected_players$DKSalary)
          
          if(total_salary <= DK_SALARY_CAP) {
            lineup_str <- paste(sort(selected_players$Name), collapse = "|")
            
            lineup_result <- data.frame(
              Lineup = lineup_str,
              Rank = rank,
              SimID = sim_id,
              TotalScore = sum(selected_players$DKFantasyPoints),
              TotalSalary = total_salary,
              stringsAsFactors = FALSE
            )
            
            all_lineups[[length(all_lineups) + 1]] <- lineup_result
          }
          
          # Shuffle top candidates for next iteration
          if(nrow(top_candidates) > DK_ROSTER_SIZE) {
            top_candidates <- top_candidates[sample(nrow(top_candidates))]
          }
        }
      }
    }
    
    # Progress reporting
    cat(sprintf("Processed %d/%d simulations (%.1f%%)\n", 
                min(end_idx, n_sims), n_sims, 
                min(end_idx, n_sims) / n_sims * 100))
  }
  
  if(length(all_lineups) == 0) return(NULL)
  
  # Combine all results
  combined_lineups <- do.call(rbind, all_lineups)
  
  # Count lineup appearances by rank - KEEP ALL LINEUPS
  lineup_counts <- combined_lineups %>%
    group_by(Lineup) %>%
    summarise(
      Top1Count = sum(Rank == 1),
      Top10Count = sum(Rank <= 10),
      Top50Count = sum(Rank <= 50),
      Top100Count = sum(Rank <= 100),
      TotalSalary = first(TotalSalary),
      .groups = 'drop'
    ) %>%
    arrange(desc(Top100Count), desc(Top10Count), desc(Top1Count)) %>%
    slice_head(n = 10000) 
  
  # Split lineup string into player columns
  player_cols <- do.call(rbind, strsplit(lineup_counts$Lineup, "\\|"))
  colnames(player_cols) <- paste0("Player", 1:DK_ROSTER_SIZE)
  
  # Create final result with ALL lineups
  result <- cbind(
    as.data.frame(player_cols),
    lineup_counts[, c("Top1Count", "Top10Count", "Top50Count", "Top100Count", "TotalSalary")]
  )
  
  # Calculate ownership statistics for ALL lineups
  ownership_map <- NULL
  if("DKOP" %in% names(sim_results)) {
    ownership_data <- unique(sim_results[!is.na(sim_results$DKOP), c("Name", "DKOP")])
    if(nrow(ownership_data) > 0) {
      ownership_map <- setNames(ownership_data$DKOP, ownership_data$Name)
      result <- calculate_golf_lineup_ownership_stats(result, ownership_map, "dk")
    }
  }
  
  # Add default ownership columns if no ownership data
  if(!"CumulativeOwnership" %in% names(result)) {
    result$CumulativeOwnership <- 0
    result$GeometricMeanOwnership <- 0
  }
  
  # **NEW: Add cut metrics if player_data provided**
  if(!is.null(player_data)) {
    message("Calculating cut metrics for lineups...")
    result <- add_cut_metrics_to_lineups(result, player_data, platform = "dk")
  }
  
  cat(sprintf("Generated %d total optimal lineups\n", nrow(result)))
  
  return(result)
}

count_fd_golf_optimal_lineups <- function(sim_results, player_data = NULL) {
  # Create data.table for better performance
  sim_results_dt <- as.data.table(sim_results)
  sim_results_dt <- sim_results_dt[FDSalary > 0]
  
  # Extract only necessary columns
  sim_results_dt <- sim_results_dt[, .(
    SimID, Name, FDSalary, FDFantasyPoints, FDOP
  )]
  
  # Split by simulation ID
  all_sim_ids <- unique(sim_results_dt$SimID)
  n_sims <- length(all_sim_ids)
  
  # Initialize lineup storage
  all_lineups <- list()
  
  # Process simulations in smaller batches
  batch_size <- 25
  chunks <- ceiling(n_sims / batch_size)
  
  for(chunk in 1:chunks) {
    start_idx <- (chunk-1) * batch_size + 1
    end_idx <- min(chunk * batch_size, n_sims)
    chunk_sim_ids <- all_sim_ids[start_idx:end_idx]
    
    message(sprintf("Processing chunk %d/%d (simulations %d to %d)", 
                    chunk, chunks, start_idx, end_idx))
    
    for(sim_id in chunk_sim_ids) {
      sim_data <- sim_results_dt[SimID == sim_id]
      
      # Simple optimization: just get top lineups by total score
      sim_data[, PPD := FDFantasyPoints / (FDSalary/1000)]
      
      # Get top candidates
      top_candidates <- sim_data[order(-FDFantasyPoints)][1:min(15, nrow(sim_data))]
      
      if(nrow(top_candidates) >= FD_ROSTER_SIZE) {
        # Generate ALL possible lineups for this simulation (up to 10 per sim)
        for(rank in 1:10) {  # Generate more lineups per simulation
          # Simple greedy selection with some variation
          selected_players <- top_candidates[1:FD_ROSTER_SIZE]
          total_salary <- sum(selected_players$FDSalary)
          
          if(total_salary <= FD_SALARY_CAP) {
            lineup_str <- paste(sort(selected_players$Name), collapse = "|")
            
            lineup_result <- data.frame(
              Lineup = lineup_str,
              Rank = rank,
              SimID = sim_id,
              TotalScore = sum(selected_players$FDFantasyPoints),
              TotalSalary = total_salary,
              stringsAsFactors = FALSE
            )
            
            all_lineups[[length(all_lineups) + 1]] <- lineup_result
          }
          
          # Shuffle top candidates for next iteration
          if(nrow(top_candidates) > FD_ROSTER_SIZE) {
            top_candidates <- top_candidates[sample(nrow(top_candidates))]
          }
        }
      }
    }
    
    # Progress reporting
    cat(sprintf("Processed %d/%d simulations (%.1f%%)\n", 
                min(end_idx, n_sims), n_sims, 
                min(end_idx, n_sims) / n_sims * 100))
  }
  
  if(length(all_lineups) == 0) return(NULL)
  
  # Combine all results
  combined_lineups <- do.call(rbind, all_lineups)
  
  # Count lineup appearances by rank - KEEP ALL LINEUPS
  lineup_counts <- combined_lineups %>%
    group_by(Lineup) %>%
    summarise(
      Top1Count = sum(Rank == 1),
      Top10Count = sum(Rank <= 10),
      Top50Count = sum(Rank <= 50),
      Top100Count = sum(Rank <= 100),
      TotalSalary = first(TotalSalary),
      .groups = 'drop'
    ) %>%
    arrange(desc(Top100Count), desc(Top10Count), desc(Top1Count)) %>% 
    slice_head(n = 10000)
  
  # Split lineup string into player columns
  player_cols <- do.call(rbind, strsplit(lineup_counts$Lineup, "\\|"))
  colnames(player_cols) <- paste0("Player", 1:FD_ROSTER_SIZE)
  
  # Create final result with ALL lineups
  result <- cbind(
    as.data.frame(player_cols),
    lineup_counts[, c("Top1Count", "Top10Count", "Top50Count", "Top100Count", "TotalSalary")]
  )
  
  # Calculate ownership statistics for ALL lineups
  ownership_map <- NULL
  if("FDOP" %in% names(sim_results)) {
    ownership_data <- unique(sim_results[!is.na(sim_results$FDOP), c("Name", "FDOP")])
    if(nrow(ownership_data) > 0) {
      ownership_map <- setNames(ownership_data$FDOP, ownership_data$Name)
      result <- calculate_golf_lineup_ownership_stats(result, ownership_map, "fd")
    }
  }
  
  # Add default ownership columns if no ownership data
  if(!"CumulativeOwnership" %in% names(result)) {
    result$CumulativeOwnership <- 0
    result$GeometricMeanOwnership <- 0
  }
  
  # **NEW: Add cut metrics if player_data provided**
  if(!is.null(player_data)) {
    message("Calculating cut metrics for lineups...")
    result <- add_cut_metrics_to_lineups(result, player_data, platform = "fd")
  }
  
  cat(sprintf("Generated %d total optimal lineups\n", nrow(result)))
  
  return(result)
}

# Function to calculate filtered pool rates for player exposure
calculate_golf_filtered_pool_rates <- function(optimal_lineups, filters, platform = "dk") {
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame())
  }
  
  # Apply filters to get filtered lineups
  filtered_lineups <- as.data.table(optimal_lineups)
  
  # Apply Top Count filters with NA safety
  if (!is.null(filters$min_top1_count) && filters$min_top1_count > 0 && "Top1Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(Top1Count) & Top1Count >= filters$min_top1_count]
  }
  
  if (!is.null(filters$min_top10_count) && filters$min_top10_count > 0 && "Top2Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(Top2Count) & Top10Count >= filters$min_top10_count]
  }
  
  if (!is.null(filters$min_top50_count) && filters$min_top50_count > 0 && "Top3Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(Top3Count) & Top50Count >= filters$min_top50_count]
  }
  
  if (!is.null(filters$min_top100_count) && filters$min_top100_count > 0 && "Top5Count" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(Top5Count) & Top100Count >= filters$min_top100_count]
  }
  
  if (!is.null(filters$min_atleast6_prob) && filters$min_atleast6_prob > 0 && 
      "AtLeast6" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(AtLeast6) & AtLeast6 >= filters$min_atleast6_prob]
  }
  
  if (!is.null(filters$min_atleast5_prob) && filters$min_atleast5_prob > 0 && 
      "AtLeast5" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(AtLeast5) & AtLeast5 >= filters$min_atleast5_prob]
  }
  
  if (!is.null(filters$min_expected_cuts) && filters$min_expected_cuts > 0 && 
      "ExpectedCuts" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(ExpectedCuts) & ExpectedCuts >= filters$min_expected_cuts]
  }
  
  # Apply minimum salary filter
  if (!is.null(filters$min_salary) && filters$min_salary > 0 && 
      "TotalSalary" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(TotalSalary) & TotalSalary >= filters$min_salary]
  }
  
  
  if(platform == "dk") {
    if (!is.null(filters$cumulative_ownership_range) && length(filters$cumulative_ownership_range) == 2 && 
        "CumulativeOwnership" %in% names(filtered_lineups)) {
      cum_vals <- filtered_lineups$CumulativeOwnership[!is.na(filtered_lineups$CumulativeOwnership)]
      if(length(cum_vals) > 0) {
        data_min <- min(cum_vals)
        data_max <- max(cum_vals)
        filter_min <- filters$cumulative_ownership_range[1]
        filter_max <- filters$cumulative_ownership_range[2]
        
        if(filter_max >= data_min && filter_min <= data_max) {
          filtered_lineups <- filtered_lineups[!is.na(CumulativeOwnership) & 
                                                 CumulativeOwnership >= filter_min & 
                                                 CumulativeOwnership <= filter_max]
        }
      }
    }
    
    if (!is.null(filters$geometric_mean_range) && length(filters$geometric_mean_range) == 2 && 
        "GeometricMeanOwnership" %in% names(filtered_lineups)) {
      geom_vals <- filtered_lineups$GeometricMeanOwnership[!is.na(filtered_lineups$GeometricMeanOwnership)]
      if(length(geom_vals) > 0) {
        data_min <- min(geom_vals)
        data_max <- max(geom_vals)
        filter_min <- filters$geometric_mean_range[1]
        filter_max <- filters$geometric_mean_range[2]
        
        if(filter_max >= data_min && filter_min <= data_max) {
          filtered_lineups <- filtered_lineups[!is.na(GeometricMeanOwnership) & 
                                                 GeometricMeanOwnership >= filter_min & 
                                                 GeometricMeanOwnership <= filter_max]
        }
      }
    }
  }
  
  # Apply player exclusion filter with NA safety
  if (!is.null(filters$excluded_players) && length(filters$excluded_players) > 0) {
    if(platform == "dk") {
      player_cols <- paste0("Player", 1:DK_ROSTER_SIZE)
    } else {
      player_cols <- paste0("Player", 1:FD_ROSTER_SIZE)
    }
    
    if(length(player_cols) > 0) {
      to_exclude <- rep(FALSE, nrow(filtered_lineups))
      
      for(col in player_cols) {
        if(col %in% names(filtered_lineups)) {
          # Add NA safety here too
          col_values <- filtered_lineups[[col]]
          to_exclude <- to_exclude | (!is.na(col_values) & col_values %in% filters$excluded_players)
        }
      }
      
      filtered_lineups <- filtered_lineups[!to_exclude]
    }
  }
  
  # Calculate filtered pool rates
  if(nrow(filtered_lineups) == 0) {
    return(data.frame())
  }
  
  # Get all players from filtered lineups
  if(platform == "dk") {
    player_cols <- paste0("Player", 1:DK_ROSTER_SIZE)
    all_players <- unique(unlist(filtered_lineups[, ..player_cols]))
  } else {
    player_cols <- paste0("Player", 1:FD_ROSTER_SIZE)
    all_players <- unique(unlist(filtered_lineups[, ..player_cols]))
  }
  
  # Calculate filtered pool rates
  filtered_rates <- data.table(
    Name = all_players,
    FilteredPoolRate = 0
  )
  
  for(player in all_players) {
    # Count appearances in filtered lineups
    player_appears <- logical(nrow(filtered_lineups))
    for(col in player_cols) {
      if(col %in% names(filtered_lineups)) {
        player_appears <- player_appears | (filtered_lineups[[col]] == player)
      }
    }
    
    filtered_rates[Name == player, FilteredPoolRate := (sum(player_appears) / nrow(filtered_lineups)) * 100]
  }
  
  return(as.data.frame(filtered_rates))
}


calculate_dk_filtered_pool_stats <- function(optimal_lineups, filters) {
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  filtered_lineups <- as.data.table(optimal_lineups)
  
  if (!is.null(filters$min_top1_count) && filters$min_top1_count > 0) {
    filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
  }
  if (!is.null(filters$min_top10_count) && filters$min_top10_count > 0) {
    filtered_lineups <- filtered_lineups[Top10Count >= filters$min_top10_count]
  }
  if (!is.null(filters$min_top50_count) && filters$min_top50_count > 0) {
    filtered_lineups <- filtered_lineups[Top50Count >= filters$min_top50_count]
  }
  if (!is.null(filters$min_top100_count) && filters$min_top100_count > 0) {
    filtered_lineups <- filtered_lineups[Top100Count >= filters$min_top100_count]
  }
  
  if (!is.null(filters$cumulative_ownership_range) && "CumulativeOwnership" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[CumulativeOwnership >= filters$cumulative_ownership_range[1] & 
                                           CumulativeOwnership <= filters$cumulative_ownership_range[2]]
  }
  
  if (!is.null(filters$geometric_mean_range) && "GeometricMeanOwnership" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[GeometricMeanOwnership >= filters$geometric_mean_range[1] & 
                                           GeometricMeanOwnership <= filters$geometric_mean_range[2]]
  }
  
  # Apply cut filters
  if (!is.null(filters$min_atleast6_prob) && filters$min_atleast6_prob > 0 && 
      "AtLeast6" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(AtLeast6) & AtLeast6 >= filters$min_atleast6_prob]
  }
  
  if (!is.null(filters$min_atleast5_prob) && filters$min_atleast5_prob > 0 && 
      "AtLeast5" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(AtLeast5) & AtLeast5 >= filters$min_atleast5_prob]
  }
  
  if (!is.null(filters$min_expected_cuts) && filters$min_expected_cuts > 0 && 
      "ExpectedCuts" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(ExpectedCuts) & ExpectedCuts >= filters$min_expected_cuts]
  }
  
  # Apply minimum salary filter
  if (!is.null(filters$min_salary) && filters$min_salary > 0 && 
      "TotalSalary" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(TotalSalary) & TotalSalary >= filters$min_salary]
  }
  
  if (!is.null(filters$excluded_players) && length(filters$excluded_players) > 0) {
    player_cols <- paste0("Player", 1:DK_ROSTER_SIZE)
    to_exclude <- rep(FALSE, nrow(filtered_lineups))
    
    for(col in player_cols) {
      if(col %in% names(filtered_lineups)) {
        to_exclude <- to_exclude | filtered_lineups[[col]] %in% filters$excluded_players
      }
    }
    filtered_lineups <- filtered_lineups[!to_exclude]
  }
  
  return(list(count = nrow(filtered_lineups), thresholds = NULL))
}

calculate_fd_filtered_pool_stats <- function(optimal_lineups, filters) {
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(list(count = 0, thresholds = NULL))
  }
  
  filtered_lineups <- as.data.table(optimal_lineups)
  
  if (!is.null(filters$min_top1_count) && filters$min_top1_count > 0) {
    filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
  }
  if (!is.null(filters$min_top10_count) && filters$min_top10_count > 0) {
    filtered_lineups <- filtered_lineups[Top10Count >= filters$min_top10_count]
  }
  if (!is.null(filters$min_top50_count) && filters$min_top50_count > 0) {
    filtered_lineups <- filtered_lineups[Top50Count >= filters$min_top50_count]
  }
  if (!is.null(filters$min_top100_count) && filters$min_top100_count > 0) {
    filtered_lineups <- filtered_lineups[Top100Count >= filters$min_top100_count]
  }
  
  if (!is.null(filters$excluded_players) && length(filters$excluded_players) > 0) {
    player_cols <- paste0("Player", 1:FD_ROSTER_SIZE)
    to_exclude <- rep(FALSE, nrow(filtered_lineups))
    
    for(col in player_cols) {
      to_exclude <- to_exclude | filtered_lineups[[col]] %in% filters$excluded_players
    }
    filtered_lineups <- filtered_lineups[!to_exclude]
  }
  
  return(list(count = nrow(filtered_lineups), thresholds = NULL))
}


# Simplified DraftKings random lineup generation for golf
generate_random_dk_golf_lineups <- function(optimal_lineups, filters) {
  # Convert to data.table for efficiency
  setDT(optimal_lineups)
  filtered_lineups <- copy(optimal_lineups)
  
  # Apply filters
  if (!is.null(filters$min_top1_count) && filters$min_top1_count > 0) {
    filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
  }
  
  if (!is.null(filters$min_top10_count) && filters$min_top10_count > 0) {
    filtered_lineups <- filtered_lineups[Top10Count >= filters$min_top10_count]
  }
  
  if (!is.null(filters$min_top50_count) && filters$min_top50_count > 0) {
    filtered_lineups <- filtered_lineups[Top50Count >= filters$min_top50_count]
  }
  
  if (!is.null(filters$min_top100_count) && filters$min_top100_count > 0) {
    filtered_lineups <- filtered_lineups[Top100Count >= filters$min_top100_count]
  }
  
  # Apply ownership range filters
  if (!is.null(filters$cumulative_ownership_range) && length(filters$cumulative_ownership_range) == 2 && "CumulativeOwnership" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[CumulativeOwnership >= filters$cumulative_ownership_range[1] & 
                                           CumulativeOwnership <= filters$cumulative_ownership_range[2]]
  }
  
  if (!is.null(filters$geometric_mean_range) && length(filters$geometric_mean_range) == 2 && "GeometricMeanOwnership" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[GeometricMeanOwnership >= filters$geometric_mean_range[1] & 
                                           GeometricMeanOwnership <= filters$geometric_mean_range[2]]
  }
  
  # Apply cut filters
  if (!is.null(filters$min_atleast6_prob) && filters$min_atleast6_prob > 0 && 
      "AtLeast6" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(AtLeast6) & AtLeast6 >= filters$min_atleast6_prob]
  }
  
  if (!is.null(filters$min_atleast5_prob) && filters$min_atleast5_prob > 0 && 
      "AtLeast5" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(AtLeast5) & AtLeast5 >= filters$min_atleast5_prob]
  }
  
  if (!is.null(filters$min_expected_cuts) && filters$min_expected_cuts > 0 && 
      "ExpectedCuts" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(ExpectedCuts) & ExpectedCuts >= filters$min_expected_cuts]
  }
  
  # Apply minimum salary filter
  if (!is.null(filters$min_salary) && filters$min_salary > 0 && 
      "TotalSalary" %in% names(filtered_lineups)) {
    filtered_lineups <- filtered_lineups[!is.na(TotalSalary) & TotalSalary >= filters$min_salary]
  }
  
  # Check if any lineups match filters
  if (nrow(filtered_lineups) == 0) {
    return(NULL)
  }
  
  # Prepare for tracking
  player_cols <- paste0("Player", 1:DK_ROSTER_SIZE)
  all_players <- unique(unlist(filtered_lineups[, ..player_cols]))
  player_counts <- setNames(numeric(length(all_players)), all_players)
  
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
    
    # Update player counts for exposure calculation
    candidates_players <- unlist(filtered_lineups[selected_idx, ..player_cols])
    player_counts[candidates_players] <- player_counts[candidates_players] + 1
  }
  
  if (nrow(selected_lineups) == 0) return(NULL)
  
  # Calculate exposure for attribute
  final_exposure <- (player_counts / nrow(selected_lineups)) * 100
  attr(selected_lineups, "exposure") <- final_exposure
  
  # Keep only needed columns
  keep_cols <- c(player_cols, "Top1Count", "Top10Count", "Top50Count", "Top100Count", "TotalSalary")
  keep_cols <- intersect(keep_cols, names(selected_lineups))
  
  if(!is.null(selected_lineups) && nrow(selected_lineups) > 0) {
    # Create ownership mapping from player_counts attribute or calculate from filtered_lineups
    ownership_map <- NULL
    if(!is.null(filtered_lineups) && "DKOP" %in% names(filtered_lineups)) {
      # Get ownership data from the first occurrence of each player
      ownership_data <- filtered_lineups[, .(DKOP = first(DKOP)), by = Name]
      ownership_map <- setNames(ownership_data$DKOP, ownership_data$Name)
    }
    
    if(!is.null(ownership_map)) {
      selected_lineups <- calculate_golf_lineup_ownership_stats(selected_lineups, ownership_map, "dk")
    }
  }
  
  return(as.data.frame(selected_lineups[, ..keep_cols]))
}

# FanDuel random lineup generation for golf
generate_random_fd_golf_lineups <- function(optimal_lineups, filters) {
  # Apply filters with early return if empty
  if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(NULL)
  }
  
  # Convert to data.table for better performance
  filtered_lineups <- as.data.table(optimal_lineups)
  
  # Apply min threshold filters efficiently with data.table syntax
  if (!is.null(filters$min_top1_count) && filters$min_top1_count > 0) {
    filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
  }
  
  if (!is.null(filters$min_top10_count) && filters$min_top10_count > 0) {
    filtered_lineups <- filtered_lineups[Top10Count >= filters$min_top10_count]
  }
  
  if (!is.null(filters$min_top50_count) && filters$min_top50_count > 0) {
    filtered_lineups <- filtered_lineups[Top50Count >= filters$min_top50_count]
  }
  
  if (!is.null(filters$min_top100_count) && filters$min_top100_count > 0) {
    filtered_lineups <- filtered_lineups[Top100Count >= filters$min_top100_count]
  }
  
  # Exclude specific players efficiently
  if (!is.null(filters$excluded_players) && length(filters$excluded_players) > 0) {
    player_cols <- paste0("Player", 1:FD_ROSTER_SIZE)
    to_exclude <- rep(FALSE, nrow(filtered_lineups))
    
    for(col in player_cols) {
      to_exclude <- to_exclude | filtered_lineups[[col]] %in% filters$excluded_players
    }
    
    filtered_lineups <- filtered_lineups[!to_exclude]
  }
  
  # Early return if no lineups match the filters
  if (nrow(filtered_lineups) == 0) {
    return(NULL)
  }
  
  # Pre-allocate for tracking
  player_cols <- paste0("Player", 1:FD_ROSTER_SIZE)
  all_players <- unique(unlist(filtered_lineups[, ..player_cols]))
  player_counts <- setNames(numeric(length(all_players)), all_players)
  
  # Use the selected lineups data.table
  selected_lineups <- data.table()
  selected_indices <- integer(0)
  
  # Use Top1Count as weight for sampling
  weight_col <- "Top1Count"
  
  # Sampling loop with early termination if we can't find enough lineups
  attempts <- 0
  max_attempts <- filters$num_lineups * 10
  
  # For logging progress
  if(filters$num_lineups > 50) {
    cat("Generating", filters$num_lineups, "FanDuel lineups from a pool of", 
        nrow(filtered_lineups), "filtered lineups...\n")
  }
  
  # Progress reporting for larger lineup sets
  report_interval <- max(1, ceiling(filters$num_lineups / 5))
  
  while (nrow(selected_lineups) < filters$num_lineups && attempts < max_attempts) {
    attempts <- attempts + 1
    
    # Available lineups (those not already selected)
    available_indices <- setdiff(1:nrow(filtered_lineups), selected_indices)
    if (length(available_indices) == 0) break
    
    # Sample based on weights (probabilities) - handle edge cases
    weights <- filtered_lineups[[weight_col]][available_indices]
    if (sum(weights) == 0) weights <- rep(1, length(available_indices))
    
    selected_idx <- tryCatch({
      sample(available_indices, 1, prob = weights)
    }, error = function(e) {
      sample(available_indices, 1)
    })
    
    # Get the selected lineup
    candidate_lineup <- filtered_lineups[selected_idx]
    
    # Get players from this lineup
    candidate_players <- unlist(candidate_lineup[, ..player_cols])
    
    # Update player counts
    for(player in candidate_players) {
      player_counts[player] <- player_counts[player] + 1
    }
    
    # Add lineup unconditionally
    selected_lineups <- rbind(selected_lineups, candidate_lineup)
    selected_indices <- c(selected_indices, selected_idx)
    
    # Progress reporting for larger lineup sets
    if(filters$num_lineups > 50 && nrow(selected_lineups) %% report_interval == 0) {
      cat("Generated", nrow(selected_lineups), "of", filters$num_lineups, "lineups...\n")
    }
    
    # Periodic cleanup for very large lineup sets
    if (attempts %% 1000 == 0) gc(verbose = FALSE)
  }
  
  # Return NULL if no lineups were selected
  if (nrow(selected_lineups) == 0) return(NULL)
  
  # Calculate exposure percentages for each player
  final_exposure <- (player_counts / nrow(selected_lineups)) * 100
  attr(selected_lineups, "exposure") <- final_exposure
  
  if(!is.null(selected_lineups) && nrow(selected_lineups) > 0) {
    # Create ownership mapping from player_counts attribute or calculate from filtered_lineups
    ownership_map <- NULL
    if(!is.null(filtered_lineups) && "FDOP" %in% names(filtered_lineups)) {
      # Get ownership data from the first occurrence of each player
      ownership_data <- filtered_lineups[, .(FDOP = first(FDOP)), by = Name]
      ownership_map <- setNames(ownership_data$FDOP, ownership_data$Name)
    }
    
    if(!is.null(ownership_map)) {
      selected_lineups <- calculate_golf_lineup_ownership_stats(selected_lineups, ownership_map, "fd")
    }
  }
  
  # Return the data frame with exposure attribute
  return(as.data.frame(selected_lineups))
}

# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(
    title = tags$div(style = "display: flex; align-items: center; font-weight: bold;", "Golf Fantasy Sims"),
    titleWidth = 350
  ),
  
  # Dashboard sidebar
  dashboardSidebar(
    useShinyjs(),
    div(
      style = "text-align: center; padding: 10px; margin-bottom: 5px;",
      tags$img(src = "logo.jpg", height = "200px", width = "auto", 
               style = "border: 2px solid #FFD700; border-radius: 10px;")
    ),
    div(style = "text-align: center; padding: 10px;"),
    br(),
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Input Check", tabName = "upload", icon = icon("upload")),
      menuItem("Finish Analysis", tabName = "finish_analysis", icon = icon("chart-line")),
      menuItem("Fantasy Projections", tabName = "fantasy", icon = icon("calculator")),
      menuItem("Optimal Lineups", tabName = "optimal_lineups", icon = icon("trophy")),
      menuItem("Lineup Builder", tabName = "lineup_builder", icon = icon("percentage"))
    ),
    br(),
    fileInput("excel_file", "Upload Excel File", accept = c(".xlsx")),
    
    # Tournament Configuration Section
    div(
      style = "background-color: #333; padding: 10px; margin: 10px 0; border-radius: 5px;",
      h4("Tournament Settings", style = "color: #FFD700; margin-top: 0;"),
      checkboxInput("no_cut", "No Cut Tournament", value = FALSE),
      conditionalPanel(
        condition = "!input.no_cut",
        numericInput("cut_line", "Cut Line (+ ties):", value = 65, min = 50, max = 85, step = 5)
      )
    ),
    
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
            DTOutput("player_stats") %>% withSpinner(color = "#ff6600")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Finish Position Boxplot",
            plotlyOutput("position_box", height = "1000px") %>% withSpinner(color = "#ff6600")
          )
        )
      ),
      
      # Fantasy Projections Tab
      tabItem(tabName = "fantasy", uiOutput("fantasy_ui")),
      tabItem(tabName = "optimal_lineups",
              fluidRow(
                box(
                  width = 12, 
                  title = "Run Lineup Optimization",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  # Radio buttons to select platform - only one at a time
                  # Radio buttons to select platform - now includes Tiers
                  radioButtons(
                    "platform_selection", 
                    "Select Fantasy Platform:",
                    choices = list(
                      "DraftKings" = "dk",
                      "FanDuel" = "fd",
                      "DK Tiers" = "tiers"
                    ),
                    selected = "dk",
                    inline = TRUE
                  ),
                  
                  # Conditional panel for DraftKings options
                  conditionalPanel(
                    condition = "input.platform_selection == 'dk' && output.has_draftkings == 'true'",
                    fluidRow(
                      column(6,
                             actionButton("run_dk_optimization", "Calculate DraftKings Lineups",
                                          class = "btn-primary", 
                                          style = "width: 100%; margin-top: 10px;")
                      ),
                      column(6,
                             div(
                               style = "margin-top: 10px;",
                               uiOutput("dk_optimization_status")
                             )
                      )
                    )
                  ),
                  
                  # Conditional panel for FanDuel options
                  conditionalPanel(
                    condition = "input.platform_selection == 'fd' && output.has_fanduel == 'true'",
                    fluidRow(
                      column(6,
                             actionButton("run_fd_optimization", "Calculate FanDuel Lineups",
                                          class = "btn-primary", 
                                          style = "width: 100%; margin-top: 10px;")
                      ),
                      column(6,
                             div(
                               style = "margin-top: 10px;",
                               uiOutput("fd_optimization_status")
                             )
                      )
                    )
                  ),
                  
                  # Conditional panel for Tiers options
                  conditionalPanel(
                    condition = "input.platform_selection == 'tiers' && output.has_tiers == 'true'",
                    fluidRow(
                      column(6,
                             actionButton("run_tiers_optimization", "Calculate DK Tiers Lineups",
                                          class = "btn-primary", 
                                          style = "width: 100%; margin-top: 10px;")
                      ),
                      column(6,
                             div(
                               style = "margin-top: 10px;",
                               uiOutput("tiers_optimization_status")
                             )
                      )
                    )
                  ),
                  
                  # Show warning if Tiers not available
                  conditionalPanel(
                    condition = "input.platform_selection == 'tiers' && output.has_tiers != 'true'",
                    div(
                      class = "alert alert-warning",
                      style = "margin-top: 10px;",
                      "Tiers data is not available in the input file. Please ensure your file contains a 'Tiers' sheet with columns: TiersName, Name, ID, Tier."
                    )
                  ),
                  
                  # Show warning if platform not available
                  conditionalPanel(
                    condition = "(input.platform_selection == 'dk' && output.has_draftkings != 'true') || 
               (input.platform_selection == 'fd' && output.has_fanduel != 'true')",
                    div(
                      class = "alert alert-warning",
                      style = "margin-top: 10px;",
                      "The selected platform data is not available in the input file. Please check your file or select another platform."
                    )
                  )
                )
              ),
              
              # DraftKings results section - only shown when DK selected and lineups available
              conditionalPanel(
                condition = "input.platform_selection == 'dk' && output.has_dk_lineups == 'true'",
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
              
              # FanDuel results section - only shown when FD selected and lineups available
              conditionalPanel(
                condition = "input.platform_selection == 'fd' && output.has_fd_lineups == 'true'",
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
              ),
              # Tiers results section - only shown when Tiers selected and lineups available
              conditionalPanel(
                condition = "input.platform_selection == 'tiers' && output.has_tiers_lineups == 'true'",
                fluidRow(
                  box(
                    width = 12,
                    title = "DK Tiers Optimal Lineups",
                    div(
                      style = "text-align: right; margin-bottom: 10px;",
                      downloadButton('download_tiers_optimal_lineups', 'Download All Tiers Lineups',
                                     style = "margin-top: 10px;")
                    ),
                    DTOutput("tiers_optimal_lineups_table")
                  )
                )
              )
      ),
      
      # Lineup Builder Tab - add this to your tabItems  
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
    has_tiers = FALSE,  # ADD THIS
    tiers_data = NULL,  # ADD THIS
    finishing_analysis = NULL,
    dk_fantasy_analysis = NULL,
    fd_fantasy_analysis = NULL,
    dk_optimal_lineups = NULL,
    fd_optimal_lineups = NULL,
    tiers_optimal_lineups = NULL,  # ADD THIS
    dk_player_exposure = NULL,
    fd_player_exposure = NULL,
    dk_random_lineups = NULL,
    fd_random_lineups = NULL,
    file_uploaded = FALSE,
    simulation_complete = FALSE
  )
  
  # File upload handler
  observeEvent(input$excel_file, {
    req(input$excel_file)
    
    # Show progress notification
    withProgress(message = 'Reading file...', value = 0, {
      tryCatch({
        # Read the input file
        rv$input_data <- read_golf_input_file(input$excel_file$datapath)
        rv$file_uploaded <- TRUE
        rv$simulation_complete <- FALSE
        
        # Reset all other values
        rv$processed_data <- NULL
        rv$simulation_results <- NULL
        rv$finishing_analysis <- NULL
        rv$dk_fantasy_analysis <- NULL
        rv$fd_fantasy_analysis <- NULL
        rv$simulation_complete <- FALSE
        
        # Store platform availability
        rv$has_draftkings <- rv$input_data$platform_info$has_draftkings
        rv$has_fanduel <- rv$input_data$platform_info$has_fanduel
        rv$has_tiers <- rv$input_data$platform_info$has_tiers 
        
        # Process tiers data if available
        if(rv$has_tiers) {
          rv$tiers_data <- read_tiers_data(rv$input_data)
          if(is.null(rv$tiers_data)) {
            rv$has_tiers <- FALSE
            showModal(modalDialog(
              title = "Tiers Data Error",
              "Tiers sheet found but could not be processed. Please check the format.",
              easyClose = TRUE
            ))
          }
        }
        
        # Process the data
        incProgress(0.5, detail = "Processing data...")
        rv$processed_data <- process_golf_input_data(rv$input_data)
        
        # Update the data preview
        output$data_preview <- renderDT({
          req(rv$input_data$sheets$Player)
          
          # Create a filtered version of the data for display
          display_data <- rv$input_data$sheets$Player
          
          # Remove ID columns if they exist
          columns_to_remove <- c("DKName", "FDName", "DKID", "FDID")
          for (col in columns_to_remove) {
            if (col %in% colnames(display_data)) {
              display_data[[col]] <- NULL
            }
          }
          
          # Create the datatable
          dt <- datatable(
            display_data,
            options = list(
              scrollX = TRUE,
              pageLength = -1,
              dom = "t",
              ordering = TRUE,
              columnDefs = list(
                list(className = "dt-center", targets = "_all")
              ),
              scrollCollapse = TRUE,
              fixedColumns = TRUE
            ),
            class = "cell-border stripe display compact",
            rownames = FALSE,
            width = "100%",
            height = "auto"
          )
          
          # Format ownership columns
          if ("DKOP" %in% colnames(display_data)) {
            dt <- dt %>% formatRound("DKOP", digits = 2)
          }
          
          if ("FDOP" %in% colnames(display_data)) {
            dt <- dt %>% formatRound("FDOP", digits = 2)
          }
          
          # Format probability columns to 2 decimal places - now includes T30, T40, Cut
          probability_cols <- c("W", "T5", "T10", "T20", "T30", "T40", "Cut")
          for (col in probability_cols) {
            if (col %in% colnames(display_data)) {
              dt <- dt %>% formatRound(col, digits = 3)
            }
          }
          
          # Format salary columns as currency
          if ("DKSalary" %in% colnames(display_data)) {
            dt <- dt %>% formatCurrency("DKSalary", currency = "$", digits = 0)
          }
          
          if ("FDSalary" %in% colnames(display_data)) {
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
  
  observeEvent(input$run_sim, {
    req(rv$processed_data)
    
    # Clear previous results
    rv$simulation_results <- NULL
    rv$finishing_analysis <- NULL
    rv$dk_fantasy_analysis <- NULL
    rv$fd_fantasy_analysis <- NULL
    
    # Get cut configuration from inputs
    cut_line <- if(!is.null(input$cut_line)) input$cut_line else 65
    no_cut <- if(!is.null(input$no_cut)) input$no_cut else FALSE
    
    # Show progress dialog
    withProgress(message = "Running simulations...", value = 0, {
      # Show a specific modal
      setProgress(0.1, detail = "Initializing simulation...")
      
      simulation_results <- run_golf_simulation(
        rv$processed_data, 
        n_sims = input$n_sims,
        cut_line = cut_line,
        no_cut = no_cut
      )
      
      # Store results
      rv$simulation_results <- simulation_results$results
      
      # Update platform availability
      rv$has_draftkings <- "DKFantasyPoints" %in% names(rv$simulation_results)
      rv$has_fanduel <- "FDFantasyPoints" %in% names(rv$simulation_results)
      
      # Process finish position analysis with cut configuration
      setProgress(0.7, detail = "Analyzing finishing positions...")
      rv$finishing_analysis <- analyze_golf_finishing_positions(
        rv$simulation_results, 
        cut_line = cut_line, 
        no_cut = no_cut
      )
      
      # Process fantasy points analysis for each platform (unchanged)
      setProgress(0.9, detail = "Analyzing fantasy points...")
      if (rv$has_draftkings) {
        rv$dk_fantasy_analysis <- analyze_dk_golf_fantasy_points(rv$simulation_results, cut_line)
      }
      
      if (rv$has_fanduel) {
        rv$fd_fantasy_analysis <- analyze_fd_golf_fantasy_points(rv$simulation_results, cut_line)
      }
      
      # Mark simulation as complete
      rv$simulation_complete <- TRUE
      
      cat("Golf simulation completed successfully with", nrow(rv$simulation_results), "total results\n")
      
      # Switch to finish analysis tab
      updateTabItems(session, "sidebar_menu", selected = "finish_analysis")
      
      # Show success message
      showModal(
        modalDialog(
          title = "Success",
          "Golf simulation completed successfully! Review the results in the analysis tabs.",
          easyClose = TRUE
        )
      )
    })
  })
  
  
  
  
  # Upload content
  output$upload_content <- renderUI({
    if (rv$file_uploaded) {
      tagList(
        fluidRow(
          box(
            width = 12,
            title = "Input Data Preview",
            DTOutput("data_preview") %>% withSpinner(color = "#ff6600")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "DraftKings Points Table",
            DTOutput("dk_points_preview") %>% withSpinner(color = "#ff6600")
          ),
          box(
            width = 6,
            title = "FanDuel Points Table", 
            DTOutput("fd_points_preview") %>% withSpinner(color = "#ff6600")
          )
        )
      )
    }
  })
  
  # Points table previews
  output$dk_points_preview <- renderDT({
    req(rv$processed_data$dk_points)
    
    datatable(
      rv$processed_data$dk_points,
      options = list(
        scrollY = "400px",
        pageLength = -1,
        dom = "t",
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe compact'
    )
  })
  
  
  
  
  output$fd_points_preview <- renderDT({
    req(rv$processed_data$fd_points)
    
    datatable(
      rv$processed_data$fd_points,
      options = list(
        scrollY = "400px",
        pageLength = -1,
        dom = "t",
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe compact'
    )
  })
  
  
  output$has_tiers <- reactive({
    result <- tolower(as.character(rv$has_tiers))
    return(result)
  })
  outputOptions(output, "has_tiers", suspendWhenHidden = FALSE)
  
  output$has_tiers_lineups <- reactive({
    result <- tolower(as.character(!is.null(rv$tiers_optimal_lineups) && nrow(rv$tiers_optimal_lineups) > 0))
    return(result)
  })
  outputOptions(output, "has_tiers_lineups", suspendWhenHidden = FALSE)
  
  output$tiers_optimal_lineups_table <- renderDT({
    req(rv$tiers_optimal_lineups)
    
    display_data <- as.data.frame(rv$tiers_optimal_lineups)
    
    cat(sprintf("Displaying tiers table with %d rows\n", nrow(display_data)))
    
    # Show top 1000 for display
    if(nrow(display_data) > 1000) {
      display_data <- display_data[1:1000, ]
    }
    
    # Select columns for display
    display_cols <- c("Tier1Name", "Tier2Name", "Tier3Name", "Tier4Name", "Tier5Name", "Tier6Name",
                      "Top1Count", "Top10Count", "Top100Count", "Top1000Count")
    
    display_data <- display_data[, display_cols]
    
    # Rename for cleaner display
    colnames(display_data) <- c("Tier 1", "Tier 2", "Tier 3", "Tier 4", "Tier 5", "Tier 6",
                                "Top 1", "Top 10", "Top 100", "Top 1000")
    
    dt <- datatable(
      display_data,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: center; color: #FFD700; font-weight: bold;",
        sprintf("Showing Top %d Tier Lineups", nrow(display_data))
      ),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        rownames = FALSE,
        dom = "ftp",
        ordering = TRUE,
        processing = TRUE,
        serverSide = FALSE,
        order = list(list(6, 'desc')),  # Sort by Top 1 (column index 6)
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Style count columns with color bars
    count_cols <- c("Top 1", "Top 10", "Top 100", "Top 1000")
    for(col in count_cols) {
      if(col %in% names(display_data) && any(!is.na(display_data[[col]]))) {
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
  
  # Tiers optimal lineups download handler
  output$download_tiers_optimal_lineups <- downloadHandler(
    filename = function() {
      paste("tiers_golf_optimal_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      if(is.null(rv$tiers_optimal_lineups) || nrow(rv$tiers_optimal_lineups) == 0) {
        empty_data <- data.frame(
          Tier1 = character(0), Tier2 = character(0), Tier3 = character(0),
          Tier4 = character(0), Tier5 = character(0), Tier6 = character(0),
          Top1Count = integer(0), Top10Count = integer(0), 
          Top100Count = integer(0), Top1000Count = integer(0)
        )
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      # Create download data using TiersName columns only
      download_data <- rv$tiers_optimal_lineups[, c("Tier1Name", "Tier2Name", "Tier3Name", 
                                                    "Tier4Name", "Tier5Name", "Tier6Name",
                                                    "Top1Count", "Top10Count", 
                                                    "Top100Count", "Top1000Count")]
      
      # Rename columns to just Tier1-6
      colnames(download_data)[1:6] <- paste0("Tier", 1:6)
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  
  
  output$dk_filtered_pool_size <- renderText({
    if(is.null(rv$dk_optimal_lineups) || nrow(rv$dk_optimal_lineups) == 0) {
      return("Number of lineups in filtered pool: 0")
    }
    
    filters <- list(
      min_top1_count = if(!is.null(input$dk_min_top1_count)) input$dk_min_top1_count else 0,
      min_top10_count = if(!is.null(input$dk_min_top10_count)) input$dk_min_top10_count else 0,
      min_top50_count = if(!is.null(input$dk_min_top50_count)) input$dk_min_top50_count else 0,
      min_top100_count = if(!is.null(input$dk_min_top100_count)) input$dk_min_top100_count else 0,
      cumulative_ownership_range = if(!is.null(input$dk_cumulative_ownership_range)) {
        input$dk_cumulative_ownership_range
      } else {
        if("CumulativeOwnership" %in% names(rv$dk_optimal_lineups)) {
          cum_vals <- rv$dk_optimal_lineups$CumulativeOwnership[!is.na(rv$dk_optimal_lineups$CumulativeOwnership)]
          if(length(cum_vals) > 0) c(min(cum_vals), max(cum_vals)) else c(0, 100)
        } else c(0, 100)
      },
      geometric_mean_range = if(!is.null(input$dk_geometric_mean_range)) {
        input$dk_geometric_mean_range
      } else {
        if("GeometricMeanOwnership" %in% names(rv$dk_optimal_lineups)) {
          geom_vals <- rv$dk_optimal_lineups$GeometricMeanOwnership[!is.na(rv$dk_optimal_lineups$GeometricMeanOwnership)]
          if(length(geom_vals) > 0) c(min(geom_vals), max(geom_vals)) else c(0, 100)
        } else c(0, 100)
      },
      min_atleast6_prob = if(!is.null(input$dk_min_atleast6_prob)) input$dk_min_atleast6_prob else 0,
      min_atleast5_prob = if(!is.null(input$dk_min_atleast5_prob)) input$dk_min_atleast5_prob else 0,
      min_expected_cuts = if(!is.null(input$dk_min_expected_cuts)) input$dk_min_expected_cuts else 0,
      min_salary = if(!is.null(input$dk_min_salary)) input$dk_min_salary else 0,
      excluded_players = if(!is.null(input$dk_excluded_players)) input$dk_excluded_players else character(0)
    )
    
    stats <- tryCatch({
      calculate_dk_filtered_pool_stats(rv$dk_optimal_lineups, filters)
    }, error = function(e) {
      list(count = nrow(rv$dk_optimal_lineups), thresholds = NULL)
    })
    
    paste("Number of lineups in filtered pool:", stats$count)
  })
  
  # FanDuel filtered pool size display
  output$fd_filtered_pool_size <- renderText({
    if(is.null(rv$fd_optimal_lineups) || nrow(rv$fd_optimal_lineups) == 0) {
      return("Number of lineups in filtered pool: 0")
    }
    
    filters <- list(
      min_top1_count = if(!is.null(input$fd_min_top1_count)) input$fd_min_top1_count else 0,
      min_top10_count = if(!is.null(input$fd_min_top10_count)) input$fd_min_top10_count else 0,
      min_top50_count = if(!is.null(input$fd_min_top50_count)) input$fd_min_top50_count else 0,
      min_top100_count = if(!is.null(input$fd_min_top100_count)) input$fd_min_top100_count else 0,
      excluded_players = if(!is.null(input$fd_excluded_players)) input$fd_excluded_players else character(0)
    )
    
    stats <- tryCatch({
      calculate_fd_filtered_pool_stats(rv$fd_optimal_lineups, filters)
    }, error = function(e) {
      list(count = 0, thresholds = NULL)
    })
    
    paste("Number of lineups in filtered pool:", stats$count)
  })
  
  
  
  output$dk_optimal_lineups_table <- renderDT({
    req(rv$dk_optimal_lineups)
    
    display_data <- as.data.frame(rv$dk_optimal_lineups)
    
    if(nrow(display_data) > 1000) {
      display_data <- display_data %>%
        arrange(desc(Top100Count), desc(Top10Count), desc(Top1Count)) %>%
        head(1000)
    }
    
    # Ensure we have all expected columns
    expected_cols <- c(paste0("Player", 1:DK_ROSTER_SIZE), 
                       "Top1Count", "Top10Count", "Top50Count", "Top100Count", 
                       "TotalSalary", "CumulativeOwnership", "GeometricMeanOwnership",
                       "ExpectedCuts", "AtLeast6", "AtLeast5")
    
    # Keep only columns that exist
    cols_to_keep <- intersect(expected_cols, names(display_data))
    display_data <- display_data[, cols_to_keep, drop = FALSE]
    
    dt <- datatable(
      display_data,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: center; color: #FFD700; font-weight: bold;",
        sprintf("Showing Top %d Optimal Lineups", nrow(display_data))
      ),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        rownames = FALSE,
        dom = "ftp",
        ordering = TRUE,
        processing = TRUE,
        serverSide = FALSE,  # Keep FALSE since we pre-filtered to 10k
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Format columns (same as before)
    if("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    if("CumulativeOwnership" %in% names(display_data)) {
      dt <- dt %>% formatRound('CumulativeOwnership', digits = 1)
    }
    
    if("GeometricMeanOwnership" %in% names(display_data)) {
      dt <- dt %>% formatRound('GeometricMeanOwnership', digits = 2)
    }
    
    # Style count columns
    count_cols <- c("Top1Count", "Top10Count", "Top50Count", "Top100Count")
    count_cols <- intersect(count_cols, names(display_data))
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
    
    display_data <- as.data.frame(rv$fd_optimal_lineups)
    
    # Ensure we have all expected columns
    expected_cols <- c(paste0("Player", 1:FD_ROSTER_SIZE), 
                       "Top1Count", "Top10Count", "Top50Count", "Top100Count", 
                       "TotalSalary", "CumulativeOwnership", "GeometricMeanOwnership")
    
    # Keep only columns that exist
    cols_to_keep <- intersect(expected_cols, names(display_data))
    display_data <- display_data[, cols_to_keep, drop = FALSE]
    
    dt <- datatable(
      display_data,
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: center; color: #FFD700; font-weight: bold;",
        sprintf("Showing Top %d Optimal Lineups", nrow(display_data))
      ),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        rownames = FALSE,
        dom = "ftp",
        ordering = TRUE,
        processing = TRUE,
        serverSide = FALSE,  # Keep FALSE since we pre-filtered to 10k
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Format columns (same as before)
    if("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    if("CumulativeOwnership" %in% names(display_data)) {
      dt <- dt %>% formatRound('CumulativeOwnership', digits = 1)
    }
    
    if("GeometricMeanOwnership" %in% names(display_data)) {
      dt <- dt %>% formatRound('GeometricMeanOwnership', digits = 2)
    }
    
    # Style count columns
    count_cols <- c("Top1Count", "Top10Count", "Top50Count", "Top100Count")
    count_cols <- intersect(count_cols, names(display_data))
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
  
  output$player_stats <- renderDT({
    req(rv$finishing_analysis)
    
    analysis_data <- rv$finishing_analysis
    
    # Add salary data from input data
    if (!is.null(rv$processed_data$players)) {
      players_data <- as.data.table(rv$processed_data$players)
      
      # Identify available columns to join
      join_cols <- c("Name")
      
      # Add salary columns based on available platforms
      if (rv$has_draftkings && "DKSalary" %in% names(players_data)) {
        join_cols <- c(join_cols, "DKSalary")
      }
      
      if (rv$has_fanduel && "FDSalary" %in% names(players_data)) {
        join_cols <- c(join_cols, "FDSalary")
      }
      
      # Join data with available columns
      if (length(join_cols) > 1) {
        join_cols <- intersect(join_cols, names(players_data))
        
        analysis_data <- merge(analysis_data,
                               players_data[, ..join_cols],
                               by = "Name",
                               all.x = TRUE)
        
        # Create dynamic column order based on available columns
        col_order <- c("Name")
        
        # Add salary columns if available
        if ("DKSalary" %in% names(analysis_data)) {
          col_order <- c(col_order, "DKSalary")
        }
        if ("FDSalary" %in% names(analysis_data)) {
          col_order <- c(col_order, "FDSalary")
        }
        
        # Add stats columns in logical order
        stats_cols <- c("Avg_Finish", "Median", "Win_Rate", "T5_Rate", "T10_Rate", 
                        "T20_Rate", "T30_Rate", "T40_Rate")
        
        # Add cut columns if they exist
        if ("Cut_Rate" %in% names(analysis_data)) {
          stats_cols <- c(stats_cols, "Cut_Rate", "MissedCut_Rate")
        }
        
        col_order <- c(col_order, intersect(stats_cols, names(analysis_data)))
        
        # Ensure all columns exist
        col_order <- intersect(col_order, names(analysis_data))
        
        # Use proper data.table syntax
        analysis_data <- analysis_data[, ..col_order]
      }
    }
    
    # Prepare the table with server-side processing
    dt <- datatable(
      analysis_data,
      options = list(
        scrollX = TRUE,
        pageLength = 25,  # Reduced from -1 (show all)
        dom = "ftp",      # Added filtering and pagination
        order = list(list(which(names(analysis_data) == "Avg_Finish"), "asc")),
        processing = TRUE,
        columnDefs = list(list(
          targets = "_all", className = "dt-center"
        ))
      ),
      rownames = FALSE,
      class = "cell-border stripe"
    )
    
    # Format Salary columns
    if ("DKSalary" %in% names(analysis_data)) {
      dt <- dt %>% formatCurrency("DKSalary", currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    if ("FDSalary" %in% names(analysis_data)) {
      dt <- dt %>% formatCurrency("FDSalary", currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    dt
  })
  
  output$position_box <- renderPlotly({
    req(rv$simulation_results)
    
    # Get average finish position for all players
    avg_finish <- rv$simulation_results[, .(avg_pos = mean(FinishPosition)), by = Name]
    
    # Get top 50 players by best average finish (lowest number = better)
    top_50_players <- avg_finish[order(avg_pos)][1:min(50, nrow(avg_finish))]$Name
    
    # Filter simulation results to only top 50 players
    plot_data <- rv$simulation_results[Name %in% top_50_players]
    
    # Create ordered factor for consistent plotting
    plot_data[, Name := factor(Name, levels = top_50_players)]
    
    # Plot with top 50 players only
    p <- ggplot(plot_data,
                aes(
                  x = Name,
                  y = FinishPosition,
                  fill = Name
                )) +
      geom_boxplot(
        alpha = 0.7,
        outlier.color = "red",
        outlier.size = 2
      ) +
      coord_flip() +
      theme_minimal() +
      theme(
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
        legend.position = "none"
      ) +
      labs(x = "Player (Top 50 by Avg Finish)", y = "Finish Position", title = NULL)
    
    ggplotly(p, height = 1000) %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 150, r = 40, b = 60, t = 30, pad = 5),
        font = list(family = "Arial", size = 12)
      )
  })
  
  # Generate dynamic fantasy UI based on available platforms
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
              downloadButton('download_dk_fantasy_projections', 'Download Projections')
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
              downloadButton('download_fd_fantasy_projections', 'Download Projections')
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
              plotlyOutput("fd_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#ff6600")
            )
          )
        )
      )
    } else if (rv$has_draftkings) {
      # Only DraftKings available
      tagList(
        fluidRow(
          box(
            width = 12,
            title = "DraftKings Fantasy Point Projections",
            DTOutput("dk_fantasy_projections") %>% withSpinner(color = "#ff6600"),
            downloadButton('download_dk_fantasy_projections', 'Download Projections')
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "DraftKings Fantasy Points vs Salary",
            plotlyOutput("dk_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#ff6600")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "DraftKings Fantasy Points Distribution",
            plotlyOutput("dk_fantasy_points_dist", height = "700px") %>% withSpinner(color = "#ff6600")
          )
        )
      )
    } else if (rv$has_fanduel) {
      # Only FanDuel available
      tagList(
        fluidRow(
          box(
            width = 12,
            title = "FanDuel Fantasy Point Projections",
            DTOutput("fd_fantasy_projections") %>% withSpinner(color = "#ff6600"),
            downloadButton('download_fd_fantasy_projections', 'Download Projections')
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "FanDuel Fantasy Points vs Salary",
            plotlyOutput("fd_fantasy_points_salary", height = "800px") %>% withSpinner(color = "#ff6600")
          )
        ),
        fluidRow(
          box(
            width = 12,
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
  
  output$dk_fantasy_projections <- renderDT({
    req(rv$dk_fantasy_analysis)
    
    dt <- datatable(
      rv$dk_fantasy_analysis,
      options = list(
        scrollX = TRUE,
        pageLength = 25,  # Reduced from -1
        dom = "ftp",      # Added filtering and pagination
        order = list(list(3, 'desc')), # Sort by median
        processing = TRUE,
        columnDefs = list(list(
          targets = "_all", className = 'dt-center'
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency('DKSalary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(c('Median_Fantasy_Pts', 'FP_90thPct', 'PPD'), digits = 1)
    
    dt
  })
  
  output$fd_fantasy_projections <- renderDT({
    req(rv$fd_fantasy_analysis)
    
    dt <- datatable(
      rv$fd_fantasy_analysis,
      options = list(
        scrollX = TRUE,
        pageLength = 25,  # Reduced from -1
        dom = "ftp",      # Added filtering and pagination
        order = list(list(3, 'desc')), # Sort by median
        processing = TRUE,
        columnDefs = list(list(
          targets = "_all", className = 'dt-center'
        ))
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatCurrency('FDSalary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(c('Median_Fantasy_Pts', 'FP_90thPct', 'PPD'), digits = 1)
    
    dt
  })
  
  # DraftKings Fantasy Points vs Salary
  output$dk_fantasy_points_salary <- renderPlotly({
    req(rv$dk_fantasy_analysis)
    
    plot_data <- rv$dk_fantasy_analysis
    
    p <- ggplot(plot_data,
                aes(
                  x = DKSalary,
                  y = Median_Fantasy_Pts,
                  size = DKOP,
                  text = Name
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
    
    ggplotly(p, height = 800, tooltip = c("text", "x", "y", "size")) %>%
      layout(hoverlabel = list(bgcolor = "white"), hovermode = "closest")
  })
  
  # FanDuel Fantasy Points vs Salary
  output$fd_fantasy_points_salary <- renderPlotly({
    req(rv$fd_fantasy_analysis)
    
    plot_data <- rv$fd_fantasy_analysis
    
    p <- ggplot(plot_data,
                aes(
                  x = FDSalary,
                  y = Median_Fantasy_Pts,
                  size = FDOP,
                  text = Name
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
    
    ggplotly(p, height = 800, tooltip = c("text", "x", "y", "size")) %>%
      layout(hoverlabel = list(bgcolor = "white"), hovermode = "closest")
  })
  
  output$dk_fantasy_points_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Get average finish position for all players and select top 50
    avg_finish <- rv$simulation_results %>%
      group_by(Name) %>%
      summarise(avg_pos = mean(FinishPosition), .groups = 'drop') %>%
      arrange(avg_pos) %>%
      slice_head(n = 50)
    
    # Get top 50 player names
    top_50_names <- avg_finish$Name
    
    # Filter data to top 50 players and order by salary
    plot_data <- rv$simulation_results %>%
      filter(Name %in% top_50_names) %>%
      arrange(DKSalary)
    
    # Create ordered factor
    ordered_names <- unique(plot_data$Name)
    
    p <- ggplot(plot_data, aes(
      x = factor(Name, levels = ordered_names),
      y = DKFantasyPoints,
      fill = Name
    )) +
      geom_boxplot(outlier.alpha = 0.25) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Player (Top 50 by Avg Finish)", y = "Fantasy Points") +
      theme(legend.position = "none")
    
    ggplotly(p, height = 700, tooltip = c("x", "y"))
  })
  
  output$fd_fantasy_points_dist <- renderPlotly({
    req(rv$simulation_results)
    
    # Get average finish position for all players and select top 50
    avg_finish <- rv$simulation_results %>%
      group_by(Name) %>%
      summarise(avg_pos = mean(FinishPosition), .groups = 'drop') %>%
      arrange(avg_pos) %>%
      slice_head(n = 50)
    
    # Get top 50 player names
    top_50_names <- avg_finish$Name
    
    # Filter data to top 50 players and order by salary
    plot_data <- rv$simulation_results %>%
      filter(Name %in% top_50_names) %>%
      arrange(FDSalary)
    
    # Create ordered factor
    ordered_names <- unique(plot_data$Name)
    
    p <- ggplot(plot_data, aes(
      x = factor(Name, levels = ordered_names),
      y = FDFantasyPoints,
      fill = Name
    )) +
      geom_boxplot(outlier.alpha = 0.25) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Player (Top 50 by Avg Finish)", y = "Fantasy Points") +
      theme(legend.position = "none")
    
    ggplotly(p, height = 700, tooltip = c("x", "y"))
  })
  
  # Download handlers
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("golf_simulation_results_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(rv$simulation_results, file, row.names = FALSE)
    }
  )
  
  output$download_dk_fantasy_projections <- downloadHandler(
    filename = function() {
      paste("dk_golf_fantasy_projections_",
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
      paste("fd_golf_fantasy_projections_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(rv$fd_fantasy_analysis, file, row.names = FALSE)
    }
  )
  
  output$has_draftkings <- reactive({
    result <- tolower(as.character(rv$has_draftkings))
    return(result)
  })
  outputOptions(output, "has_draftkings", suspendWhenHidden = FALSE)
  
  output$has_fanduel <- reactive({
    result <- tolower(as.character(rv$has_fanduel))
    return(result)
  })
  outputOptions(output, "has_fanduel", suspendWhenHidden = FALSE)
  
  output$has_dk_lineups <- reactive({
    result <- tolower(as.character(!is.null(rv$dk_optimal_lineups) && nrow(rv$dk_optimal_lineups) > 0))
    return(result)
  })
  outputOptions(output, "has_dk_lineups", suspendWhenHidden = FALSE)
  
  output$has_fd_lineups <- reactive({
    result <- tolower(as.character(!is.null(rv$fd_optimal_lineups) && nrow(rv$fd_optimal_lineups) > 0))
    return(result)
  })
  outputOptions(output, "has_fd_lineups", suspendWhenHidden = FALSE)
  
  
  output$dk_random_lineups_table <- renderDT({
    req(rv$dk_random_lineups)
    
    display_data <- as.data.frame(rv$dk_random_lineups)
    
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        rownames = FALSE,
        dom = "ftp",      # Added filtering and pagination
        ordering = TRUE,
        processing = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Format columns (same as before)
    if("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    count_cols <- c("Top1Count", "Top10Count", "Top50Count", "Top100Count")
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
  
  output$fd_random_lineups_table <- renderDT({
    req(rv$fd_random_lineups)
    
    display_data <- as.data.frame(rv$fd_random_lineups)
    
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        rownames = FALSE,
        dom = "ftp",      # Added filtering and pagination
        ordering = TRUE,
        processing = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'cell-border stripe compact',
      rownames = FALSE
    )
    
    # Format columns (same as before)
    if("TotalSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    count_cols <- c("Top1Count", "Top10Count", "Top50Count", "Top100Count")
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
  
  
  
  output$dk_player_exposure_table <- renderDT({
    req(rv$dk_player_exposure)
    
    # Check if it's an error message
    if("Message" %in% names(rv$dk_player_exposure)) {
      return(datatable(rv$dk_player_exposure, options = list(dom = "t"), rownames = FALSE))
    }
    
    display_data <- rv$dk_player_exposure
    has_random_lineups <- !is.null(rv$dk_random_lineups) && nrow(rv$dk_random_lineups) > 0
    
    # Remove columns if no random lineups
    if(!has_random_lineups) {
      display_data$Exposure <- NULL
      display_data$Leverage <- NULL
    }
    
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,  # Added pagination
        dom = "ftp",      # Added filtering and pagination
        scrollX = TRUE,
        order = list(list(3, 'desc')),  # Sort by OptimalRate
        processing = TRUE,
        rownames = FALSE
      ),
      rownames = FALSE
    )
    
    # Format columns (same as before)
    if("DKSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('DKSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    if("DKOP" %in% names(display_data)) {
      dt <- dt %>% formatRound('DKOP', digits = 1)
    }
    
    # Format numeric columns
    numeric_cols <- intersect(c('OptimalRate', 'FilteredPoolRate', 'Exposure', 'Leverage'), names(display_data))
    if(length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(numeric_cols, digits = 1)
    }
    
    return(dt)
  })
  
  output$fd_player_exposure_table <- renderDT({
    req(rv$fd_player_exposure)
    
    # Check if it's an error message
    if("Message" %in% names(rv$fd_player_exposure)) {
      return(datatable(rv$fd_player_exposure, options = list(dom = "t"), rownames = FALSE))
    }
    
    display_data <- rv$fd_player_exposure
    has_random_lineups <- !is.null(rv$fd_random_lineups) && nrow(rv$fd_random_lineups) > 0
    
    # Remove columns if no random lineups
    if(!has_random_lineups) {
      display_data$Exposure <- NULL
      display_data$Leverage <- NULL
    }
    
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 25,  # Added pagination
        dom = "ftp",      # Added filtering and pagination
        scrollX = TRUE,
        order = list(list(3, 'desc')),  # Sort by OptimalRate
        processing = TRUE,
        rownames = FALSE
      ),
      rownames = FALSE
    )
    
    # Format columns (same as before)
    if("FDSalary" %in% names(display_data)) {
      dt <- dt %>% formatCurrency('FDSalary', currency = "$", interval = 3, mark = ",", digits = 0)
    }
    
    if("FDOP" %in% names(display_data)) {
      dt <- dt %>% formatRound('FDOP', digits = 1)
    }
    
    # Format numeric columns
    numeric_cols <- intersect(c('OptimalRate', 'FilteredPoolRate', 'Exposure', 'Leverage'), names(display_data))
    if(length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(numeric_cols, digits = 1)
    }
    
    return(dt)
  })
  
  output$download_dk_optimal_lineups <- downloadHandler(
    filename = function() {
      paste("dk_golf_optimal_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      if(is.null(rv$dk_optimal_lineups) || nrow(rv$dk_optimal_lineups) == 0) {
        empty_data <- data.frame(
          Player1 = character(0), Player2 = character(0), Player3 = character(0),
          Player4 = character(0), Player5 = character(0), Player6 = character(0),
          Top1Count = integer(0), Top2Count = integer(0), Top3Count = integer(0), 
          Top5Count = integer(0), TotalSalary = integer(0),
          CumulativeOwnership = numeric(0), GeometricMeanOwnership = numeric(0)
        )
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      # Create download data with DKName substitution
      download_data <- as.data.frame(rv$dk_optimal_lineups)
      keep_cols <- c(paste0("Player", 1:DK_ROSTER_SIZE), 
                     "Top1Count", "Top10Count", "Top50Count", "Top100Count", 
                     "TotalSalary", "CumulativeOwnership", "GeometricMeanOwnership")
      keep_cols <- intersect(keep_cols, names(download_data))
      download_data <- download_data[, keep_cols, drop = FALSE]
      
      # Replace player names with DKName if available
      if(!is.null(rv$simulation_results) && "DKName" %in% names(rv$simulation_results)) {
        # Create name mapping from simulation results
        name_mapping <- unique(rv$simulation_results[, c("Name", "DKName")])
        name_mapping <- name_mapping[!is.na(name_mapping$Name) & !is.na(name_mapping$DKName), ]
        
        if(nrow(name_mapping) > 0) {
          # Create lookup table
          lookup <- setNames(name_mapping$DKName, name_mapping$Name)
          
          # Replace names in each player column
          for(col in paste0("Player", 1:DK_ROSTER_SIZE)) {
            if(col %in% names(download_data)) {
              download_data[[col]] <- ifelse(
                download_data[[col]] %in% names(lookup),
                lookup[download_data[[col]]],
                download_data[[col]]
              )
            }
          }
        }
      }
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # 2. FanDuel optimal lineups download handler
  output$download_fd_optimal_lineups <- downloadHandler(
    filename = function() {
      paste("fd_golf_optimal_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      if(is.null(rv$fd_optimal_lineups) || nrow(rv$fd_optimal_lineups) == 0) {
        empty_data <- data.frame(
          Player1 = character(0), Player2 = character(0), Player3 = character(0),
          Player4 = character(0), Player5 = character(0), Player6 = character(0),
          Top1Count = integer(0), Top2Count = integer(0), Top3Count = integer(0), 
          Top5Count = integer(0), TotalSalary = integer(0),
          CumulativeOwnership = numeric(0), GeometricMeanOwnership = numeric(0)
        )
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      # Create download data with FDName substitution
      download_data <- as.data.frame(rv$fd_optimal_lineups)
      keep_cols <- c(paste0("Player", 1:FD_ROSTER_SIZE), 
                     "Top1Count", "Top10Count", "Top50Count", "Top100Count", 
                     "TotalSalary", "CumulativeOwnership", "GeometricMeanOwnership")
      keep_cols <- intersect(keep_cols, names(download_data))
      download_data <- download_data[, keep_cols, drop = FALSE]
      
      # Replace player names with FDName if available
      if(!is.null(rv$simulation_results) && "FDName" %in% names(rv$simulation_results)) {
        # Create name mapping from simulation results
        name_mapping <- unique(rv$simulation_results[, c("Name", "FDName")])
        name_mapping <- name_mapping[!is.na(name_mapping$Name) & !is.na(name_mapping$FDName), ]
        
        if(nrow(name_mapping) > 0) {
          # Create lookup table
          lookup <- setNames(name_mapping$FDName, name_mapping$Name)
          
          # Replace names in each player column
          for(col in paste0("Player", 1:FD_ROSTER_SIZE)) {
            if(col %in% names(download_data)) {
              download_data[[col]] <- ifelse(
                download_data[[col]] %in% names(lookup),
                lookup[download_data[[col]]],
                download_data[[col]]
              )
            }
          }
        }
      }
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # 3. DraftKings random lineups download handler
  output$download_dk_random_lineups <- downloadHandler(
    filename = function() {
      paste("dk_golf_random_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      if(is.null(rv$dk_random_lineups) || nrow(rv$dk_random_lineups) == 0) {
        empty_data <- data.frame(
          Player1 = character(0), Player2 = character(0), Player3 = character(0),
          Player4 = character(0), Player5 = character(0), Player6 = character(0)
        )
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      # Create download data with DKName substitution
      download_data <- as.data.frame(rv$dk_random_lineups)
      
      # Replace player names with DKName if available
      if(!is.null(rv$simulation_results) && "DKName" %in% names(rv$simulation_results)) {
        # Create name mapping from simulation results
        name_mapping <- unique(rv$simulation_results[, c("Name", "DKName")])
        name_mapping <- name_mapping[!is.na(name_mapping$Name) & !is.na(name_mapping$DKName), ]
        
        if(nrow(name_mapping) > 0) {
          # Create lookup table
          lookup <- setNames(name_mapping$DKName, name_mapping$Name)
          
          # Replace names in each player column
          for(col in paste0("Player", 1:DK_ROSTER_SIZE)) {
            if(col %in% names(download_data)) {
              download_data[[col]] <- ifelse(
                download_data[[col]] %in% names(lookup),
                lookup[download_data[[col]]],
                download_data[[col]]
              )
            }
          }
        }
      }
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # 4. FanDuel random lineups download handler
  output$download_fd_random_lineups <- downloadHandler(
    filename = function() {
      paste("fd_golf_random_lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      if(is.null(rv$fd_random_lineups) || nrow(rv$fd_random_lineups) == 0) {
        empty_data <- data.frame(
          Player1 = character(0), Player2 = character(0), Player3 = character(0),
          Player4 = character(0), Player5 = character(0), Player6 = character(0)
        )
        write.csv(empty_data, file, row.names = FALSE)
        return()
      }
      
      # Create download data with FDName substitution
      download_data <- as.data.frame(rv$fd_random_lineups)
      
      # Replace player names with FDName if available
      if(!is.null(rv$simulation_results) && "FDName" %in% names(rv$simulation_results)) {
        # Create name mapping from simulation results
        name_mapping <- unique(rv$simulation_results[, c("Name", "FDName")])
        name_mapping <- name_mapping[!is.na(name_mapping$Name) & !is.na(name_mapping$FDName), ]
        
        if(nrow(name_mapping) > 0) {
          # Create lookup table
          lookup <- setNames(name_mapping$FDName, name_mapping$Name)
          
          # Replace names in each player column
          for(col in paste0("Player", 1:FD_ROSTER_SIZE)) {
            if(col %in% names(download_data)) {
              download_data[[col]] <- ifelse(
                download_data[[col]] %in% names(lookup),
                lookup[download_data[[col]]],
                download_data[[col]]
              )
            }
          }
        }
      }
      
      write.csv(download_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  
  
  observeEvent(input$run_dk_optimization, {
    req(rv$simulation_results, rv$has_draftkings)
    
    # Clear previous analysis results but keep simulation data
    rv$dk_optimal_lineups <- NULL
    rv$dk_player_exposure <- NULL
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
        count_dk_golf_optimal_lineups(rv$simulation_results,
                                      player_data = rv$processed_data$players)
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
      
      # Create player mapping and calculate exposure
      setProgress(0.7, detail = "Creating player mapping...")
      if(!is.null(rv$dk_optimal_lineups)) {
        # Use the fantasy analysis data if available, otherwise create from simulation results
        if(!is.null(rv$dk_fantasy_analysis)) {
          player_mapping <- rv$dk_fantasy_analysis[, c("Name", "DKSalary", "DKOP")]
        } else {
          # Get unique players from simulation results
          unique_sim_players <- unique(rv$simulation_results[, c("Name", "DKSalary", "DKOP")])
          player_mapping <- unique_sim_players[!is.na(unique_sim_players$Name), ]
        }
        
        # UPDATE SLIDERS WITH ACTUAL DATA
        if("CumulativeOwnership" %in% names(rv$dk_optimal_lineups)) {
          cum_values <- rv$dk_optimal_lineups$CumulativeOwnership[!is.na(rv$dk_optimal_lineups$CumulativeOwnership)]
          if(length(cum_values) > 0) {
            cum_min <- floor(min(cum_values))
            cum_max <- ceiling(max(cum_values))
            
            updateSliderInput(session, "dk_cumulative_ownership_range",
                              min = cum_min, max = cum_max, value = c(cum_min, cum_max))
          }
        }
        
        if("GeometricMeanOwnership" %in% names(rv$dk_optimal_lineups)) {
          geom_values <- rv$dk_optimal_lineups$GeometricMeanOwnership[!is.na(rv$dk_optimal_lineups$GeometricMeanOwnership)]
          if(length(geom_values) > 0) {
            geom_min <- floor(min(geom_values))
            geom_max <- ceiling(max(geom_values))
            
            updateSliderInput(session, "dk_geometric_mean_range",
                              min = geom_min, max = geom_max, value = c(geom_min, geom_max))
          }
        }
        
        # Calculate initial player exposure
        setProgress(0.8, detail = "Calculating player exposure...")
        rv$dk_player_exposure <- calculate_dk_golf_player_exposure(
          rv$dk_optimal_lineups, 
          player_mapping
        )
        
        if(!is.null(rv$dk_player_exposure) && !"Message" %in% names(rv$dk_player_exposure)) {
          rv$dk_player_exposure$FilteredPoolRate <- rv$dk_player_exposure$OptimalRate
          
          # Update player choices for exclusion dropdown
          player_data <- rv$dk_player_exposure
          player_names <- player_data$Name
          player_labels <- paste0(player_names, " (", round(player_data$OptimalRate, 1), "%)")
          player_choices <- setNames(player_names, player_labels)
          
          updateSelectizeInput(session, "dk_excluded_players", choices = player_choices)
        }
      }
      
      removeModal()
      
      showModal(modalDialog(
        title = "Success",
        HTML(sprintf("Successfully generated <b>%d</b> optimal lineups for DraftKings!", 
                     if(!is.null(rv$dk_optimal_lineups)) nrow(rv$dk_optimal_lineups) else 0)),
        easyClose = TRUE
      ))
    })
  })
  
  # 2. FanDuel optimization button handler
  observeEvent(input$run_fd_optimization, {
    req(rv$simulation_results, rv$has_fanduel)
    
    rv$fd_optimal_lineups <- NULL
    rv$fd_player_exposure <- NULL
    rv$fd_random_lineups <- NULL
    
    gc(verbose = FALSE, full = TRUE)
    
    withProgress(message = 'Calculating FanDuel optimal lineups...', value = 0, {
      showModal(modalDialog(
        title = "Processing FanDuel Optimal Lineups",
        "Finding optimal lineups using all simulations. This may take a few minutes.",
        footer = NULL, easyClose = FALSE
      ))
      
      setProgress(0.2, detail = "Finding optimal lineups...")
      rv$fd_optimal_lineups <- tryCatch({
        count_fd_golf_optimal_lineups(rv$simulation_results,
                                      player_data = rv$processed_data$players)
      }, error = function(e) {
        removeModal()
        showModal(modalDialog(title = "Error", paste("Error:", e$message), easyClose = TRUE))
        NULL
      })
      
      setProgress(0.7, detail = "Creating player mapping...")
      if(!is.null(rv$fd_optimal_lineups)) {
        # Use the fantasy analysis data if available, otherwise create from simulation results
        if(!is.null(rv$fd_fantasy_analysis)) {
          player_mapping <- rv$fd_fantasy_analysis[, c("Name", "FDSalary", "FDOP")]
        } else {
          # Get unique players from simulation results
          unique_sim_players <- unique(rv$simulation_results[, c("Name", "FDSalary", "FDOP")])
          player_mapping <- unique_sim_players[!is.na(unique_sim_players$Name), ]
        }
        
        setProgress(0.8, detail = "Calculating player exposure...")
        rv$fd_player_exposure <- calculate_fd_golf_player_exposure(rv$fd_optimal_lineups, player_mapping)
        
        if(!is.null(rv$fd_player_exposure) && !"Message" %in% names(rv$fd_player_exposure)) {
          rv$fd_player_exposure$FilteredPoolRate <- rv$fd_player_exposure$OptimalRate
          
          player_data <- rv$fd_player_exposure
          player_names <- player_data$Name
          player_labels <- paste0(player_names, " (", round(player_data$OptimalRate, 1), "%)")
          player_choices <- setNames(player_names, player_labels)
          
          updateSelectizeInput(session, "fd_excluded_players", choices = player_choices)
        }
      }
      
      removeModal()
      showModal(modalDialog(
        title = "Success",
        HTML(sprintf("Successfully generated <b>%d</b> optimal lineups for FanDuel!", 
                     if(!is.null(rv$fd_optimal_lineups)) nrow(rv$fd_optimal_lineups) else 0)),
        easyClose = TRUE
      ))
    })
  })
  
  # Tiers optimization button handler
  # Tiers optimization button handler
  observeEvent(input$run_tiers_optimization, {
    req(rv$simulation_results, rv$has_tiers, rv$tiers_data)
    
    rv$tiers_optimal_lineups <- NULL
    
    gc(verbose = FALSE, full = TRUE)
    
    withProgress(message = 'Calculating DK Tiers optimal lineups...', value = 0, {
      showModal(modalDialog(
        title = "Processing DK Tiers Optimal Lineups",
        "Evaluating tier combinations across all simulations. This may take several minutes.",
        footer = NULL, easyClose = FALSE
      ))
      
      setProgress(0.2, detail = "Generating and evaluating lineups...")
      rv$tiers_optimal_lineups <- tryCatch({
        count_tier_optimal_lineups(rv$simulation_results, rv$tiers_data)
      }, error = function(e) {
        removeModal()
        showModal(modalDialog(
          title = "Error", 
          paste("Error:", e$message),
          easyClose = TRUE
        ))
        NULL
      })
      
      removeModal()
      
      if(!is.null(rv$tiers_optimal_lineups) && nrow(rv$tiers_optimal_lineups) > 0) {
        showModal(modalDialog(
          title = "Success",
          HTML(sprintf("Successfully generated <b>%s</b> optimal tier lineups!", 
                       format(nrow(rv$tiers_optimal_lineups), big.mark=","))),
          easyClose = TRUE
        ))
      } else {
        showModal(modalDialog(
          title = "No Results",
          "No tier lineups were generated. This may be due to missing player names between the Tiers sheet and Player sheet.",
          easyClose = TRUE
        ))
      }
    })
  })
  
  
  # 3. Generate random DraftKings lineups
  observeEvent(input$generate_dk_lineups, {
    req(rv$dk_optimal_lineups)
    
    filters <- list(
      min_top1_count = if(!is.null(input$dk_min_top1_count)) input$dk_min_top1_count else 0,
      min_top10_count = if(!is.null(input$dk_min_top10_count)) input$dk_min_top10_count else 0,
      min_top50_count = if(!is.null(input$dk_min_top50_count)) input$dk_min_top50_count else 0,
      min_top100_count = if(!is.null(input$dk_min_top100_count)) input$dk_min_top100_count else 0,
      cumulative_ownership_range = if(!is.null(input$dk_cumulative_ownership_range)) input$dk_cumulative_ownership_range else c(0, 1000),
      geometric_mean_range = if(!is.null(input$dk_geometric_mean_range)) input$dk_geometric_mean_range else c(0, 100),
      min_atleast6_prob = if(!is.null(input$dk_min_atleast6_prob)) input$dk_min_atleast6_prob else 0,
      min_atleast5_prob = if(!is.null(input$dk_min_atleast5_prob)) input$dk_min_atleast5_prob else 0,
      min_expected_cuts = if(!is.null(input$dk_min_expected_cuts)) input$dk_min_expected_cuts else 0,
      min_salary = if(!is.null(input$dk_min_salary)) input$dk_min_salary else 0,
      excluded_players = if(!is.null(input$dk_excluded_players)) input$dk_excluded_players else character(0),
      num_lineups = if(!is.null(input$dk_num_random_lineups)) input$dk_num_random_lineups else 20
    )
    
    withProgress(message = 'Generating lineups...', value = 0, {
      rv$dk_random_lineups <- generate_random_dk_golf_lineups(rv$dk_optimal_lineups, filters)
      
      if(!is.null(rv$dk_random_lineups)) {
        # Update player exposure with random lineups
        if(!is.null(rv$dk_player_exposure) && !"Message" %in% names(rv$dk_player_exposure)) {
          # Use existing mapping data
          existing_mapping <- rv$dk_player_exposure[, c("Name", "DKSalary", "DKOP")]
          rv$dk_player_exposure <- calculate_dk_golf_player_exposure(
            rv$dk_optimal_lineups, existing_mapping, rv$dk_random_lineups)
          
          # Update filtered pool rates
          filtered_rates <- calculate_golf_filtered_pool_rates(rv$dk_optimal_lineups, filters, "dk")
          if(!is.null(filtered_rates) && nrow(filtered_rates) > 0) {
            for(i in 1:nrow(rv$dk_player_exposure)) {
              player_name <- rv$dk_player_exposure$Name[i]
              rate_match <- filtered_rates[filtered_rates$Name == player_name, ]
              if(nrow(rate_match) > 0) {
                rv$dk_player_exposure$FilteredPoolRate[i] <- rate_match$FilteredPoolRate[1]
              } else {
                rv$dk_player_exposure$FilteredPoolRate[i] <- 0
              }
            }
          }
        }
        
        showModal(modalDialog(
          title = "Success",
          sprintf("Generated %d DraftKings lineups successfully!", nrow(rv$dk_random_lineups)),
          easyClose = TRUE
        ))
      } else {
        showModal(modalDialog(
          title = "Error",
          "No lineups match the selected filters. Try adjusting your criteria.",
          easyClose = TRUE
        ))
      }
    })
  })
  
  # 4. Generate random FanDuel lineups
  observeEvent(input$generate_fd_lineups, {
    req(rv$fd_optimal_lineups)
    
    filters <- list(
      min_top1_count = if(!is.null(input$fd_min_top1_count)) input$fd_min_top1_count else 0,
      min_top10_count = if(!is.null(input$fd_min_top10_count)) input$fd_min_top10_count else 0,
      min_top50_count = if(!is.null(input$fd_min_top50_count)) input$fd_min_top50_count else 0,
      min_top100_count = if(!is.null(input$fd_min_top100_count)) input$fd_min_top100_count else 0,
      excluded_players = if(!is.null(input$fd_excluded_players)) input$fd_excluded_players else character(0),
      num_lineups = if(!is.null(input$fd_num_random_lineups)) input$fd_num_random_lineups else 20
    )
    
    withProgress(message = 'Generating lineups...', value = 0, {
      rv$fd_random_lineups <- generate_random_fd_golf_lineups(rv$fd_optimal_lineups, filters)
      
      if(!is.null(rv$fd_random_lineups)) {
        # Update player exposure with random lineups
        if(!is.null(rv$fd_player_exposure) && !"Message" %in% names(rv$fd_player_exposure)) {
          # Use existing mapping data
          existing_mapping <- rv$fd_player_exposure[, c("Name", "FDSalary", "FDOP")]
          rv$fd_player_exposure <- calculate_fd_golf_player_exposure(
            rv$fd_optimal_lineups, existing_mapping, rv$fd_random_lineups)
          
          # Update filtered pool rates
          filtered_rates <- calculate_golf_filtered_pool_rates(rv$fd_optimal_lineups, filters, "fd")
          if(nrow(filtered_rates) > 0) {
            for(i in 1:nrow(rv$fd_player_exposure)) {
              player_name <- rv$fd_player_exposure$Name[i]
              rate_match <- filtered_rates[filtered_rates$Name == player_name, ]
              if(nrow(rate_match) > 0) {
                rv$fd_player_exposure$FilteredPoolRate[i] <- rate_match$FilteredPoolRate[1]
              } else {
                rv$fd_player_exposure$FilteredPoolRate[i] <- 0
              }
            }
          }
        }
        
        showModal(modalDialog(
          title = "Success",
          sprintf("Generated %d FanDuel lineups successfully!", nrow(rv$fd_random_lineups)),
          easyClose = TRUE
        ))
      } else {
        showModal(modalDialog(
          title = "Error",
          "No lineups match the selected filters. Try adjusting your criteria.",
          easyClose = TRUE
        ))
      }
    })
  })
  
  # 5. DK filter change observer for real-time updates
  observeEvent(c(input$dk_min_top1_count, input$dk_min_top10_count, input$dk_min_top50_count, 
                 input$dk_min_top100_count, input$dk_cumulative_ownership_range, 
                 input$dk_geometric_mean_range, input$dk_excluded_players), {
                   if(!is.null(rv$dk_optimal_lineups) && !is.null(rv$dk_player_exposure) && !"Message" %in% names(rv$dk_player_exposure)) {
                     
                     filters <- list(
                       min_top1_count = if(!is.null(input$dk_min_top1_count)) input$dk_min_top1_count else 0,
                       min_top10_count = if(!is.null(input$dk_min_top10_count)) input$dk_min_top10_count else 0,
                       min_top50_count = if(!is.null(input$dk_min_top50_count)) input$dk_min_top50_count else 0,
                       min_top100_count = if(!is.null(input$dk_min_top100_count)) input$dk_min_top100_count else 0,
                       cumulative_ownership_range = if(!is.null(input$dk_cumulative_ownership_range)) {
                         input$dk_cumulative_ownership_range
                       } else {
                         if("CumulativeOwnership" %in% names(rv$dk_optimal_lineups)) {
                           cum_vals <- rv$dk_optimal_lineups$CumulativeOwnership[!is.na(rv$dk_optimal_lineups$CumulativeOwnership)]
                           if(length(cum_vals) > 0) c(min(cum_vals), max(cum_vals)) else c(0, 100)
                         } else c(0, 100)
                       },
                       geometric_mean_range = if(!is.null(input$dk_geometric_mean_range)) {
                         input$dk_geometric_mean_range
                       } else {
                         if("GeometricMeanOwnership" %in% names(rv$dk_optimal_lineups)) {
                           geom_vals <- rv$dk_optimal_lineups$GeometricMeanOwnership[!is.na(rv$dk_optimal_lineups$GeometricMeanOwnership)]
                           if(length(geom_vals) > 0) c(min(geom_vals), max(geom_vals)) else c(0, 100)
                         } else c(0, 100)
                       },
                       excluded_players = if(!is.null(input$dk_excluded_players)) input$dk_excluded_players else character(0),
                       min_atleast6_prob = if(!is.null(input$dk_min_atleast6_prob)) input$dk_min_atleast6_prob else 0,
                       min_atleast5_prob = if(!is.null(input$dk_min_atleast5_prob)) input$dk_min_atleast5_prob else 0,
                       min_expected_cuts = if(!is.null(input$dk_min_expected_cuts)) input$dk_min_expected_cuts else 0,
                       min_salary = if(!is.null(input$dk_min_salary)) input$dk_min_salary else 0
                     )
                     
                     # Calculate filtered pool rates
                     filtered_rates <- calculate_golf_filtered_pool_rates(rv$dk_optimal_lineups, filters, "dk")
                     
                     if(!is.null(filtered_rates) && nrow(filtered_rates) > 0) {
                       # Update the FilteredPoolRate column
                       for(i in 1:nrow(rv$dk_player_exposure)) {
                         player_name <- rv$dk_player_exposure$Name[i]
                         rate_match <- filtered_rates[filtered_rates$Name == player_name, ]
                         if(nrow(rate_match) > 0) {
                           rv$dk_player_exposure$FilteredPoolRate[i] <- rate_match$FilteredPoolRate[1]
                         } else {
                           rv$dk_player_exposure$FilteredPoolRate[i] <- 0
                         }
                       }
                     }
                   }
                 }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # 6. FD filter change observer for real-time updates
  observeEvent(c(input$fd_min_top1_count, input$fd_min_top10_count, input$fd_min_top50_count, 
                 input$fd_min_top100_count, input$fd_excluded_players), {
                   if(!is.null(rv$fd_optimal_lineups) && !is.null(rv$fd_player_exposure) && !"Message" %in% names(rv$fd_player_exposure)) {
                     filters <- list(
                       min_top1_count = if(!is.null(input$fd_min_top1_count)) input$fd_min_top1_count else 0,
                       min_top10_count = if(!is.null(input$fd_min_top10_count)) input$fd_min_top10_count else 0,
                       min_top50_count = if(!is.null(input$fd_min_top50_count)) input$fd_min_top50_count else 0,
                       min_top100_count = if(!is.null(input$fd_min_top100_count)) input$fd_min_top100_count else 0,
                       excluded_players = if(!is.null(input$fd_excluded_players)) input$fd_excluded_players else character(0)
                     )
                     
                     filtered_rates <- calculate_golf_filtered_pool_rates(rv$fd_optimal_lineups, filters, "fd")
                     
                     if(nrow(filtered_rates) > 0) {
                       for(i in 1:nrow(rv$fd_player_exposure)) {
                         player_name <- rv$fd_player_exposure$Name[i]
                         rate_match <- filtered_rates[filtered_rates$Name == player_name, ]
                         if(nrow(rate_match) > 0) {
                           rv$fd_player_exposure$FilteredPoolRate[i] <- rate_match$FilteredPoolRate[1]
                         } else {
                           rv$fd_player_exposure$FilteredPoolRate[i] <- 0
                         }
                       }
                     }
                   }
                 }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # Memory cleanup functions
  observe({
    invalidateLater(180000) # 3 minutes
    gc(verbose = FALSE, full = TRUE)
  })
  
  # Clean up on session end
  session$onSessionEnded(function() {
    gc(verbose = FALSE, full = TRUE)
  })
  
  # Lineup builder UI generator
  output$lineup_builder_ui <- renderUI({
    # Check if any optimal lineups exist
    has_dk_lineups <- !is.null(rv$dk_optimal_lineups) && nrow(rv$dk_optimal_lineups) > 0
    has_fd_lineups <- !is.null(rv$fd_optimal_lineups) && nrow(rv$fd_optimal_lineups) > 0
    
    # If neither platform has lineups, show a message
    if(!has_dk_lineups && !has_fd_lineups) {
      return(
        box(
          width = 12,
          status = "warning",
          title = "No Optimal Lineups Available",
          "Please calculate optimal lineups in the Optimal Lineups tab first."
        )
      )
    }
    
    # Calculate actual ranges for DK sliders if we have data
    dk_cum_range <- c(100, 300)  # Default range for cumulative ownership
    dk_geom_range <- c(15, 35)   # Default range for geometric mean
    
    if(has_dk_lineups && "CumulativeOwnership" %in% names(rv$dk_optimal_lineups)) {
      cum_vals <- rv$dk_optimal_lineups$CumulativeOwnership[!is.na(rv$dk_optimal_lineups$CumulativeOwnership)]
      if(length(cum_vals) > 0) {
        dk_cum_range <- c(floor(min(cum_vals)), ceiling(max(cum_vals)))
      }
    }
    
    if(has_dk_lineups && "GeometricMeanOwnership" %in% names(rv$dk_optimal_lineups)) {
      geom_vals <- rv$dk_optimal_lineups$GeometricMeanOwnership[!is.na(rv$dk_optimal_lineups$GeometricMeanOwnership)]
      if(length(geom_vals) > 0) {
        dk_geom_range <- c(floor(min(geom_vals)), ceiling(max(geom_vals)))
      }
    }
    
    # Generate UI for both platforms
    tagList(
      # Platform selection
      fluidRow(
        box(
          width = 12,
          title = "Select Fantasy Platform",
          status = "primary",
          solidHeader = TRUE,
          conditionalPanel(
            condition = "output.has_dk_lineups == 'true' && output.has_fd_lineups == 'true'",
            radioButtons(
              "lineup_builder_platform", 
              "Platform:",
              choices = list(
                "DraftKings" = "dk",
                "FanDuel" = "fd"
              ),
              selected = if(has_dk_lineups) "dk" else "fd",
              inline = TRUE
            )
          ),
          conditionalPanel(
            condition = "output.has_dk_lineups == 'true' && output.has_fd_lineups != 'true'",
            radioButtons(
              "lineup_builder_platform", 
              "Platform:",
              choices = list("DraftKings" = "dk"),
              selected = "dk",
              inline = TRUE
            ),
            h4("Using DraftKings lineups")
          ),
          conditionalPanel(
            condition = "output.has_dk_lineups != 'true' && output.has_fd_lineups == 'true'",
            radioButtons(
              "lineup_builder_platform", 
              "Platform:",
              choices = list("FanDuel" = "fd"),
              selected = "fd",
              inline = TRUE
            ),
            h4("Using FanDuel lineups")
          )
        )
      ),
      
      # DraftKings UI
      conditionalPanel(
        condition = "(input.lineup_builder_platform == 'dk' || (input.lineup_builder_platform == null && output.has_dk_lineups == 'true')) && output.has_dk_lineups == 'true'",
        fluidRow(
          box(width = 12,
              title = "DraftKings Lineup Filters",
              fluidRow(
                column(3, numericInput("dk_min_top1_count", "Min Top 1 Count:", value = 0, min = 0)),
                column(3, numericInput("dk_min_top10_count", "Min Top 10 Count:", value = 0, min = 0)),
                column(3, numericInput("dk_min_top50_count", "Min Top 50 Count:", value = 0, min = 0)),
                column(3, numericInput("dk_min_top100_count", "Min Top 100 Count:", value = 0, min = 0))
              ),
              fluidRow(
                column(6, sliderInput("dk_cumulative_ownership_range", "Cumulative Ownership Range:",
                                      min = dk_cum_range[1], max = dk_cum_range[2], 
                                      value = dk_cum_range, step = 1, post = "%")),
                column(6, sliderInput("dk_geometric_mean_range", "Geometric Mean Ownership Range:",
                                      min = dk_geom_range[1], max = dk_geom_range[2], 
                                      value = dk_geom_range, step = 1, post = "%"))
              ),
              fluidRow(
                column(3, numericInput("dk_min_atleast6_prob", 
                                       "Min Prob All 6 Make Cut (%):", 
                                       value = 0, min = 0, max = 100, step = 1)),
                column(3, numericInput("dk_min_atleast5_prob", 
                                       "Min Prob At Least 5 Make Cut (%):", 
                                       value = 0, min = 0, max = 100, step = 1)),
                column(3, numericInput("dk_min_expected_cuts", 
                                       "Min Expected Cuts:", 
                                       value = 0, min = 0, max = 6, step = 0.1)),
                column(3, numericInput("dk_min_salary", 
                                       "Min Total Lineup Salary:", 
                                       value = 49000, min = 0, max = 50000, step = 100))
              ),
              fluidRow(
                column(6, selectizeInput("dk_excluded_players", "Exclude Players:", choices = NULL, multiple = TRUE,
                                         options = list(plugins = list('remove_button'), placeholder = 'Click to select players to exclude'))),
                column(6, numericInput("dk_num_random_lineups", "Number of Lineups to Generate:", value = 20, min = 1, max = 150))
              ),
              fluidRow(
                column(6, div(class = "well well-sm", h4("Filtered Pool Statistics:"), textOutput("dk_filtered_pool_size"))),
                column(6, div(style = "margin-top: 20px;",
                              actionButton("generate_dk_lineups", "Randomize DraftKings Lineups", class = "btn-primary btn-lg", style = "width: 100%;"),
                              br(), br(),
                              downloadButton("download_dk_random_lineups", "Download Selected Lineups", style = "width: 100%;")))
              )
          )
        ),
        fluidRow(
          box(width = 12, title = "DraftKings Player Exposure Analysis",
              DTOutput("dk_player_exposure_table") %>% withSpinner(color = "#FFD700"))
        ),
        fluidRow(
          box(width = 12, title = "Generated DraftKings Lineups",
              DTOutput("dk_random_lineups_table") %>% withSpinner(color = "#FFD700"))
        )
      ),
      
      # FanDuel UI
      conditionalPanel(
        condition = "input.lineup_builder_platform == 'fd' && output.has_fd_lineups == 'true'",
        fluidRow(
          box(width = 12,
              title = "FanDuel Lineup Filters",
              fluidRow(
                column(3, numericInput("fd_min_top1_count", "Min Top 1 Count:", value = 0, min = 0)),
                column(3, numericInput("fd_min_top10_count", "Min Top 10 Count:", value = 0, min = 0)),
                column(3, numericInput("fd_min_top50_count", "Min Top 50 Count:", value = 0, min = 0)),
                column(3, numericInput("fd_min_top100_count", "Min Top 100 Count:", value = 0, min = 0))
              ),
              fluidRow(
                column(6, selectizeInput("fd_excluded_players", "Exclude Players:", choices = NULL, multiple = TRUE,
                                         options = list(plugins = list('remove_button'), placeholder = 'Click to select players to exclude'))),
                column(6, numericInput("fd_num_random_lineups", "Number of Lineups to Generate:", value = 20, min = 1, max = 150))
              ),
              fluidRow(
                column(6, div(class = "well well-sm", h4("Filtered Pool Statistics:"), textOutput("fd_filtered_pool_size"))),
                column(6, div(style = "margin-top: 20px;",
                              actionButton("generate_fd_lineups", "Randomize FanDuel Lineups", class = "btn-primary btn-lg", style = "width: 100%;"),
                              br(), br(),
                              downloadButton("download_fd_random_lineups", "Download Selected Lineups", style = "width: 100%;")))
              )
          )
        ),
        fluidRow(
          box(width = 12, title = "FanDuel Player Exposure Analysis",
              DTOutput("fd_player_exposure_table") %>% withSpinner(color = "#FFD700"))
        ),
        fluidRow(
          box(width = 12, title = "Generated FanDuel Lineups",
              DTOutput("fd_random_lineups_table") %>% withSpinner(color = "#FFD700"))
        )
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)