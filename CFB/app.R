# CFB SHOWDOWN SIMULATOR
# Daily Fantasy Sports Simulation for College Football  
# Author: Andrew
# Golden Ticket Sims
# Date: December 2024

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(plotly)
library(data.table)
library(shinyjs)
library(shinycssloaders)
library(lpSolve)
# Source lineup builder functions
source("cfb_lineup_builder_functions.R")


# Constants
DK_SALARY_CAP <- 50000
ROSTER_SIZE <- 6  # 1 CPT + 5 FLEX

# Team colors for common CFB teams
TEAM_COLORS <- list(
  "Utah State" = "navyblue",
  "Washington State" = "darkred"
)

# Function to get team color
get_team_color <- function(team_name) {
  # Clean up team name
  clean_name <- gsub("_", " ", team_name)
  
  if (clean_name %in% names(TEAM_COLORS)) {
    return(TEAM_COLORS[[clean_name]])
  }
  
  # Default colors if team not found
  return("#FFD700")
}

# Custom CSS for black and gold theme
custom_css <- "
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
  
  .box.box-primary .box-header {
    background-color: #333333;
    color: #FFD700;
  }
  
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
  
  .shiny-spinner .load-container .loader {
    border-top-color: #FFD700;
  }
"

# Read input file function
read_input_file <- function(file_path) {
  tryCatch({
    sheets <- excel_sheets(file_path)
    
    # Get team names from rushing sheets
    rushing_sheets <- sheets[grepl("_Rushing$", sheets)]
    team_names <- gsub("_Rushing$", "", rushing_sheets)
    
    input_data <- list()
    
    # Load team data
    for (team in team_names) {
      input_data[[team]] <- list(
        rushing = read_excel(file_path, sheet = paste0(team, "_Rushing")),
        receiving = read_excel(file_path, sheet = paste0(team, "_Receiving")),
        passing = read_excel(file_path, sheet = paste0(team, "_Passing")),
        kicking = read_excel(file_path, sheet = paste0(team, "_Kicking"))
      )
    }
    
    # Load other sheets
    input_data$dk_salaries <- read_excel(file_path, sheet = "DK_Salaries_IDs")
    input_data$sim_totals <- read_excel(file_path, sheet = "Sim_Totals")
    input_data$similar_games <- read_excel(file_path, sheet = "Similar_Games")
    input_data$team_names <- team_names
    
    return(input_data)
  }, error = function(e) {
    stop(paste("Error reading Excel file:", e$message))
  })
}

# Simulation function
simulate_team_game <- function(sim_id, team_name, team_data, sampled_game, dk_salaries) {
  
  # Sheet names have underscores, Similar_Games columns have spaces
  team_name_for_cols <- gsub("_", " ", team_name)
  
  # Extract game stats
  rush_col <- paste0(team_name_for_cols, "_Rush")
  pass_col <- paste0(team_name_for_cols, "_Pass")
  rush_td_col <- paste0(team_name_for_cols, "_Rush_TDs")
  pass_td_col <- paste0(team_name_for_cols, "_Pass_TDs")
  fg_col <- paste0(team_name_for_cols, "_FGs")
  
  team_rush_yds <- as.numeric(sampled_game[[rush_col]])
  team_pass_yds <- as.numeric(sampled_game[[pass_col]])
  team_rush_tds <- as.numeric(sampled_game[[rush_td_col]])
  team_pass_tds <- as.numeric(sampled_game[[pass_td_col]])
  team_fgs <- as.numeric(sampled_game[[fg_col]])
  
  # Validate and set defaults
  if (is.na(team_rush_yds) || length(team_rush_yds) == 0) team_rush_yds <- 0
  if (is.na(team_pass_yds) || length(team_pass_yds) == 0) team_pass_yds <- 0
  if (is.na(team_rush_tds) || length(team_rush_tds) == 0) team_rush_tds <- 0
  if (is.na(team_pass_tds) || length(team_pass_tds) == 0) team_pass_tds <- 0
  if (is.na(team_fgs) || length(team_fgs) == 0) team_fgs <- 0
  
  # Pre-allocate results list
  all_player_results <- list()
  result_idx <- 1
  
  # Helper function: sample from percentiles (MMA-style piecewise linear)
  sample_from_percentiles <- function(floor, p25, p50, p75, ceiling) {
    percentile <- runif(1, 0, 1)
    
    if (percentile <= 0.05) {
      return(0)  # Below floor = injury/benched
    } else if (percentile <= 0.25) {
      return(floor + (p25 - floor) * (percentile - 0.05) / 0.20)
    } else if (percentile <= 0.50) {
      return(p25 + (p50 - p25) * (percentile - 0.25) / 0.25)
    } else if (percentile <= 0.75) {
      return(p50 + (p75 - p50) * (percentile - 0.50) / 0.25)
    } else if (percentile <= 0.95) {
      return(p75 + (ceiling - p75) * (percentile - 0.75) / 0.20)
    } else {
      return(ceiling)  # Hard ceiling at P95
    }
  }
  
  # RUSHING
  rushing_data <- team_data$rushing
  n_rushers <- nrow(rushing_data)
  
  if (n_rushers > 0) {
    # Smart allocation with cumulative adjustment
    rush_yds_allocation <- numeric(n_rushers)
    remaining_share <- 1.0
    allocated_share <- 0.0
    
    for (i in 1:n_rushers) {
      floor_pct <- rushing_data$Pct_P10[i]
      p25_pct <- rushing_data$Pct_P25[i]
      p50_pct <- rushing_data$Pct_P50[i]
      p75_pct <- rushing_data$Pct_P75[i]
      ceiling_pct <- rushing_data$Pct_P90[i]
      
      # Handle NAs
      if (is.na(floor_pct)) floor_pct <- 0
      if (is.na(p25_pct)) p25_pct <- floor_pct
      if (is.na(p50_pct)) p50_pct <- p25_pct
      if (is.na(p75_pct)) p75_pct <- p50_pct
      if (is.na(ceiling_pct)) ceiling_pct <- p75_pct
      
      # SMART STOP: If we've allocated 98%+ and this player has Floor=0, skip
      if (allocated_share > 0.98 && floor_pct == 0) {
        rush_yds_allocation[i] <- 0
        next
      }
      
      # Sample player share
      player_share <- sample_from_percentiles(floor_pct, p25_pct, p50_pct, p75_pct, ceiling_pct)
      
      # CUMULATIVE ADJUSTMENT
      if (i > 1 && remaining_share < 1.0) {
        # Only count remaining players who are expected to play (P50 > 1%)
        remaining_players_expected <- rushing_data$Pct_P50[(i):n_rushers]
        remaining_players_expected[is.na(remaining_players_expected)] <- 0
        remaining_players_expected <- remaining_players_expected[remaining_players_expected > 0.01]
        
        expected_remaining <- sum(remaining_players_expected, na.rm = TRUE)
        
        if (expected_remaining > 0) {
          adjustment_factor <- remaining_share / expected_remaining
          adjustment_factor <- max(0.5, min(2.0, adjustment_factor))
          player_share <- player_share * adjustment_factor
        }
      }
      
      # Ensure player doesn't exceed remaining share
      player_share <- min(player_share, remaining_share)
      player_share <- max(0, player_share)
      
      # SMART ROUNDING: If remaining < 5%, give it all to this player or none
      if (remaining_share < 0.05) {
        if (player_share >= remaining_share * 0.5) {
          player_share <- remaining_share  # Give the scraps
        } else {
          player_share <- 0  # Not worth it
        }
      }
      
      rush_yds_allocation[i] <- round(team_rush_yds * player_share)
      remaining_share <- remaining_share - player_share
      allocated_share <- allocated_share + player_share
      
      # HARD STOP: If we've allocated everything, done
      if (remaining_share <= 0.01) {
        # Zero out remaining players
        if (i < n_rushers) {
          rush_yds_allocation[(i+1):n_rushers] <- 0
        }
        break
      }
    }
    
    # SWEEP UP: Distribute any remaining yards proportionally to active players
    if (remaining_share > 0.01) {
      active_players <- which(rush_yds_allocation > 0)
      
      if (length(active_players) > 0) {
        # Distribute remaining yards proportionally based on what they already got
        active_yards <- rush_yds_allocation[active_players]
        proportions <- active_yards / sum(active_yards)
        
        extra_yards <- round(team_rush_yds * remaining_share * proportions)
        rush_yds_allocation[active_players] <- rush_yds_allocation[active_players] + extra_yards
      }
    }
    
    # Second pass: Allocate rushing TDs one-by-one
    td_allocation <- rep(0, n_rushers)
    if (team_rush_tds > 0) {
      td_rates <- as.numeric(rushing_data$TD_Rate)
      td_rates[is.na(td_rates)] <- 0
      td_rates[td_rates < 0] <- 0
      
      if (sum(td_rates) > 0) {
        td_probs <- td_rates / sum(td_rates)
        
        for (td in 1:team_rush_tds) {
          selected <- sample(1:n_rushers, 1, prob = td_probs)
          td_allocation[selected] <- td_allocation[selected] + 1
        }
      }
    }
    
    # Create player results
    for (i in 1:n_rushers) {
      all_player_results[[result_idx]] <- list(
        SimID = sim_id, Team = team_name, Player = rushing_data$Player[i],
        PassYds = 0, PassTDs = 0L, INTs = 0L,
        RushYds = rush_yds_allocation[i], 
        RushTDs = as.integer(td_allocation[i]),
        Recs = 0L, RecYds = 0, RecTDs = 0L,
        FGsMade = 0L, FG_Under30 = 0L, FG_30_39 = 0L, FG_40_49 = 0L, FG_50Plus = 0L,
        XPs = 0L, FumLost = 0L
      )
      result_idx <- result_idx + 1
    }
  }
  
  # RECEIVING
  receiving_data <- team_data$receiving
  n_receivers <- nrow(receiving_data)
  
  if (n_receivers > 0) {
    # Get team total receptions from similar game
    team_recs_col <- paste0(team_name_for_cols, "_Recs")
    team_total_recs <- as.numeric(sampled_game[[team_recs_col]])
    if (is.na(team_total_recs) || length(team_total_recs) == 0) team_total_recs <- 0
    
    # First pass: Allocate receiving yards with smart cumulative adjustment
    rec_yds_allocation <- numeric(n_receivers)
    ypr_values <- numeric(n_receivers)
    remaining_share <- 1.0
    allocated_share <- 0.0
    
    for (i in 1:n_receivers) {
      floor_pct <- receiving_data$Pct_P10[i]
      p25_pct <- receiving_data$Pct_P25[i]
      p50_pct <- receiving_data$Pct_P50[i]
      p75_pct <- receiving_data$Pct_P75[i]
      ceiling_pct <- receiving_data$Pct_P90[i]
      
      # Handle NAs
      if (is.na(floor_pct)) floor_pct <- 0
      if (is.na(p25_pct)) p25_pct <- floor_pct
      if (is.na(p50_pct)) p50_pct <- p25_pct
      if (is.na(p75_pct)) p75_pct <- p50_pct
      if (is.na(ceiling_pct)) ceiling_pct <- p75_pct
      
      # SMART STOP: If we've allocated 98%+ and this player has Floor=0, skip
      if (allocated_share > 0.98 && floor_pct == 0) {
        rec_yds_allocation[i] <- 0
        ypr_values[i] <- 10
        next
      }
      
      # Sample player share
      target_share <- sample_from_percentiles(floor_pct, p25_pct, p50_pct, p75_pct, ceiling_pct)
      
      # CUMULATIVE ADJUSTMENT
      if (i > 1 && remaining_share < 1.0) {
        # Only count remaining players who are expected to play (P50 > 1%)
        remaining_players_expected <- receiving_data$Pct_P50[(i):n_receivers]
        remaining_players_expected[is.na(remaining_players_expected)] <- 0
        remaining_players_expected <- remaining_players_expected[remaining_players_expected > 0.01]
        
        expected_remaining <- sum(remaining_players_expected, na.rm = TRUE)
        
        if (expected_remaining > 0) {
          adjustment_factor <- remaining_share / expected_remaining
          adjustment_factor <- max(0.5, min(2.0, adjustment_factor))
          target_share <- target_share * adjustment_factor
        }
      }
      
      # Ensure player doesn't exceed remaining share
      target_share <- min(target_share, remaining_share)
      target_share <- max(0, target_share)
      
      # SMART ROUNDING: If remaining < 5%, give it all or none
      if (remaining_share < 0.05) {
        if (target_share >= remaining_share * 0.5) {
          target_share <- remaining_share
        } else {
          target_share <- 0
        }
      }
      
      rec_yds_allocation[i] <- round(team_pass_yds * target_share)
      remaining_share <- remaining_share - target_share
      allocated_share <- allocated_share + target_share
      
      ypr <- receiving_data$YPR[i]
      if (is.na(ypr) || ypr <= 0) ypr <- 10
      ypr_values[i] <- ypr
      
      # HARD STOP: If we've allocated everything, done
      if (remaining_share <= 0.01) {
        if (i < n_receivers) {
          rec_yds_allocation[(i+1):n_receivers] <- 0
          ypr_values[(i+1):n_receivers] <- 10
        }
        break
      }
    }
    
    # SWEEP UP: Distribute any remaining yards proportionally to active players
    if (remaining_share > 0.01) {
      active_players <- which(rec_yds_allocation > 0)
      
      if (length(active_players) > 0) {
        # Distribute remaining yards proportionally based on what they already got
        active_yards <- rec_yds_allocation[active_players]
        proportions <- active_yards / sum(active_yards)
        
        extra_yards <- round(team_pass_yds * remaining_share * proportions)
        rec_yds_allocation[active_players] <- rec_yds_allocation[active_players] + extra_yards
      }
    }
    
    # Second pass: Allocate receptions based on YPR profile
    expected_recs <- rec_yds_allocation / ypr_values
    expected_recs[is.na(expected_recs)] <- 0
    expected_recs[is.infinite(expected_recs)] <- 0
    
    # Normalize to match team total receptions
    if (sum(expected_recs) > 0 && team_total_recs > 0) {
      rec_probs <- expected_recs / sum(expected_recs)
      
      # Allocate receptions one-by-one
      rec_allocation <- rep(0, n_receivers)
      for (rec in 1:team_total_recs) {
        selected <- sample(1:n_receivers, 1, prob = rec_probs)
        rec_allocation[selected] <- rec_allocation[selected] + 1
      }
    } else {
      rec_allocation <- rep(0, n_receivers)
    }
    
    # Third pass: Allocate receiving TDs one-by-one
    td_allocation <- rep(0, n_receivers)
    if (team_pass_tds > 0) {
      td_rates <- as.numeric(receiving_data$TD_Rate)
      td_rates[is.na(td_rates)] <- 0
      td_rates[td_rates < 0] <- 0
      
      if (sum(td_rates) > 0) {
        td_probs <- td_rates / sum(td_rates)
        
        for (td in 1:team_pass_tds) {
          selected <- sample(1:n_receivers, 1, prob = td_probs)
          td_allocation[selected] <- td_allocation[selected] + 1
        }
      }
    }
    
    # Now create player results
    for (i in 1:n_receivers) {
      player_name <- receiving_data$Player[i]
      existing_idx <- which(sapply(all_player_results, function(x) x$Player == player_name))
      
      if (length(existing_idx) > 0) {
        all_player_results[[existing_idx[1]]]$Recs <- as.integer(rec_allocation[i])
        all_player_results[[existing_idx[1]]]$RecYds <- rec_yds_allocation[i]
        all_player_results[[existing_idx[1]]]$RecTDs <- as.integer(td_allocation[i])
      } else {
        all_player_results[[result_idx]] <- list(
          SimID = sim_id, Team = team_name, Player = player_name,
          PassYds = 0, PassTDs = 0L, INTs = 0L,
          RushYds = 0, RushTDs = 0L,
          Recs = as.integer(rec_allocation[i]), 
          RecYds = rec_yds_allocation[i], 
          RecTDs = as.integer(td_allocation[i]),
          FGsMade = 0L, FG_Under30 = 0L, FG_30_39 = 0L, FG_40_49 = 0L, FG_50Plus = 0L,
          XPs = 0L, FumLost = 0L
        )
        result_idx <- result_idx + 1
      }
    }
  }
  
  # PASSING
  passing_data <- team_data$passing
  
  if (nrow(passing_data) > 0) {
    for (i in 1:nrow(passing_data)) {
      pass_share <- passing_data$Pass_Share[i]
      if (is.na(pass_share)) pass_share <- 0
      
      player_pass_yds <- round(team_pass_yds * pass_share)
      player_pass_tds <- round(team_pass_tds * pass_share)
      
      player_name <- passing_data$Player[i]
      existing_idx <- which(sapply(all_player_results, function(x) x$Player == player_name))
      
      if (length(existing_idx) > 0) {
        all_player_results[[existing_idx[1]]]$PassYds <- player_pass_yds
        all_player_results[[existing_idx[1]]]$PassTDs <- player_pass_tds
      } else {
        all_player_results[[result_idx]] <- list(
          SimID = sim_id, Team = team_name, Player = player_name,
          PassYds = player_pass_yds, PassTDs = player_pass_tds, INTs = 0L,
          RushYds = 0, RushTDs = 0L,
          Recs = 0L, RecYds = 0, RecTDs = 0L,
          FGsMade = 0L, FG_Under30 = 0L, FG_30_39 = 0L, FG_40_49 = 0L, FG_50Plus = 0L,
          XPs = 0L, FumLost = 0L
        )
        result_idx <- result_idx + 1
      }
    }
  }
  
  # KICKING
  kicking_data <- team_data$kicking
  total_xps <- team_rush_tds + team_pass_tds
  
  # Get kicker name from kicking sheet
  kicker <- if (nrow(kicking_data) > 0 && "Kicker" %in% names(kicking_data)) {
    kicking_data$Kicker[1]
  } else {
    paste0(team_name, " K")
  }
  
  fg_under30 <- 0L
  fg_30_39 <- 0L
  fg_40_49 <- 0L
  fg_50plus <- 0L
  
  if (team_fgs > 0) {
    distance_probs <- as.numeric(kicking_data$Percentage) / 100
    for (fg in 1:team_fgs) {
      distance_cat <- sample(1:4, 1, prob = distance_probs)
      if (distance_cat == 1) fg_under30 <- fg_under30 + 1L
      else if (distance_cat == 2) fg_30_39 <- fg_30_39 + 1L
      else if (distance_cat == 3) fg_40_49 <- fg_40_49 + 1L
      else fg_50plus <- fg_50plus + 1L
    }
  }
  
  all_player_results[[result_idx]] <- list(
    SimID = sim_id, Team = team_name, Player = kicker,
    PassYds = 0, PassTDs = 0L, INTs = 0L,
    RushYds = 0, RushTDs = 0L,
    Recs = 0L, RecYds = 0, RecTDs = 0L,
    FGsMade = as.integer(team_fgs), FG_Under30 = fg_under30, FG_30_39 = fg_30_39, 
    FG_40_49 = fg_40_49, FG_50Plus = fg_50plus,
    XPs = as.integer(total_xps), FumLost = 0L
  )
  
  # Convert to data.table
  results <- rbindlist(all_player_results)
  
  # Calculate fantasy points
  results[, `:=`(
    PassPts = (PassYds * 0.04) + (PassTDs * 4) - INTs + ifelse(PassYds >= 300, 3, 0),
    RushPts = (RushYds * 0.1) + (RushTDs * 6) + ifelse(RushYds >= 100, 3, 0),
    RecPts = Recs + (RecYds * 0.1) + (RecTDs * 6) + ifelse(RecYds >= 100, 3, 0),
    KickPts = XPs + (FG_Under30 * 3) + (FG_30_39 * 3) + (FG_40_49 * 4) + (FG_50Plus * 5),
    FumPts = FumLost * -1
  )]
  
  results[, TotalPts := PassPts + RushPts + RecPts + KickPts + FumPts]
  
  return(results)
}

# Run all simulations
run_simulations <- function(input_data, n_sims) {
  
  cat(sprintf("\n=== RUNNING %s SIMULATIONS ===\n", format(n_sims, big.mark = ",")))
  
  team_names <- input_data$team_names
  similar_games <- input_data$similar_games
  dk_salaries <- input_data$dk_salaries
  
  team1_data <- input_data[[team_names[1]]]
  team2_data <- input_data[[team_names[2]]]
  
  cat(sprintf("Teams: %s vs %s\n", team_names[1], team_names[2]))
  cat(sprintf("Similar games: %d\n\n", nrow(similar_games)))
  
  all_results <- vector("list", n_sims * 2)
  result_idx <- 1
  
  batch_size <- 500
  n_batches <- ceiling(n_sims / batch_size)
  start_time <- Sys.time()
  
  for (batch in 1:n_batches) {
    start_sim <- (batch - 1) * batch_size + 1
    end_sim <- min(batch * batch_size, n_sims)
    
    for (sim in start_sim:end_sim) {
      sampled_game <- similar_games[sample(nrow(similar_games), 1), ]
      
      team1_result <- simulate_team_game(sim, team_names[1], team1_data, sampled_game, dk_salaries)
      all_results[[result_idx]] <- team1_result
      result_idx <- result_idx + 1
      
      team2_result <- simulate_team_game(sim, team_names[2], team2_data, sampled_game, dk_salaries)
      all_results[[result_idx]] <- team2_result
      result_idx <- result_idx + 1
    }
    
    if (batch %% 5 == 0 || batch == n_batches) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      sims_done <- end_sim
      rate <- sims_done / elapsed
      eta <- (n_sims - sims_done) / rate
      
      cat(sprintf("Batch %d/%d: %d/%d sims (%.1f%%) | %.0f sims/sec | ETA: %.0fs\n",
                  batch, n_batches, sims_done, n_sims, (sims_done/n_sims)*100, rate, eta))
    }
    
    if (batch %% 10 == 0) gc(verbose = FALSE, full = FALSE)
  }
  
  combined <- rbindlist(all_results, use.names = TRUE, fill = TRUE)
  
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(sprintf("\n✓ Complete! %.1f seconds (%.0f sims/sec)\n\n", total_time, n_sims/total_time))
  
  return(combined)
}

# Analyze fantasy scoring
analyze_fantasy_scoring <- function(sim_results, dk_salaries = NULL) {
  setDT(sim_results)
  
  scoring_stats <- sim_results[, .(
    AvgPts = mean(TotalPts, na.rm = TRUE),
    MedianPts = median(TotalPts, na.rm = TRUE),
    MaxPts = max(TotalPts, na.rm = TRUE)
  ), by = .(Player, Team)]
  
  # Add salary if available
  if (!is.null(dk_salaries)) {
    setDT(dk_salaries)
    scoring_stats <- merge(scoring_stats, 
                           dk_salaries[, .(Name, Salary, Team)], 
                           by.x = c("Player", "Team"), 
                           by.y = c("Name", "Team"), 
                           all.x = TRUE)
  }
  
  # Convert team names to abbreviations
  scoring_stats[, TeamAbbr := ifelse(grepl("Texas", Team), "TA&M",
                                     ifelse(grepl("Miami", Team), "MIA",
                                            ifelse(grepl("Utah State", Team), "USU",
                                                   ifelse(grepl("Washington State", Team), "WSU",
                                                          Team))))]
  
  setorder(scoring_stats, -AvgPts)
  
  return(as.data.frame(scoring_stats))
}

# Team level stats - TOTAL TEAM STATS PER SIMULATION
analyze_team_stats <- function(sim_results) {
  setDT(sim_results)
  
  # Sum up all stats by SimID and Team to get TEAM TOTALS per game
  # NOTE: PassTDs are already counted in RecTDs (receiver gets the TD), so don't double count
  team_stats <- sim_results[, .(
    TotalPassYds = sum(PassYds, na.rm = TRUE),
    TotalRushYds = sum(RushYds, na.rm = TRUE),
    TotalRecYds = sum(RecYds, na.rm = TRUE),
    TotalTDs = sum(RushTDs, na.rm = TRUE) + sum(RecTDs, na.rm = TRUE),  # Rush TDs + Rec TDs (which include passing)
    TotalRecs = sum(Recs, na.rm = TRUE),
    TotalFGs = sum(FGsMade, na.rm = TRUE),
    TotalXPs = sum(XPs, na.rm = TRUE)
  ), by = .(SimID, Team)]
  
  return(as.data.frame(team_stats))
}

# Generate optimal DK Showdown lineups - OPTIMIZED
generate_showdown_lineups <- function(sim_results, dk_salaries, n_sims, top_k = 5) {
  
  cat("\n=== GENERATING OPTIMAL LINEUPS ===\n")
  
  setDT(sim_results)
  setDT(dk_salaries)
  
  SALARY_CAP <- 50000
  ROSTER_SIZE <- 6
  
  sim_ids <- unique(sim_results$SimID)
  n_actual_sims <- length(sim_ids)
  
  cat(sprintf("Processing %s simulations...\n", format(n_actual_sims, big.mark = ",")))
  
  # Check if correct ID columns exist
  if ("DFS_ID" %in% names(dk_salaries) && "CPT_DFS_ID" %in% names(dk_salaries)) {
    # Use the actual ID columns
    dk_salaries_with_ids <- dk_salaries[, .(Name, Salary, DFS_ID, CPT_DFS_ID)]
  } else {
    # Generate IDs if columns don't exist
    dk_salaries_with_ids <- dk_salaries[, .(Name, Salary)]
    dk_salaries_with_ids[, DFS_ID := paste0("ID", 1:.N)]
    dk_salaries_with_ids[, CPT_DFS_ID := paste0("CPTID", 1:.N)]
    cat("Note: DFS_ID/CPT_DFS_ID columns not found, using generated IDs\n")
  }
  
  # Merge salary data ONCE
  sim_results <- merge(sim_results, dk_salaries_with_ids, 
                       by.x = "Player", by.y = "Name", all.x = TRUE)
  
  sim_results <- sim_results[!is.na(Salary)]
  
  # Pre-calculate captain scores and salaries
  sim_results[, CPT_Score := TotalPts * 1.5]
  sim_results[, CPT_Salary := as.integer(Salary * 1.5)]
  
  all_teams <- unique(sim_results$Team)
  n_teams <- length(all_teams)
  
  # Pre-allocate
  all_lineups <- vector("list", n_actual_sims)
  
  batch_size <- 500
  n_batches <- ceiling(n_actual_sims / batch_size)
  start_time <- Sys.time()
  
  for (batch in 1:n_batches) {
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, n_actual_sims)
    batch_sims <- sim_ids[start_idx:end_idx]
    
    batch_data <- sim_results[SimID %in% batch_sims]
    batch_by_sim <- split(batch_data, batch_data$SimID)
    
    batch_results <- lapply(batch_by_sim, function(sim_data) {
      
      if (nrow(sim_data) < 6 || length(unique(sim_data$Team)) < n_teams) {
        return(NULL)
      }
      
      setorder(sim_data, -TotalPts)
      
      found_lineups <- character(top_k)
      found_captains <- character(top_k)
      found_count <- 0
      
      setorder(sim_data, -CPT_Score)
      
      for (cpt_idx in 1:nrow(sim_data)) {
        if (found_count >= top_k) break
        
        cpt_player <- sim_data$Player[cpt_idx]
        cpt_team <- sim_data$Team[cpt_idx]
        cpt_salary <- sim_data$CPT_Salary[cpt_idx]
        
        remaining_budget <- SALARY_CAP - cpt_salary
        
        flex_candidates <- sim_data[Player != cpt_player & Salary <= remaining_budget]
        
        if (nrow(flex_candidates) < 5) next
        
        selected <- flex_candidates[1:min(20, nrow(flex_candidates))]
        
        lineup_players <- character(0)
        lineup_teams <- cpt_team
        total_salary <- 0
        
        for (i in 1:nrow(selected)) {
          if (length(lineup_players) >= 5) break
          
          player <- selected$Player[i]
          player_team <- selected$Team[i]
          player_salary <- selected$Salary[i]
          
          if (total_salary + player_salary <= remaining_budget) {
            lineup_players <- c(lineup_players, player)
            lineup_teams <- unique(c(lineup_teams, player_team))
            total_salary <- total_salary + player_salary
          }
        }
        
        if (length(lineup_players) != 5 || length(lineup_teams) < n_teams) next
        
        # Create lineup string with captain marked (so different captains = different lineups)
        lineup_str <- paste0("CPT:", cpt_player, "|", paste(sort(lineup_players), collapse = "|"))
        
        if (lineup_str %in% found_lineups[1:found_count]) next
        
        found_count <- found_count + 1
        found_lineups[found_count] <- lineup_str
        found_captains[found_count] <- cpt_player
      }
      
      if (found_count > 0) {
        data.table(
          Lineup = found_lineups[1:found_count],
          Captain = found_captains[1:found_count],
          Rank = 1:found_count
        )
      } else {
        NULL
      }
    })
    
    for (i in seq_along(batch_results)) {
      all_lineups[[start_idx + i - 1]] <- batch_results[[i]]
    }
    
    if (batch %% 5 == 0 || batch == n_batches) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      sims_processed <- end_idx
      rate <- sims_processed / elapsed
      eta <- (n_actual_sims - sims_processed) / rate
      
      cat(sprintf("Batch %d/%d (%.1f%%) | %.0f sims/sec | ETA: %.0fs\n",
                  batch, n_batches, (sims_processed/n_actual_sims)*100, rate, eta))
    }
    
    if (batch %% 5 == 0) gc(verbose = FALSE, full = FALSE)
  }
  
  cat("\nAggregating results...\n")
  
  valid_lineups <- all_lineups[!sapply(all_lineups, is.null)]
  
  if (length(valid_lineups) == 0) {
    cat("❌ No valid lineups found\n")
    return(NULL)
  }
  
  combined_lineups <- rbindlist(valid_lineups, fill = TRUE)
  
  lineup_counts <- combined_lineups[, .N, by = .(Lineup, Captain)]
  
  for (r in 1:top_k) {
    rank_col <- paste0("Rank", r, "Count")
    rank_data <- combined_lineups[Rank == r, .N, by = Lineup]
    setnames(rank_data, "N", rank_col)
    lineup_counts <- merge(lineup_counts, rank_data, by = "Lineup", all.x = TRUE)
    lineup_counts[is.na(get(rank_col)), (rank_col) := 0]
  }
  
  lineup_counts[, Top1Count := Rank1Count]
  lineup_counts[, Top1Count := Rank1Count]
  lineup_counts[, Top2Count := Rank1Count + Rank2Count]
  lineup_counts[, Top3Count := Rank1Count + Rank2Count + Rank3Count]
  lineup_counts[, Top5Count := Rank1Count + Rank2Count + Rank3Count + Rank4Count + Rank5Count]
  
  # Parse lineup string (format: "CPT:PlayerName|Player1|Player2...")
  lineup_counts[, FlexPlayers := gsub("^CPT:[^|]+\\|", "", Lineup)]
  lineup_counts[, Players := strsplit(FlexPlayers, "\\|")]
  
  # Extract the 5 FLEX players
  for (i in 1:5) {
    lineup_counts[, paste0("Player", i) := sapply(Players, function(p) {
      if (length(p) >= i) p[i] else NA_character_
    })]
  }
  
  lineup_counts[, Players := NULL]
  lineup_counts[, FlexPlayers := NULL]
  lineup_counts[, Lineup := NULL]
  
  # Add IDs and calculate ownership/salary metrics
  cpt_id_lookup <- setNames(dk_salaries_with_ids$CPT_DFS_ID, dk_salaries_with_ids$Name)
  flex_id_lookup <- setNames(dk_salaries_with_ids$DFS_ID, dk_salaries_with_ids$Name)
  salary_lookup <- setNames(dk_salaries_with_ids$Salary, dk_salaries_with_ids$Name)
  
  # Get ownership data if available
  if ("CPT_Own" %in% names(dk_salaries_with_ids) && "Flex_Own" %in% names(dk_salaries_with_ids)) {
    cpt_own_lookup <- setNames(dk_salaries_with_ids$CPT_Own, dk_salaries_with_ids$Name)
    flex_own_lookup <- setNames(dk_salaries_with_ids$Flex_Own, dk_salaries_with_ids$Name)
    has_ownership <- TRUE
  } else {
    has_ownership <- FALSE
  }
  
  # Format captain with CPT_DFS_ID
  lineup_counts[, Captain := paste0(Captain, " (", cpt_id_lookup[gsub(" \\(.*\\)", "", Captain)], ")")]
  
  # Format flex players with DFS_ID
  for (i in 1:5) {
    player_col <- paste0("Player", i)
    lineup_counts[, (player_col) := paste0(get(player_col), " (", flex_id_lookup[get(player_col)], ")")]
  }
  
  # Calculate total salary
  lineup_counts[, TotalSalary := 0]
  lineup_counts[, CPT_Salary := 0]
  
  # Captain salary (1.5x)
  cpt_name <- gsub(" \\(.*\\)", "", lineup_counts$Captain)
  lineup_counts[, CPT_Salary := salary_lookup[cpt_name] * 1.5]
  lineup_counts[, TotalSalary := CPT_Salary]
  
  # Flex salaries
  for (i in 1:5) {
    player_col <- paste0("Player", i)
    player_name <- gsub(" \\(.*\\)", "", lineup_counts[[player_col]])
    lineup_counts[, TotalSalary := TotalSalary + salary_lookup[player_name]]
  }
  
  # Calculate projected ownership if available
  if (has_ownership) {
    lineup_counts[, CPT_Own := cpt_own_lookup[cpt_name]]
    lineup_counts[, TotalOwn := CPT_Own]
    
    for (i in 1:5) {
      player_col <- paste0("Player", i)
      player_name <- gsub(" \\(.*\\)", "", lineup_counts[[player_col]])
      lineup_counts[, TotalOwn := TotalOwn + flex_own_lookup[player_name]]
    }
    
    lineup_counts[, AvgOwn := TotalOwn / 6]
  }
  
  # Remove individual rank count columns, keep only Top counts
  player_cols <- paste0("Player", 1:5)
  
  if (has_ownership) {
    final_cols <- c("Captain", player_cols, "Top1Count", "Top2Count", "Top3Count", "Top5Count", 
                    "TotalSalary", "AvgOwn")
  } else {
    final_cols <- c("Captain", player_cols, "Top1Count", "Top2Count", "Top3Count", "Top5Count", 
                    "TotalSalary")
  }
  
  result <- lineup_counts[, ..final_cols]
  setorder(result, -Top1Count)
  
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  cat(sprintf("\n✓ Found %s unique lineups in %.1f seconds\n\n", 
              format(nrow(result), big.mark = ","), total_time))
  
  return(as.data.frame(result))
  
  # CFB SHOWDOWN LINEUP BUILDER FUNCTIONS
  # Functions for filtering and randomizing optimal lineups
  
  # Calculate filtered pool statistics
  calculate_filtered_pool_stats <- function(optimal_lineups, filters) {
    if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
      return(list(count = 0))
    }
    
    filtered_lineups <- as.data.table(optimal_lineups)
    
    # Apply Top Count filters
    if (!is.null(filters$min_top1_count) && filters$min_top1_count > 0 && "Top1Count" %in% names(filtered_lineups)) {
      filtered_lineups <- filtered_lineups[Top1Count >= filters$min_top1_count]
    }
    
    if (!is.null(filters$min_top2_count) && filters$min_top2_count > 0 && "Top2Count" %in% names(filtered_lineups)) {
      filtered_lineups <- filtered_lineups[Top2Count >= filters$min_top2_count]
    }
    
    if (!is.null(filters$min_top3_count) && filters$min_top3_count > 0 && "Top3Count" %in% names(filtered_lineups)) {
      filtered_lineups <- filtered_lineups[Top3Count >= filters$min_top3_count]
    }
    
    if (!is.null(filters$min_top5_count) && filters$min_top5_count > 0 && "Top5Count" %in% names(filtered_lineups)) {
      filtered_lineups <- filtered_lineups[Top5Count >= filters$min_top5_count]
    }
    
    # Apply ownership filters (if available)
    if (!is.null(filters$cumulative_ownership_range) && "CumulativeOwnership" %in% names(filtered_lineups)) {
      filtered_lineups <- filtered_lineups[
        CumulativeOwnership >= filters$cumulative_ownership_range[1] &
          CumulativeOwnership <= filters$cumulative_ownership_range[2]
      ]
    }
    
    if (!is.null(filters$geometric_mean_range) && "GeometricMeanOwnership" %in% names(filtered_lineups)) {
      filtered_lineups <- filtered_lineups[
        GeometricMeanOwnership >= filters$geometric_mean_range[1] &
          GeometricMeanOwnership <= filters$geometric_mean_range[2]
      ]
    }
    
    # Apply player exclusion filter
    if (!is.null(filters$excluded_players) && length(filters$excluded_players) > 0) {
      player_cols <- c("Captain", paste0("Player", 1:5))
      to_exclude <- rep(FALSE, nrow(filtered_lineups))
      
      for(col in player_cols) {
        if(col %in% names(filtered_lineups)) {
          to_exclude <- to_exclude | filtered_lineups[[col]] %in% filters$excluded_players
        }
      }
      
      filtered_lineups <- filtered_lineups[!to_exclude]
    }
    
    return(list(count = nrow(filtered_lineups)))
  }
  
  # Generate random lineups from filtered pool
  generate_random_lineups <- function(optimal_lineups, filters) {
    setDT(optimal_lineups)
    filtered_lineups <- copy(optimal_lineups)
    
    # Apply filters
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
    
    # Apply ownership filters (if available)
    if (!is.null(filters$cumulative_ownership_range) && "CumulativeOwnership" %in% names(filtered_lineups)) {
      filtered_lineups <- filtered_lineups[
        CumulativeOwnership >= filters$cumulative_ownership_range[1] &
          CumulativeOwnership <= filters$cumulative_ownership_range[2]
      ]
    }
    
    if (!is.null(filters$geometric_mean_range) && "GeometricMeanOwnership" %in% names(filtered_lineups)) {
      filtered_lineups <- filtered_lineups[
        GeometricMeanOwnership >= filters$geometric_mean_range[1] &
          GeometricMeanOwnership <= filters$geometric_mean_range[2]
      ]
    }
    
    # Apply player exclusion filter
    if (!is.null(filters$excluded_players) && length(filters$excluded_players) > 0) {
      player_cols <- c("Captain", paste0("Player", 1:5))
      to_exclude <- rep(FALSE, nrow(filtered_lineups))
      
      for(col in player_cols) {
        if(col %in% names(filtered_lineups)) {
          to_exclude <- to_exclude | filtered_lineups[[col]] %in% filters$excluded_players
        }
      }
      
      filtered_lineups <- filtered_lineups[!to_exclude]
    }
    
    # Check if any lineups match filters
    if (nrow(filtered_lineups) == 0) {
      return(NULL)
    }
    
    # Sample lineups using Top1Count as weight
    weight_col <- "Top1Count"
    selected_lineups <- data.table()
    selected_indices <- integer(0)
    
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
      
      selected_indices <- c(selected_indices, selected_idx)
      selected_lineups <- rbind(selected_lineups, filtered_lineups[selected_idx])
    }
    
    if (nrow(selected_lineups) == 0) {
      return(NULL)
    }
    
    # Add lineup number
    selected_lineups[, LineupNum := 1:.N]
    
    return(as.data.frame(selected_lineups))
  }
  
  # Calculate player exposure from generated lineups
  calculate_player_exposure <- function(optimal_lineups, player_mapping, generated_lineups) {
    if(is.null(generated_lineups) || nrow(generated_lineups) == 0) {
      return(NULL)
    }
    
    setDT(generated_lineups)
    
    # Get all unique players
    all_players <- unique(c(
      generated_lineups$Captain,
      unlist(generated_lineups[, paste0("Player", 1:5), with = FALSE])
    ))
    
    all_players <- all_players[!is.na(all_players)]
    
    # Calculate exposure for each player
    exposure_data <- lapply(all_players, function(player) {
      # Count captain appearances
      captain_count <- sum(generated_lineups$Captain == player, na.rm = TRUE)
      
      # Count flex appearances
      flex_count <- 0
      for(i in 1:5) {
        col_name <- paste0("Player", i)
        if(col_name %in% names(generated_lineups)) {
          flex_count <- flex_count + sum(generated_lineups[[col_name]] == player, na.rm = TRUE)
        }
      }
      
      total_appearances <- captain_count + flex_count
      exposure_pct <- (total_appearances / nrow(generated_lineups)) * 100
      captain_pct <- (captain_count / nrow(generated_lineups)) * 100
      
      # Get salary and projection from mapping
      salary <- NA
      proj <- NA
      
      if(!is.null(player_mapping)) {
        match_idx <- which(player_mapping$Name == player)
        if(length(match_idx) > 0) {
          salary <- player_mapping$Salary[match_idx[1]]
          if("Proj" %in% names(player_mapping)) {
            proj <- player_mapping$Proj[match_idx[1]]
          }
        }
      }
      
      data.frame(
        Player = player,
        Salary = salary,
        Proj = proj,
        Exposure = round(exposure_pct, 1),
        CaptainPct = round(captain_pct, 1),
        TotalAppearances = total_appearances,
        CaptainAppearances = captain_count,
        stringsAsFactors = FALSE
      )
    })
    
    exposure_df <- rbindlist(exposure_data)
    setorder(exposure_df, -Exposure)
    
    return(as.data.frame(exposure_df))
  }
  
  
}

# UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "CFB Showdown Simulator"),
  
  dashboardSidebar(
    useShinyjs(),
    div(
      style = "text-align: center; padding: 10px; margin-bottom: 5px;",
      tags$img(src = "logo.jpg", height = "200px", width = "auto", 
               style = "border: 2px solid #FFD700; border-radius: 10px;")
    ),
    
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("Team Stats", tabName = "team_stats", icon = icon("users")),
      menuItem("Optimal Lineups", tabName = "optimal", icon = icon("trophy")),
      menuItem("Lineup Builder", tabName = "builder", icon = icon("clipboard-list"))
    ),
    
    hr(style = "border-color: #FFD700;"),
    
    fileInput("excel_file", "Upload SIM_INPUT Excel File",
              accept = c(".xlsx")),
    
    conditionalPanel(
      condition = "output.file_uploaded",
      numericInput("n_sims", "Number of Simulations:",
                   value = 10000, min = 100, max = 100000, step = 1000),
      
      actionButton("run_sim", "Run Simulations",
                   class = "btn-primary btn-lg btn-block",
                   icon = icon("play"),
                   style = "margin: 10px 0;")
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    
    tabItems(
      # UPLOAD TAB
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            width = 12,
            title = "File Information",
            status = "info",
            solidHeader = TRUE,
            uiOutput("file_info")
          )
        )
      ),
      
      # RESULTS TAB
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            width = 12,
            title = "Player Projections",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("player_projections") %>% withSpinner(color = "#FFD700"),
            br(),
            downloadButton("download_projections", "Download Projections", class = "btn-primary"),
            downloadButton("download_full_sims", "Download Full Sim Results", class = "btn-primary")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = uiOutput("team1_violin_title"),
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("team1_violin", height = "600px") %>% withSpinner(color = "#FFD700")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = uiOutput("team2_violin_title"),
            status = "info",
            solidHeader = TRUE,
            plotlyOutput("team2_violin", height = "600px") %>% withSpinner(color = "#FFD700")
          )
        )
      ),
      
      # TEAM STATS TAB
      tabItem(
        tabName = "team_stats",
        fluidRow(
          box(
            width = 12,
            title = "Team Passing Yards Distribution",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("team_pass_plot", height = "400px") %>% withSpinner(color = "#FFD700")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Team Rushing Yards Distribution",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("team_rush_plot", height = "400px") %>% withSpinner(color = "#FFD700")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Team Touchdowns Distribution",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("team_tds_plot", height = "400px") %>% withSpinner(color = "#FFD700")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Team Field Goals Distribution",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput("team_fgs_plot", height = "400px") %>% withSpinner(color = "#FFD700")
          )
        )
      ),
      
      # OPTIMAL LINEUPS TAB
      tabItem(
        tabName = "optimal",
        fluidRow(
          box(
            width = 12,
            title = "Generate Optimal Lineups",
            status = "primary",
            solidHeader = TRUE,
            actionButton("generate_optimal", "Run Optimal Lineups",
                         class = "btn-primary btn-lg",
                         icon = icon("rocket"))
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Optimal DK Showdown Lineups",
            status = "success",
            solidHeader = TRUE,
            DTOutput("optimal_lineups_table") %>% withSpinner(color = "#FFD700"),
            br(),
            downloadButton("download_optimal", "Download Optimal Lineups", class = "btn-primary")
          )
        )
      ),
      
      # LINEUP BUILDER TAB
      tabItem(
        tabName = "builder",
        
        # Check if optimal lineups exist
        conditionalPanel(
          condition = "!output.has_optimal_lineups",
          box(
            width = 12,
            status = "warning",
            title = "No Optimal Lineups Available",
            "Please calculate optimal lineups in the Optimal Lineups tab first."
          )
        ),
        
        # Show lineup builder when lineups exist
        conditionalPanel(
          condition = "output.has_optimal_lineups",
          
          # Filters Box
          fluidRow(
            box(
              width = 12,
              title = "Lineup Filters",
              status = "primary",
              solidHeader = TRUE,
              
              fluidRow(
                column(3, numericInput("min_top1_count", "Min Top 1 Count:", value = 0, min = 0)),
                column(3, numericInput("min_top2_count", "Min Top 2 Count:", value = 0, min = 0)),
                column(3, numericInput("min_top3_count", "Min Top 3 Count:", value = 0, min = 0)),
                column(3, numericInput("min_top5_count", "Min Top 5 Count:", value = 0, min = 0))
              ),
              
              fluidRow(
                column(6, sliderInput("cumulative_ownership_range", "Cumulative Ownership Range:",
                                      min = 0, max = 600, value = c(0, 600), step = 10, post = "%")),
                column(6, sliderInput("geometric_mean_range", "Geometric Mean Ownership Range:",
                                      min = 0, max = 100, value = c(0, 100), step = 1, post = "%"))
              ),
              
              fluidRow(
                column(6, selectizeInput("excluded_players", "Exclude Players:", 
                                         choices = NULL, multiple = TRUE,
                                         options = list(plugins = list('remove_button'), 
                                                        placeholder = 'Click to select players to exclude'))),
                column(6, numericInput("num_random_lineups", "Number of Lineups to Generate:", 
                                       value = 20, min = 1, max = 150))
              ),
              
              fluidRow(
                column(6, div(class = "well well-sm", 
                              h4("Filtered Pool Statistics:", style = "color: #FFD700;"), 
                              textOutput("filtered_pool_size"))),
                column(6, div(style = "margin-top: 20px;",
                              actionButton("generate_lineups", "Randomize Lineups", 
                                           class = "btn-primary btn-lg", style = "width: 100%;"),
                              br(), br(),
                              downloadButton("download_random_lineups", "Download Selected Lineups", 
                                             style = "width: 100%;")))
              )
            )
          ),
          
          # Player Exposure Analysis
          fluidRow(
            box(
              width = 12, 
              title = "Player Exposure Analysis",
              status = "info",
              solidHeader = TRUE,
              DTOutput("player_exposure_table") %>% withSpinner(color = "#FFD700")
            )
          ),
          
          # Generated Lineups
          fluidRow(
            box(
              width = 12, 
              title = "Generated Lineups",
              status = "success",
              solidHeader = TRUE,
              DTOutput("random_lineups_table") %>% withSpinner(color = "#FFD700")
            )
          )
        )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    input_data = NULL,
    simulation_results = NULL,
    optimal_lineups = NULL,
    random_lineups = NULL,
    player_exposure = NULL,
    file_uploaded = FALSE,
    simulation_complete = FALSE
  )
  
  # File upload handler - LOAD FILE ONLY
  observeEvent(input$excel_file, {
    req(input$excel_file)
    
    withProgress(message = 'Reading file...', value = 0, {
      tryCatch({
        setProgress(0.5, detail = "Loading Excel file...")
        rv$input_data <- read_input_file(input$excel_file$datapath)
        rv$file_uploaded <- TRUE
        rv$simulation_complete <- FALSE
        rv$simulation_results <- NULL
        rv$optimal_lineups <- NULL
        
        setProgress(1, detail = "Complete!")
        
        showModal(modalDialog(
          title = "Success!",
          paste0("File loaded successfully!\n",
                 paste(rv$input_data$team_names, collapse = " vs "),
                 "\n\nClick 'Run Simulations' to begin."),
          easyClose = TRUE
        ))
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Error:", e$message),
          easyClose = TRUE
        ))
      })
    })
  })
  
  # Output for conditional panel
  output$file_uploaded <- reactive({
    rv$file_uploaded
  })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  # Run simulations button
  observeEvent(input$run_sim, {
    req(rv$input_data)
    
    withProgress(message = 'Running simulations...', value = 0, {
      tryCatch({
        setProgress(0.1, detail = "Starting simulations...")
        
        rv$simulation_results <- run_simulations(rv$input_data, input$n_sims)
        rv$simulation_complete <- TRUE
        
        setProgress(1, detail = "Complete!")
        
        showModal(modalDialog(
          title = "Success!",
          paste0(format(input$n_sims, big.mark = ","), 
                 " simulations completed! Check the Results tab."),
          easyClose = TRUE
        ))
        
        updateTabItems(session, "sidebar_menu", selected = "results")
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Error running simulations:", e$message),
          easyClose = TRUE
        ))
      })
    })
  })
  
  # File info display
  output$file_info <- renderUI({
    req(rv$file_uploaded)
    
    tagList(
      h4(paste("Teams:", paste(rv$input_data$team_names, collapse = " vs "))),
      p(paste("Players:", nrow(rv$input_data$dk_salaries))),
      p(paste("Similar Games:", nrow(rv$input_data$similar_games))),
      if (rv$simulation_complete) {
        p(strong(paste("Simulations Complete:", format(input$n_sims, big.mark = ","))))
      }
    )
  })
  
  # Player projections table
  output$player_projections <- renderDT({
    req(rv$simulation_complete)
    
    dk_data <- rv$input_data$dk_salaries
    projections <- analyze_fantasy_scoring(rv$simulation_results, dk_data)
    
    # Join with ETR and Saber projections if available
    if (!is.null(dk_data)) {
      projections <- projections %>%
        left_join(dk_data %>% select(Name, ETR_DK_Pts, Saber_Proj), 
                  by = c("Player" = "Name"))
      
      # Reorder columns
      projections <- projections %>%
        select(Player, TeamAbbr, Salary, AvgPts, ETR_DK_Pts, Saber_Proj, MedianPts, MaxPts)
    } else {
      projections <- projections %>%
        select(Player, TeamAbbr, Salary, AvgPts, MedianPts, MaxPts)
    }
    
    datatable(
      projections,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(3, 'desc')),  # Sort by AvgPts
        columnDefs = list(
          list(className = 'dt-center', targets = 2:ncol(projections)-1)
        )
      ),
      rownames = FALSE,
      caption = "Simulated vs ETR vs Saber Projections",
      colnames = c('Player', 'Team', 'Salary', 'Avg Pts', 'ETR Proj', 'Saber Proj', 'Median Pts', 'Max Pts')
    ) %>%
      formatCurrency('Salary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(c('AvgPts', 'ETR_DK_Pts', 'Saber_Proj', 'MedianPts', 'MaxPts'), 2) %>%
      formatStyle(
        'AvgPts',
        background = styleColorBar(range(projections$AvgPts, na.rm = TRUE), '#FFD700'),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Team 1 violin plot title
  output$team1_violin_title <- renderUI({
    req(rv$simulation_complete)
    teams <- unique(rv$simulation_results$Team)
    if (length(teams) > 0) {
      h4(paste(teams[1], "- Fantasy Points Distribution"), style = "color: #FFD700;")
    }
  })
  
  # Team 2 violin plot title
  output$team2_violin_title <- renderUI({
    req(rv$simulation_complete)
    teams <- unique(rv$simulation_results$Team)
    if (length(teams) > 1) {
      h4(paste(teams[2], "- Fantasy Points Distribution"), style = "color: #FFD700;")
    }
  })
  
  # Team 1 violin plot
  output$team1_violin <- renderPlotly({
    req(rv$simulation_complete)
    
    plot_data <- rv$simulation_results
    setDT(plot_data)
    
    teams <- unique(plot_data$Team)
    if (length(teams) == 0) return(NULL)
    
    team_name <- teams[1]
    team_color <- get_team_color(team_name)
    team_data <- plot_data[Team == team_name]
    
    # Get top 10 players by average points
    top_players <- team_data[, .(AvgPts = mean(TotalPts)), by = Player][
      order(-AvgPts)
    ][1:min(10, .N)]
    
    team_data <- team_data[Player %in% top_players$Player]
    
    # Order players by median
    player_order <- team_data[, .(MedianPts = median(TotalPts)), by = Player][
      order(MedianPts)
    ]$Player
    
    p <- plot_ly(team_data, 
                 y = ~factor(Player, levels = player_order),
                 x = ~TotalPts,
                 type = "violin",
                 box = list(visible = TRUE),
                 meanline = list(visible = TRUE),
                 fillcolor = paste0(team_color, "80"),
                 line = list(color = team_color, width = 2),
                 showlegend = FALSE) %>%
      layout(
        xaxis = list(title = "Fantasy Points", gridcolor = "#333333", 
                     titlefont = list(color = "#FFD700", size = 14)),
        yaxis = list(title = "", gridcolor = "#333333",
                     tickfont = list(color = "#FFD700", size = 12)),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        font = list(color = "#FFD700", size = 12),
        margin = list(l = 150, r = 20, t = 20, b = 60)
      )
    
    p
  })
  
  # Team 2 violin plot
  output$team2_violin <- renderPlotly({
    req(rv$simulation_complete)
    
    plot_data <- rv$simulation_results
    setDT(plot_data)
    
    teams <- unique(plot_data$Team)
    if (length(teams) < 2) return(NULL)
    
    team_name <- teams[2]
    team_color <- get_team_color(team_name)
    team_data <- plot_data[Team == team_name]
    
    # Get top 10 players by average points
    top_players <- team_data[, .(AvgPts = mean(TotalPts)), by = Player][
      order(-AvgPts)
    ][1:min(10, .N)]
    
    team_data <- team_data[Player %in% top_players$Player]
    
    # Order players by median
    player_order <- team_data[, .(MedianPts = median(TotalPts)), by = Player][
      order(MedianPts)
    ]$Player
    
    p <- plot_ly(team_data, 
                 y = ~factor(Player, levels = player_order),
                 x = ~TotalPts,
                 type = "violin",
                 box = list(visible = TRUE),
                 meanline = list(visible = TRUE),
                 fillcolor = paste0(team_color, "80"),
                 line = list(color = team_color, width = 2),
                 showlegend = FALSE) %>%
      layout(
        xaxis = list(title = "Fantasy Points", gridcolor = "#333333",
                     titlefont = list(color = "#FFD700", size = 14)),
        yaxis = list(title = "", gridcolor = "#333333",
                     tickfont = list(color = "#FFD700", size = 12)),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        font = list(color = "#FFD700", size = 12),
        margin = list(l = 150, r = 20, t = 20, b = 60)
      )
    
    p
  })
  
  # Team stats
  team_stats_data <- reactive({
    req(rv$simulation_complete)
    analyze_team_stats(rv$simulation_results)
  })
  
  # Team passing plot
  output$team_pass_plot <- renderPlotly({
    req(rv$simulation_complete)
    
    team_data <- team_stats_data()
    setDT(team_data)
    
    teams <- unique(team_data$Team)
    team_color_vals <- sapply(teams, get_team_color)
    names(team_color_vals) <- teams
    
    p <- ggplot(team_data, aes(x = TotalPassYds, fill = Team)) +
      geom_density(alpha = 0.7, size = 1.2) +
      labs(title = "Team Total Passing Yards Per Game",
           x = "Total Passing Yards", y = "Density") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#222222"),
        panel.background = element_rect(fill = "#222222"),
        text = element_text(color = "#FFD700", size = 13),
        axis.text = element_text(color = "#FFD700", size = 12),
        axis.title = element_text(size = 14),
        panel.grid = element_line(color = "#333333"),
        legend.background = element_rect(fill = "#222222"),
        legend.text = element_text(color = "#FFD700", size = 12),
        legend.title = element_text(color = "#FFD700", size = 13)
      ) +
      scale_fill_manual(values = team_color_vals)
    
    ggplotly(p) %>%
      layout(paper_bgcolor = "#222222", plot_bgcolor = "#222222",
             font = list(color = "#FFD700"))
  })
  
  # Team rushing plot
  output$team_rush_plot <- renderPlotly({
    req(rv$simulation_complete)
    
    team_data <- team_stats_data()
    setDT(team_data)
    
    teams <- unique(team_data$Team)
    team_color_vals <- sapply(teams, get_team_color)
    names(team_color_vals) <- teams
    
    p <- ggplot(team_data, aes(x = TotalRushYds, fill = Team)) +
      geom_density(alpha = 0.7, size = 1.2) +
      labs(title = "Team Total Rushing Yards Per Game",
           x = "Total Rushing Yards", y = "Density") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#222222"),
        panel.background = element_rect(fill = "#222222"),
        text = element_text(color = "#FFD700", size = 13),
        axis.text = element_text(color = "#FFD700", size = 12),
        axis.title = element_text(size = 14),
        panel.grid = element_line(color = "#333333"),
        legend.background = element_rect(fill = "#222222"),
        legend.text = element_text(color = "#FFD700", size = 12),
        legend.title = element_text(color = "#FFD700", size = 13)
      ) +
      scale_fill_manual(values = team_color_vals)
    
    ggplotly(p) %>%
      layout(paper_bgcolor = "#222222", plot_bgcolor = "#222222",
             font = list(color = "#FFD700"))
  })
  
  # Team TDs plot - Distribution of TOTAL TDs (no double counting)
  output$team_tds_plot <- renderPlotly({
    req(rv$simulation_complete)
    
    team_data <- team_stats_data()
    setDT(team_data)
    
    teams <- unique(team_data$Team)
    team_color_vals <- sapply(teams, get_team_color)
    names(team_color_vals) <- teams
    
    p <- ggplot(team_data, aes(x = TotalTDs, fill = Team)) +
      geom_density(alpha = 0.7, size = 1.2) +
      labs(title = "Team Total Touchdowns Per Game",
           x = "Total Touchdowns", y = "Density") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#222222"),
        panel.background = element_rect(fill = "#222222"),
        text = element_text(color = "#FFD700", size = 13),
        axis.text = element_text(color = "#FFD700", size = 12),
        axis.title = element_text(size = 14),
        panel.grid = element_line(color = "#333333"),
        legend.background = element_rect(fill = "#222222"),
        legend.text = element_text(color = "#FFD700", size = 12),
        legend.title = element_text(color = "#FFD700", size = 13)
      ) +
      scale_fill_manual(values = team_color_vals)
    
    ggplotly(p) %>%
      layout(paper_bgcolor = "#222222", plot_bgcolor = "#222222",
             font = list(color = "#FFD700"))
  })
  
  # Team FGs plot
  output$team_fgs_plot <- renderPlotly({
    req(rv$simulation_complete)
    
    team_data <- team_stats_data()
    setDT(team_data)
    
    teams <- unique(team_data$Team)
    team_color_vals <- sapply(teams, get_team_color)
    names(team_color_vals) <- teams
    
    p <- ggplot(team_data, aes(x = TotalFGs, fill = Team)) +
      geom_density(alpha = 0.7, size = 1.2) +
      labs(title = "Team Total Field Goals Per Game",
           x = "Field Goals Made", y = "Density") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#222222"),
        panel.background = element_rect(fill = "#222222"),
        text = element_text(color = "#FFD700", size = 13),
        axis.text = element_text(color = "#FFD700", size = 12),
        axis.title = element_text(size = 14),
        panel.grid = element_line(color = "#333333"),
        legend.background = element_rect(fill = "#222222"),
        legend.text = element_text(color = "#FFD700", size = 12),
        legend.title = element_text(color = "#FFD700", size = 13)
      ) +
      scale_fill_manual(values = team_color_vals)
    
    ggplotly(p) %>%
      layout(paper_bgcolor = "#222222", plot_bgcolor = "#222222",
             font = list(color = "#FFD700"))
  })
  
  
  # Download projections
  output$download_projections <- downloadHandler(
    filename = function() {
      paste0("cfb_projections_", Sys.Date(), ".csv")
    },
    content = function(file) {
      projections <- analyze_fantasy_scoring(rv$simulation_results)
      write.csv(projections, file, row.names = FALSE)
    }
  )
  
  # Download full sim results
  output$download_full_sims <- downloadHandler(
    filename = function() {
      paste0("cfb_full_sims_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$simulation_results, file, row.names = FALSE)
    }
  )
  
  # Generate optimal lineups
  observeEvent(input$generate_optimal, {
    req(rv$simulation_complete)
    
    withProgress(message = 'Generating optimal lineups...', value = 0, {
      tryCatch({
        setProgress(0.3, detail = "Finding top lineups...")
        
        optimal_lineups <- generate_showdown_lineups(
          sim_results = rv$simulation_results,
          dk_salaries = rv$input_data$dk_salaries,
          n_sims = input$n_sims,
          top_k = 5
        )
        
        rv$optimal_lineups <- optimal_lineups
        
        setProgress(1, detail = "Complete!")
        
        showModal(modalDialog(
          title = "Success",
          paste("Generated", nrow(optimal_lineups), "optimal lineups!"),
          easyClose = TRUE
        ))
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Error generating lineups:", e$message),
          easyClose = TRUE
        ))
      })
    })
  })
  
  # Optimal lineups table
  output$optimal_lineups_table <- renderDT({
    req(rv$optimal_lineups)
    
    datatable(
      rv$optimal_lineups,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        order = list(list(6, 'desc')),  # Sort by Top1Count
        columnDefs = list(
          list(className = 'dt-center', targets = 6:ncol(rv$optimal_lineups)-1)
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe compact'
    ) %>%
      formatCurrency('TotalSalary', '$', digits = 0) %>%
      formatPercentage(if('AvgOwn' %in% names(rv$optimal_lineups)) 'AvgOwn' else NULL, 1) %>%
      formatStyle(
        'Top1Count',
        background = styleColorBar(range(rv$optimal_lineups$Top1Count, na.rm = TRUE), '#FFD700'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        columns = c('Top1Count', 'Top2Count', 'Top3Count', 'Top5Count'),
        fontWeight = 'bold'
      )
  })
  
  # Download optimal lineups
  output$download_optimal <- downloadHandler(
    filename = function() {
      paste0("cfb_optimal_lineups_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$optimal_lineups, file, row.names = FALSE)
    }
  )
  
  # Lineup builder - populate selects
  observe({
    req(rv$simulation_complete)
    
    players <- unique(rv$simulation_results$Player)
    player_choices <- setNames(players, players)
    player_choices <- c("None" = "", player_choices)
    
    updateSelectInput(session, "cpt_select", choices = player_choices)
    updateSelectInput(session, "flex1_select", choices = player_choices)
    updateSelectInput(session, "flex2_select", choices = player_choices)
    updateSelectInput(session, "flex3_select", choices = player_choices)
    updateSelectInput(session, "flex4_select", choices = player_choices)
    updateSelectInput(session, "flex5_select", choices = player_choices)
  })
  
  # Calculate lineup stats
  lineup_stats <- eventReactive(input$calculate_lineup, {
    req(input$cpt_select != "")
    
    selected_players <- c(
      input$cpt_select,
      input$flex1_select,
      input$flex2_select,
      input$flex3_select,
      input$flex4_select,
      input$flex5_select
    )
    
    selected_players <- selected_players[selected_players != ""]
    
    if (length(selected_players) < 2) {
      return(list(valid = FALSE, message = "Select at least a Captain and 1 FLEX player"))
    }
    
    if (length(unique(selected_players)) != length(selected_players)) {
      return(list(valid = FALSE, message = "Cannot select the same player multiple times"))
    }
    
    # Get salary info
    dk_salaries <- rv$input_data$dk_salaries
    setDT(dk_salaries)
    
    cpt_salary <- dk_salaries[Name == input$cpt_select, Salary] * 1.5
    flex_salaries <- sapply(selected_players[-1], function(p) {
      dk_salaries[Name == p, Salary]
    })
    
    total_salary <- cpt_salary + sum(flex_salaries, na.rm = TRUE)
    
    # Get simulation results for these players
    setDT(rv$simulation_results)
    
    lineup_sims <- rv$simulation_results[Player %in% selected_players]
    
    sim_scores <- lineup_sims[, .(
      LineupScore = sum(ifelse(Player == input$cpt_select, TotalPts * 1.5, TotalPts))
    ), by = SimID]
    
    list(
      valid = TRUE,
      captain = input$cpt_select,
      flex = selected_players[-1],
      total_salary = total_salary,
      avg_score = mean(sim_scores$LineupScore),
      median_score = median(sim_scores$LineupScore),
      max_score = max(sim_scores$LineupScore),
      sim_scores = sim_scores
    )
  })
  
  # Lineup summary
  output$lineup_summary <- renderUI({
    stats <- lineup_stats()
    
    if (!stats$valid) {
      return(h4(stats$message, style = "color: #FFD700;"))
    }
    
    salary_color <- if (stats$total_salary > DK_SALARY_CAP) "red" else "#FFD700"
    
    tagList(
      h4(paste("Captain:", stats$captain), style = "color: #FFD700;"),
      h5(paste("FLEX:", paste(stats$flex, collapse = ", ")), style = "color: #FFD700;"),
      hr(),
      h4(paste("Total Salary: $", format(stats$total_salary, big.mark = ",")), 
         style = paste0("color: ", salary_color, ";")),
      h5(paste("Average Score:", round(stats$avg_score, 2)), style = "color: #FFD700;"),
      h5(paste("Median Score:", round(stats$median_score, 2)), style = "color: #FFD700;"),
      h5(paste("Max Score:", round(stats$max_score, 2)), style = "color: #FFD700;")
    )
  })
  
  # Lineup distribution plot
  output$lineup_dist_plot <- renderPlotly({
    stats <- lineup_stats()
    req(stats$valid)
    
    p <- ggplot(stats$sim_scores, aes(x = LineupScore)) +
      geom_histogram(bins = 50, fill = "#FFD700", alpha = 0.7) +
      geom_vline(xintercept = stats$avg_score, color = "red", linetype = "dashed", size = 1) +
      labs(title = "Lineup Score Distribution",
           x = "Total Fantasy Points",
           y = "Frequency") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#222222"),
        panel.background = element_rect(fill = "#222222"),
        text = element_text(color = "#FFD700"),
        axis.text = element_text(color = "#FFD700"),
        panel.grid = element_line(color = "#333333")
      )
    
    ggplotly(p) %>%
      layout(paper_bgcolor = "#222222", plot_bgcolor = "#222222",
             font = list(color = "#FFD700"))
  })
  
  
  # ===== LINEUP BUILDER LOGIC =====
  
  # Output to check if optimal lineups exist
  output$has_optimal_lineups <- reactive({
    !is.null(rv$optimal_lineups) && nrow(rv$optimal_lineups) > 0
  })
  outputOptions(output, "has_optimal_lineups", suspendWhenHidden = FALSE)
  
  # Update player exclusion choices when optimal lineups are calculated
  observeEvent(rv$optimal_lineups, {
    if(!is.null(rv$optimal_lineups)) {
      all_players <- unique(c(
        rv$optimal_lineups$Captain,
        unlist(rv$optimal_lineups[, paste0("Player", 1:5)])
      ))
      all_players <- sort(all_players[!is.na(all_players)])
      
      updateSelectizeInput(session, "excluded_players", 
                           choices = all_players, 
                           server = TRUE)
    }
  })
  
  # Real-time filtered pool size calculation
  observeEvent(c(input$min_top1_count, input$min_top2_count, input$min_top3_count,
                 input$min_top5_count, input$cumulative_ownership_range,
                 input$geometric_mean_range, input$excluded_players), {
                   if(!is.null(rv$optimal_lineups)) {
                     filters <- list(
                       min_top1_count = if(!is.null(input$min_top1_count)) input$min_top1_count else 0,
                       min_top2_count = if(!is.null(input$min_top2_count)) input$min_top2_count else 0,
                       min_top3_count = if(!is.null(input$min_top3_count)) input$min_top3_count else 0,
                       min_top5_count = if(!is.null(input$min_top5_count)) input$min_top5_count else 0,
                       cumulative_ownership_range = input$cumulative_ownership_range,
                       geometric_mean_range = input$geometric_mean_range,
                       excluded_players = if(!is.null(input$excluded_players)) input$excluded_players else character(0)
                     )
                     
                     pool_stats <- calculate_filtered_pool_stats(rv$optimal_lineups, filters)
                     
                     output$filtered_pool_size <- renderText({
                       paste0(format(pool_stats$count, big.mark = ","), 
                              " lineups match current filters")
                     })
                   }
                 })
  
  # Generate random lineups button
  observeEvent(input$generate_lineups, {
    req(rv$optimal_lineups)
    
    filters <- list(
      min_top1_count = input$min_top1_count,
      min_top2_count = input$min_top2_count,
      min_top3_count = input$min_top3_count,
      min_top5_count = input$min_top5_count,
      cumulative_ownership_range = input$cumulative_ownership_range,
      geometric_mean_range = input$geometric_mean_range,
      num_lineups = input$num_random_lineups,
      excluded_players = input$excluded_players
    )
    
    withProgress(message = 'Generating lineups...', value = 0, {
      rv$random_lineups <- generate_random_lineups(rv$optimal_lineups, filters)
      
      if(!is.null(rv$random_lineups)) {
        # Create player mapping for exposure calculation
        dk_data <- rv$input_data$dk_salaries
        
        player_mapping <- data.frame(
          Name = dk_data$Name,
          Salary = dk_data$Salary,
          Proj = if("ETR_DK_Pts" %in% names(dk_data)) dk_data$ETR_DK_Pts else NA,
          stringsAsFactors = FALSE
        )
        
        rv$player_exposure <- calculate_player_exposure(
          rv$optimal_lineups,
          player_mapping,
          rv$random_lineups
        )
        
        showModal(modalDialog(
          title = "Success",
          sprintf("Generated %d lineups successfully!", nrow(rv$random_lineups)),
          easyClose = TRUE
        ))
      } else {
        showModal(modalDialog(
          title = "No Lineups Generated",
          "No lineups matched the selected filters. Try adjusting your filter settings.",
          easyClose = TRUE
        ))
      }
    })
  })
  
  # Player exposure table
  output$player_exposure_table <- renderDT({
    req(rv$player_exposure)
    
    datatable(
      rv$player_exposure,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(3, 'desc')),  # Sort by Exposure
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      rownames = FALSE
    ) %>%
      formatCurrency('Salary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(c('Proj', 'Exposure', 'CaptainPct'), 1) %>%
      formatStyle(
        'Exposure',
        background = styleColorBar(range(rv$player_exposure$Exposure, na.rm = TRUE), '#FFD700'),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Random lineups table
  output$random_lineups_table <- renderDT({
    req(rv$random_lineups)
    
    datatable(
      rv$random_lineups,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(0, 'asc')),  # Sort by LineupNum
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      ),
      rownames = FALSE
    ) %>%
      formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(c('CumulativeOwnership', 'GeometricMeanOwnership'), 1)
  })
  
  # Download random lineups
  output$download_random_lineups <- downloadHandler(
    filename = function() {
      paste0("CFB_Showdown_Lineups_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(rv$random_lineups)
      
      # Format for DK upload
      upload_format <- rv$random_lineups %>%
        select(Captain, Player1, Player2, Player3, Player4, Player5)
      
      write.csv(upload_format, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)