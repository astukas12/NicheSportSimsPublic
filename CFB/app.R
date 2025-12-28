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

# Load CFB team colors automatically
tryCatch({
  source("cfb_team_colors.R")
  cat("✓ Team colors loaded successfully\n")
}, error = function(e) {
  cat("Warning: cfb_team_colors.R not found. Using default colors.\n")
  # Fallback function if file doesn't exist
  get_cfb_colors <- function(team_name, type = "primary") {
    return(if(type == "primary") "#003366" else "#999999")
  }
})

# Wrapper function for backwards compatibility with existing code
get_team_color <- function(team_name) {
  return(get_cfb_colors(team_name, "primary"))
}

# ===== DST SCORING FUNCTIONS (NFL MODE) =====
# Auto-detects NFL vs CFB based on defensive columns

# Check if defensive scoring is available
has_dst_scoring <- function(similar_games, team_names) {
  # Check if defensive columns exist (NFL format)
  def_cols_team1 <- paste0(team_names[1], c("_Def_Sacks", "_Def_Ints", "_Def_Fum", "_Def_Pts_Allow"))
  def_cols_team2 <- paste0(team_names[2], c("_Def_Sacks", "_Def_Ints", "_Def_Fum", "_Def_Pts_Allow"))
  
  all_def_cols <- c(def_cols_team1, def_cols_team2)
  all_exist <- all(all_def_cols %in% names(similar_games))
  
  return(all_exist)
}

# Calculate DraftKings NFL DST Points Allowed scoring
calculate_pts_allowed_score <- function(pts_allowed) {
  sapply(pts_allowed, function(pts) {
    if (is.na(pts)) return(0)
    if (pts == 0) return(10)
    if (pts <= 6) return(7)
    if (pts <= 13) return(4)
    if (pts <= 20) return(1)
    if (pts <= 27) return(0)
    if (pts <= 34) return(-1)
    return(-4)  # 35+
  })
}

# Efficient team color highlighting for tables (F1-style)
# Pre-calculates colors and applies in one pass
apply_team_colors_to_table <- function(dt, data, team_col) {
  if (!team_col %in% names(data)) return(dt)
  
  # Get unique teams and their colors
  unique_teams <- unique(data[[team_col]])
  team_colors <- sapply(unique_teams, get_team_color)
  
  # Create color mapping
  color_map <- setNames(team_colors, unique_teams)
  
  # Apply colors using styleEqual (much faster than row-by-row)
  dt <- dt %>%
    formatStyle(
      team_col,
      backgroundColor = styleEqual(unique_teams, paste0(team_colors, "20")),  # 20 = 12% opacity
      color = styleEqual(unique_teams, team_colors)
    )
  
  return(dt)
}

# Constants
DK_SALARY_CAP <- 50000
ROSTER_SIZE <- 6  # 1 CPT + 5 FLEX

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
    
    # Fix malformed column names (Excel sometimes merges adjacent columns)
    if ("CPT_OwnPos" %in% names(input_data$dk_salaries)) {
      cat("⚠ Fixing malformed column 'CPT_OwnPos' - splitting into separate columns\n")
      # This shouldn't happen with new generator, but handle old files
    }
    
    # Sim_Totals sheet is no longer needed/used
    input_data$similar_games <- read_excel(file_path, sheet = "Similar_Games")
    input_data$team_names <- team_names
    
    return(input_data)
  }, error = function(e) {
    stop(paste("Error reading Excel file:", e$message))
  })
}

# Simulation function
simulate_team_game <- function(sim_id, team_name, team_data, sampled_game, dk_salaries, use_dst = FALSE, opponent_ints = 0) {
  
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
      floor_pct <- rushing_data$Floor[i]
      p25_pct <- rushing_data$Pct_P25[i]
      p50_pct <- rushing_data$Pct_P50[i]
      p75_pct <- rushing_data$Pct_P75[i]
      ceiling_pct <- rushing_data$Ceiling[i]
      
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
    
    # IMPROVED SWEEP UP: Distribute remaining yards respecting ceiling constraints
    if (remaining_share > 0.01) {
      # Pass 1: Go through players from top, cap at 20% of remaining per player
      for (i in 1:n_rushers) {
        if (remaining_share <= 0.01) break
        
        ceiling_pct <- rushing_data$Ceiling[i]
        if (is.na(ceiling_pct)) ceiling_pct <- rushing_data$Pct_P75[i]
        if (is.na(ceiling_pct)) ceiling_pct <- 1.0
        
        current_share <- rush_yds_allocation[i] / team_rush_yds
        room_to_ceiling <- ceiling_pct - current_share
        
        if (room_to_ceiling > 0.01) {
          additional_share <- min(
            remaining_share,
            room_to_ceiling,
            remaining_share * 0.20  # Cap at 20% per iteration
          )
          
          additional_yards <- round(team_rush_yds * additional_share)
          rush_yds_allocation[i] <- rush_yds_allocation[i] + additional_yards
          remaining_share <- remaining_share - additional_share
        }
      }
      
      # Pass 2: If still remaining, go through again without 20% cap
      if (remaining_share > 0.01) {
        for (i in 1:n_rushers) {
          if (remaining_share <= 0.01) break
          
          ceiling_pct <- rushing_data$Ceiling[i]
          if (is.na(ceiling_pct)) ceiling_pct <- 1.0
          
          current_share <- rush_yds_allocation[i] / team_rush_yds
          room_to_ceiling <- ceiling_pct - current_share
          
          if (room_to_ceiling > 0.01) {
            additional_share <- min(remaining_share, room_to_ceiling)
            additional_yards <- round(team_rush_yds * additional_share)
            
            rush_yds_allocation[i] <- rush_yds_allocation[i] + additional_yards
            remaining_share <- remaining_share - additional_share
          }
        }
      }
      
      # Pass 3: Final cleanup - distribute evenly RESPECTING CEILINGS
      if (remaining_share > 0.01) {
        final_remaining_yards <- round(team_rush_yds * remaining_share)
        
        # Only give to players who still have room to ceiling
        for (yard in 1:final_remaining_yards) {
          # Find players with room to ceiling
          players_with_room <- numeric(0)
          room_amounts <- numeric(0)
          
          for (i in 1:n_rushers) {
            if (rush_yds_allocation[i] > 0) {  # Only active players
              ceiling_pct <- rushing_data$Ceiling[i]
              if (is.na(ceiling_pct)) ceiling_pct <- 1.0
              
              current_share <- rush_yds_allocation[i] / team_rush_yds
              room <- ceiling_pct - current_share
              
              if (room > 0.001) {  # Has room
                players_with_room <- c(players_with_room, i)
                room_amounts <- c(room_amounts, room)
              }
            }
          }
          
          # If no one has room, we're done (better to under-allocate than break ceilings)
          if (length(players_with_room) == 0) break
          
          # Give 1 yard to a random player with room (equal probability)
          selected <- sample(players_with_room, 1)
          rush_yds_allocation[selected] <- rush_yds_allocation[selected] + 1
        }
      }
    }
    
    # Second pass: Allocate rushing TDs with 70% input rate / 30% production + diminishing returns
    td_allocation <- rep(0, n_rushers)
    if (team_rush_tds > 0) {
      # Historical TD rates (70% weight)
      td_rates <- as.numeric(rushing_data$TD_Rate)
      td_rates[is.na(td_rates)] <- 0
      td_rates[td_rates < 0] <- 0
      
      # Production in this sim (30% weight)
      production_weight <- rush_yds_allocation / max(rush_yds_allocation, 1)
      
      # 70/30 split
      td_probs <- (td_rates * 0.7) + (production_weight * 0.3)
      
      # Only players with rush yards are eligible
      eligible <- which(rush_yds_allocation > 0)
      
      if (length(eligible) > 0 && sum(td_probs[eligible]) > 0) {
        td_probs_eligible <- td_probs[eligible]
        td_probs_eligible <- td_probs_eligible / sum(td_probs_eligible)
        
        # Allocate TDs with diminishing returns
        for (td in 1:team_rush_tds) {
          selected_idx <- sample(1:length(eligible), 1, prob = td_probs_eligible)
          selected <- eligible[selected_idx]
          
          td_allocation[selected] <- td_allocation[selected] + 1
          
          # Diminishing returns: Each TD reduces probability (0.6^n)
          reduction_factor <- 0.6 ^ td_allocation[selected]
          td_probs_eligible[selected_idx] <- td_probs_eligible[selected_idx] * reduction_factor
          
          if (sum(td_probs_eligible) > 0) {
            td_probs_eligible <- td_probs_eligible / sum(td_probs_eligible)
          } else {
            td_probs_eligible <- rep(1/length(eligible), length(eligible))
          }
        }
      }
    }
    
    # CONSTRAINT: Rush TD requires rush yards > 0
    for (i in 1:n_rushers) {
      if (td_allocation[i] > 0 && rush_yds_allocation[i] == 0) {
        rush_yds_allocation[i] <- 1
      }
    }
    
    # WORKLOAD CONSTRAINT: Track rushing outcomes for receiving cap
    player_rush_outcome <- list()
    
    for (i in 1:n_rushers) {
      if (rush_yds_allocation[i] > 0) {
        rush_share <- rush_yds_allocation[i] / max(team_rush_yds, 1)
        p75 <- rushing_data$Pct_P75[i]
        
        # If rushing outcome >= P75, mark as "hot"
        if (!is.na(p75) && p75 > 0 && rush_share >= p75) {
          player_rush_outcome[[rushing_data$Player[i]]] <- "hot"
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
      floor_pct <- receiving_data$Floor[i]
      p25_pct <- receiving_data$Pct_P25[i]
      p50_pct <- receiving_data$Pct_P50[i]
      p75_pct <- receiving_data$Pct_P75[i]
      ceiling_pct <- receiving_data$Ceiling[i]
      
      # Handle NAs
      if (is.na(floor_pct)) floor_pct <- 0
      if (is.na(p25_pct)) p25_pct <- floor_pct
      if (is.na(p50_pct)) p50_pct <- p25_pct
      if (is.na(p75_pct)) p75_pct <- p50_pct
      if (is.na(ceiling_pct)) ceiling_pct <- p75_pct
      
      # WORKLOAD CONSTRAINT: If player had hot rushing game, cap receiving at P50
      player_name <- receiving_data$Player[i]
      if (player_name %in% names(player_rush_outcome)) {
        if (player_rush_outcome[[player_name]] == "hot") {
          ceiling_pct <- min(ceiling_pct, p50_pct)
        }
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
    
    # IMPROVED SWEEP UP: Distribute remaining yards respecting ceiling constraints
    if (remaining_share > 0.01) {
      # Pass 1: Go through players from top, cap at 20% of remaining per player
      for (i in 1:n_receivers) {
        if (remaining_share <= 0.01) break
        
        ceiling_pct <- receiving_data$Ceiling[i]
        if (is.na(ceiling_pct)) ceiling_pct <- receiving_data$Pct_P75[i]
        if (is.na(ceiling_pct)) ceiling_pct <- 1.0
        
        current_share <- rec_yds_allocation[i] / team_pass_yds
        room_to_ceiling <- ceiling_pct - current_share
        
        if (room_to_ceiling > 0.01) {
          additional_share <- min(
            remaining_share,
            room_to_ceiling,
            remaining_share * 0.20  # Cap at 20% per iteration
          )
          
          additional_yards <- round(team_pass_yds * additional_share)
          rec_yds_allocation[i] <- rec_yds_allocation[i] + additional_yards
          remaining_share <- remaining_share - additional_share
        }
      }
      
      # Pass 2: If still remaining, go through again without 20% cap
      if (remaining_share > 0.01) {
        for (i in 1:n_receivers) {
          if (remaining_share <= 0.01) break
          
          ceiling_pct <- receiving_data$Ceiling[i]
          if (is.na(ceiling_pct)) ceiling_pct <- 1.0
          
          current_share <- rec_yds_allocation[i] / team_pass_yds
          room_to_ceiling <- ceiling_pct - current_share
          
          if (room_to_ceiling > 0.01) {
            additional_share <- min(remaining_share, room_to_ceiling)
            additional_yards <- round(team_pass_yds * additional_share)
            
            rec_yds_allocation[i] <- rec_yds_allocation[i] + additional_yards
            remaining_share <- remaining_share - additional_share
          }
        }
      }
      
      # Pass 3: Final cleanup - distribute evenly RESPECTING CEILINGS
      if (remaining_share > 0.01) {
        final_remaining_yards <- round(team_pass_yds * remaining_share)
        
        # Only give to players who still have room to ceiling
        for (yard in 1:final_remaining_yards) {
          # Find players with room to ceiling
          players_with_room <- numeric(0)
          room_amounts <- numeric(0)
          
          for (i in 1:n_receivers) {
            if (rec_yds_allocation[i] > 0) {  # Only active players
              ceiling_pct <- receiving_data$Ceiling[i]
              if (is.na(ceiling_pct)) ceiling_pct <- 1.0
              
              current_share <- rec_yds_allocation[i] / team_pass_yds
              room <- ceiling_pct - current_share
              
              if (room > 0.001) {  # Has room
                players_with_room <- c(players_with_room, i)
                room_amounts <- c(room_amounts, room)
              }
            }
          }
          
          # If no one has room, we're done (better to under-allocate than break ceilings)
          if (length(players_with_room) == 0) break
          
          # Give 1 yard to a random player with room (equal probability)
          selected <- sample(players_with_room, 1)
          rec_yds_allocation[selected] <- rec_yds_allocation[selected] + 1
        }
      }
    }
    
    # DYNAMIC YPR ADJUSTMENT: Adjust YPR based on actual yards vs expected
    # High yardage games likely had explosive plays (higher YPR)
    adjusted_ypr_values <- ypr_values  # Start with historical
    
    for (i in 1:n_receivers) {
      if (rec_yds_allocation[i] > 0) {
        # Calculate expected yards for this player based on P50
        p50_pct <- receiving_data$Pct_P50[i]
        if (is.na(p50_pct) || p50_pct <= 0) p50_pct <- 0.01
        
        expected_yds <- team_pass_yds * p50_pct
        
        if (expected_yds > 0) {
          # Calculate how actual compares to expected
          yards_ratio <- rec_yds_allocation[i] / expected_yds
          
          # If significantly OVER expected (explosive game), increase YPR
          if (yards_ratio > 1.3) {
            # Scale factor: the more explosive, the higher YPR
            # 1.3x expected = 1.09x YPR
            # 2.0x expected = 1.21x YPR
            # 3.0x expected = 1.51x YPR
            ypr_multiplier <- 1 + (yards_ratio - 1) * 0.3
            
            # Cap at 2.0x historical YPR
            ypr_multiplier <- min(ypr_multiplier, 2.0)
            
            adjusted_ypr_values[i] <- ypr_values[i] * ypr_multiplier
            
          } else if (yards_ratio < 0.7) {
            # If UNDER expected (possession/short-catch role), decrease YPR slightly
            # 0.7x expected = 0.97x YPR
            # 0.5x expected = 0.95x YPR
            ypr_multiplier <- 0.9 + (yards_ratio * 0.1)
            ypr_multiplier <- max(ypr_multiplier, 0.7)  # Floor at 70%
            
            adjusted_ypr_values[i] <- ypr_values[i] * ypr_multiplier
          }
          # If yards_ratio between 0.7-1.3, no adjustment (normal game)
        }
      }
    }
    
    # Second pass: Allocate receptions based on ADJUSTED YPR profile
    expected_recs <- rec_yds_allocation / adjusted_ypr_values
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
      
      # CONSTRAINT: Everyone with yards must have at least 1 catch
      players_with_yards <- which(rec_yds_allocation > 0)
      for (i in players_with_yards) {
        if (rec_allocation[i] == 0) {
          rec_allocation[i] <- 1
          # Take 1 from player with most (who has >1)
          eligible_to_reduce <- which(rec_allocation > 1)
          if (length(eligible_to_reduce) > 0) {
            max_idx <- eligible_to_reduce[which.max(rec_allocation[eligible_to_reduce])]
            rec_allocation[max_idx] <- rec_allocation[max_idx] - 1
          }
        }
      }
    } else {
      rec_allocation <- rep(0, n_receivers)
    }
    
    # Third pass: Allocate receiving TDs with 70% input rate / 30% production + diminishing returns
    td_allocation <- rep(0, n_receivers)
    if (team_pass_tds > 0) {
      # Historical TD rates (70% weight)
      td_rates <- as.numeric(receiving_data$TD_Rate)
      td_rates[is.na(td_rates)] <- 0
      td_rates[td_rates < 0] <- 0
      
      # Production in this sim (30% weight)
      # Combine yards and receptions
      production_weight <- (rec_yds_allocation / max(rec_yds_allocation, 1)) * 0.5 + 
        (rec_allocation / max(rec_allocation, 1)) * 0.5
      
      # 70/30 split
      td_probs <- (td_rates * 0.7) + (production_weight * 0.3)
      
      # Only players who caught passes are eligible
      eligible <- which(rec_allocation > 0)
      
      if (length(eligible) > 0 && sum(td_probs[eligible]) > 0) {
        td_probs_eligible <- td_probs[eligible]
        td_probs_eligible <- td_probs_eligible / sum(td_probs_eligible)
        
        # Allocate TDs with diminishing returns
        for (td in 1:team_pass_tds) {
          selected_idx <- sample(1:length(eligible), 1, prob = td_probs_eligible)
          selected <- eligible[selected_idx]
          
          td_allocation[selected] <- td_allocation[selected] + 1
          
          # Diminishing returns: Each TD reduces probability (0.6^n)
          reduction_factor <- 0.6 ^ td_allocation[selected]
          td_probs_eligible[selected_idx] <- td_probs_eligible[selected_idx] * reduction_factor
          
          if (sum(td_probs_eligible) > 0) {
            td_probs_eligible <- td_probs_eligible / sum(td_probs_eligible)
          } else {
            td_probs_eligible <- rep(1/length(eligible), length(eligible))
          }
        }
      }
    }
    
    # CONSTRAINT: If player has rec TDs, ensure catches >= TDs
    for (i in 1:n_receivers) {
      if (td_allocation[i] > 0 && rec_allocation[i] < td_allocation[i]) {
        needed_catches <- td_allocation[i] - rec_allocation[i]
        rec_allocation[i] <- rec_allocation[i] + needed_catches
        
        # Take from others proportionally
        for (take in 1:needed_catches) {
          eligible_to_reduce <- which(rec_allocation > td_allocation & (1:n_receivers) != i)
          if (length(eligible_to_reduce) > 0) {
            excess <- rec_allocation[eligible_to_reduce] - td_allocation[eligible_to_reduce]
            max_idx <- eligible_to_reduce[which.max(excess)]
            rec_allocation[max_idx] <- rec_allocation[max_idx] - 1
          } else {
            break
          }
        }
      }
    }
    
    # FINAL VALIDATION
    for (i in 1:n_receivers) {
      # Yards > 0 → catches >= 1
      if (rec_yds_allocation[i] > 0 && rec_allocation[i] == 0) {
        rec_allocation[i] <- 1
      }
      # TDs > 0 → catches >= TDs
      if (td_allocation[i] > 0 && rec_allocation[i] < td_allocation[i]) {
        rec_allocation[i] <- td_allocation[i]
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
        all_player_results[[existing_idx[1]]]$INTs <- as.integer(opponent_ints)
      } else {
        all_player_results[[result_idx]] <- list(
          SimID = sim_id, Team = team_name, Player = player_name,
          PassYds = player_pass_yds, PassTDs = player_pass_tds, INTs = as.integer(opponent_ints),
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
  results <- rbindlist(all_player_results, use.names = TRUE, fill = TRUE)
  
  # Calculate fantasy points (same as CFB version)
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
  
  # Detect DST scoring (NFL vs CFB)
  use_dst <- has_dst_scoring(similar_games, team_names)
  
  cat(sprintf("Teams: %s vs %s\n", team_names[1], team_names[2]))
  cat(sprintf("Similar games: %d\n", nrow(similar_games)))
  
  if (use_dst) {
    cat("✓ DST scoring detected (NFL mode)\n\n")
  } else {
    cat("✓ No DST data (CFB mode)\n\n")
  }
  
  # Pre-allocate (2 teams + 2 DST if NFL)
  list_size <- if (use_dst) n_sims * 4 else n_sims * 2
  all_results <- vector("list", list_size)
  result_idx <- 1
  
  # Pre-fetch DST names and column names if using DST (avoid repeated operations in loop)
  team1_dst_name <- NULL
  team2_dst_name <- NULL
  def1_sacks_col <- NULL
  def1_ints_col <- NULL
  def1_fum_col <- NULL
  def1_pts_col <- NULL
  def2_sacks_col <- NULL
  def2_ints_col <- NULL
  def2_fum_col <- NULL
  def2_pts_col <- NULL
  
  if (use_dst) {
    # Pre-fetch DST names
    team1_dst_name <- dk_salaries %>%
      filter(Team == team_names[1], Pos == "DST") %>%
      pull(Name)
    if (length(team1_dst_name) == 0) team1_dst_name <- paste0(team_names[1], " DST")
    else team1_dst_name <- team1_dst_name[1]
    
    team2_dst_name <- dk_salaries %>%
      filter(Team == team_names[2], Pos == "DST") %>%
      pull(Name)
    if (length(team2_dst_name) == 0) team2_dst_name <- paste0(team_names[2], " DST")
    else team2_dst_name <- team2_dst_name[1]
    
    # Pre-build column names
    def1_sacks_col <- paste0(team_names[1], "_Def_Sacks")
    def1_ints_col <- paste0(team_names[1], "_Def_Ints")
    def1_fum_col <- paste0(team_names[1], "_Def_Fum")
    def1_pts_col <- paste0(team_names[1], "_Def_Pts_Allow")
    def1_tds_col <- paste0(team_names[1], "_Def_TDs")
    def1_safeties_col <- paste0(team_names[1], "_Def_Safeties")
    
    def2_sacks_col <- paste0(team_names[2], "_Def_Sacks")
    def2_ints_col <- paste0(team_names[2], "_Def_Ints")
    def2_fum_col <- paste0(team_names[2], "_Def_Fum")
    def2_pts_col <- paste0(team_names[2], "_Def_Pts_Allow")
    def2_tds_col <- paste0(team_names[2], "_Def_TDs")
    def2_safeties_col <- paste0(team_names[2], "_Def_Safeties")
  }
  
  batch_size <- 500
  n_batches <- ceiling(n_sims / batch_size)
  start_time <- Sys.time()
  
  for (batch in 1:n_batches) {
    start_sim <- (batch - 1) * batch_size + 1
    end_sim <- min(batch * batch_size, n_sims)
    
    for (sim in start_sim:end_sim) {
      sampled_game <- similar_games[sample(nrow(similar_games), 1), ]
      
      # Extract opponent INTs first (if using DST)
      team1_opponent_ints <- 0
      team2_opponent_ints <- 0
      
      if (use_dst) {
        team1_dst_ints <- as.numeric(sampled_game[[def1_ints_col]])
        team2_dst_ints <- as.numeric(sampled_game[[def2_ints_col]])
        if (is.na(team1_dst_ints)) team1_dst_ints <- 0
        if (is.na(team2_dst_ints)) team2_dst_ints <- 0
        
        # Team1's defense INTs → Team2's QB threw them
        team2_opponent_ints <- team1_dst_ints
        # Team2's defense INTs → Team1's QB threw them  
        team1_opponent_ints <- team2_dst_ints
      }
      
      team1_result <- simulate_team_game(sim, team_names[1], team1_data, sampled_game, dk_salaries, use_dst, team1_opponent_ints)
      all_results[[result_idx]] <- team1_result
      result_idx <- result_idx + 1
      
      team2_result <- simulate_team_game(sim, team_names[2], team2_data, sampled_game, dk_salaries, use_dst, team2_opponent_ints)
      all_results[[result_idx]] <- team2_result
      result_idx <- result_idx + 1
      
      # CREATE DST PLAYERS (NFL MODE)
      if (use_dst) {
        # Extract remaining defensive stats (INTs already extracted above for QB assignment)
        team1_dst_sacks <- as.numeric(sampled_game[[def1_sacks_col]])
        team1_dst_fum <- as.numeric(sampled_game[[def1_fum_col]])
        team1_dst_pts_allow <- as.numeric(sampled_game[[def1_pts_col]])
        team1_dst_def_tds <- as.numeric(sampled_game[[def1_tds_col]])
        team1_dst_safeties <- as.numeric(sampled_game[[def1_safeties_col]])
        
        team2_dst_sacks <- as.numeric(sampled_game[[def2_sacks_col]])
        team2_dst_fum <- as.numeric(sampled_game[[def2_fum_col]])
        team2_dst_pts_allow <- as.numeric(sampled_game[[def2_pts_col]])
        team2_dst_def_tds <- as.numeric(sampled_game[[def2_tds_col]])
        team2_dst_safeties <- as.numeric(sampled_game[[def2_safeties_col]])
        
        # Validate (INTs already validated above)
        if (is.na(team1_dst_sacks)) team1_dst_sacks <- 0
        if (is.na(team1_dst_fum)) team1_dst_fum <- 0
        if (is.na(team1_dst_pts_allow)) team1_dst_pts_allow <- 20
        if (is.na(team1_dst_def_tds)) team1_dst_def_tds <- 0
        if (is.na(team1_dst_safeties)) team1_dst_safeties <- 0
        if (is.na(team2_dst_sacks)) team2_dst_sacks <- 0
        if (is.na(team2_dst_fum)) team2_dst_fum <- 0
        if (is.na(team2_dst_pts_allow)) team2_dst_pts_allow <- 20
        if (is.na(team2_dst_def_tds)) team2_dst_def_tds <- 0
        if (is.na(team2_dst_safeties)) team2_dst_safeties <- 0
        
        # PRE-CALCULATE DST fantasy points (FAST - no vectorization needed)
        team1_pts_allow_score <- if (team1_dst_pts_allow == 0) 10 else
          if (team1_dst_pts_allow <= 6) 7 else
            if (team1_dst_pts_allow <= 13) 4 else
              if (team1_dst_pts_allow <= 20) 1 else
                if (team1_dst_pts_allow <= 27) 0 else
                  if (team1_dst_pts_allow <= 34) -1 else -4
        
        team2_pts_allow_score <- if (team2_dst_pts_allow == 0) 10 else
          if (team2_dst_pts_allow <= 6) 7 else
            if (team2_dst_pts_allow <= 13) 4 else
              if (team2_dst_pts_allow <= 20) 1 else
                if (team2_dst_pts_allow <= 27) 0 else
                  if (team2_dst_pts_allow <= 34) -1 else -4
        
        team1_dst_total <- (team1_dst_sacks * 1) + (team1_dst_ints * 2) + (team1_dst_fum * 2) + 
          team1_pts_allow_score + (team1_dst_def_tds * 6) + (team1_dst_safeties * 2)
        team2_dst_total <- (team2_dst_sacks * 1) + (team2_dst_ints * 2) + (team2_dst_fum * 2) + 
          team2_pts_allow_score + (team2_dst_def_tds * 6) + (team2_dst_safeties * 2)
        
        # Store DST with pre-calculated points
        team1_dst_result <- data.table(
          SimID = sim, Team = team_names[1], Player = team1_dst_name,
          PassYds = 0, PassTDs = 0L, INTs = 0L,
          RushYds = 0, RushTDs = 0L,
          Recs = 0L, RecYds = 0, RecTDs = 0L,
          FGsMade = 0L, FG_Under30 = 0L, FG_30_39 = 0L, FG_40_49 = 0L, FG_50Plus = 0L,
          XPs = 0L, FumLost = 0L,
          PassPts = 0, RushPts = 0, RecPts = 0, KickPts = 0, FumPts = 0,
          DSTPts = team1_dst_total, TotalPts = team1_dst_total
        )
        
        team2_dst_result <- data.table(
          SimID = sim, Team = team_names[2], Player = team2_dst_name,
          PassYds = 0, PassTDs = 0L, INTs = 0L,
          RushYds = 0, RushTDs = 0L,
          Recs = 0L, RecYds = 0, RecTDs = 0L,
          FGsMade = 0L, FG_Under30 = 0L, FG_30_39 = 0L, FG_40_49 = 0L, FG_50Plus = 0L,
          XPs = 0L, FumLost = 0L,
          PassPts = 0, RushPts = 0, RecPts = 0, KickPts = 0, FumPts = 0,
          DSTPts = team2_dst_total, TotalPts = team2_dst_total
        )
        
        all_results[[result_idx]] <- team1_dst_result
        result_idx <- result_idx + 1
        all_results[[result_idx]] <- team2_dst_result
        result_idx <- result_idx + 1
        
        # (INTs now assigned directly to QB in simulate_team_game function)
      }
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
    MedianPts = median(TotalPts, na.rm = TRUE),
    AvgPts = mean(TotalPts, na.rm = TRUE),
    MaxPts = max(TotalPts, na.rm = TRUE)
  ), by = .(Player, Team)]
  
  # Add salary and position if available
  if (!is.null(dk_salaries)) {
    setDT(dk_salaries)
    
    
    # Select relevant columns (handle various column names)
    dk_cols <- c("Name")
    if ("Salary" %in% names(dk_salaries)) {
      dk_cols <- c(dk_cols, "Salary")
    } else {
    }
    if ("Pos" %in% names(dk_salaries)) {
      dk_cols <- c(dk_cols, "Pos")
    } else {
    }
    
    # Merge by Name only (Team might have formatting differences)
    scoring_stats <- merge(scoring_stats, 
                           dk_salaries[, ..dk_cols], 
                           by.x = "Player", 
                           by.y = "Name", 
                           all.x = TRUE)
    
  }
  
  # Convert team names to abbreviations
  scoring_stats[, TeamAbbr := ifelse(grepl("Texas", Team), "TA&M",
                                     ifelse(grepl("Miami", Team), "MIA",
                                            ifelse(grepl("Utah State", Team), "USU",
                                                   ifelse(grepl("Washington State", Team), "WSU",
                                                          Team))))]
  
  setorder(scoring_stats, -MedianPts)
  
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
  
  # Check if correct ID columns exist and select relevant columns including ownership
  if ("DFS_ID" %in% names(dk_salaries) && "CPT_DFS_ID" %in% names(dk_salaries)) {
    # Use the actual ID columns
    cols_to_select <- c("Name", "Salary", "DFS_ID", "CPT_DFS_ID")
    
    # Add ownership columns if they exist
    if ("CPT_Own" %in% names(dk_salaries)) cols_to_select <- c(cols_to_select, "CPT_Own")
    if ("Flex_Own" %in% names(dk_salaries)) cols_to_select <- c(cols_to_select, "Flex_Own")
    
    dk_salaries_with_ids <- dk_salaries[, ..cols_to_select]
  } else {
    # Generate IDs if columns don't exist
    dk_salaries_with_ids <- dk_salaries[, .(Name, Salary)]
    dk_salaries_with_ids[, DFS_ID := paste0("ID", 1:.N)]
    dk_salaries_with_ids[, CPT_DFS_ID := paste0("CPTID", 1:.N)]
    cat("Note: DFS_ID/CPT_DFS_ID columns not found, using generated IDs\n")
    
    # Add ownership if exists
    if ("CPT_Own" %in% names(dk_salaries)) dk_salaries_with_ids[, CPT_Own := dk_salaries$CPT_Own]
    if ("Flex_Own" %in% names(dk_salaries)) dk_salaries_with_ids[, Flex_Own := dk_salaries$Flex_Own]
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
    cat("✓ Ownership data found - adding ownership metrics to lineups\n")
  } else {
    has_ownership <- FALSE
    cat("⚠ No ownership data (CPT_Own/Flex_Own) - skipping ownership metrics\n")
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
    
    # Cumulative Ownership (sum)
    lineup_counts[, CumulativeOwnership := TotalOwn]
    
    # Geometric Mean Ownership - simplified calculation
    lineup_counts[, GeometricMeanOwnership := exp(log(TotalOwn / 6))]
  }
  
  # Count players per team - show as "Team1Count-Team2Count"
  # Create player -> team lookup
  player_team_lookup <- unique(sim_results[, .(Player, Team)])
  player_team_lookup <- setNames(player_team_lookup$Team, player_team_lookup$Player)
  
  # Get team names
  team_names <- all_teams  # From earlier in function
  
  # For each lineup, count players from each team
  lineup_counts[, TeamStack := {
    teams <- character(6)
    teams[1] <- player_team_lookup[gsub(" \\(.*\\)", "", Captain)]
    for (i in 1:5) {
      player_col <- paste0("Player", i)
      teams[i+1] <- player_team_lookup[gsub(" \\(.*\\)", "", get(player_col))]
    }
    # Count by team
    team1_count <- sum(teams == team_names[1], na.rm = TRUE)
    team2_count <- sum(teams == team_names[2], na.rm = TRUE)
    paste0(team1_count, "-", team2_count)
  }, by = 1:nrow(lineup_counts)]
  
  # Remove individual rank count columns, keep only Top counts
  player_cols <- paste0("Player", 1:5)
  
  if (has_ownership) {
    final_cols <- c("Captain", player_cols, "Top1Count", "Top2Count", "Top3Count", "Top5Count", 
                    "TotalSalary", "CumulativeOwnership", "GeometricMeanOwnership", "TeamStack")
  } else {
    final_cols <- c("Captain", player_cols, "Top1Count", "Top2Count", "Top3Count", "Top5Count", 
                    "TotalSalary", "TeamStack")
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
    
    # DEBUG: Print what filters we received
    cat("\n=== FILTER DEBUG ===\n")
    cat("Excluded captains:", paste(filters$excluded_captains, collapse = ", "), "\n")
    cat("Excluded flex:", paste(filters$excluded_flex, collapse = ", "), "\n")
    cat("Excluded stacks:", paste(filters$excluded_stacks, collapse = ", "), "\n")
    cat("Starting lineups:", nrow(optimal_lineups), "\n")
    
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
    
    # Apply team stack exclusion filter
    if (!is.null(filters$excluded_stacks) && length(filters$excluded_stacks) > 0 && "TeamStack" %in% names(filtered_lineups)) {
      cat("Applying stack filter...\n")
      filtered_lineups <- filtered_lineups[!TeamStack %in% filters$excluded_stacks]
      cat("After stack filter:", nrow(filtered_lineups), "lineups\n")
    }
    
    # Apply captain exclusion filter (match on name without ID)
    if (!is.null(filters$excluded_captains) && length(filters$excluded_captains) > 0) {
      cat("Applying captain filter...\n")
      # Strip IDs from captain names for comparison
      captain_names <- gsub(" \\([^)]+\\)$", "", filtered_lineups$Captain)
      to_exclude <- captain_names %in% filters$excluded_captains
      cat("Lineups to exclude:", sum(to_exclude), "\n")
      filtered_lineups <- filtered_lineups[!to_exclude]
      cat("After captain filter:", nrow(filtered_lineups), "lineups\n")
    }
    
    # Apply flex exclusion filter (match on name without ID)
    if (!is.null(filters$excluded_flex) && length(filters$excluded_flex) > 0) {
      cat("Applying flex filter...\n")
      flex_cols <- paste0("Player", 1:5)
      to_exclude <- rep(FALSE, nrow(filtered_lineups))
      
      for(col in flex_cols) {
        if(col %in% names(filtered_lineups)) {
          # Strip IDs from player names for comparison
          player_names <- gsub(" \\([^)]+\\)$", "", filtered_lineups[[col]])
          to_exclude <- to_exclude | (player_names %in% filters$excluded_flex)
        }
      }
      
      cat("Lineups to exclude:", sum(to_exclude), "\n")
      filtered_lineups <- filtered_lineups[!to_exclude]
      cat("After flex filter:", nrow(filtered_lineups), "lineups\n")
    }
    
    cat("=== END FILTER DEBUG ===\n\n")
    
    return(list(
      count = nrow(filtered_lineups),
      filtered_lineups = as.data.frame(filtered_lineups)
    ))
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
    
    # Apply team count filter
    # Apply team stack exclusion filter
    if (!is.null(filters$excluded_stacks) && length(filters$excluded_stacks) > 0 && "TeamStack" %in% names(filtered_lineups)) {
      filtered_lineups <- filtered_lineups[!TeamStack %in% filters$excluded_stacks]
    }
    
    # Apply player exclusion filter - match on name without ID
    if (!is.null(filters$excluded_captains) && length(filters$excluded_captains) > 0) {
      captain_names <- gsub(" \\([^)]+\\)$", "", filtered_lineups$Captain)
      to_exclude <- captain_names %in% filters$excluded_captains
      filtered_lineups <- filtered_lineups[!to_exclude]
    }
    
    if (!is.null(filters$excluded_flex) && length(filters$excluded_flex) > 0) {
      flex_cols <- paste0("Player", 1:5)
      to_exclude <- rep(FALSE, nrow(filtered_lineups))
      
      for(col in flex_cols) {
        if(col %in% names(filtered_lineups)) {
          player_names <- gsub(" \\([^)]+\\)$", "", filtered_lineups[[col]])
          to_exclude <- to_exclude | (player_names %in% filters$excluded_flex)
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
    if(is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
      return(data.frame(Message = "No optimal lineups available."))
    }
    
    # Get all players from optimal lineups (strip IDs)
    player_cols <- c("Captain", paste0("Player", 1:5))
    all_players <- c()
    for(col in player_cols) {
      if(col %in% names(optimal_lineups)) {
        players <- gsub(" \\([^)]+\\)$", "", optimal_lineups[[col]])
        all_players <- c(all_players, players)
      }
    }
    all_players <- unique(all_players[!is.na(all_players) & all_players != ""])
    
    if(length(all_players) == 0) {
      return(data.frame(Message = "No players found in lineups."))
    }
    
    # Initialize metrics data frame
    metrics_data <- data.frame(
      Player = all_players,
      Salary = NA_real_,
      Proj = NA_real_,
      FlexOwn = NA_real_,
      CPTOwn = NA_real_,
      OptimalRate = 0,
      Exposure = NA_real_,
      Leverage = NA_real_,
      stringsAsFactors = FALSE
    )
    
    # Match with player mapping for salary, projection, ownership
    if(!is.null(player_mapping) && nrow(player_mapping) > 0) {
      for(i in 1:nrow(metrics_data)) {
        player_name <- metrics_data$Player[i]
        matches <- which(player_mapping$Name == player_name)
        
        if(length(matches) > 0) {
          match_idx <- matches[1]
          if("Salary" %in% names(player_mapping)) {
            metrics_data$Salary[i] <- player_mapping$Salary[match_idx]
          }
          if("Proj" %in% names(player_mapping)) {
            metrics_data$Proj[i] <- player_mapping$Proj[match_idx]
          }
          if("Flex_Own" %in% names(player_mapping)) {
            metrics_data$FlexOwn[i] <- player_mapping$Flex_Own[match_idx]
          }
          if("CPT_Own" %in% names(player_mapping)) {
            metrics_data$CPTOwn[i] <- player_mapping$CPT_Own[match_idx]
          }
        }
      }
    }
    
    # Calculate OptimalRate from filtered pool
    total_lineups <- nrow(optimal_lineups)
    if(total_lineups > 0) {
      for(player in all_players) {
        # Count appearances (Captain + Flex combined)
        captain_appears <- gsub(" \\([^)]+\\)$", "", optimal_lineups$Captain) == player
        captain_count <- sum(captain_appears, na.rm = TRUE)
        
        flex_count <- 0
        for(col in paste0("Player", 1:5)) {
          if(col %in% names(optimal_lineups)) {
            flex_appears <- gsub(" \\([^)]+\\)$", "", optimal_lineups[[col]]) == player
            flex_count <- flex_count + sum(flex_appears, na.rm = TRUE)
          }
        }
        
        # Combined optimal rate (% of filtered lineups containing this player)
        total_appears <- captain_count + flex_count
        metrics_data[metrics_data$Player == player, "OptimalRate"] <- (total_appears / total_lineups) * 100
      }
    }
    
    # Calculate Exposure from generated lineups (if provided)
    if(!is.null(generated_lineups) && nrow(generated_lineups) > 0) {
      for(player in all_players) {
        # Count appearances in generated lineups
        captain_appears <- gsub(" \\([^)]+\\)$", "", generated_lineups$Captain) == player
        captain_count <- sum(captain_appears, na.rm = TRUE)
        
        flex_count <- 0
        for(col in paste0("Player", 1:5)) {
          if(col %in% names(generated_lineups)) {
            flex_appears <- gsub(" \\([^)]+\\)$", "", generated_lineups[[col]]) == player
            flex_count <- flex_count + sum(flex_appears, na.rm = TRUE)
          }
        }
        
        # Exposure = % in generated lineups
        total_appears <- captain_count + flex_count
        exposure_pct <- (total_appears / nrow(generated_lineups)) * 100
        metrics_data[metrics_data$Player == player, "Exposure"] <- exposure_pct
        
        # Leverage = Exposure - Average Ownership
        # Use FlexOwn as baseline (could also average CPT and Flex)
        if(!is.na(metrics_data[metrics_data$Player == player, "FlexOwn"])) {
          own <- metrics_data[metrics_data$Player == player, "FlexOwn"]
          metrics_data[metrics_data$Player == player, "Leverage"] <- exposure_pct - own
        }
      }
    }
    
    # Sort by OptimalRate descending
    metrics_data <- metrics_data[order(-metrics_data$OptimalRate), ]
    
    return(metrics_data)
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
                column(3, selectizeInput("excluded_captains", "Exclude from Captain:", 
                                         choices = NULL, multiple = TRUE,
                                         options = list(plugins = list('remove_button'), 
                                                        placeholder = 'Exclude from CPT slot'))),
                column(3, selectizeInput("excluded_flex", "Exclude from Flex:", 
                                         choices = NULL, multiple = TRUE,
                                         options = list(plugins = list('remove_button'), 
                                                        placeholder = 'Exclude from FLEX slots'))),
                column(3, selectizeInput("excluded_stacks", "Exclude Stacks:", 
                                         choices = NULL, multiple = TRUE,
                                         options = list(plugins = list('remove_button'),
                                                        placeholder = 'Click to select stacks to exclude'))),
                column(3, numericInput("num_random_lineups", "Number of Lineups to Generate:", 
                                       value = 20, min = 1, max = 150))
              ),
              
              fluidRow(
                column(6, div(class = "well well-sm", 
                              h4("Filtered Pool Statistics:", style = "color: #000000;"), 
                              tags$style("#filtered_pool_size {color: #000000; font-size: 16px; font-weight: bold;}"),
                              textOutput("filtered_pool_size", inline = TRUE))),
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
    filtered_pool = NULL,
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
      
      
      # Reorder columns - Pos, Salary, Median, Avg, then other projections
      if ("Pos" %in% names(projections)) {
        projections <- projections %>%
          select(Player, Pos, TeamAbbr, Salary, MedianPts, AvgPts, ETR_DK_Pts, Saber_Proj)
      } else {
        projections <- projections %>%
          select(Player, TeamAbbr, Salary, MedianPts, AvgPts, ETR_DK_Pts, Saber_Proj)
      }
      
    } else {
      if ("Pos" %in% names(projections)) {
        projections <- projections %>%
          select(Player, Pos, TeamAbbr, Salary, MedianPts, AvgPts)
      } else {
        projections <- projections %>%
          select(Player, TeamAbbr, Salary, MedianPts, AvgPts)
      }
    }
    
    # Determine column names based on what's present
    col_names <- c('Player')
    col_idx <- 1
    if ("Pos" %in% names(projections)) {
      col_names <- c(col_names, 'Pos')
      col_idx <- col_idx + 1
    }
    col_names <- c(col_names, 'Team', 'Salary', 'Median Pts', 'Avg Pts')
    median_col_idx <- length(col_names) - 1  # MedianPts is now second-to-last before AvgPts
    
    if ("ETR_DK_Pts" %in% names(projections)) {
      col_names <- c(col_names, 'ETR Proj', 'Saber Proj')
    }
    
    datatable(
      projections,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(median_col_idx - 1, 'desc')),  # Sort by MedianPts
        columnDefs = list(
          list(className = 'dt-center', targets = col_idx:ncol(projections)-1)
        )
      ),
      rownames = FALSE,
      caption = "Simulated vs ETR vs Saber Projections",
      colnames = col_names
    ) %>%
      formatCurrency('Salary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(setdiff(names(projections), c('Player', 'Pos', 'TeamAbbr', 'Salary')), 2) %>%
      formatStyle(
        'MedianPts',
        background = styleColorBar(range(projections$MedianPts, na.rm = TRUE), '#32CD32'),  # Green for median
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'AvgPts',
        background = styleColorBar(range(projections$AvgPts, na.rm = TRUE), '#FFA500'),  # Orange for average
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      {if("ETR_DK_Pts" %in% names(projections)) {
        formatStyle(., 'ETR_DK_Pts',
                    background = styleColorBar(range(projections$ETR_DK_Pts, na.rm = TRUE), '#1E90FF'),  # Blue for ETR
                    backgroundSize = '95% 80%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')
      } else .} %>%
      {if("Saber_Proj" %in% names(projections)) {
        formatStyle(., 'Saber_Proj',
                    background = styleColorBar(range(projections$Saber_Proj, na.rm = TRUE), '#1E90FF'),  # Blue for Saber (same as ETR)
                    backgroundSize = '95% 80%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')
      } else .} %>%
      apply_team_colors_to_table(projections, "TeamAbbr")  # Add team colors
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
    
    # Get top 10 players by median points
    top_players <- team_data[, .(MedianPts = median(TotalPts)), by = Player][
      order(-MedianPts)
    ][1:min(10, .N)]
    
    team_data <- team_data[Player %in% top_players$Player]
    
    # Order players by median
    player_order <- team_data[, .(MedianPts = median(TotalPts)), by = Player][
      order(MedianPts)
    ]$Player
    
    p <- plot_ly(team_data, 
                 y = ~factor(Player, levels = player_order),
                 x = ~TotalPts,
                 type = "box",
                 orientation = "h",
                 marker = list(color = team_color),
                 line = list(color = team_color),
                 fillcolor = paste0(team_color, "80"),
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
    
    # Get top 10 players by median points
    top_players <- team_data[, .(MedianPts = median(TotalPts)), by = Player][
      order(-MedianPts)
    ][1:min(10, .N)]
    
    team_data <- team_data[Player %in% top_players$Player]
    
    # Order players by median
    player_order <- team_data[, .(MedianPts = median(TotalPts)), by = Player][
      order(MedianPts)
    ]$Player
    
    p <- plot_ly(team_data, 
                 y = ~factor(Player, levels = player_order),
                 x = ~TotalPts,
                 type = "box",
                 orientation = "h",
                 marker = list(color = team_color),
                 line = list(color = team_color),
                 fillcolor = paste0(team_color, "80"),
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
        rv$filtered_pool <- optimal_lineups  # Initialize with all lineups
        
        # Calculate initial player exposure (before random lineups)
        dk_data <- rv$input_data$dk_salaries
        player_mapping <- data.frame(
          Name = dk_data$Name,
          Salary = dk_data$Salary,
          Proj = if("ETR_DK_Pts" %in% names(dk_data)) dk_data$ETR_DK_Pts else NA,
          Flex_Own = if("Flex_Own" %in% names(dk_data)) dk_data$Flex_Own else NA,
          CPT_Own = if("CPT_Own" %in% names(dk_data)) dk_data$CPT_Own else NA,
          stringsAsFactors = FALSE
        )
        
        rv$player_exposure <- calculate_player_exposure(
          rv$optimal_lineups,
          player_mapping,
          NULL  # No random lineups yet
        )
        
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
  
  # Optimal lineups table - FAST VERSION
  output$optimal_lineups_table <- renderDT({
    req(rv$optimal_lineups)
    req(rv$simulation_results)
    
    
    # Use filtered pool if available (from filter controls), otherwise show all
    if(!is.null(rv$filtered_pool) && nrow(rv$filtered_pool) > 0) {
      display_lineups <- copy(rv$filtered_pool)
    } else {
      display_lineups <- copy(rv$optimal_lineups)
    }
    
    # Remove IDs from display (keep original for download)
    display_lineups$Captain <- gsub(" \\([^)]+\\)$", "", display_lineups$Captain)
    for (i in 1:5) {
      player_col <- paste0("Player", i)
      if (player_col %in% names(display_lineups)) {
        display_lineups[[player_col]] <- gsub(" \\([^)]+\\)$", "", display_lineups[[player_col]])
      }
    }
    
    # Skip coloring - just show clean names (MUCH FASTER)
    # Team colors can be added via CSS if needed later
    
    datatable(
      display_lineups,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        order = list(list(which(names(display_lineups) == "Top1Count") - 1, 'desc')),
        columnDefs = list(
          list(className = 'dt-center', targets = 6:ncol(display_lineups)-1)
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe compact'
    ) %>%
      formatCurrency('TotalSalary', '$', digits = 0) %>%
      formatRound(intersect(c('CumulativeOwnership', 'GeometricMeanOwnership'), names(display_lineups)), 1) %>%
      formatStyle(
        'Top1Count',
        background = styleColorBar(range(display_lineups$Top1Count, na.rm = TRUE), '#FFD700'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        columns = intersect(c('Top1Count', 'Top2Count', 'Top3Count', 'Top5Count'), names(display_lineups)),
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
      # Get all unique player names WITH IDs
      all_players_with_ids <- unique(c(
        rv$optimal_lineups$Captain,
        unlist(rv$optimal_lineups[, paste0("Player", 1:5)])
      ))
      all_players_with_ids <- all_players_with_ids[!is.na(all_players_with_ids) & all_players_with_ids != ""]
      
      # Strip IDs to get clean names for dropdown display
      all_players_clean <- gsub(" \\([^)]+\\)$", "", all_players_with_ids)
      all_players_clean <- unique(all_players_clean)  # Remove duplicates
      all_players_clean <- sort(all_players_clean)
      
      # Update both captain and flex exclusion dropdowns with clean names
      updateSelectizeInput(session, "excluded_captains", 
                           choices = all_players_clean, 
                           server = TRUE)
      
      updateSelectizeInput(session, "excluded_flex", 
                           choices = all_players_clean, 
                           server = TRUE)
      
      # Get TeamStack choices with percentages
      if ("TeamStack" %in% names(rv$optimal_lineups)) {
        stack_counts <- table(rv$optimal_lineups$TeamStack)
        stack_pcts <- round(100 * stack_counts / nrow(rv$optimal_lineups), 1)
        stack_choices <- paste0(names(stack_counts), " (", stack_pcts, "%)")
        names(stack_choices) <- names(stack_counts)  # Value is stack name, label shows percentage
        
        updateSelectizeInput(session, "excluded_stacks",
                             choices = stack_choices,
                             server = TRUE)
      }
      
      # Update ownership sliders based on actual data range
      if ("CumulativeOwnership" %in% names(rv$optimal_lineups)) {
        cum_min <- floor(min(rv$optimal_lineups$CumulativeOwnership, na.rm = TRUE))
        cum_max <- ceiling(max(rv$optimal_lineups$CumulativeOwnership, na.rm = TRUE))
        updateSliderInput(session, "cumulative_ownership_range",
                          min = cum_min, max = cum_max, value = c(cum_min, cum_max))
      }
      
      if ("GeometricMeanOwnership" %in% names(rv$optimal_lineups)) {
        geo_min <- floor(min(rv$optimal_lineups$GeometricMeanOwnership, na.rm = TRUE))
        geo_max <- ceiling(max(rv$optimal_lineups$GeometricMeanOwnership, na.rm = TRUE))
        updateSliderInput(session, "geometric_mean_range",
                          min = geo_min, max = geo_max, value = c(geo_min, geo_max))
      }
    }
  })
  
  # Real-time filtered pool size calculation
  observeEvent(c(input$min_top1_count, input$min_top2_count, input$min_top3_count,
                 input$min_top5_count, input$cumulative_ownership_range,
                 input$geometric_mean_range, input$excluded_captains, input$excluded_flex, input$excluded_stacks), {
                   if(!is.null(rv$optimal_lineups)) {
                     filters <- list(
                       min_top1_count = if(!is.null(input$min_top1_count)) input$min_top1_count else 0,
                       min_top2_count = if(!is.null(input$min_top2_count)) input$min_top2_count else 0,
                       min_top3_count = if(!is.null(input$min_top3_count)) input$min_top3_count else 0,
                       min_top5_count = if(!is.null(input$min_top5_count)) input$min_top5_count else 0,
                       cumulative_ownership_range = input$cumulative_ownership_range,
                       geometric_mean_range = input$geometric_mean_range,
                       excluded_stacks = input$excluded_stacks,
                       excluded_captains = if(!is.null(input$excluded_captains)) input$excluded_captains else character(0),
                       excluded_flex = if(!is.null(input$excluded_flex)) input$excluded_flex else character(0)
                     )
                     
                     pool_stats <- calculate_filtered_pool_stats(rv$optimal_lineups, filters)
                     
                     # Store filtered pool for exposure calculation
                     rv$filtered_pool <- pool_stats$filtered_lineups
                     
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
      excluded_stacks = input$excluded_stacks,
      num_lineups = input$num_random_lineups,
      excluded_captains = input$excluded_captains,
      excluded_flex = input$excluded_flex
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
          Flex_Own = if("Flex_Own" %in% names(dk_data)) dk_data$Flex_Own else NA,
          CPT_Own = if("CPT_Own" %in% names(dk_data)) dk_data$CPT_Own else NA,
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
        cat("⚠ No lineups matched filters\n")
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
    req(rv$simulation_complete)
    
    tryCatch({
      # Start with basic player info
      dk_data <- rv$input_data$dk_salaries
      
      # Get unique players from simulation
      all_players <- unique(rv$simulation_results$Player)
      
      display_data <- data.frame(
        Player = all_players,
        stringsAsFactors = FALSE
      )
      
      # Add Pos, Salary, CPT_Own, Flex_Own, Team from DK salaries
      if(!is.null(dk_data)) {
        display_data <- display_data %>%
          left_join(dk_data %>% select(Name, Pos, Team, Salary, CPT_Own, Flex_Own), 
                    by = c("Player" = "Name"))
        
        # Calculate total ownership (sum, not average)
        display_data$TotalOwn <- display_data$CPT_Own + display_data$Flex_Own
      }
      
      # Add Exposure from generated lineups (if randomized)
      has_random_lineups <- !is.null(rv$random_lineups) && nrow(rv$random_lineups) > 0
      
      if(has_random_lineups) {
        display_data$CPT_Exposure <- 0
        display_data$Flex_Exposure <- 0
        
        for(i in 1:nrow(display_data)) {
          player <- display_data$Player[i]
          
          # Captain exposure
          captain_count <- sum(gsub(" \\([^)]+\\)$", "", rv$random_lineups$Captain) == player, na.rm = TRUE)
          display_data$CPT_Exposure[i] <- (captain_count / nrow(rv$random_lineups)) * 100
          
          # Flex exposure
          flex_count <- 0
          for(col in paste0("Player", 1:5)) {
            if(col %in% names(rv$random_lineups)) {
              flex_count <- flex_count + sum(gsub(" \\([^)]+\\)$", "", rv$random_lineups[[col]]) == player, na.rm = TRUE)
            }
          }
          display_data$Flex_Exposure[i] <- (flex_count / nrow(rv$random_lineups)) * 100
        }
        
        # Combined exposure
        display_data$Exposure <- display_data$CPT_Exposure + display_data$Flex_Exposure
        
        # Calculate Leverage = Exposure - Own
        display_data$CPT_Leverage <- display_data$CPT_Exposure - display_data$CPT_Own
        display_data$Flex_Leverage <- display_data$Flex_Exposure - display_data$Flex_Own
        display_data$Leverage <- display_data$Exposure - display_data$TotalOwn
        
        # Select columns with Exposure
        display_data <- display_data %>%
          select(Player, Pos, Team, Salary, 
                 CPT_Own, CPT_Exposure, CPT_Leverage,
                 Flex_Own, Flex_Exposure, Flex_Leverage,
                 TotalOwn, Exposure, Leverage)
      } else {
        # Select columns without Exposure
        display_data <- display_data %>%
          select(Player, Pos, Team, Salary, CPT_Own, Flex_Own, TotalOwn)
      }
      
      # Sort by Exposure descending (or TotalOwn if no random lineups)
      if(has_random_lineups) {
        display_data <- display_data[order(-display_data$Exposure), ]
      } else {
        display_data <- display_data[order(-display_data$TotalOwn), ]
      }
      
      dt <- datatable(
        display_data,
        options = list(
          pageLength = 25,
          dom = "ftp",
          scrollX = TRUE,
          processing = TRUE
        ),
        rownames = FALSE,
        class = 'cell-border stripe compact'
      )
      
      # Format columns
      if("Salary" %in% names(display_data)) {
        dt <- dt %>% formatCurrency('Salary', currency = "$", interval = 3, mark = ",", digits = 0)
      }
      
      # Format numeric columns
      numeric_cols <- intersect(
        c('CPT_Own', 'Flex_Own', 'TotalOwn', 'CPT_Exposure', 'Flex_Exposure', 'Exposure', 
          'CPT_Leverage', 'Flex_Leverage', 'Leverage'), 
        names(display_data)
      )
      if(length(numeric_cols) > 0) {
        dt <- dt %>% formatRound(numeric_cols, digits = 1)
      }
      
      # Add team colors
      dt <- apply_team_colors_to_table(dt, display_data, "Team")
      
      return(dt)
    }, error = function(e) {
      cat("❌ Error in player_exposure_table:", e$message, "\n")
      cat("Traceback:\n")
      print(traceback())
      datatable(data.frame(Error = paste("Error displaying exposure:", e$message)),
                options = list(dom = 't'), rownames = FALSE)
    })
  })
  
  # Random lineups table - matches optimal lineups style
  output$random_lineups_table <- renderDT({
    req(rv$random_lineups)
    
    
    tryCatch({
      # Remove IDs from display
      display_lineups <- as.data.frame(rv$random_lineups)
      
      display_lineups$Captain <- gsub(" \\([^)]+\\)$", "", display_lineups$Captain)
      for (i in 1:5) {
        player_col <- paste0("Player", i)
        if (player_col %in% names(display_lineups)) {
          display_lineups[[player_col]] <- gsub(" \\([^)]+\\)$", "", display_lineups[[player_col]])
        }
      }
      
      
      dt <- datatable(
        display_lineups,
        options = list(
          pageLength = 50,
          scrollX = TRUE,
          order = list(list(0, 'asc')),
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe compact'
      )
      
      
      dt <- dt %>%
        formatCurrency('TotalSalary', '$', digits = 0) %>%
        formatRound(intersect(c('CumulativeOwnership', 'GeometricMeanOwnership'), names(display_lineups)), 1)
      
      return(dt)
    }, error = function(e) {
      cat("❌ Error in random_lineups_table:", e$message, "\n")
      print(traceback())
      datatable(data.frame(Error = paste("Error displaying lineups:", e$message)),
                options = list(dom = 't'), rownames = FALSE)
    })
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