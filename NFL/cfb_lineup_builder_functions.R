# CFB LINEUP BUILDER FUNCTIONS
# Supports both Showdown (single-game) and Classic (multi-game) formats
# Author: Andrew
# Golden Ticket Sims

#' Generate optimal DraftKings Showdown lineups using linear programming
#' 
#' @param sim_results Data frame of simulation results (player-level)
#' @param dk_data Data frame of DraftKings salaries and IDs
#' @param num_lineups Number of lineups to generate
#' @param max_exposure Maximum exposure per player (0-100)
#' @param min_salary Minimum total salary to use
#' @param max_from_team Maximum players from same team
#' @param randomness Randomness factor (0-1) for projection variation
#' @return Data frame of optimal lineups
generate_showdown_lineups <- function(sim_results, dk_data, num_lineups = 20, 
                                      max_exposure = 100, min_salary = 49500,
                                      max_from_team = NULL, randomness = 0) {
  
  library(lpSolve)
  library(dplyr)
  
  # Validate inputs
  if(is.null(sim_results) || nrow(sim_results) == 0) {
    stop("sim_results is empty")
  }
  if(is.null(dk_data) || nrow(dk_data) == 0) {
    stop("dk_data is empty")
  }
  
  # Calculate average points for each player
  player_projections <- sim_results %>%
    group_by(Player) %>%
    summarise(
      AvgPoints = mean(TotalPoints, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Merge with DK data
  players <- dk_data %>%
    left_join(player_projections, by = c("Name" = "Player")) %>%
    filter(!is.na(AvgPoints))
  
  if(nrow(players) == 0) {
    stop("No players with projections found after merge")
  }
  
  # Initialize lineups storage
  lineups <- list()
  player_usage <- setNames(rep(0, nrow(players)), players$Name)
  
  for(lineup_num in 1:num_lineups) {
    
    # Apply randomness to projections
    if(randomness > 0) {
      players$AdjustedPoints <- players$AvgPoints * (1 + rnorm(nrow(players), 0, randomness))
    } else {
      players$AdjustedPoints <- players$AvgPoints
    }
    
    # Captain multiplier (1.5x points, 1.5x salary)
    players$CPT_Points <- players$AdjustedPoints * 1.5
    
    # Build constraint matrix
    # We have n players, each can be selected as either CPT or FLEX (or neither)
    # Variables: player1_cpt, player1_flex, player2_cpt, player2_flex, ...
    n_players <- nrow(players)
    n_vars <- n_players * 2  # CPT and FLEX for each player
    
    # Objective: maximize points
    objective <- rep(0, n_vars)
    for(i in 1:n_players) {
      objective[2*i - 1] <- players$CPT_Points[i]  # CPT
      objective[2*i] <- players$AdjustedPoints[i]   # FLEX
    }
    
    # Constraints
    const_mat <- matrix(0, nrow = 0, ncol = n_vars)
    const_dir <- character(0)
    const_rhs <- numeric(0)
    
    # 1. Exactly 1 captain
    captain_constraint <- rep(0, n_vars)
    captain_constraint[seq(1, n_vars, by = 2)] <- 1
    const_mat <- rbind(const_mat, captain_constraint)
    const_dir <- c(const_dir, "==")
    const_rhs <- c(const_rhs, 1)
    
    # 2. Exactly 5 flex players
    flex_constraint <- rep(0, n_vars)
    flex_constraint[seq(2, n_vars, by = 2)] <- 1
    const_mat <- rbind(const_mat, flex_constraint)
    const_dir <- c(const_dir, "==")
    const_rhs <- c(const_rhs, 5)
    
    # 3. Each player can only be selected once (CPT OR FLEX, not both)
    for(i in 1:n_players) {
      player_once <- rep(0, n_vars)
      player_once[2*i - 1] <- 1  # CPT
      player_once[2*i] <- 1       # FLEX
      const_mat <- rbind(const_mat, player_once)
      const_dir <- c(const_dir, "<=")
      const_rhs <- c(const_rhs, 1)
    }
    
    # 4. Salary constraint
    salary_constraint <- rep(0, n_vars)
    for(i in 1:n_players) {
      salary_constraint[2*i - 1] <- players$CPT_Salary[i]
      salary_constraint[2*i] <- players$Salary[i]
    }
    const_mat <- rbind(const_mat, salary_constraint)
    const_dir <- c(const_dir, "<=")
    const_rhs <- c(const_rhs, 50000)
    
    # 5. Minimum salary constraint
    if(!is.null(min_salary) && min_salary > 0) {
      const_mat <- rbind(const_mat, salary_constraint)
      const_dir <- c(const_dir, ">=")
      const_rhs <- c(const_rhs, min_salary)
    }
    
    # 6. Exposure constraints
    for(i in 1:n_players) {
      player_name <- players$Name[i]
      current_exposure <- (player_usage[player_name] / lineup_num) * 100
      
      if(current_exposure >= max_exposure) {
        # Lock out this player
        player_lock <- rep(0, n_vars)
        player_lock[2*i - 1] <- 1  # CPT
        player_lock[2*i] <- 1       # FLEX
        const_mat <- rbind(const_mat, player_lock)
        const_dir <- c(const_dir, "==")
        const_rhs <- c(const_rhs, 0)
      }
    }
    
    # 7. Max from team constraint
    if(!is.null(max_from_team)) {
      for(team in unique(players$Team)) {
        team_players <- which(players$Team == team)
        team_constraint <- rep(0, n_vars)
        for(idx in team_players) {
          team_constraint[2*idx - 1] <- 1  # CPT counts
          team_constraint[2*idx] <- 1       # FLEX counts
        }
        const_mat <- rbind(const_mat, team_constraint)
        const_dir <- c(const_dir, "<=")
        const_rhs <- c(const_rhs, max_from_team)
      }
    }
    
    # Solve
    solution <- lp(
      direction = "max",
      objective.in = objective,
      const.mat = const_mat,
      const.dir = const_dir,
      const.rhs = const_rhs,
      all.bin = TRUE
    )
    
    if(solution$status != 0) {
      warning(paste("Failed to generate lineup", lineup_num))
      next
    }
    
    # Extract lineup
    selected_vars <- which(solution$solution == 1)
    captain_idx <- NULL
    flex_indices <- c()
    
    for(var_idx in selected_vars) {
      player_idx <- ceiling(var_idx / 2)
      is_captain <- (var_idx %% 2 == 1)
      
      if(is_captain) {
        captain_idx <- player_idx
      } else {
        flex_indices <- c(flex_indices, player_idx)
      }
    }
    
    if(is.null(captain_idx) || length(flex_indices) != 5) {
      warning(paste("Invalid lineup structure for lineup", lineup_num))
      next
    }
    
    # Build lineup
    captain_player <- players$Name[captain_idx]
    captain_id <- players$CPT_DFS_ID[captain_idx]
    
    flex_players <- players$Name[flex_indices]
    flex_ids <- players$DFS_ID[flex_indices]
    
    total_salary <- players$CPT_Salary[captain_idx] + sum(players$Salary[flex_indices])
    projected_points <- players$CPT_Points[captain_idx] + sum(players$AdjustedPoints[flex_indices])
    
    # Update usage
    player_usage[captain_player] <- player_usage[captain_player] + 1
    for(p in flex_players) {
      player_usage[p] <- player_usage[p] + 1
    }
    
    # Store lineup
    lineup_df <- data.frame(
      Lineup = lineup_num,
      Captain = paste0(captain_player, " (", captain_id, ")"),
      Player1 = paste0(flex_players[1], " (", flex_ids[1], ")"),
      Player2 = paste0(flex_players[2], " (", flex_ids[2], ")"),
      Player3 = paste0(flex_players[3], " (", flex_ids[3], ")"),
      Player4 = paste0(flex_players[4], " (", flex_ids[4], ")"),
      Player5 = paste0(flex_players[5], " (", flex_ids[5], ")"),
      TotalSalary = total_salary,
      ProjectedPoints = projected_points,
      stringsAsFactors = FALSE
    )
    
    lineups[[lineup_num]] <- lineup_df
  }
  
  if(length(lineups) == 0) {
    return(NULL)
  }
  
  # Combine all lineups
  result <- bind_rows(lineups)
  
  return(result)
}


#' Generate optimal DraftKings Classic lineups (multi-game)
#' 
#' @param sim_results_list List of simulation results (one per game)
#' @param dk_data_list List of DK data frames (one per game) 
#' @param n_sims Number of simulations that were run
#' @param top_k Number of top lineups to extract from each simulation
#' @return Data frame of optimal classic lineups
generate_classic_lineups <- function(sim_results_list, dk_data_list, n_sims, top_k = 5) {
  
  library(dplyr)
  
  # Validate inputs
  if(length(sim_results_list) < 2) {
    stop("Classic mode requires at least 2 games")
  }
  
  if(length(sim_results_list) != length(dk_data_list)) {
    stop("Mismatch between sim_results and dk_data lists")
  }
  
  # Combine all simulation results and DK data
  all_sims <- bind_rows(sim_results_list)
  all_dk <- bind_rows(dk_data_list)
  
  # Check what columns are available in all_dk
  available_cols <- names(all_dk)
  
  # Build select list based on available columns
  select_cols <- c()
  if("Name" %in% available_cols) select_cols <- c(select_cols, "Name")
  if("Salary" %in% available_cols) select_cols <- c(select_cols, "Salary")
  if("Pos" %in% available_cols) select_cols <- c(select_cols, "Pos")
  if("DFS_ID" %in% available_cols) select_cols <- c(select_cols, "DFS_ID")
  
  if(length(select_cols) == 0) {
    stop("DK data missing required columns (Name, Salary, Pos, DFS_ID)")
  }
  
  # Add Salary and Pos to simulation results by matching Player names
  all_sims <- all_sims %>%
    left_join(all_dk %>% select(all_of(select_cols)), 
              by = c("Player" = "Name"))
  
  # Check if join was successful
  if(!("Salary" %in% names(all_sims)) || !("Pos" %in% names(all_sims))) {
    stop("Failed to join salary/position data. Check that player names match between simulation and DK data.")
  }
  
  # Get unique iterations
  iterations <- unique(all_sims$Iteration)
  
  # For each iteration, find the optimal Classic lineup
  lineup_list <- list()
  
  for(iter in iterations) {
    iter_sims <- all_sims %>% filter(Iteration == iter)
    
    # Sort by TotalPoints descending
    iter_sims <- iter_sims %>% arrange(desc(TotalPoints))
    
    # Build Classic lineup following position requirements
    # 1 QB, 2 RB, 3 WR, 1 FLEX (RB/WR), 1 SUPERFLEX (QB/RB/WR)
    
    # Get position players
    qbs <- iter_sims %>% filter(Pos == "QB")
    rbs <- iter_sims %>% filter(Pos == "RB")
    wrs <- iter_sims %>% filter(Pos == "WR")
    
    # Check we have enough players
    if(nrow(qbs) < 1 || nrow(rbs) < 2 || nrow(wrs) < 3) next
    
    # Select core positions (highest scoring)
    selected <- list()
    selected$QB <- qbs[1, ]
    selected$RB1 <- rbs[1, ]
    selected$RB2 <- rbs[2, ]
    selected$WR1 <- wrs[1, ]
    selected$WR2 <- wrs[2, ]
    selected$WR3 <- wrs[3, ]
    
    # Get remaining players for FLEX and SUPERFLEX
    used_players <- c(selected$QB$Player, selected$RB1$Player, selected$RB2$Player,
                      selected$WR1$Player, selected$WR2$Player, selected$WR3$Player)
    
    remaining <- iter_sims %>% filter(!Player %in% used_players)
    
    # FLEX: Best remaining RB or WR
    flex_candidates <- remaining %>% filter(Pos %in% c("RB", "WR"))
    if(nrow(flex_candidates) > 0) {
      selected$FLEX <- flex_candidates[1, ]
      used_players <- c(used_players, selected$FLEX$Player)
      remaining <- remaining %>% filter(!Player %in% used_players)
    } else {
      next  # Can't build valid lineup
    }
    
    # SUPERFLEX: Best remaining player (any position)
    if(nrow(remaining) > 0) {
      selected$SUPERFLEX <- remaining[1, ]
    } else {
      next
    }
    
    # Check salary cap and game diversity
    all_selected <- bind_rows(selected)
    total_salary <- sum(all_selected$Salary, na.rm = TRUE)
    
    if(total_salary > 50000) next  # Over salary cap
    
    # Check game diversity (need players from at least 2 games)
    games_used <- unique(all_selected$GameNumber)
    if(length(games_used) < 2) next
    
    # Get DFS IDs from joined data
    lineup_players <- all_selected$Player
    lineup_df <- data.frame(
      Iteration = iter,
      QB = paste0(selected$QB$Player, " (", selected$QB$DFS_ID, ")"),
      RB1 = paste0(selected$RB1$Player, " (", selected$RB1$DFS_ID, ")"),
      RB2 = paste0(selected$RB2$Player, " (", selected$RB2$DFS_ID, ")"),
      WR1 = paste0(selected$WR1$Player, " (", selected$WR1$DFS_ID, ")"),
      WR2 = paste0(selected$WR2$Player, " (", selected$WR2$DFS_ID, ")"),
      WR3 = paste0(selected$WR3$Player, " (", selected$WR3$DFS_ID, ")"),
      FLEX = paste0(selected$FLEX$Player, " (", selected$FLEX$DFS_ID, ")"),
      SUPERFLEX = paste0(selected$SUPERFLEX$Player, " (", selected$SUPERFLEX$DFS_ID, ")"),
      TotalSalary = total_salary,
      ProjectedPoints = sum(all_selected$TotalPoints, na.rm = TRUE),
      GamesUsed = length(games_used),
      stringsAsFactors = FALSE
    )
    
    lineup_list[[length(lineup_list) + 1]] <- lineup_df
  }
  
  if(length(lineup_list) == 0) {
    stop("No valid Classic lineups generated")
  }
  
  # Combine all lineups
  all_lineups <- bind_rows(lineup_list)
  
  # Create lineup signature (sorted player names to identify unique lineups)
  all_lineups$Signature <- apply(all_lineups[, c("QB", "RB1", "RB2", "WR1", "WR2", "WR3", "FLEX", "SUPERFLEX")], 1, function(row) {
    players <- gsub(" \\([^)]+\\)$", "", row)
    paste(sort(players), collapse = "|")
  })
  
  # Count occurrences of each unique lineup
  lineup_counts <- all_lineups %>%
    group_by(Signature) %>%
    summarise(
      Count = n(),
      QB = first(QB),
      RB1 = first(RB1),
      RB2 = first(RB2),
      WR1 = first(WR1),
      WR2 = first(WR2),
      WR3 = first(WR3),
      FLEX = first(FLEX),
      SUPERFLEX = first(SUPERFLEX),
      TotalSalary = first(TotalSalary),
      ProjectedPoints = mean(ProjectedPoints),
      GamesUsed = first(GamesUsed),
      .groups = 'drop'
    ) %>%
    arrange(desc(Count)) %>%
    select(-Signature)
  
  # Add Lineup number
  lineup_counts$Lineup <- 1:nrow(lineup_counts)
  lineup_counts <- lineup_counts %>% select(Lineup, everything())
  
  return(lineup_counts)
}