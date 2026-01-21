#' Find Top K Lineups for a Single Simulation Using LP
#' 
#' Uses linear programming with diversity constraints to find the top K
#' distinct optimal lineups for a given simulation.
#'
#' @param candidates Data frame with candidate players for this sim
#' @param score_column Character. Column name for fantasy scores
#' @param roster_size Integer. Number of players in lineup
#' @param salary_cap Numeric. Maximum total salary
#' @param top_k Integer. Number of lineups to find
#' @param lineup_tracker Environment. Hash table to track lineups
#' @param sim_id Current simulation ID
#'
#' @keywords internal
find_top_k_lineups <- function(
  candidates,
  score_column,
  roster_size,
  salary_cap,
  top_k,
  lineup_tracker,
  sim_id
) {
  
  n_candidates <- nrow(candidates)
  
  # Need at least roster_size players
  if (n_candidates < roster_size) {
    return(NULL)
  }
  
  # Build base constraint matrix (reused for all k iterations)
  base_constraints <- build_constraint_matrix(
    salaries = candidates$Salary,
    roster_size = roster_size,
    salary_cap = salary_cap
  )
  
  # Track excluded lineup combinations for diversity
  excluded_combinations <- list()
  
  # Find k distinct lineups
  for (k in 1:top_k) {
    
    # Build constraint matrix with exclusions
    if (length(excluded_combinations) > 0) {
      constraints <- add_exclusion_constraints(
        base_constraints,
        excluded_combinations,
        n_candidates,
        roster_size
      )
    } else {
      constraints <- base_constraints
    }
    
    # Solve LP problem
    solution <- solve_lp_problem(
      objective = candidates[[score_column]],
      constraints = constraints
    )
    
    # Check if solution is valid
    if (is.null(solution) || solution$status != 0) {
      break  # No more valid lineups
    }
    
    # Get selected player indices
    selected_indices <- which(solution$solution > 0.9)
    
    # Validate roster size
    if (length(selected_indices) != roster_size) {
      break
    }
    
    # Create lineup key (sorted player names joined by "|")
    selected_players <- sort(candidates$Name[selected_indices])
    lineup_key <- paste(selected_players, collapse = "|")
    
    # Track this lineup in hash table
    track_lineup(
      lineup_tracker = lineup_tracker,
      lineup_key = lineup_key,
      players = selected_players,
      sim_id = sim_id,
      rank = k
    )
    
    # Add to exclusions for next iteration
    excluded_combinations[[length(excluded_combinations) + 1]] <- selected_indices
  }
  
  invisible(NULL)
}


#' Build Base Constraint Matrix for LP
#' @keywords internal
build_constraint_matrix <- function(salaries, roster_size, salary_cap) {
  
  n_players <- length(salaries)
  
  # Create constraint matrix
  # Row 1: Salary constraint
  # Row 2: Roster size constraint
  const_mat <- matrix(0, nrow = 2, ncol = n_players)
  const_mat[1, ] <- salaries
  const_mat[2, ] <- 1
  
  # Constraint directions
  const_dir <- c("<=", "==")
  
  # Constraint right-hand sides
  const_rhs <- c(salary_cap, roster_size)
  
  return(list(
    matrix = const_mat,
    directions = const_dir,
    rhs = const_rhs
  ))
}


#' Add Exclusion Constraints to Prevent Duplicate Lineups
#' @keywords internal
add_exclusion_constraints <- function(
  base_constraints,
  excluded_combinations,
  n_players,
  roster_size
) {
  
  n_exclusions <- length(excluded_combinations)
  n_constraints <- 2 + n_exclusions
  
  # Create expanded constraint matrix
  const_mat <- matrix(0, nrow = n_constraints, ncol = n_players)
  
  # Copy base constraints
  const_mat[1:2, ] <- base_constraints$matrix
  
  # Add exclusion constraints
  # For each excluded lineup, add constraint: sum(those players) <= roster_size - 1
  # This prevents picking that exact combination again
  for (i in 1:n_exclusions) {
    const_mat[2 + i, excluded_combinations[[i]]] <- 1
  }
  
  # Update directions and RHS
  const_dir <- c(
    base_constraints$directions,
    rep("<=", n_exclusions)
  )
  
  const_rhs <- c(
    base_constraints$rhs,
    rep(roster_size - 1, n_exclusions)
  )
  
  return(list(
    matrix = const_mat,
    directions = const_dir,
    rhs = const_rhs
  ))
}


#' Solve LP Problem
#' @keywords internal
solve_lp_problem <- function(objective, constraints) {
  
  require(lpSolve)
  
  result <- tryCatch({
    suppressWarnings(
      lp(
        direction = "max",
        objective.in = objective,
        const.mat = constraints$matrix,
        const.dir = constraints$directions,
        const.rhs = constraints$rhs,
        all.bin = TRUE,
        presolve = 0,
        compute.sens = 0
      )
    )
  }, error = function(e) {
    return(NULL)
  })
  
  return(result)
}


#' Track Lineup in Hash Table
#' @keywords internal
track_lineup <- function(
  lineup_tracker,
  lineup_key,
  players,
  sim_id,
  rank
) {
  
  # Check if lineup already exists
  if (exists(lineup_key, envir = lineup_tracker)) {
    
    # Update existing lineup
    existing <- get(lineup_key, envir = lineup_tracker)
    existing$sim_count <- existing$sim_count + 1
    existing$ranks <- c(existing$ranks, rank)
    existing$sim_ids <- c(existing$sim_ids, sim_id)
    
    assign(lineup_key, existing, envir = lineup_tracker)
    
  } else {
    
    # Create new lineup entry
    new_lineup <- list(
      players = players,
      sim_count = 1,
      ranks = rank,
      sim_ids = sim_id
    )
    
    assign(lineup_key, new_lineup, envir = lineup_tracker)
  }
  
  invisible(NULL)
}
