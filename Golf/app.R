# Load required packages
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
library(shinycssloaders)
library(shinyjs)

options(datatable.optimize = Inf)

# Set up custom CSS for app theme with black and gold color scheme
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
DK_SALARY_CAP <- 50000
FD_SALARY_CAP <- 60000

# Helper functions for calculating cut distributions
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

# Update the generate_cash_lineups function to include ownership metrics

generate_cash_lineups <- function(player_data, salary_cap, n_lineups = 50, platform = "dk") {
  # Prepare data
  players <- player_data$Golfer
  cut_probs <- player_data$CutPct
  
  # Select salaries and projections based on platform
  if (platform == "dk") {
    salaries <- player_data$DKSal
    std_proj <- player_data$DKProj
    ceil_proj <- player_data$DKCeil
    ownership <- if (max(player_data$DKOP, na.rm = TRUE) <= 1) {
      player_data$DKOP * 100
    } else {
      player_data$DKOP
    }
  } else {  # FanDuel
    salaries <- player_data$FDSal
    std_proj <- player_data$FDProj
    ceil_proj <- player_data$FDCeil
    ownership <- if (max(player_data$FDOP, na.rm = TRUE) <= 1) {
      player_data$FDOP * 100
    } else {
      player_data$FDOP
    }
  }
  
  n_players <- length(players)
  lineups <- list()
  
  # Track excluded lineups to avoid duplicates
  excluded_lineups <- matrix(0, nrow = 0, ncol = n_players)
  
  # Console progress setup
  platform_name <- if (platform == "dk") "DraftKings" else "FanDuel"
  cat("\n=== GENERATING", platform_name, "CASH LINEUPS (ECM) ===\n")
  cat("Players available:", n_players, "\n")
  cat("Target lineups:", n_lineups, "\n")
  cat("Salary cap:", format(salary_cap, big.mark = ","), "\n\n")
  
  for (lineup_num in 1:n_lineups) {
    # Show progress every 500 lineups
    if (lineup_num %% 500 == 0 || lineup_num == 1) {
      cat(
        sprintf(
          "[%s] Generating lineup %d/%d (%.1f%% complete)\n",
          format(Sys.time(), "%H:%M:%S"),
          lineup_num,
          n_lineups,
          (lineup_num / n_lineups) * 100
        )
      )
    }
    
    # Set up optimization
    objective <- cut_probs  # Maximize expected cuts
    
    # Constraints matrix
    constraints <- rbind(
      salaries,
      # Salary constraint
      rep(1, n_players),
      # Roster size constraint
      excluded_lineups            # Previously found lineups
    )
    
    # Constraint directions
    directions <- c("<=", "==", rep("<=", nrow(excluded_lineups)))
    
    # Right-hand side values
    rhs <- c(salary_cap, 6, rep(5, nrow(excluded_lineups)))  # Max 5 of 6 players from previous lineups
    
    # Solve
    result <- lp("max", objective, constraints, directions, rhs, all.bin = TRUE)
    
    if (result$status != 0) {
      cat("No more feasible lineups found at iteration",
          lineup_num,
          "\n")
      break
    }
    
    # Extract solution
    selected <- which(result$solution > 0.5)
    
    if (length(selected) != 6) {
      cat("Invalid lineup size at iteration", lineup_num, "\n")
      break
    }
    
    # Calculate lineup metrics
    lineup_players <- players[selected]
    lineup_cut_probs <- cut_probs[selected]
    lineup_salaries <- salaries[selected]
    lineup_std_proj <- std_proj[selected]
    lineup_ceil_proj <- ceil_proj[selected]
    lineup_ownership <- ownership[selected]
    
    expected_cuts <- sum(lineup_cut_probs)
    total_salary <- sum(lineup_salaries)
    total_std_proj <- sum(lineup_std_proj)
    total_ceil_proj <- sum(lineup_ceil_proj)
    
    # Calculate ownership metrics
    cumulative_ownership <- sum(lineup_ownership)
    geometric_mean_ownership <- exp(mean(log(pmax(lineup_ownership, 0.01))))  # Use 0.01 as minimum to avoid log(0)
    
    # Calculate cut distributions
    cut_dist <- calculate_cut_distribution(lineup_cut_probs)
    
    # Store lineup
    lineups[[lineup_num]] <- list(
      Player1 = lineup_players[1],
      Player2 = lineup_players[2],
      Player3 = lineup_players[3],
      Player4 = lineup_players[4],
      Player5 = lineup_players[5],
      Player6 = lineup_players[6],
      ExpectedCuts = expected_cuts,
      TotalSalary = total_salary,
      StandardProj = total_std_proj,
      CeilingProj = total_ceil_proj,
      CumulativeOwnership = cumulative_ownership,
      GeometricMeanOwnership = geometric_mean_ownership,
      Exactly0 = cut_dist$exact[1],
      Exactly1 = cut_dist$exact[2],
      Exactly2 = cut_dist$exact[3],
      Exactly3 = cut_dist$exact[4],
      Exactly4 = cut_dist$exact[5],
      Exactly5 = cut_dist$exact[6],
      Exactly6 = cut_dist$exact[7],
      AtLeast1 = cut_dist$atleast[2],
      AtLeast2 = cut_dist$atleast[3],
      AtLeast3 = cut_dist$atleast[4],
      AtLeast4 = cut_dist$atleast[5],
      AtLeast5 = cut_dist$atleast[6],
      AtLeast6 = cut_dist$atleast[7]
    )
    
    # Add this lineup to excluded list
    lineup_vector <- rep(0, n_players)
    lineup_vector[selected] <- 1
    excluded_lineups <- rbind(excluded_lineups, lineup_vector)
  }
  
  # Final summary
  final_count <- length(lineups)
  cat("\n=== CASH LINEUP GENERATION COMPLETED ===\n")
  cat("Platform:", platform_name, "\n")
  cat("Successfully generated:", final_count, "lineups\n")
  cat("Completion time:", format(Sys.time(), "%H:%M:%S"), "\n\n")
  
  # Convert to data frame
  if (length(lineups) > 0) {
    return(do.call(
      rbind,
      lapply(lineups, data.frame, stringsAsFactors = FALSE)
    ))
  } else {
    return(NULL)
  }
}

generate_mme_lineups <- function(player_data, salary_cap, n_lineups = 15000, platform = "dk") {
  # Prepare data
  players <- player_data$Golfer
  cut_probs <- player_data$CutPct
  
  # Select salaries and projections based on platform
  if (platform == "dk") {
    salaries <- player_data$DKSal
    std_proj <- player_data$DKProj
    ceil_proj <- player_data$DKCeil
    ownership <- if (max(player_data$DKOP, na.rm = TRUE) <= 1) {
      player_data$DKOP * 100
    } else {
      player_data$DKOP
    }
  } else {  # FanDuel
    salaries <- player_data$FDSal
    std_proj <- player_data$FDProj
    ceil_proj <- player_data$FDCeil
    ownership <- if (max(player_data$FDOP, na.rm = TRUE) <= 1) {
      player_data$FDOP * 100
    } else {
      player_data$FDOP
    }
  }
  
  n_players <- length(players)
  lineups <- list()
  
  # Console progress setup
  platform_name <- if (platform == "dk") "DraftKings" else "FanDuel"
  cat("\n=== GENERATING",
      platform_name,
      "MME LINEUPS (FAST 6-CUT FOCUSED) ===\n")
  cat("Players available:", n_players, "\n")
  cat("Target lineups:", n_lineups, "\n")
  cat("Salary cap:", format(salary_cap, big.mark = ","), "\n\n")
  
  # Create weighted sampling pools based on cut probability
  # Higher cut probability = higher chance of selection
  cut_weights <- cut_probs^2  # Square to emphasize high cut prob players
  
  # Track used lineups to ensure diversity
  used_lineups <- list()
  
  attempts <- 0
  max_attempts <- n_lineups * 3  # Safety valve
  
  while (length(lineups) < n_lineups && attempts < max_attempts) {
    attempts <- attempts + 1
    
    # Show progress every 500 lineups
    if (length(lineups) %% 500 == 0 && length(lineups) > 0) {
      cat(
        sprintf(
          "[%s] Generated %d/%d lineups (%.1f%% complete)\n",
          format(Sys.time(), "%H:%M:%S"),
          length(lineups),
          n_lineups,
          (length(lineups) / n_lineups) * 100
        )
      )
    }
    
    # Sample 6 players using weighted sampling
    selected_indices <- sample(n_players, 6, prob = cut_weights, replace = FALSE)
    
    # Check salary constraint
    total_salary <- sum(salaries[selected_indices])
    if (total_salary > salary_cap)
      next
    
    # Create lineup signature for uniqueness check
    lineup_sig <- paste(sort(players[selected_indices]), collapse = "|")
    if (lineup_sig %in% used_lineups)
      next
    
    # Calculate lineup metrics
    lineup_players <- players[selected_indices]
    lineup_cut_probs <- cut_probs[selected_indices]
    lineup_salaries <- salaries[selected_indices]
    lineup_std_proj <- std_proj[selected_indices]
    lineup_ceil_proj <- ceil_proj[selected_indices]
    lineup_ownership <- ownership[selected_indices]
    
    expected_cuts <- sum(lineup_cut_probs)
    total_std_proj <- sum(lineup_std_proj)
    total_ceil_proj <- sum(lineup_ceil_proj)
    
    # Calculate ownership metrics
    cumulative_ownership <- sum(lineup_ownership)
    geometric_mean_ownership <- exp(mean(log(pmax(lineup_ownership, 0.01))))  # Use 0.01 as minimum to avoid log(0)
    
    # Calculate cut distributions
    cut_dist <- calculate_cut_distribution(lineup_cut_probs)
    
    # Store lineup
    lineup_num <- length(lineups) + 1
    lineups[[lineup_num]] <- list(
      Player1 = lineup_players[1],
      Player2 = lineup_players[2],
      Player3 = lineup_players[3],
      Player4 = lineup_players[4],
      Player5 = lineup_players[5],
      Player6 = lineup_players[6],
      ExpectedCuts = expected_cuts,
      TotalSalary = total_salary,
      StandardProj = total_std_proj,
      CeilingProj = total_ceil_proj,
      CumulativeOwnership = cumulative_ownership,
      GeometricMeanOwnership = geometric_mean_ownership,
      Exactly0 = cut_dist$exact[1],
      Exactly1 = cut_dist$exact[2],
      Exactly2 = cut_dist$exact[3],
      Exactly3 = cut_dist$exact[4],
      Exactly4 = cut_dist$exact[5],
      Exactly5 = cut_dist$exact[6],
      Exactly6 = cut_dist$exact[7],
      AtLeast1 = cut_dist$atleast[2],
      AtLeast2 = cut_dist$atleast[3],
      AtLeast3 = cut_dist$atleast[4],
      AtLeast4 = cut_dist$atleast[5],
      AtLeast5 = cut_dist$atleast[6],
      AtLeast6 = cut_dist$atleast[7]
    )
    
    # Mark as used
    used_lineups[[length(used_lineups) + 1]] <- lineup_sig
  }
  
  # Convert to data frame and sort by 6-cut probability
  if (length(lineups) > 0) {
    lineup_df <- do.call(rbind,
                         lapply(lineups, data.frame, stringsAsFactors = FALSE))
    lineup_df <- lineup_df[order(lineup_df$AtLeast6, decreasing = TRUE), ]
    
    # Final summary
    final_count <- nrow(lineup_df)
    cat("\n=== MME LINEUP GENERATION COMPLETED ===\n")
    cat("Platform:", platform_name, "\n")
    cat("Successfully generated:", final_count, "lineups\n")
    cat("Total attempts:", attempts, "\n")
    cat("Success rate:", sprintf("%.1f%%", (final_count / attempts) * 100), "\n")
    cat("Completion time:", format(Sys.time(), "%H:%M:%S"), "\n\n")
    
    return(lineup_df)
  } else {
    cat("No valid lineups generated\n")
    return(NULL)
  }
}

generate_filtered_mme_lineups <- function(optimal_lineups, filters, mme_data = NULL) {
  setDT(optimal_lineups)
  filtered_lineups <- copy(optimal_lineups)
  
  # Apply filters
  if (!is.null(filters$min_expected_cuts) &&
      filters$min_expected_cuts > 0) {
    filtered_lineups <- filtered_lineups[ExpectedCuts >= filters$min_expected_cuts]
  }
  
  if (!is.null(filters$min_standard_proj) &&
      filters$min_standard_proj > 0) {
    filtered_lineups <- filtered_lineups[StandardProj >= filters$min_standard_proj]
  }
  
  if (!is.null(filters$min_ceiling_proj) &&
      filters$min_ceiling_proj > 0) {
    filtered_lineups <- filtered_lineups[CeilingProj >= filters$min_ceiling_proj]
  }
  
  if (!is.null(filters$min_6_pct) && filters$min_6_pct > 0) {
    filtered_lineups <- filtered_lineups[AtLeast6 >= filters$min_6_pct / 100]
  }
  
  if (!is.null(filters$min_5plus_pct) &&
      filters$min_5plus_pct > 0) {
    filtered_lineups <- filtered_lineups[AtLeast5 >= filters$min_5plus_pct /
                                           100]
  }
  
  if (!is.null(filters$min_4plus_pct) &&
      filters$min_4plus_pct > 0) {
    filtered_lineups <- filtered_lineups[AtLeast4 >= filters$min_4plus_pct /
                                           100]
  }
  
  # Apply ownership range filters
  if (!is.null(filters$cumulative_ownership_range) && 
      length(filters$cumulative_ownership_range) == 2) {
    filtered_lineups <- filtered_lineups[
      CumulativeOwnership >= filters$cumulative_ownership_range[1] &
        CumulativeOwnership <= filters$cumulative_ownership_range[2]
    ]
  }
  
  if (!is.null(filters$geometric_mean_ownership_range) && 
      length(filters$geometric_mean_ownership_range) == 2) {
    filtered_lineups <- filtered_lineups[
      GeometricMeanOwnership >= filters$geometric_mean_ownership_range[1] &
        GeometricMeanOwnership <= filters$geometric_mean_ownership_range[2]
    ]
  }
  
  # Apply golfer exclusion filter
  if (!is.null(filters$excluded_golfers) &&
      length(filters$excluded_golfers) > 0) {
    player_cols <- c("Player1",
                     "Player2",
                     "Player3",
                     "Player4",
                     "Player5",
                     "Player6")
    
    for (excluded_golfer in filters$excluded_golfers) {
      # Create a condition that checks if the excluded golfer is NOT in any position
      exclude_condition <- rep(TRUE, nrow(filtered_lineups))
      
      for (col in player_cols) {
        exclude_condition <- exclude_condition &
          (filtered_lineups[[col]] != excluded_golfer)
      }
      
      # Keep only lineups where the excluded golfer is not present
      filtered_lineups <- filtered_lineups[exclude_condition]
    }
  }
  
  
  # Apply wave filtering if wave data is available
  if (!is.null(filters$min_early_late) &&
      !is.null(filters$max_early_late) && !is.null(mme_data)) {
    if ("Wave" %in% names(mme_data)) {
      # Create wave lookup
      wave_lookup <- setNames(mme_data$Wave, mme_data$Golfer)
      
      # Count EarlyLate players for each lineup
      player_cols <- c("Player1",
                       "Player2",
                       "Player3",
                       "Player4",
                       "Player5",
                       "Player6")
      
      early_late_counts <- apply(filtered_count[, player_cols, drop = FALSE], 1, function(lineup) {
        player_waves <- wave_lookup[lineup]
        sum(player_waves == "EarlyLate", na.rm = TRUE)
      })
      
      # Filter based on EarlyLate count range
      wave_condition <- early_late_counts >= filters$min_early_late &
        early_late_counts <= filters$max_early_late
      filtered_lineups <- filtered_lineups[wave_condition]
    }
  }
  
  # Check if any lineups match filters
  if (nrow(filtered_lineups) == 0) {
    return(NULL)
  }
  
  # Sample lineups using AtLeast6 as weight for randomization
  selected_lineups <- data.table()
  selected_indices <- integer(0)
  
  # Sampling with minimal attempt tracking
  attempts <- 0
  max_attempts <- filters$num_lineups * 10
  
  while (nrow(selected_lineups) < filters$num_lineups &&
         attempts < max_attempts) {
    attempts <- attempts + 1
    
    # Available lineups
    available_indices <- setdiff(1:nrow(filtered_lineups), selected_indices)
    if (length(available_indices) == 0)
      break
    
    # Sample based on AtLeast6 weights
    weights <- filtered_lineups$AtLeast6[available_indices]
    if (sum(weights) == 0)
      weights <- rep(1, length(available_indices))
    
    selected_idx <- tryCatch({
      sample(available_indices, 1, prob = weights)
    }, error = function(e) {
      sample(available_indices, 1)
    })
    
    # Add lineup immediately
    selected_lineups <- rbind(selected_lineups, filtered_lineups[selected_idx])
    selected_indices <- c(selected_indices, selected_idx)
  }
  
  if (nrow(selected_lineups) == 0)
    return(NULL)
  
  return(as.data.frame(selected_lineups))
}

calculate_exposures <- function(lineups, mme_data, platform) {
  if (is.null(lineups) || nrow(lineups) == 0)
    return(NULL)
  
  # Get all unique players from lineups
  player_cols <- c("Player1",
                   "Player2",
                   "Player3",
                   "Player4",
                   "Player5",
                   "Player6")
  
  # Handle both data.frame and data.table
  if (is.data.table(lineups)) {
    all_lineup_players <- unlist(lineups[, player_cols, with = FALSE])
  } else {
    all_lineup_players <- unlist(lineups[, player_cols])
  }
  
  # Calculate exposure counts
  exposure_counts <- table(all_lineup_players)
  exposure_pct <- (exposure_counts / nrow(lineups)) * 100
  
  # Get ownership projections based on platform
  ownership_col <- if (platform == "dk")
    "DKOP"
  else
    "FDOP"
  
  # Create exposure data frame
  exposure_df <- data.frame(
    Golfer = names(exposure_pct),
    Exposure_Count = as.numeric(exposure_counts),
    Exposure_Pct = as.numeric(exposure_pct),
    stringsAsFactors = FALSE
  )
  
  # Add ownership projections
  ownership_lookup <- setNames(mme_data[[ownership_col]], mme_data$Golfer)
  exposure_df$Ownership_Proj <- ownership_lookup[exposure_df$Golfer]
  
  # Sort by exposure percentage descending
  exposure_df <- exposure_df[order(exposure_df$Exposure_Pct, decreasing = TRUE), ]
  
  return(exposure_df)
}

# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(title = "Golf DFS Optimizer"),
  
  # Dashboard sidebar
  dashboardSidebar(
    useShinyjs(),
    div(
      style = "text-align: center; padding: 10px; margin-bottom: 5px;",
      tags$img(
        src = "logo.jpg",
        height = "200px",
        width = "auto",
        style = "border: 2px solid #FFD700; border-radius: 10px;"
      )
    ),
    sidebarMenu(
      id = "sidebar_menu",
      menuItem(
        "Cut Analysis",
        tabName = "cut_analysis",
        icon = icon("chart-line")
      ),
      menuItem(
        "Cash Lineups",
        tabName = "cash",
        icon = icon("dollar-sign")
      ),
      menuItem(
        "DraftKings Builder",
        tabName = "dk_builder",
        icon = icon("percentage")
      ),
      menuItem(
        "FanDuel Builder",
        tabName = "fd_builder",
        icon = icon("percentage")
      )
    ),
    br(),
    fileInput("golf_file", "Upload Golf File", accept = c(".xlsx"))
  ),
  
  # Dashboard body
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    
    tabItems(
      # Cut Analysis Tab
      tabItem(tabName = "cut_analysis", fluidRow(
        box(
          width = 12,
          title = "Cut Probability Analysis",
          fluidRow(column(
            12,
            radioButtons(
              "platform_toggle",
              "Platform:",
              choices = c("DraftKings" = "dk", "FanDuel" = "fd"),
              selected = "dk",
              inline = TRUE
            )
          )),
          DTOutput("cut_analysis_table") %>%
            withSpinner(color = "#FFD700")
        )
      ), fluidRow(
        box(
          width = 12,
          title = "Cut Probability vs Salary",
          plotlyOutput("cut_salary_plot") %>%
            withSpinner(color = "#FFD700")
        )
      )),
      
      # Cash Lineups Tab
      tabItem(
        tabName = "cash",
        fluidRow(
          box(
            width = 12,
            title = "Generate Cash Lineups",
            status = "primary",
            solidHeader = TRUE,
            actionButton(
              "generate_cash",
              "Generate Cash Lineups",
              class = "btn-primary",
              style = "width: 100%; margin-top: 10px; padding: 15px; font-size: 16px;"
            )
          )
        ),
        
        # Cash Lineup Results with Platform Toggle
        conditionalPanel(condition = "output.cash_complete === 'true'", fluidRow(
          box(
            width = 12,
            title = "Top 50 Cash Lineups (Expected Cuts Optimized)",
            fluidRow(column(
              6,
              radioButtons(
                "cash_platform_toggle",
                "Platform:",
                choices = c("DraftKings" = "dk", "FanDuel" = "fd"),
                selected = "dk",
                inline = TRUE
              )
            ), column(
              6,
              div(
                style = "text-align: right; margin-top: 10px;",
                downloadButton('download_cash', 'Download Lineups', style = "margin-top: 10px;")
              )
            )),
            DTOutput("cash_lineups_table") %>% withSpinner(color = "#FFD700")
          )
        ))
      ),
      # DraftKings Builder Tab
      tabItem(
        tabName = "dk_builder",
        conditionalPanel(condition = "!output.mme_loaded", fluidRow(
          box(
            width = 12,
            status = "warning",
            title = "No MME Pool Available",
            "Please upload an MME pool file using the sidebar."
          )
        )),
        conditionalPanel(
          condition = "output.mme_loaded === 'true'",
          fluidRow(
            box(
              width = 12,
              title = "Calculate DraftKings MME Lineups",
              status = "primary",
              solidHeader = TRUE,
              actionButton(
                "run_dk_mme_optimization",
                "Calculate DraftKings MME Lineups",
                class = "btn-primary",
                style = "width: 100%; margin-top: 10px; padding: 15px; font-size: 16px;"
              )
            )
          ),
          
          # DK MME lineups results section
          conditionalPanel(condition = "output.dk_mme_complete === 'true'", fluidRow(
            box(
              width = 12,
              title = "DraftKings Lineup Filters",
              # Row 1: Expected Cuts and Projection Filters
              fluidRow(
                column(
                  3,
                  sliderInput(
                    "dk_min_expected_cuts",
                    "Min Expected Cuts:",
                    min = 0,
                    max = 6,
                    value = 0,
                    step = 0.01
                  )
                ),
                column(
                  3,
                  sliderInput(
                    "dk_min_standard_proj",
                    "Min Standard Proj:",
                    min = 0,
                    max = 500,
                    value = 0,
                    step = 0.1
                  )
                ),
                column(
                  3,
                  sliderInput(
                    "dk_min_ceiling_proj",
                    "Min Ceiling Proj:",
                    min = 0,
                    max = 500,
                    value = 0,
                    step = 0.1
                  )
                ),
                column(
                  3,
                  sliderInput(
                    "dk_min_6_pct",
                    "Min 6-Cut %:",
                    min = 0,
                    max = 100,
                    value = 0,
                    step = 0.01
                  )
                )
              ),
              # Row 2: Cut Percentage and Wave Filters
              fluidRow(
                column(
                  2,
                  sliderInput(
                    "dk_min_5plus_pct",
                    "Min 5+ Cut %:",
                    min = 0,
                    max = 100,
                    value = 0,
                    step = 0.01
                  )
                ),
                column(
                  2,
                  sliderInput(
                    "dk_min_4plus_pct",
                    "Min 4+ Cut %:",
                    min = 0,
                    max = 100,
                    value = 0,
                    step = 0.01
                  )
                ),
                column(
                  2,
                  sliderInput(
                    "dk_min_early_late",
                    "Min EarlyLate:",
                    min = 0,
                    max = 6,
                    value = 0,
                    step = 1
                  )
                ),
                column(
                  2,
                  sliderInput(
                    "dk_max_early_late",
                    "Max EarlyLate:",
                    min = 0,
                    max = 6,
                    value = 6,
                    step = 1
                  )
                ),
                column(
                  2,
                  selectizeInput(
                    "dk_excluded_golfers",
                    "Exclude Golfers:",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(plugins = list('remove_button'))
                  )
                ),
                column(
                  2,
                  numericInput(
                    "dk_num_mme_lineups",
                    "Number of Lineups:",
                    value = 20,
                    min = 1,
                    max = 150
                  )
                )
              ),
              # Row 3: Ownership Range Filters
              fluidRow(
                column(
                  4,
                  sliderInput(
                    "dk_cumulative_ownership_range",
                    "Cumulative Ownership Range:",
                    min = 0,
                    max = 600,
                    value = c(0, 600),
                    step = 1
                  )
                ),
                column(
                  4,
                  sliderInput(
                    "dk_geometric_mean_ownership_range",
                    "Geometric Mean Ownership Range:",
                    min = 0,
                    max = 100,
                    value = c(0, 100),
                    step = 0.1
                  )
                ),
                column(
                  4,
                  div(style = "height: 75px;")  # Spacing column
                )
              ),
              # Row 4: Pool Stats and Generate Button
              fluidRow(column(
                6, div(
                  class = "well well-sm",
                  h4("Filtered Pool Statistics:"),
                  textOutput("dk_filtered_mme_pool_size")
                )
              ), column(
                6, div(
                  style = "margin-top: 20px;",
                  actionButton(
                    "generate_dk_mme_lineups",
                    "Generate DraftKings Lineups",
                    class = "btn-primary btn-lg",
                    style = "width: 100%; margin-top: 5px;"
                  )
                )
              ))
            )
          ), fluidRow(
            box(
              width = 12,
              title = "DraftKings Player Exposures",
              DTOutput("dk_exposure_table") %>% withSpinner(color = "#FFD700")
            )
          ), fluidRow(
            box(
              width = 12,
              title = "Generated DraftKings MME Lineups",
              fluidRow(column(
                6, h4("Lineup Display", style = "margin-top: 10px;")
              ), column(
                6,
                div(
                  style = "text-align: right; margin-top: 10px;",
                  downloadButton("download_dk_mme_lineups", "Download Top 15k Lineups", style = "margin-right: 10px;"),
                  downloadButton("download_generated_dk_mme", "Download Generated Lineups")
                )
              )),
              DTOutput("generated_dk_mme_lineups_table") %>% withSpinner(color = "#FFD700")
            )
          ))
        )
      ),
      
      # FanDuel Builder Tab
      tabItem(
        tabName = "fd_builder",
        conditionalPanel(condition = "!output.mme_loaded", fluidRow(
          box(
            width = 12,
            status = "warning",
            title = "No MME Pool Available",
            "Please upload an MME pool file using the sidebar."
          )
        )),
        conditionalPanel(
          condition = "output.mme_loaded === 'true'",
          fluidRow(
            box(
              width = 12,
              title = "Calculate FanDuel MME Lineups",
              status = "primary",
              solidHeader = TRUE,
              actionButton(
                "run_fd_mme_optimization",
                "Calculate FanDuel MME Lineups",
                class = "btn-primary",
                style = "width: 100%; margin-top: 10px; padding: 15px; font-size: 16px;"
              )
            )
          ),
          
          # FD MME lineups results section
          conditionalPanel(condition = "output.fd_mme_complete === 'true'", fluidRow(
            box(
              width = 12,
              title = "FanDuel Lineup Filters",
              # Row 1: Expected Cuts and Projection Filters
              fluidRow(
                column(
                  3,
                  sliderInput(
                    "fd_min_expected_cuts",
                    "Min Expected Cuts:",
                    min = 0,
                    max = 6,
                    value = 0,
                    step = 0.01
                  )
                ),
                column(
                  3,
                  sliderInput(
                    "fd_min_standard_proj",
                    "Min Standard Proj:",
                    min = 0,
                    max = 500,
                    value = 0,
                    step = 0.1
                  )
                ),
                column(
                  3,
                  sliderInput(
                    "fd_min_ceiling_proj",
                    "Min Ceiling Proj:",
                    min = 0,
                    max = 500,
                    value = 0,
                    step = 0.1
                  )
                ),
                column(
                  3,
                  sliderInput(
                    "fd_min_6_pct",
                    "Min 6-Cut %:",
                    min = 0,
                    max = 100,
                    value = 0,
                    step = 0.01
                  )
                )
              ),
              # Row 2: Cut Percentage and Wave Filters
              fluidRow(
                column(
                  2,
                  sliderInput(
                    "fd_min_5plus_pct",
                    "Min 5+ Cut %:",
                    min = 0,
                    max = 100,
                    value = 0,
                    step = 0.01
                  )
                ),
                column(
                  2,
                  sliderInput(
                    "fd_min_4plus_pct",
                    "Min 4+ Cut %:",
                    min = 0,
                    max = 100,
                    value = 0,
                    step = 0.01
                  )
                ),
                column(
                  2,
                  sliderInput(
                    "fd_min_early_late",
                    "Min EarlyLate:",
                    min = 0,
                    max = 6,
                    value = 0,
                    step = 1
                  )
                ),
                column(
                  2,
                  sliderInput(
                    "fd_max_early_late",
                    "Max EarlyLate:",
                    min = 0,
                    max = 6,
                    value = 6,
                    step = 1
                  )
                ),
                column(
                  2,
                  selectizeInput(
                    "fd_excluded_golfers",
                    "Exclude Golfers:",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(plugins = list('remove_button'))
                  )
                ),
                column(
                  2,
                  numericInput(
                    "fd_num_mme_lineups",
                    "Number of Lineups:",
                    value = 20,
                    min = 1,
                    max = 150
                  )
                )
              ),
              # Row 3: Ownership Range Filters
              fluidRow(
                column(
                  4,
                  sliderInput(
                    "fd_cumulative_ownership_range",
                    "Cumulative Ownership Range:",
                    min = 0,
                    max = 600,
                    value = c(0, 600),
                    step = 1
                  )
                ),
                column(
                  4,
                  sliderInput(
                    "fd_geometric_mean_ownership_range",
                    "Geometric Mean Ownership Range:",
                    min = 0,
                    max = 100,
                    value = c(0, 100),
                    step = 0.1
                  )
                ),
                column(
                  4,
                  div(style = "height: 75px;")  # Spacing column
                )
              ),
              # Row 4: Pool Stats and Generate Button
              fluidRow(column(
                6, div(
                  class = "well well-sm",
                  h4("Filtered Pool Statistics:"),
                  textOutput("fd_filtered_mme_pool_size")
                )
              ), column(
                6, div(
                  style = "margin-top: 20px;",
                  actionButton(
                    "generate_fd_mme_lineups",
                    "Generate FanDuel Lineups",
                    class = "btn-primary btn-lg",
                    style = "width: 100%; margin-top: 5px;"
                  )
                )
              ))
            )
          ), fluidRow(
            box(
              width = 12,
              title = "FanDuel Player Exposures",
              DTOutput("fd_exposure_table") %>% withSpinner(color = "#FFD700")
            )
          ), fluidRow(
            box(
              width = 12,
              title = "Generated FanDuel MME Lineups",
              fluidRow(column(
                6, h4("Lineup Display", style = "margin-top: 10px;")
              ), column(
                6,
                div(
                  style = "text-align: right; margin-top: 10px;",
                  downloadButton("download_fd_mme_lineups", "Download Top 15k Lineups", style = "margin-right: 10px;"),
                  downloadButton("download_generated_fd_mme", "Download Generated Lineups")
                )
              )),
              DTOutput("generated_fd_mme_lineups_table") %>% withSpinner(color = "#FFD700")
            )
          ))
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store data
  rv <- reactiveValues(
    cash_data = NULL,
    mme_data = NULL,
    golf_data_display = NULL,
    dk_cash_lineups = NULL,
    fd_cash_lineups = NULL,
    dk_mme_lineups = NULL,
    fd_mme_lineups = NULL,
    generated_dk_mme_lineups = NULL,
    generated_fd_mme_lineups = NULL,
    file_uploaded = FALSE,
    cash_complete = FALSE,
    mme_loaded = FALSE,
    dk_mme_complete = FALSE,
    fd_mme_complete = FALSE
  )
  
  # Expose reactive values as outputs
  output$cash_complete <- reactive({
    result <- tolower(as.character(
      !is.null(rv$dk_cash_lineups) && !is.null(rv$fd_cash_lineups)
    ))
    return(result)
  })
  outputOptions(output, "cash_complete", suspendWhenHidden = FALSE)
  
  output$mme_loaded <- reactive({
    result <- tolower(as.character(!is.null(rv$mme_data)))
    return(result)
  })
  outputOptions(output, "mme_loaded", suspendWhenHidden = FALSE)
  
  output$dk_mme_complete <- reactive({
    result <- tolower(as.character(!is.null(rv$dk_mme_lineups)))
    return(result)
  })
  outputOptions(output, "dk_mme_complete", suspendWhenHidden = FALSE)
  
  output$fd_mme_complete <- reactive({
    result <- tolower(as.character(!is.null(rv$fd_mme_lineups)))
    return(result)
  })
  outputOptions(output, "fd_mme_complete", suspendWhenHidden = FALSE)
  
  # Replace the existing observeEvent(input$golf_file, {}) section with this:
  
  observeEvent(input$golf_file, {
    req(input$golf_file)
    
    tryCatch({
      # Read the single PlayerPool tab
      player_pool <- read_excel(input$golf_file$datapath, sheet = "PlayerPool")
      
      # Filter for DraftKings cash pool (DKCashPool == "Y")
      dk_cash_data <- player_pool %>%
        filter(DKCashPool == "Y")
      
      # Filter for FanDuel cash pool (FDCashPool == "Y") 
      fd_cash_data <- player_pool %>%
        filter(FDCashPool == "Y")

      
      # Store all versions
      rv$dk_cash_data <- dk_cash_data  # DK-specific for cash generation
      rv$fd_cash_data <- fd_cash_data  # FD-specific for cash generation
      rv$mme_data <- player_pool  # Full pool for MME
      rv$golf_data_display <- player_pool  # Full pool for cut analysis
      rv$file_uploaded <- TRUE
      rv$mme_loaded <- TRUE
      
      # Reset cash lineups
      rv$dk_cash_lineups <- NULL
      rv$fd_cash_lineups <- NULL
      rv$cash_complete <- FALSE
      
      # Reset MME lineups
      rv$dk_mme_lineups <- NULL
      rv$fd_mme_lineups <- NULL
      rv$generated_dk_mme_lineups <- NULL
      rv$generated_fd_mme_lineups <- NULL
      rv$dk_mme_complete <- FALSE
      rv$fd_mme_complete <- FALSE
      
      showModal(
        modalDialog(
          title = "Success",
          sprintf("Golf data uploaded successfully!\nDK Cash Pool: %d players\nFD Cash Pool: %d players\nTotal MME Pool: %d players", 
                  nrow(dk_cash_data), 
                  nrow(fd_cash_data), 
                  nrow(player_pool)),
          easyClose = TRUE
        )
      )
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Error reading golf file:", e$message),
        easyClose = TRUE
      ))
    })
  })
  
  # Cut analysis table with platform toggle
  output$cut_analysis_table <- renderDT({
    req(rv$mme_pool, input$platform_toggle)
    
    # Select columns based on platform
    if (input$platform_toggle == "dk") {
      analysis_data <- rv$mme_pool %>%
        arrange(desc(CutPct)) %>%
        select(Golfer, CutPct, DKSal, DKProj, DKCeil, DKOP) %>%
        rename(
          Salary = DKSal,
          Projection = DKProj,
          Ceiling = DKCeil,
          Ownership = DKOP
        )
    } else {
      analysis_data <- rv$mme_pool %>%
        arrange(desc(CutPct)) %>%
        select(Golfer, CutPct, FDSal, FDProj, FDCeil, FDOP) %>%
        rename(
          Salary = FDSal,
          Projection = FDProj,
          Ceiling = FDCeil,
          Ownership = FDOP
        )
    }
    
    datatable(
      analysis_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "tip",
        ordering = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Cut probability vs salary plot
  output$cut_salary_plot <- renderPlotly({
    req(rv$golf_data_display, input$platform_toggle)
    
    # Prepare data based on platform
    if (input$platform_toggle == "dk") {
      plot_data <- rv$golf_data_display %>%
        select(Golfer, CutPct, DKSal, DKOP, DKCeil) %>%
        rename(Salary = DKSal,
               Ownership = DKOP,
               Ceiling = DKCeil)
      title_platform <- "DraftKings"
    } else {
      plot_data <- rv$golf_data_display %>%
        select(Golfer, CutPct, FDSal, FDOP, FDCeil) %>%
        rename(Salary = FDSal,
               Ownership = FDOP,
               Ceiling = FDCeil)
      title_platform <- "FanDuel"
    }
    
    # Create plot
    p <- ggplot(plot_data, aes(x = CutPct, y = Salary, text = Golfer)) +
      geom_point(aes(size = Ownership, color = Ceiling)) +
      geom_smooth(method = "lm",
                  se = FALSE,
                  color = "darkgrey") +
      scale_color_gradient(low = "red",
                           high = "green",
                           name = "Ceiling Proj") +
      scale_size(name = "Ownership %") +
      scale_y_continuous(labels = scales::dollar) +
      scale_x_continuous(labels = scales::percent) +
      labs(
        title = paste("Cut Probability vs Salary -", title_platform),
        x = "Cut Probability",
        y = "Salary ($)"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = c("text", "x", "y", "size", "color"))
  })
  

  
  # Update the download handler for cash lineups to use the correct data source:
  
  output$download_cash <- downloadHandler(
    filename = function() {
      platform <- if (input$cash_platform_toggle == "dk")
        "DraftKings"
      else
        "FanDuel"
      paste(
        "golf_cash_lineups_",
        platform,
        "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      # Select data and source based on platform
      if (input$cash_platform_toggle == "dk") {
        lineup_data <- rv$dk_cash_lineups
        platform_id_col <- "DKID"
        source_data <- rv$dk_cash_data  # Use DK-specific cash data for ID lookup
      } else {
        lineup_data <- rv$fd_cash_lineups
        platform_id_col <- "FDID"
        source_data <- rv$fd_cash_data  # Use FD-specific cash data for ID lookup
      }
      
      # Add EarlyLate count column if Wave data is available
      if (!is.null(source_data) && "Wave" %in% names(source_data)) {
        wave_lookup <- setNames(source_data$Wave, source_data$Golfer)
        player_cols <- c("Player1", "Player2", "Player3", "Player4", "Player5", "Player6")
        
        lineup_data$EarlyLate_Count <- apply(lineup_data[, player_cols], 1, function(lineup) {
          player_waves <- wave_lookup[lineup]
          sum(player_waves == "EarlyLate", na.rm = TRUE)
        })
      }
      
      # Create ID lookup from appropriate cash data source
      id_lookup <- setNames(source_data[[platform_id_col]], source_data$Golfer)
      
      # Replace player names with IDs and select columns
      if ("EarlyLate_Count" %in% names(lineup_data)) {
        download_data <- lineup_data %>%
          select(
            Player1, Player2, Player3, Player4, Player5, Player6,
            ExpectedCuts, StandardProj, CeilingProj, TotalSalary,
            EarlyLate_Count, AtLeast6, AtLeast5, AtLeast4
          ) %>%
          mutate(
            Player1 = id_lookup[Player1],
            Player2 = id_lookup[Player2],
            Player3 = id_lookup[Player3],
            Player4 = id_lookup[Player4],
            Player5 = id_lookup[Player5],
            Player6 = id_lookup[Player6]
          ) %>%
          rename(
            "AM/PM Count" = EarlyLate_Count,
            "6" = AtLeast6, "5+" = AtLeast5, "4+" = AtLeast4
          )
      } else {
        download_data <- lineup_data %>%
          select(
            Player1, Player2, Player3, Player4, Player5, Player6,
            ExpectedCuts, StandardProj, CeilingProj, TotalSalary,
            AtLeast6, AtLeast5, AtLeast4
          ) %>%
          mutate(
            Player1 = id_lookup[Player1],
            Player2 = id_lookup[Player2],
            Player3 = id_lookup[Player3],
            Player4 = id_lookup[Player4],
            Player5 = id_lookup[Player5],
            Player6 = id_lookup[Player6]
          ) %>%
          rename("6" = AtLeast6, "5+" = AtLeast5, "4+" = AtLeast4)
      }
      
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  
  observeEvent(input$generate_cash, {
    req(rv$dk_cash_data, rv$fd_cash_data)
    
    withProgress(message = 'Generating cash lineups...', value = 0, {
      # Generate DraftKings lineups using DK-specific pool and platform
      incProgress(0.3, detail = "Generating DraftKings lineups...")
      rv$dk_cash_lineups <- generate_cash_lineups(rv$dk_cash_data, DK_SALARY_CAP, 50, "dk")
      
      # Generate FanDuel lineups using FD-specific pool and platform
      incProgress(0.6, detail = "Generating FanDuel lineups...")
      rv$fd_cash_lineups <- generate_cash_lineups(rv$fd_cash_data, FD_SALARY_CAP, 50, "fd")
      
      rv$cash_complete <- TRUE
      
      incProgress(1, detail = "Complete!")
      
      # Show success message
      showModal(modalDialog(
        title = "Success",
        sprintf(
          "Generated %d DraftKings and %d FanDuel cash lineups!",
          ifelse(
            is.null(rv$dk_cash_lineups),
            0,
            nrow(rv$dk_cash_lineups)
          ),
          ifelse(
            is.null(rv$fd_cash_lineups),
            0,
            nrow(rv$fd_cash_lineups)
          )
        ),
        easyClose = TRUE
      ))
    })
  })
  
  observeEvent(input$run_dk_mme_optimization, {
    req(rv$mme_data)
    
    withProgress(message = 'Generating DraftKings MME lineups...', value = 0, {
      # Start timing for DK
      dk_start_time <- Sys.time()
      cat("\n=== MME DRAFTKINGS LINEUP GENERATION STARTED ===\n")
      cat("Start time:",
          format(dk_start_time, "%Y-%m-%d %H:%M:%S"),
          "\n")
      cat("Target lineups: 15,000\n")
      cat("Players available:", nrow(rv$mme_data), "\n\n")
      
      rv$dk_mme_lineups <- generate_mme_lineups(rv$mme_data, DK_SALARY_CAP, 15000, "dk")
      
      dk_end_time <- Sys.time()
      dk_elapsed <- difftime(dk_end_time, dk_start_time, units = "mins")
      cat("=== DRAFTKINGS COMPLETED ===\n")
      cat("Total time:", sprintf("%.2f minutes", as.numeric(dk_elapsed)), "\n")
      cat("Generated:", ifelse(is.null(rv$dk_mme_lineups), 0, nrow(rv$dk_mme_lineups)), "lineups\n\n")
      
      rv$dk_mme_complete <- TRUE
      
      # Update excluded golfers dropdown
      if (!is.null(rv$mme_data)) {
        all_golfers <- unique(rv$mme_data$Golfer)
        updateSelectizeInput(session,
                             "dk_excluded_golfers",
                             choices = all_golfers,
                             selected = NULL)
      }
      
      # Show success message
      showModal(modalDialog(
        title = "Success",
        sprintf(
          "Generated %d DraftKings MME lineups for filtering!\n\nTotal time: %.2f minutes",
          ifelse(is.null(rv$dk_mme_lineups), 0, nrow(rv$dk_mme_lineups)),
          as.numeric(dk_elapsed)
        ),
        easyClose = TRUE
      ))
    })
  })
  
  
  observeEvent(input$run_fd_mme_optimization, {
    req(rv$mme_data)
    
    withProgress(message = 'Generating FanDuel MME lineups...', value = 0, {
      # Start timing for FD
      fd_start_time <- Sys.time()
      cat("\n=== MME FANDUEL LINEUP GENERATION STARTED ===\n")
      cat("Start time:",
          format(fd_start_time, "%Y-%m-%d %H:%M:%S"),
          "\n")
      cat("Target lineups: 15,000\n")
      cat("Players available:", nrow(rv$mme_data), "\n\n")
      
      rv$fd_mme_lineups <- generate_mme_lineups(rv$mme_data, FD_SALARY_CAP, 15000, "fd")
      
      fd_end_time <- Sys.time()
      fd_elapsed <- difftime(fd_end_time, fd_start_time, units = "mins")
      cat("=== FANDUEL COMPLETED ===\n")
      cat("Total time:", sprintf("%.2f minutes", as.numeric(fd_elapsed)), "\n")
      cat("Generated:", ifelse(is.null(rv$fd_mme_lineups), 0, nrow(rv$fd_mme_lineups)), "lineups\n\n")
      
      rv$fd_mme_complete <- TRUE
      
      # Update excluded golfers dropdown
      if (!is.null(rv$mme_data)) {
        all_golfers <- unique(rv$mme_data$Golfer)
        updateSelectizeInput(session,
                             "fd_excluded_golfers",
                             choices = all_golfers,
                             selected = NULL)
      }
      
      # Show success message
      showModal(modalDialog(
        title = "Success",
        sprintf(
          "Generated %d FanDuel MME lineups for filtering!\n\nTotal time: %.2f minutes",
          ifelse(is.null(rv$fd_mme_lineups), 0, nrow(rv$fd_mme_lineups)),
          as.numeric(fd_elapsed)
        ),
        easyClose = TRUE
      ))
    })
  })
  
  # Update DK slider ranges when MME lineups are generated
  observeEvent(rv$dk_mme_lineups, {
    req(rv$dk_mme_lineups)
    
    if (!is.null(rv$dk_mme_lineups)) {
      # Update slider ranges based on actual data with less granular steps
      updateSliderInput(
        session,
        "dk_min_expected_cuts",
        min = floor(min(rv$dk_mme_lineups$ExpectedCuts) * 10) / 10,
        max = ceiling(max(rv$dk_mme_lineups$ExpectedCuts) * 10) / 10,
        value = floor(min(rv$dk_mme_lineups$ExpectedCuts) * 10) / 10,
        step = 0.1
      )
      updateSliderInput(
        session,
        "dk_min_standard_proj",
        min = floor(min(rv$dk_mme_lineups$StandardProj)),
        max = ceiling(max(rv$dk_mme_lineups$StandardProj)),
        value = floor(min(rv$dk_mme_lineups$StandardProj)),
        step = 1
      )
      updateSliderInput(
        session,
        "dk_min_ceiling_proj",
        min = floor(min(rv$dk_mme_lineups$CeilingProj)),
        max = ceiling(max(rv$dk_mme_lineups$CeilingProj)),
        value = floor(min(rv$dk_mme_lineups$CeilingProj)),
        step = 1
      )
      updateSliderInput(
        session,
        "dk_min_6_pct",
        min = floor(min(rv$dk_mme_lineups$AtLeast6) * 100),
        max = ceiling(max(rv$dk_mme_lineups$AtLeast6) * 100),
        value = floor(min(rv$dk_mme_lineups$AtLeast6) * 100),
        step = 0.5
      )
      updateSliderInput(
        session,
        "dk_min_5plus_pct",
        min = floor(min(rv$dk_mme_lineups$AtLeast5) * 100),
        max = ceiling(max(rv$dk_mme_lineups$AtLeast5) * 100),
        value = floor(min(rv$dk_mme_lineups$AtLeast5) * 100),
        step = 0.5
      )
      updateSliderInput(
        session,
        "dk_min_4plus_pct",
        min = floor(min(rv$dk_mme_lineups$AtLeast4) * 100),
        max = ceiling(max(rv$dk_mme_lineups$AtLeast4) * 100),
        value = floor(min(rv$dk_mme_lineups$AtLeast4) * 100),
        step = 0.5
      )
      
      updateSliderInput(
        session,
        "dk_cumulative_ownership_range",
        min = floor(min(rv$dk_mme_lineups$CumulativeOwnership)),
        max = ceiling(max(rv$dk_mme_lineups$CumulativeOwnership)),
        value = c(floor(min(rv$dk_mme_lineups$CumulativeOwnership)), 
                  ceiling(max(rv$dk_mme_lineups$CumulativeOwnership))),
        step = 1
      )
      
      updateSliderInput(
        session,
        "dk_geometric_mean_ownership_range",
        min = floor(min(rv$dk_mme_lineups$GeometricMeanOwnership) * 10) / 10,
        max = ceiling(max(rv$dk_mme_lineups$GeometricMeanOwnership) * 10) / 10,
        value = c(floor(min(rv$dk_mme_lineups$GeometricMeanOwnership) * 10) / 10,
                  ceiling(max(rv$dk_mme_lineups$GeometricMeanOwnership) * 10) / 10),
        step = 0.1
      )
    }
  })
 
  
  # Update FD slider ranges when MME lineups are generated
  observeEvent(rv$fd_mme_lineups, {
    req(rv$fd_mme_lineups)
    
    if (!is.null(rv$fd_mme_lineups)) {
      # Update slider ranges based on actual data with less granular steps
      updateSliderInput(
        session,
        "fd_min_expected_cuts",
        min = floor(min(rv$fd_mme_lineups$ExpectedCuts) * 10) / 10,
        max = ceiling(max(rv$fd_mme_lineups$ExpectedCuts) * 10) / 10,
        value = floor(min(rv$fd_mme_lineups$ExpectedCuts) * 10) / 10,
        step = 0.1
      )
      updateSliderInput(
        session,
        "fd_min_standard_proj",
        min = floor(min(rv$fd_mme_lineups$StandardProj)),
        max = ceiling(max(rv$fd_mme_lineups$StandardProj)),
        value = floor(min(rv$fd_mme_lineups$StandardProj)),
        step = 1
      )
      updateSliderInput(
        session,
        "fd_min_ceiling_proj",
        min = floor(min(rv$fd_mme_lineups$CeilingProj)),
        max = ceiling(max(rv$fd_mme_lineups$CeilingProj)),
        value = floor(min(rv$fd_mme_lineups$CeilingProj)),
        step = 1
      )
      updateSliderInput(
        session,
        "fd_min_6_pct",
        min = floor(min(rv$fd_mme_lineups$AtLeast6) * 100),
        max = ceiling(max(rv$fd_mme_lineups$AtLeast6) * 100),
        value = floor(min(rv$fd_mme_lineups$AtLeast6) * 100),
        step = 0.5
      )
      updateSliderInput(
        session,
        "fd_min_5plus_pct",
        min = floor(min(rv$fd_mme_lineups$AtLeast5) * 100),
        max = ceiling(max(rv$fd_mme_lineups$AtLeast5) * 100),
        value = floor(min(rv$fd_mme_lineups$AtLeast5) * 100),
        step = 0.5
      )
      updateSliderInput(
        session,
        "fd_min_4plus_pct",
        min = floor(min(rv$fd_mme_lineups$AtLeast4) * 100),
        max = ceiling(max(rv$fd_mme_lineups$AtLeast4) * 100),
        value = floor(min(rv$fd_mme_lineups$AtLeast4) * 100),
        step = 0.5
      )
      # Update ownership range sliders based on actual data
      updateSliderInput(
        session,
        "fd_cumulative_ownership_range",
        min = floor(min(rv$fd_mme_lineups$CumulativeOwnership)),
        max = ceiling(max(rv$fd_mme_lineups$CumulativeOwnership)),
        value = c(floor(min(rv$fd_mme_lineups$CumulativeOwnership)), 
                  ceiling(max(rv$fd_mme_lineups$CumulativeOwnership))),
        step = 1
      )
      
      updateSliderInput(
        session,
        "fd_geometric_mean_ownership_range",
        min = floor(min(rv$fd_mme_lineups$GeometricMeanOwnership) * 10) / 10,
        max = ceiling(max(rv$fd_mme_lineups$GeometricMeanOwnership) * 10) / 10,
        value = c(floor(min(rv$fd_mme_lineups$GeometricMeanOwnership) * 10) / 10,
                  ceiling(max(rv$fd_mme_lineups$GeometricMeanOwnership) * 10) / 10),
        step = 0.1
      )
    }
  })
  
  # Update DK filtered pool size calculation to include ownership filters
  output$dk_filtered_mme_pool_size <- renderText({
    req(rv$dk_mme_lineups)
    
    # Apply all filters
    filtered_count <- rv$dk_mme_lineups
    
    if (input$dk_min_expected_cuts > 0) {
      filtered_count <- filtered_count[filtered_count$ExpectedCuts >= input$dk_min_expected_cuts, ]
    }
    if (input$dk_min_standard_proj > 0) {
      filtered_count <- filtered_count[filtered_count$StandardProj >= input$dk_min_standard_proj, ]
    }
    if (input$dk_min_ceiling_proj > 0) {
      filtered_count <- filtered_count[filtered_count$CeilingProj >= input$dk_min_ceiling_proj, ]
    }
    if (input$dk_min_6_pct > 0) {
      filtered_count <- filtered_count[filtered_count$AtLeast6 >= input$dk_min_6_pct / 100, ]
    }
    if (input$dk_min_5plus_pct > 0) {
      filtered_count <- filtered_count[filtered_count$AtLeast5 >= input$dk_min_5plus_pct / 100, ]
    }
    if (input$dk_min_4plus_pct > 0) {
      filtered_count <- filtered_count[filtered_count$AtLeast4 >= input$dk_min_4plus_pct / 100, ]
    }
    
    # Apply ownership range filters
    if (!is.null(input$dk_cumulative_ownership_range) && 
        length(input$dk_cumulative_ownership_range) == 2) {
      filtered_count <- filtered_count[
        filtered_count$CumulativeOwnership >= input$dk_cumulative_ownership_range[1] &
          filtered_count$CumulativeOwnership <= input$dk_cumulative_ownership_range[2], ]
    }
    
    if (!is.null(input$dk_geometric_mean_ownership_range) && 
        length(input$dk_geometric_mean_ownership_range) == 2) {
      filtered_count <- filtered_count[
        filtered_count$GeometricMeanOwnership >= input$dk_geometric_mean_ownership_range[1] &
          filtered_count$GeometricMeanOwnership <= input$dk_geometric_mean_ownership_range[2], ]
    }
    
    # Apply golfer exclusion filter
    if (!is.null(input$dk_excluded_golfers) &&
        length(input$dk_excluded_golfers) > 0) {
      player_cols <- c("Player1", "Player2", "Player3", "Player4", "Player5", "Player6")
      
      for (excluded_golfer in input$dk_excluded_golfers) {
        exclude_condition <- rep(TRUE, nrow(filtered_count))
        
        for (col in player_cols) {
          exclude_condition <- exclude_condition &
            (filtered_count[[col]] != excluded_golfer)
        }
        
        filtered_count <- filtered_count[exclude_condition, ]
      }
    }
    
    if (!is.null(input$dk_min_early_late) &&
        !is.null(input$dk_max_early_late)) {
      if ("Wave" %in% names(rv$mme_data)) {
        wave_lookup <- setNames(rv$mme_data$Wave, rv$mme_data$Golfer)
        player_cols <- c("Player1", "Player2", "Player3", "Player4", "Player5", "Player6")
        
        early_late_counts <- apply(filtered_count[, player_cols, drop = FALSE], 1, function(lineup) {
          player_waves <- wave_lookup[lineup]
          sum(player_waves == "EarlyLate", na.rm = TRUE)
        })
        
        wave_condition <- early_late_counts >= input$dk_min_early_late &
          early_late_counts <= input$dk_max_early_late
        filtered_count <- filtered_count[wave_condition, ]
      }
    }
    
    paste("Number of lineups in filtered pool:", nrow(filtered_count))
  })
  
  # Update FD filtered pool size calculation to include ownership filters
  output$fd_filtered_mme_pool_size <- renderText({
    req(rv$fd_mme_lineups)
    
    # Apply all filters
    filtered_count <- rv$fd_mme_lineups
    
    if (input$fd_min_expected_cuts > 0) {
      filtered_count <- filtered_count[filtered_count$ExpectedCuts >= input$fd_min_expected_cuts, ]
    }
    if (input$fd_min_standard_proj > 0) {
      filtered_count <- filtered_count[filtered_count$StandardProj >= input$fd_min_standard_proj, ]
    }
    if (input$fd_min_ceiling_proj > 0) {
      filtered_count <- filtered_count[filtered_count$CeilingProj >= input$fd_min_ceiling_proj, ]
    }
    if (input$fd_min_6_pct > 0) {
      filtered_count <- filtered_count[filtered_count$AtLeast6 >= input$fd_min_6_pct / 100, ]
    }
    if (input$fd_min_5plus_pct > 0) {
      filtered_count <- filtered_count[filtered_count$AtLeast5 >= input$fd_min_5plus_pct / 100, ]
    }
    if (input$fd_min_4plus_pct > 0) {
      filtered_count <- filtered_count[filtered_count$AtLeast4 >= input$fd_min_4plus_pct / 100, ]
    }
    
    # Apply ownership range filters
    if (!is.null(input$fd_cumulative_ownership_range) && 
        length(input$fd_cumulative_ownership_range) == 2) {
      filtered_count <- filtered_count[
        filtered_count$CumulativeOwnership >= input$fd_cumulative_ownership_range[1] &
          filtered_count$CumulativeOwnership <= input$fd_cumulative_ownership_range[2], ]
    }
    
    if (!is.null(input$fd_geometric_mean_ownership_range) && 
        length(input$fd_geometric_mean_ownership_range) == 2) {
      filtered_count <- filtered_count[
        filtered_count$GeometricMeanOwnership >= input$fd_geometric_mean_ownership_range[1] &
          filtered_count$GeometricMeanOwnership <= input$fd_geometric_mean_ownership_range[2], ]
    }
    
    # Apply golfer exclusion filter
    if (!is.null(input$fd_excluded_golfers) &&
        length(input$fd_excluded_golfers) > 0) {
      player_cols <- c("Player1", "Player2", "Player3", "Player4", "Player5", "Player6")
      
      for (excluded_golfer in input$fd_excluded_golfers) {
        exclude_condition <- rep(TRUE, nrow(filtered_count))
        
        for (col in player_cols) {
          exclude_condition <- exclude_condition &
            (filtered_count[[col]] != excluded_golfer)
        }
        
        filtered_count <- filtered_count[exclude_condition, ]
      }
    }
    
    if (!is.null(input$fd_min_early_late) &&
        !is.null(input$fd_max_early_late)) {
      if ("Wave" %in% names(rv$mme_data)) {
        wave_lookup <- setNames(rv$mme_data$Wave, rv$mme_data$Golfer)
        player_cols <- c("Player1", "Player2", "Player3", "Player4", "Player5", "Player6")
        
        early_late_counts <- apply(filtered_count[, player_cols, drop = FALSE], 1, function(lineup) {
          player_waves <- wave_lookup[lineup]
          sum(player_waves == "EarlyLate", na.rm = TRUE)
        })
        
        wave_condition <- early_late_counts >= input$fd_min_early_late &
          early_late_counts <= input$fd_max_early_late
        filtered_count <- filtered_count[wave_condition, ]
      }
    }
    
    paste("Number of lineups in filtered pool:", nrow(filtered_count))
  })
  
  # Update DK exposure table to include ownership filters
  output$dk_exposure_table <- renderDT({
    req(rv$dk_mme_lineups, rv$mme_data)
    
    # Apply filters to get filtered pool
    filtered_lineups <- rv$dk_mme_lineups
    
    if (input$dk_min_expected_cuts > 0) {
      filtered_lineups <- filtered_lineups[filtered_lineups$ExpectedCuts >= input$dk_min_expected_cuts, ]
    }
    if (input$dk_min_standard_proj > 0) {
      filtered_lineups <- filtered_lineups[filtered_lineups$StandardProj >= input$dk_min_standard_proj, ]
    }
    if (input$dk_min_ceiling_proj > 0) {
      filtered_lineups <- filtered_lineups[filtered_lineups$CeilingProj >= input$dk_min_ceiling_proj, ]
    }
    if (input$dk_min_6_pct > 0) {
      filtered_lineups <- filtered_lineups[filtered_lineups$AtLeast6 >= input$dk_min_6_pct / 100, ]
    }
    if (input$dk_min_5plus_pct > 0) {
      filtered_lineups <- filtered_lineups[filtered_lineups$AtLeast5 >= input$dk_min_5plus_pct / 100, ]
    }
    if (input$dk_min_4plus_pct > 0) {
      filtered_lineups <- filtered_lineups[filtered_lineups$AtLeast4 >= input$dk_min_4plus_pct / 100, ]
    }
    
    # Apply ownership range filters
    if (!is.null(input$dk_cumulative_ownership_range) && 
        length(input$dk_cumulative_ownership_range) == 2) {
      filtered_lineups <- filtered_lineups[
        filtered_lineups$CumulativeOwnership >= input$dk_cumulative_ownership_range[1] &
          filtered_lineups$CumulativeOwnership <= input$dk_cumulative_ownership_range[2], ]
    }
    
    if (!is.null(input$dk_geometric_mean_ownership_range) && 
        length(input$dk_geometric_mean_ownership_range) == 2) {
      filtered_lineups <- filtered_lineups[
        filtered_lineups$GeometricMeanOwnership >= input$dk_geometric_mean_ownership_range[1] &
          filtered_lineups$GeometricMeanOwnership <= input$dk_geometric_mean_ownership_range[2], ]
    }
    
    # Apply golfer exclusion filter
    if (!is.null(input$dk_excluded_golfers) && length(input$dk_excluded_golfers) > 0) {
      player_cols <- c("Player1", "Player2", "Player3", "Player4", "Player5", "Player6")
      
      for (excluded_golfer in input$dk_excluded_golfers) {
        exclude_condition <- rep(TRUE, nrow(filtered_lineups))
        
        for (col in player_cols) {
          exclude_condition <- exclude_condition & (filtered_lineups[[col]] != excluded_golfer)
        }
        
        filtered_lineups <- filtered_lineups[exclude_condition, ]
      }
    }
    
    # Apply wave filtering
    if (!is.null(input$dk_min_early_late) && !is.null(input$dk_max_early_late)) {
      if ("Wave" %in% names(rv$mme_data)) {
        wave_lookup <- setNames(rv$mme_data$Wave, rv$mme_data$Golfer)
        player_cols <- c("Player1", "Player2", "Player3", "Player4", "Player5", "Player6")
        
        # Convert to data.table if needed for efficient column selection
        if (!is.data.table(filtered_lineups)) {
          setDT(filtered_lineups)
        }
        
        early_late_counts <- apply(filtered_count[, player_cols, drop = FALSE], 1, function(lineup) {
          player_waves <- wave_lookup[lineup]
          sum(player_waves == "EarlyLate", na.rm = TRUE)
        })
        
        wave_condition <- early_late_counts >= input$dk_min_early_late & early_late_counts <= input$dk_max_early_late
        filtered_lineups <- filtered_lineups[wave_condition, ]
      }
    }
    
    # Calculate exposures for both random set and filtered pool
    random_exposures <- calculate_exposures(rv$dk_mme_lineups, rv$mme_data, "dk")
    filtered_exposures <- calculate_exposures(filtered_lineups, rv$mme_data, "dk")
    
    # Check for generated lineups exposure (current randomized set)
    generated_exposures <- NULL
    if (!is.null(rv$generated_dk_mme_lineups)) {
      generated_exposures <- calculate_exposures(rv$generated_dk_mme_lineups, rv$mme_data, "dk")
    }
    
    if (is.null(random_exposures)) return(NULL)
    
    # Get salary information
    salary_lookup <- setNames(rv$mme_data[["DKSal"]], rv$mme_data$Golfer)
    
    # Start with all golfers from MME data to ensure complete list
    all_golfers <- unique(rv$mme_data$Golfer)
    
    # Create base data frame
    combined_data <- data.frame(Golfer = all_golfers, stringsAsFactors = FALSE)
    
    # Add salary
    combined_data$Salary <- salary_lookup[combined_data$Golfer]
    
    # Add random pool exposures
    random_lookup <- setNames(random_exposures$Exposure_Pct, random_exposures$Golfer)
    combined_data$Random_Exposure <- random_lookup[combined_data$Golfer]
    combined_data$Random_Exposure[is.na(combined_data$Random_Exposure)] <- 0
    
    # Add filtered pool exposures
    if (!is.null(filtered_exposures) && nrow(filtered_exposures) > 0) {
      filtered_lookup <- setNames(filtered_exposures$Exposure_Pct, filtered_exposures$Golfer)
      combined_data$Filtered_Exposure <- filtered_lookup[combined_data$Golfer]
      combined_data$Filtered_Exposure[is.na(combined_data$Filtered_Exposure)] <- 0
    } else {
      combined_data$Filtered_Exposure <- 0
    }
    
    # Add generated lineups exposure (current randomized set)
    if (!is.null(generated_exposures) && nrow(generated_exposures) > 0) {
      generated_lookup <- setNames(generated_exposures$Exposure_Pct, generated_exposures$Golfer)
      combined_data$Generated_Exposure <- generated_lookup[combined_data$Golfer]
      combined_data$Generated_Exposure[is.na(combined_data$Generated_Exposure)] <- 0
    } else {
      combined_data$Generated_Exposure <- 0
    }
    
    # Add ownership projections (convert from decimal to percentage)
    ownership_lookup <- setNames(rv$mme_data[["DKOP"]] * 100, rv$mme_data$Golfer)
    combined_data$Ownership_Proj <- ownership_lookup[combined_data$Golfer]
    
    # Reorder columns and sort by filtered exposure
    combined_data <- combined_data[, c("Golfer", "Salary", "Generated_Exposure", "Filtered_Exposure", "Ownership_Proj")]
    combined_data <- combined_data[order(combined_data$Filtered_Exposure, decreasing = TRUE), ]
    
    datatable(
      combined_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "tip",
        ordering = TRUE
      ),
      rownames = FALSE,
      colnames = c("Golfer", "Salary", "Current Set %", "Filtered Pool %", "Ownership Proj %")
    ) %>%
      formatCurrency("Salary", "$", digits = 0) %>%
      formatRound(c("Generated_Exposure", "Filtered_Exposure", "Ownership_Proj"), 1)
  })
  
  
  # Update FD exposure table to include ownership filters
  output$fd_exposure_table <- renderDT({
    req(rv$fd_mme_lineups, rv$mme_data)
    
    # Apply filters to get filtered pool
    filtered_lineups <- rv$fd_mme_lineups
    
    if (input$fd_min_expected_cuts > 0) {
      filtered_lineups <- filtered_lineups[filtered_lineups$ExpectedCuts >= input$fd_min_expected_cuts, ]
    }
    if (input$fd_min_standard_proj > 0) {
      filtered_lineups <- filtered_lineups[filtered_lineups$StandardProj >= input$fd_min_standard_proj, ]
    }
    if (input$fd_min_ceiling_proj > 0) {
      filtered_lineups <- filtered_lineups[filtered_lineups$CeilingProj >= input$fd_min_ceiling_proj, ]
    }
    if (input$fd_min_6_pct > 0) {
      filtered_lineups <- filtered_lineups[filtered_lineups$AtLeast6 >= input$fd_min_6_pct / 100, ]
    }
    if (input$fd_min_5plus_pct > 0) {
      filtered_lineups <- filtered_lineups[filtered_lineups$AtLeast5 >= input$fd_min_5plus_pct / 100, ]
    }
    if (input$fd_min_4plus_pct > 0) {
      filtered_lineups <- filtered_lineups[filtered_lineups$AtLeast4 >= input$fd_min_4plus_pct / 100, ]
    }
    
    # Apply ownership range filters
    if (!is.null(input$fd_cumulative_ownership_range) && 
        length(input$fd_cumulative_ownership_range) == 2) {
      filtered_lineups <- filtered_lineups[
        filtered_lineups$CumulativeOwnership >= input$fd_cumulative_ownership_range[1] &
          filtered_lineups$CumulativeOwnership <= input$fd_cumulative_ownership_range[2], ]
    }
    
    if (!is.null(input$fd_geometric_mean_ownership_range) && 
        length(input$fd_geometric_mean_ownership_range) == 2) {
      filtered_lineups <- filtered_lineups[
        filtered_lineups$GeometricMeanOwnership >= input$fd_geometric_mean_ownership_range[1] &
          filtered_lineups$GeometricMeanOwnership <= input$fd_geometric_mean_ownership_range[2], ]
    }
    
    # Apply golfer exclusion filter
    if (!is.null(input$fd_excluded_golfers) && length(input$fd_excluded_golfers) > 0) {
      player_cols <- c("Player1", "Player2", "Player3", "Player4", "Player5", "Player6")
      
      for (excluded_golfer in input$fd_excluded_golfers) {
        exclude_condition <- rep(TRUE, nrow(filtered_lineups))
        
        for (col in player_cols) {
          exclude_condition <- exclude_condition & (filtered_lineups[[col]] != excluded_golfer)
        }
        
        filtered_lineups <- filtered_lineups[exclude_condition, ]
      }
    }
    
    # Apply wave filtering
    if (!is.null(input$fd_min_early_late) && !is.null(input$fd_max_early_late)) {
      if ("Wave" %in% names(rv$mme_data)) {
        wave_lookup <- setNames(rv$mme_data$Wave, rv$mme_data$Golfer)
        player_cols <- c("Player1", "Player2", "Player3", "Player4", "Player5", "Player6")
        
        # Convert to data.table if needed for efficient column selection
        if (!is.data.table(filtered_lineups)) {
          setDT(filtered_lineups)
        }
        
        early_late_counts <- apply(filtered_count[, player_cols, drop = FALSE], 1, function(lineup) {
          player_waves <- wave_lookup[lineup]
          sum(player_waves == "EarlyLate", na.rm = TRUE)
        })
        
        wave_condition <- early_late_counts >= input$fd_min_early_late & early_late_counts <= input$fd_max_early_late
        filtered_lineups <- filtered_lineups[wave_condition, ]
      }
    }
    
    # Calculate exposures for both random set and filtered pool
    random_exposures <- calculate_exposures(rv$fd_mme_lineups, rv$mme_data, "fd")
    filtered_exposures <- calculate_exposures(filtered_lineups, rv$mme_data, "fd")
    
    # Check for generated lineups exposure (current randomized set)
    generated_exposures <- NULL
    if (!is.null(rv$generated_fd_mme_lineups)) {
      generated_exposures <- calculate_exposures(rv$generated_fd_mme_lineups, rv$mme_data, "fd")
    }
    
    if (is.null(random_exposures)) return(NULL)
    
    # Get salary information
    salary_lookup <- setNames(rv$mme_data[["FDSal"]], rv$mme_data$Golfer)
    
    # Start with all golfers from MME data to ensure complete list
    all_golfers <- unique(rv$mme_data$Golfer)
    
    # Create base data frame
    combined_data <- data.frame(Golfer = all_golfers, stringsAsFactors = FALSE)
    
    # Add salary
    combined_data$Salary <- salary_lookup[combined_data$Golfer]
    
    # Add random pool exposures
    random_lookup <- setNames(random_exposures$Exposure_Pct, random_exposures$Golfer)
    combined_data$Random_Exposure <- random_lookup[combined_data$Golfer]
    combined_data$Random_Exposure[is.na(combined_data$Random_Exposure)] <- 0
    
    # Add filtered pool exposures
    if (!is.null(filtered_exposures) && nrow(filtered_exposures) > 0) {
      filtered_lookup <- setNames(filtered_exposures$Exposure_Pct, filtered_exposures$Golfer)
      combined_data$Filtered_Exposure <- filtered_lookup[combined_data$Golfer]
      combined_data$Filtered_Exposure[is.na(combined_data$Filtered_Exposure)] <- 0
    } else {
      combined_data$Filtered_Exposure <- 0
    }
    
    # Add generated lineups exposure (current randomized set)
    if (!is.null(generated_exposures) && nrow(generated_exposures) > 0) {
      generated_lookup <- setNames(generated_exposures$Exposure_Pct, generated_exposures$Golfer)
      combined_data$Generated_Exposure <- generated_lookup[combined_data$Golfer]
      combined_data$Generated_Exposure[is.na(combined_data$Generated_Exposure)] <- 0
    } else {
      combined_data$Generated_Exposure <- 0
    }
    
    # Add ownership projections (convert from decimal to percentage)
    ownership_lookup <- setNames(rv$mme_data[["FDOP"]] * 100, rv$mme_data$Golfer)
    combined_data$Ownership_Proj <- ownership_lookup[combined_data$Golfer]
    
    # Reorder columns and sort by filtered exposure
    combined_data <- combined_data[, c("Golfer", "Salary", "Generated_Exposure", "Filtered_Exposure", "Ownership_Proj")]
    combined_data <- combined_data[order(combined_data$Filtered_Exposure, decreasing = TRUE), ]
    
    datatable(
      combined_data,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "tip",
        ordering = TRUE
      ),
      rownames = FALSE,
      colnames = c("Golfer", "Salary", "Current Set %", "Filtered Pool %", "Ownership Proj %")
    ) %>%
      formatCurrency("Salary", "$", digits = 0) %>%
      formatRound(c("Generated_Exposure", "Filtered_Exposure", "Ownership_Proj"), 1)
  })

  
  
  # Update the generate DK MME lineups function to include ownership filters
  observeEvent(input$generate_dk_mme_lineups, {
    req(rv$dk_mme_lineups)
    
    # Create filters including ownership ranges
    filters <- list(
      min_expected_cuts = input$dk_min_expected_cuts,
      min_standard_proj = input$dk_min_standard_proj,
      min_ceiling_proj = input$dk_min_ceiling_proj,
      min_6_pct = input$dk_min_6_pct,
      min_5plus_pct = input$dk_min_5plus_pct,
      min_4plus_pct = input$dk_min_4plus_pct,
      excluded_golfers = input$dk_excluded_golfers,
      min_early_late = input$dk_min_early_late,
      max_early_late = input$dk_max_early_late,
      cumulative_ownership_range = input$dk_cumulative_ownership_range,
      geometric_mean_ownership_range = input$dk_geometric_mean_ownership_range,
      num_lineups = input$dk_num_mme_lineups
    )
    
    # Generate lineups
    withProgress(message = 'Generating filtered DK lineups...', value = 0.5, {
      rv$generated_dk_mme_lineups <- generate_filtered_mme_lineups(rv$dk_mme_lineups, filters, rv$mme_data)
      
      if (is.null(rv$generated_dk_mme_lineups)) {
        showModal(
          modalDialog(
            title = "No Lineups Found",
            "No DraftKings lineups match your current filter criteria. Please adjust your filters and try again.",
            easyClose = TRUE
          )
        )
      } else {
        showModal(modalDialog(
          title = "Success",
          sprintf(
            "Generated %d DraftKings lineups matching your criteria!",
            nrow(rv$generated_dk_mme_lineups)
          ),
          easyClose = TRUE
        ))
      }
    })
  })
  
  # Update the generate FD MME lineups function to include ownership filters
  observeEvent(input$generate_fd_mme_lineups, {
    req(rv$fd_mme_lineups)
    
    # Create filters including ownership ranges
    filters <- list(
      min_expected_cuts = input$fd_min_expected_cuts,
      min_standard_proj = input$fd_min_standard_proj,
      min_ceiling_proj = input$fd_min_ceiling_proj,
      min_6_pct = input$fd_min_6_pct,
      min_5plus_pct = input$fd_min_5plus_pct,
      min_4plus_pct = input$fd_min_4plus_pct,
      excluded_golfers = input$fd_excluded_golfers,
      min_early_late = input$fd_min_early_late,
      max_early_late = input$fd_max_early_late,
      cumulative_ownership_range = input$fd_cumulative_ownership_range,
      geometric_mean_ownership_range = input$fd_geometric_mean_ownership_range,
      num_lineups = input$fd_num_mme_lineups
    )
    
    # Generate lineups
    withProgress(message = 'Generating filtered FD lineups...', value = 0.5, {
      rv$generated_fd_mme_lineups <- generate_filtered_mme_lineups(rv$fd_mme_lineups, filters, rv$mme_data)
      
      if (is.null(rv$generated_fd_mme_lineups)) {
        showModal(
          modalDialog(
            title = "No Lineups Found",
            "No FanDuel lineups match your current filter criteria. Please adjust your filters and try again.",
            easyClose = TRUE
          )
        )
      } else {
        showModal(modalDialog(
          title = "Success",
          sprintf(
            "Generated %d FanDuel lineups matching your criteria!",
            nrow(rv$generated_fd_mme_lineups)
          ),
          easyClose = TRUE
        ))
      }
    })
  })
  
  # Update all table display functions to include ownership metrics
  
  # Update cash lineups table display:
  output$cash_lineups_table <- renderDT({
    req(rv$dk_cash_lineups, rv$fd_cash_lineups, input$cash_platform_toggle)
    
    # Select data based on platform
    if (input$cash_platform_toggle == "dk") {
      lineup_data <- rv$dk_cash_lineups
      source_data <- rv$dk_cash_data
    } else {
      lineup_data <- rv$fd_cash_lineups
      source_data <- rv$fd_cash_data
    }
    
    # Add EarlyLate count column if Wave data is available
    if (!is.null(source_data) && "Wave" %in% names(source_data)) {
      wave_lookup <- setNames(source_data$Wave, source_data$Golfer)
      player_cols <- c("Player1", "Player2", "Player3", "Player4", "Player5", "Player6")
      
      lineup_data$EarlyLate_Count <- apply(lineup_data[, player_cols], 1, function(lineup) {
        player_waves <- wave_lookup[lineup]
        sum(player_waves == "EarlyLate", na.rm = TRUE)
      })
    }
    
    # Select and rename columns for display
    if ("EarlyLate_Count" %in% names(lineup_data)) {
      display_data <- lineup_data %>%
        select(
          Player1, Player2, Player3, Player4, Player5, Player6,
          ExpectedCuts, StandardProj, CeilingProj, TotalSalary,
          CumulativeOwnership, GeometricMeanOwnership,
          EarlyLate_Count, AtLeast6, AtLeast5, AtLeast4
        ) %>%
        rename(
          "TotalOwn" = CumulativeOwnership,
          "GeoMeanOwn" = GeometricMeanOwnership,
          "AM/PM Count" = EarlyLate_Count,
          "6" = AtLeast6, "5+" = AtLeast5, "4+" = AtLeast4
        )
    } else {
      display_data <- lineup_data %>%
        select(
          Player1, Player2, Player3, Player4, Player5, Player6,
          ExpectedCuts, StandardProj, CeilingProj, TotalSalary,
          CumulativeOwnership, GeometricMeanOwnership,
          AtLeast6, AtLeast5, AtLeast4
        ) %>%
        rename(
          "TotalOwn" = CumulativeOwnership,
          "GeoMeanOwn" = GeometricMeanOwnership,
          "6" = AtLeast6, "5+" = AtLeast5, "4+" = AtLeast4
        )
    }
    
    # Create datatable with conditional formatting
    dt <- datatable(
      display_data,
      options = list(
        scrollX = TRUE,
        pageLength = 25,
        dom = "tip"
      ),
      rownames = FALSE
    ) %>%
      formatRound(c("ExpectedCuts", "StandardProj", "CeilingProj"), 2) %>%
      formatCurrency("TotalSalary", "$", digits = 0) %>%
      formatRound(c("TotalOwn", "GeoMeanOwn"), 1) %>%
      formatPercentage(c("6", "5+", "4+"), 1)
    
    # Add integer formatting for AM/PM Count if it exists
    if ("AM/PM Count" %in% names(display_data)) {
      dt <- dt %>% formatRound("AM/PM Count", 0)
    }
    
    return(dt)
  })
  
  # Update generated DK MME lineups table:
  output$generated_dk_mme_lineups_table <- renderDT({
    req(rv$generated_dk_mme_lineups)
    
    # Select and rename columns for display
    display_data <- rv$generated_dk_mme_lineups %>%
      select(
        Player1, Player2, Player3, Player4, Player5, Player6,
        ExpectedCuts, StandardProj, CeilingProj, TotalSalary,
        CumulativeOwnership, GeometricMeanOwnership,
        AtLeast6, AtLeast5, AtLeast4
      ) %>%
      rename(
        "TotalOwn" = CumulativeOwnership,
        "GeoMeanOwn" = GeometricMeanOwnership,
        "6" = AtLeast6, "5+" = AtLeast5, "4+" = AtLeast4
      )
    
    datatable(
      display_data,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = "tip"
      ),
      rownames = FALSE
    ) %>%
      formatRound(c("ExpectedCuts", "StandardProj", "CeilingProj"), 2) %>%
      formatCurrency("TotalSalary", "$", digits = 0) %>%
      formatRound(c("TotalOwn", "GeoMeanOwn"), 1) %>%
      formatPercentage(c("6", "5+", "4+"), 1)
  })
  
  # Update generated FD MME lineups table:
  output$generated_fd_mme_lineups_table <- renderDT({
    req(rv$generated_fd_mme_lineups)
    
    # Select and rename columns for display
    display_data <- rv$generated_fd_mme_lineups %>%
      select(
        Player1, Player2, Player3, Player4, Player5, Player6,
        ExpectedCuts, StandardProj, CeilingProj, TotalSalary,
        CumulativeOwnership, GeometricMeanOwnership,
        AtLeast6, AtLeast5, AtLeast4
      ) %>%
      rename(
        "TotalOwn" = CumulativeOwnership,
        "GeoMeanOwn" = GeometricMeanOwnership,
        "6" = AtLeast6, "5+" = AtLeast5, "4+" = AtLeast4
      )
    
    datatable(
      display_data,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = "tip"
      ),
      rownames = FALSE
    ) %>%
      formatRound(c("ExpectedCuts", "StandardProj", "CeilingProj"), 2) %>%
      formatCurrency("TotalSalary", "$", digits = 0) %>%
      formatRound(c("TotalOwn", "GeoMeanOwn"), 1) %>%
      formatPercentage(c("6", "5+", "4+"), 1)
  })
  
  # Update download handlers to include ownership metrics
  
  # Update cash download handler:
  output$download_cash <- downloadHandler(
    filename = function() {
      platform <- if (input$cash_platform_toggle == "dk")
        "DraftKings"
      else
        "FanDuel"
      paste(
        "golf_cash_lineups_",
        platform,
        "_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      # Select data and source based on platform
      if (input$cash_platform_toggle == "dk") {
        lineup_data <- rv$dk_cash_lineups
        platform_id_col <- "DKID"
        source_data <- rv$dk_cash_data
      } else {
        lineup_data <- rv$fd_cash_lineups
        platform_id_col <- "FDID"
        source_data <- rv$fd_cash_data
      }
      
      # Add EarlyLate count column if Wave data is available
      if (!is.null(source_data) && "Wave" %in% names(source_data)) {
        wave_lookup <- setNames(source_data$Wave, source_data$Golfer)
        player_cols <- c("Player1", "Player2", "Player3", "Player4", "Player5", "Player6")
        
        lineup_data$EarlyLate_Count <- apply(lineup_data[, player_cols], 1, function(lineup) {
          player_waves <- wave_lookup[lineup]
          sum(player_waves == "EarlyLate", na.rm = TRUE)
        })
      }
      
      # Create ID lookup from appropriate cash data source
      id_lookup <- setNames(source_data[[platform_id_col]], source_data$Golfer)
      
      # Replace player names with IDs and select columns
      if ("EarlyLate_Count" %in% names(lineup_data)) {
        download_data <- lineup_data %>%
          select(
            Player1, Player2, Player3, Player4, Player5, Player6,
            ExpectedCuts, StandardProj, CeilingProj, TotalSalary,
            CumulativeOwnership, GeometricMeanOwnership,
            EarlyLate_Count, AtLeast6, AtLeast5, AtLeast4
          ) %>%
          mutate(
            Player1 = id_lookup[Player1],
            Player2 = id_lookup[Player2],
            Player3 = id_lookup[Player3],
            Player4 = id_lookup[Player4],
            Player5 = id_lookup[Player5],
            Player6 = id_lookup[Player6]
          ) %>%
          rename(
            "TotalOwn" = CumulativeOwnership,
            "GeoMeanOwn" = GeometricMeanOwnership,
            "AM/PM Count" = EarlyLate_Count,
            "6" = AtLeast6, "5+" = AtLeast5, "4+" = AtLeast4
          )
      } else {
        download_data <- lineup_data %>%
          select(
            Player1, Player2, Player3, Player4, Player5, Player6,
            ExpectedCuts, StandardProj, CeilingProj, TotalSalary,
            CumulativeOwnership, GeometricMeanOwnership,
            AtLeast6, AtLeast5, AtLeast4
          ) %>%
          mutate(
            Player1 = id_lookup[Player1],
            Player2 = id_lookup[Player2],
            Player3 = id_lookup[Player3],
            Player4 = id_lookup[Player4],
            Player5 = id_lookup[Player5],
            Player6 = id_lookup[Player6]
          ) %>%
          rename(
            "TotalOwn" = CumulativeOwnership,
            "GeoMeanOwn" = GeometricMeanOwnership,
            "6" = AtLeast6, "5+" = AtLeast5, "4+" = AtLeast4
          )
      }
      
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  # Update DK MME download handlers:
  output$download_dk_mme_lineups <- downloadHandler(
    filename = function() {
      paste(
        "golf_mme_top15k_DraftKings_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      # Sort by AtLeast6 and take top 15k
      top_lineups <- rv$dk_mme_lineups %>%
        arrange(desc(AtLeast6)) %>%
        head(15000)
      
      # Create ID lookup from MME data
      id_lookup <- setNames(rv$mme_data[["DKID"]], rv$mme_data$Golfer)
      
      # Replace player names with IDs
      download_data <- top_lineups %>%
        select(
          Player1, Player2, Player3, Player4, Player5, Player6,
          ExpectedCuts, StandardProj, CeilingProj, TotalSalary,
          CumulativeOwnership, GeometricMeanOwnership,
          AtLeast6, AtLeast5, AtLeast4
        ) %>%
        mutate(
          Player1 = id_lookup[Player1],
          Player2 = id_lookup[Player2],
          Player3 = id_lookup[Player3],
          Player4 = id_lookup[Player4],
          Player5 = id_lookup[Player5],
          Player6 = id_lookup[Player6]
        ) %>%
        rename(
          "TotalOwn" = CumulativeOwnership,
          "GeoMeanOwn" = GeometricMeanOwnership,
          "6" = AtLeast6, "5+" = AtLeast5, "4+" = AtLeast4
        )
      
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  # Update generated DK download handler:
  output$download_generated_dk_mme <- downloadHandler(
    filename = function() {
      paste(
        "golf_mme_generated_DraftKings_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      # Create ID lookup from MME data
      id_lookup <- setNames(rv$mme_data[["DKID"]], rv$mme_data$Golfer)
      
      # Replace player names with IDs
      download_data <- rv$generated_dk_mme_lineups %>%
        select(
          Player1, Player2, Player3, Player4, Player5, Player6,
          ExpectedCuts, StandardProj, CeilingProj, TotalSalary,
          CumulativeOwnership, GeometricMeanOwnership,
          AtLeast6, AtLeast5, AtLeast4
        ) %>%
        mutate(
          Player1 = id_lookup[Player1],
          Player2 = id_lookup[Player2],
          Player3 = id_lookup[Player3],
          Player4 = id_lookup[Player4],
          Player5 = id_lookup[Player5],
          Player6 = id_lookup[Player6]
        ) %>%
        rename(
          "TotalOwn" = CumulativeOwnership,
          "GeoMeanOwn" = GeometricMeanOwnership,
          "6" = AtLeast6, "5+" = AtLeast5, "4+" = AtLeast4
        )
      
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  # Update FD MME download handlers:
  output$download_fd_mme_lineups <- downloadHandler(
    filename = function() {
      paste("golf_mme_top15k_FanDuel_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv",
            sep = "")
    },
    content = function(file) {
      # Sort by AtLeast6 and take top 15k
      top_lineups <- rv$fd_mme_lineups %>%
        arrange(desc(AtLeast6)) %>%
        head(15000)
      
      # Create ID lookup from MME data
      id_lookup <- setNames(rv$mme_data[["FDID"]], rv$mme_data$Golfer)
      
      # Replace player names with IDs
      download_data <- top_lineups %>%
        select(
          Player1, Player2, Player3, Player4, Player5, Player6,
          ExpectedCuts, StandardProj, CeilingProj, TotalSalary,
          CumulativeOwnership, GeometricMeanOwnership,
          AtLeast6, AtLeast5, AtLeast4
        ) %>%
        mutate(
          Player1 = id_lookup[Player1],
          Player2 = id_lookup[Player2],
          Player3 = id_lookup[Player3],
          Player4 = id_lookup[Player4],
          Player5 = id_lookup[Player5],
          Player6 = id_lookup[Player6]
        ) %>%
        rename(
          "TotalOwn" = CumulativeOwnership,
          "GeoMeanOwn" = GeometricMeanOwnership,
          "6" = AtLeast6, "5+" = AtLeast5, "4+" = AtLeast4
        )
      
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  # Update generated FD download handler:
  output$download_generated_fd_mme <- downloadHandler(
    filename = function() {
      paste(
        "golf_mme_generated_FanDuel_",
        format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      # Create ID lookup from MME data
      id_lookup <- setNames(rv$mme_data[["FDID"]], rv$mme_data$Golfer)
      
      # Replace player names with IDs
      download_data <- rv$generated_fd_mme_lineups %>%
        select(
          Player1, Player2, Player3, Player4, Player5, Player6,
          ExpectedCuts, StandardProj, CeilingProj, TotalSalary,
          CumulativeOwnership, GeometricMeanOwnership,
          AtLeast6, AtLeast5, AtLeast4
        ) %>%
        mutate(
          Player1 = id_lookup[Player1],
          Player2 = id_lookup[Player2],
          Player3 = id_lookup[Player3],
          Player4 = id_lookup[Player4],
          Player5 = id_lookup[Player5],
          Player6 = id_lookup[Player6]
        ) %>%
        rename(
          "TotalOwn" = CumulativeOwnership,
          "GeoMeanOwn" = GeometricMeanOwnership,
          "6" = AtLeast6, "5+" = AtLeast5, "4+" = AtLeast4
        )
      
      write.csv(download_data, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)