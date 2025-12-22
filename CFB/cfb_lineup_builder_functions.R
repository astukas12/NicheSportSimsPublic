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
