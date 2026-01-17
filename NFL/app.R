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


# Helper function to parse matchup into game ID
# Helper function to parse matchup into game ID
parse_matchup <- function(team, opp) {
  # Remove @ symbol
  opp_team <- gsub("@", "", opp)
  
  # Sort alphabetically so order is consistent
  teams_sorted <- sort(c(team, opp_team))
  
  # Format: TEAM1_vs_TEAM2 (alphabetical)
  game_id <- paste0(teams_sorted[1], "_vs_", teams_sorted[2])
  return(game_id)
}

build_classic_lineups <- function(sim_results, dk_salaries, sport = "NFL") {
  cat("\n=== BUILDING CLASSIC LINEUPS (SHOWDOWN STYLE) ===\n")
  cat("Sport:", sport, "\n")
  
  # Get unique simulations
  sims <- unique(sim_results$SimID)
  n_sims <- length(sims)
  
  cat("Total simulations:", n_sims, "\n")
  cat("Finding optimal lineup for each simulation...\n")
  
  # Position constraints
  if (sport == "NFL") {
    salary_cap <- 50000
    roster_size <- 9
  } else {
    salary_cap <- 50000
    roster_size <- 8
  }
  
  # For each simulation, find the optimal lineup
  all_lineups <- list()
  
  for (i in 1:n_sims) {
    if (i %% 1000 == 0)
      cat("  Processing simulation", i, "of", n_sims, "\n")
    
    # Get this simulation's results
    sim_data <- sim_results %>%
      filter(SimID == sims[i]) %>%
      filter(Pos != "K", !is.na(Pos)) %>%  # Exclude kickers
      select(Player, Team, Game, Pos, DFS_ID, Salary, Own, TotalPts)
    
    # Build optimal lineup for THIS simulation using LP
    lineup <- build_optimal_classic_lineup_for_sim(sim_data, sport, salary_cap, roster_size)
    
    if (!is.null(lineup)) {
      all_lineups[[i]] <- lineup
    }
  }
  
  cat("Successfully built lineups for",
      length(all_lineups),
      "simulations\n")
  
  # Combine all lineups
  all_lineups_df <- bind_rows(all_lineups)
  
  # Create lineup key for matching (sorted player list)
  all_lineups_df <- all_lineups_df %>%
    rowwise() %>%
    mutate(LineupKey = paste(sort(
      c(
        Player1,
        Player2,
        Player3,
        Player4,
        Player5,
        Player6,
        Player7,
        Player8,
        Player9
      )
    ), collapse = "|")) %>%
    ungroup()
  
  # Count occurrences and calculate metrics for each unique lineup
  # Count occurrences and calculate metrics for each unique lineup
  # Count occurrences and calculate metrics for each unique lineup
  unique_lineups <- all_lineups_df %>%
    group_by(LineupKey) %>%
    summarise(
      Player1 = first(Player1),
      Player2 = first(Player2),
      Player3 = first(Player3),
      Player4 = first(Player4),
      Player5 = first(Player5),
      Player6 = first(Player6),
      Player7 = first(Player7),
      Player8 = first(Player8),
      Player9 = first(Player9),
      TotalSalary = first(TotalSalary),
      ProjectedPoints = mean(LineupPoints),
      TotalOwnership = first(TotalOwnership),
      Top1Count = n(),  # Each appearance = it was #1 in that sim
      .groups = 'drop'
    ) %>%
    mutate(
      CumulativeOwnership = TotalOwnership,  # ADD as alias
      GeometricMeanOwnership = TotalOwnership / 9  # ADD geometric mean
    ) %>%
    select(-LineupKey) %>%
    arrange(desc(Top1Count))
  
  cat("Found", nrow(unique_lineups), "unique optimal lineups\n")
  cat("Top lineup appeared",
      max(unique_lineups$Top1Count),
      "times\n")
  
  return(unique_lineups)
}

# Helper function: Build optimal lineup for a single simulation
build_optimal_classic_lineup_for_sim <- function(sim_data, sport, salary_cap, roster_size) {
  n <- nrow(sim_data)
  
  if (n < roster_size)
    return(NULL)
  
  # Objective: maximize points in THIS simulation
  objective <- sim_data$TotalPts
  
  # Constraint matrix
  constraints <- list()
  dirs <- c()
  rhs <- c()
  
  # 1. Salary constraint
  constraints[[1]] <- sim_data$Salary
  dirs <- c(dirs, "<=")
  rhs <- c(rhs, salary_cap)
  
  # 2. Roster size
  constraints[[2]] <- rep(1, n)
  dirs <- c(dirs, "==")
  rhs <- c(rhs, roster_size)
  
  # 3. Position constraints
  if (sport == "NFL") {
    # QB = 1
    constraints[[3]] <- as.numeric(sim_data$Pos == "QB")
    dirs <- c(dirs, "==")
    rhs <- c(rhs, 1)
    
    # RB >= 2
    constraints[[4]] <- as.numeric(sim_data$Pos == "RB")
    dirs <- c(dirs, ">=")
    rhs <- c(rhs, 2)
    
    # WR >= 3
    constraints[[5]] <- as.numeric(sim_data$Pos == "WR")
    dirs <- c(dirs, ">=")
    rhs <- c(rhs, 3)
    
    # TE >= 1
    constraints[[6]] <- as.numeric(sim_data$Pos == "TE")
    dirs <- c(dirs, ">=")
    rhs <- c(rhs, 1)
    
    # DST = 1
    constraints[[7]] <- as.numeric(sim_data$Pos == "DST")
    dirs <- c(dirs, "==")
    rhs <- c(rhs, 1)
  } else {
    # CFB constraints
    constraints[[3]] <- as.numeric(sim_data$Pos == "QB")
    dirs <- c(dirs, ">=")
    rhs <- c(rhs, 1)
    
    constraints[[4]] <- as.numeric(sim_data$Pos == "RB")
    dirs <- c(dirs, ">=")
    rhs <- c(rhs, 2)
    
    constraints[[5]] <- as.numeric(sim_data$Pos == "WR")
    dirs <- c(dirs, ">=")
    rhs <- c(rhs, 3)
  }
  
  # Convert to matrix
  const_mat <- do.call(rbind, constraints)
  
  # Solve LP
  solution <- lp("max", objective, const_mat, dirs, rhs, all.bin = TRUE)
  
  if (solution$status != 0)
    return(NULL)
  
  # Extract lineup
  selected <- which(solution$solution == 1)
  lineup_data <- sim_data[selected, ]
  
  # Create result
  result <- data.frame(
    Player1 = paste0(lineup_data$Player[1], " (", lineup_data$DFS_ID[1], ")"),
    Player2 = paste0(lineup_data$Player[2], " (", lineup_data$DFS_ID[2], ")"),
    Player3 = paste0(lineup_data$Player[3], " (", lineup_data$DFS_ID[3], ")"),
    Player4 = paste0(lineup_data$Player[4], " (", lineup_data$DFS_ID[4], ")"),
    Player5 = paste0(lineup_data$Player[5], " (", lineup_data$DFS_ID[5], ")"),
    Player6 = paste0(lineup_data$Player[6], " (", lineup_data$DFS_ID[6], ")"),
    Player7 = paste0(lineup_data$Player[7], " (", lineup_data$DFS_ID[7], ")"),
    Player8 = paste0(lineup_data$Player[8], " (", lineup_data$DFS_ID[8], ")"),
    Player9 = paste0(lineup_data$Player[9], " (", lineup_data$DFS_ID[9], ")"),
    TotalSalary = sum(lineup_data$Salary),
    LineupPoints = sum(lineup_data$TotalPts),
    TotalOwnership = sum(lineup_data$Own, na.rm = TRUE)
  )
  
  return(result)
}


# Read DraftKings salary file (CSV or Excel)
read_dk_salaries <- function(file_path) {
  ext <- tools::file_ext(file_path)
  
  if (ext == "csv") {
    df <- read.csv(file_path,
                   stringsAsFactors = FALSE,
                   check.names = TRUE)
    
    df$Player <- trimws(df$Player)
    
    
    # Handle different column name variations
    pos_col <- if ("DK.Position" %in% names(df))
      "DK.Position"
    else if ("DK.Pos" %in% names(df))
      "DK.Pos"
    else
      "Position"
    
    salary_col <- if ("DK.Salary" %in% names(df))
      "DK.Salary"
    else
      "Salary"
    
    proj_col <- if ("DK.Projection" %in% names(df))
      "DK.Projection"
    else if ("DK.Proj" %in% names(df))
      "DK.Proj"
    else
      "Projection"
    
    own_col <- if ("DK.Ownership" %in% names(df))
      "DK.Ownership"
    else if ("Large.Field" %in% names(df))
      "Large.Field"
    else
      "Ownership"
    
    # Create Game column
    df$Game <- mapply(parse_matchup, df$Team, df$Opponent)
    
    # Create our own IDs if id column is missing/empty
    if (!"id" %in% names(df) ||
        all(is.na(df$id)) || all(df$id == "")) {
      df$id <- 1:nrow(df)
    }
    
    dk_salaries <- data.frame(
      DFS_ID = df$id,
      Name = df$Player,
      Pos = df[[pos_col]],
      # Use the column we found
      Team = df$Team,
      Game = df$Game,
      Salary = as.numeric(gsub("[$,]", "", df[[salary_col]])),
      Projection = as.numeric(df[[proj_col]]),
      Own = if (own_col %in% names(df))
        as.numeric(gsub("%", "", df[[own_col]]))
      else
        0,
      stringsAsFactors = FALSE
    )
    
    # Debug: show what we got
    cat("Parsed", nrow(dk_salaries), "players\n")
    cat("Position breakdown:\n")
    print(table(dk_salaries$Pos))
    
  } else {
    dk_salaries <- read_excel(file_path, sheet = "DK_Classic_Salaries")
  }
  
  return(dk_salaries)
}

# Load classic slate - just salary file and which game files to use
load_classic_slate <- function(salary_file, game_files, sport = "CFB") {
  # Read salary file - this tells us everything about the slate
  dk_salaries <- read_dk_salaries(salary_file)
  
  # Get unique games from salary file
  games_in_salaries <- unique(dk_salaries$Game)
  
  all_player_data <- list()
  all_similar_games <- list()
  games_loaded <- list()
  
  # Load simulation data for each game file
  for (game_file in game_files) {
    sheets <- excel_sheets(game_file)
    
    # Get team names from sheets
    team_sheets <- sheets[grepl("_(Rushing|Receiving|Passing|Kicking)$", sheets)]
    teams <- unique(gsub("_(Rushing|Receiving|Passing|Kicking)$", "", team_sheets))
    
    if (length(teams) != 2)
      next
    
    # Create game ID
    game_id <- paste(teams[1], "vs", teams[2], sep = "_")
    games_loaded[[game_id]] <- teams
    
    # Load simulation data for each team
    for (team in teams) {
      all_player_data[[team]] <- list(
        rushing = if (paste0(team, "_Rushing") %in% sheets)
          read_excel(game_file, sheet = paste0(team, "_Rushing"))
        else
          data.frame(),
        receiving = if (paste0(team, "_Receiving") %in% sheets)
          read_excel(game_file, sheet = paste0(team, "_Receiving"))
        else
          data.frame(),
        passing = if (paste0(team, "_Passing") %in% sheets)
          read_excel(game_file, sheet = paste0(team, "_Passing"))
        else
          data.frame(),
        kicking = if (paste0(team, "_Kicking") %in% sheets)
          read_excel(game_file, sheet = paste0(team, "_Kicking"))
        else
          data.frame()
      )
    }
    
    # Load similar games
    if ("Similar_Games" %in% sheets) {
      all_similar_games[[game_id]] <- read_excel(game_file, sheet = "Similar_Games")
    }
  }
  
  if (length(games_loaded) < 2) {
    stop("Need at least 2 valid game files")
  }
  
  return(
    list(
      games = games_loaded,
      player_data = all_player_data,
      dk_salaries = dk_salaries,
      similar_games = all_similar_games,
      slate_type = "classic",
      sport = sport
    )
  )
}

# Source lineup builder functions
source("cfb_lineup_builder_functions.R")

# Load team colors (NFL or CFB) automatically


UNIVERSAL_TEAM_COLORS <- list(
  # === NFL TEAMS ===
  "BUF" = list(primary = "#00338D", secondary = "#C60C30"),
  "MIA" = list(primary = "#008E97", secondary = "#FC4C02"),
  "NE" = list(primary = "#002244", secondary = "#C60C30"),
  "NYJ" = list(primary = "#125740", secondary = "#000000"),
  "BAL" = list(primary = "#241773", secondary = "#000000"),
  "CIN" = list(primary = "#FB4F14", secondary = "#000000"),
  "CLE" = list(primary = "#311D00", secondary = "#FF3C00"),
  "PIT" = list(primary = "#FFB612", secondary = "#000000"),
  "HOU" = list(primary = "#A71930", secondary = "#03202F"),
  "IND" = list(primary = "#002C5F", secondary = "#A2AAAD"),
  "JAX" = list(primary = "#006778", secondary = "#D7A22A"),
  "TEN" = list(primary = "#0C2340", secondary = "#4B92DB"),
  "DEN" = list(primary = "#FB4F14", secondary = "#002244"),
  "KC" = list(primary = "#E31837", secondary = "#FFB81C"),
  "LV" = list(primary = "#000000", secondary = "#A5ACAF"),
  "LAC" = list(primary = "#0080C6", secondary = "#FFC20E"),
  "DAL" = list(primary = "#041E42", secondary = "#869397"),
  "NYG" = list(primary = "#0B2265", secondary = "#A71930"),
  "PHI" = list(primary = "#004C54", secondary = "#A5ACAF"),
  "WAS" = list(primary = "#5A1414", secondary = "#FFB612"),
  "CHI" = list(primary = "#0B162A", secondary = "#C83803"),
  "DET" = list(primary = "#0076B6", secondary = "#B0B7BC"),
  "GB" = list(primary = "#203731", secondary = "#FFB612"),
  "MIN" = list(primary = "#4F2683", secondary = "#FFC62F"),
  "ATL" = list(primary = "#A71930", secondary = "#000000"),
  "CAR" = list(primary = "#0085CA", secondary = "#000000"),
  "NO" = list(primary = "#D3BC8D", secondary = "#000000"),
  "TB" = list(primary = "#D50A0A", secondary = "#34302B"),
  "ARI" = list(primary = "#97233F", secondary = "#000000"),
  "LA" = list(primary = "#003594", secondary = "#FFA300"),
  "SF" = list(primary = "#AA0000", secondary = "#B3995D"),
  "SEA" = list(primary = "#002244", secondary = "#69BE28"),
  
  # === CFB TEAMS ===
  "Air Force" = list(primary = "#003087", secondary = "#8A8D8F"),
  "Akron" = list(primary = "#041E42", secondary = "#A89968"),
  "Alabama" = list(primary = "#9E1B32", secondary = "#828A8F"),
  "App State" = list(primary = "#000000", secondary = "#FFB81C"),
  "Arizona" = list(primary = "#003366", secondary = "#CC0033"),
  "Arizona State" = list(primary = "#8C1D40", secondary = "#FFC627"),
  "Arkansas" = list(primary = "#9D2235", secondary = "#FFFFFF"),
  "Arkansas State" = list(primary = "#CC092F", secondary = "#000000"),
  "Army" = list(primary = "#000000", secondary = "#D4AF37"),
  "Auburn" = list(primary = "#0C2340", secondary = "#E87722"),
  "Ball State" = list(primary = "#BA0C2F", secondary = "#FFFFFF"),
  "Baylor" = list(primary = "#003015", secondary = "#FFB81C"),
  "Boise State" = list(primary = "#0033A0", secondary = "#D64309"),
  "Boston College" = list(primary = "#98002E", secondary = "#BC9B6A"),
  "Bowling Green" = list(primary = "#FE5000", secondary = "#4F2C1D"),
  "Buffalo" = list(primary = "#005BBB", secondary = "#FFFFFF"),
  "BYU" = list(primary = "#002E5D", secondary = "#FFFFFF"),
  "California" = list(primary = "#003262", secondary = "#FDB515"),
  "UCF" = list(primary = "#000000", secondary = "#FFC904"),
  "Central Michigan" = list(primary = "#6A0032", secondary = "#FFC82E"),
  "Charlotte" = list(primary = "#005035", secondary = "#B3A369"),
  "Cincinnati" = list(primary = "#E00122", secondary = "#000000"),
  "Clemson" = list(primary = "#F56600", secondary = "#522D80"),
  "Coastal Carolina" = list(primary = "#006F71", secondary = "#A27752"),
  "Colorado" = list(primary = "#000000", secondary = "#CFB87C"),
  "Colorado State" = list(primary = "#1E4D2B", secondary = "#C8C372"),
  "Connecticut" = list(primary = "#000E2F", secondary = "#E4002B"),
  "Duke" = list(primary = "#003087", secondary = "#FFFFFF"),
  "East Carolina" = list(primary = "#592A8A", secondary = "#FFB81C"),
  "Eastern Michigan" = list(primary = "#006633", secondary = "#FFFFFF"),
  "Florida" = list(primary = "#0021A5", secondary = "#FA4616"),
  "Florida Atlantic" = list(primary = "#003366", secondary = "#CC0000"),
  "Florida International" = list(primary = "#081E3F", secondary = "#B6862C"),
  "Florida State" = list(primary = "#782F40", secondary = "#CEB888"),
  "Fresno State" = list(primary = "#DB0032", secondary = "#003A70"),
  "Georgia" = list(primary = "#BA0C2F", secondary = "#000000"),
  "Georgia Southern" = list(primary = "#003087", secondary = "grey"),
  "Georgia State" = list(primary = "#0033A0", secondary = "#C8102E"),
  "Georgia Tech" = list(primary = "#B3A369", secondary = "#003057"),
  "Hawaii" = list(primary = "#024731", secondary = "#FFFFFF"),
  "Houston" = list(primary = "#C8102E", secondary = "#FFFFFF"),
  "Illinois" = list(primary = "#13294B", secondary = "#E84A27"),
  "Indiana" = list(primary = "#990000", secondary = "#EEEDEB"),
  "Iowa" = list(primary = "#000000", secondary = "#FFCD00"),
  "Iowa State" = list(primary = "#C8102E", secondary = "#F1BE48"),
  "Jacksonville State" = list(primary = "#CC092F", secondary = "#FFFFFF"),
  "James Madison" = list(primary = "#450084", secondary = "#CBB677"),
  "Kansas" = list(primary = "#0051BA", secondary = "#E8000D"),
  "Kansas State" = list(primary = "#512888", secondary = "#FFFFFF"),
  "Kennesaw State" = list(primary = "#000000", secondary = "#FFC82E"),
  "Kent State" = list(primary = "#002664", secondary = "#EAAA00"),
  "Kentucky" = list(primary = "#0033A0", secondary = "#FFFFFF"),
  "Liberty" = list(primary = "#002147", secondary = "#C8102E"),
  "Louisiana" = list(primary = "#CE181E", secondary = "#FFFFFF"),
  "Louisiana Monroe" = list(primary = "#85002B", secondary = "#FFBE0F"),
  "Louisiana Tech" = list(primary = "#002F8B", secondary = "#E31B23"),
  "Louisville" = list(primary = "#AD0000", secondary = "#000000"),
  "LSU" = list(primary = "#461D7C", secondary = "#FDD023"),
  "Marshall" = list(primary = "#00B140", secondary = "#FFFFFF"),
  "Maryland" = list(primary = "#E03A3E", secondary = "#000000"),
  "Memphis" = list(primary = "#003087", secondary = "#8D9093"),
  "Miami" = list(primary = "#F47321", secondary = "#005030"),
  "Miami (OH)" = list(primary = "#C8102E", secondary = "#000000"),
  "Michigan" = list(primary = "#00274C", secondary = "#FFCB05"),
  "Michigan State" = list(primary = "#18453B", secondary = "#FFFFFF"),
  "Middle Tennessee" = list(primary = "#0066CC", secondary = "#FFFFFF"),
  "Minnesota" = list(primary = "#7A0019", secondary = "#FFCC33"),
  "Mississippi State" = list(primary = "#660000", secondary = "#FFFFFF"),
  "Missouri" = list(primary = "#F1B82D", secondary = "#000000"),
  "Navy" = list(primary = "#00205B", secondary = "#C5B783"),
  "Nebraska" = list(primary = "#E41C38", secondary = "#FFFFFF"),
  "Nevada" = list(primary = "#003366", secondary = "#8A8D8F"),
  "New Mexico" = list(primary = "#BA0C2F", secondary = "#8A8D8F"),
  "New Mexico State" = list(primary = "#BA0C2F", secondary = "#FFFFFF"),
  "North Carolina" = list(primary = "#7BAFD4", secondary = "#13294B"),
  "North Texas" = list(primary = "#00853E", secondary = "#FFFFFF"),
  "Northern Illinois" = list(primary = "#BA0C2F", secondary = "#000000"),
  "Northwestern" = list(primary = "#4E2A84", secondary = "#FFFFFF"),
  "Notre Dame" = list(primary = "#0C2340", secondary = "#C99700"),
  "Ohio" = list(primary = "#00694E", secondary = "#FFFFFF"),
  "Ohio State" = list(primary = "#BB0000", secondary = "#666666"),
  "Oklahoma" = list(primary = "#841617", secondary = "#FDFBF5"),
  "Oklahoma State" = list(primary = "#FF6600", secondary = "#000000"),
  "Old Dominion" = list(primary = "#003057", secondary = "#8A8D8F"),
  "Ole Miss" = list(primary = "#14213D", secondary = "#CE1126"),
  "Oregon" = list(primary = "#154733", secondary = "#FEE123"),
  "Oregon State" = list(primary = "#DC4405", secondary = "#000000"),
  "Penn State" = list(primary = "#041E42", secondary = "#FFFFFF"),
  "Pittsburgh" = list(primary = "#003594", secondary = "#FFB81C"),
  "Purdue" = list(primary = "#CEB888", secondary = "#000000"),
  "Rice" = list(primary = "#00205B", secondary = "#8A8D8F"),
  "Rutgers" = list(primary = "#CC0033", secondary = "#FFFFFF"),
  "Sam Houston" = list(primary = "#F47321", secondary = "#FFFFFF"),
  "San Diego State" = list(primary = "#A6192E", secondary = "#000000"),
  "San Jose State" = list(primary = "#0055A2", secondary = "#E5A823"),
  "SMU" = list(primary = "#0033A0", secondary = "#CC0000"),
  "South Alabama" = list(primary = "#00205B", secondary = "#CC0000"),
  "South Carolina" = list(primary = "#73000A", secondary = "#000000"),
  "South Florida" = list(primary = "#006747", secondary = "#CFC493"),
  "Southern Miss" = list(primary = "#000000", secondary = "#FFAA3C"),
  "Stanford" = list(primary = "#8C1515", secondary = "#FFFFFF"),
  "Syracuse" = list(primary = "#F76900", secondary = "#000E54"),
  "TCU" = list(primary = "#4D1979", secondary = "#A3A9AC"),
  "Temple" = list(primary = "#9B1B1E", secondary = "#FFFFFF"),
  "Tennessee" = list(primary = "#FF8200", secondary = "#FFFFFF"),
  "Texas" = list(primary = "#BF5700", secondary = "#FFFFFF"),
  "Texas A&M" = list(primary = "#500000", secondary = "#FFFFFF"),
  "Texas State" = list(primary = "#501214", secondary = "#B29063"),
  "Texas Tech" = list(primary = "#CC0000", secondary = "#000000"),
  "Toledo" = list(primary = "#003E7E", secondary = "#FFCE00"),
  "Troy" = list(primary = "#890028", secondary = "#A59665"),
  "Tulane" = list(primary = "#006747", secondary = "#7BAFD4"),
  "Tulsa" = list(primary = "#002D72", secondary = "#C8102E"),
  "UAB" = list(primary = "#1E6B52", secondary = "#FFCE00"),
  "UCLA" = list(primary = "#2D68C4", secondary = "#FFD100"),
  "UMass" = list(primary = "#881C1C", secondary = "#FFFFFF"),
  "UNLV" = list(primary = "#B10202", secondary = "#474747"),
  "USC" = list(primary = "#990000", secondary = "#FFC72C"),
  "Utah" = list(primary = "#CC0000", secondary = "#FFFFFF"),
  "Utah State" = list(primary = "#0F2439", secondary = "#97999B"),
  "UTEP" = list(primary = "#FF8200", secondary = "#041E42"),
  "UTSA" = list(primary = "#0C2340", secondary = "#F15A22"),
  "Vanderbilt" = list(primary = "#866D4B", secondary = "#000000"),
  "Virginia" = list(primary = "#232D4B", secondary = "#F84C1E"),
  "Virginia Tech" = list(primary = "#630031", secondary = "#CF4420"),
  "Wake Forest" = list(primary = "#9E7E38", secondary = "#000000"),
  "Washington" = list(primary = "#4B2E83", secondary = "#B7A57A"),
  "Washington State" = list(primary = "#981E32", secondary = "#5E6A71"),
  "West Virginia" = list(primary = "#002855", secondary = "#EAAA00"),
  "Western Kentucky" = list(primary = "#C8102E", secondary = "#FFFFFF"),
  "Western Michigan" = list(primary = "#5B3819", secondary = "#FFCB0B"),
  "Wisconsin" = list(primary = "#C5050C", secondary = "#FFFFFF"),
  "Wyoming" = list(primary = "#492F24", secondary = "#FFC425")
)

get_universal_colors <- function(team_name, type = "primary") {
  if (team_name %in% names(UNIVERSAL_TEAM_COLORS)) {
    return(UNIVERSAL_TEAM_COLORS[[team_name]][[type]])
  }
  # Default fallback
  return(if (type == "primary")
    "#003366"
    else
      "#999999")
}


# Wrapper function for backwards compatibility with existing code
get_team_color <- function(team_name) {
  # Convert underscores to spaces for team name lookup
  # (sheet names use underscores, but color lookups expect spaces)
  team_name_lookup <- gsub("_", " ", team_name)
  
  # Use universal lookup
  return(get_universal_colors(team_name_lookup, "primary"))
}

# ===== DST SCORING FUNCTIONS (NFL MODE) =====
# Auto-detects NFL vs CFB based on defensive columns

# Check if defensive scoring is available
has_dst_scoring <- function(similar_games, team_names) {
  # Check if defensive columns exist (NFL format)
  def_cols_team1 <- paste0(team_names[1],
                           c("_Def_Sacks", "_Def_Ints", "_Def_Fum", "_Def_Pts_Allow"))
  def_cols_team2 <- paste0(team_names[2],
                           c("_Def_Sacks", "_Def_Ints", "_Def_Fum", "_Def_Pts_Allow"))
  
  all_def_cols <- c(def_cols_team1, def_cols_team2)
  all_exist <- all(all_def_cols %in% names(similar_games))
  
  return(all_exist)
}

# Calculate DraftKings NFL DST Points Allowed scoring
calculate_pts_allowed_score <- function(pts_allowed) {
  sapply(pts_allowed, function(pts) {
    if (is.na(pts))
      return(0)
    if (pts == 0)
      return(10)
    if (pts <= 6)
      return(7)
    if (pts <= 13)
      return(4)
    if (pts <= 20)
      return(1)
    if (pts <= 27)
      return(0)
    if (pts <= 34)
      return(-1)
    return(-4)  # 35+
  })
}

# Efficient team color highlighting for tables (F1-style)
# Pre-calculates colors and applies in one pass
apply_team_colors_to_table <- function(dt, data, team_col) {
  if (!team_col %in% names(data))
    return(dt)
  
  # Get unique teams and their colors
  unique_teams <- unique(data[[team_col]])
  team_colors <- sapply(unique_teams, get_team_color)
  
  # Create color mapping
  color_map <- setNames(team_colors, unique_teams)
  
  # Apply colors using styleEqual (much faster than row-by-row)
  dt <- dt %>%
    formatStyle(
      team_col,
      backgroundColor = styleEqual(unique_teams, paste0(team_colors, "20")),
      # 20 = 12% opacity
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
      cat("??? Fixing malformed column 'CPT_OwnPos' - splitting into separate columns\n")
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
simulate_team_game <- function(sim_id,
                               team_name,
                               team_data,
                               sampled_game,
                               dk_salaries,
                               similar_games,
                               use_dst = FALSE,
                               opponent_ints = 0) {
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
  if (is.na(team_rush_yds) ||
      length(team_rush_yds) == 0)
    team_rush_yds <- 0
  if (is.na(team_pass_yds) ||
      length(team_pass_yds) == 0)
    team_pass_yds <- 0
  if (is.na(team_rush_tds) ||
      length(team_rush_tds) == 0)
    team_rush_tds <- 0
  if (is.na(team_pass_tds) ||
      length(team_pass_tds) == 0)
    team_pass_tds <- 0
  if (is.na(team_fgs) || length(team_fgs) == 0)
    team_fgs <- 0
  
  # Pre-allocate results list
  all_player_results <- list()
  result_idx <- 1
  
  # Helper function: Calculate compression factor based on team total extremeness
  # Higher team totals = more compression (narrower individual ranges)
  calculate_compression <- function(team_total, similar_games_column) {
    if (length(similar_games_column) == 0 ||
        is.na(team_total))
      return(0)
    
    # Calculate percentile of this team total
    team_percentile <- ecdf(similar_games_column)(team_total)
    
    # Compression increases as we move away from median (0.50)
    # Distance from median: 0 (at median) to 0.5 (at extremes)
    distance_from_median <- abs(team_percentile - 0.50)
    
    # Convert to compression: 0 at median, up to 0.85 at extremes
    # This means at 99th percentile, we compress range by 85%
    compression <- distance_from_median * 1.7  # 0.5 * 1.7 = 0.85 max
    compression <- min(compression, 0.85)  # Cap at 85%
    
    return(compression)
  }
  
  # Helper function: sample from percentiles with optional compression
  # compression = 0 means full range, compression = 0.85 means very narrow range around median
  sample_from_percentiles <- function(floor,
                                      p25,
                                      p50,
                                      p75,
                                      ceiling,
                                      compression = 0) {
    percentile <- runif(1, 0, 1)
    
    # Apply compression by squeezing percentile toward 0.50 (median)
    if (compression > 0) {
      # Compress the percentile range toward median
      compressed_percentile <- 0.50 + (percentile - 0.50) * (1 - compression)
      percentile <- compressed_percentile
    }
    
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
  
  # CALCULATE COMPRESSION FACTORS based on team total extremeness
  rush_compression <- calculate_compression(team_rush_yds, similar_games[[rush_col]])
  pass_compression <- calculate_compression(team_pass_yds, similar_games[[pass_col]])
  
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
      if (is.na(floor_pct))
        floor_pct <- 0
      if (is.na(p25_pct))
        p25_pct <- floor_pct
      if (is.na(p50_pct))
        p50_pct <- p25_pct
      if (is.na(p75_pct))
        p75_pct <- p50_pct
      if (is.na(ceiling_pct))
        ceiling_pct <- p75_pct
      
      # SMART STOP: If we've allocated 98%+ and this player has Floor=0, skip
      if (allocated_share > 0.98 && floor_pct == 0) {
        rush_yds_allocation[i] <- 0
        next
      }
      
      # Sample player share WITH COMPRESSION
      player_share <- sample_from_percentiles(floor_pct,
                                              p25_pct,
                                              p50_pct,
                                              p75_pct,
                                              ceiling_pct,
                                              rush_compression)
      
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
          rush_yds_allocation[(i + 1):n_rushers] <- 0
        }
        break
      }
    }
    
    # IMPROVED SWEEP UP: Distribute remaining yards respecting ceiling constraints
    if (remaining_share > 0.01) {
      # Pass 1: Go through players from top, cap at 20% of remaining per player
      for (i in 1:n_rushers) {
        if (remaining_share <= 0.01)
          break
        
        ceiling_pct <- rushing_data$Ceiling[i]
        if (is.na(ceiling_pct))
          ceiling_pct <- rushing_data$Pct_P75[i]
        if (is.na(ceiling_pct))
          ceiling_pct <- 1.0
        
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
          if (remaining_share <= 0.01)
            break
          
          ceiling_pct <- rushing_data$Ceiling[i]
          if (is.na(ceiling_pct))
            ceiling_pct <- 1.0
          
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
            if (rush_yds_allocation[i] > 0) {
              # Only active players
              ceiling_pct <- rushing_data$Ceiling[i]
              if (is.na(ceiling_pct))
                ceiling_pct <- 1.0
              
              current_share <- rush_yds_allocation[i] / team_rush_yds
              room <- ceiling_pct - current_share
              
              if (room > 0.001) {
                # Has room
                players_with_room <- c(players_with_room, i)
                room_amounts <- c(room_amounts, room)
              }
            }
          }
          
          # If no one has room, we're done (better to under-allocate than break ceilings)
          if (length(players_with_room) == 0)
            break
          
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
          
          # AGGRESSIVE diminishing returns: Each TD drastically reduces probability
          # After 1 TD: 0.3x probability
          # After 2 TDs: 0.09x probability (basically impossible to get 3rd)
          reduction_factor <- 0.3^td_allocation[selected]
          td_probs_eligible[selected_idx] <- td_probs_eligible[selected_idx] * reduction_factor
          
          if (sum(td_probs_eligible) > 0) {
            td_probs_eligible <- td_probs_eligible / sum(td_probs_eligible)
          } else {
            td_probs_eligible <- rep(1 / length(eligible), length(eligible))
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
        SimID = sim_id,
        Team = team_name,
        Player = rushing_data$Player[i],
        PassYds = 0,
        PassTDs = 0L,
        INTs = 0L,
        RushYds = rush_yds_allocation[i],
        RushTDs = as.integer(td_allocation[i]),
        Recs = 0L,
        RecYds = 0,
        RecTDs = 0L,
        FGsMade = 0L,
        FG_Under30 = 0L,
        FG_30_39 = 0L,
        FG_40_49 = 0L,
        FG_50Plus = 0L,
        XPs = 0L,
        FumLost = 0L
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
    if (is.na(team_total_recs) ||
        length(team_total_recs) == 0)
      team_total_recs <- 0
    
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
      if (is.na(floor_pct))
        floor_pct <- 0
      if (is.na(p25_pct))
        p25_pct <- floor_pct
      if (is.na(p50_pct))
        p50_pct <- p25_pct
      if (is.na(p75_pct))
        p75_pct <- p50_pct
      if (is.na(ceiling_pct))
        ceiling_pct <- p75_pct
      
      # WORKLOAD CONSTRAINT: If player had hot rushing game, cap receiving at P50
      player_name <- receiving_data$Player[i]
      if (player_name %in% names(player_rush_outcome)) {
        if (player_rush_outcome[[player_name]] == "hot") {
          ceiling_pct <- min(ceiling_pct, p50_pct)
        }
      }
      
      # Sample player share WITH COMPRESSION
      target_share <- sample_from_percentiles(floor_pct,
                                              p25_pct,
                                              p50_pct,
                                              p75_pct,
                                              ceiling_pct,
                                              pass_compression)
      
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
      if (is.na(ypr) || ypr <= 0)
        ypr <- 10
      ypr_values[i] <- ypr
      
      # HARD STOP: If we've allocated everything, done
      if (remaining_share <= 0.01) {
        if (i < n_receivers) {
          rec_yds_allocation[(i + 1):n_receivers] <- 0
          ypr_values[(i + 1):n_receivers] <- 10
        }
        break
      }
    }
    
    # IMPROVED SWEEP UP: Distribute remaining yards respecting ceiling constraints
    if (remaining_share > 0.01) {
      # Pass 1: Go through players from top, cap at 20% of remaining per player
      for (i in 1:n_receivers) {
        if (remaining_share <= 0.01)
          break
        
        ceiling_pct <- receiving_data$Ceiling[i]
        if (is.na(ceiling_pct))
          ceiling_pct <- receiving_data$Pct_P75[i]
        if (is.na(ceiling_pct))
          ceiling_pct <- 1.0
        
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
          if (remaining_share <= 0.01)
            break
          
          ceiling_pct <- receiving_data$Ceiling[i]
          if (is.na(ceiling_pct))
            ceiling_pct <- 1.0
          
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
            if (rec_yds_allocation[i] > 0) {
              # Only active players
              ceiling_pct <- receiving_data$Ceiling[i]
              if (is.na(ceiling_pct))
                ceiling_pct <- 1.0
              
              current_share <- rec_yds_allocation[i] / team_pass_yds
              room <- ceiling_pct - current_share
              
              if (room > 0.001) {
                # Has room
                players_with_room <- c(players_with_room, i)
                room_amounts <- c(room_amounts, room)
              }
            }
          }
          
          # If no one has room, we're done (better to under-allocate than break ceilings)
          if (length(players_with_room) == 0)
            break
          
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
        if (is.na(p50_pct) || p50_pct <= 0)
          p50_pct <- 0.01
        
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
    
    # Second pass: Allocate receptions using POISSON DISTRIBUTION
    # This prevents unrealistic combinations like "4 catches for 1 yard"
    rec_allocation <- rep(0, n_receivers)
    
    if (team_total_recs > 0) {
      # Step 1: Calculate expected catches for each player based on their yards and historical YPR
      expected_catches <- numeric(n_receivers)
      
      for (i in 1:n_receivers) {
        if (rec_yds_allocation[i] > 0) {
          # Expected catches = yards / YPR
          expected_catches[i] <- rec_yds_allocation[i] / adjusted_ypr_values[i]
          expected_catches[i] <- max(1, expected_catches[i])  # Min 1 if they have yards
          
          # Sample from Poisson distribution for realistic variance
          simulated_catches <- rpois(1, lambda = expected_catches[i])
          simulated_catches <- max(1, simulated_catches)  # Ensure at least 1
          
          # Cap at reasonable maximum (prevent outliers like 50 catches)
          max_realistic <- ceiling(expected_catches[i] * 1.5)
          rec_allocation[i] <- min(simulated_catches, max_realistic)
        }
      }
      
      # Step 2: Scale to match team total receptions
      actual_total <- sum(rec_allocation)
      
      # If we're over team total, remove catches intelligently
      while (sum(rec_allocation) > team_total_recs) {
        # Find player who can best afford to lose a catch
        # (high catch count, maintains reasonable YPR after reduction)
        cushion <- numeric(n_receivers)
        for (i in 1:n_receivers) {
          if (rec_allocation[i] > 1 && rec_yds_allocation[i] > 0) {
            # How far above minimum can we go?
            new_ypr_if_reduced <- rec_yds_allocation[i] / (rec_allocation[i] - 1)
            # Prefer reducing from players with low YPR impact
            cushion[i] <- rec_allocation[i] - 1
          } else {
            cushion[i] <- -999  # Don't reduce below 1
          }
        }
        
        if (max(cushion) <= 0)
          break  # Can't reduce any more
        
        reduce_idx <- which.max(cushion)
        rec_allocation[reduce_idx] <- rec_allocation[reduce_idx] - 1
      }
      
      # If we're under team total, add catches intelligently
      while (sum(rec_allocation) < team_total_recs) {
        # Find players who could plausibly have more catches
        room <- numeric(n_receivers)
        for (i in 1:n_receivers) {
          if (rec_yds_allocation[i] > 0) {
            # Max realistic catches = yards / (YPR * 0.7) - generous allowance for short catches
            max_plausible <- ceiling(rec_yds_allocation[i] / (adjusted_ypr_values[i] * 0.7))
            room[i] <- max(0, max_plausible - rec_allocation[i])
          } else {
            room[i] <- 0  # Don't add to players with 0 yards
          }
        }
        
        if (sum(room) == 0)
          break  # No room to add
        
        # Add to player weighted by room available
        add_probs <- room / sum(room)
        add_idx <- sample(1:n_receivers, 1, prob = add_probs)
        rec_allocation[add_idx] <- rec_allocation[add_idx] + 1
      }
      
      # Step 3: FINAL VALIDATION - ensure no impossible combinations
      for (i in 1:n_receivers) {
        # Yards > 0 ??? catches >= 1
        if (rec_yds_allocation[i] > 0 && rec_allocation[i] == 0) {
          rec_allocation[i] <- 1
        }
        
        # Prevent absurd YPR (catches way too high for yards)
        if (rec_allocation[i] > 0 && rec_yds_allocation[i] > 0) {
          actual_ypr <- rec_yds_allocation[i] / rec_allocation[i]
          # If YPR drops below 3.0, that's too many catches for the yards
          if (actual_ypr < 3.0) {
            # Reduce catches to maintain at least 3.0 YPR
            rec_allocation[i] <- max(1, floor(rec_yds_allocation[i] / 3.0))
          }
        }
      }
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
          
          # AGGRESSIVE diminishing returns: Each TD drastically reduces probability
          # After 1 TD: 0.3x probability
          # After 2 TDs: 0.09x probability (basically impossible to get 3rd)
          reduction_factor <- 0.3^td_allocation[selected]
          td_probs_eligible[selected_idx] <- td_probs_eligible[selected_idx] * reduction_factor
          
          if (sum(td_probs_eligible) > 0) {
            td_probs_eligible <- td_probs_eligible / sum(td_probs_eligible)
          } else {
            td_probs_eligible <- rep(1 / length(eligible), length(eligible))
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
          eligible_to_reduce <- which(rec_allocation > td_allocation &
                                        (1:n_receivers) != i)
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
      # Yards > 0 ??? catches >= 1
      if (rec_yds_allocation[i] > 0 && rec_allocation[i] == 0) {
        rec_allocation[i] <- 1
      }
      # TDs > 0 ??? catches >= TDs
      if (td_allocation[i] > 0 &&
          rec_allocation[i] < td_allocation[i]) {
        rec_allocation[i] <- td_allocation[i]
      }
    }
    
    # Now create player results
    for (i in 1:n_receivers) {
      player_name <- receiving_data$Player[i]
      existing_idx <- which(sapply(all_player_results, function(x)
        x$Player == player_name))
      
      if (length(existing_idx) > 0) {
        all_player_results[[existing_idx[1]]]$Recs <- as.integer(rec_allocation[i])
        all_player_results[[existing_idx[1]]]$RecYds <- rec_yds_allocation[i]
        all_player_results[[existing_idx[1]]]$RecTDs <- as.integer(td_allocation[i])
      } else {
        all_player_results[[result_idx]] <- list(
          SimID = sim_id,
          Team = team_name,
          Player = player_name,
          PassYds = 0,
          PassTDs = 0L,
          INTs = 0L,
          RushYds = 0,
          RushTDs = 0L,
          Recs = as.integer(rec_allocation[i]),
          RecYds = rec_yds_allocation[i],
          RecTDs = as.integer(td_allocation[i]),
          FGsMade = 0L,
          FG_Under30 = 0L,
          FG_30_39 = 0L,
          FG_40_49 = 0L,
          FG_50Plus = 0L,
          XPs = 0L,
          FumLost = 0L
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
      if (is.na(pass_share))
        pass_share <- 0
      
      player_pass_yds <- round(team_pass_yds * pass_share)
      player_pass_tds <- round(team_pass_tds * pass_share)
      
      player_name <- passing_data$Player[i]
      existing_idx <- which(sapply(all_player_results, function(x)
        x$Player == player_name))
      
      if (length(existing_idx) > 0) {
        all_player_results[[existing_idx[1]]]$PassYds <- player_pass_yds
        all_player_results[[existing_idx[1]]]$PassTDs <- player_pass_tds
        all_player_results[[existing_idx[1]]]$INTs <- as.integer(opponent_ints)
      } else {
        all_player_results[[result_idx]] <- list(
          SimID = sim_id,
          Team = team_name,
          Player = player_name,
          PassYds = player_pass_yds,
          PassTDs = player_pass_tds,
          INTs = as.integer(opponent_ints),
          RushYds = 0,
          RushTDs = 0L,
          Recs = 0L,
          RecYds = 0,
          RecTDs = 0L,
          FGsMade = 0L,
          FG_Under30 = 0L,
          FG_30_39 = 0L,
          FG_40_49 = 0L,
          FG_50Plus = 0L,
          XPs = 0L,
          FumLost = 0L
        )
        result_idx <- result_idx + 1
      }
    }
  }
  
  # KICKING
  kicking_data <- team_data$kicking
  total_xps <- team_rush_tds + team_pass_tds
  
  # Get kicker name from kicking sheet
  kicker <- if (nrow(kicking_data) > 0 &&
                "Kicker" %in% names(kicking_data)) {
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
      if (distance_cat == 1)
        fg_under30 <- fg_under30 + 1L
      else if (distance_cat == 2)
        fg_30_39 <- fg_30_39 + 1L
      else if (distance_cat == 3)
        fg_40_49 <- fg_40_49 + 1L
      else
        fg_50plus <- fg_50plus + 1L
    }
  }
  
  all_player_results[[result_idx]] <- list(
    SimID = sim_id,
    Team = team_name,
    Player = kicker,
    PassYds = 0,
    PassTDs = 0L,
    INTs = 0L,
    RushYds = 0,
    RushTDs = 0L,
    Recs = 0L,
    RecYds = 0,
    RecTDs = 0L,
    FGsMade = as.integer(team_fgs),
    FG_Under30 = fg_under30,
    FG_30_39 = fg_30_39,
    FG_40_49 = fg_40_49,
    FG_50Plus = fg_50plus,
    XPs = as.integer(total_xps),
    FumLost = 0L
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
  cat(sprintf(
    "\n=== RUNNING %s SIMULATIONS ===\n",
    format(n_sims, big.mark = ",")
  ))
  
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
    cat("??? DST scoring detected (NFL mode)\n\n")
  } else {
    cat("??? No DST data (CFB mode)\n\n")
  }
  
  # Pre-allocate (2 teams + 2 DST if NFL)
  list_size <- if (use_dst)
    n_sims * 4
  else
    n_sims * 2
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
    if (length(team1_dst_name) == 0)
      team1_dst_name <- paste0(team_names[1], " DST")
    else
      team1_dst_name <- team1_dst_name[1]
    
    team2_dst_name <- dk_salaries %>%
      filter(Team == team_names[2], Pos == "DST") %>%
      pull(Name)
    if (length(team2_dst_name) == 0)
      team2_dst_name <- paste0(team_names[2], " DST")
    else
      team2_dst_name <- team2_dst_name[1]
    
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
        if (is.na(team1_dst_ints))
          team1_dst_ints <- 0
        if (is.na(team2_dst_ints))
          team2_dst_ints <- 0
        
        # Team1's defense INTs ??? Team2's QB threw them
        team2_opponent_ints <- team1_dst_ints
        # Team2's defense INTs ??? Team1's QB threw them
        team1_opponent_ints <- team2_dst_ints
      }
      
      team1_result <- simulate_team_game(
        sim,
        team_names[1],
        team1_data,
        sampled_game,
        dk_salaries,
        similar_games,
        use_dst,
        team1_opponent_ints
      )
      all_results[[result_idx]] <- team1_result
      result_idx <- result_idx + 1
      
      team2_result <- simulate_team_game(
        sim,
        team_names[2],
        team2_data,
        sampled_game,
        dk_salaries,
        similar_games,
        use_dst,
        team2_opponent_ints
      )
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
        if (is.na(team1_dst_sacks))
          team1_dst_sacks <- 0
        if (is.na(team1_dst_fum))
          team1_dst_fum <- 0
        if (is.na(team1_dst_pts_allow))
          team1_dst_pts_allow <- 20
        if (is.na(team1_dst_def_tds))
          team1_dst_def_tds <- 0
        if (is.na(team1_dst_safeties))
          team1_dst_safeties <- 0
        if (is.na(team2_dst_sacks))
          team2_dst_sacks <- 0
        if (is.na(team2_dst_fum))
          team2_dst_fum <- 0
        if (is.na(team2_dst_pts_allow))
          team2_dst_pts_allow <- 20
        if (is.na(team2_dst_def_tds))
          team2_dst_def_tds <- 0
        if (is.na(team2_dst_safeties))
          team2_dst_safeties <- 0
        
        # PRE-CALCULATE DST fantasy points (FAST - no vectorization needed)
        team1_pts_allow_score <- if (team1_dst_pts_allow == 0)
          10
        else
          if (team1_dst_pts_allow <= 6)
            7
        else
          if (team1_dst_pts_allow <= 13)
            4
        else
          if (team1_dst_pts_allow <= 20)
            1
        else
          if (team1_dst_pts_allow <= 27)
            0
        else
          if (team1_dst_pts_allow <= 34)
            - 1
        else-4
        
        team2_pts_allow_score <- if (team2_dst_pts_allow == 0)
          10
        else
          if (team2_dst_pts_allow <= 6)
            7
        else
          if (team2_dst_pts_allow <= 13)
            4
        else
          if (team2_dst_pts_allow <= 20)
            1
        else
          if (team2_dst_pts_allow <= 27)
            0
        else
          if (team2_dst_pts_allow <= 34)
            - 1
        else-4
        
        team1_dst_total <- (team1_dst_sacks * 1) + (team1_dst_ints * 2) + (team1_dst_fum * 2) +
          team1_pts_allow_score + (team1_dst_def_tds * 6) + (team1_dst_safeties * 2)
        team2_dst_total <- (team2_dst_sacks * 1) + (team2_dst_ints * 2) + (team2_dst_fum * 2) +
          team2_pts_allow_score + (team2_dst_def_tds * 6) + (team2_dst_safeties * 2)
        
        # Store DST with pre-calculated points
        team1_dst_result <- data.table(
          SimID = sim,
          Team = team_names[1],
          Player = team1_dst_name,
          PassYds = 0,
          PassTDs = 0L,
          INTs = 0L,
          RushYds = 0,
          RushTDs = 0L,
          Recs = 0L,
          RecYds = 0,
          RecTDs = 0L,
          FGsMade = 0L,
          FG_Under30 = 0L,
          FG_30_39 = 0L,
          FG_40_49 = 0L,
          FG_50Plus = 0L,
          XPs = 0L,
          FumLost = 0L,
          PassPts = 0,
          RushPts = 0,
          RecPts = 0,
          KickPts = 0,
          FumPts = 0,
          DSTPts = team1_dst_total,
          TotalPts = team1_dst_total
        )
        
        team2_dst_result <- data.table(
          SimID = sim,
          Team = team_names[2],
          Player = team2_dst_name,
          PassYds = 0,
          PassTDs = 0L,
          INTs = 0L,
          RushYds = 0,
          RushTDs = 0L,
          Recs = 0L,
          RecYds = 0,
          RecTDs = 0L,
          FGsMade = 0L,
          FG_Under30 = 0L,
          FG_30_39 = 0L,
          FG_40_49 = 0L,
          FG_50Plus = 0L,
          XPs = 0L,
          FumLost = 0L,
          PassPts = 0,
          RushPts = 0,
          RecPts = 0,
          KickPts = 0,
          FumPts = 0,
          DSTPts = team2_dst_total,
          TotalPts = team2_dst_total
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
      
      cat(
        sprintf(
          "Batch %d/%d: %d/%d sims (%.1f%%) | %.0f sims/sec | ETA: %.0fs\n",
          batch,
          n_batches,
          sims_done,
          n_sims,
          (sims_done / n_sims) * 100,
          rate,
          eta
        )
      )
    }
    
    if (batch %% 10 == 0)
      gc(verbose = FALSE, full = FALSE)
  }
  
  combined <- rbindlist(all_results, use.names = TRUE, fill = TRUE)
  
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(
    sprintf(
      "\n??? Complete! %.1f seconds (%.0f sims/sec)\n\n",
      total_time,
      n_sims / total_time
    )
  )
  
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
    scoring_stats <- merge(
      scoring_stats,
      dk_salaries[, ..dk_cols],
      by.x = "Player",
      by.y = "Name",
      all.x = TRUE
    )
    
  }
  
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
    TotalTDs = sum(RushTDs, na.rm = TRUE) + sum(RecTDs, na.rm = TRUE),
    # Rush TDs + Rec TDs (which include passing)
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
  
  cat(sprintf(
    "Processing %s simulations...\n",
    format(n_actual_sims, big.mark = ",")
  ))
  
  # Check if correct ID columns exist and select relevant columns including ownership
  if ("DFS_ID" %in% names(dk_salaries) &&
      "CPT_DFS_ID" %in% names(dk_salaries)) {
    # Use the actual ID columns
    cols_to_select <- c("Name", "Salary", "DFS_ID", "CPT_DFS_ID")
    
    # Add ownership columns if they exist
    if ("CPT_Own" %in% names(dk_salaries))
      cols_to_select <- c(cols_to_select, "CPT_Own")
    if ("Flex_Own" %in% names(dk_salaries))
      cols_to_select <- c(cols_to_select, "Flex_Own")
    
    dk_salaries_with_ids <- dk_salaries[, ..cols_to_select]
  } else {
    # Generate IDs if columns don't exist
    dk_salaries_with_ids <- dk_salaries[, .(Name, Salary)]
    dk_salaries_with_ids[, DFS_ID := paste0("ID", 1:.N)]
    dk_salaries_with_ids[, CPT_DFS_ID := paste0("CPTID", 1:.N)]
    cat("Note: DFS_ID/CPT_DFS_ID columns not found, using generated IDs\n")
    
    # Add ownership if exists
    if ("CPT_Own" %in% names(dk_salaries))
      dk_salaries_with_ids[, CPT_Own := dk_salaries$CPT_Own]
    if ("Flex_Own" %in% names(dk_salaries))
      dk_salaries_with_ids[, Flex_Own := dk_salaries$Flex_Own]
  }
  
  # Merge salary data ONCE
  sim_results <- merge(
    sim_results,
    dk_salaries_with_ids,
    by.x = "Player",
    by.y = "Name",
    all.x = TRUE
  )
  
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
        if (found_count >= top_k)
          break
        
        cpt_player <- sim_data$Player[cpt_idx]
        cpt_team <- sim_data$Team[cpt_idx]
        cpt_salary <- sim_data$CPT_Salary[cpt_idx]
        
        remaining_budget <- SALARY_CAP - cpt_salary
        
        flex_candidates <- sim_data[Player != cpt_player &
                                      Salary <= remaining_budget]
        
        if (nrow(flex_candidates) < 5)
          next
        
        selected <- flex_candidates[1:min(20, nrow(flex_candidates))]
        
        lineup_players <- character(0)
        lineup_teams <- cpt_team
        total_salary <- 0
        
        for (i in 1:nrow(selected)) {
          if (length(lineup_players) >= 5)
            break
          
          player <- selected$Player[i]
          player_team <- selected$Team[i]
          player_salary <- selected$Salary[i]
          
          if (total_salary + player_salary <= remaining_budget) {
            lineup_players <- c(lineup_players, player)
            lineup_teams <- unique(c(lineup_teams, player_team))
            total_salary <- total_salary + player_salary
          }
        }
        
        if (length(lineup_players) != 5 ||
            length(lineup_teams) < n_teams)
          next
        
        # Create lineup string with captain marked (so different captains = different lineups)
        lineup_str <- paste0("CPT:", cpt_player, "|", paste(sort(lineup_players), collapse = "|"))
        
        if (lineup_str %in% found_lineups[1:found_count])
          next
        
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
      
      cat(
        sprintf(
          "Batch %d/%d (%.1f%%) | %.0f sims/sec | ETA: %.0fs\n",
          batch,
          n_batches,
          (sims_processed / n_actual_sims) * 100,
          rate,
          eta
        )
      )
    }
    
    if (batch %% 5 == 0)
      gc(verbose = FALSE, full = FALSE)
  }
  
  cat("\nAggregating results...\n")
  
  valid_lineups <- all_lineups[!sapply(all_lineups, is.null)]
  
  if (length(valid_lineups) == 0) {
    cat("??? No valid lineups found\n")
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
      if (length(p) >= i)
        p[i]
      else
        NA_character_
    })]
  }
  
  lineup_counts[, Players := NULL]
  lineup_counts[, FlexPlayers := NULL]
  lineup_counts[, Lineup := NULL]
  
  # Add IDs and calculate ownership/salary metrics
  cpt_id_lookup <- setNames(dk_salaries_with_ids$CPT_DFS_ID,
                            dk_salaries_with_ids$Name)
  flex_id_lookup <- setNames(dk_salaries_with_ids$DFS_ID, dk_salaries_with_ids$Name)
  salary_lookup <- setNames(dk_salaries_with_ids$Salary, dk_salaries_with_ids$Name)
  
  # Get ownership data if available
  if ("CPT_Own" %in% names(dk_salaries_with_ids) &&
      "Flex_Own" %in% names(dk_salaries_with_ids)) {
    cpt_own_lookup <- setNames(dk_salaries_with_ids$CPT_Own, dk_salaries_with_ids$Name)
    flex_own_lookup <- setNames(dk_salaries_with_ids$Flex_Own,
                                dk_salaries_with_ids$Name)
    has_ownership <- TRUE
    cat("??? Ownership data found - adding ownership metrics to lineups\n")
  } else {
    has_ownership <- FALSE
    cat("??? No ownership data (CPT_Own/Flex_Own) - skipping ownership metrics\n")
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
      teams[i + 1] <- player_team_lookup[gsub(" \\(.*\\)", "", get(player_col))]
    }
    # Count by team
    team1_count <- sum(teams == team_names[1], na.rm = TRUE)
    team2_count <- sum(teams == team_names[2], na.rm = TRUE)
    
    # Use team1 name as prefix
    paste0(team_names[1], " ", team1_count, "-", team2_count)
  }, by = 1:nrow(lineup_counts)]
  
  # Remove individual rank count columns, keep only Top counts
  player_cols <- paste0("Player", 1:5)
  
  if (has_ownership) {
    final_cols <- c(
      "Captain",
      player_cols,
      "Top1Count",
      "Top2Count",
      "Top3Count",
      "Top5Count",
      "TotalSalary",
      "CumulativeOwnership",
      "GeometricMeanOwnership",
      "TeamStack"
    )
  } else {
    final_cols <- c(
      "Captain",
      player_cols,
      "Top1Count",
      "Top2Count",
      "Top3Count",
      "Top5Count",
      "TotalSalary",
      "TeamStack"
    )
  }
  
  result <- lineup_counts[, ..final_cols]
  setorder(result, -Top1Count)
  
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  cat(sprintf(
    "\n??? Found %s unique lineups in %.1f seconds\n\n",
    format(nrow(result), big.mark = ","),
    total_time
  ))
  
  return(as.data.frame(result))
}

# Calculate player exposure from generated lineups
calculate_player_exposure <- function(optimal_lineups,
                                      player_mapping,
                                      generated_lineups) {
  if (is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
    return(data.frame(Message = "No optimal lineups available."))
  }
  
  # Get all players from optimal lineups (strip IDs)
  player_cols <- c("Captain", paste0("Player", 1:5))
  all_players <- c()
  for (col in player_cols) {
    if (col %in% names(optimal_lineups)) {
      players <- gsub(" \\([^)]+\\)$", "", optimal_lineups[[col]])
      all_players <- c(all_players, players)
    }
  }
  all_players <- unique(all_players[!is.na(all_players) &
                                      all_players != ""])
  
  if (length(all_players) == 0) {
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
  if (!is.null(player_mapping) && nrow(player_mapping) > 0) {
    for (i in 1:nrow(metrics_data)) {
      player_name <- metrics_data$Player[i]
      matches <- which(player_mapping$Name == player_name)
      
      if (length(matches) > 0) {
        match_idx <- matches[1]
        if ("Salary" %in% names(player_mapping)) {
          metrics_data$Salary[i] <- player_mapping$Salary[match_idx]
        }
        if ("Proj" %in% names(player_mapping)) {
          metrics_data$Proj[i] <- player_mapping$Proj[match_idx]
        }
        if ("Flex_Own" %in% names(player_mapping)) {
          metrics_data$FlexOwn[i] <- player_mapping$Flex_Own[match_idx]
        }
        if ("CPT_Own" %in% names(player_mapping)) {
          metrics_data$CPTOwn[i] <- player_mapping$CPT_Own[match_idx]
        }
      }
    }
  }
  
  # Calculate OptimalRate from filtered pool
  total_lineups <- nrow(optimal_lineups)
  if (total_lineups > 0) {
    for (player in all_players) {
      # Count appearances (Captain + Flex combined)
      captain_appears <- gsub(" \\([^)]+\\)$", "", optimal_lineups$Captain) == player
      captain_count <- sum(captain_appears, na.rm = TRUE)
      
      flex_count <- 0
      for (col in paste0("Player", 1:5)) {
        if (col %in% names(optimal_lineups)) {
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
  if (!is.null(generated_lineups) &&
      nrow(generated_lineups) > 0) {
    for (player in all_players) {
      # Count appearances in generated lineups
      captain_appears <- gsub(" \\([^)]+\\)$", "", generated_lineups$Captain) == player
      captain_count <- sum(captain_appears, na.rm = TRUE)
      
      flex_count <- 0
      for (col in paste0("Player", 1:5)) {
        if (col %in% names(generated_lineups)) {
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
      if (!is.na(metrics_data[metrics_data$Player == player, "FlexOwn"])) {
        own <- metrics_data[metrics_data$Player == player, "FlexOwn"]
        metrics_data[metrics_data$Player == player, "Leverage"] <- exposure_pct - own
      }
    }
  }
  
  # Sort by OptimalRate descending
  metrics_data <- metrics_data[order(-metrics_data$OptimalRate), ]
  
  return(metrics_data)
}


# UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "GTS Football Sim"),
  
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
      menuItem("Setup", tabName = "setup", icon = icon("upload")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("Team Stats", tabName = "team_stats", icon = icon("users")),
      menuItem(
        "Optimal Lineups",
        tabName = "optimal",
        icon = icon("trophy")
      ),
      menuItem(
        "Lineup Builder",
        tabName = "builder",
        icon = icon("clipboard-list")
      )
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    
    tabItems(
      # SETUP TAB (NEW)
      tabItem(
        tabName = "setup",
        
        fluidRow(
          box(
            title = "Contest Type",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            radioButtons(
              "contest_type",
              NULL,
              choices = c(
                "Showdown (Single Game)" = "showdown",
                "Classic (Multi-Game)" = "classic"
              ),
              selected = "showdown",
              inline = TRUE
            )
          )
        ),
        
        # SHOWDOWN MODE
        conditionalPanel(condition = "input.contest_type == 'showdown'", fluidRow(
          box(
            title = "Showdown Setup",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            fileInput("excel_file", "Upload Game File (.xlsx)", accept = ".xlsx"),
            conditionalPanel(
              condition = "output.file_uploaded",
              numericInput(
                "n_sims",
                "Number of Simulations:",
                value = 10000,
                min = 100,
                max = 100000,
                step = 1000
              ),
              actionButton(
                "run_sim",
                "Run Simulations",
                class = "btn-primary btn-lg btn-block",
                icon = icon("play"),
                style = "margin: 10px 0;"
              )
            )
          )
        )),
        
        # CLASSIC MODE
        conditionalPanel(condition = "input.contest_type == 'classic'", fluidRow(
          box(
            title = "Classic Slate Setup",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            fileInput(
              "classic_salary_file",
              "1. Upload DraftKings Salary File (CSV)",
              accept = c(".csv", ".xlsx")
            ),
            
            hr(),
            
            h4("2. Upload Game Simulation Files"),
            
            fluidRow(
              column(4, fileInput("classic_game_1", "Game 1:", accept = ".xlsx")),
              column(4, fileInput("classic_game_2", "Game 2:", accept = ".xlsx")),
              column(
                4,
                fileInput("classic_game_3", "Game 3 (optional):", accept = ".xlsx")
              )
            ),
            
            fluidRow(
              column(
                4,
                fileInput("classic_game_4", "Game 4 (optional):", accept = ".xlsx")
              ),
              column(
                4,
                fileInput("classic_game_5", "Game 5 (optional):", accept = ".xlsx")
              ),
              column(
                4,
                fileInput("classic_game_6", "Game 6 (optional):", accept = ".xlsx")
              )
            ),
            
            hr(),
            
            numericInput(
              "n_sims_classic",
              "Number of Simulations:",
              value = 10000,
              min = 1000,
              max = 100000,
              step = 1000
            ),
            
            actionButton(
              "run_simulation_classic",
              "Run Classic Simulation",
              class = "btn-primary btn-lg btn-block",
              icon = icon("play")
            )
          )
        ))
      ),
      
      # RESULTS TAB (YOUR ORIGINAL)
      tabItem(tabName = "results", fluidRow(
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
      ), fluidRow(
        box(
          width = 12,
          title = uiOutput("team1_violin_title"),
          status = "primary",
          solidHeader = TRUE,
          plotlyOutput("team1_violin", height = "600px") %>% withSpinner(color = "#FFD700")
        )
      ), fluidRow(
        box(
          width = 12,
          title = uiOutput("team2_violin_title"),
          status = "info",
          solidHeader = TRUE,
          plotlyOutput("team2_violin", height = "600px") %>% withSpinner(color = "#FFD700")
        )
      )),
      
      # TEAM STATS TAB (YOUR ORIGINAL)
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
      
      # OPTIMAL LINEUPS TAB (YOUR ORIGINAL)
      tabItem(tabName = "optimal", fluidRow(
        box(
          width = 12,
          title = "Generate Optimal Lineups",
          status = "primary",
          solidHeader = TRUE,
          actionButton(
            "generate_optimal",
            "Run Optimal Lineups",
            class = "btn-primary btn-lg",
            icon = icon("rocket")
          )
        )
      ), fluidRow(
        box(
          width = 12,
          title = "Optimal Lineups",
          status = "success",
          solidHeader = TRUE,
          DTOutput("optimal_lineups_table") %>% withSpinner(color = "#FFD700"),
          br(),
          downloadButton("download_optimal", "Download Optimal Lineups", class = "btn-primary")
        )
      )),
      
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
                column(
                  3,
                  numericInput(
                    "min_top1_count",
                    "Min Top 1 Count:",
                    value = 0,
                    min = 0
                  )
                ),
                
                # Showdown-only Top 2/3/5 counts
                conditionalPanel(
                  condition = "input.contest_type == 'showdown'",
                  column(
                    3,
                    numericInput(
                      "min_top2_count",
                      "Min Top 2 Count:",
                      value = 0,
                      min = 0
                    )
                  ),
                  column(
                    3,
                    numericInput(
                      "min_top3_count",
                      "Min Top 3 Count:",
                      value = 0,
                      min = 0
                    )
                  ),
                  column(
                    3,
                    numericInput(
                      "min_top5_count",
                      "Min Top 5 Count:",
                      value = 0,
                      min = 0
                    )
                  )
                )
              ),
              
              fluidRow(column(
                6,
                sliderInput(
                  "cumulative_ownership_range",
                  "Cumulative Ownership Range:",
                  min = 0,
                  max = 600,
                  value = c(0, 600),
                  step = 10,
                  post = "%"
                )
              ), column(
                6,
                sliderInput(
                  "geometric_mean_range",
                  "Geometric Mean Ownership Range:",
                  min = 0,
                  max = 100,
                  value = c(0, 100),
                  step = 1,
                  post = "%"
                )
              )),
              
              # Showdown-only filters
              conditionalPanel(
                condition = "input.contest_type == 'showdown'",
                fluidRow(
                  column(
                    3,
                    selectizeInput(
                      "excluded_captains",
                      "Exclude from Captain:",
                      choices = NULL,
                      multiple = TRUE,
                      selected = character(0),
                      options = list(plugins = list('remove_button'), placeholder = 'Exclude from CPT slot')
                    )
                  ),
                  column(
                    3,
                    selectizeInput(
                      "excluded_flex",
                      "Exclude from Flex:",
                      choices = NULL,
                      multiple = TRUE,
                      selected = character(0),
                      options = list(plugins = list('remove_button'), placeholder = 'Exclude from FLEX slots')
                    )
                  ),
                  column(
                    3,
                    selectizeInput(
                      "excluded_stacks",
                      "Exclude Stacks:",
                      choices = NULL,
                      multiple = TRUE,
                      selected = character(0),
                      options = list(plugins = list('remove_button'), placeholder = 'Click to select stacks to exclude')
                    )
                  ),
                  column(
                    3,
                    numericInput(
                      "num_random_lineups",
                      "Number of Lineups to Generate:",
                      value = 20,
                      min = 1,
                      max = 150
                    )
                  )
                ),
                fluidRow(
                  column(
                    3,
                    selectizeInput(
                      "locked_captain",
                      "Lock Captain:",
                      choices = NULL,
                      multiple = FALSE,
                      selected = character(0),
                      options = list(placeholder = 'Lock one captain (optional)')
                    )
                  ),
                  column(
                    3,
                    selectizeInput(
                      "locked_flex",
                      "Lock Flex Players:",
                      choices = NULL,
                      multiple = TRUE,
                      selected = character(0),
                      options = list(
                        plugins = list('remove_button'),
                        placeholder = 'Lock up to 5 flex (optional)',
                        maxItems = 5
                      )
                    )
                  ),
                  column(
                    3,
                    textInput(
                      "build_label",
                      "Build Label:",
                      value = "",
                      placeholder = "e.g., 'Chalk', 'Stack 1'"
                    )
                  ),
                  column(3, div(style = "margin-top: 25px;"))  # Spacer
                )
              ),
              
              # Classic-only filters
              conditionalPanel(condition = "input.contest_type == 'classic'", fluidRow(
                column(
                  3,
                  selectizeInput(
                    "classic_locked_players",
                    "Lock Players:",
                    choices = NULL,
                    multiple = TRUE,
                    selected = character(0),
                    options = list(
                      plugins = list('remove_button'),
                      placeholder = 'Lock up to 9 players (optional)',
                      maxItems = 9
                    )
                  )
                ),
                column(
                  3,
                  selectizeInput(
                    "classic_excluded_players",
                    "Exclude Players:",
                    choices = NULL,
                    multiple = TRUE,
                    selected = character(0),
                    options = list(plugins = list('remove_button'), placeholder = 'Exclude players from lineups')
                  )
                ),
                column(
                  3,
                  numericInput(
                    "num_random_lineups",
                    "Number of Lineups to Generate:",
                    value = 20,
                    min = 1,
                    max = 150
                  )
                ),
                column(
                  3,
                  textInput(
                    "build_label",
                    "Build Label:",
                    value = "",
                    placeholder = "e.g., 'Chalk', 'Contrarian'"
                  )
                )
              )),
              
              fluidRow(column(
                6,
                div(
                  style = "margin-top: 20px;",
                  actionButton(
                    "generate_lineups",
                    "Add Lineups to Portfolio",
                    class = "btn-primary btn-lg",
                    style = "width: 100%;"
                  ),
                  br(),
                  br(),
                  actionButton(
                    "clear_builds",
                    "Clear All Builds",
                    class = "btn-warning",
                    style = "width: 100%;"
                  )
                )
              ), column(
                6, div(
                  style = "margin-top: 20px;",
                  downloadButton("download_random_lineups", "Download Portfolio", style = "width: 100%;")
                )
              ))
            )
          ),
          
          box(
            width = 12,
            title = div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              span("Filtered Pool Player Statistics"),
              span(
                textOutput("filtered_pool_count_display", inline = TRUE),
                style = "font-size: 24px; font-weight: bold; color: #000000;"
              )
            ),
            status = "info",
            solidHeader = TRUE,
            DTOutput("filtered_pool_stats_table") %>% withSpinner(color = "#FFD700")
          ),
          
          # Build Summary
          fluidRow(
            box(
              width = 12,
              title = "Lineup Builds Summary",
              status = "warning",
              solidHeader = TRUE,
              DTOutput("builds_summary_table") %>% withSpinner(color = "#FFD700")
            )
          ),
          
          # Player Exposure Analysis (after randomization - all builds combined)
          fluidRow(
            box(
              width = 12,
              title = "Player Exposure Analysis (All Builds)",
              status = "info",
              solidHeader = TRUE,
              DTOutput("player_exposure_table") %>% withSpinner(color = "#FFD700")
            )
          ),
          
          # Generated Lineups (all builds)
          fluidRow(
            box(
              width = 12,
              title = "All Generated Lineups",
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
  rv <- reactiveValues(
    input_data = NULL,
    simulation_results = NULL,
    optimal_lineups = NULL,
    filtered_pool = NULL,
    lineup_builds = list(),
    # NEW: Changed from random_lineups to list of builds
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
          paste0(
            "File loaded successfully!\n",
            paste(rv$input_data$team_names, collapse = " vs "),
            "\n\nClick 'Run Simulations' to begin."
          ),
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
        
        # Convert underscores to spaces in team names throughout the app
        if (!is.null(rv$simulation_results) &&
            "Team" %in% names(rv$simulation_results)) {
          rv$simulation_results$Team <- gsub("_", " ", rv$simulation_results$Team)
        }
        
        rv$simulation_complete <- TRUE
        
        setProgress(1, detail = "Complete!")
        
        showModal(modalDialog(
          title = "Success!",
          paste0(
            format(input$n_sims, big.mark = ","),
            " simulations completed! Check the Results tab."
          ),
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
  
  
  # Handle Classic mode simulation
  observeEvent(input$run_simulation_classic, {
    req(input$classic_salary_file)
    
    # Collect all uploaded game files
    game_files <- list(
      input$classic_game_1,
      input$classic_game_2,
      input$classic_game_3,
      input$classic_game_4,
      input$classic_game_5,
      input$classic_game_6
    )
    
    # Keep only uploaded files
    game_files <- game_files[!sapply(game_files, is.null)]
    
    # Validate minimum files
    if (length(game_files) < 2) {
      showNotification("Please upload at least 2 game files",
                       type = "warning",
                       duration = 5)
      return()
    }
    
    withProgress(message = 'Running classic slate simulations...', value = 0, {
      tryCatch({
        setProgress(0.1, detail = "Reading salary file...")
        
        # Read DK salary file and STORE IT
        dk_salaries <- read_dk_salaries(input$classic_salary_file$datapath)
        
        showNotification(
          paste("Loaded", nrow(dk_salaries), "players from salary file"),
          type = "message",
          duration = 3
        )
        
        # Simulate each game
        all_game_results <- list()
        n_sims <- input$n_sims_classic
        
        for (i in seq_along(game_files)) {
          game_file <- game_files[[i]]
          
          setProgress(0.1 + (0.7 * i / length(game_files)),
                      detail = paste("Simulating game", i, "of", length(game_files), "..."))
          
          # Load this game file (same as showdown)
          game_data <- read_input_file(game_file$datapath)
          
          # Run simulation (same as showdown)
          game_sim_results <- run_simulations(game_data, n_sims)
          
          # Create game ID from team names (sorted alphabetically)
          teams_sorted <- sort(game_data$team_names)
          game_id <- paste(teams_sorted[1], "vs", teams_sorted[2], sep = "_")
          game_sim_results$Game <- game_id
          
          all_game_results[[game_id]] <- game_sim_results
        }
        
        
        setProgress(0.9, detail = "Combining results...")
        
        # Combine all games
        combined_results <- bind_rows(all_game_results)
        
        # Merge with DK salaries - simple join by Player, Team, Game
        combined_results <- combined_results %>%
          left_join(
            dk_salaries %>%
              rename(PlayerName = Name) %>%
              select(PlayerName, DFS_ID, Pos, Team, Game, Salary, Own),
            by = c("Player" = "PlayerName", "Team", "Game")
          ) %>%
          filter(!is.na(Pos))  # Remove unmatched (kickers)
        
        cat("Final player count:", nrow(combined_results), "\n")
        cat("Positions:\n")
        print(table(combined_results$Pos, useNA = "always"))
        
        # IMPORTANT: Store both results AND slate data
        rv$simulation_results <- combined_results
        rv$classic_slate_data <- list(dk_salaries = dk_salaries,
                                      n_games = length(game_files))
        rv$contest_type <- "classic"
        rv$simulation_complete <- TRUE
        
        setProgress(1, detail = "Complete!")
        
        showNotification(HTML(
          paste0(
            "✓ Classic slate simulation complete!<br>",
            length(game_files),
            " games simulated<br>",
            format(n_sims, big.mark = ","),
            " simulations per game<br>",
            nrow(combined_results),
            " total player records"
          )
        ), type = "message", duration = 5)
        
        updateTabItems(session, "sidebar_menu", selected = "results")
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message),
                         type = "error",
                         duration = 10)
        print(paste("Full error:", e))
      })
    })
  })
  
  # File info display
  output$file_info <- renderUI({
    req(rv$file_uploaded)
    
    tagList(h4(paste(
      "Teams:", paste(rv$input_data$team_names, collapse = " vs ")
    )), p(paste(
      "Players:", nrow(rv$input_data$dk_salaries)
    )), p(paste(
      "Similar Games:", nrow(rv$input_data$similar_games)
    )), if (rv$simulation_complete) {
      p(strong(paste(
        "Simulations Complete:",
        format(input$n_sims, big.mark = ",")
      )))
    })
  })
  
  output$player_projections <- renderDT({
    req(rv$simulation_complete)
    
    # Handle differently for classic vs showdown
    # Handle differently for classic vs showdown
    if (!is.null(rv$contest_type) &&
        rv$contest_type == "classic") {
      # Get DK projections from salary file
      dk_proj_lookup <- rv$classic_slate_data$dk_salaries %>%
        select(Name, Team, Game, Projection) %>%
        rename(Player = Name, DK_Proj = Projection)
      
      # Classic mode projections with DK Proj
      projections <- rv$simulation_results %>%
        group_by(Player, Team, Pos, Game) %>%
        summarise(
          Salary = first(Salary),
          Own = first(Own),
          MedianPts = median(TotalPts, na.rm = TRUE),
          AvgPts = mean(TotalPts, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        left_join(dk_proj_lookup, by = c("Player", "Team", "Game")) %>%
        arrange(desc(MedianPts))
      
      # Filter columns: remove SD, Min, Max; add DK_Proj
      projections <- projections %>%
        select(Player,
               Team,
               Pos,
               Game,
               Salary,
               Own,
               MedianPts,
               AvgPts,
               DK_Proj)
      
      col_names <- c('Player',
                     'Team',
                     'Pos',
                     'Game',
                     'Salary',
                     'Own%',
                     'Median',
                     'Avg',
                     'DK Proj')
      
      datatable(
        projections,
        filter = 'top',
        # Column filters at top
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          order = list(list(6, 'desc')),
          # Sort by Median (column index 6)
          search = list(regex = TRUE, caseInsensitive = TRUE),
          # Enable search
          columnDefs = list(list(
            className = 'dt-center', targets = 1:8
          ))
        ),
        rownames = FALSE,
        colnames = col_names
      ) %>%
        formatCurrency(
          'Salary',
          currency = "$",
          interval = 3,
          mark = ",",
          digits = 0
        ) %>%
        formatRound(c('Own', 'MedianPts', 'AvgPts', 'DK_Proj'), 1) %>%
        formatStyle(
          'MedianPts',
          background = styleColorBar(range(projections$MedianPts, na.rm = TRUE), '#32CD32'),
          backgroundSize = '95% 80%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'AvgPts',
          background = styleColorBar(range(projections$AvgPts, na.rm = TRUE), '#FFA500'),
          backgroundSize = '95% 80%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'DK_Proj',
          background = styleColorBar(range(projections$DK_Proj, na.rm = TRUE), '#1E90FF'),
          backgroundSize = '95% 80%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        apply_team_colors_to_table(projections, "Team")
      
    } else {
      # Showdown mode - your existing logic
      dk_data <- rv$input_data$dk_salaries
      projections <- analyze_fantasy_scoring(rv$simulation_results, dk_data)
      
      # Join with ETR and Saber projections if available
      if (!is.null(dk_data)) {
        projections <- projections %>%
          left_join(dk_data %>% select(Name, ETR_DK_Pts, Saber_Proj),
                    by = c("Player" = "Name"))
        
        # Reorder columns
        if ("Pos" %in% names(projections)) {
          projections <- projections %>%
            select(Player,
                   Pos,
                   Team,
                   Salary,
                   MedianPts,
                   AvgPts,
                   ETR_DK_Pts,
                   Saber_Proj)
        } else {
          projections <- projections %>%
            select(Player,
                   Team,
                   Salary,
                   MedianPts,
                   AvgPts,
                   ETR_DK_Pts,
                   Saber_Proj)
        }
        
      } else {
        if ("Pos" %in% names(projections)) {
          projections <- projections %>%
            select(Player, Pos, Team, Salary, MedianPts, AvgPts)
        } else {
          projections <- projections %>%
            select(Player, Team, Salary, MedianPts, AvgPts)
        }
      }
      
      # Determine column names
      col_names <- c('Player')
      col_idx <- 1
      if ("Pos" %in% names(projections)) {
        col_names <- c(col_names, 'Pos')
        col_idx <- col_idx + 1
      }
      col_names <- c(col_names, 'Team', 'Salary', 'Median Pts', 'Avg Pts')
      median_col_idx <- length(col_names) - 1
      
      if ("ETR_DK_Pts" %in% names(projections)) {
        col_names <- c(col_names, 'ETR Proj', 'Saber Proj')
      }
      
      datatable(
        projections,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          order = list(list(median_col_idx - 1, 'desc')),
          columnDefs = list(list(
            className = 'dt-center',
            targets = col_idx:ncol(projections) - 1
          ))
        ),
        rownames = FALSE,
        caption = "Simulated vs ETR vs Saber Projections",
        colnames = col_names
      ) %>%
        formatCurrency(
          'Salary',
          currency = "$",
          interval = 3,
          mark = ",",
          digits = 0
        ) %>%
        formatRound(setdiff(names(projections), c('Player', 'Pos', 'Team', 'Salary')), 2) %>%
        formatStyle(
          'MedianPts',
          background = styleColorBar(range(projections$MedianPts, na.rm = TRUE), '#32CD32'),
          backgroundSize = '95% 80%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'AvgPts',
          background = styleColorBar(range(projections$AvgPts, na.rm = TRUE), '#FFA500'),
          backgroundSize = '95% 80%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        {
          if ("ETR_DK_Pts" %in% names(projections)) {
            formatStyle(
              .,
              'ETR_DK_Pts',
              background = styleColorBar(
                range(projections$ETR_DK_Pts, na.rm = TRUE),
                '#1E90FF'
              ),
              backgroundSize = '95% 80%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center'
            )
          } else
            .
        } %>%
        {
          if ("Saber_Proj" %in% names(projections)) {
            formatStyle(
              .,
              'Saber_Proj',
              background = styleColorBar(
                range(projections$Saber_Proj, na.rm = TRUE),
                '#1E90FF'
              ),
              backgroundSize = '95% 80%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'center'
            )
          } else
            .
        } %>%
        apply_team_colors_to_table(projections, "Team")
    }
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
    if (length(teams) == 0)
      return(NULL)
    
    team_name <- teams[1]
    team_color <- get_team_color(team_name)
    team_data <- plot_data[Team == team_name]
    
    # Get all players who scored points in at least one simulation
    players_with_points <- team_data[, .(MedianPts = median(TotalPts),
                                         MaxPts = max(TotalPts)), by = Player][MaxPts > 0][order(-MedianPts)]
    
    team_data <- team_data[Player %in% players_with_points$Player]
    
    # Order players by median
    player_order <- team_data[, .(MedianPts = median(TotalPts)), by = Player][order(MedianPts)]$Player
    
    p <- plot_ly(
      team_data,
      y = ~ factor(Player, levels = player_order),
      x = ~ TotalPts,
      type = "box",
      orientation = "h",
      marker = list(color = team_color),
      line = list(color = team_color),
      fillcolor = paste0(team_color, "80"),
      showlegend = FALSE
    ) %>%
      layout(
        xaxis = list(
          title = "Fantasy Points",
          gridcolor = "#333333",
          titlefont = list(color = "#FFD700", size = 14)
        ),
        yaxis = list(
          title = "",
          gridcolor = "#333333",
          tickfont = list(color = "#FFD700", size = 12)
        ),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        font = list(color = "#FFD700", size = 12),
        margin = list(
          l = 150,
          r = 20,
          t = 20,
          b = 60
        )
      )
    
    p
  })
  
  # Team 2 violin plot
  output$team2_violin <- renderPlotly({
    req(rv$simulation_complete)
    
    plot_data <- rv$simulation_results
    setDT(plot_data)
    
    teams <- unique(plot_data$Team)
    if (length(teams) < 2)
      return(NULL)
    
    team_name <- teams[2]
    team_color <- get_team_color(team_name)
    team_data <- plot_data[Team == team_name]
    
    # Get all players who scored points in at least one simulation
    players_with_points <- team_data[, .(MedianPts = median(TotalPts),
                                         MaxPts = max(TotalPts)), by = Player][MaxPts > 0][order(-MedianPts)]
    
    team_data <- team_data[Player %in% players_with_points$Player]
    
    # Order players by median
    player_order <- team_data[, .(MedianPts = median(TotalPts)), by = Player][order(MedianPts)]$Player
    
    p <- plot_ly(
      team_data,
      y = ~ factor(Player, levels = player_order),
      x = ~ TotalPts,
      type = "box",
      orientation = "h",
      marker = list(color = team_color),
      line = list(color = team_color),
      fillcolor = paste0(team_color, "80"),
      showlegend = FALSE
    ) %>%
      layout(
        xaxis = list(
          title = "Fantasy Points",
          gridcolor = "#333333",
          titlefont = list(color = "#FFD700", size = 14)
        ),
        yaxis = list(
          title = "",
          gridcolor = "#333333",
          tickfont = list(color = "#FFD700", size = 12)
        ),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        font = list(color = "#FFD700", size = 12),
        margin = list(
          l = 150,
          r = 20,
          t = 20,
          b = 60
        )
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
      labs(title = "Team Total Passing Yards Per Game", x = "Total Passing Yards", y = "Density") +
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
      layout(
        paper_bgcolor = "#222222",
        plot_bgcolor = "#222222",
        font = list(color = "#FFD700")
      )
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
      labs(title = "Team Total Rushing Yards Per Game", x = "Total Rushing Yards", y = "Density") +
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
      layout(
        paper_bgcolor = "#222222",
        plot_bgcolor = "#222222",
        font = list(color = "#FFD700")
      )
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
      labs(title = "Team Total Touchdowns Per Game", x = "Total Touchdowns", y = "Density") +
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
      layout(
        paper_bgcolor = "#222222",
        plot_bgcolor = "#222222",
        font = list(color = "#FFD700")
      )
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
      labs(title = "Team Total Field Goals Per Game", x = "Field Goals Made", y = "Density") +
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
      layout(
        paper_bgcolor = "#222222",
        plot_bgcolor = "#222222",
        font = list(color = "#FFD700")
      )
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
  
  observeEvent(input$generate_optimal, {
    cat("\n=== GENERATE OPTIMAL CLICKED ===\n")
    cat("Simulation complete:", rv$simulation_complete, "\n")
    cat("Contest type:", rv$contest_type, "\n")
    cat("Has classic_slate_data:",!is.null(rv$classic_slate_data),
        "\n")
    
    
    req(rv$simulation_complete)
    
    # Handle classic mode differently
    if (!is.null(rv$contest_type) &&
        rv$contest_type == "classic") {
      req(rv$classic_slate_data)
      
      withProgress(message = 'Generating classic lineups...', value = 0, {
        tryCatch({
          setProgress(0.3, detail = "Building lineups...")
          
          # Detect sport from data (check for DST position)
          sport <- if ("DST" %in% unique(rv$classic_slate_data$dk_salaries$Pos))
            "NFL"
          else
            "CFB"
          
          rv$optimal_lineups <- build_classic_lineups(
            sim_results = rv$simulation_results,
            dk_salaries = rv$classic_slate_data$dk_salaries,
            sport = sport
          )
          
          setProgress(1, detail = "Complete!")
          
          showNotification(
            paste(
              "Generated",
              nrow(rv$optimal_lineups),
              "classic lineups!"
            ),
            type = "message",
            duration = 5
          )
          
        }, error = function(e) {
          showNotification(
            paste("Error generating lineups:", e$message),
            type = "error",
            duration = 10
          )
          print(paste("Full error:", e))
          print(traceback())
        })
      })
      
    } else {
      # Showdown mode - existing code
      req(rv$input_data)
      
      withProgress(message = 'Generating optimal lineups...', value = 0, {
        tryCatch({
          setProgress(0.3, detail = "Analyzing simulations...")
          
          # Get DK salary data
          dk_data <- rv$input_data$dk_salaries
          
          # Generate optimal lineups
          rv$optimal_lineups <- generate_optimal_lineups(
            sim_results = rv$simulation_results,
            dk_salaries = dk_data,
            num_lineups = 150,
            salary_cap = 50000
          )
          
          setProgress(1, detail = "Complete!")
          
          showModal(modalDialog(
            title = "Success!",
            paste0(
              format(nrow(rv$optimal_lineups), big.mark = ","),
              " optimal lineups generated!"
            ),
            easyClose = TRUE
          ))
          
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("Error generating lineups:", e$message),
            easyClose = TRUE
          ))
          print(paste("Full error:", e))
        })
      })
    }
  })
  
  output$optimal_lineups_table <- renderDT({
    req(rv$optimal_lineups)
    
    if (!is.null(rv$contest_type) &&
        rv$contest_type == "classic") {
      # CLASSIC MODE - Use lineups as-is (already have Top1Count from build function)
      lineups <- rv$optimal_lineups
      
      # Format with positions
      player_pos_lookup <- rv$simulation_results %>%
        distinct(Player, Pos, Team, DFS_ID) %>%
        mutate(PlayerDisplay = paste0(Player, " (", DFS_ID, ")"))
      
      formatted_lineups <- list()
      
      for (i in 1:nrow(lineups)) {
        lineup_row <- lineups[i, ]
        
        players <- c(
          lineup_row$Player1,
          lineup_row$Player2,
          lineup_row$Player3,
          lineup_row$Player4,
          lineup_row$Player5,
          lineup_row$Player6,
          lineup_row$Player7,
          lineup_row$Player8,
          lineup_row$Player9
        )
        
        player_data <- player_pos_lookup %>%
          filter(PlayerDisplay %in% players)
        
        qbs <- player_data %>% filter(Pos == "QB") %>% pull(PlayerDisplay)
        rbs <- player_data %>% filter(Pos == "RB") %>% pull(PlayerDisplay)
        wrs <- player_data %>% filter(Pos == "WR") %>% pull(PlayerDisplay)
        tes <- player_data %>% filter(Pos == "TE") %>% pull(PlayerDisplay)
        dsts <- player_data %>% filter(Pos == "DST") %>% pull(PlayerDisplay)
        
        formatted_lineups[[i]] <- data.frame(
          QB = if (length(qbs) >= 1)
            qbs[1]
          else
            NA,
          RB1 = if (length(rbs) >= 1)
            rbs[1]
          else
            NA,
          RB2 = if (length(rbs) >= 2)
            rbs[2]
          else
            NA,
          WR1 = if (length(wrs) >= 1)
            wrs[1]
          else
            NA,
          WR2 = if (length(wrs) >= 2)
            wrs[2]
          else
            NA,
          WR3 = if (length(wrs) >= 3)
            wrs[3]
          else
            NA,
          TE = if (length(tes) >= 1)
            tes[1]
          else
            NA,
          FLEX = if (length(rbs) >= 3)
            rbs[3]
          else if (length(wrs) >= 4)
            wrs[4]
          else if (length(tes) >= 2)
            tes[2]
          else
            NA,
          DST = if (length(dsts) >= 1)
            dsts[1]
          else
            NA,
          Top1Count = lineup_row$Top1Count,
          TotalSalary = lineup_row$TotalSalary,
          CumulativeOwnership = lineup_row$TotalOwnership,
          GeometricMeanOwnership = lineup_row$TotalOwnership / 9
        )
      }
      
      display_lineups <- bind_rows(formatted_lineups)
      
      # Remove IDs from display
      for (col in c("QB", "RB1", "RB2", "WR1", "WR2", "WR3", "TE", "FLEX", "DST")) {
        if (col %in% names(display_lineups)) {
          display_lineups[[col]] <- gsub(" \\([^)]+\\)$", "", display_lineups[[col]])
        }
      }
      
      # Reorder columns
      display_lineups <- display_lineups %>%
        select(
          QB,
          RB1,
          RB2,
          WR1,
          WR2,
          WR3,
          TE,
          FLEX,
          DST,
          Top1Count,
          TotalSalary,
          CumulativeOwnership,
          GeometricMeanOwnership
        ) %>%
        arrange(desc(Top1Count))
      
      datatable(
        display_lineups,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          order = list(list(9, 'desc')),
          # Sort by Top1Count
          columnDefs = list(list(
            className = 'dt-center', targets = 0:12
          ))
        ),
        rownames = FALSE
      ) %>%
        formatCurrency(
          'TotalSalary',
          currency = "$",
          interval = 3,
          mark = ",",
          digits = 0
        ) %>%
        formatRound(c('CumulativeOwnership', 'GeometricMeanOwnership'), 1) %>%
        formatStyle(
          'Top1Count',
          background = styleColorBar(
            range(display_lineups$Top1Count, na.rm = TRUE),
            '#32CD32'
          ),
          backgroundSize = '95% 80%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      
    } else {
      # SHOWDOWN MODE - your existing code
      # Use filtered pool if available, otherwise show all
      if (!is.null(rv$filtered_pool) &&
          nrow(rv$filtered_pool) > 0) {
        display_lineups <- copy(rv$filtered_pool)
      } else {
        display_lineups <- copy(rv$optimal_lineups)
      }
      
      # Remove IDs from display
      display_lineups$Captain <- gsub(" \\([^)]+\\)$", "", display_lineups$Captain)
      for (i in 1:5) {
        player_col <- paste0("Player", i)
        if (player_col %in% names(display_lineups)) {
          display_lineups[[player_col]] <- gsub(" \\([^)]+\\)$", "", display_lineups[[player_col]])
        }
      }
      
      datatable(
        display_lineups,
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE
      ) %>%
        formatCurrency(
          'TotalSalary',
          currency = "$",
          interval = 3,
          mark = ",",
          digits = 0
        )
    }
  })
  
  output$download_optimal <- downloadHandler(
    filename = function() {
      if (!is.null(rv$contest_type) && rv$contest_type == "classic") {
        paste0("classic_lineups_", Sys.Date(), ".csv")
      } else {
        paste0("optimal_showdown_lineups_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      if (!is.null(rv$contest_type) && rv$contest_type == "classic") {
        # CLASSIC MODE
        lineups <- rv$optimal_lineups
        
        player_pos_lookup <- rv$simulation_results %>%
          distinct(Player, Pos, Team, DFS_ID) %>%
          mutate(PlayerDisplay = paste0(Player, " (", DFS_ID, ")"))
        
        formatted_lineups <- list()
        
        for (i in 1:nrow(lineups)) {
          lineup_row <- lineups[i, ]
          
          players <- c(
            lineup_row$Player1,
            lineup_row$Player2,
            lineup_row$Player3,
            lineup_row$Player4,
            lineup_row$Player5,
            lineup_row$Player6,
            lineup_row$Player7,
            lineup_row$Player8,
            lineup_row$Player9
          )
          
          player_data <- player_pos_lookup %>%
            filter(PlayerDisplay %in% players)
          
          qbs <- player_data %>% filter(Pos == "QB") %>% pull(PlayerDisplay)
          rbs <- player_data %>% filter(Pos == "RB") %>% pull(PlayerDisplay)
          wrs <- player_data %>% filter(Pos == "WR") %>% pull(PlayerDisplay)
          tes <- player_data %>% filter(Pos == "TE") %>% pull(PlayerDisplay)
          dsts <- player_data %>% filter(Pos == "DST") %>% pull(PlayerDisplay)
          
          formatted_lineups[[i]] <- data.frame(
            QB = if (length(qbs) >= 1)
              qbs[1]
            else
              NA,
            RB1 = if (length(rbs) >= 1)
              rbs[1]
            else
              NA,
            RB2 = if (length(rbs) >= 2)
              rbs[2]
            else
              NA,
            WR1 = if (length(wrs) >= 1)
              wrs[1]
            else
              NA,
            WR2 = if (length(wrs) >= 2)
              wrs[2]
            else
              NA,
            WR3 = if (length(wrs) >= 3)
              wrs[3]
            else
              NA,
            TE = if (length(tes) >= 1)
              tes[1]
            else
              NA,
            FLEX = if (length(rbs) >= 3)
              rbs[3]
            else if (length(wrs) >= 4)
              wrs[4]
            else if (length(tes) >= 2)
              tes[2]
            else
              NA,
            DST = if (length(dsts) >= 1)
              dsts[1]
            else
              NA,
            Top1Count = lineup_row$Top1Count,
            TotalSalary = lineup_row$TotalSalary,
            CumulativeOwnership = lineup_row$TotalOwnership,
            GeometricMeanOwnership = lineup_row$TotalOwnership / 9
          )
        }
        
        download_data <- bind_rows(formatted_lineups)
        write.csv(download_data, file, row.names = FALSE)
        
      } else {
        # SHOWDOWN MODE - existing code
        if (!is.null(rv$filtered_pool) &&
            nrow(rv$filtered_pool) > 0) {
          download_data <- rv$filtered_pool
        } else {
          download_data <- rv$optimal_lineups
        }
        
        write.csv(download_data, file, row.names = FALSE)
      }
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
    
    sim_scores <- lineup_sims[, .(LineupScore = sum(ifelse(
      Player == input$cpt_select, TotalPts * 1.5, TotalPts
    ))), by = SimID]
    
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
    
    salary_color <- if (stats$total_salary > DK_SALARY_CAP)
      "red"
    else
      "#FFD700"
    
    tagList(
      h4(paste("Captain:", stats$captain), style = "color: #FFD700;"),
      h5(paste(
        "FLEX:", paste(stats$flex, collapse = ", ")
      ), style = "color: #FFD700;"),
      hr(),
      h4(
        paste(
          "Total Salary: $",
          format(stats$total_salary, big.mark = ",")
        ),
        style = paste0("color: ", salary_color, ";")
      ),
      h5(paste(
        "Average Score:", round(stats$avg_score, 2)
      ), style = "color: #FFD700;"),
      h5(paste(
        "Median Score:", round(stats$median_score, 2)
      ), style = "color: #FFD700;"),
      h5(paste(
        "Max Score:", round(stats$max_score, 2)
      ), style = "color: #FFD700;")
    )
  })
  
  # Lineup distribution plot
  output$lineup_dist_plot <- renderPlotly({
    stats <- lineup_stats()
    req(stats$valid)
    
    p <- ggplot(stats$sim_scores, aes(x = LineupScore)) +
      geom_histogram(bins = 50,
                     fill = "#FFD700",
                     alpha = 0.7) +
      geom_vline(
        xintercept = stats$avg_score,
        color = "red",
        linetype = "dashed",
        size = 1
      ) +
      labs(title = "Lineup Score Distribution", x = "Total Fantasy Points", y = "Frequency") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#222222"),
        panel.background = element_rect(fill = "#222222"),
        text = element_text(color = "#FFD700"),
        axis.text = element_text(color = "#FFD700"),
        panel.grid = element_line(color = "#333333")
      )
    
    ggplotly(p) %>%
      layout(
        paper_bgcolor = "#222222",
        plot_bgcolor = "#222222",
        font = list(color = "#FFD700")
      )
  })
  
  
  # ===== LINEUP BUILDER LOGIC =====
  
  # Check if optimal lineups exist
  output$has_optimal_lineups <- reactive({
    !is.null(rv$optimal_lineups) && nrow(rv$optimal_lineups) > 0
  })
  outputOptions(output, "has_optimal_lineups", suspendWhenHidden = FALSE)
  
  
  observeEvent(rv$optimal_lineups, {
    if (!is.null(rv$optimal_lineups)) {
      
      is_classic <- !is.null(rv$contest_type) && rv$contest_type == "classic"
      
      if (is_classic) {
        # CLASSIC MODE - populate player choices from Player1-9
        all_players <- unique(unlist(rv$optimal_lineups[, paste0("Player", 1:9)]))
        all_players <- all_players[!is.na(all_players) & all_players != ""]
        all_players <- sort(all_players)
        
        updateSelectizeInput(session, "classic_locked_players", choices = all_players, server = FALSE)
        updateSelectizeInput(session, "classic_excluded_players", choices = all_players, server = FALSE)
        
      } else {
        # SHOWDOWN MODE
        captain_players <- unique(rv$optimal_lineups$Captain)
        captain_players <- captain_players[!is.na(captain_players) & captain_players != ""]
        captain_players <- sort(captain_players)
        
        flex_players <- unique(unlist(rv$optimal_lineups[, paste0("Player", 1:5)]))
        flex_players <- flex_players[!is.na(flex_players) & flex_players != ""]
        flex_players <- sort(flex_players)
        
        updateSelectizeInput(session, "excluded_captains", choices = captain_players, server = FALSE)
        updateSelectizeInput(session, "excluded_flex", choices = flex_players, server = FALSE)
        updateSelectizeInput(session, "locked_captain", 
                             choices = setNames(c("", captain_players), c("None", captain_players)),
                             server = FALSE, selected = "")
        updateSelectizeInput(session, "locked_flex", choices = flex_players, server = FALSE)
        
        if ("TeamStack" %in% names(rv$optimal_lineups)) {
          stack_choices <- sort(unique(as.character(rv$optimal_lineups$TeamStack)))
          stack_choices <- stack_choices[!is.na(stack_choices)]
          updateSelectizeInput(session, "excluded_stacks", choices = stack_choices, server = FALSE)
        }
      }
      
      # Update ownership sliders (BOTH MODES) - FIX: Remove post="%"
      if ("CumulativeOwnership" %in% names(rv$optimal_lineups)) {
        cum_min <- floor(min(rv$optimal_lineups$CumulativeOwnership, na.rm = TRUE))
        cum_max <- ceiling(max(rv$optimal_lineups$CumulativeOwnership, na.rm = TRUE))
        updateSliderInput(
          session,
          "cumulative_ownership_range",
          min = cum_min,
          max = cum_max,
          value = c(cum_min, cum_max)
        )
      }
      
      if ("GeometricMeanOwnership" %in% names(rv$optimal_lineups)) {
        geo_min <- floor(min(rv$optimal_lineups$GeometricMeanOwnership, na.rm = TRUE))
        geo_max <- ceiling(max(rv$optimal_lineups$GeometricMeanOwnership, na.rm = TRUE))
        updateSliderInput(
          session,
          "geometric_mean_range",
          min = geo_min,
          max = geo_max,
          value = c(geo_min, geo_max)
        )
      }
    }
  })
  
  
  # Reactive expression for filtered lineups
  filtered_optimal_lineups <- reactive({
    req(rv$optimal_lineups)
    
    filtered <- as.data.frame(rv$optimal_lineups)
    is_classic <- !is.null(rv$contest_type) && rv$contest_type == "classic"
    
    if (is_classic) {
      # ============================================
      # CLASSIC MODE FILTERS
      # ============================================
      
      # Player lock - must contain ALL locked players
      if (!is.null(input$classic_locked_players) && length(input$classic_locked_players) > 0) {
        player_cols <- paste0("Player", 1:9)
        for (col in player_cols) {
          if (col %in% names(filtered)) {
            filtered[[col]] <- as.character(filtered[[col]])
          }
        }
        
        # Keep only lineups that contain ALL locked players
        for (player in input$classic_locked_players) {
          filtered <- filtered[
            filtered$Player1 == player |
              filtered$Player2 == player |
              filtered$Player3 == player |
              filtered$Player4 == player |
              filtered$Player5 == player |
              filtered$Player6 == player |
              filtered$Player7 == player |
              filtered$Player8 == player |
              filtered$Player9 == player, 
          ]
        }
      }
      
      # Player exclude - must NOT contain any excluded players
      if (!is.null(input$classic_excluded_players) && length(input$classic_excluded_players) > 0) {
        player_cols <- paste0("Player", 1:9)
        for (col in player_cols) {
          if (col %in% names(filtered)) {
            filtered[[col]] <- as.character(filtered[[col]])
          }
        }
        
        # Remove lineups that contain ANY excluded players
        for (player in input$classic_excluded_players) {
          filtered <- filtered[
            filtered$Player1 != player &
              filtered$Player2 != player &
              filtered$Player3 != player &
              filtered$Player4 != player &
              filtered$Player5 != player &
              filtered$Player6 != player &
              filtered$Player7 != player &
              filtered$Player8 != player &
              filtered$Player9 != player,
          ]
        }
      }
      
    } else {
      # ============================================
      # SHOWDOWN MODE FILTERS
      # ============================================
      
      # Apply team stack exclusion
      if (!is.null(input$excluded_stacks) &&
          length(input$excluded_stacks) > 0 &&
          "TeamStack" %in% names(filtered)) {
        filtered$TeamStack <- as.character(filtered$TeamStack)
        
        for (stack in input$excluded_stacks) {
          filtered <- filtered[filtered$TeamStack != stack, ]
        }
      }
      
      # Apply captain exclusion
      if (!is.null(input$excluded_captains) &&
          length(input$excluded_captains) > 0 &&
          "Captain" %in% names(filtered)) {
        filtered$Captain <- as.character(filtered$Captain)
        
        for (captain in input$excluded_captains) {
          filtered <- filtered[filtered$Captain != captain, ]
        }
      }
      
      # Apply flex exclusion
      if (!is.null(input$excluded_flex) &&
          length(input$excluded_flex) > 0) {
        flex_cols <- paste0("Player", 1:5)
        for (col in flex_cols) {
          if (col %in% names(filtered)) {
            filtered[[col]] <- as.character(filtered[[col]])
          }
        }
        
        for (player in input$excluded_flex) {
          filtered <- filtered[filtered$Player1 != player &
                                 filtered$Player2 != player &
                                 filtered$Player3 != player &
                                 filtered$Player4 != player &
                                 filtered$Player5 != player, ]
        }
      }
      
      # Apply captain lock (INCLUDE ONLY)
      if (!is.null(input$locked_captain) &&
          input$locked_captain != "") {
        filtered$Captain <- as.character(filtered$Captain)
        filtered <- filtered[filtered$Captain == input$locked_captain, ]
      }
      
      # Apply flex locks (INCLUDE ONLY)
      if (!is.null(input$locked_flex) &&
          length(input$locked_flex) > 0) {
        flex_cols <- paste0("Player", 1:5)
        for (col in flex_cols) {
          if (col %in% names(filtered)) {
            filtered[[col]] <- as.character(filtered[[col]])
          }
        }
        
        # Keep only lineups that contain ALL locked players
        for (player in input$locked_flex) {
          filtered <- filtered[filtered$Player1 == player |
                                 filtered$Player2 == player |
                                 filtered$Player3 == player |
                                 filtered$Player4 == player |
                                 filtered$Player5 == player, ]
        }
      }
      
      # Top 2/3/5 counts (showdown only)
      if (!is.null(input$min_top2_count) &&
          input$min_top2_count > 0 &&
          "Top2Count" %in% names(filtered)) {
        filtered <- filtered[filtered$Top2Count >= input$min_top2_count, ]
      }
      
      if (!is.null(input$min_top3_count) &&
          input$min_top3_count > 0 &&
          "Top3Count" %in% names(filtered)) {
        filtered <- filtered[filtered$Top3Count >= input$min_top3_count, ]
      }
      
      if (!is.null(input$min_top5_count) &&
          input$min_top5_count > 0 &&
          "Top5Count" %in% names(filtered)) {
        filtered <- filtered[filtered$Top5Count >= input$min_top5_count, ]
      }
    }
    
    # ============================================
    # UNIVERSAL FILTERS (BOTH MODES)
    # ============================================
    
    # Apply Top 1 Count filter
    if (!is.null(input$min_top1_count) &&
        input$min_top1_count > 0 &&
        "Top1Count" %in% names(filtered)) {
      filtered <- filtered[filtered$Top1Count >= input$min_top1_count, ]
    }
    
    # Apply ownership filters
    if (!is.null(input$cumulative_ownership_range) &&
        "CumulativeOwnership" %in% names(filtered)) {
      filtered <- filtered[filtered$CumulativeOwnership >= input$cumulative_ownership_range[1] &
                             filtered$CumulativeOwnership <= input$cumulative_ownership_range[2], ]
    }
    
    if (!is.null(input$geometric_mean_range) &&
        "GeometricMeanOwnership" %in% names(filtered)) {
      filtered <- filtered[filtered$GeometricMeanOwnership >= input$geometric_mean_range[1] &
                             filtered$GeometricMeanOwnership <= input$geometric_mean_range[2], ]
    }
    
    return(filtered)
  })
  
  observe({
    filtered_pool <- filtered_optimal_lineups()
    rv$filtered_pool <- filtered_pool
    
    output$filtered_pool_size <- renderText({
      paste0(format(nrow(filtered_pool), big.mark = ","),
             " lineups match current filters")
    })
  })
  
  output$filtered_pool_count_display <- renderText({
    filtered_pool <- filtered_optimal_lineups()
    paste0(format(nrow(filtered_pool), big.mark = ","), " lineups")
  })
  
  
  # Calculate player exposure for classic mode
  calculate_player_exposure_classic <- function(optimal_lineups, dk_salaries, generated_lineups) {
    if (is.null(optimal_lineups) || nrow(optimal_lineups) == 0) {
      return(data.frame(Message = "No optimal lineups available."))
    }
    
    # Get all unique players from optimal lineups (Player1-9)
    all_players <- unique(unlist(optimal_lineups[, paste0("Player", 1:9)]))
    all_players <- gsub(" \\([^)]+\\)$", "", all_players)  # Strip IDs
    all_players <- all_players[!is.na(all_players) & all_players != ""]
    
    if (length(all_players) == 0) {
      return(data.frame(Message = "No players found in lineups."))
    }
    
    # Initialize metrics
    metrics_data <- data.frame(
      Player = all_players,
      Salary = NA_real_,
      Own = NA_real_,
      OptimalRate = 0,
      Exposure = NA_real_,
      Leverage = NA_real_,
      stringsAsFactors = FALSE
    )
    
    # Match with DK salaries
    for (i in 1:nrow(metrics_data)) {
      player_name <- metrics_data$Player[i]
      matches <- which(dk_salaries$Name == player_name)
      
      if (length(matches) > 0) {
        match_idx <- matches[1]
        metrics_data$Salary[i] <- dk_salaries$Salary[match_idx]
        metrics_data$Own[i] <- dk_salaries$Own[match_idx]
      }
    }
    
    # Calculate OptimalRate (% of optimal lineups containing player)
    total_optimal <- nrow(optimal_lineups)
    if (total_optimal > 0) {
      for (player in all_players) {
        # Count across Player1-9
        player_count <- 0
        for (col in paste0("Player", 1:9)) {
          if (col %in% names(optimal_lineups)) {
            player_appears <- gsub(" \\([^)]+\\)$", "", optimal_lineups[[col]]) == player
            player_count <- player_count + sum(player_appears, na.rm = TRUE)
          }
        }
        metrics_data[metrics_data$Player == player, "OptimalRate"] <- (player_count / total_optimal) * 100
      }
    }
    
    # Calculate Exposure (% of generated lineups containing player)
    if (!is.null(generated_lineups) && nrow(generated_lineups) > 0) {
      for (player in all_players) {
        # Count across Player1-9 in generated lineups
        player_count <- 0
        for (col in paste0("Player", 1:9)) {
          if (col %in% names(generated_lineups)) {
            player_appears <- gsub(" \\([^)]+\\)$", "", generated_lineups[[col]]) == player
            player_count <- player_count + sum(player_appears, na.rm = TRUE)
          }
        }
        exposure_pct <- (player_count / nrow(generated_lineups)) * 100
        metrics_data[metrics_data$Player == player, "Exposure"] <- exposure_pct
        
        # Leverage = Exposure - Ownership
        if (!is.na(metrics_data[metrics_data$Player == player, "Own"])) {
          own <- metrics_data[metrics_data$Player == player, "Own"]
          metrics_data[metrics_data$Player == player, "Leverage"] <- exposure_pct - own
        }
      }
    }
    
    metrics_data <- metrics_data %>%
      select(Player, Salary, Own, Exposure, Leverage) %>%
      arrange(desc(Exposure))
    
    return(metrics_data)
  }
  
  observeEvent(input$generate_lineups, {
    req(rv$optimal_lineups)
    
    # Get already-filtered lineups from reactive
    filtered_pool <- filtered_optimal_lineups()
    
    if (nrow(filtered_pool) == 0) {
      showModal(
        modalDialog(
          title = "No Lineups Available",
          "No lineups match the current filters.",
          easyClose = TRUE
        )
      )
      return()
    }
    
    withProgress(message = 'Generating lineups...', value = 0, {
      # Sample from filtered pool using Top1Count weights
      selected_lineups <- data.frame()
      selected_indices <- integer(0)
      
      num_to_generate <- input$num_random_lineups
      attempts <- 0
      max_attempts <- num_to_generate * 10
      
      while (nrow(selected_lineups) < num_to_generate &&
             attempts < max_attempts) {
        attempts <- attempts + 1
        
        available_indices <- setdiff(1:nrow(filtered_pool), selected_indices)
        if (length(available_indices) == 0)
          break
        
        weights <- filtered_pool$Top1Count[available_indices]
        if(sum(weights) == 0) weights <- rep(1, length(available_indices))
        
        # Fix for when there's only 1 lineup available
        if(length(available_indices) == 1) {
          selected_idx <- available_indices[1]
        } else {
          selected_idx <- sample(available_indices, 1, prob = weights)
        }
        
        selected_indices <- c(selected_indices, selected_idx)
        selected_lineups <- rbind(selected_lineups, filtered_pool[selected_idx, ])
      }
      
      if (nrow(selected_lineups) > 0) {
        is_classic <- !is.null(rv$contest_type) && rv$contest_type == "classic"
        
        # Create build label - auto-generate from filters if empty
        build_label <- if (!is.null(input$build_label) && input$build_label != "") {
          input$build_label
        } else {
          # Auto-generate label based on active filters
          label_parts <- c()
          
          if (is_classic) {
            # Classic auto-label
            if (!is.null(input$classic_locked_players) && length(input$classic_locked_players) > 0) {
              label_parts <- c(label_parts, paste0(length(input$classic_locked_players), "Lock"))
            }
            if (!is.null(input$classic_excluded_players) && length(input$classic_excluded_players) > 0) {
              label_parts <- c(label_parts, "Excl")
            }
          } else {
            # Showdown auto-label
            if (!is.null(input$locked_captain) && input$locked_captain != "") {
              cpt_name <- gsub(" \\(.*\\)", "", input$locked_captain)
              label_parts <- c(label_parts, paste0("CPT:", cpt_name))
            }
            if (!is.null(input$locked_flex) && length(input$locked_flex) > 0) {
              label_parts <- c(label_parts, paste0(length(input$locked_flex), "Lock"))
            }
            if (!is.null(input$excluded_stacks) && length(input$excluded_stacks) > 0) {
              label_parts <- c(label_parts, "NoStacks")
            }
          }
          
          if (length(label_parts) > 0) {
            paste(label_parts, collapse = " ")
          } else {
            paste0("Build ", length(rv$lineup_builds) + 1)
          }
        }
        
        # Add build info to lineups
        selected_lineups$BuildLabel <- build_label
        selected_lineups$LineupNum <- 1:nrow(selected_lineups)
        
        # Add to builds list
        rv$lineup_builds[[length(rv$lineup_builds) + 1]] <- list(
          label = build_label,
          lineups = selected_lineups,
          timestamp = Sys.time()
        )
        
        # Update player exposure for ALL builds combined
        all_lineups <- do.call(rbind, lapply(rv$lineup_builds, function(b) b$lineups))
        
        # Get salary data based on mode
        if (is_classic) {
          dk_data <- rv$classic_slate_data$dk_salaries
        } else {
          dk_data <- rv$input_data$dk_salaries
        }
        
        # Calculate player exposure based on mode
        if (is_classic) {
          # Classic mode - simpler exposure calculation
          rv$player_exposure <- calculate_player_exposure_classic(rv$optimal_lineups, dk_data, all_lineups)
        } else {
          # Showdown mode - original function
          player_mapping <- data.frame(
            Name = dk_data$Name,
            Salary = dk_data$Salary,
            Proj = if ("ETR_DK_Pts" %in% names(dk_data)) dk_data$ETR_DK_Pts else NA,
            Flex_Own = if ("Flex_Own" %in% names(dk_data)) dk_data$Flex_Own else NA,
            CPT_Own = if ("CPT_Own" %in% names(dk_data)) dk_data$CPT_Own else NA,
            stringsAsFactors = FALSE
          )
          rv$player_exposure <- calculate_player_exposure(rv$optimal_lineups, player_mapping, all_lineups)
        }
        
        showModal(modalDialog(
          title = "Success",
          sprintf(
            "Added %d lineups to '%s'! Total builds: %d, Total lineups: %d",
            nrow(selected_lineups),
            build_label,
            length(rv$lineup_builds),
            nrow(all_lineups)
          ),
          easyClose = TRUE
        ))
        
        # Clear build label for next use
        updateTextInput(session, "build_label", value = "")
        
      } else {
        showModal(
          modalDialog(
            title = "No Lineups Generated",
            "Could not generate lineups with current settings.",
            easyClose = TRUE
          )
        )
      }
    })
  })
  
  observeEvent(input$clear_builds, {
    rv$lineup_builds <- list()
    rv$player_exposure <- NULL
    
    showModal(
      modalDialog(
        title = "Builds Cleared",
        "All lineup builds have been removed.",
        easyClose = TRUE
      )
    )
  })
  
  output$filtered_pool_stats_table <- renderDT({
    req(rv$simulation_complete)
    req(rv$optimal_lineups)
    
    filtered_pool <- filtered_optimal_lineups()
    
    if (nrow(filtered_pool) == 0) {
      return(datatable(
        data.frame(Message = "No lineups in filtered pool"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    tryCatch({
      is_classic <- !is.null(rv$contest_type) && rv$contest_type == "classic"
      
      if (is_classic) {
        # ============================================
        # CLASSIC MODE - Simple player stats
        # ============================================
        
        # Get all unique players from filtered pool
        all_players <- unique(unlist(filtered_pool[, paste0("Player", 1:9)]))
        all_players <- all_players[!is.na(all_players) & all_players != ""]
        all_players <- gsub(" \\([^)]+\\)$", "", all_players)  # Remove IDs
        
        display_data <- data.frame(Player = all_players, stringsAsFactors = FALSE)
        
        # Add basic info from simulation results
        player_info <- rv$simulation_results %>%
          group_by(Player) %>%
          summarise(
            Pos = first(Pos),
            Team = first(Team),
            Salary = first(Salary),
            Own = first(Own),
            .groups = 'drop'
          )
        
        display_data <- display_data %>%
          left_join(player_info, by = "Player")
        
        # Calculate % in filtered pool
        display_data$Pool_Pct <- 0
        
        for (i in 1:nrow(display_data)) {
          player <- display_data$Player[i]
          
          # Count appearances across all 9 player slots
          player_count <- 0
          for (col in paste0("Player", 1:9)) {
            if (col %in% names(filtered_pool)) {
              player_count <- player_count + sum(
                gsub(" \\([^)]+\\)$", "", filtered_pool[[col]]) == player,
                na.rm = TRUE
              )
            }
          }
          display_data$Pool_Pct[i] <- (player_count / nrow(filtered_pool)) * 100
        }
        
        # Calculate leverage
        display_data$Pool_Lev <- display_data$Pool_Pct - display_data$Own
        
        # Filter to only players in pool and arrange
        display_data <- display_data %>%
          filter(Pool_Pct > 0) %>%
          select(Player, Pos, Team, Salary, Own, Pool_Pct, Pool_Lev) %>%
          arrange(desc(Pool_Pct))
        
        dt <- datatable(
          display_data,
          options = list(
            pageLength = 25,
            dom = "tp",
            scrollX = TRUE
          ),
          rownames = FALSE,
          class = 'cell-border stripe compact'
        ) %>%
          formatCurrency('Salary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
          formatRound(c('Own', 'Pool_Pct', 'Pool_Lev'), digits = 1)
        
        if ("Team" %in% names(display_data)) {
          dt <- apply_team_colors_to_table(dt, display_data, "Team")
        }
        
        return(dt)
        
      } else {
        # ============================================
        # SHOWDOWN MODE - Original captain/flex stats
        # ============================================
        
        dk_data <- rv$input_data$dk_salaries
        all_players <- unique(rv$simulation_results$Player)
        
        display_data <- data.frame(Player = all_players, stringsAsFactors = FALSE)
        
        # Add basic info
        if (!is.null(dk_data)) {
          display_data <- display_data %>%
            left_join(
              dk_data %>% select(Name, Pos, Team, Salary, CPT_Own, Flex_Own),
              by = c("Player" = "Name")
            )
          
          display_data$CPT_Own[is.na(display_data$CPT_Own)] <- 0
          display_data$Flex_Own[is.na(display_data$Flex_Own)] <- 0
          display_data$TotalOwn <- display_data$CPT_Own + display_data$Flex_Own
        } else {
          display_data$Pos <- NA
          display_data$Team <- NA
          display_data$Salary <- NA
          display_data$CPT_Own <- 0
          display_data$Flex_Own <- 0
          display_data$TotalOwn <- 0
        }
        
        # Calculate % in filtered pool
        display_data$Pool_CPT_Pct <- 0
        display_data$Pool_Flex_Pct <- 0
        
        for (i in 1:nrow(display_data)) {
          player <- display_data$Player[i]
          
          # Captain %
          captain_count <- sum(gsub(" \\([^)]+\\)$", "", filtered_pool$Captain) == player, na.rm = TRUE)
          display_data$Pool_CPT_Pct[i] <- (captain_count / nrow(filtered_pool)) * 100
          
          # Flex %
          flex_count <- 0
          for (col in paste0("Player", 1:5)) {
            if (col %in% names(filtered_pool)) {
              flex_count <- flex_count + sum(gsub(" \\([^)]+\\)$", "", filtered_pool[[col]]) == player, na.rm = TRUE)
            }
          }
          display_data$Pool_Flex_Pct[i] <- (flex_count / nrow(filtered_pool)) * 100
        }
        
        display_data$Pool_Total_Pct <- display_data$Pool_CPT_Pct + display_data$Pool_Flex_Pct
        
        # Calculate leverage
        display_data$CPT_PoolLev <- display_data$Pool_CPT_Pct - display_data$CPT_Own
        display_data$Flex_PoolLev <- display_data$Pool_Flex_Pct - display_data$Flex_Own
        display_data$Total_PoolLev <- display_data$Pool_Total_Pct - display_data$TotalOwn
        
        # Filter to only players in pool
        display_data <- display_data %>%
          filter(Pool_Total_Pct > 0) %>%
          select(Player, Pos, Team, Salary, CPT_Own, Pool_CPT_Pct, CPT_PoolLev,
                 Flex_Own, Pool_Flex_Pct, Flex_PoolLev, TotalOwn, Pool_Total_Pct, Total_PoolLev) %>%
          arrange(desc(Pool_Total_Pct))
        
        dt <- datatable(
          display_data,
          options = list(
            pageLength = 25,
            dom = "tp",
            scrollX = TRUE
          ),
          rownames = FALSE,
          class = 'cell-border stripe compact'
        )
        
        if ("Salary" %in% names(display_data)) {
          dt <- dt %>% formatCurrency('Salary', currency = "$", interval = 3, mark = ",", digits = 0)
        }
        
        numeric_cols <- intersect(
          c('CPT_Own', 'Pool_CPT_Pct', 'CPT_PoolLev', 'Flex_Own', 'Pool_Flex_Pct',
            'Flex_PoolLev', 'TotalOwn', 'Pool_Total_Pct', 'Total_PoolLev'),
          names(display_data)
        )
        if (length(numeric_cols) > 0) {
          dt <- dt %>% formatRound(numeric_cols, digits = 1)
        }
        
        if ("Team" %in% names(display_data)) {
          dt <- apply_team_colors_to_table(dt, display_data, "Team")
        }
        
        return(dt)
      }
    }, error = function(e) {
      datatable(
        data.frame(Error = paste("Error:", e$message)),
        options = list(dom = 't'),
        rownames = FALSE
      )
    })
  })
  
  # Builds summary table
  output$builds_summary_table <- renderDT({
    if (length(rv$lineup_builds) == 0) {
      return(datatable(
        data.frame(Message = "No builds yet. Use filters and click 'Add Lineups to Pool'."),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    summary_data <- data.frame(
      Build = sapply(rv$lineup_builds, function(b)
        b$label),
      Lineups = sapply(rv$lineup_builds, function(b)
        nrow(b$lineups)),
      Timestamp = sapply(rv$lineup_builds, function(b)
        format(b$timestamp, "%H:%M:%S")),
      stringsAsFactors = FALSE
    )
    
    summary_data$Build_ID <- 1:nrow(summary_data)
    summary_data <- summary_data[, c("Build_ID", "Build", "Lineups", "Timestamp")]
    
    datatable(
      summary_data,
      options = list(
        pageLength = 10,
        dom = 't',
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe compact'
    )
  })
  
  output$player_exposure_table <- renderDT({
    req(rv$player_exposure)
    
    if ("Message" %in% names(rv$player_exposure)) {
      return(datatable(
        rv$player_exposure,
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    is_classic <- !is.null(rv$contest_type) && rv$contest_type == "classic"
    
    if (is_classic) {
      # CLASSIC MODE - Only Own, Exposure, Leverage
      dt <- datatable(
        rv$player_exposure,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          order = list(list(3, 'desc'))  # Sort by Exposure
        ),
        rownames = FALSE,
        class = 'cell-border stripe compact'
      ) %>%
        formatCurrency('Salary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
        formatRound(c('Own', 'Exposure', 'Leverage'), digits = 1)
      
    } else {
      # SHOWDOWN MODE
      exposure_col_idx <- which(names(rv$player_exposure) == "Exposure") - 1
      
      dt <- datatable(
        rv$player_exposure,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          order = list(list(exposure_col_idx, 'desc'))
        ),
        rownames = FALSE,
        class = 'cell-border stripe compact'
      ) %>%
        formatCurrency('Salary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
        formatRound(c('FlexOwn', 'CPTOwn', 'OptimalRate', 'Exposure', 'Leverage'), digits = 1)
    }
    
    return(dt)
  })
  
  output$random_lineups_table <- renderDT({
    if (length(rv$lineup_builds) == 0) {
      return(datatable(
        data.frame(Message = "No lineups generated yet."),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    all_lineups <- do.call(rbind, lapply(rv$lineup_builds, function(b) b$lineups))
    
    if (nrow(all_lineups) == 0) {
      return(datatable(
        data.frame(Message = "No lineups available."),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    is_classic <- !is.null(rv$contest_type) && rv$contest_type == "classic"
    
    if (is_classic) {
      # CLASSIC MODE - Format with positions like optimal lineups table
      player_pos_lookup <- rv$simulation_results %>%
        distinct(Player, Pos, DFS_ID) %>%
        mutate(PlayerDisplay = paste0(Player, " (", DFS_ID, ")"))
      
      formatted_lineups <- list()
      
      for (i in 1:nrow(all_lineups)) {
        lineup_row <- all_lineups[i, ]
        
        players <- c(
          lineup_row$Player1, lineup_row$Player2, lineup_row$Player3,
          lineup_row$Player4, lineup_row$Player5, lineup_row$Player6,
          lineup_row$Player7, lineup_row$Player8, lineup_row$Player9
        )
        
        player_data <- player_pos_lookup %>%
          filter(PlayerDisplay %in% players)
        
        qbs <- player_data %>% filter(Pos == "QB") %>% pull(PlayerDisplay)
        rbs <- player_data %>% filter(Pos == "RB") %>% pull(PlayerDisplay)
        wrs <- player_data %>% filter(Pos == "WR") %>% pull(PlayerDisplay)
        tes <- player_data %>% filter(Pos == "TE") %>% pull(PlayerDisplay)
        dsts <- player_data %>% filter(Pos == "DST") %>% pull(PlayerDisplay)
        
        # Safer way to create data frame - always return a value
        formatted_lineups[[i]] <- data.frame(
          BuildLabel = lineup_row$BuildLabel,
          LineupNum = lineup_row$LineupNum,
          QB = if(length(qbs) >= 1) qbs[1] else "",
          RB1 = if(length(rbs) >= 1) rbs[1] else "",
          RB2 = if(length(rbs) >= 2) rbs[2] else "",
          WR1 = if(length(wrs) >= 1) wrs[1] else "",
          WR2 = if(length(wrs) >= 2) wrs[2] else "",
          WR3 = if(length(wrs) >= 3) wrs[3] else "",
          TE = if(length(tes) >= 1) tes[1] else "",
          FLEX = if(length(rbs) >= 3) rbs[3] else if(length(wrs) >= 4) wrs[4] else if(length(tes) >= 2) tes[2] else "",
          DST = if(length(dsts) >= 1) dsts[1] else "",
          TotalSalary = ifelse(!is.null(lineup_row$TotalSalary), lineup_row$TotalSalary, 0),
          CumulativeOwnership = ifelse(!is.null(lineup_row$CumulativeOwnership), lineup_row$CumulativeOwnership, 0),
          GeometricMeanOwnership = ifelse(!is.null(lineup_row$GeometricMeanOwnership), lineup_row$GeometricMeanOwnership, 0),
          stringsAsFactors = FALSE
        )
      }
      
      display_lineups <- bind_rows(formatted_lineups)
      
      # Remove IDs
      for (col in c("QB", "RB1", "RB2", "WR1", "WR2", "WR3", "TE", "FLEX", "DST")) {
        if (col %in% names(display_lineups)) {
          display_lineups[[col]] <- gsub(" \\([^)]+\\)$", "", display_lineups[[col]])
        }
      }
      
    } else {
      # SHOWDOWN MODE - Original format
      display_lineups <- all_lineups
      
      # Remove IDs
      if ("Captain" %in% names(display_lineups)) {
        display_lineups$Captain <- gsub(" \\([^)]+\\)$", "", display_lineups$Captain)
      }
      for (i in 1:5) {
        player_col <- paste0("Player", i)
        if (player_col %in% names(display_lineups)) {
          display_lineups[[player_col]] <- gsub(" \\([^)]+\\)$", "", display_lineups[[player_col]])
        }
      }
    }
    
    datatable(
      display_lineups,
      options = list(
        pageLength = 25,
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe compact'
    ) %>%
      formatCurrency('TotalSalary', currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(c('CumulativeOwnership', 'GeometricMeanOwnership'), digits = 1)
  })
  
  output$download_random_lineups <- downloadHandler(
    filename = function() {
      if (!is.null(rv$contest_type) && rv$contest_type == "classic") {
        paste0("classic_portfolio_", Sys.Date(), ".csv")
      } else {
        paste0("showdown_portfolio_", Sys.Date(), ".csv")
      }
    },
    content = function(file) {
      if (length(rv$lineup_builds) == 0) {
        write.csv(data.frame(Message = "No lineups to download"), file, row.names = FALSE)
        return()
      }
      
      all_lineups <- do.call(rbind, lapply(rv$lineup_builds, function(b) b$lineups))
      
      is_classic <- !is.null(rv$contest_type) && rv$contest_type == "classic"
      
      if (is_classic) {
        # CLASSIC MODE - Format like optimal lineups download
        player_pos_lookup <- rv$simulation_results %>%
          distinct(Player, Pos, DFS_ID) %>%
          mutate(PlayerDisplay = paste0(Player, " (", DFS_ID, ")"))
        
        formatted_lineups <- list()
        
        for (i in 1:nrow(all_lineups)) {
          lineup_row <- all_lineups[i, ]
          
          players <- c(
            lineup_row$Player1, lineup_row$Player2, lineup_row$Player3,
            lineup_row$Player4, lineup_row$Player5, lineup_row$Player6,
            lineup_row$Player7, lineup_row$Player8, lineup_row$Player9
          )
          
          player_data <- player_pos_lookup %>%
            filter(PlayerDisplay %in% players)
          
          qbs <- player_data %>% filter(Pos == "QB") %>% pull(PlayerDisplay)
          rbs <- player_data %>% filter(Pos == "RB") %>% pull(PlayerDisplay)
          wrs <- player_data %>% filter(Pos == "WR") %>% pull(PlayerDisplay)
          tes <- player_data %>% filter(Pos == "TE") %>% pull(PlayerDisplay)
          dsts <- player_data %>% filter(Pos == "DST") %>% pull(PlayerDisplay)
          
          formatted_lineups[[i]] <- data.frame(
            BuildLabel = lineup_row$BuildLabel,
            LineupNum = lineup_row$LineupNum,
            QB = if(length(qbs) >= 1) qbs[1] else "",
            RB = if(length(rbs) >= 1) rbs[1] else "",
            RB = if(length(rbs) >= 2) rbs[2] else "",
            WR = if(length(wrs) >= 1) wrs[1] else "",
            WR = if(length(wrs) >= 2) wrs[2] else "",
            WR = if(length(wrs) >= 3) wrs[3] else "",
            TE = if(length(tes) >= 1) tes[1] else "",
            FLEX = if(length(rbs) >= 3) rbs[3] else if(length(wrs) >= 4) wrs[4] else if(length(tes) >= 2) tes[2] else "",
            DST = if(length(dsts) >= 1) dsts[1] else "",
            TotalSalary = ifelse(!is.null(lineup_row$TotalSalary), lineup_row$TotalSalary, 0),
            CumulativeOwnership = ifelse(!is.null(lineup_row$CumulativeOwnership), lineup_row$CumulativeOwnership, 0),
            GeometricMeanOwnership = ifelse(!is.null(lineup_row$GeometricMeanOwnership), lineup_row$GeometricMeanOwnership, 0),
            stringsAsFactors = FALSE
          )
        }
        
        download_data <- bind_rows(formatted_lineups)
        write.csv(download_data, file, row.names = FALSE)
        
      } else {
        # SHOWDOWN MODE - Keep original format with IDs
        write.csv(all_lineups, file, row.names = FALSE)
      }
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)