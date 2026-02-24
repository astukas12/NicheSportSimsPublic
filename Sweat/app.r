# Golden Ticket MMA Sweat Tool
# Optimized for speed and user experience

if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, dplyr, tidyr, ggplot2, DT, plotly, shinyWidgets, data.table)

options(shiny.maxRequestSize = 100*1024^2)

# UI Definition
ui <- fluidPage(
  
  # Custom CSS - Golden Ticket Theme
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;600;700&display=swap');
      
      /* Base styling */
      body {
        font-family: 'Poppins', sans-serif;
        background-color: #000000;
        color: #FFFFFF;
      }
      
      /* Header styling */
      .header-container {
        background: linear-gradient(135deg, #000000 0%, #1a1a1a 100%);
        padding: 20px;
        margin-bottom: 30px;
        border-bottom: 3px solid #FFE500;
        box-shadow: 0 4px 6px rgba(255, 229, 0, 0.1);
      }
      
      .logo-title {
        display: flex;
        align-items: center;
        gap: 20px;
      }
      
      .logo-circle {
        width: 60px;
        height: 60px;
        background: #FFE500;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: 700;
        font-size: 24px;
        color: #000000;
        box-shadow: 0 4px 8px rgba(255, 229, 0, 0.3);
      }
      
      .app-title {
        color: #FFE500;
        font-size: 32px;
        font-weight: 700;
        margin: 0;
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5);
      }
      
      .app-subtitle {
        color: #CCCCCC;
        font-size: 14px;
        margin: 5px 0 0 0;
      }
      
      .sport-badge {
        background: linear-gradient(135deg, #FFE500 0%, #FFA500 100%);
        color: #000000;
        padding: 5px 15px;
        border-radius: 20px;
        font-weight: 600;
        font-size: 14px;
        margin-left: 15px;
        display: inline-block;
      }
      
      /* Panel styling */
      .well {
        background-color: #1a1a1a;
        border: 1px solid #FFE500;
        border-radius: 8px;
        padding: 20px;
        box-shadow: 0 2px 4px rgba(255, 229, 0, 0.1);
      }
      
      .upload-panel {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%);
        border: 2px solid #FFE500;
        border-radius: 12px;
        padding: 30px;
        margin-bottom: 20px;
        box-shadow: 0 4px 8px rgba(255, 229, 0, 0.2);
      }
      
      /* Input styling */
      .form-control, .selectize-input {
        background-color: #2a2a2a !important;
        border: 1px solid #FFE500;
        color: #FFFFFF !important;
        border-radius: 4px;
      }
      
      .form-control:focus, .selectize-input.focus {
        background-color: #333333 !important;
        border-color: #FFE500;
        box-shadow: 0 0 8px rgba(255, 229, 0, 0.4);
        color: #FFFFFF !important;
      }
      
      .selectize-input input {
        color: #FFFFFF !important;
      }
      
      .selectize-input .item {
        background-color: #FFE500;
        color: #000000;
        border: none;
        padding: 2px 8px;
        border-radius: 3px;
      }
      
      .selectize-dropdown {
        background-color: #2a2a2a;
        border: 1px solid #FFE500;
        color: #FFFFFF;
      }
      
      .selectize-dropdown-content .option {
        background-color: #2a2a2a;
        color: #FFFFFF;
        padding: 8px 12px;
      }
      
      .selectize-dropdown-content .option:hover,
      .selectize-dropdown-content .option.active {
        background-color: #FFE500;
        color: #000000;
      }
      
      /* Button styling */
      .btn-primary {
        background: linear-gradient(135deg, #FFE500 0%, #FFA500 100%);
        border: none;
        color: #000000;
        font-weight: 600;
        border-radius: 6px;
        padding: 10px 24px;
        transition: all 0.3s ease;
        box-shadow: 0 2px 4px rgba(255, 229, 0, 0.3);
      }
      
      .btn-primary:hover {
        background: linear-gradient(135deg, #FFA500 0%, #FFE500 100%);
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(255, 229, 0, 0.4);
        color: #000000;
      }
      
      .btn-danger {
        background: linear-gradient(135deg, #dc3545 0%, #c82333 100%);
        border: none;
        color: #FFFFFF;
        font-weight: 600;
        border-radius: 6px;
        padding: 10px 24px;
        transition: all 0.3s ease;
      }
      
      .btn-danger:hover {
        background: linear-gradient(135deg, #c82333 0%, #dc3545 100%);
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(220, 53, 69, 0.4);
      }
      
      /* Tab styling */
      .nav-tabs {
        border-bottom: 2px solid #FFE500;
        background-color: #1a1a1a;
        border-radius: 8px 8px 0 0;
        padding: 10px 10px 0 10px;
      }
      
      .nav-tabs > li > a {
        color: #CCCCCC;
        background-color: #2a2a2a;
        border: 1px solid #444444;
        margin-right: 5px;
        border-radius: 6px 6px 0 0;
        font-weight: 600;
      }
      
      .nav-tabs > li > a:hover {
        background-color: #333333;
        border-color: #FFE500;
        color: #FFE500;
      }
      
      .nav-tabs > li.active > a {
        background-color: #000000;
        border-color: #FFE500;
        border-bottom-color: transparent;
        color: #FFE500;
      }
      
      .tab-content {
        background-color: #000000;
        border: 2px solid #FFE500;
        border-top: none;
        border-radius: 0 0 8px 8px;
        padding: 20px;
      }
      
      /* DataTable styling */
      .dataTables_wrapper {
        color: #FFFFFF;
      }
      
      table.dataTable {
        background-color: #1a1a1a;
        color: #FFFFFF;
        border: 1px solid #FFE500;
      }
      
      table.dataTable thead th {
        background: linear-gradient(135deg, #FFE500 0%, #FFA500 100%);
        color: #000000;
        font-weight: 700;
        border-bottom: 2px solid #FFE500;
      }
      
      table.dataTable tbody tr {
        background-color: #1a1a1a;
        color: #FFFFFF;
      }
      
      table.dataTable tbody tr:hover {
        background-color: #2a2a2a !important;
      }
      
      table.dataTable tbody tr.even {
        background-color: #252525;
      }
      
      .dataTables_filter input,
      .dataTables_length select {
        background-color: #2a2a2a;
        color: #FFFFFF;
        border: 1px solid #FFE500;
        border-radius: 4px;
      }
      
      /* Stat boxes */
      .stat-box {
        background: linear-gradient(135deg, #1a1a1a 0%, #2a2a2a 100%);
        border: 2px solid #FFE500;
        border-radius: 8px;
        padding: 20px;
        text-align: center;
        margin-bottom: 15px;
        box-shadow: 0 2px 4px rgba(255, 229, 0, 0.2);
      }
      
      .stat-value {
        font-size: 36px;
        font-weight: 700;
        color: #FFE500;
        margin: 10px 0;
      }
      
      .stat-label {
        font-size: 14px;
        color: #CCCCCC;
        text-transform: uppercase;
        letter-spacing: 1px;
      }
      
      /* Fighter chip styling */
      .fighter-chip {
        display: inline-block;
        background-color: #2a2a2a;
        color: #FFFFFF;
        padding: 8px 16px;
        margin: 4px;
        border-radius: 20px;
        border: 1px solid #FFE500;
        font-size: 14px;
        cursor: pointer;
        transition: all 0.3s ease;
      }
      
      .fighter-chip:hover {
        background-color: #FFE500;
        color: #000000;
        transform: scale(1.05);
      }
      
      .fighter-chip.eliminated {
        background-color: #dc3545;
        border-color: #dc3545;
        color: #FFFFFF;
        text-decoration: line-through;
      }
      
      /* Plot styling */
      .js-plotly-plot {
        background-color: #1a1a1a;
      }
      
      /* Headers */
      h3, h4, h5 {
        color: #FFE500;
        font-weight: 700;
      }
      
      /* Leverage indicators */
      .positive-leverage {
        color: #28a745;
        font-weight: 700;
      }
      
      .negative-leverage {
        color: #dc3545;
        font-weight: 700;
      }
      
      /* Progress bar */
      .progress {
        background-color: #2a2a2a;
        border: 1px solid #FFE500;
      }
      
      .progress-bar {
        background: linear-gradient(135deg, #FFE500 0%, #FFA500 100%);
      }
    "))
  ),
  
  # Header
  div(class = "header-container",
      div(class = "logo-title",
          tags$img(src = "logo.jpg", height = "60px", style = "border: 2px solid #FFE500; box-shadow: 0 4px 8px rgba(255, 229, 0, 0.3);"),
          div(
            h1(class = "app-title", 
               "Contest Sweat Tool",
               uiOutput("sport_badge", inline = TRUE)),
            p(class = "app-subtitle", "Golden Ticket DFS Analytics")
          )
      )
  ),
  
  # Upload Panel
  div(class = "upload-panel",
      fluidRow(
        column(6,
               fileInput("file", 
                         label = div(style = "color: #FFE500; font-weight: 600; font-size: 16px;",
                                     "Upload Contest CSV"),
                         accept = c("text/csv", ".csv"),
                         buttonLabel = "Browse...",
                         placeholder = "No file selected")
        ),
        column(6,
               fileInput("sim_file", 
                         label = div(style = "color: #FFE500; font-weight: 600; font-size: 16px;",
                                     "Upload Sim Optimals (Optional)"),
                         accept = c("text/csv", ".csv"),
                         buttonLabel = "Browse...",
                         placeholder = "No file selected")
        )
      ),
      fluidRow(
        column(12,
               selectizeInput("username", 
                              label = div(style = "color: #FFE500; font-weight: 600;", "Your Username"),
                              choices = NULL,
                              options = list(
                                placeholder = 'Start typing username...',
                                loadThrottle = 300,
                                maxOptions = 50
                              ),
                              width = "100%")
        )
      )
  ),
  
  # Main content
  tabsetPanel(
    # My Sweat Tab
    tabPanel("My Sweat",
             uiOutput("my_sweat_content")
    ),
    
    # Dupe Analysis Tab  
    tabPanel("Dupe Analysis",
             uiOutput("dupe_analysis_content")
    ),
    
    # Live Sweat Tab
    tabPanel("Live Sweat",
             uiOutput("live_sweat_content")
    ),
    
    # Simulation Analysis Tab (NEW)
    tabPanel("Simulation Analysis",
             icon = icon("chart-line"),
             uiOutput("sim_analysis_content")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Add resource path for www folder (for logo.jpg)
  addResourcePath("www", "www")
  
  # Reactive values
  rv <- reactiveValues(
    data = NULL,
    sport = NULL,
    fighters = NULL,
    player_scores = NULL,
    eliminated_fighters = character(0),
    lineup_fighters_cache = list(),
    all_usernames = NULL,
    sim_data = NULL,
    sim_loaded = FALSE
  )
  
  # Sport badge
  output$sport_badge <- renderUI({
    req(rv$sport)
    span(class = "sport-badge", rv$sport)
  })
  
  # Fast fighter extraction with caching
  extract_fighters <- function(lineup) {
    if (!lineup %in% names(rv$lineup_fighters_cache)) {
      # Detect sport prefix (F for MMA, D for NASCAR)
      if (grepl(" D ", lineup, fixed = TRUE)) {
        # NASCAR
        fighters <- unlist(strsplit(lineup, " D ", fixed = TRUE))
        fighters <- trimws(sub("^D ", "", fighters))
      } else {
        # MMA (default)
        fighters <- unlist(strsplit(lineup, " F ", fixed = TRUE))
        fighters <- trimws(sub("^F ", "", fighters))
      }
      fighters <- fighters[nzchar(fighters)]
      rv$lineup_fighters_cache[[lineup]] <- fighters
    }
    rv$lineup_fighters_cache[[lineup]]
  }
  
  # Load and process data (OPTIMIZED)
  observeEvent(input$file, {
    req(input$file)
    
    withProgress(message = 'Loading contest data...', value = 0, {
      
      incProgress(0.3, detail = "Reading CSV...")
      
      tryCatch({
        # Fast read with data.table and multiple threads
        dt <- fread(input$file$datapath, 
                    showProgress = FALSE,
                    fill = TRUE,
                    sep = ",",
                    na.strings = c("", "NA", "NULL"),
                    nThread = 4)
        
        incProgress(0.3, detail = "Processing data...")
        
        # Detect sport based on lineup format
        sample_lineup <- dt[2, Lineup]
        if (grepl(" D ", sample_lineup, fixed = TRUE)) {
          rv$sport <- "NASCAR"
        } else if (grepl(" F ", sample_lineup, fixed = TRUE)) {
          rv$sport <- "MMA"
        } else {
          rv$sport <- "UNKNOWN"
        }
        
        # Force column names if not found
        if (!"EntryName" %in% names(dt)) {
          names(dt)[3] <- "EntryName"
        }
        if (!"Lineup" %in% names(dt)) {
          names(dt)[6] <- "Lineup"
        }
        
        # Find Points column
        points_col <- grep("Points", names(dt), value = TRUE)[1]
        
        # Extract data efficiently
        rv$data <- dt[, .(
          Username = trimws(sub(" \\(.*", "", EntryName)),
          Lineup = trimws(Lineup),
          Points = as.numeric(get(points_col))
        )][!is.na(Username) & !is.na(Lineup) & Username != "EntryName" & Username != ""]
        
        setkey(rv$data, Username, Lineup)
        
        # Extract player scores if Player and FPTS columns exist
        if ("Player" %in% names(dt) && "FPTS" %in% names(dt)) {
          drafted_col <- grep("%Drafted", names(dt), value = TRUE)[1]
          
          if (!is.na(drafted_col)) {
            player_data <- dt[, .(
              Player = trimws(Player),
              FPTS = as.numeric(FPTS),
              FieldExp = as.numeric(gsub("%", "", get(drafted_col)))
            )][!is.na(Player) & Player != "" & Player != "Player"]
          } else {
            player_data <- dt[, .(
              Player = trimws(Player),
              FPTS = as.numeric(FPTS)
            )][!is.na(Player) & Player != "" & Player != "Player"]
          }
          
          if ("FieldExp" %in% names(player_data)) {
            rv$player_scores <- player_data[, .(
              FPTS = max(FPTS, na.rm = TRUE),
              FieldExp = max(FieldExp, na.rm = TRUE)
            ), by = Player]
          } else {
            rv$player_scores <- player_data[, .(
              FPTS = max(FPTS, na.rm = TRUE)
            ), by = Player]
          }
          
          setkey(rv$player_scores, Player)
          
          # Get unique players from player_scores (much faster than parsing lineups)
          rv$fighters <- sort(unique(rv$player_scores$Player))
          
        } else {
          # Fallback: try column positions
          if (ncol(dt) >= 11) {
            player_data <- dt[, .(
              Player = trimws(.SD[[8]]),
              FPTS = as.numeric(.SD[[11]])
            )][!is.na(Player) & Player != "" & Player != "Player"]
            
            rv$player_scores <- player_data[, .(FPTS = max(FPTS, na.rm = TRUE)), by = Player]
            setkey(rv$player_scores, Player)
            rv$fighters <- sort(unique(rv$player_scores$Player))
          } else {
            rv$player_scores <- NULL
            rv$fighters <- character(0)
          }
        }
        
        incProgress(0.4, detail = "Complete!")
        
        # Update username dropdown - simplified and faster
        usernames <- unique(rv$data$Username)
        
        # Simple filtering - much faster than multiple passes
        usernames <- usernames[
          !grepl("^F |^D ", usernames) &  # Not lineup data
            !is.na(usernames) &              # Not NA
            nzchar(usernames)                # Not empty
        ]
        
        # Only filter by fighters if we have them
        if (length(rv$fighters) > 0) {
          usernames <- usernames[!usernames %in% rv$fighters]
        }
        
        # Sort once
        usernames <- sort(usernames)
        
        # Store in reactive value
        rv$all_usernames <- usernames
        
        # Update with server-side selectize
        updateSelectizeInput(session, "username", 
                             choices = usernames,
                             server = TRUE)
        
      }, error = function(e) {
        showNotification(paste("Error loading file:", e$message), type = "error", duration = 10)
      })
    })
  })
  
  # Load and process sim file (NEW - OPTIMIZED)
  observeEvent(input$sim_file, {
    req(input$sim_file, rv$player_scores)
    
    withProgress(message = 'Loading sim data...', value = 0, {
      
      incProgress(0.15, detail = "Reading sim file...")
      
      tryCatch({
        # Read sim optimals file with optimized settings
        sim_raw <- fread(input$sim_file$datapath, 
                         showProgress = FALSE,
                         nThread = 4)  # Use multiple threads
        
        incProgress(0.15, detail = "Processing lineups...")
        
        # Detect player columns dynamically
        player_cols <- grep("^(Player|Driver|Fighter|Golfer)[0-9]", names(sim_raw), value = TRUE)
        
        if (length(player_cols) == 0) {
          # Try alternate naming pattern
          player_cols <- names(sim_raw)[1:6]
        }
        
        # Clean player names efficiently (vectorized)
        sim_clean <- copy(sim_raw)
        for (col in player_cols) {
          set(sim_clean, j = col, value = trimws(gsub("\\s*\\(\\d+\\)\\s*", "", sim_clean[[col]])))
        }
        
        incProgress(0.15, detail = "Processing lineup keys...")
        
        # Check if LineupKey already exists (pre-computed in sim app)
        if ("LineupKey" %in% names(sim_clean)) {
          cat("Using pre-computed LineupKey - FAST!\n")
          # Keys already exist, just need to ensure they're clean
          sim_clean[, LineupKey := trimws(LineupKey)]
        } else {
          cat("Computing LineupKey (slow - consider adding to sim app)...\n")
          # Need to create keys - this is slow
          player_matrix <- as.matrix(sim_clean[, player_cols, with = FALSE])
          sim_clean[, LineupKey := apply(player_matrix, 1, function(x) paste(sort(x), collapse = "|"))]
        }
        
        incProgress(0.15, detail = "Scoring lineups...")
        
        # Score sim lineups against actual scores (vectorized)
        score_lookup <- setNames(rv$player_scores$FPTS, rv$player_scores$Player)
        
        # Vectorized scoring - much faster than loop
        for (col in player_cols) {
          score_col <- paste0(col, "Score")
          set(sim_clean, j = score_col, value = score_lookup[sim_clean[[col]]])
        }
        
        score_cols <- paste0(player_cols, "Score")
        sim_clean[, ActualScore := rowSums(.SD, na.rm = TRUE), .SDcols = score_cols]
        
        # Store available ranking metrics for dropdown
        # SimRank will be calculated reactively based on user selection
        # No need to calculate it here since it changes based on dropdown
        
        incProgress(0.2, detail = "Matching contest lineups (optimized)...")
        
        # OPTIMIZED MATCHING - Group by score first since matching lineups = matching scores
        # Vectorized contest key creation for speed
        contest_with_scores <- rv$data[, {
          # Parse all lineups at once
          all_players <- lapply(Lineup, extract_fighters)
          
          # Score and create keys vectorized
          scores <- sapply(all_players, function(players) sum(score_lookup[players], na.rm = TRUE))
          keys <- sapply(all_players, function(players) paste(sort(players), collapse = "|"))
          
          .(LineupKey = keys, ActualScore = scores)
        }, by = .(Lineup, Username, Points)]
        
        # Group contest lineups by score for faster matching
        contest_lookup <- contest_with_scores[, .(
          TimesPlayed = .N,
          PlayedBy = paste(unique(Username), collapse = ", ")
        ), by = .(LineupKey, ActualScore)]
        
        # Now join - but data.table will only compare lineups with matching ActualScore
        # This is MUCH faster than comparing all 25k sim lineups to all 5k contest lineups
        setkey(sim_clean, LineupKey, ActualScore)
        setkey(contest_lookup, LineupKey, ActualScore)
        
        # Perform the join on BOTH keys (LineupKey AND ActualScore)
        sim_clean[contest_lookup, `:=`(
          PlayedInContest = TRUE,
          TimesPlayed = i.TimesPlayed,
          PlayedBy = i.PlayedBy
        ), on = c("LineupKey", "ActualScore")]
        
        # Fill in FALSE for non-matches
        sim_clean[is.na(PlayedInContest), `:=`(
          PlayedInContest = FALSE,
          TimesPlayed = 0,
          PlayedBy = ""
        )]
        
        incProgress(0.1, detail = "Calculating unique players...")
        
        # Simple apply for unique counts
        sim_clean[, UniquePlayerCount := apply(player_matrix, 1, function(x) length(unique(x)))]
        
        rv$sim_data <- sim_clean
        rv$sim_loaded <- TRUE
        
        # Extract unique players for filter dropdowns
        all_sim_players <- unique(unlist(sim_clean[, player_cols, with = FALSE]))
        all_sim_players <- sort(all_sim_players)
        
        # Update player filter dropdowns
        updateSelectizeInput(session, "include_players", 
                             choices = all_sim_players,
                             server = TRUE)
        updateSelectizeInput(session, "exclude_players", 
                             choices = all_sim_players,
                             server = TRUE)
        
        incProgress(0.1, detail = "Complete!")
        
        showNotification(
          paste0("Sim data loaded! ", format(nrow(sim_clean), big.mark = ","), " lineups processed."),
          type = "message", 
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error loading file:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
  })
  
  # My Sweat Content
  output$my_sweat_content <- renderUI({
    req(rv$data, input$username, input$username != "")
    
    # Dynamic labels
    player_label <- if(rv$sport == "NASCAR") "Driver" else "Fighter"
    
    user_data <- rv$data[Username == input$username]
    field_data <- rv$data[Username != input$username]
    
    if (nrow(user_data) == 0) {
      return(div(class = "well", 
                 h4("No entries found for selected username", style = "color: #dc3545;")))
    }
    
    # Calculate exposure stats using %Drafted from DK file
    user_lineups <- unique(user_data$Lineup)
    n_user_entries <- nrow(user_data)
    
    # Get fighter exposures
    user_fighter_counts <- data.table()
    for (lineup in user_lineups) {
      fighters <- extract_fighters(lineup)
      user_fighter_counts <- rbind(user_fighter_counts, 
                                   data.table(Fighter = fighters))
    }
    user_exposure <- user_fighter_counts[, .(
      UserExp = .N / n_user_entries * 100
    ), by = Fighter]
    
    # Field exposure from %Drafted column if available
    if (!is.null(rv$player_scores) && "FieldExp" %in% names(rv$player_scores)) {
      field_exposure <- rv$player_scores[, .(Fighter = Player, FieldExp)]
    } else {
      # Fallback to manual calculation
      field_lineups <- unique(field_data$Lineup)
      n_field_entries <- nrow(field_data)
      
      field_fighter_counts <- data.table()
      for (lineup in field_lineups) {
        fighters <- extract_fighters(lineup)
        field_fighter_counts <- rbind(field_fighter_counts,
                                      data.table(Fighter = fighters))
      }
      field_exposure <- field_fighter_counts[, .(
        FieldExp = .N / n_field_entries * 100
      ), by = Fighter]
    }
    
    # Merge and calculate leverage
    exposure_data <- merge(user_exposure, field_exposure, by = "Fighter", all = TRUE)
    exposure_data[is.na(UserExp), UserExp := 0]
    exposure_data[is.na(FieldExp), FieldExp := 0]
    exposure_data[, Leverage := UserExp - FieldExp]
    exposure_data <- exposure_data[order(-UserExp)]
    
    tagList(
      h4(paste("Your", player_label, "Exposure vs Field")),
      DTOutput("exposure_table"),
      
      br(),
      
      h4("Positive Leverage Fighters"),
      plotlyOutput("positive_leverage_plot", height = "400px"),
      
      br(),
      
      h4("Negative Leverage Fighters"),
      plotlyOutput("negative_leverage_plot", height = "400px")
    )
  })
  
  # Exposure Table
  output$exposure_table <- renderDT({
    req(rv$data, input$username, input$username != "")
    
    # Make reactive to eliminated fighters
    eliminated <- rv$eliminated_fighters
    
    user_data <- rv$data[Username == input$username]
    field_data <- rv$data[Username != input$username]
    
    user_lineups <- unique(user_data$Lineup)
    n_user_entries <- nrow(user_data)
    
    # User exposure
    user_fighter_counts <- data.table()
    for (lineup in user_lineups) {
      fighters <- extract_fighters(lineup)
      user_fighter_counts <- rbind(user_fighter_counts, data.table(Fighter = fighters))
    }
    user_exposure <- user_fighter_counts[, .(UserExp = .N / n_user_entries * 100), by = Fighter]
    
    # Field exposure from %Drafted
    if (!is.null(rv$player_scores) && "FieldExp" %in% names(rv$player_scores)) {
      field_exposure <- rv$player_scores[, .(Fighter = Player, FieldExp)]
    } else {
      # Fallback
      field_lineups <- unique(field_data$Lineup)
      n_field_entries <- nrow(field_data)
      
      field_fighter_counts <- data.table()
      for (lineup in field_lineups) {
        fighters <- extract_fighters(lineup)
        field_fighter_counts <- rbind(field_fighter_counts, data.table(Fighter = fighters))
      }
      field_exposure <- field_fighter_counts[, .(FieldExp = .N / n_field_entries * 100), by = Fighter]
    }
    
    # Merge
    exposure_data <- merge(user_exposure, field_exposure, by = "Fighter", all = TRUE)
    exposure_data[is.na(UserExp), UserExp := 0]
    exposure_data[is.na(FieldExp), FieldExp := 0]
    exposure_data[, Leverage := UserExp - FieldExp]
    
    # Add live exposure columns if fighters are eliminated
    if (length(rv$eliminated_fighters) > 0) {
      # Calculate live exposures (only counting lineups with ALL 6 fighters still alive)
      
      # User live exposure
      user_live_fighter_counts <- data.table()
      for (lineup in user_lineups) {
        fighters <- extract_fighters(lineup)
        # Only count if ALL 6 fighters are still alive
        if (length(fighters) == 6 && !any(fighters %in% rv$eliminated_fighters)) {
          user_live_fighter_counts <- rbind(user_live_fighter_counts, 
                                            data.table(Fighter = fighters))
        }
      }
      
      if (nrow(user_live_fighter_counts) > 0) {
        user_live_lineups <- sum(sapply(user_lineups, function(lineup) {
          fighters <- extract_fighters(lineup)
          length(fighters) == 6 && !any(fighters %in% rv$eliminated_fighters)
        }))
        user_live_exposure <- user_live_fighter_counts[, .(
          UserLiveExp = .N / user_live_lineups * 100
        ), by = Fighter]
      } else {
        user_live_exposure <- data.table(Fighter = character(0), UserLiveExp = numeric(0))
      }
      
      # Field live exposure
      field_lineups <- unique(field_data$Lineup)
      field_live_fighter_counts <- data.table()
      for (lineup in field_lineups) {
        fighters <- extract_fighters(lineup)
        # Only count if ALL 6 fighters are still alive
        if (length(fighters) == 6 && !any(fighters %in% rv$eliminated_fighters)) {
          field_live_fighter_counts <- rbind(field_live_fighter_counts,
                                             data.table(Fighter = fighters))
        }
      }
      
      if (nrow(field_live_fighter_counts) > 0) {
        field_live_lineups <- sum(sapply(field_lineups, function(lineup) {
          fighters <- extract_fighters(lineup)
          length(fighters) == 6 && !any(fighters %in% rv$eliminated_fighters)
        }))
        field_live_exposure <- field_live_fighter_counts[, .(
          FieldLiveExp = .N / field_live_lineups * 100
        ), by = Fighter]
      } else {
        field_live_exposure <- data.table(Fighter = character(0), FieldLiveExp = numeric(0))
      }
      
      # Merge live exposures
      exposure_data <- merge(exposure_data, user_live_exposure, by = "Fighter", all.x = TRUE)
      exposure_data <- merge(exposure_data, field_live_exposure, by = "Fighter", all.x = TRUE)
      exposure_data[is.na(UserLiveExp), UserLiveExp := 0]
      exposure_data[is.na(FieldLiveExp), FieldLiveExp := 0]
      exposure_data[, LiveLeverage := UserLiveExp - FieldLiveExp]
    }
    
    exposure_data <- exposure_data[order(-UserExp)]
    
    # Format for display - keep numeric leverage for coloring
    if (length(rv$eliminated_fighters) > 0 && "UserLiveExp" %in% names(exposure_data)) {
      display_data <- exposure_data[, .(
        Fighter,
        `Your Exposure` = paste0(round(UserExp, 1), "%"),
        `Field Exposure` = paste0(round(FieldExp, 1), "%"),
        Leverage = round(Leverage, 1),
        `Your Live Exp` = paste0(round(UserLiveExp, 1), "%"),
        `Field Live Exp` = paste0(round(FieldLiveExp, 1), "%"),
        LiveLeverage = round(LiveLeverage, 1)
      )]
    } else {
      display_data <- exposure_data[, .(
        Fighter,
        `Your Exposure` = paste0(round(UserExp, 1), "%"),
        `Field Exposure` = paste0(round(FieldExp, 1), "%"),
        Leverage = round(Leverage, 1)
      )]
    }
    
    dt_output <- datatable(
      display_data,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        dom = 'frtip',
        columnDefs = list(
          list(className = 'dt-center', targets = 1:(ncol(display_data)-1))
        )
      )
    ) %>%
      formatStyle(
        'Leverage',
        color = styleInterval(0, c('#dc3545', '#28a745')),
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Your Exposure',
        background = styleColorBar(c(0, 100), '#FFE500'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    
    # Add live leverage styling if present
    if ("LiveLeverage" %in% names(display_data)) {
      dt_output <- dt_output %>%
        formatStyle(
          'LiveLeverage',
          color = styleInterval(0, c('#dc3545', '#28a745')),
          fontWeight = 'bold'
        )
    }
    
    # Format leverage columns to show % sign and + for positive
    dt_output <- dt_output %>%
      formatCurrency('Leverage', currency = "", digits = 1, 
                     before = FALSE, mark = ",", dec.mark = ".")
    
    if ("LiveLeverage" %in% names(display_data)) {
      dt_output <- dt_output %>%
        formatCurrency('LiveLeverage', currency = "", digits = 1,
                       before = FALSE, mark = ",", dec.mark = ".")
    }
    
    dt_output
  })
  
  # Positive Leverage Plot
  output$positive_leverage_plot <- renderPlotly({
    req(rv$data, input$username, input$username != "")
    
    user_data <- rv$data[Username == input$username]
    field_data <- rv$data[Username != input$username]
    
    user_lineups <- unique(user_data$Lineup)
    n_user_entries <- nrow(user_data)
    
    user_fighter_counts <- data.table()
    for (lineup in user_lineups) {
      fighters <- extract_fighters(lineup)
      user_fighter_counts <- rbind(user_fighter_counts, data.table(Fighter = fighters))
    }
    user_exposure <- user_fighter_counts[, .(UserExp = .N / n_user_entries * 100), by = Fighter]
    
    # Field exposure from %Drafted
    if (!is.null(rv$player_scores) && "FieldExp" %in% names(rv$player_scores)) {
      field_exposure <- rv$player_scores[, .(Fighter = Player, FieldExp)]
    } else {
      field_lineups <- unique(field_data$Lineup)
      n_field_entries <- nrow(field_data)
      
      field_fighter_counts <- data.table()
      for (lineup in field_lineups) {
        fighters <- extract_fighters(lineup)
        field_fighter_counts <- rbind(field_fighter_counts, data.table(Fighter = fighters))
      }
      field_exposure <- field_fighter_counts[, .(FieldExp = .N / n_field_entries * 100), by = Fighter]
    }
    
    exposure_data <- merge(user_exposure, field_exposure, by = "Fighter", all = TRUE)
    exposure_data[is.na(UserExp), UserExp := 0]
    exposure_data[is.na(FieldExp), FieldExp := 0]
    exposure_data[, Leverage := UserExp - FieldExp]
    
    # Filter for positive leverage and sort by leverage descending
    positive_data <- exposure_data[Leverage > 0][order(-Leverage)]
    
    if (nrow(positive_data) == 0) {
      return(plotly_empty())
    }
    
    plot_ly(positive_data, 
            x = ~reorder(Fighter, Leverage), 
            y = ~Leverage,
            type = 'bar',
            marker = list(
              color = '#28a745',
              line = list(color = '#FFE500', width = 1)
            ),
            hovertemplate = paste(
              '<b>%{x}</b><br>',
              'Leverage: +%{y:.1f}%<br>',
              '<extra></extra>'
            )) %>%
      layout(
        plot_bgcolor = '#1a1a1a',
        paper_bgcolor = '#000000',
        font = list(color = '#FFFFFF'),
        xaxis = list(
          title = "",
          tickangle = -45,
          gridcolor = '#333333'
        ),
        yaxis = list(
          title = "Leverage (%)",
          gridcolor = '#333333',
          zeroline = FALSE
        ),
        margin = list(b = 100)
      )
  })
  
  # Negative Leverage Plot
  output$negative_leverage_plot <- renderPlotly({
    req(rv$data, input$username, input$username != "")
    
    user_data <- rv$data[Username == input$username]
    field_data <- rv$data[Username != input$username]
    
    user_lineups <- unique(user_data$Lineup)
    n_user_entries <- nrow(user_data)
    
    user_fighter_counts <- data.table()
    for (lineup in user_lineups) {
      fighters <- extract_fighters(lineup)
      user_fighter_counts <- rbind(user_fighter_counts, data.table(Fighter = fighters))
    }
    user_exposure <- user_fighter_counts[, .(UserExp = .N / n_user_entries * 100), by = Fighter]
    
    # Field exposure from %Drafted
    if (!is.null(rv$player_scores) && "FieldExp" %in% names(rv$player_scores)) {
      field_exposure <- rv$player_scores[, .(Fighter = Player, FieldExp)]
    } else {
      field_lineups <- unique(field_data$Lineup)
      n_field_entries <- nrow(field_data)
      
      field_fighter_counts <- data.table()
      for (lineup in field_lineups) {
        fighters <- extract_fighters(lineup)
        field_fighter_counts <- rbind(field_fighter_counts, data.table(Fighter = fighters))
      }
      field_exposure <- field_fighter_counts[, .(FieldExp = .N / n_field_entries * 100), by = Fighter]
    }
    
    exposure_data <- merge(user_exposure, field_exposure, by = "Fighter", all = TRUE)
    exposure_data[is.na(UserExp), UserExp := 0]
    exposure_data[is.na(FieldExp), FieldExp := 0]
    exposure_data[, Leverage := UserExp - FieldExp]
    
    # Filter for negative leverage and sort by leverage ascending (most negative first)
    negative_data <- exposure_data[Leverage < 0][order(Leverage)]
    
    if (nrow(negative_data) == 0) {
      return(plotly_empty())
    }
    
    plot_ly(negative_data, 
            x = ~reorder(Fighter, -Leverage), 
            y = ~Leverage,
            type = 'bar',
            marker = list(
              color = '#dc3545',
              line = list(color = '#FFE500', width = 1)
            ),
            hovertemplate = paste(
              '<b>%{x}</b><br>',
              'Leverage: %{y:.1f}%<br>',
              '<extra></extra>'
            )) %>%
      layout(
        plot_bgcolor = '#1a1a1a',
        paper_bgcolor = '#000000',
        font = list(color = '#FFFFFF'),
        xaxis = list(
          title = "",
          tickangle = -45,
          gridcolor = '#333333'
        ),
        yaxis = list(
          title = "Leverage (%)",
          gridcolor = '#333333',
          zeroline = FALSE
        ),
        margin = list(b = 100)
      )
  })
  
  # Dupe Analysis Content
  output$dupe_analysis_content <- renderUI({
    req(rv$data, input$username, input$username != "")
    
    tagList(
      h4("Your Lineup Duplication Distribution"),
      plotlyOutput("user_dupe_chart", height = "400px"),
      
      br(),
      
      h4("Most Duplicated Lineups in Contest"),
      DTOutput("dupe_table")
    )
  })
  
  output$dupe_table <- renderDT({
    req(rv$data)
    
    # Count lineup duplicates
    lineup_counts <- rv$data[, .N, by = Lineup][order(-N)]
    
    # Get top 50 most duplicated
    top_dupes <- head(lineup_counts[N > 1], 50)
    
    # Format for display
    display_data <- top_dupes[, .(
      Lineup,
      `Times Used` = N,
      `% of Contest` = paste0(round(N / nrow(rv$data) * 100, 2), "%")
    )]
    
    datatable(
      display_data,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        dom = 'frtip',
        columnDefs = list(
          list(width = '60%', targets = 0),
          list(className = 'dt-center', targets = 1:2)
        )
      )
    ) %>%
      formatStyle(
        'Times Used',
        background = styleColorBar(c(0, max(top_dupes$N)), '#FFE500'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # User Dupe Chart
  output$user_dupe_chart <- renderPlotly({
    req(rv$data, input$username, input$username != "")
    
    user_data <- rv$data[Username == input$username]
    
    # Count how many times each of user's lineups appears in contest
    lineup_counts <- rv$data[, .N, by = Lineup]
    user_lineups <- unique(user_data$Lineup)
    user_dupe_counts <- lineup_counts[Lineup %in% user_lineups, N]
    
    # Create distribution
    dupe_dist <- data.table(
      ActualOwnership = factor(user_dupe_counts, levels = sort(unique(user_dupe_counts))),
      Count = 1
    )[, .(Lineups = .N), by = ActualOwnership]
    
    plot_ly(dupe_dist,
            x = ~ActualOwnership,
            y = ~Lineups,
            type = 'bar',
            marker = list(
              color = '#FFE500',
              line = list(color = '#FFA500', width = 1)
            ),
            hovertemplate = paste(
              '<b>%{x} duplicates</b><br>',
              'Your lineups: %{y}<br>',
              '<extra></extra>'
            )) %>%
      layout(
        plot_bgcolor = '#1a1a1a',
        paper_bgcolor = '#000000',
        font = list(color = '#FFFFFF'),
        xaxis = list(
          title = "Number of Duplicates in Contest",
          gridcolor = '#333333'
        ),
        yaxis = list(
          title = "Number of Your Lineups",
          gridcolor = '#333333'
        )
      )
  })
  
  # Live Sweat Content
  output$live_sweat_content <- renderUI({
    req(rv$data, rv$fighters)
    
    # Dynamic labels based on sport
    player_label <- if(rv$sport == "NASCAR") "Driver" else "Fighter"
    players_label <- if(rv$sport == "NASCAR") "Drivers" else "Fighters"
    
    tagList(
      div(class = "well",
          h4(paste(player_label, "Status - Click to Mark as Eliminated"), style = "margin-top: 0;"),
          p(paste(players_label, "with current scores shown. Click to mark as eliminated."), 
            style = "color: #CCCCCC; margin-bottom: 15px;"),
          uiOutput("fighter_chips"),
          br(),
          div(style = "display: flex; gap: 10px; margin-top: 15px;",
              actionButton("clear_eliminated", "Clear All Eliminated", class = "btn-danger"),
              actionButton("mark_zeros", "Mark All 0.0 as Eliminated", class = "btn-primary")
          )
      ),
      
      br(),
      
      conditionalPanel(
        condition = "input.username != ''",
        h4(paste("Live", players_label, "Distribution - You vs Field")),
        DTOutput("live_distribution_table"),
        
        br(),
        
        h4("Your Live Lineup Status"),
        DTOutput("live_lineups_table")
      )
    )
  })
  
  # Fighter Chips
  output$fighter_chips <- renderUI({
    req(rv$fighters)
    
    # Get scores if available
    if (!is.null(rv$player_scores)) {
      fighter_info <- lapply(rv$fighters, function(fighter) {
        score <- rv$player_scores[Player == fighter, FPTS]
        if (length(score) == 0) score <- NA
        list(name = fighter, score = score)
      })
    } else {
      fighter_info <- lapply(rv$fighters, function(fighter) {
        list(name = fighter, score = NA)
      })
    }
    
    lapply(fighter_info, function(info) {
      is_eliminated <- info$name %in% rv$eliminated_fighters
      
      # Create label with score if available
      if (!is.na(info$score)) {
        label_text <- sprintf("%s (%.1f)", info$name, info$score)
      } else {
        label_text <- info$name
      }
      
      actionButton(
        inputId = paste0("fighter_", gsub("[^A-Za-z0-9]", "_", info$name)),
        label = label_text,
        class = if(is_eliminated) "fighter-chip eliminated" else "fighter-chip",
        onclick = sprintf("Shiny.setInputValue('toggle_fighter', '%s', {priority: 'event'})", info$name)
      )
    })
  })
  
  # Toggle fighter elimination
  observeEvent(input$toggle_fighter, {
    fighter <- input$toggle_fighter
    
    if (fighter %in% rv$eliminated_fighters) {
      rv$eliminated_fighters <- rv$eliminated_fighters[rv$eliminated_fighters != fighter]
    } else {
      rv$eliminated_fighters <- c(rv$eliminated_fighters, fighter)
    }
  })
  
  # Clear eliminated fighters
  observeEvent(input$clear_eliminated, {
    rv$eliminated_fighters <- character(0)
  })
  
  # Mark all 0.0 fighters as eliminated
  observeEvent(input$mark_zeros, {
    req(rv$player_scores)
    
    zero_fighters <- rv$player_scores[FPTS == 0, Player]
    rv$eliminated_fighters <- union(rv$eliminated_fighters, zero_fighters)
    
    showNotification(
      sprintf("Marked %d fighters with 0.0 points as eliminated", length(zero_fighters)),
      type = "message",
      duration = 3
    )
  })
  
  # Live Distribution Table
  output$live_distribution_table <- renderDT({
    req(rv$data, input$username, input$username != "")
    
    user_data <- rv$data[Username == input$username]
    field_data <- rv$data[Username != input$username]
    
    # Calculate live fighters for each lineup
    calc_live_fighters <- function(lineups) {
      sapply(lineups, function(lineup) {
        fighters <- extract_fighters(lineup)
        sum(!fighters %in% rv$eliminated_fighters)
      })
    }
    
    # User distribution
    user_lineups <- unique(user_data$Lineup)
    user_live <- calc_live_fighters(user_lineups)
    user_dist <- data.table(live = user_live)[, .(You = .N), by = live]
    
    # Field distribution
    field_lineups <- unique(field_data$Lineup)
    field_live <- calc_live_fighters(field_lineups)
    field_dist <- data.table(live = field_live)[, .(Field = .N), by = live]
    
    # Merge
    dist_table <- merge(
      data.table(live = 0:6),
      user_dist,
      by = "live",
      all.x = TRUE
    )
    dist_table <- merge(dist_table, field_dist, by = "live", all.x = TRUE)
    dist_table[is.na(You), You := 0]
    dist_table[is.na(Field), Field := 0]
    
    # Calculate percentages
    dist_table[, `You %` := paste0(round(You / sum(You) * 100, 1), "%")]
    dist_table[, `Field %` := paste0(round(Field / sum(Field) * 100, 1), "%")]
    
    # Rename and reorder
    player_label_plural <- if(rv$sport == "NASCAR") "Drivers" else "Fighters"
    
    display_table <- dist_table[, .(
      `Live Count` = live,
      `Your Lineups` = You,
      `You %`,
      `Field Lineups` = Field,
      `Field %`
    )][order(-`Live Count`)]
    
    # Rename first column dynamically
    setnames(display_table, "Live Count", paste("Live", player_label_plural))
    
    datatable(
      display_table,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom = 't',
        columnDefs = list(
          list(className = 'dt-center', targets = 0:4)
        )
      )
    ) %>%
      formatStyle(
        'You %',
        background = styleColorBar(c(0, 100), '#FFE500'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Field %',
        background = styleColorBar(c(0, 100), '#888888'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Live Lineups Table
  output$live_lineups_table <- renderDT({
    req(rv$data, input$username, input$username != "")
    
    user_data <- rv$data[Username == input$username]
    
    if (nrow(user_data) == 0) return(NULL)
    
    # Process each lineup
    lineup_status <- rbindlist(lapply(unique(user_data$Lineup), function(lineup) {
      fighters <- extract_fighters(lineup)
      live_fighters <- fighters[!fighters %in% rv$eliminated_fighters]
      dead_fighters <- fighters[fighters %in% rv$eliminated_fighters]
      
      # Calculate projected score if player_scores available
      projected_score <- NA
      if (!is.null(rv$player_scores)) {
        # Get scores for live fighters
        live_scores <- sapply(live_fighters, function(f) {
          score <- rv$player_scores[Player == f, FPTS]
          if (length(score) == 0) return(0)
          return(score)
        })
        projected_score <- sum(live_scores)
      }
      
      data.table(
        Lineup = lineup,
        `Live Fighters` = length(live_fighters),
        `Score` = if(!is.na(projected_score)) round(projected_score, 1) else NA_real_,
        `Live` = paste(live_fighters, collapse = ", "),
        `Eliminated` = if(length(dead_fighters) > 0) paste(dead_fighters, collapse = ", ") else "-"
      )
    }))
    
    # Sort by projected score descending (then by live fighters)
    if (!is.null(rv$player_scores)) {
      lineup_status <- lineup_status[order(-`Score`, -`Live Fighters`)]
    } else {
      lineup_status <- lineup_status[order(-`Live Fighters`)]
    }
    
    # Create table
    dt_table <- datatable(
      lineup_status,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        dom = 'frtip',
        columnDefs = list(
          list(width = '30%', targets = 0),
          list(className = 'dt-center', targets = 1:2)
        )
      )
    ) %>%
      formatStyle(
        'Live Fighters',
        background = styleColorBar(c(0, 6), '#28a745'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'Eliminated',
        color = '#dc3545',
        fontWeight = 'bold'
      )
    
    # Add score styling if available
    if (!is.null(rv$player_scores) && any(!is.na(lineup_status$`Score`))) {
      dt_table <- dt_table %>%
        formatStyle(
          'Score',
          background = styleColorBar(range(lineup_status$`Score`, na.rm = TRUE), '#FFE500'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center',
          fontWeight = 'bold'
        )
    }
    
    dt_table
  })
  
  # ============================================================================
  # SIMULATION ANALYSIS TAB (NEW)
  # ============================================================================
  
  # ============================================================================
  # SIMULATION ANALYSIS - COMPLETE REBUILD
  # ============================================================================
  # This replaces output$sim_analysis_content in the original file
  
  output$sim_analysis_content <- renderUI({
    if (!rv$sim_loaded) {
      return(
        div(class = "well",
            h4("No Simulation Data Loaded", style = "color: #FFE500; text-align: center; margin-top: 50px;"),
            p("Upload a sim optimals file to unlock simulation analysis features.",
              style = "color: #CCCCCC; text-align: center; font-size: 16px;")
        )
      )
    }
    
    tagList(
      br(),
      
      # MAIN ANALYSIS TABS
      tabsetPanel(
        # All Sim Lineups Tab (moved from Scored Lineups)
        tabPanel("All Sim Lineups",
                 br(),
                 downloadButton("download_all_sim", "Download All Sim Lineups", 
                                style = "margin-bottom: 15px; background-color: #FFE500; color: #000000; border: none; font-weight: bold;"),
                 br(), br(),
                 DTOutput("all_sim_lineups_table")
        ),
        
        # Sim Summary Tab (renamed from Sim Results Graphs)
        tabPanel("Sim Summary",
                 br(),
                 h4("Simulation Performance Overview", style = "color: #FFE500;"),
                 fluidRow(
                   column(6, plotlyOutput("sim_score_distribution", height = "400px")),
                   column(6, plotlyOutput("sim_finish_rates", height = "400px"))
                 ),
                 br(),
                 fluidRow(
                   column(6, plotlyOutput("sim_ownership_dist", height = "400px")),
                   column(6, plotlyOutput("sim_played_vs_not", height = "400px"))
                 )
        ),
        
        # Contest Tab (renamed from Contest-Only Lineups, now includes violin)
        tabPanel("Contest",
                 br(),
                 h4("Contest Lineup Analysis", style = "color: #FFE500;"),
                 
                 # Violin plot with 3 groups
                 plotlyOutput("contest_violin_plot", height = "600px"),
                 
                 br(),
                 h4("Contest-Only Lineups (NOT in Sim)", style = "color: #FFE500;"),
                 p(textOutput("contest_only_count"), style = "color: #CCCCCC; font-size: 16px;"),
                 br(),
                 fluidRow(
                   column(3, div(class = "stat-box",
                                 div(class = "stat-label", "Total Contest-Only"),
                                 div(class = "stat-value", textOutput("contest_only_total")))),
                   column(3, div(class = "stat-box",
                                 div(class = "stat-label", "Avg Score"),
                                 div(class = "stat-value", textOutput("contest_only_avg")))),
                   column(3, div(class = "stat-box",
                                 div(class = "stat-label", "Best Score"),
                                 div(class = "stat-value", textOutput("contest_only_best")))),
                   column(3, div(class = "stat-box",
                                 div(class = "stat-label", "Would Win"),
                                 div(class = "stat-value", textOutput("contest_only_win"))))
                 ),
                 br(),
                 plotlyOutput("contest_only_score_dist", height = "400px"),
                 br(),
                 DTOutput("contest_only_table")
        ),
        
        # Ownership vs ActualOwnership Tab
        tabPanel("Ownership vs ActualOwnership",
                 br(),
                 h4("Predicted Ownership vs Actual Duplication", style = "color: #FFE500;"),
                 p("How often were lineups with similar predicted ownership actually duplicated?", 
                   style = "color: #CCCCCC;"),
                 plotlyOutput("ownership_dupe_scatter", height = "600px"),
                 br(),
                 h4("Duplication Statistics by Ownership Bucket", style = "color: #FFE500;"),
                 DTOutput("ownership_dupe_table")
        ),
        
        # Filter Review Tab (separate - like lineup builder)
        tabPanel("Filter Review",
                 br(),
                 
                 # Filter Panel
                 div(style = "background-color: #1a1a1a; border: 2px solid #FFE500; border-radius: 8px; padding: 20px; margin-bottom: 20px;",
                     h4("LINEUP FILTERS", style = "color: #FFE500; margin: 0 0 15px 0; font-weight: bold;"),
                     
                     # Player Filters
                     fluidRow(
                       column(6,
                              div(style = "margin-bottom: 15px;",
                                  div(style = "color: #FFFFFF; font-weight: 600; margin-bottom: 5px; font-size: 14px;", "Must Include Players"),
                                  selectizeInput("filter_include_players", NULL,
                                                 choices = NULL, multiple = TRUE,
                                                 options = list(placeholder = 'Select players...'))
                              )
                       ),
                       column(6,
                              div(style = "margin-bottom: 15px;",
                                  div(style = "color: #FFFFFF; font-weight: 600; margin-bottom: 5px; font-size: 14px;", "Exclude Players"),
                                  selectizeInput("filter_exclude_players", NULL,
                                                 choices = NULL, multiple = TRUE,
                                                 options = list(placeholder = 'Select players...'))
                              )
                       )
                     ),
                     
                     # Min Rates & Ranges in 2-column layout
                     fluidRow(
                       column(2,
                              div(style = "background-color: #2a2a2a; padding: 15px; border-radius: 5px;",
                                  h5("Min Rates", style = "color: #FFE500; margin-top: 0; font-size: 14px;"),
                                  numericInput("filter_min_win", "Win:", value = 0, min = 0, max = 100, step = 0.1, width = "100%"),
                                  numericInput("filter_min_top1", "Top1:", value = 0, min = 0, max = 100, step = 0.1, width = "100%"),
                                  numericInput("filter_min_top5", "Top5:", value = 0, min = 0, max = 100, step = 0.1, width = "100%"),
                                  numericInput("filter_min_top10", "Top10:", value = 0, min = 0, max = 100, step = 0.1, width = "100%"),
                                  numericInput("filter_min_top20", "Top20:", value = 0, min = 0, max = 100, step = 0.1, width = "100%")
                              )
                       ),
                       column(8,
                              div(style = "background-color: #2a2a2a; padding: 15px; border-radius: 5px;",
                                  h5("Ranges", style = "color: #FFE500; margin-top: 0; font-size: 14px;"),
                                  fluidRow(
                                    column(6,
                                           div(style = "margin-bottom: 10px;",
                                               div(style = "color: #FFFFFF; font-size: 12px; margin-bottom: 3px;", "Salary (K)"),
                                               sliderInput("filter_salary", NULL, min = 39, max = 50, value = c(39, 50), step = 0.5, width = "100%")),
                                           div(style = "margin-bottom: 10px;",
                                               div(style = "color: #FFFFFF; font-size: 12px; margin-bottom: 3px;", "Avg Own"),
                                               sliderInput("filter_avgown", NULL, min = 0, max = 50, value = c(0, 50), step = 0.1, width = "100%")),
                                           div(style = "margin-bottom: 10px;",
                                               div(style = "color: #FFFFFF; font-size: 12px; margin-bottom: 3px;", "Avg Start"),
                                               sliderInput("filter_avgstart", NULL, min = 0, max = 40, value = c(0, 40), step = 0.5, width = "100%"))
                                    ),
                                    column(6,
                                           div(style = "margin-bottom: 10px;",
                                               div(style = "color: #FFFFFF; font-size: 12px; margin-bottom: 3px;", "Total Own"),
                                               sliderInput("filter_totalown", NULL, min = 0, max = 300, value = c(0, 300), step = 5, width = "100%")),
                                           div(style = "margin-bottom: 10px;",
                                               div(style = "color: #FFFFFF; font-size: 12px; margin-bottom: 3px;", "Total Start"),
                                               sliderInput("filter_totalstart", NULL, min = 0, max = 200, value = c(0, 200), step = 5, width = "100%"))
                                    )
                                  )
                              )
                       ),
                       column(2,
                              div(style = "background-color: #2a2a2a; padding: 15px; border-radius: 5px; text-align: center;",
                                  h5("Filtered Pool", style = "color: #FFE500; margin-top: 0; font-size: 14px;"),
                                  div(style = "font-size: 32px; color: #FFE500; font-weight: bold; margin: 20px 0;",
                                      textOutput("filter_pool_count", inline = TRUE)),
                                  div(style = "font-size: 12px; color: #FFFFFF; margin-bottom: 20px;", "lineups"),
                                  actionButton("reset_filter_review", "Reset Filters",
                                               style = "width: 100%; background-color: #FFE500; color: #000000; border: none; font-weight: bold; padding: 10px;")
                              )
                       )
                     )
                 ),
                 
                 # Summary Stats for Filtered Pool
                 h4("How This Pool Would Have Performed", style = "color: #FFE500;"),
                 fluidRow(
                   column(2, div(class = "stat-box",
                                 div(class = "stat-label", "Played in Contest"),
                                 div(class = "stat-value", textOutput("filter_played_count")))),
                   column(2, div(class = "stat-box",
                                 div(class = "stat-label", "Would Win"),
                                 div(class = "stat-value", textOutput("filter_would_win")))),
                   column(2, div(class = "stat-box",
                                 div(class = "stat-label", "Top 1%"),
                                 div(class = "stat-value", textOutput("filter_top1")))),
                   column(2, div(class = "stat-box",
                                 div(class = "stat-label", "Top 5%"),
                                 div(class = "stat-value", textOutput("filter_top5")))),
                   column(2, div(class = "stat-box",
                                 div(class = "stat-label", "Top 10%"),
                                 div(class = "stat-value", textOutput("filter_top10")))),
                   column(2, div(class = "stat-box",
                                 div(class = "stat-label", "Avg Score"),
                                 div(class = "stat-value", textOutput("filter_avg_score"))))
                 ),
                 br(),
                 
                 # Filtered Lineups Table
                 h4("Filtered Lineups", style = "color: #FFE500;"),
                 DTOutput("filter_lineups_table")
        ),
        
        # Keep metric tabs as sub-analysis
        tabPanel("Win Rate",
                 br(),
                 plotlyOutput("winrate_chart", height = "600px"),
                 br(),
                 h5("Performance by Win Rate Bucket", style = "color: #FFE500;"),
                 DTOutput("winrate_buckets")
        ),
        
        # Top 1% Tab
        tabPanel("Top 1%",
                 br(),
                 plotlyOutput("top1_chart", height = "600px"),
                 br(),
                 h5("Performance by Top 1% Bucket", style = "color: #FFE500;"),
                 DTOutput("top1_buckets")
        ),
        
        # Top 5% Tab
        tabPanel("Top 5%",
                 br(),
                 plotlyOutput("top5_chart", height = "600px"),
                 br(),
                 h5("Performance by Top 5% Bucket", style = "color: #FFE500;"),
                 DTOutput("top5_buckets")
        ),
        
        # Top 10% Tab
        tabPanel("Top 10%",
                 br(),
                 plotlyOutput("top10_chart", height = "600px"),
                 br(),
                 h5("Performance by Top 10% Bucket", style = "color: #FFE500;"),
                 DTOutput("top10_buckets")
        ),
        
        # Top 20% Tab
        tabPanel("Top 20%",
                 br(),
                 plotlyOutput("top20_chart", height = "600px"),
                 br(),
                 h5("Performance by Top 20% Bucket", style = "color: #FFE500;"),
                 DTOutput("top20_buckets")
        ),
        
        # Total Own Tab
        tabPanel("Total Own",
                 br(),
                 plotlyOutput("totalown_chart", height = "600px"),
                 br(),
                 h5("Performance by Total Ownership Bucket", style = "color: #FFE500;"),
                 DTOutput("totalown_buckets")
        ),
        
        # Avg Own Tab
        tabPanel("Avg Own",
                 br(),
                 plotlyOutput("avgown_chart", height = "600px"),
                 br(),
                 h5("Performance by Avg Ownership Bucket", style = "color: #FFE500;"),
                 DTOutput("avgown_buckets")
        ),
        
        # Total Start Tab  
        tabPanel("Total Start",
                 br(),
                 plotlyOutput("totalstart_chart", height = "600px"),
                 br(),
                 h5("Performance by Total Start Bucket", style = "color: #FFE500;"),
                 DTOutput("totalstart_buckets")
        ),
        
        # Avg Start Tab
        tabPanel("Avg Start",
                 br(),
                 plotlyOutput("avgstart_chart", height = "600px"),
                 br(),
                 h5("Performance by Avg Start Bucket", style = "color: #FFE500;"),
                 DTOutput("avgstart_buckets")
        ),
        
        # Salary Tab
        tabPanel("Salary",
                 br(),
                 plotlyOutput("salary_chart", height = "600px"),
                 br(),
                 h5("Performance by Salary Bucket", style = "color: #FFE500;"),
                 DTOutput("salary_buckets")
        ),
        
        # Ownership/Dupe Analysis Tab
        tabPanel("Ownership vs ActualOwnership",
                 br(),
                 h4("Predicted Ownership vs Actual Duplication", style = "color: #FFE500;"),
                 p("How often were lineups with similar predicted ownership actually duplicated?", 
                   style = "color: #CCCCCC;"),
                 plotlyOutput("ownership_dupe_scatter", height = "600px"),
                 br(),
                 h4("Duplication Statistics by Ownership Bucket", style = "color: #FFE500;"),
                 DTOutput("ownership_dupe_table")
        )
      )
    )
  })
  
  # ============================================================================
  # UNIFIED POOL CREATION (includes contest lineups not in sim)
  # ============================================================================
  
  observe({
    req(rv$sim_data, rv$data, rv$player_scores)
    
    # Building sim pool with actual ownership
    
    # Start with sim data
    sim_data <- copy(rv$sim_data)
    sim_data[, `:=`(InSim = TRUE, InContest = PlayedInContest)]
    
    # Calculate actual ownership for sim lineups
    if (!is.null(rv$data) && nrow(rv$data) > 0) {
      lineup_counts <- rv$data[, .(ActualOwnership = .N), by = Lineup]
      
      # Create lineup keys for contest lineups
      lineup_counts[, LineupKey := {
        sapply(Lineup, function(lineup) {
          fighters <- extract_fighters(lineup)
          paste(sort(fighters), collapse = "|")
        })
      }]
      
      # Merge actual ownership to sim lineups
      sim_data <- merge(sim_data, 
                        lineup_counts[, .(LineupKey, ActualOwnership)], 
                        by = "LineupKey", all.x = TRUE)
      sim_data[is.na(ActualOwnership), ActualOwnership := 0]
      
      # Find contest lineups NOT in sim (for violin/contest tab only)
      contest_only_keys <- lineup_counts[!LineupKey %in% sim_data$LineupKey]
      
      if (nrow(contest_only_keys) > 0) {
        # Get full contest data for these lineups
        contest_only <- rv$data[Lineup %in% contest_only_keys$Lineup]
        contest_only[, LineupKey := {
          sapply(Lineup, function(lineup) {
            fighters <- extract_fighters(lineup)
            paste(sort(fighters), collapse = "|")
          })
        }]
        
        # Score them
        score_lookup <- setNames(rv$player_scores$FPTS, rv$player_scores$Player)
        contest_only[, ActualScore := {
          sapply(Lineup, function(lineup) {
            fighters <- extract_fighters(lineup)
            sum(score_lookup[fighters], na.rm = TRUE)
          })
        }]
        
        # Extract player columns
        player_cols <- grep("^(Fighter|Player|Driver)[0-9]$", names(sim_data), value = TRUE)
        for (i in seq_along(player_cols)) {
          contest_only[, (player_cols[i]) := {
            sapply(LineupKey, function(key) {
              players <- strsplit(key, "|", fixed = TRUE)[[1]]
              if (i <= length(players)) players[i] else NA_character_
            })
          }]
        }
        
        # Merge actual ownership
        contest_only <- merge(contest_only, contest_only_keys[, .(LineupKey, ActualOwnership)], by = "LineupKey")
        
        # Store separately - not in unified pool
        rv$contest_only <- contest_only
        cat("✓ Created pool:", nrow(sim_data), "sim,", nrow(contest_only), "contest-only\n")
      } else {
        rv$contest_only <- NULL
      }
    } else {
      sim_data[, ActualOwnership := 0]
      rv$contest_only <- NULL
    }
    
    # Unified pool is just sim data
    rv$unified_pool <- sim_data
    
    # Update slider ranges using isolate to prevent reactive loops
    isolate({
      if ("WinRate" %in% names(sim_data)) {
        rng <- range(sim_data$WinRate, na.rm = TRUE)
        updateSliderInput(session, "winrate_slider", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
      }
      if ("Top1Pct" %in% names(sim_data)) {
        rng <- range(sim_data$Top1Pct, na.rm = TRUE)
        updateSliderInput(session, "top1_slider", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
      }
      if ("Top5Pct" %in% names(sim_data)) {
        rng <- range(sim_data$Top5Pct, na.rm = TRUE)
        updateSliderInput(session, "top5_slider", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
      }
      if ("Top10Pct" %in% names(sim_data)) {
        rng <- range(sim_data$Top10Pct, na.rm = TRUE)
        updateSliderInput(session, "top10_slider", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
      }
      if ("Top20Pct" %in% names(sim_data)) {
        rng <- range(sim_data$Top20Pct, na.rm = TRUE)
        updateSliderInput(session, "top20_slider", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
      }
      if ("CumulativeOwnership" %in% names(sim_data)) {
        rng <- range(sim_data$CumulativeOwnership, na.rm = TRUE)
        updateSliderInput(session, "totalown_slider", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
      }
      if ("GeometricMeanOwnership" %in% names(sim_data)) {
        rng <- range(sim_data$GeometricMeanOwnership, na.rm = TRUE)
        updateSliderInput(session, "avgown_slider", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
      }
      if ("TotalSalary" %in% names(sim_data)) {
        rng <- range(sim_data$TotalSalary, na.rm = TRUE)
        updateSliderInput(session, "salary_slider", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
      }
      if ("CumulativeStarting" %in% names(sim_data)) {
        rng <- range(sim_data$CumulativeStarting, na.rm = TRUE)
        updateSliderInput(session, "totalstart_slider", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
      }
      if ("GeometricMeanStarting" %in% names(sim_data)) {
        rng <- range(sim_data$GeometricMeanStarting, na.rm = TRUE)
        updateSliderInput(session, "avgstart_slider", min = floor(rng[1]), max = ceiling(rng[2]), value = rng)
      }
      
      # Update player dropdowns
      player_cols <- grep("^(Fighter|Player|Driver)[0-9]$", names(sim_data), value = TRUE)
      all_players <- sort(unique(unlist(sim_data[, player_cols, with = FALSE])))
      
      updateSelectizeInput(session, "sim_include_players", choices = all_players, server = TRUE)
      updateSelectizeInput(session, "sim_exclude_players", choices = all_players, server = TRUE)
      
    })
  })
  
  # ============================================================================
  # FILTERED SIM DATA (Reactive based on slider filters)
  # ============================================================================
  
  filtered_sim_pool <- reactive({
    req(rv$unified_pool)
    
    data <- copy(rv$unified_pool)
    
    player_cols <- grep("^(Fighter|Player|Driver)[0-9]$", names(data), value = TRUE)
    
    # Player filters
    if (!is.null(input$sim_include_players) && length(input$sim_include_players) > 0) {
      for (player in input$sim_include_players) {
        data <- data[Reduce(`|`, lapply(.SD, function(x) x == player)), .SDcols = player_cols]
      }
    }
    
    if (!is.null(input$sim_exclude_players) && length(input$sim_exclude_players) > 0) {
      for (player in input$sim_exclude_players) {
        data <- data[!Reduce(`|`, lapply(.SD, function(x) x == player)), .SDcols = player_cols]
      }
    }
    
    # Slider filters - ONLY filter on InSim lineups, keep contest-only (InSim=FALSE)
    if (!is.null(input$winrate_slider) && "WinRate" %in% names(data)) {
      before <- nrow(data)
      data <- data[InSim == FALSE | (WinRate >= input$winrate_slider[1] & WinRate <= input$winrate_slider[2])]
    }
    if (!is.null(input$top1_slider) && "Top1Pct" %in% names(data)) {
      data <- data[InSim == FALSE | (Top1Pct >= input$top1_slider[1] & Top1Pct <= input$top1_slider[2])]
    }
    if (!is.null(input$top5_slider) && "Top5Pct" %in% names(data)) {
      data <- data[InSim == FALSE | (Top5Pct >= input$top5_slider[1] & Top5Pct <= input$top5_slider[2])]
    }
    if (!is.null(input$top10_slider) && "Top10Pct" %in% names(data)) {
      data <- data[InSim == FALSE | (Top10Pct >= input$top10_slider[1] & Top10Pct <= input$top10_slider[2])]
    }
    if (!is.null(input$top20_slider) && "Top20Pct" %in% names(data)) {
      data <- data[InSim == FALSE | (Top20Pct >= input$top20_slider[1] & Top20Pct <= input$top20_slider[2])]
    }
    if (!is.null(input$totalown_slider) && "CumulativeOwnership" %in% names(data)) {
      data <- data[InSim == FALSE | (CumulativeOwnership >= input$totalown_slider[1] & CumulativeOwnership <= input$totalown_slider[2])]
    }
    if (!is.null(input$avgown_slider) && "GeometricMeanOwnership" %in% names(data)) {
      data <- data[InSim == FALSE | (GeometricMeanOwnership >= input$avgown_slider[1] & GeometricMeanOwnership <= input$avgown_slider[2])]
    }
    if (!is.null(input$salary_slider) && "TotalSalary" %in% names(data)) {
      before <- nrow(data)
      data <- data[InSim == FALSE | (TotalSalary >= input$salary_slider[1] & TotalSalary <= input$salary_slider[2])]
    }
    if (!is.null(input$totalstart_slider) && "CumulativeStarting" %in% names(data)) {
      data <- data[InSim == FALSE | (CumulativeStarting >= input$totalstart_slider[1] & CumulativeStarting <= input$totalstart_slider[2])]
    }
    if (!is.null(input$avgstart_slider) && "GeometricMeanStarting" %in% names(data)) {
      data <- data[InSim == FALSE | (GeometricMeanStarting >= input$avgstart_slider[1] & GeometricMeanStarting <= input$avgstart_slider[2])]
    }
    
    # Played filter
    if (!is.null(input$sim_played_filter) && input$sim_played_filter != "all") {
      if (input$sim_played_filter == "yes") {
        data <- data[InContest == TRUE]
      } else {
        data <- data[InContest == FALSE]
      }
    }
    
    return(data)
  }) %>% debounce(300)
  
  # ============================================================================
  # SUMMARY STAT OUTPUTS
  # ============================================================================
  
  output$sim_total_lineups <- renderText({
    req(filtered_sim_pool())
    as.character(format(nrow(filtered_sim_pool()), big.mark = ","))
  })
  
  output$sim_played_count <- renderText({
    req(filtered_sim_pool())
    played <- sum(filtered_sim_pool()$InContest, na.rm = TRUE)
    pct <- round(played / nrow(filtered_sim_pool()) * 100, 1)
    paste0(format(played, big.mark = ","), " (", pct, "%)")
  })
  
  output$sim_would_win <- renderText({
    req(filtered_sim_pool(), rv$data)
    winning_score <- max(rv$data$Points, na.rm = TRUE)
    wins <- sum(filtered_sim_pool()$ActualScore >= winning_score, na.rm = TRUE)
    format(wins, big.mark = ",")  # Raw count, not percentage
  })
  
  output$sim_top1 <- renderText({
    req(filtered_sim_pool(), rv$data)
    threshold <- quantile(rv$data$Points, 0.99, na.rm = TRUE)
    count <- sum(filtered_sim_pool()$ActualScore >= threshold, na.rm = TRUE)
    total <- nrow(filtered_sim_pool())
    paste0(round(count/total * 100, 1), "%")
  })
  
  output$sim_top5 <- renderText({
    req(filtered_sim_pool(), rv$data)
    threshold <- quantile(rv$data$Points, 0.95, na.rm = TRUE)
    count <- sum(filtered_sim_pool()$ActualScore >= threshold, na.rm = TRUE)
    total <- nrow(filtered_sim_pool())
    paste0(round(count/total * 100, 1), "%")
  })
  
  output$sim_top10 <- renderText({
    req(filtered_sim_pool(), rv$data)
    threshold <- quantile(rv$data$Points, 0.90, na.rm = TRUE)
    count <- sum(filtered_sim_pool()$ActualScore >= threshold, na.rm = TRUE)
    total <- nrow(filtered_sim_pool())
    paste0(round(count/total * 100, 1), "%")
  })
  
  output$sim_top20 <- renderText({
    req(filtered_sim_pool(), rv$data)
    threshold <- quantile(rv$data$Points, 0.80, na.rm = TRUE)
    count <- sum(filtered_sim_pool()$ActualScore >= threshold, na.rm = TRUE)
    total <- nrow(filtered_sim_pool())
    paste0(round(count/total * 100, 1), "%")
  })
  
  output$sim_avg_score <- renderText({
    req(filtered_sim_pool())
    round(mean(filtered_sim_pool()$ActualScore, na.rm = TRUE), 1)
  })
  
  output$sim_best_score <- renderText({
    req(filtered_sim_pool())
    round(max(filtered_sim_pool()$ActualScore, na.rm = TRUE), 1)
  })
  
  # ============================================================================
  # HELPER FUNCTION: CREATE 3-COLOR SCATTER PLOT
  # ============================================================================
  
  create_metric_scatter <- function(data, x_col, x_label, title, contest_data) {
    if (is.null(data) || nrow(data) < 2) {
      return(plot_ly() %>% layout(
        title = list(text = "Not enough data", font = list(color = '#FFE500')),
        paper_bgcolor = '#000000', plot_bgcolor = '#1a1a1a'
      ))
    }
    
    plot_data <- copy(data)
    
    # Calculate thresholds
    winning_score <- max(contest_data$Points, na.rm = TRUE)
    top1_threshold <- quantile(contest_data$Points, 0.99, na.rm = TRUE)
    top10_threshold <- quantile(contest_data$Points, 0.90, na.rm = TRUE)
    top20_threshold <- quantile(contest_data$Points, 0.80, na.rm = TRUE)
    
    # Get x-axis range for threshold lines
    x_min <- min(plot_data[[x_col]], na.rm = TRUE)
    x_max <- max(plot_data[[x_col]], na.rm = TRUE)
    
    # Create status for 3-color system
    plot_data[, Status := ifelse(InSim & InContest, "In Sim & Played",
                                 ifelse(InSim & !InContest, "In Sim Only",
                                        ifelse(!InSim & InContest, "Played (Not in Sim)", "Unknown")))]
    
    # Sort so played lineups are plotted LAST (on top)
    setorder(plot_data, InContest)
    
    # Build model (only on sim lineups with the metric)
    model <- tryCatch({
      model_data <- plot_data[InSim == TRUE & !is.na(get(x_col))]
      if (nrow(model_data) < 2) return(NULL)
      lm(ActualScore ~ get(x_col), data = model_data)
    }, error = function(e) NULL)
    
    if (!is.null(model)) {
      plot_data[InSim == TRUE, fitted := predict(model, newdata = .SD)]
      r_sq <- summary(model)$r.squared
    } else {
      r_sq <- 0
    }
    
    # Create plot - played lineups will be on top since we sorted by InContest
    p <- plot_ly(plot_data,
                 x = as.formula(paste0("~", x_col)),
                 y = ~ActualScore,
                 color = ~Status,
                 colors = c("In Sim & Played" = "#FFE500",
                            "In Sim Only" = "#666666",
                            "Played (Not in Sim)" = "#FF0000"),
                 type = 'scatter',
                 mode = 'markers',
                 marker = list(size = 10, opacity = 0.7, line = list(color = '#000000', width = 1)),
                 text = ~paste(x_label, ":", round(get(x_col), 1), "<br>",
                               "Score:", round(ActualScore, 1), "<br>",
                               "Status:", Status),
                 hoverinfo = 'text') %>%
      add_trace(
        data = plot_data[InSim == TRUE & !is.na(fitted)],
        x = as.formula(paste0("~", x_col)),
        y = ~fitted,
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#FFE500', width = 3, dash = 'dash'),
        name = 'Trendline',
        hoverinfo = 'skip',
        showlegend = TRUE,
        inherit = FALSE
      )
    
    # Add threshold lines
    p <- p %>%
      add_trace(x = c(x_min, x_max), y = c(winning_score, winning_score),
                type = 'scatter', mode = 'lines',
                line = list(color = '#00FF00', width = 2, dash = 'dot'),
                name = paste0('Winning (', round(winning_score, 1), ')'),
                hoverinfo = 'name', showlegend = TRUE, inherit = FALSE) %>%
      add_trace(x = c(x_min, x_max), y = c(top1_threshold, top1_threshold),
                type = 'scatter', mode = 'lines',
                line = list(color = '#3498db', width = 2, dash = 'dot'),
                name = paste0('Top 1% (', round(top1_threshold, 1), ')'),
                hoverinfo = 'name', showlegend = TRUE, inherit = FALSE) %>%
      add_trace(x = c(x_min, x_max), y = c(top10_threshold, top10_threshold),
                type = 'scatter', mode = 'lines',
                line = list(color = '#e74c3c', width = 2, dash = 'dot'),
                name = paste0('Top 10% (', round(top10_threshold, 1), ')'),
                hoverinfo = 'name', showlegend = TRUE, inherit = FALSE) %>%
      add_trace(x = c(x_min, x_max), y = c(top20_threshold, top20_threshold),
                type = 'scatter', mode = 'lines',
                line = list(color = '#f39c12', width = 2, dash = 'dot'),
                name = paste0('Top 20% (', round(top20_threshold, 1), ')'),
                hoverinfo = 'name', showlegend = TRUE, inherit = FALSE) %>%
      layout(
        title = list(text = sprintf("%s vs Actual Score (R² = %.3f)", title, r_sq),
                     font = list(color = '#FFE500', size = 18)),
        xaxis = list(title = x_label, color = '#FFFFFF', gridcolor = '#333333'),
        yaxis = list(title = "Actual Score", color = '#FFFFFF', gridcolor = '#333333'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF', size = 14),
        showlegend = TRUE,
        legend = list(
          title = list(text = "Lineup Status"),
          bgcolor = '#1a1a1a',
          bordercolor = '#FFE500',
          borderwidth = 1
        )
      )
    
    return(p)
  }
  
  # ============================================================================
  # SCORED LINEUPS TABLE & DOWNLOAD
  # ============================================================================
  
  output$scored_lineups_table <- renderDT({
    req(filtered_sim_pool())
    
    data <- copy(filtered_sim_pool())
    player_cols <- grep("^(Fighter|Player|Driver)[0-9]$", names(data), value = TRUE)
    
    # Select columns to display
    display_cols <- c(player_cols, "WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct",
                      "CumulativeOwnership", "GeometricMeanOwnership", 
                      "ActualScore", "InContest", "TimesPlayed")
    display_cols <- intersect(display_cols, names(data))
    
    # Rename columns for display
    col_names <- display_cols
    col_names <- gsub("CumulativeOwnership", "TotalOwn", col_names)
    col_names <- gsub("GeometricMeanOwnership", "AvgOwn", col_names)
    col_names <- gsub("ActualOwnership", "Dupes", col_names)
    
    datatable(data[, ..display_cols],
              colnames = col_names,
              options = list(pageLength = 50, scrollX = TRUE, dom = 'Bfrtip'),
              rownames = FALSE,
              extensions = 'Buttons') %>%
      formatRound(c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct"), 1) %>%
      formatRound("ActualScore", 1) %>%
      formatStyle('InContest',
                  backgroundColor = styleEqual(c(TRUE, FALSE), c('#a3e4d7', '#f5b7b1')))
  })
  
  output$download_scored_lineups <- downloadHandler(
    filename = function() {
      paste0("scored_lineups_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_sim_pool(), file, row.names = FALSE)
    }
  )
  
  # ============================================================================
  # METRIC SCATTER PLOTS (9 total)
  # ============================================================================
  
  output$winrate_chart <- renderPlotly({
    req(rv$unified_pool, "WinRate" %in% names(rv$unified_pool))
    create_metric_scatter(rv$unified_pool, "WinRate", "Win Rate %", "Win Rate", rv$data)
  })
  
  output$top1_chart <- renderPlotly({
    req(rv$unified_pool, "Top1Pct" %in% names(rv$unified_pool))
    create_metric_scatter(rv$unified_pool, "Top1Pct", "Top 1%", "Top 1%", rv$data)
  })
  
  output$top5_chart <- renderPlotly({
    req(rv$unified_pool, "Top5Pct" %in% names(rv$unified_pool))
    create_metric_scatter(rv$unified_pool, "Top5Pct", "Top 5%", "Top 5%", rv$data)
  })
  
  output$top10_chart <- renderPlotly({
    req(rv$unified_pool, "Top10Pct" %in% names(rv$unified_pool))
    create_metric_scatter(rv$unified_pool, "Top10Pct", "Top 10%", "Top 10%", rv$data)
  })
  
  output$top20_chart <- renderPlotly({
    req(rv$unified_pool, "Top20Pct" %in% names(rv$unified_pool))
    create_metric_scatter(rv$unified_pool, "Top20Pct", "Top 20%", "Top 20%", rv$data)
  })
  
  output$totalown_chart <- renderPlotly({
    req(rv$unified_pool, "CumulativeOwnership" %in% names(rv$unified_pool))
    create_metric_scatter(rv$unified_pool, "CumulativeOwnership", "Total Ownership %", "Total Ownership", rv$data)
  })
  
  output$avgown_chart <- renderPlotly({
    req(rv$unified_pool, "GeometricMeanOwnership" %in% names(rv$unified_pool))
    create_metric_scatter(rv$unified_pool, "GeometricMeanOwnership", "Avg Ownership %", "Avg Ownership", rv$data)
  })
  
  output$totalstart_chart <- renderPlotly({
    req(rv$unified_pool, "CumulativeStarting" %in% names(rv$unified_pool))
    create_metric_scatter(rv$unified_pool, "CumulativeStarting", "Total Start Position", "Total Start", rv$data)
  })
  
  output$avgstart_chart <- renderPlotly({
    req(rv$unified_pool, "GeometricMeanStarting" %in% names(rv$unified_pool))
    create_metric_scatter(rv$unified_pool, "GeometricMeanStarting", "Avg Start Position", "Avg Start", rv$data)
  })
  
  # ============================================================================
  # HELPER FUNCTION: CREATE BUCKET TABLE
  # ============================================================================
  
  create_bucket_table <- function(data, metric_col, breaks, labels) {
    req(rv$data)
    
    # Filter to sim lineups only (contest-only don't have metrics)
    sim_data <- data[InSim == TRUE & !is.na(get(metric_col))]
    
    if (nrow(sim_data) == 0) {
      return(datatable(data.table(Message = "No data"), rownames = FALSE))
    }
    
    # Create buckets
    sim_data[, Bucket := cut(get(metric_col), breaks = breaks, labels = labels, include.lowest = TRUE)]
    
    # Calculate thresholds
    winning_score <- max(rv$data$Points, na.rm = TRUE)
    top1_threshold <- quantile(rv$data$Points, 0.99, na.rm = TRUE)
    top5_threshold <- quantile(rv$data$Points, 0.95, na.rm = TRUE)
    top10_threshold <- quantile(rv$data$Points, 0.90, na.rm = TRUE)
    top20_threshold <- quantile(rv$data$Points, 0.80, na.rm = TRUE)
    
    # Bucket stats
    bucket_stats <- sim_data[, .(
      Count = .N,
      `Avg Score` = round(mean(ActualScore, na.rm = TRUE), 1),
      `Max Score` = round(max(ActualScore, na.rm = TRUE), 1),
      `Min Score` = round(min(ActualScore, na.rm = TRUE), 1),
      `Played Count` = sum(InContest, na.rm = TRUE),
      `Play Rate %` = round(sum(InContest, na.rm = TRUE) / .N * 100, 1),
      `Win Rate %` = round(sum(ActualScore >= winning_score, na.rm = TRUE) / .N * 100, 1),
      `Top 1% Rate` = round(sum(ActualScore >= top1_threshold, na.rm = TRUE) / .N * 100, 1),
      `Top 5% Rate` = round(sum(ActualScore >= top5_threshold, na.rm = TRUE) / .N * 100, 1),
      `Top 10% Rate` = round(sum(ActualScore >= top10_threshold, na.rm = TRUE) / .N * 100, 1),
      `Top 20% Rate` = round(sum(ActualScore >= top20_threshold, na.rm = TRUE) / .N * 100, 1)
    ), by = Bucket][order(Bucket)]
    
    datatable(
      bucket_stats,
      rownames = FALSE,
      options = list(pageLength = 10, dom = 't')
    ) %>%
      formatStyle('Avg Score',
                  background = styleColorBar(range(bucket_stats$`Avg Score`), '#FFE500'),
                  backgroundSize = '100% 90%', backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  }
  
  # ============================================================================
  # BUCKET TABLES (9 total)
  # ============================================================================
  
  output$winrate_buckets <- renderDT({
    req(filtered_sim_pool(), "WinRate" %in% names(filtered_sim_pool()))
    create_bucket_table(filtered_sim_pool(), "WinRate",
                        breaks = c(0, 2, 5, 10, 20, 100),
                        labels = c("0-2%", "2-5%", "5-10%", "10-20%", "20%+"))
  })
  
  output$top1_buckets <- renderDT({
    req(filtered_sim_pool(), "Top1Pct" %in% names(filtered_sim_pool()))
    create_bucket_table(filtered_sim_pool(), "Top1Pct",
                        breaks = c(0, 1, 3, 5, 10, 100),
                        labels = c("0-1%", "1-3%", "3-5%", "5-10%", "10%+"))
  })
  
  output$top5_buckets <- renderDT({
    req(filtered_sim_pool(), "Top5Pct" %in% names(filtered_sim_pool()))
    create_bucket_table(filtered_sim_pool(), "Top5Pct",
                        breaks = c(0, 5, 10, 20, 40, 100),
                        labels = c("0-5%", "5-10%", "10-20%", "20-40%", "40%+"))
  })
  
  output$top10_buckets <- renderDT({
    req(filtered_sim_pool(), "Top10Pct" %in% names(filtered_sim_pool()))
    create_bucket_table(filtered_sim_pool(), "Top10Pct",
                        breaks = c(0, 10, 20, 40, 60, 100),
                        labels = c("0-10%", "10-20%", "20-40%", "40-60%", "60%+"))
  })
  
  output$top20_buckets <- renderDT({
    req(filtered_sim_pool(), "Top20Pct" %in% names(filtered_sim_pool()))
    create_bucket_table(filtered_sim_pool(), "Top20Pct",
                        breaks = c(0, 20, 40, 60, 80, 100),
                        labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80%+"))
  })
  
  output$totalown_buckets <- renderDT({
    req(filtered_sim_pool(), "CumulativeOwnership" %in% names(filtered_sim_pool()))
    create_bucket_table(filtered_sim_pool(), "CumulativeOwnership",
                        breaks = c(0, 100, 200, 300, 400, 1000),
                        labels = c("0-100%", "100-200%", "200-300%", "300-400%", "400%+"))
  })
  
  output$avgown_buckets <- renderDT({
    req(filtered_sim_pool(), "GeometricMeanOwnership" %in% names(filtered_sim_pool()))
    create_bucket_table(filtered_sim_pool(), "GeometricMeanOwnership",
                        breaks = c(0, 10, 20, 30, 40, 100),
                        labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40%+"))
  })
  
  output$totalstart_buckets <- renderDT({
    req(filtered_sim_pool(), "CumulativeStarting" %in% names(filtered_sim_pool()))
    create_bucket_table(filtered_sim_pool(), "CumulativeStarting",
                        breaks = c(0, 20, 40, 60, 80, 200),
                        labels = c("0-20", "20-40", "40-60", "60-80", "80+"))
  })
  
  output$avgstart_buckets <- renderDT({
    req(filtered_sim_pool(), "GeometricMeanStarting" %in% names(filtered_sim_pool()))
    create_bucket_table(filtered_sim_pool(), "GeometricMeanStarting",
                        breaks = c(0, 5, 10, 15, 20, 50),
                        labels = c("0-5", "5-10", "10-15", "15-20", "20+"))
  })
  
  # ============================================================================
  # SALARY SCATTER PLOT & BUCKET TABLE
  # ============================================================================
  
  output$salary_chart <- renderPlotly({
    req(rv$unified_pool, "TotalSalary" %in% names(rv$unified_pool))
    create_metric_scatter(rv$unified_pool, "TotalSalary", "Total Salary", "Salary", rv$data)
  })
  
  output$salary_buckets <- renderDT({
    req(filtered_sim_pool(), "TotalSalary" %in% names(filtered_sim_pool()))
    create_bucket_table(filtered_sim_pool(), "TotalSalary",
                        breaks = c(0, 44000, 46000, 48000, 49000, 50000),
                        labels = c("<44K", "44-46K", "46-48K", "48-49K", "49-50K"))
  })
  
  # ============================================================================
  # RESET FILTERS
  # ============================================================================
  
  observeEvent(input$reset_sim_filters, {
    req(rv$unified_pool)
    
    # Reset all sliders to data ranges
    if ("WinRate" %in% names(rv$unified_pool)) {
      updateSliderInput(session, "winrate_slider",
                        value = c(min(rv$unified_pool$WinRate, na.rm = TRUE),
                                  max(rv$unified_pool$WinRate, na.rm = TRUE)))
    }
    if ("Top1Pct" %in% names(rv$unified_pool)) {
      updateSliderInput(session, "top1_slider",
                        value = c(min(rv$unified_pool$Top1Pct, na.rm = TRUE),
                                  max(rv$unified_pool$Top1Pct, na.rm = TRUE)))
    }
    # ... similar for all other sliders
    
    # Reset player filters
    updateSelectizeInput(session, "sim_include_players", selected = character(0))
    updateSelectizeInput(session, "sim_exclude_players", selected = character(0))
    updateSelectInput(session, "sim_played_filter", selected = "all")
  })
  
  # ============================================================================
  # UPDATE SLIDER RANGES WHEN SIM LOADS
  # ============================================================================
  
  # Add this observer after rv$unified_pool is created
  observe({
    req(rv$unified_pool)
    
    data <- rv$unified_pool
    
    # Update all slider ranges
    if ("WinRate" %in% names(data)) {
      updateSliderInput(session, "winrate_slider",
                        min = floor(min(data$WinRate, na.rm = TRUE)),
                        max = ceiling(max(data$WinRate, na.rm = TRUE)),
                        value = c(floor(min(data$WinRate, na.rm = TRUE)),
                                  ceiling(max(data$WinRate, na.rm = TRUE))))
    }
    
    # Similar for all metrics...
    # Update player filter choices
    player_cols <- grep("^(Fighter|Player|Driver)[0-9]$", names(data), value = TRUE)
    all_players <- sort(unique(unlist(data[, player_cols, with = FALSE])))
    
    updateSelectizeInput(session, "sim_include_players", choices = all_players, server = TRUE)
    updateSelectizeInput(session, "sim_exclude_players", choices = all_players, server = TRUE)
  })
  output$actual_score_dist <- renderPlotly({
    req(filtered_sim_pool())
    
    data <- filtered_sim_pool()
    
    plot_ly(data,
            x = ~ActualScore,
            type = 'histogram',
            nbinsx = 30,
            marker = list(color = '#FFE500',
                          line = list(color = '#000000', width = 1))) %>%
      layout(
        xaxis = list(title = "Actual Score", color = '#FFFFFF'),
        yaxis = list(title = "Number of Lineups", color = '#FFFFFF'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        showlegend = FALSE,
        margin = list(t = 20)
      )
  })
  
  # Score by percentile
  output$score_by_percentile <- renderPlotly({
    req(filtered_sim_pool(), rv$data)
    
    data <- filtered_sim_pool()
    
    # Calculate what percentile each sim lineup would have finished at
    contest_scores <- sort(rv$data$Points, decreasing = TRUE)
    
    data[, ContestPercentile := sapply(ActualScore, function(score) {
      rank <- sum(contest_scores >= score)
      (rank / length(contest_scores)) * 100
    })]
    
    # Create bins
    data[, PercentileBin := cut(ContestPercentile,
                                breaks = c(0, 1, 5, 10, 20, 50, 100),
                                labels = c("Top 1%", "Top 5%", "Top 10%", "Top 20%", "Top 50%", "50%+"),
                                include.lowest = TRUE)]
    
    percentile_counts <- data[, .N, by = PercentileBin]
    percentile_counts[, Percentage := round(N / sum(N) * 100, 1)]
    
    plot_ly(percentile_counts,
            x = ~PercentileBin,
            y = ~N,
            type = 'bar',
            marker = list(color = '#FFE500',
                          line = list(color = '#000000', width = 1)),
            text = ~paste0(N, " (", Percentage, "%)"),
            textposition = 'outside',
            hoverinfo = 'text',
            hovertext = ~paste("Finish:", PercentileBin, "<br>",
                               "Lineups:", N, "<br>",
                               "Percentage:", Percentage, "%")) %>%
      layout(
        xaxis = list(title = "Contest Finish", color = '#FFFFFF'),
        yaxis = list(title = "Number of Lineups", color = '#FFFFFF'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        showlegend = FALSE,
        margin = list(t = 20)
      )
  })
  
  # Correlation summary table
  output$correlation_summary_table <- renderDT({
    req(filtered_sim_pool())
    
    data <- filtered_sim_pool()
    
    # Calculate correlations with ActualScore
    cor_cols <- c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct",
                  "CumulativeOwnership", "GeometricMeanOwnership", 
                  "TotalSalary", "CumulativeStarting", "GeometricMeanStarting")
    cor_cols <- intersect(cor_cols, names(data))
    
    correlations <- sapply(cor_cols, function(col) {
      cor(data[[col]], data$ActualScore, use = "pairwise.complete.obs")
    })
    
    # Calculate R-squared
    r_squared <- sapply(cor_cols, function(col) {
      model <- lm(ActualScore ~ get(col), data = data)
      summary(model)$r.squared
    })
    
    cor_table <- data.table(
      Metric = names(correlations),
      Correlation = round(correlations, 4),
      `R-Squared` = round(r_squared, 4)
    )[order(-abs(Correlation))]
    
    # Add interpretation
    cor_table[, Strength := ifelse(abs(Correlation) > 0.7, "Strong",
                                   ifelse(abs(Correlation) > 0.4, "Moderate", "Weak"))]
    
    datatable(
      cor_table,
      rownames = FALSE,
      options = list(pageLength = 15, dom = 't')
    ) %>%
      formatStyle(
        'Correlation',
        background = styleColorBar(c(-1, 1), '#FFE500'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Strength',
        backgroundColor = styleEqual(
          c("Strong", "Moderate", "Weak"),
          c('#a3e4d7', '#FFE500', '#f5b7b1')
        )
      )
  })
  
  # Sim lineups table
  output$sim_lineups_table <- renderDT({
    req(filtered_sim_pool())
    
    data <- filtered_sim_pool()
    
    # Select columns to display
    player_cols <- grep("^(Player|Driver|Fighter|Golfer)[0-9]$", names(data), value = TRUE)
    
    metric_cols <- c("SimRank", "WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct",
                     "CumulativeOwnership", "GeometricMeanOwnership",
                     "TotalSalary", "CumulativeStarting", "GeometricMeanStarting",
                     "ActualScore", "InContest", 
                     "TimesPlayed", "PlayedBy")
    metric_cols <- intersect(metric_cols, names(data))
    
    display_cols <- c(player_cols, metric_cols)
    display_data <- data[order(SimRank), ..display_cols]  # Sort by SimRank (lower = better)
    
    datatable(
      display_data,
      rownames = FALSE,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons'
    ) %>%
      formatRound(c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct"), 2) %>%
      formatRound(c("ActualScore", "GeometricMeanStarting"), 2) %>%
      formatStyle(
        'SimRank',
        background = styleColorBar(c(max(display_data$SimRank, na.rm = TRUE), 1), '#a5d6a7'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'ActualScore',
        background = styleColorBar(range(display_data$ActualScore, na.rm = TRUE), '#FFE500'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'InContest',
        backgroundColor = styleEqual(c(TRUE, FALSE), c('#a3e4d7', '#f5b7b1'))
      )
  })
  
  # Win Rate scatter
  output$winrate_scatter <- renderPlotly({
    req(filtered_sim_pool(), "WinRate" %in% names(filtered_sim_pool()))
    
    data <- filtered_sim_pool()
    
    # Calculate trendline
    model <- lm(ActualScore ~ WinRate, data = data)
    data$fitted <- predict(model)
    
    plot_ly(data, 
            x = ~WinRate, 
            y = ~ActualScore,
            color = ~InContest,
            colors = c("#888888", "#FFE500"),
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 8, opacity = 0.6),
            text = ~paste("Win Rate:", round(WinRate, 2), "%<br>",
                          "Actual Score:", round(ActualScore, 2)),
            hoverinfo = 'text',
            name = ~ifelse(InContest, "Played", "Not Played")) %>%
      add_trace(
        x = ~WinRate,
        y = ~fitted,
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#FFE500', width = 2, dash = 'dash'),
        name = 'Trendline',
        hoverinfo = 'skip',
        showlegend = TRUE,
        inherit = FALSE
      ) %>%
      layout(
        title = list(text = sprintf("Win Rate vs Actual Score (R² = %.3f)", 
                                    summary(model)$r.squared),
                     font = list(color = '#FFE500')),
        xaxis = list(title = "Win Rate %", color = '#FFFFFF'),
        yaxis = list(title = "Actual Score", color = '#FFFFFF'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        showlegend = TRUE,
        legend = list(title = list(text = ""))
      )
  })
  
  # Win Rate buckets
  output$winrate_buckets <- renderDT({
    req(filtered_sim_pool(), "WinRate" %in% names(filtered_sim_pool()))
    
    data <- copy(filtered_sim_pool())
    
    # Create buckets
    data[, WinRateBucket := cut(WinRate, 
                                breaks = c(0, 2, 5, 10, 20, 100),
                                labels = c("0-2%", "2-5%", "5-10%", "10-20%", "20%+"),
                                include.lowest = TRUE)]
    
    # Calculate stats by bucket
    bucket_stats <- data[, .(
      Count = .N,
      `Avg Actual Score` = round(mean(ActualScore, na.rm = TRUE), 2),
      `Max Actual Score` = round(max(ActualScore, na.rm = TRUE), 2),
      `Min Actual Score` = round(min(ActualScore, na.rm = TRUE), 2),
      `Played Count` = sum(InContest, na.rm = TRUE),
      `Play Rate %` = round(sum(InContest, na.rm = TRUE) / .N * 100, 1)
    ), by = WinRateBucket][order(WinRateBucket)]
    
    datatable(
      bucket_stats,
      rownames = FALSE,
      options = list(pageLength = 10, dom = 't')
    ) %>%
      formatStyle(
        'Avg Actual Score',
        background = styleColorBar(range(bucket_stats$`Avg Actual Score`), '#FFE500'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Top 1% scatter
  output$top1_scatter <- renderPlotly({
    req(filtered_sim_pool(), "Top1Pct" %in% names(filtered_sim_pool()))
    
    data <- filtered_sim_pool()
    
    # Calculate trendline
    model <- lm(ActualScore ~ Top1Pct, data = data)
    data$fitted <- predict(model)
    
    plot_ly(data, 
            x = ~Top1Pct, 
            y = ~ActualScore,
            color = ~InContest,
            colors = c("#888888", "#FFE500"),
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 8, opacity = 0.6),
            text = ~paste("Top 1%:", round(Top1Pct, 2), "%<br>",
                          "Actual Score:", round(ActualScore, 2)),
            hoverinfo = 'text',
            name = ~ifelse(InContest, "Played", "Not Played")) %>%
      add_trace(
        x = ~Top1Pct,
        y = ~fitted,
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#FFE500', width = 2, dash = 'dash'),
        name = 'Trendline',
        hoverinfo = 'skip',
        showlegend = TRUE,
        inherit = FALSE
      ) %>%
      layout(
        title = list(text = sprintf("Top 1%% vs Actual Score (R² = %.3f)", 
                                    summary(model)$r.squared),
                     font = list(color = '#FFE500')),
        xaxis = list(title = "Top 1% Probability", color = '#FFFFFF'),
        yaxis = list(title = "Actual Score", color = '#FFFFFF'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        showlegend = TRUE
      )
  })
  
  output$top1_buckets <- renderDT({
    req(filtered_sim_pool(), "Top1Pct" %in% names(filtered_sim_pool()))
    
    data <- copy(filtered_sim_pool())
    
    data[, Top1Bucket := cut(Top1Pct, 
                             breaks = c(0, 1, 3, 5, 10, 100),
                             labels = c("0-1%", "1-3%", "3-5%", "5-10%", "10%+"),
                             include.lowest = TRUE)]
    
    bucket_stats <- data[, .(
      Count = .N,
      `Avg Actual Score` = round(mean(ActualScore, na.rm = TRUE), 2),
      `Max Actual Score` = round(max(ActualScore, na.rm = TRUE), 2),
      `Played Count` = sum(InContest, na.rm = TRUE),
      `Play Rate %` = round(sum(InContest, na.rm = TRUE) / .N * 100, 1)
    ), by = Top1Bucket][order(Top1Bucket)]
    
    datatable(
      bucket_stats,
      rownames = FALSE,
      options = list(pageLength = 10, dom = 't')
    ) %>%
      formatStyle(
        'Avg Actual Score',
        background = styleColorBar(range(bucket_stats$`Avg Actual Score`), '#FFE500'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Top 5% scatter
  output$top5_scatter <- renderPlotly({
    req(filtered_sim_pool(), "Top5Pct" %in% names(filtered_sim_pool()))
    
    data <- filtered_sim_pool()
    
    # Calculate trendline
    model <- lm(ActualScore ~ Top5Pct, data = data)
    data$fitted <- predict(model)
    
    plot_ly(data, 
            x = ~Top5Pct, 
            y = ~ActualScore,
            color = ~InContest,
            colors = c("#888888", "#FFE500"),
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 8, opacity = 0.6),
            text = ~paste("Top 5%:", round(Top5Pct, 2), "%<br>",
                          "Actual Score:", round(ActualScore, 2)),
            hoverinfo = 'text',
            name = ~ifelse(InContest, "Played", "Not Played")) %>%
      add_trace(
        x = ~Top5Pct,
        y = ~fitted,
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#FFE500', width = 2, dash = 'dash'),
        name = 'Trendline',
        hoverinfo = 'skip',
        showlegend = TRUE,
        inherit = FALSE
      ) %>%
      layout(
        title = list(text = sprintf("Top 5%% vs Actual Score (R² = %.3f)", 
                                    summary(model)$r.squared),
                     font = list(color = '#FFE500')),
        xaxis = list(title = "Top 5% Probability", color = '#FFFFFF'),
        yaxis = list(title = "Actual Score", color = '#FFFFFF'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        showlegend = TRUE
      )
  })
  
  output$top5_buckets <- renderDT({
    req(filtered_sim_pool(), "Top5Pct" %in% names(filtered_sim_pool()))
    
    data <- copy(filtered_sim_pool())
    
    data[, Top5Bucket := cut(Top5Pct, 
                             breaks = c(0, 5, 10, 20, 40, 100),
                             labels = c("0-5%", "5-10%", "10-20%", "20-40%", "40%+"),
                             include.lowest = TRUE)]
    
    bucket_stats <- data[, .(
      Count = .N,
      `Avg Actual Score` = round(mean(ActualScore, na.rm = TRUE), 2),
      `Max Actual Score` = round(max(ActualScore, na.rm = TRUE), 2),
      `Played Count` = sum(InContest, na.rm = TRUE),
      `Play Rate %` = round(sum(InContest, na.rm = TRUE) / .N * 100, 1)
    ), by = Top5Bucket][order(Top5Bucket)]
    
    datatable(
      bucket_stats,
      rownames = FALSE,
      options = list(pageLength = 10, dom = 't')
    ) %>%
      formatStyle(
        'Avg Actual Score',
        background = styleColorBar(range(bucket_stats$`Avg Actual Score`), '#FFE500'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Ownership scatter
  output$ownership_scatter <- renderPlotly({
    req(filtered_sim_pool(), "CumulativeOwnership" %in% names(filtered_sim_pool()))
    
    data <- filtered_sim_pool()
    
    # Calculate trendline
    model <- lm(ActualScore ~ CumulativeOwnership, data = data)
    data$fitted <- predict(model)
    
    plot_ly(data, 
            x = ~CumulativeOwnership, 
            y = ~ActualScore,
            color = ~InContest,
            colors = c("#888888", "#FFE500"),
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 8, opacity = 0.6),
            text = ~paste("Ownership:", round(CumulativeOwnership, 1), "%<br>",
                          "Actual Score:", round(ActualScore, 2)),
            hoverinfo = 'text',
            name = ~ifelse(InContest, "Played", "Not Played")) %>%
      add_trace(
        x = ~CumulativeOwnership,
        y = ~fitted,
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#FFE500', width = 2, dash = 'dash'),
        name = 'Trendline',
        hoverinfo = 'skip',
        showlegend = TRUE,
        inherit = FALSE
      ) %>%
      layout(
        title = list(text = sprintf("Cumulative Ownership vs Actual Score (R² = %.3f)", 
                                    summary(model)$r.squared),
                     font = list(color = '#FFE500')),
        xaxis = list(title = "Cumulative Ownership %", color = '#FFFFFF'),
        yaxis = list(title = "Actual Score", color = '#FFFFFF'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        showlegend = TRUE
      )
  })
  
  output$ownership_buckets <- renderDT({
    req(filtered_sim_pool(), "CumulativeOwnership" %in% names(filtered_sim_pool()))
    
    data <- copy(filtered_sim_pool())
    
    data[, OwnBucket := cut(CumulativeOwnership, 
                            breaks = c(0, 100, 150, 200, 300, 1000),
                            labels = c("0-100%", "100-150%", "150-200%", "200-300%", "300%+"),
                            include.lowest = TRUE)]
    
    bucket_stats <- data[, .(
      Count = .N,
      `Avg Actual Score` = round(mean(ActualScore, na.rm = TRUE), 2),
      `Max Actual Score` = round(max(ActualScore, na.rm = TRUE), 2),
      `Played Count` = sum(InContest, na.rm = TRUE),
      `Play Rate %` = round(sum(InContest, na.rm = TRUE) / .N * 100, 1)
    ), by = OwnBucket][order(OwnBucket)]
    
    datatable(
      bucket_stats,
      rownames = FALSE,
      options = list(pageLength = 10, dom = 't')
    ) %>%
      formatStyle(
        'Avg Actual Score',
        background = styleColorBar(range(bucket_stats$`Avg Actual Score`), '#FFE500'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Geometric Ownership scatter
  output$geo_ownership_scatter <- renderPlotly({
    req(filtered_sim_pool(), "GeometricMeanOwnership" %in% names(filtered_sim_pool()))
    
    data <- filtered_sim_pool()
    
    # Calculate trendline
    model <- lm(ActualScore ~ GeometricMeanOwnership, data = data)
    data$fitted <- predict(model)
    
    plot_ly(data, 
            x = ~GeometricMeanOwnership, 
            y = ~ActualScore,
            color = ~InContest,
            colors = c("#888888", "#FFE500"),
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 8, opacity = 0.6),
            text = ~paste("Geo Mean Ownership:", round(GeometricMeanOwnership, 1), "%<br>",
                          "Actual Score:", round(ActualScore, 2)),
            hoverinfo = 'text',
            name = ~ifelse(InContest, "Played", "Not Played")) %>%
      add_trace(
        x = ~GeometricMeanOwnership,
        y = ~fitted,
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#FFE500', width = 2, dash = 'dash'),
        name = 'Trendline',
        hoverinfo = 'skip',
        showlegend = TRUE,
        inherit = FALSE
      ) %>%
      layout(
        title = list(text = sprintf("Geometric Mean Ownership vs Actual Score (R² = %.3f)", 
                                    summary(model)$r.squared),
                     font = list(color = '#FFE500')),
        xaxis = list(title = "Geometric Mean Ownership %", color = '#FFFFFF'),
        yaxis = list(title = "Actual Score", color = '#FFFFFF'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        showlegend = TRUE
      )
  })
  
  output$geo_ownership_buckets <- renderDT({
    req(filtered_sim_pool(), "GeometricMeanOwnership" %in% names(filtered_sim_pool()))
    
    data <- copy(filtered_sim_pool())
    
    data[, GeoOwnBucket := cut(GeometricMeanOwnership, 
                               breaks = c(0, 20, 30, 40, 50, 100),
                               labels = c("0-20%", "20-30%", "30-40%", "40-50%", "50%+"),
                               include.lowest = TRUE)]
    
    bucket_stats <- data[, .(
      Count = .N,
      `Avg Actual Score` = round(mean(ActualScore, na.rm = TRUE), 2),
      `Max Actual Score` = round(max(ActualScore, na.rm = TRUE), 2),
      `Played Count` = sum(InContest, na.rm = TRUE),
      `Play Rate %` = round(sum(InContest, na.rm = TRUE) / .N * 100, 1)
    ), by = GeoOwnBucket][order(GeoOwnBucket)]
    
    datatable(
      bucket_stats,
      rownames = FALSE,
      options = list(pageLength = 10, dom = 't')
    ) %>%
      formatStyle(
        'Avg Actual Score',
        background = styleColorBar(range(bucket_stats$`Avg Actual Score`), '#FFE500'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Total Start scatter
  output$total_start_scatter <- renderPlotly({
    req(filtered_sim_pool(), "CumulativeStarting" %in% names(filtered_sim_pool()))
    
    data <- filtered_sim_pool()
    
    # Calculate trendline
    model <- lm(ActualScore ~ CumulativeStarting, data = data)
    data$fitted <- predict(model)
    
    plot_ly(data, 
            x = ~CumulativeStarting, 
            y = ~ActualScore,
            color = ~InContest,
            colors = c("#888888", "#FFE500"),
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 8, opacity = 0.6),
            text = ~paste("Total Start:", CumulativeStarting, "<br>",
                          "Actual Score:", round(ActualScore, 2)),
            hoverinfo = 'text',
            name = ~ifelse(InContest, "Played", "Not Played")) %>%
      add_trace(
        x = ~CumulativeStarting,
        y = ~fitted,
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#FFE500', width = 2, dash = 'dash'),
        name = 'Trendline',
        hoverinfo = 'skip',
        showlegend = TRUE,
        inherit = FALSE
      ) %>%
      layout(
        title = list(text = sprintf("Total Starting Position vs Actual Score (R² = %.3f)", 
                                    summary(model)$r.squared),
                     font = list(color = '#FFE500')),
        xaxis = list(title = "Total Starting Position (Lower = Better)", color = '#FFFFFF'),
        yaxis = list(title = "Actual Score", color = '#FFFFFF'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        showlegend = TRUE
      )
  })
  
  output$total_start_buckets <- renderDT({
    req(filtered_sim_pool(), "CumulativeStarting" %in% names(filtered_sim_pool()))
    
    data <- copy(filtered_sim_pool())
    
    data[, StartBucket := cut(CumulativeStarting, 
                              breaks = c(0, 150, 180, 210, 240, 500),
                              labels = c("0-150", "150-180", "180-210", "210-240", "240+"),
                              include.lowest = TRUE)]
    
    bucket_stats <- data[, .(
      Count = .N,
      `Avg Actual Score` = round(mean(ActualScore, na.rm = TRUE), 2),
      `Max Actual Score` = round(max(ActualScore, na.rm = TRUE), 2),
      `Played Count` = sum(InContest, na.rm = TRUE),
      `Play Rate %` = round(sum(InContest, na.rm = TRUE) / .N * 100, 1)
    ), by = StartBucket][order(StartBucket)]
    
    datatable(
      bucket_stats,
      rownames = FALSE,
      options = list(pageLength = 10, dom = 't')
    ) %>%
      formatStyle(
        'Avg Actual Score',
        background = styleColorBar(range(bucket_stats$`Avg Actual Score`), '#FFE500'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Avg Start scatter
  output$avg_start_scatter <- renderPlotly({
    req(filtered_sim_pool(), "GeometricMeanStarting" %in% names(filtered_sim_pool()))
    
    data <- filtered_sim_pool()
    
    # Calculate trendline
    model <- lm(ActualScore ~ GeometricMeanStarting, data = data)
    data$fitted <- predict(model)
    
    plot_ly(data, 
            x = ~GeometricMeanStarting, 
            y = ~ActualScore,
            color = ~InContest,
            colors = c("#888888", "#FFE500"),
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 8, opacity = 0.6),
            text = ~paste("Avg Start:", round(GeometricMeanStarting, 1), "<br>",
                          "Actual Score:", round(ActualScore, 2)),
            hoverinfo = 'text',
            name = ~ifelse(InContest, "Played", "Not Played")) %>%
      add_trace(
        x = ~GeometricMeanStarting,
        y = ~fitted,
        type = 'scatter',
        mode = 'lines',
        line = list(color = '#FFE500', width = 2, dash = 'dash'),
        name = 'Trendline',
        hoverinfo = 'skip',
        showlegend = TRUE,
        inherit = FALSE
      ) %>%
      layout(
        title = list(text = sprintf("Average Starting Position vs Actual Score (R² = %.3f)", 
                                    summary(model)$r.squared),
                     font = list(color = '#FFE500')),
        xaxis = list(title = "Average Starting Position (Lower = Better)", color = '#FFFFFF'),
        yaxis = list(title = "Actual Score", color = '#FFFFFF'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        showlegend = TRUE
      )
  })
  
  output$avg_start_buckets <- renderDT({
    req(filtered_sim_pool(), "GeometricMeanStarting" %in% names(filtered_sim_pool()))
    
    data <- copy(filtered_sim_pool())
    
    data[, GeometricMeanStartingBucket := cut(GeometricMeanStarting, 
                                              breaks = c(0, 25, 30, 35, 40, 50),
                                              labels = c("0-25", "25-30", "30-35", "35-40", "40+"),
                                              include.lowest = TRUE)]
    
    bucket_stats <- data[, .(
      Count = .N,
      `Avg Actual Score` = round(mean(ActualScore, na.rm = TRUE), 2),
      `Max Actual Score` = round(max(ActualScore, na.rm = TRUE), 2),
      `Played Count` = sum(InContest, na.rm = TRUE),
      `Play Rate %` = round(sum(InContest, na.rm = TRUE) / .N * 100, 1)
    ), by = GeometricMeanStartingBucket][order(GeometricMeanStartingBucket)]
    
    datatable(
      bucket_stats,
      rownames = FALSE,
      options = list(pageLength = 10, dom = 't')
    ) %>%
      formatStyle(
        'Avg Actual Score',
        background = styleColorBar(range(bucket_stats$`Avg Actual Score`), '#FFE500'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # ============================================================================
  # OWNERSHIP VS DUPES TAB OUTPUTS
  # ============================================================================
  
  output$ownership_dupe_scatter <- renderPlotly({
    req(rv$unified_pool)
    
    data <- rv$unified_pool[InSim == TRUE & InContest == TRUE]
    
    if (nrow(data) < 2) {
      return(plot_ly() %>% layout(
        title = list(text = "Not enough played lineups", font = list(color = '#FFE500')),
        paper_bgcolor = '#000000', plot_bgcolor = '#1a1a1a'
      ))
    }
    
    plot_ly(data, x = ~CumulativeOwnership, y = ~ActualOwnership,
            type = 'scatter', mode = 'markers',
            marker = list(size = 10, color = '#FFE500', opacity = 0.7,
                          line = list(color = '#000000', width = 1)),
            text = ~paste("Predicted Own:", round(CumulativeOwnership, 1), "%<br>",
                          "Times Played:", ActualOwnership, "<br>",
                          "Score:", round(ActualScore, 1)),
            hoverinfo = 'text') %>%
      layout(
        title = list(text = "Predicted Ownership % vs Times Actually Played", 
                     font = list(color = '#FFE500', size = 18)),
        xaxis = list(title = "Predicted Ownership %", color = '#FFFFFF', gridcolor = '#333333'),
        yaxis = list(title = "Times Played in Contest", color = '#FFFFFF', gridcolor = '#333333'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF', size = 14)
      )
  })
  
  output$ownership_dupe_table <- renderDT({
    req(rv$unified_pool)
    
    data <- rv$unified_pool[InSim == TRUE & InContest == TRUE]
    
    if (nrow(data) == 0) {
      return(datatable(data.table(Message = "No played lineups"), rownames = FALSE))
    }
    
    # Create ownership buckets
    data[, OwnBucket := cut(CumulativeOwnership, 
                            breaks = c(0, 50, 100, 150, 200, 1000),
                            labels = c("0-50%", "50-100%", "100-150%", "150-200%", "200%+"),
                            include.lowest = TRUE)]
    
    bucket_stats <- data[, .(
      `Lineups` = .N,
      `Avg Predicted Own %` = round(mean(CumulativeOwnership, na.rm = TRUE), 1),
      `Avg Times Played` = round(mean(ActualOwnership, na.rm = TRUE), 2),
      `Max ActualOwnership` = max(ActualOwnership, na.rm = TRUE),
      `Single Entry %` = round(sum(ActualOwnership == 1) / .N * 100, 1),
      `5+ ActualOwnership %` = round(sum(ActualOwnership >= 5) / .N * 100, 1),
      `10+ ActualOwnership %` = round(sum(ActualOwnership >= 10) / .N * 100, 1)
    ), by = OwnBucket]
    
    datatable(bucket_stats, rownames = FALSE,
              options = list(dom = 't', pageLength = 10)) %>%
      formatStyle('Avg Times Played',
                  background = styleColorBar(range(bucket_stats$`Avg Times Played`), '#FFE500'),
                  backgroundSize = '100% 90%', backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  # ============================================================================
  # ALL SIM LINEUPS TAB OUTPUTS
  # ============================================================================
  
  output$all_sim_lineups_table <- renderDT({
    req(rv$unified_pool)
    
    data <- rv$unified_pool[InSim == TRUE]
    
    player_cols <- grep("^(Fighter|Player|Driver)[0-9]$", names(data), value = TRUE)
    
    display_cols <- c(player_cols, "WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct",
                      "CumulativeOwnership", "GeometricMeanOwnership", 
                      "ActualScore", "InContest", "ActualOwnership")
    display_cols <- intersect(display_cols, names(data))
    
    # Rename columns for display
    col_names <- display_cols
    col_names <- gsub("CumulativeOwnership", "TotalOwn", col_names)
    col_names <- gsub("GeometricMeanOwnership", "AvgOwn", col_names)
    col_names <- gsub("ActualOwnership", "Dupes", col_names)
    
    datatable(data[, ..display_cols],
              colnames = col_names,
              options = list(pageLength = 50, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct", 
                    "CumulativeOwnership", "GeometricMeanOwnership"), 1) %>%
      formatRound("ActualScore", 1) %>%
      formatStyle('InContest',
                  backgroundColor = styleEqual(c(TRUE, FALSE), c('#2ecc71', '#34495e')))
  })
  
  output$download_all_sim <- downloadHandler(
    filename = function() {
      paste0("all_sim_lineups_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$unified_pool[InSim == TRUE], file, row.names = FALSE)
    }
  )
  
  # ============================================================================
  # SIM RESULTS GRAPHS TAB OUTPUTS
  # ============================================================================
  
  output$sim_score_distribution <- renderPlotly({
    req(rv$unified_pool)
    
    data <- rv$unified_pool[InSim == TRUE]
    
    plot_ly(data, x = ~ActualScore, type = 'histogram', nbinsx = 50,
            marker = list(color = '#FFE500', line = list(color = '#000000', width = 1))) %>%
      layout(
        title = list(text = "Sim Lineup Score Distribution", font = list(color = '#FFE500')),
        xaxis = list(title = "Actual Score", color = '#FFFFFF', gridcolor = '#333333'),
        yaxis = list(title = "Count", color = '#FFFFFF', gridcolor = '#333333'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a'
      )
  })
  
  output$sim_finish_rates <- renderPlotly({
    req(rv$unified_pool, rv$data)
    
    sim_data <- rv$unified_pool[InSim == TRUE]
    total <- nrow(sim_data)
    
    winning_score <- max(rv$data$Points, na.rm = TRUE)
    top1 <- quantile(rv$data$Points, 0.99, na.rm = TRUE)
    top5 <- quantile(rv$data$Points, 0.95, na.rm = TRUE)
    top10 <- quantile(rv$data$Points, 0.90, na.rm = TRUE)
    top20 <- quantile(rv$data$Points, 0.80, na.rm = TRUE)
    
    finish_data <- data.table(
      Finish = c("Win", "Top 1%", "Top 5%", "Top 10%", "Top 20%"),
      Percentage = c(
        sum(sim_data$ActualScore >= winning_score, na.rm = TRUE) / total * 100,
        sum(sim_data$ActualScore >= top1, na.rm = TRUE) / total * 100,
        sum(sim_data$ActualScore >= top5, na.rm = TRUE) / total * 100,
        sum(sim_data$ActualScore >= top10, na.rm = TRUE) / total * 100,
        sum(sim_data$ActualScore >= top20, na.rm = TRUE) / total * 100
      )
    )
    finish_data[, Finish := factor(Finish, levels = c("Win", "Top 1%", "Top 5%", "Top 10%", "Top 20%"))]
    
    plot_ly(finish_data, x = ~Finish, y = ~Percentage, type = 'bar',
            marker = list(color = '#FFE500', line = list(color = '#000000', width = 1)),
            text = ~paste0(round(Percentage, 1), "%"), textposition = 'outside') %>%
      layout(
        title = list(text = "Sim Lineups by Finish Threshold", font = list(color = '#FFE500')),
        xaxis = list(title = "", color = '#FFFFFF'),
        yaxis = list(title = "Percentage", color = '#FFFFFF', gridcolor = '#333333'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a'
      )
  })
  
  output$sim_ownership_dist <- renderPlotly({
    req(rv$unified_pool)
    
    data <- rv$unified_pool[InSim == TRUE]
    
    plot_ly(data, x = ~CumulativeOwnership, type = 'histogram', nbinsx = 40,
            marker = list(color = '#FFE500', line = list(color = '#000000', width = 1))) %>%
      layout(
        title = list(text = "Predicted Ownership Distribution", font = list(color = '#FFE500')),
        xaxis = list(title = "Cumulative Ownership %", color = '#FFFFFF', gridcolor = '#333333'),
        yaxis = list(title = "Count", color = '#FFFFFF', gridcolor = '#333333'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a'
      )
  })
  
  output$sim_played_vs_not <- renderPlotly({
    req(rv$unified_pool)
    
    data <- rv$unified_pool[InSim == TRUE]
    
    plot_ly() %>%
      add_trace(data = data[InContest == TRUE], x = ~ActualScore, type = 'histogram',
                name = 'Played in Contest', nbinsx = 30,
                marker = list(color = '#FFE500', opacity = 0.7)) %>%
      add_trace(data = data[InContest == FALSE], x = ~ActualScore, type = 'histogram',
                name = 'Not Played', nbinsx = 30,
                marker = list(color = '#666666', opacity = 0.7)) %>%
      layout(
        title = list(text = "Score Distribution: Played vs Not Played", font = list(color = '#FFE500')),
        xaxis = list(title = "Actual Score", color = '#FFFFFF', gridcolor = '#333333'),
        yaxis = list(title = "Count", color = '#FFFFFF', gridcolor = '#333333'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        barmode = 'overlay'
      )
  })
  
  # ============================================================================
  # CONTEST TAB - VIOLIN PLOT
  # ============================================================================
  
  output$contest_violin_plot <- renderPlotly({
    req(rv$unified_pool, rv$data)
    
    # Create plot data from sim lineups
    sim_played <- copy(rv$unified_pool[InContest == TRUE])
    sim_played[, Group := "Sim (Played)"]
    
    sim_not_played <- copy(rv$unified_pool[InContest == FALSE])
    sim_not_played[, Group := "Sim (Not Played)"]
    
    # Add contest-only if available
    plot_list <- list(sim_played, sim_not_played)
    
    if (!is.null(rv$contest_only) && nrow(rv$contest_only) > 0) {
      contest_only_plot <- copy(rv$contest_only)
      contest_only_plot[, Group := "Contest Only"]
      plot_list <- c(plot_list, list(contest_only_plot))
    }
    
    # Combine
    plot_data <- rbindlist(plot_list, use.names = TRUE, fill = TRUE)
    
    # Order groups
    plot_data[, Group := factor(Group, levels = c("Sim (Played)", "Sim (Not Played)", "Contest Only"))]
    
    plot_ly(plot_data, x = ~Group, y = ~ActualScore, 
            type = 'violin',
            box = list(visible = TRUE),
            meanline = list(visible = TRUE),
            points = 'all',
            jitter = 0.3,
            pointpos = 0,
            color = ~Group,
            colors = c("Sim (Played)" = "#FFE500", 
                       "Sim (Not Played)" = "#666666", 
                       "Contest Only" = "#FF0000"),
            marker = list(size = 4, opacity = 0.5)) %>%
      layout(
        title = list(text = "Score Distribution by Lineup Category",
                     font = list(color = '#FFE500', size = 18)),
        xaxis = list(title = "", color = '#FFFFFF'),
        yaxis = list(title = "Actual Score", color = '#FFFFFF', gridcolor = '#333333'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        showlegend = FALSE
      )
  })
  
  # ============================================================================
  # CONTEST TAB - CONTEST-ONLY STATS
  # ============================================================================
  
  output$contest_only_count <- renderText({
    req(rv$contest_only)
    count <- nrow(rv$contest_only)
    paste0(count, " lineups were played in the contest but were NOT in your simulation")
  })
  
  output$contest_only_total <- renderText({
    req(rv$contest_only)
    as.character(format(nrow(rv$contest_only), big.mark = ","))
  })
  
  output$contest_only_avg <- renderText({
    req(rv$contest_only)
    round(mean(rv$contest_only$ActualScore, na.rm = TRUE), 1)
  })
  
  output$contest_only_best <- renderText({
    req(rv$contest_only)
    round(max(rv$contest_only$ActualScore, na.rm = TRUE), 1)
  })
  
  output$contest_only_win <- renderText({
    req(rv$contest_only, rv$data)
    contest_only <- rv$contest_only
    winning_score <- max(rv$data$Points, na.rm = TRUE)
    wins <- sum(contest_only$ActualScore >= winning_score, na.rm = TRUE)
    as.character(format(wins, big.mark = ","))  # Raw count
  })
  
  output$contest_only_score_dist <- renderPlotly({
    req(rv$unified_pool)
    
    contest_only <- rv$contest_only
    sim_only <- rv$unified_pool[InSim == TRUE]
    
    plot_ly() %>%
      add_trace(data = sim_only, x = ~ActualScore, type = 'histogram',
                name = 'Sim Lineups', nbinsx = 40,
                marker = list(color = '#FFE500', opacity = 0.6)) %>%
      add_trace(data = contest_only, x = ~ActualScore, type = 'histogram',
                name = 'Contest-Only', nbinsx = 40,
                marker = list(color = '#FF0000', opacity = 0.6)) %>%
      layout(
        title = list(text = "Score Comparison: Sim vs Contest-Only", font = list(color = '#FFE500', size = 18)),
        xaxis = list(title = "Actual Score", color = '#FFFFFF', gridcolor = '#333333'),
        yaxis = list(title = "Count", color = '#FFFFFF', gridcolor = '#333333'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        barmode = 'overlay'
      )
  })
  
  output$contest_only_table <- renderDT({
    req(rv$unified_pool)
    
    data <- rv$contest_only[order(-ActualScore)]
    
    player_cols <- grep("^(Fighter|Player|Driver)[0-9]$", names(data), value = TRUE)
    
    display_cols <- c(player_cols, "ActualScore", "ActualOwnership")
    
    # Rename columns for display
    col_names <- display_cols
    col_names <- gsub("ActualOwnership", "Dupes", col_names)
    display_cols <- intersect(display_cols, names(data))
    
    datatable(data[, ..display_cols], 
              colnames = col_names,
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound("ActualScore", 1)
  })
  
  # ============================================================================
  # FILTER REVIEW TAB OUTPUTS
  # ============================================================================
  
  # Filter review reactive
  filter_review_pool <- reactive({
    req(rv$unified_pool)
    
    data <- copy(rv$unified_pool[InSim == TRUE])
    player_cols <- grep("^(Fighter|Player|Driver)[0-9]$", names(data), value = TRUE)
    
    # Player filters
    if (!is.null(input$filter_include_players) && length(input$filter_include_players) > 0) {
      for (player in input$filter_include_players) {
        data <- data[Reduce(`|`, lapply(.SD, function(x) x == player)), .SDcols = player_cols]
      }
    }
    
    if (!is.null(input$filter_exclude_players) && length(input$filter_exclude_players) > 0) {
      for (player in input$filter_exclude_players) {
        data <- data[!Reduce(`|`, lapply(.SD, function(x) x == player)), .SDcols = player_cols]
      }
    }
    
    # Min rate filters
    if (!is.null(input$filter_min_win) && input$filter_min_win > 0 && "WinRate" %in% names(data)) {
      data <- data[WinRate >= input$filter_min_win]
    }
    if (!is.null(input$filter_min_top1) && input$filter_min_top1 > 0 && "Top1Pct" %in% names(data)) {
      data <- data[Top1Pct >= input$filter_min_top1]
    }
    if (!is.null(input$filter_min_top5) && input$filter_min_top5 > 0 && "Top5Pct" %in% names(data)) {
      data <- data[Top5Pct >= input$filter_min_top5]
    }
    if (!is.null(input$filter_min_top10) && input$filter_min_top10 > 0 && "Top10Pct" %in% names(data)) {
      data <- data[Top10Pct >= input$filter_min_top10]
    }
    if (!is.null(input$filter_min_top20) && input$filter_min_top20 > 0 && "Top20Pct" %in% names(data)) {
      data <- data[Top20Pct >= input$filter_min_top20]
    }
    
    # Range filters
    if (!is.null(input$filter_salary) && "TotalSalary" %in% names(data)) {
      data <- data[TotalSalary >= (input$filter_salary[1] * 1000) & TotalSalary <= (input$filter_salary[2] * 1000)]
    }
    if (!is.null(input$filter_totalown) && "CumulativeOwnership" %in% names(data)) {
      data <- data[CumulativeOwnership >= input$filter_totalown[1] & CumulativeOwnership <= input$filter_totalown[2]]
    }
    if (!is.null(input$filter_avgown) && "GeometricMeanOwnership" %in% names(data)) {
      data <- data[GeometricMeanOwnership >= input$filter_avgown[1] & GeometricMeanOwnership <= input$filter_avgown[2]]
    }
    if (!is.null(input$filter_totalstart) && "CumulativeStarting" %in% names(data)) {
      data <- data[CumulativeStarting >= input$filter_totalstart[1] & CumulativeStarting <= input$filter_totalstart[2]]
    }
    if (!is.null(input$filter_avgstart) && "GeometricMeanStarting" %in% names(data)) {
      data <- data[GeometricMeanStarting >= input$filter_avgstart[1] & GeometricMeanStarting <= input$filter_avgstart[2]]
    }
    
    return(data)
  }) %>% debounce(300)
  
  # Filter outputs
  output$filter_pool_count <- renderText({
    req(filter_review_pool())
    as.character(format(nrow(filter_review_pool()), big.mark = ","))
  })
  
  output$filter_played_count <- renderText({
    req(filter_review_pool())
    played <- sum(filter_review_pool()$InContest, na.rm = TRUE)
    as.character(format(played, big.mark = ","))
  })
  
  output$filter_would_win <- renderText({
    req(filter_review_pool(), rv$data)
    winning_score <- max(rv$data$Points, na.rm = TRUE)
    wins <- sum(filter_review_pool()$ActualScore >= winning_score, na.rm = TRUE)
    as.character(format(wins, big.mark = ","))  # Raw count
  })
  
  output$filter_top1 <- renderText({
    req(filter_review_pool(), rv$data)
    threshold <- quantile(rv$data$Points, 0.99, na.rm = TRUE)
    count <- sum(filter_review_pool()$ActualScore >= threshold, na.rm = TRUE)
    paste0(round(count / nrow(filter_review_pool()) * 100, 1), "%")
  })
  
  output$filter_top5 <- renderText({
    req(filter_review_pool(), rv$data)
    threshold <- quantile(rv$data$Points, 0.95, na.rm = TRUE)
    count <- sum(filter_review_pool()$ActualScore >= threshold, na.rm = TRUE)
    paste0(round(count / nrow(filter_review_pool()) * 100, 1), "%")
  })
  
  output$filter_top10 <- renderText({
    req(filter_review_pool(), rv$data)
    threshold <- quantile(rv$data$Points, 0.90, na.rm = TRUE)
    count <- sum(filter_review_pool()$ActualScore >= threshold, na.rm = TRUE)
    paste0(round(count / nrow(filter_review_pool()) * 100, 1), "%")
  })
  
  output$filter_avg_score <- renderText({
    req(filter_review_pool())
    round(mean(filter_review_pool()$ActualScore, na.rm = TRUE), 1)
  })
  
  output$filter_lineups_table <- renderDT({
    req(filter_review_pool())
    
    data <- filter_review_pool()
    player_cols <- grep("^(Fighter|Player|Driver)[0-9]$", names(data), value = TRUE)
    
    display_cols <- c(player_cols, "WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct",
                      "ActualScore", "InContest")
    display_cols <- intersect(display_cols, names(data))
    
    # Rename columns for display
    col_names <- display_cols
    col_names <- gsub("CumulativeOwnership", "TotalOwn", col_names)
    col_names <- gsub("GeometricMeanOwnership", "AvgOwn", col_names)
    
    datatable(data[, ..display_cols],
              colnames = col_names,
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct"), 1) %>%
      formatRound("ActualScore", 1)
  })
  
  # Reset button
  observeEvent(input$reset_filter_review, {
    updateNumericInput(session, "filter_min_win", value = 0)
    updateNumericInput(session, "filter_min_top1", value = 0)
    updateNumericInput(session, "filter_min_top5", value = 0)
    updateNumericInput(session, "filter_min_top10", value = 0)
    updateNumericInput(session, "filter_min_top20", value = 0)
    
    updateSelectizeInput(session, "filter_include_players", selected = character(0))
    updateSelectizeInput(session, "filter_exclude_players", selected = character(0))
  })
  
  # Update player dropdowns and slider ranges when sim loads
  observe({
    req(rv$unified_pool)
    player_cols <- grep("^(Fighter|Player|Driver)[0-9]$", names(rv$unified_pool), value = TRUE)
    all_players <- sort(unique(unlist(rv$unified_pool[, player_cols, with = FALSE])))
    
    updateSelectizeInput(session, "filter_include_players", choices = all_players, server = TRUE)
    updateSelectizeInput(session, "filter_exclude_players", choices = all_players, server = TRUE)
    
    # Update slider ranges based on actual data
    if ("TotalSalary" %in% names(rv$unified_pool)) {
      rng <- range(rv$unified_pool$TotalSalary / 1000, na.rm = TRUE)
      updateSliderInput(session, "filter_salary", min = floor(rng[1]), max = ceiling(rng[2]), value = c(floor(rng[1]), ceiling(rng[2])))
    }
    if ("CumulativeOwnership" %in% names(rv$unified_pool)) {
      rng <- range(rv$unified_pool$CumulativeOwnership, na.rm = TRUE)
      updateSliderInput(session, "filter_totalown", min = floor(rng[1]), max = ceiling(rng[2]), value = c(floor(rng[1]), ceiling(rng[2])))
    }
    if ("GeometricMeanOwnership" %in% names(rv$unified_pool)) {
      rng <- range(rv$unified_pool$GeometricMeanOwnership, na.rm = TRUE)
      updateSliderInput(session, "filter_avgown", min = floor(rng[1]), max = ceiling(rng[2]), value = c(floor(rng[1]), ceiling(rng[2])))
    }
    if ("CumulativeStarting" %in% names(rv$unified_pool)) {
      rng <- range(rv$unified_pool$CumulativeStarting, na.rm = TRUE)
      updateSliderInput(session, "filter_totalstart", min = floor(rng[1]), max = ceiling(rng[2]), value = c(floor(rng[1]), ceiling(rng[2])))
    }
    if ("GeometricMeanStarting" %in% names(rv$unified_pool)) {
      rng <- range(rv$unified_pool$GeometricMeanStarting, na.rm = TRUE)
      updateSliderInput(session, "filter_avgstart", min = floor(rng[1]), max = ceiling(rng[2]), value = c(floor(rng[1]), ceiling(rng[2])))
    }
  })
  
} # Close server function

# Run the application
shinyApp(ui = ui, server = server)