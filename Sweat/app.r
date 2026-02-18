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
    
    # Duplication Tab  
    tabPanel("Duplication",
             uiOutput("dupe_analysis_content")
    ),
    
    # Live Sweat Tab
    tabPanel("Live Sweat",
             uiOutput("live_sweat_content")
    ),
    
    # Sim Results Tab - now split into 3 sub-sections
    tabPanel("Scored Lineups",
             uiOutput("scored_lineups_content")
    ),
    
    tabPanel("Sim Performance", 
             uiOutput("sim_performance_content")
    ),
    
    tabPanel("Filter Rewind",
             uiOutput("filter_rewind_content")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Add resource path for www folder (for logo.jpg)
  addResourcePath("www", "www")
  
  # Helper function for safe scatter plots with validation
  safe_scatter_plot <- function(data, x_col, y_col = "ActualScore", 
                                x_label, title_prefix, use_three_colors = FALSE) {
    # Validate data
    if (is.null(data) || nrow(data) < 2) {
      return(plot_ly() %>%
               layout(
                 title = list(text = "Not enough data to display chart", font = list(color = '#FFE500')),
                 paper_bgcolor = '#000000',
                 plot_bgcolor = '#1a1a1a'
               ))
    }
    
    # Make a copy to avoid modifying the original
    plot_data <- copy(data)
    
    # Build model
    model <- tryCatch({
      formula_str <- paste(y_col, "~", x_col)
      lm(as.formula(formula_str), data = plot_data)
    }, error = function(e) NULL)
    
    if (is.null(model)) {
      return(plot_ly() %>%
               layout(
                 title = list(text = "Unable to calculate trendline", font = list(color = '#FFE500')),
                 paper_bgcolor = '#000000',
                 plot_bgcolor = '#1a1a1a'
               ))
    }
    
    plot_data$fitted <- predict(model)
    r_sq <- summary(model)$r.squared
    
    # 3-color system if requested and flags exist
    if (use_three_colors && "InSim" %in% names(plot_data) && "InContest" %in% names(plot_data)) {
      # Create status column for coloring
      plot_data[, Status := data.table::fcase(
        InSim & InContest, "In Sim & Played",
        InSim & !InContest, "In Sim Only",
        !InSim & InContest, "Played Only (Not in Sim)",
        default = "Unknown"
      )]
      
      plot_ly(plot_data,
              x = as.formula(paste0("~", x_col)),
              y = as.formula(paste0("~", y_col)),
              color = ~Status,
              colors = c("In Sim & Played" = "#FFE500",      # Gold
                         "In Sim Only" = "#666666",           # Gray
                         "Played Only (Not in Sim)" = "#FF0000"),  # Red
              type = 'scatter',
              mode = 'markers',
              marker = list(size = 10, opacity = 0.7, line = list(color = '#000000', width = 1)),
              text = ~paste(x_label, ":", round(get(x_col), 1), "<br>",
                            "Actual Score:", round(get(y_col), 1), "<br>",
                            "Status:", Status),
              hoverinfo = 'text') %>%
        add_trace(
          x = as.formula(paste0("~", x_col)),
          y = ~fitted,
          type = 'scatter',
          mode = 'lines',
          line = list(color = '#FFE500', width = 3, dash = 'dash'),
          name = 'Trendline',
          hoverinfo = 'skip',
          showlegend = TRUE,
          inherit = FALSE
        ) %>%
        layout(
          title = list(text = sprintf("%s vs Actual Score (R² = %.3f)", title_prefix, r_sq),
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
    } else {
      # 2-color system (legacy - InContest only)
      color_col <- if ("InContest" %in% names(plot_data)) "InContest" else "InContest"
      
      plot_ly(plot_data,
              x = as.formula(paste0("~", x_col)),
              y = as.formula(paste0("~", y_col)),
              color = as.formula(paste0("~", color_col)),
              colors = c("#666666", "#FFE500"),
              type = 'scatter',
              mode = 'markers',
              marker = list(size = 10, opacity = 0.7, line = list(color = '#000000', width = 1)),
              text = ~paste(x_label, ":", round(get(x_col), 1), "<br>",
                            "Actual Score:", round(get(y_col), 1), "<br>",
                            "Played:", ifelse(get(color_col), "Yes", "No")),
              hoverinfo = 'text',
              name = ~ifelse(get(color_col), "Played in Contest", "Not Played")) %>%
        add_trace(
          x = as.formula(paste0("~", x_col)),
          y = ~fitted,
          type = 'scatter',
          mode = 'lines',
          line = list(color = '#FFE500', width = 3, dash = 'dash'),
          name = 'Trendline',
          hoverinfo = 'skip',
          showlegend = TRUE,
          inherit = FALSE
        ) %>%
        layout(
          title = list(text = sprintf("%s vs Actual Score (R² = %.3f)", title_prefix, r_sq),
                       font = list(color = '#FFE500', size = 18)),
          xaxis = list(title = x_label, color = '#FFFFFF', gridcolor = '#333333'),
          yaxis = list(title = "Actual Score", color = '#FFFFFF', gridcolor = '#333333'),
          paper_bgcolor = '#000000',
          plot_bgcolor = '#1a1a1a',
          font = list(color = '#FFFFFF', size = 14),
          showlegend = TRUE,
          legend = list(
            title = list(text = "Status"),
            bgcolor = '#1a1a1a',
            bordercolor = '#FFE500',
            borderwidth = 1
          )
        )
    }
  }
  
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
  
  # Classify users by entry count into volume groups
  classify_entry_group <- function(entry_count) {
    dplyr::case_when(
      entry_count == 1 ~ "Single Entry",
      entry_count <= 5 ~ "Low Volume",
      entry_count <= 20 ~ "Medium Volume",
      entry_count <= 50 ~ "High Volume",
      TRUE ~ "Maxer"
    )
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
              FPTS = as.numeric(max(FPTS, na.rm = TRUE)),
              FieldExp = as.numeric(max(FieldExp, na.rm = TRUE))
            ), by = Player]
          } else {
            rv$player_scores <- player_data[, .(
              FPTS = as.numeric(max(FPTS, na.rm = TRUE))
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
        
        # Calculate entry groups for all users
        entry_counts <- rv$contest_data[, .(EntryCount = .N), by = Username]
        entry_counts[, EntryGroup := classify_entry_group(EntryCount)]
        rv$entry_groups <- entry_counts
        
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
          InContest = TRUE,
          TimesPlayed = i.TimesPlayed,
          PlayedBy = i.PlayedBy
        ), on = c("LineupKey", "ActualScore")]
        
        # Fill in FALSE for non-matches
        sim_clean[is.na(InContest), `:=`(
          InContest = FALSE,
          TimesPlayed = 0,
          PlayedBy = ""
        )]
        
        # Add InSim flag (all sim lineups have this as TRUE)
        sim_clean[, InSim := TRUE]
        # Rename InContest to InContest for clarity
        setnames(sim_clean, "InContest", "InContest")
        
        incProgress(0.1, detail = "Building unified pool...")
        
        # Create unified pool: Sim lineups + Contest lineups not in sim
        # Get all contest lineups with their keys
        contest_all <- copy(contest_with_scores)
        contest_all[, `:=`(
          InContest = TRUE,
          InSim = FALSE,
          TimesPlayed = 1
        )]
        
        # Add player columns to contest lineups by parsing LineupKey
        for (i in 1:length(player_cols)) {
          col_name <- player_cols[i]
          contest_all[, (col_name) := {
            sapply(LineupKey, function(key) {
              players <- sort(strsplit(key, "|", fixed = TRUE)[[1]])
              if (i <= length(players)) players[i] else NA_character_
            })
          }]
        }
        
        # Mark contest lineups that are in sim as InSim = TRUE
        contest_all[LineupKey %in% sim_clean$LineupKey, InSim := TRUE]
        
        # Get contest lineups NOT in sim
        contest_only <- contest_all[InSim == FALSE]
        
        # Add missing columns to contest_only to match sim_clean structure
        sim_cols <- names(sim_clean)
        for (col in sim_cols) {
          if (!col %in% names(contest_only)) {
            contest_only[, (col) := NA]
          }
        }
        
        # Combine sim and contest-only lineups
        unified_pool <- rbindlist(list(
          sim_clean,
          contest_only[, names(sim_clean), with = FALSE]
        ), use.names = TRUE, fill = TRUE)
        
        # Store both versions
        rv$sim_data <- sim_clean  # Original sim lineups only
        rv$unified_pool <- unified_pool  # All lineups (sim + contest)
        rv$sim_loaded <- TRUE
        
        incProgress(0.1, detail = "Calculating unique players...")
        
        # Calculate unique counts for unified pool
        if (!"UniquePlayerCount" %in% names(unified_pool)) {
          player_matrix_unified <- as.matrix(unified_pool[, player_cols, with = FALSE])
          unified_pool[, UniquePlayerCount := apply(player_matrix_unified, 1, function(x) length(unique(x)))]
          # Update the stored version
          rv$unified_pool <- unified_pool
        }
        
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
        
        # Update Filter Rewind player dropdowns
        updateSelectizeInput(session, "include_players_rewind",
                             choices = all_sim_players,
                             server = TRUE)
        updateSelectizeInput(session, "exclude_players_rewind",
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
      `% of Contest` = paste0(round(N / nrow(rv$data) * 100, 1), "%")
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
      Dupes = factor(user_dupe_counts, levels = sort(unique(user_dupe_counts))),
      Count = 1
    )[, .(Lineups = .N), by = Dupes]
    
    plot_ly(dupe_dist,
            x = ~Dupes,
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
  # SCORED LINEUPS TAB
  # ============================================================================
  
  output$scored_lineups_content <- renderUI({
    if (!rv$sim_loaded) {
      return(
        div(class = "well",
            h4("No Simulation Data Loaded", style = "color: #FFE500; text-align: center;"),
            p("Upload sim optimals file to view scored lineups.",
              style = "color: #CCCCCC; text-align: center;")
        )
      )
    }
    
    tagList(
      br(),
      
      # Ranking controls
      div(class = "well",
          h4("Lineup Ranking", style = "color: #FFE500; margin-top: 0;"),
          fluidRow(
            column(3,
                   selectInput("rank_primary", "Primary Rank By",
                               choices = c("Win Rate" = "WinRate",
                                           "Top 1%" = "Top1Pct",
                                           "Top 5%" = "Top5Pct",
                                           "Top 10%" = "Top10Pct",
                                           "Top 20%" = "Top20Pct",
                                           "Actual Score" = "ActualScore"),
                               selected = "ActualScore")
            ),
            column(3,
                   selectInput("rank_secondary", "Tiebreaker 1",
                               choices = c("None" = "none",
                                           "Win Rate" = "WinRate",
                                           "Top 1%" = "Top1Pct",
                                           "Actual Score" = "ActualScore"),
                               selected = "WinRate")
            ),
            column(3,
                   selectInput("rank_tertiary", "Tiebreaker 2",
                               choices = c("None" = "none",
                                           "Win Rate" = "WinRate",
                                           "Top 1%" = "Top1Pct",
                                           "Actual Score" = "ActualScore"),
                               selected = "none")
            )
          )
      ),
      
      br(),
      
      # Scored lineups table
      h4("All Simulated Lineups with Contest Scores", style = "color: #FFE500;"),
      p("Complete lineup pool ranked by selected metric. Downloadable to CSV/Excel.",
        style = "color: #CCCCCC;"),
      DTOutput("sim_lineups_table")
    )
  })
  
  # ============================================================================
  # SIM PERFORMANCE TAB
  # ============================================================================
  
  output$sim_performance_content <- renderUI({
    if (!rv$sim_loaded) {
      return(
        div(class = "well",
            h4("No Simulation Data Loaded", style = "color: #FFE500; text-align: center;"),
            p("Upload sim optimals to view performance analysis.",
              style = "color: #CCCCCC; text-align: center;")
        )
      )
    }
    
    tagList(
      br(),
      h4("How Well Did Each Metric Predict Contest Success?", style = "color: #FFE500;"),
      p("Full-screen charts and bucket analysis for each sim metric.",
        style = "color: #CCCCCC;"),
      br(),
      
      tabsetPanel(
        tabPanel("Win Rate",
                 br(),
                 plotlyOutput("winrate_scatter", height = "600px"),
                 br(),
                 h5("Performance by Win Rate Bucket", style = "color: #FFE500;"),
                 DTOutput("winrate_buckets")
        ),
        
        tabPanel("Top 1%",
                 br(),
                 plotlyOutput("top1_scatter", height = "600px"),
                 br(),
                 h5("Performance by Top 1% Bucket", style = "color: #FFE500;"),
                 DTOutput("top1_buckets")
        ),
        
        tabPanel("Top 5%",
                 br(),
                 plotlyOutput("top5_scatter", height = "600px"),
                 br(),
                 h5("Performance by Top 5% Bucket", style = "color: #FFE500;"),
                 DTOutput("top5_buckets")
        ),
        
        tabPanel("Total Own",
                 br(),
                 plotlyOutput("ownership_scatter", height = "600px"),
                 br(),
                 h5("Performance by Total Ownership Bucket", style = "color: #FFE500;"),
                 DTOutput("ownership_buckets")
        ),
        
        tabPanel("Average Own",
                 br(),
                 plotlyOutput("geo_ownership_scatter", height = "600px"),
                 br(),
                 h5("Performance by Average Ownership Bucket", style = "color: #FFE500;"),
                 DTOutput("geo_ownership_buckets")
        ),
        
        tabPanel("Avg Start",
                 br(),
                 plotlyOutput("avg_start_scatter", height = "600px"),
                 br(),
                 h5("Performance by Avg Start Bucket", style = "color: #FFE500;"),
                 DTOutput("avg_start_buckets")
        )
      )
    )
  })
  
  # ============================================================================
  # FILTER REWIND TAB  
  # ============================================================================
  
  output$filter_rewind_content <- renderUI({
    if (!rv$sim_loaded) {
      return(
        div(class = "well",
            h4("No Simulation Data Loaded", style = "color: #FFE500; text-align: center;"),
            p("Upload sim optimals to use Filter Rewind.",
              style = "color: #CCCCCC; text-align: center;")
        )
      )
    }
    
    tagList(
      br(),
      
      # Filter Panel with SLIDERS
      div(class = "well",
          h4("Lineup Filters", style = "color: #FFE500; margin-top: 0;"),
          p("Filter sim lineups and see how they would have performed in the contest.",
            style = "color: #CCCCCC;"),
          
          br(),
          
          # Player Filters
          fluidRow(
            column(12, h5("Player Filters", style = "color: #FFE500;"))
          ),
          fluidRow(
            column(6,
                   selectizeInput("include_players_rewind", "Must Include",
                                  choices = NULL, multiple = TRUE,
                                  options = list(placeholder = 'Select players...'))
            ),
            column(6,
                   selectizeInput("exclude_players_rewind", "Exclude",
                                  choices = NULL, multiple = TRUE,
                                  options = list(placeholder = 'Select players...'))
            )
          ),
          
          br(),
          
          # Metric Filters with SLIDERS
          fluidRow(
            column(12, h5("Metric Filters", style = "color: #FFE500;"))
          ),
          fluidRow(
            column(6,
                   sliderInput("winrate_slider", "Win Rate %",
                               min = 0, max = 100, value = c(0, 100), step = 0.1)
            ),
            column(6,
                   sliderInput("top1_slider", "Top 1%",
                               min = 0, max = 100, value = c(0, 100), step = 0.1)
            )
          ),
          fluidRow(
            column(6,
                   sliderInput("top5_slider", "Top 5%",
                               min = 0, max = 100, value = c(0, 100), step = 0.1)
            ),
            column(6,
                   sliderInput("top10_slider", "Top 10%",
                               min = 0, max = 100, value = c(0, 100), step = 0.1)
            )
          ),
          fluidRow(
            column(6,
                   sliderInput("top20_slider", "Top 20%",
                               min = 0, max = 100, value = c(0, 100), step = 0.1)
            ),
            column(6,
                   sliderInput("totalown_slider", "Total Own %",
                               min = 0, max = 1000, value = c(0, 1000), step = 10)
            )
          ),
          fluidRow(
            column(6,
                   sliderInput("avgown_slider", "Average Own %",
                               min = 0, max = 100, value = c(0, 100), step = 1)
            ),
            column(6,
                   sliderInput("salary_slider", "Salary",
                               min = 0, max = 50000, value = c(0, 50000), step = 100)
            )
          ),
          fluidRow(
            column(6,
                   sliderInput("avgstart_slider", "Avg Start Position",
                               min = 0, max = 50, value = c(0, 50), step = 0.5)
            ),
            column(6,
                   selectInput("played_rewind", "Played in Contest?",
                               choices = c("All" = "all", "Yes" = "yes", "No" = "no"),
                               selected = "all")
            )
          ),
          
          br(),
          
          fluidRow(
            column(3,
                   actionButton("reset_rewind", "Reset All Filters",
                                class = "btn-primary", style = "width: 100%;")
            )
          )
      ),
      
      br(),
      
      # Performance Stats
      h4("Filtered Pool Performance", style = "color: #FFE500;"),
      
      fluidRow(
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Lineups"),
                   div(class = "stat-value", textOutput("rewind_count"))
               )
        ),
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Played"),
                   div(class = "stat-value", textOutput("rewind_played"))
               )
        ),
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Would Win"),
                   div(class = "stat-value", textOutput("rewind_win"))
               )
        ),
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Top 1%"),
                   div(class = "stat-value", textOutput("rewind_top1"))
               )
        ),
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Top 5%"),
                   div(class = "stat-value", textOutput("rewind_top5"))
               )
        ),
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Top 10%"),
                   div(class = "stat-value", textOutput("rewind_top10"))
               )
        )
      ),
      
      fluidRow(
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Top 20%"),
                   div(class = "stat-value", textOutput("rewind_top20"))
               )
        ),
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Avg Score"),
                   div(class = "stat-value", textOutput("rewind_avg"))
               )
        ),
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Best Score"),
                   div(class = "stat-value", textOutput("rewind_best"))
               )
        )
      ),
      
      br(),
      
      h4("Duplication Distribution", style = "color: #FFE500;"),
      plotlyOutput("rewind_dupe_dist", height = "300px")
    )
  })
  
  output$sim_analysis_content <- renderUI({
    if (!rv$sim_loaded) {
      return(
        div(class = "well",
            h4("No Simulation Data Loaded", style = "color: #FFE500; text-align: center; margin-top: 50px;"),
            p("Upload a sim optimals file to unlock simulation analysis features.",
              style = "color: #CCCCCC; text-align: center; font-size: 16px;"),
            p("This will show you how your simulated lineups performed in the actual contest.",
              style = "color: #CCCCCC; text-align: center;")
        )
      )
    }
    
    tagList(
      br(),
      
      # Filter Panel
      div(class = "well",
          h4("Filter Sim Lineups", style = "color: #FFE500; margin-top: 0;"),
          
          # Player filters
          fluidRow(
            column(12,
                   h5("Player Filters", style = "color: #FFE500;")
            )
          ),
          fluidRow(
            column(3,
                   selectizeInput("include_players", "Must Include Players",
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select players...'))
            ),
            column(3,
                   selectizeInput("exclude_players", "Exclude Players",
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(placeholder = 'Select players...'))
            ),
            column(3,
                   numericInput("min_unique", "Min Unique Players",
                                value = 0, min = 0, max = 6, step = 1)
            ),
            column(3,
                   numericInput("max_unique", "Max Unique Players",
                                value = 6, min = 0, max = 6, step = 1)
            )
          ),
          
          br(),
          
          # Metric filters
          fluidRow(
            column(12,
                   h5("Metric Filters", style = "color: #FFE500;")
            )
          ),
          fluidRow(
            column(3,
                   numericInput("min_winrate", "Min Win Rate %", 
                                value = 0, min = 0, max = 100, step = 0.5)
            ),
            column(3,
                   numericInput("min_top1", "Min Top 1%", 
                                value = 0, min = 0, max = 100, step = 0.5)
            ),
            column(3,
                   numericInput("min_top5", "Min Top 5%", 
                                value = 0, min = 0, max = 100, step = 0.5)
            ),
            column(3,
                   numericInput("min_top10", "Min Top 10%", 
                                value = 0, min = 0, max = 100, step = 0.5)
            )
          ),
          
          fluidRow(
            column(3,
                   numericInput("min_top20", "Min Top 20%", 
                                value = 0, min = 0, max = 100, step = 0.5)
            ),
            column(3,
                   numericInput("max_ownership", "Max Total Ownership %", 
                                value = 1000, min = 0, max = 1000, step = 10)
            ),
            column(3,
                   numericInput("max_geo_ownership", "Max Geo Ownership %", 
                                value = 100, min = 0, max = 100, step = 5)
            ),
            column(3,
                   numericInput("max_salary", "Max Salary", 
                                value = 50000, min = 0, max = 50000, step = 100)
            )
          ),
          
          fluidRow(
            column(3,
                   numericInput("min_avg_start", "Min Avg Start", 
                                value = 0, min = 0, max = 50, step = 1)
            ),
            column(3,
                   numericInput("max_avg_start", "Max Avg Start", 
                                value = 50, min = 0, max = 50, step = 1)
            ),
            column(3,
                   selectInput("sim_rank_metric", "Rank Lineups By",
                               choices = c("Win Rate" = "WinRate",
                                           "Top 1%" = "Top1Pct",
                                           "Top 5%" = "Top5Pct",
                                           "Top 10%" = "Top10Pct",
                                           "Top 20%" = "Top20Pct",
                                           "Actual Score" = "ActualScore"),
                               selected = "WinRate")
            ),
            column(3,
                   selectInput("played_filter", "Played in Contest?",
                               choices = c("All" = "all",
                                           "Yes" = "yes", 
                                           "No" = "no"),
                               selected = "all")
            )
          ),
          
          fluidRow(
            column(3,
                   actionButton("reset_filters", "Reset All Filters", 
                                class = "btn-primary", style = "margin-top: 25px; width: 100%;")
            )
          )
      ),
      
      # Summary Stats
      fluidRow(
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Total Lineups"),
                   div(class = "stat-value", textOutput("total_sim_lineups"))
               )
        ),
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Played in Contest"),
                   div(class = "stat-value", textOutput("played_sim_lineups"))
               )
        ),
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Would Win"),
                   div(class = "stat-value", textOutput("would_win_count"))
               )
        ),
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Top 1%"),
                   div(class = "stat-value", textOutput("top1_count"))
               )
        ),
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Top 5%"),
                   div(class = "stat-value", textOutput("top5_count"))
               )
        ),
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Top 10%"),
                   div(class = "stat-value", textOutput("top10_count"))
               )
        )
      ),
      
      fluidRow(
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Top 20%"),
                   div(class = "stat-value", textOutput("top20_count"))
               )
        ),
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Avg Actual Score"),
                   div(class = "stat-value", textOutput("avg_sim_score"))
               )
        ),
        column(2,
               div(class = "stat-box",
                   div(class = "stat-label", "Best Actual Score"),
                   div(class = "stat-value", textOutput("best_sim_score"))
               )
        )
      ),
      
      br(),
      
      # Dupe Distribution
      h4("Player Duplication Distribution", style = "color: #FFE500;"),
      plotlyOutput("dupe_distribution", height = "300px"),
      
      br(),
      
      # Sim Lineups Table (Filtered Pool)
      h4("Filtered Simulation Lineups", style = "color: #FFE500;"),
      p("This table shows all lineups matching your filters. Click column headers to sort.",
        style = "color: #CCCCCC;"),
      DTOutput("sim_lineups_table"),
      
      br(),
      
      # Performance Analysis Section
      h4("Performance Analysis", style = "color: #FFE500;"),
      
      tabsetPanel(
        tabPanel("Score Distribution",
                 br(),
                 fluidRow(
                   column(6,
                          h5("Actual Score Distribution", style = "color: #FFE500;"),
                          plotlyOutput("actual_score_dist", height = "350px")
                   ),
                   column(6,
                          h5("Score by Percentile", style = "color: #FFE500;"),
                          plotlyOutput("score_by_percentile", height = "350px")
                   )
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          h5("Player Duplication Distribution", style = "color: #FFE500;"),
                          plotlyOutput("dupe_distribution", height = "300px")
                   )
                 )
        ),
        
        tabPanel("Metric Correlations",
                 br(),
                 fluidRow(
                   column(6,
                          h5("Win Rate vs Actual Score", style = "color: #FFE500;"),
                          plotlyOutput("winrate_scatter", height = "350px")
                   ),
                   column(6,
                          h5("Top 1% vs Actual Score", style = "color: #FFE500;"),
                          plotlyOutput("top1_scatter", height = "350px")
                   )
                 ),
                 br(),
                 fluidRow(
                   column(6,
                          h5("Top 5% vs Actual Score", style = "color: #FFE500;"),
                          plotlyOutput("top5_scatter", height = "350px")
                   ),
                   column(6,
                          h5("Ownership vs Actual Score", style = "color: #FFE500;"),
                          plotlyOutput("ownership_scatter", height = "350px")
                   )
                 )
        ),
        
        tabPanel("Additional Metrics",
                 br(),
                 fluidRow(
                   column(6,
                          h5("Geometric Ownership vs Actual Score", style = "color: #FFE500;"),
                          plotlyOutput("geo_ownership_scatter", height = "350px")
                   ),
                   column(6,
                          h5("Total Start vs Actual Score", style = "color: #FFE500;"),
                          plotlyOutput("total_start_scatter", height = "350px")
                   )
                 ),
                 br(),
                 fluidRow(
                   column(6,
                          h5("Avg Start vs Actual Score", style = "color: #FFE500;"),
                          plotlyOutput("avg_start_scatter", height = "350px")
                   ),
                   column(6,
                          h5("Correlation Summary", style = "color: #FFE500;"),
                          DTOutput("correlation_summary_table")
                   )
                 )
        ),
        
        tabPanel("Bucket Analysis",
                 br(),
                 h5("Performance by Metric Buckets", style = "color: #FFE500;"),
                 p("See how different ranges of each metric actually performed in the contest.",
                   style = "color: #CCCCCC;"),
                 br(),
                 tabsetPanel(
                   tabPanel("Win Rate",
                            br(),
                            DTOutput("winrate_buckets")
                   ),
                   tabPanel("Top 1%",
                            br(),
                            DTOutput("top1_buckets")
                   ),
                   tabPanel("Top 5%",
                            br(),
                            DTOutput("top5_buckets")
                   ),
                   tabPanel("Ownership",
                            br(),
                            DTOutput("ownership_buckets")
                   ),
                   tabPanel("Geo Ownership",
                            br(),
                            DTOutput("geo_ownership_buckets")
                   ),
                   tabPanel("Total Start",
                            br(),
                            DTOutput("total_start_buckets")
                   ),
                   tabPanel("Avg Start",
                            br(),
                            DTOutput("avg_start_buckets")
                   )
                 )
        )
      )
    )
  })
  
  # Filtered sim data (with debouncing for performance)
  filtered_sim_data <- reactive({
    req(rv$sim_data)
    
    # Debounce - wait 500ms after user stops adjusting filters
    input$min_winrate
    input$min_top1
    input$min_top5
    input$min_top10
    input$min_top20
    input$max_ownership
    input$max_geo_ownership
    input$max_salary
    input$min_avg_start
    input$max_avg_start
    input$played_filter
    input$include_players
    input$exclude_players
    input$min_unique
    input$max_unique
    
    data <- copy(rv$sim_data)
    
    # Get player columns
    player_cols <- grep("^(Player|Driver|Fighter|Golfer)[0-9]$", names(data), value = TRUE)
    
    # Apply player inclusion filters (optimized)
    if (!is.null(input$include_players) && length(input$include_players) > 0) {
      for (player in input$include_players) {
        # More efficient than rowSums
        data <- data[Reduce(`|`, lapply(.SD, function(x) x == player)), .SDcols = player_cols]
      }
    }
    
    # Apply player exclusion filters (optimized)
    if (!is.null(input$exclude_players) && length(input$exclude_players) > 0) {
      for (player in input$exclude_players) {
        data <- data[!Reduce(`|`, lapply(.SD, function(x) x == player)), .SDcols = player_cols]
      }
    }
    
    # Apply unique player filters
    if (!is.null(input$min_unique) && input$min_unique > 0) {
      data <- data[UniquePlayerCount >= input$min_unique]
    }
    
    if (!is.null(input$max_unique) && input$max_unique < 6) {
      data <- data[UniquePlayerCount <= input$max_unique]
    }
    
    # Apply metric filters (all vectorized - very fast)
    if (!is.null(input$min_winrate) && "WinRate" %in% names(data) && input$min_winrate > 0) {
      data <- data[WinRate >= input$min_winrate]
    }
    
    if (!is.null(input$min_top1) && "Top1Pct" %in% names(data) && input$min_top1 > 0) {
      data <- data[Top1Pct >= input$min_top1]
    }
    
    if (!is.null(input$min_top5) && "Top5Pct" %in% names(data) && input$min_top5 > 0) {
      data <- data[Top5Pct >= input$min_top5]
    }
    
    if (!is.null(input$min_top10) && "Top10Pct" %in% names(data) && input$min_top10 > 0) {
      data <- data[Top10Pct >= input$min_top10]
    }
    
    if (!is.null(input$min_top20) && "Top20Pct" %in% names(data) && input$min_top20 > 0) {
      data <- data[Top20Pct >= input$min_top20]
    }
    
    if (!is.null(input$max_ownership) && "CumulativeOwnership" %in% names(data) && input$max_ownership < 1000) {
      data <- data[CumulativeOwnership <= input$max_ownership]
    }
    
    if (!is.null(input$max_geo_ownership) && "GeometricMeanOwnership" %in% names(data) && input$max_geo_ownership < 100) {
      data <- data[GeometricMeanOwnership <= input$max_geo_ownership]
    }
    
    if (!is.null(input$max_salary) && "TotalSalary" %in% names(data) && input$max_salary < 50000) {
      data <- data[TotalSalary <= input$max_salary]
    }
    
    if (!is.null(input$min_avg_start) && "AvgStart" %in% names(data) && input$min_avg_start > 0) {
      data <- data[AvgStart >= input$min_avg_start]
    }
    
    if (!is.null(input$max_avg_start) && "AvgStart" %in% names(data) && input$max_avg_start < 50) {
      data <- data[AvgStart <= input$max_avg_start]
    }
    
    if (!is.null(input$played_filter) && input$played_filter != "all") {
      if (input$played_filter == "yes") {
        data <- data[InContest == TRUE]
      } else {
        data <- data[InContest == FALSE]
      }
    }
    
    # Calculate SimRank based on selected metric
    if (!is.null(input$sim_rank_metric) && input$sim_rank_metric %in% names(data)) {
      if (input$sim_rank_metric == "ActualScore") {
        data[, SimRank := frank(-ActualScore, ties.method = "min")]
      } else {
        data[, SimRank := frank(-get(input$sim_rank_metric), ties.method = "min")]
      }
    } else {
      # Default to WinRate if selected metric doesn't exist
      if ("WinRate" %in% names(data)) {
        data[, SimRank := frank(-WinRate, ties.method = "min")]
      } else if ("Top1Pct" %in% names(data)) {
        data[, SimRank := frank(-Top1Pct, ties.method = "min")]
      }
    }
    
    return(data)
  }) %>% debounce(500)  # Wait 500ms after last filter change
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "include_players", selected = character(0))
    updateSelectizeInput(session, "exclude_players", selected = character(0))
    updateNumericInput(session, "min_unique", value = 0)
    updateNumericInput(session, "max_unique", value = 6)
    updateNumericInput(session, "min_winrate", value = 0)
    updateNumericInput(session, "min_top1", value = 0)
    updateNumericInput(session, "min_top5", value = 0)
    updateNumericInput(session, "min_top10", value = 0)
    updateNumericInput(session, "min_top20", value = 0)
    updateNumericInput(session, "max_ownership", value = 1000)
    updateNumericInput(session, "max_geo_ownership", value = 100)
    updateNumericInput(session, "max_salary", value = 50000)
    updateNumericInput(session, "min_avg_start", value = 0)
    updateNumericInput(session, "max_avg_start", value = 50)
    updateSelectInput(session, "played_filter", selected = "all")
  })
  
  # Summary stats - Fixed to show actual contest results
  output$total_sim_lineups <- renderText({
    req(filtered_sim_data())
    data <- filtered_sim_data()
    if (nrow(data) == 0) return("0")
    format(nrow(data), big.mark = ",")
  })
  
  output$played_sim_lineups <- renderText({
    req(filtered_sim_data())
    data <- filtered_sim_data()
    if (nrow(data) == 0) return("0")
    played <- sum(data$InContest, na.rm = TRUE)
    pct <- round(played / nrow(data) * 100, 1)
    paste0(format(played, big.mark = ","), " (", pct, "%)")
  })
  
  output$would_win_count <- renderText({
    req(filtered_sim_data(), rv$contest_data)
    data <- filtered_sim_data()
    if (nrow(data) == 0) return("0")
    winning_score <- max(rv$contest_data$Points, na.rm = TRUE)
    wins <- sum(data$ActualScore >= winning_score, na.rm = TRUE)
    format(wins, big.mark = ",")
  })
  
  output$top1_count <- renderText({
    req(filtered_sim_data(), rv$contest_data)
    data <- filtered_sim_data()
    if (nrow(data) == 0) return("0")
    top1_threshold <- quantile(rv$contest_data$Points, probs = 0.99, na.rm = TRUE)
    top1 <- sum(data$ActualScore >= top1_threshold, na.rm = TRUE)
    format(top1, big.mark = ",")
  })
  
  output$top5_count <- renderText({
    req(filtered_sim_data(), rv$contest_data)
    data <- filtered_sim_data()
    if (nrow(data) == 0) return("0")
    top5_threshold <- quantile(rv$contest_data$Points, probs = 0.95, na.rm = TRUE)
    top5 <- sum(data$ActualScore >= top5_threshold, na.rm = TRUE)
    format(top5, big.mark = ",")
  })
  
  output$top10_count <- renderText({
    req(filtered_sim_data(), rv$contest_data)
    data <- filtered_sim_data()
    if (nrow(data) == 0) return("0")
    top10_threshold <- quantile(rv$contest_data$Points, probs = 0.90, na.rm = TRUE)
    top10 <- sum(data$ActualScore >= top10_threshold, na.rm = TRUE)
    format(top10, big.mark = ",")
  })
  
  output$top20_count <- renderText({
    req(filtered_sim_data(), rv$contest_data)
    data <- filtered_sim_data()
    if (nrow(data) == 0) return("0")
    top20_threshold <- quantile(rv$contest_data$Points, probs = 0.80, na.rm = TRUE)
    top20 <- sum(data$ActualScore >= top20_threshold, na.rm = TRUE)
    format(top20, big.mark = ",")
  })
  
  output$avg_sim_score <- renderText({
    req(filtered_sim_data())
    data <- filtered_sim_data()
    if (nrow(data) == 0) return("0.0")
    round(mean(data$ActualScore, na.rm = TRUE), 1)
  })
  
  output$best_sim_score <- renderText({
    req(filtered_sim_data())
    data <- filtered_sim_data()
    if (nrow(data) == 0) return("0.0")
    round(max(data$ActualScore, na.rm = TRUE), 1)
  })
  
  # Dupe distribution chart
  output$dupe_distribution <- renderPlotly({
    req(filtered_sim_data())
    
    data <- filtered_sim_data()
    
    # Count frequency of each unique player count
    dupe_counts <- data[, .N, by = UniquePlayerCount][order(UniquePlayerCount)]
    dupe_counts[, Percentage := round(N / sum(N) * 100, 1)]
    
    plot_ly(dupe_counts,
            x = ~UniquePlayerCount,
            y = ~N,
            type = 'bar',
            marker = list(color = '#FFE500',
                          line = list(color = '#000000', width = 1)),
            text = ~paste0(N, " (", Percentage, "%)"),
            textposition = 'outside',
            hoverinfo = 'text',
            hovertext = ~paste("Unique Players:", UniquePlayerCount, "<br>",
                               "Lineups:", N, "<br>",
                               "Percentage:", Percentage, "%")) %>%
      layout(
        xaxis = list(title = "Number of Unique Players", 
                     color = '#FFFFFF',
                     tickvals = 1:6),
        yaxis = list(title = "Number of Lineups", color = '#FFFFFF'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        showlegend = FALSE,
        margin = list(t = 20)
      )
  })
  
  # Actual score distribution
  output$actual_score_dist <- renderPlotly({
    req(filtered_sim_data())
    
    data <- filtered_sim_data()
    
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
    req(filtered_sim_data(), rv$contest_data)
    
    data <- filtered_sim_data()
    
    # Calculate what percentile each sim lineup would have finished at
    contest_scores <- sort(rv$contest_data$Points, decreasing = TRUE)
    
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
    req(filtered_sim_data())
    
    data <- filtered_sim_data()
    
    # Check if we have data
    if (nrow(data) < 2) {
      return(datatable(
        data.table(Message = "Not enough data for correlation analysis"),
        rownames = FALSE,
        options = list(dom = 't')
      ))
    }
    
    # Calculate correlations with ActualScore
    cor_cols <- c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct",
                  "CumulativeOwnership", "GeometricMeanOwnership", 
                  "TotalSalary", "TotalStart", "AvgStart")
    cor_cols <- intersect(cor_cols, names(data))
    
    if (length(cor_cols) == 0) {
      return(datatable(
        data.table(Message = "No correlation columns available"),
        rownames = FALSE,
        options = list(dom = 't')
      ))
    }
    
    correlations <- sapply(cor_cols, function(col) {
      tryCatch({
        cor(data[[col]], data$ActualScore, use = "pairwise.complete.obs")
      }, error = function(e) NA_real_)
    })
    
    # Calculate R-squared
    r_squared <- sapply(cor_cols, function(col) {
      tryCatch({
        model <- lm(ActualScore ~ get(col), data = data)
        summary(model)$r.squared
      }, error = function(e) NA_real_)
    })
    
    cor_table <- data.table(
      Metric = names(correlations),
      Correlation = round(correlations, 4),
      `R-Squared` = round(r_squared, 4)
    )[!is.na(Correlation)][order(-abs(Correlation))]
    
    if (nrow(cor_table) == 0) {
      return(datatable(
        data.table(Message = "Unable to calculate correlations"),
        rownames = FALSE,
        options = list(dom = 't')
      ))
    }
    
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
    req(filtered_sim_data())
    
    data <- filtered_sim_data()
    
    # Select columns to display
    player_cols <- grep("^(Player|Driver|Fighter|Golfer)[0-9]$", names(data), value = TRUE)
    
    metric_cols <- c("SimRank", "WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct",
                     "CumulativeOwnership", "GeometricMeanOwnership",
                     "TotalSalary", "TotalStart", "AvgStart",
                     "ActualScore", "InContest", 
                     "TimesPlayed", "PlayedBy")
    metric_cols <- intersect(metric_cols, names(data))
    
    display_cols <- c(player_cols, metric_cols)
    display_data <- data[order(SimRank), ..display_cols]  # Sort by SimRank (lower = better)
    
    # Don't rename - causes formatStyle issues
    # Just change column labels in datatable instead
    col_names <- names(display_data)
    col_names[col_names == "CumulativeOwnership"] <- "Total Own"
    col_names[col_names == "GeometricMeanOwnership"] <- "Average Own"
    col_names[col_names == "InContest"] <- "In Contest"
    
    datatable(
      display_data,
      colnames = col_names,
      rownames = FALSE,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons'
    ) %>%
      formatRound(c("WinRate", "Top1Pct", "Top5Pct", "Top10Pct", "Top20Pct"), 1) %>%
      formatRound(c("ActualScore", "AvgStart"), 1) %>%
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
    req(rv$unified_pool, "WinRate" %in% names(rv$unified_pool))
    safe_scatter_plot(rv$unified_pool, "WinRate", "ActualScore", 
                      "Win Rate %", "Win Rate", use_three_colors = TRUE)
  })
  
  # Win Rate buckets
  output$winrate_buckets <- renderDT({
    req(rv$unified_pool, "WinRate" %in% names(filtered_sim_data()))
    
    data <- copy(filtered_sim_data())
    
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
    req(rv$unified_pool, "Top1Pct" %in% names(rv$unified_pool))
    safe_scatter_plot(rv$unified_pool, "Top1Pct", "ActualScore",
                      "Top 1% Probability", "Top 1%", use_three_colors = TRUE)
  })
  
  output$top1_buckets <- renderDT({
    req(rv$unified_pool, "Top1Pct" %in% names(filtered_sim_data()))
    
    data <- copy(filtered_sim_data())
    
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
    req(filtered_sim_data(), "Top5Pct" %in% names(filtered_sim_data()))
    
    data <- filtered_sim_data()
    
    # Calculate trendline
    model <- lm(ActualScore ~ Top5Pct, data = data)
    data$fitted <- predict(model)
    
    plot_ly(data, 
            x = ~Top5Pct, 
            y = ~ActualScore,
            color = ~InContest,
            colors = c("#666666", "#FFE500"),
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 10, opacity = 0.7, line = list(color = "#000000", width = 1)),
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
    req(filtered_sim_data(), "Top5Pct" %in% names(filtered_sim_data()))
    
    data <- copy(filtered_sim_data())
    
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
    req(filtered_sim_data(), "CumulativeOwnership" %in% names(filtered_sim_data()))
    
    data <- filtered_sim_data()
    
    # Calculate trendline
    model <- lm(ActualScore ~ CumulativeOwnership, data = data)
    data$fitted <- predict(model)
    
    plot_ly(data, 
            x = ~CumulativeOwnership, 
            y = ~ActualScore,
            color = ~InContest,
            colors = c("#666666", "#FFE500"),
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 10, opacity = 0.7, line = list(color = "#000000", width = 1)),
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
    req(filtered_sim_data(), "CumulativeOwnership" %in% names(filtered_sim_data()))
    
    data <- copy(filtered_sim_data())
    
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
    req(filtered_sim_data(), "GeometricMeanOwnership" %in% names(filtered_sim_data()))
    
    data <- filtered_sim_data()
    
    # Calculate trendline
    model <- lm(ActualScore ~ GeometricMeanOwnership, data = data)
    data$fitted <- predict(model)
    
    plot_ly(data, 
            x = ~GeometricMeanOwnership, 
            y = ~ActualScore,
            color = ~InContest,
            colors = c("#666666", "#FFE500"),
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 10, opacity = 0.7, line = list(color = "#000000", width = 1)),
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
    req(filtered_sim_data(), "GeometricMeanOwnership" %in% names(filtered_sim_data()))
    
    data <- copy(filtered_sim_data())
    
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
    req(filtered_sim_data(), "TotalStart" %in% names(filtered_sim_data()))
    
    data <- filtered_sim_data()
    
    # Calculate trendline
    model <- lm(ActualScore ~ TotalStart, data = data)
    data$fitted <- predict(model)
    
    plot_ly(data, 
            x = ~TotalStart, 
            y = ~ActualScore,
            color = ~InContest,
            colors = c("#666666", "#FFE500"),
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 10, opacity = 0.7, line = list(color = "#000000", width = 1)),
            text = ~paste("Total Start:", TotalStart, "<br>",
                          "Actual Score:", round(ActualScore, 2)),
            hoverinfo = 'text',
            name = ~ifelse(InContest, "Played", "Not Played")) %>%
      add_trace(
        x = ~TotalStart,
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
    req(filtered_sim_data(), "TotalStart" %in% names(filtered_sim_data()))
    
    data <- copy(filtered_sim_data())
    
    data[, StartBucket := cut(TotalStart, 
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
    req(filtered_sim_data(), "AvgStart" %in% names(filtered_sim_data()))
    
    data <- filtered_sim_data()
    
    # Calculate trendline
    model <- lm(ActualScore ~ AvgStart, data = data)
    data$fitted <- predict(model)
    
    plot_ly(data, 
            x = ~AvgStart, 
            y = ~ActualScore,
            color = ~InContest,
            colors = c("#666666", "#FFE500"),
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 10, opacity = 0.7, line = list(color = "#000000", width = 1)),
            text = ~paste("Avg Start:", round(AvgStart, 1), "<br>",
                          "Actual Score:", round(ActualScore, 2)),
            hoverinfo = 'text',
            name = ~ifelse(InContest, "Played", "Not Played")) %>%
      add_trace(
        x = ~AvgStart,
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
    req(filtered_sim_data(), "AvgStart" %in% names(filtered_sim_data()))
    
    data <- copy(filtered_sim_data())
    
    data[, AvgStartBucket := cut(AvgStart, 
                                 breaks = c(0, 25, 30, 35, 40, 50),
                                 labels = c("0-25", "25-30", "30-35", "35-40", "40+"),
                                 include.lowest = TRUE)]
    
    bucket_stats <- data[, .(
      Count = .N,
      `Avg Actual Score` = round(mean(ActualScore, na.rm = TRUE), 2),
      `Max Actual Score` = round(max(ActualScore, na.rm = TRUE), 2),
      `Played Count` = sum(InContest, na.rm = TRUE),
      `Play Rate %` = round(sum(InContest, na.rm = TRUE) / .N * 100, 1)
    ), by = AvgStartBucket][order(AvgStartBucket)]
    
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
  # FILTER REWIND OUTPUTS
  # ============================================================================
  
  # Reactive data for Filter Rewind (separate filtering)
  rewind_filtered_data <- reactive({
    req(rv$sim_data)
    
    data <- copy(rv$sim_data)
    player_cols <- grep("^(Player|Driver|Fighter|Golfer)[0-9]$", names(data), value = TRUE)
    
    # Player filters
    if (!is.null(input$include_players_rewind) && length(input$include_players_rewind) > 0) {
      for (player in input$include_players_rewind) {
        data <- data[Reduce(`|`, lapply(.SD, function(x) x == player)), .SDcols = player_cols]
      }
    }
    
    if (!is.null(input$exclude_players_rewind) && length(input$exclude_players_rewind) > 0) {
      for (player in input$exclude_players_rewind) {
        data <- data[!Reduce(`|`, lapply(.SD, function(x) x == player)), .SDcols = player_cols]
      }
    }
    
    # Slider filters
    if (!is.null(input$winrate_slider) && "WinRate" %in% names(data)) {
      data <- data[WinRate >= input$winrate_slider[1] & WinRate <= input$winrate_slider[2]]
    }
    if (!is.null(input$top1_slider) && "Top1Pct" %in% names(data)) {
      data <- data[Top1Pct >= input$top1_slider[1] & Top1Pct <= input$top1_slider[2]]
    }
    if (!is.null(input$top5_slider) && "Top5Pct" %in% names(data)) {
      data <- data[Top5Pct >= input$top5_slider[1] & Top5Pct <= input$top5_slider[2]]
    }
    if (!is.null(input$top10_slider) && "Top10Pct" %in% names(data)) {
      data <- data[Top10Pct >= input$top10_slider[1] & Top10Pct <= input$top10_slider[2]]
    }
    if (!is.null(input$top20_slider) && "Top20Pct" %in% names(data)) {
      data <- data[Top20Pct >= input$top20_slider[1] & Top20Pct <= input$top20_slider[2]]
    }
    if (!is.null(input$totalown_slider) && "CumulativeOwnership" %in% names(data)) {
      data <- data[CumulativeOwnership >= input$totalown_slider[1] & CumulativeOwnership <= input$totalown_slider[2]]
    }
    if (!is.null(input$avgown_slider) && "GeometricMeanOwnership" %in% names(data)) {
      data <- data[GeometricMeanOwnership >= input$avgown_slider[1] & GeometricMeanOwnership <= input$avgown_slider[2]]
    }
    if (!is.null(input$salary_slider) && "TotalSalary" %in% names(data)) {
      data <- data[TotalSalary >= input$salary_slider[1] & TotalSalary <= input$salary_slider[2]]
    }
    if (!is.null(input$avgstart_slider) && "AvgStart" %in% names(data)) {
      data <- data[AvgStart >= input$avgstart_slider[1] & AvgStart <= input$avgstart_slider[2]]
    }
    if (!is.null(input$played_rewind) && input$played_rewind != "all") {
      if (input$played_rewind == "yes") {
        data <- data[InContest == TRUE]
      } else {
        data <- data[InContest == FALSE]
      }
    }
    
    return(data)
  }) %>% debounce(500)
  
  # Reset rewind filters
  observeEvent(input$reset_rewind, {
    req(rv$sim_data)
    
    if ("WinRate" %in% names(rv$sim_data)) {
      updateSliderInput(session, "winrate_slider", 
                        value = c(min(rv$sim_data$WinRate, na.rm = TRUE),
                                  max(rv$sim_data$WinRate, na.rm = TRUE)))
    }
    if ("Top1Pct" %in% names(rv$sim_data)) {
      updateSliderInput(session, "top1_slider",
                        value = c(min(rv$sim_data$Top1Pct, na.rm = TRUE),
                                  max(rv$sim_data$Top1Pct, na.rm = TRUE)))
    }
    updateSelectInput(session, "played_rewind", selected = "all")
    updateSelectizeInput(session, "include_players_rewind", selected = character(0))
    updateSelectizeInput(session, "exclude_players_rewind", selected = character(0))
  })
  
  # Rewind stat outputs
  output$rewind_count <- renderText({
    req(rewind_filtered_data())
    format(nrow(rewind_filtered_data()), big.mark = ",")
  })
  
  output$rewind_played <- renderText({
    req(rewind_filtered_data())
    played <- sum(rewind_filtered_data()$InContest, na.rm = TRUE)
    pct <- round(played / nrow(rewind_filtered_data()) * 100, 1)
    paste0(played, " (", pct, "%)")
  })
  
  output$rewind_win <- renderText({
    req(rewind_filtered_data(), rv$contest_data)
    winning_score <- max(rv$contest_data$Points, na.rm = TRUE)
    sum(rewind_filtered_data()$ActualScore >= winning_score, na.rm = TRUE)
  })
  
  output$rewind_top1 <- renderText({
    req(rewind_filtered_data(), rv$contest_data)
    threshold <- quantile(rv$contest_data$Points, 0.99, na.rm = TRUE)
    sum(rewind_filtered_data()$ActualScore >= threshold, na.rm = TRUE)
  })
  
  output$rewind_top5 <- renderText({
    req(rewind_filtered_data(), rv$contest_data)
    threshold <- quantile(rv$contest_data$Points, 0.95, na.rm = TRUE)
    sum(rewind_filtered_data()$ActualScore >= threshold, na.rm = TRUE)
  })
  
  output$rewind_top10 <- renderText({
    req(rewind_filtered_data(), rv$contest_data)
    threshold <- quantile(rv$contest_data$Points, 0.90, na.rm = TRUE)
    sum(rewind_filtered_data()$ActualScore >= threshold, na.rm = TRUE)
  })
  
  output$rewind_top20 <- renderText({
    req(rewind_filtered_data(), rv$contest_data)
    threshold <- quantile(rv$contest_data$Points, 0.80, na.rm = TRUE)
    sum(rewind_filtered_data()$ActualScore >= threshold, na.rm = TRUE)
  })
  
  output$rewind_avg <- renderText({
    req(rewind_filtered_data())
    round(mean(rewind_filtered_data()$ActualScore, na.rm = TRUE), 1)
  })
  
  output$rewind_best <- renderText({
    req(rewind_filtered_data())
    round(max(rewind_filtered_data()$ActualScore, na.rm = TRUE), 1)
  })
  
  output$rewind_dupe_dist <- renderPlotly({
    req(rewind_filtered_data())
    
    data <- rewind_filtered_data()
    dupe_counts <- data[, .N, by = UniquePlayerCount][order(UniquePlayerCount)]
    dupe_counts[, Percentage := round(N / sum(N) * 100, 1)]
    
    plot_ly(dupe_counts, x = ~UniquePlayerCount, y = ~N, type = 'bar',
            marker = list(color = '#FFE500', line = list(color = '#000000', width = 1)),
            text = ~paste0(N, " (", Percentage, "%)"),
            textposition = 'outside') %>%
      layout(
        xaxis = list(title = "Unique Players", color = '#FFFFFF', tickvals = 1:6),
        yaxis = list(title = "Count", color = '#FFFFFF'),
        paper_bgcolor = '#000000',
        plot_bgcolor = '#1a1a1a',
        font = list(color = '#FFFFFF'),
        margin = list(t = 20)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)