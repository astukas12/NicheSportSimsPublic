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
            h1(class = "app-title", "MMA Contest Sweat"),
            p(class = "app-subtitle", "Golden Ticket DFS Analytics")
          )
      )
  ),
  
  # Upload Panel
  div(class = "upload-panel",
      fluidRow(
        column(8,
               fileInput("file", 
                         label = div(style = "color: #FFE500; font-weight: 600; font-size: 16px;",
                                     "Upload Contest CSV"),
                         accept = c("text/csv", ".csv"),
                         buttonLabel = "Browse...",
                         placeholder = "No file selected")
        ),
        column(4,
               div(style = "margin-top: 25px;",
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
    fighters = NULL,
    player_scores = NULL,
    eliminated_fighters = character(0),
    lineup_fighters_cache = list(),
    all_usernames = NULL
  )
  
  # Fast fighter extraction with caching
  extract_fighters <- function(lineup) {
    if (!lineup %in% names(rv$lineup_fighters_cache)) {
      fighters <- unlist(strsplit(lineup, " F ", fixed = TRUE))
      fighters <- trimws(sub("^F ", "", fighters))
      fighters <- fighters[nzchar(fighters)]
      rv$lineup_fighters_cache[[lineup]] <- fighters
    }
    rv$lineup_fighters_cache[[lineup]]
  }
  
  # Load and process data
  observeEvent(input$file, {
    req(input$file)
    
    withProgress(message = 'Loading contest data...', value = 0, {
      
      incProgress(0.2, detail = "Reading CSV...")
      
      tryCatch({
        # Fast read with data.table
        dt <- fread(input$file$datapath, 
                    showProgress = FALSE,
                    fill = TRUE,
                    sep = ",",
                    na.strings = c("", "NA", "NULL"))
        
        incProgress(0.2, detail = "Processing lineups...")
        
        # Force column names if not found (original approach)
        if (!"EntryName" %in% names(dt)) {
          names(dt)[3] <- "EntryName"
        }
        if (!"Lineup" %in% names(dt)) {
          names(dt)[6] <- "Lineup"
        }
        
        # Find Points column
        points_col <- grep("Points", names(dt), value = TRUE)[1]
        
        # Extract data
        rv$data <- dt[, .(
          Username = trimws(sub(" \\(.*", "", EntryName)),
          Lineup = trimws(Lineup),
          Points = as.numeric(get(points_col))
        )][!is.na(Username) & !is.na(Lineup) & Username != "EntryName" & Username != ""]
        
        setkey(rv$data, Username, Lineup)
        
        incProgress(0.2, detail = "Extracting player scores...")
        
        # Extract player scores if Player and FPTS columns exist
        # Also get %Drafted for field exposure
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
        } else {
          # Try column positions as fallback
          if (ncol(dt) >= 11) {
            player_data <- dt[, .(
              Player = trimws(.SD[[8]]),
              FPTS = as.numeric(.SD[[11]])
            )][!is.na(Player) & Player != "" & Player != "Player"]
            
            rv$player_scores <- player_data[, .(FPTS = max(FPTS, na.rm = TRUE)), by = Player]
            setkey(rv$player_scores, Player)
          }
        }
        
        incProgress(0.2, detail = "Extracting fighters...")
        
        # Extract all unique fighters
        all_lineups <- unique(rv$data$Lineup)
        all_fighters <- unique(unlist(lapply(all_lineups, extract_fighters)))
        rv$fighters <- sort(all_fighters)
        
        incProgress(0.2, detail = "Complete!")
        
        # Update username dropdown - extract and clean usernames
        usernames <- unique(rv$data$Username)
        
        # Filter out anything that looks like it came from the lineup column
        usernames <- usernames[!grepl("^F ", usernames)]
        
        # Filter out any that match known fighters
        usernames <- usernames[!usernames %in% rv$fighters]
        
        # Remove any empty or NA values
        usernames <- usernames[!is.na(usernames) & nzchar(usernames)]
        
        # Sort alphabetically
        usernames <- sort(usernames)
        
        # Store in reactive value for server-side search
        rv$all_usernames <- usernames
        
        # Update with server-side selectize (only shows matches as you type)
        updateSelectizeInput(session, "username", 
                             choices = usernames,
                             server = TRUE)
        
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
      h4("Your Fighter Exposure vs Field"),
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
    
    tagList(
      div(class = "well",
          h4("Fighter Status - Click to Mark as Eliminated", style = "margin-top: 0;"),
          p("Fighters with current scores shown. Click to mark as eliminated.", 
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
        h4("Live Fighters Distribution - You vs Field"),
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
    display_table <- dist_table[, .(
      `Live Fighters` = live,
      `Your Lineups` = You,
      `You %`,
      `Field Lineups` = Field,
      `Field %`
    )][order(-`Live Fighters`)]
    
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
}

# Run the application
shinyApp(ui = ui, server = server)