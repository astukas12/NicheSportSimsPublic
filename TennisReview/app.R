# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(stringr)
library(plotly)
library(shinyjs)
options(shiny.maxRequestSize = 40*1024^2)


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Tennis Sim Review"),
  
  # Sidebar with inputs
  dashboardSidebar(
    fileInput("sim_file", "Upload Sim Optimals File"),
    fileInput("contest_file", "Upload DK Contest File"),
    actionButton("process_btn", "Process Data", icon = icon("play")),
    textOutput("data_status"),
    br(),
    sidebarMenu(
      menuItem("Simulation Analysis", tabName = "sim_analysis", icon = icon("chart-line")),
      menuItem("Contest Analysis", tabName = "contest_analysis", icon = icon("trophy")),
      menuItem("All Users", tabName = "all_users", icon = icon("users")),
      menuItem("User Analysis", tabName = "user_analysis", icon = icon("users"))
    )
  ),
  
  # Main panel with outputs
  dashboardBody(
    useShinyjs(),
    tabItems(
      # Simulation Analysis Tab
      tabItem(tabName = "sim_analysis",
              h2("Simulation Lineup Analysis"),
              fluidRow(
                box(
                  title = "Top Simulation Lineups", width = 12, status = "success", solidHeader = TRUE,
                  DTOutput("sim_lineup_table")
                )
              ),
              fluidRow(
                box(
                  title = "Actual Score Distribution by Sim Rank", width = 12, status = "info", solidHeader = TRUE,
                  plotlyOutput("sim_score_by_rank", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Summary Statistics by Sim Rank", width = 12, status = "primary", solidHeader = TRUE,
                  DTOutput("rank_bucket_stats")
                )
              )
      ),
      
      # Contest Analysis Tab
      tabItem(tabName = "contest_analysis",
              h2("Contest Lineup Analysis"),
              fluidRow(
                box(
                  title = "Contest Lineups", width = 12, status = "success", solidHeader = TRUE,
                  DTOutput("contest_lineup_table")
                )
              ),
              fluidRow(
                box(
                  title = "Actual Score Distribution by Sim Rank", width = 12, status = "info", solidHeader = TRUE,
                  plotlyOutput("contest_score_by_rank", height = "400px")
                )
              ),
              fluidRow(
                box(
                  title = "Summary Statistics by Contest Rank", width = 12, status = "primary", solidHeader = TRUE,
                  DTOutput("contest_rank_bucket_stats")
                )
              )
      ),
      
      tabItem(tabName = "all_users",
              h2("All Users Analysis"),
              fluidRow(
                box(
                  title = "All Users Performance", width = 12, status = "info", solidHeader = TRUE,
                  DTOutput("all_users_table")
                )
              )
      ),
      tabItem(tabName = "user_analysis",
              h2("User Analysis"),
              fluidRow(
                box(
                  title = "User Selection", width = 12, status = "primary", solidHeader = TRUE,
                  selectizeInput("user_select", "Select User:", choices = NULL, multiple = FALSE, options = list(maxOptions = 10000)),
                  actionButton("load_user_btn", "Load User Data", icon = icon("user"))
                )
              ),
              fluidRow(
                box(
                  title = "User Summary Statistics", width = 6, status = "info", solidHeader = TRUE,
                  tableOutput("user_summary_stats")
                ),
                box(
                  title = "User Performance Distribution", width = 6, status = "success", solidHeader = TRUE,
                  plotlyOutput("user_performance_dist", height = "300px")
                )
              ),
              fluidRow(
                box(
                  title = "Sim Rank Distribution", width = 6, status = "warning", solidHeader = TRUE,
                  plotlyOutput("user_sim_rank_dist", height = "300px")
                ),
                box(
                  title = "Performance by Sim Rank", width = 6, status = "danger", solidHeader = TRUE,
                  plotlyOutput("user_perf_by_sim", height = "300px") 
                )
              ),
              fluidRow(
                box(
                  title = "User Lineups", width = 12, status = "primary", solidHeader = TRUE,
                  DTOutput("user_lineups_table")
                )
              )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Create reactive values to store processed data
  data_store <- reactiveValues(
    sim_data = NULL,
    contest_data = NULL,
    scored_sims = NULL,
    sim_with_keys = NULL,
    contest_formatted = NULL,
    contest_with_keys = NULL,
    matches = NULL,
    data_ready = FALSE
  )
  
  # Process files when the button is clicked
  observeEvent(input$process_btn, {
    req(input$sim_file, input$contest_file)
    
    # Display processing message
    output$data_status <- renderText("Processing data...")
    
    # STEP 1: Read and process contest data
    contest_file <- input$contest_file$datapath
    contest <- read.csv(contest_file)
    
    # Read scores section
    scores <- contest %>% 
      distinct(Player, Roster.Position, FPTS) %>% 
      filter(!is.na(FPTS))
    
    # Process scores
    Actuals <- scores %>%
      mutate(
        Player = str_trim(Player),
        ActualScore = FPTS
      )
    
    # STEP 2: Read and process simulation data
    sim_file <- input$sim_file$datapath
    sim_data <- read.csv(sim_file, stringsAsFactors = FALSE) %>% 
      mutate(
        # Use Top1Count instead of Top1Count for ranking
        Rank = min_rank(desc(Top1Count)),
        # Clean names by removing ID numbers
        Player1 = str_replace(Player1, "\\s*\\(\\d+\\)", ""),
        Player2 = str_replace(Player2, "\\s*\\(\\d+\\)", ""),
        Player3 = str_replace(Player3, "\\s*\\(\\d+\\)", ""),
        Player4 = str_replace(Player4, "\\s*\\(\\d+\\)", ""),
        Player5 = str_replace(Player5, "\\s*\\(\\d+\\)", ""),
        Player6 = str_replace(Player6, "\\s*\\(\\d+\\)", "")
      ) %>%
      rowwise() %>%
      mutate(
        # Sort Names alphabetically
        Players_sorted = list(sort(c(Player1, Player2, Player3, Player4, Player5, Player6))),
        
        # Assign sorted Names to columns
        Player1 = Players_sorted[1],
        Player2 = Players_sorted[2],
        Player3 = Players_sorted[3],
        Player4 = Players_sorted[4],
        Player5 = Players_sorted[5],
        Player6 = Players_sorted[6]
      ) %>%
      ungroup() %>%
      # Remove temporary column
      select(-Players_sorted)
    
    # STEP 3: Score simulation lineups
    # Create lookup for scores by name
    score_lookup <- Actuals %>%
      select(Player, ActualScore) %>%
      deframe()
    
    # Score each lineup
    scored_sims <- sim_data %>%
      mutate(
        Player1Score = score_lookup[Player1],
        Player2Score = score_lookup[Player2],
        Player3Score = score_lookup[Player3],
        Player4Score = score_lookup[Player4],
        Player5Score = score_lookup[Player5],
        Player6Score = score_lookup[Player6],
        
        TotalScore = Player1Score + Player2Score + Player3Score + 
          Player4Score + Player5Score + Player6Score
      ) %>%
      arrange(desc(TotalScore))
    
    # STEP 4: Parse contest lineups into same format
    contest_data <- contest %>% 
      select(Rank, EntryName, Points, Lineup)
    
    contest_formatted <- contest_data %>% 
      select(Rank, EntryName, Points)
    
    # Parse Players from Lineup
    contest_formatted$Player1 <- NA
    contest_formatted$Player2 <- NA
    contest_formatted$Player3 <- NA
    contest_formatted$Player4 <- NA
    contest_formatted$Player5 <- NA
    contest_formatted$Player6 <- NA
    
    # Process each lineup row by row
    for (i in 1:nrow(contest_data)) {
      # Get current lineup
      current_lineup <- contest_data$Lineup[i]
      
      # Split at " D " markers
      Player_split <- unlist(str_split(current_lineup, "P\\s+"))
      
      # Remove the first empty element
      Players <- str_trim(Player_split[Player_split != ""])
      
      # Sort Players to match simulation format
      Players_sorted <- sort(Players)
      
      # Assign to the correct row in contest_formatted
      if (length(Players_sorted) >= 1) contest_formatted$Player1[i] <- Players_sorted[1]
      if (length(Players_sorted) >= 2) contest_formatted$Player2[i] <- Players_sorted[2]
      if (length(Players_sorted) >= 3) contest_formatted$Player3[i] <- Players_sorted[3]
      if (length(Players_sorted) >= 4) contest_formatted$Player4[i] <- Players_sorted[4]
      if (length(Players_sorted) >= 5) contest_formatted$Player5[i] <- Players_sorted[5]
      if (length(Players_sorted) >= 6) contest_formatted$Player6[i] <- Players_sorted[6]
    }
    
    # STEP 5: Create matching lineup keys for comparison
    sim_with_keys <- scored_sims %>%
      mutate(
        # Create lineup key by combining columns
        lineup_key = paste(
          Player1, Player2, Player3, Player4, Player5, Player6,
          sep = "|"
        )
      ) %>%
      select(lineup_key, TotalScore, Top1Count, Rank, Player1, 
             Player2, Player3, Player4, Player5, Player6,
             Top1Count, Top2Count, Top3Count, Top5Count)
    
    # Add lineup keys for contest lineups
    contest_with_keys <- contest_formatted %>%
      mutate(
        # Create lineup key with the same structure
        lineup_key = paste(
          Player1, Player2, Player3, Player4, Player5, Player6, 
          sep = "|"
        )
      ) %>%
      select(lineup_key, Rank, EntryName, Points, Player1, 
             Player2, Player3, Player4, Player5, Player6) %>% 
      filter(Points > 0)
    
    # Try to match lineups
    matches <- contest_with_keys %>%
      left_join(sim_with_keys, by = "lineup_key", suffix = c("_contest", "_sim"))
    
    # Count frequency of each lineup in the contest
    contest_count <- contest_with_keys %>%
      group_by(lineup_key) %>%
      summarize(contest_count = n(), .groups = "drop")
    
    # Add contest frequency to contest data
    contests_with_count <- matches %>%
      left_join(contest_count, by = "lineup_key")
    
    # Add play frequency to simulations
    play_frequency <- contest_with_keys %>%
      group_by(lineup_key) %>%
      summarize(play_count = n(), .groups = "drop")
    
    sim_with_keys <- sim_with_keys %>%
      left_join(play_frequency, by = "lineup_key") %>%
      mutate(play_count = ifelse(is.na(play_count), 0, play_count))
    
    # Store processed data
    data_store$sim_data <- sim_data
    data_store$contest_data <- contest_data
    data_store$scored_sims <- scored_sims
    data_store$sim_with_keys <- sim_with_keys
    data_store$contest_formatted <- contest_formatted
    data_store$contest_with_keys <- contest_with_keys
    data_store$matches <- contests_with_count
    data_store$data_ready <- TRUE
    
    # Show success message
    output$data_status <- renderText("Data processed successfully!")
  })
  
  filtered_sim_data <- reactive({
    req(data_store$sim_with_keys)
    req(data_store$data_ready)
    
    # Simply return the data without filtering
    return(data_store$sim_with_keys)
  })
  
  output$sim_lineup_table <- renderDT({
    req(filtered_sim_data())
    
    filtered_sim_data() %>%
      arrange(desc(TotalScore)) %>%
      mutate(
        In_Contest = ifelse(play_count > 0, "Yes", "No"),
        TotalScore = round(TotalScore, 2)
      ) %>%
      select(
        Rank, 
        Player1, 
        Player2, 
        Player3, 
        Player4, 
        Player5,
        Player6,
        TotalScore, 
        Top1Count,
        Top1Count,
        Top3Count,
        Top5Count,
        In_Contest,
        ContestCount = play_count
      ) %>%
      datatable(
        options = list(
          pageLength = 50,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf')
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
      formatStyle(
        'In_Contest',
        backgroundColor = styleEqual(c("Yes", "No"), c('#a3e4d7', '#f5b7b1'))
      ) %>%
      formatStyle(
        'TotalScore',
        background = styleColorBar(c(0, max(filtered_sim_data()$TotalScore)), '#90caf9'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Top1Count',
        background = styleColorBar(c(0, max(filtered_sim_data()$Top1Count)), '#a5d6a7'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'ContestCount',
        background = styleColorBar(c(0, max(filtered_sim_data()$play_count)), '#ffcc80'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Actual score distribution by individual Top1Count (horizontal box plot)
  output$sim_score_by_rank <- renderPlotly({
    req(data_store$sim_with_keys)
    
    
    # Filter data to include only Top1Count values with enough data points
    plot_data <- data_store$sim_with_keys %>%
      mutate(
        # Convert Top1Count to factor and order by value (descending)
        Top1Count_factor = factor(Top1Count, levels = sort(unique(Top1Count), decreasing = TRUE))
      )
    
    # Create horizontal box plot
    plot_ly(plot_data, y = ~Top1Count_factor, x = ~TotalScore, 
            type = 'box', orientation = 'h',
            name = 'Score Distribution',
            marker = list(color = '#5e35b1'),
            boxmean = TRUE) %>%
      layout(
        title = "Actual Score Distribution by Top1Count",
        yaxis = list(title = "Top1Count", automargin = TRUE),
        xaxis = list(title = "Actual Score", automargin = TRUE),
        margin = list(l = 120, r = 50, b = 80, t = 70)
      )
  })
  
  # Table view for individual Optimal Count stats
  output$rank_bucket_stats <- renderDT({
    req(data_store$sim_with_keys)
    
    # Find the most common Top1Count values (top 20)
    top_counts <- data_store$sim_with_keys %>%
      count(Top1Count) %>%
      arrange(desc(n)) %>%
      pull(Top1Count)
    
    # Create summary table for these values
    data_store$sim_with_keys %>%
      filter(Top1Count %in% top_counts) %>%
      group_by(Top1Count) %>%
      summarize(
        `Lineup Count` = n(),
        `Avg Score` = round(mean(TotalScore, na.rm = TRUE), 2),
        `Avg Top3 Count` = round(mean(Top3Count, na.rm = TRUE), 2),
        `Avg Top5 Count` = round(mean(Top5Count, na.rm = TRUE), 2),
        `Played in Contest %` = round(mean(play_count > 0, na.rm = TRUE) * 100, 2),
        .groups = "drop"
      ) %>%
      arrange(desc(Top1Count)) %>%
      datatable(
        options = list(
          dom = 't',
          scrollX = TRUE,
          pageLength = -1
        ),
        rownames = FALSE
      ) %>%
      formatStyle(
        'Played in Contest %',
        background = styleColorBar(c(0, 100), '#ffcc80'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Avg Score',
        background = styleColorBar(c(0, max(.$`Avg Score`)), '#90caf9'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) 
  })
  
  # Render contest lineup table
  output$contest_lineup_table <- renderDT({
    req(data_store$matches)
    req(data_store$data_ready)
    
    data_store$matches %>%
      mutate(
        In_Simulation = ifelse(is.na(Top1Count), "No", "Yes"),
        Sim_Rank = ifelse(is.na(Rank_sim), NA, Rank_sim),
        Points = round(as.numeric(Points), 2)
      ) %>%
      select(
        Rank = Rank_contest,
        User = EntryName,
        Player1 = Player1_contest, 
        Player2 = Player2_contest, 
        Player3 = Player3_contest, 
        Player4 = Player4_contest, 
        Player5 = Player5_contest,
        Player6 = Player6_contest,
        Points, 
        ContestCount = contest_count, 
        In_Simulation,
        `Sim Rank` = Sim_Rank,
        Top1Count,
        Top1Count,
        Top3Count,
        Top5Count
      ) %>%
      datatable(
        options = list(
          pageLength = 50,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf')
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
      formatStyle(
        'In_Simulation',
        backgroundColor = styleEqual(c("Yes", "No"), c('#a3e4d7', '#f5b7b1'))
      ) %>%
      formatStyle(
        'Points',
        background = styleColorBar(c(0, max(as.numeric(data_store$matches$Points), na.rm = TRUE)), '#90caf9'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Top1Count',
        background = styleColorBar(c(0, max(data_store$matches$Top1Count, na.rm = TRUE)), '#a5d6a7'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'ContestCount',
        background = styleColorBar(c(0, max(data_store$matches$contest_count, na.rm = TRUE)), '#ffcc80'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Score distribution by Top1Count for Contest Lineups (including "Not in Sims")
  output$contest_score_by_rank <- renderPlotly({
    req(data_store$matches)
    
    # Make a copy of the data to avoid modifying the original
    plot_data <- data_store$matches
    
    # Handle NA values explicitly
    plot_data <- plot_data %>%
      mutate(
        # Create a factor for Top1Count (handling NA)
        Top1Count_label = ifelse(is.na(Top1Count), "Not in Sims", as.character(Top1Count)),
        # For sorting purposes
        Top1Count_numeric = ifelse(is.na(Top1Count), -1, as.numeric(Top1Count)),
        # Ensure Points is numeric
        Points = as.numeric(Points)
      )
    
    # Get level order - "Not in Sims" first, then descending numeric order
    numeric_levels <- plot_data %>%
      filter(Top1Count_label != "Not in Sims") %>%
      pull(Top1Count_numeric) %>%
      unique() %>%
      sort(decreasing = TRUE) %>%
      as.character()
    
    level_order <- c("Not in Sims", numeric_levels)
    
    # Create ordered factor with explicit levels
    plot_data$Top1Count_label <- factor(plot_data$Top1Count_label, levels = level_order)
    
    # Create horizontal box plot with explicit colors
    p <- plot_ly(plot_data, y = ~Top1Count_label, x = ~Points, 
                 type = 'box', orientation = 'h',
                 name = 'Score Distribution',
                 marker = list(color = '#5e35b1', opacity = 0.8),
                 line = list(color = '#3949ab', width = 2),
                 boxmean = TRUE)
    
    p %>% layout(
      title = "Contest Score Distribution by Top1Count",
      yaxis = list(
        title = "Top1Count", 
        automargin = TRUE,
        titlefont = list(size = 14),
        tickfont = list(size = 12)
      ),
      xaxis = list(
        title = "Contest Points", 
        automargin = TRUE,
        titlefont = list(size = 14),
        tickfont = list(size = 12)
      ),
      margin = list(l = 120, r = 50, b = 80, t = 80),
      paper_bgcolor = 'white',
      plot_bgcolor = 'white',
      hoverlabel = list(bgcolor = "white"),
      hovermode = "closest"
    )
  })
  
  # Summary statistics by Contest Rank for Contest Lineups
  output$contest_rank_bucket_stats <- renderDT({
    req(data_store$matches)
    
    # Get total count to calculate percentiles
    total_entries <- nrow(data_store$matches)
    
    # Calculate percentage thresholds
    top_1_pct_threshold <- ceiling(total_entries * 0.01)
    top_5_pct_threshold <- ceiling(total_entries * 0.05)
    top_10_pct_threshold <- ceiling(total_entries * 0.10)
    top_20_pct_threshold <- ceiling(total_entries * 0.20)
    top_50_pct_threshold <- ceiling(total_entries * 0.50)
    
    # Prepare data for summary table with contest rank buckets
    bucket_data <- data_store$matches %>%
      mutate(
        # Convert rank to numeric to ensure proper comparison
        Rank_num = as.numeric(Rank_contest),
        ContestRankBucket = case_when(
          Rank_num <= top_1_pct_threshold ~ "Top 1%",
          Rank_num <= top_5_pct_threshold ~ "Top 5%",
          Rank_num <= top_10_pct_threshold ~ "Top 10%",
          Rank_num <= top_20_pct_threshold ~ "Top 20%",
          Rank_num <= top_50_pct_threshold ~ "Top 50%",
          TRUE ~ "Bottom 50%"
        ),
        ContestRankBucket = factor(ContestRankBucket, 
                                   levels = c("Top 1%", "Top 5%", "Top 10%", "Top 20%", "Top 50%", "Bottom 50%"))
      ) %>%
      group_by(ContestRankBucket) %>%
      summarize(
        `Lineup Count` = n(),
        `In Simulations %` = round(mean(!is.na(Rank_sim)) * 100, 2),
        `Avg Optimal Count` = round(mean(Top1Count, na.rm = TRUE), 2),
        `Avg Top1 Count` = round(mean(Top1Count, na.rm = TRUE), 2),
        `Avg Top3 Count` = round(mean(Top3Count, na.rm = TRUE), 2),
        `Avg Top5 Count` = round(mean(Top5Count, na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Handle empty or zero values to prevent -Inf errors
    if(nrow(bucket_data) == 0 || all(is.na(bucket_data$`Avg Optimal Count`))) {
      max_opt_count <- 1
    } else {
      max_opt_count <- max(bucket_data$`Avg Optimal Count`, na.rm = TRUE)
    }
    
    bucket_data %>%
      datatable(
        options = list(
          dom = 't',
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
      formatStyle(
        'In Simulations %',
        background = styleColorBar(c(0, 100), '#a3e4d7'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Avg Optimal Count',
        background = styleColorBar(c(0, max_opt_count), '#a5d6a7'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Avg Top1 Count',
        background = styleColorBar(c(0, max(bucket_data$`Avg Top1 Count`, na.rm = TRUE)), '#ffcc80'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Avg Top3 Count',
        background = styleColorBar(c(0, max(bucket_data$`Avg Top3 Count`, na.rm = TRUE)), '#90caf9'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Avg Top5 Count',
        background = styleColorBar(c(0, max(bucket_data$`Avg Top5 Count`, na.rm = TRUE)), '#ce93d8'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Store user data when user is selected and load button is clicked
  user_filtered_data <- reactiveVal(NULL)
  
  observe({
    req(data_store$matches)
    
    # Parse usernames from EntryName format and count entries per user
    user_counts <- data_store$matches %>%
      mutate(
        # Extract username (everything before the space followed by an opening parenthesis)
        Username = str_replace(EntryName, "\\s+\\(.*\\)$", "")
      ) %>%
      group_by(Username) %>%
      summarize(Count = n(), .groups = "drop")
    
    # Create dropdown options with entry counts in parentheses
    user_choices <- setNames(
      user_counts$Username,
      paste0(user_counts$Username, " (", user_counts$Count, " entries)")
    )
    
    # Sort alphabetically
    user_choices <- user_choices[order(names(user_choices))]
    
    # Update selectize input with user choices
    updateSelectizeInput(session, "user_select", choices = user_choices, server = TRUE)
  })
  
  observeEvent(input$load_user_btn, {
    req(data_store$matches)
    req(input$user_select)
    
    # Get selected username (without the entries count)
    selected_username <- input$user_select
    
    # Filter matches data for the selected user
    user_lineups <- data_store$matches %>%
      mutate(
        Username = str_replace(EntryName, "\\s+\\(.*\\)$", ""),
        Lineup_Number = as.integer(str_extract(str_extract(EntryName, "\\(\\d+/\\d+\\)"), "\\d+")),
        Total_Lineups = as.integer(str_extract(str_extract(EntryName, "/\\d+\\)"), "\\d+"))
      ) %>%
      filter(Username == selected_username)
    
    # Calculate contest percentile based on this user's entries only
    total_entries <- nrow(user_lineups)
    contest_size <- max(as.numeric(user_lineups$Rank_contest), na.rm = TRUE)
    
    user_lineups <- user_lineups %>%
      mutate(
        Rank_num = as.numeric(Rank_contest),
        Percentile = round(Rank_num / contest_size * 100, 2),
        Points = as.numeric(Points),
        Is_Top_1pct = Percentile <= 1,
        Is_Top_5pct = Percentile <= 5,
        Is_Top_10pct = Percentile <= 10,
        Is_Top_20pct = Percentile <= 20,
        Is_Top_50pct = Percentile <= 50,
        In_Sims = !is.na(Rank_sim),
        In_Top_100_Sims = !is.na(Rank_sim) & Rank_sim <= 100,
        In_Top_500_Sims = !is.na(Rank_sim) & Rank_sim <= 500,
        In_Top_1000_Sims = !is.na(Rank_sim) & Rank_sim <= 1000,
        In_Top_2500_Sims = !is.na(Rank_sim) & Rank_sim <= 2500,
        In_Top_5000_Sims = !is.na(Rank_sim) & Rank_sim <= 5000,
        Sim_Rank_Bucket = case_when(
          is.na(Rank_sim) ~ "Not in Sims",
          Rank_sim <= 100 ~ "Top 100",
          Rank_sim <= 500 ~ "101-500",
          Rank_sim <= 1000 ~ "501-1000",
          Rank_sim <= 2500 ~ "1001-2500",
          Rank_sim <= 5000 ~ "2501-5000",
          TRUE ~ "5000+"
        )
      )
    
    # Store the filtered data
    user_filtered_data(user_lineups)
  })
  
  # User Summary Statistics Table
  output$user_summary_stats <- renderTable({
    req(user_filtered_data())
    
    user_data <- user_filtered_data()
    total_entries <- nrow(user_data)
    
    # Create summary statistics
    tibble(
      Metric = c(
        "Total Lineups", 
        "Average Score", 
        "Highest Score",
        "Top 1% Rate", 
        "Top 5% Rate", 
        "Top 10% Rate", 
        "Top 20% Rate",
        "% In Sim Database",
        "% In Top 100 Sims",
        "% In Top 500 Sims",
        "% In Top 1000 Sims",
        "% In Top 2500 Sims",
        "% In Top 5000 Sims",
        "Avg Sim Rank (when in sims)",
        "Avg Optimal Count",
        "Avg Top1 Count",
        "Avg Top3 Count",
        "Avg Top5 Count"
      ),
      Value = c(
        total_entries,
        round(mean(user_data$Points, na.rm = TRUE), 2),
        round(max(user_data$Points, na.rm = TRUE), 2),
        paste0(round(mean(user_data$Is_Top_1pct) * 100, 2), "%"),
        paste0(round(mean(user_data$Is_Top_5pct) * 100, 2), "%"),
        paste0(round(mean(user_data$Is_Top_10pct) * 100, 2), "%"),
        paste0(round(mean(user_data$Is_Top_20pct) * 100, 2), "%"),
        paste0(round(mean(user_data$In_Sims) * 100, 2), "%"),
        paste0(round(mean(user_data$In_Top_100_Sims) * 100, 2), "%"),
        paste0(round(mean(user_data$In_Top_500_Sims) * 100, 2), "%"),
        paste0(round(mean(user_data$In_Top_1000_Sims) * 100, 2), "%"),
        paste0(round(mean(user_data$In_Top_2500_Sims) * 100, 2), "%"),
        paste0(round(mean(user_data$In_Top_5000_Sims) * 100, 2), "%"),
        round(mean(user_data$Rank_sim[!is.na(user_data$Rank_sim)], na.rm = TRUE), 2),
        round(mean(user_data$Top1Count, na.rm = TRUE), 2),
        round(mean(user_data$Top1Count, na.rm = TRUE), 2),
        round(mean(user_data$Top3Count, na.rm = TRUE), 2),
        round(mean(user_data$Top5Count, na.rm = TRUE), 2)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # User Performance Distribution (Histogram of final scores)
  output$user_performance_dist <- renderPlotly({
    req(user_filtered_data())
    
    plot_data <- user_filtered_data()
    
    plot_ly(plot_data, x = ~Points, type = "histogram", 
            marker = list(color = "#1976D2", line = list(color = "white", width = 1))) %>%
      layout(
        title = "Distribution of User Scores",
        xaxis = list(title = "Points"),
        yaxis = list(title = "Count"),
        bargap = 0.1
      )
  })
  
  # User Sim Rank Distribution
  output$user_sim_rank_dist <- renderPlotly({
    req(user_filtered_data())
    
    # Prepare data: count lineups in each Sim Rank bucket
    plot_data <- user_filtered_data() %>%
      group_by(Sim_Rank_Bucket) %>%
      summarize(Count = n(), .groups = "drop") %>%
      # Ensure buckets are in correct order
      mutate(Sim_Rank_Bucket = factor(Sim_Rank_Bucket, 
                                      levels = c("Top 100", "101-500", "501-1000", "1001-2500", "2501-5000", "5000+", "Not in Sims")))
    
    # Create a pie chart
    plot_ly(plot_data, labels = ~Sim_Rank_Bucket, values = ~Count, 
            type = 'pie',
            marker = list(colors = c('#1e88e5', '#43a047', '#e53935', '#f9a825', '#8e24aa', '#5e35b1', '#757575')),
            textinfo = 'label+percent',
            hoverinfo = 'label+value+percent') %>%
      layout(
        title = "Sim Rank Distribution",
        showlegend = TRUE
      )
  })
  
  # Performance by Sim Rank (Box Plot)
  output$user_perf_by_sim <- renderPlotly({
    req(user_filtered_data())
    
    plot_data <- user_filtered_data() %>%
      # Ensure buckets are in correct order
      mutate(Sim_Rank_Bucket = factor(Sim_Rank_Bucket, 
                                      levels = c("Top 100", "101-500", "501-1000", "1001-2500", "2501-5000", "5000+", "Not in Sims")))
    
    # Create a box plot
    plot_ly(plot_data, y = ~Sim_Rank_Bucket, x = ~Points, 
            type = 'box', orientation = 'h',
            marker = list(color = '#5e35b1'),
            boxmean = TRUE) %>%
      layout(
        yaxis = list(title = "Sim Rank Bucket"),
        xaxis = list(title = "Contest Points"),
        margin = list(l = 120, r = 50, b = 80, t = 50)
      )
  })
  
  # User Lineups Table
  output$user_lineups_table <- renderDT({
    req(user_filtered_data())
    
    user_filtered_data() %>%
      mutate(
        In_Simulation = ifelse(is.na(Top1Count), "No", "Yes"),
        Sim_Rank = ifelse(is.na(Rank_sim), NA, Rank_sim),
        Points = round(as.numeric(Points), 2)
      ) %>%
      select(
        Rank = Rank_contest,
        Player1 = Player1_contest, 
        Player2 = Player2_contest, 
        Player3 = Player3_contest, 
        Player4 = Player4_contest, 
        Player5 = Player5_contest,
        Player6 = Player6_contest,
        Points, 
        `Contest Percentile` = Percentile,
        In_Simulation,
        `Sim Rank` = Sim_Rank,
        Top1Count,
        Top1Count,
        Top3Count,
        Top5Count
      ) %>%
      datatable(
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Brtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
      formatStyle(
        'In_Simulation',
        backgroundColor = styleEqual(c("Yes", "No"), c('#a3e4d7', '#f5b7b1'))
      ) %>%
      formatStyle(
        'Points',
        background = styleColorBar(c(0, max(as.numeric(user_filtered_data()$Points), na.rm = TRUE)), '#90caf9'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Top1Count',
        background = styleColorBar(c(0, max(user_filtered_data()$Top1Count, na.rm = TRUE)), '#a5d6a7'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )  %>%
      formatStyle(
        'Top3Count',
        background = styleColorBar(c(0, max(user_filtered_data()$Top3Count, na.rm = TRUE)), '#ce93d8'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Top5Count',
        background = styleColorBar(c(0, max(user_filtered_data()$Top5Count, na.rm = TRUE)), '#81d4fa'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Process all users data for the table
  output$all_users_table <- renderDT({
    req(data_store$matches)
    
    # Process the data to get per-user statistics
    all_users <- data_store$matches %>%
      mutate(
        # Extract username (everything before the space followed by an opening parenthesis)
        Username = str_replace(EntryName, "\\s+\\(.*\\)$", ""),
        # Convert numeric columns
        Points = as.numeric(Points),
        Rank_num = as.numeric(Rank_contest)
      ) %>%
      group_by(Username) %>%
      mutate(
        Total_Entries = n(),
        # Compute contest percentiles
        Contest_Size = max(as.numeric(Rank_contest), na.rm = TRUE),
        Percentile = Rank_num / Contest_Size * 100
      ) %>%
      summarize(
        `Entries` = n(),
        `AvgScore` = round(mean(Points, na.rm = TRUE), 2),
        # Calculate top percentages
        `T1%` = round(mean(Percentile <= 1, na.rm = TRUE) * 100, 2),
        `T5%` = round(mean(Percentile <= 5, na.rm = TRUE) * 100, 2),
        `T10%` = round(mean(Percentile <= 10, na.rm = TRUE) * 100, 2),
        `T20%` = round(mean(Percentile <= 20, na.rm = TRUE) * 100, 2),
        # Calculate simulation overlap metrics
        `Sim Lineups` = sum(!is.na(Rank_sim), na.rm = TRUE),
        `Sim %` = round(mean(!is.na(Rank_sim), na.rm = TRUE) * 100, 2),
        `T100 Sims` = sum(!is.na(Rank_sim) & Rank_sim <= 100, na.rm = TRUE),
        `T2000 Sims` = sum(!is.na(Rank_sim) & Rank_sim <= 2000, na.rm = TRUE),
        # Calculate average sim rank for sims in database
        `Avg Sim Rank` = round(mean(Rank_sim, na.rm = TRUE), 2),
        `Avg Optimal Count` = round(mean(Top1Count, na.rm = TRUE), 2),
        `Avg Top1 Count` = round(mean(Top1Count, na.rm = TRUE), 2),
        `Avg Top3 Count` = round(mean(Top3Count, na.rm = TRUE), 2),
        `Avg Top5 Count` = round(mean(Top5Count, na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    # Convert NA values to 0 or appropriate values to avoid errors
    all_users <- all_users %>%
      mutate(across(everything(), ~ifelse(is.na(.), 0, .)))
    
    # Create DataTable with filtering and sorting built-in
    datatable(
      all_users,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Blrtip',  # Added 'l' for length menu and 'f' for search/filter
        buttons = c('copy', 'csv', 'excel'),
        lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All'))
      ),
      filter = 'top',  # Add filters at the top of each column
      extensions = c('Buttons', 'Scroller'),  # Add scroller for better performance
      rownames = FALSE
    ) %>%
      formatStyle(
        'AvgScore',
        background = styleColorBar(c(0, max(all_users$AvgScore)), '#90caf9'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'T1%',
        background = styleColorBar(c(0, 100), '#f06292'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'T5%',
        background = styleColorBar(c(0, 100), '#ba68c8'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'T10%',
        background = styleColorBar(c(0, 100), '#9575cd'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'T20%',
        background = styleColorBar(c(0, 100), '#7986cb'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Sim Lineups',
        background = styleColorBar(c(0, max(all_users$`Sim Lineups`)), '#4fc3f7'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Avg Sim Rank',
        background = styleColorBar(c(1000, 1), '#80cbc4'),  # Reversed for ranks (lower is better)
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Avg Optimal Count',
        background = styleColorBar(c(0, max(all_users$`Avg Optimal Count`)), '#a5d6a7'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Avg Top1 Count',
        background = styleColorBar(c(0, max(all_users$`Avg Top1 Count`)), '#ffcc80'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Avg Top3 Count',
        background = styleColorBar(c(0, max(all_users$`Avg Top3 Count`)), '#81c784'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Avg Top5 Count',
        background = styleColorBar(c(0, max(all_users$`Avg Top5 Count`)), '#64b5f6'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)