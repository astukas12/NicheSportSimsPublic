# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(stringr)
library(shinyjs)
options(shiny.maxRequestSize = 40*1024^2)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "NASCAR Contest Analysis"),
  
  # Sidebar with inputs
  dashboardSidebar(
    fileInput("sim_file", "Upload Sim Optimals File"),
    fileInput("contest_file", "Upload DK Contest File"),
    actionButton("process_btn", "Process Data", icon = icon("play")),
    textOutput("data_status")
  ),
  
  # Main panel with outputs
  dashboardBody(
    useShinyjs(),
    h2("Contest Lineup Analysis"),
    fluidRow(
      box(
        title = "Contest Lineups with Ownership Analysis", width = 12, status = "success", solidHeader = TRUE,
        DTOutput("contest_lineup_table")
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Create reactive values to store processed data
  data_store <- reactiveValues(
    matches = NULL,
    contest_ownership = NULL,
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
        # Use Top1Count for ranking
        Rank = min_rank(desc(Top1Count)),
        # Clean names by removing ID numbers
        Driver1 = str_replace(Driver1, "\\s*\\(\\d+\\)", ""),
        Driver2 = str_replace(Driver2, "\\s*\\(\\d+\\)", ""),
        Driver3 = str_replace(Driver3, "\\s*\\(\\d+\\)", ""),
        Driver4 = str_replace(Driver4, "\\s*\\(\\d+\\)", ""),
        Driver5 = str_replace(Driver5, "\\s*\\(\\d+\\)", ""),
        Driver6 = str_replace(Driver6, "\\s*\\(\\d+\\)", "")
      ) %>%
      rowwise() %>%
      mutate(
        # Sort drivers alphabetically
        drivers_sorted = list(sort(c(Driver1, Driver2, Driver3, Driver4, Driver5, Driver6))),
        Driver1 = drivers_sorted[1],
        Driver2 = drivers_sorted[2],
        Driver3 = drivers_sorted[3],
        Driver4 = drivers_sorted[4],
        Driver5 = drivers_sorted[5],
        Driver6 = drivers_sorted[6]
      ) %>%
      ungroup() %>%
      select(-drivers_sorted)
    
    # STEP 3: Score simulation lineups
    score_lookup <- Actuals %>%
      select(Player, ActualScore) %>%
      deframe()
    
    scored_sims <- sim_data %>%
      mutate(
        Driver1Score = score_lookup[Driver1],
        Driver2Score = score_lookup[Driver2],
        Driver3Score = score_lookup[Driver3],
        Driver4Score = score_lookup[Driver4],
        Driver5Score = score_lookup[Driver5],
        Driver6Score = score_lookup[Driver6],
        TotalScore = Driver1Score + Driver2Score + Driver3Score + 
          Driver4Score + Driver5Score + Driver6Score
      ) %>%
      arrange(desc(TotalScore))
    
    # STEP 4: Parse contest lineups
    contest_data <- contest %>% 
      select(Rank, EntryName, Points, Lineup)
    
    contest_formatted <- contest_data %>% 
      select(Rank, EntryName, Points)
    
    # Parse drivers from Lineup
    contest_formatted$Driver1 <- NA
    contest_formatted$Driver2 <- NA
    contest_formatted$Driver3 <- NA
    contest_formatted$Driver4 <- NA
    contest_formatted$Driver5 <- NA
    contest_formatted$Driver6 <- NA
    
    for (i in 1:nrow(contest_data)) {
      current_lineup <- contest_data$Lineup[i]
      driver_split <- unlist(str_split(current_lineup, "D\\s+"))
      drivers <- str_trim(driver_split[driver_split != ""])
      drivers_sorted <- sort(drivers)
      
      if (length(drivers_sorted) >= 1) contest_formatted$Driver1[i] <- drivers_sorted[1]
      if (length(drivers_sorted) >= 2) contest_formatted$Driver2[i] <- drivers_sorted[2]
      if (length(drivers_sorted) >= 3) contest_formatted$Driver3[i] <- drivers_sorted[3]
      if (length(drivers_sorted) >= 4) contest_formatted$Driver4[i] <- drivers_sorted[4]
      if (length(drivers_sorted) >= 5) contest_formatted$Driver5[i] <- drivers_sorted[5]
      if (length(drivers_sorted) >= 6) contest_formatted$Driver6[i] <- drivers_sorted[6]
    }
    
    # STEP 5: Calculate actual ownership from contest data
    contest_with_keys <- contest_formatted %>%
      mutate(
        lineup_key = paste(Driver1, Driver2, Driver3, Driver4, Driver5, Driver6, sep = "|")
      ) %>%
      select(lineup_key, Rank, EntryName, Points, Driver1, Driver2, Driver3, Driver4, Driver5, Driver6) %>% 
      filter(Points > 0)
    
    # Calculate individual player ownership from contest
    total_contest_lineups <- nrow(contest_with_keys)
    
    contest_ownership <- contest_with_keys %>%
      pivot_longer(cols = c(Driver1, Driver2, Driver3, Driver4, Driver5, Driver6), 
                   names_to = "position", values_to = "player") %>%
      filter(!is.na(player)) %>%
      group_by(player) %>%
      summarize(
        contest_appearances = n(),
        contest_ownership_pct = round((n() / total_contest_lineups) * 100, 2),
        .groups = "drop"
      )
    
    # Function to calculate actual cumulative ownership for a lineup
    calculate_actual_ownership <- function(d1, d2, d3, d4, d5, d6, ownership_data) {
      drivers <- c(d1, d2, d3, d4, d5, d6)
      drivers <- drivers[!is.na(drivers)]
      
      ownerships <- sapply(drivers, function(x) {
        result <- ownership_data$contest_ownership_pct[ownership_data$player == x]
        if(length(result) == 0) return(0)
        return(result[1])
      })
      
      cumulative <- sum(ownerships, na.rm = TRUE)
      geometric <- ifelse(all(ownerships > 0), exp(mean(log(ownerships), na.rm = TRUE)), 0)
      
      return(list(cumulative = cumulative, geometric = geometric))
    }
    
    # Add actual ownership calculations to contest data
    contest_with_keys <- contest_with_keys %>%
      rowwise() %>%
      mutate(
        ownership_calc = list(calculate_actual_ownership(Driver1, Driver2, Driver3, Driver4, Driver5, Driver6, contest_ownership)),
        ActualCumulativeOwnership = ownership_calc$cumulative,
        ActualGeometricMeanOwnership = round(ownership_calc$geometric, 2)
      ) %>%
      ungroup() %>%
      select(-ownership_calc)
    
    # STEP 6: Create sim lookup
    sim_with_keys <- scored_sims %>%
      mutate(
        lineup_key = paste(Driver1, Driver2, Driver3, Driver4, Driver5, Driver6, sep = "|")
      )
    
    # Dynamically select available columns
    available_cols <- names(sim_with_keys)
    base_cols <- c("lineup_key", "TotalScore", "Top1Count", "Rank", "Driver1", 
                   "Driver2", "Driver3", "Driver4", "Driver5", "Driver6")
    optional_cols <- c("Top2Count", "Top3Count", "Top5Count", "TotalSalary", 
                       "CumulativeOwnership", "GeometricMean", "CumulativeStarting", 
                       "GeometricMeanStarting")
    
    cols_to_select <- c(base_cols, optional_cols[optional_cols %in% available_cols])
    sim_with_keys <- sim_with_keys %>% select(all_of(cols_to_select))
    
    # STEP 7: Match lineups and calculate ownership differences
    matches <- contest_with_keys %>%
      left_join(sim_with_keys, by = "lineup_key", suffix = c("_contest", "_sim")) %>%
      mutate(
        CumulativeOwnership_Diff = ifelse(!is.na(CumulativeOwnership), 
                                          ActualCumulativeOwnership - CumulativeOwnership, NA),
        GeometricMeanOwnership_Diff = ifelse(!is.na(GeometricMean), 
                                             ActualGeometricMeanOwnership - GeometricMean, NA)
      )
    
    # Count frequency of each lineup in the contest
    contest_count <- contest_with_keys %>%
      group_by(lineup_key) %>%
      summarize(contest_count = n(), .groups = "drop")
    
    matches <- matches %>%
      left_join(contest_count, by = "lineup_key")
    
    # Store processed data
    data_store$matches <- matches
    data_store$contest_ownership <- contest_ownership
    data_store$data_ready <- TRUE
    
    # Show success message
    output$data_status <- renderText("Data processed successfully!")
  })
  
  # Contest lineup table with ownership analysis
  output$contest_lineup_table <- renderDT({
    req(data_store$matches)
    req(data_store$data_ready)
    
    # Check which columns exist
    has_total_salary <- "TotalSalary" %in% names(data_store$matches)
    has_cumulative_ownership <- "CumulativeOwnership" %in% names(data_store$matches)
    has_geometric_mean <- "GeometricMean" %in% names(data_store$matches)
    has_cumulative_starting <- "CumulativeStarting" %in% names(data_store$matches)
    has_geometric_mean_starting <- "GeometricMeanStarting" %in% names(data_store$matches)
    has_top2_count <- "Top2Count" %in% names(data_store$matches)
    
    # Base data preparation
    display_data <- data_store$matches %>%
      mutate(
        In_Simulation = ifelse(is.na(Top1Count), "No", "Yes"),
        Sim_Rank = ifelse(is.na(Rank_sim), NA, Rank_sim),
        Points = round(as.numeric(Points), 2),
        ActualCumulativeOwnership = round(ActualCumulativeOwnership, 2),
        ActualGeometricMeanOwnership = round(ActualGeometricMeanOwnership, 2)
      )
    
    # Conditionally round columns if they exist
    if (has_cumulative_ownership) {
      display_data <- display_data %>% 
        mutate(CumulativeOwnership = round(CumulativeOwnership, 2))
    }
    if (has_geometric_mean) {
      display_data <- display_data %>% 
        mutate(GeometricMean = round(GeometricMean, 2))
    }
    if (has_geometric_mean_starting) {
      display_data <- display_data %>% 
        mutate(GeometricMeanStarting = round(GeometricMeanStarting, 2))
    }
    
    # Build column selection dynamically
    base_columns <- c(
      "Rank" = "Rank_contest",
      "User" = "EntryName",
      "Driver1" = "Driver1_contest", 
      "Driver2" = "Driver2_contest", 
      "Driver3" = "Driver3_contest", 
      "Driver4" = "Driver4_contest", 
      "Driver5" = "Driver5_contest",
      "Driver6" = "Driver6_contest",
      "Points" = "Points", 
      "ContestCount" = "contest_count", 
      "In_Simulation" = "In_Simulation",
      "Sim Rank" = "Sim_Rank",
      "Top1Count" = "Top1Count"
    )
    
    # Add optional sim columns
    if (has_top2_count) base_columns <- c(base_columns, "Top2Count" = "Top2Count")
    base_columns <- c(base_columns, "Top3Count" = "Top3Count", "Top5Count" = "Top5Count")
    if (has_total_salary) base_columns <- c(base_columns, "TotalSalary" = "TotalSalary")
    
    # Add ownership columns
    base_columns <- c(base_columns, "Actual Cumulative Own%" = "ActualCumulativeOwnership")
    base_columns <- c(base_columns, "Actual Geometric Own%" = "ActualGeometricMeanOwnership")
    
    # Add sim ownership predictions if they exist
    if (has_cumulative_ownership) {
      base_columns <- c(base_columns, "Sim Cumulative Own%" = "CumulativeOwnership")
      base_columns <- c(base_columns, "Cumulative Own% Diff" = "CumulativeOwnership_Diff")
    }
    if (has_geometric_mean) {
      base_columns <- c(base_columns, "Sim Geometric Own%" = "GeometricMean")
      base_columns <- c(base_columns, "Geometric Own% Diff" = "GeometricMeanOwnership_Diff")
    }
    
    # Add remaining optional columns
    if (has_cumulative_starting) base_columns <- c(base_columns, "CumulativeStarting" = "CumulativeStarting")
    if (has_geometric_mean_starting) base_columns <- c(base_columns, "GeometricMeanStarting" = "GeometricMeanStarting")
    
    # Select columns
    final_data <- display_data %>% select(all_of(unname(base_columns)))
    names(final_data) <- names(base_columns)
    
    # Create datatable
    dt <- datatable(
      final_data,
      options = list(
        pageLength = 50,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'csv', exportOptions = list(modifier = list(page = "all"))),
          list(extend = 'excel', exportOptions = list(modifier = list(page = "all")))
        ),
        serverSide = FALSE
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
        background = styleColorBar(c(0, max(as.numeric(display_data$Points), na.rm = TRUE)), '#90caf9'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Top1Count',
        background = styleColorBar(c(0, max(display_data$Top1Count, na.rm = TRUE)), '#a5d6a7'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'ContestCount',
        background = styleColorBar(c(0, max(display_data$contest_count, na.rm = TRUE)), '#ffcc80'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Actual Cumulative Own%',
        background = styleColorBar(c(0, max(final_data$`Actual Cumulative Own%`, na.rm = TRUE)), '#ffcdd2'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Actual Geometric Own%',
        background = styleColorBar(c(0, max(final_data$`Actual Geometric Own%`, na.rm = TRUE)), '#d1c4e9'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    
    # Add conditional styling for sim ownership if it exists
    if (has_cumulative_ownership && "Sim Cumulative Own%" %in% names(final_data)) {
      dt <- dt %>%
        formatStyle(
          'Sim Cumulative Own%',
          background = styleColorBar(c(0, max(final_data$`Sim Cumulative Own%`, na.rm = TRUE)), '#f8bbd9'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'Cumulative Own% Diff',
          background = styleInterval(c(-5, 5), c('#ffcdd2', '#fff3e0', '#c8e6c9')),
          color = styleInterval(c(-5, 5), c('#d32f2f', '#f57c00', '#388e3c'))
        )
    }
    
    if (has_geometric_mean && "Sim Geometric Own%" %in% names(final_data)) {
      dt <- dt %>%
        formatStyle(
          'Sim Geometric Own%',
          background = styleColorBar(c(0, max(final_data$`Sim Geometric Own%`, na.rm = TRUE)), '#b39ddb'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'Geometric Own% Diff',
          background = styleInterval(c(-2, 2), c('#ffcdd2', '#fff3e0', '#c8e6c9')),
          color = styleInterval(c(-2, 2), c('#d32f2f', '#f57c00', '#388e3c'))
        )
    }
    
    return(dt)
  })
}

# Run the app
shinyApp(ui = ui, server = server)