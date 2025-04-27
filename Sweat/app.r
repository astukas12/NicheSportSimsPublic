# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, dplyr, tidyr, ggplot2, DT, writexl, plotly, shinyWidgets, data.table)

# Configure maximum file upload size (increase to 100MB)
options(shiny.maxRequestSize = 100*1024^2)

# UI Definition
ui <- fluidPage(
  titlePanel("NASCAR Contest Sweat Tool"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Contest CSV",
                accept = c("text/csv", "application/vnd.ms-excel")),
      checkboxInput("results_final", "Contest Results are Final", FALSE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Contest Analysis",
                 h4("Entry Group Analysis"),
                 DTOutput("entry_analysis_table"),
                 
                 br(),
                 h4("Most Duplicated Lineups"),
                 DTOutput("most_duped_table")
        ),
        
        tabPanel("Driver Combinations",
                 div(class = "well",
                     numericInput("combo_size", "Number of Drivers in Combination", 
                                  value = 2, min = 2, max = 6),
                     actionButton("analyze_combos", "Analyze Combinations", 
                                  class = "btn-primary")
                 ),
                 
                 br(),
                 h4("Most Common Driver Combinations"),
                 DTOutput("driver_combos_table")
        ),
        
        tabPanel("Personal Analysis",
                 div(class = "well",
                     textInput("personal_username", "Your Username"),
                     div(style = "display: flex; gap: 10px;",
                         actionButton("analyze_personal", "Analyze My Entries", 
                                      class = "btn-primary"),
                         actionButton("clear_personal", "Clear", 
                                      class = "btn-secondary")
                     )
                 ),
                 
                 conditionalPanel(
                   condition = "input.personal_username != ''",
                   h4("Your Driver Exposure vs Field"),
                   DTOutput("personal_exposure_table"),
                   
                   br(),
                   h4("Your Lineup Duplicates Distribution"),
                   plotlyOutput("personal_dupes_plot")
                 )
        ),
        
        tabPanel("User Comparison",
                 div(class = "well",
                     pickerInput(
                       inputId = "selected_users",
                       label = "Select Users to Compare", 
                       choices = NULL,
                       multiple = TRUE,
                       options = list(`live-search` = TRUE)
                     )
                 ),
                 
                 conditionalPanel(
                   condition = "input.selected_users.length > 0",
                   h4("Selected Users Entry Summary"),
                   DTOutput("user_comparison_table"),
                   
                   br(),
                   h4("Lineup Duplicates Distribution"),
                   plotlyOutput("user_dupes_plot"),
                   
                   br(),
                   h4("Driver Exposure Comparison"),
                   plotlyOutput("exposure_comparison_plot")
                 )
        ),
        
        tabPanel("Final Results",
                 conditionalPanel(
                   condition = "input.results_final == true",
                   div(class = "well",
                       h4("Entry Group Performance"),
                       plotlyOutput("group_score_distribution"),
                       
                       br(),
                       h4("Performance by Duplication Level"),
                       plotlyOutput("duplication_performance"),
                       
                       br(),
                       h4("Top Performers"),
                       DTOutput("top_performers_table")
                   )
                 )
        ),
        
        tabPanel("Live Lineups",
                 div(class = "well",
                     h4("Select Dead Drivers"),
                     pickerInput(
                       inputId = "dead_drivers",
                       label = NULL, 
                       choices = character(0),  # Initialize with empty vector
                       multiple = TRUE,
                       options = list(
                         `live-search` = TRUE,
                         `actions-box` = TRUE,
                         `selected-text-format` = "count > 3"
                       )
                     ),
                     actionButton("update_dead", "Update Dead Drivers", 
                                  class = "btn-primary mt-2")
                 ),
                 
                 h4("Field Analysis"),
                 DTOutput("field_live_analysis"),
                 
                 br(),
                 div(class = "panel panel-default",
                     div(class = "panel-heading",
                         h4("User Analysis", class = "panel-title")),
                     div(class = "panel-body",
                         div(class = "row",
                             div(class = "col-sm-6",
                                 textInput("live_username", "Enter Username")),
                             div(class = "col-sm-6",
                                 div(style = "margin-top: 25px;",
                                     actionButton("analyze_live_user", "Analyze User", 
                                                  class = "btn-primary")))
                         ),
                         conditionalPanel(
                           condition = "input.live_username != ''",
                           div(style = "margin-top: 20px;",
                               h4("Lineups for User: ", textOutput("current_user", inline = TRUE)),
                               div(class = "row",
                                   div(class = "col-sm-3",
                                       h5(strong("Summary:")),
                                       uiOutput("live_summary")),
                                   div(class = "col-sm-9",
                                       DTOutput("user_live_analysis"))
                               )
                           )
                         )
                     )
                 )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  library(memoise)
  
  # Initialize reactive values
  rv <- reactiveValues(
    data = NULL,
    entry_groups = data.frame(
      group = c("Single Entry", "Small Multi", "Medium Multi", "Large Multi", "Max Entry"),
      min_entries = c(1, 2, 21, 101, 150),
      max_entries = c(1, 20, 100, 149, 150)
    ),
    all_drivers = NULL,
    current_dead_drivers = NULL,
    lineup_counts = NULL,
    user_entries = NULL,
    user_groups = NULL,
    lineup_drivers = list(),  # Changed from new.env() to list()
    duplication_groups = NULL,
    personal_user = NULL,    # Added missing values
    live_user = NULL        # Added missing values
  )
  
  # Clean driver names function - FIXED
  clean_driver_names <- function(lineup) {
    # Check if we already processed this lineup
    if (!lineup %in% names(rv$lineup_drivers)) {
      drivers <- unlist(strsplit(lineup, " D | D "))
      drivers <- drivers[drivers != ""]
      drivers <- trimws(drivers)
      drivers <- sub("^D ", "", drivers)
      # Store in our list instead of environment
      rv$lineup_drivers[[lineup]] <- drivers
    }
    
    # Return the cached drivers
    return(rv$lineup_drivers[[lineup]])
  }
  
  # File upload handler
  observeEvent(input$file, {
    req(input$file)
    withProgress(message = 'Loading data...', value = 0, {
      tryCatch({
        raw_data <- fread(input$file$datapath,
                          showProgress = FALSE,
                          fill = TRUE,
                          sep = ",",
                          na.strings = c("", "NA", "NULL"))
        
        if (!"EntryName" %in% names(raw_data)) {
          names(raw_data)[3] <- "EntryName"
        }
        if (!"Lineup" %in% names(raw_data)) {
          names(raw_data)[6] <- "Lineup"
        }
        
        # Convert to data.table and process
        rv$data <- as.data.table(raw_data)[, .(
          EntryName = trimws(sub(" \\(.*", "", EntryName)),
          Lineup = trimws(Lineup),
          Points = as.numeric(get(grep("Points", names(raw_data), value = TRUE)[1]))
        )][!is.na(EntryName) & !is.na(Lineup)]
        
        # Pre-calculate common values
        rv$lineup_counts <- rv$data[, .N, by = Lineup]
        setkey(rv$lineup_counts, Lineup)
        
        # Pre-calculate user entries and groups
        rv$user_entries <- rv$data[, .(
          entries = .N,
          group = fcase(
            .N == 1, "Single Entry",
            .N <= 20, "Small Multi",
            .N <= 100, "Medium Multi",
            .N <= 149, "Large Multi",
            default = "Max Entry"
          )
        ), by = EntryName]
        setkey(rv$user_entries, EntryName)
        
        # Pre-calculate duplication groups
        rv$duplication_groups <- rv$data[, .(
          dupe_count = .N,
          dupe_group = fcase(
            .N == 1, "Unique",
            .N <= 5, "2-5 Copies",
            .N <= 10, "6-10 Copies",
            .N <= 20, "11-20 Copies",
            default = "20+ Copies"
          )
        ), by = Lineup]
        setkey(rv$duplication_groups, Lineup)
        
        # Update picker input
        updatePickerInput(
          session,
          "selected_users",
          choices = setNames(
            rv$user_entries$EntryName,
            sprintf("%s (%d entries)", 
                    rv$user_entries$EntryName, 
                    rv$user_entries$entries)
          )
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error loading file:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })
  })
  
  # Entry Analysis Table
  output$entry_analysis_table <- renderDT({
    req(rv$data)
    
    total_users <- n_distinct(rv$data$EntryName)
    
    entry_analysis <- rv$data %>%
      group_by(EntryName) %>%
      summarise(entries = n()) %>%
      mutate(group = case_when(
        entries == 1 ~ "Single Entry (1 entry)",
        entries <= 20 ~ "Small Multi (2-20 entries)",
        entries <= 100 ~ "Medium Multi (21-100 entries)",
        entries <= 149 ~ "Large Multi (101-149 entries)",
        TRUE ~ "Max Entry (150 entries)"
      )) %>%
      group_by(group) %>%
      summarise(
        Users = n(),
        `% of Contest` = round(Users / total_users * 100, 1),
        `Total Entries` = sum(entries)
      )
    
    datatable(
      entry_analysis,
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        pageLength = 25
      ),
      extensions = 'Buttons'
    )
  })
  
  # Optimized Driver Combinations Analysis
  output$driver_combos_table <- renderDT({
    req(rv$data, input$analyze_combos)
    
    withProgress(message = 'Analyzing combinations...', value = 0, {
      # Validate combination size
      combo_size <- min(input$combo_size, 6)
      
      # Create a lookup for lineup counts (how many times each lineup appears)
      lineup_lookup <- as.data.frame(table(rv$data$Lineup))
      names(lineup_lookup) <- c("Lineup", "Count")
      
      # First get unique lineups to process
      unique_lineups <- unique(rv$data$Lineup)
      
      # Create a hash map to store combination counts
      combo_counts <- new.env(hash = TRUE, parent = emptyenv())
      
      # Process in batches for better UI responsiveness
      batch_size <- 500
      n_batches <- ceiling(length(unique_lineups) / batch_size)
      
      for (i in seq_len(n_batches)) {
        incProgress(1/n_batches, 
                    detail = paste("Processing batch", i, "of", n_batches))
        
        start_idx <- (i-1) * batch_size + 1
        end_idx <- min(i * batch_size, length(unique_lineups))
        
        batch_lineups <- unique_lineups[start_idx:end_idx]
        
        # Process this batch
        for (lineup in batch_lineups) {
          # Get the drivers in this lineup
          drivers <- clean_driver_names(lineup)
          
          # Only process if we have enough drivers
          if (length(drivers) >= combo_size) {
            # Get the count of this lineup in the full dataset
            lineup_count <- lineup_lookup$Count[lineup_lookup$Lineup == lineup]
            
            # Generate all combinations of the specified size
            if (combo_size > 1) {
              # For combinations of 2+ drivers, use combn
              combos <- combn(sort(drivers), combo_size, simplify = FALSE)
              for (combo in combos) {
                combo_key <- paste(combo, collapse = " | ")
                if (exists(combo_key, envir = combo_counts)) {
                  combo_counts[[combo_key]] <- combo_counts[[combo_key]] + lineup_count
                } else {
                  combo_counts[[combo_key]] <- lineup_count
                }
              }
            } else {
              # For single driver "combinations", just count each driver
              for (driver in drivers) {
                if (exists(driver, envir = combo_counts)) {
                  combo_counts[[driver]] <- combo_counts[[driver]] + lineup_count
                } else {
                  combo_counts[[driver]] <- lineup_count
                }
              }
            }
          }
        }
      }
      
      # Convert environment to data frame
      combo_names <- ls(combo_counts)
      combo_values <- sapply(combo_names, function(name) combo_counts[[name]])
      
      # Create result data frame
      result <- data.frame(
        Combination = combo_names,
        Count = combo_values,
        stringsAsFactors = FALSE
      ) %>%
        arrange(desc(Count)) %>%
        head(50) %>%
        mutate(`% of Lineups` = round(Count / nrow(rv$data) * 100, 2))
    })
    
    datatable(
      result,
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        pageLength = 25
      ),
      extensions = 'Buttons'
    )
  })
  
  # Most Duplicated Lineups Table
  output$most_duped_table <- renderDT({
    req(rv$data)
    
    dupes <- as.data.frame(table(rv$data$Lineup)) %>%
      setNames(c("Lineup", "Count")) %>%
      arrange(desc(Count)) %>%
      head(10) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, Lineup, Count)
    
    datatable(
      dupes,
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        pageLength = 10
      ),
      extensions = 'Buttons'
    )
  })
  
  # Personal Analysis
  observeEvent(input$analyze_personal, {
    req(rv$data, input$personal_username)
    rv$personal_user <- input$personal_username
  })
  
  observeEvent(input$clear_personal, {
    rv$personal_user <- NULL
    updateTextInput(session, "personal_username", value = "")
  })
  
  # Personal exposure table
  output$personal_exposure_table <- renderDT({
    req(rv$data, rv$personal_user)
    
    total_contest_entries <- nrow(rv$data)
    
    field_exposures <- rv$data %>%
      pull(Lineup) %>%
      sapply(clean_driver_names) %>%
      unlist() %>%
      table() %>%
      as.data.frame() %>%
      setNames(c("Driver", "FieldCount")) %>%
      mutate(
        `Field %` = round(FieldCount / total_contest_entries * 100, 1)
      )
    
    user_entries <- rv$data %>%
      filter(EntryName == rv$personal_user) %>%
      nrow()
    
    user_exposures <- rv$data %>%
      filter(EntryName == rv$personal_user) %>%
      pull(Lineup) %>%
      sapply(clean_driver_names) %>%
      unlist() %>%
      table() %>%
      as.data.frame() %>%
      setNames(c("Driver", "Count")) %>%
      mutate(
        `Your %` = round(Count / user_entries * 100, 1)
      )
    
    exposure_data <- full_join(user_exposures, field_exposures, by = "Driver") %>%
      mutate(
        Count = replace_na(Count, 0),
        FieldCount = replace_na(FieldCount, 0),
        `Your %` = replace_na(`Your %`, 0),
        `Field %` = replace_na(`Field %`, 0),
        `Leverage` = round(`Your %` - `Field %`, 1)
      ) %>%
      select(Driver, Count, `Your %`, `Field %`, Leverage) %>%
      arrange(desc(`Your %`))
    
    datatable(
      exposure_data,
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        pageLength = 25
      ),
      extensions = 'Buttons'
    ) %>%
      formatStyle(
        'Leverage',
        backgroundColor = styleInterval(
          c(-10, 10),
          c('#ffb3b3', '#ffffff', '#b3ffb3')
        )
      )
  })
  
  # Personal duplicates plot
  output$personal_dupes_plot <- renderPlotly({
    req(rv$data, rv$personal_user)
    
    lineup_counts <- table(rv$data$Lineup)
    
    user_dupes <- rv$data %>%
      filter(EntryName == rv$personal_user) %>%
      pull(Lineup) %>%
      sapply(function(x) lineup_counts[x]) %>%
      as.numeric()
    
    plot_ly(type = "box") %>%
      add_boxplot(y = user_dupes, name = rv$personal_user, 
                  boxpoints = "all", jitter = 0.3, pointpos = 0) %>%
      layout(
        yaxis = list(title = "Number of Times Lineup Used in Contest"),
        showlegend = FALSE
      )
  })
  
  # User Comparison
  output$user_comparison_table <- renderDT({
    req(rv$data, length(input$selected_users) > 0)
    
    comparison_data <- rv$data %>%
      filter(EntryName %in% input$selected_users) %>%
      group_by(EntryName) %>%
      summarise(
        `Total Entries` = n()
      )
    
    datatable(
      comparison_data,
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        pageLength = 25
      ),
      extensions = 'Buttons'
    )
  })
  
  # User comparison duplicates plot
  output$user_dupes_plot <- renderPlotly({
    req(rv$data, length(input$selected_users) > 0)
    
    lineup_counts <- table(rv$data$Lineup)
    
    user_dupes_data <- do.call(rbind, lapply(input$selected_users, function(user) {
      dupes <- rv$data %>%
        filter(EntryName == user) %>%
        pull(Lineup) %>%
        sapply(function(x) lineup_counts[x]) %>%
        as.numeric()
      
      data.frame(
        User = factor(user),  # Convert to factor
        Duplicates = dupes
      )
    }))
    
    p <- plot_ly(data = user_dupes_data, type = "box", 
                 x = ~User, y = ~Duplicates,
                 boxpoints = "all", jitter = 0.3, pointpos = 0)
    
    p <- p %>% layout(
      xaxis = list(title = "", type = "category"),  # Force categorical x-axis
      yaxis = list(title = "Number of Times Lineup Used in Contest")
    )
    
    p  # Return the plot directly instead of using ggplotly
  })
  
  # User comparison exposure plot
  output$exposure_comparison_plot <- renderPlotly({
    req(rv$data, length(input$selected_users) > 0)
    
    exposure_data <- do.call(rbind, lapply(input$selected_users, function(user) {
      user_entries <- nrow(filter(rv$data, EntryName == user))
      
      exposures <- rv$data %>%
        filter(EntryName == user) %>%
        pull(Lineup) %>%
        sapply(clean_driver_names) %>%
        unlist() %>%
        table() %>%
        as.data.frame() %>%
        setNames(c("Driver", "Count")) %>%
        mutate(
          EntryName = factor(user),  # Convert to factor
          Driver = factor(Driver),   # Convert to factor
          `Exposure %` = round(Count / user_entries * 100, 1)
        )
      
      return(exposures)
    }))
    
    plot_height <- max(600, 30 * length(unique(exposure_data$Driver)))
    
    p <- plot_ly(
      data = exposure_data,
      y = ~Driver,
      x = ~`Exposure %`,
      color = ~EntryName,
      type = "bar",
      orientation = 'h'
    )
    
    p <- p %>% layout(
      barmode = "group",
      xaxis = list(title = "Exposure %"),
      yaxis = list(title = "", type = "category"),  # Force categorical y-axis
      margin = list(l = 150),
      height = plot_height
    )
    
    p  # Return the plot directly
  })
  
  # Update driver list when data is loaded
  observe({
    req(rv$data)
    
    # Extract and clean driver names
    all_lineups <- rv$data$Lineup
    all_drivers <- character()
    
    for (lineup in all_lineups) {
      drivers <- clean_driver_names(lineup)
      all_drivers <- union(all_drivers, drivers)
    }
    
    # Sort and store drivers
    rv$all_drivers <- sort(all_drivers)
    
    # Update picker input
    updatePickerInput(
      session,
      "dead_drivers",
      choices = rv$all_drivers,
      selected = NULL
    )
  })
  
  # Observe dead drivers updates
  observeEvent(input$update_dead, {
    req(rv$data, input$dead_drivers)
    
    # Store current dead drivers in reactive values
    rv$current_dead_drivers <- input$dead_drivers
  })
  
  # Field Live Analysis
  output$field_live_analysis <- renderDT({
    req(rv$data, rv$current_dead_drivers)
    
    # Calculate live drivers for each lineup
    dt <- data.table(rv$data)
    
    dt[, LiveDrivers := sapply(Lineup, function(x) {
      drivers <- clean_driver_names(x)
      sum(!drivers %in% rv$current_dead_drivers)
    })]
    
    live_analysis <- dt[, .(
      Lineups = .N,
      `% of Field` = round(.N / nrow(dt) * 100, 1)
    ), by = LiveDrivers][order(-LiveDrivers)]
    
    # Add summary statistics
    live_analysis <- rbindlist(list(
      live_analysis,
      data.table(
        LiveDrivers = "AllLineups",
        Lineups = sum(live_analysis$Lineups),
        `% of Field` = 100
      )
    ))
    
    datatable(
      live_analysis,
      rownames = FALSE,  # This removes the row numbers
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        pageLength = 25,
        order = list(list(1, 'desc'))
      ),
      extensions = 'Buttons'
    )
  })
  
  # User Live Analysis
  observeEvent(input$analyze_live_user, {
    req(rv$data, input$live_username, length(input$dead_drivers) > 0)
    
    if (input$live_username == "") {
      showNotification("Please enter a username", type = "warning")
      return(NULL)
    }
    
    rv$live_user <- input$live_username
  })
  
  
  
  
  output$user_live_analysis <- renderDT({
    req(rv$data, rv$live_user, rv$current_dead_drivers)
    
    # Get user's lineups
    user_lineups <- rv$data[rv$data$EntryName == rv$live_user, ]
    
    if (nrow(user_lineups) == 0) {
      showNotification("No entries found for this username", type = "warning")
      return(NULL)
    }
    
    # Process each lineup
    lineup_data <- lapply(user_lineups$Lineup, function(lineup) {
      drivers <- clean_driver_names(lineup)
      live_drivers <- drivers[!drivers %in% rv$current_dead_drivers]
      dead_drivers <- drivers[drivers %in% rv$current_dead_drivers]
      
      return(data.frame(
        Lineup = lineup,
        LiveDrivers = length(live_drivers),
        LiveDriversList = paste(live_drivers, collapse = ", "),
        DeadDriversList = paste(dead_drivers, collapse = ", "),
        stringsAsFactors = FALSE
      ))
    })
    
    # Combine results
    lineup_df <- do.call(rbind, lineup_data)
    
    # Sort by number of live drivers descending
    lineup_df <- lineup_df[order(-lineup_df$LiveDrivers), ]
    
    # Create the table
    datatable(
      lineup_df,
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        pageLength = 10,
        order = list(list(1, 'desc'))
      ),
      extensions = 'Buttons'
    ) %>%
      formatStyle(
        'LiveDrivers',
        background = styleColorBar(c(0, max(lineup_df$LiveDrivers)), 'lightgreen'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'DeadDriversList',
        color = 'red',
        fontWeight = 'bold'
      )
  })
  
  # Display current user in Live Lineups
  output$current_user <- renderText({
    req(rv$live_user)
    rv$live_user
  })
  
  # User Live Summary 
  output$live_summary <- renderUI({
    req(rv$data, rv$live_user, rv$current_dead_drivers)
    
    # Get user's lineups
    user_lineups <- rv$data[rv$data$EntryName == rv$live_user, ]
    
    if (nrow(user_lineups) == 0) {
      return(NULL)
    }
    
    # Calculate summary statistics
    live_counts <- sapply(user_lineups$Lineup, function(lineup) {
      drivers <- clean_driver_names(lineup)
      sum(!drivers %in% rv$current_dead_drivers)
    })
    
    # Calculate summary stats
    total_entries <- length(live_counts)
    alive_entries <- sum(live_counts > 0)
    
    tagList(
      p(strong("Total Entries:"), total_entries),
      p(strong("Entries Still Alive:"), alive_entries, 
        sprintf("(%.1f%%)", alive_entries/total_entries*100)),
      p(strong("Entries Dead:"), total_entries - alive_entries,
        sprintf("(%.1f%%)", (total_entries - alive_entries)/total_entries*100))
    )
  })
  
  # Final Results Analysis
  
  # Memoize quantile calculations
  memoised_quantile <- memoise(quantile)
  
  # Create reactive for final results data
  results_data <- reactive({
    req(rv$data, input$results_final)
    rv$data
  })
  
  
  output$top_performers_table <- renderDT({
    req(rv$data)
    
    # Create a copy of the data table
    dt <- data.table::copy(rv$data)
    
    # Sort by Points in descending order and add rank
    dt <- dt[order(-Points)]
    dt[, Rank := .I]
    
    # Calculate thresholds for percentiles
    total_entries <- nrow(dt)
    threshold_0001 <- ceiling(total_entries * 0.001)
    threshold_01 <- ceiling(total_entries * 0.01)
    threshold_05 <- ceiling(total_entries * 0.05)
    threshold_20 <- ceiling(total_entries * 0.20)
    
    # Calculate user stats using data.table syntax
    user_stats <- dt[, .(
      Entries = .N,
      `Avg Score` = round(mean(Points), 1),
      `Max Score` = max(Points),
      `Top 0.01%` = sum(Rank <= threshold_0001) / .N * 100,  # Don't round here - will format later
      `Top 1%` = round(sum(Rank <= threshold_01) / .N * 100, 1), 
      `Top 5%` = round(sum(Rank <= threshold_05) / .N * 100, 1),
      `Top 20%` = round(sum(Rank <= threshold_20) / .N * 100, 1)
    ), by = EntryName]
    
    # Sort by average score
    user_stats <- user_stats[order(-`Avg Score`)]
    
    # Create the table with filtering
    datatable(
      user_stats,
      options = list(
        dom = 'Blfrtip',
        buttons = c('csv', 'excel'),
        pageLength = 25,
        lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All'))
      ),
      filter = 'top',
      extensions = c('Buttons')
    ) %>%
      formatRound(columns = "Top 0.01%", digits = 3) %>%  # Format the 0.01% column with 3 decimal places
      formatStyle(
        columns = c("Top 0.01%", "Top 1%", "Top 5%", "Top 20%"),
        background = styleColorBar(c(0, 100), "lightblue"),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  
  # Entry Group Score Distribution
  output$group_score_distribution <- renderPlotly({
    req(rv$data, input$results_final)
    
    group_data <- rv$data %>%
      group_by(EntryName) %>%
      mutate(
        entry_count = n(),
        group = case_when(
          entry_count == 1 ~ "Single Entry",
          entry_count <= 20 ~ "Small Multi",
          entry_count <= 100 ~ "Medium Multi",
          entry_count <= 149 ~ "Large Multi",
          TRUE ~ "Max Entry"
        )
      )
    
    p <- plot_ly(data = group_data, type = "box",
                 x = ~group,
                 y = ~Points,
                 boxpoints = "all",
                 jitter = 0.3,
                 pointpos = 0) %>%
      layout(
        title = "Score Distribution by Entry Group",
        xaxis = list(title = "Entry Group"),
        yaxis = list(title = "Points")
      )
    
    p
  })
  
  # Duplication Performance Analysis
  output$duplication_performance <- renderPlotly({
    req(rv$data, input$results_final)
    
    lineup_counts <- table(rv$data$Lineup)
    
    duplication_data <- rv$data %>%
      mutate(
        dupes = sapply(Lineup, function(x) lineup_counts[x]),
        dupe_group = case_when(
          dupes == 1 ~ "Unique",
          dupes <= 5 ~ "2-5 Copies",
          dupes <= 10 ~ "6-10 Copies",
          dupes <= 20 ~ "11-20 Copies",
          TRUE ~ "20+ Copies"
        )
      )
    
    p <- plot_ly(data = duplication_data, type = "box",
                 x = ~dupe_group,
                 y = ~Points,
                 boxpoints = "all",
                 jitter = 0.3,
                 pointpos = 0) %>%
      layout(
        title = "Score Distribution by Duplication Level",
        xaxis = list(title = "Duplication Level"),
        yaxis = list(title = "Points")
      )
    
    p
  })
}

# Run the app
shinyApp(ui = ui, server = server)