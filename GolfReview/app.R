# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, dplyr, tidyr, ggplot2, DT, writexl, plotly, shinyWidgets, data.table)

# Configure maximum file upload size (increase to 100MB)
options(shiny.maxRequestSize = 100*1024^2)

# UI Definition
ui <- fluidPage(
  titlePanel("Golf Contest Analysis Tool"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Contest CSV or MME Lineup File",
                accept = c("text/csv", "application/vnd.ms-excel")),
      
      # Show file type info when file is loaded
      conditionalPanel(
        condition = "output.fileInfo",
        div(class = "alert alert-info", 
            style = "margin-top: 10px;",
            strong("File Type: "), textOutput("fileInfo", inline = TRUE))
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Contest Analysis",
                 h4("Entry Group Analysis"),
                 DTOutput("entry_analysis_table"),
                 
                 br(),
                 h4("Most Duplicated Lineups"),
                 DTOutput("most_duped_table"),
                 
                 br(),
                 h4("Cut Performance Analysis"),
                 DTOutput("cut_analysis_table")
        ),
        
        tabPanel("Golfer Combinations",
                 div(class = "well",
                     numericInput("combo_size", "Number of Golfers in Combination", 
                                  value = 2, min = 2, max = 6),
                     actionButton("analyze_combos", "Analyze Combinations", 
                                  class = "btn-primary")
                 ),
                 
                 br(),
                 h4("Most Common Golfer Combinations"),
                 DTOutput("golfer_combos_table")
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
                   h4("Your Golfer Exposure vs Field"),
                   DTOutput("personal_exposure_table"),
                   
                   br(),
                   h4("Your Cut Performance vs Field"),
                   DTOutput("personal_cut_performance_table"),
                   
                   br(),
                   h4("Your Lineup Performance"),
                   DTOutput("personal_lineup_table"),
                   
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
                   h4("Cut Performance Comparison"),
                   DTOutput("user_cut_comparison_table"),
                   
                   br(),
                   h4("Lineup Duplicates Distribution"),
                   plotlyOutput("user_dupes_plot"),
                   
                   br(),
                   h4("Golfer Exposure Comparison"),
                   plotlyOutput("exposure_comparison_plot")
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
    golfer_data = NULL,
    file_type = NULL,  # Track whether it's "MME" or "Contest" file
    entry_groups = data.frame(
      group = c("Single Entry", "Small Multi", "Medium Multi", "Large Multi", "Max Entry"),
      min_entries = c(1, 2, 21, 101, 150),
      max_entries = c(1, 20, 100, 149, 150)
    ),
    all_golfers = NULL,
    current_missed_cut_golfers = NULL,
    lineup_counts = NULL,
    user_entries = NULL,
    user_groups = NULL,
    lineup_golfers = list(),
    duplication_groups = NULL,
    personal_user = NULL,
    lineup_metrics = NULL
  )
  
  # Clean golfer names function
  clean_golfer_names <- function(lineup) {
    if (!lineup %in% names(rv$lineup_golfers)) {
      if (rv$file_type == "MME") {
        # For MME files, split by " G " and clean up
        golfers <- unlist(strsplit(lineup, " G "))
        golfers <- golfers[golfers != "" & golfers != "G"]
        golfers <- trimws(golfers)
      } else {
        # Original contest file format
        golfers <- unlist(strsplit(lineup, " G | G "))
        golfers <- golfers[golfers != ""]
        golfers <- trimws(golfers)
        golfers <- sub("^G ", "", golfers)
      }
      rv$lineup_golfers[[lineup]] <- golfers
    }
    return(rv$lineup_golfers[[lineup]])
  }
  
  # Calculate lineup metrics (cuts made, total score, made-cut score)
  calculate_lineup_metrics <- function(lineup) {
    if (is.null(rv$golfer_data)) return(list(cuts_made = 0, total_score = 0, made_cut_score = 0))
    
    golfers <- clean_golfer_names(lineup)
    golfer_info <- rv$golfer_data[rv$golfer_data$Golfer %in% golfers, ]
    
    cuts_made <- sum(golfer_info$Cut == "Y", na.rm = TRUE)
    total_score <- sum(golfer_info$Score, na.rm = TRUE)
    made_cut_score <- sum(golfer_info$Score[golfer_info$Cut == "Y"], na.rm = TRUE)
    
    return(list(
      cuts_made = cuts_made,
      total_score = total_score,
      made_cut_score = made_cut_score
    ))
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
        
        # Detect file type based on columns
        if ("Player1" %in% names(raw_data) && "Player2" %in% names(raw_data)) {
          # This is an MME lineup file
          rv$file_type <- "MME"
          
          # Create lineup strings from Player columns
          player_cols <- paste0("Player", 1:6)
          existing_player_cols <- player_cols[player_cols %in% names(raw_data)]
          
          rv$data <- raw_data %>%
            mutate(
              EntryName = "MME_Generated",  # Single user for MME files
              Lineup = paste("G", get(existing_player_cols[1]), 
                             if(length(existing_player_cols) > 1) paste("G", get(existing_player_cols[2])) else "",
                             if(length(existing_player_cols) > 2) paste("G", get(existing_player_cols[3])) else "",
                             if(length(existing_player_cols) > 3) paste("G", get(existing_player_cols[4])) else "",
                             if(length(existing_player_cols) > 4) paste("G", get(existing_player_cols[5])) else "",
                             if(length(existing_player_cols) > 5) paste("G", get(existing_player_cols[6])) else "",
                             sep = " "),
              Lineup = gsub("\\s+", " ", trimws(Lineup)),  # Clean up extra spaces
              Points = ifelse("StandardProj" %in% names(raw_data), StandardProj, 0)
            ) %>%
            select(EntryName, Lineup, Points) %>%
            as.data.table()
          
          # Process golfer data (scores and cuts) - same as contest files since you'll paste this data
          if ("Golfer" %in% names(raw_data) && "Score" %in% names(raw_data) && "Cut" %in% names(raw_data)) {
            rv$golfer_data <- as.data.table(raw_data)[, .(
              Golfer = trimws(Golfer),
              Score = as.numeric(Score),
              Cut = trimws(Cut)
            )][!is.na(Golfer)]
            
            # Remove duplicates from golfer data
            rv$golfer_data <- unique(rv$golfer_data)
            
            # Calculate lineup metrics using actual golfer data
            unique_lineups <- unique(rv$data$Lineup)
            lineup_metrics_list <- lapply(unique_lineups, function(lineup) {
              metrics <- calculate_lineup_metrics(lineup)
              data.table(
                Lineup = lineup,
                CutsMade = metrics$cuts_made,
                TotalScore = metrics$total_score,
                MadeCutScore = metrics$made_cut_score
              )
            })
            
            rv$lineup_metrics <- rbindlist(lineup_metrics_list)
          } else {
            # No golfer data available, use ExpectedCuts as fallback
            rv$golfer_data <- NULL
            rv$lineup_metrics <- raw_data %>%
              mutate(
                Lineup = paste("G", get(existing_player_cols[1]), 
                               if(length(existing_player_cols) > 1) paste("G", get(existing_player_cols[2])) else "",
                               if(length(existing_player_cols) > 2) paste("G", get(existing_player_cols[3])) else "",
                               if(length(existing_player_cols) > 3) paste("G", get(existing_player_cols[4])) else "",
                               if(length(existing_player_cols) > 4) paste("G", get(existing_player_cols[5])) else "",
                               if(length(existing_player_cols) > 5) paste("G", get(existing_player_cols[6])) else "",
                               sep = " "),
                Lineup = gsub("\\s+", " ", trimws(Lineup)),  # Clean up extra spaces
                CutsMade = ifelse("ExpectedCuts" %in% names(raw_data), round(ExpectedCuts), 0),
                TotalScore = ifelse("StandardProj" %in% names(raw_data), StandardProj, 0),
                MadeCutScore = ifelse("StandardProj" %in% names(raw_data), StandardProj, 0)
              ) %>%
              select(Lineup, CutsMade, TotalScore, MadeCutScore) %>%
              as.data.table()
          }
          
        } else {
          # This is a contest results file (original format)
          rv$file_type <- "Contest"
          
          # Clean up column names
          if (!"EntryName" %in% names(raw_data)) {
            names(raw_data)[3] <- "EntryName"
          }
          if (!"Lineup" %in% names(raw_data)) {
            names(raw_data)[6] <- "Lineup"
          }
          
          # Process main contest data
          rv$data <- as.data.table(raw_data)[, .(
            EntryName = trimws(sub(" \\(.*", "", EntryName)),
            Lineup = trimws(Lineup),
            Points = as.numeric(get(grep("Points", names(raw_data), value = TRUE)[1]))
          )][!is.na(EntryName) & !is.na(Lineup)]
          
          # Process golfer data (scores and cuts)
          rv$golfer_data <- as.data.table(raw_data)[, .(
            Golfer = trimws(Golfer),
            Score = as.numeric(Score),
            Cut = trimws(Cut)
          )][!is.na(Golfer)]
          
          # Remove duplicates from golfer data
          rv$golfer_data <- unique(rv$golfer_data)
          
          # Calculate lineup metrics for each unique lineup
          unique_lineups <- unique(rv$data$Lineup)
          lineup_metrics_list <- lapply(unique_lineups, function(lineup) {
            metrics <- calculate_lineup_metrics(lineup)
            data.table(
              Lineup = lineup,
              CutsMade = metrics$cuts_made,
              TotalScore = metrics$total_score,
              MadeCutScore = metrics$made_cut_score
            )
          })
          
          rv$lineup_metrics <- rbindlist(lineup_metrics_list)
        }
        
        setkey(rv$lineup_metrics, Lineup)
        
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
  
  # Cut Analysis Table
  output$cut_analysis_table <- renderDT({
    req(rv$data, rv$lineup_metrics)
    
    cut_analysis <- rv$lineup_metrics %>%
      group_by(CutsMade) %>%
      summarise(
        Lineups = n(),
        `% of Lineups` = round(n() / nrow(rv$lineup_metrics) * 100, 1),
        `Avg Total Score` = round(mean(TotalScore), 1),
        `Avg Made Cut Score` = round(mean(MadeCutScore), 1)
      ) %>%
      arrange(desc(CutsMade))
    
    datatable(
      cut_analysis,
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        pageLength = 25
      ),
      extensions = 'Buttons'
    )
  })
  
  # Golfer Combinations Analysis
  output$golfer_combos_table <- renderDT({
    req(rv$data, input$analyze_combos)
    
    withProgress(message = 'Analyzing combinations...', value = 0, {
      combo_size <- min(input$combo_size, 6)
      
      lineup_lookup <- as.data.frame(table(rv$data$Lineup))
      names(lineup_lookup) <- c("Lineup", "Count")
      
      unique_lineups <- unique(rv$data$Lineup)
      combo_counts <- new.env(hash = TRUE, parent = emptyenv())
      
      batch_size <- 500
      n_batches <- ceiling(length(unique_lineups) / batch_size)
      
      for (i in seq_len(n_batches)) {
        incProgress(1/n_batches, 
                    detail = paste("Processing batch", i, "of", n_batches))
        
        start_idx <- (i-1) * batch_size + 1
        end_idx <- min(i * batch_size, length(unique_lineups))
        
        batch_lineups <- unique_lineups[start_idx:end_idx]
        
        for (lineup in batch_lineups) {
          golfers <- clean_golfer_names(lineup)
          
          if (length(golfers) >= combo_size) {
            lineup_count <- lineup_lookup$Count[lineup_lookup$Lineup == lineup]
            
            if (combo_size > 1) {
              combos <- combn(sort(golfers), combo_size, simplify = FALSE)
              for (combo in combos) {
                combo_key <- paste(combo, collapse = " | ")
                if (exists(combo_key, envir = combo_counts)) {
                  combo_counts[[combo_key]] <- combo_counts[[combo_key]] + lineup_count
                } else {
                  combo_counts[[combo_key]] <- lineup_count
                }
              }
            } else {
              for (golfer in golfers) {
                if (exists(golfer, envir = combo_counts)) {
                  combo_counts[[golfer]] <- combo_counts[[golfer]] + lineup_count
                } else {
                  combo_counts[[golfer]] <- lineup_count
                }
              }
            }
          }
        }
      }
      
      combo_names <- ls(combo_counts)
      combo_values <- sapply(combo_names, function(name) combo_counts[[name]])
      
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
    req(rv$data, rv$lineup_metrics)
    
    dupes <- rv$data %>%
      group_by(Lineup) %>%
      summarise(Count = n()) %>%
      left_join(rv$lineup_metrics, by = "Lineup") %>%
      arrange(desc(Count)) %>%
      head(10) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, Lineup, Count, CutsMade, TotalScore, MadeCutScore)
    
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
      sapply(clean_golfer_names) %>%
      unlist() %>%
      table() %>%
      as.data.frame() %>%
      setNames(c("Golfer", "FieldCount")) %>%
      mutate(
        `Field %` = round(FieldCount / total_contest_entries * 100, 1)
      )
    
    user_entries <- rv$data %>%
      filter(EntryName == rv$personal_user) %>%
      nrow()
    
    user_exposures <- rv$data %>%
      filter(EntryName == rv$personal_user) %>%
      pull(Lineup) %>%
      sapply(clean_golfer_names) %>%
      unlist() %>%
      table() %>%
      as.data.frame() %>%
      setNames(c("Golfer", "Count")) %>%
      mutate(
        `Your %` = round(Count / user_entries * 100, 1)
      )
    
    exposure_data <- full_join(user_exposures, field_exposures, by = "Golfer") %>%
      mutate(
        Count = replace_na(Count, 0),
        FieldCount = replace_na(FieldCount, 0),
        `Your %` = replace_na(`Your %`, 0),
        `Field %` = replace_na(`Field %`, 0),
        `Leverage` = round(`Your %` - `Field %`, 1)
      ) %>%
      select(Golfer, Count, `Your %`, `Field %`, Leverage) %>%
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
  
  # Personal cut performance table
  output$personal_cut_performance_table <- renderDT({
    req(rv$data, rv$personal_user, rv$lineup_metrics)
    
    # Get field cut performance
    field_cut_performance <- rv$data %>%
      left_join(rv$lineup_metrics, by = "Lineup") %>%
      group_by(CutsMade) %>%
      summarise(
        FieldLineups = n(),
        `Field %` = round(n() / nrow(rv$data) * 100, 1)
      )
    
    # Get user cut performance
    user_cut_performance <- rv$data %>%
      filter(EntryName == rv$personal_user) %>%
      left_join(rv$lineup_metrics, by = "Lineup") %>%
      group_by(CutsMade) %>%
      summarise(
        YourLineups = n(),
        `Your %` = round(n() / nrow(filter(rv$data, EntryName == rv$personal_user)) * 100, 1)
      )
    
    # Combine the data
    cut_comparison <- full_join(user_cut_performance, field_cut_performance, by = "CutsMade") %>%
      mutate(
        YourLineups = replace_na(YourLineups, 0),
        FieldLineups = replace_na(FieldLineups, 0),
        `Your %` = replace_na(`Your %`, 0),
        `Field %` = replace_na(`Field %`, 0),
        `Leverage` = round(`Your %` - `Field %`, 1)
      ) %>%
      arrange(desc(CutsMade)) %>%
      select(CutsMade, YourLineups, `Your %`, `Field %`, Leverage)
    
    datatable(
      cut_comparison,
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
  
  # Personal lineup performance table
  output$personal_lineup_table <- renderDT({
    req(rv$data, rv$personal_user, rv$lineup_metrics)
    
    user_lineups <- rv$data %>%
      filter(EntryName == rv$personal_user) %>%
      left_join(rv$lineup_metrics, by = "Lineup") %>%
      arrange(desc(CutsMade), desc(MadeCutScore)) %>%
      select(Lineup, Points, CutsMade, TotalScore, MadeCutScore)
    
    datatable(
      user_lineups,
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        pageLength = 25
      ),
      extensions = 'Buttons'
    ) %>%
      formatStyle(
        'CutsMade',
        backgroundColor = styleColorBar(c(0, 6), 'lightgreen'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
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
  
  # User cut comparison table
  output$user_cut_comparison_table <- renderDT({
    req(rv$data, length(input$selected_users) > 0, rv$lineup_metrics)
    
    # Get field cut performance for comparison
    field_cut_performance <- rv$data %>%
      left_join(rv$lineup_metrics, by = "Lineup") %>%
      group_by(CutsMade) %>%
      summarise(
        `Field %` = round(n() / nrow(rv$data) * 100, 1)
      )
    
    # Get cut performance for each selected user
    user_cut_data <- do.call(rbind, lapply(input$selected_users, function(user) {
      user_data <- rv$data %>%
        filter(EntryName == user) %>%
        left_join(rv$lineup_metrics, by = "Lineup")
      
      user_total <- nrow(user_data)
      
      user_cut_performance <- user_data %>%
        group_by(CutsMade) %>%
        summarise(
          Lineups = n(),
          Percentage = round(n() / user_total * 100, 1)
        ) %>%
        mutate(User = user)
      
      return(user_cut_performance)
    }))
    
    # Reshape for better display
    cut_comparison_wide <- user_cut_data %>%
      select(User, CutsMade, Percentage) %>%
      pivot_wider(names_from = User, values_from = Percentage, values_fill = 0) %>%
      left_join(field_cut_performance, by = "CutsMade") %>%
      arrange(desc(CutsMade))
    
    datatable(
      cut_comparison_wide,
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        pageLength = 25
      ),
      extensions = 'Buttons'
    ) %>%
      formatStyle(
        columns = names(cut_comparison_wide)[names(cut_comparison_wide) != "CutsMade"],
        background = styleColorBar(c(0, 100), "lightblue"),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
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
        User = factor(user),
        Duplicates = dupes
      )
    }))
    
    p <- plot_ly(data = user_dupes_data, type = "box", 
                 x = ~User, y = ~Duplicates,
                 boxpoints = "all", jitter = 0.3, pointpos = 0)
    
    p <- p %>% layout(
      xaxis = list(title = "", type = "category"),
      yaxis = list(title = "Number of Times Lineup Used in Contest")
    )
    
    p
  })
  
  # User comparison exposure plot
  output$exposure_comparison_plot <- renderPlotly({
    req(rv$data, length(input$selected_users) > 0)
    
    exposure_data <- do.call(rbind, lapply(input$selected_users, function(user) {
      user_entries <- nrow(filter(rv$data, EntryName == user))
      
      exposures <- rv$data %>%
        filter(EntryName == user) %>%
        pull(Lineup) %>%
        sapply(clean_golfer_names) %>%
        unlist() %>%
        table() %>%
        as.data.frame() %>%
        setNames(c("Golfer", "Count")) %>%
        mutate(
          EntryName = factor(user),
          Golfer = factor(Golfer),
          `Exposure %` = round(Count / user_entries * 100, 1)
        )
      
      return(exposures)
    }))
    
    plot_height <- max(600, 30 * length(unique(exposure_data$Golfer)))
    
    p <- plot_ly(
      data = exposure_data,
      y = ~Golfer,
      x = ~`Exposure %`,
      color = ~EntryName,
      type = "bar",
      orientation = 'h'
    )
    
    p <- p %>% layout(
      barmode = "group",
      xaxis = list(title = "Exposure %"),
      yaxis = list(title = "", type = "category"),
      margin = list(l = 150),
      height = plot_height
    )
    
    p
  })
  
  # File type info output
  output$fileInfo <- renderText({
    req(rv$file_type)
    if (rv$file_type == "MME") {
      "MME Generated Lineups"
    } else {
      "Contest Results"
    }
  })
  
  # Check if file info should be shown
  outputOptions(output, "fileInfo", suspendWhenHidden = FALSE)
}

# Run the app
shinyApp(ui = ui, server = server)