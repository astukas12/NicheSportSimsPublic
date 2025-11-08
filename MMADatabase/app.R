# MMA Base Score Research Workbench - Redesigned
# Horizontal tabs, wider plots, platform toggle, percentile-based stats

library(shiny)
library(tidyverse)
library(readxl)
library(DT)
library(plotly)
library(scales)
library(shinycssloaders)

# Custom CSS for clean black and gold theme with horizontal tabs
custom_css <- "
  /* Main theme */
  body {
    background-color: #f4f4f4;
  }
  
  /* Navbar styling */
  .navbar {
    background-color: #000000 !important;
    border: none;
  }
  
  .navbar-brand {
    color: #FFD700 !important;
    font-weight: bold;
    font-size: 24px;
  }
  
  /* Tab styling */
  .nav-tabs {
    border-bottom: 2px solid #FFD700;
    background-color: #1a1a1a;
  }
  
  .nav-tabs > li > a {
    color: #FFD700;
    background-color: #1a1a1a;
    border: none;
    font-size: 16px;
    padding: 12px 24px;
  }
  
  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:hover,
  .nav-tabs > li.active > a:focus {
    color: #000000;
    background-color: #FFD700;
    border: none;
    font-weight: bold;
  }
  
  .nav-tabs > li > a:hover {
    background-color: #333333;
    border: none;
  }
  
  /* Control panel */
  .well {
    background-color: #ffffff;
    border: 1px solid #ddd;
    border-radius: 4px;
    padding: 15px;
  }
  
  /* Box styling */
  .box {
    border-top: 3px solid #FFD700;
    box-shadow: 0 1px 3px rgba(0,0,0,0.12);
  }
  
  .box-header {
    background-color: #333333;
    color: #FFD700;
  }
  
  /* Button styling */
  .btn-primary {
    background-color: #FFD700;
    border-color: #DAA520;
    color: #000000;
    font-weight: bold;
  }
  
  .btn-primary:hover {
    background-color: #DAA520;
    border-color: #B8860B;
    color: #000000;
  }
  
  /* Tables */
  .dataTables_wrapper .dataTables_length,
  .dataTables_wrapper .dataTables_filter,
  .dataTables_wrapper .dataTables_info,
  .dataTables_wrapper .dataTables_processing,
  .dataTables_wrapper .dataTables_paginate {
    color: #333;
  }
"

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML(custom_css))
  ),
  
  # Header
  div(
    style = "background-color: #000000; padding: 15px; margin-bottom: 20px;",
    h2("MMA Base Score Research Workbench", 
       style = "color: #FFD700; margin: 0; font-weight: bold;")
  ),
  
  # Control Panel
  fluidRow(
    column(12,
           wellPanel(
             fluidRow(
               column(3,
                      fileInput("fantasy_db", "Upload Fantasy Database",
                                accept = c(".xlsx", ".xls"),
                                width = "100%")
               ),
               column(2,
                      uiOutput("fighter_select")
               ),
               column(2,
                      uiOutput("weight_class_select")
               ),
               column(2,
                      radioButtons("platform", "Platform:",
                                   choices = c("DraftKings" = "DK", "FanDuel" = "FD"),
                                   selected = "DK",
                                   inline = TRUE)
               ),
               column(2,
                      checkboxInput("remove_outliers", "Remove Outliers", value = TRUE),
                      numericInput("outlier_threshold", "IQR Multiplier", 
                                   value = 1.5, min = 0.5, max = 3, step = 0.1)
               ),
               column(1,
                      div(style = "padding-top: 25px;",
                          uiOutput("data_summary_badge"))
               )
             )
           )
    )
  ),
  
  # Tabs
  tabsetPanel(
    id = "main_tabs",
    
    # Fighter Analysis Tab
    tabPanel(
      "Fighter Analysis",
      br(),
      fluidRow(
        column(8,
               plotlyOutput("fighter_violin", height = "550px") %>% withSpinner(color = "#FFD700")
        ),
        column(4,
               div(
                 style = "background: white; padding: 15px; border: 1px solid #ddd; border-radius: 4px; min-height: 550px;",
                 h4("Adjustment Recommendations", style = "color: #333; border-bottom: 2px solid #FFD700; padding-bottom: 10px;"),
                 uiOutput("adjustment_recommendations")
               )
        )
      ),
      br(),
      fluidRow(
        column(12,
               plotOutput("fighter_comparison", height = "400px") %>% withSpinner(color = "#FFD700")
        )
      ),
      br(),
      fluidRow(
        column(6,
               div(
                 style = "background: white; padding: 15px; border: 1px solid #ddd; border-radius: 4px;",
                 h4("Fighter Statistics (Percentiles)", style = "color: #333; border-bottom: 2px solid #FFD700; padding-bottom: 10px;"),
                 DTOutput("fighter_stats_table")
               )
        ),
        column(6,
               div(
                 style = "background: white; padding: 15px; border: 1px solid #ddd; border-radius: 4px;",
                 h4("Fighter Fight History", style = "color: #333; border-bottom: 2px solid #FFD700; padding-bottom: 10px;"),
                 DTOutput("fighter_history_table")
               )
        )
      )
    ),
    
    # Weight Class Comparison Tab
    tabPanel(
      "Weight Class Comparison",
      br(),
      fluidRow(
        column(12,
               plotlyOutput("weight_class_violin", height = "600px") %>% withSpinner(color = "#FFD700")
        )
      ),
      br(),
      fluidRow(
        column(12,
               div(
                 style = "background: white; padding: 15px; border: 1px solid #ddd; border-radius: 4px;",
                 h4("Weight Class Statistics (Percentiles)", style = "color: #333; border-bottom: 2px solid #FFD700; padding-bottom: 10px;"),
                 DTOutput("weight_class_stats")
               )
        )
      )
    ),
    
    # Round Comparison Tab
    tabPanel(
      "Round Comparison",
      br(),
      fluidRow(
        column(6,
               plotOutput("round_progression", height = "500px") %>% withSpinner(color = "#FFD700")
        ),
        column(6,
               plotOutput("round_base_by_platform", height = "500px") %>% withSpinner(color = "#FFD700")
        )
      ),
      br(),
      fluidRow(
        column(6,
               plotOutput("win_loss_comparison", height = "400px") %>% withSpinner(color = "#FFD700")
        ),
        column(6,
               plotOutput("decision_vs_finish", height = "400px") %>% withSpinner(color = "#FFD700")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load fantasy database
  fantasy_data <- reactive({
    req(input$fantasy_db)
    
    tryCatch({
      df <- read_excel(input$fantasy_db$datapath, sheet = "UFC_Fantasy_Fighter_Data")
      
      # Add simplified method for grouping
      df <- df %>%
        mutate(
          MethodSimple = case_when(
            METHOD == "Quick1" ~ "Quick R1",
            METHOD == "1" ~ "R1",
            METHOD == "2" ~ "R2",
            METHOD == "3" ~ "R3",
            METHOD == "4" ~ "R4",
            METHOD == "5" ~ "R5",
            METHOD == "Decision- 3" ~ "Decision-3",
            METHOD == "Decision- 5" ~ "Decision-5",
            TRUE ~ "Other"
          ),
          Gender = ifelse(grepl("Women", WEIGHTCLASS), "Women", "Men"),
          WeightClassSimple = gsub("Women's ", "", WEIGHTCLASS)
        )
      
      return(df)
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Get last weight class for selected fighter
  fighter_last_weight_class <- reactive({
    req(input$selected_fighter)
    req(fantasy_data())
    
    last_fight <- fantasy_data() %>%
      filter(FIGHTER == input$selected_fighter) %>%
      arrange(desc(EVENT_DATE)) %>%
      slice(1)
    
    if (nrow(last_fight) > 0) {
      return(last_fight$WEIGHTCLASS)
    }
    return(NULL)
  })
  
  # Fighter selection UI
  output$fighter_select <- renderUI({
    req(fantasy_data())
    
    fighters <- fantasy_data() %>%
      filter(RESULT == "W") %>%
      arrange(FIGHTER) %>%
      pull(FIGHTER) %>%
      unique()
    
    selectInput("selected_fighter", 
                "Select Fighter:",
                choices = c("", fighters),
                selected = "",
                width = "100%")
  })
  
  # Weight class selection UI - auto-populates with fighter's last weight class
  output$weight_class_select <- renderUI({
    req(fantasy_data())
    
    weight_classes <- fantasy_data() %>%
      arrange(WEIGHTCLASS) %>%
      pull(WEIGHTCLASS) %>%
      unique()
    
    # Auto-select fighter's last weight class
    selected_wc <- if (!is.null(input$selected_fighter) && input$selected_fighter != "") {
      fighter_last_weight_class()
    } else {
      "All"
    }
    
    selectInput("selected_weight_class",
                "Weight Class:",
                choices = c("All", weight_classes),
                selected = selected_wc,
                width = "100%")
  })
  
  # Data summary badge
  output$data_summary_badge <- renderUI({
    req(fantasy_data())
    
    total_fights <- nrow(fantasy_data())
    total_fighters <- fantasy_data() %>% pull(FIGHTER) %>% unique() %>% length()
    
    div(
      style = "text-align: center; background: #FFD700; padding: 10px; border-radius: 4px;",
      div(style = "font-size: 20px; font-weight: bold; color: #000;", total_fights),
      div(style = "font-size: 11px; color: #000;", "Fights"),
      div(style = "font-size: 16px; font-weight: bold; color: #000; margin-top: 5px;", total_fighters),
      div(style = "font-size: 11px; color: #000;", "Fighters")
    )
  })
  
  # Get base column name based on platform
  base_column <- reactive({
    paste0(input$platform, "Base")
  })
  
  # Get score column name based on platform
  score_column <- reactive({
    paste0(input$platform, "Score")
  })
  
  # Platform display name
  platform_name <- reactive({
    if (input$platform == "DK") "DraftKings" else "FanDuel"
  })
  
  # Filtered data based on selections
  filtered_data <- reactive({
    req(fantasy_data())
    
    df <- fantasy_data()
    
    # Filter by weight class
    if (!is.null(input$selected_weight_class) && input$selected_weight_class != "All") {
      df <- df %>% filter(WEIGHTCLASS == input$selected_weight_class)
    }
    
    # Remove outliers if requested
    if (input$remove_outliers) {
      base_col <- base_column()
      df <- df %>%
        group_by(FIGHTER, RESULT) %>%
        mutate(
          Q1 = quantile(.data[[base_col]], 0.25, na.rm = TRUE),
          Q3 = quantile(.data[[base_col]], 0.75, na.rm = TRUE),
          IQR = Q3 - Q1,
          Lower = Q1 - input$outlier_threshold * IQR,
          Upper = Q3 + input$outlier_threshold * IQR
        ) %>%
        filter(.data[[base_col]] >= Lower & .data[[base_col]] <= Upper) %>%
        select(-Q1, -Q3, -IQR, -Lower, -Upper) %>%
        ungroup()
    }
    
    return(df)
  })
  
  # Fighter-specific data
  fighter_data <- reactive({
    req(input$selected_fighter)
    req(filtered_data())
    
    filtered_data() %>%
      filter(FIGHTER == input$selected_fighter)
  })
  
  # Weight class data for selected fighter (excluding the fighter)
  weight_class_data <- reactive({
    req(fighter_data())
    
    wc <- unique(fighter_data()$WEIGHTCLASS)[1]
    
    filtered_data() %>%
      filter(WEIGHTCLASS == wc, FIGHTER != input$selected_fighter)
  })
  
  # Calculate percentiles helper function
  calc_percentiles <- function(data, column) {
    if (nrow(data) == 0) {
      return(rep(NA, 7))
    }
    quantile(data[[column]], probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95), na.rm = TRUE)
  }
  
  # Fighter Violin Plot
  output$fighter_violin <- renderPlotly({
    req(fighter_data())
    base_col <- base_column()
    
    # Combine fighter and weight class data
    fighter <- fighter_data() %>%
      mutate(Group = "Selected Fighter") %>%
      filter(RESULT == "W")
    
    wc <- weight_class_data() %>%
      mutate(Group = "Weight Class Avg") %>%
      filter(RESULT == "W")
    
    combined <- bind_rows(fighter, wc)
    
    # Create violin plot by outcome
    p <- ggplot(combined, aes(x = MethodSimple, y = .data[[base_col]], fill = Group)) +
      geom_violin(position = position_dodge(0.8), alpha = 0.6, scale = "width") +
      geom_jitter(aes(color = Group), 
                  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
                  alpha = 0.4, size = 2) +
      stat_summary(aes(group = Group), fun = mean, geom = "point", 
                   shape = 23, size = 4, fill = "white",
                   position = position_dodge(0.8)) +
      scale_fill_manual(values = c("Selected Fighter" = "#FFD700", "Weight Class Avg" = "#808080")) +
      scale_color_manual(values = c("Selected Fighter" = "#DAA520", "Weight Class Avg" = "#606060")) +
      labs(
        title = paste0(input$selected_fighter, " - ", platform_name(), " Base Score by Finish Type (Winners)"),
        subtitle = "Diamond = Mean | Selected fighter vs weight class average",
        x = "Finish Type",
        y = paste(platform_name(), "Base Score"),
        fill = "",
        color = ""
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5)
      )
    
    ggplotly(p, tooltip = c("y", "fill")) %>%
      layout(hovermode = "closest")
  })
  
  # Fighter Comparison Bar Chart
  output$fighter_comparison <- renderPlot({
    req(fighter_data())
    req(weight_class_data())
    base_col <- base_column()
    
    # Calculate averages by outcome for winners
    fighter_avg <- fighter_data() %>%
      filter(RESULT == "W") %>%
      group_by(MethodSimple) %>%
      summarise(
        AvgBase = mean(.data[[base_col]], na.rm = TRUE),
        Count = n(),
        .groups = "drop"
      ) %>%
      mutate(Group = "Fighter")
    
    wc_avg <- weight_class_data() %>%
      filter(RESULT == "W") %>%
      group_by(MethodSimple) %>%
      summarise(
        AvgBase = mean(.data[[base_col]], na.rm = TRUE),
        Count = n(),
        .groups = "drop"
      ) %>%
      mutate(Group = "Weight Class")
    
    combined <- bind_rows(fighter_avg, wc_avg) %>%
      filter(Count >= 3)  # Only show outcomes with 3+ samples
    
    ggplot(combined, aes(x = MethodSimple, y = AvgBase, fill = Group)) +
      geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
      geom_text(aes(label = paste0(round(AvgBase, 1), "\n(n=", Count, ")")),
                position = position_dodge(0.7), vjust = -0.3, size = 3.5) +
      scale_fill_manual(values = c("Fighter" = "#FFD700", "Weight Class" = "#808080")) +
      labs(
        title = paste0(input$selected_fighter, " vs ", unique(fighter_data()$WEIGHTCLASS)[1], " Average - ", platform_name()),
        subtitle = "Base Score by Outcome (Winners Only, n ≥ 3)",
        x = "Finish Type",
        y = paste("Average", platform_name(), "Base Score"),
        fill = ""
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = "top",
        panel.grid.major.x = element_blank()
      ) +
      ylim(0, NA)
  })
  
  # Fighter Statistics Table (Percentiles)
  output$fighter_stats_table <- renderDT({
    req(fighter_data())
    base_col <- base_column()
    
    stats <- fighter_data() %>%
      filter(RESULT == "W") %>%
      group_by(MethodSimple) %>%
      summarise(
        Fights = n(),
        P5 = round(quantile(.data[[base_col]], 0.05, na.rm = TRUE), 1),
        P10 = round(quantile(.data[[base_col]], 0.10, na.rm = TRUE), 1),
        P25 = round(quantile(.data[[base_col]], 0.25, na.rm = TRUE), 1),
        P50 = round(quantile(.data[[base_col]], 0.50, na.rm = TRUE), 1),
        P75 = round(quantile(.data[[base_col]], 0.75, na.rm = TRUE), 1),
        P90 = round(quantile(.data[[base_col]], 0.90, na.rm = TRUE), 1),
        P95 = round(quantile(.data[[base_col]], 0.95, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(Fights))
    
    datatable(
      stats,
      options = list(
        pageLength = 10,
        dom = 't',
        ordering = FALSE,
        scrollX = FALSE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(columns = 1:9, fontSize = '12px')
  })
  
  # Fighter History Table - Emphasizing scoring components
  output$fighter_history_table <- renderDT({
    req(fighter_data())
    base_col <- base_column()
    score_col <- score_column()
    
    history <- fighter_data() %>%
      select(Date = EVENT_DATE, 
             Result = RESULT,
             Method = MethodSimple,
             SigStr = SigStrikes,
             TD = Takedowns,
             KD = Knockdowns,
             Sub = SubAttempts,
             Base = !!sym(base_col), 
             Total = !!sym(score_col)) %>%
      arrange(desc(Date)) %>%
      mutate(
        Base = round(Base, 1),
        Total = round(Total, 1)
      )
    
    datatable(
      history,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searching = FALSE,
        ordering = FALSE,
        columnDefs = list(
          list(width = '80px', targets = 0),
          list(width = '50px', targets = 1),
          list(width = '80px', targets = 2)
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Result',
        backgroundColor = styleEqual(c('W', 'L', 'D'), c('#d4edda', '#f8d7da', '#fff3cd'))
      ) %>%
      formatStyle(
        c('SigStr', 'TD', 'KD'),
        fontWeight = 'bold',
        color = styleInterval(c(0, 50, 100), c('#999', '#333', '#FF6B35', '#4CAF50'))
      ) %>%
      formatStyle(columns = 1:9, fontSize = '11px')
  })
  
  # Adjustment Recommendations
  output$adjustment_recommendations <- renderUI({
    req(fighter_data())
    req(weight_class_data())
    base_col <- base_column()
    
    # Calculate differences
    fighter_avg <- fighter_data() %>%
      filter(RESULT == "W") %>%
      group_by(MethodSimple) %>%
      summarise(FighterAvg = mean(.data[[base_col]], na.rm = TRUE), FighterN = n(), .groups = "drop")
    
    wc_avg <- weight_class_data() %>%
      filter(RESULT == "W") %>%
      group_by(MethodSimple) %>%
      summarise(WCAvg = mean(.data[[base_col]], na.rm = TRUE), WCN = n(), .groups = "drop")
    
    comparison <- fighter_avg %>%
      left_join(wc_avg, by = "MethodSimple") %>%
      filter(FighterN >= 2, WCN >= 5) %>%
      mutate(
        Diff = FighterAvg - WCAvg,
        DiffPct = (Diff / WCAvg) * 100,
        Recommendation = case_when(
          DiffPct > 10 ~ paste0("Increase base by +", round(Diff * 0.6, 0), " to +", round(Diff * 0.8, 0)),
          DiffPct < -10 ~ paste0("Decrease base by ", round(Diff * 0.6, 0), " to ", round(Diff * 0.8, 0)),
          TRUE ~ "Use weight class default"
        ),
        Color = case_when(
          DiffPct > 10 ~ "#4CAF50",
          DiffPct < -10 ~ "#f44336",
          TRUE ~ "#2196F3"
        )
      ) %>%
      arrange(desc(abs(DiffPct)))
    
    if (nrow(comparison) == 0) {
      return(tags$p("Not enough data for recommendations. Fighter needs at least 2 fights per outcome, weight class needs at least 5.", 
                    style = "padding: 20px; color: #666;"))
    }
    
    # Create recommendation HTML
    rec_list <- lapply(1:min(5, nrow(comparison)), function(i) {
      row <- comparison[i, ]
      
      tags$div(
        style = paste0("margin-bottom: 15px; padding: 12px; background: #f9f9f9; border-left: 4px solid ", row$Color, "; border-radius: 3px;"),
        tags$div(
          tags$strong(row$MethodSimple, style = "font-size: 15px; color: #333;"),
          style = "margin-bottom: 5px;"
        ),
        tags$div(
          sprintf("Fighter: %.1f | WC: %.1f | Diff: %+.1f (%+.0f%%)", 
                  row$FighterAvg, row$WCAvg, row$Diff, row$DiffPct),
          style = "font-size: 12px; color: #666; margin-bottom: 5px;"
        ),
        tags$div(
          sprintf("Sample: n=%d (fighter) | n=%d (class)", row$FighterN, row$WCN),
          style = "font-size: 11px; color: #999; margin-bottom: 8px;"
        ),
        tags$div(
          tags$strong("→ ", row$Recommendation),
          style = paste0("font-size: 13px; color: ", row$Color, "; font-weight: bold;")
        )
      )
    })
    
    tags$div(rec_list)
  })
  
  # Weight Class Violin Plot
  output$weight_class_violin <- renderPlotly({
    req(filtered_data())
    base_col <- base_column()
    
    # Filter for winners only
    data_winners <- filtered_data() %>%
      filter(RESULT == "W", MethodSimple %in% c("R1", "R2", "R3", "Decision-3", "Decision-5"))
    
    # Calculate stats for ordering
    outcome_stats <- data_winners %>%
      group_by(MethodSimple) %>%
      summarise(MedianBase = median(.data[[base_col]], na.rm = TRUE), .groups = "drop") %>%
      arrange(MedianBase)
    
    data_winners <- data_winners %>%
      mutate(MethodSimple = factor(MethodSimple, levels = outcome_stats$MethodSimple))
    
    # Create violin plot
    p <- ggplot(data_winners, aes(x = MethodSimple, y = .data[[base_col]], fill = MethodSimple)) +
      geom_violin(alpha = 0.7, scale = "width") +
      geom_jitter(aes(color = MethodSimple), width = 0.2, alpha = 0.3, size = 1.5) +
      stat_summary(fun = median, geom = "point", shape = 23, size = 4, fill = "white") +
      scale_fill_brewer(palette = "Set2") +
      scale_color_brewer(palette = "Set2") +
      labs(
        title = paste0("Base Score Distribution by Finish Type - ", platform_name()),
        subtitle = "Winners only | Diamond = Median",
        x = "Finish Type",
        y = paste(platform_name(), "Base Score")
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5)
      )
    
    ggplotly(p, tooltip = c("y")) %>%
      layout(hovermode = "closest")
  })
  
  # Weight Class Statistics Table (Percentiles) - with All WC comparison
  output$weight_class_stats <- renderDT({
    req(filtered_data())
    base_col <- base_column()
    
    # Individual weight class stats
    stats <- filtered_data() %>%
      filter(RESULT == "W") %>%
      group_by(WEIGHTCLASS, MethodSimple) %>%
      summarise(
        Fights = n(),
        P5 = round(quantile(.data[[base_col]], 0.05, na.rm = TRUE), 1),
        P10 = round(quantile(.data[[base_col]], 0.10, na.rm = TRUE), 1),
        P25 = round(quantile(.data[[base_col]], 0.25, na.rm = TRUE), 1),
        P50 = round(quantile(.data[[base_col]], 0.50, na.rm = TRUE), 1),
        P75 = round(quantile(.data[[base_col]], 0.75, na.rm = TRUE), 1),
        P90 = round(quantile(.data[[base_col]], 0.90, na.rm = TRUE), 1),
        P95 = round(quantile(.data[[base_col]], 0.95, na.rm = TRUE), 1),
        .groups = "drop"
      )
    
    # All weight classes combined stats
    all_wc_stats <- filtered_data() %>%
      filter(RESULT == "W") %>%
      group_by(MethodSimple) %>%
      summarise(
        Fights = n(),
        P5 = round(quantile(.data[[base_col]], 0.05, na.rm = TRUE), 1),
        P10 = round(quantile(.data[[base_col]], 0.10, na.rm = TRUE), 1),
        P25 = round(quantile(.data[[base_col]], 0.25, na.rm = TRUE), 1),
        P50 = round(quantile(.data[[base_col]], 0.50, na.rm = TRUE), 1),
        P75 = round(quantile(.data[[base_col]], 0.75, na.rm = TRUE), 1),
        P90 = round(quantile(.data[[base_col]], 0.90, na.rm = TRUE), 1),
        P95 = round(quantile(.data[[base_col]], 0.95, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      mutate(WEIGHTCLASS = "*** ALL WEIGHT CLASSES ***")
    
    # Combine and sort
    combined_stats <- bind_rows(all_wc_stats, stats) %>%
      arrange(WEIGHTCLASS, MethodSimple)
    
    datatable(
      combined_stats,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        searching = TRUE,
        columnDefs = list(list(width = '180px', targets = 0))
      ),
      rownames = FALSE,
      filter = 'top'
    ) %>%
      formatStyle(
        'WEIGHTCLASS',
        target = 'row',
        fontWeight = styleEqual("*** ALL WEIGHT CLASSES ***", "bold"),
        backgroundColor = styleEqual("*** ALL WEIGHT CLASSES ***", "#FFD700"),
        color = styleEqual("*** ALL WEIGHT CLASSES ***", "#000")
      ) %>%
      formatStyle(columns = 1:9, fontSize = '11px')
  })
  
  # Round Progression Plot
  output$round_progression <- renderPlot({
    req(filtered_data())
    base_col <- base_column()
    
    # Filter to main rounds
    round_data <- filtered_data() %>%
      filter(RESULT == "W", MethodSimple %in% c("R1", "R2", "R3", "R4", "R5", "Decision-3", "Decision-5")) %>%
      mutate(
        RoundNum = case_when(
          MethodSimple == "R1" ~ 1,
          MethodSimple == "R2" ~ 2,
          MethodSimple == "R3" ~ 3,
          MethodSimple == "R4" ~ 4,
          MethodSimple == "R5" ~ 5,
          MethodSimple == "Decision-3" ~ 3,
          MethodSimple == "Decision-5" ~ 5
        )
      )
    
    # Calculate stats by round
    round_stats <- round_data %>%
      group_by(RoundNum) %>%
      summarise(
        Mean = mean(.data[[base_col]], na.rm = TRUE),
        Median = median(.data[[base_col]], na.rm = TRUE),
        Q25 = quantile(.data[[base_col]], 0.25, na.rm = TRUE),
        Q75 = quantile(.data[[base_col]], 0.75, na.rm = TRUE),
        Count = n(),
        .groups = "drop"
      )
    
    ggplot(round_stats, aes(x = RoundNum)) +
      geom_ribbon(aes(ymin = Q25, ymax = Q75), fill = "#FFD700", alpha = 0.3) +
      geom_line(aes(y = Median), color = "#FFD700", size = 1.5) +
      geom_line(aes(y = Mean), color = "#808080", size = 1.5, linetype = "dashed") +
      geom_point(aes(y = Median), color = "#FFD700", size = 4) +
      geom_point(aes(y = Mean), color = "#808080", size = 4, shape = 17) +
      scale_x_continuous(breaks = 1:5) +
      labs(
        title = paste0("Base Score Progression by Round - ", platform_name()),
        subtitle = "Solid line = Median | Dashed line = Mean | Shaded area = IQR",
        x = "Round Finished",
        y = paste(platform_name(), "Base Score")
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 11)
      )
  })
  
  # Round Base by Platform Comparison
  output$round_base_by_platform <- renderPlot({
    req(filtered_data())
    
    # Calculate for both platforms
    round_data <- filtered_data() %>%
      filter(RESULT == "W", MethodSimple %in% c("R1", "R2", "R3", "Decision-3", "Decision-5")) %>%
      mutate(
        RoundNum = case_when(
          MethodSimple == "R1" ~ 1,
          MethodSimple == "R2" ~ 2,
          MethodSimple == "R3" ~ 3,
          MethodSimple == "Decision-3" ~ 3
        )
      ) %>%
      filter(!is.na(RoundNum))
    
    round_stats <- round_data %>%
      group_by(RoundNum) %>%
      summarise(
        DK_Median = median(DKBase, na.rm = TRUE),
        FD_Median = median(FDBase, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(DK_Median, FD_Median), 
                   names_to = "Platform", 
                   values_to = "MedianBase") %>%
      mutate(Platform = ifelse(Platform == "DK_Median", "DraftKings", "FanDuel"))
    
    ggplot(round_stats, aes(x = RoundNum, y = MedianBase, color = Platform, group = Platform)) +
      geom_line(size = 1.5) +
      geom_point(size = 4) +
      scale_color_manual(values = c("DraftKings" = "#FFD700", "FanDuel" = "#4CAF50")) +
      scale_x_continuous(breaks = 1:5) +
      labs(
        title = "Platform Comparison - Base Score Progression",
        subtitle = "Median base scores by round",
        x = "Round Finished",
        y = "Median Base Score",
        color = "Platform"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = "top"
      )
  })
  
  # Win vs Loss Comparison
  output$win_loss_comparison <- renderPlot({
    req(filtered_data())
    base_col <- base_column()
    
    comparison <- filtered_data() %>%
      filter(MethodSimple %in% c("R1", "R2", "R3", "Decision-3", "Decision-5")) %>%
      group_by(MethodSimple, RESULT) %>%
      summarise(AvgBase = mean(.data[[base_col]], na.rm = TRUE), .groups = "drop")
    
    ggplot(comparison, aes(x = MethodSimple, y = AvgBase, fill = RESULT)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = c("W" = "#4CAF50", "L" = "#f44336", "D" = "#FFA500")) +
      labs(
        title = paste0("Winners vs Losers Base Scores - ", platform_name()),
        x = "Finish Type",
        y = paste("Avg", platform_name(), "Base Score"),
        fill = "Result"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "top"
      )
  })
  
  # Decision vs Finish Comparison
  output$decision_vs_finish <- renderPlot({
    req(filtered_data())
    base_col <- base_column()
    
    comparison <- filtered_data() %>%
      filter(RESULT == "W") %>%
      mutate(
        FightType = case_when(
          MethodSimple %in% c("Decision-3", "Decision-5") ~ "Decision",
          TRUE ~ "Finish"
        )
      ) %>%
      filter(FightType %in% c("Decision", "Finish"))
    
    ggplot(comparison, aes(x = FightType, y = .data[[base_col]], fill = FightType)) +
      geom_violin(alpha = 0.7) +
      geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
      scale_fill_manual(values = c("Decision" = "#2196F3", "Finish" = "#FFD700")) +
      labs(
        title = paste0("Decision vs Finish Base Scores - ", platform_name()),
        x = "",
        y = paste(platform_name(), "Base Score")
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none"
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)