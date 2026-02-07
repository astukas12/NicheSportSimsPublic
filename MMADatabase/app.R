library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)
library(scales)
library(plotly)

# Read database from working directory
fantasy_db <- read_excel("UFC_Fantasy_Database.xlsx")

# Prepare data
fantasy_db <- fantasy_db %>%
  mutate(
    rounds_lasted = case_when(
      METHOD == "Quick1" ~ 0.5,
      METHOD == "1" ~ 1,
      METHOD == "2" ~ 2,
      METHOD == "3" ~ 3,
      METHOD == "4" ~ 4,
      METHOD == "5" ~ 5,
      grepl("Decision- 3", METHOD) ~ 3,
      grepl("Decision- 5", METHOD) ~ 5,
      TRUE ~ 3
    ),
    DK_per_round = DKBase / rounds_lasted,
    FD_per_round = FDBase / rounds_lasted,
    is_win = RESULT == "W"
  )

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { 
        background-color: #0a0a0a; 
        color: #d4af37;
        font-family: 'Segoe UI', Arial, sans-serif;
      }
      .well { 
        background-color: #1a1a1a; 
        border: 1px solid #d4af37;
        box-shadow: 0 0 10px rgba(212, 175, 55, 0.3);
      }
      .selectize-input, .selectize-dropdown {
        background-color: #1a1a1a;
        color: #d4af37;
        border: 1px solid #d4af37;
      }
      .selectize-dropdown .option {
        background-color: #1a1a1a;
        color: #d4af37;
      }
      .selectize-dropdown .option:hover,
      .selectize-dropdown .active {
        background-color: #d4af37;
        color: #0a0a0a;
      }
      .selectize-dropdown .selected {
        background-color: #2a2a2a;
        color: #d4af37;
      }
      .selectize-input.focus {
        border-color: #d4af37;
        box-shadow: 0 0 5px rgba(212, 175, 55, 0.5);
      }
      h2, h3, h4 { 
        color: #d4af37; 
        text-shadow: 0 0 10px rgba(212, 175, 55, 0.5);
      }
      .shiny-output-error { color: #ff6b6b; }
      .dataTables_wrapper { color: #d4af37; }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #d4af37 !important;
      }
      table.dataTable tbody tr { background-color: #1a1a1a; }
      table.dataTable tbody tr:hover { background-color: #2a2a2a; }
      .info-box {
        background-color: #1a1a1a;
        border: 1px solid #d4af37;
        border-radius: 5px;
        padding: 15px;
        margin-bottom: 20px;
      }
      .stat-label {
        font-weight: bold;
        color: #d4af37;
      }
    "))
  ),
  
  titlePanel(
    div(style = "text-align: center;",
        img(src = "logo.jpg", height = "80px", style = "margin-bottom: 10px;"),
        h2("Golden Ticket Sims", style = "margin: 10px 0 5px 0;"),
        h4("MMA Fighter Analysis & Scoring Ranges", 
           style = "color: #888; font-weight: normal; margin-top: 0;")
    )
  ),
  
  fluidRow(
    column(12,
           wellPanel(
             selectInput("fighter", "Select Fighter:", 
                         choices = NULL,  # Will be populated server-side
                         width = "100%")
           )
    )
  ),
  
  # Fighter Analysis Section
  fluidRow(
    column(12,
           h3("Fighter Analysis"),
           
           # Fighter Summary Stats and Platform Toggle
           fluidRow(
             column(3,
                    div(class = "info-box",
                        h4("Career Record", style = "margin-top: 0;"),
                        uiOutput("career_record")
                    )
             ),
             column(3,
                    div(class = "info-box",
                        h4("Current Weight Class", style = "margin-top: 0;"),
                        uiOutput("weight_class_info")
                    )
             ),
             column(6,
                    div(class = "info-box",
                        h4("Platform", style = "margin-top: 0;"),
                        radioButtons("platform", NULL,
                                     choices = c("DraftKings" = "DK", "FanDuel" = "FD"),
                                     selected = "DK",
                                     inline = TRUE)
                    )
             )
           ),
           
           # Base Score Distribution - Individual Violin per Outcome
           fluidRow(
             column(12,
                    h4("Base Score Distribution by Outcome"),
                    plotlyOutput("violin_plot_combined", height = "600px")
             )
           ),
           
           # Round by Round Progression - Full Width Charts
           fluidRow(
             column(12,
                    h4("Per Round Scoring - Wins"),
                    plotlyOutput("round_progression_wins", height = "350px")
             )
           ),
           
           fluidRow(
             column(12,
                    h4("Per Round Scoring - Losses"),
                    plotlyOutput("round_progression_losses", height = "350px")
             )
           ),
           
           # Full Fighter History - Wins and Losses
           fluidRow(
             column(12,
                    h4("Fight History - Wins"),
                    DTOutput("fighter_history_wins")
             )
           ),
           
           fluidRow(
             column(12,
                    h4("Fight History - Losses"),
                    DTOutput("fighter_history_losses")
             )
           )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Server-side selectize for fighter selection
  updateSelectizeInput(session, "fighter", 
                       choices = sort(unique(fantasy_db$FIGHTER)),
                       server = TRUE)
  
  # Reactive fighter data
  fighter_data <- reactive({
    req(input$fighter)
    fantasy_db %>%
      filter(FIGHTER == input$fighter) %>%
      arrange(desc(EVENT_DATE))
  })
  
  # Get most recent weight class
  current_weight_class <- reactive({
    req(fighter_data())
    fighter_data()$WEIGHTCLASS[1]
  })
  
  # Get weight class comparison data
  weight_class_data <- reactive({
    req(current_weight_class())
    fantasy_db %>%
      filter(WEIGHTCLASS == current_weight_class())
  })
  
  # Career Record
  output$career_record <- renderUI({
    data <- fighter_data()
    wins <- sum(data$RESULT == "W")
    losses <- sum(data$RESULT == "L")
    
    HTML(paste0(
      "<div style='font-size: 24px; text-align: center; margin: 10px 0;'>",
      wins, " - ", losses,
      "</div>",
      "<div style='text-align: center; color: #888;'>",
      nrow(data), " Total Fights",
      "</div>"
    ))
  })
  
  # Weight Class Info
  output$weight_class_info <- renderUI({
    wc <- current_weight_class()
    data <- fighter_data()
    fights_in_wc <- sum(data$WEIGHTCLASS == wc)
    
    HTML(paste0(
      "<div style='font-size: 18px; text-align: center; margin: 10px 0;'>",
      wc,
      "</div>",
      "<div style='text-align: center; color: #888;'>",
      fights_in_wc, " fight", if(fights_in_wc != 1) "s" else "", " in class",
      "</div>"
    ))
  })
  
  # Violin Plot by Outcome - Separate violin for each outcome type
  output$violin_plot_combined <- renderPlotly({
    fighter <- fighter_data()
    wc_data <- weight_class_data()
    platform <- input$platform
    
    base_col <- if(platform == "DK") "DKBase" else "FDBase"
    
    # Prepare fighter data with outcomes - WINS ONLY
    fighter_outcomes <- fighter %>%
      filter(is_win) %>%
      mutate(
        outcome_label = case_when(
          rounds_lasted == 0.5 ~ "Quick R1",
          rounds_lasted == 1 ~ "R1",
          rounds_lasted == 2 ~ "R2", 
          rounds_lasted == 3 & grepl("Decision", METHOD) ~ "Dec R3",
          rounds_lasted == 3 ~ "R3",
          rounds_lasted == 4 ~ "R4",
          rounds_lasted == 5 & grepl("Decision", METHOD) ~ "Dec R5",
          rounds_lasted == 5 ~ "R5",
          TRUE ~ "Other"
        ),
        score = !!sym(base_col),
        hover_text = paste0(
          "<b>", FIGHTER, "</b><br>",
          "Event: ", EVENT, "<br>",
          "Date: ", format(EVENT_DATE, "%Y-%m-%d"), "<br>",
          "Outcome: ", outcome_label, "<br>",
          "<b>Base: ", round(score, 2), "</b><br>",
          "Sig Str: ", SigStrikes, " | TD: ", Takedowns, "<br>",
          "KD: ", Knockdowns, " | Sub: ", SubAttempts, "<br>",
          "Ctrl: ", round(ControlTimeSec), "s"
        )
      ) %>%
      filter(outcome_label != "Other")
    
    # Prepare weight class data with outcomes - WINS ONLY
    wc_outcomes <- wc_data %>%
      filter(is_win) %>%
      mutate(
        outcome_label = case_when(
          rounds_lasted == 0.5 ~ "Quick R1",
          rounds_lasted == 1 ~ "R1",
          rounds_lasted == 2 ~ "R2",
          rounds_lasted == 3 & grepl("Decision", METHOD) ~ "Dec R3",
          rounds_lasted == 3 ~ "R3",
          rounds_lasted == 4 ~ "R4",
          rounds_lasted == 5 & grepl("Decision", METHOD) ~ "Dec R5",
          rounds_lasted == 5 ~ "R5",
          TRUE ~ "Other"
        ),
        score = !!sym(base_col)
      ) %>%
      filter(outcome_label != "Other")
    
    # Order outcomes logically
    outcome_order <- c("Quick R1", "R1", "R2", "R3", "Dec R3", "R4", "R5", "Dec R5")
    fighter_outcomes$outcome_label <- factor(fighter_outcomes$outcome_label, levels = outcome_order)
    wc_outcomes$outcome_label <- factor(wc_outcomes$outcome_label, levels = outcome_order)
    
    # Create subplots - one for each outcome
    fig <- plot_ly()
    
    for(outcome in outcome_order) {
      wc_subset <- wc_outcomes %>% filter(outcome_label == outcome)
      fighter_subset <- fighter_outcomes %>% filter(outcome_label == outcome)
      
      if(nrow(wc_subset) > 0) {
        # Add weight class violin
        fig <- fig %>%
          add_trace(
            data = wc_subset,
            x = ~outcome_label,
            y = ~score,
            type = 'violin',
            name = if(outcome == outcome_order[1]) 'Weight Class' else NULL,
            box = list(visible = TRUE),
            meanline = list(visible = TRUE),
            fillcolor = 'rgba(102, 102, 102, 0.4)',
            line = list(color = '#888888'),
            marker = list(color = '#888888'),
            hoverinfo = 'skip',
            showlegend = outcome == outcome_order[1],
            legendgroup = 'wc'
          )
      }
      
      if(nrow(fighter_subset) > 0) {
        # Add fighter points
        fig <- fig %>%
          add_trace(
            data = fighter_subset,
            x = ~outcome_label,
            y = ~score,
            type = 'scatter',
            mode = 'markers',
            name = if(outcome == outcome_order[1]) input$fighter else NULL,
            marker = list(
              size = 10,
              color = '#FFD700',
              line = list(color = '#000000', width = 2)
            ),
            text = ~hover_text,
            hoverinfo = 'text',
            showlegend = outcome == outcome_order[1],
            legendgroup = 'fighter'
          )
      }
    }
    
    fig %>%
      layout(
        xaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = outcome_order,
          color = '#d4af37',
          gridcolor = '#2a2a2a'
        ),
        yaxis = list(
          title = paste(platform, "Base Score (Wins Only)"),
          color = '#d4af37',
          gridcolor = '#2a2a2a'
        ),
        paper_bgcolor = '#0a0a0a',
        plot_bgcolor = '#0a0a0a',
        font = list(color = '#d4af37', size = 13),
        legend = list(
          orientation = 'h',
          x = 0.5,
          xanchor = 'center',
          y = 1.08,
          bgcolor = 'rgba(0,0,0,0)',
          font = list(color = '#d4af37')
        ),
        hovermode = 'closest',
        violingap = 0.3,
        violinmode = 'overlay'
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  # Round by Round Progression - Wins (Interactive)
  output$round_progression_wins <- renderPlotly({
    fighter <- fighter_data()
    wc_data <- weight_class_data()
    platform <- input$platform
    
    per_round_col <- if(platform == "DK") "DK_per_round" else "FD_per_round"
    
    # Filter for wins only
    fighter_wins <- fighter %>% filter(is_win)
    wc_wins <- wc_data %>% filter(is_win)
    
    # Prepare fighter individual data points
    fighter_points <- fighter_wins %>%
      mutate(
        rounds_label = case_when(
          rounds_lasted == 0.5 ~ "Quick R1",
          rounds_lasted == 1 ~ "R1",
          rounds_lasted == 2 ~ "R2",
          rounds_lasted == 3 ~ "R3",
          rounds_lasted == 4 ~ "R4",
          rounds_lasted == 5 ~ "R5",
          TRUE ~ as.character(rounds_lasted)
        ),
        per_round = !!sym(per_round_col),
        hover_text = paste0(
          "<b>", FIGHTER, "</b><br>",
          "Event: ", EVENT, "<br>",
          "Date: ", format(EVENT_DATE, "%Y-%m-%d"), "<br>",
          "Rounds: ", rounds_label, "<br>",
          "<b>Per Round: ", round(per_round, 2), "</b><br>",
          "Sig Str: ", SigStrikes, " | Tot: ", TotalStrikes, "<br>",
          "TD: ", Takedowns, " | KD: ", Knockdowns, "<br>",
          "Sub: ", SubAttempts, " | Ctrl: ", round(ControlTimeSec), "s"
        )
      )
    
    wc_points <- wc_wins %>%
      mutate(
        rounds_label = case_when(
          rounds_lasted == 0.5 ~ "Quick R1",
          rounds_lasted == 1 ~ "R1",
          rounds_lasted == 2 ~ "R2",
          rounds_lasted == 3 ~ "R3",
          rounds_lasted == 4 ~ "R4",
          rounds_lasted == 5 ~ "R5",
          TRUE ~ as.character(rounds_lasted)
        ),
        per_round = !!sym(per_round_col)
      )
    
    fighter_avg <- fighter_wins %>%
      group_by(rounds_lasted) %>%
      summarise(per_round = mean(!!sym(per_round_col), na.rm = TRUE), .groups = "drop") %>%
      mutate(rounds_label = case_when(
        rounds_lasted == 0.5 ~ "Quick R1", rounds_lasted == 1 ~ "R1",
        rounds_lasted == 2 ~ "R2", rounds_lasted == 3 ~ "R3",
        rounds_lasted == 4 ~ "R4", rounds_lasted == 5 ~ "R5",
        TRUE ~ as.character(rounds_lasted)
      )) %>% arrange(rounds_lasted)
    
    wc_avg <- wc_wins %>%
      group_by(rounds_lasted) %>%
      summarise(per_round = mean(!!sym(per_round_col), na.rm = TRUE), .groups = "drop") %>%
      mutate(rounds_label = case_when(
        rounds_lasted == 0.5 ~ "Quick R1", rounds_lasted == 1 ~ "R1",
        rounds_lasted == 2 ~ "R2", rounds_lasted == 3 ~ "R3",
        rounds_lasted == 4 ~ "R4", rounds_lasted == 5 ~ "R5",
        TRUE ~ as.character(rounds_lasted)
      )) %>% arrange(rounds_lasted)
    
    round_order <- c("Quick R1", "R1", "R2", "R3", "R4", "R5")
    
    plot_ly() %>%
      add_trace(data = wc_points, y = ~rounds_label, x = ~per_round, type = 'scatter', mode = 'markers',
                marker = list(color = '#888888', size = 5, opacity = 0.2), name = 'WC', hoverinfo = 'skip',
                legendgroup = 'wc') %>%
      add_trace(data = wc_avg, y = ~rounds_label, x = ~per_round, type = 'scatter', mode = 'lines+markers',
                line = list(color = '#888888', width = 2), marker = list(color = '#888888', size = 8),
                name = 'WC Avg', showlegend = TRUE, legendgroup = 'wc',
                hovertemplate = "Avg: %{x:.2f}<extra></extra>") %>%
      add_trace(data = fighter_points, y = ~rounds_label, x = ~per_round, type = 'scatter', mode = 'markers',
                marker = list(color = '#d4af37', size = 7, opacity = 0.6), name = 'Fighter',
                text = ~hover_text, hoverinfo = 'text', legendgroup = 'fighter') %>%
      add_trace(data = fighter_avg, y = ~rounds_label, x = ~per_round, type = 'scatter', mode = 'lines+markers',
                line = list(color = '#d4af37', width = 2), marker = list(color = '#d4af37', size = 8),
                name = 'Fighter Avg', showlegend = TRUE, legendgroup = 'fighter',
                hovertemplate = "Avg: %{x:.2f}<extra></extra>") %>%
      layout(yaxis = list(title = "", categoryorder = "array", categoryarray = rev(round_order),
                          color = '#d4af37', gridcolor = '#2a2a2a'),
             xaxis = list(title = paste(platform, "Per Round"), color = '#d4af37', gridcolor = '#2a2a2a'),
             paper_bgcolor = '#0a0a0a', plot_bgcolor = '#0a0a0a', font = list(color = '#d4af37', size = 11),
             legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = 1.12,
                           bgcolor = 'rgba(0,0,0,0)', font = list(color = '#d4af37', size = 9)),
             hovermode = 'closest', margin = list(l = 50, r = 10, t = 50, b = 50)) %>%
      config(displayModeBar = FALSE)
  })
  
  # Round by Round Progression - Losses (Interactive)
  output$round_progression_losses <- renderPlotly({
    fighter <- fighter_data()
    wc_data <- weight_class_data()
    platform <- input$platform
    
    per_round_col <- if(platform == "DK") "DK_per_round" else "FD_per_round"
    
    # Filter for losses only
    fighter_losses <- fighter %>% filter(!is_win)
    wc_losses <- wc_data %>% filter(!is_win)
    
    # Prepare fighter individual data points
    fighter_points <- fighter_losses %>%
      mutate(
        rounds_label = case_when(
          rounds_lasted == 0.5 ~ "Quick R1",
          rounds_lasted == 1 ~ "R1",
          rounds_lasted == 2 ~ "R2",
          rounds_lasted == 3 ~ "R3",
          rounds_lasted == 4 ~ "R4",
          rounds_lasted == 5 ~ "R5",
          TRUE ~ as.character(rounds_lasted)
        ),
        per_round = !!sym(per_round_col),
        hover_text = paste0(
          "<b>", FIGHTER, "</b><br>",
          "Event: ", EVENT, "<br>",
          "Date: ", format(EVENT_DATE, "%Y-%m-%d"), "<br>",
          "Rounds: ", rounds_label, "<br>",
          "<b>Per Round: ", round(per_round, 2), "</b><br>",
          "Sig Str: ", SigStrikes, " | Tot: ", TotalStrikes, "<br>",
          "TD: ", Takedowns, " | KD: ", Knockdowns, "<br>",
          "Sub: ", SubAttempts, " | Ctrl: ", round(ControlTimeSec), "s"
        )
      )
    
    wc_points <- wc_losses %>%
      mutate(
        rounds_label = case_when(
          rounds_lasted == 0.5 ~ "Quick R1",
          rounds_lasted == 1 ~ "R1",
          rounds_lasted == 2 ~ "R2",
          rounds_lasted == 3 ~ "R3",
          rounds_lasted == 4 ~ "R4",
          rounds_lasted == 5 ~ "R5",
          TRUE ~ as.character(rounds_lasted)
        ),
        per_round = !!sym(per_round_col)
      )
    
    fighter_avg <- fighter_losses %>%
      group_by(rounds_lasted) %>%
      summarise(per_round = mean(!!sym(per_round_col), na.rm = TRUE), .groups = "drop") %>%
      mutate(rounds_label = case_when(
        rounds_lasted == 0.5 ~ "Quick R1", rounds_lasted == 1 ~ "R1",
        rounds_lasted == 2 ~ "R2", rounds_lasted == 3 ~ "R3",
        rounds_lasted == 4 ~ "R4", rounds_lasted == 5 ~ "R5",
        TRUE ~ as.character(rounds_lasted)
      )) %>% arrange(rounds_lasted)
    
    wc_avg <- wc_losses %>%
      group_by(rounds_lasted) %>%
      summarise(per_round = mean(!!sym(per_round_col), na.rm = TRUE), .groups = "drop") %>%
      mutate(rounds_label = case_when(
        rounds_lasted == 0.5 ~ "Quick R1", rounds_lasted == 1 ~ "R1",
        rounds_lasted == 2 ~ "R2", rounds_lasted == 3 ~ "R3",
        rounds_lasted == 4 ~ "R4", rounds_lasted == 5 ~ "R5",
        TRUE ~ as.character(rounds_lasted)
      )) %>% arrange(rounds_lasted)
    
    round_order <- c("Quick R1", "R1", "R2", "R3", "R4", "R5")
    
    plot_ly() %>%
      add_trace(data = wc_points, y = ~rounds_label, x = ~per_round, type = 'scatter', mode = 'markers',
                marker = list(color = '#888888', size = 5, opacity = 0.2), name = 'WC', hoverinfo = 'skip',
                legendgroup = 'wc') %>%
      add_trace(data = wc_avg, y = ~rounds_label, x = ~per_round, type = 'scatter', mode = 'lines+markers',
                line = list(color = '#888888', width = 2), marker = list(color = '#888888', size = 8),
                name = 'WC Avg', showlegend = TRUE, legendgroup = 'wc',
                hovertemplate = "Avg: %{x:.2f}<extra></extra>") %>%
      add_trace(data = fighter_points, y = ~rounds_label, x = ~per_round, type = 'scatter', mode = 'markers',
                marker = list(color = '#d4af37', size = 7, opacity = 0.6), name = 'Fighter',
                text = ~hover_text, hoverinfo = 'text', legendgroup = 'fighter') %>%
      add_trace(data = fighter_avg, y = ~rounds_label, x = ~per_round, type = 'scatter', mode = 'lines+markers',
                line = list(color = '#d4af37', width = 2), marker = list(color = '#d4af37', size = 8),
                name = 'Fighter Avg', showlegend = TRUE, legendgroup = 'fighter',
                hovertemplate = "Avg: %{x:.2f}<extra></extra>") %>%
      layout(yaxis = list(title = "", categoryorder = "array", categoryarray = rev(round_order),
                          color = '#d4af37', gridcolor = '#2a2a2a'),
             xaxis = list(title = paste(platform, "Per Round"), color = '#d4af37', gridcolor = '#2a2a2a'),
             paper_bgcolor = '#0a0a0a', plot_bgcolor = '#0a0a0a', font = list(color = '#d4af37', size = 11),
             legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = 1.12,
                           bgcolor = 'rgba(0,0,0,0)', font = list(color = '#d4af37', size = 9)),
             hovermode = 'closest', margin = list(l = 50, r = 10, t = 50, b = 50)) %>%
      config(displayModeBar = FALSE)
  })
  
  
  # Helper function to calculate percentile
  calculate_percentile <- function(value, weight_class_values) {
    mean(weight_class_values <= value, na.rm = TRUE) * 100
  }
  
  # Fighter History Table - Wins
  output$fighter_history_wins <- renderDT({
    fighter <- fighter_data()
    wc_data <- weight_class_data()
    platform <- input$platform
    
    base_col <- if(platform == "DK") "DKBase" else "FDBase"
    win_col <- if(platform == "DK") "DKWin" else "FDWin"
    score_col <- if(platform == "DK") "DKScore" else "FDScore"
    
    # Filter wins
    wins <- fighter %>% filter(is_win)
    
    if(nrow(wins) == 0) return(datatable(data.frame(Message = "No wins found")))
    
    # Calculate percentiles for each fight
    wins <- wins %>%
      mutate(
        outcome_label = case_when(
          rounds_lasted == 0.5 ~ "Quick R1",
          rounds_lasted == 1 ~ "R1",
          rounds_lasted == 2 ~ "R2",
          rounds_lasted == 3 & grepl("Decision", METHOD) ~ "Dec R3",
          rounds_lasted == 3 ~ "R3",
          rounds_lasted == 4 ~ "R4",
          rounds_lasted == 5 & grepl("Decision", METHOD) ~ "Dec R5",
          rounds_lasted == 5 ~ "R5",
          TRUE ~ "Other"
        )
      )
    
    # Calculate percentiles using a loop to avoid rowwise issues
    wins$base_percentile <- NA_real_
    for(i in 1:nrow(wins)) {
      outcome_data <- wc_data %>% 
        filter(is_win, 
               abs(rounds_lasted - wins$rounds_lasted[i]) < 0.1)
      if(nrow(outcome_data) > 0) {
        wins$base_percentile[i] <- calculate_percentile(wins[[base_col]][i], outcome_data[[base_col]])
      }
    }
    
    # Select and format columns
    data <- wins %>%
      select(EVENT_DATE, EVENT, WEIGHTCLASS, outcome_label, METHOD,
             SigStrikes, TotalStrikes, Takedowns, Knockdowns, SubAttempts,
             ControlTimeSec, all_of(base_col), all_of(win_col), all_of(score_col),
             base_percentile) %>%
      mutate(
        EVENT_DATE = format(as.Date(EVENT_DATE), "%Y-%m-%d"),
        ControlTimeSec = round(ControlTimeSec, 0),
        !!base_col := round(!!sym(base_col), 2),
        !!score_col := round(!!sym(score_col), 2),
        base_percentile = round(base_percentile, 0)
      )
    
    colnames(data) <- c("Date", "Event", "Weight", "Outcome", "Method",
                        "Sig Str", "Tot Str", "TD", "KD", "Sub Att",
                        "Ctrl Sec", "Base", "Win Bonus", "Total", "Percentile")
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        dom = 'frtip',
        scrollX = TRUE,
        ordering = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          list(width = '90px', targets = 14)  # Percentile column
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'Percentile',
        background = styleColorBar(c(0, 100), '#d4af37'),
        backgroundSize = '98% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        color = '#FFFFFF'
      )
  })
  
  # Fighter History Table - Losses
  output$fighter_history_losses <- renderDT({
    fighter <- fighter_data()
    wc_data <- weight_class_data()
    platform <- input$platform
    
    base_col <- if(platform == "DK") "DKBase" else "FDBase"
    win_col <- if(platform == "DK") "DKWin" else "FDWin"
    score_col <- if(platform == "DK") "DKScore" else "FDScore"
    
    # Filter losses
    losses <- fighter %>% filter(!is_win)
    
    if(nrow(losses) == 0) return(datatable(data.frame(Message = "No losses found")))
    
    # Calculate percentiles for each fight
    losses <- losses %>%
      mutate(
        outcome_label = case_when(
          rounds_lasted == 0.5 ~ "Quick R1",
          rounds_lasted == 1 ~ "R1",
          rounds_lasted == 2 ~ "R2",
          rounds_lasted == 3 & grepl("Decision", METHOD) ~ "Dec R3",
          rounds_lasted == 3 ~ "R3",
          rounds_lasted == 4 ~ "R4",
          rounds_lasted == 5 & grepl("Decision", METHOD) ~ "Dec R5",
          rounds_lasted == 5 ~ "R5",
          TRUE ~ "Other"
        )
      )
    
    # Calculate percentiles using a loop to avoid rowwise issues
    losses$base_percentile <- NA_real_
    for(i in 1:nrow(losses)) {
      outcome_data <- wc_data %>% 
        filter(!is_win, 
               abs(rounds_lasted - losses$rounds_lasted[i]) < 0.1)
      if(nrow(outcome_data) > 0) {
        losses$base_percentile[i] <- calculate_percentile(losses[[base_col]][i], outcome_data[[base_col]])
      }
    }
    
    # Select and format columns
    data <- losses %>%
      select(EVENT_DATE, EVENT, WEIGHTCLASS, outcome_label, METHOD,
             SigStrikes, TotalStrikes, Takedowns, Knockdowns, SubAttempts,
             ControlTimeSec, all_of(base_col), all_of(win_col), all_of(score_col),
             base_percentile) %>%
      mutate(
        EVENT_DATE = format(as.Date(EVENT_DATE), "%Y-%m-%d"),
        ControlTimeSec = round(ControlTimeSec, 0),
        !!base_col := round(!!sym(base_col), 2),
        !!score_col := round(!!sym(score_col), 2),
        base_percentile = round(base_percentile, 0)
      )
    
    colnames(data) <- c("Date", "Event", "Weight", "Outcome", "Method",
                        "Sig Str", "Tot Str", "TD", "KD", "Sub Att",
                        "Ctrl Sec", "Base", "Win Bonus", "Total", "Percentile")
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        dom = 'frtip',
        scrollX = TRUE,
        ordering = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all'),
          list(width = '90px', targets = 14)  # Percentile column
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'Percentile',
        background = styleColorBar(c(0, 100), '#d4af37'),
        backgroundSize = '98% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center',
        color = '#FFFFFF'
      )
  })
}

shinyApp(ui = ui, server = server)
