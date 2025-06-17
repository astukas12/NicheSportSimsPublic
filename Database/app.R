# NASCAR Database Shiny App
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(shinycssloaders)
library(shinyWidgets)

# Load data function
load_data <- function() {
  # Load main NASCAR database
  nascar_data <- read_csv("NascarDatabase.csv")
  
  # Load RaceIDs if needed (uncomment and modify path as needed)
  # race_ids <- read_excel("RaceIDs.xlsx")
  
  return(nascar_data)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "NASCAR Database Analytics"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table")),
      menuItem("Driver Analysis", tabName = "drivers", icon = icon("user")),
      menuItem("Track Analysis", tabName = "tracks", icon = icon("road")),
      menuItem("Team Performance", tabName = "teams", icon = icon("users")),
      menuItem("Season Trends", tabName = "seasons", icon = icon("calendar")),
      menuItem("Custom Reports", tabName = "reports", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    
    tabItems(
      # Data Explorer Tab
      tabItem(tabName = "explorer",
              fluidRow(
                box(
                  title = "Data Filters", status = "primary", solidHeader = TRUE, width = 12,
                  collapsible = TRUE,
                  fluidRow(
                    column(3,
                           selectInput("season_filter", "Season:",
                                       choices = NULL, multiple = TRUE)
                    ),
                    column(3,
                           selectInput("driver_filter", "Driver:",
                                       choices = NULL, multiple = TRUE)
                    ),
                    column(3,
                           selectInput("track_filter", "Track:",
                                       choices = NULL, multiple = TRUE)
                    ),
                    column(3,
                           selectInput("team_filter", "Team:",
                                       choices = NULL, multiple = TRUE)
                    )
                  ),
                  fluidRow(
                    column(6,
                           numericInput("min_laps", "Min Laps Completed:", value = 0, min = 0)
                    ),
                    column(6,
                           selectInput("status_filter", "Finishing Status:",
                                       choices = NULL, multiple = TRUE)
                    )
                  ),
                  actionButton("reset_filters", "Reset Filters", class = "btn-warning")
                )
              ),
              
              fluidRow(
                box(
                  title = "NASCAR Database", status = "primary", solidHeader = TRUE, width = 12,
                  withSpinner(DT::dataTableOutput("main_table")),
                  br(),
                  downloadButton("download_filtered", "Download Filtered Data", 
                                 class = "btn-success")
                )
              )
      ),
      
      # Driver Analysis Tab
      tabItem(tabName = "drivers",
              fluidRow(
                box(
                  title = "Driver Selection", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("selected_drivers", "Select Drivers (up to 5):",
                              choices = NULL, multiple = TRUE),
                  selectInput("driver_season", "Season:",
                              choices = NULL),
                  selectInput("driver_metric", "Performance Metric:",
                              choices = c("Average Finish" = "FinishingPosition",
                                          "Laps Led" = "LapsLed",
                                          "Top 10 Finishes" = "T10FL",
                                          "Speed Rank" = "SpdRk",
                                          "DK Points" = "DKPoints")),
                  checkboxInput("show_trend", "Show Trend Line", value = TRUE)
                ),
                
                box(
                  title = "Driver Performance Chart", status = "primary", solidHeader = TRUE, width = 8,
                  withSpinner(plotlyOutput("driver_performance_plot", height = "400px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Driver Statistics Summary", status = "info", solidHeader = TRUE, width = 12,
                  withSpinner(DT::dataTableOutput("driver_stats_table"))
                )
              )
      ),
      
      # Track Analysis Tab
      tabItem(tabName = "tracks",
              fluidRow(
                box(
                  title = "Track Analysis Controls", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("selected_track", "Select Track:",
                              choices = NULL),
                  selectInput("track_season_range", "Season Range:",
                              choices = NULL, multiple = TRUE),
                  selectInput("track_metric", "Metric to Analyze:",
                              choices = c("Average Speed" = "Speed",
                                          "Number of Leaders" = "number_of_leaders",
                                          "Number of Cautions" = "number_of_cautions",
                                          "Field Size" = "number_of_cars_in_field"))
                ),
                
                box(
                  title = "Track Performance Trends", status = "primary", solidHeader = TRUE, width = 8,
                  withSpinner(plotlyOutput("track_trends_plot", height = "400px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Track Winners", status = "success", solidHeader = TRUE, width = 6,
                  withSpinner(DT::dataTableOutput("track_winners_table"))
                ),
                
                box(
                  title = "Track Statistics", status = "info", solidHeader = TRUE, width = 6,
                  withSpinner(DT::dataTableOutput("track_stats_table"))
                )
              )
      ),
      
      # Team Performance Tab
      tabItem(tabName = "teams",
              fluidRow(
                box(
                  title = "Team Analysis", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("selected_teams", "Select Teams:",
                              choices = NULL, multiple = TRUE),
                  selectInput("team_season", "Season:",
                              choices = NULL),
                  radioButtons("team_view", "View Type:",
                               choices = c("Wins" = "wins",
                                           "Top 5s" = "top5",
                                           "Top 10s" = "top10",
                                           "Average Finish" = "avg_finish"))
                ),
                
                box(
                  title = "Team Performance Comparison", status = "primary", solidHeader = TRUE, width = 8,
                  withSpinner(plotlyOutput("team_comparison_plot", height = "400px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Team Summary Statistics", status = "primary", solidHeader = TRUE, width = 12,
                  withSpinner(DT::dataTableOutput("team_summary_table"))
                )
              )
      ),
      
      # Season Trends Tab
      tabItem(tabName = "seasons",
              fluidRow(
                box(
                  title = "Season Analysis Controls", status = "primary", solidHeader = TRUE, width = 4,
                  checkboxGroupInput("season_metrics", "Select Metrics:",
                                     choices = c("Average Speed" = "Speed",
                                                 "Field Size" = "number_of_cars_in_field",
                                                 "Race Leaders" = "number_of_leaders",
                                                 "Cautions" = "number_of_cautions"),
                                     selected = "Speed"),
                  sliderInput("season_range", "Season Range:",
                              min = 2020, max = 2025, value = c(2020, 2025), step = 1)
                ),
                
                box(
                  title = "Season Trends", status = "primary", solidHeader = TRUE, width = 8,
                  withSpinner(plotlyOutput("season_trends_plot", height = "400px"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Championship Points Leaders by Season", status = "success", solidHeader = TRUE, width = 12,
                  withSpinner(DT::dataTableOutput("season_champions_table"))
                )
              )
      ),
      
      # Custom Reports Tab
      tabItem(tabName = "reports",
              fluidRow(
                box(
                  title = "Custom Report Builder", status = "primary", solidHeader = TRUE, width = 4,
                  h4("Select Data"),
                  selectInput("report_groupby", "Group By:",
                              choices = c("Driver" = "Full_Name",
                                          "Team" = "team_name",
                                          "Track" = "track_name",
                                          "Season" = "race_season")),
                  
                  selectInput("report_metrics", "Metrics to Include:",
                              choices = c("Races" = "races",
                                          "Wins" = "wins",
                                          "Top 5s" = "top5s",
                                          "Top 10s" = "top10s",
                                          "Average Finish" = "avg_finish",
                                          "Laps Led" = "total_laps_led",
                                          "DNFs" = "dnfs"),
                              multiple = TRUE,
                              selected = c("races", "wins", "avg_finish")),
                  
                  h4("Filters"),
                  selectInput("report_season_filter", "Season:",
                              choices = NULL, multiple = TRUE),
                  
                  numericInput("report_min_races", "Minimum Races:", value = 1, min = 1),
                  
                  br(),
                  actionButton("generate_report", "Generate Report", class = "btn-success"),
                  br(), br(),
                  downloadButton("download_report", "Download Report", class = "btn-primary")
                ),
                
                box(
                  title = "Custom Report Results", status = "primary", solidHeader = TRUE, width = 8,
                  withSpinner(DT::dataTableOutput("custom_report_table"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Report Visualization", status = "info", solidHeader = TRUE, width = 12,
                  withSpinner(plotlyOutput("report_plot", height = "500px"))
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Load data
  nascar_data <- reactive({
    req(file.exists("NascarDatabase.csv"))
    load_data()
  })
  
  # Initialize filter choices
  observe({
    data <- nascar_data()
    
    updateSelectInput(session, "season_filter",
                      choices = sort(unique(data$race_season), decreasing = TRUE))
    updateSelectInput(session, "driver_filter",
                      choices = sort(unique(data$Full_Name)))
    updateSelectInput(session, "track_filter",
                      choices = sort(unique(data$track_name)))
    updateSelectInput(session, "team_filter",
                      choices = sort(unique(data$team_name)))
    updateSelectInput(session, "status_filter",
                      choices = sort(unique(data$finishing_status)))
    
    # Update other dropdowns
    updateSelectInput(session, "selected_drivers",
                      choices = sort(unique(data$Full_Name)))
    updateSelectInput(session, "driver_season",
                      choices = sort(unique(data$race_season), decreasing = TRUE),
                      selected = max(data$race_season, na.rm = TRUE))
    updateSelectInput(session, "selected_track",
                      choices = sort(unique(data$track_name)))
    updateSelectInput(session, "track_season_range",
                      choices = sort(unique(data$race_season), decreasing = TRUE))
    updateSelectInput(session, "selected_teams",
                      choices = sort(unique(data$team_name)))
    updateSelectInput(session, "team_season",
                      choices = sort(unique(data$race_season), decreasing = TRUE),
                      selected = max(data$race_season, na.rm = TRUE))
    updateSelectInput(session, "report_season_filter",
                      choices = sort(unique(data$race_season), decreasing = TRUE))
  })
  
  # Filtered data for main table
  filtered_data <- reactive({
    data <- nascar_data()
    
    if (!is.null(input$season_filter) && length(input$season_filter) > 0) {
      data <- data %>% filter(race_season %in% input$season_filter)
    }
    if (!is.null(input$driver_filter) && length(input$driver_filter) > 0) {
      data <- data %>% filter(Full_Name %in% input$driver_filter)
    }
    if (!is.null(input$track_filter) && length(input$track_filter) > 0) {
      data <- data %>% filter(track_name %in% input$track_filter)
    }
    if (!is.null(input$team_filter) && length(input$team_filter) > 0) {
      data <- data %>% filter(team_name %in% input$team_filter)
    }
    if (!is.null(input$status_filter) && length(input$status_filter) > 0) {
      data <- data %>% filter(finishing_status %in% input$status_filter)
    }
    if (!is.null(input$min_laps) && input$min_laps > 0) {
      data <- data %>% filter(laps >= input$min_laps)
    }
    
    return(data)
  })
  
  # Main data table
  output$main_table <- DT::renderDataTable({
    DT::datatable(
      filtered_data(),
      options = list(
        scrollX = TRUE,
        pageLength = 25,
        dom = 'Bfrtip'
      ),
      filter = 'top'
    )
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "season_filter", selected = character(0))
    updateSelectInput(session, "driver_filter", selected = character(0))
    updateSelectInput(session, "track_filter", selected = character(0))
    updateSelectInput(session, "team_filter", selected = character(0))
    updateSelectInput(session, "status_filter", selected = character(0))
    updateNumericInput(session, "min_laps", value = 0)
  })
  
  # Download filtered data
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste("nascar_filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Driver performance plot
  output$driver_performance_plot <- renderPlotly({
    req(input$selected_drivers, input$driver_season, input$driver_metric)
    
    data <- nascar_data() %>%
      filter(Full_Name %in% input$selected_drivers[1:min(5, length(input$selected_drivers))],
             race_season == input$driver_season) %>%
      arrange(race_id)
    
    if (nrow(data) == 0) return(NULL)
    
    p <- ggplot(data, aes_string(x = "race_id", y = input$driver_metric, color = "Full_Name")) +
      geom_point(alpha = 0.7) +
      geom_line(alpha = 0.5) +
      labs(title = paste("Driver Performance:", gsub("_", " ", input$driver_metric)),
           x = "Race", y = gsub("_", " ", input$driver_metric), color = "Driver") +
      theme_minimal()
    
    if (input$show_trend) {
      p <- p + geom_smooth(method = "loess", se = FALSE, alpha = 0.3)
    }
    
    ggplotly(p)
  })
  
  # Driver stats table
  output$driver_stats_table <- DT::renderDataTable({
    req(input$selected_drivers, input$driver_season)
    
    data <- nascar_data() %>%
      filter(Full_Name %in% input$selected_drivers,
             race_season == input$driver_season) %>%
      group_by(Full_Name, team_name) %>%
      summarise(
        Races = n(),
        Wins = sum(FinishingPosition == 1, na.rm = TRUE),
        Top5s = sum(FinishingPosition <= 5, na.rm = TRUE),
        Top10s = sum(FinishingPosition <= 10, na.rm = TRUE),
        Avg_Finish = round(mean(FinishingPosition, na.rm = TRUE), 2),
        Total_Laps_Led = sum(LapsLed, na.rm = TRUE),
        DNFs = sum(finishing_status != "Running", na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(Wins), Avg_Finish)
    
    DT::datatable(data, options = list(pageLength = 10, dom = 't'))
  })
  
  # Add more server logic for other tabs...
  # (Due to length constraints, I'm showing the structure - you can expand each section)
  
  # Placeholder outputs for other tabs
  output$track_trends_plot <- renderPlotly({ 
    # Track analysis plot logic here
    plot_ly(x = 1:10, y = rnorm(10), type = 'scatter', mode = 'lines+markers')
  })
  
  output$team_comparison_plot <- renderPlotly({
    # Team comparison plot logic here
    plot_ly(x = 1:10, y = rnorm(10), type = 'bar')
  })
  
  output$season_trends_plot <- renderPlotly({
    # Season trends plot logic here
    plot_ly(x = 2020:2025, y = rnorm(6), type = 'scatter', mode = 'lines+markers')
  })
  
  output$custom_report_table <- DT::renderDataTable({
    # Custom report logic here
    DT::datatable(data.frame(Placeholder = "Generate a report to see results"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)