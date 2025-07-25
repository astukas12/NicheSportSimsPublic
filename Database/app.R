# NASCAR Data Explorer
# Simplified version with just the data explorer functionality

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readr)
library(readxl)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(openxlsx)

#--------------------- Data Loading Functions ---------------------#

# Load main NASCAR database
load_nascar_database <- function() {
  if (file.exists("NascarDatabase.csv")) {
    return(read_csv("NascarDatabase.csv"))
  } else {
    return(NULL)
  }
}

#--------------------- UI Definition ---------------------#

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "NASCAR Data Explorer",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Data Explorer", tabName = "data_explorer", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    # Custom CSS - Black and Red Theme
    tags$head(
      tags$style(HTML("
        /* Main theme colors */
        .content-wrapper, .right-side { 
          background-color: #1a1a1a !important; 
          color: #ffffff !important;
        }
        
        /* Sidebar styling */
        .main-sidebar { 
          background-color: #000000 !important; 
        }
        .sidebar-menu > li > a {
          color: #ffffff !important;
          border-left: 3px solid transparent;
        }
        .sidebar-menu > li.active > a,
        .sidebar-menu > li:hover > a {
          background-color: #dc143c !important;
          border-left: 3px solid #ff4444 !important;
          color: #ffffff !important;
        }
        
        /* Header styling */
        .main-header .navbar {
          background-color: #000000 !important;
        }
        .main-header .logo {
          background-color: #dc143c !important;
          color: #ffffff !important;
          border-bottom: none !important;
        }
        .main-header .logo:hover {
          background-color: #b91c3c !important;
        }
        
        /* Box styling */
        .box { 
          background-color: #2d2d2d !important;
          border: 1px solid #444444 !important;
          border-radius: 3px; 
          box-shadow: 0 1px 3px rgba(0,0,0,.3) !important;
          color: #ffffff !important;
        }
        .box:hover { 
          box-shadow: 0 3px 6px rgba(220, 20, 60, 0.3) !important; 
        }
        .box-header {
          background-color: #333333 !important;
          color: #ffffff !important;
          border-bottom: 1px solid #555555 !important;
        }
        .box-header .box-title {
          color: #ffffff !important;
        }
        .box-primary {
          border-top-color: #dc143c !important;
        }
        .box-info {
          border-top-color: #ff4444 !important;
        }
        .box-success {
          border-top-color: #dc143c !important;
        }
        .box-warning {
          border-top-color: #ff6666 !important;
        }
        
        /* DataTable styling */
        .dataTable th { 
          background-color: #333333 !important;
          color: #ffffff !important;
          border-bottom: 2px solid #dc143c !important;
        }
        .dataTable td {
          background-color: #2d2d2d !important;
          color: #ffffff !important;
          border-bottom: 1px solid #444444 !important;
        }
        .dataTable tbody tr:hover {
          background-color: #404040 !important;
        }
        .dataTables_wrapper {
          color: #ffffff !important;
        }
        .dataTables_info, .dataTables_length label, .dataTables_filter label {
          color: #ffffff !important;
        }
        .dataTables_paginate .paginate_button {
          background-color: #333333 !important;
          color: #ffffff !important;
          border: 1px solid #555555 !important;
        }
        .dataTables_paginate .paginate_button:hover {
          background-color: #dc143c !important;
          color: #ffffff !important;
        }
        .dataTables_paginate .paginate_button.current {
          background-color: #dc143c !important;
          color: #ffffff !important;
        }
        
        /* Form controls styling */
        .form-control {
          background-color: #404040 !important;
          border: 1px solid #666666 !important;
          color: #ffffff !important;
        }
        .form-control:focus {
          border-color: #dc143c !important;
          box-shadow: 0 0 0 0.2rem rgba(220, 20, 60, 0.25) !important;
        }
        .selectize-control.single .selectize-input,
        .selectize-control.multi .selectize-input {
          background-color: #404040 !important;
          border: 1px solid #666666 !important;
          color: #ffffff !important;
        }
        .selectize-dropdown {
          background-color: #333333 !important;
          border: 1px solid #666666 !important;
          color: #ffffff !important;
        }
        .selectize-dropdown-content .option {
          color: #ffffff !important;
        }
        .selectize-dropdown-content .option:hover {
          background-color: #dc143c !important;
          color: #ffffff !important;
        }
        
        /* Button styling */
        .btn-primary {
          background-color: #dc143c !important;
          border-color: #dc143c !important;
          color: #ffffff !important;
        }
        .btn-primary:hover {
          background-color: #b91c3c !important;
          border-color: #b91c3c !important;
        }
        .btn-success {
          background-color: #dc143c !important;
          border-color: #dc143c !important;
        }
        .btn-success:hover {
          background-color: #b91c3c !important;
          border-color: #b91c3c !important;
        }
        .btn-warning {
          background-color: #ff4444 !important;
          border-color: #ff4444 !important;
          color: #ffffff !important;
        }
        .btn-warning:hover {
          background-color: #ff6666 !important;
          border-color: #ff6666 !important;
        }
        
        /* Labels */
        label {
          color: #ffffff !important;
        }
        
        /* Loading spinner */
        .loading-spinner { 
          display: inline-block; width: 20px; height: 20px; 
          border: 3px solid rgba(255,255,255,0.1); border-radius: 50%; 
          border-top-color: #dc143c; animation: spin 1s ease-in-out infinite; 
        }
        @keyframes spin { to { transform: rotate(360deg); } }
      "))
    ),
    
    tabItems(
      # Data Explorer Tab
      tabItem(tabName = "data_explorer",
              fluidRow(
                box(
                  title = "Data Filters", status = "primary", solidHeader = TRUE, width = 12,
                  collapsible = TRUE,
                  fluidRow(
                    column(2, 
                           selectizeInput("season_filter", "Season:", 
                                          choices = NULL, multiple = TRUE,
                                          options = list(placeholder = "All Seasons"))
                    ),
                    column(2, 
                           selectizeInput("series_filter", "Series:", 
                                          choices = c("Cup Series" = 1, "Xfinity Series" = 2, "Truck Series" = 3),
                                          multiple = TRUE,
                                          options = list(placeholder = "All Series"))
                    ),
                    column(2, 
                           selectizeInput("driver_filter", "Driver:", 
                                          choices = NULL, multiple = TRUE,
                                          options = list(placeholder = "All Drivers"))
                    ),
                    column(2, 
                           selectizeInput("track_filter", "Track:", 
                                          choices = NULL, multiple = TRUE,
                                          options = list(placeholder = "All Tracks"))
                    ),
                    column(2, 
                           selectizeInput("team_filter", "Team:", 
                                          choices = NULL, multiple = TRUE,
                                          options = list(placeholder = "All Teams"))
                    ),
                    column(2, 
                           div(
                             actionButton("reset_filters", "Reset All Filters", 
                                          class = "btn-warning", style = "margin-top: 25px; margin-bottom: 10px; width: 100%;"),
                             br(),
                             downloadButton("download_filtered_csv", "Download CSV", 
                                            class = "btn-success", style = "width: 100%;")
                           )
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "NASCAR Data Explorer", status = "primary", solidHeader = TRUE, width = 12,
                  withSpinner(DT::dataTableOutput("main_explorer_table"))
                )
              )
      )
    )
  )
)

#--------------------- Server Function ---------------------#

server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    nascar_data = NULL
  )
  
  # Load initial data
  observe({
    withProgress(message = 'Loading NASCAR Database...', {
      values$nascar_data <- load_nascar_database()
      
      if (!is.null(values$nascar_data)) {
        incProgress(0.3, detail = "Processing seasons...")
        seasons <- sort(unique(values$nascar_data$race_season), decreasing = TRUE)
        
        incProgress(0.5, detail = "Processing drivers...")
        drivers <- sort(unique(values$nascar_data$Full_Name[!is.na(values$nascar_data$Full_Name)]))
        
        incProgress(0.7, detail = "Processing tracks...")
        tracks <- sort(unique(values$nascar_data$track_name[!is.na(values$nascar_data$track_name)]))
        
        incProgress(0.8, detail = "Processing teams...")
        teams <- sort(unique(values$nascar_data$team_name[!is.na(values$nascar_data$team_name)]))
        
        # Initialize filter choices
        updateSelectizeInput(session, "season_filter", choices = seasons)
        updateSelectizeInput(session, "driver_filter", choices = drivers)
        updateSelectizeInput(session, "track_filter", choices = tracks)
        updateSelectizeInput(session, "team_filter", choices = teams)
        
        incProgress(1.0, detail = "Complete!")
      }
    })
  })
  
  # Reactive data for filter updates (cascading filters)
  filter_base_data <- reactive({
    req(values$nascar_data)
    data <- values$nascar_data
    
    # Apply season filter first
    if (!is.null(input$season_filter) && length(input$season_filter) > 0) {
      data <- data %>% filter(race_season %in% input$season_filter)
    }
    
    # Apply series filter
    if (!is.null(input$series_filter) && length(input$series_filter) > 0) {
      data <- data %>% filter(series_id %in% as.numeric(input$series_filter))
    }
    
    return(data)
  })
  
  # Update driver choices based on other filters
  observe({
    base_data <- filter_base_data()
    if (!is.null(base_data)) {
      # Further filter by track and team if selected
      if (!is.null(input$track_filter) && length(input$track_filter) > 0) {
        base_data <- base_data %>% filter(track_name %in% input$track_filter)
      }
      if (!is.null(input$team_filter) && length(input$team_filter) > 0) {
        base_data <- base_data %>% filter(team_name %in% input$team_filter)
      }
      
      available_drivers <- sort(unique(base_data$Full_Name[!is.na(base_data$Full_Name)]))
      updateSelectizeInput(session, "driver_filter", choices = available_drivers, 
                           selected = input$driver_filter[input$driver_filter %in% available_drivers])
    }
  })
  
  # Update track choices based on other filters
  observe({
    base_data <- filter_base_data()
    if (!is.null(base_data)) {
      # Further filter by driver and team if selected
      if (!is.null(input$driver_filter) && length(input$driver_filter) > 0) {
        base_data <- base_data %>% filter(Full_Name %in% input$driver_filter)
      }
      if (!is.null(input$team_filter) && length(input$team_filter) > 0) {
        base_data <- base_data %>% filter(team_name %in% input$team_filter)
      }
      
      available_tracks <- sort(unique(base_data$track_name[!is.na(base_data$track_name)]))
      updateSelectizeInput(session, "track_filter", choices = available_tracks,
                           selected = input$track_filter[input$track_filter %in% available_tracks])
    }
  })
  
  # Update team choices based on other filters
  observe({
    base_data <- filter_base_data()
    if (!is.null(base_data)) {
      # Further filter by driver and track if selected
      if (!is.null(input$driver_filter) && length(input$driver_filter) > 0) {
        base_data <- base_data %>% filter(Full_Name %in% input$driver_filter)
      }
      if (!is.null(input$track_filter) && length(input$track_filter) > 0) {
        base_data <- base_data %>% filter(track_name %in% input$track_filter)
      }
      
      available_teams <- sort(unique(base_data$team_name[!is.na(base_data$team_name)]))
      updateSelectizeInput(session, "team_filter", choices = available_teams,
                           selected = input$team_filter[input$team_filter %in% available_teams])
    }
  })
  
  # Filtered data
  filtered_data <- reactive({
    req(values$nascar_data)
    
    withProgress(message = 'Applying filters...', {
      data <- values$nascar_data
      
      incProgress(0.2, detail = "Season filter...")
      if (!is.null(input$season_filter) && length(input$season_filter) > 0) {
        data <- data %>% filter(race_season %in% input$season_filter)
      }
      
      incProgress(0.4, detail = "Series filter...")
      if (!is.null(input$series_filter) && length(input$series_filter) > 0) {
        data <- data %>% filter(series_id %in% as.numeric(input$series_filter))
      }
      
      incProgress(0.6, detail = "Driver filter...")
      if (!is.null(input$driver_filter) && length(input$driver_filter) > 0) {
        data <- data %>% filter(Full_Name %in% input$driver_filter)
      }
      
      incProgress(0.8, detail = "Track filter...")
      if (!is.null(input$track_filter) && length(input$track_filter) > 0) {
        data <- data %>% filter(track_name %in% input$track_filter)
      }
      
      incProgress(0.9, detail = "Team filter...")
      if (!is.null(input$team_filter) && length(input$team_filter) > 0) {
        data <- data %>% filter(team_name %in% input$team_filter)
      }
      
      incProgress(1.0, detail = "Complete!")
      return(data)
    })
  })
  
  # Main explorer table with smooth gradient and black/red theme
  output$main_explorer_table <- DT::renderDataTable({
    req(filtered_data())
    
    withProgress(message = 'Rendering table...', {
      # Default columns to display
      default_columns <- c("Full_Name", "start_ps", "ps", "ARP", "SpdRk", "fl", "ll", 
                           "DKSP", "FDSP", "DKDomRank", "FDDomRank", "car_number", 
                           "team_name", "finishing_status", "LapsDown", "race_season", "track_name")
      
      # Filter to only columns that exist in data
      valid_columns <- intersect(default_columns, names(filtered_data()))
      
      # Prepare data with rounding for ARP
      display_data <- filtered_data() %>%
        select(all_of(valid_columns)) %>%
        mutate(
          ARP = if("ARP" %in% names(.)) round(ARP, 1) else ARP
        ) %>%
        rename_with(~ case_when(
          .x == "Full_Name" ~ "Driver Name",
          .x == "start_ps" ~ "Start",
          .x == "ps" ~ "Finish", 
          .x == "fl" ~ "FL",
          .x == "ll" ~ "LL",
          .x == "DKSP" ~ "DK Speed Pts",
          .x == "FDSP" ~ "FD Speed Pts",
          .x == "car_number" ~ "Car Number",
          .x == "team_name" ~ "Team",
          .x == "finishing_status" ~ "Finishing Status",
          .x == "LapsDown" ~ "Laps Down",
          .x == "race_season" ~ "Season",
          .x == "track_name" ~ "Track",
          TRUE ~ .x
        ))
      
      incProgress(0.5, detail = "Applying formatting...")
      
      # Create the datatable with dark theme
      dt <- DT::datatable(
        display_data,
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = 'frtip',
          deferRender = TRUE,
          scroller = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          ),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#333333', 'color': '#ffffff'});",
            "}"
          )
        ),
        filter = 'top',
        rownames = FALSE,
        escape = FALSE,
        class = "display nowrap compact"
      )
      
      # Apply smooth gradient formatting for position-related columns
      position_columns <- intersect(c("Start", "Finish", "ARP", "SpdRk"), 
                                    names(display_data))
      
      incProgress(0.8, detail = "Adding gradient formatting...")
      
      for (col in position_columns) {
        if (col %in% names(display_data)) {
          # Get the range of values for this column
          col_values <- display_data[[col]]
          col_values <- col_values[!is.na(col_values)]
          
          if (length(col_values) > 0) {
            min_val <- min(col_values)
            max_val <- max(col_values)
            
            # Create smooth gradient from black (best) to red (worst)
            dt <- dt %>% formatStyle(
              col,
              backgroundColor = JS(paste0(
                "function(value, type, row) {",
                "if (type === 'display' && value !== null && value !== '') {",
                "  var min = ", min_val, ";",
                "  var max = ", max_val, ";",
                "  var normalized = (value - min) / (max - min);",
                "  var red = Math.round(220 + (255 - 220) * normalized);", # From dark red to bright red
                "  var green = Math.round(20 * (1 - normalized));",        # From some red to no green
                "  var blue = Math.round(60 * (1 - normalized));",         # From some blue to no blue
                "  return 'rgb(' + red + ',' + green + ',' + blue + ')';",
                "} else {",
                "  return '#2d2d2d';", # Default dark background
                "}",
                "}"
              )),
              color = "#ffffff",
              fontWeight = "bold"
            )
          }
        }
      }
      
      incProgress(1.0, detail = "Complete!")
      return(dt)
    })
  })
  
  # Download CSV handler
  output$download_filtered_csv <- downloadHandler(
    filename = function() {
      paste("nascar_filtered_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(filtered_data())
      
      # Default columns to display
      default_columns <- c("Full_Name", "start_ps", "ps", "ARP", "SpdRk", "fl", "ll", 
                           "DKSP", "FDSP", "DKDomRank", "FDDomRank", "car_number", 
                           "team_name", "finishing_status", "LapsDown", "race_season", "track_name")
      
      valid_columns <- intersect(default_columns, names(filtered_data()))
      
      export_data <- filtered_data() %>% 
        select(all_of(valid_columns)) %>%
        mutate(
          ARP = if("ARP" %in% names(.)) round(ARP, 1) else ARP
        ) %>%
        rename_with(~ case_when(
          .x == "Full_Name" ~ "Driver Name",
          .x == "start_ps" ~ "Start",
          .x == "ps" ~ "Finish", 
          .x == "fl" ~ "FL",
          .x == "ll" ~ "LL",
          .x == "DKSP" ~ "DK Speed Pts",
          .x == "FDSP" ~ "FD Speed Pts",
          .x == "car_number" ~ "Car Number",
          .x == "team_name" ~ "Team",
          .x == "finishing_status" ~ "Finishing Status",
          .x == "LapsDown" ~ "Laps Down",
          .x == "race_season" ~ "Season",
          .x == "track_name" ~ "Track",
          TRUE ~ .x
        ))
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "season_filter", selected = character(0))
    updateSelectizeInput(session, "driver_filter", selected = character(0))
    updateSelectizeInput(session, "track_filter", selected = character(0))
    updateSelectizeInput(session, "team_filter", selected = character(0))
    updateSelectizeInput(session, "series_filter", selected = character(0))
    
    showNotification("All filters have been reset", type = "message", duration = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)