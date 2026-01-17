#!/usr/bin/env Rscript

# Enhanced Sports Simulation Launcher
# Clean, reliable, and user-friendly

cat("==============================================\n")
cat("      Golden Ticket Sims Launcher v2.0       \n")
cat("==============================================\n")
cat("Initializing...\n\n")

# Configure system settings
if (.Platform$OS.type == "windows") {
  options(download.file.method = "wininet")
} else {
  options(download.file.method = "libcurl")
}
options(timeout = 300, repos = "https://cloud.r-project.org")

# Streamlined package list - only essentials  
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", 
  "tidyverse", "data.table", "readxl", 
  "ggplot2", "DT", "plotly", "lpSolve", 
  "memoise", "shinycssloaders", "shinyjs",
  "parallel", "rstudioapi", "openxlsx", "writexl"
)

# Enhanced package installation with progress
install_packages <- function() {
  cat("=== SYSTEM DIAGNOSTICS ===\n")
  cat("R Version:", R.version.string, "\n")
  cat("Platform:", R.version$platform, "\n")
  cat("CRAN Mirror:", getOption("repos"), "\n")
  cat("Library Paths:\n")
  for(i in 1:length(.libPaths())) {
    path <- .libPaths()[i]
    cat("  ", i, ": ", path, " (", ifelse(file.access(path, 2) == 0, "writable", "read-only"), ")\n", sep = "")
  }
  
  # Test internet connection to CRAN
  cat("Testing CRAN connection...")
  cran_test <- tryCatch({
    con <- url("https://cloud.r-project.org", open = "rb")
    close(con)
    TRUE
  }, error = function(e) {
    cat(" b\n")
    cat("CRAN Connection Error:", conditionMessage(e), "\n")
    FALSE
  })
  if(cran_test) cat(" b\n")
  
  cat("\n=== PACKAGE INSTALLATION ===\n")
  
  missing_packages <- c()
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  if (length(missing_packages) > 0) {
    cat("Missing packages:", paste(missing_packages, collapse = ", "), "\n")
    cat("Starting installation...\n\n")
    
    for (pkg in missing_packages) {
      cat("Installing", pkg, "...\n")
      
      # Check if package exists on CRAN first
      tryCatch({
        available_packages <- available.packages(repos = getOption("repos"))
        if (!pkg %in% rownames(available_packages)) {
          cat("  b Package", pkg, "not found in CRAN repository\n")
          next
        }
      }, error = function(e) {
        cat("  Warning: Could not check CRAN availability:", conditionMessage(e), "\n")
      })
      
      # Attempt installation with verbose output
      install_result <- tryCatch({
        install.packages(pkg, repos = getOption("repos"), dependencies = TRUE, quiet = FALSE)
        "success"
      }, error = function(e) {
        paste("error:", conditionMessage(e))
      }, warning = function(w) {
        paste("warning:", conditionMessage(w))
      })
      
      cat("  Installation result:", install_result, "\n")
      
      # Verify installation
      if (requireNamespace(pkg, quietly = TRUE)) {
        cat("  b Package", pkg, "successfully installed and loadable\n")
      } else {
        cat("  b Package", pkg, "installation may have failed\n")
        
        # Additional diagnostics for failed packages
        cat("  Checking installation location...\n")
        for(lib_path in .libPaths()) {
          pkg_path <- file.path(lib_path, pkg)
          if(dir.exists(pkg_path)) {
            cat("    Found in:", pkg_path, "\n")
            desc_file <- file.path(pkg_path, "DESCRIPTION")
            if(file.exists(desc_file)) {
              cat("    DESCRIPTION file exists\n")
            } else {
              cat("    DESCRIPTION file missing - incomplete installation\n")
            }
          }
        }
      }
      cat("\n")
    }
  } else {
    cat("All packages already installed b\n")
  }
  
  # Load packages with detailed error reporting
  cat("=== PACKAGE LOADING ===\n")
  failed_packages <- c()
  for (pkg in required_packages) {
    cat("Loading", pkg, "...")
    tryCatch({
      # Use do.call to avoid issues with character.only parameter
      do.call("library", list(pkg, character.only = TRUE, quietly = TRUE))
      cat(" b\n")
    }, error = function(e) {
      # Try alternative loading method
      tryCatch({
        require(pkg, character.only = TRUE, quietly = TRUE)
        cat(" b (via require)\n")
      }, error = function(e2) {
        failed_packages <<- c(failed_packages, pkg)
        cat(" b\n")
        cat("  Error details:", conditionMessage(e), "\n")
        cat("  Require error:", conditionMessage(e2), "\n")
        
        # Try to get more specific error info
        tryCatch({
          loadNamespace(pkg)
          cat("  Note: loadNamespace worked, library() failed\n")
        }, error = function(e3) {
          cat("  loadNamespace also failed:", conditionMessage(e3), "\n")
        })
      })
    })
  }
  
  if (length(failed_packages) > 0) {
    cat("\nb FAILED PACKAGES:", paste(failed_packages, collapse = ", "), "\n")
    cat("This may cause app functionality issues.\n")
    return(FALSE)
  }
  
  cat("\nb All packages loaded successfully\n\n")
  return(TRUE)
}

# Setup permanent directory in user's home
setup_directories <- function() {
  home_dir <- path.expand("~")
  app_dir <- file.path(home_dir, "Golden_Ticket_Sims")
  repo_dir <- file.path(app_dir, "NicheSportSimsPublic-main")
  
  if (!dir.exists(app_dir)) {
    cat("Creating Golden_Ticket_Sims directory in your home folder...\n")
    dir.create(app_dir, recursive = TRUE)
  }
  
  return(list(app_dir = app_dir, repo_dir = repo_dir))
}

# Check internet connection
check_internet <- function() {
  tryCatch({
    con <- url("https://www.google.com", open = "rb")
    close(con)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Enhanced download with multiple fallback methods
download_repo <- function(app_dir) {
  repo_url <- "https://github.com/astukas12/NicheSportSimsPublic/archive/refs/heads/main.zip"
  zip_file <- file.path(app_dir, "repo.zip")
  repo_dir <- file.path(app_dir, "NicheSportSimsPublic-main")
  
  cat("Downloading latest version...\n")
  
  # Remove old version if exists
  if (dir.exists(repo_dir)) {
    unlink(repo_dir, recursive = TRUE)
  }
  
  download_success <- FALSE
  
  # Method 1: Try curl if available
  if (Sys.which("curl") != "" && !download_success) {
    cat("  Trying curl method...")
    curl_cmd <- sprintf('curl -L "%s" -o "%s"', repo_url, zip_file)
    result <- system(curl_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    
    if (result == 0 && file.exists(zip_file) && file.info(zip_file)$size > 1000) {
      cat(" b\n")
      download_success <- TRUE
    } else {
      cat(" b\n")
    }
  }
  
  # Method 2: R's download.file
  if (!download_success) {
    cat("  Trying R download method...")
    tryCatch({
      download.file(repo_url, zip_file, mode = "wb", quiet = TRUE)
      if (file.exists(zip_file) && file.info(zip_file)$size > 1000) {
        cat(" b\n")
        download_success <- TRUE
      } else {
        cat(" b\n")
      }
    }, error = function(e) {
      cat(" b\n")
    })
  }
  
  if (download_success) {
    cat("  Extracting files...")
    tryCatch({
      unzip(zip_file, exdir = app_dir)
      unlink(zip_file) # Clean up zip file
      cat(" b\n")
      return(repo_dir)
    }, error = function(e) {
      cat(" b\n")
      return(NULL)
    })
  }
  
  return(NULL)
}

# Smart repository management with offline mode
manage_repository <- function() {
  dirs <- setup_directories()
  repo_dir <- dirs$repo_dir
  
  # Check if local version exists
  local_exists <- dir.exists(repo_dir)
  internet_available <- check_internet()
  
  if (local_exists && internet_available) {
    cat("Local version found. Checking for updates...\n")
    new_repo <- download_repo(dirs$app_dir)
    if (!is.null(new_repo)) {
      cat("Updated to latest version b\n\n")
      return(new_repo)
    } else {
      cat("Update failed, using local version b\n\n")
      return(repo_dir)
    }
  } else if (local_exists && !internet_available) {
    cat("No internet connection. Using local version b\n\n")
    return(repo_dir)
  } else if (!local_exists && internet_available) {
    cat("First time setup. Downloading repository...\n")
    new_repo <- download_repo(dirs$app_dir)
    if (!is.null(new_repo)) {
      cat("Download complete b\n\n")
      return(new_repo)
    } else {
      cat("Download failed b\n")
      return(NULL)
    }
  } else {
    cat("No local version and no internet connection b\n")
    cat("Please connect to the internet for first-time setup.\n")
    return(NULL)
  }
}

# Clean menu display
select_app <- function() {
  cat("==============================================\n")
  cat("       Available Simulations                  \n")
  cat("==============================================\n")
  cat("1) NASCAR Simulation     7) Sweat\n")
  cat("2) Formula 1 Simulation  8) Tennis\n")
  cat("3) NASCAR Review         9) Tennis Review\n")
  cat("4) Formula 1 Review      10) Golf\n")
  cat("5) MMA                   11) Football Showdown\n")
  cat("6) MMA Review            12) Football Classic\n")
  cat("13) Exit\n")
  cat("----------------------------------------------\n")
  
  choice <- as.integer(readline(prompt = "Enter your choice (1-13): "))
  while(is.na(choice) || choice < 1 || choice > 13) {
    cat("Please enter a valid number between 1 and 13.\n")
    choice <- as.integer(readline(prompt = "Enter your choice (1-13): "))
  }
  return(choice)
}

# Enhanced app launcher with better browser handling
launch_app <- function(choice, repo_path) {
  subdirs <- c("Nascar", "Formula1", "NascarReview", "F1Review", "MMA",
               "MMAReview", "Sweat", "Tennis", "TennisReview", "Golf",
               "CFB", "NFL")
  
  app_names <- c("NASCAR Simulation", "Formula 1 Simulation", "NASCAR Review",
                 "Formula 1 Review", "MMA Simulation", "MMA Review",
                 "NASCAR Live Sweat Tool", "Tennis Simulation", "Tennis Review",
                 "Golf Cut Optimizer", "Football Showdown", "Football Classic")
  
  if (choice < 1 || choice > 12) return()
  
  app_path <- file.path(repo_path, subdirs[choice])
  
  if (!dir.exists(app_path)) {
    cat("Error: Could not find", app_names[choice], "directory.\n")
    cat("Please try downloading again or contact support.\n")
    return()
  }
  
  cat("\n==============================================\n")
  cat("Starting:", app_names[choice], "\n")
  cat("==============================================\n")
  cat("Loading application... This may take a moment.\n")
  
  # Smart window launching - prioritize dedicated window
  launch_browser <- function(url) {
    # Method 1: Try RStudio external window (like original script)
    tryCatch({
      if (rstudioapi::isAvailable() && exists(".rs.invokeShinyWindowExternal", mode = "function")) {
        .rs.invokeShinyWindowExternal(url)
        return()
      }
    }, error = function(e) {})
    
    # Method 2: Force external browser window (original behavior)
    tryCatch({
      browseURL(url)
    }, error = function(e) {
      cat("Error launching application window.\n")
    })
  }
  
  tryCatch({
    shiny::runApp(
      app_path, 
      launch.browser = function(url) {
        browseURL(url)
      },
      host = "127.0.0.1",
      port = NULL  # Let R choose available port
    )
  }, error = function(e) {
    cat("Error starting application:\n")
    cat("This might be a temporary issue. Try:\n")
    cat("1) Closing any other R applications\n")
    cat("2) Restarting R and running the script again\n")
    cat("3) Checking that all required packages are installed\n")
    cat("\nTechnical error:", e$message, "\n")
  })
}

# Main execution flow
main <- function() {
  # Step 1: Install and load packages
  packages_ok <- install_packages()
  
  if (!packages_ok) {
    cat("\nPackage installation issues detected.\n")
    cat("The app may not work properly. Continue anyway? (y/n): ")
    response <- tolower(trimws(readline()))
    if (response != "y" && response != "yes") {
      cat("Setup cancelled. Please resolve package issues and try again.\n")
      return()
    }
  }
  
  # Step 2: Setup repository
  repo_path <- manage_repository()
  
  if (is.null(repo_path)) {
    cat("\nSetup failed. Please check your internet connection and try again.\n")
    cat("If problems persist, contact support.\n")
    return()
  }
  
  cat("Setup complete! Ready to run simulations.\n\n")
  
  # Step 3: Main application loop
  while(TRUE) {
    choice <- select_app()
    if(choice == 13) {
      cat("\nThanks for using Golden Ticket Sims!\n")
      cat("Your files are saved in: ~/Golden_Ticket_Sims\n")
      break
    }
    launch_app(choice, repo_path)
    cat("\n") # Add spacing between runs
  }
}

# Run the application
main()