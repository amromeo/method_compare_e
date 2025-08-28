# Log Viewer Module
# Provides in-app log inspection capabilities

# Check if user has log viewing permissions  
has_log_access <- function(session) {
  # Load admin configuration
  if (file.exists("config/admin_config.R")) {
    source("config/admin_config.R", local = TRUE)
  } else {
    # Fallback configuration if config file doesn't exist
    is_user_admin <- function(user, user_groups = NULL) {
      # Development fallback - allow access if environment variable set
      if (Sys.getenv("ENABLE_ADMIN_LOGS") == "true") {
        return(TRUE)
      }
      return(FALSE)
    }
  }
  
  # Get user information from Posit Connect
  user <- session$user
  user_groups <- session$groups
  
  # Use configuration function
  return(is_user_admin(user, user_groups))
}

# Read log files with date filtering
read_log_entries <- function(log_type = "app", date = Sys.Date(), max_lines = 1000) {
  log_file <- get_log_file_path(log_type)
  
  if (!file.exists(log_file)) {
    return(data.frame(
      timestamp = character(0),
      level = character(0),
      session_id = character(0),
      user_id = character(0),
      context = character(0),
      message = character(0)
    ))
  }
  
  tryCatch({
    # Read log file
    lines <- readLines(log_file, n = max_lines)
    
    # Parse log entries (assuming format: [timestamp] level | session | user | context | message)
    parsed <- lapply(lines, function(line) {
      # Extract components using regex
      pattern <- "\\[([^\\]]+)\\] (\\w+) \\| ([^\\|]+) \\| ([^\\|]+) \\| ([^\\|]+) \\| (.+)"
      match <- regmatches(line, regexec(pattern, line))
      
      if (length(match[[1]]) >= 7) {
        list(
          timestamp = match[[1]][2],
          level = match[[1]][3],
          session_id = trimws(match[[1]][4]),
          user_id = trimws(match[[1]][5]),
          context = trimws(match[[1]][6]),
          message = trimws(match[[1]][7])
        )
      } else {
        # Fallback for malformed lines
        list(
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          level = "RAW",
          session_id = "unknown",
          user_id = "unknown", 
          context = "unparsed",
          message = line
        )
      }
    })
    
    # Convert to data frame
    do.call(rbind, lapply(parsed, as.data.frame, stringsAsFactors = FALSE))
    
  }, error = function(e) {
    data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      level = "ERROR",
      session_id = "system",
      user_id = "system",
      context = "log_reader",
      message = paste("Failed to read log:", e$message)
    )
  })
}

# Create log viewer UI
create_log_viewer_ui <- function() {
  fluidPage(
    titlePanel("Application Logs"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("log_type", "Log Type:",
                   choices = list("Application" = "app", "Audit Trail" = "audit"),
                   selected = "app"),
        
        dateInput("log_date", "Date:", value = Sys.Date()),
        
        selectInput("log_level", "Minimum Level:",
                   choices = list("DEBUG" = 1, "INFO" = 2, "WARNING" = 3, "ERROR" = 4, "AUDIT" = 5),
                   selected = 2),
        
        numericInput("max_lines", "Max Lines:", value = 500, min = 10, max = 5000),
        
        actionButton("refresh_logs", "Refresh", class = "btn-primary"),
        
        br(),
        
        downloadButton("download_logs", "Download Logs", class = "btn-info"),
        
        br(), br(),
        
        h4("Quick Filters"),
        textInput("filter_session", "Session ID:", placeholder = "abc12345"),
        textInput("filter_context", "Context:", placeholder = "data_processing"),
        textInput("filter_message", "Message contains:", placeholder = "error")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Log Entries",
            DT::dataTableOutput("log_table")
          ),
          
          tabPanel("Statistics", 
            fluidRow(
              column(6, plotOutput("log_level_chart")),
              column(6, plotOutput("context_chart"))
            ),
            
            fluidRow(
              column(12, verbatimTextOutput("log_summary"))
            )
          ),
          
          tabPanel("Raw Log",
            h4("Raw Log File Contents"),
            verbatimTextOutput("raw_log_content")
          )
        )
      )
    )
  )
}

# Create log viewer server logic
create_log_viewer_server <- function(input, output, session) {
  
  # Check access permissions
  if (!has_log_access(session)) {
    showNotification("Access denied. Contact administrator for log access.", type = "error")
    return()
  }
  
  # Reactive log data
  log_data <- eventReactive(input$refresh_logs, {
    req(input$log_type, input$log_date, input$log_level)
    
    data <- read_log_entries(
      log_type = input$log_type,
      date = input$log_date,
      max_lines = input$max_lines
    )
    
    # Filter by level
    min_level <- as.integer(input$log_level)
    level_map <- c("DEBUG" = 1, "INFO" = 2, "WARNING" = 3, "ERROR" = 4, "AUDIT" = 5)
    data$level_num <- level_map[data$level]
    data <- data[data$level_num >= min_level | is.na(data$level_num), ]
    
    # Apply filters
    if (!is.null(input$filter_session) && input$filter_session != "") {
      data <- data[grepl(input$filter_session, data$session_id, ignore.case = TRUE), ]
    }
    
    if (!is.null(input$filter_context) && input$filter_context != "") {
      data <- data[grepl(input$filter_context, data$context, ignore.case = TRUE), ]
    }
    
    if (!is.null(input$filter_message) && input$filter_message != "") {
      data <- data[grepl(input$filter_message, data$message, ignore.case = TRUE), ]
    }
    
    return(data)
  }, ignoreNULL = FALSE)
  
  # Initialize log data
  observe({
    log_data()
  })
  
  # Log table output
  output$log_table <- DT::renderDataTable({
    data <- log_data()
    
    DT::datatable(
      data[, c("timestamp", "level", "session_id", "context", "message")],
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(0, 'desc'))  # Sort by timestamp descending
      ),
      filter = 'top'
    ) %>%
      DT::formatStyle(
        'level',
        backgroundColor = DT::styleEqual(
          c('ERROR', 'WARNING', 'INFO', 'DEBUG', 'AUDIT'),
          c('#ffebee', '#fff3e0', '#e8f5e8', '#e3f2fd', '#f3e5f5')
        )
      )
  })
  
  # Log level chart
  output$log_level_chart <- renderPlot({
    data <- log_data()
    if (nrow(data) > 0) {
      level_counts <- table(data$level)
      barplot(level_counts, main = "Log Entries by Level", 
              col = c("lightblue", "lightgreen", "orange", "red", "purple"))
    }
  })
  
  # Context chart
  output$context_chart <- renderPlot({
    data <- log_data()
    if (nrow(data) > 0) {
      context_counts <- head(sort(table(data$context), decreasing = TRUE), 10)
      barplot(context_counts, main = "Top 10 Contexts", 
              col = "lightcoral", las = 2, cex.names = 0.8)
    }
  })
  
  # Log summary
  output$log_summary <- renderText({
    data <- log_data()
    
    if (nrow(data) == 0) {
      return("No log entries found for the selected criteria.")
    }
    
    paste(
      "Log Summary:",
      paste("Total Entries:", nrow(data)),
      paste("Time Range:", min(data$timestamp), "to", max(data$timestamp)),
      paste("Unique Sessions:", length(unique(data$session_id))),
      paste("Error Count:", sum(data$level == "ERROR")),
      paste("Warning Count:", sum(data$level == "WARNING")),
      sep = "\n"
    )
  })
  
  # Raw log content
  output$raw_log_content <- renderText({
    log_file <- get_log_file_path(input$log_type)
    if (file.exists(log_file)) {
      lines <- readLines(log_file, n = 100)  # Last 100 lines
      paste(rev(lines), collapse = "\n")
    } else {
      "Log file not found."
    }
  })
  
  # Download logs
  output$download_logs <- downloadHandler(
    filename = function() {
      paste0("logs_", input$log_type, "_", input$log_date, ".zip")
    },
    content = function(file) {
      # Create temporary directory
      temp_dir <- tempdir()
      
      # Copy relevant log files
      log_files <- list.files("logs/", pattern = paste0(".*", input$log_date, ".*\\.log$"), full.names = TRUE)
      
      if (length(log_files) > 0) {
        file.copy(log_files, temp_dir)
        
        # Create zip file
        zip_files <- file.path(temp_dir, basename(log_files))
        zip(file, zip_files, flags = "-j")  # -j stores just filenames
      } else {
        # Create empty file with message
        writeLines("No log files found for selected date", file)
      }
    }
  )
}

# Add debug endpoint for Posit Connect
create_debug_endpoint <- function() {
  function(req) {
    if (req$PATH_INFO == "/debug/logs") {
      # Simple log viewer accessible via URL
      log_files <- list.files("logs/", pattern = "*.log", full.names = TRUE)
      
      content <- paste(
        "<h1>Application Logs</h1>",
        "<h2>Available Log Files:</h2>",
        paste("<li><a href='/debug/logs/", basename(log_files), "'>", basename(log_files), "</a></li>", collapse = ""),
        sep = "\n"
      )
      
      list(
        status = 200,
        headers = list('Content-Type' = 'text/html'),
        body = content
      )
    } else if (grepl("^/debug/logs/.+\\.log$", req$PATH_INFO)) {
      # Serve specific log file
      log_file <- file.path("logs", basename(req$PATH_INFO))
      
      if (file.exists(log_file)) {
        content <- readLines(log_file)
        list(
          status = 200,
          headers = list('Content-Type' = 'text/plain'),
          body = paste(content, collapse = "\n")
        )
      } else {
        list(status = 404, body = "Log file not found")
      }
    } else {
      list(status = 404, body = "Not found")
    }
  }
}