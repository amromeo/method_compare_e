# Admin Panel Module
# Provides admin-only features including log viewing

# Create admin panel UI (conditionally rendered)
create_admin_panel_ui <- function() {
  conditionalPanel(
    condition = "output.show_admin_panel",
    
    wellPanel(
      h3("🔧 Admin Panel", style = "color: #d9534f;"),
      
      fluidRow(
        column(6,
          h4("System Status"),
          verbatimTextOutput("admin_system_status"),
          br(),
          actionButton("admin_refresh_status", "Refresh Status", class = "btn-info btn-sm")
        ),
        
        column(6,
          h4("Quick Actions"),
          actionButton("admin_view_logs", "View Logs", class = "btn-warning btn-sm"),
          br(), br(),
          downloadButton("admin_download_logs", "Download Today's Logs", class = "btn-secondary btn-sm"),
          br(), br(),
          actionButton("admin_clear_old_logs", "Clear Old Logs", class = "btn-danger btn-sm",
                      onclick = "return confirm('Clear logs older than 7 days?');")
        )
      ),
      
      # Collapsible log viewer
      conditionalPanel(
        condition = "input.admin_view_logs % 2 == 1",
        
        hr(),
        
        h4("📋 Log Viewer"),
        
        fluidRow(
          column(4,
            selectInput("admin_log_type", "Type:",
                       choices = list("Application" = "app", "Audit" = "audit"),
                       selected = "app")
          ),
          column(4,
            selectInput("admin_log_level", "Min Level:",
                       choices = list("DEBUG" = 1, "INFO" = 2, "WARNING" = 3, "ERROR" = 4),
                       selected = 3)
          ),
          column(4,
            numericInput("admin_max_lines", "Max Lines:", value = 100, min = 10, max = 1000)
          )
        ),
        
        fluidRow(
          column(6,
            textInput("admin_filter_context", "Filter Context:", placeholder = "e.g., data_processing")
          ),
          column(6,
            textInput("admin_filter_message", "Filter Message:", placeholder = "e.g., error")
          )
        ),
        
        actionButton("admin_refresh_logs", "Refresh Logs", class = "btn-primary btn-sm"),
        
        br(), br(),
        
        DT::dataTableOutput("admin_log_table", height = "400px")
      ),
      
      style = "border: 2px solid #d9534f; margin-top: 20px;"
    )
  )
}

# Create admin panel server logic
create_admin_panel_server <- function(input, output, session, session_id) {
  
  # Check if user has admin access
  is_admin <- reactive({
    has_log_access(session)
  })
  
  # Show/hide admin panel based on permissions
  output$show_admin_panel <- reactive({
    is_admin()
  })
  outputOptions(output, "show_admin_panel", suspendWhenHidden = FALSE)
  
  # Log admin access attempts
  observe({
    if (is_admin()) {
      log_audit("ADMIN_ACCESS_GRANTED", paste("User:", session$user, "accessed admin panel"), session_id = session_id)
    } else if (!is.null(session$user) && session$user != "") {
      log_audit("ADMIN_ACCESS_DENIED", paste("User:", session$user, "denied admin access"), session_id = session_id)
    }
  })
  
  # System status
  system_status <- reactiveVal()
  
  # Refresh system status
  observeEvent(input$admin_refresh_status, {
    tryCatch({
      log_stats <- get_log_stats(session_id)
      
      status <- paste(
        "System Status (", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ")",
        "",
        "📁 Log Files:",
        paste("  • App log:", ifelse(log_stats$app_log_exists, "✅", "❌"), 
              "Size:", round(log_stats$app_log_size/1024, 1), "KB"),
        paste("  • Audit log:", ifelse(log_stats$audit_log_exists, "✅", "❌"), 
              "Size:", round(log_stats$audit_log_size/1024, 1), "KB"),
        "",
        "💾 System:",
        paste("  • R Version:", R.version.string),
        paste("  • Memory Usage:", round(as.numeric(gc()[2,2]), 1), "MB"),
        paste("  • Working Directory:", getwd()),
        "",
        "📊 Session:",
        paste("  • Current Session ID:", session_id),
        paste("  • User:", ifelse(is.null(session$user), "anonymous", session$user)),
        sep = "\n"
      )
      
      system_status(status)
      log_audit("ADMIN_STATUS_REFRESH", "System status refreshed", session_id = session_id)
      
    }, error = function(e) {
      system_status(paste("Error retrieving system status:", e$message))
      log_error_with_context(e$message, "admin_system_status", "", session_id)
    })
  })
  
  # Initialize system status
  observe({
    if (is_admin()) {
      system_status("Click 'Refresh Status' to load system information")
    }
  })
  
  output$admin_system_status <- renderText({
    req(is_admin())
    system_status()
  })
  
  # Log data for admin viewer
  admin_log_data <- eventReactive(input$admin_refresh_logs, {
    req(is_admin(), input$admin_log_type, input$admin_log_level)
    
    log_audit("ADMIN_LOG_VIEW", paste("Viewing", input$admin_log_type, "logs"), session_id = session_id)
    
    data <- read_log_entries(
      log_type = input$admin_log_type,
      date = Sys.Date(),
      max_lines = input$admin_max_lines
    )
    
    # Filter by level
    min_level <- as.integer(input$admin_log_level)
    level_map <- c("DEBUG" = 1, "INFO" = 2, "WARNING" = 3, "ERROR" = 4, "AUDIT" = 5)
    data$level_num <- level_map[data$level]
    data <- data[data$level_num >= min_level | is.na(data$level_num), ]
    
    # Apply context filter
    if (!is.null(input$admin_filter_context) && input$admin_filter_context != "") {
      data <- data[grepl(input$admin_filter_context, data$context, ignore.case = TRUE), ]
    }
    
    # Apply message filter  
    if (!is.null(input$admin_filter_message) && input$admin_filter_message != "") {
      data <- data[grepl(input$admin_filter_message, data$message, ignore.case = TRUE), ]
    }
    
    return(data)
  }, ignoreNULL = FALSE)
  
  # Admin log table
  output$admin_log_table <- DT::renderDataTable({
    req(is_admin())
    data <- admin_log_data()
    
    if (nrow(data) == 0) {
      return(data.frame(Message = "No log entries found. Click 'Refresh Logs' to load."))
    }
    
    display_data <- data[, c("timestamp", "level", "context", "message")]
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        order = list(list(0, 'desc')),
        dom = 'ftp'
      ),
      class = 'compact stripe',
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        'level',
        backgroundColor = DT::styleEqual(
          c('ERROR', 'WARNING', 'INFO', 'DEBUG', 'AUDIT'),
          c('#ffebee', '#fff3e0', '#e8f5e8', '#e3f2fd', '#f3e5f5')
        )
      )
  })
  
  # Download logs
  output$admin_download_logs <- downloadHandler(
    filename = function() {
      req(is_admin())
      paste0("application_logs_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(is_admin())
      
      log_audit("ADMIN_LOG_DOWNLOAD", "Downloading log files", session_id = session_id)
      
      temp_dir <- tempdir()
      
      # Get today's log files
      log_files <- list.files("logs/", pattern = paste0(".*", Sys.Date(), ".*\\.log$"), full.names = TRUE)
      
      if (length(log_files) > 0) {
        file.copy(log_files, temp_dir)
        zip_files <- file.path(temp_dir, basename(log_files))
        zip(file, zip_files, flags = "-j")
      } else {
        writeLines(paste("No log files found for", Sys.Date()), file)
      }
    }
  )
  
  # Clear old logs
  observeEvent(input$admin_clear_old_logs, {
    req(is_admin())
    
    tryCatch({
      old_files <- list.files("logs/", pattern = "\\.log$", full.names = TRUE)
      cutoff_date <- Sys.Date() - 7  # 7 days old
      
      files_to_remove <- c()
      for (file in old_files) {
        file_date <- file.mtime(file)
        if (as.Date(file_date) < cutoff_date) {
          files_to_remove <- c(files_to_remove, file)
        }
      }
      
      if (length(files_to_remove) > 0) {
        file.remove(files_to_remove)
        log_audit("ADMIN_LOG_CLEANUP", paste("Removed", length(files_to_remove), "old log files"), session_id = session_id)
        showNotification(paste("Removed", length(files_to_remove), "old log files"), type = "message")
      } else {
        showNotification("No old log files found to remove", type = "message")
      }
      
    }, error = function(e) {
      log_error_with_context(e$message, "admin_clear_old_logs", "", session_id)
      showNotification("Error clearing old logs", type = "error")
    })
  })
}