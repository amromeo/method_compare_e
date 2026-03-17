# Logging Module
# Provides structured logging for debugging and audit trails

# Initialize logging configuration
LOG_LEVELS <- list(
  DEBUG = 1,
  INFO = 2, 
  WARNING = 3,
  ERROR = 4,
  AUDIT = 5
)

# Default log level (can be changed via environment variable or input)
app_log_env <- Sys.getenv("APP_LOG_LEVEL")
current_log_level <- if (app_log_env != "") as.integer(app_log_env) else LOG_LEVELS$INFO

# Create log directory if it doesn't exist (override with APP_LOG_DIR)
log_dir <- Sys.getenv("APP_LOG_DIR", unset = "logs")
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

# Generate log file paths
get_log_file_path <- function(log_type = "app") {
  timestamp <- format(Sys.Date(), "%Y-%m-%d")
  file.path(log_dir, paste0(log_type, "_", timestamp, ".log"))
}

# Core logging function
write_log <- function(level, message, context = "", user_id = "unknown", session_id = "unknown") {
  if (LOG_LEVELS[[level]] < current_log_level) {
    return(invisible(NULL))
  }
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Format log entry
  log_entry <- sprintf("[%s] %s | %s | %s | %s | %s", 
                      timestamp, level, session_id, user_id, context, message)
  
  # Write to appropriate log file
  log_file <- if (level == "AUDIT") get_log_file_path("audit") else get_log_file_path("app")
  
  tryCatch({
    cat(log_entry, "\n", file = log_file, append = TRUE)
  }, error = function(e) {
    # Fallback to console if file logging fails
    cat("LOG_ERROR:", log_entry, "\n")
  })
  
  # Also print to console for development (only for WARNING and above)
  if (LOG_LEVELS[[level]] >= LOG_LEVELS$WARNING) {
    cat(log_entry, "\n")
  }
}

# Convenience logging functions
log_debug <- function(message, context = "", session_id = "unknown") {
  write_log("DEBUG", message, context, session_id = session_id)
}

log_info <- function(message, context = "", session_id = "unknown") {
  write_log("INFO", message, context, session_id = session_id)
}

log_warning <- function(message, context = "", session_id = "unknown") {
  write_log("WARNING", message, context, session_id = session_id)
}

log_error <- function(message, context = "", session_id = "unknown") {
  write_log("ERROR", message, context, session_id = session_id)
}

log_audit <- function(action, details = "", user_id = "unknown", session_id = "unknown") {
  message <- paste("ACTION:", action, "|", details)
  write_log("AUDIT", message, "user_action", user_id, session_id)
}

# Data operation logging
log_data_operation <- function(operation, data_summary, session_id = "unknown") {
  message <- paste(operation, "|", data_summary)
  log_info(message, "data_processing", session_id)
}

# Analysis logging
log_analysis <- function(analysis_type, parameters, result_summary, session_id = "unknown") {
  message <- paste("ANALYSIS:", analysis_type, "| Parameters:", parameters, "| Result:", result_summary)
  log_info(message, "analysis", session_id)
}

# Error with context logging
log_error_with_context <- function(error_message, function_name, input_context = "", session_id = "unknown") {
  context_msg <- if (input_context != "") paste("| Input:", input_context) else ""
  message <- paste("Function:", function_name, "| Error:", error_message, context_msg)
  log_error(message, "error_context", session_id)
}

# Session management
get_session_id <- function(session) {
  # Extract session ID from Shiny session object
  if (!is.null(session) && !is.null(session$token)) {
    return(substr(session$token, 1, 8))  # Use first 8 chars of session token
  }
  return("unknown")
}

# Initialize session logging
init_session_logging <- function(session) {
  session_id <- get_session_id(session)
  log_audit("SESSION_START", "Application initialized", session_id = session_id)
  return(session_id)
}

# Log user interactions
log_user_interaction <- function(action, input_name, value, session_id = "unknown") {
  # Sanitize sensitive values
  display_value <- if (is.character(value) && nchar(value) > 100) {
    paste0(substr(value, 1, 100), "... [truncated]")
  } else if (is.numeric(value) && length(value) > 10) {
    paste0("[", length(value), " numeric values]")
  } else {
    as.character(value)
  }
  
  log_audit(action, paste("Input:", input_name, "| Value:", display_value), session_id = session_id)
}

# Log data quality issues
log_data_quality_issue <- function(issue_type, details, data_summary = "", session_id = "unknown") {
  message <- paste("QUALITY_ISSUE:", issue_type, "|", details, "|", data_summary)
  log_warning(message, "data_quality", session_id)
}

# Session cleanup logging
cleanup_session_logging <- function(session_id) {
  log_audit("SESSION_END", "Application session ended", session_id = session_id)
}

# Utility function to set log level dynamically
set_log_level <- function(level) {
  if (level %in% names(LOG_LEVELS)) {
    current_log_level <<- LOG_LEVELS[[level]]
    log_info(paste("Log level set to:", level), "system")
  } else {
    log_warning(paste("Invalid log level:", level, "- Valid levels:", paste(names(LOG_LEVELS), collapse = ", ")), "system")
  }
}

# Get log file statistics
get_log_stats <- function(session_id = "unknown") {
  app_log <- get_log_file_path("app")
  audit_log <- get_log_file_path("audit")
  
  stats <- list(
    app_log_exists = file.exists(app_log),
    audit_log_exists = file.exists(audit_log),
    app_log_size = if (file.exists(app_log)) file.size(app_log) else 0,
    audit_log_size = if (file.exists(audit_log)) file.size(audit_log) else 0
  )
  
  log_info(paste("Log stats - App:", stats$app_log_size, "bytes | Audit:", stats$audit_log_size, "bytes"), "system", session_id)
  return(stats)
}
