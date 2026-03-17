# Error Handling Module
# Contains utility functions for comprehensive error handling and validation

# Validate numerical data for medical tests
validate_medical_data <- function(data, test_name = "Unknown") {
  errors <- character(0)
  
  if (is.null(data) || nrow(data) == 0) {
    errors <- c(errors, paste("No data provided for", test_name))
    return(list(valid = FALSE, errors = errors))
  }
  
  # Check required columns (support both X,Y and M1,M2 naming)
  has_xy <- all(c("X", "Y") %in% names(data))
  has_m1m2 <- all(c("M1", "M2") %in% names(data))
  
  if (!has_xy && !has_m1m2) {
    errors <- c(errors, "Missing required columns: need either X,Y or M1,M2")
    return(list(valid = FALSE, errors = errors))
  }
  
  # Use appropriate column names
  x_col <- if (has_xy) "X" else "M1"
  y_col <- if (has_xy) "Y" else "M2"
  
  # Check if columns are numeric
  if (!is.numeric(data[[x_col]])) {
    errors <- c(errors, paste(x_col, "values must be numeric"))
  }
  if (!is.numeric(data[[y_col]])) {
    errors <- c(errors, paste(y_col, "values must be numeric"))
  }
  
  # Check for valid data (not all NA)
  valid_rows <- !is.na(data[[x_col]]) & !is.na(data[[y_col]])
  if (sum(valid_rows) < 2) {
    errors <- c(errors, "At least 2 pairs of valid data values are required for analysis")
  }
  
  if (sum(valid_rows) >= 2) {
    # Check for reasonable ranges (medical test values should be positive)
    if (any(data[[x_col]][valid_rows] < 0, na.rm = TRUE)) {
      errors <- c(errors, paste(x_col, "values should not be negative for medical tests"))
    }
    if (any(data[[y_col]][valid_rows] < 0, na.rm = TRUE)) {
      errors <- c(errors, paste(y_col, "values should not be negative for medical tests"))
    }
    
    # Check for extremely large values that might indicate data entry errors
    max_reasonable_value <- 10000
    if (any(data[[x_col]][valid_rows] > max_reasonable_value, na.rm = TRUE) || 
        any(data[[y_col]][valid_rows] > max_reasonable_value, na.rm = TRUE)) {
      errors <- c(errors, paste("Values above", max_reasonable_value, "detected - please verify data entry"))
    }
  }
  
  return(list(valid = length(errors) == 0, errors = errors))
}

# Safe wrapper for mcreg function calls
safe_mcreg <- function(data_or_x, y = NULL, ..., context = "Analysis") {
  tryCatch({
    # Handle both data frame and vector inputs
    if (is.data.frame(data_or_x)) {
      # Data frame input - extract M1, M2 columns
      if (!all(c("M1", "M2") %in% names(data_or_x))) {
        stop("Data frame must contain M1 and M2 columns")
      }
      x <- data_or_x$M1
      y <- data_or_x$M2
    } else {
      # Vector inputs
      x <- data_or_x
      if (is.null(y)) {
        stop("Y values must be provided when using vector input")
      }
    }
    
    # Validate inputs
    if (length(x) != length(y)) {
      stop("X and Y must have the same length")
    }
    
    valid_indices <- !is.na(x) & !is.na(y)
    if (sum(valid_indices) < 3) {
      stop("At least 3 valid data pairs are required for regression analysis")
    }
    
    result <- mcreg(x, y, ...)
    return(list(success = TRUE, result = result, error = NULL))
    
  }, error = function(e) {
    error_msg <- paste(context, "failed:", e$message)
    log_error_with_context(e$message, "safe_mcreg", context)
    showNotification(error_msg, type = "error", duration = 10)
    return(list(success = FALSE, result = NULL, error = error_msg))
  })
}

# Safe wrapper for plot generation
safe_plot <- function(plot_func, data, context = "Plot generation") {
  tryCatch({
    if (is.null(data)) {
      return(NULL)
    }
    
    # Execute the plot function
    plot_func()
    
  }, error = function(e) {
    error_msg <- paste(context, "failed:", e$message)
    log_error_with_context(e$message, "safe_plot", context)
    showNotification(error_msg, type = "error", duration = 8)
    
    # Return a simple error plot
    plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Plot Error")
    text(1, 1, paste("Error:", e$message), cex = 0.8, col = "red")
    box()
  })
}

# Validate input parameters
validate_input_params <- function(input, required_params = character(0)) {
  errors <- character(0)
  
  for (param in required_params) {
    if (is.null(input[[param]]) || is.na(input[[param]]) || input[[param]] == "") {
      errors <- c(errors, paste("Required parameter", param, "is missing or empty"))
    }
  }
  
  # Validate specific parameter types and ranges
  if (!is.null(input$limitValue)) {
    if (!is.numeric(input$limitValue) || input$limitValue < 0 || input$limitValue > 1) {
      errors <- c(errors, "Limit value must be numeric between 0 and 1")
    }
  }
  
  if (!is.null(input$syx)) {
    if (!is.numeric(input$syx) || input$syx < 0) {
      errors <- c(errors, "SYX value must be a positive number")
    }
  }
  
  return(list(valid = length(errors) == 0, errors = errors))
}

# Show consolidated error messages
show_validation_errors <- function(errors, title = "Validation Error") {
  if (length(errors) > 0) {
    error_msg <- paste(errors, collapse = "\n• ")
    showNotification(
      HTML(paste0("<strong>", title, ":</strong><br>• ", error_msg)), 
      type = "error", 
      duration = 15
    )
  }
}