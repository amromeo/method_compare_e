# Data Processing Module
# Contains reactive functions and observers for data calculations and transformations

# Initialize reactive values for data storage
initialize_data_values <- function() {
  reactiveValues(
    hot_data = data.frame(
      'Sample' = rep(NA_character_, 10),
      'X' = round(c(rep(NA, 10)), digits = 2),
      'Y' = round(c(rep(NA, 10)), digits = 2),
      'Abs_Diff' = round(c(rep(NA, 10)), digits = 2),
      'Per_Diff' = round(c(rep(NA, 10)), digits = 2),
      'Pass_Fail' = rep(NA_character_, 10),
      'Limit_per' = rep(TRUE, 10)
    ), 
    final_data = data.frame(
      'Sample' = rep(NA_character_, 10),
      'X' = round(c(rep(NA, 10)), digits = 2),
      'Y' = round(c(rep(NA, 10)), digits = 2),
      'Abs_Diff' = round(c(rep(NA, 10)), digits = 2),
      'Per_Diff' = round(c(rep(NA, 10)), digits = 2),
      'Pass_Fail' = rep(NA_character_, 10),
      'Limit_per' = rep(.15, 10)
    )
  )
}

# Create observer for data calculations
create_data_observer <- function(input, vals) {
  observe({
    req(input$hot)
    req(input$limitValue) 
    
    # Validate limit value
    if (!is.numeric(input$limitValue) || input$limitValue < 0 || input$limitValue > 1) {
      showNotification("Limit value must be between 0 and 1", type = "error")
      return()
    }
    
    df <- hot_to_r(input$hot)
    
    if (is.null(df) || nrow(df) == 0) return()
    if (!all(c("X", "Y") %in% names(df))) {
      showNotification("Data must contain X and Y columns", type = "error")
      return()
    }
    
    # Enhanced data validation
    tryCatch({
      # Convert to numeric and validate
      df$X <- as.numeric(df$X)
      df$Y <- as.numeric(df$Y)
      
      # Check for valid data pairs
      valid_pairs <- !is.na(df$X) & !is.na(df$Y)
      valid_count <- sum(valid_pairs)
      
      # Check if user has actually entered data (not just default NAs)
      has_user_data <- any(!is.na(df$X) | !is.na(df$Y) | 
                          (df$Sample != "" & !is.na(df$Sample)), na.rm = TRUE)
      
      if (valid_count == 0) {
        # Only show warning if user has actually tried to enter data
        if (has_user_data) {
          showNotification("No valid X,Y data pairs found", type = "warning")
        }
        return()
      }
      
      # Check for negative values (medical tests should be positive)
      negative_x <- sum(df$X[valid_pairs] < 0, na.rm = TRUE)
      negative_y <- sum(df$Y[valid_pairs] < 0, na.rm = TRUE)
      
      if (negative_x > 0 || negative_y > 0) {
        showNotification(
          paste("Warning:", negative_x + negative_y, "negative values detected. Medical test values should typically be positive."),
          type = "warning", duration = 8
        )
      }
      
      # Check for extreme values
      max_reasonable <- 10000
      extreme_x <- sum(df$X[valid_pairs] > max_reasonable, na.rm = TRUE)
      extreme_y <- sum(df$Y[valid_pairs] > max_reasonable, na.rm = TRUE)
      
      if (extreme_x > 0 || extreme_y > 0) {
        showNotification(
          paste("Warning:", extreme_x + extreme_y, "values above", max_reasonable, "detected. Please verify data entry."),
          type = "warning", duration = 10
        )
      }
      
      # Calculate differences
      df$Abs_Diff <- df$Y - df$X
      df$Per_Diff <- df$Abs_Diff / df$X
      
      # Handle division by zero
      zero_x_indices <- which(df$X == 0 & valid_pairs)
      if (length(zero_x_indices) > 0) {
        df$Per_Diff[zero_x_indices] <- NA
        showNotification(
          paste("Warning:", length(zero_x_indices), "rows with X=0 cannot calculate percentage difference"),
          type = "warning"
        )
      }
      
      limit_value <- input$limitValue
      
      # Calculate Pass/Fail with better error handling
      df$Per_Diff <- suppressWarnings(as.numeric(df$Per_Diff))
      
      df$Pass_Fail <- dplyr::case_when(
        is.na(df$Per_Diff) ~ "",
        is.na(df$X) | is.na(df$Y) ~ "",
        abs(df$Per_Diff) <= limit_value ~ "PASS",
        TRUE ~ "FAIL"
      )
      
      df$Limit_per <- limit_value
      vals$final_data <- df
      
    }, error = function(e) {
      showNotification(
        paste("Data processing error:", e$message), 
        type = "error", duration = 10
      )
    })
  })
}

# Create reactive for modified data with summary statistics
create_mod_data_reactive <- function(vals) {
  reactive({
    df <- vals$final_data

    # Filter out rows where columns 1, 2, and 3 are all empty
    df <- df %>% 
      filter(!is.na(X) & !is.na(Y))

    if (nrow(df) == 0) {
      shiny::showNotification("Please enter data into the table.", type = "error")
      return(data.frame())
    }
    
    summary_stats <- data.frame(matrix(ncol = ncol(df), nrow = 1))
    colnames(summary_stats) <- colnames(df)
   
    # Calculate summary statistics
    summary_stats[1, 4] <- mean(df[[4]], na.rm = TRUE)
    summary_stats[1, 5] <- mean(df[[5]], na.rm = TRUE)
    summary_stats[1, 1] <- "SUMMARY"
    
    # Append summary statistics row to df
    df <- rbind(df, summary_stats)
    
    df <- df %>%
      mutate(
        Per_Diff = percent(Per_Diff, accuracy = 0.1),
        Limit_per = percent(Limit_per, accuracy = 0.1)
      ) %>%
      mutate(across(everything(), ~ifelse(is.na(.), "", .)))
    
    colnames(df) <- c(
      'Sample',
      'X',
      'Y',
      'AbsDiff',
      'PerDiff',
      'PassFail',
      'Limitper'
    )
    
    return(df)
  })
}

# Create dataset input reactive
create_dataset_input_reactive <- function(vals) {
  eventReactive(vals$hot_data, {
    vals$hot_data
  })
}