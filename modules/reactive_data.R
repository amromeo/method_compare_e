# Reactive Data Module  
# Creates a clear, linear reactive data flow pipeline
# Requires: dplyr, scales (for percent formatting)

# Initialize the reactive data store with clear naming
create_reactive_data_store <- function() {
  reactiveValues(
    # Raw input data directly from HOT table - source of truth
    raw_input = data.frame(
      'Sample' = rep(NA_character_, 10),
      'X' = rep(NA_real_, 10),
      'Y' = rep(NA_real_, 10)
    )
  )
}

# Create reactive for raw data input (from HOT table)
create_raw_data_reactive <- function(input) {
  reactive({
    # Always provide a default structure, even without HOT input
    default_structure <- data.frame(
      Sample = rep(NA_character_, 10),
      X = rep(NA_real_, 10),
      Y = rep(NA_real_, 10)
    )
    
    # If no HOT input yet, return default structure
    if (is.null(input$hot)) {
      return(default_structure)
    }
    
    # Get data directly from HOT table
    raw_df <- hot_to_r(input$hot)
    
    # Ensure we have the required columns
    if (is.null(raw_df) || !all(c("Sample", "X", "Y") %in% names(raw_df))) {
      return(default_structure)
    }
    
    # Return clean raw data
    raw_df[c("Sample", "X", "Y")]
  })
}

# Create reactive for processed data (validation + calculations)
create_processed_data_reactive <- function(raw_data_reactive, input) {
  reactive({
    # Get raw data (which always provides default structure)
    raw_df <- raw_data_reactive()
    if (is.null(raw_df) || nrow(raw_df) == 0) {
      # Return default structure for HOT table
      return(data.frame(
        'Sample' = rep(NA_character_, 10),
        'X' = rep(NA_real_, 10),
        'Y' = rep(NA_real_, 10),
        'Abs_Diff' = rep(NA_real_, 10),
        'Per_Diff' = rep(NA_real_, 10),
        'Pass_Fail' = rep(NA_character_, 10),
        'Limit_per' = rep(0.15, 10)
      ))
    }
    
    # Default limit value if not set
    limit_value <- if (is.null(input$limitValue)) 0.15 else input$limitValue
    
    # Validate limit value (only if it exists)
    if (!is.null(input$limitValue) && (!is.numeric(input$limitValue) || input$limitValue < 0 || input$limitValue > 1)) {
      showNotification("Limit value must be between 0 and 1", type = "error")
      # Return default structure even with invalid limit
      return(data.frame(
        'Sample' = rep(NA_character_, 10),
        'X' = rep(NA_real_, 10),
        'Y' = rep(NA_real_, 10),
        'Abs_Diff' = rep(NA_real_, 10),
        'Per_Diff' = rep(NA_real_, 10),
        'Pass_Fail' = rep(NA_character_, 10),
        'Limit_per' = rep(0.15, 10)
      ))
    }
    
    tryCatch({
      # Start with raw data and add calculated columns
      processed_df <- raw_df
      processed_df$X <- as.numeric(processed_df$X)
      processed_df$Y <- as.numeric(processed_df$Y)
      
      # Always add the calculated columns (initially with defaults)
      processed_df$Abs_Diff <- rep(NA_real_, nrow(processed_df))
      processed_df$Per_Diff <- rep(NA_real_, nrow(processed_df))
      processed_df$Pass_Fail <- rep(NA_character_, nrow(processed_df))
      processed_df$Limit_per <- rep(limit_value, nrow(processed_df))
      
      # Check if user has actually entered data
      valid_pairs <- !is.na(processed_df$X) & !is.na(processed_df$Y)
      has_user_data <- any(!is.na(processed_df$X) | !is.na(processed_df$Y) | 
                          (processed_df$Sample != "" & !is.na(processed_df$Sample)), na.rm = TRUE)
      
      # If no user data, return default structure without processing
      if (!has_user_data) {
        return(processed_df)
      }
      
      if (sum(valid_pairs) == 0 && has_user_data) {
        showNotification("No valid X,Y data pairs found", type = "warning")
        return(processed_df)  # Return structure instead of NULL
      }
      
      # Data quality checks for valid pairs only
      if (sum(valid_pairs) > 0) {
        # Check for negative values
        negative_count <- sum(processed_df$X[valid_pairs] < 0, na.rm = TRUE) + 
                         sum(processed_df$Y[valid_pairs] < 0, na.rm = TRUE)
        if (negative_count > 0) {
          showNotification(
            paste("Warning:", negative_count, "negative values detected. Medical test values should typically be positive."),
            type = "warning", duration = 8
          )
        }
        
        # Check for extreme values
        max_reasonable <- 10000
        extreme_count <- sum(processed_df$X[valid_pairs] > max_reasonable, na.rm = TRUE) + 
                        sum(processed_df$Y[valid_pairs] > max_reasonable, na.rm = TRUE)
        if (extreme_count > 0) {
          showNotification(
            paste("Warning:", extreme_count, "values above", max_reasonable, "detected. Please verify data entry."),
            type = "warning", duration = 10
          )
        }
      }
      
      # Only calculate for valid data pairs
      if (sum(valid_pairs) > 0) {
        # Calculate differences and pass/fail
        processed_df$Abs_Diff[valid_pairs] <- processed_df$Y[valid_pairs] - processed_df$X[valid_pairs]
        processed_df$Per_Diff[valid_pairs] <- ifelse(processed_df$X[valid_pairs] == 0, NA, 
                                                    processed_df$Abs_Diff[valid_pairs] / processed_df$X[valid_pairs])
      }
      
      # Handle division by zero warnings
      zero_x_count <- sum(processed_df$X == 0 & valid_pairs, na.rm = TRUE)
      if (zero_x_count > 0) {
        showNotification(
          paste("Warning:", zero_x_count, "rows with X=0 cannot calculate percentage difference"),
          type = "warning"
        )
      }
      
      # Calculate Pass/Fail status
      limit_value <- input$limitValue
      processed_df$Pass_Fail <- dplyr::case_when(
        is.na(processed_df$Per_Diff) ~ "",
        is.na(processed_df$X) | is.na(processed_df$Y) ~ "",
        abs(processed_df$Per_Diff) <= limit_value ~ "PASS",
        TRUE ~ "FAIL"
      )
      
      processed_df$Limit_per <- limit_value
      
      return(processed_df)
      
    }, error = function(e) {
      # Only show error if user has actually entered data
      if (has_user_data) {
        showNotification(
          paste("Data processing error:", e$message), 
          type = "error", duration = 10
        )
      }
      # Return default structure instead of NULL
      return(data.frame(
        'Sample' = rep(NA_character_, 10),
        'X' = rep(NA_real_, 10),
        'Y' = rep(NA_real_, 10),
        'Abs_Diff' = rep(NA_real_, 10),
        'Per_Diff' = rep(NA_real_, 10),
        'Pass_Fail' = rep(NA_character_, 10),
        'Limit_per' = rep(limit_value, 10)
      ))
    })
  })
}

# Create reactive for analysis data (clean data for plots/statistics)
create_analysis_data_reactive <- function(processed_data_reactive) {
  reactive({
    processed_df <- processed_data_reactive()
    if (is.null(processed_df)) return(NULL)
    
    # Filter to only valid data pairs for analysis
    analysis_df <- processed_df %>% 
      filter(!is.na(X) & !is.na(Y)) %>%
      select(Sample, X, Y, Abs_Diff, Per_Diff, Pass_Fail, Limit_per)
    
    # Rename for analysis functions
    names(analysis_df) <- c('Sample', 'M1', 'M2', 'Abs_Diff', 'Per_Diff', 'Pass_Fail', 'Limit_per')
    
    return(analysis_df)
  })
}

# Create reactive for display data (formatted for tables/reports)
create_display_data_reactive <- function(processed_data_reactive) {
  reactive({
    processed_df <- processed_data_reactive()
    if (is.null(processed_df)) return(data.frame())
    
    # Filter to rows with actual data
    display_df <- processed_df %>% 
      filter(!is.na(X) & !is.na(Y))
    
    if (nrow(display_df) == 0) {
      showNotification("Please enter data into the table.", type = "error")
      return(data.frame())
    }
    
    # Add summary statistics row
    summary_stats <- data.frame(
      Sample = "SUMMARY",
      X = NA,
      Y = NA,
      Abs_Diff = mean(display_df$Abs_Diff, na.rm = TRUE),
      Per_Diff = mean(display_df$Per_Diff, na.rm = TRUE),
      Pass_Fail = "",
      Limit_per = display_df$Limit_per[1]
    )
    
    display_df <- rbind(display_df, summary_stats)
    
    # Format for display
    display_df <- display_df %>%
      mutate(
        Per_Diff = percent(Per_Diff, accuracy = 0.1),
        Limit_per = percent(Limit_per, accuracy = 0.1)
      ) %>%
      mutate(across(everything(), ~ifelse(is.na(.), "", .)))
    
    # Rename columns for display
    colnames(display_df) <- c('Sample', 'X', 'Y', 'AbsDiff', 'PerDiff', 'PassFail', 'Limitper')
    
    return(display_df)
  })
}