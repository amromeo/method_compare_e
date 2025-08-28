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
    df <- hot_to_r(input$hot)
    
    if (is.null(df) || nrow(df) == 0) return()
    if (!all(c("X", "Y") %in% names(df))) return()
    if (!is.numeric(df$X) || !is.numeric(df$Y)) return()
    
    df$Abs_Diff <- df$Y - df$X
    df$Per_Diff <- df$Abs_Diff / df$X
    limit_value <- input$limitValue
    
    df$Per_Diff <- suppressWarnings(as.numeric(df$Per_Diff))
    if (length(df$Per_Diff) == 0 || length(df$Per_Diff) != nrow(df)) {
      df$Pass_Fail <- rep("", nrow(df))
    } else {
      print("DEBUG: Per_Diff content")
      print(df$Per_Diff)
      print("DEBUG: limit_value")
      print(limit_value)
      print("DEBUG: abs(Per_Diff) <= limit_value")
      print(abs(df$Per_Diff) <= limit_value)
      
      logic_check <- abs(df$Per_Diff) <= limit_value
      if (length(logic_check) != nrow(df)) {
        logic_check <- rep(NA, nrow(df))
      }
      
      df$Pass_Fail <- dplyr::case_when(
        is.na(df$Per_Diff) ~ "",
        logic_check ~ "PASS",
        TRUE ~ "FAIL"
      )
    }
    
    df$Limit_per <- limit_value
    vals$final_data <- df
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