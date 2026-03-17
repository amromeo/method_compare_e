
safe_filename <- function(x) gsub("[^a-zA-Z0-9_\\-]", "_", x)

# Source modules
source("modules/infra/logging.R")
source("modules/infra/log_viewer.R")
source("modules/infra/config_loader.R")
source("modules/infra/error_handling.R")

source("modules/domain/admin_panel.R")
source("modules/domain/reactive_data.R")
source("modules/domain/data_processing.R")
source("modules/domain/plot_generation.R")
source("modules/domain/ui_reactive.R")
source("modules/domain/download_handler.R")
source("modules/domain/validation.R")

shinyServer(function(input, output, session) {
  
  # Initialize session logging
  session_id <- init_session_logging(session)
  
  # Debug button with proper logging
  observeEvent(input$debug_button, {
    input_state <- reactiveValuesToList(input)
    log_audit("DEBUG_SNAPSHOT", paste("Input state captured,", length(input_state), "inputs"), session_id = session_id)
    log_debug(paste("Input snapshot:", jsonlite::toJSON(input_state, auto_unbox = TRUE)), "debug", session_id)
  })
  
  input_snapshot <- reactiveVal()
  
  observeEvent(input$debug_button, {
    input_snapshot(reactiveValuesToList(input))
    log_audit("SNAPSHOT_SAVED", "Input state saved for restore", session_id = session_id)
  })
  
  output$snapshot_text <- renderPrint({
    input_snapshot()
  })
  
  default_limits <- load_test_limits()
  
  
  create_test_input_observer(input, session, default_limits)
  
  # Log key input changes
  observeEvent(input$testInput, {
    if (!is.null(input$testInput)) {
      log_user_interaction("TEST_SELECTED", "testInput", input$testInput, session_id)
    }
  })
  
  observeEvent(input$limitValue, {
    if (!is.null(input$limitValue)) {
      log_user_interaction("LIMIT_CHANGED", "limitValue", input$limitValue, session_id)
    }
  })
  
  observeEvent(input$hot, {
    if (!is.null(input$hot)) {
      hot_data <- hot_to_r(input$hot)
      valid_count <- if (!is.null(hot_data)) sum(!is.na(hot_data$X) & !is.na(hot_data$Y), na.rm = TRUE) else 0
      log_user_interaction("DATA_ENTERED", "hot", paste("Valid data pairs:", valid_count), session_id)
    }
  })
  
  test_name <- create_test_name_reactive(input)
  
  
  output$limitValueUI <- create_limit_value_ui_render(input, default_limits)
  
  
  
  observeEvent(input$restore_button, {
    snapshot <- input_snapshot()
    if (is.null(snapshot)) {
      log_warning("Restore attempted but no snapshot available", "restore", session_id)
      return()
    }
    log_audit("RESTORE_INPUTS", "Restoring previous input state", session_id = session_id)
    
    # Standard inputs
    if (!is.null(snapshot$limitValue)) updateNumericInput(session, "limitValue", value = snapshot$limitValue)
#   if (!is.null(snapshot$limitPerInput)) updateSelectInput(session, "limitPerInput", selected = snapshot$limitPerInput)
    if (!is.null(snapshot$batype)) updateSelectInput(session, "batype", selected = snapshot$batype)
    if (!is.null(snapshot$regmodel)) updateSelectInput(session, "regmodel", selected = snapshot$regmodel)
    if (!is.null(snapshot$cimethod)) updateSelectInput(session, "cimethod", selected = snapshot$cimethod)
    if (!is.null(snapshot$metbootci)) updateSelectInput(session, "metbootci", selected = snapshot$metbootci)
    if (!is.null(snapshot$cormet)) updateSelectInput(session, "cormet", selected = snapshot$cormet)
    if (!is.null(snapshot$dateInput)) updateDateInput(session, "dateInput", value = snapshot$dateInput)
    if (!is.null(snapshot$format)) updateSelectInput(session, "format", selected = snapshot$format)
    
    if (!is.null(snapshot$identity)) updateCheckboxInput(session, "identity", value = snapshot$identity)
    if (!is.null(snapshot$ciarea)) updateCheckboxInput(session, "ciarea", value = snapshot$ciarea)
    if (!is.null(snapshot$legend)) updateCheckboxInput(session, "legend", value = snapshot$legend)
    if (!is.null(snapshot$addcor)) updateCheckboxInput(session, "addcor", value = snapshot$addcor)
    
    if (!is.null(snapshot$syx)) updateNumericInput(session, "syx", value = snapshot$syx)
    if (!is.null(snapshot$testInput)) updateTextInput(session, "testInput", value = snapshot$testInput)
    if (!is.null(snapshot$customTestInput)) updateTextInput(session, "customTestInput", value = snapshot$customTestInput)
    if (!is.null(snapshot$reagentLotInput)) updateTextInput(session, "reagentLotInput", value = snapshot$reagentLotInput)
    if (!is.null(snapshot$expirationInput)) updateTextInput(session, "expirationInput", value = snapshot$expirationInput)
    
    # HOT table
    # HOT table
    if (!is.null(snapshot$hot$params$data)) {
      raw_data <- snapshot$hot$params$data
      
      rows <- lapply(raw_data, function(row) {
        clean_row <- lapply(row, function(x) if (is.null(x)) NA else x)
        unlist(clean_row, use.names = FALSE)
      })
      
      rows <- Filter(function(r) length(r) == 7, rows)
      
      if (length(rows) > 0) {
        df <- do.call(rbind, rows)
        df <- as.data.frame(df, stringsAsFactors = FALSE)
        colnames(df) <- c("Sample", "X", "Y", "Abs_Diff", "Per_Diff", "Pass_Fail", "Limit_per")
        
        df$X <- as.numeric(df$X)
        df$Y <- as.numeric(df$Y)
        df$Abs_Diff <- as.numeric(df$Abs_Diff)
        df$Per_Diff <- as.numeric(df$Per_Diff)
        df$Limit_per <- as.numeric(df$Limit_per)
        
        vals$final_data <- df
      } else {
        showNotification("No valid HOT rows after cleanup", type = "warning")
      }
    }
    
    
    
    
  })
  
  method_names <- reactive({
    req(input$comparisons)
    switch(input$comparisons,
           "old-new"    = list(m1 = "old",  m2 = "new"),
           "mil1-mil2"  = list(m1 = "mil1", m2 = "mil2"),
           "mil1-mil3"  = list(m1 = "mil1", m2 = "mil3"),
           "mil2-mil3"  = list(m1 = "mil2", m2 = "mil3"),
           list(m1 = "method1", m2 = "method2"))
  })

  label_xy_columns <- function(df, m) {
    if (is.null(df) || nrow(df) == 0) return(df)
    if ("X" %in% names(df)) names(df)[names(df) == "X"] <- m$m1
    if ("Y" %in% names(df)) names(df)[names(df) == "Y"] <- m$m2
    df
  }
  
  
  # Create clear reactive data pipeline
  data_store <- create_reactive_data_store()
  
  # Define reactive chain: raw → processed → analysis/display
  raw_data <- create_raw_data_reactive(input)
  processed_data <- create_processed_data_reactive(raw_data, input, session_id)
  analysis_data <- create_analysis_data_reactive(processed_data)
  display_data <- create_display_data_reactive(processed_data)
  
  

  output$dynamicReagentLot <- create_dynamic_reagent_lot_render(input)
  
  output$dynamicExpiration <- create_dynamic_expiration_render(input)
  
  output$dynamicLimitPer <- create_dynamic_limit_per_render(input, test_name)
  
  
  
  output$dynamicDate <- create_dynamic_date_render(input)
  
  
  
  # Use display data for tables and reports
  mod_data <- display_data
  

  output$kableTable <- renderText({
    df <- label_xy_columns(mod_data(), method_names())
    tableHTML<-generateKableTable(df, format = "html") 
    tableHTML
    
  })
    


  output$hot <- renderRHandsontable({
    # Always show a table structure for data entry
    a <- processed_data()
    m <- method_names()
    required_cols <- c("Sample", "X", "Y", "Abs_Diff", "Per_Diff", "Pass_Fail", "Limit_per")
    
    # If no processed data, create default table structure
    if (is.null(a) || nrow(a) == 0 || !all(required_cols %in% names(a))) {
      a <- data.frame(
        'Sample' = rep(NA_character_, 10),
        'X' = rep(NA_real_, 10),
        'Y' = rep(NA_real_, 10),
        'Abs_Diff' = rep(NA_real_, 10),
        'Per_Diff' = rep(NA_real_, 10),
        'Pass_Fail' = rep(NA_character_, 10),
        'Limit_per' = rep(0.15, 10)
      )
    }

    # Enforce stable schema before applying HOT column formatting.
    a <- a[, required_cols, drop = FALSE]

    rhandsontable(a
                  , height = 482
                  , rowHeaders = NULL
                  , colHeaders = c("Sample", m$m1, m$m2, "Abs_Diff", "Per_Diff", "Pass_Fail", "Limit_per")) %>%
      hot_col(col = "Sample") %>%
      hot_col(col = "X", format = '0.00', type = 'numeric') %>%
      hot_col(col = "Y", format = '0.00', type = 'numeric') %>%
      hot_cols(colWidths = ifelse(names(a) %in% c("Sample", "X", "Y"), 150, 0.1))

    
  })
  
  
  
  output$plot1 <- create_bland_altman_plot_render(analysis_data, input)
  
  output$plot2 <- create_method_comparison_plot_render(analysis_data, input)

  output$plot3 <- create_fit_comparison_plot_render(analysis_data, input)  
  
  
  output$summary <- create_summary_render(analysis_data, input)
  

  
  output$downloadReport <- create_download_handler(input, analysis_data, display_data, method_names, test_name, safe_filename, session_id)
  
  # Initialize admin panel (admin users only)
  create_admin_panel_server(input, output, session, session_id)
  
  # Cleanup logging when session ends
  session$onSessionEnded(function() {
    cleanup_session_logging(session_id)
  })
  
})
