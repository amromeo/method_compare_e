# Download Handler Module
# Contains report generation functionality and file handling

# Generate filename for download
generate_report_filename <- function(input, test_name, method_names, safe_filename, session_id = "unknown") {
  req(input$format, test_name())
  m <- method_names()
  filename <- paste0(
    safe_filename(test_name()), "_",
    safe_filename(m$m1), "_vs_", safe_filename(m$m2), "_",
    Sys.Date(), ".", tolower(input$format)
  )
  
  log_audit("REPORT_DOWNLOAD_INITIATED", paste("Format:", input$format, "| Test:", test_name(), "| File:", filename), session_id = session_id)
  
  return(filename)
}

# Prepare parameters for report rendering
prepare_report_params <- function(input, analysis_data_reactive, display_data_reactive, method_names, test_name) {
  display_data <- display_data_reactive()
  analysis_data <- analysis_data_reactive()
  m <- method_names()
  
  list(
    tabledata = display_data,
    data = analysis_data,
    m1 = m$m1,
    m2 = m$m2,
    syx = input$syx,
    regmodel = input$regmodel,
    cimethod = input$cimethod,
    metbootci = input$metbootci,
    batype = input$batype,
    ciarea = input$ciarea,
    legend = input$legend,
    identity = input$identity,
    addcor = input$addcor,
    cormet = input$cormet,
    name = input$name,
    test = test_name(),
    limit = input$limitValue,
    reagentLot = input$reagentLotInput,
    expiration = input$expirationInput,
    date = format(input$dateInput, "%Y-%m-%d")
  )
}

# Render report content
render_report_content <- function(file, params, input, session_id = "unknown") {
  tryCatch({
    # Validate report template exists
    if (!file.exists("report.Rmd")) {
      stop("Report template 'report.Rmd' not found")
    }
    
    # Prepare the R Markdown file for rendering
    tempReport <- file.path(tempdir(), "report.Rmd")
    if (!file.copy("report.Rmd", tempReport, overwrite = TRUE)) {
      stop("Failed to copy report template to temporary directory")
    }
    
    # Validate output format
    valid_formats <- c("PDF", "HTML", "Word")
    if (!input$format %in% valid_formats) {
      stop(paste("Invalid output format. Must be one of:", paste(valid_formats, collapse = ", ")))
    }
    
    # Log report generation start
    log_info(paste("Starting report generation - Format:", input$format), "report_generation", session_id)
    
    # Render the R Markdown document to the appropriate format
    rmarkdown::render(
      input = tempReport, 
      output_file = file,
      params = params,
      output_format = switch(
        input$format,
        PDF = rmarkdown::pdf_document(), 
        HTML = rmarkdown::html_document(), 
        Word = rmarkdown::word_document()
      ),
      envir = new.env(parent = globalenv())
    )
    
    log_audit("REPORT_GENERATED", paste("Successfully created", input$format, "report"), session_id = session_id)
    
  }, error = function(e) {
    error_msg <- paste("Report generation failed:", e$message)
    log_error_with_context(e$message, "render_report_content", paste("Format:", input$format), session_id)
    showNotification(error_msg, type = "error", duration = 15)
    
    # Create a simple error file
    writeLines(c(
      "Report Generation Error",
      paste("Time:", Sys.time()),
      paste("Error:", e$message),
      "",
      "Please check your data and try again."
    ), file)
  })
}

# Create complete download handler
create_download_handler <- function(input, analysis_data_reactive, display_data_reactive, method_names, test_name, safe_filename, session_id = "unknown") {
  downloadHandler(
    filename = function() {
      generate_report_filename(input, test_name, method_names, safe_filename)
    },
    content = function(file) {
      params <- prepare_report_params(input, analysis_data_reactive, display_data_reactive, method_names, test_name)
      render_report_content(file, params, input, session_id)
    }
  )
}