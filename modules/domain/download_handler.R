# Download Handler Module
# Contains report generation functionality and file handling

# Required fields for report generation (download-only gating)
get_missing_required_report_fields <- function(input) {
  is_blank <- function(x) {
    if (is.null(x) || length(x) == 0) return(TRUE)
    if (is.character(x)) return(all(trimws(x) == ""))
    FALSE
  }
  is_missing_date <- function(x) {
    is.null(x) || length(x) == 0 || all(is.na(x))
  }

  missing <- character(0)
  
  if (is_blank(input$testInput)) {
    missing <- c(missing, "Select Test")
  }
  
  if (!is_blank(input$testInput) && identical(input$testInput, "Other")) {
    if (is_blank(input$customTestInput)) {
      missing <- c(missing, "Custom Test Name")
    }
  }
  
  if (is_blank(input$comparisons)) {
    missing <- c(missing, "Comparison")
  }
  
  if (is.null(input$limitValue) || !is.numeric(input$limitValue) || is.na(input$limitValue)) {
    missing <- c(missing, "Cutoff (%)")
  }
  
  if (is_blank(input$reagentLotInput)) {
    missing <- c(missing, "Reagent Lot")
  }
  
  if (is_blank(input$expirationInput)) {
    missing <- c(missing, "Expiration")
  }
  
  if (is_missing_date(input$dateInput)) {
    missing <- c(missing, "Date")
  }
  
  missing
}

report_fields_complete <- function(input, notify = FALSE) {
  missing <- get_missing_required_report_fields(input)
  if (length(missing) > 0) {
    msg <- paste("Please complete required fields before downloading:", paste(missing, collapse = ", "))
    if (notify) {
      showNotification(msg, type = "error", duration = 10)
    }
    return(FALSE)
  }
  TRUE
}

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
render_report_content <- function(file, params, format, session_id = "unknown", notify = TRUE, write_error_file = TRUE) {
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
    if (!format %in% valid_formats) {
      stop(paste("Invalid output format. Must be one of:", paste(valid_formats, collapse = ", ")))
    }
    
    # Log report generation start
    log_info(paste("Starting report generation - Format:", format), "report_generation", session_id)
    
    # Render the R Markdown document to the appropriate format
    rmarkdown::render(
      input = tempReport, 
      output_file = file,
      params = params,
      output_format = switch(
        format,
        PDF = rmarkdown::pdf_document(), 
        HTML = rmarkdown::html_document(), 
        Word = rmarkdown::word_document()
      ),
      envir = new.env(parent = globalenv())
    )
    
    log_audit("REPORT_GENERATED", paste("Successfully created", format, "report"), session_id = session_id)
    TRUE
    
  }, error = function(e) {
    error_msg <- paste("Report generation failed:", e$message)
    log_error_with_context(e$message, "render_report_content", paste("Format:", format), session_id)
    if (notify) {
      showNotification(error_msg, type = "error", duration = 15)
    }
    
    if (write_error_file) {
      # Create a simple error file
      writeLines(c(
        "Report Generation Error",
        paste("Time:", Sys.time()),
        paste("Error:", e$message),
        "",
        "Please check your data and try again."
      ), file)
    }
    FALSE
  })
}

get_allowed_report_recipients <- function() {
  c(
    "diogon@chop.edu",
    "obstfelda@chop.edu",
    "lambertm@chop.edu",
    "nguyenlp@chop.edu"
  )
}

send_report_email <- function(input, analysis_data_reactive, display_data_reactive, method_names, test_name, safe_filename, session_id = "unknown") {
  if (!report_fields_complete(input, notify = TRUE)) {
    return(invisible(FALSE))
  }
  
  recipients <- input$emailRecipients
  if (is.null(recipients) || length(recipients) == 0) {
    showNotification("Select at least one email recipient.", type = "error")
    return(invisible(FALSE))
  }
  
  allowed <- get_allowed_report_recipients()
  if (!all(recipients %in% allowed)) {
    showNotification("One or more selected recipients are not allowed.", type = "error")
    return(invisible(FALSE))
  }
  
  params <- prepare_report_params(input, analysis_data_reactive, display_data_reactive, method_names, test_name)
  tmp_pdf <- tempfile(pattern = "method_compare_report_", fileext = ".pdf")
  ok <- render_report_content(tmp_pdf, params, format = "PDF", session_id = session_id, notify = FALSE, write_error_file = FALSE)
  
  if (!isTRUE(ok) || !file.exists(tmp_pdf) || isTRUE(file.info(tmp_pdf)$size <= 0)) {
    showNotification("Could not generate PDF for email attachment.", type = "error")
    return(invisible(FALSE))
  }
  
  sender <- Sys.getenv("REPORT_FROM_EMAIL", unset = "clinical-diagnostics@chop.edu")
  smtp_server <- Sys.getenv("REPORT_SMTP_SERVER", unset = "mailrouter.chop.edu")
  subject <- if (!is.null(input$emailSubject) && nzchar(trimws(input$emailSubject))) input$emailSubject else "CHOP Coagulation Report"
  body_text <- if (!is.null(input$emailBody) && nzchar(trimws(input$emailBody))) input$emailBody else "Please see attached PDF report."
  attachment_name <- paste0(safe_filename(test_name()), "_", Sys.Date(), ".pdf")
  
  msg_body <- sendmailR::mime_part(body_text)
  msg_body[["headers"]][["Content-Type"]] <- "text/plain; charset=utf-8"
  msg_attachment <- sendmailR::mime_part(tmp_pdf, name = attachment_name)
  
  tryCatch({
    sendmailR::sendmail(
      from = sender,
      to = recipients,
      subject = subject,
      msg = list(msg_body, msg_attachment),
      control = list(smtpServer = smtp_server)
    )
    
    log_audit(
      "REPORT_EMAIL_SENT",
      paste("Recipients:", paste(recipients, collapse = ","), "| Subject:", subject),
      session_id = session_id
    )
    showNotification("Email sent successfully.", type = "message")
    TRUE
  }, error = function(e) {
    log_error_with_context(e$message, "send_report_email", paste("Recipients:", paste(recipients, collapse = ",")), session_id)
    showNotification(paste("Failed to send email:", e$message), type = "error", duration = 12)
    FALSE
  })
}

# Create complete download handler
create_download_handler <- function(input, analysis_data_reactive, display_data_reactive, method_names, test_name, safe_filename, session_id = "unknown") {
  downloadHandler(
    filename = function() {
      generate_report_filename(input, test_name, method_names, safe_filename)
    },
    content = function(file) {
      if (!report_fields_complete(input, notify = TRUE)) {
        return(invisible(NULL))
      }
      params <- prepare_report_params(input, analysis_data_reactive, display_data_reactive, method_names, test_name)
      render_report_content(file, params, format = input$format, session_id = session_id, notify = TRUE, write_error_file = TRUE)
    }
  )
}
