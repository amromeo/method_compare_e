# Download Handler Module
# Contains report generation functionality and file handling

# Generate filename for download
generate_report_filename <- function(input, test_name, method_names, safe_filename) {
  req(input$format, test_name())
  m <- method_names()
  paste0(
    safe_filename(test_name()), "_",
    safe_filename(m$m1), "_vs_", safe_filename(m$m2), "_",
    Sys.Date(), ".", tolower(input$format)
  )
}

# Prepare parameters for report rendering
prepare_report_params <- function(input, vals, mod_data, method_names, test_name) {
  currentData <- mod_data()
  m <- method_names()
  
  list(
    tabledata = currentData,
    data = vals$final_data,
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
render_report_content <- function(file, params, input) {
  # Prepare the R Markdown file for rendering
  tempReport <- file.path(tempdir(), "report.Rmd")
  file.copy("report.Rmd", tempReport, overwrite = TRUE)
  
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
}

# Create complete download handler
create_download_handler <- function(input, vals, mod_data, method_names, test_name, safe_filename) {
  downloadHandler(
    filename = function() {
      generate_report_filename(input, test_name, method_names, safe_filename)
    },
    content = function(file) {
      params <- prepare_report_params(input, vals, mod_data, method_names, test_name)
      render_report_content(file, params, input)
    }
  )
}