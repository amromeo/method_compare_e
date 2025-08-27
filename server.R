
safe_filename <- function(x) gsub("[^a-zA-Z0-9_\\-]", "_", x)

# Source data processing module
source("modules/data_processing.R")

shinyServer(function(input, output, session) {
  
  observeEvent(input$debug_button, {
    print(reactiveValuesToList(input))
  })
  
  input_snapshot <- reactiveVal()
  
  observeEvent(input$debug_button, {
    input_snapshot(reactiveValuesToList(input))
  })
  
  output$snapshot_text <- renderPrint({
    input_snapshot()
  })
  
  default_limits <- c(
    QFA = 0.20,
    Ddimer = 0.24,
    AntiXa = 0.20,
    DTIBI = 0.20,
    APTT = 0.15,
    PT = 0.15,
    "vWF Antigen" = 0.15,
    "vWF Activity" = 0.15,
    "Thrombin Time" = 0.20,
    "Protein C" = 0.15,
    "Protein S" = 0.15,
    "Protein S Free" = 0.15,
    "ADAMTS13 activity" = 0.20,
    sC5B9 = 0.20,
    "Factor II (Prothrombin)" = 0.15,
    "Factor V" = 0.15,
    "Factor VII" = 0.15,
    "Factor VIII" = 0.15,
    "Factor VIII Chromogenic" = 0.15,
    "Factor VIII Inhibitor" = 0.15,
    "Chromogenic Factor VIII Inhibitor" = 0.15,
    "Factor IX" = 0.15,
    "Factor IX Inhibitor" = 0.15,
    "Factor X" = 0.15,
    "Factor XI" = 0.15,
    "Factor XII" = 0.15,
    "Factor XIII" = 0.15
  )
  
  
  observeEvent(input$testInput, {
    req(input$testInput)
    if (input$testInput %in% names(default_limits)) {
      updateNumericInput(session, "limitValue", value = default_limits[[input$testInput]])
    }
  })
  
  test_name <- reactive({
    if (input$testInput == "Other") {
      req(input$customTestInput)
      input$customTestInput
    } else {
      input$testInput
    }
  })
  
  
  output$limitValueUI <- renderUI({
    req(input$testInput)
    
    init_limit <- if (input$testInput %in% names(default_limits)) {
      default_limits[[input$testInput]]
    } else {
      0.15  # fallback if "Other" or unknown
    }
    
    numericInput("limitValue", "Set % Cutoff", 
                 value = init_limit, 
                 min = 0, max = 1, step = 0.01)
  })
  
  
  
  observeEvent(input$restore_button, {
    snapshot <- input_snapshot()
    if (is.null(snapshot)) return()
    
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
  
  
  vals <- initialize_data_values()
  create_data_observer(input, vals)
  
  

  output$dynamicReagentLot <- renderUI({
    HTML(paste("<p><strong>Reagent Lot:</strong>", ifelse(is.null(input$reagentLotInput), "Not entered", input$reagentLotInput), "</p>"))
  })
  
  output$dynamicExpiration <- renderUI({
    HTML(paste("<p><strong>Expiration:</strong>", ifelse(is.null(input$expirationInput), "Not entered", input$expirationInput), "</p>"))
  })
  
  output$dynamicLimitPer <- renderUI({
    req(test_name(), input$limitValue)
    HTML(paste("<p><strong>Test:</strong>", test_name(), "</p>",
               "<p><strong>Limit (%):</strong>", percent(input$limitValue, accuracy = 0.1), "</p>"))
  })
  
  
  
  output$dynamicDate <- renderUI({
    # Ensure input$dateInput is a Date object before formatting
    formattedDate <- if(!is.null(input$dateInput) && inherits(input$dateInput, "Date")) {
      format(input$dateInput, "%Y-%m-%d")
    } else {
      "Not entered"
    }
    HTML(paste("<p><strong>Date:</strong>", formattedDate, "</p>"))
  })
  
  
  
  datasetInput <- create_dataset_input_reactive(vals)
  
  mod_data <- create_mod_data_reactive(vals)
  

  output$kableTable <- renderText({
    df <- mod_data()  
    tableHTML<-generateKableTable(df, format = "html") 
    tableHTML
    
  })
    


  output$hot <- renderRHandsontable({
    a <- vals$final_data 
    rhandsontable(a
                  , height = 482
                  , rowHeaders = NULL) %>%
      hot_col(col = colnames(a)[1]) %>%
      hot_col(col = colnames(a)[2], format = '0.00', type = 'numeric') %>%
      hot_col(col = colnames(a)[3], format = '0.00', type = 'numeric') %>%
      hot_cols(colWidths = ifelse(!names(a) %in% c('Abs_Diff', 'Per_Diff', 'Pass_Fail','Limit_per') == T, 150, 0.1))

    
  })
  
  
  
  output$plot1 <- renderPlot({
    
    a <- vals$final_data
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c('Sample','M1', 'M2')
        data1 <- try(mcreg(a$M1, a$M2,
                       mref.name = input$m1, mtest.name = input$m2, 
                       na.rm = TRUE), silent = TRUE)
        try(MCResult.plotDifference(data1, plot.type = input$batype,
                                add.grid = TRUE), silent = TRUE)
        
      }
    
  })
  
  output$plot2 <- renderPlot({
    
    a <- vals$final_data
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c('Sample','M1', 'M2')
        data1 <- try(mcreg(a$M1, a$M2, error.ratio = input$syx, 
                       method.reg = input$regmodel, method.ci = input$cimethod,
                       method.bootstrap.ci = input$metbootci, 
                       slope.measure = "radian", na.rm = TRUE), silent = TRUE)
        try(MCResult.plot(data1, ci.area = input$ciarea,
                      add.legend = input$legend, identity = input$identity,
                      add.cor = input$addcor, x.lab = input$m1,
                      y.lab = input$m2, cor.method = input$cormet,
                      equal.axis = TRUE, add.grid = TRUE, 
                      na.rm = TRUE), silent = TRUE)
        
      }
    
  })

  output$plot3 <- renderPlot({
    
    a <- vals$final_data
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c('Sample','M1', 'M2')
        data1 <- try(mcreg(a$M1, a$M2, error.ratio = input$syx, 
                       method.reg = input$regmodel, method.ci = input$cimethod,
                       method.bootstrap.ci = input$metbootci, slope.measure = "radian",
                       mref.name = input$m1, mtest.name = input$m2, 
                       na.rm = TRUE), silent = TRUE)
        try(compareFit(data1), silent = TRUE)
        
      }
    
  })  
  
  
  output$summary <- renderPrint({
    
    a <- vals$final_data
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c('Sample','M1', 'M2')
        data1 <- try(mcreg(a$M1, a$M2, error.ratio = input$syx, 
                       method.reg = input$regmodel, method.ci = input$cimethod,
                       method.bootstrap.ci = input$metbootci, slope.measure = "radian",
                       mref.name = input$m1, mtest.name = input$m2, 
                       na.rm = TRUE), silent = TRUE)
        try(printSummary(data1), silent = TRUE)
      }
    
  })
  

  
  output$downloadReport <- downloadHandler(

    
    filename = function() {
      req(input$format, test_name())
      m <- method_names()
      paste0(
        safe_filename(test_name()), "_",
        safe_filename(m$m1), "_vs_", safe_filename(m$m2), "_",
        Sys.Date(), ".", tolower(input$format)
      )
    },
    content = function(file) {
      currentData <- mod_data()
      m <- method_names()
      
      params <- list(
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
  )
  
})
