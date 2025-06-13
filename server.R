


shinyServer(function(input, output, session) {
  
  
  vals <- reactiveValues(hot_data = data.frame('Sample'= rep(NA_character_, 10),
                                               'X'= round(c(rep(NA, 10)), digits = 2),
                                               'Y'= round(c(rep(NA, 10)), digits = 2),
                                               'Abs_Diff'= round(c(rep(NA, 10)), digits = 2),
                                               'Per_Diff'= round(c(rep(NA, 10)), digits = 2),
                                               'Pass_Fail'= rep(NA_character_, 10),
                                               'Limit_per' = rep(TRUE,10)
                                               ),
                        final_data = data.frame('Sample'= rep(NA_character_, 10),
                                                'X'= round(c(rep(NA, 10)), digits = 2),
                                                'Y'= round(c(rep(NA, 10)), digits = 2),
                                                'Abs_Diff'= round(c(rep(NA, 10)), digits = 2),
                                                'Per_Diff'= round(c(rep(NA, 10)), digits = 2),
                                                'Pass_Fail'= rep(NA_character_, 10),
                                                'Limit_per' = rep(.15,10)
                                                )
                        )

board <- board_connect()

observe({
  pins <- try(pin_list(board), silent = TRUE)
  
  if (inherits(pins, "try-error") || length(pins) == 0) {
    choices <- character(0)
  } else if (is.data.frame(pins) && "name" %in% names(pins)) {
    choices <- pins$name
  } else {
    choices <- pins
  }

  updateSelectInput(session, "load_state", choices = choices)
})


  observeEvent(input$save_state, {
    state <- list(
      inputs = reactiveValuesToList(input),
      vals = list(final_data = vals$final_data, hot_data = vals$hot_data)
    )
    name <- paste0("state_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    pin_write(board, state, name = name, type = "rds")
    updateSelectInput(session, "load_state", choices = pin_list(board)$name)
  })

  observeEvent(input$load_state, {
    req(input$load_state)
    state <- pin_read(board, input$load_state)
    for(n in names(state$inputs)) {
      val <- state$inputs[[n]]
      if(!is.null(input[[n]])) {
        if(inherits(val, "Date")) {
          updateDateInput(session, n, value = val)
        } else if(is.numeric(val) && length(val) == 1) {
          updateNumericInput(session, n, value = val)
        } else if(is.logical(val) && length(val) == 1) {
          updateCheckboxInput(session, n, value = val)
        } else if(is.character(val) && length(val) == 1) {
          if(n %in% c("format", "regmodel", "cimethod", "metbootci", "batype", "limitPerInput", "cormet")) {
            updateSelectInput(session, n, selected = val)
          } else {
            updateTextInput(session, n, value = val)
          }
        } else if(is.character(val) && length(val) > 1) {
          updateCheckboxGroupInput(session, n, selected = val)
        }
      }
    }
    vals$final_data <- state$vals$final_data
    vals$hot_data <- state$vals$hot_data
  })
  
  observe({
    # This will trigger the code inside whenever `input$hot` or `input$limitPerInput` changes.
    
    # Check if `input$hot` is not NULL to prevent errors on initial load or before any table interaction
    if(!is.null(input$hot)) {
      limit_value <-as.numeric(switch(input$limitPerInput,
                                      "QFA" = 0.20,
                                      "Ddimer" = 0.24,
                                      "Factors" = 0.15,
                                      "AntiXa" = 0.20,
                                      "DTIBI" = 0.20,
                                      "APTT" = 0.20,
                                      "SpecialCoag" = 0.15
      )) # Retrieve the user-defined limit value
      
      vals$final_data <- hot_to_r(input$hot) %>%
        mutate(Abs_Diff = Y - X) %>%
        mutate(Per_Diff = Abs_Diff / X) %>%
        mutate(Pass_Fail = case_when(
          is.na(Per_Diff) ~ "",
          abs(Per_Diff) <= limit_value ~ "PASS", # Use the dynamic limit value
          TRUE ~ "FAIL"
        )) %>%
        mutate(Limit_per = limit_value) # Update Limit_per column with the user-defined value

    }
  })
  
  output$dynamicTest <- renderUI({
    HTML(paste("<p><strong>Test:</strong>", ifelse(is.null(input$testInput), "Not entered", input$testInput), "</p>"))
  })
  
  output$dynamicReagentLot <- renderUI({
    HTML(paste("<p><strong>Reagent Lot:</strong>", ifelse(is.null(input$reagentLotInput), "Not entered", input$reagentLotInput), "</p>"))
  })
  
  output$dynamicExpiration <- renderUI({
    HTML(paste("<p><strong>Expiration:</strong>", ifelse(is.null(input$expirationInput), "Not entered", input$expirationInput), "</p>"))
  })
  
  output$dynamicLimitPer <- renderUI({
    
    selected_text <- switch(input$limitPerInput,
                            "QFA" = "QFA Fibrinogen - 20%",
                            "Ddimer" = "D dimer - 24%",
                            "Factors" = "Factors - 15%",
                            "AntiXa" = "Anti-Xa Parameter - 20%",
                            "DTIBI" = "DTIBI Parameter - 20%",
                            "APTT" = "APTT - 20%",
                            "SpecialCoag" = "Special Coag Testing - 15%"
    )
    
    HTML(paste("<p><strong>Selected Option:</strong>", ifelse(is.null(input$limitPerInput), "Not entered", selected_text), "</p>"))
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
  
  
  
  datasetInput <- eventReactive(vals$hot_data,{
    vals$hot_data
  })
  
  mod_data <- reactive({
    df <- vals$final_data

    # Filter out rows where columns 1, 2, and 3 are all empty
    df <- df %>% 
      filter(!is.na(X) & !is.na(Y))

    
    if (nrow(df) == 0) {
      # No data entered, show a user-friendly error message
      shiny::showNotification("Please enter data into the table.", type = "error")
      return(data.frame())  # Return an empty data frame
    }
    
    summary_stats <- data.frame(matrix(ncol = ncol(df), nrow = 1)) # Create a data frame for summary stats
    colnames(summary_stats) <- colnames(df)
   
    
    # Assuming 4th and 5th columns are numeric and we're calculating their mean
    summary_stats[1, 4] <- mean(df[[4]], na.rm = TRUE)
    summary_stats[1, 5] <- mean(df[[5]], na.rm = TRUE)
    summary_stats[1, 1] <- "SUMMARY"
    
    # Append summary statistics row to df
    df <- rbind(df, summary_stats)
    
    df <- df%>%
      mutate(
        Per_Diff = percent(Per_Diff, accuracy = 0.1),
        Limit_per =percent(Limit_per, accuracy = 0.1)
      ) %>%
      mutate(across(everything(), ~ifelse(is.na(.), "", .)))
    
    colnames(df) <- c(
    'Sample',
    'X',
    'Y',
    'AbsDiff',
    'PerDiff',
    'PassFail',
    'Limitper')
    
    return(df)
  })
  

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
      # Construct the file name dynamically based on user input and date
      paste0(input$m1, " vs. ", input$m2, " @ ", Sys.Date(), ".", 
             switch(input$format, PDF = "pdf", HTML = "html", Word = "docx"))
    },
    content = function(file) {
      currentData <- mod_data() 
      # Define the parameters for the R Markdown document
      params <- list(
        tabledata = currentData,
        data = vals$final_data,
        m1 = input$m1,
        m2 = input$m2,
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
        test = input$testInput,
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
