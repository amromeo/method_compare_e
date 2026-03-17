# Plot Generation Module
# Contains render functions for plots and summary output

# Create render function for Bland-Altman plot
create_bland_altman_plot_render <- function(analysis_data_reactive, input, method_names_reactive) {
  renderPlot({
    a <- analysis_data_reactive()
    m <- method_names_reactive()
    if (is.null(a)) {
      return(NULL)
    }
    
    # Validate data first
    validation <- validate_medical_data(a, "Bland-Altman analysis")
    if (!validation$valid) {
      show_validation_errors(validation$errors, "Bland-Altman Plot Error")
      return(NULL)
    }
    
    # Use safe mcreg wrapper with data frame
    result <- safe_mcreg(a, 
                        mref.name = m$m1, mtest.name = m$m2, 
                        na.rm = TRUE, 
                        context = "Bland-Altman regression")
    
    if (!result$success) {
      return(NULL)
    }
    
    # Generate plot safely
    safe_plot(function() {
      MCResult.plotDifference(result$result, plot.type = input$batype, add.grid = TRUE)
    }, a, "Bland-Altman plot generation")
  })
}

# Create render function for method comparison plot
create_method_comparison_plot_render <- function(analysis_data_reactive, input, method_names_reactive) {
  renderPlot({
    a <- analysis_data_reactive()
    m <- method_names_reactive()
    if (is.null(a)) {
      return(NULL)
    }
    
    # Validate data first
    validation <- validate_medical_data(a, "Method comparison analysis")
    if (!validation$valid) {
      show_validation_errors(validation$errors, "Method Comparison Plot Error")
      return(NULL)
    }
    
    # Validate input parameters
    param_validation <- validate_input_params(input, c("syx", "regmodel", "cimethod", "metbootci"))
    if (!param_validation$valid) {
      show_validation_errors(param_validation$errors, "Parameter Validation Error")
      return(NULL)
    }
    
    # Use safe mcreg wrapper with data frame
    result <- safe_mcreg(a, 
                        error.ratio = input$syx, 
                        method.reg = input$regmodel, 
                        method.ci = input$cimethod,
                        method.bootstrap.ci = input$metbootci, 
                        slope.measure = "radian", 
                        na.rm = TRUE,
                        context = "Method comparison regression")
    
    if (!result$success) {
      return(NULL)
    }
    
    # Generate plot safely
    safe_plot(function() {
      MCResult.plot(result$result, ci.area = input$ciarea,
                    add.legend = input$legend, identity = input$identity,
                    add.cor = input$addcor, x.lab = m$m1,
                    y.lab = m$m2, cor.method = input$cormet,
                    equal.axis = TRUE, add.grid = TRUE, 
                    na.rm = TRUE)
    }, a, "Method comparison plot generation")
  })
}

# Create render function for fit comparison plot
create_fit_comparison_plot_render <- function(analysis_data_reactive, input, method_names_reactive) {
  renderPlot({
    a <- analysis_data_reactive()
    m <- method_names_reactive()
    if (is.null(a)) {
      return(NULL)
    }
    
    # Validate data first
    validation <- validate_medical_data(a, "Fit comparison analysis")
    if (!validation$valid) {
      show_validation_errors(validation$errors, "Fit Comparison Plot Error")
      return(NULL)
    }
    
    # Validate input parameters
    param_validation <- validate_input_params(input, c("syx", "regmodel", "cimethod", "metbootci"))
    if (!param_validation$valid) {
      show_validation_errors(param_validation$errors, "Parameter Validation Error")
      return(NULL)
    }
    
    # Use safe mcreg wrapper with data frame
    result <- safe_mcreg(a, 
                        error.ratio = input$syx, 
                        method.reg = input$regmodel, 
                        method.ci = input$cimethod,
                        method.bootstrap.ci = input$metbootci, 
                        slope.measure = "radian",
                        mref.name = m$m1, mtest.name = m$m2, 
                        na.rm = TRUE,
                        context = "Fit comparison regression")
    
    if (!result$success) {
      return(NULL)
    }
    
    # Generate plot safely
    safe_plot(function() {
      compareFit(result$result)
    }, a, "Fit comparison plot generation")
  })
}

# Create render function for summary output
create_summary_render <- function(analysis_data_reactive, input, method_names_reactive) {
  renderPrint({
    a <- analysis_data_reactive()
    m <- method_names_reactive()
    if (is.null(a)) {
      return(NULL)
    }
    
    # Validate data first
    validation <- validate_medical_data(a, "Summary statistics")
    if (!validation$valid) {
      cat("Summary Error:\n")
      cat(paste("•", validation$errors, collapse = "\n"))
      return(invisible(NULL))
    }
    
    # Validate input parameters
    param_validation <- validate_input_params(input, c("syx", "regmodel", "cimethod", "metbootci"))
    if (!param_validation$valid) {
      cat("Parameter Error:\n")
      cat(paste("•", param_validation$errors, collapse = "\n"))
      return(invisible(NULL))
    }
    
    # Use safe mcreg wrapper with data frame
    result <- safe_mcreg(a, 
                        error.ratio = input$syx, 
                        method.reg = input$regmodel, 
                        method.ci = input$cimethod,
                        method.bootstrap.ci = input$metbootci, 
                        slope.measure = "radian",
                        mref.name = m$m1, mtest.name = m$m2, 
                        na.rm = TRUE,
                        context = "Summary statistics regression")
    
    if (!result$success) {
      cat("Analysis failed:", result$error)
      return(invisible(NULL))
    }
    
    # Generate summary safely
    tryCatch({
      printSummary(result$result)
    }, error = function(e) {
      cat("Summary generation failed:", e$message)
    })
  })
}
