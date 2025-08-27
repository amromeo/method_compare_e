# Plot Generation Module
# Contains render functions for plots and summary output

# Create render function for Bland-Altman plot
create_bland_altman_plot_render <- function(vals, input) {
  renderPlot({
    a <- vals$final_data
    if (is.null(a)) {
      return(NULL)
    } else {
      names(a) <- c('Sample','M1', 'M2')
      data1 <- try(mcreg(a$M1, a$M2,
                     mref.name = input$m1, mtest.name = input$m2, 
                     na.rm = TRUE), silent = TRUE)
      try(MCResult.plotDifference(data1, plot.type = input$batype,
                              add.grid = TRUE), silent = TRUE)
    }
  })
}

# Create render function for method comparison plot
create_method_comparison_plot_render <- function(vals, input) {
  renderPlot({
    a <- vals$final_data
    if (is.null(a)) {
      return(NULL)
    } else {
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
}

# Create render function for fit comparison plot
create_fit_comparison_plot_render <- function(vals, input) {
  renderPlot({
    a <- vals$final_data
    if (is.null(a)) {
      return(NULL)
    } else {
      names(a) <- c('Sample','M1', 'M2')
      data1 <- try(mcreg(a$M1, a$M2, error.ratio = input$syx, 
                     method.reg = input$regmodel, method.ci = input$cimethod,
                     method.bootstrap.ci = input$metbootci, slope.measure = "radian",
                     mref.name = input$m1, mtest.name = input$m2, 
                     na.rm = TRUE), silent = TRUE)
      try(compareFit(data1), silent = TRUE)
    }
  })
}

# Create render function for summary output
create_summary_render <- function(vals, input) {
  renderPrint({
    a <- vals$final_data
    if (is.null(a)) {
      return(NULL)
    } else {
      names(a) <- c('Sample','M1', 'M2')
      data1 <- try(mcreg(a$M1, a$M2, error.ratio = input$syx, 
                     method.reg = input$regmodel, method.ci = input$cimethod,
                     method.bootstrap.ci = input$metbootci, slope.measure = "radian",
                     mref.name = input$m1, mtest.name = input$m2, 
                     na.rm = TRUE), silent = TRUE)
      try(printSummary(data1), silent = TRUE)
    }
  })
}