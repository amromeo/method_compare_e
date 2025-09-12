# Tab for Scatter Plot
tabItem_subitem2 <- function(){
  # Load configuration
  source("modules/config_loader.R", local = TRUE)
  tabItem(tabName = "subitem2",
          box(title = "Scatter Plot", status='info', width = 8,
              plotOutput("plot2")
              ),
          box(title = "Options", status='info', width = 4,
              selectInput('regmodel', h5('Regression Model'),
                          choices=list('Ordinary Least Square' = 'LinReg',
                                       'Weighted Ordinary Least Square' = 'WLinReg',
                                       'Deming' = 'Deming',
                                       'Weighted Deming' = 'WDeming',
                                       'Passing-Bablok' = 'PaBa',
                                       'Passing-Bablok Large Dataset' = 'PaBaLarge'),
                        selected = get_plot_default("scatter_plot", "regression_model")
                          ),
              fixedRow(
                column(6,
                selectInput('cimethod', h5('CI Method'),
                            choices=list('Analytical' = 'analytical',
                                         'Jackknife' = 'jackknife',
                                         'Bootstrap' = 'bootstrap'
                                         ),
                            selected = get_plot_default("scatter_plot", "ci_method")
                            )
                       )
                ,
                column(6, selectInput('metbootci',h5('Bootstrap CI Method'),
                                      choices = list('BCa' = 'BCa',
                                                     'Quantile' = 'quantile'),
                                      selected = get_plot_default("scatter_plot", "bootstrap_ci_method")
                                      )
                       )
              ),
              fixedRow(
                column(6, selectInput('cormet',h5('Correlation Method'),
                                      choices = list('Pearson' = 'pearson',
                                                     'Spearman' = 'spearman',
                                                     'Kendall' = 'kendall'
                                      ),
                                      selected = get_plot_default("scatter_plot", "correlation_method")
                ))
                ),
                column(6, numericInput('syx', h5('Error Ratio'), 
                                       value = get_plot_default("scatter_plot", "error_ratio"))
                       )
              ),
              fixedRow(
                column(6,
                       checkboxInput('identity', 'Add identity line', 
                                     value = get_plot_default("scatter_plot", "add_identity_line")),
                       checkboxInput('ciarea', 'Add CI Area', 
                                     value = get_plot_default("scatter_plot", "add_ci_area"))
                       ),
                column(6,
                       checkboxInput('legend', 'Add Legend', 
                                     value = get_plot_default("scatter_plot", "add_legend")),
                       checkboxInput('addcor', 'Add Correlation', 
                                     value = get_plot_default("scatter_plot", "add_correlation"))
                       )
                )
              )
  )
}
