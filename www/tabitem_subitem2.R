# Tab for Scatter Plot
tabItem_subitem2 <- function(){
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
                        selected = "Deming"
                          ),
              fixedRow(
                column(6,
                selectInput('cimethod', h5('CI Method'),
                            choices=list('Analytical' = 'analytical',
                                         'Jackknife' = 'jackknife',
                                         'Bootstrap' = 'bootstrap'
                                         )
                            )
                       )
                ,
                column(6, selectInput('metbootci',h5('Bootstrap CI Method'),
                                      choices = list('BCa' = 'BCa',
                                                     'Quantile' = 'quantile'),
                                      )
                       )
              ),
              fixedRow(
                column(6, selectInput('cormet',h5('Correlation Method'),
                                      choices = list('Pearson' = 'pearson',
                                                     'Spearman' = 'spearman',
                                                     'Kendall' = 'kendall'
                                      )
                )
                ),
                column(6, numericInput('syx', h5('Error Ratio'), value = 1)
                       )
              ),
              fixedRow(
                column(6,
                       checkboxInput('identity', 'Add identity line', value = TRUE),
                       checkboxInput('ciarea', 'Add CI Area', value = TRUE)
                       ),
                column(6,
                       checkboxInput('legend', 'Add Legend', value = TRUE),
                       checkboxInput('addcor', 'Add Correlation',value = TRUE)
                       )
                )
              )
  )
}
