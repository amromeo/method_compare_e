
dashboardPage(
  dashboardHeader(title = "CHOP Coagulation Lab Method Comparison Tool"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", 
               icon = icon("table", "fa-lg")
               ),
      menuItem("Excel View", tabName = "modData", icon = icon("dashboard")), # New tab for modified data
      menuItem("Plots", tabName = "plots",
               icon = icon("line-chart", "fa-lg"),
               menuSubItem("Scatter Plot", tabName = "subitem2"),
               menuSubItem("Coefficient Plot", tabName = "subitem3"),
               menuSubItem("Bland-Altman Plot", tabName = "subitem1")
               ),
      menuItem("Statistics", tabName = "stats",
               icon = icon("users", "fa-lg")
               ),
      menuItem("Download", tabName = "download",
               icon = icon("download", "fa-lg")
               ),
      menuItem("Information", tabName = "info", 
               icon = icon("info", "fa-lg")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem_info(),
      tabItem_data(),
      tabItem_moddata(),
      tabItem(tabName = "subitem1",
              box(title = "Bland-Altman Plot", status = 'info', width = 8,
                  plotOutput("plot1")
                  ),
              box(title = "Options", status='info', width = 4,
                  selectInput('batype', h5('Bland-Altman Plot Type'), 
                              choices=list('0.5*(X+Y) vs. Y-X' = 3,
                                           'X vs. Y-X' = 1,
                                           'rank(X) vs. Y-X' = 5,
                                           '0.5*(X+Y) vs. (Y-X)/X' = 4,                                       
                                           'X vs. (Y-X)/X' = 2,
                                           'rank(X) vs. (Y-X)/X' = 6,
                                           'sqrt(X*Y) vs. Y/X' = 7,
                                           '0.5*(X+Y) vs. (Y-X) / (0.5*(X+Y))' = 8
                                           )
                              )
                  )
      ),
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
                                                         'Quantile' = 'quantile')
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
      ),
      tabItem(tabName = 'subitem3',
            box(title = 'Coefficient Plot', status='info', width = 12, 
            plotOutput('plot3')
              )
            ),
      tabItem(tabName = 'subitem4',
            box(title = '', status = 'info', width = 6,
            plotOutput('plot4')  
              ),
            box(title = '', status = 'info', width = 6,
                plotOutput('plot5')  
            )            
            ),
      tabItem(tabName = "stats",
              box(title = "Statistics", status = 'info', width = 12,
              verbatimTextOutput("summary")
              )
      ),
      tabItem(tabName = "download",
              box(title = "Download Report", status = 'info',
              radioButtons('format', h5('Document format'), 
                           c('PDF', 'HTML'),
                           inline = TRUE),
              downloadButton('downloadReport')
              )
          )
      )
    )
  )
