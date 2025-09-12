# Tab for Bland-Altman Plot
tabItem_subitem1 <- function(){
  # Load configuration
  source("modules/config_loader.R", local = TRUE)
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
                                       ),
                          selected = get_plot_default("bland_altman", "plot_type")
                          )
              )
  )
}
