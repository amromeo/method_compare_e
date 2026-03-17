# Statistics tab
tabItem_stats <- function(){
  tabItem(tabName = "stats",
          box(title = "Statistics", status = 'info', width = 12,
              verbatimTextOutput("summary")
          )
  )
}
