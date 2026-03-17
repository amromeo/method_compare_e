# Additional placeholder tab
tabItem_subitem4 <- function(){
  tabItem(tabName = 'subitem4',
          box(title = '', status = 'info', width = 6,
              plotOutput('plot4')
              ),
          box(title = '', status = 'info', width = 6,
              plotOutput('plot5')
          )
  )
}
