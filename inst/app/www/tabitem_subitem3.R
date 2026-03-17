# Tab for Coefficient Plot
tabItem_subitem3 <- function(){
  tabItem(tabName = 'subitem3',
          box(title = 'Coefficient Plot', status='info', width = 12,
              plotOutput('plot3')
          )
  )
}
