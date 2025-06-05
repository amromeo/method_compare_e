tabItem_moddata <- function() { 
  tabItem(tabName = "modData",
          column(12,
                 box(width = 12, title = "Excel Table View", status = 'info', 
                     fluidRow(
                       column(12, 
                              box(
                                title = "Test Information",
                                status = "info",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = 12,
                                htmlOutput("dynamicTest"),
                                htmlOutput("dynamicLimitPer") ,
                                htmlOutput("dynamicReagentLot"),
                                htmlOutput("dynamicExpiration"),
                                htmlOutput("dynamicDate")
                              )
                       ),
                       fluidRow(
                         column(6, htmlOutput("kableTable")), # kable table on the left
                         column(6) # Space for potential future modifications or additional content
                       )
                     )
                 )
          )
  )
}
