
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
      tabItem_subitem1(),
      tabItem_subitem2(),
      tabItem_subitem3(),
      tabItem_subitem4(),
      tabItem_stats(),
      tabItem_download()
      )
    )
  )
