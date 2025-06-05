tabItem_data <- function() { 
  tabItem(tabName = "data",
          # Consolidated box for all inputs at the top
          box(title = "Settings", status = "primary", solidHeader = TRUE, width = 12,
              selectInput("limitPerInput", "Select Limit Percentage", 
                          choices = c("QFA Fibrinogen - 20%" = "QFA", 
                                      "D dimer - 24%" = "Ddimer", 
                                      "Factors - 15%" = "Factors",
                                      "Anti-Xa Parameter - 20%" = "AntiXa",
                                      "DTIBI Parameter - 20%" = "DTIBI",
                                      "Special Coag Testing - 15%" = "SpecialCoag")),
              # Adding new inputs within the same box
              textInput('testInput', label = 'Test:'),
              textInput('reagentLotInput', label = 'Reagent Lot:'),
              textInput('expirationInput', label = 'Expiration:'),
              dateInput('dateInput', label = 'Date:', value = Sys.Date()),
              checkboxGroupInput('comparisons', 'Comparisons',
                                 choices = c('old-new', 'mil1-mil2', 'mil1-mil3', 'mil2-mil3'))
          ),
          fluidRow(
            column(9,
                   box(width = 12, title = "Enter Data", status = 'info', solidHeader = TRUE,
                       fluidRow(
                         column(12, rHandsontableOutput("hot"))  # Table for data entry
                       )
                   )
            )
          )
  )
}
