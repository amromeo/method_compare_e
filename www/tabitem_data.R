tabItem_data <- function() { 
  tabItem(tabName = "data",
          # Consolidated box for all inputs at the top
          box(title = "Settings", status = "primary", solidHeader = TRUE, width = 12,
              selectInput("testInput", "Select Test", 
                          choices = get_test_choices()),
              conditionalPanel(
                condition = "input.testInput == 'Other'",
                textInput("customTestInput", "Enter Custom Test Name:")
              ),
              uiOutput("limitValueUI"),  # replaces static numericInput
              # Adding new inputs within the same box
              textInput('reagentLotInput', label = 'Reagent Lot:'),
              textInput('expirationInput', label = 'Expiration:'),
              dateInput('dateInput', label = 'Date:', value = Sys.Date()),
              radioButtons('comparisons', 'Comparison',
                           choices = c('old-new', 'mil1-mil2', 'mil1-mil3', 'mil2-mil3'),
                           selected = 'old-new',
                           inline = TRUE)
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
