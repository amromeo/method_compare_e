tabItem_data <- function() { 
  tabItem(tabName = "data",
          # Consolidated box for all inputs at the top
          box(title = "Settings", status = "primary", solidHeader = TRUE, width = 12,
              selectInput("testInput", "Select Test", 
                          choices = c("QFA", "Ddimer", "AntiXa", "DTIBI", "APTT", "PT","Thrombin Time", "Protein C", "Protein S", "ADAMTS13 activity","sC5B9",
                                          "Factor II (Prothrombin)", "Factor V", "Factor VII", "Factor VIII",
                                          "Factor VIII Chromogenic", "Factor VIII Inhibitor", "Chromogenic Factor VIII Inhibitor",
                                          "Factor IX", "Factor IX Inhibitor", "Factor X", "Factor XI", "Factor XII", "Factor XIII",
                                          "Other")),
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
