tabItem_data <- function() { 
  tabItem(tabName = "data",
          # Quick start guide
          box(title = "Start Here", status = "info", solidHeader = TRUE, width = 12,
              p("1) Enter or load data, 2) Set limits/models, 3) View plots & stats, 4) Download report."),
              div(
                class = "start-here-links",
                actionLink("jump_data", "Data", icon = icon("table")),
                " · ",
                actionLink("jump_plots", "Plots", icon = icon("line-chart")),
                " · ",
                actionLink("jump_stats", "Stats", icon = icon("calculator")),
                " · ",
                actionLink("jump_download", "Download", icon = icon("download"))
              )
          ),
          
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
            column(7,
                   box(width = 12, title = "Enter Data", status = 'info', solidHeader = TRUE,
                       div(class = "mb-2",
                           actionButton("load_sample", "Load Sample Dataset", icon = icon("magic"), class = "btn-sm btn-success"),
                           actionButton("reset_hot", "Reset Table", icon = icon("undo"), class = "btn-sm btn-default"),
                           span(class = "label label-primary", textOutput("valid_pairs_badge", inline = TRUE)),
                           span(class = "label label-info", textOutput("row_count_badge", inline = TRUE)),
                           span(class = "label label-warning", textOutput("limit_badge", inline = TRUE))
                       ),
                       rHandsontableOutput("hot"),
                       uiOutput("validation_alert")
                   )
            ),
            column(5,
                   box(width = 12, title = "Clean Data (read-only)", status = 'primary', solidHeader = TRUE,
                       DT::dataTableOutput("clean_table")
                   )
            )
          )
  )
}
