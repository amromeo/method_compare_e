tabItem_data <- function() {
  tabItem(
    tabName = "data",
    # Consolidated box for all inputs at the top
    box(
      title = "Settings",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      selectInput("testInput", "Select Test", choices = get_test_choices()),
      conditionalPanel(
        condition = "input.testInput == 'Other'",
        textInput("customTestInput", "Enter Custom Test Name:")
      ),
      uiOutput("limitValueUI"),
      textInput("reagentLotInput", label = "Reagent Lot:"),
      textInput("expirationInput", label = "Expiration:"),
      dateInput("dateInput", label = "Date:", value = Sys.Date()),
      radioButtons(
        "comparisons",
        "Comparison",
        choices = c("old-new", "mil1-mil2", "mil1-mil3", "mil2-mil3"),
        selected = "old-new",
        inline = TRUE
      )
    ),
    fluidRow(
      column(
        12,
        box(
          width = 12,
          title = "Enter Data",
          status = "info",
          solidHeader = TRUE,
          div(
            class = "mb-2",
            actionButton("load_sample", "Load Sample Dataset", icon = icon("magic"), class = "btn-sm btn-success"),
            actionButton("reset_hot", "Reset Table", icon = icon("undo"), class = "btn-sm btn-default"),
            span(class = "label label-primary", textOutput("valid_pairs_badge", inline = TRUE)),
            span(class = "label label-info", textOutput("row_count_badge", inline = TRUE)),
            span(class = "label label-warning", textOutput("limit_badge", inline = TRUE))
          ),
          rHandsontableOutput("hot"),
          uiOutput("validation_alert")
        )
      )
    )
  )
}
