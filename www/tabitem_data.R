tabItem_data <- function() {
  tabItem(
    tabName = "data",
    # Consolidated box for all inputs at the top
    box(
      title = "Settings",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      fluidRow(
        column(
          4,
          selectInput(
            "testInput",
            "Select Test *",
            choices = c("-- Select Test --" = "", get_test_choices()),
            selected = ""
          )
        ),
        column(
          4,
          uiOutput("limitValueUI")
        ),
        column(
          4,
          radioButtons(
            "comparisons",
            "Comparison *",
            choices = c(
              "-- Select Comparison --" = "",
              "old-new" = "old-new",
              "mil1-mil2" = "mil1-mil2",
              "mil1-mil3" = "mil1-mil3",
              "mil2-mil3" = "mil2-mil3"
            ),
            selected = "",
            inline = TRUE
          )
        )
      ),
      conditionalPanel(
        condition = "input.testInput == 'Other'",
        textInput("customTestInput", "Enter Custom Test Name *")
      )
      ,
      fluidRow(
        column(4, textInput("reagentLotInput", label = "Reagent Lot *")),
        column(4, textInput("expirationInput", label = "Expiration *")),
        column(4, dateInput("dateInput", label = "Date *", value = NA))
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
