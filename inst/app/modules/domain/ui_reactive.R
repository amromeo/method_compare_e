# UI Reactive Module
# Contains dynamic UI renderers and input observers

# Create render function for limit value UI
create_limit_value_ui_render <- function(input, default_limits) {
  renderUI({
    if (is.null(input$testInput) || input$testInput == "") {
      return(helpText("Select Test to auto-populate required cutoff."))
    }
    
    init_limit <- if (input$testInput %in% names(default_limits)) {
      default_limits[[input$testInput]]
    } else {
      0.15  # fallback if "Other" or unknown
    }
    
    numericInput("limitValue", "Set % Cutoff * (auto from Test)", 
                 value = init_limit, 
                 min = 0, max = 1, step = 0.01)
  })
}

# Create render function for dynamic reagent lot display
create_dynamic_reagent_lot_render <- function(input) {
  renderUI({
    HTML(paste("<p><strong>Reagent Lot:</strong>", 
               ifelse(is.null(input$reagentLotInput), "Not entered", input$reagentLotInput), 
               "</p>"))
  })
}

# Create render function for dynamic expiration display
create_dynamic_expiration_render <- function(input) {
  renderUI({
    HTML(paste("<p><strong>Expiration:</strong>", 
               ifelse(is.null(input$expirationInput), "Not entered", input$expirationInput), 
               "</p>"))
  })
}

# Create render function for dynamic limit percentage display
create_dynamic_limit_per_render <- function(input, test_name) {
  renderUI({
    req(test_name(), input$limitValue)
    HTML(paste("<p><strong>Test:</strong>", test_name(), "</p>",
               "<p><strong>Limit (%):</strong>", percent(input$limitValue, accuracy = 0.1), "</p>"))
  })
}

# Create render function for dynamic date display
create_dynamic_date_render <- function(input) {
  renderUI({
    # Ensure input$dateInput is a Date object before formatting
    formattedDate <- if(!is.null(input$dateInput) && inherits(input$dateInput, "Date")) {
      format(input$dateInput, "%Y-%m-%d")
    } else {
      "Not entered"
    }
    HTML(paste("<p><strong>Date:</strong>", formattedDate, "</p>"))
  })
}
