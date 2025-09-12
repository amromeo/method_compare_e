# Validation Module
# Contains default limits, input validation functions, and test name resolution

# Load test limits from external configuration file
load_test_limits <- function(config_file = "config/test_limits.json") {
  if (file.exists(config_file)) {
    tryCatch({
      limits <- jsonlite::fromJSON(config_file)
      return(unlist(limits))
    }, error = function(e) {
      warning("Could not load config file, using default limits: ", e$message)
      return(get_default_limits())
    })
  } else {
    warning("Config file not found, using default limits")
    return(get_default_limits())
  }
}

# Define default limits (fallback)
get_default_limits <- function() {
  c(
    QFA = 0.20,
    Ddimer = 0.24,
    AntiXa = 0.20,
    DTIBI = 0.20,
    APTT = 0.15,
    PT = 0.15,
    "vWF Antigen" = 0.15,
    "vWF Activity" = 0.15,
    "Thrombin Time" = 0.20,
    "Protein C" = 0.15,
    "Protein S" = 0.15,
    "Protein S Free" = 0.15,
    "ADAMTS13 activity" = 0.20,
    sC5B9 = 0.20,
    "Factor II (Prothrombin)" = 0.15,
    "Factor V" = 0.15,
    "Factor VII" = 0.15,
    "Factor VIII" = 0.15,
    "Factor VIII Chromogenic" = 0.15,
    "Factor VIII Inhibitor" = 0.15,
    "Chromogenic Factor VIII Inhibitor" = 0.15,
    "Factor IX" = 0.15,
    "Factor IX Inhibitor" = 0.15,
    "Factor X" = 0.15,
    "Factor XI" = 0.15,
    "Factor XII" = 0.15,
    "Factor XIII" = 0.15
  )
}

# Get test choices from config file with 'Other' always included
get_test_choices <- function(config_file = "config/test_limits.json") {
  limits <- load_test_limits(config_file)
  test_names <- names(limits)
  c(test_names, "Other")
}

# Create observer for test input validation and limit updates
create_test_input_observer <- function(input, session, default_limits) {
  observeEvent(input$testInput, {
    req(input$testInput)
    if (input$testInput %in% names(default_limits)) {
      updateNumericInput(session, "limitValue", value = default_limits[[input$testInput]])
    }
  })
}

# Create reactive for test name resolution
create_test_name_reactive <- function(input) {
  reactive({
    if (input$testInput == "Other") {
      req(input$customTestInput)
      input$customTestInput
    } else {
      input$testInput
    }
  })
}