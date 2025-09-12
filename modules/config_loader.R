# Configuration Loader Module
# Loads plot defaults from JSON config file and maps user-friendly labels to internal codes

library(jsonlite)

# Mapping from user-friendly labels to internal codes used by the application
LABEL_TO_CODE_MAPPINGS <- list(
  regression_model = list(
    "Ordinary Least Square" = "LinReg",
    "Weighted Ordinary Least Square" = "WLinReg", 
    "Deming" = "Deming",
    "Weighted Deming" = "WDeming",
    "Passing-Bablok" = "PaBa",
    "Passing-Bablok Large Dataset" = "PaBaLarge"
  ),
  
  ci_method = list(
    "Analytical" = "analytical",
    "Jackknife" = "jackknife", 
    "Bootstrap" = "bootstrap"
  ),
  
  bootstrap_ci_method = list(
    "BCa" = "BCa",
    "Quantile" = "quantile"
  ),
  
  correlation_method = list(
    "Pearson" = "pearson",
    "Spearman" = "spearman",
    "Kendall" = "kendall"
  ),
  
  plot_type = list(
    "0.5*(X+Y) vs. Y-X" = 3,
    "X vs. Y-X" = 1,
    "rank(X) vs. Y-X" = 5,
    "0.5*(X+Y) vs. (Y-X)/X" = 4,
    "X vs. (Y-X)/X" = 2,
    "rank(X) vs. (Y-X)/X" = 6,
    "sqrt(X*Y) vs. Y/X" = 7,
    "0.5*(X+Y) vs. (Y-X) / (0.5*(X+Y))" = 8
  )
)

# Global variable to cache loaded config
PLOT_CONFIG <- NULL

# Load plot configuration from JSON file
load_plot_config <- function(config_file = "config/plot_defaults.json") {
  tryCatch({
    if (file.exists(config_file)) {
      config_data <- fromJSON(config_file)
      
      # Validate configuration
      validation_result <- validate_plot_config(config_data)
      if (!validation_result$valid) {
        warning(paste("Plot config validation errors:", paste(validation_result$errors, collapse = "; ")))
        return(get_fallback_config())
      }
      
      # Cache the config globally
      PLOT_CONFIG <<- config_data
      message("Plot configuration loaded successfully from ", config_file)
      return(config_data)
    } else {
      warning("Plot config file not found: ", config_file, ". Using fallback defaults.")
      return(get_fallback_config())
    }
  }, error = function(e) {
    warning("Error loading plot config: ", e$message, ". Using fallback defaults.")
    return(get_fallback_config())
  })
}

# Validate plot configuration structure and values
validate_plot_config <- function(config) {
  errors <- c()
  
  # Check required sections
  required_sections <- c("scatter_plot", "bland_altman")
  for (section in required_sections) {
    if (!section %in% names(config)) {
      errors <- c(errors, paste("Missing required section:", section))
    }
  }
  
  # Validate scatter plot defaults
  if ("scatter_plot" %in% names(config) && "defaults" %in% names(config$scatter_plot)) {
    scatter_defaults <- config$scatter_plot$defaults
    
    # Check each setting against available options and mappings
    if ("regression_model" %in% names(scatter_defaults)) {
      if (!scatter_defaults$regression_model %in% names(LABEL_TO_CODE_MAPPINGS$regression_model)) {
        errors <- c(errors, paste("Invalid regression_model:", scatter_defaults$regression_model))
      }
    }
    
    if ("ci_method" %in% names(scatter_defaults)) {
      if (!scatter_defaults$ci_method %in% names(LABEL_TO_CODE_MAPPINGS$ci_method)) {
        errors <- c(errors, paste("Invalid ci_method:", scatter_defaults$ci_method))
      }
    }
    
    if ("bootstrap_ci_method" %in% names(scatter_defaults)) {
      if (!scatter_defaults$bootstrap_ci_method %in% names(LABEL_TO_CODE_MAPPINGS$bootstrap_ci_method)) {
        errors <- c(errors, paste("Invalid bootstrap_ci_method:", scatter_defaults$bootstrap_ci_method))
      }
    }
    
    if ("correlation_method" %in% names(scatter_defaults)) {
      if (!scatter_defaults$correlation_method %in% names(LABEL_TO_CODE_MAPPINGS$correlation_method)) {
        errors <- c(errors, paste("Invalid correlation_method:", scatter_defaults$correlation_method))
      }
    }
  }
  
  # Validate bland-altman defaults
  if ("bland_altman" %in% names(config) && "defaults" %in% names(config$bland_altman)) {
    ba_defaults <- config$bland_altman$defaults
    
    if ("plot_type" %in% names(ba_defaults)) {
      if (!ba_defaults$plot_type %in% names(LABEL_TO_CODE_MAPPINGS$plot_type)) {
        errors <- c(errors, paste("Invalid bland_altman plot_type:", ba_defaults$plot_type))
      }
    }
  }
  
  return(list(valid = length(errors) == 0, errors = errors))
}

# Get fallback configuration if config file fails to load
get_fallback_config <- function() {
  list(
    scatter_plot = list(
      defaults = list(
        regression_model = "Passing-Bablok",
        ci_method = "Analytical", 
        bootstrap_ci_method = "BCa",
        correlation_method = "Pearson",
        error_ratio = 1,
        add_identity_line = TRUE,
        add_ci_area = TRUE,
        add_legend = TRUE,
        add_correlation = TRUE
      )
    ),
    bland_altman = list(
      defaults = list(
        plot_type = "0.5*(X+Y) vs. Y-X"
      )
    )
  )
}

# Convert user-friendly label to internal code
map_label_to_code <- function(setting_name, user_label) {
  if (setting_name %in% names(LABEL_TO_CODE_MAPPINGS)) {
    mapping <- LABEL_TO_CODE_MAPPINGS[[setting_name]]
    if (user_label %in% names(mapping)) {
      return(mapping[[user_label]])
    }
  }
  
  # If no mapping found, return the original value (for boolean/numeric settings)
  return(user_label)
}

# Get default value for a specific plot setting
get_plot_default <- function(plot_type, setting_name) {
  # Ensure config is loaded
  if (is.null(PLOT_CONFIG)) {
    PLOT_CONFIG <<- load_plot_config()
  }
  
  # Get the user-friendly default from config
  if (plot_type %in% names(PLOT_CONFIG) && "defaults" %in% names(PLOT_CONFIG[[plot_type]])) {
    user_default <- PLOT_CONFIG[[plot_type]]$defaults[[setting_name]]
    
    if (!is.null(user_default)) {
      # Convert to internal code if needed
      return(map_label_to_code(setting_name, user_default))
    }
  }
  
  # Fallback to hardcoded defaults if config lookup fails
  fallback_defaults <- list(
    scatter_plot = list(
      regression_model = "PaBa",
      ci_method = "analytical",
      bootstrap_ci_method = "BCa", 
      correlation_method = "pearson",
      error_ratio = 1,
      add_identity_line = TRUE,
      add_ci_area = TRUE,
      add_legend = TRUE,
      add_correlation = TRUE
    ),
    bland_altman = list(
      plot_type = 3
    )
  )
  
  if (plot_type %in% names(fallback_defaults)) {
    return(fallback_defaults[[plot_type]][[setting_name]])
  }
  
  return(NULL)
}