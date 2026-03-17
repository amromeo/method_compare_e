generateKableTable <- function(df, format = "html") {
  # Adjust format validation to include "latex" for PDF output
  if (!format %in% c("html", "latex")) {
    stop("Format must be either 'html' or 'latex'")
  }
  
  # Check if the data frame is empty
  if (nrow(df) == 0) {
    return(knitr::kable(data.frame(Message = "No data available"), format = format))
  }
  
  # Round specified columns to two decimal places using tidy syntax
  # Round numeric columns (X, Y, AbsDiff) to two decimal places
  df <- df %>%
    mutate_if(is.numeric, round, 2)
  
  # Define the colors for HTML and LaTeX
  colors_html <- list(pass = "#28a745", fail = "#dc3545")  # HEX color codes for HTML
  colors_latex <- list(pass = "passGreen", fail = "failRed")  # Custom named colors for LaTeX
  
  if ("PassFail" %in% colnames(df)) {
    df$PassFail <- sapply(df$PassFail, function(x) {
      if (!is.na(x)) {
        if (x == "PASS") {
          # Select color based on format
          bg_color <- ifelse(format == "html", colors_html$pass, colors_latex$pass)
          cell_spec(x, format, background = bg_color, color = "white")
        } else if (x == "FAIL") {
          # Select color based on format
          bg_color <- ifelse(format == "html", colors_html$fail, colors_latex$fail)
          cell_spec(x, format, background = bg_color, color = "white")
        } else {
          x  # No formatting for other non-NA values
        }
      } else {
        ""  # Handling for NA values
      }
    }, USE.NAMES = FALSE)
  }
  
  # Apply gsub to escape percentages in LaTeX format
  if (format == "latex") {
    char_cols <- sapply(df, is.character)
    df[char_cols] <- lapply(df[char_cols], function(x) gsub("%", "\\%", x, fixed = TRUE))
  }
  
  # Generate the table with kable and apply additional styling
  tableOutput <- knitr::kable(df, format = format, escape = FALSE, booktabs = TRUE) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) %>%
    kableExtra::column_spec(1, bold = TRUE)
  
  return(tableOutput)
  
}
