# Admin Configuration
# Configure admin access for different environments

# Development environment admin access
if (Sys.getenv("R_CONFIG_ACTIVE") %in% c("", "development")) {
  # Allow all users in development
  Sys.setenv("ENABLE_ADMIN_LOGS" = "true")
  message("Admin logging enabled for development environment")
}

# Additional fallback - enable admin logs if not in production
if (Sys.getenv("ENABLE_ADMIN_LOGS") != "false") {
  Sys.setenv("ENABLE_ADMIN_LOGS" = "true")
  message("Admin logs enabled via fallback")
}

# Production environment admin configuration
ADMIN_USERS <- c(
  # Add your specific admin users here - multiple formats to catch SSO variations
  "obstfelda@chop.edu",             # Full email
  "obstfelda",                       # Username only
  "Obstfelda",                       # Capitalized username
  "your.email@company.com",
  "admin.user@company.com",
  "support@company.com"
)

ADMIN_GROUPS <- c(
  "admin",
  "support", 
  "it-team"
)

ADMIN_DOMAINS <- c(
  "@admin.company.com",
  "@it.company.com"
)

# Function to check if user is admin (called from log_viewer.R)
is_user_admin <- function(user, user_groups = NULL) {
  
  # Development override
  if (Sys.getenv("ENABLE_ADMIN_LOGS") == "true") {
    return(TRUE)
  }
  
  # No user info
  if (is.null(user) || user == "") {
    return(FALSE)
  }
  
  # Check specific users
  if (user %in% ADMIN_USERS) {
    return(TRUE)
  }
  
  # Check group membership
  if (!is.null(user_groups) && any(ADMIN_GROUPS %in% user_groups)) {
    return(TRUE)
  }
  
  # Check domain
  for (domain in ADMIN_DOMAINS) {
    if (grepl(domain, user, ignore.case = TRUE, fixed = TRUE)) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}