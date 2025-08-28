# Admin UI Integration
# Add this to your existing UI structure

# Option 1: Add admin panel to an existing tab (recommended)
# Add this to the bottom of your "Information" or "Data" tab:

add_admin_to_existing_tab <- function() {
  tagList(
    hr(),
    create_admin_panel_ui()
  )
}

# Option 2: Create separate admin tab (if you want a dedicated tab)
tabItem_admin <- function() {
  tabItem(tabName = "admin",
    fluidRow(
      column(12,
        h2("🔧 System Administration"),
        p("Administrative tools and system monitoring for authorized users only."),
        
        create_admin_panel_ui(),
        
        # Additional admin-only information
        conditionalPanel(
          condition = "output.show_admin_panel",
          
          br(),
          
          wellPanel(
            h4("📖 Log Information"),
            p("Logs are automatically rotated daily. Files are stored in the 'logs/' directory with the following naming convention:"),
            tags$ul(
              tags$li(code("app_YYYY-MM-DD.log"), " - Application logs (INFO, WARNING, ERROR, DEBUG)"),
              tags$li(code("audit_YYYY-MM-DD.log"), " - User action audit trail")
            ),
            
            h4("🔒 Access Control"),
            p("Admin access is granted to users meeting any of these criteria:"),
            tags$ul(
              tags$li("Listed in admin users configuration"),
              tags$li("Member of 'admin' group in Posit Connect"),
              tags$li("Email domain matches admin domains"),
              tags$li("Development environment variable set")
            ),
            
            h4("⚠️ Security Notice"),
            div(class = "alert alert-warning",
              "Admin access allows viewing of all application logs including user data and system information. Access is logged for security audit purposes.")
          )
        )
      )
    )
  )
}

# Option 3: Add minimal admin indicator to sidebar
add_admin_sidebar_indicator <- function() {
  conditionalPanel(
    condition = "output.show_admin_panel",
    div(
      style = "margin: 10px; padding: 8px; background-color: #d9534f; color: white; text-align: center; border-radius: 4px; font-size: 12px;",
      "🔧 ADMIN MODE"
    )
  )
}