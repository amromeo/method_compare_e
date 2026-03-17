# Download tab
tabItem_download <- function(){
  tabItem(tabName = "download",
          box(title = "Download Report", status = 'info',
              radioButtons('format', h5('Document format'),
                           c('PDF', 'HTML'),
                           inline = TRUE),
              downloadButton('downloadReport')
          ),
          box(title = "Email PDF Report", status = "primary",
              selectizeInput(
                "emailRecipients",
                "Recipients",
                choices = c(
                  "diogon@chop.edu",
                  "obstfelda@chop.edu",
                  "lambertm@chop.edu",
                  "nguyenlp@chop.edu"
                ),
                selected = character(0),
                multiple = TRUE,
                options = list(placeholder = "Select one or more recipients")
              ),
              textInput(
                "emailSubject",
                "Subject",
                value = "CHOP Coagulation Report"
              ),
              textAreaInput(
                "emailBody",
                "Message",
                value = "Please see attached PDF report.",
                rows = 3
              ),
              actionButton("sendReportEmail", "Send Email", class = "btn-primary")
          )
  )
}
