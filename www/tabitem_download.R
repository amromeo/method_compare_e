# Download tab
tabItem_download <- function(){
  tabItem(tabName = "download",
          box(title = "Download Report", status = 'info',
              radioButtons('format', h5('Document format'),
                           c('PDF', 'HTML'),
                           inline = TRUE),
              downloadButton('downloadReport')
          )
  )
}
