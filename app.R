library(shiny)
library(DT)
library(anonymizer)

ui <- fluidPage(
  fluidRow(
    column(3,
           wellPanel(
                fileInput('file', 'Choose file to upload',
                          accept = c(
                            'text/csv',
                            'text/comma-separated-values',
                            'text/tab-separated-values',
                            'text/plain',
                            '.csv',
                            '.tsv'
                          )
                ),
                # Taken from: http://shiny.rstudio.com/gallery/file-upload.html
                checkboxInput('header', 'Header', TRUE),
                radioButtons('sep', 'Separator',
                             c(Comma=',',
                               Semicolon=';',
                               Tab='\t'),
                             ','),
                radioButtons('quote', 'Quote',
                             c(None='',
                               'Double Quote'='"',
                               'Single Quote'="'"),
                             '"'),
                ################################################################
                
                actionButton("choice", "Parse file")
           ),
           wellPanel(
             h3("Anonymizer Settings"),
             selectInput("anon_algo", label = "Algorithm",
                         choices = c("md5", "sha1", "crc32", "sha256", "sha512",
                                          "xxhash32", "xxhash64", "murmur32"),
                         selected = "sha256"
             ),
             numericInput("anon_seed", label = "Set Seed", 0),
             numericInput("anon_nchars", label = "Number of characters to salt", 5)
             
           ),
           wellPanel(
             checkboxGroupInput("columns", "Select Columns", choices = NULL) # no choices before uploading 
           ),
           wellPanel(
             downloadButton('downloadData', 'Download Anonymized Data')
           )
  ),
  column(9,
        h3("Raw Data:"),
        DT::dataTableOutput("rawdata"),
        h3("Anonymized Data:"),
        DT::dataTableOutput("data2") 
  )
  )
  
)

server <- function(input, output, session) { # added session for updateSelectInput
  
  info <- eventReactive(input$choice, {
    inFile <- input$file
    req(inFile)
    
    # Changes in read.table 
    f <- read.table(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    vars <- names(f)
    # Update select input immediately after clicking on the action button. 
    updateCheckboxGroupInput(session, "columns","Select Columns", choices = vars)
    
    f
  })
  
output$rawdata <- DT::renderDataTable(
  DT::datatable(info(),
  options = list(searching = F, pageLength = 5)
  )
)
  
  anonymizeData <- reactive({
    pii_cols <- input$columns
    if(!is.null(pii_cols)) {
      pii <- info()
      for(i in 1:length(input$columns)) {
        pii[, input$columns[i]] <- anonymize(pii[, input$columns[i]], .algo = input$anon_algo, .seed = input$anon_seed, .n_chars = input$anon_nchars)
      } 
    } else pii <- info()
    return(pii)
  })
  
  output$data2 <- DT::renderDataTable(
    DT::datatable(anonymizeData(), 
    options = list(searching = F, pageLength = 5, columnDefs = list(list(
      targets = 1:ncol(anonymizeData()),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 6 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
        "}")
      ))), callback = JS('table.page(3).draw(false);'))
  )

  
  output$downloadData <- downloadHandler(
    filename = function() { paste(tools::file_path_sans_ext(input$file), '_anonymized.csv', sep='') },
    content = function(file) {
      write.csv(anonymizeData(), file, row.names = F)
    }
  )
  
  
}
shinyApp(ui, server)
