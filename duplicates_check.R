library(shiny)
library(dplyr)

ui <- fluidPage(
  titlePanel("Duplicate DOI Checker"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV", accept = ".csv"),
      hr(),
      uiOutput("progress_text"),
      hr(),
      actionButton("prev_btn", "← Previous"),
      actionButton("next_btn", "Next →"),
      actionButton("dup_btn", "Mark as Duplicate", class = "btn-warning"),
      hr(),
      tags$strong("Flagged as duplicates:"),
      verbatimTextOutput("flagged_list"),
      downloadButton("download_btn", "Download flagged DOIs")
    ),

    mainPanel(
      uiOutput("doi_o_header"),
      tableOutput("rows_table")
    )
  )
)

server <- function(input, output, session) {
  data      <- reactiveVal(NULL)
  doi_list  <- reactiveVal(character(0))
  current   <- reactiveVal(1L)
  flagged   <- reactiveVal(character(0))

  observeEvent(input$file, {
    df <- read.csv(input$file$datapath, fileEncoding = "UTF-8-BOM",
                   stringsAsFactors = FALSE, na.strings = c("NA", ""))
    required <- c("doi_o", "doi_r", "apa_ref_r")
    missing  <- setdiff(required, names(df))
    if (length(missing) > 0) {
      showNotification(paste("Missing columns:", paste(missing, collapse = ", ")),
                       type = "error")
      return()
    }
    data(df)
    doi_list(unique(df$doi_o))
    current(1L)
    flagged(character(0))
  })

  observeEvent(input$next_btn, {
    if (current() < length(doi_list())) current(current() + 1L)
  })

  observeEvent(input$prev_btn, {
    if (current() > 1L) current(current() - 1L)
  })

  observeEvent(input$dup_btn, {
    req(doi_list())
    doi <- doi_list()[current()]
    if (!doi %in% flagged()) {
      flagged(c(flagged(), doi))
      showNotification(paste("Flagged:", doi), type = "message")
    } else {
      showNotification("Already flagged.", type = "warning")
    }
  })

  output$progress_text <- renderUI({
    req(doi_list())
    doi <- doi_list()[current()]
    is_flagged <- doi %in% flagged()
    tags$p(
      paste0("Paper ", current(), " of ", length(doi_list())),
      if (is_flagged) tags$span(" ★ flagged", style = "color: orange; font-weight: bold;")
    )
  })

  output$doi_o_header <- renderUI({
    req(doi_list())
    tags$div(
      tags$h4("Original DOI"),
      tags$p(doi_list()[current()])
    )
  })

  output$rows_table <- renderTable({
    req(data(), doi_list())
    data() %>%
      filter(doi_o == doi_list()[current()]) %>%
      select(doi_r, apa_ref_r)
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$flagged_list <- renderText({
    if (length(flagged()) == 0) "None yet" else paste(flagged(), collapse = "\n")
  })

  output$download_btn <- downloadHandler(
    filename = "flagged_duplicates.csv",
    content  = function(file) {
      write.csv(data.frame(doi_o = flagged()), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
