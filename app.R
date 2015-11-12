library(shiny)
data(mtcars)


ui <- fluidPage(
  title = 'Data generating process',
  
  
  fluidRow(
    column(4,
           dataTableOutput('tbl')
           ),
    column(8,
           plotOutput('dataplot'),
           textOutput("caption")
           )
    
    )
  
#  sidebarLayout(
#    sidebarPanel(textOutput('rows_out'),
#                 plotOutput('dataplot')),
#    mainPanel(),
#    position = 'right'
#  )
)

server <- function(input, output) {
  output$tbl <- renderDataTable(
    trees,
    options = list(pageLength = 10),
    callback = "function(table) {
    table.on('click.dt', 'tr', function() {
    $(this).toggleClass('selected').siblings().removeClass('selected');
    Shiny.onInputChange('rows',
    table.rows('.selected').indexes().toArray());
    });
}"
  )

  output$dataplot <- renderPlot({    
    par(mar=c(4,4,0,0)+.1, cex=1.4)
    plot(Volume ~ Height, data=trees, pch=20)
    if (!is.null(input$rows)) {
        points(trees$Height[input$rows+1], trees$Volume[input$rows+1], col="red", cex=1.6, pch=20)
    }    
  })


  output$rows_out <- renderText({
    paste(c('You selected these rows on the page:', input$rows, "UUU"),
          collapse = ' ')
  })
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
    formulaText <- reactive({
      paste("mpg ~", )
    })
  
  # Return the formula text for printing as a caption
    output$caption <- renderText({
      formulaText()
    })
  
  
  
  
}

shinyServer(server)


shinyApp(ui = ui, server = server)


