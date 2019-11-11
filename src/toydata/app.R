library(shiny)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(6,
           plotOutput("plot1", click = "plot1_click")
    ),
    column(5,
           br(), br(), br(),
           htmlOutput("x_value"),
           verbatimTextOutput("selected_rows")
    ))
)

server <- function(input, output) {
  global <- reactiveValues(toHighlight = rep(FALSE, length(control.res.filter)), 
                           selectedBar = NULL)
  
  observeEvent(eventExpr = input$plot1_click, {
    global$selectedBar <- letters$word[round(input$plot1_click$x)]
    global$toHighlight <- letters$word %in% global$selectedBar
  })
  
  output$plot1 <- renderPlot({
    plotUpAndDown(control.res.filter)
  })
  
  # Print the name of the x value
  # output$x_value <- renderText({
  #   if (is.null(input$plot1_click$x)) return("")
  #   else {
  #     print(input$plot1_click$x)
  #     # lvls <- levels(ToothGrowth$supp)
  #     # name <- lvls[round(input$plot1_click$x)]
  #     # HTML("You've selected <code>", name, "</code>",
  #     #      "<br><br>Here are the first 10 rows that ",
  #     #      "match that category:")
  #   }
  # })
  
  # Print the rows of the data frame which match the x value
  # output$selected_rows <- renderPrint({
  #   if (is.null(input$plot1_click$x)) return()
  #   else {
  #     keeprows <- round(input$plot1_click$x) == as.numeric(ToothGrowth$supp)
  #     head(ToothGrowth[keeprows, ], 10)
  #   }
  # })
}

shinyApp(ui, server)