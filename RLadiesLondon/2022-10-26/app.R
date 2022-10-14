# Demo app from https://github.com/rstudio/shiny/pull/3006

library(shiny)

shinyApp(
  ui = fluidPage(
    actionButton("btn1", "Update plots"),
    plotOutput("plot1", width = "400px")
  ),
  server = function(input, output, session) {
    dt <- reactive({
      invalidateLater(3 * 1000)
      rnorm(30)
    })
    output$plot1 <- renderPlot({
        hist(dt())
      },
      alt = reactive({
        paste("Dynamic alt text. Mean(x):", round(mean(dt()), 3))
      })
    )
  }
)