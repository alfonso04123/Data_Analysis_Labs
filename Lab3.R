library(shiny)


# Define UI for confidence interval calculator
ui <- fluidPage(
  titlePanel("Confidence Interval Calculator"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("confidenceLevel", "Confidence Level (%)",
                  min = 0, max = 99, value = 95, step = 1),
      radioButtons("shadeArea", "Show Shaded Area",
                   choices = c("Yes" = "yes", "No" = "no"),
                   selected = "yes")
    ),
    mainPanel(
      plotOutput("confidencePlot"),
      textOutput("intervalOutput")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Function to calculate confidence interval
  calculateCI <- reactive({
    z <- qnorm((1 + input$confidenceLevel / 100) / 2)
    mean <- 0  # Placeholder for example, can be made dynamic
    stddev <- 1  # Placeholder, adjust as needed
    error <- z * stddev
    c(mean - error, mean + error)
  })
  
  # Output confidence interval as text
  output$intervalOutput <- renderText({
    interval <- calculateCI()
    paste("Confidence Interval:", round(interval[1], 2), "to", round(interval[2], 2))
  })
  
  # Render plot with or without shaded area
  output$confidencePlot <- renderPlot({
    interval <- calculateCI()
    x <- seq(-4, 4, length.out = 100)
    y <- dnorm(x)
    
    plot(x, y, type = "l", lwd = 2, ylab = "Density", xlab = "Value",
         main = paste(input$confidenceLevel, "% Confidence Interval"))
    
    if (input$shadeArea == "yes") {
      polygon(c(interval[1], x[x >= interval[1] & x <= interval[2]], interval[2]),
              c(0, y[x >= interval[1] & x <= interval[2]], 0),
              col = "lightblue", border = NA)
    }
    
    abline(v = interval, col = "red", lwd = 2, lty = 2)
  })
}

shinyApp(ui = ui, server = server)
