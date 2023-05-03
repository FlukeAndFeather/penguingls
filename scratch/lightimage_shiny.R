library(shiny)
library(ggplot2)

# Define the data (see trying_sgat.R)
track_bas

# Define the UI
ui <- fluidPage(
  titlePanel("Daily light levels"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "zlim", "Light Range:",
        min = min(track_bas$Light),
        max = max(track_bas$Light),
        value = range(track_bas$Light),
        step = 0.1
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define the server
server <- function(input, output) {

  # Render the plot
  output$plot <- renderPlot({
    lightImage(track_bas, zlim = input$zlim)
  })

}

# Run the app
shinyApp(ui, server)
