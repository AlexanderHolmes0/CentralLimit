library(shinythemes)
library(shinyWidgets)
library(shiny)
library(ggplot2)
library(plotly)
library(spsComps)

# Define UI for application that draws a histogram
ui <- fixedPage(
  theme = shinytheme("united"),
  # Application title
  titlePanel("Central Limit Theorem Tester"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("size",
        "Sample Size ( lil n)",
        min = 1,
        max = 5000,
        value = 500
      ),
      sliderInput("range",
        "Range in Sample (How wide?)",
        min = 1,
        max = 5000,
        value = c(250, 750)
      ),
      sliderInput("samps",
        "Number of Samples (How many subgroups?)",
        min = 2,
        max = 5000,
        value = 500
      ),
      materialSwitch(
        inputId = "rep",
        label = "Sample with Replacement",
        status = "primary",
        right = F,
        value = T
      )
    ),


    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("cltPlot", width = "100%"),
      setShadow(id = "cltPlot"),
      br(),
      verbatimTextOutput("label"),
      br(),
      verbatimTextOutput("summary"),
      br(),
      textOutput("barlabel"),
      progressBar(id = "pb3", value = 0, status = "danger", striped = T, display_pct = T),
      HTML('<iframe width="100%" height ="512px" src="https://www.youtube.com/embed/JNm3M9cqWyc"
                title="YouTube video player" frameborder="0" allow="accelerometer;
                autoplay; clipboard-write; encrypted-media; gyroscope;
                picture-in-picture; web-share" allowfullscreen></iframe>')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  averages <- reactive({
    averages <- c()
    for (i in 1:input$samps) {
      sample <- sample(input$range[1]:input$range[2], input$size, replace = input$rep)
      averages[i] <- mean(sample)
      updateProgressBar(id = "pb3", value = (i / input$samps) * 100)
    }
    averages
  })


  output$cltPlot <- renderPlotly({
    ggplotly(
      ggplot(data.frame(averages = scale(averages()))) +
        geom_histogram(aes(averages, y = after_stat(density)), binwidth = .5) +
        ylab("") +
        xlab("Scaled Sample Average") +
        theme_bw() +
        scale_y_continuous(breaks = NULL) +
        stat_function(fun = dnorm, color = "red")
    )
  })
  output$barlabel <- renderText({
    "Nifty Loading Bar"
  })

  output$summary <- renderPrint({
    summary(averages())
  })


  output$label <- renderPrint({
    "Summary of Values"
  })
}
# Run the application
shinyApp(ui = ui, server = server)
