require(shiny)
require(ggplot2)
require(plyr)

# Define UI for application that draws a histogram
shinyUI <- fluidPage(
  # Application title
  titlePanel("Hello Kids!"),
  sidebarLayout(
    sidebarPanel(
      numericInput("numYellow", label = h3("# of Yellow M&Ms"), value = 1),
      numericInput("numBlue",   label = h3("# of Blue M&Ms"),   value = 1),
      numericInput("numBrown",  label = h3("# of Brown M&Ms"),  value = 1),
      numericInput("numGreen",  label = h3("# of Green M&Ms"),  value = 1),
      numericInput("numOrange", label = h3("# of Orange M&Ms"), value = 1),
      numericInput("numRed",    label = h3("# of Red M&Ms"),    value = 1),
      actionButton("goButton", "Go!", class = "btn-primary")
    ),
    # Show a plot of the generated distribution
    mainPanel(plotOutput("distPlot"))
  )
)


# Define server logic required to draw a histogram
shinyServer <- function(input, output){
  require(ggplot2)
  require(reshape2)
  d <- data.frame(Yellow=c(0),
                  Blue=c(0),
                  Brown=c(0),
                  Green=c(0),
                  Orange=c(0),
                  Red=c(0))
  write.csv(x = d, file="colordata.csv", row.names = FALSE)
  
  observeEvent(input$goButton, {
    d <- read.csv("colordata.csv", header=TRUE)
    d <- rbind(d,c(input$numYellow, input$numBlue, input$numBrown, input$numGreen, input$numOrange, input$numRed))
    write.csv(x = d, file="colordata.csv", row.names = FALSE)
  })
  
  output$distPlot <- renderPlot({
    input$goButton
    d <- read.csv("colordata.csv", header=TRUE)
    d <- melt(d)
    d <- rep(d$variable, d$value)
    # draw the histogram with the specified number of bins
    ggplot() + geom_bar(aes(x=d)) + 
      labs(x="M&M Color") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) + 
      theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
  })
  
}


shinyApp(ui = shinyUI, server = shinyServer)
