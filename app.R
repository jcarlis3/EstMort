# Shiny app

# Define UI for app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Estimate Mortality (M) given Number of Carcasses Found (X) and Detection Probability (g)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width=2,
                 
                 
                 # Input: Carcasses found ----
                 numericInput("X",
                              "Number of carcasses found (X)",
                              min = 0,
                              max = 1000,
                              value = NA),
                 
                 # Input: Detection probability ----
                 tags$hr(),
                 numericInput("g",
                              "Probability of detection (g)",
                              min = 0,
                              max = 1,
                              value = NA))
    ,
    
    # Main panel for displaying outputs ----
    mainPanel(width=4,
      
      # Output: histogram and detection function ----
      plotOutput("likePlot"),
      
      
    )
  )
)





# Define server logic ----
server <- function(input, output) {
  
  
  x <- reactive({
    0:(4*(input$X/input$g))
  })
  
  y <- reactive({
    sapply(x(), function(M)dbinom(x=input$X, size=M, prob=input$g))
  })
  
  
  # Figure output
  output$likePlot <- renderPlot({
    
    
    # Plot detection function
    plot(x(),
         y(),
         ylab="Likelihood",
         xlab="Mortality (M)",
         type="h",
         lwd=4,
         col="dodgerblue",
         main=paste0(input$X, " carcass(es) found\n", input$g, " prob. of detection"),
         cex.lab=1.3,
         cex.axis=1.3,
         cex.main=1.5
    )
  })
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)