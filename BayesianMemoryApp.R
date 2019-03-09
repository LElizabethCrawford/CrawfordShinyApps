
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Combining noisy memory with prior knowledge"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         numericInput("traceMu",
                     "Choose a stimulus value",
                     min = 0,
                     max = 10,
                     value = 7)
      ,
      numericInput("traceSd",
                   "How noisy is the trace memory? Set the standard deviation",
                   min = .1,
                   max = 5,
                   value = 1)
      ,
      numericInput("priorMu",
                   "Choose the mean of the prior",
                   min = 0,
                   max = 10,
                   value = 5)
      ,
      numericInput("priorSd",
                   "How spread out is the prior? Set the standard deviation",
                   min = 1,
                   max = 10,
                   value = 500)
      
   ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("stimPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #This function generates the posterior
   
   output$stimPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x <- seq(1,20,length=200)
      tracex <-dnorm(x, mean = input$traceMu, sd=input$traceSd)
      priorx<-dnorm(x, mean = input$priorMu, sd = input$priorSd)
      postMu<-((input$priorMu * 1/input$priorSd^2 +input$traceMu * 1/input$traceSd^2)/ (1/input$priorSd^2 + 1/input$traceSd^2))
      postTau <- (1/input$priorSd^2 + 1/input$traceSd^2)
      postSd <-1/sqrt(postTau)
      postx<-dnorm(x, mean=postMu,sd = postSd)
      df<-as.data.frame(x,tracex,priorx,postx)
 
      ggplot(df,(aes(x,tracex)))+ 
        geom_line(color = "red")+
        geom_line(aes(x,priorx), color = "blue")+
        geom_line(aes(x,postx), color = "purple")+
        xlim(0,20)+
        ylim(0,1)+
        geom_hline(yintercept = 0)+
        theme_minimal()+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),axis.text.y = element_blank(), axis.title.y=element_blank())
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

