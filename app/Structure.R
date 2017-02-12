library(shiny)

shinyApp(
ui = fluidPage(
  navbarPage("Our App Names",
              tabPanel("First tab name",
                       absolutePanel(top = 20,
                                     left = 20,
                                     width = 300,
                                     height = 600,
                                     draggable = TRUE,
                                     cursor = "move",
                                     wellPanel(
                                       sliderInput("n", "", min=3, max=20, value=5)
                                     )
                       )
                       ),
              tabPanel("Second tab name"),
              tabPanel("Thrid tab name",
                       absolutePanel(top = 20,
                                     left = 20,
                                     draggable = TRUE,
                                     wellPanel(
                                       sliderInput("n","",min = 5, max = 20,value=5)
                                              )
                                     )
                       
                       ),
              tabPanel("About us")
            )
  
                ),
server = function(input, output){}
)

