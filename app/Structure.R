#Structure of our APP:
library(shiny)

shinyApp(
ui = fluidPage(
  navbarPage("Our App's Name",
              tabPanel("Locate Your School!",
                       absolutePanel(top = 50,
                                     left = 20,
                                     width = 300,
                                     height = 600,
                                     draggable = TRUE,
                                     cursor = "move",
                                     wellPanel(
                                       #sliderInput("stat","start Comparison",min=1,max=20,step=1,value =1)
                                       selectInput("major","Your Major",choices = c("NULL","Major1","Major2"),selected = "NULL"),
                                       numericInput("score.sat","Your SAT Scores",value=0,min=20,max=100),
                                       numericInput("score.act","Your ACT Scores",value=0,min=0,max=36),
                                       radioButtons("cost","Preferred Cost of Attendence",choices=c("NULL","$2000-$2999","$3000-$3999"),selected = "NULL"),
                                       checkboxGroupInput("stat","Start Comparison!",choices="Show stats!",selected = NULL)
                                              ),
                                     style = "opacity: 0.9"
                                     ),
                       conditionalPanel("input.stat =='Show stats!'",
                         absolutePanel(top = 50,
                                       right = 20,
                                       width =300,
                                       height = 10000,
                                       draggable = TRUE,
                                       cursor = "move",
                                       wellPanel(
                                        sliderInput("input","BLBLABLA",min =1 ,max =20 ,step =1, value =1) 
                                                ),
                                       style = "opacity: 0.9"
                                       )
                                        )
                                  
                       
                                     
                       ),
              tabPanel("Comparision!"),
              tabPanel("Historical Data Overview"),
              tabPanel("Data Reference",
                       absolutePanel(top = 20,
                                     left = 20,
                                     height = 600,
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




