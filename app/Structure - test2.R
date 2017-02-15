#Structure of our APP:
#Have not been able to dynamically change the opacity of the panel. 
library(shiny)
library(ggmap)
library(leaflet)
college<-read.csv(file="../data/College2014_15.csv", stringsAsFactors = FALSE,na.strings = "NULL")
map<-as.data.frame(cbind(college$LONGITUDE, college$LATITUDE, college$HIGHDEG))
colnames(map)<-c("lon", "lat", "degree")
map$conm<-college$INSTNM
map<-na.omit(map)

cPal <- colorFactor(palette = c("blue","green", "yellow", "red", "black"),domain = map$degree)

shinyApp(
ui = fluidPage(
  navbarPage("Our App's Name",
              tabPanel("Locate Your School!",
                       sidebarLayout(
                         sidebarPanel(  
                       #absolutePanel(top = 50,
                        #             right = 20,
                         #            width = 300,       Remove these comments to initate moveable panel
                          #           height = 600,
                           #          draggable = TRUE,
                                     #cursor = "move",
                                     wellPanel(
                                       #sliderInput("stat","start Comparison",min=1,max=20,step=1,value =1)
                                       selectInput("major","Your Major",choices = c("NULL","Major1","Major2"),selected = "NULL"),
                                       numericInput("score.sat","Your SAT Scores",value=0,min=20,max=100),
                                       numericInput("score.act","Your ACT Scores",value=0,min=0,max=36),
                                       radioButtons("cost","Preferred Cost of Attendence",choices=c("NULL","$2000-$2999","$3000-$3999"),selected = "NULL"),
                                       checkboxGroupInput("stat","Start Comparison!",choices="Show stats!",selected = NULL),
                                       sliderInput("Alt","Altitude",min=40.5,max=45.04,step = 0.01,value = 41),
                                       sliderInput("Long","Longitude",min=-80.52,max=-71.95,step = 0.01,value=-73)
                                               )#Do not forget to add comma, if you want to initate moveable panel.
                            #         style = "opacity: 0.9"
                             #        )
                                      ),
                       mainPanel(
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
                                        ),
                                  
                       
                         leafletOutput("map")
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
server = function(input, output){
  map.plot.date = reactive({
    
    data.frame(alt = input$Alt,
               long = input$Long
              )
                          })
 
  output$map = renderLeaflet({
    leaflet() %>%
      setView(lng = -74, lat = 42, zoom = 6) %>%
      addTiles() %>%
      addCircleMarkers(lng = map.plot.date()$long, lat = map.plot.date()$alt, popup = c("Testing For Project"))%>%
    addCircleMarkers(lng = map$lon, lat = map$lat, fillColor=cPal(map$degree), stroke=FALSE, fillOpacity=0.8, popup=map$conm) %>%
      addLegend("bottomright", pal = cPal, values = map$degree,title = "Degree",opacity = 1)
    })
                                }
)


#Add Multiple marker on map
#map.1 = leaflet() %>%
 # setView(lng = -74, lat = 42, zoom = 6) %>%
  #addTiles() %>%
  #addCircleMarkers(lng = c(-74.121212121,-73.12313212312313), lat = c(42.1231212123,41.12345738453), popup = c("HAHA","haskjdhasd"))
