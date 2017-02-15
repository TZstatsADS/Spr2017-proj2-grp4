#Structure of our APP:
#Have not been able to dynamically change the opacity of the panel. 
library(shiny)
library(ggmap)
library(leaflet)
library(dplyr)

#college<-read.csv(file="D:/Columbia University/Spring2017-Applied Data Science/Project_2_Bz2290/Spr2017-proj2-grp4/data/College2014_15.csv", stringsAsFactors = FALSE,na.strings = "NULL")
#map<-as.data.frame(cbind(college$LONGITUDE, college$LATITUDE, college$HIGHDEG))
#colnames(map)<-c("lon", "lat", "degree")
#map$conm<-college$INSTNM
#map<-na.omit(map)
college = read.csv("C:/Users/sh355/Documents/GitHub/Spr2017-proj2-grp4/data/school.select.csv", header = TRUE, stringsAsFactors = FALSE)
major = c("Agriculture, Agriculture Operations, And Related Sciences","Natural Resources And Conservation", "Architecture And Related Services","Area, Ethnic, Cultural, Gender, And Group Studies"," Communication, Journalism, And Related Programs","Communications Technologies/Technicians And Support Services","Computer And Information Sciences And Support Services","Personal And Culinary Services"," Education","Engineering","Engineering Technologies And Engineering-Related Fields","Foreign Languages, Literatures, And Linguistics"," Family And Consumer Sciences/Human Sciences","Legal Professions And Studies","English Language And Literature/Letters","Liberal Arts And Sciences, General Studies And Humanities","Library Science"," Biological And Biomedical Sciences","Mathematics And Statistics","Military Technologies And Applied Sciences","Multi/Interdisciplinary Studies","Parks, Recreation, Leisure, And Fitness Studies","Philosophy And Religious Studies","Theology And Religious Vocations"," Physical Sciences"," Science Technologies/Technicians"," Psychology"," Homeland Security, Law Enforcement, Firefighting And Related Protective Services","Public Administration And Social Service Professions","Social Sciences","Construction Trades","Mechanic And Repair Technologies/Technicians","Precision Production","Transportation And Materials Moving","Visual And Performing Arts","Health Professions And Related Programs","Business, Management, Marketing, And Related Support Services","History")
major.index =c("PCIP01",
               "PCIP03",
               "PCIP04",
               "PCIP05",
               "PCIP09",
               "PCIP10",
               "PCIP11",
               "PCIP12",
               "PCIP13",
               "PCIP14",
               "PCIP15",
               "PCIP16",
               "PCIP19",
               "PCIP22",
               "PCIP23",
               "PCIP24",
               "PCIP25",
               "PCIP26",
               "PCIP27",
               "PCIP29",
               "PCIP30",
               "PCIP31",
               "PCIP38",
               "PCIP39",
               "PCIP40",
               "PCIP41",
               "PCIP42",
               "PCIP43",
               "PCIP44",
               "PCIP45",
               "PCIP46",
               "PCIP47",
               "PCIP48",
               "PCIP49",
               "PCIP50",
               "PCIP51",
               "PCIP52",
               "PCIP54")
shinyApp(
ui = fluidPage(
  navbarPage("Our App's Name", theme="styles.css",
              tabPanel("Locate Your School!",
                       sidebarLayout(
                         sidebarPanel(  
                       #absolutePanel(top = 50,
                        #             right = 20,
                         #            width = 300,       Remove these comments to initate moveable panel
                          #           height = 600,
                           #          draggable = TRUE,
                                     #cursor = "move",
                                     
                                       #sliderInput("stat","start Comparison",min=1,max=20,step=1,value =1)
                                       fluidRow(column(11,selectInput("major","Your Major",choices = c("NONE",major),selected = "None"))),
                                       fluidRow(column(3,numericInput("sat.reading","SAT Read",value=0,min=0,max=100)),
                                       column(3,numericInput("sat.math","SAT Math",value=0,min=0,max=100),offset = 1),
                                       column(3,numericInput("sat.writing","SAT Write",value=0,min=20,max=100),offset = 1)),
                                       fluidRow(column(11,numericInput("score.act","ACT Scores",value=15,min=0,max=36))),
                                       fluidRow(column(6,checkboxGroupInput("in","In or Out State?",choices = c("In state", "Out state"),selected = "In state"))),
                                       fluidRow(column(11,numericInput("max","Your Maximum acceptable Tution",min = 0, max = 51010, value = 0))),

                                       #radioButtons("cost","Preferred Cost of Attendence",choices=c("NONE","$2000-$2999","$3000-$3999"),selected = "NONE"),
                                       #checkboxGroupInput("stat","Start Comparison!",choices="Show stats!",selected = NULL),
                                       actionButton("search", "Start Searching!")
                                       #sliderInput("Alt","Altitude",min=40.5,max=45.04,step = 0.0001,value = 40.7484),
                                       #sliderInput("Long","Longitude",min=-80.52,max=-71.95,step = 0.0001,value=-73.9857)
                                               #Do not forget to add comma, if you want to initate moveable panel.
                            #         style = "opacity: 0.9"
                             #        )
                                      ),
                       mainPanel(
                       #conditionalPanel("input.stat =='Show stats!'",
                        # absolutePanel(top = 50,
                         #             width =300,
                          #             height = 10000,
                           #            draggable = TRUE,
                            #          wellPanel(
                             #           sliderInput("input","BLBLABLA",min =1 ,max =20 ,step =1, value =1) 
                              #                  ),
                               #        style = "opacity: 0.9"
                                #       )
                                #        ),
                                  
                       
                         uiOutput("map")
                               )
                       )
                       ),
              tabPanel("Comparision!",
                       fluidRow(
                         column(width = 4, textOutput("test.1"))
                               ),
                       hr(),
                       fluidRow(
                         column(width = 4, textOutput("test.2")),
                         column(width = 4, offset = 1, textOutput("test.3"))
                               )
                       ),
              tabPanel("Historical Data Overview"),
              tabPanel("Data Reference",
                       absolutePanel(top = 20,
                                     left = 20,
                                     height = 600,
                                     width = 300,
                                     draggable = TRUE,
                                     wellPanel(
                                       sliderInput("n","",min = 5, max = 20,value=5)
                                              )
                                     )
                       
                       ),
              tabPanel("About us",
                      textOutput("test.6")
                      )
            )
  
                ),
server = function(input, output){
  
  output$map=renderUI({
    leafletOutput('myMap', width = "300%", height = 700)
                      })
  
  school.selection = eventReactive(input$search,{
    college %>% filter(ACTCMMID <= input$score.act | ((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math))) %>% slice(1:2)
  })
  
  output$myMap = renderLeaflet({
    leaflet() %>%
      setView(lng = -73.9857, lat = 40.7484, zoom = 12) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(lng = school.selection()$LONGITUDE, lat = school.selection()$LATITUDE, popup = school.selection()$INSTNM)
                         })
  output$test.1 = renderPrint({
    "Our Graphs go to here...."
                          })
  
  output$test.2 = renderPrint({
    "Our Options go to here..."
                            })
  
  output$test.3 = renderPrint({
    "Our Options go to here too..."
                            })
  output$test.6 = renderText({
    "Our Team introduction, project introduction, and stuff..."
                            })
  
                                }
)


#Add Multiple marker on map
#map.1 = leaflet() %>%
 # setView(lng = -74, lat = 42, zoom = 6) %>%
  #addTiles() %>%
  #addCircleMarkers(lng = c(-74.121212121,-73.12313212312313), lat = c(42.1231212123,41.12345738453), popup = c("HAHA","haskjdhasd"))
