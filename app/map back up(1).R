library(shiny)
library(ggmap)
library(leaflet)
library(dplyr)
#Current Problems:
#Multople Criteria Searcing failure.


#college = read.csv("C:/Users/sh355/Documents/GitHub/Spr2017-proj2-grp4/data/school.select.csv", header = TRUE, stringsAsFactors = FALSE)

college.1 = read.csv("D:/Columbia University/Spring2017-Applied Data Science/Project_2_Bz2290/Spr2017-proj2-grp4/data/school.select.csv",header = TRUE,stringsAsFactors = FALSE)

major = c("Agriculture, Agriculture Operations, And Related Sciences","Natural Resources And Conservation", "Architecture And Related Services","Area, Ethnic, Cultural, Gender, And Group Studies"," Communication, Journalism, And Related Programs","Communications Technologies/Technicians And Support Services","Computer And Information Sciences And Support Services","Personal And Culinary Services"," Education","Engineering","Engineering Technologies And Engineering-Related Fields","Foreign Languages, Literatures, And Linguistics"," Family And Consumer Sciences/Human Sciences","Legal Professions And Studies","English Language And Literature/Letters","Liberal Arts And Sciences, General Studies And Humanities","Library Science"," Biological And Biomedical Sciences","Mathematics And Statistics","Military Technologies And Applied Sciences","Multi/Interdisciplinary Studies","Parks, Recreation, Leisure, And Fitness Studies","Philosophy And Religious Studies","Theology And Religious Vocations"," Physical Sciences"," Science Technologies/Technicians"," Psychology"," Homeland Security, Law Enforcement, Firefighting And Related Protective Services","Public Administration And Social Service Professions","Social Sciences","Construction Trades","Mechanic And Repair Technologies/Technicians","Precision Production","Transportation And Materials Moving","Visual And Performing Arts","Health Professions And Related Programs","Business, Management, Marketing, And Related Support Services","History")
major.index =c("PCIP01","PCIP03","PCIP04","PCIP05","PCIP09","PCIP10","PCIP11","PCIP12","PCIP13","PCIP14","PCIP15","PCIP16","PCIP19","PCIP22","PCIP23","PCIP24","PCIP25","PCIP26","PCIP27","PCIP29","PCIP30","PCIP31","PCIP38","PCIP39","PCIP40","PCIP41","PCIP42","PCIP43","PCIP44","PCIP45","PCIP46","PCIP47","PCIP48","PCIP49","PCIP50","PCIP51","PCIP52","PCIP54")
major.frame = data.frame(major = major, index = major.index)

shinyApp(
ui = fluidPage(
  navbarPage("Our App's Name",
              tabPanel("Locate Your School!",
                       div(class="outer",
                           
                           tags$head(
                             # Include our custom CSS
                             includeCSS("styles.css")
                                                   ),
                           uiOutput("map"),
                           #Our Search Panel
                           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                         draggable = TRUE, top = 60, left = 20, bottom = "auto",
                                         width = 480, height = "auto", cursor = "move",
                                         fluidRow(wellPanel(
                                           fluidRow(column(10,selectInput("major","Your Major",choices = c("None",major),selected = "None"))),
                                           fluidRow(column(3,numericInput("sat.reading","SAT Read",value=0,min=0,max=800)),
                                                    column(3,numericInput("sat.math","SAT Math",value=0,min=0,max=800),offset = 1),
                                                    column(3,numericInput("sat.writing","SAT Write",value=0,min=20,max=800),offset = 1)),
                                           fluidRow(column(10,numericInput("score.act","ACT Cumulative Scores",value=0,min=0,max=36))))),
                                         fluidRow(
                                           wellPanel(
                                             fluidRow(
                                               column(width = 5,numericInput("max","Maximum Tution",min = 0, max = 999999, value = 0)),
                                               column(width = 5, offset = 1,radioButtons("location","State Resident?",choices = c("Yes", "No"),selected = "Yes", inline = TRUE))
                                             ))),
                                         fluidRow(
                                           wellPanel(
                                             fluidRow(selectInput("Focus","Area of Focus",choices = c("New York State","New York City","Western New York","Finger Lakes","Southern Tier","Central New York","North Country","Mohawk Valley","Capital District","Hudson Valley","Long Island"), selected = "New York Sate")),
                                             fluidRow(radioButtons("opt","Map options",choices=c("Regular","Satellite"),selected = "Regular",inline = TRUE)),
                                             fluidRow(radioButtons("output","Cluster by Options",choices=list("Degree","Length","Transfer Rate","Type"),selected = "Degree",inline=TRUE))
                                           )),
                                         actionButton("search", "Start Searching!")
                                    )
                               #Our Panel for the cluster graph
                               #uiOutput("map_1"),
                           # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            #                 draggable = TRUE, top = 60, right = -30, bottom = "auto",
                             #                width = 470, height = 400, cursor = "move",
                                              
                              #            leafletOutput('myMap_1', width = "95%", height = 450)   
                               #           ),
                           #absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            #             draggable = TRUE, top = 500, right = -30, bottom = "auto",
                             #            width = 470, height = 400, cursor = "move",
                                         
                            #             leafletOutput("myMap_2",width="95%",height=450)
                           #)

                                             
                       
                       )),
                      tabPanel("Comparision!"
                      ###########################################TEAM 2 IMPLEMENTATION STARTS##########################################################
                      
                      ############################################TEAM 2 IMPLEMENTATION ENDS############################################################
                       ),
              tabPanel("Data Exploror",
                       fluidPage(
                       tabsetPanel(
                       tabPanel("Filtering Data Set",
                                 dataTableOutput("mytable_1")),
                       tabPanel("General Data Set",
                                 dataTableOutput("mytable_2"))
                       
                                )
                       
                       )
                       
                       ),
              tabPanel("About us",
                      textOutput("test.6")
                      )
            )
  
                ),
server = function(input, output){
  
  #Define interactive coordinates
  mapping = reactive({
    
    if(input$Focus == "New York State")
      {
        list(X=-74.2179,Y = 43.2994, Z = 6)
      }
    else if(input$Focus == "New York City")
    {
      list(X=-73.989308,Y=40.741895,Z=12)
    }
    else if(input$Focus == "Western New York")
    {
      list(X=-78.8784,Y=42.8864, Z=8)
    }
    else if(input$Focus == "Finger Lakes")
    {
      list(X=-77.610924,Y=43.1610,Z=9)
    }
    else if(input$Focus == "Southern Tier")
    {
      list(X=-76.5019,Y=42.4440,Z=9)
    }
    else if(input$Focus == "Central New York")
    {
      list(X=-76.154480,Y=43.088947,Z=9)
    }
    else if(input$Focus == "North Country")
    {
      list(X=-74.1713,Y=43.4657,Z=9)
    }
    else if(input$Focus == "Mohawk Valley")
    {
      list(X=-74.8596,Y=43.0434,Z=9)
    }
    else if(input$Focus == "Capital District")
    {
      list(X=-73.756233,Y=42.652580,Z=9)
    }
    else if(input$Focus == "Hudson Valley")
    {
      list(X=-74.0104,Y=41.5034,Z=9)
    }
    else if(input$Focus == "Long Island")
    {
      list(X=-72.9933,Y=40.8858,Z=9)
    }
      
    
  })
  
  #Define interactive map change
  mapping.opt = reactive({
    
    if(input$opt=="Regular")
    {
      list(O=0.1)
    }
    else if(input$opt=="Satellite")
    {
      list(O=0.95)
    }
  })
  
  #Get interactive major index
  major.data.index = reactive({
    major.frame[which(major.frame$major == input$major),"index"]
  })
  
  
  major.data.frame.mean = reactive({
    mean(college.1[,major.data.index()])
  })
  
  major.data.frame.upper = reactive({
    summary(college.1[,major.data.index()])[5]
  })
  
  school.selection = eventReactive(input$search,{
    
    
    if(input$major != "None" & ((input$sat.reading == 0 & input$sat.writing == 0 & input$sat.math == 0)|(input$score.act == 0))  & input$max == 0)
    {
      college.1 %>% filter(college.1[,major.data.index()] >= major.data.frame.mean())
    }
    else if(input$major == "None" & ((input$sat.reading != 0 & input$sat.writing != 0 & input$sat.math != 0)|(input$score.act != 0))  & input$max == 0)
    {
      college.1 %>% filter((ACTCMMID <= input$score.act | ((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing))))
    }
    else if(input$major == "None" & ((input$sat.reading == 0 & input$sat.writing == 0 & input$sat.math == 0)|(input$score.act == 0))  & input$max != 0)
    {
      if(input$location == "Yes")
      {
        college.1 %>% filter(TUITIONFEE_IN <= input$max)
      }
      else if(input$location == "No")
      {
        college.1 %>% filter(TUITIONFEE_OUT <= input$max)
      }
    }
    else if(input$major != "None" & ((input$sat.reading != 0 & input$sat.writing != 0 & input$sat.math != 0)|(input$score.act != 0))  & input$max != 0)
    {
      if(input$location == "Yes")
      {
        college.1 %>% filter((ACTCMMID <= input$score.act | ((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing))) & TUITIONFEE_IN <= input$max & college.1[,major.data.index()] > major.data.frame.mean())
      }
      else if(input$location == "No")
      {
        college.1 %>% filter((ACTCMMID <= input$score.act | ((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing))) & TUITIONFEE_OUT <= input$max & college.1[,major.data.index()] > major.data.frame.mean())
      }
    }
    else if(input$major != "None" & ((input$sat.reading != 0 & input$sat.writing != 0 & input$sat.math != 0)|(input$score.act != 0))  & input$max == 0)
    {
      college.1 %>% filter((ACTCMMID <= input$score.act | ((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing)))  & college.1[,major.data.index()] > major.data.frame.mean())
    }
    else if(input$major != "None" & ((input$sat.reading == 0 & input$sat.writing == 0 & input$sat.math == 0)|(input$score.act == 0))  & input$max != 0)
    {
      if(input$location == "Yes")
      {
        college.1 %>% filter(TUITIONFEE_IN <= input$max & college.1[,major.data.index()] > major.data.frame.mean())
      }
      else if(input$location == "No")
      {
        college.1 %>% filter(TUITIONFEE_OUT <= input$max & college.1[,major.data.index()] > major.data.frame.mean())
      }
    }
    else if(input$major == "None" & ((input$sat.reading != 0 & input$sat.writing != 0 & input$sat.math != 0)|(input$score.act != 0))  & input$max != 0)
    {
      if(input$location == "Yes")
      {
        college.1 %>% filter((ACTCMMID <= input$score.act | ((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing))) & TUITIONFEE_IN <= input$max)
      }
      else if(input$location == "No")
      {
        college.1 %>% filter((ACTCMMID <= input$score.act | ((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing))) & TUITIONFEE_OUT <= input$max)
      }
    }
  })
  
  output$map=renderUI({
    leafletOutput('myMap', width = "100%", height = 700)
  })
 
  
  output$myMap = renderLeaflet({
    leaflet()%>%setView(lng = mapping()$X, lat = mapping()$Y, zoom = mapping()$Z)%>%addProviderTiles("Esri.WorldStreetMap")%>%addMarkers(lng = school.selection()$LONGITUDE, lat = school.selection()$LATITUDE, popup = paste0(school.selection()$INSTNM), icon=list(iconUrl='https://cdn0.iconfinder.com/data/icons/back-to-school/90/school-learn-study-hat-graduate_2-512.png',iconSize=c(25,25)))%>%addProviderTiles("Esri.WorldImagery",options = providerTileOptions(opacity = mapping.opt()$O))
      #"<br><strong>Average Score of SAT Reading: </strong>","<mark>",school.selection()$SATVRMID
      #"</mark>","<br><strong>Average Score of SAT Math: </strong>",school.selection()$SATMTMID
    })
  
  outputmap = reactive({
    if(input$output == "Degree")
    {
      list(info=school.selection()[,c("LONGITUDE","LATITUDE","HIGHDEG_1")], color = c("blue","green", "yellow", "orange", "red"))
      
    }
    else if(input$output == "Length")
    {
      list(info=school.selection()[,c("LONGITUDE","LATITUDE","twoorfour")],color = c("yellow","red"))
    }
    else if(input$output == "Transfer Rate")
    {
      list(info=school.selection()[,c("LONGITUDE","LATITUDE","loworhigh")],color = c("yellow","red"))
    }
    else if(input$output == "Type")
    {
      list(info=school.selection()[,c("LONGITUDE","LATITUDE","partorfull")],color = c("yellow","red"))
    }
    
  })
  
 # output$myMap_1 = renderLeaflet({
  #  leaflet()%>%setView(lng = mapping()$X, lat = mapping()$Y, zoom = mapping()$Z)%>%addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")%>%addCircleMarkers(lng = outputmap()[[1]][,1], lat = outputmap()[[1]][,2],clusterOptions = markerClusterOptions(),fillColor=colorFactor(palette = outputmap()[[2]],domain = outputmap()[[1]][,3])(outputmap()[[1]][,3]), stroke=FALSE, fillOpacity=0.8)%>%addLegend("bottomright", pal = colorFactor(palette = outputmap()[[2]],domain = outputmap()[[1]][,3]), values = outputmap()[[1]][,3],opacity = 1)
  #})
  
  #output$myMap_2 = renderLeaflet({
  #  leaflet()%>%setView(lng = mapping()$X, lat = mapping()$Y, zoom = mapping()$Z)%>%addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")%>%addCircleMarkers(lng = outputmap()[[1]][,1], lat = outputmap()[[1]][,2],clusterOptions = markerClusterOptions(),fillColor=colorFactor(palette = outputmap()[[2]],domain = outputmap()[[1]][,3])(outputmap()[[1]][,3]), stroke=FALSE, fillOpacity=0.8)%>%addLegend("bottomright", pal = colorFactor(palette = outputmap()[[2]],domain = outputmap()[[1]][,3]), values = outputmap()[[1]][,3],opacity = 1)
  #})
  
  ###########################################TEAM 2 IMPLEMENTATION STARTS#################################################################################
  
  ###########################################TEAM 2 IMPLEMENTATION ENDS###################################################################################
 
  
                                }
)


