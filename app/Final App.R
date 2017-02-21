library(shiny)
library(ggmap)
library(leaflet)
library(dplyr)


#college = read.csv("C:/Users/sh355/Documents/GitHub/Spr2017-proj2-grp4/data/school.select.csv", header = TRUE, stringsAsFactors = FALSE)

college.filtered = read.csv("../data/school.select.csv",header = TRUE,stringsAsFactors = FALSE)

major = c("Agriculture, Agriculture Operations, And Related Sciences","Natural Resources And Conservation", "Architecture And Related Services","Area, Ethnic, Cultural, Gender, And Group Studies"," Communication, Journalism, And Related Programs","Communications Technologies/Technicians And Support Services","Computer And Information Sciences And Support Services","Personal And Culinary Services"," Education","Engineering","Engineering Technologies And Engineering-Related Fields","Foreign Languages, Literatures, And Linguistics"," Family And Consumer Sciences/Human Sciences","Legal Professions And Studies","English Language And Literature/Letters","Liberal Arts And Sciences, General Studies And Humanities","Library Science"," Biological And Biomedical Sciences","Mathematics And Statistics","Military Technologies And Applied Sciences","Multi/Interdisciplinary Studies","Parks, Recreation, Leisure, And Fitness Studies","Philosophy And Religious Studies","Theology And Religious Vocations"," Physical Sciences"," Science Technologies/Technicians"," Psychology"," Homeland Security, Law Enforcement, Firefighting And Related Protective Services","Public Administration And Social Service Professions","Social Sciences","Construction Trades","Mechanic And Repair Technologies/Technicians","Precision Production","Transportation And Materials Moving","Visual And Performing Arts","Health Professions And Related Programs","Business, Management, Marketing, And Related Support Services","History")
major.index =c("PCIP01","PCIP03","PCIP04","PCIP05","PCIP09","PCIP10","PCIP11","PCIP12","PCIP13","PCIP14","PCIP15","PCIP16","PCIP19","PCIP22","PCIP23","PCIP24","PCIP25","PCIP26","PCIP27","PCIP29","PCIP30","PCIP31","PCIP38","PCIP39","PCIP40","PCIP41","PCIP42","PCIP43","PCIP44","PCIP45","PCIP46","PCIP47","PCIP48","PCIP49","PCIP50","PCIP51","PCIP52","PCIP54")
major.frame = data.frame(major = major, index = major.index)

shinyApp(
ui =  div(id="canvas",
          
          navbarPage(strong("Our App's name",style="color: blue;"), theme="style.css",
                     
                     tabPanel(strong(tags$i("Map")),
                              div(class="outer",  
                                  # lealfet map
                                  uiOutput("map"),
                                  
                                  # control panel
                                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                draggable = TRUE, top = 60, left = 10, bottom = "auto",
                                                width = 500, height = "auto", cursor = "move",
                                                fluidRow(wellPanel(
                                                  fluidRow(column(10,selectInput("major",tags$strong("Your Major"),choices = c("I don't konw...",major),selected = "I don't konw..."))),
                                                  fluidRow(column(3,numericInput("sat.reading",tags$strong("SAT Read"),value=800,min=0,max=800,step=10)),
                                                           column(3,numericInput("sat.math",tags$strong("SAT Math"),value=800,min=0,max=800,step=10),offset = 0),
                                                           column(3,numericInput("sat.writing",tags$strong("SAT Write"),value=800,min=20,max=800,step=10),offset = 0)),
                                                  fluidRow(column(3,numericInput("score.act",tags$strong("ACT"),value=36,min=0,max=36,step=1)),column(7,radioButtons("score.opt","I want to ignore...",choices = c("SAT","ACT","Both"),selected="Both",inline = TRUE))))),
                                                fluidRow(
                                                  wellPanel(
                                                    fluidRow(
                                                      column(width = 3,numericInput("max",tags$strong("Max Tution"),min = 0, max = 999999, value = 999999)),
                                                      column(width = 8, offset = 1,radioButtons("location",tags$strong("Tuition Options"),choices = list("State Resident", "Non-State Resident","Ignore Tuition"),selected = "Ignore Tuition", inline = FALSE))
                                                    ))),
                                                fluidRow(
                                                  wellPanel(
                                                    fluidRow(column(4,selectInput("Focus",tags$strong("Area of Focus"),choices = c("New York State","New York City","Western New York","Finger Lakes","Southern Tier","Central New York","North Country","Mohawk Valley","Capital District","Hudson Valley","Long Island"), selected = "New York Sate")),
                                                             column(6,radioButtons("opt",tags$strong("Map types"),choices=c("Regular","Satellite"),selected = "Regular",inline = TRUE))),
                                                    fluidRow(column(10,radioButtons("output",tags$strong("Cluster by Options"),choices=list("Degree","Length","Transfer Rate","Type"),selected = "Degree",inline=TRUE)))
                                                  )),
                                                actionButton("search", tags$strong("Start Searching!"))
                                  ),
                                  
                                  # output panel
                                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                draggable = TRUE, top = 60, right = -30, bottom = "auto",
                                                width = 470, height = 400, cursor = "move",
                                                
                                                leafletOutput('myMap_1', width = "95%", height = 450)   
                                  )
                                 
                                  #absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                   #             draggable = TRUE, top = 500, right = -30, bottom = "auto",
                                    #            width = 470, height = 400, cursor = "move",
                                                
                                     #           leafletOutput("myMap_2",width="95%",height=450)
                                  #)                                 
                              )
                     ,div(class="footer", "Applied Data Science Group 4")
                     ),
                    #Comparison 
                    tabPanel(strong(tags$i("Comparision!"))
                             ###########################################TEAM 2 IMPLEMENTATION STARTS##########################################################
                             
                             ############################################TEAM 2 IMPLEMENTATION ENDS############################################################
                    ),#Comparison ends here
                    #Data presentation
                    tabPanel(strong(tags$i("Data Exploror")),
                             fluidPage(
                               tabsetPanel(
                                 tabPanel(tags$strong("Filtering Data Set"),
                                          dataTableOutput("mytable_1")),
                                 tabPanel(tags$strong("General Data Set"),
                                          dataTableOutput("mytable_2"))
                                 
                               )
                               
                             ),div(class="footer", "Applied Data Science Group 4")
                             
                    ),#data presentation ends here
                    #Introduction
                    tabPanel(strong(tags$i("About us")),
                             textOutput("test.6")
                             ,div(class="footer", "Applied Data Science Group 4")
                             )#Introduciton ends here
                     
                     
          )#navarbarPage ends here
),#div ends here
                
server = function(input, output){#Sever function starts
  
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
    mean(college.filtered[,major.data.index()])
  })
  
  school.selection = eventReactive(input$search,{
    
    if(input$major == "I don't konw...")
    {
      if(input$score.opt == "SAT")
      {
        if(input$location == "State Resident")
        {
          college.filtered %>% filter((ACTCMMID <= input$score.act) & TUITIONFEE_IN <= input$max)
        }
        else if(input$location == "Non-State Resident")
        {
          college.filtered %>% filter((ACTCMMID <= input$score.act) & TUITIONFEE_OUT <= input$max)
          
        }
        else if(input$location == "Ignore Tuition")
        {
          college.filtered %>% filter((ACTCMMID <= input$score.act))
          
        }
      }
      else if(input$score.opt == "ACT")
      {
        if(input$location == "State Resident")
        {
          college.filtered %>% filter(((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing)) & TUITIONFEE_IN <= input$max)
        }
        else if(input$location == "Non-State Resident")
        {
          college.filtered %>% filter(((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing)) & TUITIONFEE_OUT <= input$max)
        }
        else if(input$location == "Ignore Tuition")
        {
          college.filtered %>% filter(((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing)))
          
        }
        
      }
      else if(input$score.opt == "Both")
      {
        if(input$location == "State Resident")
        {
          college.filtered %>% filter(TUITIONFEE_IN <= input$max)
          
        }
        else if(input$location == "Non-State Resident")
        {
          college.filtered %>% filter(TUITIONFEE_OUT <= input$max)
          
        }
        else if(input$location == "Ignore Tuition")
        {
          college.filtered %>% filter()
        }
      }
    }
   else if(input$major != "I don't konw...")
    {
      if(input$score.opt == "SAT")
      {
        if(input$location == "State Resident")
        {
          college.filtered %>% filter((ACTCMMID <= input$score.act) & TUITIONFEE_IN <= input$max & college.filtered[,major.data.index()] > major.data.frame.mean())
        }
        else if(input$location == "Non-State Resident")
        {
          college.filtered %>% filter((ACTCMMID <= input$score.act) & TUITIONFEE_OUT <= input$max & college.filtered[,major.data.index()] > major.data.frame.mean())
          
        }
        else if(input$location == "Ignore Tuition")
        {
          college.filtered %>% filter((ACTCMMID <= input$score.act) & college.filtered[,major.data.index()] > major.data.frame.mean())
          
        }
      }
      else if(input$score.opt == "ACT")
      {
        if(input$location == "State Resident")
        {
          college.filtered %>% filter(((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing)) & TUITIONFEE_IN <= input$max & college.filtered[,major.data.index()] > major.data.frame.mean())
        }
        else if(input$location == "Non-State Resident")
        {
          college.filtered %>% filter(((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing)) & TUITIONFEE_OUT <= input$max & college.filtered[,major.data.index()] > major.data.frame.mean())
        }
        else if(input$location == "Ignore Tuition")
        {
          college.filtered %>% filter(((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing))  & college.filtered[,major.data.index()] > major.data.frame.mean())
          
        }
      }
      else if(input$score.opt == "Both")
      {
        if(input$location == "State Resident")
        {
          college.filtered %>% filter(TUITIONFEE_IN <= input$max & college.filtered[,major.data.index()] > major.data.frame.mean())
          
        }
        else if(input$location == "Non-State Resident")
        {
          college.filtered %>% filter(TUITIONFEE_OUT <= input$max & college.filtered[,major.data.index()] > major.data.frame.mean())
          
        }
        else if(input$location == "Ignore Tuition")
        {
          college.filtered %>% filter(college.filtered[,major.data.index()] > major.data.frame.mean())
          
        }
      }
    }
    
   
  })
  
  output$map=renderUI({
    leafletOutput('myMap', width = "100%", height = 700)
  })
 
  
  output$myMap = renderLeaflet({
    leaflet()%>%
      setView(lng = mapping()$X, lat = mapping()$Y, zoom = mapping()$Z)%>%
      addProviderTiles("Esri.WorldStreetMap")%>%
      addMarkers(lng = school.selection()$LONGITUDE, lat = school.selection()$LATITUDE, popup = paste(school.selection()$INSTNM,school.selection()$INSTURL), icon=list(iconUrl='https://cdn0.iconfinder.com/data/icons/back-to-school/90/school-learn-study-hat-graduate_2-512.png',iconSize=c(25,25)))%>%
      addProviderTiles("Esri.WorldImagery",options = providerTileOptions(opacity = mapping.opt()$O))
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
  
  output$myMap_1 = renderLeaflet({
    leaflet()%>%setView(lng = mapping()$X, lat = mapping()$Y, zoom = mapping()$Z)%>%addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")%>%addCircleMarkers(lng = outputmap()[[1]][,1], lat = outputmap()[[1]][,2],clusterOptions = markerClusterOptions(),fillColor=colorFactor(palette = outputmap()[[2]],domain = outputmap()[[1]][,3])(outputmap()[[1]][,3]), stroke=FALSE, fillOpacity=0.8)%>%addLegend("bottomright", pal = colorFactor(palette = outputmap()[[2]],domain = outputmap()[[1]][,3]), values = outputmap()[[1]][,3],opacity = 1)
  })
  
  output$myMap_2 = renderLeaflet({
    leaflet()%>%setView(lng = mapping()$X, lat = mapping()$Y, zoom = mapping()$Z)%>%addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")%>%addCircleMarkers(lng = outputmap()[[1]][,1], lat = outputmap()[[1]][,2],clusterOptions = markerClusterOptions(),fillColor=colorFactor(palette = outputmap()[[2]],domain = outputmap()[[1]][,3])(outputmap()[[1]][,3]), stroke=FALSE, fillOpacity=0.8)%>%addLegend("bottomright", pal = colorFactor(palette = outputmap()[[2]],domain = outputmap()[[1]][,3]), values = outputmap()[[1]][,3],opacity = 1)
  })
  
  ###########################################TEAM 2 IMPLEMENTATION STARTS#################################################################################
  
  ###########################################TEAM 2 IMPLEMENTATION ENDS###################################################################################
 
  
}#Sever function ends
)#Shiny App ends


