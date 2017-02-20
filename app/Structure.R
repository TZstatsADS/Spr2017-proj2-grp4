library(shiny)
library(ggmap)
library(leaflet)
library(dplyr)
#install.packages("plotly")
library(plotly)
#install.packages("extrafont")

library(extrafont)
library(grDevices)


#college = read.csv("C:/Users/sh355/Documents/GitHub/Spr2017-proj2-grp4/data/school.select.csv", header = TRUE, stringsAsFactors = FALSE)

college.1 = read.csv("D:/Columbia University/Spring2017-Applied Data Science/Project_2_Bz2290/Spr2017-proj2-grp4/data/school.select.csv",header = TRUE)

college<-read.csv("D:/Columbia University/Spring2017-Applied Data Science/Project_2_Bz2290/Spr2017-proj2-grp4/data/College2014_15_new.csv",stringsAsFactors = F,na.strings = "NULL")

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
                                         width = 410, height = "auto", cursor = "move",
                                         fluidRow(wellPanel(
                                           fluidRow(column(10,selectInput("major","Your Major",choices = c("None",major),selected = "None"))),
                                           fluidRow(column(3,numericInput("sat.reading","SAT Read",value=0,min=0,max=800)),
                                                    column(3,numericInput("sat.math","SAT Math",value=0,min=0,max=800),offset = 1),
                                                    column(3,numericInput("sat.writing","SAT Write",value=0,min=20,max=800),offset = 1)),
                                           fluidRow(column(10,numericInput("score.act","ACT Cumulative Scores",value=0,min=0,max=36))))),
                                         fluidRow(
                                           wellPanel(
                                             fluidRow(
                                               column(width = 5,numericInput("max","Maximum Tution",min = 0, max = 51010, value = 0)),
                                               column(width = 5, offset = 1,radioButtons("location","State Resident?",choices = c("Yes", "No"),selected = "Yes", inline = TRUE))
                                             ))),
                                         fluidRow(
                                           wellPanel(
                                             fluidRow(selectInput("Focus","Area of Focus",choices = c("New York State","New York City","Western New York","Finger Lakes","Southern Tier","Central New York","North Country","Mohawk Valley","Capital District","Hudson Valley","Long Island"), selected = "New York Sate")),
                                             fluidRow(radioButtons("opt","Map options",choices=c("Regular","Satellite"),selected = "Regular",inline = TRUE))
                                           )),
                                         actionButton("search", "Start Searching!")
                                    ),
                               #Our Panel for the cluster graph
                               absolutePanel(id = "control", class = "panel panel-default", fixed = TRUE,
                                             draggable = TRUE, top = 60, right = 20, bottom = "auto",
                                             width = 500, height = "auto", cursor = "move",
                                             fluidPage( 
                                               uiOutput("map_1"),
                                               fluidRow(column(width = 5,radioButtons("output","",choices=c("Degree","Two Year vs Four Year","Transfer rate","Full vs Part Time")),selected = "Degree",inline=TRUE))
                                               )

                                             )
                       )),
                      tabPanel("Comparision!",
                       
                               
                               tags$hr(style="border-color: #6088B1;"),
                               h1("Side-by-Side Two School Comparison",align= "center",style = "color: #333333; font-family: Times; 
                                  font-size:50pt"),
                               tags$hr(style="border-color: #6088B1;"),
                               
                               fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                                   selectInput("input1",label="Select a School",choices=college$INSTNM,size=10,selectize=F,width="90%"),
                                                                   selectInput("input2",label="Select a School",choices=college$INSTNM,size=10,selectize=F,width="90%")
                               )),
                               br(),br(),
                               fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                                   imageOutput("logo1"),imageOutput("logo2")
                               )),
                               
                               #fluidRow(align="center",
                               #         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                               #         column(6,offset=3,
                               #                br(),
                               #                helpText( strong("help_test" , style="color:Orange ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                               #               hr()
                               #        )),
                               
                               # === Some text to explain the Figure:
                               #fluidRow(align="justify",
                               #         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                               #         column(6,offset=3,
                               #                br(),"explain_test",br()
                               #         )
                               #),
                               br(),
                               
                               
                               
                               # ==== Title in Orange
                               fluidRow(align="center",
                                        style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                                        column(6,offset=3,
                                               br(),hr(style="color:#808080"),
                                               helpText( strong("Basic Information" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                               hr(style="color:#808080")
                                        )),
                               
                               # === Some text to explain the Figure:
                               #fluidRow(align="justify",
                               #         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                               #         column(6,offset=3,
                               #                br(),"explain_test2",br(),br(),br(),br()
                               #         )
                               #),
                               br(),
                               # === display instnm
                               fluidRow(align = "center",splitLayout(cellWidths = c("50%","50%"),
                                                                     fluidRow(strong(column(width=2,offset=1,"Institution Name: ")),textOutput("instnm1")),
                                                                     fluidRow(strong(column(width=2,offset=1,"Institution Name: ")),textOutput("instnm2")))
                               ),br(),
                               # === display city
                               fluidRow(align = "justify",splitLayout(cellWidths = c("50%","50%"),
                                                                      fluidRow(strong(column(width=2,offset=1,"City: ")),textOutput("city1")),
                                                                      fluidRow(strong(column(width=2,offset=1,"City: ")),textOutput("city2")))
                               ),br(),
                               
                               # === display clevel
                               fluidRow(align = "justify",splitLayout(cellWidths = c("50%","50%"),
                                                                      fluidRow(strong(column(width=4,offset=1,"Level of Institution: ")),textOutput("iclevel1")),
                                                                      fluidRow(strong(column(width=4,offset=1,"Level of Institution: ")),textOutput("iclevel2")))
                               ),br(),
                               # === display control
                               
                               fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                                    fluidRow(strong(column(width=4,offset=1,"Control of Institution: ")),textOutput("control1")),
                                                                    fluidRow(strong(column(width=4,offset=1,"Control of Institution: ")),textOutput("control2")))
                               ),br(),
                               # === display highest degree
                               
                               fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                                    fluidRow(strong(column(width=3,offset=1,"Highest Degree: ")),textOutput("highdeg1")),
                                                                    fluidRow(strong(column(width=3,offset=1,"Highest Degree: ")),textOutput("highdeg2")))
                               ),br(),
                               # === display locale
                               
                               fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                                    fluidRow(strong(column(width=2,offset=1,"Locale: ")),textOutput("locale1")),
                                                                    fluidRow(strong(column(width=2,offset=1,"Locale: ")),textOutput("locale2")))
                               ),br(),
                               # === display admission rate
                               
                               fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                                    fluidRow(strong(column(width=4,offset=1,"Admission Rate: ")),textOutput("adm_rate1")),
                                                                    fluidRow(strong(column(width=4,offset=1,"Admission Rate: ")),textOutput("adm_rate2")))
                               ),br(),
                               # === display in-state tuition
                               
                               fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"), 
                                                                    fluidRow(strong(column(width=4,offset=1,"In-State Tuition: ")),textOutput("tuitionfee_in1")),
                                                                    fluidRow(strong(column(width=4,offset=1,"In-State Tuition: ")),textOutput("tuitionfee_in2")))
                               ),br(),
                               # === display out-of-state tuition
                               
                               fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"), 
                                                                    fluidRow(strong(column(width=4,offset=1,"Out-of-State Tuition: ")),textOutput("tuitionfee_out1")),
                                                                    fluidRow(strong(column(width=4,offset=1,"Out-of-State Tuition: ")),textOutput("tuitionfee_out2")))
                               ),br(),
                               # === display percentage of federal loans
                               
                               fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"), 
                                                                   fluidRow(strong(column(width=6,offset=1,"Percentage of Students Receiving Federal Loans: ")),
                                                                            textOutput("pctfloan1")),
                                                                   fluidRow(strong(column(width=6,offset=1,"Percentage of Students Receiving Federal Loans: ")),
                                                                            textOutput("pctfloan2")))
                               ),br(),
                               # === display total Undergraduates Seeking Degrees
                               
                               fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"), 
                                                                   fluidRow(strong(column(width=5,offset=1,"Total Undergraduates Seeking Degrees: ")),
                                                                            textOutput("ugds1")),
                                                                   fluidRow(strong(column(width=5,offset=1,"Total Undergraduates Seeking Degrees: ")),
                                                                            textOutput("ugds2")))
                               ),
                               br(),
                               br(),
                               fluidRow(align="center",
                                        style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                                        column(6,offset=3,
                                               br(),hr(style="color:#808080"),
                                               helpText( strong("Calculate Median Debt by Family Income" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                               hr(style="color:#808080")
                                        )),
                               br(),
                               
                               # === display sliderInput for family income
                               fluidRow(align="center",sliderInput("fincome","Family Income: ",
                                                                   min=0,max=200000,value=0,width = 600)
                                        
                               ),br(),
                               # === display median debt based on family income input 
                               fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"), 
                                                                    fluidRow(strong(column(width=7,offset = 1,"Median Debt based on Family Income: ")),
                                                                             textOutput("debt1")),
                                                                    fluidRow(strong(column(width=7,offset=1,"Median Debt based on Family Income: ")),
                                                                             textOutput("debt2")))
                               ),
                               br(),
                               br(),
                               fluidRow(align="center",
                                        style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                                        column(6,offset=3,
                                               br(),
                                               hr(style="color:#808080"),
                                               helpText( strong("Demographics of Students" , style="color: #6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                               hr(style="color:#808080")
                                        )),
                               br(),
                               # === Bar with corresponding widget
                               fluidRow(align="center",column(4,h2("Major Diversity",
                                                                   style="color:#4C4C4C ; font-family: Times"),
                                                              tags$hr(style="border-color: #6088B1;")),br()),
                               fluidRow(align="center",
                                        splitLayout(cellWidths = c("50%","50%"),
                                                    plotlyOutput("my_barplot1" , height = "500px"),
                                                    plotlyOutput("my_barplot2" , height = "500px")
                                        )
                                        
                               ),br(),br(),
                               # === pie chart of ethnicity
                               fluidRow(align="center",column(8,h2("Degree-Seeking Undergraduates by Ethnicity",
                                                                   style="color:#4C4C4C ; font-family: Times"),
                                                              tags$hr(style="border-color: #6088B1;")),br()),
                               fluidRow(align="center",
                                        splitLayout(cellWidths = c("50%","50%"),
                                                    plotlyOutput("demographics1",height="760"),
                                                    plotlyOutput("demographics2",height="760"))
                                        
                               ),br(),
                               fluidRow(align="center",column(8,h2("Degree-Seeking Undergraduates by Gender",
                                                                   style="color:#4C4C4C ; font-family: Times"),
                                                              tags$hr(style="border-color: #6088B1;")),br()),
                               fluidRow(align="center",
                                        splitLayout(cellWidths = c("50%","50%"),
                                                    plotlyOutput("female1",height="630"),
                                                    plotlyOutput("female2",height="630")
                                                    
                                        ))
                               
                               
                               
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
    leaflet()%>%
      setView(lng = mapping()$X, lat = mapping()$Y, zoom = mapping()$Z)%>%
      addProviderTiles("Esri.WorldStreetMap")%>%
      addMarkers(lng = school.selection()$LONGITUDE, lat = school.selection()$LATITUDE, popup = paste0(school.selection()$INSTNM,
                                                                                                       "<br><strong>Average Score of SAT Reading: </strong>",
                                                                                                       "<mark>",school.selection()$SATVRMID,"</mark>",
                                                                                                       "<br><strong>Average Score of SAT Math: </strong>",
                                                                                                       school.selection()$SATMTMID), icon=list(iconUrl='https://cdn0.iconfinder.com/data/icons/back-to-school/90/school-learn-study-hat-graduate_2-512.png',iconSize=c(25,25)))%>%
      addProviderTiles("Esri.WorldImagery",options = providerTileOptions(opacity = mapping.opt()$O))
                         
    })
  
  outputmap = reactive({
    if(input$output == "Degree")
    {
      list(info=school.selection()[,c("LONGITUDE","LATITUDE","HIGHDEG_1")], color = c("blue","green", "yellow", "orange", "red"))
      
    }
    else if(input$output == "Two Year vs Four Year")
    {
      list(info=school.selection()[,c("LONGITUDE","LATITUDE","twoorfour")],color = c("yellow","red"))
    }
    else if(input$output == "Transfer rate")
    {
      list(info=school.selection()[,c("LONGITUDE","LATITUDE","loworhigh")],color = c("yellow","red"))
    }
    else if(input$output == "Full vs Part Time")
    {
      list(info=school.selection()[,c("LONGITUDE","LATITUDE","partorfull")],color = c("yellow","red"))
    }
    
  })
  
  
  
  output$map_1=renderUI({
    leafletOutput('myMap_1', width = "100%", height = 500)
  })
 

  output$myMap_1 = renderLeaflet({
    leaflet()%>%
      setView(lng = mapping()$X, lat = mapping()$Y, zoom = mapping()$Z)%>%
      addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")%>%
      addCircleMarkers(lng = outputmap()[[1]][,1], lat = outputmap()[[1]][,2],clusterOptions = markerClusterOptions(),fillColor=colorFactor(palette = outputmap()[[2]],domain = outputmap()[[1]][,3])(outputmap()[[1]][,3]), stroke=FALSE, fillOpacity=0.8)%>%
      addLegend("bottomright", pal = colorFactor(palette = outputmap()[[2]],domain = outputmap()[[1]][,3]), values = outputmap()[[1]][,3],opacity = 1)

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
  
  
  ###############################################################################################################
  my_schools = reactive({c(input$input1,input$input2)})
  
  output$logo1 = renderImage({
    filename <- normalizePath(file.path("../data",
                                        'Tri-State College of Acupuncture School.jpg'))
    
    list(src=filename)
  },deleteFile = FALSE)
  
  output$logo2 = renderImage({
    filename <- normalizePath(file.path("../data",
                                        'Tri-State College of Acupuncture School.jpg'))
    
    list(src=filename)
  },deleteFile = FALSE)
  
  ############## get data used to display city
  
  MY_city_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    get.city.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      #get city
      city <- relevent_data$CITY
      return(city)
    }
    output <- as.matrix(rbind(get.city.outputs(college,my_schools[1]),get.city.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$city1= renderText(MY_city_display()[1,])
  output$city2= renderText(MY_city_display()[2,])
  
  ############## get data used to display iclevel
  
  MY_iclevel_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    get.iclevel.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      #get city
      #get ICLEVEL 
      if(relevent_data$ICLEVEL == 1){
        iclevel <- "4-year Institution"
      }
      if(relevent_data$ICLEVEL == 2){
        iclevel <- "2-year Institution"
      }
      if(relevent_data$ICLEVEL == 3){
        iclevel <- "Less-that-2-year Institution"
      }
      return(iclevel)
    }
    output <- as.matrix(rbind(get.iclevel.outputs(college,my_schools[1]),get.iclevel.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$iclevel1= renderText(MY_iclevel_display()[1,])
  output$iclevel2= renderText(MY_iclevel_display()[2,])
  
  ############## get data used to display control
  MY_control_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.control.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      #get CONTROL 
      if(relevent_data$CONTROL == 1){
        control <- "Public"
      }
      if(relevent_data$CONTROL == 2){
        control <- "Private Nonprofit"
      }
      if(relevent_data$CONTROL == 3){
        control <- "Private For-Profit"
      }
      return(control)
    }
    output <- as.matrix(rbind(get.control.outputs(college,my_schools[1]),get.control.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$control1= renderText(MY_control_display()[1,])
  output$control2= renderText(MY_control_display()[2,])
  ############## get data used to display highest degree
  MY_highdeg_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.highdeg.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      #get HIGHDEG
      if(relevent_data$HIGHDEG == 0){
        highdeg <- "Non-Degree Granting"
      }
      if(relevent_data$HIGHDEG == 1){
        highdeg <- "Certificate Degree"
      }
      if(relevent_data$HIGHDEG == 2){
        highdeg <- "Associate Degree"
      }
      if(relevent_data$HIGHDEG == 3){
        highdeg <- "Bachelor's Degree"
      }
      if(relevent_data$HIGHDEG == 4){
        highdeg <- "Graduate Degree"
      }
      return(highdeg)
    }
    output <- as.matrix(rbind(get.highdeg.outputs(college,my_schools[1]),get.highdeg.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$highdeg1= renderText(MY_highdeg_display()[1,])
  output$highdeg2= renderText(MY_highdeg_display()[2,])
  ############## get data used to display locale
  MY_locale_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.locale.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      #get LOCALE
      if(relevent_data$LOCALE == 11){
        locale <- "City, large (population of 250,000 or more)"
      }
      if(relevent_data$LOCALE == 12){
        locale <- "City, midsize (population of at least 100,000 but less than 250,000)"
      }
      if(relevent_data$LOCALE == 13){
        locale <- "City, small (population of less than 100,000)"
      }
      if(relevent_data$LOCALE == 21){
        locale <- "Suburb, large (population of 250,000 or more)"
      }
      if(relevent_data$LOCALE == 22){
        locale <- "Suburb, midsize (population of at least 100,000 but less than 250,000)"
      }
      if(relevent_data$LOCALE == 23){
        locale <- "Suburb, midsize (population of less than 100,000)"
      }
      if(relevent_data$LOCALE == 31){
        locale <- "Town, fringe"
      }
      if(relevent_data$LOCALE == 32){
        locale <- "Town, distant"
      }
      if(relevent_data$LOCALE == 33){
        locale <- "Town, remote"
      }
      if(relevent_data$LOCALE == 41){
        locale <- "Rural, fringe"
      }
      if(relevent_data$LOCALE == 42){
        locale <- "Rural, distant"
      }
      if(relevent_data$LOCALE == 43){
        locale <- "Rural, remote"
      }
      return(locale)
    }
    output <- as.matrix(rbind(get.locale.outputs(college,my_schools[1]),get.locale.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$locale1= renderText(MY_locale_display()[1,])
  output$locale2= renderText(MY_locale_display()[2,])
  ############## get data used to display admission rate
  MY_adrate_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.adrate.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      #get adm_rate
      adm_rate <- relevent_data$ADM_RATE
      return(adm_rate)
    }
    output <- as.matrix(rbind(get.adrate.outputs(college,my_schools[1]),get.adrate.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$adm_rate1= renderText(MY_adrate_display()[1,])
  output$adm_rate2= renderText(MY_adrate_display()[2,])
  ############## get data used to display in-state tuition
  MY_tuitionfee_in_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.tuitionfee_in.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      
      tuitionfee_in <- relevent_data$TUITIONFEE_IN
      return(tuitionfee_in)
    }
    output <- as.matrix(rbind(get.tuitionfee_in.outputs(college,my_schools[1]),get.tuitionfee_in.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$tuitionfee_in1= renderText(paste0("$",MY_tuitionfee_in_display()[1,]))
  output$tuitionfee_in2= renderText(paste0("$",MY_tuitionfee_in_display()[2,]))
  ############## get data used to display out-of-state tuition
  MY_tuitionfee_out_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.tuitionfee_out.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      
      tuitionfee_out <- relevent_data$TUITIONFEE_OUT
      return(tuitionfee_out)
    }
    output <- as.matrix(rbind(get.tuitionfee_out.outputs(college,my_schools[1]),get.tuitionfee_out.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$tuitionfee_out1= renderText(paste0("$",MY_tuitionfee_out_display()[1,]))
  output$tuitionfee_out2= renderText(paste0("$",MY_tuitionfee_out_display()[2,]))
  ############## get data used to display out-of-state tuition
  MY_pctfloan_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.pctfloan.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      
      #get PCTFLOAN
      pctfloan <- relevent_data$PCTFLOAN
      pctfloan <- paste(pctfloan*100,"%",collapse="")
      return(pctfloan)
    }
    output <- as.matrix(rbind(get.pctfloan.outputs(college,my_schools[1]),get.pctfloan.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$pctfloan1= renderText(MY_pctfloan_display()[1,])
  output$pctfloan2= renderText(MY_pctfloan_display()[2,])
  
  ############## get data used to display ugds
  MY_ugds_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.ugds.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      
      ugds <- relevent_data$UGDS
      
      return(ugds)
    }
    output <- as.matrix(rbind(get.ugds.outputs(college,my_schools[1]),get.ugds.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$ugds1= renderText(MY_ugds_display()[1,])
  output$ugds2= renderText(MY_ugds_display()[2,])
  ############## get data used to display instnm
  MY_instnm_display=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.instnm.outputs <- function(data,school){
      relevent_data <- data[data$INSTNM == school,]
      
      instnm <- relevent_data$INSTNM
      
      return(instnm)
    }
    output <- as.matrix(rbind(get.instnm.outputs(college,my_schools[1]),get.instnm.outputs(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
  })
  
  output$instnm1= renderText(MY_instnm_display()[1,])
  output$instnm2= renderText(MY_instnm_display()[2,])
  ####################### get data used to draw bar graph of different majors
  MY_summary_stat=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.bargraph.data <- function(data,school){
      my.text <- "PCIP[0-9][0-9]"
      indices <- grepl(my.text,colnames(data))
      my.data <- data[data$INSTNM==school,indices]
      if (is.na(my.data[1,1])){
        my.data <- rep(0,length(my.data))
      }
      return(my.data)
    }
    
    output <- as.matrix(rbind(get.bargraph.data(college,my_schools[1]),get.bargraph.data(college,my_schools[2])))
    rownames(output) <- c(my_schools[1], my_schools[2])
    return(output)
    
  })
  
  major.names <- c("Agriculture", "Natural Resources and Conservation",
                   "Architecture", "Group (Gender, Ethnic, etc.) Studies",
                   "Communication & Journalism", "Communications Technologies",
                   "Computer & Information Sciences", "Personal & Culinary Services",
                   "Education", "Engineering","Engieering Technologies",
                   "Foreign Languages","Consumer/Human Sciences", "Legal Professions",
                   "English", "General Studies & Humanities", "Library Science", 
                   "Biological & Biomedical Sciences", "Mathematics & Statistics",
                   "Military Techologies", "Multi/Interdisciplinary Studies",
                   "Fitness Studies", "Philosophy & Religious Studies", "Theology",
                   "Physical Sciences","Science Technologies", "Psychology", 
                   "Homeland Security", "Public Admin. & Social Service", "Social Sciences",
                   "Construction Trades", "Mechanic and Repair Technologies", 
                   "Precision Production", "Transportation","Visual & Performing Arts",
                   "Health Professions","Business","History")
  
  output$my_barplot1=renderPlotly({ 
    plot_ly(
      x = major.names,
      y = MY_summary_stat()[1,],
      name = "school",
      type = "bar"
    ) %>%
      layout(title = paste("Major distribution of <br>", my_schools()[1]),
             xaxis = list(tickangle=-65),margin=list(b=230))
    
  })
  
  output$my_barplot2=renderPlotly({ 
    plot_ly(
      x = major.names,
      y = MY_summary_stat()[2,],
      name = "school",
      type = "bar"
    ) %>%
      layout(title = paste("Major distribution of <br>", my_schools()[2]),
             xaxis = list(tickangle=-65), margin=list(b=230))
    
  })
  
  ###############get data used to draw pie chart of ethnicity
  
  MY_ethnicity_data1=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.pie.chart <- function(data, school){
      my.text <- "UGDS_2*[A-Z]+"
      indices <- grepl(my.text,colnames(data))
      my.data <- data[data$INSTNM==school,indices]
      my.data <- my.data[,1:9]
      demo.names <- c("White","Black","Hispanic","Asian","American Indian/Alaska Native",
                      "Native Hawaiian/Pacific Islander","2 or More Races","Non-resident Aliens",
                      "Unknown")
      colnames(my.data) <- demo.names
      to.remove <- NULL
      for (i in 1:length(my.data)){
        if (my.data[i] == 0 | is.na(my.data[i])){
          to.remove <- c(to.remove,i)
        }
      }
      
      if (length(my.data) == length(to.remove)){
        my.df <- data.frame(1)
        colnames(my.df) <- "NA"
        return(my.df)
      } else {
        if (!is.null(to.remove)){
          my.df <- my.data[,-to.remove]
          colnames(my.df) <- demo.names[-to.remove]
          return(my.df)
        } else{return(my.data)}
      } 
      
    }
    output <- as.matrix(get.pie.chart(college, my_schools[1]))
    rownames(output) <- 1
    output <- data.frame("type" = demo.names, "value" = t(output))
    return(output)
    
  })
  
  output$demographics1 <-renderPlotly(
    plot_ly(MY_ethnicity_data1(), labels = ~type, values = ~X1, type = 'pie')%>%
      layout(title = paste("Ethnicity diversity of <br>", my_schools()[1],"<br>"),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend=list(orientation='h'))
    
  )     
  
  MY_ethnicity_data2=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.pie.chart <- function(data, school){
      my.text <- "UGDS_2*[A-Z]+"
      indices <- grepl(my.text,colnames(data))
      my.data <- data[data$INSTNM==school,indices]
      my.data <- my.data[,1:9]
      demo.names <- c("White","Black","Hispanic","Asian","American Indian/Alaska Native",
                      "Native Hawaiian/Pacific Islander","2 or More Races","Non-resident Aliens",
                      "Unknown")
      colnames(my.data) <- demo.names
      to.remove <- NULL
      for (i in 1:length(my.data)){
        if (my.data[i] == 0 | is.na(my.data[i])){
          to.remove <- c(to.remove,i)
        }
      }
      
      if (length(my.data) == length(to.remove)){
        my.df <- data.frame(1,"Privacy Suppressed")
        #colnames(my.df) <- "NA"
        return(my.df)
      } else {
        if (!is.null(to.remove)){
          my.df <- my.data[,-to.remove]
          colnames(my.df) <- demo.names[-to.remove]
          my.df <- cbind(t(my.df),colnames(my.df))
          return(my.df)
        } else{
          my.df <- cbind(t(my.data),colnames(my.data))
          return(my.df)}
      } 
      
    }
    #output <- as.matrix(get.pie.chart(college, my_schools[2]))
    #rownames(output) <- 2
    output <- get.pie.chart(college,my_schools[2])
    return(output)
    
  })
  
  output$demographics2 <-
    renderPlotly(
      plot_ly(MY_ethnicity_data2(), labels = MY_ethnicity_data2()[,2],
              values = MY_ethnicity_data2()[,1], 
              type = 'pie')%>%
        layout(title = paste("Ethnicity diversity of <br>", my_schools()[2],"<br>"),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend=list(orientation='h'))
      
    )
  
  #############get data used to draw pie chart of female students
  MY_female_data1=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.mf.data <- function(data,school){
      index <- which(data$INSTNM == school)
      female <- data$FEMALE[index]
      
      if (is.na(female)) {female <- "PrivacySuppressed"}
      if(female=="PrivacySuppressed"){
        out.val<-data.frame(1,"Privacy Suppressed")
      } else{
        female<-as.numeric(female) 
        out.val<-data.frame(c(female,1-female),c("Female","Male"))
      }
      
      return(out.val)
    }
    output <- get.mf.data(college, my_schools[1])
    colnames(output) <- c("1","mf")
    return(output)
    
  })
  output$female1 <- renderPlotly(
    plot_ly(MY_female_data1(), labels = ~MY_female_data1()[,2], values = ~MY_female_data1()[,1], type = 'pie') %>%
      layout(title = paste("Gender diversity of <br>", my_schools()[1]),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend=list(orientation='h'))
  )
  MY_female_data2=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.mf.data <- function(data,school){
      index <- which(data$INSTNM == school)
      female <- data$FEMALE[index]
      
      if (is.na(female)) {female <- "PrivacySuppressed"}
      
      #relevent_data <- data[data$INSTNM == school,]
      #female <- relevent_data$FEMALE
      if(female=="PrivacySuppressed"){
        out.val<-data.frame(1,"Privacy Suppressed")
      } else{
        female<-as.numeric(female) 
        out.val<-data.frame(c(female,1-female),c("Female","Male"))
      }
      return(out.val)
      
    }
    output <- get.mf.data(college, my_schools[2])
    colnames(output) <- c("2","mf")
    return(output)
    
  })
  output$female2 <- renderPlotly(
    plot_ly(MY_female_data2(), labels = MY_female_data2()[,2], values = MY_female_data2()[,1], type = 'pie') %>%
      layout(title = paste("Gender diversity of <br>", my_schools()[2]),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend=list(orientation='h'))
  )
  
  debt.data.1 <- reactive({
    family.income = input$fincome
    my_schools=c(input$input1,input$input2)
    
    get.debt.mdn <- function(data,school,family.income) {
      
      if (family.income <= 30000) {
        debt.mdn <- data$LO_INC_DEBT_MDN[data$INSTNM==school]
      } else if (family.income > 30000 & family.income <= 75000) {
        debt.mdn <- data$MD_INC_DEBT_MDN[data$INSTNM==school]
      } else if (family.income > 75000) {
        debt.mdn <- data$HI_INC_DEBT_MDN[data$INSTNM==school]
      }
      
      return(debt.mdn)
    }
    
    output1 = get.debt.mdn(college,my_schools()[1],family.income)
    return(output1)
  })
  
  debt.data.2 <- reactive({
    family.income = input$fincome
    my_schools=c(input$input1,input$input2)
    
    get.debt.mdn <- function(data,school,family.income) {
      
      if (family.income <= 30000) {
        debt.mdn <- data$LO_INC_DEBT_MDN[data$INSTNM==school]
      } else if (family.income > 30000 & family.income <= 75000) {
        debt.mdn <- data$MD_INC_DEBT_MDN[data$INSTNM==school]
      } else if (family.income > 75000) {
        debt.mdn <- data$HI_INC_DEBT_MDN[data$INSTNM==school]
      }
      
      return(debt.mdn)
    }
    
    output2 = get.debt.mdn(college,my_schools()[2],family.income)
    return(output2)
  })
  
  output$debt1 <- renderText(paste0("$",debt.data.1()))
  output$debt2 <- renderText(paste0("$",debt.data.2()))
  
  
  
  
                                }
)


