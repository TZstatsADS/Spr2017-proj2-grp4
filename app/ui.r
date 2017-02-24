packages.used=c("shiny","ggmap","leaflet","dplyr","shinyBS","plotly","extrafont","grDevices","shinyjs")

# check packages that need to be installed.
packages.needed=setdiff(packages.used,intersect(installed.packages()[,1],packages.used))

# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(shiny)
library(ggmap)
library(leaflet)
library(dplyr)
library(shinyBS)
library(plotly)
library(extrafont)
library(grDevices)
library(shinyjs)
college.filtered = readRDS("../data/school.select.rds")
college =  readRDS("../data/College2014_15_new.rds")
#college.filtered = read.csv("../data/school.select.csv",header = TRUE,stringsAsFactors = FALSE)
#college =  read.csv("../data/College2014_15_new.csv",header = TRUE,stringsAsFactors = FALSE, na.strings = "NULL")
major = c("Agriculture, Agriculture Operations, And Related Sciences","Natural Resources And Conservation", "Architecture And Related Services","Area, Ethnic, Cultural, Gender, And Group Studies"," Communication, Journalism, And Related Programs","Communications Technologies/Technicians And Support Services","Computer And Information Sciences And Support Services","Personal And Culinary Services"," Education","Engineering","Engineering Technologies And Engineering-Related Fields","Foreign Languages, Literatures, And Linguistics"," Family And Consumer Sciences/Human Sciences","Legal Professions And Studies","English Language And Literature/Letters","Liberal Arts And Sciences, General Studies And Humanities","Library Science"," Biological And Biomedical Sciences","Mathematics And Statistics","Military Technologies And Applied Sciences","Multi/Interdisciplinary Studies","Parks, Recreation, Leisure, And Fitness Studies","Philosophy And Religious Studies","Theology And Religious Vocations"," Physical Sciences"," Science Technologies/Technicians"," Psychology"," Homeland Security, Law Enforcement, Firefighting And Related Protective Services","Public Administration And Social Service Professions","Social Sciences","Construction Trades","Mechanic And Repair Technologies/Technicians","Precision Production","Transportation And Materials Moving","Visual And Performing Arts","Health Professions And Related Programs","Business, Management, Marketing, And Related Support Services","History")
major.index =c("PCIP01","PCIP03","PCIP04","PCIP05","PCIP09","PCIP10","PCIP11","PCIP12","PCIP13","PCIP14","PCIP15","PCIP16","PCIP19","PCIP22","PCIP23","PCIP24","PCIP25","PCIP26","PCIP27","PCIP29","PCIP30","PCIP31","PCIP38","PCIP39","PCIP40","PCIP41","PCIP42","PCIP43","PCIP44","PCIP45","PCIP46","PCIP47","PCIP48","PCIP49","PCIP50","PCIP51","PCIP52","PCIP54")
major.frame = data.frame(major = major, index = major.index)

shinyUI(fluidPage(
  div(id="canvas",
      
      navbarPage(strong("NY School Hunter
                        ",style="color: white;"), theme="style.css",
                 
                 tabPanel(strong(tags$i("Map")),
                          div(class="outer",  
                              # lealfet map
                              uiOutput("map"),
                              #absolutePanel(id  = "controls", class = "panel panel-default", fixed = TRUE,
                               #             draggable = FALSE, top = 60, left = 10, bottom = "auto",
                                #            width = 500, height = "auto", cursor = "move",
                                 #           wellPanel(sliderInput("opt","",min=-3,max=5,value=-3,step=1))
                                  #          ),
                              
                              #bootstrapPage(
                              #conditionalPanel(condition = "input$opt>0",
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 100, left = 5, bottom = "auto",
                                            width = "auto", height = "auto", cursor = "move",
                                            #wellPanel(fluidRow(sliderInput("opt.232"," ",min=-4,max=4,step=1,value=-4))),
                                            #conditionalPanel(condition = "input$opt.232>0",
                                            wellPanel(style = "overflow-y:scroll; max-height: 600px",
                                          #bsCollapse(id = "default",
                                          bsCollapse(id="collapse.filter",open="Filter", 
                                          
                                                     bsCollapsePanel(tags$strong("Filter"),style="primary",
                                                            fluidRow(column(12,checkboxGroupInput("filter","Filtered By...",choices=list("Scores","Major","Tuition"),inline = TRUE))),
                                                            fluidRow(column(10,uiOutput("ui.filter")))),
                                                            
                                                          
                                                     #),
                                         #bsCollapse(id="collapse.default",
                                          bsCollapsePanel(tags$strong("Filter Options"),style = "primary",
                                            bsCollapsePanel(tags$strong("Major"),style="info",
                                                            fluidRow(column(10,selectInput("major",tags$strong("Your Major"),choices = c(major),selected = ""))
                                                            )
                                            ),
                                            bsCollapsePanel(tags$strong("SAT"),style="info",
                                                            fluidRow(column(3,numericInput("sat.reading",tags$strong("Read"),value=800,min=0,max=800,step=10)),
                                                                     column(3,numericInput("sat.math",tags$strong("Math"),value=800,min=0,max=800,step=10),offset = 0),
                                                                     column(3,numericInput("sat.writing",tags$strong("Write"),value=800,min=20,max=800,step=10),offset = 0)
                                                            )
                                            ),
                                            
                                            bsCollapsePanel(tags$strong("ACT"),style="info",  
                                                            fluidRow(column(10,numericInput("score.act",tags$strong("Cumulative Scores"),value=36,min=0,max=36,step=1))
                                                            )
                                            ),
                                            
                                            bsCollapsePanel(tags$strong("Tuition"),style="info",    
                                                            fluidRow(
                                                              column(10,numericInput("max",tags$strong("Max Tution"),min = 0, max = 90000, value = 10000))),
                                                             fluidRow(column(10, offset = 1,radioButtons("location",tags$strong("Tuition Options"),choices = list("State Resident", "Non-State Resident"),selected = "", inline = FALSE))
                                                            )
                                            )
                                           )
                                           ),
                                            
                                            
                                            bsCollapsePanel(tags$strong("Map Options"),style="primary",  
                                                            fluidRow(column(10,selectInput("Focus",tags$strong("Area of Focus"),choices = c("New York State","New York City","Western New York","Finger Lakes","Southern Tier","Central New York","North Country","Mohawk Valley","Capital District","Hudson Valley","Long Island"), selected = "New York Sate"))
                                                            ),
                                                            fluidRow(column(10,radioButtons("output.cluster",tags$strong("Cluster by Options"),choices=list("Degree","Length","Transfer Rate","Type"),selected = "Degree",inline=TRUE)))
                                                            #fluidRow(column())
                                            
                                            ),
                                            actionButton("search", tags$strong("Searching!"))
                                                   )#WellPanel ends here
                              #
                              #)#Conditional Panel ends here
                              ),
                              
                              # output panel

                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, right = -90, bottom = "auto",
                                            width = 470, height = 400, cursor = "move",

                                            leafletOutput('myMap_1', width = "80%", height = 450)   
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
                 tabPanel(strong(tags$i("Comparision!")),
                          ###########################################TEAM 2 IMPLEMENTATION STARTS##########################################################
                          wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                          tags$hr(style="border-color: #6088B1;"),
                          h1("Side-by-Side Two School Comparison",align= "center",style = "color: #333333; font-family: Times; 
                             font-size:50pt"),
                          tags$hr(style="border-color: #6088B1;"),
                          
                          fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                              uiOutput("ui.1"),
                                                              uiOutput("ui.2")
                                                              #selectInput("input1",label="Select a School",choices=college$INSTNM,size=10,selectize=F,width="90%"),
                                                              #selectInput("input2",label="Select a School",choices=college$INSTNM,size=10,selectize=F,width="90%")
                          )
                          ),
                          br(),br(),
                          fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                              imageOutput("logo1",height = "400", width = "400"),imageOutput("logo2",height = "400", width = "400")
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
                                                              fluidRow(strong(column(width=7,offset=1,"Percentage of Students Receiving Federal Loans: ")),
                                                                       textOutput("pctfloan1")),
                                                              fluidRow(strong(column(width=7,offset=1,"Percentage of Students Receiving Federal Loans: ")),
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
                                                               fluidRow(column(width=7,offset=1,textOutput("school1")),
                                                                        strong(column(width=7,offset = 1,"Median Debt based on Family Income : ")),br(),
                                                                        textOutput("debt1")),
                                                               fluidRow(column(width=7,offset=1,textOutput("school2")),
                                                                        strong(column(width=7,offset=1,"Median Debt based on Family Income: ")),br(),
                                                                        textOutput("debt2")))
                          ),
                          br(),
                          br(),
                          fluidRow(align="center",
                                   style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                                   column(6,offset=3,
                                          br(),hr(style="color:#808080"),
                                          helpText( strong("SAT & ACT Scores" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                          helpText( strong("25th-75th Percentile" , style="color:#6088B1 ; font-family: 'times'; font-size:20pt ; font-type:bold" )),
                                          hr(style="color:#808080")
                                   )),
                          br(),
                          
                          fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                              textOutput("school1.2"),
                                                              textOutput("school2.2"))
                                   
                          ),
                          
                          fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                              tableOutput("sat1"),
                                                              tableOutput("sat2"))
                                   
                          ),br(),
                          
                          fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                              tableOutput("act1"),
                                                              tableOutput("act2"))
                                   
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
                                               plotlyOutput("demographics1",height="550"),
                                               plotlyOutput("demographics2",height="550"))
                                   
                          ),br(),
                          fluidRow(align="center",column(8,h2("Degree-Seeking Undergraduates by Gender",
                                                              style="color:#4C4C4C ; font-family: Times"),
                                                         tags$hr(style="border-color: #6088B1;")),br()),
                          fluidRow(align="center",
                                   splitLayout(cellWidths = c("50%","50%"),
                                               plotlyOutput("female1",height="450"),
                                               plotlyOutput("female2",height="450")
                                               
                                   ))
                          ############################################TEAM 2 IMPLEMENTATION ENDS############################################################
                          )
                          ),#Comparison ends here
                 #Data presentation
                 #Introduction
                 tabPanel(strong(tags$i("About us")),
                          mainPanel(width=12,
                                    h1("Project: NY School Hunter - A Shiny App Development"),
                                    h3("Background"),
                                    p("Our project takes all available data on colleges and universities in New York State and creates a useful shiny app that allows users to explore and compare schools based on user-specific filtering criteria. The purpose of our design is to provide users with a bird's eye view of New York colleges and universities; allow them to filter, search, and group schools by their preferred criteria; and further compare two schools on a more micro level."),#Our motivation
                                    h3("Project summary"),
                                    p("A distinguishing feature of our app is the map search function - users can see all the specified schools on the map (normal map view or satellite map view), focus in on a specific area of New York State.
                                      The side-by-side school comparison feature allows users to see a detailed breakdown of meaningful data and statistics from our available data on each school."),#What did we do
                                   
                                    h3("Team Members"),
                                    p("   - ",strong("Ka Heng (Helen) Lo - Presenter")),
                                      p("   - ",strong("Boxuan Zhao")),
                                        p("   - ",strong("Song Wang")),
                                          p("   - ",strong("Senyao Han")),
                                            p("   - ",strong("Zijun Nie")),
                                    p(""),
                                    p(""),
                                    br(),
                                    p(em("Release 02/22/2017.","VERSION 1.0.0")),
                                    p(em(a("Github link",href="https://github.com/TZstatsADS/Spr2017-proj2-grp4.git"))))
                          ,div(class="footer", "Applied Data Science Group 4")
                 )#Introduciton ends here
                 
                 
      )#navarbarPage ends here
  )
 
))

