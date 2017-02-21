library(shiny)
library(plotly)
library(extrafont)
library(grDevices)

college<-read.csv("../data/College2014_15_new.csv",stringsAsFactors = F,na.strings = "NULL")
college.filtered = read.csv("../data/school.select.csv",header = TRUE,stringsAsFactors = FALSE)

ui <- fluidPage(tags$hr(style="border-color: #6088B1;"),
                h1("Side-by-Side Two School Comparison",align= "center",style = "color: #333333; font-family: Times; 
                   font-size:50pt"),
                tags$hr(style="border-color: #6088B1;"),
                
                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                    selectInput("input1",label="Select a School",choices=college$INSTNM,size=10,selectize=F,width="90%"),
                                                    selectInput("input2",label="Select a School",choices=college$INSTNM,size=10,selectize=F,width="90%")
                )),
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
                                br(),hr(style="color:#808080"),
                                helpText( strong("SAT & ACT Scores" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                hr(style="color:#808080")
                         )),
                br(),
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
                
                )
server <- function(input, output) {
        school <- c("Cornell University", "Columbia University in the City of New York")
        college<-read.csv("../data/College2014_15_new.csv",stringsAsFactors = F,na.strings = "NULL")
        
        my_schools = reactive({c(input$input1,input$input2)})
        
        output$logo1 = renderImage({
                my_schools_file <- input$input1
                regex_image <- ".png"
                filename <- normalizePath(file.path("../www",
                                                    paste(my_schools_file, regex_image, sep = "")))
                
                list(src=filename)
        },deleteFile = FALSE)
        
        output$logo2 = renderImage({
                my_schools_file <- input$input2
                regex_image <- ".png"
                filename <- normalizePath(file.path("../www",
                                                    paste(my_schools_file, regex_image, sep = "")))
                
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
                output <- as.matrix(rbind(get.tuitionfee_in.outputs(college.filtered,my_schools[1]),get.tuitionfee_in.outputs(college.filtered,my_schools[2])))
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
                output <- as.matrix(rbind(get.tuitionfee_out.outputs(college.filtered,my_schools[1]),get.tuitionfee_out.outputs(college.filtered,my_schools[2])))
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
                        my.data <- as.vector(my.data[1,1:9])
                        demo.names <- c("White","Black","Hispanic","Asian","American Indian/Alaska Native",
                                        "Native Hawaiian/Pacific Islander","2 or More Races","Non-resident Aliens",
                                        "Unknown")
                        #colnames(my.data) <- demo.names
                        my.df <- as.data.frame(cbind(my.data,demo.names))
                        
                        to.remove <- NULL
                        for (i in 1:length(my.data)){
                                if (my.data[i] == 0 | is.na(my.data[i])){
                                        to.remove <- c(to.remove,i)
                                }
                        }
                        
                        if (length(my.data) == length(to.remove)){
                                output.df <- data.frame(1,"Privacy Suppressed")
                                #colnames(my.df) <- "NA"
                                return(output.df)
                        } else if (!is.null(to.remove)){
                                #my.df <- my.data[-to.remove]
                                #colnames(my.df) <- demo.names[-to.remove]
                                output.df <- my.df[-to.remove,]
                                return(output.df)
                        } else {return(my.df)}
                        
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
        
        ### sat cat
        MY_score_data1=reactive({
                
                # Get the needed reactive objects:
                my_schools=c(input$input1,input$input2)
                
                get.testscore.data <- function(data,school){
                        # "400-780" 
                        #SAT Scores
                        my.text <- "SAT[A-Z][A-Z][0-9]5"
                        indices <- grepl(my.text,colnames(data))
                        sat.data <- data[data$INSTNM==school,indices]
                        
                        #ACT Scores
                        my.text <- "ACT[A-Z][A-Z][0-9]5"
                        indices <- grepl(my.text,colnames(data))
                        act.data <- data[data$INSTNM==school,indices]
                        
                        my.vector <- c(sat.data[1,1],sat.data[1,2],
                                       sat.data[1,3],sat.data[1,4],
                                       sat.data[1,5],sat.data[1,6],
                                       act.data[1,1],act.data[1,2],
                                       act.data[1,3],act.data[1,4],
                                       act.data[1,5],act.data[1,6],
                                       act.data[1,7],act.data[1,8])
                        
                        return(my.vector)
                }
                output <- get.testscore.data(college, my_schools[1])
                return(output)
                
        })
        
        MY_score_data2=reactive({
                
                # Get the needed reactive objects:
                my_schools=c(input$input1,input$input2)
                
                get.testscore.data <- function(data,school){
                        # "400-780" 
                        #SAT Scores
                        my.text <- "SAT[A-Z][A-Z][0-9]5"
                        indices <- grepl(my.text,colnames(data))
                        sat.data <- data[data$INSTNM==school,indices]
                        
                        #ACT Scores
                        my.text <- "ACT[A-Z][A-Z][0-9]5"
                        indices <- grepl(my.text,colnames(data))
                        act.data <- data[data$INSTNM==school,indices]
                        
                        my.vector <- c(sat.data[1,1],sat.data[1,2],
                                       sat.data[1,3],sat.data[1,4],
                                       sat.data[1,5],sat.data[1,6],
                                       act.data[1,1],act.data[1,2],
                                       act.data[1,3],act.data[1,4],
                                       act.data[1,5],act.data[1,6],
                                       act.data[1,7],act.data[1,8])
                        
                        return(my.vector)
                }
                output <- get.testscore.data(college, my_schools[2])
                return(output)
                
        })
        
        output$sat1 <- renderTable(
                # as.matrix(MY_score_data2())[1:6,],
                #rownames = c("SAT Verbal 25th","SAT Verbal 75th","SAT Math 25th","SAT Math 75th","SAT Writing 25th","SAT Writing 75th"),
                #colnames = "Score"
                stack(data.frame("SAT Verbal 25th"=MY_score_data1()[1], "SAT Verbal 75th"=MY_score_data1()[2],"SAT Math 25th"=MY_score_data1()[3],
                           "SAT Math 75th"=MY_score_data1()[4],"SAT Writing 25th"=MY_score_data1()[5],"SAT Writing 75th"=MY_score_data1()[6]))
        )
        output$sat2 <- renderTable(
                #as.matrix(MY_score_data2())[1:6,],
                #rownames = c("SAT Verbal 25th","SAT Verbal 75th","SAT Math 25th","SAT Math 75th","SAT Writing 25th","SAT Writing 75th"),
                #colnames = "Score"
                stack(data.frame("SAT Verbal 25th"=MY_score_data2()[1], "SAT Verbal 75th"=MY_score_data2()[2],"SAT Math 25th"=MY_score_data2()[3],
                           "SAT Math 75th"=MY_score_data2()[4],"SAT Writing 25th"=MY_score_data2()[5],"SAT Writing 75th"=MY_score_data2()[6]))
        )
        output$act1 <- renderTable(
                #as.matrix(MY_score_data1())[7:14,],
                #rownames = c("ACT Cumulative Score 25th","ACT Cumulative Score 75th","ACT English 25th","ACT English 75th","ACT Math 25th","ACT Math 75th","ACT Writing 25th","ACT Writing 75th"),
                #colnames = "Score"
                stack(data.frame("ACT Cumulative Score 25th"=MY_score_data1()[7],"ACT Cumulative Score 75th"=MY_score_data1()[8],"ACT English 25th"=MY_score_data1()[9], "ACT English 75th"=MY_score_data1()[10],
                           "ACT Math 25th"=MY_score_data1()[11],
                           "ACT Math 75th"=MY_score_data1()[12],"ACT Writing 25th"=MY_score_data1()[13],"ACT Writing 75th"=MY_score_data1()[14]))
        )
        output$act2 <- renderTable(
                #as.matrix(MY_score_data2())[7:14,],
                #rownames = c("ACT Cumulative Score 25th","ACT Cumulative Score 75th","ACT English 25th","ACT English 75th","ACT Math 25th","ACT Math 75th","ACT Writing 25th","ACT Writing 75th"),
                #colnames = "Score",
                stack(data.frame("ACT Cumulative Score 25th"=MY_score_data2()[7],"ACT Cumulative Score 75th"=MY_score_data2()[8],"ACT English 25th"=MY_score_data2()[9], "ACT English 75th"=MY_score_data2()[10],"ACT Math 25th"=MY_score_data2()[11],
                           "ACT Math 75th"=MY_score_data2()[12],"ACT Writing 25th"=MY_score_data2()[13],"ACT Writing 75th"=MY_score_data2()[14]))
                
        )
}

shinyApp(ui,server)

