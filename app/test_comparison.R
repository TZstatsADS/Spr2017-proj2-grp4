library(shiny)
library(plotly)

college<-read.csv("../data/College2014_15.csv",stringsAsFactors = F,na.strings = "NULL")

ui <- fluidPage(
        titlePanel("Comparing two schools!"),
        
        fluidRow(splitLayout(cellWidths = c("50%","50%"),
                             selectInput("input1",label="Select a School",choices=college$INSTNM,size=7,selectize=F),
                             selectInput("input2",label="Select a School",choices=college$INSTNM,size=7,selectize=F)
        )),
        
        fluidRow(splitLayout(cellWidths = c("50%","50%"),
                             imageOutput("logo1"),imageOutput("logo2")
                )),
        
        fluidRow(align="center",
                 style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                 column(6,offset=3,
                        br(),
                        helpText( strong("help_test" , style="color:Orange ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                        hr()
                 )),
        
        # === Some text to explain the Figure:
        fluidRow(align="justify",
                 style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                 column(6,offset=3,
                        br(),"explain_test",br()
                 )
        ),br(),
        
      
        
        # ==== Title in Orange
        fluidRow(align="center",
                 style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                 column(6,offset=3,
                        br(),
                        helpText( strong("Basic Information" , style="color:Orange ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                        hr()
                 )),
        
        # === Some text to explain the Figure:
        fluidRow(align="justify",
                 style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                 column(6,offset=3,
                        br(),"explain_test2",br(),br(),br(),br()
                 )
        ),br(),
        # === display instnm
        fluidRow(align = "center",splitLayout(cellWidths = c("50%","50%"),
                                               fluidRow(strong(column(width=2,"Institution Name: ")),textOutput("instnm1")),
                                               fluidRow(strong(column(width=2,"Institution Name: ")),textOutput("instnm2")))
        ),br(),
        # === display city
        fluidRow(align = "justify",splitLayout(cellWidths = c("50%","50%"),
                             fluidRow(strong(column(width=2,"City: ")),textOutput("city1")),
                             fluidRow(strong(column(width=2,"City: ")),textOutput("city2")))
        ),br(),
        
        # === display clevel
        fluidRow(align = "justify",splitLayout(cellWidths = c("50%","50%"),
                             fluidRow(strong(column(width=4,"Level of Institution: ")),textOutput("iclevel1")),
                             fluidRow(strong(column(width=4,"Level of Institution: ")),textOutput("iclevel2")))
        ),br(),
        # === display control
        
        fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                             fluidRow(strong(column(width=4,"Control of Institution: ")),textOutput("control1")),
                             fluidRow(strong(column(width=4,"Control of Institution: ")),textOutput("control2")))
        ),br(),
        # === display highest degree
        
        fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                             fluidRow(strong(column(width=3,"Highest Degree: ")),textOutput("highdeg1")),
                             fluidRow(strong(column(width=3,"Highest Degree: ")),textOutput("highdeg2")))
        ),br(),
        # === display locale
        
        fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                             fluidRow(strong(column(width=2,"Locale: ")),textOutput("locale1")),
                             fluidRow(strong(column(width=2,"Locale: ")),textOutput("locale2")))
        ),br(),
        # === display admission rate
        
        fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                             fluidRow(strong(column(width=2,"Admission Rate: ")),textOutput("adm_rate1")),
                             fluidRow(strong(column(width=2,"Admission Rate: ")),textOutput("adm_rate2")))
        ),br(),
        # === display in-state tuition
        
        fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"), 
                             fluidRow(strong(column(width=3,"In-State Tuition: ")),textOutput("tuitionfee_in1")),
                             fluidRow(strong(column(width=3,"In-State Tuition: ")),textOutput("tuitionfee_in2")))
        ),br(),
        # === display out-of-state tuition
        
        fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"), 
                             fluidRow(strong(column(width=3,"Out-of-State Tuition: ")),textOutput("tuitionfee_out1")),
                             fluidRow(strong(column(width=3,"Out-of-State Tuition: ")),textOutput("tuitionfee_out2")))
        ),br(),
        # === display percentage of federal loans
        
        fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"), 
                             fluidRow(strong(column(width=5,"Percentage of Students Receiving Federal Loans: ")),
                                      textOutput("pctfloan1")),
                             fluidRow(strong(column(width=5,"Percentage of Students Receiving Federal Loans: ")),
                                      textOutput("pctfloan2")))
        ),br(),
        # === display total Undergraduates Seeking Degrees
        
        fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"), 
                             fluidRow(strong(column(width=5,"Total Undergraduates Seeking Degrees: ")),
                                      textOutput("ugds1")),
                             fluidRow(strong(column(width=5,"Total Undergraduates Seeking Degrees: ")),
                                      textOutput("ugds2")))
        ),
        
        # === Bar whit corresponding widget
        fluidRow(h2("Major Diversity"),
                splitLayout(cellWidths = c("50%","50%"),
                plotlyOutput("my_barplot1" , height = "500px"),
                plotlyOutput("my_barplot2" , height = "500px")
                )
                
        ),br(),
        # === pie chart of ethnicity
        fluidRow(align="justify",h2("Degree-Seeking Undergraduates by Ethnicity"),
                 splitLayout(cellWidths = c("50%","50%"),
                             plotlyOutput("demographics1",height="730"),
                           plotlyOutput("demographics2",height="730"))
                 
        ),
        fluidRow(align="center",h2("Degree-Seeking Undergraduates by Gender"),
                 splitLayout(cellWidths = c("50%","50%"),
                             plotlyOutput("female1",height="630"),
                             plotlyOutput("female2",height="630")
                 
        ))
  
)
server <- function(input, output) {
        school <- c("Cornell University", "Columbia University in the City of New York")
        college<-read.csv("../data/College2014_15.csv",stringsAsFactors = F,na.strings = "NULL")
        
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
        
        output$tuitionfee_in1= renderText(MY_tuitionfee_in_display()[1,])
        output$tuitionfee_in2= renderText(MY_tuitionfee_in_display()[2,])
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
        
        output$tuitionfee_out1= renderText(MY_tuitionfee_out_display()[1,])
        output$tuitionfee_out2= renderText(MY_tuitionfee_out_display()[2,])
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
                               xaxis = list(tickangle=-65),margin=list(b=200))
                
        })
        
        output$my_barplot2=renderPlotly({ 
                plot_ly(
                        x = major.names,
                        y = MY_summary_stat()[2,],
                        name = "school",
                        type = "bar"
                ) %>%
                        layout(title = paste("Major distribution of <br>", my_schools()[2]),
                               xaxis = list(tickangle=-65), margin=list(b=200))
 
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
                output <- as.matrix(get.pie.chart(college, my_schools[2]))
                rownames(output) <- 2
                output <- data.frame("type" = demo.names, "value" = t(output))
                return(output)
                
        })
        
        output$demographics2 <-
          renderPlotly(
                plot_ly(MY_ethnicity_data2(), labels = ~type, values = ~X2, type = 'pie')%>%
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
                        relevent_data <- data[data$INSTNM == school,]
                        female <- relevent_data$FEMALE
                        if(female=="PrivacySuppressed"){
                                out.val<-list(1,"Privacy Suppressed")
                        }
                        if(is.na(female)){
                                out.val<-list(1,"Privacy Suppressed")
                        } else{female<-as.numeric(female) 
                        out.val<-list(c(female,1-female),c("Female","Male"))
                        }
                        
                        return(out.val)
                }
                output <- get.mf.data(college, my_schools[1])
                output <- as.data.frame(output)
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
                        relevent_data <- data[data$INSTNM == school,]
                        female <- relevent_data$FEMALE
                        if(female=="PrivacySuppressed"){
                                out.val<-list(1,"Privacy Suppressed")
                        }
                        if(is.na(female)){
                                out.val<-list(1,"Privacy Suppressed")
                        } else{female<-as.numeric(female) 
                        out.val<-list(c(female,1-female),c("Female","Male"))
                        }
                        
                        return(out.val)
                }
                output <- get.mf.data(college, my_schools[2])
                output <- as.data.frame(output)
                colnames(output) <- c("2","mf")
                return(output)
                
        })
        output$female2 <- renderPlotly(
                plot_ly(MY_female_data2(), labels = ~MY_female_data2()[,2], values = ~MY_female_data2()[,1], type = 'pie') %>%
                        layout(title = paste("Gender diversity of <br>", my_schools()[2]),
                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                               legend=list(orientation='h'))
        )
        
        
}

shinyApp(ui,server)

