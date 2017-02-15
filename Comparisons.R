#my.ui 

library(shiny)
library(plotrix)

#function to get data for both schools:
#output a list of all text outputs 
get.text.outputs <- function(data,school){
  relevent_data <- data[data$INSTNM == school,]
  output.list <- list()
  #get city
  city <- relevent_data$CITY
  output.list[[1]] <- city
  
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
  output.list[[2]] <- iclevel
  
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
  output.list[[3]] <- control
  
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
  output.list[[4]] <- highdeg
  
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
  output.list[[5]] <- locale
  
  #get adm_rate
  adm_rate <- relevent_data$ADM_RATE
  output.list[[6]] <- adm_rate
  
  #get TUITIONFEE_IN
  tuitionfee_in <- relevent_data$TUITIONFEE_IN
  output.list[[7]] <- tuitionfee_in
  #get TUITIONFEE_OUT
  tuitionfee_out <- relevent_data$TUITIONFEE_OUT
  output.list[[8]] <- tuitionfee_out
  
  #get PCTFLOAN
  pctfloan <- relevent_data[,relevent_data$PCTFLOAN]
  pctfloan <- paste(pctfloan*100,"%",collapse="")
  output.list[[9]] <- pctfloan
  
  ugds <- relevent_data[,relevent_data$UGDS]
  output.list[[10]] <- ugds
  
  return(output.list)
}

get.pie.chart <- function(data, school){
  my.text <- "UGDS_2*[A-Z]+"
  indices <- grepl(my.text,colnames(data))
  my.data <- data[data$INSTNM==school,indices]
  return(my.data)
}

get.bargraph.data <- function(data,school){
  my.text <- "PCIP[0-9][0-9]"
  indices <- grepl(my.text,colnames(data))
  
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
  my.data <- data[data$INSTNM==school,indices]
  if (is.na(my.data[1,1])){
    my.data <- rep(0,length(my.data))
  }
  return(cbind(major.names,my.data))
}

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
  
  my.vector <- c(paste(sat.data[1,1],sat.data[1,2],collapse="-"),
    paste(sat.data[1,3],sat.data[1,4],collapse="-"),
    paste(sat.data[1,5],sat.data[1,6],collapse="-"),
    paste(act.data[1,1],act.data[1,2],collapse="-"),
    paste(act.data[1,3],act.data[1,4],collapse="-"),
    paste(act.data[1,5],act.data[1,6],collapse="-"),
    paste(act.data[1,7],act.data[1,8],collapse="-"))
  
  return(my.vector)
}

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

get.debt.data <- function(data,school,income){
  relevent_data <- data[data$INSTNM == school,]
  lo_inc_debt_mdn <- relevant_data$LO_INC_DEBT_MDN
  md_inc_debt_mdn <- relevant_data$MD_INC_DEBT_MDN
  hi_inc_debt_mdn <- relevant_data$HI_INC_DEBT_MDN
  
  if (income <= 30000){
    return(lo_inc_debt_mdn)
  }
  if ( 30000 < income & income <= 75000){
    return(md_inc_debt_mdn)
  }
  if (income > 75000){
    return(hi_inc_debt_mdn)
  }
}




ui =fluidPage(
  h1("Comparing Two Schools"),
  hr(),
  br(),
  fluidRow(splitLayout(cellWidths = c("50%","50%"),
                       fluidRow(column(width=2,"School: "),textOutput("school1")),
                       fluidRow(column(width=2,"School: "),textOutput("school2")))
           ),
  fluidRow(splitLayout(cellWidths = c("50%","50%"),
                       fluidRow(column(width=4,"Level of Institution: "),textOutput("iclevel1")),
                       fluidRow(column(width=4,"Level of Institution: "),textOutput("iclevel2")))
           ),
  fluidRow(splitLayout(cellWidths = c("50%","50%"),
                       fluidRow(column(width=4,"Control of Institution: "),textOutput("control1")),
                       fluidRow(column(width=4,"Control of Institution: "),textOutput("control2")))
           ),
  fluidRow(splitLayout(cellWidths = c("50%","50%"),
                       fluidRow(column(width=3,"Highest Degree: "),textOutput("highdeg1")),
                       fluidRow(column(width=3,"Highest Degree: "),textOutput("highdeg2")))
  ),
  fluidRow(splitLayout(cellWidths = c("50%","50%"),
                       fluidRow(column(width=2,"Locale: "),textOutput("locale1")),
                       fluidRow(column(width=2,"Locale: "),textOutput("locale2")))
  ),
  fluidRow(splitLayout(cellWidths = c("50%","50%"),
                       fluidRow(column(width=2,"Admission Rate: "),textOutput("adm_rate1")),
                       fluidRow(column(width=2,"Admission Rate: "),textOutput("adm_rate2")))
  ),
  
  fluidRow(splitLayout(cellWidths = c("50%","50%"),
                       tableOutput("sat1"),
                       tableOutput("sat2"))
    
  ),
  
  fluidRow(splitLayout(cellWidths = c("50%","50%"),
                       tableOutput("act1"),
                       tableOutput("act2"))
           
  ),
  
  fluidRow(splitLayout(cellWidths = c("50%","50%"), 
          fluidRow(column(width=3,"In-State Tuition: "),textOutput("tuitionfee_in1")),
          fluidRow(column(width=3,"In-State Tuition: "),textOutput("tuitionfee_in2")))
  ),
  
  fluidRow(splitLayout(cellWidths = c("50%","50%"), 
                       fluidRow(column(width=3,"Out-of-State Tuition: "),textOutput("tuitionfee_out1")),
                       fluidRow(column(width=3,"Out-of-State Tuition: "),textOutput("tuitionfee_out2")))
  ),
  
  fluidRow(splitLayout(cellWidths = c("50%","50%"), 
                       fluidRow(column(width=5,"Percent of Students Receiving Federal Loans: "),
                                textOutput("pctfloan1")),
                       fluidRow(column(width=5,"Percent of Students Receiving Federal Loans: "),
                                textOutput("pctfloan2")))
  ),
  
  fluidRow(splitLayout(plotOutput("majors.bar.1"),plotOutput("majors.bar.2"))
           
  ),
  
  fluidRow(splitLayout(cellWidths = c("50%","50%"), 
                       fluidRow(column(width=5,"Total Undergraduates Seeking Degrees: "),
                                textOutput("ugds1")),
                       fluidRow(column(width=5,"Total Undergraduates Seeking Degrees: "),
                                textOutput("ugds2")))
  ),
  
  fluidRow(h2("Degree-Seeking Undergraduates by Ethnicity"),
           splitLayout(cellWidths = c("50%","50%"),
                       plotOutput("demographics1"),
                       plotOutput("demographics2"))
           
  ),
  
  fluidRow(h2("Degree-Seeking Undergraduates by Gender"),
           splitLayout(cellWidths = c("50%","50%"),
                       plotOutput("female1"),
                       plotOutput("female2"))
           
  ),
  
  fluidRow(h2("Median Debt Based on Family Income"),
           splitLayout(cellWidths = c("50%","50%"),
                       sliderInput("income1","Select family income", min = 0, max = 30000, value = 5000),
                       sliderInput("income2","Select family income", min = 0, max = 30000, value = 5000))
           
  ),
  
  fluidRow(splitLayout(cellWidths = c("50%","50%"), 
                       fluidRow(column(width=2,"Median Debt: "),
                                textOutput("debt1")),
                       fluidRow(column(width=2,"Median Debt: "),
                                textOutput("debt2")))
  )

)

server = function(inuput,output){
  
  data<-read.csv("../data/College2014_15.csv",stringsAsFactors = F,na.strings = "NULL")
  school1 <- "Allen School-Brooklyn"
  school2 <- "Bank Street College of Education"
  
  text1 <- get.text.outputs(data,school1)
  text2 <- get.text.outputs(data,school2)
  output$school1 = renderText(text1[[1]])
  output$school2 = renderText(text2[[1]])
  
  output$iclevel1 = renderText(text1[[2]])
  output$iclevel2 = renderText(text2[[2]])
  
  output$control1 = renderText(text1[[3]])
  output$control2 = renderText(text2[[3]])
  
  output$highdeg1 = renderText(text1[[4]])
  output$highdeg2 = renderText(text2[[4]])
  
  output$locale1 = renderText(text1[[5]])
  output$locale2 = renderText(text2[[5]])
  
  output$adm_rate1 = renderText(text1[[6]])
  output$adm_rate2 = renderText(text2[[6]])
  
  test.scores.1 <- get.testscore.data(data,school1)
  test.scores.2 <- get.testscore.data(data,school2)
  output$sat1 <- renderTable(
    data.frame("SAT Verbal"=test.scores.1[1],"SAT Math"=test.scores.1[2],
               "SAT Writing"=test.scores.1[3])
  )
  output$sat2 <- renderTable(
    data.frame("SAT Verbal"=test.scores.2[1],"SAT Math"=test.scores.2[2],
               "SAT Writing"=test.scores.2[3])
  )
  output$act1 <- renderTable(
    data.frame("ACT Cumulative Score" =test.scores.1[4],"ACT English"=test.scores.1[5],
               "ACT Math"=test.scores.1[6],"ACT Writing"=test.scores.1[7])
  )
  output$act2 <- renderTable(
    data.frame("ACT Cumulative Score" =test.scores.2[4],"ACT English"=test.scores.2[5],
               "ACT Math"=test.scores.2[6],"ACT Writing"=test.scores.2[7])
  )
  
  output$tuitionfee_in1 <- renderText(text1[[7]])
  output$tuitionfee_in2 <- renderText(text2[[7]])
  output$tuitionfee_out1 <- renderText(text1[[8]])
  output$tuitionfee_out2 <- renderText(text2[[8]])
  
  output$pctfloan1 <- renderText(text1[[9]])
  output$pctfloan2 <- renderText(text2[[9]])
  
  majors1 <- get.bar.graph.data(data,school1)
  majors2 <- get.bar.graph.data(data,school2)
  output$majors.bar.1 = renderPlot(barplot(majors1[,2],names.arg=majors1[,1]))
  output$majors.bar.2 = renderPlot(barplot(majors2[,2],names.arg=majors2[,1]))
  
  output$ugds1 <- renderText(text1[[10]])
  output$ugds2 <- renderText(text2[[10]])
  
  demo1 <- get.pie.chart(data,school1)
  demo2 <- get.pie.chart(data,school2)
  demo.names <- c("White","Black","Hispanic","Asian","American Indian/Alaska Native",
                  "Native Hawaiian/Pacific Islander","2 or More Races","Non-resident Aliens",
                  "Unknown")
  output$demographics1 <- renderPlot(
    pie3D(demo1,labels=demo.names)
  )
  output$demographics2 <- renderPlot(
    pie3D(demo2,labels=demo.names)
  )
  
  female1 <- get.mf.data(data,school1)
  female2 <- get.mf.data(data,school2)
  output$female1 <- renderPlot(
    pie3D(female1[[1]],labels=female1[[2]])
  )
  output$female2 <- renderPlot(
    pie3D(female2[[1]],labels=female2[[2]])
  )
  
  output$debt1 <- reactive({renderText({
    income1 <- input$income1
    get.debt.data(data,school1,income1)
  })})
  
  output$debt2 <- reactive({renderText({
    income2 <- input$income2
    get.debt.data(data,school2,income2)
  })})
                                   
}


shinyApp(ui=ui,server=server)