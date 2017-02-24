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

#Read in Files
college.filtered = readRDS("../data/school.select.rds")
college =  readRDS("../data/College2014_15_new.rds")

#Support data frame
major = c("Agriculture, Agriculture Operations, And Related Sciences","Natural Resources And Conservation", "Architecture And Related Services","Area, Ethnic, Cultural, Gender, And Group Studies"," Communication, Journalism, And Related Programs","Communications Technologies/Technicians And Support Services","Computer And Information Sciences And Support Services","Personal And Culinary Services"," Education","Engineering","Engineering Technologies And Engineering-Related Fields","Foreign Languages, Literatures, And Linguistics"," Family And Consumer Sciences/Human Sciences","Legal Professions And Studies","English Language And Literature/Letters","Liberal Arts And Sciences, General Studies And Humanities","Library Science"," Biological And Biomedical Sciences","Mathematics And Statistics","Military Technologies And Applied Sciences","Multi/Interdisciplinary Studies","Parks, Recreation, Leisure, And Fitness Studies","Philosophy And Religious Studies","Theology And Religious Vocations"," Physical Sciences"," Science Technologies/Technicians"," Psychology"," Homeland Security, Law Enforcement, Firefighting And Related Protective Services","Public Administration And Social Service Professions","Social Sciences","Construction Trades","Mechanic And Repair Technologies/Technicians","Precision Production","Transportation And Materials Moving","Visual And Performing Arts","Health Professions And Related Programs","Business, Management, Marketing, And Related Support Services","History")
major.index =c("PCIP01","PCIP03","PCIP04","PCIP05","PCIP09","PCIP10","PCIP11","PCIP12","PCIP13","PCIP14","PCIP15","PCIP16","PCIP19","PCIP22","PCIP23","PCIP24","PCIP25","PCIP26","PCIP27","PCIP29","PCIP30","PCIP31","PCIP38","PCIP39","PCIP40","PCIP41","PCIP42","PCIP43","PCIP44","PCIP45","PCIP46","PCIP47","PCIP48","PCIP49","PCIP50","PCIP51","PCIP52","PCIP54")
major.frame = data.frame(major = major, index = major.index)

shinyServer(function(input, output,session) {
  
  #Define reactive ui for scores
  output$ui.filter = renderUI({
    if(c("Scores") %in% input$filter)
    {radioButtons("sat.act","Which Score?",choices=list("SAT","ACT"),selected = "",inline=TRUE)}
    else
    {return()}
    
  })
  
  
  
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
  
  #Get interactive major index
  major.data.index = reactive({
    major.frame[which(major.frame$major == input$major),"index"]
  })
  
  
  major.data.frame.mean = reactive({
    mean(college.filtered[,major.data.index()])
  })
  
  #Filtering Process
  school.selection = eventReactive(input$search,{
    
    
    if(c("None") %in% input$filter)
    {
      college.filtered
    }
    else if(c("Scores") %in% input$filter && c("Tuition") %in% input$filter)
    {
      if(input$sat.act=="SAT")
      {
        if(input$location == "State Resident")
        {
          college.filtered %>% filter(((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing)) & TUITIONFEE_IN <= input$max)
          
        }
        else if(input$location == "Non-State Resident")
        {
          college.filtered %>% filter(((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing)) & TUITIONFEE_OUT <= input$max)
        }
      }
      else if(input$sat.act=="ACT")
      {
        if(input$location == "State Resident")
        {
          college.filtered %>% filter((ACTCMMID <= input$score.act) & TUITIONFEE_IN <= input$max)          
        }
        else if(input$location == "Non-State Resident")
        {
          college.filtered %>% filter((ACTCMMID <= input$score.act) & TUITIONFEE_OUT <= input$max)          
          
        }
      } 
    }
    
    else if(c("Major") %in% input$filter && c("Tuition") %in% input$filter)
      {
        if(input$location == "State Resident")
        {
          college.filtered %>% filter(TUITIONFEE_IN <= input$max & college.filtered[,major.data.index()] > major.data.frame.mean())
        }
        else if(input$location == "Non-State Resident")
        {

          college.filtered %>% filter(TUITIONFEE_OUT <= input$max & college.filtered[,major.data.index()] > major.data.frame.mean())
        }
      }
        
        else if(c("Scores") %in% input$filter && c("Major") %in% input$filter)
        {
          if(input$sat.act=="SAT")
          {
            college.filtered %>% filter(((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing))  & college.filtered[,major.data.index()] > major.data.frame.mean())
            
          }
          else if(input$sat.act=="ACT")
          {
            college.filtered %>% filter((ACTCMMID <= input$score.act) & college.filtered[,major.data.index()] > major.data.frame.mean())
            
          } 
        }
          
        else if(c("Scores") %in% input$filter && c("Tuition") %in% input$filter && c("Major") %in% input$filter)
          {
            if(input$sat.act=="SAT")
            {
              if(input$location == "State Resident")
              {
                college.filtered %>% filter(((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing)) & TUITIONFEE_IN <= input$max & college.filtered[,major.data.index()] > major.data.frame.mean())
                
              }
              else if(input$location == "Non-State Resident")
              {

                college.filtered %>% filter(((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing)) & TUITIONFEE_OUT <= input$max & college.filtered[,major.data.index()] > major.data.frame.mean())
                
              }
            }
            else if(input$sat.act=="ACT")
            {
              if(input$location == "State Resident")
              {
                college.filtered %>% filter((ACTCMMID <= input$score.act) & TUITIONFEE_IN <= input$max & college.filtered[,major.data.index()] > major.data.frame.mean())
                
              }
              else if(input$location == "Non-State Resident")
              {
                college.filtered %>% filter((ACTCMMID <= input$score.act) & TUITIONFEE_OUT <= input$max & college.filtered[,major.data.index()] > major.data.frame.mean())
                
                
              }
            } 
        }
    else if(c("Scores") %in% input$filter)
    {
      if(input$sat.act=="SAT")
      {
        college.filtered %>% filter(((SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math) | (SATVRMID <= input$sat.reading & SATWRMID <= input$sat.writing) | (SATWRMID <= input$sat.writing & SATMTMID <= input$sat.math)|(SATVRMID <= input$sat.reading & SATMTMID <= input$sat.math & SATWRMID <= input$sat.writing)))
        
      }
      else if(input$sat.act=="ACT")
      {
        college.filtered %>% filter((ACTCMMID <= input$score.act))
        
      } 
    }
      
      else if(c("Tuition") %in% input$filter)
      {
        if(input$location == "State Resident")
        {
          college.filtered %>% filter(TUITIONFEE_IN <= input$max)
          
        }
        else if(input$location == "Non-State Resident")
        {
          college.filtered %>% filter(TUITIONFEE_OUT <= input$max )
        }
      }
        
        else if(c("Major") %in% input$filter)
        {
          college.filtered %>% filter(college.filtered[,major.data.index()] > major.data.frame.mean())
          
        }
          
    
    
    
    
    
  })
  
  
  output$map=renderUI({
    leafletOutput('myMap', width = "100%", height = 700)
  })
  
  #Main Map definition
  output$myMap = renderLeaflet({
    leaflet()%>%
      setView(lng = mapping()$X, lat = mapping()$Y, zoom = mapping()$Z)%>%
      addTiles(group = "Esri.WorldStreetMap") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addProviderTiles("Stamen.Toner", group = "Toner by Stamen") %>%
      addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      addProviderTiles("Thunderforest.SpinalMap", group = "Thunderforest.SpinalMap") %>%
      addProviderTiles("Thunderforest.TransportDark", group = "Thunderforest.TransportDark") %>%
      addMarkers(lng = school.selection()$LONGITUDE, lat = school.selection()$LATITUDE, popup = paste(school.selection()$INSTNM,school.selection()$INSTURL), icon=list(iconUrl='https://cdn0.iconfinder.com/data/icons/back-to-school/90/school-learn-study-hat-graduate_2-512.png',iconSize=c(25,25)))%>%
      addLayersControl(
        baseGroups = c("Esri.WorldStreetMap","Esri.WorldImagery","Toner by Stamen","OpenStreetMap","Thunderforest.SpinalMap","Thunderforest.TransportDark")
      )%>%addMeasure(
        position = "topright"
      )%>%addMiniMap()
  })
  
  outputmap = reactive({
    if(input$output.cluster == "Degree")
    {
      list(info=school.selection()[,c("LONGITUDE","LATITUDE","HIGHDEG_1", "INSTNM")], color = c("blue","green", "yellow", "orange", "red"))
      
    }
    else if(input$output.cluster == "Length")
    {
      list(info=school.selection()[,c("LONGITUDE","LATITUDE","twoorfour","INSTNM")],color = c("yellow","red"))
    }
    else if(input$output.cluster == "Transfer Rate")
    {
      list(info=school.selection()[,c("LONGITUDE","LATITUDE","loworhigh","INSTNM")],color = c("yellow","red"))
    }
    else if(input$output.cluster == "Type")
    {
      list(info=school.selection()[,c("LONGITUDE","LATITUDE","partorfull","INSTNM")],color = c("yellow","red"))
    }
    
  })
  
  #Classification map definition
  output$myMap_1 = renderLeaflet({
  
    leaflet()%>% addTiles()%>%addProviderTiles("OpenStreetMap.BlackAndWhite")%>%
      addCircleMarkers(lng = outputmap()[[1]][,1], lat = outputmap()[[1]][,2],popup=outputmap()[[1]][,4],clusterOptions = markerClusterOptions(iconCreateFunction =
                                                                                                                                                 JS("
                                                                                                                                                    function(cluster) {
                                                                                                                                                    return new L.DivIcon({
                                                                                                                                                    html: '<div style=\"background-color:rgba(77,77,77,0.5)\"><span><b>' + cluster.getChildCount() + '</b></div><span>',
                                                                                                                                                    className: 'marker-cluster'
                                                                                                                                                    });
                                                                                                                                                    }")),fillColor=colorFactor(palette = outputmap()[[2]],domain = outputmap()[[1]][,3])(outputmap()[[1]][,3]), stroke=FALSE, fillOpacity=0.8)%>%addLegend("bottomright", pal = colorFactor(palette = outputmap()[[2]],domain = outputmap()[[1]][,3]), values = outputmap()[[1]][,3],opacity = 1)%>%addSimpleGraticule()
      
   })
  
  #Sync the movement of the main map to the second map
  observe({
    b <- input$myMap_bounds
    leafletProxy("myMap_1") %>% setView(mean(c(b$west, b$east)), mean(c(b$north, b$south)), input$myMap_zoom)
  })
  
  ###########################################TEAM 2 IMPLEMENTATION STARTS#################################################################################
  
  
  
  output$ui.1 = renderUI({
    selectInput("input1",label="Select a School",choices=school.selection()$INSTNM,size=10,selectize=F,width="90%")
  })
  output$ui.2 = renderUI({
    selectInput("input2",label="Select a School",choices=school.selection()$INSTNM,size=10,selectize=F,width="90%")
  })
  my_schools = reactive({c(input$input1,input$input2)})
  
  output$logo1 = renderImage({
    my_schools_file <- input$input1
    regex_image <- ".png"
    filename <- normalizePath(file.path(
                                        paste("../output/www/",my_schools_file, regex_image, sep = "")))
    
    list(src=filename)
  },deleteFile = FALSE)
  
  output$logo2 = renderImage({
    my_schools_file <- input$input2
    regex_image <- ".png"
    filename <- normalizePath(file.path(
                                        paste("../output/www/",my_schools_file, regex_image, sep = "")))
    
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
      my.data <- as.vector(as.matrix(my.data[1,]))
      
      demo.names <- c("White","Black","Hispanic","Asian","American Indian/Alaska Native",
                      "Native Hawaiian/Pacific Islander","2 or More Races","Non-resident Aliens",
                      "Unknown")
      #colnames(my.data) <- demo.names
      my.df <- data.frame(my.data,demo.names,colors=c(1,2,3,4,5,6,7,8,9)
      )
      
      to.remove <- NULL
      for (i in 1:length(my.data)){
        if (my.data[i] == 0 | is.na(my.data[i])){
          to.remove <- cbind(to.remove,i)
        }
      }
      to.remove <- as.vector(to.remove[1,])
      
      if (length(my.data) == length(to.remove)){
        output.df <- data.frame(1,"Privacy Suppressed",color='rgb(128,133,133)')
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
    output <- get.pie.chart(college,my_schools[1])
    return(output)
    
  })
  #colors <- c('rgb(128,133,133)','rgb(211,94,96)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
  
  output$demographics1 <-
    renderPlotly(
      plot_ly(MY_ethnicity_data1(), labels = MY_ethnicity_data1()[,2],
              values = MY_ethnicity_data1()[,1], 
              type = 'pie',marker = list(colors = MY_ethnicity_data1()[,3],
                                         line = list(color = '#FFFFFF', width = 1)),
              width = 500, height = 500, textposition = 'inside+outside',
              textinfo = 'label',
              insidetextfont = list(color = '#FFFFFF'), showlegend=F )
      %>%
        layout(title = paste("Ethnicity diversity of <br>", my_schools()[1],"<br>"),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend=list(orientation='h'), 
               margin = list(l = 100,r = 100,b = 20,t = 100,pad = 5))
      
      
    )
  
  MY_ethnicity_data2=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$input1,input$input2)
    
    get.pie.chart <- function(data, school){
      my.text <- "UGDS_2*[A-Z]+"
      indices <- grepl(my.text,colnames(data))
      my.data <- data[data$INSTNM==school,indices]
      my.data <- my.data[,1:9]
      my.data <- as.vector(as.matrix(my.data[1,]))
      
      demo.names <- c("White","Black","Hispanic","Asian","American Indian/Alaska Native",
                      "Native Hawaiian/Pacific Islander","2 or More Races","Non-resident Aliens",
                      "Unknown")
      #colnames(my.data) <- demo.names
      my.df <- data.frame(my.data,demo.names,colors=c(1,2,3,4,5,6,7,8,9)
      )
      
      to.remove <- NULL
      for (i in 1:length(my.data)){
        if (my.data[i] == 0 | is.na(my.data[i])){
          to.remove <- cbind(to.remove,i)
        }
      }
      to.remove <- as.vector(to.remove[1,])
      
      if (length(my.data) == length(to.remove)){
        output.df <- data.frame(1,"Privacy Suppressed",color='rgb(128,133,133)')
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
              type = 'pie',marker = list(colors = MY_ethnicity_data2()[,3],
                                         line = list(color = '#FFFFFF', width = 1)),
              width = 500, height = 500, textposition = 'inside+outside',
              textinfo = 'label',
              insidetextfont = list(color = '#FFFFFF'), showlegend=F )
      %>%
        layout(title = paste("Ethnicity diversity of <br>", my_schools()[2],"<br>"),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend=list(orientation='h'),
               margin = list(l = 100,r = 100,b = 20,t = 100,pad = 5))
      
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
        out.val<-data.frame(1,"Privacy Suppressed",color='rgb(128,133,133)')
      } else{
        female<-as.numeric(female) 
        out.val<-data.frame(c(female,1-female),c("Female","Male"), color=c(1,2))
      }
      
      return(out.val)
    }
    output <- get.mf.data(college, my_schools[1])
    #colnames(output) <- c("1","mf")
    return(output)
    
  })
  output$female1 <- renderPlotly(
    plot_ly(MY_female_data1(), labels = ~MY_female_data1()[,2], values = ~MY_female_data1()[,1], type = 'pie',
            marker = list(colors = MY_female_data1()[,3], 
                          line = list(color = '#FFFFFF', width = 1)), 
            width = 400, height = 400, textposition = 'inside+outside',
            textinfo = 'label',
            insidetextfont = list(color = '#FFFFFF'), showlegend=F
    ) 
    %>%
      layout(title = paste("Gender diversity of <br>", my_schools()[1]),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend=list(orientation='h'),
             margin = list(l = 50,r = 50,b = 100,t = 100,pad = 4))
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
        out.val<-data.frame(1,"Privacy Suppressed",color='rgb(128,133,133)')
      } else{
        female<-as.numeric(female) 
        out.val<-data.frame(c(female,1-female),c("Female","Male"),color=c(1,2))
      }
      return(out.val)
      
    }
    output <- get.mf.data(college, my_schools[2])
    #colnames(output) <- c("2","mf")
    return(output)
    
  })
  output$female2 <- renderPlotly(
    plot_ly(MY_female_data2(), labels = MY_female_data2()[,2], values = MY_female_data2()[,1], type = 'pie',
            marker = list(colors = MY_female_data2()[,3], 
                          line = list(color = '#FFFFFF', width = 1)), 
            width = 400, height = 400, textposition = 'inside+outside',
            textinfo = 'label',
            insidetextfont = list(color = '#FFFFFF'), showlegend=F) 
    %>%
      layout(title = paste("Gender diversity of <br>", my_schools()[2]),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend=list(orientation='h'),
             margin = list(l = 50,r = 50,b = 100,t = 100,pad = 4))
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
    
    cbind(SAT = c("Verbal" ,"Math", "Writing"),
          Score = c(paste(MY_score_data1()[1],"-",MY_score_data1()[2]),
                    paste(MY_score_data1()[2],"-", MY_score_data1()[4]),
                    paste(MY_score_data1()[5],"-",MY_score_data1()[6])))
  )
  output$sat2 <- renderTable(
    #as.matrix(MY_score_data2())[1:6,],
    #rownames = c("SAT Verbal 25th","SAT Verbal 75th","SAT Math 25th","SAT Math 75th","SAT Writing 25th","SAT Writing 75th"),
    #colnames = "Score"
    
    cbind(SAT = c("Verbal" ,"Math", "Writing"),
          Score = c(paste(MY_score_data2()[1],"-",MY_score_data2()[2]),
                    paste(MY_score_data2()[2],"-", MY_score_data2()[4]),
                    paste(MY_score_data2()[5],"-",MY_score_data2()[6])))
  )
  output$act1 <- renderTable(
    #as.matrix(MY_score_data1())[7:14,],
    #rownames = c("ACT Cumulative Score 25th","ACT Cumulative Score 75th","ACT English 25th","ACT English 75th","ACT Math 25th","ACT Math 75th","ACT Writing 25th","ACT Writing 75th"),
    #colnames = "Score"
    
    
    cbind(ACT = c("Cumulative Score" ,"English", "Math","Writing"),
          Score = c(paste(MY_score_data1()[7],"-",MY_score_data1()[8]),
                    paste(MY_score_data1()[9],"-", MY_score_data1()[10]),
                    paste(MY_score_data1()[11],"-",MY_score_data1()[12]),
                    paste(MY_score_data1()[13],"-",MY_score_data1()[14])))
  )
  output$act2 <- renderTable(
    #as.matrix(MY_score_data2())[7:14,],
    #rownames = c("ACT Cumulative Score 25th","ACT Cumulative Score 75th","ACT English 25th","ACT English 75th","ACT Math 25th","ACT Math 75th","ACT Writing 25th","ACT Writing 75th"),
    #colnames = "Score",
    cbind(ACT = c("Cumulative Score" ,"English", "Math","Writing"),
          Score = c(paste(MY_score_data2()[7],"-",MY_score_data2()[8]),
                    paste(MY_score_data2()[9],"-", MY_score_data2()[10]),
                    paste(MY_score_data2()[11],"-",MY_score_data2()[12]),
                    paste(MY_score_data2()[13],"-",MY_score_data2()[14])))
  )
  
  output$school1 <- renderText(paste("(",my_schools()[1],")"))
  output$school2 <- renderText(paste("(",my_schools()[2],")"))
  output$school1.2 <- renderText(paste("(",my_schools()[1],")"))
  output$school2.2 <- renderText(paste("(",my_schools()[2],")"))
  ###########################################TEAM 2 IMPLEMENTATION ENDS###################################################################################
  
  
})