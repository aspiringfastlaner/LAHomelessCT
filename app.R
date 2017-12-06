library(knitr)
library(shiny)
library(leaflet)
library(shinydashboard)
library(graphics)
library(dplyr)
library(ggplot2)
library(geojsonio)
library(htmltools)
library(sp)
library(rgdal)
library(lubridate)
library(readxl)
library(rgeos)
library(maptools)
library(ggmap)

#rsconnect::deployApp('C:\\Users\\Fang\\Desktop\\USC MSBAN\\Fall 2017\\DSO 545\\Homeless Project\\LAHomelessCT')

############################ Data Cleaning for Map Tab
hcdata = read.csv("data/homelessagg.csv") %>%
  mutate(density2017 = totHomeless2017/as.numeric(Land.Area)) %>%
  mutate(density2016 = totHomeless2016/as.numeric(Land.Area))

comdata = hcdata %>%
  group_by(Community_Name) %>%
  summarise(totHomeless2017 = sum(totHomeless2017),
            totUnshelt2017 = sum(totUnshelt2017),
            totShelt2017 = sum(totShelt2017),
            totHomeless2016 = sum(totHomeless2016),
            totUnshelt2016 = sum(totUnshelt2016),
            totShelt2016 = sum(totShelt2016),
            landarea = sum(Land.Area)) %>%
  mutate(density2017 = totHomeless2017/landarea,
         density2016 = totHomeless2016/landarea)

shelters = read.csv("data/sheltersCT.csv")
# Shelters filtering
lacities = c('Los Angeles',
             'Hollywood',
             'Sherman Oaks',
             'San Pedro',
             'Canoga Park',
             'Venice',
             'North Hills',
             'North Hollywood',
             'Winnetka',
             'Wilmington',
             'Van Nuys',
             'Reseda',
             'Chatsworth')
shelters = filter(shelters, CITY %in% lacities)

crimes = read.csv("data/crimesagg.csv")
calls = read.csv('data/callsagg.csv')

labound = readOGR("shp/pit/Homeless_Count_2017_Results_CD.shp",
                   layer = "Homeless_Count_2017_Results_CD")
labound@data = hcdata

cabounds = unionSpatialPolygons(labound, labound$Community_Name,
                                ID = labound@data$Community_Name)

df = data.frame(getSpPPolygonsIDSlots(cabounds))
colnames(df) = "Community_Name"

comdata = left_join(df,comdata,by = "Community_Name")

cabounds = SpatialPolygonsDataFrame(cabounds, comdata, match.ID = FALSE)




########################### Data Cleaning or Heat map tab
crimesheat = read.csv("data/crimeCT.csv")
callsheat = read.csv('data/call311CT.csv')

callsheat$CREATEDDATE = strptime(x = as.character(callsheat$CREATEDDATE), format = "%m/%d/%Y %I:%M:%S %p")

callsheat["weekday"] = weekdays(callsheat$CREATEDDATE)
callsheat["hour"] = hour(callsheat$CREATEDDATE)
hour = seq(0,23)

timeslot = rep(c("Morning","Afternoon","Evening","Night"),each = 6)

timeslots = data.frame(hour,timeslot)

crimerate <- read_xlsx("data/Crimerate.xlsx", sheet = "Sheet1")

colnames(crimerate)[4] <- "Time Slot"

crimerate$Day <- factor(crimerate$Day, levels= c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

crimedescription <- read_xlsx("data/CrimeDescriptionByday.xlsx", sheet = "Sheet1")

crimedescription$Day <- factor(crimedescription$Day, levels= c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))


Call_TimeDate = callsheat%>%
  select(SRNUMBER,REQUESTSOURCE,APC,NCNAME,POLICEPRECINCT,CT10,weekday,hour)

Call_TimeDate1 = merge(Call_TimeDate,timeslots)

Call_TimeDate2 = Call_TimeDate1 %>%
  group_by(weekday,timeslot)%>%
  summarise(count = n())

Call_TimeDate2$weekday <- factor(Call_TimeDate2$weekday, levels= c("Monday", 
                                                                   "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))

Call_TimeDate2$timeslot <- factor(Call_TimeDate2$timeslot, levels= c("Morning", "Afternoon", "Evening", "Night"))


##############################33 Choices for drop-downs
yearvars = c('2017' = '2017',
             '2016' = '2016')

hcvars = c(
  "Total Homeless" = "totHomeless",
  "Total Unsheltered" = "totUnshelt",
  "Total Sheltered" = "totShelt",
  "Homeless Density" = "density"
)

# Choices for drop-downs
timeofday = c(
  "All Times" = "all",
  "12:00 am to 6:00 am" = 1,
  "6:00 am to 12:00 pm" = 2,
  "12:00 pm to 6:00 pm" = 3,
  "6:00 pm to 12:00 am" = 4
)

weekday = c(
  "All Days" = "all",
  "Monday" = 1,
  "Tuesday" = 2,
  "Wednesday" = 3,
  "Thursday" = 4,
  "Friday" = 5,
  "Saturday" = 6,
  "Sunday" = 7
)

month = c(
  "All Months" = "all",
  "January" = 1,
  "February" = 2,
  "March" = 3,
  "April" = 4,
  "May" = 5,
  "June" = 6,
  "July" = 7,
  "August" = 8,
  "September" = 9,
  "October" = 10,
  "November" = 11,
  "December" = 12
)

splitvars = c("Tract" = "tract",
              "Communities" = "communities")

ui = navbarPage(
  "DSO 545 Homeless Census", id="nav",
  
  tabPanel("Appendix and Reccommendations",
    fluidPage(
      mainPanel(
        includeHTML("appendix.html"),
        img(src='http://via.placeholder.com/350x150'),
        HTML('</br>'),
        img(src='http://via.placeholder.com/350x150'),
        HTML('</br>')
      )
    )
  ),
  
  tabPanel("Interactive Map", div(class = "outer",
                                  tags$head(includeCSS("styles.css"),
                                            includeScript("gomap.js")),
                                  
                                  leafletOutput("map", width = "100%", height = "100%"),
                                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                draggable = TRUE, top = 200, left = "auto", right = 20, 
                                                bottom = "auto",
                                                width = 300, height = "auto",
                                                HTML('<button data-toggle="collapse" data-target="#demo">Map Controls</button>'),
                                                tags$div(id = 'demo',  class="collapse",
                                                         h3("Homeless View: Tract or Communities"),
                                                         
                                                         selectInput("splitchoice", "View By",
                                                                     splitvars, selected = "communities",
                                                                     width = '100%'),
                                                         
                                                         h3("Homeless Population Filters"),
                                                         
                                                         selectInput("yearchoice", "Year", 
                                                                     yearvars, selected = "2017",
                                                                     width = '100%'),
                                                         
                                                         selectInput("hchoice", "HC Population Filters", 
                                                                     hcvars, selected = "totHomeless",
                                                                     width = '100%'),
                                                         h3("Crime and Calls Time Filters"),
                                                         
                                                         
                                                         selectInput("timechoice", "Time of Day", 
                                                                     timeofday, selected = "all",
                                                                     width = '100%'),
                                                         selectInput("daychoice", "Weekday", 
                                                                     weekday, selected = "all",
                                                                     width = '100%'),
                                                         selectInput("monthchoice", "Month of Year", 
                                                                     month, selected = "all",
                                                                     width = '100%')
                                                         )
                                                
                                  ))),
  tabPanel("Crimes and Calls Over Time",
           fluidRow(
             
             sidebarPanel(
               helpText('Select Different Variables for Plot'),
               HTML('</br>'),
               uiOutput('dvh'),    
               HTML('</br>'),
               uiOutput('cvh'),    
               HTML('</br>')),
             mainPanel(
               h5("Day-Time Distribution"),
               plotOutput("heatmap"),
               HTML('</br>')
             )
           ))
)


server = function(input, output, session){
  
  
  ########### R Code for Heatmap
  # summary statistics
  
  output$dvh = renderUI({
    selectInput('dvh', h5('Which variable with date and time?'), choices = (c("Homeless 311 Calls","Crimes")) )
  })
  
  output$cvh = renderUI({
    selectInput('cvh', h5('Type of Crime Filter'), choices = (c("ALL",
                                                                "ASSAULT",
                                                                "CHILD ABUSE",
                                                                "CRIMINAL HOMICIDE",
                                                                "KIDNAPPING",
                                                                "SEXUAL ASSAULT",
                                                                "THEFT",
                                                                "VANDALISM",
                                                                "OTHER MISCELLANEOUS CRIME")) )
  })
  
  crimerateheat = reactive( {
    if (input$cvh == "ALL") {
      crimedescription %>%
        select(Day, `Time Slot`) %>%
        group_by(Day, `Time Slot`) %>%
        summarise("NumberofCrimes" = n())
    } else {
      crimedescription %>%
        filter(`Crime Description` == input$cvh) %>%
        select(Day, `Time Slot`) %>%
        group_by(Day, `Time Slot`) %>%
        summarise("NumberofCrimes" = n())
    }
  })
  
  output$heatmap <- renderPlot({height = 500
    
    crimeheattitle = paste("Number of Crimes by Day and Time - ", input$cvh)
    
    if (input$dvh == "Homeless 311 Calls") {
      ggplot(Call_TimeDate2, aes(x=timeslot,y=count))+
        geom_point(aes(color=timeslot), size = 4)+
        facet_grid(~weekday) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(title = "Number of Homeless 311 Calls by Day and Time", 
             y = "No. of 311 Calls", x= "Timeslots by 6 Hour Block",
             color = "Time Slot"
        ) +
        theme(plot.title = element_text(hjust = 0.5))
      
    } else if (input$dvh == "Crimes") {
      ggplot(crimerateheat(), aes(x=`Time Slot`, y = crimerateheat()$`NumberofCrimes`))+
        geom_point(aes(color=`Time Slot`), size = 4)+
        facet_grid(~Day)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        labs(title = crimeheattitle, 
             y = "No. of Crimes", 
             x = "Timeslots by 6 Hour Block",
             color = "Time Slot") +
        theme(plot.title = element_text(hjust = 0.5))
    } 
    
    
    
  })
  
  
  ########### R Code for Interactive Map
  #output$commvh = renderUI({
  #  selectInput('commvh', h5('Community Selection'), 
  #              choices = c("All",levels(unique(hcdata$Community_Name))))
  #})
  
  output$map = renderLeaflet({
    
    bounds = switch(input$splitchoice,
                    "tract" = labound,
                    "communities" = cabounds
    )
    
    hcdata = switch(input$splitchoice,
                    "tract" = hcdata,
                    "communities" = comdata
    )
    
    ## Updating reactive dataframes for calls and crimes
    crimedf = reactive({
      if ((input$timechoice == "all") & (input$daychoice == "all") & (input$monthchoice == "all")) {
        crimes
      } else if ((input$timechoice == "all") & (input$daychoice == "all")) {
        filter(crimes,
               Month == input$monthchoice)
        
      } else if ((input$timechoice == "all") & (input$monthchoice == "all")) {
        filter(crimes,
               Weekday == input$daychoice)
        
      } else if ((input$daychoice == "all") & (input$monthchoice == "all")) {
        filter(crimes,
               DayPartition == input$timechoice)
        
      } else if (input$daychoice == "all"){
        filter(crimes,
               Month == input$monthchoice,
               DayPartition == input$timechoice)
        
      } else if (input$timechoice == "all"){
        filter(crimes,
               Month == input$monthchoice,
               Weekday == input$daychoice)
        
      } else if (input$monthchoice == "all"){
        filter(crimes,
               DayPartition == input$timechoice,
               Weekday == input$daychoice)
      } else {
        filter(crimes,
               DayPartition == input$timechoice,
               Weekday == input$daychoice,
               Month == input$monthchoice)
      }
    })
    
    calldf = reactive({
      if ((input$timechoice == "all") & (input$daychoice == "all") & (input$monthchoice == "all")) {
        calls
      } else if ((input$timechoice == "all") & (input$daychoice == "all")) {
        filter(calls,
               Month == input$monthchoice)
        
      } else if ((input$timechoice == "all") & (input$monthchoice == "all")) {
        filter(calls,
               Weekday == input$daychoice)
        
      } else if ((input$daychoice == "all") & (input$monthchoice == "all")) {
        filter(calls,
               DayPartition == input$timechoice)
        
      } else if (input$daychoice == "all"){
        filter(calls,
               Month == input$monthchoice,
               DayPartition == input$timechoice)
        
      } else if (input$timechoice == "all"){
        filter(calls,
               Month == input$monthchoice,
               Weekday == input$daychoice)
        
      } else if (input$monthchoice == "all"){
        filter(calls,
               DayPartition == input$timechoice,
               Weekday == input$daychoice)
      } else {
        filter(calls,
               DayPartition == input$timechoice,
               Weekday == input$daychoice,
               Month == input$monthchoice)
      }
    })
    
    if (input$splitchoice == "tract") {
      binsize = switch(input$hchoice,
                       "totHomeless" = c(0, 20, 60, 100, 150, 250, 420, 605, Inf),
                       "totUnshelt" = c(0, 20, 60, 100, 150, 200, 450, 520, Inf),
                       "totShelt" = c(0, 10, 30, 60, 100, 150, 210, 460, Inf),
                       "density" = c(0, 50, 200, 500, 1500, 2500, 3000, 3500, Inf))
      } else {
      binsize = switch(input$hchoice,
                       "totHomeless" = c(0, 85, 160, 280, 550, 900, Inf),
                       "totUnshelt" = c(0, 85, 150, 225, 400, 650, Inf),
                       "totShelt" = c(0, 10, 30, 60, 100, 150, 210, 460, Inf),
                       "density" = c(0, 15, 45, 65, 180, 220, Inf))
    }
    
    if (input$yearchoice == "2017") {
      totals = hcdata$totHomeless2017
      totalsh = hcdata$totShelt2017
      totalunsh = hcdata$totUnshelt2017
      density = hcdata$density2017
    } else {
      totals = hcdata$totHomeless2016
      totalsh = hcdata$totShelt2016
      totalunsh = hcdata$totUnshelt2016
      density = hcdata$density2016
    }
    
    if (input$yearchoice == "2017") {
      homelessdomain = switch(input$hchoice,
                              "totHomeless" = hcdata$totHomeless2017,
                              "totUnshelt" = hcdata$totUnshelt2017,
                              "totShelt" = hcdata$totShelt2017,
                              "density" = hcdata$density2017
      )
    } else {
      homelessdomain = switch(input$hchoice,
                              "totHomeless" = hcdata$totHomeless2016,
                              "totUnshelt" = hcdata$totUnshelt2016,
                              "totShelt" = hcdata$totShelt2016,
                              "density" = hcdata$density2016
      )
    }
    
    pal = colorBin("YlOrRd", domain = homelessdomain, bins = binsize)
    
    crimeicon = awesomeIcons(
      icon = 'times-circle',
      iconColor = 'black',
      library = 'fa',
      markerColor = 'red'
    )
    
    sheltericon = awesomeIcons(
      icon = 'home',
      iconColor = 'black',
      library = 'fa',
      markerColor = 'green'
    )
    
    callicon = awesomeIcons(
      icon = 'phone',
      iconColor = 'black',
      library = 'fa',
      markerColor = 'blue'
    )
    
    leaflet(bounds) %>% addTiles()  %>%
      addPolygons(color = "#444444", weight = 2, smoothFactor = 0.5,
                  opacity = 0.5,
                  fillColor = ~pal(homelessdomain),
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 3,
                                                      bringToFront = TRUE),
                  popup = paste("<dt>",hcdata$Community_Name, "</dt>",
                                "<dd>","Total Homeless",
                                round(as.numeric(totals)),"</dd>",
                                "<dd>","Total Sheltered",
                                round(as.numeric(totalsh)),"</dd>",
                                "<dd>","Total Unsheltered",
                                round(as.numeric(totalunsh)),"</dd>",
                                "<dd>","Density",
                                round(as.numeric(density)),"</dd>"),
                  group = "Homeless") %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                position = "bottomright") %>%
      
      # Adding Markers for Shelters
      addAwesomeMarkers(icon = sheltericon,
                        lng = as.numeric(shelters$LONGITUDE), 
                        lat = as.numeric(shelters$LATITUDE),
                        popup = paste("<dt>",shelters$NAME, "</dt>",
                                      "<dd>","Hours","</dd>","<dd>",
                                      shelters$HOURS,"</dd>"),
                        clusterOptions=markerClusterOptions(),
                        group = "Shelters") %>%
      setView(lng = -118.2437, lat = 34.05223, zoom = 13) %>%
      
      
      # Adding Markers for Crimes
      addAwesomeMarkers(icon = crimeicon,
                        lng=crimedf()$longitude, 
                        lat=crimedf()$latitude, 
                        popup = paste("<dt>",crimedf()$CRIME.DESCRIPTION.SHORT, "</dt>",
                                      "<dd>","Victim Age:",
                                      round(as.numeric(crimedf()$Victim.Age)),"</dd>",
                                      "<dd>","Victim Gender:",
                                      crimedf()$Victim.Sex,"</dd>",
                                      "<dd>","Victim Race:",
                                      crimedf()$Victim.Descent,"</dd>"),
                        clusterOptions=markerClusterOptions(),
                        group = "Crimes") %>%
      
      # Adding Markers for Calls
      addAwesomeMarkers(icon = callicon,
                        lng=calldf()$LONGITUDE, 
                        lat=calldf()$LATITUDE, 
                        popup = paste("<dt>",calldf()$NCNAME, "</dt>",
                                      "<dd>","Request Source:",
                                      calldf()$REQUESTSOURCE,"</dd>"),
                        clusterOptions=markerClusterOptions(),
                        group = "311 Calls") %>%
      
      # Layer Controls
      addLayersControl(
        overlayGroups = c("Shelters","Homeless","Crimes", "311 Calls"),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomright"
      ) %>% 
      hideGroup("Shelters") %>%
      hideGroup("311 Calls")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
