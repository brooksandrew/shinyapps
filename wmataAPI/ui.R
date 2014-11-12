library('RCurl')
library('jsonlite')
library('ggmap')
library('shiny')
library('geosphere')
library('stringdist')

## GETTING BUS ROUTES
key <- scan('wmataKey.txt', what='character')
requestBusRoutes <- paste('http://api.wmata.com/Bus.svc/json/jRoutes?api_key=', key, sep='')
tempBusRoutes <- getURL(URLencode(requestBusRoutes), ssl.verifypeer=F)
busesRoutes <- data.frame(fromJSON(tempBusRoutes, simplifyVector=T))
buses <- busesRoutes[,2]

shinyUI(fluidPage(
  titlePanel("Dude, Where's my Bus?"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput('busid', 'Choose a bus:', buses, selected='64'),
      uiOutput("stopsid"),
      uiOutput("dirid"),
      actionButton('updateid', 'Refresh data')
    ),
    
    mainPanel(
        tabPanel("Map", plotOutput("mymap", "600px", "600px")),
        tabPanel("Table", dataTableOutput("mytable")),
        tabPanel("geoTable", dataTableOutput("mygeotable"))
    )  
    
  )
))



