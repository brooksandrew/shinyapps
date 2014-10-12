require('RCurl')
require('jsonlite')
require('ggmap')
require('shiny')
require('ggmap')

key <- 'x42rp9qg6jjjydn2u8ng8stx'
requestBusRoutes <- paste('http://api.wmata.com/Bus.svc/json/jRoutes?api_key=', key, sep='')
tempBusRoutes <- getURL(URLencode(requestBusRoutes), ssl.verifypeer=F)
busesRoutes <- data.frame(fromJSON(tempBusRoutes, simplifyVector=T))
buses <- busesRoutes[,2]

shinyUI(fluidPage(
  titlePanel("64 Bus Map"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('busid', 'Choose a bus', buses, selected='64'),
      actionButton('updateid', 'Refresh data')
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", plotOutput("mymap", "600px", "600px")),
        tabPanel("Text", textOutput("mytext"))
      )
    )  
    
  )
))



