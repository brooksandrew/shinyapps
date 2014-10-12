require('RCurl')
require('jsonlite')
require('ggmap')
require('shiny')
require('ggmap')

key <- 'x42rp9qg6jjjydn2u8ng8stx'
request <- paste('http://api.wmata.com/Bus.svc/json/jRoutes?api_key=', key, sep='')
temp <- getURL(URLencode(request), ssl.verifypeer=F)
busesdf <- data.frame(fromJSON(temp, simplifyVector=T))
buses <- busesdf[,2]

shinyUI(fluidPage(
  titlePanel("64 Bus Map"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('busid', 'Choose a bus', buses, selected='64')
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", plotOutput("mymap", "600px", "600px")),
        tabPanel("Text", textOutput("mytext"))
      )
    )  
    
  )
))



