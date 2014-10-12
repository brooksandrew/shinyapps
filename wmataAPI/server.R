## reactiveTimer. See ?reactiveTimer
#setwd('/Users/ajb/Documents/shiny/nextbus')


baseurl <- 'http://api.wmata.com/Bus.svc/json/jBusPositions?routeId='
endurl <- '&includingVariations=true&lat=0&lon=0&radius=0&api_key='
key <- 'x42rp9qg6jjjydn2u8ng8stx'

shinyServer(function(input, output, session) {
  
  ## GET DATA FROM API
  data <- reactive({
    route = input$busid 
    request <- paste(baseurl, route, endurl, key, sep='')
    temp <- getURL(URLencode(request), ssl.verifypeer=F)
    busdf <- data.frame(fromJSON(temp, simplifyVector=T))
  })
  
  ## MAP
  output$mymap <- renderPlot ({
    input$updateid
    busdf <- data()
    
    map <- get_map(location = c(busdf$BusPositions.Lon[1], busdf$BusPositions.Lat[1]), zoom=13)
    mapPoints <- ggmap(map) + 
      geom_point(aes(x=BusPositions.Lon, y=BusPositions.Lat), data=busdf, alpha=0.85, size=7) +
      ylab('') + xlab('') + theme(axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
    
    plot(mapPoints)
  })
  
  ## TEXT
  output$mytext <- renderText ({
    input$updateid
    busdf <- data()
    busdf[1,1]
  })
  
  ## DATATABLE
  output$mytable <- renderDataTable ({
    input$updateid
    df2print <- data()
    df2print <- df2print[,1:3]
  })
  
  
  output$stopsid <- renderUI({
    requestBusStops <- paste('http://api.wmata.com/Bus.svc/json/jRouteDetails?routeId=', input$busid, '&date=', Sys.Date(), '&api_key=', key, sep='')
    
    requestBusStops <- paste('http://api.wmata.com/Bus.svc/json/jRouteDetails?routeId=', '64', '&date=', Sys.Date(), '&api_key=', key, sep='')
    temp <- getURL(URLencode(requestBusStops), ssl.verifypeer=F)
    stops <- fromJSON(temp, simplifyVector=T)
    stopsdf <- stops$Direction0$Stops
    selectInput("stops", "Choose a bus stop", stopsdf$Name)
                       
  })
  
  
})

