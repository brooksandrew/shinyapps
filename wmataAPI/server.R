## reactiveTimer. See ?reactiveTimer
#setwd('/Users/ajb/Documents/shiny/nextbus')


baseurl <- 'http://api.wmata.com/Bus.svc/json/jBusPositions?routeId='
endurl <- '&includingVariations=true&lat=0&lon=0&radius=0&api_key='
key <- 'x42rp9qg6jjjydn2u8ng8stx'

shinyServer(function(input, output, session) {
  
  ## GET BUS POSITIONS FROM API
  data <- reactive({
    route = input$busid 
    request <- paste(baseurl, route, endurl, key, sep='')
    temp <- getURL(URLencode(request), ssl.verifypeer=F)
    busdf <- data.frame(fromJSON(temp, simplifyVector=T))
  })
    
  ## GET BUS STOPS FROM API
  stops <- reactive ({
    requestBusStops <- paste('http://api.wmata.com/Bus.svc/json/jRouteDetails?routeId=', input$busid, '&date=', Sys.Date(), '&api_key=', key, sep='')
    temp <- getURL(URLencode(requestBusStops), ssl.verifypeer=F)
    stops <- fromJSON(temp, simplifyVector=T)
  })
    
  ## CREATING UI: DIRECTION
  output$dirid <- renderUI({
    input$updateid
    stops <- stops()
    radioButtons('dirSign', 'Choose a Direction:', unique(c(stops$Direction0$TripHeadsign, stops$Direction1$TripHeadsign)))
  })
  
  ## CREATING STOPSDF
  stopsdf <- reactive ({
    input$updateid
    stops <- stops()
    dirs <- c(stops$Direction0$TripHeadsign, stops$Direction1$TripHeadsign)
    print(input$dirSign)
    if(input$dirSign==dirs[1]) stopsdf <- stops$Direction0$Stops
    else stopsdf <- stops$Direction1$Stops
  })
  
  ## CREATING UI: STOPS
  output$stopsid <- renderUI({
    input$updateid
    stopsdf <- stopsdf()
    selectInput("stops", "Choose a bus stop:", stopsdf$Name)   
  })
  
  ## GET PREDICTIONS FROM API
  preddf <- reactive ({
    stopsdf <- stopsdf()
    stopID <- stopsdf$StopID[match(input$stops, stopsdf$Name)]
    request <- paste('http://api.wmata.com/NextBusService.svc/json/jPredictions?StopID=', stopID, '&api_key=', key, sep='')
    temp <- getURL(URLencode(request), ssl.verifypeer=F)
    preddf <- data.frame(fromJSON(temp, simplifyVector=T))
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
  
  ##############################
  ## DATATABLE OF PREDICTIONS ##
  ##############################
  
  output$mytable <- renderDataTable ({
    input$updateid
    stopsdf <- stopsdf()
    pred2print <- preddf()
    pred2print <- pred2print[,c('Predictions.DirectionText', 'Predictions.Minutes', 'Predictions.RouteID')]
    names(pred2print)[names(pred2print)=='Predictions.DirectionText'] <- 'Direction'
    names(pred2print)[names(pred2print)=='Predictions.Minutes'] <- 'Prediction'
    names(pred2print)[names(pred2print)=='Predictions.RouteID'] <- 'Bus'
    pred2print$time <- Sys.time()

    pred2print
  })
  
  
  
})

