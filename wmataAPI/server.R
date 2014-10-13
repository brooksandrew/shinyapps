#setwd('C:/Users/abrooks/Documents/github/shinyapps/wmataAPI/')

key <- 'x42rp9qg6jjjydn2u8ng8stx'

source('helperFunctions.R')

shinyServer(function(input, output, session) {
  
  ## GET BUS POSITIONS FROM API
  data <- reactive({
    route = input$busid 
    request <- paste('http://api.wmata.com/Bus.svc/json/jBusPositions?routeId=', route, '&includingVariations=true&lat=0&lon=0&radius=0&api_key=', key, sep='')
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
    if(input$dirSign==dirs[1]) stopsdf <- stops$Direction0
    else stopsdf <- stops$Direction1
  })
  
  ## CREATING UI: STOPS
  output$stopsid <- renderUI({
    input$updateid
    stopsdf <- stopsdf()$Stops
    selectInput("stops", "Choose a bus stop:", stopsdf$Name)   
  })
  
  ## GET PREDICTIONS FROM API
  preddf <- reactive ({
    stopsdf <- stopsdf()$Stops
    stopID <- stopsdf$StopID[match(input$stops, stopsdf$Name)]
    request <- paste('http://api.wmata.com/NextBusService.svc/json/jPredictions?StopID=', stopID, '&api_key=', key, sep='')
    temp <- getURL(URLencode(request), ssl.verifypeer=F)
    preddf <- data.frame(fromJSON(temp, simplifyVector=T))
  })
  
  ## MAP
  output$mymap <- renderPlot ({
    input$updateid
    busdf <- data()
    
    ## plotting just buses going in the right direction
    strdist <- stringdist(input$dirSign, busdf$BusPositions.TripHeadsign)
    busdf <- busdf[which(strdist==min(strdist)), ]
    
    map <- ggmap::get_map(location = c(busdf$BusPositions.Lon[1], busdf$BusPositions.Lat[1]), zoom=13)
    mapPoints <- ggmap::ggmap(map) + 
      geom_point(aes(x=BusPositions.Lon, y=BusPositions.Lat), data=busdf, alpha=0.85, size=7) +
      ylab('') + xlab('') + theme(axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())
    
    plot(mapPoints)
  })  
  
  ##############################
  ## DATATABLE OF PREDICTIONS ##
  ##############################
  
  output$mytable <- renderDataTable ({
    input$updateid
    pred2print <- preddf()
    
    if(nrow(pred2print)==0) {pred2print <- data.frame("No Predictions Available"); names(pred2print)[1] <- ''
    } else {
      pred2print <- pred2print[,c('Predictions.DirectionText', 'Predictions.Minutes', 'Predictions.RouteID')]
      names(pred2print)[names(pred2print)=='Predictions.DirectionText'] <- 'Direction'
      names(pred2print)[names(pred2print)=='Predictions.Minutes'] <- 'Prediction'
      names(pred2print)[names(pred2print)=='Predictions.RouteID'] <- 'Bus'
    }
  
    pred2print
  })
  
  ##############################
  ## DATATABLE OF GEO PREDICTIONS ##
  ##############################
  
  output$mygeotable <- renderDataTable ({
  
    preddf <- preddf()
    stopsdf <- stopsdf()
    #stops <- stops()
    datadf <- data()
    
    ## taking just BUS GPS on the selected route
    strdist <- stringdist(input$dirSign, datadf$BusPositions.TripHeadsign)
    closestBus <- datadf[which(strdist==min(strdist)), ]
    deleted <- c()
    
    ## for each bus on the radar, calculate # of stops to go and how distance away
    for(i in 1:nrow(closestBus)) {
      busLatLon <- c(closestBus$BusPositions.Lon[i], closestBus$BusPositions.Lat[i])
      stopID <- stopsdf$Stops$StopID[match(input$stops, stopsdf$Stops$Name)]      
      stopsNotArrived <- stopsdf$Stops[1:which(stopsdf$Stops$StopID==stopID),]
      closeStop <- findClosestStop(stopsdf$Stops, busLatLon)[1,'StopID']
      print(stopsNotArrived)
      
      ## if there are buses on the radar that haven't passed stopID yet
      if(closeStop %in% stopsNotArrived$StopID) {
        
        closestBus$closeStop[i] <- closeStop
        closestBus$Name[i] <- stopsdf$Stops$Name[match(closeStop, stopsdf$Stops$StopID)]
        busStats <- closestStop2myStop(closeStop, stopID, stopsNotArrived)
        closestBus$dist[i] <- round(busStats$cumdist*(1/1609.34),2)
        closestBus$numStops[i] <- busStats$numStops
      } else {deleted <- c(deleted, i)}
    }
    
    if(length(deleted)>0) closestBus <- closestBus[-deleted,]
    
    if(nrow(closestBus)>0) { 
      closestBus2 <- closestBus[,c('Name', 'dist', 'numStops')]
      names(closestBus2) <- c("Where bus is now", "Miles away", "Stops away")
    } else {closestBus2 <- data.frame(Sorry="No buses on the radar :(")}
    
    
    closestBus2
  })

  
}
)
