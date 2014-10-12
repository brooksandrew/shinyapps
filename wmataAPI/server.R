## reactiveTimer. See ?reactiveTimer
#setwd('/Users/ajb/Documents/shiny/nextbus')

## setting up data to be reactive
  
  maxRows <- 100
  baseurl <- 'http://api.wmata.com/Bus.svc/json/jBusPositions?routeId='
  route <- '64' 
  endurl <- '&includingVariations=true&lat=0&lon=0&radius=0&api_key='
  key <- 'x42rp9qg6jjjydn2u8ng8stx'
  
  request <- paste(baseurl, route, endurl, key, sep='')
  temp <- getURL(URLencode(request), ssl.verifypeer=F)
  busdf <- data.frame(fromJSON(temp, simplifyVector=T))

shinyServer(function(input, output, session) {
  wait <- reactiveTimer(50000, session)
  
  output$mymap <- renderPlot ({
    wait()  
    temp <- getURL(URLencode(request), ssl.verifypeer=F)
    dl <<- data.frame(fromJSON(temp, simplifyVector=T))
    busdf <<- rbind(busdf, dl)
    
    ## just keeping maxRows in busdf
    if(nrow(busdf)>maxRows) busdf <<- busdf[(nrow(busdf)-maxRows):nrow(busdf), ]
    
    map <- get_map(location = c(busdf$BusPositions.Lon[1], busdf$BusPositions.Lat[1]), zoom=13)
    mapPoints <- ggmap(map) + 
      geom_point(aes(x=BusPositions.Lon, y=BusPositions.Lat), data=busdf, alpha=0.25)
    
    plot(mapPoints)
  })
  
  output$mytext <- renderText ({
    wait()
    busdf[nrow(busdf),1]
  })
  
})

