## reactiveTimer. See ?reactiveTimer
#setwd('/Users/ajb/Documents/shiny/nextbus')


baseurl <- 'http://api.wmata.com/Bus.svc/json/jBusPositions?routeId='
endurl <- '&includingVariations=true&lat=0&lon=0&radius=0&api_key='
key <- 'x42rp9qg6jjjydn2u8ng8stx'

shinyServer(function(input, output, session) {
  
  data <- reactive({
    route = input$busid 
    request = paste(baseurl, route, endurl, key, sep='')
    temp = getURL(URLencode(request), ssl.verifypeer=F)
    busdf = data.frame(fromJSON(temp, simplifyVector=T))
  })
  
  output$mymap <- renderPlot ({
    input$updateid
    busdf <- data()
    ## just keeping maxRows in busdf   
    
    map <- get_map(location = c(busdf$BusPositions.Lon[1], busdf$BusPositions.Lat[1]), zoom=13)
    mapPoints <- ggmap(map) + 
      geom_point(aes(x=BusPositions.Lon, y=BusPositions.Lat), data=busdf, alpha=0.85, size=7)
    
    plot(mapPoints)
  })
  
  output$mytext <- renderText ({
    input$updateid
    busdf <- data()
    busdf[1,1]
  })
  
})

