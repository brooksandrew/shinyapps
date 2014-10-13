require('RCurl')
require('jsonlite')
require('ggmap')

## setting up request
baseurl <- 'http://api.wmata.com/Bus.svc/json/jBusPositions?routeId='
route <- '64' 
endurl <- '&includingVariations=true&lat=0&lon=0&radius=0&api_key='

## hit API once
request <- paste(baseurl, route, endurl, key, sep='')
temp <- getURL(URLencode(request), ssl.verifypeer=F)
dl <- fromJSON(temp, simplifyVector=T)

## make a function to hit API every x seconds and save results to data.frame with max of 10000 rows
saveTrips <- function(request, maxRows=1000, seconds=1) {
  
  ## create busdf if doesn't exist in global env
  if(!'busdf' %in% ls(globalenv())) {
    temp <- getURL(URLencode(request), ssl.verifypeer=F)
    busdf <<- data.frame(fromJSON(temp, simplifyVector=T))
  }
  
  ## keep adding rows to busdf
  while(1==1) {
    Sys.sleep(seconds)
    temp <- getURL(URLencode(request), ssl.verifypeer=F)
    dl <<- data.frame(fromJSON(temp, simplifyVector=T))
    busdf <<- rbind(busdf, dl)
    
    ## just keeping maxRows in busdf
    if(nrow(busdf)>maxRows) busdf <<- busdf[(nrow(busdf)-maxRows):nrow(busdf), ]
  }
}

saveTrips(request)

#######################################################
## plot bus positions on map ##########################
#######################################################

require('ggmaps')
map <- get_map(location = c(busdf$BusPositions.Lon[1], busdf$BusPositions.Lat[1]), zoom=13)
mapPoints <- ggmap(map) + 
  geom_point(aes(x=BusPositions.Lon, y=BusPositions.Lat), data=busdf, alpha=0.25)

plot(mapPoints)

#######################################################
## getBusRoutes #######################################
#######################################################

key <- 'x42rp9qg6jjjydn2u8ng8stx'
request <- paste('http://api.wmata.com/Bus.svc/json/jRoutes?api_key=', key, sep='')
temp <- getURL(URLencode(request), ssl.verifypeer=F)
buses <- fromJSON(temp, simplifyVector=T)
