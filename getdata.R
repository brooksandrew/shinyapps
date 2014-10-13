require('RCurl')
require('jsonlite')
require('ggmap')

## setting up request
baseurl <- 'http://api.wmata.com/Bus.svc/json/jBusPositions?routeId='
key <- 'x42rp9qg6jjjydn2u8ng8stx'
route <- '64' 
endurl <- '&includingVariations=true&lat=0&lon=0&radius=0&api_key='

## hit API once
request <- paste(baseurl, route, endurl, key, sep='')
temp <- getURL(URLencode(request), ssl.verifypeer=F)
dl <- data.frame(fromJSON(temp, simplifyVector=T))

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

route = '64'
request <- paste('http://api.wmata.com/Bus.svc/json/jBusPositions?routeId=', route, '&includingVariations=true&lat=0&lon=0&radius=0&api_key=', key, sep='')
temp <- getURL(URLencode(request), ssl.verifypeer=F)
busdf <- data.frame(fromJSON(temp, simplifyVector=T))
busdf <- busdf[busdf$BusPositions.DirectionText=='SOUTH',]

require('ggmap')
map <- get_map(location = c(busdf$BusPositions.Lon[1], busdf$BusPositions.Lat[1]), zoom=13)
mapPoints <- ggmap(map) + 
  geom_point(aes(x=BusPositions.Lon, y=BusPositions.Lat), data=busdf, alpha=1, size=7)

plot(mapPoints)

#######################################################
## getBusRoutes #######################################
#######################################################

request <- paste('http://api.wmata.com/Bus.svc/json/jRoutes?api_key=', key, sep='')
temp <- getURL(URLencode(request), ssl.verifypeer=F)
buses <- fromJSON(temp, simplifyVector=T)

#######################################################
## Caclulate distance between points ##################
#######################################################

requestBusStops <- paste('http://api.wmata.com/Bus.svc/json/jRouteDetails?routeId=', '64', '&date=', Sys.Date(), '&api_key=', key, sep='')
temp <- getURL(URLencode(requestBusStops), ssl.verifypeer=F)
stopsL <- fromJSON(temp, simplifyVector=T)

stops <- stopsL$Direction0$Stops

map <- get_map(location = c(stops$Lon[1], stops$Lat[1]), zoom=13)
mapPoints <- ggmap(map) + 
  geom_point(aes(x=Lon, y=Lat), data=stops, alpha=0.85)
plot(mapPoints)

library('geosphere')
stops$dist[2:nrow(stops)] <- sapply(2:nrow(stops), function(x) distHaversine(c(stops$Lon[x], stops$Lat[x]), c(stops$Lon[x-1], stops$Lat[x-1])))

findClosestStop <- function(allStops, myStop) {
  dist <- sapply(2:nrow(stops), function(x) distHaversine(c(allStops$Lon[x], allStops$Lat[x]), myStop))
  closestStop <- stops[which(dist==min(dist)),]
  return(closestStop)
}

busStop <- findClosestStop(stops, c(dl$BusPositions.Lon[1], dl$BusPositions.Lat[1]))[1,'StopID']
myStop <- findClosestStop(stops, c(dl$BusPositions.Lon[1]-.001, dl$BusPositions.Lat[1]+.05))[1,'StopID']

closestStop2myStop <- function(busStop, myStop, allStops) {
  allStops2 <- allStops[(which(allStops$StopID==busStop)):(which(allStops$StopID==myStop)),]
  dist <- cumsum(allStops2$dist)[nrow(allStops2)]
  numStops <- nrow(allStops2)
  return(list('dist'=dist, 'numStops'=numStops))
}

closestStop2myStop(busStop, myStop, stops)
