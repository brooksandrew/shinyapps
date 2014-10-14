###########################################
## HELPER FUNCTIONS #######################
###########################################
findClosestStop <- function(allStops, myStop) {
  dist <- sapply(2:nrow(allStops), function(x) distHaversine(c(allStops$Lon[x], allStops$Lat[x]), myStop))
  closestStop <- allStops[which(dist==min(dist)),]
  return(closestStop)
}

closestStop2myStop <- function(busStop, myStop, allStops) {
  allStops2 <- allStops[(which(allStops$StopID==busStop)):(which(allStops$StopID==myStop)),]
  if(nrow(allStops2)>=2){
    allStops2$dist[2:nrow(allStops2)] <- sapply(2:nrow(allStops2), function(x) 
                                              distHaversine(c(allStops2$Lon[x], allStops2$Lat[x]), c(allStops2$Lon[x-1], allStops2$Lat[x-1])))
    allStops2$dist[1] <- 0
    cumdist <- cumsum(allStops2$dist)[nrow(allStops2)]
    numStops <- nrow(allStops2)-1
  } else {
    cumdist=0; numStops=0
  }
  return(list('cumdist'=cumdist, 'numStops'=numStops, 'allStops2'=allStops2))
}


## #########################################
## TESTING FUNCTIONS #######################
############################################

if(1==0) {
  key <- 'x42rp9qg6jjjydn2u8ng8stx'
  
  ## BUS POSITIONS
  route = '64'
  request <- paste('http://api.wmata.com/Bus.svc/json/jBusPositions?routeId=', route, '&includingVariations=true&lat=0&lon=0&radius=0&api_key=', key, sep='')
  temp <- getURL(URLencode(request), ssl.verifypeer=F)
  dfbus <- data.frame(fromJSON(temp, simplifyVector=T))
  
  ## BUS STOPS
  requestBusStops <- paste('http://api.wmata.com/Bus.svc/json/jRouteDetails?routeId=', route, '&date=', Sys.Date(), '&api_key=', key, sep='')
  temp <- getURL(URLencode(requestBusStops), ssl.verifypeer=F)
  dfstopsL <- fromJSON(temp, simplifyVector=T)
  dfstops <- dfstopsL$Direction0$Stops
  
  ## BUS PREDICTIONS
  stopID <- dfstops[sample(1:nrow(dfstops), 1),'StopID']
  request <- paste('http://api.wmata.com/NextBusService.svc/json/jPredictions?StopID=', stopID, '&api_key=', key, sep='')
  temp <- getURL(URLencode(request), ssl.verifypeer=F)
  dfpred <- data.frame(fromJSON(temp, simplifyVector=T))
  
  head(dfbus)
  head(dfpred)
  head(dfstops)
  
  ## TESTING HELPER FUNCTIONS ##
  ## setwd('C:/Users/abrooks/Documents/github/shinyapps/wmataAPI')
  
  stopLatLon <- c(dfstops$Lon[dfstops$StopID==stopID], dfstops$Lat[dfstops$StopID==stopID])
  closestStop <- findClosestStop(dfstops, stopLatLon)
  closestStop2myStop(closestStop$StopID[1], stopID, dfstops)
  
}




