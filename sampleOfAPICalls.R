key <- 'x42rp9qg6jjjydn2u8ng8stx'


## BUS POSITIONS
route = '5A'
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