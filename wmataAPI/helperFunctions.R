## HELPER FUNCTIONS #####

findClosestStop <- function(allStops, myStop) {
  dist <- sapply(2:nrow(allStops), function(x) distHaversine(c(allStops$Lon[x], allStops$Lat[x]), myStop))
  closestStop <- allStops[which(dist==min(dist)),]
  return(closestStop)
}

closestStop2myStop <- function(busStop, myStop, allStops) {
  allStops2 <- allStops[(which(allStops$StopID==busStop)):(which(allStops$StopID==myStop)),]
  allStops2$dist[2:nrow(allStops2)] <- sapply(2:nrow(allStops2), function(x) 
                                            distHaversine(c(allStops2$Lon[x], allStops2$Lat[x]), c(allStops2$Lon[x-1], allStops2$Lat[x-1])))
  allStops2$dist[1] <- 0
  cumdist <- cumsum(allStops2$dist)[nrow(allStops2)]
  numStops <- nrow(allStops2)
  return(list('cumdist'=cumdist, 'numStops'=numStops, 'allStops2'=allStops2))
}

