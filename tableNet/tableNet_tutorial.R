##url <- 'http://seanlahman.com/files/database/lahman2012-csv.zip'

require('shiny')
require('igraph')

source('C:/Users/abrooks/Documents/github/shinyapps/tableNet/tableNet.R')
source('C:/Users/abrooks/Documents/github/Rsenal/R/shiny_tableNet.R')
thedir <- 'C:/Users/abrooks/Documents/github/shinyapps/tableNet/data/'

dfL2 <- dir2dfList(thedir, ext='.csv', nrows=-1, sep=',', stringsAsFactors=F)

commonv <- unlist(lapply(dfL2, names),use.names=F)
commonv <- unique(commonv[duplicated(commonv)==T])
keyL2 <- sapply(commonv, function(x) isKey(dfL2, x)) #20 seconds

if(1==0) runApp('C:/Users/abrooks/Documents/github/shinyapps/tableNet/shiny')

# graph
g <- dfL2network(dfL2)
V(g)$size <- igraph::degree(g, V(g)$name, mode='total')/2 +5
E(g)$color <- rep('gray', length(E(g)))
plot(g, layout=layout.circle, vertex.label.cex=.7, edge.curved=F)

## testing shiny function
tableNet(dfL2, keyL2)
