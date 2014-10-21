
##LOADING PACKAGES ####################################################
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
pkglist <- c('shiny', 'igraph', 'gplots', 'rgl', 'shinyRGL') 
for(p in pkglist) if(is.installed(p)==F) install.packages(p, character.only=T)
for(p in pkglist) require(p, character.only=T) 
# require('shiny')
# require('igraph')
# require('gplots')

#########################################################################
## DOWNLOADS DATA FROM A DIRECTORY INTO A LIST OF DATA FRAMES ###########
#########################################################################
dir2dfList <- function(dfdir, ext='.txt', exclude=NULL, printdf=T, ...) {
  # get list of .txt text files in directory
  setwd(dfdir)
  tables <- list.files()[sapply(list.files(), function(x) substr(x,nchar(x)-3, nchar(x)))==ext]
  tableNames <- sapply(tables, function(x) substr(x,0, nchar(x)-4), USE.NAMES=F)
  
  # create list of dfs from directory
  dfL <- list()
  for(i in 1:length(tables)) {
    dfL[[tableNames[i]]] <- read.delim(tables[i], ...)
    dfL[[tableNames[i]]] <- dfL[[tableNames[i]]][,!names(dfL[[tableNames[i]]]) %in% exclude]
    if(printdf==T) print(paste(tableNames[i], nrow(dfL[[tableNames[i]]]), Sys.time()))
  }
  
  return(dfL)
}

###############################################################################
## takes list of Data frames and makes igraph network  ########################
###############################################################################
dfL2network <- function(dfL, islands=T){
  # create adjacency list
  edgeL <- data.frame(v1=as.character(), v2=as.character(), v3=as.character(), stringsAsFactors=F)
  k<-1
  for(i in 1:length(dfL)){
    for(j in 1:length(dfL)){
      commonv <- intersect(names(dfL[[i]]), names(dfL[[j]]))
      if(length(commonv)>0){
        for(cv in 1:length(commonv)){
          edgeL[k,1] <- names(dfL)[i]
          edgeL[k,2] <- names(dfL)[j]
          edgeL[k,3] <- commonv[cv]
          k<-k+1
        }
      }    
    }
  }
  
  ## creating graph from adjacency
  edgeL <- edgeL[edgeL[,1]!=edgeL[,2],]
  g <- graph.data.frame(edgeL[,c(1,2)], directed=F)
  if(islands==T) {
    xvars <- unlist(lapply(dfL, names),use.names=F)
    keys <- unique(xvars[duplicated(xvars)==T])
    islandtab <- names(dfL)[sapply(dfL, function(x) is.na(table(names(x) %in% keys)['TRUE']))]
    g <- g + vertices(islandtab)
  }
  E(g)$color <- 'gray'
  E(g)$name <- edgeL[,3]
  
  return(g)
}

#######################################################
## DETERMINES STRENGTH OF KEYS ########################
#######################################################
isKey <- function(dfL, xvar) {
  
  tabNames <- lapply(dfL, names)
  tabs <- names(which(lapply(tabNames, function(x) xvar %in% x)==T))
  mat <- matrix(nrow=length(tabs), ncol=length(tabs))
  ii <- 1; 
  for(i in tabs){
    iivar <- dfL[[i]][,xvar]
    jj <- 1
    for(j in tabs){
      jjvar <- dfL[[j]][,xvar]
      stop
      mat[jj,ii] <- sum(jjvar %in% iivar)/length(jjvar)
      jj<-jj+1
    }
    ii<-ii+1
  }
  
  mat[is.na(mat)] <- 0
  colnames(mat) <- tabs
  rownames(mat) <- tabs
  return(mat)
  
}
Rprof('test.out')
a<-isKey(dfL2, xvar)
Rprof(NULL)
summaryRprof('test.out')
###################################################################################
## MAKE ATTRIBUTE FUNCTION FOR G (used dynamically in shiny  ######################
###################################################################################
makeVA <- function(g, df, id, att) {
  x <- data.frame(as.character(V(g)$name))
  names(x) <- 'var1'
  x$var2 <- ''
  for(i in 1:nrow(x)) {
    nn <- as.character(x[i,1])
    line2eval <- paste(df, '$', att, '[match(', '\'', nn, '\'', ', ', df, '$', id, ')]', sep='')  
    ans <- eval(parse(text=line2eval))
    x[i,2] <- ans
  }
  return(x[,2])
}

if(1==0){
  dfn<-data.frame(keysInTab)
  dfn$vnames <- row.names(dfn)
  V(g)$keysin <- as.numeric(makeVA(g, df='dfn', id='vnames', att='keysInTab'))
}


###################################################################################
## PUTTING IT ALL TOGETHER... will call all necessary functions. ##################
## Although in practice, might be safer to run parts of this function separately ##
###################################################################################
db2shiny <- function(thedir, Aext='.txt', Anrows=10, Aislands=T, keys=T, Aexclude=NULL) {
  if(!'dfL' %in% ls(name='.GlobalEnv')) dfL <<- dir2dfList(thedir, ext=Aext, nrows=Anrows, exclude=Aexclude)
  g <<- dfL2network(dfL,islands=Aislands)
  if(keys==T){
    commonv <- unlist(lapply(dfL, names),use.names=F)
    commonv <- unique(commonv[duplicated(commonv)==T])
    if(!'keyL' %in% ls(name='.GlobalEnv')) {keyL <<- sapply(commonv, function(x) isKey(dfL, x))}
  }
  
  require('shiny')
  runApp('S:/OI/abrooks/R/functions/tableNet/shiny')
}



## ###################################
## EXAMPLES ##########################
######################################
showExample <- 0
KeyExample <- 0

## IsKey Example
if(KeyExample==1){
  ## example 1
  commonv <- unlist(lapply(dfL, names),use.names=F)
  commonv <- unique(commonv[duplicated(commonv)==T])
  system.time(keyL <- sapply(commonv, function(x) isKey(dfL, x)))
  
  ## example 2
  check <- isKey(dfL, 'COMPLAINTID')
  require('gplots')
  
  ## example 3
  myPalette <- colorRampPalette(c("white", "firebrick"))(n = 20)
  checklab <- round(keyL[['COMPLAINTID']],2)
  a<-heatmap.2(keyL[['COMPLAINTID']], trace='none', dendrogram='none', Rowv=F, Colv=F, margins=c(15,15), 
               col=myPalette, cellnote=checklab, notecol='black', cexRow=1)
}



if(showExample==1){
  thedir <- 'R:/IRAT_Misc/CSEC Model/Employee/EEOC/EEO Extract 2-22-13'
  dfL2 <- dir2dfList(thedir, ext='.txt', nrows=-1)
  #save(dfL2, file='S:/OI/abrooks/R/functions/tableNet/dfL.Rdata')
  #load('S:/OI/abrooks/R/functions/tableNet/dfL.Rdata')
  g <- dfL2network(dfL2)
  
  V(g)$size <- igraph::degree(g, V(g)$name, mode='total')/2 +5
  E(g)$color <- rep('gray', length(E(g)))
  #E(g)$color[E(g)$name=='USERID'] = 'red'
  plot(g, layout=layout.circle, vertex.label.cex=.7, edge.curved=F)
  
  require('shiny')
  runApp('S:/OI/abrooks/R/functions/tableNet/shiny')
  
  rm(list=ls())
  thedir <- 'R:/IRAT_Misc/CSEC Model/Employee/EEOC/EEO Extract 2-22-13'
  system.time(db2shiny(thedir, Anrows=100, Aexclude=c("DESCRIPTION","CREATEDTS","LUTS","CREATEDBY","LUBY","DELETED")))
}





