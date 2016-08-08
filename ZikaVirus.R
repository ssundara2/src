#Load required I/O and mapping libraries
library(RCurl)
library(RJSONIO)
library(plyr)
library(mapboxer)
library(gsheet)
library(leaflet)

#utility function to return the GeoCoded 
url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}
#geocode a location that is not in lat/long form
geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}

zika<-read.csv(text=gsheet2text("https://docs.google.com/spreadsheetsd/1_S6PA5L32Mq7H_cfp9NAe-Y8-17hNer2OMyb3hVPTvU/edit#gid=1516521608",format='csv'))
zika2<-zika[-1,];
address<-lapply(zika2$Country...territory,geoCode)
m1 <- do.call(rbind,lapply(address, function(x) if(is.null(x)) NA else c(x)))
address2<-as.data.frame(m1)
names(address2)<-c("latitude","longitude","type","description")
df<-cbind(address2,zika2$Cases);
m<- leaflet(data=df) %>% addTiles() %>% addMarkers(~longitude,~latitude,popup=as.character(df$cases));