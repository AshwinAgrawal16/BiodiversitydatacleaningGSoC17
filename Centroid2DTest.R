#Flagging records based on extrenal csv file containing centroid coordinates of countries downloaded from
# https://developers.google.com/public-data/docs/canonical/countries_csv


library(ggplot2)
library(geosphere)
library(rgbif)
library(mapr)
library(ggmap)



CentroidCheck <- function(dat,country_code,radius=1000000,precision=FALSE,centroids.ref=FALSE){
  if(centroids.ref==FALSE)
  centroids.ref<-read.csv(file="centroid.csv",header = TRUE)

 
  # Calculating distance between centroid and data points
  if(precision==FALSE){
    distance <- distm(dat$data[,c(4,3)], centroids.ref[centroids.ref$country == country_code,c(3,2)], fun = distHaversine)}
  else {
    distance <- distm(dat$data[,c(4,3)], centroids.ref[centroids.ref$country == country_code,c(3,2)], fun = distVincentyEllipsoid)}
  
  distance<-distance[,!colSums(is.na(distance))>0]
  
  # Adding distance field
  dat$data <- cbind(dat$data,centroid_dist=distance)
  
  # Removing data outside the set radius
  dat$data = dat$data[dat$data$centroid_dist < radius,]
  
  return(dat)
}


#Eaxmple plotting of reptile US data.
reptiliaUS<- occ_search(taxonKey = 358,country = "US", limit=1000,hasCoordinate = TRUE)
reptiliaUS_centroid <- CentroidCheck(reptiliaUS,"US")
map_ggmap(reptiliaUS_centroid,zoom = 6)
