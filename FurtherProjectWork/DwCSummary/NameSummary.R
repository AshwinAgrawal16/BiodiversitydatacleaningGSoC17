library(rgbif)

d1 <- occ_data(
  country = "AU",     # Country code for australia
  classKey= 358,      # Class code for mammalia
  from = 'gbif',
  limit=50000,
  minimal=FALSE,
  hasCoordinate = T
  
)

X<-read.csv(file = "C:\\yourfile.csv")
X<-d1$data


d2 <- occ_data(
  country = "AU",     # Country code for australia
  classKey= 358,      # Class code for mammalia
  from = 'gbif',
  limit=50000,
  minimal=FALSE,
  hasCoordinate = T
  
)

c1<-ddply(X,~name,summarise,number_of_distinct_orders=length((name)))
typeof(c1)
View(c1)
nrow(c1)
v1<-max(c1[,2])
v2<-min(c1[,2])
print(c1[c1$number_of_distinct_orders==v1,])
print(c1[c1$number_of_distinct_orders==v2,])
