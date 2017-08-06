# BiodiversitydatacleaningGSoC17
GSoC 2017 -Biodiversitydatacleaning

Currently there are three tasks in the package:-

# 1) DwC Summary Table:-

This contains 3 funtions and 2 shiny flexboards.

Use cases for testing the functions:-

1) DwC_Summary_Spatial:-

```
suppressMessages(library(rgbif))
suppressMessages(library(sp))
suppressMessages(library(spatialEco))
suppressMessages(library(ggmap))
suppressMessages(library(plyr))
suppressMessages(library(htmlTable))
suppressMessages(library(bdvis))
suppressMessages(library(plotly))
suppressMessages(library(pracma))
suppressMessages(library(xtable))
suppressMessages(library(htmlTable))

d1 <- occ_data(
country = "AU",     # Country code for australia
classKey= 359,      # Class code for mammalia
limit=50000,
hasCoordinate = T
 
)

X<-d1$data


DwC_Summary_spatial(X)
```

2) DwC_Summary_Temporal:-

```
library(rgbif)
library(plyr)
library(htmlTable)
library(bdvis)
library(plotly)

d1 <- occ_data(
country = "AU",     # Country code for australia
classKey= 359,      # Class code for mammalia
limit=50000,
hasCoordinate = T

)

X<-d1$data

DwC_Summary_Temporal(X,MONTH = 0)
DwC_Summary_Temporal(X,MONTH = 2)
DwC_Summary_Temporal(X,YEAR = 2017)
DwC_Summary_Temporal(X,YEAR = 2018)
DwC_Summary_Temporal(X,DAY=10)
```

# 2) Flagging System

This conatains various functions which assist in flagging and filtering of biodiveristy data based on taxonomic, spatial, temporal nad geo-spatial parameters.


1) Taxonomic_Flagging_and_Filtering

```
library(rgbif)

d1 <- occ_data(
country = "AU",     # Country code for australia
classKey= 359,      # Class code for mammalia
limit=50000,
hasCoordinate = T
)

X<-d1$data

Z1<-Taxonomic_Flagging(X,FLAG_NAME = "name") 
View(Z1)

Z2<-Taxonomic_Filtering(X,FLAG_NAME = "name")
View(Z2)
```

2) Taxonomic_Validation_and_Flagging:-

```
library(taxize)
library(rgbif)
library(plyr)

d1 <- occ_data(
country = "AU",     # Country code for australia
classKey= 359,      # Class code for mammalia
limit=50000,
hasCoordinate = T
)

X<-d1$data
CC1<-Taxonomic_validation_Flagging(X,Species_Name = "Bettongia penicillata")
View(CC1)

CC2<-Taxonomic_validation_Flagging(X,Species_Name = "Wallabia bicolor")
View(CC2)
```

3) Spatial_Flags


```
library(sp)
library(raster)
library(XML)
library(lattice)
library(grid)
library(foreign)
library(maptools)
library(rgbif)    
library(dismo)
library(rgdal)
library(utils)
library(foreach)
library(doParallel)
library(doSNOW)
library(rgeos)

# # taxize soap extension for taxize utilizing soap to access the web service
# # by World register of marine species (worms)

library(devtools)

install.packages(c("XMLSchema", "SSOAP"), 
                 repos = c("http://packages.ropensci.org", 
                           "http://cran.rstudio.com"))
devtools::install_github("ropensci/taxizesoap")


library(taxizesoap)



example1<-Spatial_Flags(species_name ="Scardinius erythrophthalmus")
View(example1)
example2<-Spatial_Flags(species_name ="Scardinius erythrophthalmus" ,correct_sign = TRUE)
View(example2)
example3<-Spatial_Flags(species_name ="Scardinius erythrophthalmus" ,correct_sign = TRUE,correct_swap=TRUE)
View(example3)
```

4) Spatial_Flagging_and_Filtering


```
library(rgbif)

d1 <- occ_data(
country = "AU",     # Country code for australia
classKey= 359,      # Class code for mammalia
limit=50000,
hasCoordinate = T
)

X<-d1$data
 
Z1<-Spatial_Flagging(X,FLAG_NAME = "coordinatePrecision") 
View(Z1)

Z2<-Spatial_Filtering(X,FLAG_NAME = "coordinateUncertaintyInMeters")
View(Z2)
```

5) Geo_Spatial_Flagging:

```
d1 <- occ_data(
country = "AU",     # Country code for australia
classKey= 359,      # Class code for mammalia
limit=50000,
hasCoordinate = T
)

X<-d1$data
x<-X[,4:3]
countries<-X$country 
Z<-CountryCheck(x,countries)
View(Z)
```
ToDo:- Add use cases for 3 remaining functions in Geo_Spatial_Flaggging

# 3) Outlier Analysis

Outlier analysis is a major preprocessing task done before any data analysis is done. Biodiversity data is huge in volume therefore this task becomes much more important.
I have used Alpha Hull and Reverse-Jack-Knife to identify the outliers.

1) Outlier_Detection

```
library(alphahull)
library(rgbif)
library(plyr)
library(maptools)
library(raster)
library(biogeo)


d1111 <- occ_data(
country = "AU",     # Country code for australia
classKey= 359,      # Class code for mammalia
limit=50000,
hasCoordinate = T
 
)

X1<-d1111$data
Outlier_Detection(X1)
 
d111 <- occ_data(
country = "US",     # Country code for USA
classKey= 359,      # Class code for mammalia
limit=50000,
hasCoordinate = T
  
)

X2<-d111$data
Outlier_Detection(X2)

d11 <- occ_data(
country = "CN",     # Country code for Cannada
classKey= 359,      # Class code for mammalia
limit=50000,
hasCoordinate = T
  
)

X3<-d11$data
Outlier_Detection(X3)

d1 <- occ_data(
country = "IN",     # Country code for India
classKey= 359,      # Class code for mammalia
limit=50000,
hasCoordinate = T
  
)

X4<-d1$data
Outlier_Detection(X4) 

d <- occ_data(
country = "JP",     # Country code for Japan
classKey= 359,      # Class code for mammalia
limit=50000,
hasCoordinate = T
  
)

X5<-d$data
Outlier_Detection(X5)
```


More functions will be added in future for Biodiveristy Data Cleaning.
Hope you find these functions usefull.!!!!
