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
