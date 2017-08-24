# BiodiversitydatacleaningGSoC17
# GSoC 2017 -Biodiversitydatacleaning


# Introduction

There are an increasing number of scientists using R for their data analyses, however, the skill set required to handle biodiversity data in R, is considerably varies. Since, users need to retrieve, manage and assess high volume data with complex structure (Darwin Core standard, DwC); only users with an extremely sound R programming background can attempt this. Recently, various R packages dealing with biodiversity data and specifically data cleaning have been published (e.g. scrubr, biogeo, rgeospatialquality, assertr , and taxize). Though numerous new procedures are now available, implementing them requires users to prepare the data according to the formats of each of these packages and learning each R package. Dealing with the integration related tasks which would facilitate the data format conversions and smooth execution of all the available data cleaning functions from various packages, is being addressed in another GSOC project (link). The purpose of my project is to identify and address missing crucial functionalities for handling biodiversity (big) data in R. Properly addressing these gaps will hopefully enable us to offer a more complete framework for data quality assessment in R.

# Proposed components

# 1. Standardized flagging system:

Biodiversity quality assessment is based upon a user capability to execute variety of data checks. Thus, a well-designed flagging system will allow users to easily manage their data checks result, and facilitate control on the desired quality level on one hand, and user flexibility on the other hand. I will assess several approaches for designing such a system, factoring comprehensibility and programming complexity.

Any insights and ideas regarding this task will be highly appreciated (please create a github issues).

# 2. A DwC summary table:

When dealing with high complexity and high-volume data, summary statistics of different fields and categories, can have an immense value. I will develop a DwC summary table based on DwC fields and vocabulary. First, I will explore different R packages dealing with descriptive statistics and table visualizations. Then, I will map key DwC data fields and key categories for easy faceting of the summary table. In addition, the developed framework can be used to enhance the flagging system, by utilizing it unique functionality to summarize the data quality checks results.

# 3. Outliers analysis:

Identifying spatial, temporal, and environmental outliers can single out erroneous records. However, identifying an outlier is a subjective exercise, and not all outliers are errors.  I will develop a set of functions which will aid in detection of outliers. Various statistical methods and techniques will be evaluated (e.g. Reverse Jackknife, Standard Deviations from the Mean, Alphahull).

# 4. Developing new data quality checks and procedure

I will identify critically missing spatial, taxonomic and temporal data cleaning routines, factoring users need level and programming complexity.  Ideas and needs regarding this task will be highly appreciated (please create a github issues).

# Significance

Improving the quality of biodiversity research, in some measure, is based on improving user-level data cleaning tools and skills. Adopting a more comprehensive approach for incorporating data cleaning as part of data analysis will not only improve the quality of biodiversity data, but will impose a more appropriate usage of such data. This can greatly serve the scientific community and consequently our ability to address more accurately urgent conservation issues.


Currently there are four parts in the package:-

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


# 4) User-Level Workflow

This is a very new concept ot summarize the data particularly required by the user. In this workflow there will be certain set of questions which the user will have to answer and then based on the responses several data checks will be initiated and after that the final filtered and flagged data will be given to the user with the summary of all the questions.


Currently there are only few below questions. We will be adding more questions further.


1) What is the lowest taxonomic level you require in your data.
- Sub-species
- Species
- Genus
- Family

2) What you want to do with data with mismatched names.
- Try to match
- Remove

3) What is the spatial resolution required for your data (in meters).
-

4) Do you care about dates of your observations?
-Yes
-No

5) What is the earliest date of your observations in this data set
- Date

6) What is the temporal resolution are you interested in?
- Year
- Month
- Day


More functions will be added in future for Biodiveristy Data Cleaning.
Hope you find these functions usefull.!!!!
