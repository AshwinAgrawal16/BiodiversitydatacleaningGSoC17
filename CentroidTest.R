#Here refData is the data which contains the capital and centroid coordinates of the countries.
#Considering both the country and province.

library(sp)
library(raster)
library(rgeos)
library(rgbif)

CentroidCoordinatesTest <- function(x, testdist = 0.1, buf = 1, testtype = c("both", "country", "provinces"), refData = NULL) {
  data <- sp::SpatialPoints(x) # Using sp package to convert to spatial points.
  if (is.null(refData)) {

    #Checking the country and province
    if (testtype[1] == "country") {
      refData<-refData[refData$type == "country", ]
    }
    if (testtype[1] == "province") {
      refData <- refData[refData$type == "province", ]
    }
  }
  # Getting the extent of the data
  # Keeping a buffer
  limits <- raster::extent(data) + buf

  # Subset of testdatset according to limits the limits defined

  refData<-raster::crop(SpatialPoints(refData[, c("longitude", "latitude")]), limits)
  if(is.null(refData)){ #Incase no capitals or centroid points are found
    out <- rep(TRUE, nrow(x))
  }else{
    #Checking if points near centroid with specifies width in 3D space
    refData <- rgeos::gBuffer(refData, width = testdist, byid = T)
    #Identifying records very close to centroid of any country
    out <- is.na(sp::over(x = dat, y =refData))
  }

  return(out)
}

# Main function for the calling the centroid test on the data.
FUNCTION1 <- function(x,index=T,centroids=T, centRad = 0.01,centDetail = "both"
                             ,centRef=NULL){

  #Checking if missing centroid data
  if(missing(centRef)){
    centRef <- NULL
  }


  if (centroids) {
    if (index) {
      cat("Running centroids test on the coordinates\n")
    }
    #Calling of function
    cent <-CentroidCoordinatesTest(x, testdist = centRad, buf = 1,
                                testtype = centDetail, refData = centRef)
    if (index) {
      cat(sprintf("Flagged %s records from the data \n", sum(!cent)))
    }
  } else {
    cent <- rep(NA, nrow(x))
  }
}
