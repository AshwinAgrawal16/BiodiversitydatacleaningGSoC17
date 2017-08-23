
#' 
#'
#' @param x (data.frame) Data frame containing coordinates
#' @param countries (character.vector) Contains a list of countries corresponding to the coordinates from the data frame
#' @para poly (spatial object) Contains the shape file of the country region
#' Validating the country based on the coordinates.
#' 
#' 
#' 
#' @example 
#' d1 <- occ_data(
#' country = "AU",     # Country code for australia
#' classKey= 359,      # Class code for mammalia
#' limit=50000,
#' hasCoordinate = T
#' )
#'
#' X<-d1$data
#' x<-X[,4:3]
#' countries<-X$country 
#' Z<-CountryCheck(x,countries)
#' View(Z)
#' 
#' @description 
#' This function checks the country field by georefrencing the coordinates on standard data from naturalearth
#' 
#' 
CountryCheck <- function(x, countries, poly = NULL) {
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    stop("rnaturalearth needed for countries test option. Please install", 
         call. = FALSE)
  }
  pts <- SpatialPoints(x)
  
  if (is.null(poly)) {
    testpolys <- rnaturalearth::ne_countries(scale = "medium")
  }else{
    testpolys <- poly
    
    warning("assuming lat/lon for country.ref")
  }
  
  proj4string(testpolys) <- ""
  testpolys <- crop(testpolys, extent(pts))
  
  country <- sp::over(x = pts, y = testpolys)[, "iso_a3"]
  out <- as.character(country) == as.character(countries)
  out[is.na(out)] <- TRUE
  #View(country)
  return(out)
  #View(out)
}

#'
#'
#' @param x (data.frame) Data frame containing latitude and longitude
#'
#' This function flags the points near the gbif head quarters in copenhagen

GBIF <- function(x) {
  pts <- sp::SpatialPoints(x)
  poly <- rgeos::gBuffer(SpatialPoints(cbind(12.58, 55.67)), width = 0.5)
  warning("running GBIF test, flagging records around Copenhagen")
  
  out <- sp::over(x = pts, y = poly)
  out <- is.na(out)
  
  return(out)
}

#'
#'
#'@param x (data.frame) Data frame containing latitude and longitude
#'
#' This function flags the records with invalid coordinates
#'

ValidCoordinates <- function(x) {
  out <- list(is.na(x$decimalLongitude), is.na(x$decimalLatitude), suppressWarnings(is.na(as.numeric(as.character(x$decimalLongitude)))), suppressWarnings(is.na(as.numeric(as.character(x$decimalLatitude)))), 
              suppressWarnings(as.numeric(as.character(x$decimalLongitude))) < -180, suppressWarnings(as.numeric(as.character(x$decimalLongitude))) > 180, suppressWarnings(as.numeric(as.character(x$decimalLatitude))) < 
                -90, suppressWarnings(as.numeric(as.character(x$decimalLatitude))) > 90)
  
  out <- !Reduce("|", out)
  return(out)
}

#'
#'
#' @param x (data.frame) Data frame containing latitude and longitude
#' 
#' 
#'
#' This function flags the data with zero coordinates or near zero coordinates
#'
#'
ZeroCoordinates <- function(x, pointlim = 0.5) {
  pts <- SpatialPoints(x)
  out <- rep(T, nrow(x))
  
  # plain zero in coordinates
  out[which(x$decimalLongitude == 0 | x$decimalLatitude == 0)] <- FALSE
  
  # radius around point 0/0
  test <- rgeos::gBuffer(sp::SpatialPoints(cbind(0, 0)), width = pointlim)
  out[which(!is.na(over(y = test, x = pts)))] <- FALSE
  
  # lat == long
  out[which(x$decimalLongitude == x$decimalLatitude)] <- FALSE
  
  # #abs lat == abs lon
  # out[which(x$decimallongitude == x$decimallatitude)] <- FALSE
  
  return(out)
} 
