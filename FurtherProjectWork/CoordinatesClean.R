CountryCheck <- function(x, countries, poly = NULL) {
  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    stop("Please install rnaturalearth ", 
         call. = FALSE)
  }
  pts <- SpatialPoints(x)
  
  if (is.null(poly)) {
    testpolys <- rnaturalearth::ne_countries(scale = "medium")
  } else {
    testpolys <- poly
  }
  if(is.na(proj4string(testpolys))){
    warning("Guessing 'country.ref'")
    proj4string(testpolys) <- proj4string(pts)
  }
  if(proj4string(pts) != proj4string(testpolys)){
    warning("Guessing the records")
    proj4string(pts) <- proj4string(testpolys)
  }
  
  testpolys <- crop(testpolys, extent(pts))
  
  country <- sp::over(x = pts, y = testpolys)[, "ISO3"]
  out <- as.character(country) == as.character(countries)
  out[is.na(out)] <- TRUE
  
  return(out)
}


CapitalCoordinates <- function(x, testdist = 0.1, buffer = 1, referencedat = NULL) {
  dat <- sp::SpatialPoints(x)
  
  limits <- raster::extent(dat) + buffer
  
  # subset of testdatset according to limits
  referencedat <- raster::crop(SpatialPoints(referencedat[, c("longitude", "latitude")]), limits)
  
  if(is.null(referencedat)){ # incase no capitals are found in the study area
    out <- rep(TRUE, nrow(x))
  }else{
    referencedat <- rgeos::gBuffer(referencedat, width = testdist, byid = T)
    out <- is.na(sp::over(x = dat, y = referencedat))
  }
  
  return(out)
}

CentroidCoordinates <- function(x, testdist = 0.1, buffer = 1, testtype = c("both", "country", "provinces"), referencedat = NULL) {
  dat <- sp::SpatialPoints(x)
  if (is.null(referencedat)) {
    
    
    if (testtype[1] == "country") {
      referencedat <- referencedat[referencedat$type == "country", ]
    }
    if (testtype[1] == "province") {
      referencedat <- referencedat[referencedat$type == "province", ]
    }
  }
  
  limits <- raster::extent(dat) + buffer
  
  # subset of testdatset according to limits
  referencedat <- raster::crop(SpatialPoints(referencedat[, c("longitude", "latitude")]), limits)
  if(is.null(referencedat)){ # incase no capitals are found in the study area
    out <- rep(TRUE, nrow(x))
  }else{
    referencedat <- rgeos::gBuffer(referencedat, width = testdist, byid = T)
    out <- is.na(sp::over(x = dat, y = referencedat))
  }
  
  return(out)
}


#Haversine method followed 
#The haversine formula determines the great-circle distance between two points on a sphere given their longitudes and latitudes.
#Important in navigation, it is a special case of a more general formula in spherical trigonometry, 
#the law of haversines, that relates the sides and angles of spherical triangles.
OutlierCoordinates <- function(x, species, mltpl, tdi, method = "Haversine") {
  
  splist <- split(x, f = as.character(species))
  
  test <- lapply(splist, "duplicated")
  test <- lapply(test, "!")
  test <- as.vector(unlist(lapply(test, "sum")))
  splist <- splist[test > 2]
  
  flags <- lapply(splist, function(k, td = tdi, mu = mltpl) {
    
    test <- nrow(k[!duplicated(k), ])
    dist <- geosphere::distm(k, fun = geosphere::distHaversine)
    dist[dist == 0] <- NA
    
    if (!is.null(mu) & !is.null(td)) {
      stop("set outliers.td OR outliers.mtp, and other one to NULL")
    }
    
    if (!is.null(mu)) {
      mins <- apply(dist, 1, min, na.rm = T)
      quo <- quantile(mins, c(0.99), na.rm = T)
      out <- which(mins > (quo + mean(mins, na.rm = T) * mu))
    }
    
    if (!is.null(td)) {
      mins <- apply(dist, 1, min, na.rm = T)
      out <- which(mins > td * 1000)
    }
    
    if (length(out) == 0) {
      ret <- NA
    }
    if (length(out) > 0) {
      ret <- rownames(k)[out]
    }
    return(ret)
  })
  
  
  flags <- as.numeric(as.vector(unlist(flags)))
  flags <- flags[!is.na(flags)]
  
  out <- rep(TRUE, nrow(x))
  out[flags] <- FALSE
  
  return(out)
}

UrbanCoordinates <- function(x, poly = NULL) {
  pts <- SpatialPoints(x)
  limits <- extent(pts) + 1
  
  if (is.null(poly)) {
    stop("No reference found. Set 'urban.ref'")
  }
  
  poly <- crop(poly, limits)
  
  urban <- over(x = pts, y = poly)[, 1]
  out <- is.na(urban)
  
  return(out)
}

#GBIF Headquarters
GBIF <- function(x) {
  pts <- sp::SpatialPoints(x)
  poly <- rgeos::gBuffer(SpatialPoints(cbind(12.5, 55.6)), width = 0.5)
  warning("running GBIF test, flagging records around Copenhagen")
  
  out <- sp::over(x = pts, y = poly)
  out <- is.na(out)
  
  return(out)
}

ValidCoordinates <- function(x) {
  out <- list(is.na(x$decimallongitude), is.na(x$decimallatitude), suppressWarnings(is.na(as.numeric(as.character(x$decimallongitude)))), suppressWarnings(is.na(as.numeric(as.character(x$decimallatitude)))), 
              suppressWarnings(as.numeric(as.character(x$decimallongitude))) < -180, suppressWarnings(as.numeric(as.character(x$decimallongitude))) > 180, suppressWarnings(as.numeric(as.character(x$decimallatitude))) < 
                -90, suppressWarnings(as.numeric(as.character(x$decimallatitude))) > 90)
  
  out <- !Reduce("|", out)
  return(out)
}



ZeroCoordinates <- function(x, pointlim = 0.5) {
  pts <- SpatialPoints(x)
  out <- rep(T, nrow(x))
  
  
  # point 0/0
  test <- rgeos::gBuffer(sp::SpatialPoints(cbind(0, 0)), width = pointlim)
  out[which(!is.na(over(y = test, x = pts)))] <- FALSE
  
  
  # plain zero in coordinates
  out[which(x$decimallongitude == 0 | x$decimallatitude == 0)] <- FALSE
  
 
  
  # lat == long
  out[which(x$decimallongitude == x$decimallatitude)] <- FALSE
  
  return(out)
} 


#For GBIF institutions
Institutions <- function(x, testdist = 0.001, buffer = 1, referencedat = NULL){
  dat <- sp::SpatialPoints(x)
  
  
  limits <- raster::extent(dat) + buffer
  
  # subset of testdatset according to limits
  referencedat <- raster::crop(SpatialPoints(referencedat[, c("decimallongitude", "decimallatitude")]), limits)
  
  if(is.null(referencedat)){ # incase no bdinstitutions
    out <- rep(TRUE, nrow(x))
  }else{
    referencedat <- rgeos::gBuffer(referencedat, width = testdist, byid = T)
    out <- is.na(sp::over(x = dat, y = referencedat))
  }
  
}
