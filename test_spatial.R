X<-read.csv(file = "C:\\yourfile.csv")
X1<-X[,1:4]
X1<-X1[,-2]
dim(X1)
X1<-unique(X1)

# creating a sample data.frame with your lat/lon points
lon <- c(-38.31,-35.5)
lat <- c(40.96, 37.5)
df <- as.data.frame(cbind(lon,lat))
df<-X1
View(df)
X1$
  # getting the map
  mapgilbert <- get_map(location = c(lon = mean(df$decimalLongitude), lat = mean(df$decimalLatitude)), zoom = 4,
                        maptype = "satellite", scale = 2)


# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = df[1:100,], aes(x = decimalLongitude, y = decimalLatitude, alpha = 0.5, size = name, colour= name ),shape=20) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)
