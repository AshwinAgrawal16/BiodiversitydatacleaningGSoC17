#'
#'
#'
#'
#'
#' In the data the alpha hull is not formed properly therefore the outlier predicted 
#' are not correct, therefore as initially the outlier are not marked correctly
#' then the model formed by SVM and CART are automatically wrong, therefore I am using
#' Reverse-Jack-Knife method for identifying the outliers and forming the model.
#'
#'
#'
#'
#'



library(alphahull)
library(caret)
library(e1071)
library(rgbif)
library(plyr)
library(maptools)
library(raster)



d11 <- occ_data(
  country = "AU",     # Country code for australia
  classKey= 359,      # Class code for mammalia
  from = 'gbif',
  scientificName = "Dasyurus viverrinus",
  limit=20000,
  minimal=FALSE,
  hasCoordinate = T
  
)
S1<-d11$data

S1C<-S1[,4:3]
#View(S1)


d12 <- occ_data(
  country = "AU",     # Country code for australia
  from = 'gbif',
  scientificName = "Macropus rufus",
  limit=20000,
  minimal=FALSE,
  hasCoordinate = T
  
)

S2<-d12$data
S2C<-S2[,4:3]
#View(S2)

d13 <- occ_data(
  country = "AU",     # Country code for australia
  
  
  scientificName = "Petrogale penicillata",
  limit=20000,
  minimal=FALSE,
  hasCoordinate = T
  
)

S3<-d13$data
S3C<-S3[,4:3]
#View(S3)


d14 <- occ_data(
  country = "AU",     # Country code for australia
  
  
  scientificName = "Pseudomys desertor",
  limit=20000,
  minimal=FALSE,
  hasCoordinate = T
  
)

S4<-d14$data
S4C<-S4[,4:3]
#View(S4)



d15 <- occ_data(
  country = "AU",     # Country code for australia
  
  
  scientificName = "Pteropus alecto",
  limit=20000,
  minimal=FALSE,
  hasCoordinate = T
  
)

S5<-d15$data
S5C<-S5[,4:3]
#View(S5)

DATA<-rbind(S1C,S2C,S3C,S4C,S5C)


Z<-unique(DATA)
ahull.obj<-ahull(Z,alpha = 2)
plot(ahull.obj)
#ashape.obj<-ashape(Z,alpha = 0.2)

#View(DATA)
# Now checking the points which are in hull using alpha hull and identifying outliers which are outside hull.
V<-inahull(ahull.obj,p = cbind(Z$decimalLongitude,Z$decimalLatitude))
V<-as.integer(V)
#head(V)


## Checking rgeos availability: TRUE
data(wrld_simpl)

wc <- getData('worldclim', res=10, var='bio')


#DATA1 <- extract(wc, unique(X[,4:3]))
DATA_ALL <- extract(wc, (DATA))
#head(DATA1)
#View(DATA_ALL)


#i <- which(is.na(DATA1[,1]))
#i

#plot(Z, cex=0.5, col='blue')
#plot(wrld_simpl, add=TRUE)
#points(Z[i, ], pch=20, cex=3, col='red')



DATA_ALL<-as.data.frame(DATA_ALL)
#View(DATA_ALL)
DATA_ALL[,20]<-V
#View(DATA_ALL)
DATA_ALL<-na.exclude(DATA_ALL)
set.seed(2)
ind = sample(2, nrow(DATA_ALL), replace = TRUE, prob=c(0.7,0.3)) 
trainset = DATA_ALL[ind == 1,]
testset = DATA_ALL[ind == 2,]


#View(trainset)
#View(testset)

dim(trainset)
dim(testset)


# using SVM model for classification
# This step might take some time
model <- svm(V20 ~.,family=binomial,data=trainset)

# Now this step shows an error because there are no outliers present in the data that is the
# outlier column has only zeros and no ones therefore nothing to train on for binary model

# This is because all the points in the data above are very close to each other
# therefore while using alphaHull the hull or ploygon formed covers each and every
# point and no point is flagged as outlier therefore the model fails.
# After all there has to be some outliers present to predict them.

summary(model)
#anova(model, test="Chisq")
#library(pscl)
#pR2(model)
fitted.results <- predict(model,newdata=subset(testset,select=c(1:19)),type='response')

# Checking the results for 95% confidence interval
fitted.results <- ifelse(fitted.results > 0.95,1,0)
fitted.results
misClasificError <- mean(fitted.results != testset$V20)
misClasificError
print(paste('Accuracy',1-misClasificError))
# Accuracy about 93% 

