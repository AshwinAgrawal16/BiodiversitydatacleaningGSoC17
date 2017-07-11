#'
#'
#' Here I have downloaded the world climate data for australian mammals and then 
#' identified the outliers by using alpha hull method and then applied SVM and CART models 
#' to build a model based on climate variables and outliers , I was able to increase the
#' accuracy by just introducing newe data which has around 19 variables, so as more data is 
#' present automatically the model becomes robust and accuracy increase. Moreover now as the
#' data is large conventional models can also be applied and even Deep learning can be applied
#' to make accuracy greater than 95% which is excellent, though we will have to check for
#' over fitting by running it on varied data sets.
#' One advantage of Biodiversity data is that very huge amount of data is present which is best
#' possible condition to train the model and remove any biasness and over fitting therefore a very 
#' robust model can be build.
#' Moreover we can also include other type of data with the climate data as well to reduce over fitting. Habitat Data etc.
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

d1 <- occ_data(
  country = "AU",     # Country code for australia
  classKey= 359,      # Class code for mammalia
  from = 'gbif',
  limit=50000,
  minimal=FALSE,
  hasCoordinate = T
  
)

X<-d1$data

# using alpha hull to find the hull and alpha requires unique values.
Z<-unique(X[,4:3])
ahull.obj<-ahull(Z,alpha = 0.2)
#ashape.obj<-ashape(Z,alpha = 0.2)


# Now checking the points which are in hull using alpha hull and identifying outliers which are outside hull.
V<-inahull(ahull.obj,p = cbind(X$decimalLongitude,X$decimalLatitude))
V<-as.integer(V)



## Checking rgeos availability: TRUE
data(wrld_simpl)

wc <- getData('worldclim', res=10, var='bio')


#DATA1 <- extract(wc, unique(X[,4:3]))
DATA_ALL <- extract(wc, (X[,4:3]))
#head(DATA1)
#View(DATA_ALL)


#i <- which(is.na(DATA1[,1]))
#i

#plot(Z, cex=0.5, col='blue')
#plot(wrld_simpl, add=TRUE)
#points(Z[i, ], pch=20, cex=3, col='red')


DATA_ALL<-as.data.frame(DATA_ALL)

DATA_ALL[,20]<-V

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


# CART model-Random Forest
library(rpart)
Model<-rpart(V20 ~.,data=trainset)
#summary(Model)
printcp(Model)
fitted.results <- predict(Model,subset(testset,select=c(1:19)))

# Checking the results for 95% confidence interval
fitted.results <- ifelse(fitted.results > 0.95,1,0)
fitted.results
misClasificError <- mean(fitted.results != testset$V20)
misClasificError
print(paste('Accuracy',1-misClasificError))
# Accuracy about 85%