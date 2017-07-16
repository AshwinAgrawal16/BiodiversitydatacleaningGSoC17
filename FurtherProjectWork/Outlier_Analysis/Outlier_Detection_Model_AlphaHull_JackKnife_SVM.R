#'
#' @param 
#' X (data.frame) Contains biodiversity data from gbif portal
#'
#' @description 
#' Below model is based on combination of Alpha Hull and Reverse Jack knife to identify the
#' outliers and then use the machine learning models to classify the data.
#' If the alpha hull technique dosent converge to from a hull then the reverse jack knife
#' method is used for identification of outliers and then form the model.
#' Here I have just used SVM as machine learning model, If required I will integrate some other models.
#' 
#'
#' @example 
#' library(alphahull)
#' library(caret)
#' library(e1071)
#' library(rgbif)
#' library(plyr)
#' library(maptools)
#' library(raster)
#' library(biogeo)
#'
#'
#'  d1111 <- occ_data(
#'  country = "AU",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  from = 'gbif',
#'  limit=50000,
#'  minimal=FALSE,
#'  hasCoordinate = T
#'  
#' )
#'
#' X1<-d1111$data
#' Model_Outlier_SVM(X1)
#' 
#' 
#'  d111 <- occ_data(
#'  country = "US",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  from = 'gbif',
#'  limit=50000,
#'  minimal=FALSE,
#'  hasCoordinate = T
#'  
#' )
#'
#' X2<-d111$data
#' Model_Outlier_SVM(X2)

#'  d11 <- occ_data(
#'  country = "CN",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  from = 'gbif',
#'  limit=50000,
#'  minimal=FALSE,
#'  hasCoordinate = T
#'  
#' )
#'
#' X3<-d11$data
#' Model_Outlier_SVM(X4)

#'  d1 <- occ_data(
#'  country = "IN",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  from = 'gbif',
#'  limit=50000,
#'  minimal=FALSE,
#'  hasCoordinate = T
#'  
#' )
#'
#' X4<-d1$data
#' Model_Outlier_SVM(X4) 
#' 
#'  d <- occ_data(
#'  country = "JP",     # Country code for australia
#'  classKey= 359,      # Class code for mammalia
#'  from = 'gbif',
#'  limit=50000,
#'  minimal=FALSE,
#'  hasCoordinate = T
#'  
#' )
#'
#' X5<-d$data
#' Model_Outlier_SVM(X5)
#' 
#' 
#' 
#' 
Model_Outlier_SVM<-function(X){

##########################################

DATA<-X[,4:3]

# Alpha Hull approach
Z<-unique(DATA)
ahull.obj<-ahull(Z,alpha = 2)
plot(ahull.obj)
# ashape.obj<-ashape(Z,alpha = 0.2)

# View(DATA)
# Now checking the points which are in hull using alpha hull and identifying outliers which are outside hull.
V<-inahull(ahull.obj,p = cbind(DATA$decimalLongitude,DATA$decimalLatitude))
V<-as.integer(V)
Val1<-nrow(DATA)
DATA1<-DATA
DATA1[,3]<-V
count<-subset(DATA1,DATA1$V3==0)
Val2<-nrow(count)
# This is the condition which checks whether Hull is formed successfully, if not 
# Reverse Jack knife is used
if(Val2<=(Val1/2)){
  # head(V)
  
  # Checking rgeos availability: TRUE
  data(wrld_simpl)
  
  wc <- getData('worldclim', res=10, var='bio')
  
  # DATA1 <- extract(wc, unique(X[,4:3]))
  DATA_ALL <- extract(wc, (DATA))
  
  DATA_ALL<-as.data.frame(DATA_ALL)
  DATA_ALL[,20]<-V
  #View(DATA_ALL)
  DATA_ALL<-na.exclude(DATA_ALL)
  
  #AAA<-subset(DATA_ALL,DATA_ALL$V20==0)
  #View(AAA)
  set.seed(2)
  ind = sample(2, nrow(DATA_ALL), replace = TRUE, prob=c(0.7,0.3)) 
  trainset = DATA_ALL[ind == 1,]
  testset = DATA_ALL[ind == 2,]
  
  
# dim(trainset)
# dim(testset)
  
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
  
  fitted.results <- predict(model,newdata=subset(testset,select=c(1:19)),type='response')
  
  # Checking the results for 95% confidence interval
  fitted.results <- ifelse(fitted.results > 0.95,1,0)
  fitted.results
  misClasificError <- mean(fitted.results != testset$V20)
  misClasificError
  print(paste('Accuracy',1-misClasificError))
  
}else{
  
  # Reverse Jack Knife approach
  XXXX1<-rjack((X1$decimalLatitude))
  # XXXX1
  XXXX2<-rjack((X1$decimalLongitude))
  # XXXX2
  
  data(wrld_simpl)
  wc <- getData('worldclim', res=10, var='bio')
  DATA_ALL <- extract(wc, (DATA))
  DATA_ALL<-as.data.frame(DATA_ALL)
  #View(DATA_ALL)
  DATA_ALL[,20]<-0
  DATA_ALL[XXXX1,20]<-1
  DATA_ALL[XXXX2,20]<-1
  
  DATA_ALL<-na.exclude(DATA_ALL)
  #AAA<-subset(DATA_ALL, DATA_ALL$V20==1)
  
  set.seed(2)
  ind = sample(2, nrow(DATA_ALL), replace = TRUE, prob=c(0.7,0.3)) 
  trainset = DATA_ALL[ind == 1,]
  testset = DATA_ALL[ind == 2,]
  
# dim(trainset)
# dim(testset)
  
  # using SVM model for classification
  # This step might take some time
  model <- svm(V20 ~.,family=binomial,data=trainset)

  summary(model)
  
  fitted.results <- predict(model,newdata=subset(testset,select=c(1:19)),type='response')
  
  # Checking the results for 95% confidence interval
  fitted.results <- ifelse(fitted.results > 0.95,1,0)
  fitted.results
  misClasificError <- mean(fitted.results != testset$V20)
  misClasificError
  print(paste('Accuracy',1-misClasificError))
  
  
   }

}


Model_Outlier_SVM(X1)
