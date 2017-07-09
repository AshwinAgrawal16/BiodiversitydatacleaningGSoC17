# One important point ot note is that I have just included latitude and logitude as data parameters, we can also include altitude and other 
# parameters to make the model more robust.

# I have also tried some fairly simple model like logistic regression and random forest
# but logistic regression gave very poor results and problem with random forest is that data is very large and complex.
# SVM is particular method to suit this kind of data.

# There are many other methods which are combination of two or more models which give better results called as emsemble models,
# but with higher accuracy comes the risk of over fitting, so I will carefully try to increase the accuracy.

# The data set used here is small(50000) and if we increase the size of data the SVM model performes very well, good for us as alpha hull is slow and only works on unique data.

# There is definately a large scope of research in this area, but on other side its a very time consuming task.



library(alphahull)
library(caret)
library(e1071)
library(rgbif)
library(plyr)


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

Z1<-X[,4:3]
Z1[,3]<-V


View(Z1)
DATA<-Z1
#Data splitting in test and train data
set.seed(2)
ind = sample(2, nrow(DATA), replace = TRUE, prob=c(0.7,0.3)) 
trainset = DATA[ind == 1,]
testset = DATA[ind == 2,]


#View(trainset)
#View(testset)

dim(trainset)
dim(testset)


# using SVM model for classification
model <- svm(V3 ~.,family=binomial,data=trainset)
summary(model)
#anova(model, test="Chisq")
#library(pscl)
#pR2(model)
fitted.results <- predict(model,newdata=subset(testset,select=c(1:2)),type='response')

# Checking the results for 95% confidence interval
fitted.results <- ifelse(fitted.results > 0.95,1,0)
fitted.results
misClasificError <- mean(fitted.results != testset$V3)
misClasificError
print(paste('Accuracy',1-misClasificError))
# The accuracy comes around 90% which is pretty good in terms of SVM, if we use some
# other models like DNN or neural networks the accuracy will increase but there is a high risk of overfitting.
