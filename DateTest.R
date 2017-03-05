
#Checking if date column is present or not
#If date column name is different than "eventDate" can be easily changed by:
#R Code:colnames(dataframe)[which(names(dataframe) == "columnName")] <- "newColumnName"
#Counter to maintain count of the flagged records,initialized to 0

library(rgbif)

#CheckDateRecords<-function(x,size=2000,count=0,refData=NULL){
#
#                  refData<-as.data.frame(x$data)
#                   #Adding the flag column with initial entries as 0
#                   redData<-cbind(flag = 0,refData)
#                   if (("eventDate" %in% names(redData)) ){
#                   for(i in 1:size){
#
#                   if(!is.na(refData[i,"eventDate"]) ){
#                   count=count+1
#                   refData[i,"flag"]=1    # Flagging the records
#                                        }
#                                   }
#
#                                            }
#                   #Number of records flagged
#                   cat(sprintf("Number of flagged  records from the data \n"))
#                   print(count)
#                   #returns a array of flagged records with value 1 if flagged otherwise 0
#                   return(refData[,ncol(refData)])
#}

CheckDateRecords<-function(data){
                    data$flag <- "Good"
                    #Checking the bad indexes 
                    badindex <- which(is.na(data$day)|is.na(data$month)|is.na(data$year))
  
                    if(length(badindex)>0){
                    data[badindex,]$flag <- "Bad"   #flagging the bad indexes
                    }
  
                    print(head(data[, c("day", "month", "year", "flag")]))
  
  
  
  }


##EXAMPLE:

#Downloading data from the rgbif portal .
#Specifying the default size to be 2000 ,but is alterable.
size=2000
#Class key=359 corresponds to mammals
res <- occ_search(classKey = 359, limit = size)
res<-res$data
#Function call
CheckDateRecords(res)
