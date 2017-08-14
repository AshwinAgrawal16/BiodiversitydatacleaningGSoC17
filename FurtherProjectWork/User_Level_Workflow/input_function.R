# Example code:

options<-list(taxoLevel=data.frame(choice=c(1,2,3,4),value=c("Sub-species", "Species","Genus","Family")),
              misNames=data.frame(choice=c(1,2),value=c("Yes","No")),
              dateCheck=data.frame(choice=c(1,2),value=c("Yes","No")),
              temporalResolution=data.frame(choice=c(1,2,3),value=c("Year","Month","Day")))


Quest<-read.csv("C:\\quest.csv")


data<-input_function(quest = Quest)
View(data)



# Functions for input.
IsDate <- function(mydate, date.format = "%Y-%m-%d") {
  
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}

input_function<-function(quest=NULL){
  quest<-as.matrix(quest)
  quest1<-(quest)
  res<-vector(mode="character",length = nrow(quest))
  
for (i in 1:nrow(quest)){
  
  rval <- readline(prompt=quest[i,2])
  
  if(quest[i,3]=="Numeric"){
    
    res[i]=rval
  }
  

  if(quest[i,3]=="Date"){
    
    if(IsDate(rval)){
      res[i]=rval
      
    }
    else{
      stop(paste("The input in wrong or the date format is not correct"))
    }
    
    
  }
  
  if(quest[i,3]=="I_Numeric"){
    
    if(rval<=quest[i,4] && rval>=1){
     qvar<-quest[i,1]
     data<-as.data.frame(options[qvar])
     colnames(data)<-c("choice","value")
     res[i]<-as.character((data[data$choice==rval,2]))
     quest1[i,3]="Numeric"
     
    }
    else{
      stop(paste("The entered choice is wrong."))
     }
    
   }
  
 
 }

response<-data.frame(quest1[,1:2],reponse=res)
return(response)

}


