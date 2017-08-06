
## Read integer function
readinteger <- function(prompt="Enter an integer:")
{ 
  n <- readline(prompt=prompt)
  if(!grepl("^[0-9]+$",n))
  {
    return(readinteger())
  }
  
  return(as.integer(n))
}


## Read date function

IsDate <- function(mydate, date.format = "%Y-%m-%d") {
  
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}


readdate <- function(prompt="Enter a date: ")
{ 
  n <- readline(prompt=prompt)
  if(IsDate(n))
  {
    return(n)
  }
  else{
    print("Wrong input, date not detected")
  }
  
}
#print(readdate())

## Usage of above functions

library(rgbif)
d1 <- occ_data(
country = "AU",     # Country code for australia
classKey= 359,      # Class code for mammalia
limit=500,
hasCoordinate = T)

X<-d1$data
#View(X)
data<-subset(X,select=c(decimalLatitude,decimalLongitude,eventDate))
#View(data)
n<-(readinteger())

# filtering based on gievn input
data_subset<-subset(data,data$decimalLatitude<n)
View(data_subset)

#Similarly many filtering and flagging can be performed based on given inputs.



# Another Method to take input...This method is internally used in devtools for taking
# input. 
# If you enter "Yes" it will return 1 and if you enter "No" it will return 0, otherwise
# it will display an message saying enter choice from given list.

menu(c("Yes", "No"), title="Do you want this?")
