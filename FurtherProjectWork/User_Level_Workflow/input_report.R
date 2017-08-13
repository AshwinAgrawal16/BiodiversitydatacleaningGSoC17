

# Workflow for taking input from the user based on the questions


read_taxonomic_input <- function(prompt2="What is the lowest taxonomic level you require in your data.
1) Sub-species 2) Species 3) Genus 4) Family")
{ 
  input_1 <- readline(prompt=prompt2)
  if(as.integer(input_1)==1 || as.integer(input_1)==2 || as.integer(input_1)==3 || as.integer(input_1)==4){
    if(as.integer(input_1)==1){
      return("Sub-species")
      
    }
    else if(as.integer(input_1)==2){
      return("Species")
    
    }
    else if(as.integer(input_1)==3){
      return("Genus")
    
    }
    else {
      return("Family")
    
    }
    
  }else{
    stop(paste("The entered choice is wrong"))
  }
  
  
}



read_mismatch_input <- function(prompt2="What you want to do with data with mismatched names
1) Try to match 2) Remove")
{ 
  input_1 <- readline(prompt=prompt2)
  if(as.integer(input_1)==1 || as.integer(input_1)==2){
    if(as.integer(input_1)==1){
      return("Match")
     
    }
    else{
      return("Remove")
      
    }
    
  }else{
    stop(paste("The entered choice is wrong"))
  }
  
  
}


read_spatial_resolution_input <- function(prompt2="What is the spatial resolution required for your data")
{ 
  input_1 <- readline(prompt=prompt2)
  return((input_1))
  
  
}


read_region_input <- function(prompt2="What is the region of your interest, 
Enter your choice (Enter 1 for continent and 2 for country):
                              1) Continent 2) Country")
{ 
  input_1 <- readline(prompt=prompt2)
  if(as.integer(input_1)==1 || as.integer(input_1)==2){
    if(as.integer(input_1)==1){
      #In gbif data there is no field as continent
      input_11<-readline(prompt="Enter the continent of your interest.")
      return(input_11)
      
    }
    else{
      input_12<-readline(prompt="Enter the country of your interest.")
      return(input_12)
      
    }
    
  }else{
    stop(paste("The entered choice is wrong"))
  }
  
  
}



read_dates_input <- function(prompt2=" Do you care about dates of your observations?
1) Yes 2) No")
{ 
  input_1 <- readline(prompt=prompt2)
  if(as.integer(input_1)==1 || as.integer(input_1)==2){
    if(as.integer(input_1)==1){
      return("Yes")
      
    }
    else{
      return("No")
      
    }
    
  }else{
    stop(paste("The entered choice is wrong"))
  }
  
  
}




IsDate <- function(mydate, date.format = "%Y-%m-%d") {
  
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}


read_date_earliest_input <- function(prompt2=" What is the earliest date of your observations in this data set (date format-> %Y-%m-%d)")
{ 
  input_1 <- readline(prompt=prompt2)
    if(IsDate(input_1)){
      return(input_1)
     
    }
  else{
    stop(paste("The entered date is wrong or else the date format is wrong"))
  }
}




read_temporal_input <- function(prompt2="What is the temporal resolution are you interested in?
1) Year 2) Month 3) Day")
{ 
  input_1 <- readline(prompt=prompt2)
  if(as.integer(input_1)==1 || as.integer(input_1)==2 || as.integer(input_1)==3 ){
    if(as.integer(input_1)==1){
      return("Year")
      
    }
    else if(as.integer(input_1)==2){
      return("Month")
      
    }
    else {
      return("Day")
      
    }
    
  }else{
    stop(paste("The entered choice is wrong"))
  }
  
  
}





# Vector for questions tags.
Questions<-c("Taxonomic Level","Mistached Names","Spatial Resolution","Region of interest",
             "Dates","Earliest Date","Temporal Resolution")

Responses<-as.vector(nrow(Questions))

Flags<-c(0)

# Below is the part for running teh whole workflow, run one function at a time then enter the input,
# then run the next function.

i=1
Responses[i]<-read_taxonomic_input()
#  Enter the input
i=i+1


Responses[i]<-read_mismatch_input()
# Enter the input
i=i+1


Responses[i]<-read_spatial_resolution_input()
# Enter the input
i=i+1


Responses[i]<-read_region_input()
# Enter the input
i=i+1

Responses[i]<-read_dates_input()
# Enter the input
i=i+1


Responses[i]<-read_date_earliest_input()
# Enter the input
i=i+1

Responses[i]<-read_temporal_input()
# Enter the input
i=i+1


# Now I will pass this below data frame to the output section. I have intoduced a flag column which will 
# maintain record for the tests passed.
# Combining all the vectors into data frame.
report_questions_answers<-data.frame(Questions,Responses,Flags)
View(report_questions_answers)
