

# Workflow for taking input from the user based on the questions

# Vector for questions tags.
questions<-c("Taxonomic Level","Mistached Names","Spatial Resolution","Region of interest",
           "Dates","Earliest Date","Temporal Resolution")

responses<-c(0)
flags<-c(0)

read_taxonomic_input <- function(prompt2="What is the lowest taxonomic level you require in your data.
1) Sub-species 2) Species 3) Genus 4) Family")
{ 
  input_1 <- readline(prompt=prompt2)
  if(as.integer(input_1)==1 || as.integer(input_1)==2 || as.integer(input_1)==3 || as.integer(input_1)==4){
    if(as.integer(input_1)==1){
      Responses[i]="Sub-species"
      i=i+1
    }
    else if(as.integer(input_1)==2){
      Responses[i]="Species"
      i=i+1
    }
    else if(as.integer(input_1)==3){
      Responses[i]="Genus"
      i=i+1
    }
    else {
      Responses[i]="Family"
      i=i+1
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
      Responses[i]="Match"
      i=i+1
    }
    else{
      Responses[i]="Remove"
      i=i+1
    }
    
  }else{
    stop(paste("The entered choice is wrong"))
  }
  
  
}


read_spatial_resolution_input <- function(prompt2="What is the spatial resolution required for your data")
{ 
  input_1 <- readline(prompt=prompt2)
  responses[i]=as.integer(input_1)
  i=i+1
  
}


read_region_input <- function(prompt2="What is the region of your interest, 
Enter your choice (Enter 1 for continent and 2 for country):
                              1) Continent2) Country")
{ 
  input_1 <- readline(prompt=prompt2)
  if(as.integer(input_1)==1 || as.integer(input_1)==2){
    if(as.integer(input_1)==1){
      #In gbif data there is no field as continent
      input_11<-readline(prompt="Enter the continent of your interest.")
      Responses[i]=input_11
      i=i+1
    }
    else{
      input_12<-readline(prompt="Enter the country of your interest.")
      Responses[i]=input_11
      i=i+1
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
      Responses[i]="Yes"
      i=i+1
    }
    else{
      Responses[i]="No"
      i=i+1
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
      Responses[i]=input_1
      i=i+1
    }
  else{
    stop(paste("The entered date is wrong or else the date format is wrong"))
  }
}


read_taxonomic_input <- function(prompt2="What is the temporal resolution are you interested in?
1) Year 2) Month 3) Day")
{ 
  input_1 <- readline(prompt=prompt2)
  if(as.integer(input_1)==1 || as.integer(input_1)==2 || as.integer(input_1)==3 ){
    if(as.integer(input_1)==1){
      Responses[i]="Year"
      i=i+1
    }
    else if(as.integer(input_1)==2){
      Responses[i]="Month"
      i=i+1
    }
    else {
      Responses[i]="Day"
      i=i+1
    }
    
  }else{
    stop(paste("The entered choice is wrong"))
  }
  
  
}

# Now I will pass this below data frame to the output section. I have intoduced a flag column which will 
# maintain record for the tests passed.
# Combining all the vectors into data frame.
report_questions_answers<-data.frame(questions,responses,flags)
View(report_questions_answers)
