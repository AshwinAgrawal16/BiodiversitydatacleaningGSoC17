---
title: "Dashboard_DwC_spatial"
author: "Ashwin Agrawal"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    social: menu
    source_code: embed
runtime: shiny
---

```{r global, include=FALSE}
# load data in 'global' chunk so it can be shared by all users of the dashboard
suppressMessages(library(rgbif))
suppressMessages(library(sp))
suppressMessages(library(spatialEco))
suppressMessages(library(ggmap))
suppressMessages(library(plyr))
suppressMessages(library(htmlTable))
suppressMessages(library(bdvis))
suppressMessages(library(plotly))
suppressMessages(library(pracma))
suppressMessages(library(xtable))
suppressMessages(library(htmlTable))
d1 <- occ_data(
  country = "AU",     # Country code for australia
  classKey= 359,      # Class code for mammalia
  from = 'gbif',
  limit=500,
  minimal=FALSE,
  hasCoordinate = T
  
)
X<-d1$data



```

Inputs {.sidebar}
-----------------------------------------------------------------------

```{r}
 selectInput("clusterNum", label = h3("Cluster number"), 
    choices = list( "10" = 10, "100" = 100, "1000" = 1000, "10000" = 10000,"100000" = 100000), 
    selected = 1)
```

Plotting the data based on coordinate uncertainity in meters of the position.
Sample data used is Australian Mammals.

```{r}
# Reactive that returns the whole dataset if there is no brush
selectedData <- reactive({
  
  num <- reactive(as.integer(input$clusterNum))
  XCP<-subset(X,X$coordinateUncertaintyInMeters<num())
    
    c_2<-ddply(XCP,~coordinateUncertaintyInMeters,summarise,number_of_distinct_orders=length((coordinateUncertaintyInMeters)))
    
    c_2
})
```


Row
-----------------------------------------------------------------------

### map
    
```{r}


  
  
  num <- reactive(as.integer(input$clusterNum))


  
  
  

  renderPlot({
  mapgilbert <- get_map(location = c(lon = mean(X$decimalLongitude), lat = mean(X$decimalLatitude)), zoom = 4,
                        maptype = "terrain", scale = 2)
    
  
  #Plotting with coordinateuncertainityinmeters
  
  XCU<-subset(X,X$coordinateUncertaintyInMeters<num())
  if(nrow(XCU)>0)
  {
  c<-ggmap(mapgilbert) +
    geom_point(data = XCU, aes(x = decimalLongitude, y = decimalLatitude, alpha = 0.5, size =coordinateUncertaintyInMeters, colour= coordinateUncertaintyInMeters ),shape=20)
  print(c)
    } else{
    
    c<-ggmap(mapgilbert) +
    geom_point(data = X, aes(x = decimalLongitude, y = decimalLatitude, alpha = 0.5, size =coordinateUncertaintyInMeters, colour= coordinateUncertaintyInMeters ),shape=20)
  print(c)
    }
 
  
  })
  
```


Row
-----------------------------------------------------------------------

### plot


```{r}
library(ggplot2)

renderPlot({
  ggplot(selectedData(), aes(x=coordinateUncertaintyInMeters,y=number_of_distinct_orders)) + geom_bar(stat="identity")
})
```


