
library(mapr)
library(rgbif)

res2 <- occ_search(classKey = 358, limit = 100) #Reptile species
map_ggmap(res2)
map_ggplot(res2)
res1 <- occ_search(classKey = 359, limit = 200) #Mammals species
map_ggplot(res1)
map_ggmap(res1)

#View(res1$data)

#View(res2$data)
#y<-(name_lookup(query = 'Insecta',limit = 100, return = 'data'))
