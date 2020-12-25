
library(tidyverse)

efiles <- list.files("datasets/earthquakes/",full.names = TRUE)

eq <- efiles %>% 
  map_dfr(read_csv)

eq$year <- substr(eq$time,1,4)
eq$month <- substr(eq$time,6,7)
eq$day <- substr(eq$time,9,10)

eq.clean <- eq[,c("id","year","month","day",
                  "latitude","longitude","depth","mag","place")]
names(eq.clean)[8] <- "magnitude"

coords2country <- function(points){  
  countriesSP <- rworldmap::getMap(resolution='low')
  
  #setting CRS directly to that from rworldmap
  pointsSP = sp::SpatialPoints(points, 
                               proj4string=sp::CRS(sp::proj4string(countriesSP)))  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = sp::over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

coords2continent <- function(points){  
  countriesSP <- rworldmap::getMap(resolution='low')
  
  #setting CRS directly to that from rworldmap
  pointsSP = sp::SpatialPoints(points, 
                               proj4string=sp::CRS(sp::proj4string(countriesSP)))  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = sp::over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  # indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

eq.paises <- eq.clean %>% 
  mutate(country = coords2country(data.frame(longitude,latitude)),
         continent = coords2continent(data.frame(longitude,latitude)))

write.csv2(eq.paises,file = "datasets/earthquakes.csv",row.names = FALSE)
