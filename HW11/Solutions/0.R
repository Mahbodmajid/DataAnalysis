library(ggthemes)
library(ggplot2)
library(readr)
library(ggmap)
library(plyr)
library(dplyr)
library(stringr)
library(highcharter)
library(countrycode)
library(knitr)


#setwd("~/Desktop/96-97-2/Data Analysis/HW/HW11/")
read_rds(path = "../data_eq/historical_web_data_26112015.rds") -> historical_eq
disaster = read_delim("../data_eq/disaster.txt", "\t",
                      escape_double = FALSE, trim_ws = TRUE)
world = map_data("world")
read_rds("../data_eq/iran_earthquake.rds") -> iequake
myMap = read_rds("../data_eq/Tehrn_map_6.rds")
read_csv("../data_eq/worldwide.csv") -> ww

get_world_map= function(){
  library(maps)
  library(ggplot2)
  library(sp)
  library(ggmap)
  ###################################################################################################
  # Recentre worldmap (and Mirrors coordinates) on longitude 160
  ### Code by Claudia Engel  March 19, 2012, www.stanford.edu/~cengel/blog
  
  ### Recenter ####
  center <- 0 # positive values only
  
  # shift coordinates to recenter worldmap
  worldmap <- map_data ("world")
  worldmap$long.recenter <- ifelse(worldmap$long < center - 180 , worldmap$long + 360, worldmap$long)
  
  ### Function to regroup split lines and polygons
  # Takes dataframe, column with long and unique group variable, returns df with added column named group.regroup
  RegroupElements <- function(df, longcol, idcol){
    g <- rep(1, length(df[,longcol]))
    if (diff(range(df[,longcol])) > 300) { # check if longitude within group differs more than 300 deg, ie if element was split
      d <- df[,longcol] > mean(range(df[,longcol])) # we use the mean to help us separate the extreme values
      g[!d] <- 1 # some marker for parts that stay in place (we cheat here a little, as we do not take into account concave polygons)
      g[d] <- 2 # parts that are moved
    }
    g <- paste(df[, idcol], g, sep=".") # attach to id to create unique group variable for the dataset
    df$group.regroup <- g
    df
  }
  
  ### Function to close regrouped polygons
  # Takes dataframe, checks if 1st and last longitude value are the same, if not, inserts first as last and reassigns order variable
  ClosePolygons <- function(df, longcol, ordercol){
    if (df[1,longcol] != df[nrow(df),longcol]) {
      tmp <- df[1,]
      df <- rbind(df,tmp)
    }
    o <- c(1: nrow(df)) # rassign the order variable
    df[,ordercol] <- o
    df
  }
  
  # now regroup
  worldmap.rg <- ddply(worldmap, .(group), RegroupElements, "long.recenter", "group")
  
  # close polys
  worldmap.cp <- ddply(worldmap.rg, .(group.regroup), ClosePolygons, "long.recenter", "order") # use the new grouping var
  #############################################################################
  return(ggplot(aes(x = long.recenter, y = lat), data = worldmap.cp) + 
           geom_polygon(aes(group = group.regroup), fill="#f9f9f9", colour = "grey65") + 
           scale_y_continuous(limits = c(-60, 85)) + 
           coord_equal() + theme_Publication()+
           theme(legend.position = "none",
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 axis.text.x = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(), 
                 panel.border = element_rect(colour = "black")))
}

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  
  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
}

place2country <- function (place){
  place %>% str_trim() %>% str_split_fixed(",", 5) -> place_parts
  result = place_parts[,5]
  for(i in 4:1){
    current = place_parts[,i]
    result = if_else(condition = (result == ""), true = place_parts[,i], false = result)
    for(cstate in c(state.name, "CA")){
      result %>% str_replace(cstate,"United States of America") -> result
    }
  } 
  return(result)
}
