###Load libraries####
library(sf)
library(tidyverse)


### Determine the immediate surrounding neighbors that touch each neighborhood polygon 

neighborhoods <- st_read("inputs/spatial/Sacramento-Neighborhoods-shp/Neighborhoods.shp") %>%
  st_make_valid()

neighborhoods.split <- split(neighborhoods, f=neighborhoods$NAME)
touching.db.list <- list()
for (i in 1:length(neighborhoods.split)) {
  
  hood <- neighborhoods.split[[i]]

  ### Filter to include only those polygons that touch the neighborhood polygon and pull out into comma separate list
    neighborhoods.touching <- neighborhoods %>%
    filter(st_touches(geometry, hood, sparse = FALSE)) %>%
    dplyr::select(NAME) %>% 
    st_drop_geometry() %>%
    dplyr::pull()
    
    neighborhoods.touching.vec <- paste(neighborhoods.touching,  collapse=",")
  
  #### Create dataframe
  neighborhood.surround.df <- data.frame(neighborhood = hood$NAME,
                                         touches = neighborhoods.touching.vec)
  
  #### write to list 
  touching.db.list[[i]] <- neighborhood.surround.df
}

touching.db <- bind_rows(touching.db.list)
