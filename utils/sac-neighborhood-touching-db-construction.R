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
    dplyr::pull() %>%
      sort(.)
    
    neighborhoods.touching.vec <- sort(paste(neighborhoods.touching,  collapse=","))
  
  #### Create dataframe
  neighborhood.surround.df <- data.frame(neighborhood = hood$NAME,
                                         touches = neighborhoods.touching.vec)
  
  #### write to list 
  touching.db.list[[i]] <- neighborhood.surround.df
}

touching.db <- bind_rows(touching.db.list)

write_csv(touching.db, "/Volumes/GoogleDrive/My Drive/00_Personal Documents/Consulting/05_SFStandard_Expansion/pf-sfstandard-expansion/utils/sac-neighborhood-to-neighborhood-touching-db.csv")
