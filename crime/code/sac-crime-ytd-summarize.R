##### Summarizing Year-to-Date Crime Data for Sacramento ####
#### Date updated: July 12, 2022 #####
#### Patrick T. Freeman: patrickfreeman1990@gmail.com ####

#### Load libraries ####
library(tidyverse)
library(sf)
library(lubridate)
library(janitor)

#### Load the neighborhood shapefile ####
neighborhoods <- st_read("inputs/spatial/Sacramento-Neighborhoods-shp/Neighborhoods.shp")

#### Load a database the does some consolidation of the OFFENSE_CATEGORY field of the crime database provided by Sacramento Police Department based on PTFreeman's read of offense codes/alignment with FBI's UCR database 

crime_types <- read_csv("inputs/tabular/sacramento-offense-category-mapping.csv") %>%
  dplyr::mutate(Offense_Category_clean = str_replace_all(Offense_Category, "[\r\n]" , "")) %>%
  dplyr::select(-Offense_Category)

#### Load police grids #### 

#### City of Sacramento police district boundaries. Districts are the largest of the areas; the city is divided into 6 Districts. The Districts are divided into Beats, which are assigned to patrol officers. The Beats are divided into Grids for reporting purposes.

police_grids <- st_read("inputs/spatial/POLICE_GRIDS-shp/9c1206b0-8fb0-40ef-a00a-ed622069a08f202042-1-714a1w.4dbgd.shp")

### Load the most recent crime database sourced from ####
crime <- read_csv('inputs/tabular/Sacramento_Crime_Data_From_Current_Year.csv')
### Clean up date information
crime$Occurence_Date <- as_datetime(crime$Occurence_Date)
crime$month <- month(crime$Occurence_Date)
crime$day <- day(crime$Occurence_Date)


#### Get max date of crime dataset ####
max(crime$Occurence_Date)

#### Summarize the data by category by grid by month#####
#### Also do a join to the crime type mapping to consolidate crime types ###
crime_grid <- crime %>% 
  group_by(Grid, Offense_Category, month) %>% 
  dplyr::rename(GRID = Grid) %>% 
  dplyr::summarise(
    count.in.month = n()
  ) %>% 
  dplyr::arrange(., GRID, desc(count.in.month)) %>% 
  dplyr::mutate(Offense_Category_clean = str_remove_all(Offense_Category, " ")) %>%
  dplyr::mutate(Offense_Category_clean = str_replace_all(Offense_Category_clean, "[\r\n]" , "")) %>%
  dplyr::left_join(., crime_types, by="Offense_Category_clean")

#### THIS ENTIRE SECTION CREATES A KEY FOR MAPPING GRIDS TO NEIGHBORHOODS ####
#### Want to determine the police district(s), beat(s), and grid(s) that cover neighborhoods of interest

### Calculate area of intersection of neighborhoods and police grids ####
intersect_pct_grid <- st_intersection(st_make_valid(neighborhoods), st_make_valid(police_grids)) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(NAME, GRID, intersect_area) %>% 
  st_make_valid() %>% 
  st_drop_geometry()

# Create a fresh area variable for neighborhoods
neighborhoods_area <- mutate(neighborhoods, neighborhood_area = st_area(neighborhoods))

# Merge by neighborhood name
neighborhoods_grid <- merge(neighborhoods_area, intersect_pct_grid, by = "NAME", all.x = TRUE)

# Calculate coverage
neighborhoods_grid_coverage  <- neighborhoods_grid %>% 
  mutate(coverage = (as.numeric(intersect_area/neighborhood_area))*100) %>%
  st_drop_geometry() %>% 
  group_by(NAME) %>% 
  dplyr::arrange(., NAME, GRID, desc(coverage)) %>% 
  dplyr::select(NAME, FID, GRID, coverage)

####### SECTION TO PRODUCE TOPLINE SUMMARY OF TOTAL CRIMES AND % CHANGE SINCE PREVIOUS MONTH ######

### Join the police grids to the crime calculations 
### Calculate the sum total of all crimes of all types in each month 
#police_grids_total_crime <- left_join(st_make_valid(police_grids), crime_grid, by="GRID") %>% 
#  dplyr::group_by(GRID, month) %>% 
#  dplyr::summarise(total.crimes.month = sum(count.in.month, na.rm=T)) %>%
 # replace(is.na(.), 0)

### Calculate city-wide statistics to slot in where neighborhoods don't have any reported crimes ####
city.crimes <- crime_grid %>% 
  dplyr::filter(!is.na(month)) %>%
  ungroup() %>%
  group_by(month) %>%
  summarise(curr.total.crimes = sum(count.in.month)) %>%
  dplyr::mutate(year = 2022) %>% ## change as desired
  dplyr::mutate(change.total.rel.last.month = curr.total.crimes - lag(curr.total.crimes, n = 1)) %>%
  dplyr::mutate(
    direction.change.rel.last.month = case_when(
      change.total.rel.last.month > 0 ~ "up",
      change.total.rel.last.month < 0 ~ "down",
      change.total.rel.last.month == 0 ~ "no change",
      is.na(change.total.rel.last.month) ~ "NA"
    )
  ) %>%
  dplyr::mutate(abs.change.total = abs(change.total.rel.last.month)) %>%
  dplyr::mutate(prev.total.crimes = lag(curr.total.crimes, n=1))

### Return the last row of the monthly time series of total crime (the most recent month)
total.crime.city.this.month <- city.crimes %>% 
  dplyr::filter(month == max(month)) %>%
  dplyr::mutate(month_name = month.name[month]) %>%
  dplyr::relocate(month_name, .before=everything())

#### Total crime by type for entire city for the entire time series ####
city.crimes.type <- crime_grid %>% 
  group_by(month, standard.crime.type.label) %>%
  dplyr::summarise(total.of.type = sum(count.in.month)) %>% 
  dplyr::filter(!is.na(month)) %>%
  ungroup() %>%
  pivot_wider(., names_from="standard.crime.type.label", values_from="total.of.type", values_fill=0) %>%
  clean_names()

### Calculate crimes by type for current/most recent month 
city.crimes.type.curr <- city.crimes.type %>%
  dplyr::filter(month == max(month)) %>% 
  dplyr::mutate(month_name = month.name[month]) %>% 
  dplyr::relocate(month_name, .before=everything()) 
  
dim(city.crimes.type.curr)

colnames(city.crimes.type.curr) <- paste("curr", colnames(city.crimes.type.curr),sep="_")

### Calculate crimes by type for previous month
city.crimes.type.prev <- city.crimes.type %>%
  dplyr::filter(month == max(month)-1) %>% 
  dplyr::mutate(month_name = month.name[month]) %>% 
  dplyr::relocate(month_name, .before=everything()) 

colnames(city.crimes.type.prev) <- paste("prev", colnames(city.crimes.type.prev),sep="_")

### Bind both dataframes together 
city.crimes.type.curr.prev <- cbind(city.crimes.type.curr, city.crimes.type.prev)


### Final full city-wide crime data
final.citywide.crime <- cbind(total.crime.city.this.month, city.crimes.type.curr.prev) %>%
  dplyr::mutate(neighborhood = "city-wide") %>%
  relocate(neighborhood, .before=everything()) %>%
  dplyr::mutate(no_crime_flag = "")

### Create list of all column names that need to be in the ultimate dataframe - very important! 
mother.column.names <-  colnames(final.citywide.crime)   # Vector of columns you want in this data.frame


### For-loop for neighborhood-level crime stats or, if no crime, filling in with city-wide total. ####


### Split neighborhoods into list 
neighborhood.list <- split(neighborhoods, f=neighborhoods$NAME)


### Prep list
summary.df.list <- list()

for(i in 1:length(neighborhood.list)){
  
  target <- neighborhood.list[[i]]
  print(target$NAME)
  
  ## Filter to include only grids that cumulatively cover 95% of the area of the neighborhood 
  target_grid_coverage <- neighborhoods_grid_coverage %>% 
    dplyr::filter(NAME == target$NAME) %>% 
    dplyr::arrange(., desc(coverage)) %>% 
    dplyr::mutate(cum_sum = cumsum(coverage)) %>% 
    dplyr::filter(cum_sum <= 96)
  
  ### Filter police grids to include only the target grids identified above
  target_grids <- police_grids %>% 
    dplyr::filter(GRID %in% target_grid_coverage$GRID)
  
  ### join the crime summaries to the target grids for this year ### 
  target_grids_crime <- left_join(st_make_valid(target_grids), crime_grid, by="GRID") 
    
  ### Summarize total crime by month in the neighborhood and calculate absolute change from previous month (data only start in January)
  total.crimes <- target_grids_crime %>%
    dplyr::filter(!is.na(month)) %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(curr.total.crimes = sum(count.in.month)) %>%
    dplyr::mutate(year = 2022) %>%
    dplyr::mutate(NAME = target$NAME) %>%
    st_drop_geometry() %>%
    dplyr::mutate(change.total.rel.last.month = curr.total.crimes - lag(curr.total.crimes, n = 1)) %>%
    dplyr::mutate(
      direction.change.rel.last.month = case_when(
        change.total.rel.last.month > 0 ~ "up",
        change.total.rel.last.month < 0 ~ "down",
        change.total.rel.last.month == 0 ~ "no change",
        is.na(change.total.rel.last.month) ~ "NA"
      )
    ) %>%
    dplyr::mutate(abs.change.total = abs(change.total.rel.last.month)) %>%
    dplyr::mutate(prev.total.crimes = lag(curr.total.crimes, n=1))
  
  ### If nrow of total crimes for neighborhood == 0 
  if(nrow(total.crimes) == 0){
    
    df <- final.citywide.crime %>%
      dplyr::mutate(neighborhood = target$NAME)
     
    
    ### add missing columns 
    Missing <- setdiff(mother.column.names, names(df))  # Find names of missing columns
    df[Missing] <- 0                    # Add them, filled with '0's
    df <- df[mother.column.names]        # Put columns in desired order
    
    ### Set row to NA 
    df[] <- NA
    
    df <- df %>% 
      dplyr::mutate(no_crime_flag = "THERE ARE NO CRIMES REPORTED FOR THIS NEIGHBORHOOD")
    
    df$no_crime_flag <- as.character(df$no_crime_flag)
    
    summary.df.list[[i]] <- df
  }
  
  ### If nrow of total crimes for neighborhood > 0 
  
  if(nrow(total.crimes) > 0){
    
    ### Return the last row of the monthly time series of total crime
    total.crime.curr <- total.crimes %>% 
      dplyr::filter(month == max(month)) %>%
      dplyr::mutate(month_name = month.name[month]) %>%
      dplyr::relocate(month_name, .before = everything())
    
    ### Create crime type dataframes 
    total.crime.type.neighborhood <-  target_grids_crime %>%
      st_drop_geometry() %>%
      group_by(month, standard.crime.type.label) %>%
      dplyr::summarise(total.of.type = sum(count.in.month)) %>% 
      dplyr::filter(!is.na(month)) %>%
      ungroup() %>%
      pivot_wider(., names_from="standard.crime.type.label", values_from="total.of.type", values_fill=0) %>%
      clean_names() %>% 
      dplyr::mutate(no_crime_flag = "THERE ARE CRIMES REPORTED FOR THIS NEIGHBORHOOD")
    
    crimes.type.neighborhood.curr <-total.crime.type.neighborhood  %>%
      dplyr::filter(month == max(month)) %>% 
      dplyr::mutate(month_name = month.name[month]) %>% 
      dplyr::relocate(month_name, .before=everything()) 
    
    dim(crimes.type.neighborhood.curr)
    
    ### add current prefix
    colnames(crimes.type.neighborhood.curr) <- paste("curr", colnames(crimes.type.neighborhood.curr),sep="_")
    
    ### Calculate crimes by type for previous month
    crimes.type.neighborhood.prev <- total.crime.type.neighborhood %>%
      dplyr::filter(month == max(month)-1) %>% 
      dplyr::mutate(month_name = month.name[month]) %>% 
      dplyr::relocate(month_name, .before=everything()) 
    
    ### If the number of rows in crimes.type.neighborhood.prev == 0 fill with NA
    if(nrow(crimes.type.neighborhood.prev) == 0){

      crimes.type.neighborhood.prev <- crimes.type.neighborhood.prev %>% 
        add_row() 
      
    }
    
    colnames(crimes.type.neighborhood.prev) <- paste("prev", colnames(crimes.type.neighborhood.prev),sep="_")
    
    ### Bind both dataframes together 
    neighborhood.crimes.type.curr.prev <- cbind(crimes.type.neighborhood.curr, crimes.type.neighborhood.prev)
    
    ### AND NOW CREATE THE MOTHER OF ALL DATAFRAMES 
    
    df <- cbind(total.crime.curr, neighborhood.crimes.type.curr.prev) %>%
      dplyr::mutate(neighborhood = target$NAME) %>%
      dplyr::relocate(neighborhood, .before=everything())
    
    Missing <- setdiff(mother.column.names, names(df))  # Find names of missing columns
    df[Missing] <- 0                    # Add them, filled with '0's
    df <- df[mother.column.names]                       # Put columns in desired order
  
    ### Convert the no crime flag to character 
    df$no_crime_flag <- as.character(df$no_crime_flag)
    
    summary.df.list[[i]] <- df
    
    }
    
   
}

### Bind all summary statements together into a dataframe
all.summary.statements <- bind_rows(summary.df.list)

### Check nrow - should be 129 neighborhoods
nrow(all.summary.statements)


### Write to csv
#write_csv(all.summary.statements, "outputs/sac-neighborhood-total-crimes-2022-07-14.csv")
