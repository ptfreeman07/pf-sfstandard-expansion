##### Summarizing Year-to-Date Crime Data for Sacramento ####
#### Date updated: July 12, 2022 #####
#### Patrick T. Freeman: patrickfreeman1990@gmail.com ####

#### Load libraries ####
library(tidyverse)
library(sf)
library(lubridate)
library(ggpubr)
library(ggthemes)

#### Load the neighborhood shapefile ####
neighborhoods <- st_read("inputs/spatial/Sacramento-Neighborhoods-shp/Neighborhoods.shp")

#### Load police districts, beats, and grids #### 

#### City of Sacramento police district boundaries. Districts are the largest of the areas; the city is divided into 6 Districts. The Districts are divided into Beats, which are assigned to patrol officers. The Beats are divided into Grids for reporting purposes.

police_districts <- st_read("inputs/spatial/POLICE_DISTRICTS-shp/132d4fc4-0c45-4947-a260-40facf99a7892020328-1-zte2o6.wz1u.shp")

police_beats <- st_read("inputs/spatial/POLICE_BEATS/POLICE_BEATS.shp")

police_grids <- st_read("inputs/spatial/POLICE_GRIDS-shp/9c1206b0-8fb0-40ef-a00a-ed622069a08f202042-1-714a1w.4dbgd.shp")



### Load the most recent crime database ####
crime <- read_csv('inputs/tabular/Sacramento_Crime_Data_From_Current_Year.csv')
crime$Occurence_Date <- as_datetime(crime$Occurence_Date)
crime$month <- month(crime$Occurence_Date)
crime$day <- day(crime$Occurence_Date)


#### Get max date of crime dataset ####
max(crime$Occurence_Date)

#### Summarize the data by category by grid by month#####
crime_grid <- crime %>% 
  group_by(Grid, Offense_Category, month) %>% 
  dplyr::rename(GRID = Grid) %>% 
  dplyr::summarise(
    count.in.month = n()
  ) %>% 
  dplyr::arrange(., GRID, desc(count.in.month))

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
police_grids_total_crime <- left_join(st_make_valid(police_grids), crime_grid, by="GRID") %>% 
  dplyr::group_by(GRID, month) %>% 
  dplyr::summarise(total.crimes.month = sum(count.in.month, na.rm=T)) %>%
  replace(is.na(.), 0)


### Split neighborhoods into list 
neighborhood.list <- split(neighborhoods, f=neighborhoods$NAME)

### Create list to hold results of the percent change in total crimes information 
total.crimes.summary.list <- list()


#### SET MONTH OF ANALYSIS ####
month.of.analysis <- 6 #June 

summary.statements.list <- list()

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
  
  ### join the crime summaries to the target grids for this year and last year ### 
  target_grids_crime <- left_join(st_make_valid(target_grids), crime_grid, by="GRID")
  
  ### Summarize total crime by month in the neighborhood and calculate percent change month over month
  total.crimes <- target_grids_crime %>%
    dplyr::filter(!is.na(month)) %>%
    ungroup() %>%
    group_by(month) %>%
    summarise(total.crimes = sum(count.in.month)) %>%
    dplyr::mutate(year = 2022) %>%
    dplyr::mutate(NAME = target$NAME) %>%
    st_drop_geometry() %>%
    dplyr::mutate(change.total.rel.last.month = total.crimes - lag(total.crimes, n = 1)) %>%
    dplyr::mutate(
      direction.change.rel.last.month = case_when(
        change.total.rel.last.month > 0 ~ "up",
        change.total.rel.last.month < 0 ~ "down",
        change.total.rel.last.month == 0 ~ "no change",
        is.na(change.total.rel.last.month) ~ "NA"
      )
    ) %>%
    dplyr::mutate(abs.change.total = abs(change.total.rel.last.month)) %>%
    dplyr::mutate(last.months.total = lag(total.crimes, n=1))
  
  if(nrow(total.crimes) == 0){
    
    total.crime.statement <- paste0("There have been no crimes recorded in this neighborhood this year.")
    
    total.crime.change.statement <- paste0("There have been no crimes recorded in this neighborhood this year.")
    
    summary.statement.df <- data.frame(
      neighborhood = target$NAME,
      total.crime.statement = total.crime.statement,
      total.crime.change.statement = total.crime.change.statement)
    
    summary.statements.list[[i]] <- summary.statement.df
  }
  
  if(nrow(total.crimes) > 0){
    
    ### Return the last row of the monthly time series of total crime
    total.crime.this.month <- total.crimes %>% 
      dplyr::filter(month == max(month)) %>%
      dplyr::mutate(month_name = month.name[month])
    
    
    total.crime.statement <- paste0("There were ",total.crime.this.month$total.crimes," crimes recorded during the month of ", total.crime.this.month$month_name, ".")
    
    if(total.crime.this.month$abs.change.total == 0){
      
      total.crime.change.statement <-
        paste0(
          "That's ",
          total.crime.this.month$direction.change.rel.last.month,
          " from the previous month's total of ",
          total.crime.this.month$last.months.total,
          "."
        )}
      
   if(total.crime.this.month$abs.change.total > 0){
     total.crime.change.statement <-
       paste0(
         "That's ",
         total.crime.this.month$direction.change.rel.last.month,
         " ",
         total.crime.this.month$abs.change.total,
         " from the previous month's total of ",
         total.crime.this.month$last.months.total,
         "."
       )
   }
        
    summary.statement.df <- data.frame(
      neighborhood = target$NAME,
      total.crime.statement =  total.crime.statement,
      total.crime.change.statement = total.crime.change.statement)
    
    summary.statements.list[[i]] <- summary.statement.df
    
    }
    
   
}

all.summary.statements <- bind_rows(summary.statements.list)

View(all.summary.statements)

nrow(all.summary.statements)

write_csv(all.summary.statements, "outputs/sac-neighborhood-total-crimes-2022-07-12.csv")
