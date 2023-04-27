
# Load required libraries
library(wdpar) 
library(parallel)
library(purrr)
library(tidyverse)
library(sf)
library(stringdist)



# Define a vector of countries
countries.in.pa<-sort(unique(dat_modified$country))


# Set up parallel processing with 6 cores using mclapply to download  
# the protected areas from the word data protected area database

pas_data<- parallel::mclapply(countries.in.pa, 
                              function(x) wdpa_fetch(x), mc.cores = 6)

#length(countries.in.pa)==length(pas_data)
#all(map_int(pas_data, nrow)>0) TRUE


# Find marine areas. 
# of the wdpa database legend is here: 
# https://developers.google.com/earth-engine/datasets/catalog/WCMC_WDPA_current_polygons

pas_data_marine=map(pas_data, function(x) x %>% 
              filter(MARINE==2) %>% 
              select(NAME, MARINE) %>% 
              st_drop_geometry())

for (i in seq_along(pas_data_marine)){
  pas_data_marine[[i]]$COUNTRY <- countries.in.pa[i]}



# Find marine areas based on text distance. the differences in names of protected 
# areas beetween survey and dataase makes it inefficient to match by name




# Split the dataset by country
dataset1 <- dat_modified %>% select(protected_area, country)
                            
dataset1 <- unlist(dataset1$protected_area, use.names = F)

dataset1<- data.frame(do.call(rbind,strsplit(dataset1, " - ")))

colnames(dataset1) <- c("protected_area", "country")

#split both datasets by country
dataset1_by_country <- split(dataset1, dataset1$country)
dataset2_by_country <- split(do.call(rbind, pas_data_marine), do.call(rbind, pas_data_marine)[, "COUNTRY"])

# Initialize an empty data frame to store the matched protected areas
matched_data <- vector(mode = "list", 
                       length = length(intersect(names(dataset1_by_country), names(dataset2_by_country))))

names(matched_data)<-intersect(names(dataset1_by_country), names(dataset2_by_country))

# Loop over each country subset
for (country in intersect(names(dataset1_by_country), names(dataset2_by_country))) {
  # Get the protected areas for the current country from each dataset
  dataset1_country <- dataset1_by_country[[country]]
  dataset2_country <- dataset2_by_country[[country]]
  
  # Compute pairwise distances between all pairs of protected area names for the current country
  distances <- stringdistmatrix(dataset1_country$protected_area, dataset2_country$NAME, method = "lv")
  
  # Find the protected areas with the minimum distances for the current country
  matches <- apply(distances, 1, which.min)
  min_distances <- apply(distances, 1, min)
  
  # Create a new data frame with the matched protected areas for the current country
  matched_data_country <- data.frame(
    protected_area_dat = dataset1_country$protected_area,
    country1 = country,
    protected_area_wdpa = dataset2_country$NAME[matches],
    country2 = country,
    distance = min_distances
  )
  
  # Append the matched protected areas for the current country to the overall matched data
  matched_data[[country]] <- matched_data_country
}

# Checking the results the columns 
#  protected_area_dat and protected_area_wdpa match for the Mexico list in 
# three protected areas

#  Parque Nacional Isla Isabel
#  Parque Nacional Sistema Arrecifal Veracruzano
#  Parque Nacional Isla Contoy


# These are removed from the data
indexes.marine.protected.areas=
which(map_lgl(dat_modified$protected_area, \(x) 
    grepl(pattern = "Isabel|Veracruzano|Contoy", x)))

# The responses including these protected areas only included
# each one of them singly. Therefore, just remove the  rows
#dat_modified$protected_area[indexes.marine.protected.areas]

terrestrial_data=dat_modified[-indexes.marine.protected.areas,]


