
# Load required libraries
library(wdpar) # https://developers.google.com/earth-engine/datasets/catalog/WCMC_WDPA_current_polygons
library(parallel)
library(purrr)
library(tidyverse)
library(sf)
library(stringdist)





# Define a vector of countries
countries.in.pa<-sort(unique(dat_modified$))


countries.temp=map(dat_modified$protected_area, function(x)
  strsplit(x = x, ',')[[1]])

countries.temp=map(countries.temp, function(x) strsplit(x = x, ' - ')[[1]])

countries.temp=sort(unique(sapply(countries.temp, function(x) x[length(x)])))

countries.temp=c("Pakistan", "Papua New Guinea", "Peru", "Philippines",                 
                 "Republic of Congo", "Tanzania", "Thailand", "Uganda" )

# Set up parallel processing with 4 cores using mclapply
pas_data<- parallel::mclapply(iso_codes, function(x) wdpa_fetch(x), mc.cores = 6 )

# Find marine areas. 
pas_data=map(pas_data, function(x) x %>% 
              filter(MARINE==2) %>% 
              select(NAME, MARINE) %>% 
              st_drop_geometry())

for (i in seq_along(pas_data)) {pas_data[[i]]$COUNTRY <- countries.temp[i]}

pas_data_marine=do.call(rbind, pas_data)





# Find marine areas based on text distance. the differences in names of protected 
#areas beetween survey and dataase makes it innefficient to match by name


# Load the two datasets (assuming they are data frames)
dataset1 <- read.csv("dataset1.csv")
dataset2 <- read.csv("dataset2.csv")

# Split the datasets by country
dataset1_by_country <- split(pas_data_marine, pas_data_marine$COUNTRY)
dataset2_by_country <- split(dat_modified, dat_modified$country)

# Initialize an empty data frame to store the matched protected areas
matched_data <- data.frame()

# Loop over each country subset
for (country in intersect(names(dataset1_by_country), names(dataset2_by_country))) {
  # Get the protected areas for the current country from each dataset
  dataset1_country <- dataset1_by_country[[country]]
  dataset2_country <- dataset2_by_country[[country]]
  
  # Compute pairwise distances between all pairs of protected area names for the current country
  distances <- stringdistmatrix(dataset1_country$protected_area, dataset2_country$protected_area, method = "lv")
  
  # Find the protected areas with the minimum distances for the current country
  matches <- apply(distances, 1, which.min)
  min_distances <- apply(distances, 1, min)
  
  # Create a new data frame with the matched protected areas for the current country
  matched_data_country <- data.frame(
    protected_area1 = dataset1_country$protected_area,
    country1 = country,
    protected_area2 = dataset2_country$protected_area[matches],
    country2 = country,
    distance = min_distances
  )
  
  # Append the matched protected areas for the current country to the overall matched data
  matched_data <- rbind(matched_data, matched_data_country)
}
