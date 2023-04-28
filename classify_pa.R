
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

for (i in seq_along(pas_data)){
  pas_data[[i]]$COUNTRY <- countries.in.pa[i]}

names(pas_data) <- countries.in.pa


#length(countries.in.pa)==length(pas_data)
#all(map_int(pas_data, nrow)>0) TRUE



#find the key words for protected areas in the data.
#first find all the words in the protected area column 
# and then remove the ones like "Park", "Reserve" etc
unique.terms.of.pas=
map(unlist(dat_modified$protected_area), 
        function(x) strsplit(x, " - ")[[1]]) %>% 
  map_vec(.f = function(y) head(y, n=1)) %>% 
  map_vec(.f = function(y) str_split(y, "\\s+")) %>% 
  unlist()

unique.terms.of.pas=table(unique.terms.of.pas)

terms.to.remove=names(unique.terms.of.pas[unique.terms.of.pas>1])

#adding some repeated terms that are key to find protected area names
terms.to.remove=terms.to.remove[
  !(terms.to.remove%in%c("Luangwa", "Pantanos", "Wangchuck"))]

#deleting the "useless" terms
keyword.to.search.for.pas.in.data=
  names(unique.terms.of.pas)[
    !(names(unique.terms.of.pas)%in%terms.to.remove)]

# removing more "useless" terms mannually
keyword.to.search.for.pas.in.data=
  keyword.to.search.for.pas.in.data[
    !(keyword.to.search.for.pas.in.data%in%
        c("Alto", 'Alta', "Archipiélago", "Autonoma", "Biósfera", "Barbara",
          "Cabo", "Cerro", "Canon",
          "Community",
          "Conservancy", "Descentralizada",
          "Divisor", "Ecológica", "Falls", "Landscape","Hills","Islas",
          "mountain", "Mountain", "nacional", "Nor", "North",
          "Pacífico", "Paisajística", "Pantanos",
          "Proteccion", "Protección", "protegida",
          "Refugio", "Reserva", "Río", "San", "Sierra", "Sistema",
          "South", "Special", "Chetu", "Cóndor", "Playa", "Playon", "Pampa",
          "Resource", "Santo", "Muja-Cordillera", "range", "Triangle"))
  ]

# replaace Endau-Rompin  with Rompin
keyword.to.search.for.pas.in.data[
  keyword.to.search.for.pas.in.data=="Endau-Rompin"]<-"Endau Rompin"

# wdpa dataset with te NAME MARINE and COUNTRY columns

pas_data_data_frame=
pas_data %>%
  map(function(x) x %>% 
  select(NAME, MARINE, COUNTRY) %>%
  st_drop_geometry()) 

pas_data_data_frame<- data.frame(do.call(rbind, pas_data_data_frame))

colnames(pas_data_data_frame) <- c("NAME", "MARINE", "COUNTRY")



#search for protected areas in the wdpa based on keywords from the
#protected area names in the surveys

pas_in_wdpa_dataset_matching_keywords_pas_survey=
map(keyword.to.search.for.pas.in.data, 
    function(x) 
      pas_data_data_frame %>% 
      filter(str_detect(string = pas_data_data_frame$NAME, 
                    pattern = x)))


# keywords not mtching a pa in the wdpa data
keywords.not.matching.a.pa=
  keyword.to.search.for.pas.in.data[
  map_vec(pas_in_wdpa_dataset_matching_keywords_pas_survey, nrow)==0]


# now search the protected areas in the survey based on the keywords
#length(unlist(dat_modified$protected_area)) # 83

pas_in_survey_dataset_matching_keywords_pas_survey=
  map(keyword.to.search.for.pas.in.data, 
      function(x) 
        dat_modified %>% 
        filter(str_detect(string = unlist(dat_modified$protected_area), 
                          pattern = x)) %>% 
        select(protected_area, country) %>% 
        st_drop_geometry() %>% 
        unnest(protected_area)
      )

result<-vector(mode = "list", 
               length = length(keyword.to.search.for.pas.in.data))

for(i in seq_along(keyword.to.search.for.pas.in.data)){
# Check if both datasets have at least one row
if (nrow(pas_in_wdpa_dataset_matching_keywords_pas_survey[[i]])>0) {
  # Perform full join
  result[[i]] <- full_join(pas_in_survey_dataset_matching_keywords_pas_survey[[i]], 
                      pas_in_wdpa_dataset_matching_keywords_pas_survey[[i]], 
                      by = c("country" = "COUNTRY"), multiple = "all")
} else {
  # Handle empty datasets
  if (nrow(pas_in_wdpa_dataset_matching_keywords_pas_survey[[i]])  == 0) {
    result[[i]] <- pas_in_survey_dataset_matching_keywords_pas_survey[[i]]
  } 
}}


names(result)<-keyword.to.search.for.pas.in.data

#remove rows that have NA in protected areas because that means that the 
# matched protected area does not beloong to the same country

result<-map(result, \(x) x %>% filter(!is.na(protected_area)))



#split the results by protected area
result_2<-do.call(rbind, result[sapply(result, ncol, USE.NAMES = F)==4])

result_2<-split(result_2, result_2$protected_area)
  
result_2<-map(result_2, unique)

#find those PAs with at least one MARINE ==2
# legend of the wdpa database legend is here: 
# https://developers.google.com/earth-engine/datasets/catalog/WCMC_WDPA_current_polygons


result_2<-result_2[map_lgl(result_2, \(x) any(x$MARINE==2))]

#based on the results above, the marine areas are:

marine<-c("Parque Nacional Archipiélago de Espíritu Santo - Mexico",
          "Parque Nacional Isla Contoy - Mexico",
          "Parque Nacional Isla Isabel - Mexico",
          "Parque Nacional Sistema Arrecifal Veracruzano - Mexico")


# manually checking those keywords that generated a non-satisfacetory matching 
# between the protected area name in the survey and protected area names in 
# the wdpa

# Reserva Comunal Ashaninka - Peru
# Area de Proteccion de Flora y Fauna Cabo San Lucas - Mexico
# Parque Nacional Archipiélago de Espíritu Santo - Mexico Mexico
# Mara Triangle - Kenya


marine<-c(marine, "Area de Proteccion de Flora y Fauna Cabo San Lucas - Mexico")

# keywords wo matching
result3<-result[sapply(result, ncol, USE.NAMES = F)==2]

#no other marine area based on the protected areas in result 3




# These are removed from the data
indexes.marine.protected.areas=
map_lgl(dat_modified$protected_area, \(x) x%in%marine)
    

# they all represent a single response so remove the rows form the dataset.
terrestrial_data=dat_modified[-indexes.marine.protected.areas,]


