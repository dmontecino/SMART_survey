


# Define a vector of countries
countries.in.pa<-sort(unique(dat_modified_filtered$country))


# -------------------------------------------------------------------- #
# donwloading data from the wdpa database and create a dataset pf wdpa #              
# -------------------------------------------------------------------- #

#> Set up parallel processing with 6 cores using mclapply to download  
#> the protected areas from the world data protected area database

pas_data<- parallel::mclapply(countries.in.pa, 
                              function(x) wdpa_fetch(x, wait = T), mc.cores = 6)

for (i in seq_along(pas_data)){
  pas_data[[i]]$COUNTRY <- countries.in.pa[i]}

names(pas_data) <- countries.in.pa


# wdpa dataset with the NAME MARINE and COUNTRY columns

pas_data_data_frame=
  pas_data %>%
  map(function(x) x %>% 
        select(NAME, MARINE, COUNTRY) %>%
        st_drop_geometry()) 

pas_data_data_frame<- data.frame(do.call(rbind, pas_data_data_frame))

colnames(pas_data_data_frame) <- c("NAME", "MARINE", "COUNTRY")

pas_data_data_frame$NAME<-map_vec(pas_data_data_frame$NAME, \(x) 
                                  stri_trans_general(x, "Latin-ASCII")) %>% 
                                  tolower()






#> expand the dataset based on the real number of protected areas included so 
#> each row represent a sing protected area


dat_modified_filtered_expanded<-dat_modified_filtered %>% 
  unnest(protected_area) %>% 
  mutate(protected_area=tolower(protected_area)) %>% 
  mutate(protected_area=stri_trans_general(protected_area, "Latin-ASCII")) 

dat_modified_filtered_expanded$protected_area<-
  map(dat_modified_filtered_expanded$protected_area, \(x) 
      unlist(strsplit(x =x, split =  " - ")[[1]])) %>% 
  map_vec(\(x) x[1])



  
# -------------------------------------------------------- #
# identifying the PAs in the survey data in wdpa database  #              
# -------------------------------------------------------- #

#> find the key words for protected areas in the survey dataset to id them in
#> the wdpa dataset. 
#> first find all the words in the protected area column 
#> and then remove the ones like "Park", "Reserve" etc that will not guide the 
#> finding of a specific protected areas
#>  in the wdpa database because they are not specific enough

unique.terms.of.pas=
map(unlist(dat_modified_filtered_expanded$protected_area), 
        function(x) strsplit(x, " - ")[[1]] %>% 
      stri_trans_general("Latin-ASCII")) %>% 
  map_vec(.f = function(y) head(y, n=1)) %>% 
  map_vec(.f = function(y) str_split(y, "\\s+")) %>% 
  unlist() %>% tolower()

unique.terms.of.pas=table(unique.terms.of.pas)

terms.to.remove=names(unique.terms.of.pas[unique.terms.of.pas>1])

# removing terms that are actually key for the names of protected areas
terms.to.remove=terms.to.remove[
  !(terms.to.remove%in%c("luangwa", "pantanos", "wangchuck"))]

#adding some repeated terms that are key to find protected area names
terms.to.remove<-c(terms.to.remove, "monumento", "natural", "sistema", "playon",
                   "leon", "cuenca", "canon", "archipielago", "pok", "rio", 
                   "refugio", "manglares", "alto", "reserva", "paisajistica",
                   "conservancy", "chiquibul", "elettaria", "forest", "reserve",
                   "dorji", "khesar", "hills", "wetlands", "a", "west", "east", 
                   "north","resource")



# creating object to store the data from wdpa matching PAs in the survey data
PAs_detected_per_pa_survey_name<-vector(mode = "list", 
                                        length = nrow(dat_modified_filtered_expanded))
  
#getting the wdpa data for each protected area in the survey dataset  
for(i in seq_along(dat_modified_filtered_expanded$protected_area)){
    
  #first suset wdpa PAs in the same country
    sub.data.temp<-pas_data_data_frame %>% 
      filter(COUNTRY==dat_modified_filtered_expanded$country[i])
    
    #then split the name of the ith PA in the survey dataset and search for eaach word 
    # in the names of the pas in the wdpa dataset
    temp.searching.terms<-strsplit(dat_modified_filtered_expanded$protected_area[i], " ")[[1]]
    
    PAs_detected_per_pa_survey_name[[i]]<-
    map(
      map(temp.searching.terms[!(temp.searching.terms%in%terms.to.remove)], \(x)
          grepl(pattern = x, x = sub.data.temp$NAME,ignore.case = T)), \(y)
      sub.data.temp %>% filter(y)) %>% 
      bind_rows() %>% 
      distinct()} 

#check if the pa found in wdpa makes sense with the reported protected area in the dataset manually
names(PAs_detected_per_pa_survey_name)<-paste(dat_modified_filtered_expanded$protected_area, 
                                              dat_modified_filtered_expanded$country,sep =  "-")

PAs_detected_per_pa_survey_name

#> The two protected areas it does not make sense are these
#> c("Sambor wildlife sanctuary-Cambodia", "ngulia rhino sanctuary-Kenya")
#> manual chacking if they are terrestrial or not says they are terrestrial

# index of PAs in the survey dataset wo any matching oa in the wdpa dataset
index_pas_wo_wdpa<-which(map_vec(PAs_detected_per_pa_survey_name, \(x) nrow(x)==0))
# checking these protected areas manually. they are all terrestrial



#> all PAs in the survey datastset matching only terrestrial PAs in the wdpa dataset 
#> are terestrial (MARINE !=2)

index_pas_terrestrial<-
  which(map_vec(PAs_detected_per_pa_survey_name, \(x) all(x$MARINE!=2)))



# adding the terrestrial column to the survey dataset
dat_modified_filtered_expanded$terrestrial<-NA

dat_modified_filtered_expanded$terrestrial[index_pas_terrestrial]<-"yes"
dat_modified_filtered_expanded$terrestrial[index_pas_wo_wdpa]<-"yes" 




# checking the pa with matches in the wdpa that include marine areas
names(PAs_detected_per_pa_survey_name[
map_lgl(PAs_detected_per_pa_survey_name, \(x) any(x$MARINE==2))])
#> manually checking if these PAs are marine or terrestrial
#> based on the results above, the marine areas are:

# [1] "parque nacional archipielago de espiritu santo-Mexico"    
# [2] "parque nacional isla contoy-Mexico"                       
# [3] "parque nacional sistema arrecifal veracruzano-Mexico"     
# [5] "parque nacional isla isabel-Mexico"                       
# [6] "reserva de la biosfera islas del pacifico-Mexico"         

#based on the results above, the remainingg terrestrial areas are:

# [4] "area de proteccion de flora y fauna cabo san lucas-Mexico"
# [7] "reserva ecologica arenillas-Ecuador"

#classifying the remainini terrestiral protected areas in the survey dataset
dat_modified_filtered_expanded[
  grepl(pattern = "lucas|arenillas", 
        x = dat_modified_filtered_expanded$protected_area, 
        ignore.case = T),]$terrestrial<-"yes"


# finally the terrestrial data
terrestrial_data=dat_modified_filtered_expanded %>% filter(terrestrial=="yes")


