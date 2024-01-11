


# -------------------------------------------------------------------- #
# downloading data from the wdpa database and create a dataset pf wdpa #              
# -------------------------------------------------------------------- #

#> Set up parallel processing with 6 cores using mclapply to download  
#> the protected areas from the world data protected area database
#> For some weird reason, before running the parallel process, just run wdpa_fetch
#> with a single country. Then the parallel process will work

# wdpa_fetch("Belize", wait = T, page_wait = 5)

dat_modified$country<-map(dat_modified$protected_area, \(x) x$protected_area) %>% 
  map(\(x) strsplit(x=x, split = " - ")[[1]]) %>% 
  map_chr(\(x) tail(x, n = 1))

countries<-sort(unique(dat_modified$country)) #29

## Parallel code to get the data of the protected areas

# registerDoParallel(cl <- makeCluster(6))
# 
# pas_data <- foreach(i = seq_along(countries), .packages='wdpar') %dopar% 
#   {wdpa_fetch(countries[i], wait = T)}
#   
# stopCluster(cl)

# library(future)
# library(doFuture)
library(wdpar)
# future::plan(multisession, workers = 8)

# pas_data <- foreach(i = seq_along(countries)) %dofuture% wdpa_fetch(countries[i], wait = T)

pas_data <- vector(mode = "list", length = length(countries))

for(i in seq_along(countries)){pas_data[[i]] <- wdpa_fetch(countries[i], wait = T)}

for (i in seq_along(pas_data)){pas_data[[i]]$COUNTRY <- countries[i]}

names(pas_data) <- countries


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
#> each row represent a single protected area


dat_modified_local_expanded<-dat_modified %>% 
  unnest(protected_area) %>% 
  mutate(protected_area=tolower(protected_area)) %>% 
  mutate(protected_area=stri_trans_general(protected_area, "Latin-ASCII")) 


dat_modified_local_expanded$protected_area<-
  map(dat_modified_local_expanded$protected_area, \(x) 
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

unique_terms_local_pas<-
map(unlist(dat_modified_local_expanded$protected_area), 
        function(x) strsplit(x, " - ")[[1]] %>% 
      stri_trans_general("Latin-ASCII")) %>% 
  map_vec(.f = function(y) head(y, n=1)) %>% 
  map_vec(.f = function(y) str_split(y, "\\s+")) %>% 
  unlist() %>% tolower()

unique_terms_local_pas<-table(unique_terms_local_pas)

terms_to_remove<-names(unique_terms_local_pas[unique_terms_local_pas>1])

# removing terms that are actually key for the names of protected areas
# terms.to.remove=terms.to.remove[
  # !(terms.to.remove%in%c("luangwa", "pantanos", "wangchuck"))]

#adding some repeated terms that are key to find protected area names
terms_to_remove<-unique(c(terms_to_remove, "monumento", "natural", "sistema", "playon",
                   "leon", "cuenca", "canon", "archipielago", "pok", "rio", 
                   "refugio", "manglares", "alto", "reserva", "paisajistica",
                   "conservancy", "chiquibul", "elettaria", "forest", "reserve",
                   "dorji", "khesar", "hills", "wetlands", "a", "west", "east", 
                   "north","resource", "zona", "zone", "park", "parque", "national",
                   "nacional"))




# creating object to store the data from wdpa matching PAs in the survey data
PAs_detected_per_pa_survey_name<-vector(mode = "list", 
                                        length = nrow(dat_modified_local_expanded))
  
#getting the wdpa data for each protected dat_modified_local_expanded in the survey dataset  
for(i in seq_along(dat_modified_local_expanded$protected_area)){
    
  #first suset wdpa PAs in the same country
    sub.data.temp<-pas_data_data_frame %>% 
      filter(COUNTRY==dat_modified_local_expanded$country[i])
    
    #then split the name of the ith PA in the survey dataset and search for eaach word 
    # in the names of the pas in the wdpa dataset
    temp.searching.terms<-strsplit(dat_modified_local_expanded$protected_area[i], " ")[[1]]
    
    PAs_detected_per_pa_survey_name[[i]]<-
    map(
      map(temp.searching.terms[!(temp.searching.terms%in%terms.to.remove)], \(x)
          grepl(pattern = x, x = sub.data.temp$NAME,ignore.case = T)), \(y)
      sub.data.temp %>% filter(y)) %>% 
      bind_rows() %>% 
      distinct()} 

#check if the pa found in wdpa makes sense with the reported protected area in the dataset manually
names(PAs_detected_per_pa_survey_name)<-paste(dat_modified_local_expanded$protected_area, 
                                              dat_modified_local_expanded$country,sep =  " - ")


pas_no_match<-unique(names(PAs_detected_per_pa_survey_name[map_vec(PAs_detected_per_pa_survey_name, \(x) nrow(x))==0])) #64
PAs_detected_per_pa_survey_name<-PAs_detected_per_pa_survey_name[map_vec(PAs_detected_per_pa_survey_name, \(x) nrow(x))>0] # 193

# Checking the  protected areas manually to find marine ones in local responses

PAs_detected_per_pa_survey_name[193] 


keys_marine_pa<-c("laughing", "blue hole", "half moon", "corozal", 
                  "caulker", "gladden spit", "glover's", "hol chan",
                  "port honduras", 'sapodilla', "south water", 
                  "marine reserve",
                  "karimata", "tiga", "espíritu", "contoy",
                  "arrecifal veracruzano", "cabo san lucas",
                  "isla isabel", "islas del pacífico")


# Create column to store if they are marine or not 

# adding a marine column
for(i in seq_len(nrow(dat_modified))){dat_modified$protected_area[[i]]$marine<-FALSE}


## Adding a marker for marine protected areas 

for(i in seq_along(keys_marine_pa)){
  index_response_with_marine_pa<-
    grep(keys_marine_pa[i], dat_modified$protected_area, ignore.case = T)
  
    for(j in seq_along(index_response_with_marine_pa)){
  
  index_of_marine_pa_within_response<- 
    grep(pattern = keys_marine_pa[i], ignore.case = T,
         x = dat_modified$protected_area[[index_response_with_marine_pa[j]]]$protected_area)
  
  
  dat_modified$protected_area[[index_response_with_marine_pa[j]]]$marine[index_of_marine_pa_within_response]<-TRUE
}
}

# responses_with_marine_areas<-map_vec(dat_modified$protected_area, \(x) all(x$marine==TRUE)) # 119
# index_responses_with_marine_areas<-unname(which(responses_with_marine_areas))
# dat_modified$protected_area[index_responses_with_marine_areas]



#Pas WO Match in the wdpa database (all terrestrial checked one by one)
pas_no_match

# [1] "mann wildlife sanctuary - Myanmar"                      
# [2] "mgeno conservancy - Kenya"                              
# [3] "rombo district - Kenya"                                 
# [4] "mwanga district - Kenya"                                
# [5] "same district - Kenya"                                  
# [6] "lushoto district - Kenya"                               
# [7] "korogwe district - Kenya"                               
# [8] "jomotshangkha wildlife sanctuary - Bhutan"              
# [10] "chiquibul national park - Belize"                       
# [11] "sarstoon-temash national park - Belize"                 
# [12] "chiquibul forest reserve - Belize"                      
# [13] "dinarides mountain range - Croatia"                     
# [14] "tanjung anolengo wildlife reserve - Indonesia"          
# [15] "north luangwa conservation programme - Zambia"          
# [16] "nouabale-ndoki national park - Republic of Congo"       
# [17] "espace tridom inter-zone - Republic of Congo"           
# [18] "lesio-luna gorilla nature reserve - Republic of Congo"  
# [19] "parc national d'azagny - Côte d'Ivoire"                 
# [20] "reserve naturelle partielle d'aghien - Côte d'Ivoire"   
# [21] "reserve naturelle de mabi-yaya - Côte d'Ivoire"         
# [22] "reserve naturelle de bossomatie - Côte d'Ivoire"        
# [23] "endau-rompin national park - Malaysia"                  
# [24] "khoid mogoin gol - Mongolia"                            
# [25] "gunung naning protected forest - Indonesia"             
# [26] "protected area - Madagascar"                            
# [27] "rungwa game reserve - Tanzania"                         
# [28] "kizigo game reserve - Tanzania"                         
# [29] "muhesi game reserve - Tanzania"                         
# [30] "lukwati-piti game reserve - Tanzania"                   
# [31] "mount goplom conservation area - Papua New Guinea"      
# [32] "mount waugerema conservation area - Papua New Guinea"   
# [33] "yasina nature parkk - Papua New Guinea"                 
# [34] "yasuni national park - Ecuador"                         
# [35] "sedilu-ulu sebuyau-lesong landscape - Malaysia"         
# [36] "reserva comunal machiguengay - Peru"                    
# [37] "protected area - Peru"                                  
# [38] "parque nacional del rio abiseo - Peru"                  
# [39] "parque nacional rio abiseo - Peru"                      
# [40] "santuario nacional de calipuy - Peru"                   
# [41] "reserva nacional de calipuy - Peru"                     
# [42] "srepok wildlife sanctuary - Cambodia"                   
# [43] "lanjak-entimau wildlife sanctuary - Malaysia"           
# [44] "sedilu-sebuyau-lesong- landscape  - Malaysia"           
# [45] "nouabale-ndoki national park (nnnp) - Republic of Congo"
# [46] "parque nacional cotacachi-cayapas - Ecuador"            
# [47] "reserva de biosfera tawahka-asangni - Honduras"         
# [48] "protected area - Colombia"                              
# [49] "pnnn - Republic of Congo"                               
# [50] "pnok - Republic of Congo"                               
# [51] "pncd - Republic of Congo"                               
# [52] "rclt - Republic of Congo"                               
# [53] "pnnp - Republic of Congo"                               
# [54] "rngll - Republic of Congo"                              
# [55] "progepp-kabo - Republic of Congo"                       
# [56] "progepp - Republic of Congo"                            
# [57] "ngombe - Republic of Congo"                             
# [58] "etic - Republic of Congo"                               
# [59] "tala - Republic of Congo"                               
# [60] "parque nacional yasuni - Ecuador"                       
# [61] "rungwa kizigo muhesi game reserve - Tanzania"           
# [62] "rungwa - Tanzania"                                      
# [63] "kizigo - Tanzania"                                      
# [64] "muhesi game reserves - Tanzania"  




## Removing the PAs classified as marine

# temp<-do.call(rbind, dat_modified$protected_area)
# nrow(temp)
# nrow(temp[temp$marine==F,])

dat_modified$protected_area<-map(dat_modified$protected_area, \(x) x %>% filter(marine==FALSE))

# leaving responses that contain terrestrial answers
#dat_modified[map_vec(dat_modified$protected_area, nrow)!=0,]

dat_modified<-dat_modified[map_vec(dat_modified$protected_area, \(x) nrow(x))>0,]


## Removing 
#the mongolian ecological police department - Mongolia"
#$`unite de surveillance et de lutte anti-braconnage - Republic of Congo`

dat_modified$protected_area$'27'<-dat_modified$protected_area$'27'[-3,]
dat_modified$protected_area$'17'<-dat_modified$protected_area$'17'[-8,]


# finally the terrestrial data

saveRDS(dat_modified, "data/terrestrial_data.RDS")


