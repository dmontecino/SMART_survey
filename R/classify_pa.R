#test

wdpa1<-read.csv("data/WDPA_Apr2025_Public_csv/WDPA_Apr2025_Public_part1.csv")
wdpa2<-read.csv("data/WDPA_Apr2025_Public_csv/WDPA_Apr2025_Public_part2.csv")
# sort(colnames(wdpa))
# head(wdpa)

wdpa<-rbind(wdpa1, wdpa2)


library(countrycode)
wdpa$COUNTRY<-countrycode(wdpa$ISO3, origin = "iso3c", destination = "country.name")

wdpa[wdpa$ISO3=="CIV",]$COUNTRY<-"Côte d'Ivoire"
wdpa[wdpa$ISO3=="COD",]$COUNTRY<-"Democratic Republic of Congo"
wdpa[wdpa$ISO3=="MMR",]$COUNTRY<-"Myanmar"
wdpa[wdpa$ISO3=="COG",]$COUNTRY<-"Republic of Congo"

pas_data_data_frame<-wdpa |> 
  dplyr::select(NAME, MARINE, COUNTRY) 

pas_data_data_frame$NAME<-stri_trans_general(pas_data_data_frame$NAME, "Latin-ASCII") |> tolower()




# get the countries in the responses dataset
dat_modified$protected_area<-
  map(dat_modified$protected_area, \(x)
      x %>% 
        mutate(country= 
                 strsplit(x$protected_area, " - ")  %>% 
                 map_vec(\(y) y[2])))


list_countries<-sort(unique(unlist(dat_modified$protected_area %>% map(\(x) x$country)))) #30


# restrict wdpa dataset to the countries present in the dataset

pas_data_data_frame<-pas_data_data_frame[pas_data_data_frame$COUNTRY%in%list_countries,]

# length(unique(pas_data_data_frame$COUNTRY)) == length(list_countries) # T
# identical(sort(unique(pas_data_data_frame$COUNTRY)), sort(list_countries)) # T


#> Expand the original dataset so each protected area included is represented in a single 
#> row

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
      map(temp.searching.terms[!(temp.searching.terms%in%terms_to_remove)], \(x)
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

PAs_detected_per_pa_survey_name[1]

keys_marine_pa<-c("laughing", "halfmoon", "corozal", 
                  "caulker", "gladden spit", "glover's", "hol chan",
                  "port honduras", 'sapodilla', "south water", 
                  "marine reserve",
                  "karimata", "tiga", "espíritu santo", "contoy",
                  "arrecifal veracruzano", "cabo san lucas",
                  "isla isabel", "islas del pacífico", "Maite", "Cabangcalan",
                  "Olang", "Tulapos", "Cangbagsa", "Caticugan" , "marine protected area")


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

 # responses_with_marine_areas<-map_vec(dat_modified$protected_area, \(x) all(x$marine==TRUE)) # 120
# index_responses_with_marine_areas<-unname(which(responses_with_marine_areas))
# dat_modified$protected_area[index_responses_with_marine_areas]



#Pas WO Match in the wdpa database 
# pas_no_match

# [1] "mann wildlife sanctuary - Myanmar"                                                      
# [2] "wangchuck centennial national park - Bhutan"                                            
# [3] "tapir mountain nature reserve - Belize"                                                 
# [4] "mgeno conservancy - Kenya"                                                              
# [5] "taita hills sanctuary - Kenya"                                                          
# [6] "taita wildlife conservancy - Kenya"                                                     
# [7] "rombo district - Tanzania"                                                              
# [8] "mwanga district - Tanzania"                                                             
# [9] "same district - Tanzania"                                                               
# [10] "jomotshangkha wildlife sanctuary - Bhutan"                                              
# [11] "phibsoo wildlife sanctuary - Bhutan"                                                    
# [12] "jigme singye wangchuck national park - Bhutan"                                          
# [13] "bacalar chico national park - Belize"                                                   
# [14] "chiquibul national park - Belize"                                                       
# [15] "monkey bay national park - Belize"                                                      
# [16] "payne's creek national park - Belize"                                                   
# [17] "sarstoon-temash national park - Belize"                                                 
# [18] "blue hole\tnatural monument - Belize"                                                   
# [19] "bladen nature reserve - Belize"                                                         
# [20] "caye caulker forest reserve - Belize"                                                   
# [21] "chiquibul forest reserve - Belize"                                                      
# [22] "monkey caye forest reserve - Belize"                                                    
# [23] "maya mountain forest reserve - Belize"                                                  
# [24] "bacalar chico marine reserve - Belize"                                                  
# [25] "caye caulker marine reserve - Belize"                                                   
# [26] "south water caye marine reserve - Belize"                                               
# [27] "dinarides mountain range - Croatia"                                                     
# [28] "tanjung anolengo wildlife reserve - Indonesia"                                          
# [29] "conservation south luangwa - Zambia"                                                    
# [30] "nouabale-ndoki national park - Republic of Congo"                                       
# [31] "management of peripheral ecosystems in nouabale-ndoki national park - Republic of Congo"
# [32] "lake tele community reserve - Republic of Congo"                                        
# [33] "espace tridom inter-zone - Republic of Congo"                                           
# [34] "lesio-luna gorilla nature reserve - Republic of Congo"                                  
# [35] "reserve naturelle de bossomatie - Côte d'Ivoire"                                        
# [36] "endau-rompin national park - Malaysia"                                                  
# [37] "gunung naning protected forest - Indonesia"                                             
# [38] "protected area - Madagascar"                                                            
# [39] "rungwa game reserve - Tanzania"                                                         
# [40] "kizigo game reserve - Tanzania"                                                         
# [41] "muhesi game reserve - Tanzania"                                                         
# [42] "lukwati-piti game reserve - Tanzania"                                                   
# [43] "rukwa game reserve - Tanzania"                                                          
# [44] "mount goplom conservation area - Papua New Guinea"                                      
# [45] "mount waugerema conservation area - Papua New Guinea"                                   
# [46] "yasina nature parkk - Papua New Guinea"                                                 
# [47] "yasuni national park - Ecuador"                                                         
# [48] "4.) tulapos marine protected area - Philippines"                                        
# [49] "6.) caticugan marine protected area - Philippines"                                      
# [50] "reserva de biosfera maya - Guatemala"                                                   
# [51] "batang ai national park - Malaysia"                                                     
# [52] "lanjak-entimau wildlife sanctuary - Malaysia"                                           
# [53] "sedilu-ulu sebuyau-lesong landscape - Malaysia"                                         
# [54] "reserva comunal machiguengay - Peru"                                                    
# [55] "santuario nacional cordillera de colan - Peru"                                          
# [56] "zona reservada ancon - Peru"                                                            
# [57] "protected area - Peru"                                                                  
# [58] "parque nacional huascaran - Peru"                                                       
# [59] "reserva nacional matses - Peru"                                                         
# [60] "parque nacional del rio abiseo - Peru"                                                  
# [61] "parque nacional rio abiseo - Peru"                                                      
# [62] "santuario nacional de calipuy - Peru"                                                   
# [63] "reserva nacional de calipuy - Peru"                                                     
# [64] "srepok wildlife sanctuary - Cambodia"                                                   
# [65] "lake tele community reserve (lcr) - Republic of Congo"                                  
# [66] "parque nacional cotacachi-cayapas - Ecuador"                                            
# [67] "reserva del hombre y la biosfera de rio platano - Honduras"                             
# [68] "reserva de biosfera tawahka-asangni - Honduras"                                         
# [69] "protected area - Colombia"                                                              
# [70] "pnnn - Republic of Congo"                                                               
# [71] "pnok - Republic of Congo"                                                               
# [72] "pncd - Republic of Congo"                                                               
# [73] "rclt - Republic of Congo"                                                               
# [74] "pnnp - Republic of Congo"                                                               
# [75] "rngll - Republic of Congo"                                                              
# [76] "progepp-kabo - Republic of Congo"                                                       
# [77] "progepp - Republic of Congo"                                                            
# [78] "ngombe - Republic of Congo"                                                             
# [79] "etic - Republic of Congo"                                                               
# [80] "tala - Republic of Congo"                                                               
# [81] "parque nacional yasuni - Ecuador"                                                       
# [82] "muhezi game reserve - Tanzania"   

# completing the marine data

for(i in seq_along(c("bacalar", "blue hole", "marine reserve"))){
  index_response_with_marine_pa<-
    grep(keys_marine_pa[i], dat_modified$protected_area, ignore.case = T)
  
  for(j in seq_along(index_response_with_marine_pa)){
    
    index_of_marine_pa_within_response<- 
      grep(pattern = keys_marine_pa[i], ignore.case = T,
           x = dat_modified$protected_area[[index_response_with_marine_pa[j]]]$protected_area)
    
    
    dat_modified$protected_area[[index_response_with_marine_pa[j]]]$marine[index_of_marine_pa_within_response]<-TRUE
  }
}



