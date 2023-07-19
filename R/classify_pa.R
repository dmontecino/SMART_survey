


# Define a vector of countries
countries.in.pa<-sort(unique(dat_modified$country))


# Set up parallel processing with 6 cores using mclapply to download  
# the protected areas from the word data protected area database

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



dat_modified<-dat_modified %>% 
  unnest(protected_area) %>% 
  mutate(protected_area=tolower(protected_area)) %>% 
  mutate(protected_area=stri_trans_general(protected_area, "Latin-ASCII")) 

dat_modified$protected_area<-
  map(dat_modified$protected_area, \(x) unlist(strsplit(x =x, split =  " - ")[[1]])) %>% 
  map_vec(\(x) x[1])



    

  










#length(countries.in.pa)==length(pas_data)
#all(map_int(pas_data, nrow)>0) TRUE







#find the key words for protected areas in the data.
#first find all the words in the protected area column 
# and then remove the ones like "Park", "Reserve" etc
unique.terms.of.pas=
map(unlist(dat_modified$protected_area), 
        function(x) strsplit(x, " - ")[[1]] %>% 
      stri_trans_general("Latin-ASCII")) %>% 
  map_vec(.f = function(y) head(y, n=1)) %>% 
  map_vec(.f = function(y) str_split(y, "\\s+")) %>% 
  unlist() %>% tolower()

unique.terms.of.pas=table(unique.terms.of.pas)

terms.to.remove=names(unique.terms.of.pas[unique.terms.of.pas>1])

#adding some repeated terms that are key to find protected area names
terms.to.remove=terms.to.remove[
  !(terms.to.remove%in%c("luangwa", "pantanos", "wangchuck"))]

terms.to.remove<-c(terms.to.remove, "monumento", "natural", "sistema", "playon",
                   "leon", "cuenca", "canon", "archipielago", "pok", "rio", 
                   "refugio", "manglares", "alto", "reserva", "paisajistica",
                   "conservancy", "chiquibul", "elettaria", "forest", "reserve",
                   "dorji", "khesar", "hills", "wetlands", "a", "west", "east", 
                   "north","resource")



PAs_detected_per_pa_survey_name<-vector(mode = "list", length = nrow(dat_modified))
  
  for(i in seq_along(dat_modified$protected_area)){
    
    sub.data.temp<-pas_data_data_frame %>% 
      filter(COUNTRY==dat_modified$country[i])
    
    temp.searching.terms<-strsplit(dat_modified$protected_area[i], " ")[[1]]
    
    PAs_detected_per_pa_survey_name[[i]]<-
    map(
      map(temp.searching.terms[!(temp.searching.terms%in%terms.to.remove)], \(x)
          grepl(pattern = x, x = sub.data.temp$NAME,ignore.case = T)), \(y)
      sub.data.temp %>% filter(y)) %>% 
      bind_rows() %>% 
      distinct()} 

#check if the pa found in wdpa makes sense with the reported protecte area in the dataset
names(PAs_detected_per_pa_survey_name)<-paste(dat_modified$protected_area, dat_modified$country,sep =  "-")

# PAs_detected_per_pa_survey_name

#> The two protected areas it doesnot make sense are these
#> checking them manuall shows they are terrestrial
# c("Sambor wildlife sanctuary-Cambodia", "ngulia rhino sanctuary-Kenya")


index_pas_wo_wdpa<-which(map_vec(PAs_detected_per_pa_survey_name, \(x) nrow(x)==0))
# checking thes eprotected areas manually, the are all terrestrial

index_pas_terrestrial<-
  which(map_vec(PAs_detected_per_pa_survey_name, \(x) all(x$MARINE!=2)))



# adding the dat aif te PAs are terrestrial
dat_modified$terrestrial<-NA

dat_modified$terrestrial[index_pas_terrestrial]<-"yes"
dat_modified$terrestrial[index_pas_wo_wdpa]<-"yes" # checking thes eprotected areas manually, the are all terrestrial



# dat_modified %>% filter(terrestrial!="yes" | is.na(terrestrial)) %>% 
#   select(protected_area, country, terrestrial)

# checking the pa with matches in the wdpa that include marine areas
names(PAs_detected_per_pa_survey_name[
map_lgl(PAs_detected_per_pa_survey_name, \(x) any(x$MARINE==2))])



#based on the results above, the marine areas are:

# [1] "parque nacional archipielago de espiritu santo-Mexico"    
# [2] "parque nacional isla contoy-Mexico"                       
# [3] "parque nacional sistema arrecifal veracruzano-Mexico"     
# [5] "parque nacional isla isabel-Mexico"                       
# [6] "reserva de la biosfera islas del pacifico-Mexico"         

#based on the results above, the remainingg terrestrial areas are:

# [4] "area de proteccion de flora y fauna cabo san lucas-Mexico"
# [7] "reserva ecologica arenillas-Ecuador"

dat_modified[
  grepl(pattern = "lucas|arenillas", 
        x = dat_modified$protected_area, 
        ignore.case = T),]$terrestrial<-"yes"


# dat_modified[is.na(dat_modified$terrestrial),]$protected_area

# they all represent a single response so remove the rows form the dataset.
terrestrial_data=dat_modified %>% filter(terrestrial=="yes")



