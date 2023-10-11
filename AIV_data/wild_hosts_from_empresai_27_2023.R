library(taxize)
library(tidyverse)
library(auk)
library(myTAI)

dat<-read.csv("~/Downloads/ai_empres_i_202309272010.csv")

dat<-dat[-c(1:11),]

names(dat)<-c("Event_ID",	
              "Disease",
              "Serotype",
              "Region",
              "Subregion",
              "Country",
              "Admin_level",
              "Locality",
              "Latitude",
              "Longitude",
              "Diagnosis_source",
              "Diagnosis_status",
              "Animal_type",
              "Species", 
              "Observation_date",
              "Report_date",
              "Humans_affected",
              "Human_deaths")



latin_terms_in_parethensis_host<-
  str_match_all(sort(unique(dat$Species)), "\\((.*?)\\)") %>% map(\(x) x[,2])

latin_host<-rep(NA, length(sort(unique(dat$Species))))


for(i in seq_along(sort(unique(dat$Species)))){
  
  #> if no latin terms between parenthesis in the host original name
  #> then use the original name
  
  if(length(latin_terms_in_parethensis_host[[i]])==0){ 
       
  latin_host[i]<-sort(unique(dat$Species))[i]}
  
  # if the content has the unidentified, incognita, or unspecified then use the last term in parenthesis 
  if(any(grepl("Incognita|Unidentified|Unspecified",
           x = latin_terms_in_parethensis_host[[i]]))){
  
  latin_host[i]<-tail(latin_terms_in_parethensis_host[[i]],1)}
  
  if(is.na(latin_host[i])){
    latin_host[i]<-latin_terms_in_parethensis_host[[i]]}
  
}
       


       
host<-
tibble(original_sp=sort(unique(dat$Species)),
       latin_term=latin_host)

# host$latin_term<-host$latin_term






# get scientific names from common names ncbi

common_to_scientific<-comm2sci(com=host$latin_term)

common_to_scientific_ncbi<-tibble(latin_term=names(unlist(common_to_scientific)),
                                  scientific_ncbi=unlist(common_to_scientific, use.names = F))


host<-left_join(host,common_to_scientific_ncbi)


#get scientific names of birds from common names e bird

host$scientific_ebird<-ebird_species(x = host$latin_term, type = "scientific")

host<-
host %>%
  mutate(ncbi_ebird = coalesce(scientific_ncbi, scientific_ebird))

host$host[grepl("idae|inae", host$latin_term)]<-
  host$latin_term[grepl("idae|inae", host$latin_term)]



#> get taxonomy of remaining species. If they are found it means 
#> the scientific name od the species (e.g., mammals) is correct

#temp<-host %>%  filter(is.na(host))


# check_remain_latin_tax<-taxonomy( organism = host$latin_term, 
#                      db       = "ncbi",
#                      output   = "classification" )

check_remain_latin_tax<-vector(mode = "list", length = nrow(host))

for(i in 1:nrow(host)){
check_remain_latin_tax[[i]]<-
      myTAI::taxonomy( organism = host$latin_term[i],
                       db       = "ncbi",
                       output   = "classification" )}


host$ncbi_all<-map_vec(check_remain_latin_tax, \(x) x[nrow(x),1])

host<-
  host %>%
  mutate(final = coalesce(host, ncbi_all))

#data.frame(host %>%  select(latin_term, final))




temp<-host[is.na(host$final),] %>% select(original_sp, latin_term)

data.frame(temp)

temp<-host 

data.frame(temp)



host[host$original_sp=="Accipitridae (Incognita)",]$final<-"Accipitridae"   
host[host$original_sp=="Anatidae (Incognita)",]$final<-"Anatidae"   
host[host$original_sp=="Arctic Skua (Or Parasitic Jaeger)",]$final<-"Stercorarius parasiticus"   
host[host$original_sp=="Arctic Tern (Sterna Paradisea)",]$final<-"Sterna paradisaea"   
host[host$original_sp=="Buzzard",]$final<-"Buteo sp."   
host[host$original_sp=="Cabot's Tern (Thalasseus Acuflavidus)",]$final<-"Thalasseus acuflavidus"   
host[host$original_sp=="Cape Gannet Shy Albatross",]$final<-"Thalassarche cauta"   
host[host$original_sp=="Casmerodiu",]$final<-"Ardea alba"   
host[host$original_sp=="Civet",]$final<-"Viverridae"   
host[host$original_sp=="Condor",]$final<-"Cathartidae"   
host[host$original_sp=="Corvidae (Incognita)",]$final<-"Corvidae"   
host[host$original_sp=="Crane",]$final<-"Gruiformes"   
host[host$original_sp=="Crow",]$final<-"Corvidae"   
host[host$original_sp=="Curlew",]$final<-"Scolopacidae"   
host[host$original_sp=="Egret",]$final<-"Ardeidae"   
host[host$original_sp=="European Turtle Dove",]$final<-"Streptopelia turtur"   
host[host$original_sp=="Falcon",]$final<-"Falconidae"   
host[host$original_sp=="Fox",]$final<-"Canidae"   
host[host$original_sp=="Goosander",]$final<-"Mergus merganser"   
host[host$original_sp=="Grebe",]$final<-"Podicipedidae"   
host[host$original_sp=="Guanay Cormorant (Leucocarbo Bougainvilliorum)",]$final<-"Leucocarbo bougainvillii"   
host[host$original_sp=="Guanay Cormorant Or Guanay Shag (Leucocarbo Bougainvilliorum)",]$final<-"Leucocarbo bougainvillii"   
host[host$original_sp=="Hawk",]$final<-"Falconidae"   
host[host$original_sp=="Heron",]$final<-"Ardeidae"   
host[host$original_sp=="Jackass Penguin Spheniscus Demersus",]$final<-"Spheniscus demersus"   
host[host$original_sp=="Kite",]$final<-"Accipitridae"   
host[host$original_sp=="Magpie",]$final<-"Corvidae"   
host[host$original_sp=="Nyctereutes Viverrinus (Japanese Racoon Dog)",]$final<-"Nyctereutes viverrinus"   
host[host$original_sp=="Owl",]$final<-"Strigiformes"   
host[host$original_sp=="Oyster Catcher",]$final<-"Haematopus"   
host[host$original_sp=="Pigeon",]$final<-"Columbidae"   
host[host$original_sp=="Polar Fox",]$final<-"Vulpes lagopus"   
host[host$original_sp=="Polecat",]$final<-"Mustelidae"   
host[host$original_sp=="Red Breasted Goose",]$final<-"Branta ruficollis"   
host[host$original_sp=="Sea Lion",]$final<-"Otariidae"   
host[host$original_sp=="South America Fur Seal (Arctophoca Australis)",]$final<-"Arctophoca australis"   
host[host$original_sp=="Stone Marten",]$final<-"Martes foina"   
host[host$original_sp=="Stork",]$final<-"Ciconiidae"   
host[host$original_sp=="Swallow",]$final<-"Hirundinidae"   
host[host$original_sp=="Swan",]$final<-"Cygnus"   
host[host$original_sp=="Teal",]$final<-"Accipitridae"   
host[host$original_sp=="Tern",]$final<-"Accipitridae"   
host[host$original_sp=="Unspecified Bird",]$final<-"Aves"   
host[host$original_sp=="Unspecified Mammal",]$final<-"Mammalia"   
host[host$original_sp=="Vulture",]$final<-"Cathartidae"   
host[host$original_sp=="Wader",]$final<-"Charadriiformes"   
host[host$original_sp=="Water Hen",]$final<-"Rallidae"   
host[host$original_sp=="Wild Duck",]$final<-"Anatidae"   
host[host$original_sp=="Wild Fox",]$final<-"Canidae"   
host[host$original_sp=="Wild Tundra Swan",]$final<-"Cygnus columbianus"   

#sort(unique(host$final))
# "Anser sp."
# "Buteo sp."
# "Numididae sp." 
# "unidentified"

host[host$final=="unidentified",]$final<-"Ardeidae"
host[host$final=="Numididae sp.",]$final<-"Numididae"
host[host$final=="Buteo sp.",]$final<-"Buteo"
host[host$final=="Anser sp.",]$final<-"Anser sp"

# sort(unique(host$final))
table(host$final)

wild_species_affected_confirmed_empres_i<-
sort(
  unique(
host$final[
which(map_vec(strsplit(host$final, split = " "), \(x) length(x))>1)]))

# number of species identified with confirmed H5N1
length(wild_species_affected_confirmed_empres_i)

