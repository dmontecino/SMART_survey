library(tidyverse)

dat<-readRDS("/Users/DMontecino/Downloads/genbank_h5n1_aiv_hosts_data_cleaned_hosts_full.RDS")

dat$class_my_tai[is.na(dat$class_my_tai)]<-"Unknown"

dat$order_my_tai[is.na(dat$order_my_tai)]<-"Unknown"

dat$family_my_tai[is.na(dat$family_my_tai)]<-"Unknown"

dat$genus_my_tai[is.na(dat$genus_my_tai)]<-"Unknown"

dat$species_my_tai[is.na(dat$species_my_tai)]<-"Unknown"



dat.not.unknown<-dat %>% filter(species_my_tai!="Unknown")


known.hosts.not.experimental<-
  dat.not.unknown %>% 
  # filter(!(if_any(everything(), grepl, pattern = "2.3.4.4b"))) %>% 
  filter(!(if_any(everything(), grepl, pattern = "experiment"))) %>% 
  distinct(species_my_tai) %>% 
  pull(species_my_tai) %>% sort()

known.hosts.not.experimental.2.3.4.b<-
  dat.not.unknown %>% 
  filter(if_any(everything(), grepl, pattern = "2.3.4.4b|2.3.4.4.b")) %>% 
  filter(!(if_any(everything(), grepl, pattern = "experiment"))) %>% 
  distinct(species_my_tai) %>% 
  pull(species_my_tai) %>% sort()


sort(unique(dat$order_my_tai))

dat.not.unknown %>% 
  # filter(!(if_any(everything(), grepl, pattern = "2.3.4.4b"))) %>% 
  filter(!(if_any(everything(), grepl, pattern = "experiment"))) %>% 
  select(#accession, 
                 #country , 
                 #collection.date, 
                 #organism, 
                 class_my_tai, 
                 order_my_tai,
                 species_my_tai) %>%  distinct() %>% 
                 arrange(class_my_tai, order_my_tai, species_my_tai)






