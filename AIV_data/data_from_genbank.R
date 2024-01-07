library(furrr)
library(purrr)
library(xml2)
library(parallel)
library(lubridate)
library(tidyverse)
library(janitor)

queries<-"(H5N1) AND Influenza A virus[porgn] AND 2020/10/01[PDAT]:2023/10/10[PDAT]"

# Define query parameters
db <- "nucleotide"
retmax <- 99000

# Construct URL for the query

url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/",
              "esearch.fcgi?db=", db,
              "&term=", 
              URLencode(queries),
              "&retmax=", retmax)

# Get the XML content of the search results
xml <- map(url, function(url) read_xml(url))

# Extract the list of matching IDs from the search results
ids <- map(xml, function(xml) xml_text(xml_find_all(xml, "//IdList/Id")))

#unique ids from all queries
ids<-unique(unlist(ids, use.names = F))

# Construct URL for retrieving the records
# ad00c6777dcbd34e47c9cceb00d0bf8eed09	
urls <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/",
               "efetch.fcgi?db=", db,
               # "&id=", paste(ids, collapse = ","),
               "&id=", ids,
               "&rettype=xml")



xmls<-vector(mode = "list", length = length(seq_along(urls)))

for(i in seq_along(urls)){
xmls[[i]]<-urls[i] %>% read_xml() %>% xml_serialize(NULL)
cat(i, sep = ", ")}


saveRDS(xmls, file = "/Users/DMontecino/Downloads/genbank_aiv_hosts_h5n1.RDS")
# xmls6<-readRDS( file = "/Users/DMontecino/Downloads/genbank_aiv_hosts_6.RDS")

xml<-lapply(xmls, xml_unserialize)


dat<-mclapply(seq_along(xml), function(x)
  
  data.frame(
    
    name= ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_locus"))==0, NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_locus"))),
    
    accession = ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_primary-accession"))==0, NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_primary-accession"))),
    
    common.name=ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_source"))==0, NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_source"))),
    
    description=ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_definition"))==0,
      NA, xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_definition"))),
    
    note=ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), 
                          ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='note']/GBQualifier_value"))==0,
      NA, 
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), 
                            ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='note']/GBQualifier_value"))),
    
    molecule.type=ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_moltype"))==0, NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_moltype"))),
    
    organism= ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_organism"))==0, NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_organism"))),
    
    taxonomy= ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_taxonomy"))==0, 
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_taxonomy"))),
    
    collection.date =ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), 
                          ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='collection_date']/GBQualifier_value"))==0,
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), 
                            ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='collection_date']/GBQualifier_value"))),
    
    created.date = ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_create-date"))==0,
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_create-date"))),
    
    country=ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='country']/GBQualifier_value"))==0,
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), 
                            ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='country']/GBQualifier_value"))),
    
    host = ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='host']/GBQualifier_value"))==0, 
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), 
                            ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='host']/GBQualifier_value"))),
    
    pubmed.id=ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_references/GBReference[1]/GBReference_pubmed"))==0, 
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_references/GBReference[1]/GBReference_pubmed"))),
    
    title= ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_references/GBReference[1]/GBReference_title"))==0, 
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_references/GBReference[1]/GBReference_title"))),
    
    fulljournalname = ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_references/GBReference[1]/GBReference_journal"))==0, 
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_references/GBReference[1]/GBReference_journal")))), 
  
  mc.cores=6)

gc()

#dataframe
dat<-do.call(rbind , dat)

#modify the date
dat$created.date <- dmy(dat$created.date)

gc()


sort(unique(dat$host))
# dir.create("Data/Original_data_dataframe")
clade<-
dat %>% 
  filter(if_any(everything(), grepl, pattern = "2.3.4.4b"))

sort(unique(clade$host))


nrow(dat)
sort(unique(dat$host))

library(skimr)
skim(dat)

#save the data
#saveRDS(dat, "/Users/DMontecino/Downloads/genbank_h5n1_aiv_hosts_data.RDS")
dat<-readRDS("/Users/DMontecino/Downloads/genbank_h5n1_aiv_hosts_data.RDS")


#auk_set_ebd_path("/Users/DMontecino/Downloads/")

dat$host<-tolower(dat$host)
dat$host<-trimws(dat$host, which = "both")
dat$host<-gsub("_", " ", dat$host)
dat$host<-gsub(" domesticus| var.domesticus", "", dat$host)
dat$host[grepl(pattern = "anser albifrons", x = dat$host)]<-"greater white-fronted goose"
dat$host<-gsub("backyard ", "", dat$host)
dat$host[grepl(pattern = "bald eagle; haliaeetus leucocephalus", x = dat$host)]<-"haliaeetus leucocephalus"
dat$host[grepl(pattern = "belcher gull|belcher's gull", x = dat$host)]<-"larus belcheris"
dat$host[grepl(pattern = "bulbucus ibis", x = dat$host)]<-"haliaeetus leucocephalus"
dat$host[grepl(pattern = "buteo japonicus", x = dat$host)]<-"buteo japonicus"
dat$host[grepl(pattern = "chicken|chciken|poultry|chichen", x = dat$host)]<-"gallus gallus"
dat$host<-gsub(" spp.| sp.", "", dat$host)
dat$host<-gsub("anade", "anas", dat$host)
dat$host[dat$host=="env"]<-"environment"
dat$host[dat$host=="feline"]<-"felidae"
dat$host[grepl(pattern = "anser cygnoides", x = dat$host)]<-"Anser cygnoides"
dat$host[dat$host=="gys fulvus"]<-"Gyps fulvus"
dat$host<-gsub(" blue-morph| white-morph", "", dat$host)
dat$host[dat$host=="silver pheasant; lophura nycthemera"]<-"lophura nycthemera"
dat$host[grepl(pattern = "cygnus", x = dat$host)]<-"cygnus"
dat$host[grepl(pattern = "turkey", x = dat$host)]<-"meleagris gallopavo"
dat$host[dat$host=="u"]<-"unknown"
dat$host<-gsub("wild", "", dat$host)
dat$host[grepl(pattern = "bird", x = dat$host)]<-"aves"
dat$host[grepl(pattern = "aix sponsa", x = dat$host)]<-"aix sponsa"
dat$host<-trimws(dat$host, which = "both")

sort(unique(dat$host))

library(taxize)
library(auk)

raw_host_to_scientific<-taxize::comm2sci(tolower(sort(unique(dat$host))))


host_dat<-tibble(genbank_host=tolower(sort(unique(dat$host))),
                 taxize_to_sci_host=map_vec(raw_host_to_scientific, \(x) ifelse(length(x)==0, NA, x)))

# ebird_to_sci
host_dat$ebird_to_sci_host<-NA

host_dat$ebird_to_sci_host[is.na(host_dat$taxize_to_sci_host)]<-
auk::ebird_species(host_dat[is.na(host_dat$taxize_to_sci_host),]$genbank_host, type="scientific")




library(myTAI)

# Sys.setenv(ENTREZ_KEY=your_entrez_key)

order=vector(mode = "list", length = nrow(host_dat))

for(i in 162:nrow(host_dat)){

  order[[i]]=
    
    taxonomy( organism = host_dat$genbank_host[i],
              db       = "ncbi",
              output   = "classification")#}
  
  cat(paste0(i, " "))}

#saveRDS(order, file = "/Users/DMontecino/OneDrive - Wildlife Conservation Society/PAPERS/SMART_survey/AIV_data/my_TAI_genbak_hosts_data.RDS")
order<-readRDS(file = "/Users/DMontecino/OneDrive - Wildlife Conservation Society/PAPERS/SMART_survey/AIV_data/my_TAI_genbak_hosts_data.RDS")


order2<-map(order, \(x) if(length(x)==1){
  tibble(class=NA,
         order=NA, 
         family=NA, 
         genus=NA,
         species=NA)}else{
           x %>% 
             select(-id) %>% 
             pivot_wider(names_from = rank, values_from = name) %>% 
             clean_names()})

order2<-bind_rows(order2) %>% select(class, order, family, genus, species)


order2$class<-map_vec(order2$class, \(x) ifelse(is.null(x), NA, x))
order2$order<-map_vec(order2$order, \(x) ifelse(is.null(x), NA, x))
order2$family<-map_vec(order2$family, \(x) ifelse(is.null(x), NA, x))
order2$genus<-map_vec(order2$genus, \(x) ifelse(is.null(x), NA, x))
order2$species<-map_vec(order2$species, \(x) ifelse(is.null(x), NA, x))

order2<-order2 %>% rename(c("class_my_tai"=class,
                            "order_my_tai"=order, 
                            "family_my_tai" = family,
                            "genus_my_tai" = genus,
                            "species_my_tai" = species))


host_dat<-bind_cols(host_dat, order2)





#hosts wo scientific name originally by given a sicientific name given by ebird based on the common name
# so I will try to find the full info ased on the data provided by ebird 
host_dat_missing<-
host_dat %>% 
  filter(is.na(order_my_tai) & !is.na(host_dat$ebird_to_sci_host)) %>% 
  pull(ebird_to_sci_host)

host_dat_missing_ebird_sci=vector(mode = "list", length = length(host_dat_missing))

  for(i in seq_along(host_dat_missing)){
  
  host_dat_missing_ebird_sci[[i]]=
    
    taxonomy( organism = host_dat_missing[i],
              db       = "ncbi",
              output   = "classification")#}
  
  cat(paste0(i, " "))}



host_dat[which(is.na(host_dat$order_my_tai) & !is.na(host_dat$ebird_to_sci_host)),
         c("class_my_tai",
         "order_my_tai",
         "family_my_tai",
         "genus_my_tai")]<-

                  map(host_dat_missing_ebird_sci, \(x)
                      x %>% 
                  select(-id) %>% 
                    filter(rank%in%c("class", "order", "family", "genus")) %>% 
                    pivot_wider(names_from = rank, values_from = name))  %>% 
                    bind_rows()


host_dat %>% filter(is.na(class_my_tai))
host_dat %>% filter(is.na(order_my_tai))
host_dat %>% filter(is.na(family_my_tai))
host_dat %>% filter(is.na(genus_my_tai))
host_dat %>% filter(is.na(species_my_tai))

host_dat %>% filter(is.na(class_my_tai)) %>% pull(genbank_host)

# [1] "american pelican"              "andean guayata"               
# [3] "blue-winged and cinnamon teal" "branta canasnsis"             
# [5] "crow"                          "dove"                         
# [7] "eagle"                         "environment"                  
# [9] "hawk"                          "heron"                        
# [11] "ibis"                          "larus belcheris"              
# [13] "lesser snow goose"             "magpie"                       
# [15] "marbled duck"                  "merganser"                    
# [17] "mule duck"                     "owl"                          
# [19] "pelican"                       "pheasant"                     
# [21] "pigeon"                        "red-back shrike"              
# [23] "swan"                          "teal"                         
# [25] "tern"                          "unknown"                      
# [27] "vulture"                       "western barn owl"             
# [29] "wood duck; aixnsa"

host_dat2<-data.frame(host_dat)

host_dat2[is.na(host_dat2$class_my_tai) & host_dat2$genbank_host=="american pelican",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai",
           "genus_my_tai", 
           "species_my_tai")]<-c("Aves", "Pelecaniformes", "Pelecanidae", "Pelecanus", "Pelecanus erythrorhynchos")

host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="andean guayata",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai",
           "genus_my_tai", 
           "species_my_tai")]<-c("Aves", "Anseriformes", "Anatidae", "Oressochen", "Oressochen melanopterus")

host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="branta canasnsis",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai",
           "genus_my_tai", 
           "species_my_tai")]<-c("Aves", "Anseriformes", "Anatidae", "Branta", "Branta canadensis")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="crow",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai")]<-c("Aves", "Passeriformes", "Corvidae")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="dove",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai")]<-c("Aves", "Columbiformes", "Columbidae")



host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="eagle",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai")]<-c("Aves", "Accipitriformes", "Accipitridae")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="hawk",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai")]<-c("Aves", "Accipitriformes", "Accipitridae")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="heron",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai")]<-c("Aves", "Pelecaniformes", "Ardeidae")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="ibis",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai")]<-c("Aves", "Pelecaniformes", "Threskiornithidae")

host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="larus belcheris",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai",
           "genus_my_tai", 
           "species_my_tai")]<-c("Aves", "Charadriiformes", "Laridae", "Larus", "Larus belcheri")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="lesser snow goose",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai",
           "genus_my_tai", 
           "species_my_tai")]<-c("Aves", "Anseriformes", "Anatidae", "Anser", "Anser caerulescens")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="magpie",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai")]<-c("Aves", "Passeriformes", "Corvidae")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="marbled duck",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai",
           "genus_my_tai", 
           "species_my_tai")]<-c("Aves", "Anseriformes", "Anatidae", "Marmaronetta", "Marmaronetta angustirostris")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="merganser",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai",
           "genus_my_tai")]<-c("Aves", "Anseriformes", "Anatidae", "Mergus")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="mule duck",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai")]<-c("Aves", "Anseriformes", "Anatidae")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="owl",
         c("class_my_tai",
           "order_my_tai")]<-c("Aves", "Strigiformes")



host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="pelican",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai",
           "genus_my_tai")]<-c("Aves", "Pelecaniformes", "Pelecanidae", "Pelecanus")



host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="pheasant",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai")]<-c("Aves", "Galliformes", "Phasianidae")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="pigeon",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai")]<-c("Aves", "Columbiformes", "Columbidae")



host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="marbled duck",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai",
           "genus_my_tai", 
           "species_my_tai")]<-c("Aves", "Passeriformes", "Laniidae", "Lanius", "Lanius collurio")



host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="swan",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai",
           "genus_my_tai")]<-c("Aves", "Anseriformes", "Anatidae", "Cygnus")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="teal",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai",
           "genus_my_tai")]<-c("Aves", "Anseriformes", "Anatidae", "Anas")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="tern",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai")]<-c("Aves", "Charadriiformes", "Laridae")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="vulture",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai")]<-c("Aves", "Accipitriformes", "Cathartidae")


host_dat2[is.na(host_dat2$class_my_tai) & host_dat2$genbank_host=="western barn owl",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai",
           "genus_my_tai", 
           "species_my_tai")]<-c("Aves", "Strigiformes", "Tytonidae", "Tyto", "Tyto alba")


host_dat2[is.na(host_dat2$class_my_tai) & 
           host_dat2$genbank_host=="wood duck; aixnsa",
         c("class_my_tai",
           "order_my_tai",
           "family_my_tai",
           "genus_my_tai", 
           "species_my_tai")]<-c("Aves", "Anseriformes", "Anatidae", "Aix", "Aix sponsa")


host_dat2[is.na(host_dat2$class_my_tai) & 
            host_dat2$genbank_host=="blue-winged and cinnamon teal",
          c("class_my_tai",
            "order_my_tai",
            "family_my_tai")]<-c("Aves", "Anseriformes", "Anatidae")


host_dat2[is.na(host_dat2$class_my_tai) & 
            host_dat2$genbank_host=="red-back shrike",
          c("class_my_tai",
            "order_my_tai",
            "family_my_tai",
            "genus_my_tai", 
            "species_my_tai")]<-c("Aves", "	Passeriformes", "Laniidae", "Lanius", "Lanius collurio")



# host_dat2 %>% filter(is.na(class_my_tai))
# host_dat2 %>% filter(is.na(order_my_tai))
# host_dat2 %>% filter(is.na(family_my_tai))
# host_dat2 %>% filter(is.na(genus_my_tai))

host_dat2$species_my_tai[host_dat2$species_my_tai=="Numididae sp."]<-NA

#host_dat2 %>% filter(is.na(species_my_tai))

#> adding the ebird scientific name to the species_my_tai_name column when the 
#> latter does snot have the species

host_dat2[is.na(host_dat2$species_my_tai) & !is.na(host_dat2$ebird_to_sci_host), "species_my_tai"]<-
  host_dat2$ebird_to_sci_host[is.na(host_dat2$species_my_tai) & !is.na(host_dat2$ebird_to_sci_host)]

# host_dat2 %>% filter(is.na(species_my_tai))

saveRDS(host_dat2, "/Users/DMontecino/Downloads/genbank_h5n1_aiv_hosts_data_cleaned_hosts.RDS")

cleaned_hosts<-
host_dat2 %>% 
  select(genbank_host,
         class_my_tai, 
         order_my_tai,
         family_my_tai, 
         genus_my_tai, 
         species_my_tai) 

#join cleaned host with full dataset

dat_full<-left_join(dat, unique(cleaned_hosts), by = c("host"="genbank_host"))
dat_full<-dat_full %>% filter(!is.na(class_my_tai))

#final checking and cleaninig
unique(dat_full$class_my_tai)
sort(unique(dat_full$order_my_tai))
dat_full$order_my_tai[grepl(pattern = "Passeriformes", dat_full$order_my_tai)]<-"Passeriformes"
sort(unique(dat_full$family_my_tai))
sort(unique(dat_full$genus_my_tai))
sort(unique(dat_full$species_my_tai))
dat_full$species_my_tai[dat_full$species_my_tai=="Anser sp."]<-NA

saveRDS(dat_full, "/Users/DMontecino/Downloads/genbank_h5n1_aiv_hosts_data_cleaned_hosts_full.RDS")





