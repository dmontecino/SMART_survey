
# ------------------------------------------------------------------------ #
# Identify those rows that include protected areas at a national level     #
# and remove the protected areas already included in these national-level  #
# responses.
# ------------------------------------------------------------------------ #

dat_modified_filtered_2<-
dat_modified_filtered_1 %>% 
  filter(map_vec(dat_modified_filtered_1$protected_area, length)<=2)


#dat_modified_filtered_2$protected_area

#19 "45 Protected Areas - Madagascar"
#49 "SINANPE - Peru" 
#90 59 Colombia

index_other_pas_national_level<-c(19, 49, 90)

# country_level_data=dat_modified_filtered[country_level_responses,]

dat_modified_filtered_2=dat_modified_filtered_2[-index_other_pas_national_level,]        

# nrow(dat_modified_filtered_2)



# ------------------------------------------------------------------ #
# Identify the responses that involve the same PAs in a country      #
# and remove rows if necessary so that each PA is represented once   #
# in the dataset                                                     #
# ------------------------------------------------------------------ #


countries.represented=
  map(dat_modified_filtered_2$protected_area, \(x) tail(x, n = 1)) %>% 
  map(\(x) strsplit(x=x, split = " - ")[[1]]) %>% 
  map_chr(\(x) tail(x, n = 1)) %>% 
  unique() %>%  sort()
  


#add the country as a column
dat_modified_filtered_2$country <-map(dat_modified_filtered_2$protected_area, 
                                      \(x) tail(x, n = 1)) %>% 
                       map(\(x) strsplit(x=x, split = " - ")[[1]]) %>% 
                       map_chr(\(x) tail(x, n = 1))


#group the protected areas associated to the same country
PAs.per.country<-split(x = dat_modified_filtered_2, f = dat_modified_filtered_2$country)

#summarize them 
PAs.per.country.table<-map(PAs.per.country, 
                          function(x) table(unlist(x$protected_area)))

#identify repeated PAs
repeated.PAs<- map(PAs.per.country.table, \(x) x[x>1]) %>% 
               map(names) %>% unlist(use.names = F)

# find the rows where each protected area is repeated
temp.indexes=sapply(repeated.PAs , 
                    function(x) which(grepl(pattern = x, 
                                            dat_modified_filtered_2$protected_area)))





#>"Jigme Singye Wangchuck National Park - Bhutan" 
#> examining the single responses for "Jigme Singye Wangchuck National Park - 
#> Bhutan" reveals that responses are the same 

# data.frame(dat_modified_filtered_2[c(19, 20),]) #leaving the response that is only 
#for this PA (row 19)

# this is vector holding the positions of the rows to be removed because 
# they represent the same protected area

index_responses_repeated_pas_to_remove<-c(20)


#> "Parque Nacional Yasuní. 
#> examining the single responses for "Parque Nacional Yasuni" . Both responses
#> represent only this protected area
#> leaving the first response because the second one has an likely implausible
#> response (patrols per month = 104, row 95)

# data.frame(dat_modified_filtered_2[c(91, 95),])

index_responses_repeated_pas_to_remove<-
  c(index_responses_repeated_pas_to_remove, 95)


#> Endau-Rompin National Park
#> Four answers represent only Endau=Rompin. The second answer is more 
#> associated with the audience to respond the survey

# data.frame(dat_modified_filtered_2[c(13, 30, 77, 96),]) 

index_responses_repeated_pas_to_remove<-
  c(index_responses_repeated_pas_to_remove, 13, 77, 96)


#> unique terms. If there is a term with a value higher than 1 
#> that is obviously associated withe the name of a park, then
# it means the Pa is repeated

unlist(dat_modified_filtered_2$protected_area) %>% 
  strsplit(split = " - ") %>% 
  map(\(x) x[1]) %>% 
  stri_trans_general("Latin-ASCII") %>% 
  map_vec(.f = function(y) str_split(y, "\\s+")) %>% 
  unlist() %>%  tolower() %>% table() %>% sort()

# #yasuni
# #wangchuck
# 
indexes.repeated.PA.left.overs=
map(dat_modified_filtered_2$protected_area, \(x) strsplit(x, split = " - ")) %>%
map(\(x) map_vec(x, \(y) y[1])) %>%
map(\(x) stri_trans_general(x, "Latin-ASCII")) %>%
map(\(x) str_split(x, "\\s+")) %>%
map(\(x) map(x, \(y) tolower(y))) %>%
map(\(x) map(x, \(y) any(grepl(pattern="yasuni|wangchuck", x=y)))) %>%
map_vec(\(x) any(unlist(x))) %>%
  which()

#> 2 19 20 23 91 95 have repeated protected areas
 
# dat_modified_filtered_2$protected_area[indexes.repeated.PA.left.overs]
# data.frame(dat_modified_filtered_2[c(19, 20),])


#> [1] "Wangchuck Centennial National Park - Bhutan"
#> [1] "Jigme Singye Wangchuck National Park - Bhutan"
#> are actually different protected areas


#> survey 20 removed. Same responses
index_responses_repeated_pas_to_remove<-
  c(index_responses_repeated_pas_to_remove, 20) #




# data.frame(dat_modified_filtered_2[c(23, 91, 95),])

#> 23 position is other and 95 has 104 patrols per month
index_responses_repeated_pas_to_remove<-c(index_responses_repeated_pas_to_remove, 23, 95)


dat_modified_filtered_3<-dat_modified_filtered_2[-unique(index_responses_repeated_pas_to_remove),]


# indexes.repeated.PA.left.overs=
#   map(dat_modified_filtered_3$protected_area, \(x) strsplit(x, split = " - ")) %>%
#   map(\(x) map_vec(x, \(y) y[1])) %>%
#   map(\(x) stri_trans_general(x, "Latin-ASCII")) %>%
#   map(\(x) str_split(x, "\\s+")) %>%
#   map(\(x) map(x, \(y) tolower(y))) %>%
#   map(\(x) map(x, \(y) any(grepl(pattern="yasuni|wangchuck", x=y)))) %>%
#   map_vec(\(x) any(unlist(x))) %>%
#   which()
# 
# dat_modified_filtered_3[indexes.repeated.PA.left.overs, ]$protected_area


