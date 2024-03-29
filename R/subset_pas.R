        

# ------------------------------------------------------------------ #
# Identify the responses that involve the same PAs in a country      #
# and remove rows if necessary so that each PA is represented once   #
# in the dataset                                                     #
# ------------------------------------------------------------------ #


countries.represented=
  map(dat_modified_filtered$protected_area, \(x) tail(x, n = 1)) %>% 
  map(\(x) strsplit(x=x, split = " - ")[[1]]) %>% 
  map_chr(\(x) tail(x, n = 1)) %>% 
  unique() %>%  sort()
  


#add the country as a column
dat_modified_filtered$country <-map(dat_modified_filtered$protected_area, \(x) tail(x, n = 1)) %>% 
                       map(\(x) strsplit(x=x, split = " - ")[[1]]) %>% 
                       map_chr(\(x) tail(x, n = 1))


#group the protected areas associated to the same country
PAs.per.country<-split(x = dat_modified_filtered, f = dat_modified_filtered$country)

#summarize them 
PAs.per.country.table<-map(PAs.per.country, 
                          function(x) table(unlist(x$protected_area)))

#identify repeated PAs
repeated.PAs<- map(PAs.per.country.table, \(x) x[x>1]) %>% 
               map(names) %>% unlist(use.names = F)

# find the rows where each protected area is repated
temp.indexes=sapply(repeated.PAs , 
                    function(x) which(grepl(pattern = x, 
                                            dat_modified_filtered$protected_area)))



# Tapir Mountain Nature Reserve - Belize. 
# The second survey includes this PA and all other PAs from Belize. 
# The first row with the PA is removed
# dat_modified_filtered$protected_area[temp.indexes[[1]]]

dat_modified_filtered$protected_area[[7]]<-dat_modified_filtered$protected_area[[7]][
                                !grepl("Tapir Mountain Nature Reserve - Belize", 
                                      dat_modified_filtered$protected_area[[7]])]




#"Jigme Singye Wangchuck National Park - Bhutan" 
#remove the PA from the first survey that includes several PAs from Bhutan 
# and exam the single responses for this protected area

dat_modified_filtered$protected_area[[5]]<-dat_modified_filtered$protected_area[[5]][
                                  !grepl("Wangchuck National Park", 
                                        dat_modified_filtered$protected_area[[5]])]

#examining the single responses for "Jigme Singye Wangchuck National Park - 
#Bhutan" reveals that responses are the same and both represent only this 
# protected area
#data.frame(dat_modified_filtered[c(29, 30),]) #leaving the response that is only 
#for this PA (row 29)

# this is vector holding the positions of the rows to be removed because 
# they represent the same protected area

index.to.remove=c(30)




#Phibsoo Wildlife Sanctuary - Bhutan. 
#removing from the collection and leaving the response that is only for this PA

dat_modified_filtered$protected_area[[5]]<-dat_modified_filtered$protected_area[[5]][
                                  !grepl("Phibsoo Wildlife Sanctuary", 
                                         dat_modified_filtered$protected_area[[5]])]




#"Wangchuck Centennial National Park - Bhutan"  Same as above 
#removing from the collection and leaving the response that is only for this PA

dat_modified_filtered$protected_area[[5]]<-dat_modified_filtered$protected_area[[5]][
                                  !grepl("Wangchuck Centennial", 
                                         dat_modified_filtered$protected_area[[5]])]




#"Parque Nacional Yasuní. 
# examining the single responses for "Parque Nacional Yasuni" . Both responses
# represent only this protected area
#leaving the first response because the second one has an likely implausible
#response (patrols per month = 104, row 116)

#data.frame(dat_modified_filtered[c(112, 116),])

index.to.remove=c(index.to.remove, 116)





#Batang Ai National Park - Malaysia
#leaving the first response because the second one seems to have answered 
# "yes" to everything. Furthermore, it is not extremely relevant because 
# both answers include more than one protected area and will be filtered

#data.frame(dat_modified_filtered[c(42, 92),])

dat_modified_filtered$protected_area[[92]]<-dat_modified_filtered$protected_area[[92]][
  !grepl("Batang Ai National", 
         dat_modified_filtered$protected_area[[92]])]





#Endau-Rompin National Park
# Four answers represent only Endau=Rompin. the second answer is more 
# associated with the audience to respond the survey
# data.frame(dat_modified_filtered[c(19, 41,91,117),]) 

index.to.remove=c(index.to.remove, 19, 91, 117)






#Lanjak - Entimau Wildlife Sanctuary - Malaysia
# already solved when dealing with Batang Ai National Park - Malaysia




#Parque Nacional Huascaran - Peru
#removing the park from the answer that represents more than one protected
#area and leaving the row representing this park only

dat_modified_filtered$protected_area[[71]]<-dat_modified_filtered$protected_area[[71]][
                                  !grepl("Nacional Huascaran", 
                                         dat_modified_filtered$protected_area[[71]])]





# Espace TRIDOM Inter-Zone - Republic of Congo
# Lesio-Luna Gorilla Nature Reserve - Republic of Congo
# Management of Peripheral Ecosystems in Nouabale-Ndoki National Park - Republic of Congo
# Management of Peripheral Ecosystems in Odzala-Kokoua National Park - Republic of Congo
# Ntokou Pikounda National Park - Republic of Congo
# Odzala Kokoua National Park - Republic of Congo
# Unite de Surveillance et de Lutte Anti-Braconnage - Republic of Congo

# These protected areas are represented in rows 14 and 109, but row 109 belongs
# to other audience

dat_modified_filtered$protected_area[[109]]<-dat_modified_filtered$protected_area[[109]][
  !grepl("TRIDOM|Lesio-Luna|Nouabale-Ndoki|Odzala-Kokoua|Ntokou Pikounda|Odzala Kokoua|Anti-Braconnage", 
         dat_modified_filtered$protected_area[[109]])]




#Lake Téle Community Reserve - Republic of Congo
# keeping the first representative because it is more conservative
#data.frame(dat_modified_filtered[c(14, 95, 109),])

dat_modified_filtered$protected_area[[95]]<-dat_modified_filtered$protected_area[[95]][
  !grepl("Téle", 
         dat_modified_filtered$protected_area[[95]])]






#`Nouabale-Ndoki National Park - Republic of Congo`
# the last response is only for this protected area
# so remove the pa
# from 14, 95, and 109
# data.frame(dat_modified_filtered[c(14, 95, 109, 111),])

dat_modified_filtered$protected_area[[14]]<-dat_modified_filtered$protected_area[[14]][
  !grepl("Nouabale-Ndoki National Park", 
         dat_modified_filtered$protected_area[[14]])]

dat_modified_filtered$protected_area[[95]]<-dat_modified_filtered$protected_area[[95]][
  !grepl("Nouabale-Ndoki National Park", 
         dat_modified_filtered$protected_area[[95]])]

dat_modified_filtered$protected_area[[109]]<-dat_modified_filtered$protected_area[[109]][
  !grepl("Nouabale-Ndoki National Park", 
         dat_modified_filtered$protected_area[[109]])]




# Kizigo Game Reserve - Tanzania`
# Muhesi Game Reserve - Tanzania
# Rungwa Game Reserve - Tanzania
# These three protected area are represented in rows 27, 118, and 120
# 27 represents other audience and 118 has a wrong answer for the SMART version
# remove these three protected areas from answers 27 and 118
# data.frame(dat_modified_filtered[c(27, 118, 120),])


dat_modified_filtered$protected_area[[27]]<-dat_modified_filtered$protected_area[[27]][
  !grepl("Kizigo|Muhesi|Rungwa", 
         dat_modified_filtered$protected_area[[27]])]

dat_modified_filtered$protected_area[[118]]<-dat_modified_filtered$protected_area[[118]][
  !grepl("Kizigo|Muhesi|Rungwa", 
         dat_modified_filtered$protected_area[[118]])]






# removing the rows representing a single protected area that are repeated in
# other response

dat_modified_filtered<-dat_modified_filtered[-index.to.remove, ]




# ------------------------------------------------------------------------ #
# Identify those rows that include protected areas at a national level     #
# and remove the protected areas already included in these national-level  #
# responses.
# ------------------------------------------------------------------------ #

#58 "SINANPE - Peru" 
#25 "45 Protected Areas - Madagascar"  
#104 59 Colombia
#7  Belize
#5 Bhutan

country_level_data=dat_modified_filtered[c(5,7, 25, 58, 104),]

dat_modified_filtered=dat_modified_filtered[-c(5,7, 25, 58, 104),]





# ------------------------------------------------------------------------- #
# current number of protected areas per response versus original number of  #
# protected areas per response                                              #
# ------------------------------------------------------------------------- #

#after the deletion of certain protected areas from specific answers, some 
# responses might seeem to represent a single protected area when originally
# they did not. Check which ones these are to avoid wrong inclusion

# current number of protected areas after subsetting
dat_modified_filtered$new_number_pas <-map_int(dat_modified_filtered$protected_area,  
                                     \(x) length(x))  

#surveys that originally represented several protected areas and now they
#seem to represent a single one
surveys.from.many.pas.to.one.response=
data.frame(dat_modified_filtered %>%  
     select(or_number_pas, new_number_pas, survey) %>% 
     rowwise %>% 
     # mutate(equal=identical(or_number_pas,new_number_pas)))
     mutate(red_to_one=if_else(or_number_pas>1 & new_number_pas==1, T, F))) %>%
     filter(red_to_one==T)  


# data.frame(dat_modified_filtered %>% filter(survey%in%c(104, 118)))

#   only Lake Tele left which is by itself in survey 14
#>  Nouabalé-Ndoki National Park Peripheral Ecosystems 
#>  originally represented 3 protected areas and now it has one. Remove as well
#>  Removal occurs in line 371


# hist(map_vec(dat_modified_filtered$protected_area, length))
# table(map_vec(dat_modified_filtered$protected_area, length))


# ---------------------------------------------------------------------------------- #
# leaving protected areas that represent a single or two protected areas  originally #              
# ---------------------------------------------------------------------------------- #
data.frame(dat_modified_filtered %>% filter(or_number_pas%in%c(1,2)) %>% select(or_number_pas, new_number_pas))
data.frame(dat_modified_filtered %>% filter(new_number_pas%in%c(1,2)) %>% select(or_number_pas, new_number_pas))

dat_modified_filtered <-  dat_modified_filtered %>% filter(or_number_pas%in%c(1,2))
# dat_modified_filtered$survey

# nrow(dat_modified_filtered) #92





#> unique terms. If there is a term with a value higher than 1 
#> that is obviously associated withe the name of a park, then
# it means the Pa is repeated

unlist(dat_modified_filtered$protected_area) %>% 
  strsplit(split = " - ") %>% 
  map(\(x) x[1]) %>% 
  stri_trans_general("Latin-ASCII") %>% 
  map_vec(.f = function(y) str_split(y, "\\s+")) %>% 
  unlist() %>%  tolower() %>% table() %>% sort()

#yasuni
#wangchuck

indexes.repeated.PA.left.overs=
map(dat_modified_filtered$protected_area, \(x) strsplit(x, split = " - ")) %>% 
map(\(x) map_vec(x, \(y) y[1])) %>% 
map(\(x) stri_trans_general(x, "Latin-ASCII")) %>% 
map(\(x) str_split(x, "\\s+")) %>% 
map(\(x) map(x, \(y) tolower(y))) %>% 
map(\(x) map(x, \(y) any(grepl(pattern="yasuni|wangchuck", x=y)))) %>% 
map_vec(\(x) any(unlist(x))) %>% 
  which()
  

# 2 18 21 88 have repeated protected areas

dat_modified_filtered$protected_area[indexes.repeated.PA.left.overs]

# [1] "Wangchuck Centennial National Park - Bhutan"
# 
# [1] "Jigme Singye Wangchuck National Park - Bhutan"
# are actually different protected areas


# as.data.frame(dat_modified_filtered[c(21, 88),])

# survey 33 removed. the audience is closer to survey number 112
dat_modified_filtered<-dat_modified_filtered %>% filter(survey!=38)



# survey 90 and 55 are removed becuase they have inconsistency between 
# dead , sick or injured wildlife found (never) and recorded (Yes)

dat_modified_filtered<-dat_modified_filtered %>% filter(!(survey%in%c(90, 55)))


#> Check the position of the respondents. This refers to who are the respondents
#> "managers" or "other".

dat_modified_filtered %>% distinct(survey, position) %>% count(position)


# check the answers from "others" in position

# temp<-dat_modified_filtered %>% filter(position=="Other")

#write.csv(temp %>% unnest(protected_area), "data/check_data_position_is_other.csv")


# nrow(dat_modified_filtered)  #89
# nrow(temp)  #14


#> having checked the answers manually, I think removing these answers from "others"
#> is consistent with out methodology and our intended audience

dat_modified_filtered<-dat_modified_filtered %>% filter(position!="Other")

# nrow(dat_modified_filtered)  #75

