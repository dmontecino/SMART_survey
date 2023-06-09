# Filtering the answers based on the protected area names


# --------------------------------------------------- #
# Assign "Unknown protected area" to specific answers #
# --------------------------------------------------- #


#"Community Forests - Unknown"
dat_modified$protected_area[[14]]<-"Unknown protected area" 

# The respondent provided a person's name instead of the protected area name
dat_modified$protected_area[c(12, 13, 20, 23, 59, 64, 81)]<-
                                   "Unknown protected area" 


# --------------------------------------------------------------- #
#  Fiix responses with protected areas from more than one country #
# --------------------------------------------------------------- #

# only one that has protected areas from kenya nad tanzania

#[[4]]
# [1] "LUMO community Conservancy - Kenya"   "Mgeno Conservancy - Kenya"           
# [3] "Taita Hills Sanctuary - Kenya"        "Taita Wildlife conservancy - Kenya"  
# [5] "Kasigau wildlife conservancy - Kenya" "Rombo District - Tanzania"           
# [7] "Mwanga district - Tanzania"           "Same District - Tanzania"            
# [9] "Lushoto District - Tanzania"          "Korogwe District - Tanzania"  


# split in two rows. One for the PAs in Kenya and another one for the PAs in Tanzania

#take the row and repeat it in the dataset
dat_modified<-dat_modified %>% bind_rows(dat_modified[4,])

#add the PAs to the last row associated with Tanzania
dat_modified$protected_area[[nrow(dat_modified)]]<-
                                  dat_modified$protected_area[[4]][
                                        grepl(pattern = "Tanzania", 
                                              ignore.case = T, 
                                              dat_modified$protected_area[[4]])]


#remove the PAs in the original row associated with the second country 
# in the original row

dat_modified$protected_area[[4]]<-dat_modified$protected_area[[4]][
                                        grepl(pattern = "Kenya", 
                                              ignore.case = T, 
                                              dat_modified$protected_area[[4]])]

# ---------------------------------------------------------------- #
# add the original number of pas per response as a column          #
# ---------------------------------------------------------------- #

dat_modified$or_number_pas <-map_int(dat_modified$protected_area,  
                                     \(x) length(x))               



# ---------------------------------------------------------------------- #
#  Remove the rows where the PAs are unknown and also remove the example #
# ---------------------------------------------------------------------- #

dat_modified <- dat_modified %>% 
  filter(protected_area!="Unknown protected area")

dat_modified <- dat_modified %>% 
  filter(protected_area!="ejemplo")


# ------------------------------------------------------------------ #
# Identify the responses that involve the same PAs in a country      #
# and remove rows if necessary so that each PA is represented once   #
# in the dataset                                                     #
# ------------------------------------------------------------------ #


countries.represented=
  map(dat_modified$protected_area, \(x) tail(x, n = 1)) %>% 
  map(\(x) strsplit(x=x, split = " - ")[[1]]) %>% 
  map_chr(\(x) tail(x, n = 1)) %>% 
  unique() %>%  sort()
  


#add the country as a column
dat_modified$country <-map(dat_modified$protected_area, \(x) tail(x, n = 1)) %>% 
                       map(\(x) strsplit(x=x, split = " - ")[[1]]) %>% 
                       map_chr(\(x) tail(x, n = 1))


#add the survey number as a column
dat_modified$survey <- 1:nrow(dat_modified)

#group the protected areas associated to the same country
PAs.per.country<-split(x = dat_modified, f = dat_modified$country)

#summarize them 
PAs.per.country.table<-map(PAs.per.country, 
                          function(x) table(unlist(x$protected_area)))

#identify repeated PAs
repeated.PAs<- map(PAs.per.country.table, \(x) x[x>1]) %>% 
               map(names) %>% unlist(use.names = F)

# find the rows where each protected area is repated
temp.indexes=sapply(repeated.PAs , 
                    function(x) which(grepl(pattern = x, 
                                            dat_modified$protected_area)))



# Tapir Mountain Nature Reserve - Belize. 
# The second survey includes this PA and all other PAs from Belize. 
# The first row with the PA is removed
# dat_modified$protected_area[temp.indexes[[1]]]

dat_modified$protected_area[[7]]<-dat_modified$protected_area[[7]][
                                !grepl("Tapir Mountain Nature Reserve - Belize", 
                                      dat_modified$protected_area[[7]])]




#"Jigme Singye Wangchuck National Park - Bhutan" 
#remove the PA from the first survey that includes several PAs from Bhutan 
# and exam the single responses for this protected area

dat_modified$protected_area[[5]]<-dat_modified$protected_area[[5]][
                                  !grepl("Wangchuck National Park", 
                                        dat_modified$protected_area[[5]])]

#examining the single responses for "Jigme Singye Wangchuck National Park - 
#Bhutan" reveals that responses are the same and both represent only this 
# protected area
#data.frame(dat_modified[c(29, 30),]) #leaving the response that is only 
#for this PA (row 29)

# this is vector holding the positions of the rows to be removed because 
# they represent the same protected area

index.to.remove=c(30)




#Phibsoo Wildlife Sanctuary - Bhutan. 
#removing from the collection and leaving the response that is only for this PA

dat_modified$protected_area[[5]]<-dat_modified$protected_area[[5]][
                                  !grepl("Phibsoo Wildlife Sanctuary", 
                                         dat_modified$protected_area[[5]])]




#"Wangchuck Centennial National Park - Bhutan"  Same as above 
#removing from the collection and leaving the response that is only for this PA

dat_modified$protected_area[[5]]<-dat_modified$protected_area[[5]][
                                  !grepl("Wangchuck Centennial", 
                                         dat_modified$protected_area[[5]])]




#"Parque Nacional Yasuní. 
# examining the single responses for "Parque Nacional Yasuni" . Both responses
# represent only this protected area
#leaving the first response because the second one has an likely implausible
#response (patrols per month = 104, row 116)

#data.frame(dat_modified[c(112, 116),])

index.to.remove=c(index.to.remove, 116)





#Batang Ai National Park - Malaysia
#leaving he first response because the second one seems yto have answered 
# "yes" to everything. Furthermore, it is not extremeley relevant becuase 
# both answers include more than one protected area and will be filtered

#data.frame(dat_modified[c(42, 92),])

dat_modified$protected_area[[92]]<-dat_modified$protected_area[[92]][
  !grepl("Batang Ai National", 
         dat_modified$protected_area[[92]])]





#Endau-Rompin National Park
# the four answers represent only Endau=Rompin. the second answer is more 
# associated with the audience to respond the survey
# data.frame(dat_modified[c(19, 41,91,117),]) # More consistent data from first 
# response.

index.to.remove=c(index.to.remove, 19, 91, 117)






#Lanjak - Entimau Wildlife Sanctuary - Malaysia
# already solved when dealing with Batang Ai National Park - Malaysia




#Parque Nacional Huascaran - Peru
#removing the park from the answer that represents more than one protected
#area and leaving the row representing this park only

dat_modified$protected_area[[71]]<-dat_modified$protected_area[[71]][
                                  !grepl("Nacional Huascaran", 
                                         dat_modified$protected_area[[71]])]





# Espace TRIDOM Inter-Zone - Republic of Congo
# Lesio-Luna Gorilla Nature Reserve - Republic of Congo
# Management of Peripheral Ecosystems in Nouabale-Ndoki National Park - Republic of Congo
# Management of Peripheral Ecosystems in Odzala-Kokoua National Park - Republic of Congo
# Ntokou Pikounda National Park - Republic of Congo
# Odzala Kokoua National Park - Republic of Congo
# Unite de Surveillance et de Lutte Anti-Braconnage - Republic of Congo

# These protected areas are represented in rows 14 and 109, but row 109 belongs
# to other audience

dat_modified$protected_area[[109]]<-dat_modified$protected_area[[109]][
  !grepl("TRIDOM|Lesio-Luna|Nouabale-Ndoki|Odzala-Kokoua|Ntokou Pikounda|Odzala Kokoua|Anti-Braconnage", 
         dat_modified$protected_area[[109]])]




#Lake Téle Community Reserve - Republic of Congo
# keeping the first representative because it is more conservative
#data.frame(dat_modified[c(14, 95, 109),])

dat_modified$protected_area[[95]]<-dat_modified$protected_area[[95]][
  !grepl("Téle", 
         dat_modified$protected_area[[95]])]






#`Nouabale-Ndoki National Park - Republic of Congo`
# the last response is only for this protected area
# so remove the pa
# from 14, 95, and 109
# data.frame(dat_modified[c(14, 95, 109, 111),])

dat_modified$protected_area[[14]]<-dat_modified$protected_area[[14]][
  !grepl("Nouabale-Ndoki National Park", 
         dat_modified$protected_area[[14]])]

dat_modified$protected_area[[95]]<-dat_modified$protected_area[[95]][
  !grepl("Nouabale-Ndoki National Park", 
         dat_modified$protected_area[[95]])]

dat_modified$protected_area[[109]]<-dat_modified$protected_area[[109]][
  !grepl("Nouabale-Ndoki National Park", 
         dat_modified$protected_area[[109]])]




# Kizigo Game Reserve - Tanzania`
# Muhesi Game Reserve - Tanzania
# Rungwa Game Reserve - Tanzania
# These three protected area are represented in rows 27, 118, and 120
# 27 represents other audience and 118 has a wrong answer for the SMART version
# remove these three protected areas from answers 27 and 118
# data.frame(dat_modified[c(27, 118, 120),])


dat_modified$protected_area[[27]]<-dat_modified$protected_area[[27]][
  !grepl("Kizigo|Muhesi|Rungwa", 
         dat_modified$protected_area[[27]])]

dat_modified$protected_area[[118]]<-dat_modified$protected_area[[118]][
  !grepl("Kizigo|Muhesi|Rungwa", 
         dat_modified$protected_area[[118]])]






# removing the rows representing a single protected area that are repeated in
# other response

dat_modified<-dat_modified[-index.to.remove, ]




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

country_level_data=dat_modified[c(5,7, 25, 58, 104),]

dat_modified=dat_modified[-c(5,7, 25, 58, 104),]





# ------------------------------------------------------------------------- #
# current number of protected areas per response versus original number of  #
# protected areas per response                                              #
# ------------------------------------------------------------------------- #

#after the deletion of certain protected areas from specific answers, some 
# responses might seeem to represent a single protected area when originally
# they did not. Check which ones these are to avoid wrong inclusion

# current number of protected areas after subsetting
dat_modified$new_number_pas <-map_int(dat_modified$protected_area,  
                                     \(x) length(x))  

#surveys that originally represented several protected areas and now they
#seem to represent a single one
surveys.from.many.pas.to.one.response=
data.frame(dat_modified %>%  
     select(or_number_pas, new_number_pas, survey) %>% 
     rowwise %>% 
     # mutate(equal=identical(or_number_pas,new_number_pas)))
     mutate(red_to_one=if_else(or_number_pas>1 & new_number_pas==1, T, F))) %>%
     filter(red_to_one==T)  


data.frame(dat_modified %>% filter(survey%in%c(95, 109)))

# 109 only Lake Tele left which is by itself in survey 14
# 95 originally represented 3 protected areas and now it has one. Remove as well



# -------------------------------------------------------------------------- #
# leaving protected areas that represent a single protected area  originally #              
# -------------------------------------------------------------------------- #

dat_modified <- dat_modified %>% filter(or_number_pas==1)

nrow(dat_modified ) #83
