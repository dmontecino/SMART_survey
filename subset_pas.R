# Editing the protected area names column


# <!-- Making all elements to have length 1 -->
 
data_modified$protected_area<-lapply(data_from_r_modified$protected_area, function(x) if(length(x)>1) paste(x,collapse = ", ") else(x))


# <!-- Edit content of the protected areas with problems in the "name of the protected area(s)" -->

data_from_r_modified$protected_area[[14]]<-"Unknown protected area" #it could be included in other responses. we dont know is it is terrestrial or marine. etc. Originally #"Community Forests\nPrivate Conservancy"
data_from_r_modified$protected_area[c(12, 13, 20, 23, 59, 64, 81)]<-"Unknown protected area" # the asnwer was a person's name

# <!-- Modify the protected area to a vector and add it as a column to the dataset -->

data_from_r_modified$protected_area <- unlist(data_from_r_modified$protected_area)


# <!-- Remove the rows where the PAs are unknown and also remove the example -->
#| echo: false

data_from_r_modified <- data_from_r_modified %>%  filter(protected_area!="Unknown protected area")
data_from_r_modified <- data_from_r_modified %>%  filter(protected_area!="ejemplo")


# <!-- Identify responses that involve protected areas in more than one country and add rows to represent each country with the correponding PAs -->
  
temp=sapply(strsplit(data_from_r_modified$protected_area, ", ") , strsplit, " - ")
temp=lapply(temp, function(x) unique(sapply(x, function(y) y[length(y)])))
temp.index=which(sapply(temp, length)>1) # row that involves PAs in more ethan one country


#take the row and repeat it in the dataset
data_from_r_modified<-data_from_r_modified %>% bind_rows(data_from_r_modified[4,])

#remove the PAs in the original row associated with the second country in the original row
data_from_r_modified$protected_area[4]<-"LUMO community Conservancy - Kenya, Mgeno Conservancy - Kenya, Taita Hills Sanctuary - Kenya, Taita Wildlife conservancy - Kenya, Kasigau wildlife conservancy - Kenya"

#remove the PAs in the copied row associated with the first country in the nadded row
data_from_r_modified$protected_area[nrow(data_from_r_modified)]<-"Rombo District - Tanzania, Mwanga district - Tanzania, Same District - Tanzania, Lushoto District - Tanzania, Korogwe District - Tanzania"



# <!-- Identify the responses that involve the same PAs in a country and remove rows so that each PA is represented once in the dataset -->
countries.represented=sort(unique(unlist(lapply(lapply(strsplit(data_from_r_modified$protected_area, ","), strsplit, " - "), function(x) sapply(x, function(y) y[length(y)])))))

#add the country as a column
data_from_r_modified$country=sapply(strsplit(data_from_r_modified$protected_area, " - "), function(x) x[length(x)])

#group the protected areas associated to the same country

PAs.per.country=lapply(countries.represented, function(x) data_from_r_modified$protected_area[data_from_r_modified$country==x])

PAs.per.country=sapply(PAs.per.country, strsplit, ", ")
PAs.per.country=sapply(PAs.per.country, unlist, USE.NAMES = F)

PAs.per.country.table=sapply(PAs.per.country, table)

#identify repeated PAs
repeated.PAs=names(unlist(PAs.per.country.table)[unlist(PAs.per.country.table)>1])

# find the rows where each protected area is repated
temp.indexes=sapply(repeated.PAs , function(x) which(grepl(pattern = x, data_from_r_modified$protected_area)))

#data with repeated PAs
temp=lapply(temp.indexes, function(x) data_from_r_modified$protected_area[x])

#Tapir Mountain Nature Reserve - Belize. the second survey includes this PA and all other PAs from Belize. The first row with the PA is removed
#temp[[1]]

data_from_r_modified$protected_area[7]<-"Aguas Turbias National Park - Belize, Bacalar Chico National Park - Belize, Billy Barquedier National Park - Belize, Chiquibul National Park - Belize, Five Blues Lakes National Park - Belize, Gra Gra Lagoon National Park - Belize, Guanacaste National Park - Belize, Honey Camp National Park - Belize, Laughing Bird Caye National Park - Belize, Mayflower Bocawina National Park - Belize, Monkey Bay National Park - Belize, Nojkaaxmeen Elijio Panti National Park - Belize, Payne's Creek National Park - Belize, Peccary Hills National Park - Belize, Río Blanco National Park - Belize, Sarstoon-Temash National Park - Belize, St. Herman's Blue Hole National Park - Belize, Actun Tunichil Muknal Natural Monument - Belize, Blue Hole\tNatural Monument - Belize, Half Moon Caye Natural Monument - Belize, Thousand Foot Falls Natural Monument - Belize, Victoria Peak Natural Monument - Belize, Bladen Nature Reserve - Belize, Burdon Canal Nature Reserve - Belize, Aguacaliente Wildlife Sanctuary - Belize, Cockscomb Basin Wildlife Sanctuary - Belize, Corozal Bay Wildlife Sanctuary - Belize, Crooked Tree Wildlife Sanctuary - Belize, Gales Point Wildlife Sanctuary - Belize, Spanish Creek Wildlife Sanctuary - Belize, Swallow Caye Wildlife Sanctuary - Belize, Caye Caulker Forest Reserve - Belize, Chiquibul Forest Reserve - Belize, Columbia River Forest Reserve - Belize, Deep River Forest Reserve - Belize, Fresh Water Creek Forest Reserve - Belize, Grants Work Forest Reserve - Belize, Machaca Forest Reserve - Belize, Manatee Forest Reserve - Belize, Mango Creek Forest Reserve - Belize, Monkey Caye Forest Reserve - Belize, Mountain Pine Ridge Forest Reserve - Belize, Maya Mountain Forest Reserve - Belize, Sibun Forest Reserve - Belize, Sittee River Forest Reserve - Belize, Swasey Bladen Forest Reserve - Belize, Vaca Forest Reserve - Belize, Bacalar Chico Marine Reserve - Belize, Caye Caulker Marine Reserve - Belize, Gladden Spit and Silk Cayes Marine Reserve - Belize, Glover's Reef Marine Reserve - Belize, Hol Chan Marine Reserve - Belize, Port Honduras Marine Reserve - Belize, Sapodilla Cayes Marine Reserve - Belize, South Water Caye Marine Reserve - Belize"

#"Jigme Singye Wangchuck National Park - Bhutan" 
#remove the PA from the first survey that includes several PAs from Bhutan and exam the single responses for tthis protected area

#removing from the collection
data_from_r_modified$protected_area[5]<-"Wangchuck Centennial National Park - Bhutan, Jigme Dorji National Park - Bhutan, Jomotshangkha Wildlife Sanctuary - Bhutan, Phibsoo Wildlife Sanctuary - Bhutan, Royal Manas National Park - Bhutan, Jigme Khesar Strict Nature Reserve - Bhutan, Phrumsengla National Park - Bhutan, Sakteng Wildlife Sanctuary - Bhutan, Bumdeling Wildlife Sanctuary - Bhutan, Bumthang Forest Division - Bhutan, Gedu Forest Division - Bhutan, Paro Forest Division - Bhutan, Samtse Forest Division - Bhutan, Samdrup Jongkhar Forest Division - Bhutan, Tashigang Forest Division - Bhutan, Sarpang Forest Division - Bhutan, Zhemgang Forest Division - Bhutan, Mongar Forest Division - Bhutan, Wangdue Forest Division - Bhutan, Thimphu Forest - Bhutan, Tsirang Forest Division - Bhutan, Dagana Forest Division - Bhutan, Pema Gatshel Forest Division - Bhutan"

#examining the single responses for "Jigme Singye Wangchuck National Park - Bhutan" reveals that responses are the same

#data.frame(data_from_r_modified[c(29, 30),])
#leaving the response that is only for this PA
index.to.remove=c(30)

#Phibsoo Wildlife Sanctuary - Bhutan. 
#temp[[3]]

#removing from the collection and leaving the response that is only for this PA
data_from_r_modified$protected_area[5]<-"Wangchuck Centennial National Park - Bhutan, Jigme Dorji National Park - Bhutan, Jomotshangkha Wildlife Sanctuary - Bhutan, Royal Manas National Park - Bhutan, Jigme Khesar Strict Nature Reserve - Bhutan, Phrumsengla National Park - Bhutan, Sakteng Wildlife Sanctuary - Bhutan, Bumdeling Wildlife Sanctuary - Bhutan, Bumthang Forest Division - Bhutan, Gedu Forest Division - Bhutan, Paro Forest Division - Bhutan, Samtse Forest Division - Bhutan, Samdrup Jongkhar Forest Division - Bhutan, Tashigang Forest Division - Bhutan, Sarpang Forest Division - Bhutan, Zhemgang Forest Division - Bhutan, Mongar Forest Division - Bhutan, Wangdue Forest Division - Bhutan, Thimphu Forest - Bhutan, Tsirang Forest Division - Bhutan, Dagana Forest Division - Bhutan, Pema Gatshel Forest Division - Bhutan"


#"Wangchuck Centennial National Park - Bhutan"  Same as above 
#temp[[4]]

data_from_r_modified$protected_area[5]<-"Jigme Dorji National Park - Bhutan, Jomotshangkha Wildlife Sanctuary - Bhutan, Royal Manas National Park - Bhutan, Jigme Khesar Strict Nature Reserve - Bhutan, Phrumsengla National Park - Bhutan, Sakteng Wildlife Sanctuary - Bhutan, Bumdeling Wildlife Sanctuary - Bhutan, Bumthang Forest Division - Bhutan, Gedu Forest Division - Bhutan, Paro Forest Division - Bhutan, Samtse Forest Division - Bhutan, Samdrup Jongkhar Forest Division - Bhutan, Tashigang Forest Division - Bhutan, Sarpang Forest Division - Bhutan, Zhemgang Forest Division - Bhutan, Mongar Forest Division - Bhutan, Wangdue Forest Division - Bhutan, Thimphu Forest - Bhutan, Tsirang Forest Division - Bhutan, Dagana Forest Division - Bhutan, Pema Gatshel Forest Division - Bhutan"


#"Parque Nacional Yasuní. 
#temp[[5]]
#examining the single responses for "Parque Nacional Yasun" . leaveing he first response because the second one has an likely implausible response (patrols per month = 104)

#data.frame(data_from_r_modified[c(111, 115),])

index.to.remove=c(index.to.remove, 115)

#Endau-Rompin National Park
#temp[[6]] both answers represent the same set of protected areas. exam the responses to decide which one stays

#data.frame(data_from_r_modified[c(42,91),]) # More consistent data from first response.

index.to.remove=c(index.to.remove, 91)

#temp[[7]]  four answers for the same PA. The one staying is based on individual check on responses

# data.frame(data_from_r_modified[temp.indexes[[7]],]) #the third response seems more consistet and it was answreed by someone "directly responsible for managing SMART data in one or more protected areas"

index.to.remove=c(index.to.remove, c(19, 41, 116))

#Parque Nacional Huascaran - Peru
#temp[[8]] Leave the single response and remove Huascaran from the response representing several PAs

data_from_r_modified$protected_area[temp.indexes[[8]][2]]<-"Parque Nacional Río Abiseo - Peru, Santuario Nacional de Calipuy - Peru, Reserva Nacional de Calipuy - Peru, Parque Nacional Manu - Peru"

#temp[9:16] represent a set of protected areas from congo. Two repsonses involving the same protected areas. the index for the first repeasted protected area applies for repeated protected areas 9 to 17 in the temp object except for 14.

# data.frame(data_from_r_modified[temp.indexes[[9]],]) # responder of 108 is from another audience
#temp.indexes[9:17]
index.to.remove=c(index.to.remove, 108)

#`Nouabale-Ndoki National Park - Republic of Congo`
# temp[[14]]
# temp.indexes[[14]]
#here we leave 110 as represneting #`Nouabale-Ndoki National Park - Republic of Congo` by itslef and edit 14 ad 94 so they do not include #`Nouabale-Ndoki National Park - Republic of Congo`

#data_from_r_modified[temp.indexes[[14]],]

#edition to remove Nouabale-Ndoki National Park - Republic of Congo from the other two responses
data_from_r_modified$protected_area[temp.indexes[[14]][1]]<-"Odzala Kokoua National Park - Republic of Congo, Ntokou Pikounda National Park - Republic of Congo, Management of Peripheral Ecosystems in Nouabale-Ndoki National Park - Republic of Congo, Management of Peripheral Ecosystems in Odzala-Kokoua National Park - Republic of Congo, Lake Tele Community Reserve - Republic of Congo, Espace TRIDOM Inter-Zone - Republic of Congo, Unite de Surveillance et de Lutte Anti-Braconnage - Republic of Congo, Lesio-Luna Gorilla Nature Reserve - Republic of Congo"

data_from_r_modified$protected_area[temp.indexes[[14]][2]]<-"Nouabalé-Ndoki National Park Peripheral Ecosystems Management Project - Republic of Congo, Lake Téle Community Reserve - Republic of Congo"


#temp[18:20] involves the same 3 responses:27 117 119. Let's check the answers. Leaving just the last response because it is more consistent.

# as.data.frame(data_from_r_modified[temp.indexes[[18]],])

index.to.remove=c(index.to.remove, c(27, 117))



data_from_r_modified<-data_from_r_modified[-index.to.remove, ]


# <!-- Identify those rows that include protected areas at a national level and remove the protected areas already included in these national-level responses.  -->
  
  # <!-- I am removing the "Peru"-level row and leaving the bunch of PAs from Peru that answered individually. Rule: prioritize single protected area responses -->
  # <!-- Later on we can check differences between the national-level response and the local-level responses. This could be done for Peru and Belize. Colombia and Madagascar also answered per country. -->
  

#| echo: false
#56 "SINANPE - Peru" 
#25 "45 Protected Areas - Madagascar"  
#101 59 Colombia
#7  Belize
#5 Bhutan

country_level_data=data_from_r_modified[c(5,7, 25, 56, 101),]

data_from_r_modified=data_from_r_modified[-c(5,7, 25, 56, 101),]

# <!-- Add the country as a column and add a counter of the number of PAs represented per row -->
#add the country as a column
data_from_r_modified$country=sapply(strsplit(data_from_r_modified$protected_area, " - "), function(x) x[length(x)])

data_from_r_modified$num_protected_areas=map_int(strsplit(data_from_r_modified$protected_area,split = ", "), length)




