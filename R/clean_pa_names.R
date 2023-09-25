
dat_modified<-dat_modified %>%  as_tibble()

dat_modified$protected_area<-map(dat_modified$protected_area, \(x) strsplit(x, "\n|, ")[[1]])


#Mann Wildlife Sanctuary
dat_modified$protected_area[[1]]<-"Mann Wildlife Sanctuary - Myanmar"



#Mann Wildlife Sanctuary
dat_modified$protected_area[[2]]<-"Wangchuck Centennial National Park - Bhutan"



#Tapir Mountain Nature Reserve
dat_modified$protected_area[[3]]<-"Tapir Mountain Nature Reserve - Belize"



#LUMO community Conservancy\nMgeno Conservancy\nTaita Hills Sanctuary\nTaita Wildlife conservancy\nKasigau wildlife conservancy\nTanzania\nRombo District\nMwanga district\nSame District\nLushoto District\nKorogwe District

dat_modified$protected_area[[4]][2:6]<-paste(dat_modified$protected_area[[4]][2:6], "- Kenya") # adding the country to the corresponding set of PAs

dat_modified$protected_area[[4]][8:12]<-paste(dat_modified$protected_area[[4]][8:12], "- Tanzania")  # adding the country to the corresponding set of PAs

dat_modified$protected_area[[4]]<-dat_modified$protected_area[[4]][-c(1,7)]


#"WCNP, JDNP, JWS, PWS, Royal Manas National Park, Jigme Khesar Strict Nature Reserve, Jigme Singye Wangchuck, PNP, SWS & BWS and 14 Divisional forest"
dat_modified$protected_area[[5]]<-c("Wangchuck Centennial National Park",
                                    "Jigme Dorji National Park", 
                                    "Jomotshangkha Wildlife Sanctuary", 
                                    "Phibsoo Wildlife Sanctuary", 
                                    "Royal Manas National Park", 
                                    "Jigme Khesar Strict Nature Reserve", 
                                    "Jigme Singye Wangchuck National Park", 
                                    "Phrumsengla National Park", 
                                    "Sakteng Wildlife Sanctuary",
                                    "Bumdeling Wildlife Sanctuary",
                                    "Bumthang Forest Division",
                                    "Gedu Forest Division",
                                    "Paro Forest Division",
                                    "Samtse Forest Division",
                                    "Samdrup Jongkhar Forest Division",
                                    "Tashigang Forest Division",
                                    "Sarpang Forest Division",
                                    "Zhemgang Forest Division",
                                    "Mongar Forest Division",
                                    "Wangdue Forest Division",
                                    "Thimphu Forest",
                                    "Tsirang Forest Division",
                                    "Dagana Forest Division",
                                    "Pema Gatshel Forest Division")

dat_modified$protected_area[[5]]<-paste(dat_modified$protected_area[[5]], "- Bhutan")


#Gbele Resource Reserve
dat_modified$protected_area[[6]]<-paste(dat_modified$protected_area[[6]], "- Ghana")




#"Belize National Protected Areas System"
dat_modified$protected_area[[7]]<-c( "Aguas Turbias National Park",
                                      "Bacalar Chico National Park",
                                     "Billy Barquedier National Park",
                                      "Chiquibul National Park",
                                     "Five Blues Lakes National Park",
                                      "Gra Gra Lagoon National Park",
                                     "Guanacaste National Park",
                                      "Honey Camp National Park",
                                     "Laughing Bird Caye National Park",
                                      "Mayflower Bocawina National Park",
                                     "Monkey Bay National Park",
                                     "Nojkaaxmeen Elijio Panti National Park",
                                     "Payne's Creek National Park",
                                      "Peccary Hills National Park",
                                      "Río Blanco National Park",
                                      "Sarstoon-Temash National Park",
                                      "St. Herman's Blue Hole National Park", 
                                      "Actun Tunichil Muknal Natural Monument",
                                      "Blue Hole	Natural Monument",
                                      "Half Moon Caye Natural Monument",
                                      "Thousand Foot Falls Natural Monument",
                                      "Victoria Peak Natural Monument",
                                      "Bladen Nature Reserve",
                                      "Burdon Canal Nature Reserve",
                                      "Tapir Mountain Nature Reserve",
                                      "Aguacaliente Wildlife Sanctuary",	
                                      "Cockscomb Basin Wildlife Sanctuary",	
                                      "Corozal Bay Wildlife Sanctuary",	
                                      "Crooked Tree Wildlife Sanctuary",	
                                      "Gales Point Wildlife Sanctuary",	
                                      "Spanish Creek Wildlife Sanctuary",	
                                      "Swallow Caye Wildlife Sanctuary",
                                      "Caye Caulker Forest Reserve",
                                      "Chiquibul Forest Reserve",
                                      "Columbia River Forest Reserve",
                                      "Deep River Forest Reserve",
                                      "Fresh Water Creek Forest Reserve",
                                      "Grants Work Forest Reserve",
                                      "Machaca Forest Reserve",
                                      "Manatee Forest Reserve",
                                      "Mango Creek Forest Reserve",
                                      "Monkey Caye Forest Reserve",
                                      "Mountain Pine Ridge Forest Reserve",
                                      "Maya Mountain Forest Reserve",
                                      "Sibun Forest Reserve",
                                      "Sittee River Forest Reserve",
                                      "Swasey Bladen Forest Reserve",
                                      "Vaca Forest Reserve",
                                      "Bacalar Chico Marine Reserve",
                                      "Caye Caulker Marine Reserve",
                                      "Gladden Spit and Silk Cayes Marine Reserve",
                                      "Glover's Reef Marine Reserve",
                                      "Hol Chan Marine Reserve",
                                      "Port Honduras Marine Reserve",
                                      "Sapodilla Cayes Marine Reserve",
                                      "South Water Caye Marine Reserve")

dat_modified$protected_area[[7]]<-paste(dat_modified$protected_area[[7]], "- Belize")


# survey 5 contains survey 2
# survey 7 contains survey 3
# surveys 2 and 3 to be removed below

#The mountainous part of Croatia where wolves live (about 20000 sq km)
dat_modified$protected_area[[8]]<-"Dinarides mountain range - Croatia"


#"1. SM Tanjung Peropa\n2. SM Tanjung Anolengo\n3. SM Buton Utara\n4. SM Tanjung Batikolo\n5. SM Lambusango"
dat_modified$protected_area[[9]]<-gsub("SM ", replacement = "", dat_modified$protected_area[[9]])
dat_modified$protected_area[[9]]<-paste(dat_modified$protected_area[[9]], "Wildlife Reserve - Indonesia")
dat_modified$protected_area[[9]]<-sub(".*? ", "", dat_modified$protected_area[[9]])



#Conservation South Luangwa
dat_modified$protected_area[[10]]<-"Conservation South Luangwa - Zambia"


#"Siem Pang Wildlife Sanctuary"
dat_modified$protected_area[[11]]<-"Siem Pang Wildlife Sanctuary - Cambodia"


#Myo Myint Aung
#dat_modified$protected_area[[12]]<- need to check. 'it seems he gave his name. Smithsonian?

# Pierre Megfy
#dat_modified$protected_area[[13]]<- need to check. 'it seems he gave his name. Pierre Meggy/Tanzania?


#"Community Forests\nPrivate Conservancy"
dat_modified$protected_area[[14]]<-"Community Forests - Unknown"


#"North Luangwa Conservation Programme
dat_modified$protected_area[[15]]<-"North Luangwa Conservation Programme - Zambia"

#"Khunjerab National Park\nChitral Gol National Park"
dat_modified$protected_area[[16]]<-paste0(dat_modified$protected_area[[16]], " - Pakistan") 


#"Nouabale-Ndoki National Park (NNNP)\nOdzala Kokoua National Park (PNOK)\nNtokou Pikounda National Park (PNNP)\nProject for the Management of Preipheral Ecosystems in Nouabale-Ndoki National Park (PROGEPP-Kabo)\nPreipherical Ecosystem Management Project in Odzala-Kokoua National Park (PROGEPP-Ngombe)\nLac Tele Community Reserve (RCLT)\nEspace TRIDOM Inter-Zone (ETIC)\nUnite de Surveillance et de Lutte Anti-Braconnage (USLAB) Tala-Tala\nLesio-Luna Gorilla Nature Reserve (RNGLL)"

dat_modified$protected_area[[17]]<-dat_modified$protected_area[[17]][1:9]

dat_modified$protected_area[[17]]<-gsub(" [(].*","",dat_modified$protected_area[[17]])

dat_modified$protected_area[[17]][4]<-"Management of Peripheral Ecosystems in Nouabale-Ndoki National Park"

dat_modified$protected_area[[17]][5]<-"Management of Peripheral Ecosystems in Odzala-Kokoua National Park"

dat_modified$protected_area[[17]][6]<-"Lake Téle Community Reserve"

dat_modified$protected_area[[17]]<-paste(dat_modified$protected_area[[17]], "- Republic of Congo")


#COMATSA
dat_modified$protected_area[[18]]<-"COMATSA Protected Area - Madagascar"



# "Parc national du Banco  Parc national d'Azagny  Parc national des Iles Ehotilé  Réserve naturelle de Dahliafleur  Réserve naturelle partielle d'Aghien  Réserve naturelle de Mabi-Yaya  Réserve naturelle de Bossomatié"
dat_modified$protected_area[[19]]<-unlist(strsplit(dat_modified$protected_area[[19]], "  "))

dat_modified$protected_area[[19]]<-paste(dat_modified$protected_area[[19]], "- Côte d'Ivoire")



#"KAPUKU MBOMBO JOHN"
#dat_modified$protected_area[[20]] Also provided his name



#"Tsavo west Intensive Protection Zone\nNgulia Rhino Sanctuary"

dat_modified$protected_area[[21]]<-paste(dat_modified$protected_area[[21]], "- Kenya")

#Queen Elizabeth Protected Area
dat_modified$protected_area[[22]]<-"Queen Elizabeth Protected Area - Uganda"



#"Daudi Mollel"
#dat_modified$protected_area[[23]] # Provided his name.



#"Endau Rompin Landscape"
dat_modified$protected_area[[24]]<-c("Endau-Rompin National Park - Malaysia")



#"Sambor wildlife sanctuary, Prek Prasab Wildlife Sanctuary"
dat_modified$protected_area[[25]]<-paste(dat_modified$protected_area[[25]], "- Cambodia")



#"Chipanje Chetu"
dat_modified$protected_area[[26]]<-c("Chipanje Chetu Community Conservation - Mozambique")



#"Khoid Mogoin Gol - Teel LPA\nGreat Gobi Strictly Protected Area Part \"A\"\nMongolian Ecological Police Department"
dat_modified$protected_area[[27]]<-gsub('\"', replacement = "", dat_modified$protected_area[[27]])

dat_modified$protected_area[[27]]<-gsub('LPA', replacement = "Local Protected Area", dat_modified$protected_area[[27]])

dat_modified$protected_area[[27]]<-paste(dat_modified$protected_area[[27]], "- Mongolia")

dat_modified$protected_area[[27]]<-dat_modified$protected_area[[27]][-3] # removing the police department because it is new, 
#it encompasses several departments and cannot track specific protected areas



#"Gunung Nyiut Nature Reserve\nGunung Naning Protected Forest\nKarimata Marine Reserve\nSeruat Pulau Tiga Protected Forest"
dat_modified$protected_area[[28]]<-paste(dat_modified$protected_area[[28]], "- Indonesia")



#"Phibsoo Wildlife Sanctuary, Department Of Forest and Park Services, Ministry of Agriculture and Forest. Bhutan"
dat_modified$protected_area[[29]]<-"Phibsoo Wildlife Sanctuary - Bhutan" # included in survey 5

dat_modified$protected_area[[30]]<-"Tawau Hills Park - Malaysia"




#"WCS all protected areas - Madagascar National parks Protected areas (45)"
dat_modified$protected_area[[31]]<-"45 Protected Areas - Madagascar"



#"Rungwa Game Reserve\nKizigo Game Reserve\nMuhesi Game Reserve\nUgalla Game Reserve\nLukwati-Piti Game Reserve\nRukwa Game Reserve\nLwafi Game Reserve"

dat_modified$protected_area[[32]]<-paste(dat_modified$protected_area[[32]], "- Tanzania")



#"Mt. Goplom Conservation Area, Kwiop village, Jimi District, Jiwaka Province, Papua New Guinea\nMt. Waugerema Conservation Area, Daulo District, Eastern Highlands Province\nYasina Nature Park, Ungai-Bena District, Eastern Highlands Province"

dat_modified$protected_area[[33]]<-paste(dat_modified$protected_area[[33]], "- Papua New Guinea")



#Jigme Singye Wangchuck National Park"
dat_modified$protected_area[[34]]<-"Jigme Singye Wangchuck National Park - Bhutan"



#"Jigme Singye Wangchuck National Park"
dat_modified$protected_area[[35]]<-"Jigme Singye Wangchuck National Park - Bhutan"



#"Budongo\nItwara"
dat_modified$protected_area[[36]]<-c("Budongo Conservation Field Station - Uganda", 
                                    "Itwara Forest Reserve - Uganda")



#Nsumbu Tanganyika
dat_modified$protected_area[[37]]<-"Nsumbu Tanganyika Conservation Programme - Zambia"



#"Yasuni"
dat_modified$protected_area[[38]]<-"Yasuni National Park - Ecuador"



#"Cardamom Landscape"
dat_modified$protected_area[[39]]<-"Cardamom Landscape - Cambodia"



#[1] "1.) Maite Marine Protected Area (MPA)\n2.) Lower Cabangcalan MPA\n3.) Olang MPA\n4.) Tulapos MPA\n5.) Cangbagsa MPA\n6.) Caticugan MPA"

dat_modified$protected_area[[40]]<-gsub(" [(]MPA[)]| MPA", " Marine Protected Area", dat_modified$protected_area[[40]])

dat_modified$protected_area[[40]]<-paste(dat_modified$protected_area[[40]], "- Philippines")




#"Reserva de Biosfera Maya, SIGAP, Petén, Guatemala"
dat_modified$protected_area[[41]]<-"Reserva de Biosfera Maya - Guatemala"



#"Parque Nacional Volcan Isluga, R. N. Pampa del Tamarugal"
dat_modified$protected_area[[42]]<-c("Parque Nacional Volcan Isluga - Chile", 
                                     "Reserva Nacional Pampa del Tamarugal - Chile")



#Mugie Conservancy
dat_modified$protected_area[[43]]<-"Mugie Conservancy - Kenya"



#KAHUZI-BIEGA
dat_modified$protected_area[[44]]<-"Kahuzi-Biega National Park - Democratic Republic of Congo"



#"Mara Triangle"
dat_modified$protected_area[[45]]<-"Mara Triangle - Kenya"



#Endau - Rompin Landscape
dat_modified$protected_area[[46]]<-c("Endau-Rompin National Park - Malaysia")



#"Batang Ai National Park\nLanjak - Entimau Wildlife Sanctuary \nSedilu-Ulu Sebuyau-Lesong Landscape"

dat_modified$protected_area[[47]][2]<-trimws(x = dat_modified$protected_area[[47]][2], which = "right")

dat_modified$protected_area[[47]]<-paste(dat_modified$protected_area[[47]], "- Malaysia")



#"Murchison Falls National Park"
dat_modified$protected_area[[48]]<-"Murchison Falls National Park - Uganda"



#"Itombwe nature reserve"
dat_modified$protected_area[[49]]<-"Itombwe Nature Reserve - Democratic Republic of Congo"



#Santuario Nacional de Ampay
dat_modified$protected_area[[50]]<-"Santuario Nacional de Ampay - Peru"



#"Reserva Comunal Machiguenga"
dat_modified$protected_area[[51]]<-"Reserva Comunal Machiguengay - Peru"



#RP NOR YAUYOS COCHAS
dat_modified$protected_area[[52]]<-"Reserva Paisajística Nor Yauyos-Cochas - Peru"



#"PARQUE NACIONAL ICHIGKAT MUJA CORDILLERA DEL CONDOR"
dat_modified$protected_area[[53]]<-"Parque Nacional Ichigkat Muja-Cordillera del Cóndor - Peru"



#"Reserva Comunal Yanesha"
dat_modified$protected_area[[54]]<-"Reserva Comunal Yanesha - Peru"



# "Reserva Comunal Ashaninka"
dat_modified$protected_area[[55]]<-"Reserva Comunal Ashaninka - Peru"



#"Zona Reservada Santiago Comaina"
dat_modified$protected_area[[56]]<-"Zona Reservada Santiago Comaina - Peru"



#"Santuario Nacional de Huayllay"
dat_modified$protected_area[[57]]<-"Santuario Nacional de Huayllay - Peru"



#"Parque Nacional Otishi"
dat_modified$protected_area[[58]]<-"Parque Nacional Otishi - Peru"



#"Naldo Auber  Peña Manosalva"
#dat_modified$protected_area[[59]]<-"Naldo Auber  Peña Manosalva". Name provided



#"RESERVA COMUNAL EL SIRA"
dat_modified$protected_area[[60]]<-"Reserva Comunal El Sira - Peru"



#"SANTUARIO NACIONAL CORDILLERA DE COLAN"
dat_modified$protected_area[[61]]<-"Santuario Nacional Cordillera de Colan - Peru"



#"PARQUE NACIONAL ALTO PURUS"
dat_modified$protected_area[[62]]<-"Parque Nacional de Alto Purus - Peru"



#"Zona Reservada Ancón"
dat_modified$protected_area[[63]]<-"Zona Reservada Ancón - Peru"



#"William Zeña sencio" 
#dat_modified$protected_area[[64]]<-"William Zeña Sencio" ths is the name



#"Reserva Nacional Illescas"
dat_modified$protected_area[[65]]<-"Reserva Nacional Illescas - Peru"



#"Reserva Nacional Illescas"
dat_modified$protected_area[[66]]<-"Reserva Nacional Pacaya Samiria - Peru"



#"Reserva Nacional Pacaya Samiria"
dat_modified$protected_area[[67]]<-"SINANPE - Peru" # would this imply 76 protected areas?



#"PARQUE NACIONAL HUASCARAN"
dat_modified$protected_area[[68]]<-"Parque Nacional Huascaran - Peru" 



#Alder Feijoo
dat_modified$protected_area[[69]]<-"Santuario Nacional Los Manglares de Tumbes - Peru" 



#"SN Megantoni"
dat_modified$protected_area[[70]]<-"Santuario Nacional Megantoni - Peru"



#"Reserva Nacional de Lachay"
dat_modified$protected_area[[71]]<-"Reserva Nacional de Lachay - Peru"



#Reserva Nacional Matsés
dat_modified$protected_area[[72]]<-"Reserva Nacional Matsés - Peru"



#RN Pampa Galeras Barbara D'Achille
dat_modified$protected_area[[73]]<-"Reserva Nacional Pampa Galeras Barbara D'Achille - Peru"



#"Parque Nacional Sierra del Divisor"
dat_modified$protected_area[[74]]<-"Parque Nacional Sierra del Divisor - Peru"



#RVS LOS PANTANOS DE VILLA"
dat_modified$protected_area[[75]]<-"Reserva de Vida Silvestre Los Pantanos de Villa - Peru"



#Refugio de Vida silvestre Laquipampa - SERNANP - Perú"
dat_modified$protected_area[[76]]<-"Refugio de Vida Silvestre Laquipampa - Peru"



#Parque Nacional del Río Abiseo (Abiseo River's National Park from Peru)"
dat_modified$protected_area[[77]]<-"Parque Nacional del Río Abiseo - Peru"



#"Parque Nacional Río Abiseo, Santuario Nacional de Calipuy, Reserva Nacional de Calipuy. Parque Nacional Huascarán, Parque Nacional Manu, etc."
dat_modified$protected_area[[78]]<-c("Parque Nacional Río Abiseo - Peru", 
                                     "Santuario Nacional de Calipuy - Peru",
                                     "Reserva Nacional de Calipuy - Peru",
                                     "Parque Nacional Huascaran - Peru",
                                     "Parque Nacional Manu - Peru")



#"Reserva Nacional Allpahuayo Mishana"
dat_modified$protected_area[[79]]<- "Reserva Nacional Allpahuayo Mishana - Peru"



#Reserva Nacional Tambopata
dat_modified$protected_area[[80]]<- "Reserva Nacional Tambopata - Peru"



#Mambo Phillip suggest to remove. Seems to be from Tanzania but no affiliation found
#dat_modified$protected_area[[81]]<- "Unknown"



#Parque Nacional de Cutervo
dat_modified$protected_area[[82]]<-"Parque Nacional de Cutervo - Peru"



#Parque Nacional Bahuaja Sonene
dat_modified$protected_area[[83]]<-"Parque Nacional Bahuaja Sonene - Peru"




#Srepok Wildlife Sanctuary, Phnom Prich Wildlife Sanctuary"
dat_modified$protected_area[[84]]<-c("Sre Pok Wildlife Sanctuary - Cambodia", 
                                     "Phnom Prich Wildlife Sanctuary - Cambodia")




#"Parque Nacional Zona Marina Archipiélago de Espíritu Santo,"
dat_modified$protected_area[[85]]<-c("Parque Nacional Archipiélago de Espíritu Santo - Mexico")




#PARQUE NACIONAL CAÑÓN DEL SUMIDERO
dat_modified$protected_area[[86]]<-c("Parque Nacional Canon del Sumidero - Mexico")




#PARQUE NACIONAL CAÑÓN DEL SUMIDERO
dat_modified$protected_area[[87]]<-c("Area de Protección de Flora y Fauna Bavispe - Mexico")




#"San Felipe de León\nSan Antonio Analco"
dat_modified$protected_area[[88]]<-c("San Felipe de León - Mexico", 
                                     "San Antonio Analco - Mexico")



#RB EL VIZCAINO"
dat_modified$protected_area[[89]]<-c("Reserva Biologica El Vizcaino - Mexico")



#Reserva de la Biosfera Los Petenes
dat_modified$protected_area[[90]]<-c("Reserva de la Biosfera Los Petenes - Mexico")



#"PARQUE NACIONAL ISLA CONTOY-COMISION NACIONAL DE AREAS NATURALES PROTEGIDAS (CONANP), MÉXICO."
dat_modified$protected_area[[91]]<-c("Parque Nacional Isla Contoy - Mexico")



#SANTUARIO PLAYA DE MISMALOYA
dat_modified$protected_area[[92]]<-c("Santuario Playon de Mismaloya - Mexico")




#Parque Nacional Tulum CONANP
dat_modified$protected_area[[93]]<-c("Parque Nacional Tulum - Mexico")




#"PARQUE NACIONAL SISTEMA ARRECIFAL VERACRUZANO"
dat_modified$protected_area[[94]]<-c("Parque Nacional Sistema Arrecifal Veracruzano - Mexico")




#RB Los Tuxtlas"
dat_modified$protected_area[[95]]<-c("Reserva de la Biosfera Los Tuxtlas - Mexico")




#"RESERVA DE LA BIOSFERA PANTANOS DE CENTLA."
dat_modified$protected_area[[96]]<-c("Reserva de la Biósfera Pantanos de Centla - Mexico")




#"AREA DE PROTECCION DE FLORA Y FAUNA CABO SAN LUCAS"
dat_modified$protected_area[[97]]<-c("Area de Proteccion de Flora y Fauna Cabo San Lucas - Mexico")




#"Parque Nacional Isla Isabel"
dat_modified$protected_area[[98]]<-c("Parque Nacional Isla Isabel - Mexico")



#ejemplo
#dat_modified$protected_area[[99]]

#Endau-Rompin"
dat_modified$protected_area[[100]]<-c("Endau-Rompin National Park - Malaysia")




#Batang Ai National Park\nLanjak-Entimau Wildlife Sanctuary\nSedilu-Sebuyau-Lesong- landscape
dat_modified$protected_area[[101]]<-c("Batang Ai National Park - Malaysia",
                                      "Lanjak - Entimau Wildlife Sanctuary - Malaysia",
                                      "Sedilu National Park - Malaysia", 
                                      "Ulu Sebuyau National Park - Malaysia", 
                                      "Gunung Lesung National Park - Malaysia")



#"Huai Kha Khaeng Wildlife Sanctuary\nThung Yai Naresuan Wildlife Sanctuary\nKaeng Krachan National Park\nThap Lan National Park"
dat_modified$protected_area[[102]]<-c("Huai Kha Khaeng Wildlife Sanctuary - Thailand",
                                      "Thung Yai Naresuan Wildlife Sanctuary - Thailand", 
                                      "Kaeng Krachan National Park - Thailand",
                                      "Thap Lan National Park - Thailand")



#"Taman Negara Terengganu Malaysia"
dat_modified$protected_area[[103]]<-c("Taman Negara Terengganu National Park - Malaysia")



#"Nouabale-Ndoki National Park (NNNP)\nPeripheral Ecosystem Management Project in Nouabale-Ndoki National Park (PROGEPP-Kabo)\nLake Tele Community Reserve (LCR)"
dat_modified$protected_area[[104]]<-c("Nouabale-Ndoki National Park - Republic of Congo", 
                                      "Nouabalé-Ndoki National Park Peripheral Ecosystems Management Project - Republic of Congo",
                                      "Lake Téle Community Reserve - Republic of Congo")




#""Parque Nacional y Área Natural de Manejo Integrado Madidi\nÁrea Natural de Manejo Integrado Nacional Apolobamba\nReseva de la Biosfera y Terriorio Comunitario de Origen Pilón Lajas\nReserva de la Biosfera Estación Biológica del Beni""
dat_modified$protected_area[[105]]<-c("Parque Nacional y Área Natural de Manejo Integrado Madidi - Bolivia", 
                                      "Parque Nacional y Área Natural de Manejo Integrado Apolobamba - Bolivia", 
                                      "Reserva de la biosfera y tierra comunitaria de origen Pilón Lajas - Bolivia", 
                                      "Reserva de la Biosfera Estación Biológica del Beni - Bolivia")



#Reserva de la Biosfera Montes Azules\nMonumento Natural Bonampak
dat_modified$protected_area[[106]]<-c("Reserva de la Biosfera Montes Azules - Mexico", 
                                      "Monumento Natural Bonampak - Mexico")



#"Niassa Special Reserve - Mozambique"
#dat_modified$protected_area[[107]]



#"Area protegida Autonoma Descentralizada Cordillera Oriental del Carchi"
dat_modified$protected_area[[108]]<-c("Area protegida Autonoma Descentralizada Cordillera Oriental del Carchi - Ecuador")



#"PARQUE NACIONAL LLANGANATES"
dat_modified$protected_area[[109]]<-c("Parque Nacional Llanganates - Ecuador")



#Parque Nacional Yacuri
dat_modified$protected_area[[110]]<-c("Parque Nacional Yacuri - Ecuador")



#"Reserva Biológica Cerro Plateado"
dat_modified$protected_area[[111]]<-c("Reserva Biologica Cerro Plateado - Ecuador")



#"Parque Nacional Sangay Zona Alta"
dat_modified$protected_area[[112]]<-c("Parque Nacional Sangay Zona Alta - Ecuador")



#Parque nacional Cotacachi-Cayapas
dat_modified$protected_area[[113]]<-c("Parque nacional Cotacachi-Cayapas - Ecuador")



#"Reserva del Hombre y la Biosfera de Río Plátano\nReserva de Biosfera Tawahka-Asangni\nParque Nacional Warunta"
dat_modified$protected_area[[114]]<-c("Reserva del Hombre y la Biosfera del Río Plátano - Honduras", 
                                      "Reserva de la Biosfera Tawahka Asangni - Honduras",
                                      "Parque Nacional Río Warunta - Honduras")



#"Parque Nacional Podocarpus"
dat_modified$protected_area[[115]]<-c("Parque Nacional Podocarpus - Ecuador")



#"National Parks System"
dat_modified$protected_area[[116]] <- "59 Protected Areas - Colombia" 



#Parque Nacional Cajas
dat_modified$protected_area[[117]]<-c("Parque Nacional Cajas - Ecuador")



# "PNNN PNOK PNCD RCLT PNNP RNGLL PROGEPP-KABO PROGEPP NGOMBE ETIC TALA TALA"
dat_modified$protected_area[[118]] <-dat_modified$protected_area[[17]] # same as 17



#"RB ISLAS DEL PACIFICO"
dat_modified$protected_area[[119]]<-c("Reserva de la Biosfera Islas del Pacífico - Mexico")




#"Nouabale-Ndoki National Park, Republic of the Congo"
dat_modified$protected_area[[120]] <- "Nouabale-Ndoki National Park - Republic of Congo"



#Parque Nacional Yasuní
dat_modified$protected_area[[121]] <- "Parque Nacional Yasuní - Ecuador"



#"Reserva Ecológica Arenillas"
dat_modified$protected_area[[122]] <- "Reserva Ecológica Arenillas - Ecuador"



#Parque Nacional Antisana
dat_modified$protected_area[[123]] <- "Parque Nacional Antisana - Ecuador"



#"Parque Nacional Huatulco"
dat_modified$protected_area[[124]] <- "Parque Nacional Huatulco - Mexico"



#Parque Nacional Yasuní
dat_modified$protected_area[[125]] <- "Parque Nacional Yasuní - Ecuador"



#"Endau Rompin Landscape"
dat_modified$protected_area[[126]]<-c("Endau-Rompin National Park - Malaysia")



#""RUKWA GAME RESERVE\nRUNGWA KIZIGO MUHESI GAME RESERVE\nRUKWATI PITI GAMERESERVE." Smae as 32
dat_modified$protected_area[[127]]<-c("Uwanda Rukwa Game Reserve - Tanzania", 
                                      "Rungwa Game Reserve - Tanzania",
                                      "Kizigo Game Reserve - Tanzania", 
                                      "Muhesi Game Reserve - Tanzania",
                                      "Lukwati Game Reserve - Tanzania", 
                                      "Piti Game Reserve - Tanzania")



#Tanintharyi Nature Reserve
dat_modified$protected_area[[128]]<-c("Tanintharyi Nature Reserve - Myanmar")


#"Rungwa\nKizigo\nMuhesi Game Reserves"
dat_modified$protected_area[[129]]<-c("Rungwa Game Reserve - Tanzania",
                                      "Kizigo Game Reserve - Tanzania", 
                                      "Muhesi Game Reserve - Tanzania")


dat_modified_filtered<-dat_modified
  
#add the survey number as a column
dat_modified_filtered$survey <- 1:nrow(dat_modified_filtered)

# --------------------------------------------------- #
# Assign "Unknown protected area" to specific answers #
# --------------------------------------------------- #


#"Community Forests - Unknown"
dat_modified_filtered$protected_area[[14]]<-"Unknown protected area" 

# The respondent provided a person's name instead of the protected area name
dat_modified_filtered$protected_area[c(12, 13, 20, 23, 59, 64, 81)]<-
                                   "Unknown protected area" 


# --------------------------------------------------------------- #
#  Fix responses with protected areas from more than one country #
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
dat_modified_filtered<-dat_modified_filtered %>% bind_rows(dat_modified_filtered[4,])

#add the PAs to the last row associated with Tanzania
dat_modified_filtered$protected_area[[nrow(dat_modified_filtered)]]<-
                                  dat_modified_filtered$protected_area[[4]][
                                        grepl(pattern = "Tanzania", 
                                              ignore.case = T, 
                                              dat_modified_filtered$protected_area[[4]])]


#remove the PAs in the original row associated with the second country 
# in the original row

dat_modified_filtered$protected_area[[4]]<-dat_modified_filtered$protected_area[[4]][
                                        grepl(pattern = "Kenya", 
                                              ignore.case = T, 
                                              dat_modified_filtered$protected_area[[4]])]

# ---------------------------------------------------------------- #
# add the original number of pas per response as a column          #
# ---------------------------------------------------------------- #

dat_modified_filtered$or_number_pas <-map_int(dat_modified_filtered$protected_area,  
                                     \(x) length(x))               







