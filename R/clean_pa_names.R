
dat_modified<-dat_modified %>%  as_tibble()

#dat_modified$protected_area<-map(dat_modified$protected_area, \(x) strsplit(x, "\n|, ")[[1]])

# column with protected areas

protected_areas<-split(x = dat_modified$protected_area, f = c(1:nrow(dat_modified)))
protected_areas<-map(protected_areas, \(x) tibble(protected_area=x))


#Mann Wildlife Sanctuary
protected_areas[[1]]$protected_area<-"Mann Wildlife Sanctuary - Myanmar"

#Wangchuck Centennial National Park
protected_areas[[2]]$protected_area<-"Wangchuck Centennial National Park - Bhutan"

#Tapir Mountain Nature Reserve
protected_areas[[3]]$protected_area<-"Tapir Mountain Nature Reserve - Belize"

#LUMO community Conservancy\nMgeno Conservancy\nTaita Hills Sanctuary\nTaita Wildlife conservancy\nKasigau wildlife conservancy\nTanzania\nRombo District\nMwanga district\nSame District\nLushoto District\nKorogwe District

protected_areas[[4]]<-protected_areas[[4]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[4]]$protected_area[2:6]<-paste(protected_areas[[4]]$protected_area[2:6], "- Kenya") # adding the country to the corresponding set of PAs
protected_areas[[4]]$protected_area[8:12]<-paste(protected_areas[[4]]$protected_area[8:12], "- Tanzania")  # adding the country to the corresponding set of PAs
protected_areas[[4]]<-protected_areas[[4]][-c(1,7),]


#"WCNP, JDNP, JWS, PWS, Royal Manas National Park, Jigme Khesar Strict Nature Reserve, Jigme Singye Wangchuck, PNP, SWS & BWS and 14 Divisional forest"

protected_areas[[5]]<-protected_areas[[5]] %>% separate_longer_delim(protected_area, delim = ", ")
protected_areas[[5]]<-protected_areas[[5]] %>% separate_longer_delim(protected_area, delim = "& ")
protected_areas[[5]]<-protected_areas[[5]] %>% separate_longer_delim(protected_area, delim = "and ")

protected_areas[[5]]<-protected_areas[[5]][-11,]
protected_areas[[5]]$protected_area<-c("Wangchuck Centennial National Park",
                                    "Jigme Dorji National Park", 
                                    "Jomotshangkha Wildlife Sanctuary", 
                                    "Phibsoo Wildlife Sanctuary", 
                                    "Royal Manas National Park", 
                                    "Jigme Khesar Strict Nature Reserve", 
                                    "Jigme Singye Wangchuck National Park", 
                                    "Phrumsengla National Park", 
                                    "Sakteng Wildlife Sanctuary",
                                    "Bumdeling Wildlife Sanctuary")

protected_areas[[5]]$protected_area<-paste(protected_areas[[5]]$protected_area, "- Bhutan")

#Gbele Resource Reserve
protected_areas[[6]]$protected_area<-paste(protected_areas[[6]]$protected_area, "- Ghana")


#"Belize National Protected Areas System"

protected_areas[[7]] <- tibble(protected_area=
                                  c( "Aguas Turbias National Park",
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
                                      "South Water Caye Marine Reserve"))

protected_areas[[7]]$protected_area<-paste(protected_areas[[7]]$protected_area, "- Belize")

#The mountainous part of Croatia where wolves live (about 20000 sq km)
protected_areas[[8]]$protected_area<-"Dinarides mountain range - Croatia"

#"1. SM Tanjung Peropa\n2. SM Tanjung Anolengo\n3. SM Buton Utara\n4. SM Tanjung Batikolo\n5. SM Lambusango"

protected_areas[[9]]<-protected_areas[[9]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[9]]$protected_area<-gsub("SM ", replacement = "", protected_areas[[9]]$protected_area)
protected_areas[[9]]$protected_area<-paste(protected_areas[[9]]$protected_area, "Wildlife Reserve - Indonesia")
protected_areas[[9]]$protected_area<-sub(".*? ", "", protected_areas[[9]]$protected_area)

#Conservation South Luangwa
protected_areas[[10]]$protected_area<-"Conservation South Luangwa - Zambia"

#"Siem Pang Wildlife Sanctuary"
protected_areas[[11]]$protected_area<-"Siem Pang Wildlife Sanctuary - Cambodia"


#Myo Myint Aung
protected_areas[[12]]$protected_area<-"Unknown protected area"

#Pierre Megfy
protected_areas[[13]]$protected_area<-"Unknown protected area"

#"Community Forests\nPrivate Conservancy"
protected_areas[[14]]<-protected_areas[[14]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[14]]$protected_area<-paste(protected_areas[[14]]$protected_area, " - Unknown")


#"North Luangwa Conservation Programme
protected_areas[[15]]$protected_area<-"North Luangwa Conservation Programme - Zambia"

#"Khunjerab National Park\nChitral Gol National Park"
protected_areas[[16]]<-protected_areas[[16]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[16]]$protected_area<-paste0(protected_areas[[16]]$protected_area, " - Pakistan") 


#"Nouabale-Ndoki National Park (NNNP)\nOdzala Kokoua National Park (PNOK)\nNtokou Pikounda National Park (PNNP)\nProject for the Management of Preipheral Ecosystems in Nouabale-Ndoki National Park (PROGEPP-Kabo)\nPreipherical Ecosystem Management Project in Odzala-Kokoua National Park (PROGEPP-Ngombe)\nLac Tele Community Reserve (RCLT)\nEspace TRIDOM Inter-Zone (ETIC)\nUnite de Surveillance et de Lutte Anti-Braconnage (USLAB) Tala-Tala\nLesio-Luna Gorilla Nature Reserve (RNGLL)"
protected_areas[[17]]<-protected_areas[[17]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[17]]<-protected_areas[[17]][1:9,]
protected_areas[[17]]$protected_area<-gsub(" [(].*","",protected_areas[[17]]$protected_area)
protected_areas[[17]]$protected_area[4]<-"Management of Peripheral Ecosystems in Nouabale-Ndoki National Park"
protected_areas[[17]]$protected_area[5]<-"Management of Peripheral Ecosystems in Odzala-Kokoua National Park"
protected_areas[[17]]$protected_area[6]<-"Lake Téle Community Reserve"
protected_areas[[17]]$protected_area<-paste(protected_areas[[17]]$protected_area, "- Republic of Congo")

#COMATSA
protected_areas[[18]]$protected_area<-"COMATSA Protected Area - Madagascar"

# "Parc national du Banco  Parc national d'Azagny  Parc national des Iles Ehotilé  Réserve naturelle de Dahliafleur  Réserve naturelle partielle d'Aghien  Réserve naturelle de Mabi-Yaya  Réserve naturelle de Bossomatié"
protected_areas[[19]]<-protected_areas[[19]] %>% separate_longer_delim(protected_area, delim = "  ")
protected_areas[[19]]$protected_area<-paste(protected_areas[[19]]$protected_area, "- Côte d'Ivoire")

#"KAPUKU MBOMBO JOHN"
protected_areas[[20]]$protected_area<-"Unknown protected area"

#"Tsavo west Intensive Protection Zone\nNgulia Rhino Sanctuary"
protected_areas[[21]]<-protected_areas[[21]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[21]]$protected_area<-paste(protected_areas[[21]]$protected_area, "- Kenya")

#Queen Elizabeth Protected Area
protected_areas[[22]]$protected_area<-"Queen Elizabeth Protected Area - Uganda"

#"Daudi Mollel"
protected_areas[[23]]$protected_area<-"Unknown protected area"

#"Endau Rompin Landscape"
protected_areas[[24]]$protected_area<-"Endau-Rompin National Park - Malaysia"

#"Sambor wildlife sanctuary, Prek Prasab Wildlife Sanctuary"
protected_areas[[25]]<-protected_areas[[25]] %>% separate_longer_delim(protected_area, delim = ", ")
protected_areas[[25]]$protected_area<-paste(protected_areas[[25]]$protected_area, "- Cambodia")

#"Chipanje Chetu"
protected_areas[[26]]$protected_area<-"Chipanje Chetu Community Conservation - Mozambique"

#"Khoid Mogoin Gol - Teel LPA\nGreat Gobi Strictly Protected Area Part \"A\"\nMongolian Ecological Police Department"
protected_areas[[27]]<-protected_areas[[27]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[27]]$protected_area<-gsub('\"', replacement = "", protected_areas[[27]]$protected_area)
protected_areas[[27]]$protected_area<-gsub('LPA', replacement = "Local Protected Area", protected_areas[[27]]$protected_area)
protected_areas[[27]]$protected_area<-gsub(' - ', replacement = " ", protected_areas[[27]]$protected_area)
protected_areas[[27]]$protected_area<-paste(protected_areas[[27]]$protected_area, "- Mongolia")


#"Gunung Nyiut Nature Reserve\nGunung Naning Protected Forest\nKarimata Marine Reserve\nSeruat Pulau Tiga Protected Forest"
protected_areas[[28]]<-protected_areas[[28]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[28]]$protected_area<-paste(protected_areas[[28]]$protected_area, "- Indonesia")


#"Phibsoo Wildlife Sanctuary, Department Of Forest and Park Services, Ministry of Agriculture and Forest. Bhutan"
protected_areas[[29]]$protected_area<-"Phibsoo Wildlife Sanctuary - Bhutan"

#SABAH PARK (STATION : TAWAU HILLS PARK) - PILOT IMPLEMENTATION
protected_areas[[30]]$protected_area<-"Tawau Hills Park - Malaysia"

#"WCS all protected areas - Madagascar National parks Protected areas (45)"
protected_areas[[31]]<-tibble(protected_area=rep("protected area - Madagascar", 45))

#"Rungwa Game Reserve\nKizigo Game Reserve\nMuhesi Game Reserve\nUgalla Game Reserve\nLukwati-Piti Game Reserve\nRukwa Game Reserve\nLwafi Game Reserve"
protected_areas[[32]]<-protected_areas[[32]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[32]]$protected_area<-paste(protected_areas[[32]]$protected_area, "- Tanzania")


#"Mt. Goplom Conservation Area, Kwiop village, Jimi District, Jiwaka Province, Papua New Guinea\nMt. Waugerema Conservation Area, Daulo District, Eastern Highlands Province\nYasina Nature Park, Ungai-Bena District, Eastern Highlands Province"
protected_areas[[33]]<-protected_areas[[33]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[33]]$protected_area[1]<-"Mount goplom conservation area - Papua New Guinea"
protected_areas[[33]]$protected_area[2]<-"Mount Waugerema Conservation Area - Papua New Guinea"
protected_areas[[33]]$protected_area[3]<-"Yasina Nature Parkk - Papua New Guinea"

#Jigme Singye Wangchuck National Park"
protected_areas[[34]]$protected_area<-"Jigme Singye Wangchuck National Park - Bhutan"

#"Jigme Singye Wangchuck National Park"
protected_areas[[35]]$protected_area<-"Jigme Singye Wangchuck National Park - Bhutan"

#"Budongo\nItwara"
protected_areas[[36]]<-protected_areas[[36]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[36]]$protected_area<-paste(protected_areas[[36]]$protected_area, "- Uganda")

#Nsumbu Tanganyika
protected_areas[[37]]$protected_area<-"Nsumbu Tanganyika Conservation Programme - Zambia"

#"Yasuni"
protected_areas[[38]]$protected_area<-"Yasuni National Park - Ecuador"

#"Cardamom Landscape"
protected_areas[[39]]$protected_area<-"Cardamom Landscape - Cambodia"

#[1] "1.) Maite Marine Protected Area (MPA)\n2.) Lower Cabangcalan MPA\n3.) Olang MPA\n4.) Tulapos MPA\n5.) Cangbagsa MPA\n6.) Caticugan MPA"
protected_areas[[40]]<-protected_areas[[40]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[40]]$protected_area<-gsub(" [(]MPA[)]| MPA", " Marine Protected Area", protected_areas[[40]]$protected_area)
protected_areas[[40]]$protected_area<-paste(protected_areas[[40]]$protected_area, "- Philippines")

#"Reserva de Biosfera Maya, SIGAP, Petén, Guatemala"
protected_areas[[41]]$protected_area<-"Reserva de Biosfera Maya - Guatemala"

#"Parque Nacional Volcan Isluga, R. N. Pampa del Tamarugal"
protected_areas[[42]]<-protected_areas[[42]] %>% separate_longer_delim(protected_area, delim = ", ")
protected_areas[[42]]$protected_area<-paste(protected_areas[[42]]$protected_area, "- Chile")

#Mugie Conservancy
protected_areas[[43]]$protected_area<-"Mugie Conservancy - Kenya"

#KAHUZI-BIEGA
protected_areas[[44]]$protected_area <-"Kahuzi-Biega National Park - Democratic Republic of Congo"

#"Mara Triangle"
protected_areas[[45]]$protected_area<-"Mara Triangle - Kenya"

#Endau - Rompin Landscape
protected_areas[[46]]$protected_area<-"Endau-Rompin National Park - Malaysia"

#"Batang Ai National Park\nLanjak - Entimau Wildlife Sanctuary \nSedilu-Ulu Sebuyau-Lesong Landscape"
protected_areas[[47]]<-protected_areas[[47]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[47]]$protected_area<-gsub('-', replacement = " ", protected_areas[[47]]$protected_area)
protected_areas[[47]]$protected_area<-paste(protected_areas[[47]]$protected_area, "- Malaysia")

#"Murchison Falls National Park"
protected_areas[[48]]$protected_area<-"Murchison Falls National Park - Uganda"

#"Itombwe nature reserve"
protected_areas[[49]]$protected_area<-"Itombwe Nature Reserve - Democratic Republic of Congo"

#Santuario Nacional de Ampay
protected_areas[[50]]$protected_area<-"Santuario Nacional de Ampay - Peru"

#"Reserva Comunal Machiguenga"
protected_areas[[51]]$protected_area<-"Reserva Comunal Machiguengay - Peru"

#RP NOR YAUYOS COCHAS
protected_areas[[52]]$protected_area<-"Reserva Paisajística Nor Yauyos-Cochas - Peru"

#"PARQUE NACIONAL ICHIGKAT MUJA CORDILLERA DEL CONDOR"
protected_areas[[53]]$protected_area<-"Parque Nacional Ichigkat Muja-Cordillera del Cóndor - Peru"

#"Reserva Comunal Yanesha"
protected_areas[[54]]$protected_area<-"Reserva Comunal Yanesha - Peru"

# "Reserva Comunal Ashaninka"
protected_areas[[55]]$protected_area<-"Reserva Comunal Ashaninka - Peru"

#"Zona Reservada Santiago Comaina"
protected_areas[[56]]$protected_area<-"Zona Reservada Santiago Comaina - Peru"

#"Santuario Nacional de Huayllay"
protected_areas[[57]]$protected_area<-"Santuario Nacional de Huayllay - Peru"

#"Parque Nacional Otishi"
protected_areas[[58]]$protected_area<-"Parque Nacional Otishi - Peru"

#"Naldo Auber  Peña Manosalva"
protected_areas[[59]]$protected_area<-"Unknown protected area"

#"RESERVA COMUNAL EL SIRA"
protected_areas[[60]]$protected_area<-"Reserva Comunal El Sira - Peru"

#"SANTUARIO NACIONAL CORDILLERA DE COLAN"
protected_areas[[61]]$protected_area<-"Santuario Nacional Cordillera de Colan - Peru"

#"PARQUE NACIONAL ALTO PURUS"
protected_areas[[62]]$protected_area<-"Parque Nacional de Alto Purus - Peru"

#"Zona Reservada Ancón"
protected_areas[[63]]$protected_area<-"Zona Reservada Ancón - Peru"

#"William Zeña sencio" 
protected_areas[[64]]$protected_area<-"Unknown protected area"

#"Reserva Nacional Illescas"
protected_areas[[65]]$protected_area<-"Reserva Nacional Illescas - Peru"

#"Reserva Nacional Illescas"
protected_areas[[66]]$protected_area<-"Reserva Nacional Pacaya Samiria - Peru"

#"SINANPE"
protected_areas[[67]]<-tibble(protected_area=rep("protected area - Peru", 76))

#"PARQUE NACIONAL HUASCARAN"
protected_areas[[68]]$protected_area<-"Parque Nacional Huascaran - Peru" 

#Alder Feijoo
protected_areas[[69]]$protected_area<-"Santuario Nacional Los Manglares de Tumbes - Peru" 

#"SN Megantoni"
protected_areas[[70]]$protected_area<-"Santuario Nacional Megantoni - Peru"

#"Reserva Nacional de Lachay"
protected_areas[[71]]$protected_area<-"Reserva Nacional de Lachay - Peru"

#Reserva Nacional Matsés
protected_areas[[72]]$protected_area<-"Reserva Nacional Matsés - Peru"

#RN Pampa Galeras Barbara D'Achille
protected_areas[[73]]$protected_area<-"Reserva Nacional Pampa Galeras Barbara D'Achille - Peru"

#"Parque Nacional Sierra del Divisor"
protected_areas[[74]]$protected_area<-"Parque Nacional Sierra del Divisor - Peru"

#RVS LOS PANTANOS DE VILLA"
protected_areas[[75]]$protected_area<-"Reserva de Vida Silvestre Los Pantanos de Villa - Peru"

#Refugio de Vida silvestre Laquipampa - SERNANP - Perú"
protected_areas[[76]]$protected_area<-"Refugio de Vida Silvestre Laquipampa - Peru"

#Parque Nacional del Río Abiseo (Abiseo River's National Park from Peru)"
protected_areas[[77]]$protected_area<-"Parque Nacional del Río Abiseo - Peru"

#"Parque Nacional Río Abiseo, Santuario Nacional de Calipuy, Reserva Nacional de Calipuy. Parque Nacional Huascarán, Parque Nacional Manu, etc."
protected_areas[[78]]<-protected_areas[[78]] %>% separate_longer_delim(protected_area, delim = ", ")
protected_areas[[78]]<-protected_areas[[78]] %>% separate_longer_delim(protected_area, delim = ". ")
protected_areas[[78]]<-protected_areas[[78]][1:5,]
protected_areas[[78]]$protected_area<-paste(protected_areas[[78]]$protected_area, "- Peru")

#"Reserva Nacional Allpahuayo Mishana"
protected_areas[[79]]$protected_area<- "Reserva Nacional Allpahuayo Mishana - Peru"

#Reserva Nacional Tambopata
protected_areas[[80]]$protected_area<- "Reserva Nacional Tambopata - Peru"

#Mambo Phillip suggest to remove.
protected_areas[[81]]$protected_area<-"Unknown protected area"

#Parque Nacional de Cutervo
protected_areas[[82]]$protected_area<-"Parque Nacional de Cutervo - Peru"

#Parque Nacional Bahuaja Sonene
protected_areas[[83]]$protected_area<-"Parque Nacional Bahuaja Sonene - Peru"

#Srepok Wildlife Sanctuary, Phnom Prich Wildlife Sanctuary"
protected_areas[[84]]<-protected_areas[[84]] %>% separate_longer_delim(protected_area, delim = ", ")
protected_areas[[84]]$protected_area<-paste(protected_areas[[84]]$protected_area, "- Cambodia")

#"Parque Nacional Zona Marina Archipiélago de Espíritu Santo,"
protected_areas[[85]]$protected_area<-"Parque Nacional Archipiélago de Espíritu Santo - Mexico"

#PARQUE NACIONAL CAÑÓN DEL SUMIDERO
protected_areas[[86]]$protected_area<-"Parque Nacional Canon del Sumidero - Mexico"

#PARQUE NACIONAL CAÑÓN DEL SUMIDERO
protected_areas[[87]]$protected_area<-"Area de Protección de Flora y Fauna Bavispe - Mexico"

#"San Felipe de León\nSan Antonio Analco"
protected_areas[[88]]<-protected_areas[[88]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[88]]$protected_area<-paste(protected_areas[[88]]$protected_area, "- Mexico")

#RB EL VIZCAINO"
protected_areas[[89]]$protected_area<-"Reserva Biologica El Vizcaino - Mexico"

#Reserva de la Biosfera Los Petenes
protected_areas[[90]]$protected_area<-"Reserva de la Biosfera Los Petenes - Mexico"

#"PARQUE NACIONAL ISLA CONTOY-COMISION NACIONAL DE AREAS NATURALES PROTEGIDAS (CONANP), MÉXICO."
protected_areas[[91]]$protected_area<-"Parque Nacional Isla Contoy - Mexico"

#SANTUARIO PLAYA DE MISMALOYA
protected_areas[[92]]$protected_area<-"Santuario Playon de Mismaloya - Mexico"

#Parque Nacional Tulum CONANP
protected_areas[[93]]$protected_area<-"Parque Nacional Tulum - Mexico"

#"PARQUE NACIONAL SISTEMA ARRECIFAL VERACRUZANO"
protected_areas[[94]]$protected_area<-"Parque Nacional Sistema Arrecifal Veracruzano - Mexico"

#RB Los Tuxtlas"
protected_areas[[95]]$protected_area<-"Reserva de la Biosfera Los Tuxtlas - Mexico"

#"RESERVA DE LA BIOSFERA PANTANOS DE CENTLA."
protected_areas[[96]]$protected_area<-"Reserva de la Biósfera Pantanos de Centla - Mexico"

#"AREA DE PROTECCION DE FLORA Y FAUNA CABO SAN LUCAS"
protected_areas[[97]]$protected_area<-"Area de Proteccion de Flora y Fauna Cabo San Lucas - Mexico"

#"Parque Nacional Isla Isabel"
protected_areas[[98]]$protected_area<-"Parque Nacional Isla Isabel - Mexico"

#ejemplo
#dat_modified$protected_area[[99]]

#Endau-Rompin"
protected_areas[[100]]$protected_area<-"Endau-Rompin National Park - Malaysia"

#Batang Ai National Park\nLanjak-Entimau Wildlife Sanctuary\nSedilu-Sebuyau-Lesong- landscape
protected_areas[[101]]<-protected_areas[[101]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[101]]$protected_area<-paste(protected_areas[[101]]$protected_area, "- Malaysia")

#"Huai Kha Khaeng Wildlife Sanctuary\nThung Yai Naresuan Wildlife Sanctuary\nKaeng Krachan National Park\nThap Lan National Park"
protected_areas[[102]]<-protected_areas[[102]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[102]]$protected_area<-paste(protected_areas[[102]]$protected_area, "- Thailand")

#"Taman Negara Terengganu Malaysia"
protected_areas[[103]]$protected_area<-"Taman Negara Terengganu National Park - Malaysia"

#"Nouabale-Ndoki National Park (NNNP)\nPeripheral Ecosystem Management Project in Nouabale-Ndoki National Park (PROGEPP-Kabo)\nLake Tele Community Reserve (LCR)"
protected_areas[[104]]<-protected_areas[[104]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[104]]$protected_area<-paste(protected_areas[[104]]$protected_area, "- Republic of Congo")
protected_areas[[104]]<-protected_areas[[104]][c(1,3),]
protected_areas[[104]]$protected_area[1]<-"Nouabale-Ndoki National Park - Republic of Congo"

#""Parque Nacional y Área Natural de Manejo Integrado Madidi\nÁrea Natural de Manejo Integrado Nacional Apolobamba\nReseva de la Biosfera y Terriorio Comunitario de Origen Pilón Lajas\nReserva de la Biosfera Estación Biológica del Beni""
protected_areas[[105]]<-protected_areas[[105]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[105]]$protected_area<-paste(protected_areas[[105]]$protected_area, "- Bolivia")


#Reserva de la Biosfera Montes Azules\nMonumento Natural Bonampak
protected_areas[[106]]<-protected_areas[[106]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[106]]$protected_area<-paste(protected_areas[[106]]$protected_area, "- Mexico")

#"Niassa Special Reserve - Mozambique"

#"Area protegida Autonoma Descentralizada Cordillera Oriental del Carchi"
protected_areas[[108]]$protected_area<-"Area protegida Autonoma Descentralizada Cordillera Oriental del Carchi - Ecuador"

#"PARQUE NACIONAL LLANGANATES"
protected_areas[[109]]$protected_area<-"Parque Nacional Llanganates - Ecuador"

#Parque Nacional Yacuri
protected_areas[[110]]$protected_area<-"Parque Nacional Yacuri - Ecuador"

#"Reserva Biológica Cerro Plateado"
protected_areas[[111]]$protected_area<-"Reserva Biologica Cerro Plateado - Ecuador"

#"Parque Nacional Sangay Zona Alta"
protected_areas[[112]]$protected_area<-"Parque Nacional Sangay Zona Alta - Ecuador"

#Parque nacional Cotacachi-Cayapas
protected_areas[[113]]$protected_area<-"Parque nacional Cotacachi-Cayapas - Ecuador"


#"Reserva del Hombre y la Biosfera de Río Plátano\nReserva de Biosfera Tawahka-Asangni\nParque Nacional Warunta"
protected_areas[[114]]<-protected_areas[[114]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[114]]$protected_area<-paste(protected_areas[[114]]$protected_area, "- Honduras")

#"Parque Nacional Podocarpus"
protected_areas[[115]]$protected_area<-"Parque Nacional Podocarpus - Ecuador"

#"National Parks System"
protected_areas[[116]]<-tibble(protected_area=rep("protected area - Colombia", 59))

#Parque Nacional Cajas
protected_areas[[117]]$protected_area<-"Parque Nacional Cajas - Ecuador"

# "PNNN PNOK PNCD RCLT PNNP RNGLL PROGEPP-KABO PROGEPP NGOMBE ETIC TALA TALA"
protected_areas[[118]]<-protected_areas[[118]] %>% separate_longer_delim(protected_area, delim = " ")
protected_areas[[118]]<-protected_areas[[118]][-12, ]
protected_areas[[118]]$protected_area<-paste(protected_areas[[118]]$protected_area, "- Republic of Congo")

#"RB ISLAS DEL PACIFICO"
protected_areas[[119]]$protected_area<-"Reserva de la Biosfera Islas del Pacífico - Mexico"

#"Nouabale-Ndoki National Park, Republic of the Congo"
protected_areas[[120]]$protected_area <- "Nouabale-Ndoki National Park - Republic of Congo"

#Parque Nacional Yasuní
protected_areas[[121]]$protected_area <- "Parque Nacional Yasuní - Ecuador"

#"Reserva Ecológica Arenillas"
protected_areas[[122]]$protected_area <- "Reserva Ecológica Arenillas - Ecuador"

#Parque Nacional Antisana
protected_areas[[123]]$protected_area <- "Parque Nacional Antisana - Ecuador"

#"Parque Nacional Huatulco"
protected_areas[[124]]$protected_area <- "Parque Nacional Huatulco - Mexico"

#Parque Nacional Yasuní
protected_areas[[125]]$protected_area  <- "Parque Nacional Yasuní - Ecuador"

#"Endau Rompin Landscape"
protected_areas[[126]]$protected_area  <- "Endau-Rompin National Park - Malaysia"


#""RUKWA GAME RESERVE\nRUNGWA KIZIGO MUHESI GAME RESERVE\nRUKWATI PITI GAMERESERVE." Smae as 32
protected_areas[[127]]<-protected_areas[[127]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[127]]<-protected_areas[[127]][-4, ]
protected_areas[[127]]$protected_area<-paste(protected_areas[[127]]$protected_area, "- Tanzania")

#Tanintharyi Nature Reserve
protected_areas[[128]]$protected_area  <- "Endau-Rompin National Park - Malaysia"

#"Rungwa\nKizigo\nMuhesi Game Reserves"
protected_areas[[129]]<-protected_areas[[129]] %>% separate_longer_delim(protected_area, delim = "\n")
protected_areas[[129]]$protected_area<-paste(protected_areas[[129]]$protected_area, "- Tanzania")


# dat_modified_filtered<-dat_modified
dat_modified$protected_area<-protected_areas

#add the survey number as a column
dat_modified$survey <- 1:nrow(dat_modified)


# ---------------------------------------------------------------- #
# add the original number of pas per response as a column          #
# ---------------------------------------------------------------- #

dat_modified$or_number_pas <-unname(map_int(dat_modified$protected_area, \(x) nrow(x)))








