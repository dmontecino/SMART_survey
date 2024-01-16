

full_terrestrial_data<-readRDS("data/terrestrial_data.RDS")

terrestrial_data<-terrestrial_data %>% filter(position!="Other")

other_audience<-full_terrestrial_data %>% filter(position=="Other")





length(other_audience) # 27

countries_other_audience<-sort(unique(unlist(map(other_audience$protected_area, \(x) x$country))))
list_countries

countries_in_other_audience_not_in_final_data<-countries_other_audience[
                                              !(countries_other_audience%in%list_countries)]
       
# all countries excuded by other audience are represetned in the final dataset. 





# same idea but now with the protected areas themseleves

other_audience<-other_audience %>% filter(local==T) # 18 local responses 
  
other_audience<-other_audience$protected_area
other_audience<-do.call(rbind, other_audience)
other_audience<-other_audience %>% arrange(country)
other_audience_pas<-other_audience$protected_area
other_audience_pas<-unique(other_audience_pas)

# pas_in_inther_audience represented at the loca level?

local_responses<-terrestrial_data %>% filter(local==T)
local_responses<-local_responses$protected_area
local_responses<-do.call(rbind, local_responses)
local_responses<-local_responses %>% arrange(country)
local_responses_pas<-local_responses$protected_area
local_responses_pas<-unique(local_responses_pas)

#represented
# "Yasuni National Park - Ecuador"  
#"Endau-Rompin National Park - Malaysia"


