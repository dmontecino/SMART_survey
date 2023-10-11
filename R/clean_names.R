new.names<-c(
  
  "time",                           #1. Timestamp
  
  "score",                          #2. Score
  
  "protected_area",                 #3. Protected area names
  
  "wildlife_health_important",      #4. Wildlife health is important to achieve 
  #conservation goals of the protected areas
  
  "wildlife_path_affect_livestock", #5. Wildlife pathogens can affect livestock 
  #health" 
  
  "wildlife_path_affect_phealth",   #6. Wildlife pathogens can affect public 
  #health
  
  "hum_liv_path_affect_wildlife",   #7. Human or livestock pathogens can affect 
  #wildlife populations 
  
  "dom_animals_concern",            #8. Introduced domestic animals are a 
  #concern for conservation" 
  
  "dead_found",                     #9. Are dead animals encountered
  
  "sick_injured_found",             #10. Are sick or injured animals encountered
  
  "livestock_found",                #11. Are livestock encountered
  
  "patrol_length",                  #12. How long, on average, are the patrols 
  #in the protected area?
  
  "patrols_per_month",              #13. On average, how many patrols are 
  #completed in the protected area in one 
  #month?
  
  "healthy_wl_recorded",            #14. Are healthy wildlife found during 
  #patrols recorded as a 
  #specific category of individuals?"
  
  "how_healthy_wl_recorded",        #15. How are healthy wildlife recorded
  
  "healthy_wl_data_recorded",       #16. Type of data recorded from healthy 
  #wildlife
  
  "healthy_wl_data_in_smart",       #17. Are these data entered and stored in
  #SMART Desktop?
  
  "healthy_wl_data_other",          #18. If none of the items are recorded in 
  #the corresponding SMART Conservation Area 
  #where are they recorded? 
  
  "dead_wl_recorded",               #19. Are dead wildlife found during patrols 
  #recorded as a specific category of 
  #individuals?"               
  
  "how_dead_wl_recorded",           #20. How are dead wildlife recorded" 
  
  "dead_wl_data_recorded",          #21. Type of data recorded from dead 
  #wildlife
  
  "dead_wl_data_in_smart",          #22. Are these data entered and stored in 
  #SMART Desktop?
  
  "dead_wl_data_other",             #23 If none of the items are recorded in the
  #corresponding SMART Conservation Area
  #where are they recorded?" 
  
  "injured_wl_recorded",            #24. Are injured wildlife found during 
  #patrols recorded as a specific category of
  #individuals?" 
  
  "how_injured_wl_recorded",        #25."How are injured wildlife are recorded
  #during a patrol:" 
  
  "injured_wl_data_recorded",       #26. "Type of data recorded from injured
  #wildlife
  
  "injured_wl_data_in_smart",       #27. Are these data entered and stored in 
  #SMART Desktop?"                 
  
  "injured_wl_data_other",          #28. If none of the items are recorded in
  #the corresponding SMART Conservation Area
  #where are they recorded?"  
  
  "sick_wl_recorded",               #29.Are sick wildlife found during patrols
  #recorded as a specific category 
  #of individuals?
  
  "how_sick_wl_recorded",           #30.How are sick wildlife recorded during a
  #patrol"
  
  "sick_wl_data_recorded",          #31. Type of data recorded from sick 
  #wildlife 
  
  "sick_wl_data_in_smart",          #32. Are these data entered and stored in 
  #SMART Desktop?
  
  "sick_wl_data_other",             #33. If none of the items are recorded 
  #in the corresponding SMART Conservation 
  # Area where are they recorded?" 
  
  "dom_animals_in_pa",              #34. Domestic animals found in the PA
  #(free-ranging, captive, on a farm)?"
  
  "dom_animals_recorded",           #35. Presence of domestic animals recorded" 
  
  
  "dom_animal_in_smart",            #36. Is the presence of domestic animals 
  #data entered in SMART Desktop?"  
  
  "dom_animal_health_status_recorded", #37.Is the health status of the observed
  #domestic animals recorded"
  
  "dom_animal_health_status_smart", #38. "Are these data entered in SMART 
  #Desktop?" 
  
  "other_wl_health_data",           #39. Other data relevant to wildlife health
  #collected"                  
  
  "why_not_wl_health_data",         #40. "If no data are collected on dead, 
  #sick or injured wildlife, why"         
  
  "smart_version",                  #41. What version of SMART Desktop is 
  #currently used in the protected area?"          
  
  "smart_connect",                  #42. Is SMART Connect available to manage 
  #and transfer information between SMART 
  #Desktop and SMART Mobile?"                                                         
  
  
  "set_up_connect",                 #43. Are there plans to set up SMART Connect
  #instance and when?"
  
  "smart_fully_rolled_out",         #44. Is SMART fully rolled-out or 
  #is it being piloted?"  
  
  "add_health_atributes",           #45. Would you be interested in adding a set
  #of categories and attributes to your data 
  #model in order to facilitate the collection
  #of wildlife health data 
  #(morbidity/mortality findings and events)?"
  
  "position"                        #46. Please select the option that best 
  #describes your position"                     
)

colnames(dat)<-c(new.names)


# Removing the score column
dat_modified=dat %>% select(-score)