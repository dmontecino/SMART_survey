# <!-- Are these data entered and stored in SMART Desktop? -->


num_responses_dom_animals_in_pa_are_recorded_in_smart<-
  terrestrial_data |> 
  distinct(survey, 
           dom_animals_in_pa, 
           dom_animals_recorded, 
           dom_animal_in_smart) |>
  filter(dom_animals_in_pa=="Yes" & dom_animals_recorded=="Yes") |> 
  count(dom_animals_recorded, 
        dom_animal_in_smart) %>% 
  filter(dom_animal_in_smart=="Yes") %>% 
  pull(n)


#domestic animal health status in smart

dom_animal_health_status_in_smart<-
terrestrial_data %>% 
  distinct(survey, dom_animals_in_pa, 
           dom_animals_recorded, 
           dom_animal_health_status_recorded, 
           dom_animal_health_status_smart) |> 
  select(-survey) |>
  filter(dom_animals_in_pa=="Yes" &
           dom_animals_recorded=="Yes" & 
           dom_animal_health_status_recorded=="Yes") %>% 
  count(dom_animal_health_status_recorded, 
        dom_animal_health_status_smart) %>% 
  filter(dom_animal_health_status_smart=="Yes") %>% 
  pull(n)




 
 