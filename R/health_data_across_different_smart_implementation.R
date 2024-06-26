sick<-terrestrial_data %>% 
  select(sick_wl_recorded, 
         smart_fully_rolled_out, 
         sick_wl_data_in_smart, 
         sick_wl_data_other) %>% 
  print(n=100)

sick %>% filter(sick_wl_recorded=="No") %>%   print(n=100)

sick %>% filter(sick_wl_recorded=="Yes") %>%   print(n=100)

sick %>% 
  filter(sick_wl_recorded=="Yes") %>% 
  filter(sick_wl_data_in_smart == "None of these items are recorded in the corresponding SMART Conservation Area")

sick %>% 
  filter(sick_wl_recorded=="Yes") %>% 
  filter(sick_wl_data_in_smart != "None of these items are recorded in the corresponding SMART Conservation Area") %>% 
  print(n=100)


injured<-terrestrial_data %>% 
  select(injured_wl_recorded, 
         smart_fully_rolled_out, 
         injured_wl_data_in_smart, 
         injured_wl_data_other) %>% 
  print(n=100)

injured %>% filter(injured_wl_recorded=="No") %>%   print(n=100)

injured %>% filter(injured_wl_recorded=="Yes") %>%   print(n=100)

injured %>% 
  filter(injured_wl_recorded=="Yes") %>% 
  filter(injured_wl_data_in_smart == "None of these items are recorded in the corresponding SMART Conservation Area")

injured %>% 
  filter(injured_wl_recorded=="Yes") %>% 
  filter(injured_wl_data_in_smart != "None of these items are recorded in the corresponding SMART Conservation Area") %>% 
  print(n=100)





dead<-terrestrial_data %>% 
  select(dead_wl_recorded, 
         smart_fully_rolled_out, 
         dead_wl_data_in_smart, 
         dead_wl_data_other) %>% 
  print(n=100)

dead %>% filter(dead_wl_recorded=="No") %>%   print(n=100)

dead %>% filter(dead_wl_recorded=="Yes") %>%   print(n=100)

dead %>% 
  filter(dead_wl_recorded=="Yes") %>% 
  filter(dead_wl_data_in_smart == "None of these items are recorded in the corresponding SMART Conservation Area")

dead %>% 
  filter(dead_wl_recorded=="Yes") %>% 
  filter(dead_wl_data_in_smart != "None of these items are recorded in the corresponding SMART Conservation Area") %>% 
  print(n=100)




terrestrial_data %>% 
  filter(injured_wl_data_in_smart == "None of these items are recorded in the corresponding SMART Conservation Area" |
         sick_wl_data_in_smart == "None of these items are recorded in the corresponding SMART Conservation Area" |  
         dead_wl_data_in_smart == "None of these items are recorded in the corresponding SMART Conservation Area") %>% 
  filter(dead_wl_recorded=="Yes" | 
         sick_wl_recorded=="Yes" |    
         injured_wl_recorded=="Yes") %>% 
  select(smart_fully_rolled_out, 
         sick_wl_data_in_smart, 
         sick_wl_data_other,
         injured_wl_data_in_smart, 
         injured_wl_data_other, 
         dead_wl_data_in_smart, 
         dead_wl_data_other) %>% 
  print(n=100) %>% 
  count(smart_fully_rolled_out)




