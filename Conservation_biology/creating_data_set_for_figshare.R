terrestrial_data<-readRDS("data/terrestrial_data.RDS")

terrestrial_data <- terrestrial_data |> 
  dplyr::select(-protected_area)

write.csv(terrestrial_data, "v2/terrestrial_data_no_pa_or_country_data.csv")

