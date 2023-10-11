dat_2021<-read.csv("AIV_data/world_chickens_fao/FAOSTAT_data_en_9-28-2023.csv")


unit<-map_vec(strsplit(dat_2021$Unit, " "), \(x) x[1]) %>% as.numeric()
value<-dat_2021$Value

sum(value*unit)


document %>% 
  xml_find_all(xpath = "/html/body/div[1]/div[1]/form/div[1]/div[4]/div/div[5]/div[2]/div[1]/div")



#/html/body/div[1]/div[1]/form/div[1]/div[4]/div/div[5]/div[2]/div[1]/div/div/pre/span[1]/text()[1]