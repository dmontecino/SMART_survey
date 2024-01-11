

#> Check the position of the respondents. This refers to who are the respondents
#> "managers" or "other".

terrestrial_data %>% dplyr::count(position)


# check the answers from "others" in position

#temp<-terrestrial_data %>% filter(position=="Other")

#write.csv(temp %>% unnest(protected_area), "data/check_data_position_is_other.csv")


# nrow(terrestrial_data)  #113
# nrow(temp)  #27


#> having checked the answers manually, I think removing these answers from "others"
#> is consistent with out methodology and our intended audience

terrestrial_data<-terrestrial_data %>% filter(position!="Other")

# nrow(temp)  86

