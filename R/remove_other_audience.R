

#> Check the position of the respondents. This refers to who are the respondents
#> "managers" or "other".

dat_modified_filtered_3 %>% distinct(survey, position) %>% dplyr::count(position)


# check the answers from "others" in position

temp<-dat_modified_filtered_3 %>% filter(position=="Other")

#write.csv(temp %>% unnest(protected_area), "data/check_data_position_is_other.csv")


# nrow(dat_modified_filtered_3)  #91
# nrow(temp)  #16


#> having checked the answers manually, I think removing these answers from "others"
#> is consistent with out methodology and our intended audience

dat_modified_filtered_4<-dat_modified_filtered_3 %>% filter(position!="Other")

# nrow(dat_modified_filtered_4)  #75

