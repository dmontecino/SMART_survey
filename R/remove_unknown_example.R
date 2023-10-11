# ---------------------------------------------------------------------- #
#  Remove the rows where the PAs are unknown and also remove the example #
# ---------------------------------------------------------------------- #

dat_modified_filtered <- dat_modified_filtered %>% 
  filter(protected_area!="ejemplo")

dat_modified_filtered_1 <- dat_modified_filtered %>% 
  filter(protected_area!="Unknown protected area")


# nrow(dat_modified_filtered_1)
