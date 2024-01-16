#   <!-- 23. Is SMART fully rolled-out in the protected area(s) you work in or is it being piloted? -->
#   <!-- 21. What version of SMART Desktop is currently used in the protected area? -->
#   <!-- 22. Is SMART Connect available to manage and transfer information between SMART Desktop and SMART Mobile?  -->


smart_use<-
  terrestrial_data |>
  filter(local==T) %>% 
  select(#survey, 
           # sick_wl_data_in_smart,
           # injured_wl_data_in_smart,
           # dead_wl_data_in_smart,
           protected_area,
           smart_fully_rolled_out,
           smart_version,
           smart_connect, 
           set_up_connect) |>
  select(protected_area,
         smart_fully_rolled_out,
         smart_version,
         smart_connect,
         set_up_connect)

# smart_use$smart_version

seven<-c(1,3,5,8,10,11,12,15,19, 21, 23, 24, 36,39,43,45,46,59,60)
six<-c(2,4,6,7,9,14,16,17,18,20,22, 25,28,29,30,31,32,33,34,35, 37,38,
       40,41,42,47,48,49,50,51,58,61,62,63,64,65,66,67,68,70, 71)
five<-c(27, 69, 73)
four<-c(52,53,54,55,56, 57)
unknown<-c(13,26,44, 57,72)


# all(
# tibble(
#   seven=if_else(1:nrow(smart_use)%in%seven, 1, 0),
#   six=if_else(1:nrow(smart_use)%in%six, 1, 0),
#   five=if_else(1:nrow(smart_use)%in%five, 1, 0),
#   four=if_else(1:nrow(smart_use)%in%four, 1, 0),
#   unknown=if_else(1:nrow(smart_use)%in%unknown, 1, 0)) %>%
#   rowSums() ==1 )

smart_use$smart_version[seven]<-7
smart_use$smart_version[six]<-6
smart_use$smart_version[five]<-5
smart_use$smart_version[four]<-4
smart_use$smart_version[unknown]<-"unknown"



smart_use_for_text<-
smart_use |>
  count(smart_fully_rolled_out)


smart_version_for_text<-
  smart_use |>
  count(smart_version)


smart_connect_for_text<-
  smart_use |>
  count(smart_connect)

smart_connect_set_up_for_text<-
  smart_use |>
  filter(smart_connect=="No" & grepl(pattern="Si |Si,|Yes|Yes.|Yes,  ", x= smart_use$set_up_connect, ignore.case=T)) |>
  nrow()
  

