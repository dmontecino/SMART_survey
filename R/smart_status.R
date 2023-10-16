#   <!-- 23. Is SMART fully rolled-out in the protected area(s) you work in or is it being piloted? -->
#   <!-- 21. What version of SMART Desktop is currently used in the protected area? -->
#   <!-- 22. Is SMART Connect available to manage and transfer information between SMART Desktop and SMART Mobile?  -->


smart_use<-
  terrestrial_data |>
  distinct(survey, 
           # sick_wl_data_in_smart,
           # injured_wl_data_in_smart,
           # dead_wl_data_in_smart,
           protected_area,
           country,
           smart_fully_rolled_out,
           smart_version,
           smart_connect, 
           set_up_connect) |>
  select(protected_area,
         country,
         smart_fully_rolled_out,
         smart_version,
         smart_connect,
         set_up_connect)

# smart_use$smart_version

seven<-c(1,3,5,8,10,11,12,15,18, 20, 22, 23, 24,35,38,42,44,45,59)
six<-c(2,4,6,7,9,14,16,17,19,21,27,28,29,30,31,32,33,34,36,37,39,40,41,46,47,48,49,50,58,60,61,62,63,64,65,66,67,69,70)
five<-c(26,68)
four<-c(51,52,53,54,55,56)
unknown<-c(13, 25,43, 57,71)


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


# smart_use |>
#   filter(smart_connect=="Yes") |>
#   filter(smart_version%in%c(4, 5))

# data.frame(
# smart_use |>
#   filter(country=="Peru") |>
#   select(protected_area, smart_version) |>
#   filter()
  #   filter(smart_connect=="Yes") |>
  #   filter(smart_version%in%c(4, 5))


# status, version and connect
smart_use |> 
  count(smart_fully_rolled_out, 
        smart_version, 
        smart_connect) |>
  pivot_wider(names_from = smart_connect, values_from = n, values_fill = 0) |>
  arrange(smart_fully_rolled_out, 
          smart_version)


# subset no connect, future plans

connect_future_plans<-
  data.frame(smart_use |> 
               filter(smart_connect=="No") |>
               distinct(smart_fully_rolled_out, 
                        smart_version, 
                        set_up_connect) |>
               arrange(smart_fully_rolled_out, 
                       smart_version)) |>
  mutate(smart_connect_plan_bin=if_else(grepl(pattern="no", 
                                              x=set_up_connect, 
                                              ignore.case=T), "no", "yes")) |>
  select(set_up_connect,
         smart_connect_plan_bin)



connect_future_plans$smart_connect_plan_bin[connect_future_plans$set_up_connect=="Si, pero aún no se tiene fecha" |
                                              connect_future_plans$set_up_connect=="Si hay planes, pero primero tenemos que instalar internet en los PVC. Para ello se requiere presupuesto que el ANP no cuenta." | 
                                              connect_future_plans$set_up_connect=="Si pero no se tiene fecha" |
                                              connect_future_plans$set_up_connect=="When we have the appropriate equipment like good internet connection on smartphone in the forest." |
                                              connect_future_plans$set_up_connect=="desconozco"]<-"unknown"

connect_future_plans$smart_connect_plan_bin[connect_future_plans$set_up_connect=="Cuando se socialice dicha versión y sea compatible para iOS y Android." |
                                              connect_future_plans$set_up_connect=="Would need assistance"]<- "no"






