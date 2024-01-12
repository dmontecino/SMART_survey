data_from_dead_wl<-
terrestrial_data %>%
  filter(local==T) %>% 
  select(#survey, 
           dead_wl_recorded,
           dead_wl_data_recorded) %>% 
  filter(dead_wl_recorded=="Yes") %>% 
  tidyr::separate_longer_delim(dead_wl_data_recorded, ", ") %>% 
  dplyr::count(dead_wl_data_recorded, name = "dead") %>% 
  rename("items"="dead_wl_data_recorded")

data_from_dead_wl$items[data_from_dead_wl$items=="Anomalies in carcass (if any)"]<-"Anomalies"
data_from_dead_wl$items[data_from_dead_wl$items=="Carcass condition"]<-"Condition"
data_from_dead_wl$items[data_from_dead_wl$items=="Suspect cause of death"]<-"Suspect Cause"


data_from_sick_wl<-
  terrestrial_data %>%
  filter(local==T) %>% 
  select(#survey, 
           sick_wl_recorded,
           sick_wl_data_recorded) %>% 
  filter(sick_wl_recorded=="Yes") %>% 
  tidyr::separate_longer_delim(sick_wl_data_recorded, ", ") %>% 
  dplyr::count(sick_wl_data_recorded, name = "sick") %>% 
  rename("items"="sick_wl_data_recorded")

data_from_sick_wl$items[data_from_sick_wl$items=="Anomalies/signs if any"]<-"Anomalies"
data_from_sick_wl$items[data_from_sick_wl$items=="Body condition"]<-"Condition"
data_from_sick_wl$items[data_from_sick_wl$items=="Suspect cause of disease"]<-"Suspect Cause"




data_from_inj_wl<-
  terrestrial_data %>%
  filter(local==T) %>% 
  select(#survey, 
           injured_wl_recorded,
           injured_wl_data_recorded) %>% 
  filter(injured_wl_recorded=="Yes") %>% 
  tidyr::separate_longer_delim(injured_wl_data_recorded, ", ") %>% 
  dplyr::count(injured_wl_data_recorded, name = "injured") %>% 
  rename("items"="injured_wl_data_recorded")

data_from_inj_wl$items[data_from_inj_wl$items=="Anomalies/signs if any"]<-"Anomalies"
data_from_inj_wl$items[data_from_inj_wl$items=="Body condition"]<-"Condition"
data_from_inj_wl$items[data_from_inj_wl$items=="Suspect cause of injury"]<-"Suspect Cause"

what_is_collected<-
dplyr::full_join(data_from_dead_wl, 
                 data_from_sick_wl) %>% 
                 dplyr::full_join(data_from_inj_wl) %>% 
  # tidyr::replace_na(list(sick = 0, injured = 0)) %>% 
  mutate(dead=dead/terrestrial_data %>%
           # distinct(survey, 
           #          dead_wl_recorded) %>% 
           filter(dead_wl_recorded=="Yes") %>% nrow() *100) %>%
  mutate(sick=sick/terrestrial_data %>%
           # distinct(survey, 
           #          sick_wl_recorded) %>% 
           filter(sick_wl_recorded=="Yes") %>% nrow *100) %>%
  mutate(injured=injured/terrestrial_data %>%
           # distinct(survey, 
           #          injured_wl_recorded) %>% 
           filter(injured_wl_recorded=="Yes") %>% nrow *100)


# convert to long format
temp_long <- pivot_longer(what_is_collected, cols = 2:4, names_to = "class", values_to = "percentage")


# create plot
temp_long$class <- as.factor(temp_long$class)


what_is_recorded=
  ggplot(temp_long, aes(x = class, y = items, size = percentage, fill = class)) +
  geom_point(shape = 21, stroke=0.2) +
  geom_label(aes(label = round(percentage,1)), 
             size = 2.6, 
             fontface = "bold",
             # vjust = 0.4, 
             fill="white", 
             label.padding = unit(0.1, "lines")) +
  scale_size(range = c(5, 28), name = "Response count") +
  scale_y_discrete(name = "") + #,
  #                  labels = c("Recorded in\nanother way",
  #                             "Each animal is an\nindividual\nobservation",
  #                             
  #                             "Part of\nthe full count\nof the corresponding\nspecies",
  #                             "Present/absent"
  #                  )) +
  
  scale_x_discrete(limits = c("sick",
                              "injured",
                              "dead"),
                   position = "top",
                   name = "",
                   labels = c(
                              "Sick\nwildlife",
                              "Injured\nwildlife",
                              "Dead\nwildlife")) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    strip.background  = element_blank(),  
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size=8, colour = "black"),
    axis.text.y = element_text(size =8, colour = "black"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(linewidth = 1, color = "black")
  ) +
  scale_fill_manual(values = c("#D7191C", "#ABDDA4", "#3288BD")) +
  guides(size = "none", fill = "none")


# library(scales)
# show_col(brewer.pal(11,"Spectral"))

gg_resize_film(
  height = 3.6*2,
  width = 5/5*4,
  units = "in",
  dpi = 300
)

#what_is_recorded

ggsave("plots/plot_section_2E.png",
       plot = what_is_recorded,
       height = 3.6*2,
       width = 5/5*4,
       units = "in",
       dpi = 600)
2
