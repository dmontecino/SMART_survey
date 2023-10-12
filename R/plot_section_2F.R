library(ggplot2)
library(camcorder)


dead<- 
  terrestrial_data %>%
  distinct(survey,
           dead_found,
           #sick_injured_found,
           dead_wl_recorded,
           #sick_wl_recorded,
           #injured_wl_recorded, 
           wildlife_health_important) %>% 
  dplyr::select(-survey) %>%
  dplyr::filter(dead_found!="Never" & dead_wl_recorded=="No") %>% 
  dplyr::count(wildlife_health_important)

dead$health_status<-"dead"

sick<-
  terrestrial_data %>%
  distinct(survey,
           #dead_found,
           sick_injured_found,
           #dead_wl_recorded,
           sick_wl_recorded,
           #injured_wl_recorded, 
           wildlife_health_important) %>% 
  dplyr::select(-survey) %>%
  dplyr::filter(sick_injured_found!="Never" & sick_wl_recorded=="No") %>% 
  dplyr::count(wildlife_health_important)

sick$health_status<-"sick"

injured<-  
  terrestrial_data %>%
  distinct(survey,
           #dead_found,
           sick_injured_found,
           #dead_wl_recorded,
           #sick_wl_recorded,
           injured_wl_recorded, 
           wildlife_health_important) %>% 
  dplyr::select(-survey) %>%
  dplyr::filter(sick_injured_found!="Never" & injured_wl_recorded=="No") %>% 
  dplyr::count(wildlife_health_important)

injured$health_status<-"injured"


section_2F<-bind_rows(dead,sick, injured)

section_2F<-
  section_2F %>% 
  rename("number_responses"=n) %>% 
  tidyr::complete(terrestrial_data %>% distinct(wildlife_health_important), 
                  health_status,
                  fill = list(number_responses=0)) %>% 
  mutate(wildlife_health_important=factor(wildlife_health_important,
                                          levels = rev(c("Strongly agree",
                                                         "Agree",
                                                         "Neutral",
                                                         "Somewhat disagree",
                                                         "Disagree",
                                                         "Strongly disagree")),
                                          labels = rev(c(
                                            "Strongly\nagree",
                                            "Agree",
                                            "Neutral",
                                            "Somewhat\ndisagree",
                                            "Disagree",
                                            "Strongly\ndisagree"))))


#strat recording plot development setting the reald width and height
# gg_record(
#   dir = file.path(tempdir(), "recording100"), # where to save the recording
#   device = "png", # device to use to save images
#   width = 7,      # width of saved image
#   height = 3.5,     # height of saved image
#   units = "in",   # units for width and height
#   dpi = 300       # dpi to use when saving image
# )

gg_resize_film(
  height = 2.2*1.33,
  width = 6.4,
  units = "in",
  dpi = 300
)


# plot
# create x labels for questions

plot_section_2F<-
  ggplot(data = section_2F %>%  
           # pivot_longer(cols=c("Yes", "No")) %>% 
           arrange(wildlife_health_important) %>% 
           mutate(health_status=factor(health_status, 
                              levels = c("dead", "sick", "injured"),
                              labels = c("Dead wildlife\nfound during\npatrols are\nnot documented",
                                         "Sick wildlife\nfound during\npatrols are\nnot documented",
                                         "Injured wildlife\nfound during\npatrols are\nnot documented")))) +
  likert_theme +
  theme(text = element_text(size = 10.5),
        plot.title = element_text(size = 10, hjust = 0.5, vjust = 3.5, face = "plain",
                                  margin = margin(1, 2, 1, 1)),
        plot.margin = unit(c(0.3,0,0.1,.4), "cm"),
        plot.background = element_rect(linewidth = 1, color = "black")) +
  geom_col(aes(health_status, number_responses, fill = wildlife_health_important)) +
  geom_text(aes(health_status, number_responses, label = number_responses), # add percentage
            # geom_col(aes(name, value, fill = dead_found)) +
            #   geom_text(aes(name, value, label = value), # add percentage
            position = position_stack(vjust = .5),
            fontface = "bold",
            size=2.6) + # ) + # center the label
  scale_fill_brewer(type = "div") +
  facet_wrap(.~wildlife_health_important, nrow = 1) +
  coord_flip() +
  ggtitle("Wildlife health is important")

plot_section_2F

# if(!file.exists("../SMART_survey/plots/plot_section_2A.png")) {
  
  ggsave("plots/plot_section_2F.png",
         plot = plot_section_2F,
         height = 2.2*1.33,
         width = 6.4,
         units = "in",
         dpi = 300)
  #}



