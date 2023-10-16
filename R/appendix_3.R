appendix_3A <-
  terrestrial_data %>%
  distinct(survey,
           wildlife_health_important,
           hum_liv_path_affect_wildlife,
           wildlife_path_affect_livestock,
           wildlife_path_affect_phealth,
           country) %>% 
  filter(country!="Peru")


appendix_3A<-
  appendix_3A %>%
  dplyr::select(-survey, -country) %>%
  tidyr::pivot_longer(cols = everything()) %>%
  dplyr::group_by(name, value, .drop=FALSE) %>%
  dplyr::count(name,value) %>%
  dplyr::rename(c("question"="name", "rank"="value", "num_responses"="n")) %>%
  ungroup() %>%
  complete(question,rank,fill=list(num_responses=0)) %>%
  mutate(rank=factor(rank,
                     levels = rev(c("Strongly agree",
                                    "Agree",
                                    "Neutral",
                                    "Somewhat Disagree",
                                    "Disagree",
                                    "Strongly disagree")),
                     labels = rev(c("Strongly\nagree",
                                    "Agree",
                                    "Neutral",
                                    "Somewhat\ndisagree",
                                    "Disagree",
                                    "Strongly\ndisagree")))) %>%
  mutate(question=factor(question,
                         levels = rev(c("wildlife_health_important",
                                        "hum_liv_path_affect_wildlife",
                                        "wildlife_path_affect_phealth",
                                        "wildlife_path_affect_livestock")),
                         labels = rev(c("Wildlife health\nis important",
                                        "Human and livestock\npathogens can\nimpact wildlife\nhealth",
                                        "Wildlife pathogens can\nimpact human health",
                                        "Wildlife pathogens can\nimpact livestock health"))))


likert_theme <-
  theme_gray() +
  theme(text = element_text(size = 60),
        plot.title = element_text(size = 60, face = "bold",
                                  margin = margin(10, 0, 10, 0)),
        plot.margin = unit(c(2.4,0,2.4,.4), "cm"),
        plot.background = element_rect(linewidth = 5, color = "black"),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        strip.background  = element_blank(),
        legend.position = "none")

plot_appendix_3A<-
ggplot(data = appendix_3A %>% arrange(rank)) +
  likert_theme +
  theme(text = element_text(size = 10.5),
        plot.title = element_text(size = 10, hjust = 0.5, vjust = 3.5, face = "plain",
                                  margin = margin(1, 2, 1, 1)),
        plot.margin = unit(c(0.3,0,0.1,.4), "cm"),
        plot.background = element_rect(linewidth = 1, color = "black")) +
  geom_col(aes(question, num_responses, fill = rank)) +
  geom_text(aes(question, num_responses, label = num_responses), # add percentage
            position = position_stack(vjust = .5),
            fontface = "bold",
            size=2.6) + # ) + # center the label
  scale_fill_brewer(type = "div") +
  facet_wrap(.~rank, nrow = 1) +
  coord_flip()+
  ggtitle("Agreement ranking")



ggsave("plots/appendix_3A.png",
       plot = plot_appendix_3A,
       height = 3.6,
       width = 6.4,
       units = "in",
       dpi = 300)


###------------------------------------ ##

appendix_3B <-
  terrestrial_data %>%
  distinct(survey,
           dead_found,
           # sick_injured_found,
           dead_wl_recorded,
           # sick_wl_recorded,
           # injured_wl_recorded)
           country) %>% 
  filter(country!="Peru")


appendix_3B<-
  appendix_3B %>%
  dplyr::select(-survey) %>%
  dplyr::count(dead_found,dead_wl_recorded) %>% 
  tidyr::pivot_wider(names_from = dead_wl_recorded,
                     values_from = "n", 
                     values_fill = 0) %>% 
   mutate(dead_found=factor(dead_found,
                     levels = rev(c("Always",
                                "Very frequently",
                                 "Occasionally",
                                "Sometimes",
                                "Rarely",
                                "Very rarely",
                                "Never")),
                     labels = rev(c("Always",
                                "Very\nfrequently",
                                "Occasionally",
                                "Sometimes",
                                "Rarely",
                                "Very\nrarely",
                                "Never"))))
 


#strat recording plot development setting the reald width and height
# gg_record(
#   dir = file.path(tempdir(), "recording100"), # where to save the recording
#   device = "png", # device to use to save images
#   width = 7,      # width of saved image
#   height = 3.5,     # height of saved image
#   units = "in",   # units for width and height
#   dpi = 300       # dpi to use when saving image
# )

# gg_resize_film(
#   height = 2.2,
#   width = 6.4,
#   units = "in",
#   dpi = 300
# )


# plot
# create x labels for questions

plot_appendix_3B<-
  ggplot(data = appendix_3B %>%  
           pivot_longer(cols=c("Yes", "No")) %>% 
           arrange(dead_found) %>% 
           mutate(name=factor(name, 
                              levels = c("Yes", "No"),
                              labels = c("Dead wildlife\nfound during\npatrols are\ndocumented",
                                         "Dead wildlife\nfound during\npatrols are not\ndocumented"))))+
  likert_theme +
  theme(text = element_text(size = 10.5),
        plot.title = element_text(size = 10, hjust = 0.5, vjust = 3.5, face = "plain",
                                  margin = margin(1, 2, 1, 1)),
        plot.margin = unit(c(0.3,0,0.1,.4), "cm"),
        plot.background = element_rect(linewidth = 1, color = "black")) +
  geom_col(aes(name, value, fill = dead_found)) +
  geom_text(aes(name, value, label = value), # add percentage
            position = position_stack(vjust = .5),
            fontface = "bold",
            size=2.6) + # ) + # center the label
  scale_fill_brewer(type = "div") +
  facet_wrap(.~dead_found, nrow = 1) +
  coord_flip() +
  ggtitle("Overall dead wildlife encountering")

 # plot_section_2A

# if(!file.exists("../SMART_survey/plots/plot_section_2A.png")) {
  
  ggsave("plots/appendix_3B.png",
         plot = plot_appendix_3B,
         height = 2.2,
         width = 6.4,
         units = "in",
         dpi = 300)
  #}


###------------------------------------ ##

  
  appendix_3C <-
    terrestrial_data %>%
    distinct(survey,
             #dead_found,
              sick_injured_found,
             #dead_wl_recorded,
              sick_wl_recorded,
             # injured_wl_recorded)
             country) %>% 
    filter(country!="Peru")
  
  
  appendix_3C<-
    appendix_3C %>%
    dplyr::select(-survey) %>%
    dplyr::count(sick_injured_found,sick_wl_recorded) %>% 
    tidyr::pivot_wider(names_from = sick_wl_recorded,
                       values_from = "n", 
                       values_fill = 0) %>% 
    mutate(sick_injured_found=factor(sick_injured_found,
                             levels = rev(c("Always",
                                            "Very frequently",
                                            "Occasionally",
                                            "Sometimes",
                                            "Rarely",
                                            "Very rarely",
                                            "Never")),
                             labels = rev(c("Always",
                                            "Very\nfrequently",
                                            "Occasionally",
                                            "Sometimes",
                                            "Rarely",
                                            "Very\nrarely",
                                            "Never"))))
  
  
  
  #strat recording plot development setting the reald width and height
  # gg_record(
  #   dir = file.path(tempdir(), "recording100"), # where to save the recording
  #   device = "png", # device to use to save images
  #   width = 7,      # width of saved image
  #   height = 3.5,     # height of saved image
  #   units = "in",   # units for width and height
  #   dpi = 300       # dpi to use when saving image
  # )
  
  # gg_resize_film(
  #   height = 2.2,
  #   width = 6.4,
  #   units = "in",
  #   dpi = 300
  # )
  
  
  # plot
  # create x labels for questions
  
  appendix_3C<-
    ggplot(data = appendix_3C %>%  
             pivot_longer(cols=c("Yes", "No")) %>% 
             arrange(sick_injured_found) %>% 
             mutate(name=factor(name, 
                                levels = c("Yes", "No"),
                                labels = c("Sick wildlife\nfound during\npatrols are\ndocumented",
                                           "Sick wildlife\nfound during\npatrols are not\ndocumented"))))+
    likert_theme +
    theme(text = element_text(size = 10.5),
          plot.title = element_text(size = 10, hjust = 0.5, vjust = 3.5, face = "plain",
                                    margin = margin(1, 2, 1, 1)),
          plot.margin = unit(c(0.3,0,0.1,.4), "cm"),
          plot.background = element_rect(linewidth = 1, color = "black")) +
    geom_col(aes(name, value, fill = sick_injured_found)) +
    geom_text(aes(name, value, label = value), # add percentage
              position = position_stack(vjust = .5),
              fontface = "bold",
              size=2.6) + # ) + # center the label
    scale_fill_brewer(type = "div") +
    facet_wrap(.~sick_injured_found, nrow = 1) +
    coord_flip() +
    ggtitle("Overall sick and injured wildlife encountering")
  
  # plot_section_2A
  
  # if(!file.exists("../SMART_survey/plots/plot_section_2A.png")) {
  
  ggsave("plots/appendix_3C.png",
         plot = appendix_3C,
         height = 2.2,
         width = 6.4,
         units = "in",
         dpi = 300)
  
  
  ###------------------------------------ ##
  
  
  appendix_3D <-
    terrestrial_data %>%
    distinct(survey,
             #dead_found,
             sick_injured_found,
             #dead_wl_recorded,
             #sick_wl_recorded,
             injured_wl_recorded,
             country) %>% 
    filter(country!="Peru")
  
  
  appendix_3D<-
    appendix_3D %>%
    dplyr::select(-survey) %>%
    dplyr::count(sick_injured_found,injured_wl_recorded) %>% 
    tidyr::pivot_wider(names_from = injured_wl_recorded,
                       values_from = "n", 
                       values_fill = 0) %>% 
    mutate(sick_injured_found=factor(sick_injured_found,
                             levels = rev(c("Always",
                                            "Very frequently",
                                            "Occasionally",
                                            "Sometimes",
                                            "Rarely",
                                            "Very rarely",
                                            "Never")),
                             labels = rev(c("Always",
                                            "Very\nfrequently",
                                            "Occasionally",
                                            "Sometimes",
                                            "Rarely",
                                            "Very\nrarely",
                                            "Never"))))
  
  
  
  #strat recording plot development setting the reald width and height
  # gg_record(
  #   dir = file.path(tempdir(), "recording100"), # where to save the recording
  #   device = "png", # device to use to save images
  #   width = 7,      # width of saved image
  #   height = 3.5,     # height of saved image
  #   units = "in",   # units for width and height
  #   dpi = 300       # dpi to use when saving image
  # )
  
  # gg_resize_film(
  #   height = 2.2,
  #   width = 6.4,
  #   units = "in",
  #   dpi = 300
  # )
  
  
  # plot
  # create x labels for questions
  
  appendix_3D<-
    ggplot(data = appendix_3D %>%  
             pivot_longer(cols=c("Yes", "No")) %>% 
             arrange(sick_injured_found) %>% 
             mutate(name=factor(name, 
                                levels = c("Yes", "No"),
                                labels = c("Injured wildlife\nfound during\npatrols are\ndocumented",
                                           "Injured wildlife\nfound during\npatrols are not\ndocumented"))))+
    likert_theme +
    theme(text = element_text(size = 10.5),
          plot.title = element_text(size = 10, hjust = 0.5, vjust = 3.5, face = "plain",
                                    margin = margin(1, 2, 1, 1)),
          plot.margin = unit(c(0.3,0,0.1,.4), "cm"),
          plot.background = element_rect(linewidth = 1, color = "black")) +
    geom_col(aes(name, value, fill = sick_injured_found)) +
    geom_text(aes(name, value, label = value), # add percentage
              position = position_stack(vjust = .5),
              fontface = "bold",
              size=2.6) + # ) + # center the label
    scale_fill_brewer(type = "div") +
    facet_wrap(.~sick_injured_found, nrow = 1) +
    coord_flip() +
    ggtitle("Overall sick and injured wildlife encountering")
  
  # plot_section_2A
  
  # if(!file.exists("../SMART_survey/plots/plot_section_2A.png")) {
  
  ggsave("plots/appendix_3D.png",
         plot = appendix_3D,
         height = 2.2,
         width = 6.4,
         units = "in",
         dpi = 300)



