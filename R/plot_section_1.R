library(ggplot2)
library(camcorder)

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


section_1 =
  terrestrial_data %>%
  distinct(survey,
           wildlife_health_important,
           hum_liv_path_affect_wildlife,
           wildlife_path_affect_livestock,
           wildlife_path_affect_phealth)


section_1<-
  section_1 %>%
  dplyr::select(-survey) %>%
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



#strat recording plot development setting the reald width and height
# gg_record(
#   dir = file.path(tempdir(), "recording100"), # where to save the recording
#   device = "png", # device to use to save images
#   width = 7,      # width of saved image
#   height = 3.5,     # height of saved image
#   units = "in",   # units for width and height
#   dpi = 300       # dpi to use when saving image
# )
# 
gg_resize_film(
  height = 3.6,
  width = 6.4,
  units = "in",
  dpi = 300
)


# plot
# create x labels for questions

plot_section_1<-
  ggplot(data = section_1 %>% arrange(rank)) +
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

 # plot_section_1

# if(!file.exists("../SMART_survey/plots/plot_section_1.png")) {
 
  ggsave("plots/plot_section_1.png",
         plot = plot_section_1,
         height = 3.6,
         width = 6.4,
         units = "in",
         dpi = 300)
  # }

  
  
  