library(tidyverse)
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


section_2B <-
  terrestrial_data %>%
  distinct(survey,
           #dead_found,
           sick_injured_found,
           #dead_wl_recorded,
           sick_wl_recorded,
           # injured_wl_recorded)
  )

section_2B<-
  section_2B %>%
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
gg_record(
  dir = file.path(tempdir(), "recording100"), # where to save the recording
  device = "png", # device to use to save images
  width = 7,      # width of saved image
  height = 3.5,     # height of saved image
  units = "in",   # units for width and height
  dpi = 300       # dpi to use when saving image
)

gg_resize_film(
  height = 2.2,
  width = 6.4,
  units = "in",
  dpi = 300
)


# plot
# create x labels for questions

plot_section_2B<-
  ggplot(data = section_2B %>%  
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

 plot_section_2B

# if(!file.exists("../SMART_survey/plots/plot_section_2A.png")) {

ggsave("../SMART_survey/plots/plot_section_2B.png",
       plot = plot_section_2B,
       height = 2.2,
       width = 6.4,
       units = "in",
       dpi = 300)
#}



