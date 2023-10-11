library(ggpattern)
library(ggrepel)

dom_animals<-
  terrestrial_data %>%
  distinct(survey, 
           dom_animals_concern,
           dom_animals_in_pa,
           dom_animals_recorded,
           dom_animal_health_status_recorded) %>% 
  filter(dom_animals_in_pa=="Yes") %>% 
  select(-survey, -dom_animals_in_pa) %>% 
  dplyr::count(dom_animals_concern, 
               dom_animals_recorded, 
               dom_animal_health_status_recorded) %>% 
  tidyr::pivot_wider(names_from = dom_animal_health_status_recorded, 
                     names_prefix = "health_status_recorded_",
                     values_from = n, 
                     values_fill = 0) %>%
  tibble::add_row(dom_animals_concern = rep("Strongly Disagree", 2),
                 dom_animals_recorded = c("No","Yes"),
                 health_status_recorded_No=c(0,0),
                 health_status_recorded_Yes=c(0,0)) %>% 
  tidyr::complete(dom_animals_concern, 
                  dom_animals_recorded) %>% 
  tidyr::replace_na(list(health_status_recorded_No=0,
                         health_status_recorded_Yes=0)) %>% 
  rowwise(dom_animals_concern, dom_animals_recorded) %>% 
  mutate(dom_animals_recorded_total=sum(c(health_status_recorded_No,
                                          health_status_recorded_Yes))) %>% 
  mutate(health_status_recorded_Yes_perc=health_status_recorded_Yes/
                                         dom_animals_recorded_total*100) %>% 
  select(dom_animals_concern,
         dom_animals_recorded, 
         dom_animals_recorded_total,
         health_status_recorded_No,
         health_status_recorded_Yes,
         health_status_recorded_Yes_perc)



section_3<-dom_animals  %>% 
  mutate(dom_animals_concern=factor(dom_animals_concern,
                                    levels = rev(c("Strongly agree",
                                              "Agree",
                                              "Neutral",
                                              "Somewhat Disagree",
                                              "Disagree",
                                              "Strongly Disagree")),
                                    labels = rev(c("Strongly\nagree",
                                                "Agree",
                                                "Neutral",
                                                "Somewhat\ndisagree",
                                                "Disagree",
                                                "Strongly\ndisagree"))))


gg_resize_film(
  height = 2.2,
  width = 6.4/7*6,
  units = "in",
  dpi = 300
)

section_3<-section_3 %>% 
  pivot_longer(cols=c("health_status_recorded_No",
                      "health_status_recorded_Yes")) %>% 
  select(-health_status_recorded_Yes_perc,
         dom_animals_concern,
         name) %>% 
  mutate(percent=value/dom_animals_recorded_total) 
  


plot_section_3<-
  ggplot(data = section_3 %>% 
         mutate(dom_animals_recorded=factor(dom_animals_recorded, 
                            levels = c("Yes", "No"),
                            labels = c("Domestic animals\nfound during\npatrols are\ndocumented",
                                       "Domestic animals\nfound during\npatrols are not\ndocumented")))) +
  likert_theme +
  theme(text = element_text(size = 10.5),
        plot.title = element_text(size = 10, hjust = 0.5, vjust = 3.5, face = "plain",
                                  margin = margin(1, 2, 1, 1)),
        plot.margin = unit(c(0.3,0,0.1,.4), "cm"),
        plot.background = element_rect(linewidth = 1, color = "black")) +
  geom_col_pattern(aes(dom_animals_recorded, 
               value, 
               fill = dom_animals_concern, 
               #alpha=percent
               ), color="black", linewidth=0, 
               pattern = dplyr::if_else(section_3$name=="health_status_recorded_Yes", "none", "stripe"),
               pattern_density = 0.01, 
               pattern_size=0.3, 
               pattern_color="grey50") +
  geom_text(aes(dom_animals_recorded, 
                value, 
                label=if_else(value==0, NA, value)), # add percentage
            position = position_stack(vjust = .5),
            fontface = "bold", color="black",
            size=2.6) + # ) + # center the label
  scale_fill_brewer(type = "div") +
  facet_wrap(.~dom_animals_concern, nrow = 1) +
  coord_flip() +
  ggtitle("Domestic animals are a concern for conservation goals")

plot_section_3

ggsave("plots/plot_section_3.png",
       plot = plot_section_3,
       height = 2.2,
       width = 6.4/7*6,
       units = "in",
       dpi = 300)


