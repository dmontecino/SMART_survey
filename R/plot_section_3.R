library(ggpattern)
library(ggrepel)

dom_animals<-
  terrestrial_data %>%
  filter(local==T) %>% 
  select(
           dom_animals_concern,
           dom_animals_in_pa,
           dom_animals_recorded,
           dom_animal_health_status_recorded) %>% 
  filter(dom_animals_in_pa=="Yes") %>% 
  select(-dom_animals_in_pa) %>% 
  dplyr::count(dom_animals_concern, 
               dom_animals_recorded) %>% 
  # tidyr::pivot_wider(names_from = dom_animal_health_status_recorded, 
  #                    names_prefix = "health_status_recorded_",
  #                    values_from = n, 
  #                    values_fill = 0) %>%
  tibble::add_row(dom_animals_concern = rep("Strongly Disagree", 2),
                  dom_animals_recorded = c("Yes", "No"),
                  n = rep(0,2)) %>% 
  #                 dom_animals_concern = rep("Strongly Disagree", 2),
  #                #dom_animals_recorded = c("No","Yes")#,
  #                # health_status_recorded_No=c(0,0),
  #                # health_status_recorded_Yes=c(0,0)
  #                ) %>% 
  tidyr::complete(dom_animals_concern, 
                  dom_animals_recorded) %>% 
  
  tidyr::replace_na(list(n=0)) #%>% 

#dom_animals

# tidyr::replace_na(list(health_status_recorded_No=0,
  #                        health_status_recorded_Yes=0)) %>% 
  #rowwise(dom_animals_concern, dom_animals_recorded) %>% 
  # mutate(dom_animals_recorded_total=sum(c(health_status_recorded_No,
  #                                         health_status_recorded_Yes))) %>% 
  #mutate(health_status_recorded_Yes_perc=health_status_recorded_Yes/
                                         #dom_animals_recorded_total*100) %>% 
  # select(dom_animals_concern,
  #        dom_animals_recorded, 
  #        dom_animals_recorded_total,
  #        health_status_recorded_No,
  #        health_status_recorded_Yes,
  #        health_status_recorded_Yes_perc)



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


# gg_resize_film(
#   height = 2.2,
#   width = 6.4/7*6,
#   units = "in",
#   dpi = 300
# )










# section_3<-section_3 %>% 
#   pivot_longer(cols=c("health_status_recorded_No",
#                       "health_status_recorded_Yes")) %>% 
#   select(dom_animals_recorded_total,
#          dom_animals_concern,
#          name)# %>% 
  #mutate(percent=value/dom_animals_recorded_total) 
  


# section_3 <- section_3 %>%  
#   distinct(dom_animals_concern,
#            dom_animals_recorded, 
#            dom_animals_recorded_total) 
  

#labels all dead per freq encountering 

labels<-
  section_3 %>% 
  group_by(dom_animals_concern) %>%
  mutate(num = sum(n)) %>%
  distinct(dom_animals_concern, num) %>% 
  ungroup() %>% 
  pull(num)

position.y<-c(rbind(labels/2, rep(NA, 6)))
position.y[11]<-NA

plot_section_3<-
  ggplot(data = section_3) +
  
  likert_theme +
  
  theme(text = element_text(size = 10.5),
        plot.title = element_text(size = 10, hjust = 0.5, vjust = 3.5, face = "plain",
                                  margin = margin(1, 2, 1, 1)),
        plot.margin = unit(c(0.3,0,0.1,.4), "cm"),
        plot.background = element_rect(linewidth = 1, color = "black")) +
  
  geom_col(aes("Introduced domestic\nanimals are\na conservation\nconcern", 
               n, 
               fill = dom_animals_concern), width = 0.3) +
  
  geom_col_pattern(aes(1, 
                       n, 
                       fill = dom_animals_concern), color="black", linewidth=0, 
                   pattern = dplyr::if_else(section_3$dom_animals_recorded=="Yes", "crosshatch", "none"),
                   pattern_density = 0.01, 
                   pattern_angle = 90,
                   pattern_size=0.2, 
                   pattern_color="grey25") +

  geom_label(
    aes(x = rep(1, 12) -0.37, 
        y = position.y,
        label = c(rbind(labels, rep(NA, 6))),
        colour="red"), # add percentage
    fontface = "bold",
    size=2.6, 
    label.padding = unit(0.1, "lines")) +
  
  
  geom_label(aes(x = c(rep(1, 2), 0.93, 1.08, rep(1,2), 0.93, 1.08, rep(1, 4)),    
                 y = c(3.8, 12.5, 0.5, 1.5, 0, 2.5, 0.5, 2, 3, 14, 0,0),
                 label = replace(n, 
                                 list = c(n==0), 
                                 value = NA)),
             #fill = "white",
             size = 2,
             label.padding = unit(0.1, "lines")) +
  
  
  #scale_fill_brewer(type = "div") +  
  scale_fill_manual(values = brewer.pal(n = 9, name = "GnBu")[2:8]) +
  facet_wrap(.~dom_animals_concern, nrow = 1) +
  coord_flip() +
  ggtitle("Agreement ranking")

plot_section_3


# plot_section_3<-
#   ggplot(data = 
#          section_3 %>%
#          mutate(dom_animals_recorded=factor(dom_animals_recorded,
#                             levels = c("Yes", "No"),
#                             labels = c("Domestic animals\nfound during\npatrols are\ndocumented",
#                                        "Domestic animals\nfound during\npatrols are not\ndocumented")))) +
#   likert_theme +
#   theme(text = element_text(size = 10.5),
#         plot.title = element_text(size = 10, hjust = 0.5, vjust = 3.5, face = "plain",
#                                   margin = margin(1, 2, 1, 1)),
#         plot.margin = unit(c(0.3,0,0.1,.4), "cm"),
#         plot.background = element_rect(linewidth = 1, color = "black")) +
#   geom_col_pattern(aes(dom_animals_recorded, 
#                value, 
#                fill = dom_animals_concern, 
#                #alpha=percent
#                ), color="black", linewidth=0, 
#                pattern = dplyr::if_else(section_3$name=="health_status_recorded_Yes", "none", "stripe"),
#                pattern_density = 0.01, 
#                pattern_size=0.3, 
#                pattern_color="grey50") +
#   geom_text(aes(dom_animals_recorded, 
#                 value, 
#                 label=if_else(value==0, NA, value)), # add percentage
#             position = position_stack(vjust = .5),
#             fontface = "bold", color="black",
#             size=2.6) + # ) + # center the label
#   scale_fill_brewer(type = "div") +
#   facet_wrap(.~dom_animals_concern, nrow = 1) +
#   coord_flip() +
#   ggtitle("Domestic animals are a concern for conservation goals")
# 
# plot_section_3

ggsave("plots/plot_section_3.png",
       plot = plot_section_3,
       height = 2.2,
       width = 6.4,
       units = "in",
       dpi = 600)


