library(ggplot2)
library(camcorder)
library(ggpattern)
library(RColorBrewer)

likert_theme <-
  theme_gray() +
  theme(text = element_text(size = 60),
        plot.title = element_text(size = 60, face = "bold",
                                  margin = margin(10, 0, 10, 0)),
        plot.margin = unit(c(2.4,0,2.4,.4), "cm"),
        plot.background = element_rect(linewidth = 5, color = "black"),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        strip.background  = element_blank(),
        legend.position = "none")


section_2 <-
  terrestrial_data %>%
  distinct(survey,
           dead_found,
           sick_injured_found,
           dead_wl_recorded,
           sick_wl_recorded,
           injured_wl_recorded)



dead_found_recorded<-
  section_2 %>%
  dplyr::select(-survey) %>%
  dplyr::count(dead_found, dead_wl_recorded, name = "total_dead_found_recorded_group") %>% 
  tidyr::complete(dead_found, dead_wl_recorded ) %>% 
  tidyr::replace_na(replace = list(total_dead_found_recorded_group=0)) #%>% 

sick_found_recorded<-
  section_2 %>%
  dplyr::select(-survey) %>%
  dplyr::count(sick_injured_found, sick_wl_recorded, name = "total_sick_found_recorded_group") %>% 
  tidyr::complete(sick_injured_found, sick_wl_recorded ) %>% 
  tidyr::replace_na(replace = list(total_sick_found_recorded_group=0)) #%>% 

injured_found_recorded<-
  section_2 %>%
  dplyr::select(-survey) %>%
  dplyr::count(sick_injured_found, injured_wl_recorded, name = "total_injured_found_recorded_group") %>% 
  tidyr::complete(sick_injured_found, injured_wl_recorded ) %>% 
  tidyr::replace_na(replace = list(total_injured_found_recorded_group=0)) #%>% 







freq_found_recorded<-full_join(
                      dead_found_recorded, 
                      sick_found_recorded, 
                      by = c("dead_found" = "sick_injured_found",
                             "dead_wl_recorded" = "sick_wl_recorded")) 

freq_found_recorded<-
  full_join(
  freq_found_recorded, 
  injured_found_recorded, 
  by = c("dead_found" = "sick_injured_found",
         "dead_wl_recorded" = "injured_wl_recorded")) %>% 
  rename(found = dead_found) %>% 
  rename(recorded=dead_wl_recorded) %>% 
  rename(dead=total_dead_found_recorded_group) %>% 
  rename(sick=total_sick_found_recorded_group) %>%
  rename(injured=total_injured_found_recorded_group) %>% 
  mutate(found=factor(found,
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


freq_found_recorded<-
  freq_found_recorded %>%  
  pivot_longer(cols=c("dead", "sick", "injured")) %>% 
  arrange(found) %>% 
  mutate(name=factor(name, 
                     levels = c("injured", "sick", "dead"),
                     labels = c("Sick or injured\nwildlife (b)",
                                "Sick or injured\nwildlife (a)",
                                "Dead wildlife")))




#labels all dead per freq encountering 

labels<-
freq_found_recorded %>% 
  group_by(found, name) %>% 
  mutate(label=sum(value)) %>% 
  ungroup() %>%  
  distinct(found, name, label) %>% 
  pull(label)



# plot
plot_section_2A<-
  
  ggplot(data = freq_found_recorded) +
  
  likert_theme +
  
  theme(text = element_text(size = 10.5),
        plot.title = element_text(size = 10, hjust = 0.5, vjust = 3.5, face = "plain",
                                  margin = margin(1, 2, 1, 1)),
        plot.margin = unit(c(0.3,0,0.1,.4), "cm"),
        plot.background = element_rect(linewidth = 1, color = "black")) +
  
  geom_col(aes(name, value, fill = found), width = 0.3) +
  
  geom_col_pattern(aes(name, 
                       value, 
                       fill = found), color="black", linewidth=0, 
                   pattern = dplyr::if_else(freq_found_recorded$recorded=="Yes", "crosshatch", "none"),
                   pattern_density = 0.01, 
                   pattern_orientation = 'horizontal',
                   pattern_size=0.2, 
                   pattern_color="grey25") +
  
  geom_label(
    aes(x = rep(c(3,2,1), 14) -0.37, 
        y = c(rbind(labels/2, rep(NA, 21))),
        label = c(rbind(labels, rep(NA, 21))),
        colour="red"), # add percentage
    fontface = "bold",
    size=2.6, 
    label.padding = unit(0.1, "lines")) +
  
  geom_label(aes(x = c(rep(c(3,2,1), 10), 3,2.08,1,3,1.93,1, 3.08,2, 1.08, 2.93,2, 0.93),   
                 y = c(1.5, 2.5, 2.5, 4.5, 6.5, 6.5,
                       4, 7.5, 6.5, 11.5, 17, 16, 
                       1, 6, 3.5, 5, 12.5, 10.5,
                       2, 3, 2, 11, 9, 8,
                       0.5, 3.5, 5, 8.5, 11, 12.5,
                       0.5, 0.5, 0, 6, 2, 1.5, 
                       0.5, 1.5, 0.5, 1.5, 4.5, 2),
                 label = replace(value, list = c(value==0), values = NA)),
             #fill = "white",
             size = 2,
             label.padding = unit(0.1, "lines")) +
  
  #scale_fill_brewer(type = "seq", palette = "GnBu") +
  scale_fill_manual(values = brewer.pal(n = 9, name = "GnBu")[2:8]) +
  facet_wrap(.~found, nrow = 1) +
  coord_flip() +
  ggtitle("Overall encountering frecuency in protected area(s)")

  plot_section_2A 
 
 

# if(!file.exists("../SMART_survey/plots/plot_section_2A.png")) {

ggsave("plots/plot_section_2A_option2.png",
       plot = plot_section_2A,
       height = 2.2/2*3,
       width = 6.4,
       units = "in",
       dpi = 600)
#}



