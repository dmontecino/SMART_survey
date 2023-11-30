data_smart_dead<-
  terrestrial_data |> 
  distinct(survey, 
           dead_wl_recorded,
           dead_wl_data_in_smart) |>
  select(-survey) |>
  filter(dead_wl_recorded=="Yes") |>
  count(dead_wl_recorded, dead_wl_data_in_smart, name = "count_for_dead_wl") |>
  select(-dead_wl_recorded) %>% 
  rename("data_in_smart"="dead_wl_data_in_smart") |>
  mutate(percentage=round(count_for_dead_wl/sum(count_for_dead_wl)*100,2)) |>
  mutate(class="dead") |>
  select(-count_for_dead_wl)


data_smart_sick<-
  terrestrial_data |> 
  distinct(survey, 
           sick_wl_recorded,
           sick_wl_data_in_smart) |>
  select(-survey) |>
  filter(sick_wl_recorded=="Yes") |>
  count(sick_wl_recorded, sick_wl_data_in_smart, name = "count_for_sick_wl") |>
  select(-sick_wl_recorded)  |>
  rename("data_in_smart"="sick_wl_data_in_smart") |>
  mutate(percentage=round(count_for_sick_wl/sum(count_for_sick_wl)*100,2))|>
  mutate(class="sick") |>
  select(-count_for_sick_wl)


data_smart_injured<-
  terrestrial_data |> 
  distinct(survey, 
           injured_wl_recorded,
           injured_wl_data_in_smart) |>
  select(-survey) |>
  filter(injured_wl_recorded=="Yes") |>
  count(injured_wl_recorded, injured_wl_data_in_smart, name = "count_for_injured_wl") |>
  select(-injured_wl_recorded) %>% 
  rename("data_in_smart"="injured_wl_data_in_smart") |>
  mutate(percentage=round(count_for_injured_wl/sum(count_for_injured_wl)*100,2)) |>
  mutate(class="injured") |>
  select(-count_for_injured_wl)


temp_long<-
bind_rows(data_smart_sick, 
          data_smart_injured,
          data_smart_dead)
          


# create plot
wildlife_health_in_smart<-
  ggplot(temp_long, aes(x = class, y = data_in_smart, size = percentage, fill = class)) +
  geom_point(shape = 21, stroke=0.2) +
  geom_label(aes(label = round(percentage,1)),             
            size = 2.6, 
            fontface = "bold",
            vjust = 0.4, 
            fill="white") +
  scale_size(range = c(6, 24), name = "Response count") +
  scale_y_discrete(name = "",
                   labels = c("All of these items are\nstored in a SMART\ndatabase",
                              "None of these items are\nentered in a SMART\ndatabase",
                              "Some of these items are\nentered in a SMART\ndatabase")) +
  
  scale_x_discrete(limits = c("sick",
                              "injured",
                              "dead"),
                   position = "top",
                   name = "",
                   labels = c("Sick\nwildlife",
                              "Injured\nwildlife",
                              "Dead\nwildlife")) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    # panel.background = element_rect(fill = "white"),
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

wildlife_health_in_smart

gg_resize_film(
  height = 3.6*2/8*4,
  width = 5/5*4/5*5,
  units = "in",
  dpi = 300
)


ggsave("plots/plot_section_4A.png",
       plot = wildlife_health_in_smart,
       height = 3.6*2/8*4,
       width = 5/5*4/5*5,
       units = "in",
       dpi = 600)
