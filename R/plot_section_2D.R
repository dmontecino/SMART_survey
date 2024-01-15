healthy= 
  terrestrial_data %>% filter(local==T) %>% 
  select(how_healthy_wl_recorded, healthy_wl_recorded) %>%
  filter(healthy_wl_recorded=="Yes") %>%
  count(how_healthy_wl_recorded) %>%
  arrange(-n) %>% 
  rename("How_recorded"="how_healthy_wl_recorded")

healthy$How_recorded<-c("Part of the full count",
                                         "Individual observation",
                                         "Present or absent",
                                         "Another way")

healthy$status<-"healthy"

injured= 
  terrestrial_data %>% filter(local==T) %>% 
  select(how_injured_wl_recorded, injured_wl_recorded) %>%
  filter(injured_wl_recorded=="Yes") %>%
  count(how_injured_wl_recorded) %>%
  arrange(-n) %>% 
  rename("How_recorded"="how_injured_wl_recorded")

injured$How_recorded<- c("Individual observation",
                                         "Part of the full count",
                                         "Present or absent",
                                         "Another way")

injured$status<-"injured"

sick=
  terrestrial_data %>% filter(local==T) %>% 
  select(how_sick_wl_recorded, sick_wl_recorded) %>%
  filter(sick_wl_recorded=="Yes") %>%
  count(how_sick_wl_recorded) %>%
  arrange(-n) %>% 
  rename("How_recorded"="how_sick_wl_recorded")


sick$How_recorded <- c("Individual observation",
                                         "Part of the full count",
                                         "Present or absent",
                                         "Another way")

sick$status<-"sick"

dead= 
  terrestrial_data %>% filter(local==T) %>% 
  select(how_dead_wl_recorded, dead_wl_recorded) %>%
  filter(dead_wl_recorded=="Yes") %>%
  count(how_dead_wl_recorded) %>%
  arrange(-n) %>% 
  rename("How_recorded"="how_dead_wl_recorded")

dead$How_recorded <- c("Individual observation",
                              "Part of the full count",
                              "Present or absent",
                              "Another way")

dead$status<-"dead"

# create data frame

# temp <- data.frame("How_recorded" = how,
#                    "Responses_healthy" = healthy$n,
#                    "Responses_sick" = sick$n,
#                    "Responses_injured" = injured$n,
#                    "Responses_dead" = dead$n)

# convert to long format
# temp_long <- pivot_longer(temp, cols = 2:5, 
#                           names_to = "variable", 
#                           values_to = "value")
temp_long<-rbind(healthy, sick, injured, dead)


# create plot
how_wildlife_health_recorded=
  ggplot(temp_long, aes(x = status, y = How_recorded, size = n, fill = status)) +
  geom_point(shape = 22, stroke=0.2) +
  geom_label(aes(
    x = status, 
    y = How_recorded,
    label = n), 
             fill="white",
             colour="black",
             fontface = "bold",
             size=2.6, 
             label.padding = unit(0.1, "lines")) +
  scale_size(range = c(2, 30), name = "Response count") +
  scale_y_discrete(name = "",
                   labels = c("Recorded in\nanother way",
                              "Each animal is an\nindividual\nobservation",
                              
                              "Part of\nthe full count\nof the corresponding\nspecies",
                              "Present/absent"
                   )) +
  
  scale_x_discrete(limits = c("healthy",
                              "sick",
                              "injured",
                              "dead"),
                   position = "top",
                   name = "",
                   labels = c("Healthy\nwildlife",
                              "Sick\nwildlife",
                              "Injured\nwildlife",
                              "Dead\nwildlife")) +
  
  theme(
    plot.title = element_text(size = 10, hjust = 0.5, vjust = 2.5, face = "plain",
                              margin = margin(1, 2, 1, 1)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    strip.background  = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size=8, colour = "black"),
    axis.text.y = element_text(size =8, hjust = 0, colour = "black"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(linewidth = 1, color = "black")
  ) +
  scale_fill_brewer(type = "div", palette = "Spectral") +
  guides(size = "none", fill = "none")#+
  #ggtitle("Type of wildlife")




# gg_record(
#   dir = file.path(tempdir(), "recording100"), # where to save the recording
#   device = "png", # device to use to save images
#   width = 5,      # width of saved image
#   height = 3.6,     # height of saved image
#   units = "in",   # units for width and height
#   dpi = 300       # dpi to use when saving image
# )

how_wildlife_health_recorded

# gg_resize_film(
#   height = 3.7,
#   width = 5,
#   units = "in",
#   dpi = 300
# )


ggsave("plots/plot_section_2D.png",
       plot = how_wildlife_health_recorded,
       height = 3.7,
       width = 5,
       units = "in",
       dpi = 600)

