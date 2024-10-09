source("R/load_packages.R")
library(ggthemes)
library(grid)
library(patchwork)

terrestrial_data<-readRDS("data/terrestrial_data.RDS")

terrestrial_data<-terrestrial_data |> dplyr::filter(position!="Other")


section_2 <-
  terrestrial_data |> filter(local==T) |> 
  select(dead_found,
         sick_injured_found,
         dead_wl_recorded,
         sick_wl_recorded,
         injured_wl_recorded)



dead_found_recorded<-
  section_2 |>
  # dplyr::select(-survey) |>
  dplyr::count(dead_found, dead_wl_recorded, name = "total_dead_found_recorded_group") |> 
  tidyr::complete(dead_found, dead_wl_recorded ) |> 
  tidyr::replace_na(replace = list(total_dead_found_recorded_group=0)) #|> 

sick_found_recorded<-
  section_2 |>
  # dplyr::select(-survey) |>
  dplyr::count(sick_injured_found, sick_wl_recorded, name = "total_sick_found_recorded_group") |> 
  tidyr::complete(sick_injured_found, sick_wl_recorded ) |> 
  tidyr::replace_na(replace = list(total_sick_found_recorded_group=0)) #|> 

injured_found_recorded<-
  section_2 |>
  # dplyr::select(-survey) |>
  dplyr::count(sick_injured_found, injured_wl_recorded, name = "total_injured_found_recorded_group") |> 
  tidyr::complete(sick_injured_found, injured_wl_recorded ) |> 
  tidyr::replace_na(replace = list(total_injured_found_recorded_group=0)) #|> 







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
           "dead_wl_recorded" = "injured_wl_recorded")) |> 
  rename(found = dead_found) |> 
  rename(recorded=dead_wl_recorded) |> 
  rename(dead=total_dead_found_recorded_group) |> 
  rename(sick=total_sick_found_recorded_group) |>
  rename(injured=total_injured_found_recorded_group) |> 
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
  freq_found_recorded |>  
  tidyr::pivot_longer(cols=c("dead", "sick", "injured")) |> 
  arrange(found) |> 
  mutate(name=factor(name, 
                     levels = c("injured", "sick", "dead"),
                     labels = c("Injured*",
                                "Sick*",
                                "Dead")))

freq_found_recorded<-
freq_found_recorded |> 
  arrange(name, found) |> 
  group_by(name, found) |> 
  mutate(prop = value/sum(value))

#labels all dead per freq encountering 

# labels<-
#   freq_found_recorded |> 
#   group_by(found, name) |> 
#   mutate(label=sum(value)) |> 
#   ungroup() |>  
#   distinct(found, name, label) |> 
#   pull(label)


#Plot

likert_theme <-
  # theme_gray() + 
  theme_wsj() +
  theme(text = element_text(size = 20),
        plot.title = element_text(size = 20, hjust = 0.5, 
                                  margin = margin(5, 0, 10, 0), family = "arial", face = "plain"),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        #plot.margin = unit(c(2.4,0,2.4,.4), "cm"),
        # plot.background = element_rect(linewidth = 2, color = "black"),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_blank(),
        axis.text = element_text(size=16, face = "plain"),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"),
        axis.title.y  = element_text(colour = "grey10", angle = 180, size=18, family = "arial"),
        axis.title.x  = element_text(colour = "grey10", size=18, family = "arial"),
        #panel.background = element_blank(),
        #strip.background  = element_blank(),
        axis.line.y.right  = element_line(colour="black"),
        strip.text = element_text(colour="grey95", size = 18),
        #strip.text.x = element_text(colour="grey75"),
        strip.text.y.left = element_text(angle = 0, hjust = 0),
        legend.position = "none")


freq_found_recorded<-
  freq_found_recorded |>
  mutate(transparency=if_else(recorded=="No", 0.4,1))


# camcorder::gg_resize_film( height = 2.2/2*3,
#                            width = 6.4,
#                            units = "in")



# Create a new column combining `recorded` and `name` for unique colors
freq_found_recorded$combined <- with(freq_found_recorded, paste(recorded, name, sep = "_"))

custom_colors <- c(
  "No_Dead" = "#1f77b4",    # Blue for No - Dead wildlife
  "Yes_Dead" = "navyblue",   # Orange for Yes - Dead wildlife
  "No_Sick*" = "pink",   # Green for No - Sick wildlife
  "Yes_Sick*" = "#d62728",  # Red for Yes - Sick wildlife
  "No_Injured*" = "orange", # Purple for No - Injured wildlife
  "Yes_Injured*" = "orange3" # Brown for Yes - Injured wildlife
)

p<-
ggplot(freq_found_recorded, 
                  aes(x = recorded, 
                      y = value, 
                      fill = combined, 
                      color = combined, 
                      alpha=as.numeric(found))) +
     likert_theme +
     theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
     geom_bar(stat = "identity", position=position_dodge(width = 0.97)) +
     geom_bar(aes(y = prop*100/5,
               x = recorded), 
           stat="identity", 
           width = 0.2, 
           position = position_nudge(x = 0.6), 
           fill=ifelse(freq_found_recorded$recorded=="No", NA, "darkgreen"),
           colour=ifelse(freq_found_recorded$recorded=="No", NA, "darkgreen")) +
     facet_grid(name ~ found, scales = "free_x", space = "free_x", switch = "y") +
     scale_fill_manual(values = custom_colors) +
     scale_color_manual(values = custom_colors) +
     scale_y_continuous(position = "right") +
     scale_alpha_continuous(range = c(0.3, 0.9)) +  # Set min and max alpha values
     ggtitle("Overall encounter frequency in protected area(s)") +
     labs(x = "\nRecording", y = "Number of responses\n")



# Generate the ggplot2 plot grob
g <- grid.force(ggplotGrob(p))
# Get the names of grobs and their gPaths into a data.frame structure
grobs_df <- do.call(cbind.data.frame, grid.ls(g, print = FALSE))
# Build optimal gPaths that will be later used to identify grobs and edit them
grobs_df$gPath_full <- paste(grobs_df$gPath, grobs_df$name, sep = "::")
grobs_df$gPath_full <- gsub(pattern = "layout::", 
                            replacement = "", 
                            x = grobs_df$gPath_full, 
                            fixed = TRUE)

# Get the gPaths of the strip background grobs
strip_bg_gpath <- grobs_df$gPath_full[grepl(pattern = ".*strip\\.background.*", 
                                              x = grobs_df$gPath_full)]
# strip_bg_gpath[1] # example of a gPath for strip background 
## [1] "strip-t-1.7-5-7-5::strip.1-1-1-1::strip.background.x..rect.5374"

# Get the gPaths of the strip titles
strip_txt_gpath <- grobs_df$gPath_full[grepl(pattern = "strip.*titleGrob.*text.*",
                                             x = grobs_df$gPath_full)]
# strip_txt_gpath[1] # example of a gPath for strip title
## [1] "strip-t-1.7-5-7-5::strip.1-1-1-1::GRID.titleGrob.5368::GRID.text.5364"

#Now we can edit the grobs:
  
  # Generate some color
# n_cols <- length(strip_bg_gpath[1:7])
# fills <- rainbow(n_cols)
#bkg_colors1 <- RColorBrewer::brewer.pal(n = 9, name = "Greys")[3:9]
bkg_colors1 <- paste0("grey",seq(from=80, to=10, by=-11))
bkg_colors2 <- c("orange3", "#ee6b6e", "cornflowerblue")

# Edit the grobs
# for (i in 1:length(strip_bg_gpath)){
# for (i in 1:7){
#   #g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i-7]))
#    g <- editGrob(grob = g, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors1[i]))
# }

for (i in 1:7){
  #g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i-7]))
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = bkg_colors1[i]))
}

for (i in 8:10){
  #g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i-7]))
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = bkg_colors2[i-7]))
}


# for (i in 8:10){
#   #g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i-7]))
#   g <- editGrob(grob = g, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors2[i-7]))
# }


# Draw the edited plot
grid.newpage(); 
grid.draw(g)

g<-ggpubr::as_ggplot(g)

# Create a single y-axis title as a separate plot
y_title <- ggplot() + 
  likert_theme +
  theme(panel.background = element_rect(fill = "#F7F2E6", colour = NA),
        plot.title = element_text(size = 20, hjust = 0.5, 
                                  margin = margin(0, 0, 0, 0), family = "arial", face = "plain"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        plot.background = element_rect(fill ="#F7F2E6", colour = NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.ticks.y  = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y= element_blank()) +
  
  geom_text(aes(x = 0.5, y = 0.5), label = "Wildlife category", angle = 90, size = 7) 

# y_title

# Combine the y-axis title and the plot using patchwork
mix <- y_title + g + plot_layout(widths = c(1, 15))  # Adjust widths as needed
mix <- mix + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

ggsave(plot = mix, filename = "v2/plot_2.png", width = 15, height = 9)

# same plot but now with the proporcion label


