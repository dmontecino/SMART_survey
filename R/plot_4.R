source("R/load_packages.R")
library(ggthemes)
library(grid)
library(patchwork)

# 
# terrestrial_data<-readRDS("data/terrestrial_data.RDS")
# 
# terrestrial_data<-terrestrial_data |> dplyr::filter(position!="Other")

# terrestrial_data |>  filter(dom_animals_concern=="Somewhat Disagree")


section_3 <-
  terrestrial_data |> filter(local==T) |> 
  select(dom_animals_concern,
         dom_animals_in_pa,
         dom_animals_recorded,
         dom_animal_health_status_recorded)

section_3<-section_3  %>% 
  mutate(dom_animals_concern=factor(dom_animals_concern,
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
                                                   "Strongly\ndisagree"))))






dom_found_recorded<-
  section_3 |>
  dplyr::count(dom_animals_concern, 
               dom_animals_in_pa,
               dom_animals_recorded, 
               dom_animal_health_status_recorded,
               name = "n_concern_dom_animals_recorded") |> 
  group_by(dom_animals_in_pa) |> 
  complete(dom_animals_concern,
           nesting(dom_animals_recorded,
                   dom_animal_health_status_recorded)) |> 
  ungroup() |>  
  select(dom_animals_concern, 
         dom_animals_in_pa, 
         dom_animals_recorded, 
         dom_animal_health_status_recorded,
         n_concern_dom_animals_recorded) |> 
  arrange(dom_animals_concern, dom_animals_in_pa) |> 
  replace_na(list(n_concern_dom_animals_recorded=0))



dom_not_found<-dom_found_recorded |> 
  filter(dom_animals_in_pa=="No")

sum(dom_not_found$n_concern_dom_animals_recorded)

dom_found<-dom_found_recorded |> 
  filter(dom_animals_in_pa=="Yes")

dom_found[dom_found$dom_animal_health_status_recorded=="Yes",]$dom_animal_health_status_recorded<-"Presence and health status"
dom_found[dom_found$dom_animal_health_status_recorded=="No",]$dom_animal_health_status_recorded<-"Presence only"
dom_found[dom_found$dom_animal_health_status_recorded=="",]$dom_animal_health_status_recorded<-"No recording"

dom_found<-dom_found  %>% 
  mutate(dom_animal_health_status_recorded=factor(dom_animal_health_status_recorded,
                                                  levels = c("No recording",
                                                             "Presence only",
                                                             "Presence and health status"),
                                                  labels = c("No recording",
                                                             "Presence only",
                                                             "Presence and\nhealth status")))


## PLOT 

custom_colors <- c(
  "No recording" = "#d62728",
  "Presence only" = "cornflowerblue",
  "Presence and\nhealth status" = "navyblue")

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
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text = element_text(size=16, face = "plain"),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"),
        axis.title.y  = element_text(colour = "grey10", angle = 90, size=18, family = "arial"),
        axis.title.x  = element_text(colour = "grey10", size=18, family = "arial"),
        #panel.background = element_blank(),
        #strip.background  = element_blank(),
        axis.line.y.left   = element_line(colour="black"),
        strip.text = element_text(colour="grey95", size = 18),
        #strip.text.x = element_text(colour="grey75"),
        strip.text.y.left = element_text(angle = 0, hjust = 0),
        legend.position = "none")


p<-
  ggplot(dom_found , 
         aes(x = dom_animal_health_status_recorded, 
             y = n_concern_dom_animals_recorded,
             fill = dom_animal_health_status_recorded,
             colour = dom_animal_health_status_recorded,
             alpha = as.numeric(dom_animals_concern))) +
  geom_bar(stat = "identity", position="stack", width = 0.97) +
  #facet_grid(.~dom_animals_concern, switch = "y")
  facet_grid(.~dom_animals_concern, 
             scales = "free_x", 
             space = "free_x", switch = "y") +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  likert_theme +
  theme(plot.margin=grid::unit(c(0.5,0.5,0.5,0.5), "mm")) +
  scale_alpha_continuous(range = c(0.3, 0.9)) +  # Set min and max alpha values
  # ggtitle("Overall agreement domestic animals are a conservation concern") +
  labs(x = "\nRecording", y = "Number of responses\n")

p


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
bkg_colors1 <- paste0("grey",seq(from=80, to=10, by=-13))
# bkg_colors2 <- c("#ee6b6e", "cornflowerblue")


for (i in 1:6){
  #g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i-7]))
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = bkg_colors1[i]))
}

# for (i in 7:8){
#   #g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i-7]))
#   g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = bkg_colors2[i-6]))
# }


# for (i in 8:10){
#   #g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i-7]))
#   g <- editGrob(grob = g, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors2[i-7]))
# }


# Draw the edited plot
# grid.newpage(); 
# grid.draw(g)

g<-ggpubr::as_ggplot(g)

# g

ggsave(plot = g, filename = "plots/plot_4.png", width = 12, height = 5)
