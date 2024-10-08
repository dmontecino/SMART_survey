source("R/load_packages.R")
library(ggthemes)
library(grid)
library(patchwork)
library(RColorBrewer)

terrestrial_data<-readRDS("data/terrestrial_data.RDS")

terrestrial_data<-terrestrial_data |> dplyr::filter(position!="Other")


section_1 =
  terrestrial_data %>% filter(local==T) %>% 
  select(#survey,
    wildlife_health_important,
    hum_liv_path_affect_wildlife,
    wildlife_path_affect_livestock,
    wildlife_path_affect_phealth)


section_1<-
  section_1 %>%
  #dplyr::select(-survey) %>%
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


#Plot

likert_theme <-
  theme_wsj() +
  theme(text = element_text(size = 20),
        plot.title = element_text(size = 20, hjust = 0.5, 
                                  margin = margin(5, 0, 10, 0), family = "arial", face = "plain"),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=16, face = "plain"),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"),
        axis.title.y  = element_text(colour = "grey10", angle = 180, size=18, family = "arial"),
        axis.title.x  = element_text(colour = "grey10", size=18, family = "arial"),
        axis.line.y.right  = element_line(colour="black"),
        strip.text = element_text(colour="grey95", size = 18),
        strip.text.y.left = element_text(angle = 0, hjust = 0),
        legend.position = "none")

custom_colors<-c("Human and livestock\npathogens can\nimpact wildlife\nhealth" ="cornflowerblue", 
                 "Wildlife health\nis important" = "darkgreen" ,
                 "Wildlife pathogens can\nimpact livestock health" = "orange3", 
                 "Wildlife pathogens can\nimpact human health"="#ee6b6e")

section_1$dummy<-"a"

p<-
  ggplot(section_1, 
         aes(x = dummy, 
             y = num_responses,
             fill=question, 
             color=question, 
             alpha=rank)) +
  geom_bar(stat = "identity", position=position_dodge(width = 0.97), width=0.6) +
  #coord_flip()+
  facet_grid(question~rank, switch = "y")+
  scale_y_continuous(position = "right") +
  likert_theme +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  # facet_grid(name ~ found, scales = "free_x", space = "free_x", switch = "y") +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  # scale_y_continuous(position = "right") +
  scale_alpha_discrete(range = c(0.3, 0.9)) +  # Set min and max alpha values
  ggtitle("Agreement ranking") +
  labs(y = "Number of responses\n")






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
bkg_colors2 <- c("orange3", "#ee6b6e", "cornflowerblue", "darkgreen")

# Edit the grobs
# for (i in 1:length(strip_bg_gpath)){
# for (i in 1:7){
#   #g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i-7]))
#    g <- editGrob(grob = g, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors1[i]))
# }

for (i in 1:6){
  #g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i-7]))
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = bkg_colors1[i]))
}

for (i in 7:10){
  #g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i-7]))
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = bkg_colors2[i-6]))
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
  
  geom_text(aes(x = 0.5, y = 0.5), label = "Item             ", angle = 90, size = 7) 

# y_title

# Combine the y-axis title and the plot using patchwork
mix <- y_title + g + plot_layout(widths = c(1, 15))  # Adjust widths as needed
mix <- mix + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))


# if(!file.exists("../SMART_survey/plots/plot_section_1.png")) {

ggsave(plot = mix, filename = "v2/plot_1.png", width = 12.85714, height = 9, dpi=300)

