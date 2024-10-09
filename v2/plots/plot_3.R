source("R/load_packages.R")
library(ggthemes)
library(grid)
library(patchwork)


terrestrial_data<-readRDS("data/terrestrial_data.RDS")

terrestrial_data<-terrestrial_data |> dplyr::filter(position!="Other")

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
  dplyr::add_count(dom_animals_concern, 
                   dom_animals_recorded, 
                   dom_animal_health_status_recorded,
                   name = "n_concern_dom_animals_recorded") |> 
  tidyr::complete(dom_animals_concern, dom_animals_in_pa, dom_animals_recorded) |> 
  dplyr::filter(!(dom_animals_in_pa=="No" & dom_animals_recorded!="")) |> 
  dplyr::filter(!(dom_animals_in_pa=="Yes" & dom_animals_recorded=="")) 

# dom_found_recorded |> arrange(dom_animals_concern)

dom_found_recorded[which(
  dom_found_recorded$dom_animals_concern=="Strongly\ndisagree" &
    dom_found_recorded$dom_animals_in_pa=="Yes"),]$n_concern_dom_animals_recorded<-0

# dom_found_recorded<-
# dom_found_recorded |> 
#   filter(!is.na(dom_animals_concern))

dom_found_recorded[dom_found_recorded$dom_animals_recorded=="",]$dom_animals_recorded<-"NA"


# dplyr::mutate(
#   n_concern_dom_animals_recorded=
#     if_else(dom_animals_recorded=="",
#             NA,
#             n_concern_dom_animals_recorded))
# 

# Create a new column combining `recorded` and `name` for unique colors
dom_found_recorded$combined <- 
  with(dom_found_recorded, 
       paste(dom_animals_in_pa, 
             dom_animals_recorded, 
             dom_animal_health_status_recorded, 
             sep = "_"))

custom_colors <- c(
  "No_NA_NA" = "#d62728",
  "No_NA_" = "#d62728",
  "Yes_No_" =  "cornflowerblue",
  "Yes_No_NA" = "cornflowerblue",
  "Yes_Yes_NA" = "blue",
  "Yes_Yes_No" = "blue",
  "Yes_Yes_Yes" = "navyblue")

dom_found_recorded<-
  dom_found_recorded |> distinct()

dom_found_recorded$n_concern_dom_animals_recorded[
  is.na(dom_found_recorded$n_concern_dom_animals_recorded) &
    dom_found_recorded$combined=="No_NA_NA"]<-0

dom_found_recorded$n_concern_dom_animals_recorded[
  dom_found_recorded$dom_animals_concern=="Neutral" &
  dom_found_recorded$dom_animals_recorded=="No"]<-0

# Plot 

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

p<-
  ggplot(dom_found_recorded , 
         aes(x = dom_animals_recorded, 
             y = n_concern_dom_animals_recorded,
             fill = combined,
             colour = combined,
             alpha = as.numeric(dom_animals_concern))) +
  geom_bar(stat = "identity", position="stack", width = 0.97) +
  #facet_grid(.~dom_animals_concern, switch = "y")
  facet_grid(dom_animals_in_pa~dom_animals_concern, 
             scales = "free_x", 
             space = "free_x", switch = "y") +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  likert_theme +
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  scale_y_continuous(position = "right") +
  scale_alpha_continuous(range = c(0.3, 0.9)) +  # Set min and max alpha values
  ggtitle("Overall agreement domestic animals are a conservation concern") +
  labs(x = "\nRecording", y = "Number of responses\n")

# p

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
bkg_colors2 <- c("#ee6b6e", "cornflowerblue")


for (i in 1:6){
  #g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i-7]))
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = bkg_colors1[i]))
}

for (i in 7:8){
  #g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i-7]))
  g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = bkg_colors2[i-6]))
}


# for (i in 8:10){
#   #g <- editGrob(grob = g, gPath = strip_bg_gpath[i], gp = gpar(fill = fills[i-7]))
#   g <- editGrob(grob = g, gPath = strip_txt_gpath[i], gp = gpar(col = txt_colors2[i-7]))
# }


# Draw the edited plot
# grid.newpage(); 
# grid.draw(g)

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
  
  geom_text(aes(x = 0.5, y = 0.5), 
            label = "Domestic animals in protected area    ", 
            angle = 90, 
            size = 7) 

# y_title
  
# Combine the y-axis title and the plot using patchwork
mix <- y_title + g + plot_layout(widths = c(1, 15))  # Adjust widths as needed
mix <- mix + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

# mix

ggsave(plot = mix, filename = "v2/plot_3.png", width = 12.85714, height = 6)





