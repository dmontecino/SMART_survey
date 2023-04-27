library(rnaturalearth)
library(rnaturalearthdata)
library(sp)


# world map

world<-ne_countries(scale="medium", returnclass = "sf")

# box around the map 
url <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_wgs84_bounding_box.zip"
filename <- tempfile(fileext = ".zip")
download.file(url, destfile = filename, mode = "wb")



# extract the contents of the zipped file to a temporary directory
temp_dir <- tempdir()
unzip(filename, exdir = temp_dir)



# read the unzipped shapefile into an sf object
box <- st_read(dsn = temp_dir, layer = "ne_50m_wgs84_bounding_box")



#countries included in the dataset

countries_in_dat<-sort(unique(terrestrial_data$country))

countries.for.plot<-world %>% 
  filter(name %in% c(countries_in_dat, grep("Congo", world$name, value = T))) 

 # nrow(countries.for.plot) == length(countries_in_dat) #T


# ---- #
# PLOT #
# ---- #

# themes for the plot 
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="#e6e8ed"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title =element_blank()))

# reproject
world_robin <- st_transform(world, "+proj=robin")
box_robin <- st_transform(box, "+proj=robin")  # reproject bounding box
countries.for.plot_robin <- st_transform(countries.for.plot, "+proj=robin")  # reproject bounding box


ggplot() +
  geom_sf(data = box_robin, fill = "white", color = "grey", linewidth=0.3) +  
  geom_sf(data = world_robin, fill = "black", color = NA) + 
  geom_sf(data = countries.for.plot_robin, fill = "#33DB4F", color = NA) + 
  theme_opts
  

# ggsave("maps/countries_included_map.png", width=110, height=50.8, units = "cm", dpi=300)


