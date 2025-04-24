
source("R/bar_chart_in_gt.R")
library(ggthemes)
library(grid)
library(paletteer)
library(gt)


collapse_rows_gt<-source("https://gist.githubusercontent.com/jmclawson/6852c14023d7d9b7e91bbcfa419adeb8/raw/7435c51c05ff5f537bd545c63325ea2ed4681a91/collapse_rows.R")

collapse_rows_gt<-collapse_rows_gt$value

source("R/create_datasets_for_table_1.R")


#healthy dataset
healthy<-create_status_tibble(status="healthy")

healthy<-
  healthy |> 
  mutate(across(where(is.double), ~replace_na(., 0))) |> 
  rename("how_wl_recorded" = "how_healthy_wl_recorded") |> 
  rename("condition" = "body_condition")|> 
  arrange(how_wl_recorded)



#injured dataset
injured<-create_status_tibble(status="injured")

injured<-
  injured |> 
  mutate(across(where(is.double), ~replace_na(., 0))) |>   # Replace NA with 0 for double columns
  rename("anomalies" ='anomalies_signs_if_any') |> 
  rename("suspected_cause" = contains("suspect_cause")) |> 
  rename("how_wl_recorded" = "how_injured_wl_recorded") |> 
  rename("condition" = "body_condition")|> 
  arrange(how_wl_recorded)



#sick dataset
sick<-create_status_tibble(status="sick")

sick<-
  sick |> 
  mutate(across(where(is.double), ~replace_na(., 0))) |>   # Replace NA with 0 for double columns
  rename("anomalies" ='anomalies_signs_if_any') |> 
  rename("suspected_cause" = contains("suspect_cause")) |> 
  rename("how_wl_recorded" = "how_sick_wl_recorded") |> 
  rename("condition" = "body_condition")|> 
  arrange(how_wl_recorded)





#dead dataset

# terrestrial_data |> 
#   select(contains("dead")) |> 
#   filter(dead_wl_recorded=="Yes") |> 
#   count(how_dead_wl_recorded)

dead<-create_status_tibble(status="dead")

dead<-
  dead |> 
  mutate(across(where(is.double), ~replace_na(., 0))) |>   # Replace NA with 0 for double columns
  rename("anomalies" ='anomalies_in_carcass_if_any') |> 
  rename("suspected_cause" = contains("suspect_cause")) |> 
  rename("how_wl_recorded" = "how_dead_wl_recorded") |> 
  rename("condition" = "carcass_condition") |> 
  arrange(how_wl_recorded)



full_data<-
  full_join(healthy, injured) |> 
  full_join(sick) |> 
  full_join(dead) 


number_recording_healthy<-
  terrestrial_data |> 
  filter(local == FALSE) |> 
  count(healthy_wl_recorded) |> 
  filter(healthy_wl_recorded=="Yes") |> 
  pull(n)

number_recording_injured<-
  terrestrial_data |> 
  filter(local == FALSE) |> 
  count(injured_wl_recorded) |> 
  filter(injured_wl_recorded=="Yes") |> 
  pull(n)


number_recording_sick<-
  terrestrial_data |> 
  filter(local == FALSE) |> 
  count(sick_wl_recorded) |> 
  filter(sick_wl_recorded=="Yes") |> 
  pull(n)


number_recording_dead<-
  terrestrial_data |> 
  filter(local == FALSE) |> 
  count(dead_wl_recorded) |> 
  filter(dead_wl_recorded=="Yes") |> 
  pull(n)


number_recording_at_least_one_non_healthly_cat<-
  terrestrial_data |> 
  filter(local == FALSE) |> 
  filter(sick_wl_recorded=="Yes" | injured_wl_recorded=="Yes" | dead_wl_recorded=="Yes") |> 
  select(sick_wl_recorded, 
         injured_wl_recorded,
         dead_wl_recorded) |> 
  nrow()


full_data[full_data$status=="Healthy",]$status<-paste0("Healthy (n = ", number_recording_healthy, ")")
full_data[full_data$status=="Injured",]$status<-paste0("Injured (n = ", number_recording_injured, ")")
full_data[full_data$status=="Sick",]$status<-paste0("Sick (n = ", number_recording_sick, ")")
full_data[full_data$status=="Dead",]$status<-paste0("Dead (n = ", number_recording_dead, ")")




## TABLES ##




prop_how_recorded_colors<-
  purrr::map(c("#000080", "#d62728", "#C76E00", "#06402B"), \(x) rep(x, 4)) |> unlist() |> rev()

prop_what_recorded_colors<-
  purrr::map(c("#1f77b4", "pink", "orange", "#ABDDA4"), \(x) rep(x, 4)) |> unlist() |> rev()



full_table<-
  full_data |> 
  mutate(
    prop_how_recorded = 
      purrr::map2_chr(.x = full_data$prop_how_recorded*100, 
                      .y = prop_how_recorded_colors, .f = 
                        ~bar_chart(label = .x, 
                                   fill = .y, 
                                   background = "#d2d2d2")) |> 
      purrr::map(gt::html),
    
    species = purrr::map2_chr(.x = full_data$species*100, 
                              .y = prop_what_recorded_colors, .f = 
                                ~bar_chart(label = .x, 
                                           fill = .y, 
                                           background = "#d2d2d2")) |> 
      purrr::map(gt::html),
    
    
    age = purrr::map2_chr(.x = full_data$age*100, 
                          .y = prop_what_recorded_colors, .f = 
                            ~bar_chart(label = .x, 
                                       fill = .y, 
                                       background = "#d2d2d2")) |> 
      purrr::map(gt::html),
    
    sex = purrr::map2_chr(.x = full_data$sex*100, 
                          .y = prop_what_recorded_colors, .f = 
                            ~bar_chart(label = .x, 
                                       fill = .y, 
                                       background = "#d2d2d2")) |> 
      purrr::map(gt::html),
    
    condition = purrr::map2_chr(.x = full_data$condition*100, 
                                .y = prop_what_recorded_colors, .f = 
                                  ~bar_chart(label = .x, 
                                             fill = .y, 
                                             background = "#d2d2d2")) |> 
      purrr::map(gt::html),
    
    anomalies = purrr::map2_chr(.x = full_data$anomalies*100, 
                                .y = prop_what_recorded_colors, .f = 
                                  ~bar_chart(label = .x, 
                                             fill = .y, 
                                             background = "#d2d2d2")) |> 
      purrr::map(gt::html),
    
    photographs = purrr::map2_chr(.x = full_data$photographs*100, 
                                  .y = prop_what_recorded_colors, .f = 
                                    ~bar_chart(label = .x, 
                                               fill = .y, 
                                               background = "#d2d2d2")) |> 
      purrr::map(gt::html),
    
    other = purrr::map2_chr(.x = full_data$other*100, 
                            .y = prop_what_recorded_colors, .f = 
                              ~bar_chart(label = .x, 
                                         fill = .y, 
                                         background = "#d2d2d2")) |> 
      purrr::map(gt::html)) %>% 
  dplyr::select(
    status,
    how_wl_recorded,
    prop_how_recorded, 
    species,
    age,
    sex,
    condition,
    anomalies,
    photographs,
    other) %>%
  gt() |> 
  cols_align(align = "left") |> 
  cols_label(
    status = "Wildlife<br>status",
    species = "Species",
    age = "Age",
    sex = "Sex", 
    condition = "Condition",
    anomalies = "Anomalies",
    photographs = "Photographs",
    other = "Other",
    .fn = md) |> 
  tab_spanner(label = md("**Documentation method used (%)**"),
              columns = c(how_wl_recorded,
                          prop_how_recorded)) |> 
  tab_spanner(label = md("**Data recorded (%)**"),
              columns = c(species,
                          age,
                          sex,
                          condition,
                          anomalies,
                          photographs,
                          other)) |>
  cols_label(
    how_wl_recorded = "",
    prop_how_recorded = "") |> 
  cols_width(everything() ~ px(140)) |> 
  collapse_rows_gt(status)


fix_anomalies_for_healthy<-gt::html("<div style='display:flex;align-items:left;'><div style='position:relative;flex-grow:1;margin-left:8px;background:white;'><div style='position:absolute;width:0;height:30px;border-left:2px solid white;left:50%;top:0;'></div><div style='background:white;width:NA%;height:30px;'></div></div></div>")
full_table$`_data`$anomalies[[1]]<-fix_anomalies_for_healthy
full_table$`_data`$anomalies[[2]]<-fix_anomalies_for_healthy
full_table$`_data`$anomalies[[3]]<-fix_anomalies_for_healthy
full_table$`_data`$anomalies[[4]]<-fix_anomalies_for_healthy

# test<-as_gtable(full_table)
# library(patchwork)
# wrap_elements(full_table)
#   
# patchwork::wrap_ggplot_grob(full_table)
# 
# class(full_table)
# test<-gtExtras::gt_reprex_image(full_table)
# gtExtras::gtsave_extra(full_table, "v2/table1.png")
# 
# gtsave(full_table, "v2/table1.rtf")
# png("v2/table1.png", width = 10, height = 7, units = 'in', res = 300)

str(full_table)

# install.packages("chromote")
# library(chromote)
# gtsave(full_table, filename = "plots/my_table.png")


# Convert to PNG using 'pdftools' and 'magick'
# library(pdftools)
# library(magick)
# 
# gtsave(full_table, filename = "my_table.pdf")
# img <- image_read_pdf("my_table.pdf", density = 300)
# image_write(img, path = "my_table.png", format = "png")

library(webshot2)
gtsave(full_table, "plots/my_table.html")

# Then manually take a larger screenshot (height set manually)
webshot2::webshot("plots/my_table.html", 
                  file = "plots/plot_3.png", 
                  vwidth = 1400, 
                  vheight = 3000, 
                  zoom = 4)



