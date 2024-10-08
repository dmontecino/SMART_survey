library(dplyr)
library(tidyr)
library(purrr)
library(janitor)
library(stringr)

create_status_tibble <- function(status) {
  # Dynamically create the column names based on the status
  status_column <- paste0(status, "_wl_recorded")
  how_status_col <- paste0("how_", status, "_wl_recorded")
  what_status_col <- paste0(status, "_wl_data_recorded")
  
  # Process the data
  result <- terrestrial_data |> 
    filter(local == TRUE) |> 
    select(all_of(status_column), all_of(how_status_col), all_of(what_status_col)) |> 
    filter(get(status_column) == "Yes") |> 
    add_count(!!sym(how_status_col), name = "count_how_recorded") |> 
    nest(data = !!sym(what_status_col)) |> 
    mutate(prop_how_recorded = count_how_recorded / sum(count_how_recorded)) |> 
    unnest(data) |> 
    tidyr::separate_longer_delim(!!sym(what_status_col), ", ") |> 
    add_count(!!sym(how_status_col), !!sym(what_status_col), name = "count_what_recorded") |> 
    distinct() |> 
    mutate(prop_what_recorded = count_what_recorded / count_how_recorded) |> 
    select(-all_of(status_column)) |> 
    # Correcting factor conversion based on string patterns
    mutate(!!sym(how_status_col) := factor(
      case_when(
        grepl("individual observation", !!sym(how_status_col)) ~ "Individual observation",
        grepl("part of the full count", !!sym(how_status_col)) ~ "Part of the full count",
        grepl("present/absent", !!sym(how_status_col)) ~ "Present or absent",
        grepl("another way", !!sym(how_status_col)) ~ "Another way",
        TRUE ~ NA_character_  # Handle cases where no match is found
      ),
      levels = c("Individual observation", 
                 "Part of the full count", 
                 "Present or absent", 
                 "Another way")
    )) |> 
    arrange(!!sym(how_status_col)) |> 
    mutate(status = str_to_title(status)) |>  # Set status
    select(status, 
           !!sym(how_status_col), 
           prop_how_recorded, 
           !!sym(what_status_col), 
           prop_what_recorded) |> 
    nest(data = c(!!sym(what_status_col), prop_what_recorded)) |> 
    mutate(data = purrr::map_df(data, \(x) x |> 
                                  pivot_wider(
                                    names_from = !!sym(what_status_col), 
                                    values_from = prop_what_recorded
                                  ))) |> 
    unnest(cols = data) |> 
    janitor::clean_names()
  
  return(result)
}
