
library(countrycode)

country_data<-
  tibble(
    country=dat_modified |> 
      dplyr::filter(position!="Other") |> 
      select(protected_area) |> 
      pull(protected_area)|> 
      map_vec(\(x) x$country[1]),
    local= dat_modified |>  
      dplyr::filter(position!="Other") |> 
      pull(local))


country_data$continent <- 
  countrycode(sourcevar = country_data |>  pull(country) |> as.numeric(),
              origin = "un",
              destination = "continent")


country_data<-  
  country_data |> 
  select(continent, country, local) |> 
  group_by(local) |> 
  count(continent, country, .drop = FALSE) |> 
  pivot_wider(values_from = n, names_from = "local") |> 
  rename("non_local" = 'FALSE', 'local' = 'TRUE') |> 
  arrange(continent, country) |> 
  print(n =40) 


# countries in local data

number_countries_local_data<-
country_data |> 
  filter(!is.na(local)) |> 
  nrow()

countries_per_zone_local_data<-
country_data |> 
  filter(!is.na(local)) |> 
  count(continent)

country_data |> 
  filter(!is.na(non_local))

country_data |> 
  filter(!is.na(local)) |> 
  group_by(continent) |> 
  summarise(n_pa_per_continent = sum(local))

country_data |> 
  filter(!is.na(non_local)) |> 
  group_by(continent) |> 
  summarise(n_pa_per_continent = sum(non_local))

