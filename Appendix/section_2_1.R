
protected_area_data<-
  terrestrial_data %>% 
  select(protected_area) %>% 
  unlist(recursive = F)

local_and_global_indexes<-
  map_vec(protected_area_data, \(x) 
          any(x$country%in%c("Bhutan", "Republic of Congo", "Madagascar", "Peru"))) 


local_in_global<-terrestrial_data[local_and_global_indexes,] %>% filter(local==T)

local_in_global$country<-map_vec(local_in_global$protected_area, \(x) x$country[1])

local_in_global<-local_in_global %>% nest(.by = country)

local_in_global<-
  local_in_global %>% 
  rename("local_data"="data")




global<- terrestrial_data[local_and_global_indexes,] %>% filter(local==F)

global$country<-map_vec(global$protected_area, \(x) x$country[1])

global<-global %>% nest(.by = country)

global<-
  global %>% 
  rename("global_data"="data")




compare_global_versus_local<-full_join(local_in_global, global, "country")

##-- SECTION 1

data<-c("global_data", "local_data")
questions<-c("wildlife_health_important",
             "hum_liv_path_affect_wildlife",
             "wildlife_path_affect_phealth",
             "wildlife_path_affect_livestock")


data_from_countries<-vector(mode = "list", 
                            length = nrow(compare_global_versus_local))


for(i in 1:nrow(compare_global_versus_local)){ # countries
  
  out<-
    map(questions, \(y) 
        map(data, \(x) 
    compare_global_versus_local %>% 
    slice(i) %>% 
    unnest(x) %>% 
    select(country, 
           all_of(y)) %>% 
    select(-country) %>% 
    count(across(y)) %>% 
    mutate(percentage=round(n/sum(n)*100,2)) %>% 
    mutate(across(y, str_to_sentence)) %>% 
    mutate(across(y, \(v) factor(v, levels=c("Strongly agree",
                                            "Agree", 
                                            "Somewhat agree", 
                                            "Neutral", 
                                            "Somewhat disagree",
                                            "Disagree", 
                                            "Strongly disagree")))) %>% 
    add_column("response_type" = rep(if_else(x=="local_data", "Local", "Non-local"), 
                                     nrow(.)), .before = 1) %>%  
    mutate(percentage = paste0(percentage, "% (n=", n, ")")) %>% 
    arrange(-n, across(y)) %>% 
    select(-n)))
  
  out<-map(out, \(x) do.call(rbind, x))

  data_from_countries[[i]]<-out     
  
}

#data_from_countries[[country]][[response]]

out<-
map(1:length(questions), \(y) 
map(data_from_countries, \(x) x[[y]])) #%>%  

out<-
map(seq_along(out), \(x) #each response
    map(1:length(questions), \(y)  #each country
    out[[x]][[y]] %>% 
    kableExtra::kable( align = "l",
                         caption = paste0("Non-local and local responses for section 1 in country ", y, "."),
                         col.names = 
                           c("Response category",
                             c("Wildlife health is important",
                             "Human and livestock pathogens can impact wildlife health",
                             "Wildlife pathogens can impact human health",
                             "Wildlife pathogens can impact livestock health")[x], 
                             "Percentage"),
                         escape=F, format = "html") %>% 
    column_spec(1, width = "2cm") %>% 
    column_spec(2:3, width = c(rep("3cm", 2))) %>% 
    kable_styling() %>% 
    collapse_rows(columns = 1, valign = "middle") %>% 
    kable_styling(c("striped", "bordered"), full_width = T)))
  
  
# out[[1]][[3]] #first response country 3
# out[[1]][[4]] #first response country 4
# out[[2]][[3]] 




#mget(paste0("section_1", "_table", 1:nrow(compare_global_versus_local)))