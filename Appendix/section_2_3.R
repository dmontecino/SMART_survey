

##-- SECTION 3
data<-c("global_data", "local_data")
questions<-c("dom_animals_concern",
             "dom_animals_in_pa",
             "dom_animals_recorded",
             "dom_animal_health_status_recorded")


data_from_countries<-vector(mode = "list", 
                            length = nrow(compare_global_versus_local))


for(i in 1:nrow(compare_global_versus_local)){ # countries
  
  out<-  
    map(seq_along(questions), \(y) 
        map(data, \(x)
            compare_global_versus_local %>% 
              slice(i) %>%
              unnest(x) %>% 
              mutate(across(questions[y], str_to_sentence)) %>% 
              mutate(dom_animals_concern= 
                       factor(dom_animals_concern, 
                              levels=c("Strongly agree",
                                       "Agree", 
                                       "Somewhat agree", 
                                       "Neutral", 
                                       "Somewhat disagree",
                                       "Disagree", 
                                       "Strongly disagree"))) %>%  
              mutate(dom_animal_health_status_recorded=
                       if_else(dom_animal_health_status_recorded=="",
                               NA,
                               dom_animal_health_status_recorded)) %>% 
              mutate(dom_animals_recorded=
                       if_else(dom_animals_recorded=="",
                               NA,
                               dom_animals_recorded)) %>%          
              count(across(questions[y])) %>% 
              mutate(percentage=round(n/sum(n)*100,2)) %>%
              add_column("response_type" = rep(if_else(x=="local_data", "Local", "Non-local"),
                                               nrow(.)), .before = 1) %>%
              mutate(percentage = paste0(percentage, "% (n=", n, ")")) %>%
              arrange(-n, across(y)) %>%
              select(-n)))
  
  
  out<-map(out, \(x) do.call(rbind, x))
  
  data_from_countries[[i]]<-out    
  
}
  
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
                                   c("Domestic animals are a conservation concern",
                                     "Domestic animals are found in the protected area",
                                     "Domestic animals recorded if found during a patrol",
                                     "Domestic animal health status recorded")[x], 
                                   "Percentage"),
                               escape=F, format = "html") %>% 
            column_spec(1, width = "2cm") %>% 
            column_spec(2:3, width = c(rep("3cm", 2))) %>% 
            kable_styling() %>% 
            collapse_rows(columns = 1, valign = "middle") %>% 
            kable_styling(c("striped", "bordered"), full_width = T)))




#mget(paste0("section_1", "_table", 1:nrow(compare_global_versus_local)))