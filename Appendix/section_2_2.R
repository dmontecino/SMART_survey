

##-- SECTION 2

#dead and sicj and injured found

data<-c("global_data", "local_data")

questions1<-c("dead_found",
             "sick_injured_found")

questions2<-c("dead_wl_recorded",
             "sick_wl_recorded",
             "injured_wl_recorded")

data_from_countries<-vector(mode = "list", 
                            length = nrow(compare_global_versus_local))

for(i in 1:nrow(compare_global_versus_local)){
  #  for(i in 1){

  
out<-
map(questions1, \(y) 
    map(data, \(x) 
        compare_global_versus_local %>% 
        slice(i) %>% 
        unnest(x) %>% 
        select(country, all_of(y)) %>% 
        select(-country) %>% 
        count(across(y)) %>% 
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
  map(1:length(questions1), \(y) 
      map(data_from_countries, \(x) x[[y]])) #%>%  

out<-
  map(seq_along(out), \(x) #each response
      map(1:nrow(compare_global_versus_local), \(y)  #each country
          out[[x]][[y]] %>% 
            kableExtra::kable( align = "l",
                               caption = paste0("Non-local and local responses for section 2 in country ", y, "."),
                               col.names = 
                                 c("Response category",
                                   c("Overall encountering frequency with dead animals",
                                     "Overall encountering frequency with sick or injured animals")[x], 
                                   "Percentage"),
                               escape=F, format = "html") %>% 
            column_spec(1:3, width = "4cm")  %>% 
            # column_spec(2, width ="3cm"))) #%>% 
            kable_styling() %>% 
            collapse_rows(columns = 1, valign = "middle") %>% 
            kable_styling(c("striped", "bordered"), full_width = T)))









#dad, sick or injured animals recorded when found




data_from_countries<-vector(mode = "list", 
                            length = nrow(compare_global_versus_local))

for(i in 1:nrow(compare_global_versus_local)){
  #  for(i in 1){
  
  
  out2<-
    map(questions2, \(y) 
        map(data, \(x) 
            compare_global_versus_local %>% 
            slice(i) %>%
            unnest(x) %>% 
            select(country, all_of(y)) %>%
            count(across(y)) %>% 
            mutate(percentage=round(n/sum(n)*100,2)) %>% 
            add_column("response_type" = rep(if_else(x=="local_data", "Local", "Non-local"), 
                                             nrow(.)), .before = 1) %>%  
            mutate(percentage = paste0(percentage, "% (n=", n, ")")) %>% 
            arrange(-n, across(y)) %>% 
            select(-n)))

  
  out2<-map(out2, \(x) do.call(rbind, x))
  
  data_from_countries[[i]]<-out2       
  
  
}      


out2<-
  map(1:length(questions2), \(y) 
      map(data_from_countries, \(x) x[[y]])) #%>%  

out2<-
  map(seq_along(out2), \(x) #each response
      map(1:nrow(compare_global_versus_local), \(y)  #each country
          out2[[x]][[y]] %>% 
            kableExtra::kable( align = "l",
                               caption = paste0("Non-local and local responses for section 2 in country ", y, "."),
                               col.names = 
                                 c("Response category",
                                   c("Dead wildlife recorded if found during a patrol",
                                     "Sick wildlife recorded if found during a patrol",
                                     "Injured wildlife recorded if found during a patrol")[x], 
                                   "Percentage"),
                               escape=F, format = "html") %>% 
            column_spec(1:3, width = "4cm")  %>% 
            kable_styling() %>% 
            collapse_rows(columns = 1, valign = "middle") %>% 
            kable_styling(c("striped", "bordered"), full_width = F)))


