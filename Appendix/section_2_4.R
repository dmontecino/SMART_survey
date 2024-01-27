



##-- SECTION 3

data<-c("global_data", "local_data")


dead_in_smart<-vector(mode = "list", 
                            length = nrow(compare_global_versus_local))


for(i in 1:nrow(compare_global_versus_local)){ # countries
  
  out<-  
    #map(seq_along(questions1), \(y)
        map(data, \(x)
            compare_global_versus_local %>% 
              slice(i) %>%
              unnest(x) %>%  
              select(dead_wl_recorded, dead_wl_data_in_smart) %>% 
              mutate(dead_wl_data_in_smart=ifelse(grepl("All", dead_wl_data_in_smart), "All", dead_wl_data_in_smart)) %>% 
              mutate(dead_wl_data_in_smart=ifelse(grepl("Some", dead_wl_data_in_smart), "Some", dead_wl_data_in_smart)) %>% 
              mutate(dead_wl_data_in_smart=ifelse(grepl("None", dead_wl_data_in_smart), "None", dead_wl_data_in_smart)) %>% 
              filter(dead_wl_recorded=="Yes") %>% 
              count(dead_wl_data_in_smart) %>% 
              mutate(percentage=round(n/sum(n)*100,2)) %>%
              mutate(dead_wl_data_in_smart = factor(dead_wl_data_in_smart, 
                                                    levels=c("All", "Some", "None"))) %>% 
              add_column("response_type" = rep(if_else(x=="local_data", "Local", "Non-local"),
                                               nrow(.)), .before = 1) %>%
              mutate(percentage = paste0(percentage, "% (n=", n, ")")) %>%
              arrange(-n, dead_wl_data_in_smart) %>%
              select(-n))
  
  
  out<-do.call(rbind, out)
  
  dead_in_smart[[i]]<-out    
  
}


dead_in_smart<-
  # map(seq_along(out), \(x) #each response
      map(1:length(dead_in_smart), \(y)  #each country
          dead_in_smart[[y]] %>% 
            kableExtra::kable( align = "l",
                   caption = paste0("Non-local and local responses for section 4 in country ", y, "."),
                   col.names = 
                     c("Response category",
                       "Data from dead wildlife in SMART",
                       "Percentage"),
                   escape=F, format = "html") %>% 
            column_spec(1, width = "2cm") %>% 
            column_spec(2:3, width = c(rep("3cm", 2))) %>% 
            kable_styling() %>% 
            collapse_rows(columns = 1, valign = "middle") %>% 
            kable_styling(c("striped", "bordered"), full_width = T))








sick_in_smart<-vector(mode = "list", 
                      length = nrow(compare_global_versus_local))


for(i in 1:nrow(compare_global_versus_local)){ # countries
  
  out<-  
    #map(seq_along(questions1), \(y)
    map(data, \(x)
        compare_global_versus_local %>% 
          slice(i) %>%
          unnest(x) %>%  
          select(sick_wl_recorded, sick_wl_data_in_smart) %>% 
          mutate(sick_wl_data_in_smart=ifelse(grepl("All", sick_wl_data_in_smart), "All", sick_wl_data_in_smart)) %>% 
          mutate(sick_wl_data_in_smart=ifelse(grepl("Some", sick_wl_data_in_smart), "Some", sick_wl_data_in_smart)) %>% 
          mutate(sick_wl_data_in_smart=ifelse(grepl("None", sick_wl_data_in_smart), "None", sick_wl_data_in_smart)) %>% 
          filter(sick_wl_recorded=="Yes") %>% 
          count(sick_wl_data_in_smart) %>% 
          mutate(percentage=round(n/sum(n)*100,2)) %>%
          mutate(sick_wl_data_in_smart = factor(sick_wl_data_in_smart, 
                                                levels=c("All", "Some", "None"))) %>% 
          add_column("response_type" = rep(if_else(x=="local_data", "Local", "Non-local"),
                                           nrow(.)), .before = 1) %>%
          mutate(percentage = paste0(percentage, "% (n=", n, ")")) %>%
          arrange(-n, sick_wl_data_in_smart) %>%
          select(-n))
  
  
  out<-do.call(rbind, out)
  
  sick_in_smart[[i]]<-out    
  
}

#sick_in_smart<-sick_in_smart[map_vec(sick_in_smart, nrow)!=0]

sick_in_smart<-
  # map(seq_along(out), \(x) #each response
  map(seq_along(sick_in_smart), \(y)  #each country
      if(nrow(sick_in_smart[[y]])>0){
    
        sick_in_smart[[y]] %>% kableExtra::kable( align = "l",
                           caption = paste0("Non-local and local responses for section 4 in country ", y, "."),
                           col.names = 
                             c("Response category",
                               "Data from sick wildlife in SMART",
                               "Percentage"),
                           escape=F, format = "html") %>% 
        column_spec(1, width = "2cm") %>% 
        column_spec(2:3, width = c(rep("3cm", 2))) %>% 
        kable_styling() %>% 
        collapse_rows(columns = 1, valign = "middle") %>% 
        kable_styling(c("striped", "bordered"), full_width = T)}) 

sick_in_smart<-sick_in_smart[c(1,3,4)]









injured_in_smart<-vector(mode = "list", 
                      length = nrow(compare_global_versus_local))


for(i in 1:nrow(compare_global_versus_local)){ # countries
  
  out<-  
    #map(seq_along(questions1), \(y)
    map(data, \(x)
        compare_global_versus_local %>% 
          slice(i) %>%
          unnest(x) %>%  
          select(injured_wl_recorded, injured_wl_data_in_smart) %>% 
          mutate(injured_wl_data_in_smart=ifelse(grepl("All", injured_wl_data_in_smart), "All", injured_wl_data_in_smart)) %>% 
          mutate(injured_wl_data_in_smart=ifelse(grepl("Some", injured_wl_data_in_smart), "Some", injured_wl_data_in_smart)) %>% 
          mutate(injured_wl_data_in_smart=ifelse(grepl("None", injured_wl_data_in_smart), "None", injured_wl_data_in_smart)) %>% 
          #filter(injured_wl_recorded=="Yes") %>% 
          count(injured_wl_data_in_smart) %>% 
          mutate(percentage=round(n/sum(n)*100,2)) %>%
          mutate(injured_wl_data_in_smart = factor(injured_wl_data_in_smart, 
                                                levels=c("All", "Some", "None"))) %>% 
          add_column("response_type" = rep(if_else(x=="local_data", "Local", "Non-local"),
                                           nrow(.)), .before = 1) %>%
          mutate(percentage = paste0(percentage, "% (n=", n, ")")) %>%
          arrange(-n, injured_wl_data_in_smart) %>%
          select(-n))
  
  
  out<-do.call(rbind, out)
  
  injured_in_smart[[i]]<-out    
  
}


injured_in_smart<-
  # map(seq_along(out), \(x) #each response
  map(seq_along(injured_in_smart), \(y)  #each country
      if(nrow(injured_in_smart[[y]])>0){
        
        injured_in_smart[[y]] %>% 
          kableExtra::kable( align = "l",
                caption = paste0("Non-local and local responses for section 4 in country ", y, "."),
                col.names = 
                  c("Response category",
                    "Data from injured wildlife in SMART",
                    "Percentage"),
                escape=F, format = "html") %>% 
          column_spec(1, width = "2cm") %>% 
          column_spec(2:3, width = c(rep("3cm", 2))) %>% 
          kable_styling() %>% 
          collapse_rows(columns = 1, valign = "middle") %>% 
          kable_styling(c("striped", "bordered"), full_width = T)}) 





injured_in_smart<-injured_in_smart[c(1,3,4)]


