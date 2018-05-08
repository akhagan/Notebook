inner_join(input_df, input_df, by=c("related.manu"="manuscript.number")) %>% inner_join(., input_df, by=c("related.manu"="manuscript.number")) %>% View()
  select(manuscript.number, starts_with(“related.manu”)) %>% distinct()

full_join(input_df, input_df, by=c("related.manu"="manuscript.number")) %>% full_join(., input_df, by=c("related.manu"="manuscript.number")) %>% View()
  select(manuscript.number, starts_with("related.manu")) %>% distinct() %>% unite(sep = ";")

left_join(input_df, input_df, by=c("related.manu"="manuscript.number")) %>% left_join(., input_df, by=c("related.manu"="manuscript.number")) %>% View()  


join_one <- input_df %>% filter(is.na(related.manu)) %>% 
  select(manuscript.number) %>% 
  left_join(., input_df, by = c("manuscript.number"="related.manu"))

join_two <- join_one %>%  
  filter(manuscript.number.y != "NA") %>% 
  left_join(., input_df, by = c("manuscript.number.y"="related.manu")) 

one_unite <- join_one %>% filter(is.na(manuscript.number.y)) %>% unite(sep = ";")

two_unite <- join_two %>% filter(is.na(manuscript.number.y.y)) %>% unite(sep = ";")

complete <- rbind(one_unite, two_unite)

tes <- function(x) {

    loop_results <- list()
    i=0
    repeat
    {
      x %>% 
        filter(is.na(related.manu)) %>% 
        select(manuscript.number) %>% 
        left_join(., input_df, by = c("manuscript.number"="related.manu"))
      loop_results[[i]] <- list(a=a, b=b, c=c)
      if (all(is.na(c(NA, NaN))))) {break}
    }
  }
  return(list(a=a, b=b, c=c, loop_results=loop_results))
}
