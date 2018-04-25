source("Files/xml_parse_all.R")
#if grepl("AEM01118-14", all_manu$manuscript.number) #returns true/false
#paste("manuscript.number" sep = ; )

link_manu <- function(input_df, input_related_manu, input_manu){
  
  list.manu.numbers <- input_df$manuscript.number
  list.related.manu <- input_df$related.manu
  first_related_pos <- str_match(list.manu.numbers, input_related_manu) %>% 
    second_related <- ifelse(first_related_pos != "NA", 
                             
                             case_when(
                               grepl(input_manu, list.related.manu) ~ paste(input_related_manu, input_manu, sep = ";"),
                               TRUE ~ paste(input_manu, sep = ";")
                             )
}

all_manu %>% select(related.manu, manuscript.number) %>% 
  mutate(revised.manu.number = paste(related.manu, manuscript.number, sep = ";")) %>% View()
?paste

#trace back relevant manuscript numbers


ifelse(related.manu == "NA", paste(), ifelse(grepl(related.manu, all_manu$manuscript.number), , paste())