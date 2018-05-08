library(tidyverse)
all_manu <- read_csv("Processed_Data/all_manu.csv")#import csv 

input_df <- all_manu %>% select(related.manu, manuscript.number) %>% #select only columns needed to test function (save as input_df to simplify troubleshooting my function)
  dplyr::distinct() %>%  #filter out duplicate entries
  mutate(revised.manu.number = link_manu(input_df, related.manu, manuscript.number)) #add new column using link_manu function to trace back the manuscripts to previously submitted versions (each of which have a different manuscript number) & then populate the column with a new, unique identifier (revised.manu.number) that combines all relevant manu.numbers.

link_manu <- function(input_df, input_related_manu, input_manu){

  repeat{#The following steps will be repeated until the if statement evaluates TRUE
    
  if(is.na(input_related_manu)){
    revised.manu.number <- input_manu #idea is to temporarily store & rewrite revised.manu.number until the final linked manuscript has been traced
    paste(revised.manu.number);#if there is not a related manu, then paste the manuscript number in the revised.manu column
    break}
    else #if there is a related manu, then repeat the following steps
            
      x <- str_which(input_df$manuscript.number, input_related_manu) #identify & returns row(s) with matching string in the manuscript.number column, assign as x 
      
      y <- input_df[x,] %>% pluck("related.manu") #go to row x, pull the value in the related. manu column, assign as y
      
      input_related_manu <- y #reassign the input_related_manu as y inorder to repeat the function, also meant to function as a temporary store/rewrite
      
      revised.manu.number <- case_when(
        y != "NA" ~ paste(";", y) %>% append(revised.manu.number, .), #if y is a true value, then paste a semi-colon in front of the value & append that true value to the end of the unique value being built
        is.na(y) ~ paste(revised.manu.number))#if y reaches a point that it becomes NA, then re-return the same value 
      }
  
  return(revised.manu.number)#populate the column with the revised.manu.number
}

#I'm only getting the manu.number back as the revised.manu.number. I think that's because it's been assigned in the beginning & isn't being re-written or the re-writing is being lost b/c of assigning the input_manu to it. So... how do I re-write it such that I don't lose the assignment? Flip it to a while statement?


