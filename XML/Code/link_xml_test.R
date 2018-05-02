#when manuscripts are submitted, they are assigned a unique manuscript number which follows them until they are either accepted or rejected from the system (either full rejection, or reject with resubmission). If the manuscript is then revised and resubmitted, they are assigned a new unique manuscript number and the manuscript number from the previous version is entered as a "related manuscript". This cycle can repeat multiple times until a manuscripts author either gives up or the manuscript is accepted. The problem is that only ONE "related manuscript" is provided, regardless of the number of previous iterations. In order to prevent double/triple counting authors, I need a way to link all versions of a manuscript through the process. This involves tracing a given "related manuscript" number to the version assigned that as a manuscript number, then checking if that manuscript has a "related manuscript" number to trace back, and so on and so forth. Below is my attempt at this function. Key sticking points are that 1) R doesn't like the output of my 'if()' statement 2)I can't figure out how to recycle a value within a repeat 3) am I even doing this right?

all_manu <- read_csv("Processed_Data/all_manu.csv")#import csv 

input_df <- all_manu %>% select(related.manu, manuscript.number) %>% #select only columns needed to test function (save as input_df to simplify troubleshooting my function)
  dplyr::distinct() %>%  #filter out duplicate entries
  mutate(revised.manu.number = link_manu(input_df, related.manu, manuscript.number)) #add new column using link_manu function to trace back the manuscripts to previously submitted versions (each of which have a different manuscript number) & then populate the column with a new, unique identifier (revised.manu.number) that combines all relevant manu.numbers.

link_manu <- function(input_df, input_related_manu, input_manu){

  repeat{#The following steps will be repeated until the if statement evaluates TRUE
    
    revised.manu.number <- input_manu #idea is to temporarily store & rewrite revised.manu.number until the final linked manuscript has been traced
    
  if(input_related_manu == "NA"){
    paste(revised.manu.number);#if there is not a related manu, then paste the manuscript number in the revised.manu column
    break}
    else #if there is a related manu, then repeat the following steps
            
      x <- str_which(input_df$manuscript.number, input_related_manu) #identify & returns row(s) with matching string in the manuscript.number column, assign as x 
      
      y <- input_df[x,] %>% pluck("related.manu") #go to row x, pull the value in the related. manu column, assign as y
      
      input_related_manu <- y #reassign the input_related_manu as y inorder to repeat the function, also meant to function as a temporary store/rewrite
      
      revised.manu.number <- case_when(
        y != "NA" ~ paste(";", y) %>% append(revised.manu.number, .), #if y is a true value, then paste a semi-colon in front of the value & append that true value to the end of the unique value being built
        y == "NA" ~ paste(revised.manu.number))#if y reaches a point that it becomes NA, then re-return the same value 
      }
  
  return(revised.manu.number)#populate the column with the revised.manu.number
}
