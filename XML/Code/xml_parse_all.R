source("Code/xml_parse_functions.R") #source functions & libraries

setwd("~/GitHub/Notebook/XML/Files") #set wd to pull & parse xml files

all_xml_list <- list.files(pattern="*.xml") #generate list of xml files

xml_top_list <- lapply(all_xml_list, get_top) #parse & get top root of all xml files

setwd("~/GitHub/Notebook/XML") #change wd back

all_people <- map_df(xml_top_list, parse_people) #parse for people info

all_manu <- map_df(xml_top_list, parse_manu) #parse for manuscript info

#Next steps: link manuscript versions between XML files, pull list of names for genderize.io, run through genderize.io then link back

write_csv(all_people, "Processed_Data/all_people.csv", col_names = TRUE)

write_csv(all_manu, "Processed_Data/all_manu.csv", col_names = TRUE)
