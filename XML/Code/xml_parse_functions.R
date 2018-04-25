library(XML)
library(tidyverse)
library(lubridate)
setwd("~/GitHub/Notebook/XML")

#need to assign a version to referee data/outcomes so that I can track them by revision - use dates where if review return falls between submitted & decision date then its assigned that version
assign_version <- function(x, version_meta) {

  f_to_date <- function(x){
    ymd_hms(as.character(x)) #convert to characters and read as dates
  }

  x <- f_to_date(x) #ensure that all inputs are converted to characters and read as dates

  case_when(
    x >= f_to_date(version_meta[1,"submitted.date"]) & x <= f_to_date(version_meta[1,"decision.date"]) ~ as.character(0), #Intital version
    x >= f_to_date(version_meta[2,"submitted.date"]) & x <= f_to_date(version_meta[2,"decision.date"]) ~ as.character(1),
    x >= f_to_date(version_meta[3,"submitted.date"]) & x <= f_to_date(version_meta[3,"decision.date"]) ~ as.character(2),
    x >= f_to_date(version_meta[4,"submitted.date"]) & x <= f_to_date(version_meta[4,"decision.date"]) ~ as.character(3),  
    x >= f_to_date(version_meta[5,"submitted.date"]) & x <= f_to_date(version_meta[5,"decision.date"]) ~ as.character(4), #Highest version level is 4
    x <= f_to_date(version_meta[5,"submitted.date"]) ~ as.character(NA) #>5 versions is assigned a value of NA.
    )
}

#parse XML doc & find the root node before running further parsing functions
get_top <- function(input_XML){
  xmldoc1 <- xmlParse(input_XML)
  xmltop <- xmlRoot(xmldoc1)
  return(xmltop)
}  

#generate a named column from scraped xml data of a single type, use tryCatch to avoid errors for blank XML nodes
get_column <- function(input_xml, node, newname){
  column <- tryCatch(
    setNames(xmlToDataFrame(nodes = getNodeSet(input_xml, node)), newname),
    error = function(e) {setNames(as.data.frame("NA"), newname)} #if nothing present, return NA value in a dataframe
  )
}

#get the people involved in the manuscript, assign their roles, identify by manuscript number
parse_people <- function(input_xmltop){
  
  #Manuscript identifiers and person data
  manu_number <- get_column(input_xmltop, "//manuscript-number", "manu.number")

  author_id <- get_column(input_xmltop, "//author-person-id", "person.id")
  author_corr <- get_column(input_xmltop, "//is-corr", "author.corres")
  author_seq <- get_column(input_xmltop, "//author-seq", "author.seq")

  editor_id <- get_column(input_xmltop, "//editor-person-id", "person.id")
  sen_editor_id <- get_column(input_xmltop, "//senior-editor-person-id", "person.id")
  reviewer_id <- get_column(input_xmltop, "//referee-person-id", "person.id")
 
  #person identifiers and demographic data
  person_df <- xmlSApply(input_xmltop[[2]], function(x) xmlSApply(x, xmlValue)) #find all values in the "person" node 
  person_data <- cbind(data.frame(manu_number %>% head(n=1)), data.frame(t(person_df),row.names=NULL)) #convert into useable dataframe & add manuscript number identifier
  
  #pool person data & demographics by first assigning each individual a "role"
  role <- c("author", "editor", "senior_editor", "reviewer")

  authors <- cbind(data.frame(role[1]), author_id, author_corr, author_seq) %>% #combine author data (id, if corresponding, and sequence), add column to designate role as author
    rename(role = role.1.) %>% #rename
    filter(author.seq == "1" | author.corres == "true") #restrict to only first and/or corresponding authors
 
  editor <- cbind(data.frame(role[2]), person.id = editor_id) %>% rename(role = role.2.) #combine editor data

  senior_editor <- cbind(data.frame(role[3]), person.id = sen_editor_id) %>% rename(role = role.3.) #combine senior editor data

  reviewers <- cbind(data.frame(role[4]), person.id = reviewer_id) %>% rename(role = role.4.) #combine reviewer data

  #dataframe of people, their roles and demographics
  people <- list(authors, editor, senior_editor, reviewers) %>% 
    reduce(full_join, by = c("person.id", "role")) %>% #merge author, editor, senior editor & reviewer ids & roles
    left_join(person_data, by = "person.id") %>% #merge ids and roles with the identifying data using person.id
    dplyr::distinct() %>% #filter duplicate entries (issue if multiple versions)
    filter(role != "NA", person.id != "NA") #filter any persons who don't have an assigned role in the manuscript or any NA person ids (e.g., if no reviewers)
  
  return(people)
}

#parse manuscript and version metadata, reviewer decisions, identify by manuscript number
parse_manu <- function(input_xmltop){
    
  xmlmanu <- input_xmltop[[1]] #specify manuscript data
    
  #parse production data node to identify doi and date ready for production
  xmlprod <- xmlmanu[[2]] #specifiy production data node
  prod_df <- xmlSApply(xmlprod, function(x) xmlSApply(x, xmlValue)) %>% #pull all xml values in node (returns as lists in df)
    sapply(unlist) %>% sapply(function(x) if_else(any(is.null(x)), "NA", x)) #replace null values with "NA" - enables maintenance of a dataframe for later join
  prod_data <- data.frame(doi = prod_df["production-data-doi"], 
                          ready.for.production.date = prod_df["production-data-ready-for-production-date"],
                          stringsAsFactors = FALSE) #select doi & date

  #Manuscript identifiers and categorical data
  xmlvers1 <- xmlmanu[[3]] #specify version 1 data
  manu_df <- xmlSApply(xmlvers1, function(x) xmlSApply(x, xmlValue)) #pull values within node
    manu_data <- data.frame(manuscript.number = manu_df$`manuscript-number`, 
                             category = manu_df$category, 
                          manuscript.type = manu_df$`manuscript-type`,
                          submission.date = manu_df$`submission-date`,
                          is.resubmission = manu_df$is_resubmission,
                          stringsAsFactors = FALSE) #select relevant columns

  #version specific information
  key <- get_column(input_xmltop, "//key", "key")
  version <- get_column(input_xmltop, "//version-number", "version")
  submitted_date <- get_column(input_xmltop, "//version/submission-date", "submitted.date")
  decision_date <- get_column(input_xmltop, "//version/decision-date", "decision.date")
  decision <- get_column(input_xmltop, "//version/ejp-decision", "EJP.decision")
  related_manu <- get_column(input_xmltop, "//related-manuscript-number-from", "related.manu")
  number_authors <- xmlToDataFrame(nodes = getNodeSet(input_xmltop, "//author-person-id")) %>% n_distinct()
    
  reviewer_id <- get_column(input_xmltop, "//referee-person-id", "person.id")
  reviewer_recommendation <- get_column(input_xmltop, "//referee-recommendation", "review.recommendation")
  reviewer_start <- get_column(input_xmltop, "//referee-started-date", "review.start")
  reviewer_return <- get_column(input_xmltop, "//referee-received-date", "review.return")
    
  #join version data
  version_meta <- cbind(data.frame(manu_data$manuscript.number, stringsAsFactors = FALSE), 
                        version, submitted_date, decision_date, decision, related_manu, number_authors, stringsAsFactors = FALSE) %>% 
    mutate(days.to.decision = as.duration(ymd_hms(submitted.date) %--% ymd_hms(decision.date))/ddays(1)) %>%
    rename(manuscript.number = manu_data.manuscript.number,
           number.authors = number_authors) %>% 
    select(version, submitted.date, decision.date, EJP.decision, days.to.decision, manuscript.number, related.manu, number.authors)
  
  #dataframe of manuscript meta data
  manu_meta <- cbind(prod_data,manu_data)

  #join referee data
  review_outcome <- cbind(data.frame(manu_data$manuscript.number, stringsAsFactors = FALSE), 
                          reviewer_id, reviewer_recommendation, reviewer_start, reviewer_return, stringsAsFactors = FALSE) %>% 
    mutate(days.to.review = as.duration(ymd_hms(review.start) %--% ymd_hms(review.return))/ddays(1),
           version.reviewed = assign_version(review.return, version_meta)) %>% 
    rename(manuscript.number = manu_data.manuscript.number)

  #full join of manuscript meta data & decisions
  manu_data <- list(version_meta, manu_meta, review_outcome) %>% 
    reduce(full_join, by = "manuscript.number")
  
  return(manu_data)
}

get_xml_to_people <- function(input_xmlfile){
  get_top(input_xmlfile) %>% parse_people()
}
