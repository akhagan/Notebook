library(XML)
library(tidyverse)
library(lubridate)
setwd("~/GitHub/Notebook/XML")

#need to assign a version to referee data/outcomes so that I can track them by revision - use dates where if review return falls between submitted & decision date then its assigned that version
assign_version <- function(x, version_meta) {
  if_else((ymd_hms(as.character(x)) >= ymd_hms(as.character(version_meta[1,"Submitted_date"])) & ymd_hms(as.character(x)) <= ymd_hms(as.character(version_meta[1,"Decision_date"]))), #if review is returned after the manuscript submitted date & before the manuscript decision date
          0, #then it assigned that manuscript version, if not, then iterate through possible versions. 
          if_else((ymd_hms(as.character(x)) >= ymd_hms(as.character(version_meta[2,"Submitted_date"])) & ymd_hms(as.character(x)) <= ymd_hms(as.character(version_meta[2,"Decision_date"]))), 
                  1,
                  if_else((ymd_hms(as.character(x)) >= ymd_hms(as.character(version_meta[3,"Submitted_date"])) & ymd_hms(as.character(x)) <= ymd_hms(as.character(version_meta[3,"Decision_date"]))), 
                          3, 
                          if_else((ymd_hms(as.character(x)) >= ymd_hms(as.character(version_meta[4,"Submitted_date"])) & ymd_hms(as.character(x)) <= ymd_hms(as.character(version_meta[4,"Decision_date"]))), 
                                  4, 5)))) #>4 versions is assigned a value of NA, since intital version is 0, then highest version level is 3
}

parse_people <- function(input_XML){
  
  xmldoc1 <- xmlParse(input_XML)

#Manuscript identifiers and person data
  manu_number <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/manuscript-number")), "Manu_number")

  author_id <-setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/authors/author/author-person-id")), "ID")
  author_corr <-setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/authors/author/is-corr")), "Author_corres")
  author_seq <-setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/authors/author/author-seq")), "Author_seq")

  editor_id <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/editors/editor/editor-person-id")), "ID")
  sen_editor_id <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/senior-editors/senior-editor/senior-editor-person-id")), "ID")
  referee_id <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/referees/referee/referee-person-id")), "ID")
 
#person identifiers and demographic data
  person_id <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/people/person/person-id")), "ID")
  person_country <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/people/person/addresses/address/address-country")), "Home_country")
  person_name <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/people/person/first-name")), "Name")
  person_gender <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/people/person/gender")), "Gender")
  person_gender_prob <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/people/person/gender-probability")), "Gender_prob")
  person_title <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/people/person/title")), "Title")

#pool person data & demographics by first assigning each individual a "role"
  role <- c("Author", "Editor", "Senior_Editor", "Reviewer")

  person_data <- cbind(data.frame(manu_number %>% head(n=1)), person_id, person_country, person_gender, person_gender_prob, person_name, person_title)
  
  authors <- cbind(data.frame(role[1]), author_id, author_corr, author_seq) %>% #combine author data (id, if corresponding, and sequence), add column to designate role as author
    rename(Role = role.1.) #rename
 
  editor <- cbind(data.frame(role[2]),editor_id) %>% rename(Role = role.2.)

  senior_editor <- cbind(data.frame(role[3]), sen_editor_id) %>% rename(Role = role.3.)

  reviewers <- cbind(data.frame(role[4]), referee_id) %>% rename(Role = role.4.)

#dataframe of people, their roles and demographics
  people <- list(authors, editor, senior_editor, reviewers) %>% 
    reduce(full_join, by = c("ID", "Role")) %>% 
    full_join(person_data, by = "ID") %>% 
    dplyr::distinct() %>% #filter duplicate entries (issue if multiple versions)
    filter(Role != "NA")
  
  return(people)
}

  
parse_manu <- function(input_XML){
    
    xmldoc1 <- xmlParse(input_XML)
    
    #Manuscript identifiers and categorical data
    doi <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/production-data/production-data-doi")), "DOI")
    manu_number <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/manuscript-number")), "Manu_number")
    resub_status <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/is_resubmission")), "Resubmission")
    category <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/category")), "Category")
    manu_type <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/manuscript-type")), "Type")
    
    #version specific information
    key <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/key")), "Key")
    version <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/version-number")), "Version")
    submitted_date <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/submission-date")), "Submitted_date")
    decision_date <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/decision-date")), "Decision_date")
    decision <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/ejp-decision")), "EJP_decision")
    
    referee_id <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/referees/referee/referee-person-id")), "ID")
    referee_recommendation <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/referees/referee/referee-recommendation")), "Review_recommendation")
    referee_start <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/referees/referee/referee-started-date")), "Review_start")
    referee_return <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/referees/referee/referee-received-date")), "Review_return")
    
#join version data
  version_meta <- cbind(manu_number %>% head(n=1), version, submitted_date, decision_date, decision) %>% 
    mutate(Time_to_decision = ymd_hms(Decision_date) - ymd_hms(Submitted_date))
  
#dataframe of manuscript meta data
  manu_meta <- cbind(manu_number, manu_type, category, resub_status, doi) %>% 
   head(n=1)

#join referee data
  review_outcome <- cbind(data.frame(manu_number %>% head(n=1)), referee_id, referee_recommendation, referee_start, referee_return) %>% 
    mutate(Time_in_review = ymd_hms(Review_return) - ymd_hms(Review_start),
          Version_reviewed = assign_version(Review_return, version_meta))

#full join of manuscript meta data & decisions
  manu_data <- list(version_meta, manu_meta, review_outcome) %>% 
    reduce(full_join, by = "Manu_number")
  
  return(manu_data)
}
