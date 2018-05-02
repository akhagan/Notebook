xmldoc1 <- xmlParse("Files/AEM00003-14.xml")

manu_number <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "//manuscript-number")), "Manu_number")

author_id <-setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/authors/author/author-person-id")), "ID")
author_corr <-setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/authors/author/is-corr")), "Author_corres")
author_seq <-setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/authors/author/author-seq")), "Author_seq")

editor_id <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/editors/editor/editor-person-id")), "ID")
sen_editor_id <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/senior-editors/senior-editor/senior-editor-person-id")), "ID")
referee_id <- setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/referees/referee/referee-person-id")), "ID")

xmldoc2 <- read_xml("Files/AEM00003-14.xml")

xmldoc3 <- xmlTreeParse("Files/AEM00003-14.xml", useInternalNodes = TRUE)
top3 <- xmlRoot(xmldoc3)

manu_test <- set_names(xml_text(xml_find_all(top3, "//manuscript-number")), "manu_number")

manu_2 <- setNames(xml_text(xml_find_first(xmldoc2, "//manuscript-number")), "Manu_number")

xmlSApply(top3[[2]], xmlValue)

person 
"United States2016-08-16  15:52:53Primary WorkLARRYunknownn/an/aJ5197Prof." 

manu_3 <-xmlToDataFrame(nodes=getNodeSet(top3, "//manuscript-number"))
country <- xmlToDataFrame(nodes=getNodeSet(top3, "//country"))


dataFrame <- xmlSApply(top3[[1]][2], function(x) xmlSApply(x, xmlValue))
data.frame(t(dataFrame),row.names=NULL) %>% View()

AEM02150_16_top <- get_top("Files/AEM02150-16.xml")
AEM02150_16_people <- AEM02150_16_top %>% parse_people()
AEM02150_16_manu <- AEM02150_16_top %>% parse_manu()

AEM03057_16_top <- get_top("Files/AEM03057-16.xml")
AEM03057_16_people <- AEM03057_16_top %>% parse_people()
AEM03057_16_manu <- AEM03057_16_top %>% parse_manu()

doc <- xmlRoot(xmlTreeParse("Files/AEM02150-16.xml"))
table(names(doc))
xmlApply(AEM02150_16_top[[1]], names)
AEM_manu <- AEM02150_16_top[[1]]
AEM_prod <- AEM_manu[2]
fields <- xpathSApply(AEM_manu, "//production-data", names)
table(sapply(fields, identical, fields[[1]])) %>% nrow()

size <- xpathSApply(AEM_manu, "//production-data", xmlSize)
ans <- as.data.frame(fields$V1, stringsAsFactors = FALSE)

#automating 
xml_list <- c("Files/AEM02917-14.xml")

setwd("~/GitHub/Notebook/XML/Files")
all_xml_list <- list.files(pattern="*.xml")
xml_top_list <- lapply(xml_list, get_top)
all_people <- map_df(xml_top_list, parse_people)
all_manu <- map_df(xml_top_list, parse_manu)

top_norev <- get_top("Files/AEM02130-14.xml")


