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

> xmlSApply(top3[[2]], xmlValue)

person 
"United States2016-08-16  15:52:53Primary WorkLARRYunknownn/an/aJ5197Prof." 

manu_3 <-xmlToDataFrame(nodes=getNodeSet(top3, "//manuscript-number"))
country <- xmlToDataFrame(nodes=getNodeSet(top3, "//country"))

dataFrame <- xmlSApply(top3[[2]], function(x) xmlSApply(x, xmlValue))
cbind(data.frame(manu_3[1,]), data.frame(t(dataFrame),row.names=NULL)) %>% View()

dataFrame <- xmlSApply(top3[[1]], function(x) xmlSApply(x, xmlValue))
data.frame(t(dataFrame),row.names=NULL) %>% View()