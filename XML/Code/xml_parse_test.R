library(XML)

#Parse XML file & get the root node (huh?)
#root node is the origin of data for a cascade of nodes
#any node is a root node, relative to what is below
xmldoc1 <- xmlParse("Files/AEM00003-14.xml")
rootnode <- xmlRoot(xmldoc1)
rootnode[1]

#Extract XML data
data <- xmlSApply(rootnode,function(x) xmlSApply(x, xmlValue))

#convert extracted data into a dataframe
cd.catalog <- data.frame(t(data),row.names=NULL)

#Verify the results
cd.catalog[1:2,]

#tried to generate lists of the data - didn't work
xml_list <- xmlToList(xmldoc1)
View(xml_list)

#generate data frame with specific columns
xmlToDataFrame(nodes=getNodeSet(xmldoc1,"//manuscript"))
  c("country")

#not getting what I want easily -> try xml2 package
library("xml2")
  
xmldoc1 <- read_xml("XML/Files/AEM00003-14.xml")

class(xmldoc1) #class identifed as "xml_document" "xml_node"
xml_name(xmldoc1) #apparently titled "xml" haha
xml_parent(xmldoc1)
xml_children(xmldoc1)

#attempting to extract the value of a specific variable - Country
country <- xml_find_all(xmldoc1, ".//country")
xml_attr(xmldoc1, ".//country")
xml_text(xmldoc1, ".//country")
xml_text(xml_attr(xmldoc1, ".//country"))
xml_text(xmldoc1, trim = TRUE)
xml_name(xmldoc1, "//country")
#some of the above will return <country>China</country> but I can't
# return "china"

#try from this blog: http://www.informit.com/articles/article.aspx?p=2215520
library("plyr")
#load xml w. "XML" package-> xmlParse()
#use the xmlRoot function to access the top node
xmltop <- xmlRoot(xmldoc1)
#look at the XML code of the first subnodes
print(xmltop)[1:2]
#ugh, doesn't look like it seems it should when I print... 
#is there something different about how AEM xml nodes are organized?

#going to try xml2 again:
xml_attrs(xml_find_all(xmldoc1, ".//country")) #returns [[1]] named character (0)
xml_attrs(xml_find_all(xmldoc1, ".//author-person-id")) #returns [[22]]
#so, okay, it's working, just isn't giving me "china"

#According to wikipedia, an attribute precedes an =, and is generally inside tags (aka elements). 
#If I want to isolate "country", that is content
#retry again w. XML package & figured out that xml_text returns text of full document
#nest xml_find_all: --> returns "China"
xml_text(xml_find_all(xml_doc1, "//country"))

#generate list of author ids:
author_id <- as.list(xml_text(xml_find_all(xmldoc1, "//author-person-id")))

#Next task - pull needed data for gender study into data frame/tibble  
# e.g., 1st/corresponding author IDs/gender, editor/reviewer IDs/gender, 
# reviewer recommendations, date submitted, date of final decision, doi
library(tidyverse)
pubdata <- tibble(
  doi = xml_text(xml_find_first(xmldoc1, "//production-data-doi")),
  sub_date = xml_text(xml_find_first(xmldoc1, "//author-approval-date")),
  first_auth_id = xml_text(xml_find_first(xmldoc1, "//author-person-id")), 
  editor_id = xml_text(xml_find_first(xmldoc1, "//editor-person-id"))
)

#going to try a different approach, which is to convert the XML file 
#to a dataframe, then parse out the needed data
#uses XML package
xmldata <- xmlToDataFrame("Files/AEM00003-14.xml")
# Returns: Error in `[<-.data.frame`(`*tmp*`, i, names(nodes[[i]]), value = c("China",  : 
# duplicate subscripts for columns

# testing to see what various commands get
xmldoc1 <- xmlParse("Files/AEM00003-14.xml")
keydf <-setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/key")), "Key")
authoriddf <-setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/authors/author/author-person-id")), "Author_ID")
authorcorresdf <-setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/authors/author/is-corr")), "Author_corres")
authorseqdf <-setNames(xmlToDataFrame(nodes = getNodeSet(xmldoc1, "/xml/manuscript/version/authors/author/author-seq")), "Author_seq")
authors <- cbind(authoriddf, authorcorresdf, authorseqdf)
first_auth <- authors %>% filter(Author_seq == 1) #identify first author
corres_auth <- authors %>% filter(Author_corres == "true") #identify corresponding author
