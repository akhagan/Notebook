library(XML)

#Parse XML file & get the root node (huh?)
#root node is the origin of data for a cascade of nodes
#any node is a root node, relative to what is below
xmldoc1 <- xmlParse("XML/Files/AEM00003-14.xml")
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
xml_text(xmldoc1, "//country")
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

