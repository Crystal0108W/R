#####################Creating Dynamic Repors with R and Markdowns#################################
  #R Markdown templates can be used to create HTML, PDF, and MS Word documents. 
  #ODT and DOCX tem- plates are used to create Open Document and Microsoft Word documents, respectively. 
  #LaTeX templates are used to create publication-quality PDF documents, including reports, articles, and books

## With R and Markdown
install.packages("rmarkdown")
install.packages("xtable")


library(rmarkdown)
library(xtable)
methods(xtable)

render("/Users/yw7986/Desktop/women.Rmd", "html_document")




