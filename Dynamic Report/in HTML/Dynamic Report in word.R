#####################Creating Dynamic Repors with R and Markdowns#################################
#R Markdown templates can be used to create HTML, PDF, and MS Word documents. 
#ODT and DOCX tem- plates are used to create Open Document and Microsoft Word documents, respectively. 
#LaTeX templates are used to create publication-quality PDF documents, including reports, articles, and books


## With R and Microsoft word
install.packages("R2wd")
install.packages("RDCOMClient")
library(R2wd)
library(RDCOMClient)

require(R2wd)
require(car) 

df<- Salaries
n <- nrow(df)
fit <- lm(salary~rank*sex, data = df)
aovTable <- Anova(fit, type = 3)
aovTable <- round(as.data.frame(aovTable), 3)
aovTable[is.na(aovTable)] <- ""

wdGet("C:/Users/Crystal/Desktop/Sample Report1.docx", method = "RDCOMClient")
wdGoToBookmark("n")
wdWrite(n)

wdGoToBookmark("aovTable")
wdTable(aovTable, caption = "Two-way Analysis of Variance",
        caption.pos = "above", pointsize = 12, autoformat = 4)

wdGoToBookmark("effectsPlot")
myplot <- function(){
  require(effects)
  par(mar = c(2,2,2,2))
  plot(allEffects(fit), main = "")
}

wdPlot(plotfun = myplot, caption = "Mean Effects Plot",
       height = 4, width = 5, method = "metafile")
wdSave("C:/Users/Crystal/Desktop/Sample Report1.docx")
wdQuit()





