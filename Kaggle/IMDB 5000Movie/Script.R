movie_metadata <- read.csv("/Users/yw7986/Desktop/movie_metadata.csv")
movie_metadata$genres <- as.character(movie_metadata$genres)
movie_metadata$plot_keywords <- as.character(movie_metadata$plot_keywords)


gener.df <- NULL
for (i in 1:5043) {
  temp_gener <- strsplit(movie_metadata$genres[i], "\\|")
  temp_gener <- as.data.frame(temp_gener)
  colnames(temp_gener) <- c("Gener")
  gener.df <- rbind(gener.df, temp_gener)
}
remove(temp_gener)
summary(gener.df)
gener.table <- table(gener.df)
gener.table <- as.matrix(gener.table)
gener.dftable <- as.data.frame(gener.table)
colnames(gener.dftable) <- c("Freq")
gener.dftable$Gener <- row.names(gener.dftable)
row.names(gener.dftable) <- seq(nrow(gener.dftable))
colnames(gener.dftable$Gener) <- c("Gener")
gener.dftable <- gener.dftable[, c(2,1)]

write(gener.table,file = "/Users/yw7986/Desktop/GenerTotal.csv")


plot.df <- NULL 
for (i in 1:5043) {
  temp_plot <- strsplit(movie_metadata$plot_keywords[i], "\\|")
  temp_plot <- as.data.frame(temp_plot)
  colnames(temp_plot) <- c("Plot Keyword")
  plot.df <- rbind(plot.df, temp_plot)
}
summary(plot.df)
remove(temp_plot)
plot.table <- table(plot.df)
plot.dftable <- as.data.frame(plot.table)
colnames(plot.dfplot) <- c("Freq")
plot.dftable$PlotKeywords <- row.names(plot.dftable)
row.names(plot.dftable) <- seq(nrow(plot.dftable))
colnames(plot.dftable$Plot) <- c("Plot Keyword")
plot.dftable <- plot.dftable[, c(2,1)]

write.csv(plot.dftable,file = "/Users/yw7986/Desktop/PlotKeywords.csv")


# Create WordCloud
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("RColorBrewer")
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
wordcloud2(gener.dftable, color = "white", backgroundColor = "pink", shape = "star")
letterCloud(plot.dftable, word = "PLOT", color = "white", backgroundColor="pink")


summary(gener.df)
gener.df <- as.matrix(gener.df)
write(gener.df,file = "/Users/yw7986/Desktop/GenerTotal.csv")
