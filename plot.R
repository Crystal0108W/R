install.packages("gcookbook", type = "source")
library(gcookbook)

csub <- subset(climate, Source == "Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >=0

library(ggplot2)
ggplot(csub, aes(x = Year, y = Anomaly10y, fill = pos)) + 
  geom_bar(stat = 'identity', position = 'identity') + 
  guides(fill = "none")

tophit <- tophitters2001[1:25,]
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
tophit$name <- factor(tophit$name, levels = nameorder)


ggplot(tophit, aes(x = avg, y = name)) + 
  geom_segment(aes(yend=name), xend = 0, colour = "grey")+
  geom_point(size = 2, aes(colour = lg))  + 
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(1,0.9),
        legend.justification = c(1, 0.5))


sunspotyear <- data.frame(
  Year = as.numeric(time(sunspot.year)),
  Sunspots = as.numeric(sunspot.year)
)

ggplot(sunspotyear, aes(x = Year, y = Sunspots)) + 
  geom_area(colour = "orangered1", fill = "orangered2") + 
  labs(title = "Sunspots Change ove Time") + 
  theme(plot.title = element_text(hjust = 0.5))


p <- ggplot(subset(climate, Source == "Berkeley"), aes(x = Year, y = Anomaly10y)) + 
  geom_line()

p + annotate("rect", xmin = 1950, xmax = 2000, ymin = -1, ymax = 1, alpha = .5,
             fill = "lightpink")

ggplot(wind, aes(x = DirCat, fill = SpeedCat)) + 
  geom_histogram(binwidth = 15, boundary = -7.5, colour = "white", size = .25) + 
  guides(fill = guide_legend(reverse = FALSE)) + 
  coord_polar() + 
  scale_x_continuous(limits = c(0,360), breaks = seq(0, 360, by = 45),
                     minor_breaks = seq(0, 360, by=15)) +
  scale_fill_brewer() + 
  labs(title = "Polar Plot") + 
  theme(plot.title = element_text(hjust = 0.5))


cb <- subset(climate, Source == "Berkeley")

cb$valence[cb$Anomaly10y >= 0] <- 'pos'
cb$valence[cb$Anomaly10y < 0] <- 'neg'

ggplot(cb, aes(x = Year, y = Anomaly10y)) + 
  geom_area(colour = "white", aes(fill = valence)) + 
  geom_line() + 
  geom_hline(yintercept = 0) + 
  guides(fill = FALSE)


# Network Graph
install.packages("igraph")
library(igraph)

g <- graph.data.frame(madmen2, directed = TRUE)
par(mar = c(0,0,0,0))

plot(g, layout = layout.fruchterman.reingold, vertex.size = 8, edge.arrow.size = 0.5)

g <- graph.data.frame(madmen2, directed = FALSE)
plot(g,layout = layout.circle, vertex.size = 8, vertex.label = NA)


install.packages("maps")
library(maps)

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
states_map <- map_data("state")
crime_map <- merge(states_map, crimes, by.x = "region", by.y = "state")

library(plyr)
crime_map <- arrange(crime_map, group, order)

install.packages("mapproj")
library(mapproj)

ggplot(crimes, aes(map_id = state, fill = Assault)) + 
  geom_map(map = states_map, colour = "white") + 
  scale_fill_gradient2(low = "hotpink1", mid = "grey90", high = "turquoise2",
                       midpoint = median(crimes$Assault)) + 
  expand_limits(x = states_map$long, y = states_map$lat) + 
  coord_map("polyconic") + 
  labs(title = "Crime in the States") + 
  theme(plot.title = element_text(hjust = 0.5))

# High-Density Scatter Plots
library(hexbin)

set.seed(1324)
n <- 10000

c1 <- matrix(rnorm(n, mean = 0, sd = .5), ncol = 2)
c2 <- matrix(rnorm(n, mean = 3, sd = 2),ncol = 2)
mydata <- rbind(c1,c2)
mydata <- as.data.frame(mydata)

names(mydata) <- c('x', 'y')
with(mydata, {
  bin <- hexbin(x,y, xbins = 50)
  plot(bin, main = "Hexagonal Binning with 10,000 Obsrevations")
})

par(mar = c(3,6,3,6))
with(mydata, smoothScatter(x,y, main = "Scatter Plot Colored by Smoothed Densities"))



