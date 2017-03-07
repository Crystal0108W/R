install.packages("ggplot2")
install.packages("gridBase")
install.packages("grid")
install.packages("lattice")
install.packages("gridExtra")

library(ggplot2)
library(lattice)
library(gridExtra)

ggplot(data = mtcars, aes(x=wt, y=mpg)) + geom_point() + labs(title = "Automobile Data", x = "Weight", y = "Miles Per Gallon")
ggplot(data = mtcars, aes(x=wt, y=mpg)) + geom_point(pch = 17, color = "pink", size=2) + 
  geom_smooth(method = "lm", color = "salmon", linetype = 2) + 
  labs(title = "Automobile Data", x = "Weight", y = "Miles Per Gallon")

# Grouping and Faceting
mtcars$am <- factor(mtcars$am, levels = c(0,1),
                    labels = c("Automatic", "Manual"))
mtcars$vs <- factor(mtcars$vs, levels = c(0,1), 
                    labels = c("V-Engine", "Straight Engine"))
mtcars$cyl <- factor(mtcars$cyl)

ggplot(data = mtcars, aes(x = hp, y = mpg,
                          shape = cyl, color = cyl)) +
  geom_point(size = 3) + 
  facet_grid(am~vs) + 
  labs(title="Automobile Data by Engine Type",
       x="Horsepower", y="Miles Per Gallon")

data("singer", package = "lattice")
ggplot(singer, aes(x=height)) + geom_histogram()
ggplot(singer, aes(x=voice.part, y=height)) + geom_boxplot()

# Specifying the plot type using Geo
install.packages("car")
library(car)

ggplot(Salaries, aes(x=rank, y=salary)) + 
  geom_boxplot(fill = "cornflowerblue",
               color = "black", notch = TRUE) + 
  geom_point(position = "jitter", color = "blue", alpha = .5) + 
  geom_rug(side = 1, color = "grey")

ggplot(singer, aes(x = voice.part, y = height)) + 
  geom_violin(fill = "lightblue") + 
  geom_boxplot(fill = "pink", width = .2)


# Grouping
ggplot(data=Salaries, aes(x=salary, fill = rank)) + 
  geom_density(alpha = .2)

ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank, shape = sex)) + geom_point()


ggplot(Salaries, aes(x=rank, fill=sex)) + geom_bar(position = "stack") + labs(title="position = 'stack'")
ggplot(Salaries, aes(x=rank, fill=sex)) + geom_bar(position = "dodge") + labs(title="position = 'dodge'")
ggplot(Salaries, aes(x=rank, fill=sex)) + geom_bar(position = "fill") + labs(title="position = 'fill'", y = "Proportion")


# Faceting (trellis graph)
ggplot(data = singer, aes(x = height)) + 
        geom_histogram() + 
        facet_wrap(~voice.part, nrow = 4)


ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color = rank, shape = rank)) + geom_point() + facet_grid(.~sex)  
ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color = rank, shape = rank)) + geom_point() + facet_grid(sex~.)  
ggplot(data = singer, aes(x=height, fill=voice.part))+geom_density() + facet_grid(voice.part~.)







