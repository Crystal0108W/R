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

# Adding smooth lines
ggplot(Salaries, aes(x=yrs.since.phd, y=salary)) + 
        geom_smooth() + geom_point()

# geom_smooth() option
  # method = : lm, glm, smooth(loess)[default], rlm and gam
  # formula = : y~x[default], y~log(x), y~poly(x,n) for an nth degree polynomial fit, y~ns(x,n) for a spine fit with n degrees of freedom
  # se : Plots confidence interval, TRUE is default
  # level: levels of confidence interval to use 95% by default
  # fullrange: specifies whether the fit should span the full range of the plot or just the data. FALSE is the default.

p4 <-ggplot(Salaries, aes(x = yrs.since.phd, y=salary, linetype = sex, shape=sex, color = sex)) + 
        geom_smooth(method = lm, formula = y ~ poly(x,2), se = FALSE, size = 1) + 
        geom_point(size = 2)


# Modifying teh appearance of GGplot2 graphs
## Axes Functions
  # scale_x_continuous(), scale_y_continuous(): breaks= specifies tick marks, labels= specifies labels for tick marks,and limits= controls the range of the values displayed.
  # scale_x_discrete(), scale_y_discrete(): breaks= places and orders the levels of a factor, labels= specifies the labels for these levels, and limits= indicates which levels should be displayed.
  # coord_flip(): Reverses the x and y axes.

ggplot(Salaries, aes(x=rank, y=salary, fill=sex)) + 
        geom_boxplot() + 
        scale_x_discrete(breaks = c("AsstProf", "AssocProf", "Prof"), 
                          labels = c("Assistant\nProfessor",
                                     "Associate\nProfessor",
                                     "Full\nProfessor")) +
        scale_y_continuous(breaks=c(50000, 100000, 150000, 200000),
                   labels=c("$50K", "$100K", "$150K", "$200K")) +
        labs(title="Faculty Salary by Rank and Sex", x="", y="")

## Legends Functions
ggplot(Salaries, aes(x=rank, y=salary, fill = sex)) + 
        geom_boxplot() + 
        scale_x_discrete(breaks = c("AsstProf", "AssocProf", "Prof"),
                         labels = c("Assistant\nProfessor",
                                    "Associate\nProfessor",
                                    "Full\nProfessor")) + 
        scale_y_continuous(breaks=c(50000, 100000, 150000, 200000),
                     labels=c("$50K", "$100K", "$150K", "$200K")) +
        labs(title = "Faculty Salary by Rank and Gender", 
             x = "", y="", fill = "Gender") + 
        theme(legend.position = c(.1,.8))
# To omit legend, use legend.position = "none". 

## Scale Functions
# The ggplot2 package uses scales to map observations from the data space to the visual space.

ggplot(mtcars, aes(x=wt, y=mpg, size=disp, alpha = 0.6)) + 
        geom_point(shape=21, color = "white", fill = "pink") + 
        labs(x = "Weight", y="Miles Per Gallon", 
             title = "Bubble Chart", size = "Engine/nDisplacement")
# The aes() parameter size=disp generates a scale for the continuous variable disp (engine displacement) and uses it to control the size of the points.

ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color = rank)) + 
        scale_color_manual(values = c("grey", "pink", "salmon")) + 
        geom_point(size = 2)
# For In the discrete case, you can use a scale to associate visual cues (for example, color, shape, line type, size, and transparency) with the levels of a factor.
# Use the scale_color_manual() function to set the point colors for the three aca- demic ranks
# Or you can use color presets via the scale_color_brewer() and scale_fill_brewer() functions to specify attractive color sets.


ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank)) + 
        scale_color_brewer(palette = "Accent") + geom_point(size = 2)

# Replacing palette="Set1" with another value (such as "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Dark2", or "Accent") will result in a different color scheme
# To see the available color sets, use library(RColorBrewer) display.brewer.all()
library(RColorBrewer) 
display.brewer.all()

## Themes Function
# Themes allow you to control the overall appearance of these graphs. 
# Options in the theme() function let you change fonts, backgrounds, colors, gridlines, and more.

mytheme <- theme(plot.title = element_text(face = "bold.italic",
                                           size = 20, color = "Salmon",
                                           hjust = 0.5),
                  axis.title = element_text(face = "bold.italic",
                                            size = 14, colour = "salmon"),
                 axis.text = element_text(face = "bold", size = 9, colour = "black"),
                 panel.background = element_rect(fill = "white", colour = "darkblue"),
                 panel.grid.major.y = element_line(colour = "grey", linetype = 1), 
                 panel.grid.minor.y = element_line(colour = "grey", linetype = 2),
                 panel.grid.minor.x = element_blank(),
                 legend.position = "top")

ggplot(Salaries, aes(x=rank, y=salary, fill=sex)) + 
         geom_boxplot() + 
         labs(title = "Salary by Rank and Sex", x="Rank", y="Salary") + 
         mytheme

## Multiple graphs per page
p1 <- ggplot(Salaries, aes(x=rank)) + geom_bar()
p2 <- ggplot(Salaries, aes(x=sex)) + geom_bar()
p3 <- ggplot(Salaries, aes(x=yrs.since.phd, y=salary)) + geom_point()
library(gridExtra)
grid.arrange(p1, p2, p3, ncol=3)
# Note the difference between faceting and multiple graphs. Faceting creates an array of plots based on one or more categorical variables. 
# In grid.arrange, you’re arranging completely independent plots into a single graph.

# Saving Graphs
ggsave(file = "salary.png", plot = p4, width = 5, height = 4)
