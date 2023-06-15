
#  R Crash Course, Day 1 Module 3
 # Casa Tisaru Lepsa, Romania   

#  Data visualization in ggplot 

library(cowplot)
library(tidyverse)
library(leaflet)
library(ggpubr)
library(RColorBrewer)
library(stringr)
ggplot(data, aes(x = x_variable, y = y_variable)) +
  geom_chooseGeom

# Scatterplots ----------------------------

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + # define the data and x and y axis
  
  # add the points
  geom_point()


# Box-whisker plots  -----------------------

# box-whisker plot
ggplot(iris, aes(x = Species, y = Sepal.Length)) +    
  geom_boxplot() 

# violin plot
ggplot(iris, aes(x = Species, y = Sepal.Length)) +    
  geom_violin() 

  # Bar plots ----------------------

ggplot(iris, aes(x = Species)) + # we don't specify a y variable if we want sample size
  geom_bar()
# bar plot with width specified

ggplot(iris, aes(x = Species)) + # we don't specify a y variable if we want sample size
  geom_bar(width = 0.5)
# first we need to calculate some summary statistics since these data aren't available in the current dataset

bar_heights <- iris %>% 
  
  # create groups for each species
  group_by(Species) %>%
  
  # calculate the mean sepal length for each species
  summarize(meanSL = mean(Sepal.Length))

# create plot of mean sepal length for each species
ggplot(bar_heights, aes(Species, meanSL)) +
  geom_col(width = 0.5)


  # Bar plots with error bars ----------------------

# first we need to add a few calculations to our summary data for this graph
bar_heights <- iris %>% 
  
    # create groups for each species
  group_by(Species) %>% 
  
  # calculate mean, sample size (n), sd, and SE for each species sepal length
  summarize(meanSL = mean(Sepal.Length),
            n = n(),
            sdSL = sd(Sepal.Length),
            se = sdSL/sqrt(n)) # there isn't a function for SE so we have to write our own

# plot mean and SE sepal length for each species
ggplot(bar_heights, aes(x = Species, y = meanSL)) + 
  
  # add columns
  geom_col(width = 0.5) +
  
  # add error bars
  geom_errorbar(aes(ymin = meanSL - 2*sdSL, # ymin specifies the lower limit of the error bar
                    ymax = meanSL + 2*sdSL), # ymax specifies the upper limit of the error bar
                width = 0.2) 

  # Export plots ----------------------

?ggsave()

# save plot_1

ggsave('iris_plot_1.tiff',
       plot_1,
       path = 'figures')
  # Combining plots ----------------------

# Explore different geoms with iris data

  # plot the relationship as a line
plot1 <- ggplot(iris, aes(Petal.Length, Petal.Width)) + 
  
  # add line
  geom_line()

# plot a smoothed "spline" fit of the relationship
plot2 <- ggplot(iris ,aes(Petal.Length, Petal.Width)) + 
  
  # add smoothed line
  geom_smooth()

# plot scatterplot
plot3 <- ggplot(iris ,aes(Petal.Length, Petal.Width)) + 
  
  # add points
  geom_point() 

   # plot scatterplot with smoothed regression line
plot4 <- ggplot(iris ,aes(Petal.Length, Petal.Width)) + 
  
  # add points
  geom_point() + 
  
  # add regression line
  geom_smooth()



ggarrange(plot1,
          plot2,
          plot3,
          plot4,
          labels = "auto")
figure_1 <- ggarrange(plot1, plot2, plot3, plot4,
                      labels = c('A', 'B', 'C', 'D'),
                      ncol = 2,
                      nrow = 2)

figure_1

# save to hard drive
# ggsave('figure_1.tiff', 
#        figure_1,
#        path = 'figures')
ggarrange(plot4,
          ggarrange(plot1, plot2, plot3,
                    ncol = 3,
                    labels = c('A', 'B', 'C')),
          nrow = 2,
          labels = 'D')

  # Changing aesthetics----------------------

  # Color----------------------

# single color for all points by name
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  
  # add points by name
  geom_point(col = 'blue')

# single color for all points by hex code
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  
  # add colored points by hex code
  geom_point(col = '#33A5FF')

# single color for all points by name
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  
  # add colored points by name
  geom_point(aes(col = 'blue'))

 # try representing tree species using color
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  
  # add points colored by height
  geom_point(aes(col = Species))

  # Shape----------------------

# changing point shape

# set all points to a specified shape
ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  
  # add points
  geom_point(shape = 2)

# change shape based on variable in data
ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  
    # shape represents species
  geom_point(aes(shape = Species))


  # Fill ----------------------

# using fill to color points

# comparing color to fill

# color
ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  
  # add points
  geom_point(shape = 2,
             col = 'blue')

# fill
ggplot(iris, aes(Sepal.Length, Petal.Length)) +
  
  # add points
  geom_point(shape = 2,
             fill = 'blue') # notice this didn't work because points can't be filled in 

# let's try with bar graphs instead

# color
ggplot(bar_heights, aes(Species, meanSL)) +
  
  # add bars
  geom_col(col = 'blue') # notice just the border is blue

# fill
ggplot(bar_heights, aes(Species, meanSL)) +
  
  # add bars
  geom_col(fill = 'blue') # now the whole bar is blue

# change fill based on variable in data
ggplot(bar_heights, aes(Species, meanSL)) +
  
  # add bars
  geom_col(aes(fill = Species)) # bar color varies by species

  # Size ----------------------


ggplot(iris, aes(Sepal.Length, Petal.Length)) + 
  
  # size represents species
  geom_point(aes(size = Species)) 

# size is same across the board
ggplot(iris, aes(Sepal.Length, Petal.Length)) +    
  geom_point(size = 2) 

  # Scales ----------------------

# manual color

 # try representing iris species using color
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  
  # add points colored by height
  geom_point(aes(color = Species)) +
  
  # specify colors 
  scale_color_manual(name = 'Iris Species', # this changes the legend title
                     values = c('#BD6BF1', '#3829A3', '#9498D8'), # I selected these hex codes using the ColorPick Eyedropper and googline images of each iris species :)
                     labels = c('I. setosa', 'I. versicolor', 'I. virginica')) # this changes the labels inside the legend, these must be in the same order as the data and the colors you want for each

  # color gradients ----------------------

# specify data and variables
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  
  # add points colored by petal length
  geom_point(aes(color = Petal.Length))


# specify color gradient with scales

# specify data and variables
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  
  # add points colored by petal length
  geom_point(aes(color = Petal.Length)) +
  
  # set scales
  scale_color_gradient(low = 'yellow',
                       high = 'green')

# Playing with colors in ggplot!

display.brewer.all()


# Choose a new color palette from the RColorBrewer package

# specify data and variables
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  
  # add points colored by petal length
  geom_point(aes(color = Petal.Length)) +
  
  # set scales
  scale_color_distiller(palette = 'YlGn')



  # Axis ----------------------

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  
  # add points colored by height
  geom_point(aes(color = Species)) +
  
  # specify colors and legend info
  scale_color_manual(name = 'Iris Species', 
                     values = c('#BD6BF1', '#3829A3', '#9498D8'), 
                     labels = c('I. setosa', 'I. versicolor', 'I. virginica')) +
  
  # change axis breaks
  scale_x_continuous(limits = c(0, 8), # changes the range of values for x axis
                     breaks = seq(0, 8, by = 2)) + # sets the breaks (where the numbers are shown) alternatively could set breaks = c(0, 2, 4, 6, 8)
  
  scale_y_continuous(limits = c(0, 5),
                     breaks = seq(0, 5, by = 1))
  # Axis titles & text----------------------

# change axis information

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  
  # add points colored by height
  geom_point(aes(color = Species)) +
  
  # specify colors and legend info
  scale_color_manual(values = c('#BD6BF1', '#3829A3', '#9498D8'), 
                     labels = c('I. setosa', 'I. versicolor', 'I. virginica')) +
  
  # change axis breaks
  scale_x_continuous(limits = c(4, 8), # changes the range of values for x axis
                     breaks = seq(4, 8, by = 1)) + # sets the breaks (where the numbers are shown) alternatively could set breaks = c(0, 2, 4, 6, 8)
  
  scale_y_continuous(limits = c(2, 5),
                     breaks = seq(2, 5, by = 1)) +
  
  labs(x = 'Sepal length (cm)',
       y = 'Sepal width (cm)',
       color = 'Iris species') # we can change the legend title here using the aesthetic name instead of in scales

  # Titles ----------------------

# create vector for colors
iris_colors <- c('#BD6BF1', '#3829A3', '#9498D8')

# create vector for labels
iris_labels <- c('I. setosa', 'I. versicolor', 'I. virginica')

  # scatterplot
plot1 <- ggplot(iris, aes(Petal.Length, Petal.Width)) + 
  
  # add points
  geom_point(aes(color = Species)) +
  
  # specify colors and labels
  scale_color_manual(values = iris_colors,
                     labels = iris_labels) +
  
  # add labels
  labs(x = 'Petal length (cm)',
       y = 'Petal width (cm)',
       subtitle = 'Scat')

# plot a smoothed "spline" fit of the relationship
plot2 <- ggplot(iris ,aes(Petal.Length, Petal.Width)) + 
  
  # add smoothed line
  geom_smooth(aes(color = Species)) +
  
   # specify colors and labels
  scale_color_manual(values = iris_colors,
                     labels = iris_labels) +
  
  # add labels
    labs(x = 'Petal length (cm)',
       y = 'Petal width (cm)',
       subtitle = 'Linear regression')

# plot scatterplot
plot3 <- ggplot(iris ,aes(Species, Petal.Length)) + 
  
  # add points
  geom_boxplot(aes(color = Species)) +
  
   # specify colors and labels
  scale_color_manual(values = iris_colors,
                     labels = iris_labels) +
  
  # add labels
    labs(x = 'Petal length (cm)',
       y = 'Petal width (cm)',
       subtitle = 'Boxpl0t')

   # plot scatterplot with smoothed regression line
plot4 <- ggplot(iris ,aes(Petal.Length, Petal.Width)) + 
  
  # add points
  geom_point(aes(color = Species)) + 
  
  # add regression line
  geom_smooth(aes(color = Species)) +
  
   # specify colors and labels
  scale_color_manual(values = iris_colors,
                     labels = iris_labels) +
  
    # add labels
    labs(x = 'Petal length (cm)',
       y = 'Petal width (cm)',
       subtitle = 'Scatterplot with linear regression')


ggarrange(plot1,
          plot2,
          plot3,
          plot4,
          common.legend = TRUE,
          legend = 'right',
          labels = 'AUTO')
  # Themes ----------------------

# explore themes 

# scatterplot theme bw
theme1 <- ggplot(iris, aes(Petal.Length, Petal.Width)) + 
  
  # add points
  geom_point(aes(color = Species)) +
  
  # specify colors and labels
  scale_color_manual(values = iris_colors,
                     labels = iris_labels) +
  
  # add labels
  labs(x = 'Petal length (cm)',
       y = 'Petal width (cm)',
       subtitle = 'Theme bw') +
  
  # add theme
  theme_bw()

# scatterplot theme classic
theme2 <- ggplot(iris, aes(Petal.Length, Petal.Width)) + 
  
  # add points
  geom_point(aes(color = Species)) +
  
  # specify colors and labels
  scale_color_manual(values = iris_colors,
                     labels = iris_labels) +
  
  # add labels
  labs(x = 'Petal length (cm)',
       y = 'Petal width (cm)',
       subtitle = 'Theme classic') +
  
  # add theme
  theme_classic()

# scatterplot theme minimal
theme3 <- ggplot(iris, aes(Petal.Length, Petal.Width)) + 
  
  # add points
  geom_point(aes(color = Species)) +
  
  # specify colors and labels
  scale_color_manual(values = iris_colors,
                     labels = iris_labels) +
  
  # add labels
  labs(x = 'Petal length (cm)',
       y = 'Petal width (cm)',
       subtitle = 'Theme minimal') +
  
  # add theme
  theme_minimal()

# scatterplot theme dark
theme4 <- ggplot(iris, aes(Petal.Length, Petal.Width)) + 
  
  # add points
  geom_point(aes(color = Species)) +
  
  # specify colors and labels
  scale_color_manual(values = iris_colors,
                     labels = iris_labels) +
  
  # add labels
  labs(x = 'Petal length (cm)',
       y = 'Petal width (cm)',
       subtitle = 'Theme dark') +
  
  # add theme
  theme_dark()

ggarrange(theme1,
          theme2,
          theme3,
          theme4,
          common.legend = TRUE,
          legend = 'right')

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  
  # add points colored by height
  geom_point(aes(color = Species)) +
  
  # specify colors and legend info
  scale_color_manual(values = c('#BD6BF1', '#3829A3', '#9498D8'), 
                     labels = c('I. setosa', 'I. versicolor', 'I. virginica')) +
  
  # change axis breaks
  scale_x_continuous(limits = c(4, 8), # changes the range of values for x axis
                     breaks = seq(4, 8, by = 1)) + # sets the breaks (where the numbers are shown) alternatively could set breaks = c(0, 2, 4, 6, 8)
  
  scale_y_continuous(limits = c(2, 5),
                     breaks = seq(2, 5, by = 1)) +
  
  labs(x = 'Sepal length (cm)',
       y = 'Sepal width (cm)',
       color = 'Iris species') + # we can change the legend title here using the aesthetic name instead of in scales
  
  theme(axis.title.y = element_text(size = 14,                   # change size of y axis title text
                                    color = 'red'),              # change color of y axis title text
        axis.title.x = element_text(size = 20),                  # change size of x axis title text
        axis.ticks = element_blank(),                            # remove all axis ticks
        panel.grid.major = element_line(colour = 'grey'),        # change color of major gridlines
        panel.grid.minor = element_blank(),                      # remove minor gridlines
        panel.background = element_rect(fill = 'yellow'),        # change panel background color
        panel.border = element_rect(fill = NA, color = "black"), # make pnael border black
        legend.position = "top")                                 # move legend to top

# check out the theme documentation for more things you can change, the options are endless!
# ?theme

  # -Example walkthrough ---------------------

# ?ToothGrowth
head(ToothGrowth)


# toothgrowth plot 

sumTC <- ToothGrowth %>% 
  
  # make dose a factor
  mutate(dose = as.factor(dose)) %>% 
  
  # group by dose
  group_by(supp,dose) %>% 
  
  # summarize mean and sd tooth length per group
  summarize(mean = mean(len),
            sd = sd(len),
            n = n())

sumTC

# specify data and variables
ggplot(sumTC, aes(x = dose, y = mean))

# specify data and variables
ggplot(sumTC, aes(x = dose, y = mean)) +
  
  # add data bars
  geom_col()


# specify data and variables
ggplot(sumTC, aes(x = dose, y = mean)) +
  
  # add data bars
  geom_col(aes(fill = supp),
           color = 'black') # remember since black isn't a variable in the data this argument goes OUTSIDE the aes()


# specify data and variables
ggplot(sumTC, aes(x = dose, y = mean)) +
  
  # add data bars
  geom_col(aes(fill = supp),
           color = 'black',
           position = position_dodge()) # this puts the groups side-by-side instead of stacked


# specify data and variables
ggplot(sumTC, aes(x = dose, y = mean)) +
  
  # add data bars
  geom_col(aes(fill = supp),
           color = 'black',
           position = position_dodge()) + # this puts the groups side-by-side instead of stacked
  
  # add error bars
  geom_errorbar(aes(ymin = mean - sd, 
                    ymax = mean + sd))

# specify data and variables
ggplot(sumTC, aes(x = dose, y = mean,)) +
  
  # add data bars
  geom_col(aes(fill = supp),
           color = 'black',
           position = position_dodge()) + # this puts the groups side-by-side instead of stacked
  
  # add error bars
  geom_errorbar(aes(ymin = mean - sd, 
                    ymax = mean + sd),
                position = position_dodge(0.9),
                width = 0.2) 

# specify data and variables
ggplot(sumTC, aes(x = dose, y = mean,)) +
  
  # add data bars
  geom_col(aes(fill = supp),
           color = 'black',
           position = position_dodge()) + # this puts the groups side-by-side instead of stacked
  
  # add error bars
  geom_errorbar(aes(ymin = mean - sd, 
                    ymax = mean + sd,
                    fill = supp),
                position = position_dodge(0.9),
                width = 0.2) 

# or a simpler way is to specify the fill in the entire plot aesthetics

# specify data and variables
ggplot(sumTC, aes(x = dose, y = mean, fill = supp)) +
  
  # add data bars
  geom_col(color = 'black',
           position = position_dodge()) + # this puts the groups side-by-side instead of stacked
  
  # add error bars
  geom_errorbar(aes(ymin = mean - sd, 
                    ymax = mean + sd),
                position = position_dodge(0.9),
                width = 0.2) 

# specify data and variables
ggplot(sumTC, aes(x = dose, y = mean, fill = supp)) +
  
  # add data bars
  geom_col(color = 'black',
           position = position_dodge()) + # this puts the groups side-by-side instead of stacked
  
  # add error bars
  geom_errorbar(aes(ymin = mean - sd, 
                    ymax = mean + sd),
                position = position_dodge(0.9),
                width = 0.2) +

# add mean value above bars
  geom_text(aes(label = mean),
            position = position_dodge(width = 0.9))

# specify data and variables
ggplot(sumTC, aes(x = dose, y = mean, fill = supp)) +
  
  # add data bars
  geom_col(color = 'black',
           position = position_dodge()) + # this puts the groups side-by-side instead of stacked
  
  # add error bars
  geom_errorbar(aes(ymin = mean - sd, 
                    ymax = mean + sd),
                position = position_dodge(0.9),
                width = 0.2) +

# add mean value above bars
  geom_text(aes(label = mean,
                 y = mean + (sd + 1)),
            position = position_dodge(width = 0.9))

# specify data and variables
ggplot(sumTC, aes(x = dose, y = mean, fill = supp)) +
  
  # add data bars
  geom_col(color = 'black',
           position = position_dodge()) + # this puts the groups side-by-side instead of stacked
  
  # add error bars
  geom_errorbar(aes(ymin = mean - sd, 
                    ymax = mean + sd),
                position = position_dodge(0.9),
                width = 0.2) +

# add mean value above bars
  geom_text(aes(label = mean,
                 y = mean + (sd + 1)),
            position = position_dodge(width = 0.9)) +
  
   # add labels
  labs(title = "Tooth growth", 
       x= "Dose (mg)", 
       y = "Length (mm)" ,
       fill = 'Delivery method') 

# specify data and variables
ggplot(sumTC, aes(x = dose, y = mean, fill = supp)) +
  
  # add data bars
  geom_col(color = 'black',
           position = position_dodge()) + # this puts the groups side-by-side instead of stacked
  
  # add error bars
  geom_errorbar(aes(ymin = mean - sd, 
                    ymax = mean + sd),
                position = position_dodge(0.9),
                width = 0.2) +

# add mean value above bars
  geom_text(aes(label = mean,
                 y = mean + (sd + 1)),
            position = position_dodge(width = 0.9)) +
  
   # add labels
  labs(title = "Tooth growth", 
       x= "Dose (mg)", 
       y = "Length (mm)" ,
       fill = 'Delivery method') +
  
   # add theme
   theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5,
                                  size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) 

# specify data and variables
ggplot(sumTC, aes(x = dose, y = mean, fill = supp)) +
  
  # add data bars
  geom_col(color = 'black',
           position = position_dodge()) + # this puts the groups side-by-side instead of stacked
  
  # add error bars
  geom_errorbar(aes(ymin = mean - sd, 
                    ymax = mean + sd),
                position = position_dodge(0.9),
                width = 0.2) +

# add mean value above bars
  geom_text(aes(label = mean,
                 y = mean + (sd + 1)),
            position = position_dodge(width = 0.9)) +
  
   # add labels
  labs(title = "Tooth growth", 
       x= "Dose (mg)", 
       y = "Length (mm)" ,
       fill = 'Delivery method') +
  
   # add theme
   theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5,
                                  size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) +
  
  # manually alter colors
   scale_fill_manual(values = c('#E69F00', '#999999'),
                     labels = c('Orange juice', 'Ascorbic acid'))

# specify data and variables
ggplot(sumTC, aes(x = dose, y = mean, fill = supp)) +
  
  # add data bars
  geom_col(color = 'black',
           position = position_dodge()) + # this puts the groups side-by-side instead of stacked
  
  # add error bars
  geom_errorbar(aes(ymin = mean - sd, 
                    ymax = mean + sd),
                position = position_dodge(0.9),
                width = 0.2) +

# add mean value above bars
  geom_text(aes(label = mean,
                 y = mean + (sd + 1)),
            position = position_dodge(width = 0.9)) +
  
   # add labels
  labs(title = "Tooth growth", 
       x= "Dose (mg)", 
       y = "Length (mm)" ,
       fill = 'Delivery method') +
  
   # add theme
   theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5,
                                  size = 16),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14)) +
  
  # manually alter colors
   scale_fill_manual(values = c('#E69F00', '#999999'),
                     labels = c('Orange juice', 'Ascorbic acid')) +
  
  # remove extra space between graph and x axis
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 35))

# example of messy code
ggplot(sumTC, aes(x = dose, y = mean, fill = supp)) + geom_col(color = 'black', position = position_dodge()) +
  geom_errorbar(aes(ymin=mean -sd, ymax = mean + sd),position = position_dodge(0.9),width = 0.2) +
  geom_text(aes(label=mean,y =mean +(sd +1)),position = position_dodge(width = 0.9)) + abs(title = "Tooth growth", 
       x= "Dose (mg)", y = "Length (mm)" ,fill = 'Delivery method') +
   theme_classic() +
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14), plot.title = element_text(hjust = 0.5,size = 16),
        legend.text = element_text(size = 12),legend.title = element_text(size = 14)) +
   scale_fill_manual(values = c('#E69F00', '#999999'),labels = c('Orange juice', 'Ascorbic acid')) +
  scale_x_discrete(expand = c(0, 0)) +cale_y_continuous(expand = c(0, 0),
                     limits = c(0, 35))

plant_boxplot <- ggplot(PlantGrowth, aes(x = group, y = weight)) +
  
  # add boxplots filled by group 
  geom_boxplot(aes(fill = group),
               color = 'black') + # outline in black
  
  labs(x = 'Treatment group',
       y = 'Weight (g)') + # I am just guessing it is in grams since the R documentation did not say
  
  # select greyscale colors
  scale_fill_manual(values = c('#DDDDDD', '#888888', '#444444')) +
  
  # change axis labels
  scale_x_discrete(labels = c('Control', 'Treatment 1', 'Treatment 2')) +
  
  # select theme with no gridlines
  theme_classic() +
  
  # remove legend and increase text size
  theme(legend.position = 'NONE',
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) 

# specify data and variables
trees_scatterplot <- ggplot(trees, aes(x = Height, y = Girth)) +
  
  # add points as plus signs
  geom_point(shape = 3,
             size = 4) +
  
  # create informative labels and title
  labs(x = 'Height (ft)',
       y = 'Diameter (in)',
       title = 'Black Cherry Tree Measurements') +
  
  # add breaks based on range of data
  scale_x_continuous(breaks = seq(60, 85, by = 5)) +
  scale_y_continuous(breaks = seq(5, 20, by = 5)) +
  
  # choose theme with gridlines
  theme_bw() +
  
  # adjust text and title
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# read in bear data

bears <- read.csv('data/bear_2008_2016.csv') %>% 
  
   # set column names to lowercase
  set_names(
    names(.) %>%  # the period here is a placeholder for the data it tells R to use the element before the last pipe (e.g., turtles.df)
      tolower()) %>% 
  
     # select only damage points
  filter(damage == 1) %>% 
  
  # select specified columns
  select(damage, year, targetspp, bear_abund:dist_to_town)

str(bears)
  
  
# filter data to only damage points


# mutate data so year and targetspp are factors

bears_summary <- bears %>% 

  
  # change variables to factor
  mutate(year = as.factor(year),
         targetspp = as.factor(targetspp))


bear_damage_bargraph <-  ggplot(bears_summary, aes( x = year)) +
  
  # add bars
  geom_bar(aes(fill = targetspp),
           position = position_dodge()) +
  
  # specify colors and labels
  scale_fill_manual(name = 'Livestock type',
                    values = c('#A31E17', '#4F3124', '#AF9985'),
                    labels = c('Other', 'Cows', 'Sheep')) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 80, by = 20)) +
  
  # add labels, caption, and title
  labs(x = 'Year',
       y = 'Number of events',
       title = 'Reported livestock damage by brown bears',
       caption = str_wrap('The total number of reported livestock damage events caused by brown bears in Romania per year and livestock type (tan = sheep, brown = cows, red = other livesotock [e.g. chickens]) from 2008 to 2016.',
                          width = 90)) +
  
  
  theme_classic() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12,
                                   angle = 45,
                                  vjust = 0.7),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 18),
        legend.position = 'NONE',
        plot.caption = element_text(size = 12,
                                    hjust = 0))

# create vectors for landcover types

art_surfaces <- c(112,121,131) 

ag <- c(211,221,222, 242,243) 

open <- c(231, 321) 

forest <- c(311,312,313, 324) 


bears_cows <- bears %>% 
  
  # keep only data for cows
  #filter(targetspp == 'bovine') %>% 
  
  # combine landcover types using case_when
  mutate(landcover_grouped = case_when(landcover_code %in% art_surfaces ~ 'art_surfaces',
                                       landcover_code %in% ag ~ 'ag',
                                       landcover_code %in% open ~ 'open',
                                       landcover_code %in% forest ~ 'forest'))

str(bears_cows)

# create vectors for plot colors and labels

spp_colors <- c('#A31E17', '#4F3124', '#AF9985')

spp_shapes = c(17, 16, 15)

spp_labels <- c('Other', 'Cows', 'Sheep')


# plot 1

# specify data and variables
plot_5.1 <- ggplot(bears_cows, aes(x = landcover_grouped, y = dist_to_forest)) +
  
  # add points
  geom_jitter(aes(color = targetspp,
                  shape = targetspp),
              width = 0.4, # this changes the amount of 'jitter'
              alpha = 0.6, # this sets opacity
              size = 2) +
  
  # manually set colors
  scale_color_manual(name = 'Livestock type',
                     values = spp_colors,
                     labels = spp_labels) +
  scale_shape_manual(name = 'Livestock type',
                     values = spp_shapes,
                     labels = spp_labels) +
  scale_x_discrete(labels = c('Agriculture', 'Artifical', 'Forest', 'Open')) +
  
  # informative labels
  labs(x = 'Landcover type',
       y = 'Distance to forest (m)') +
  
  # add theme elements
  theme_bw() +
    theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

# plot 2

# specify data and variables
plot_5.2 <- ggplot(bears_cows, aes(x = landcover_grouped, y = dist_to_town)) +
  
  # add points
  geom_jitter(aes(color = targetspp,
                  shape = targetspp),
              width = 0.4, # this changes the amount of 'jitter'
              alpha = 0.6, # this sets opacity
              size = 2) +
  
  # manually set colors
  scale_color_manual(name = 'Livestock type',
                     values = spp_colors,
                     labels = spp_labels) +
  scale_shape_manual(name = 'Livestock type',
                     values = spp_shapes,
                     labels = spp_labels) +
  scale_x_discrete(labels = c('Agriculture', 'Artifical', 'Forest', 'Open')) +
  
  # informative labels
  labs(x = 'Landcover type',
       y = 'Distance to town (m)') +
  
  # add theme elements
  theme_bw() +
    theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

plot_combined <-  ggarrange(plot_5.1, plot_5.2,
          common.legend = TRUE)
