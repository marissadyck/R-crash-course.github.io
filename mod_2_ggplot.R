
#  ES482 R labs   
#      University of Victoria, Victoria BC Canada             

#  Data Visualization
# Module 2: Data visualization in ggplot 

library(cowplot)
library(tidyverse)
library(leaflet)
library(ggpubr)
library(RColorBrewer)
library(stringr)

# ggplot(data, aes(x = x_variable, y = y_variable)) +
#   geom_chooseGeom


# Box-whisker plots  -----------------------

# box-whisker plot
ggplot(iris, aes(x = Species, y = Sepal.Length)) +    
  geom_boxplot() 

# violin plot
ggplot(iris, aes(x = Species, y = Sepal.Length)) +    
  geom_violin() 


# YOUR CODE HERE ----------------------------------------------------------

# ?geom_violin()

# violin plot with quantiles




###


  # Bar plots ----------------------

ggplot(iris, aes(x = Species)) + # we don't specify a y variable if we want sample size
  geom_bar()
# bar plot with width specified

ggplot(iris, aes(x = Species)) + # we don't specify a y variable if we want sample size
  geom_bar(width = 0.5)



# YOUR CODE HERE ----------------------------------------------------------


# first we need to calculate some summary statistics since these data aren't available in the current dataset, create a data frame object called bar_heights from the iris data. Calculate the mean Sepal length per species and assin this to data as meanSL. Then use code to plot the data







# create plot of mean sepal length for each species

ggplot(bar_heights, aes(Species, meanSL)) +
  
  # add bars
  geom_col(width = 0.5)


# adapt code to pipe directly into ggplot








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


