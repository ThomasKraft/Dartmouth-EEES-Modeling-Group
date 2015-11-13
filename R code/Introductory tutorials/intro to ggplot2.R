###### Example time ######
## Diamonds dataset comes with ggplot2, gives characteristics of lots of diamonds 
library(ggplot2)
head(diamonds)
str(diamonds)

## ggplot2 has some quick functions for plotting, but this is not the best way of doing things
qplot(carat, price, data=diamonds)


############################################################################
### Now let's turn to ggplot, the more advanced plotting tool in ggplot2. Here, you build layers of a plot sequentially 

#make a simple histogram of clarity
ggplot(diamonds, aes(clarity)) + geom_bar()

#make a histogram of clarity categories, with stacked parts indicating cut
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()

#make a scatterplot of carat vs. price, coloured by the color of the diamond
ggplot(diamonds, aes(x=carat, y=price, colour=color)) +
  geom_point()


#now let's add some trendlines to that last graph. ggplot knows how to fit a number of different models to your data and we will demonstrate some of them here. The stat_smooth() function will automatically plot 95% confidence intervals. We can turn them off if we want, or plot some other error measure.
#notice that the auto method here will actually plot GAM fits
ggplot(diamonds, aes(x=carat, y=price, colour=color)) +
  geom_point()+stat_smooth(method="auto")

#we can change those to loess fits
ggplot(diamonds, aes(x=carat, y=price, colour=color)) +
  geom_point()+stat_smooth(method="loess")

#we can change those to lm fits
ggplot(diamonds, aes(x=carat, y=price, colour=color)) +
  geom_point()+stat_smooth(method="lm")

#we can change those to glm fits with overdispersion (quasi-poisson)
ggplot(diamonds, aes(x=carat, y=price, colour=color)) +
  geom_point()+stat_smooth(method="glm", family="quasipoisson")+scale_y_continuous(limits = c(0,20000))+labs(x="CARAT", y="PRICE")

#we can change those to glm fits with overdispersion (quasi-poisson) with log (base 10) y-axis
ggplot(diamonds, aes(x=carat, y=price, colour=color)) +
  geom_point()+stat_smooth(method="lm")+labs(x="CARAT", y="PRICE")+scale_y_log10()

# with any of these cases we can remove the gray background and make a more traditional white background using the theme_classic() parameter
#notice that the auto method here will actually plot GAM fits
ggplot(diamonds, aes(x=carat, y=price, colour=color)) +
  geom_point()+stat_smooth(method="auto")+
  theme_classic()

#### PLOTTING MULTIPLE PANELS
# The easiest way: facetting. Add the facet_wrap() parameter with a variable of your choice to avoid overplotting.

ggplot(diamonds, aes(x=carat, y=price)) + geom_point() + facet_wrap( ~ clarity)

# or we might want to make truly distinct panels. For this we use a custom built function called multiplot:



# What about making some nice boxplots?
ggplot(diamonds, aes(cut, price, fill=color)) + geom_boxplot() + scale_y_log10()

#Or violin plots. Tom loves violin plots.
ggplot(diamonds, aes(cut, price, fill=color)) + geom_violin()

#Or kernel density plots (smoothed version of histogram based on nonparametric algorithm with a bandwidth)
ggplot(diamonds, aes(depth, fill=cut)) +
  geom_density(alpha=1/4) + 
  xlim(55, 70) +
  ggtitle("Depths by Cut Quality") +
  xlab("Depth") + ylab("Density")



# Get some other example datasets from R Cookbook
install.packages("gcookbook")
library("gcookbook")

# Look at the dataset on cabbages
head(cabbage_exp)
str(cabbage_exp)

# Barplots in ggplot2
# Grouped bars
plot<-ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar)) # note:nothing visual exists here yet
plot<-plot+geom_bar(position="dodge",stat="identity") # Note that 'fill' is the argument that does the grouping here
plot # make the plot

#Add standard error bars
seplot<-ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar)) # initialize
seplot<-seplot+geom_bar(position="dodge",stat="identity") # add barplot
seplot<-seplot+geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se),width=.2,position=position_dodge(.9)) # add SEs to barplot
seplot

# Label means on grouped bars
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar)) + geom_bar(stat="identity",position="dodge") + geom_text(aes(label=Weight),vjust=1.5,colour="white", position=position_dodge(.9),size=5)




