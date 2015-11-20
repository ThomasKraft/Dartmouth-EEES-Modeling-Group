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

# notice that this is the same as the following:
dplot <- ggplot(diamonds, aes(x=carat, y=price, colour=color)) 
dplot + geom_point()

#make a scatterplot of carat vs. price, using shapes to denote the cut of the diamond
ggplot(diamonds[diamonds$cut %in% c("Fair", "Good"),], aes(x=carat, y=price, shape=cut)) +geom_point()


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

# we can easily log (base 10) the y-axis
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
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

####### END MULTIPLOT FUNCTION #######

I1 <- ggplot(diamonds[diamonds$clarity=="I1",], aes(x=carat, y=price)) + geom_point()

SI2 <- ggplot(diamonds[diamonds$clarity == "SI2",], aes(x=carat, y=price)) + geom_point()

multiplot(I1, SI2, cols=2)

# What about making some nice boxplots?
ggplot(diamonds, aes(cut, price, fill=color)) + geom_boxplot() + scale_y_log10()

#Or violin plots. Tom loves violin plots.
ggplot(diamonds, aes(cut, price, fill=color)) + geom_violin() + scale_y_log10()

#Or kernel density plots (smoothed version of histogram based on nonparametric algorithm with a bandwidth)
ggplot(diamonds, aes(depth, fill=cut)) +
  geom_density(alpha=1/4) + 
  xlim(55, 70) +
  ggtitle("Depths by Cut Quality") +
  xlab("Depth") + ylab("Density")


#### CONTOUR PLOTS
# Generate data
library(reshape2) # for melt
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")

# Basic contour plot
v <- ggplot(volcano3d, aes(x, y, z = z))
v + stat_contour()

# Use color gradient to denote z axis
v + stat_contour(aes(colour = ..level..))

# Manually set the color gradient scale
v + stat_contour(aes(colour = ..level..), size = 2) +
  scale_colour_gradient(low = "brown", high = "white")

# use different geometry combinations
v + geom_tile(aes(fill = z)) + stat_contour()

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




### Spatial graphics with R
#if you need to install the packages please do so (note th)

## Load required packages
library(sp)
library(rgeos)
library(rgdal)
library(maptools)
library(RColorBrewer)
library(ggmap)


# Load map from google using ggmap package

#the geocode interfaces with google maps to find the geographic coordinates of a place
geocode("Taman Negara")
geocode("Hanover, NH")
geocode("983 West Dryden Rd., Freeville, NY")

# the get_map() function pulls out a map from google maps and creates a ggmap object. the ggmap() function can then be used to produce a plot.
TN <- get_map("Taman Negara National park", zoom=12, maptype="terrain")
TN <- ggmap(TN)
TN



# We can layer ggmap plots the same way we do with ggplot! This makes the ggplot framework very attractive for spatial mapping.

#load in points from GPS device
koom <- read.csv("/Users/thomaskraft/Desktop/Google Drive/Malaysia/Honeybee work/GPS data/Adorsata waypoints.csv", header=T)
koom$harvestfactor <- koom$harvested
koom$harvestfactor <- gsub(1, "Harvested",koom$harvestfactor)
koom$harvestfactor <- gsub("0", "Present, not harvested",koom$harvestfactor)
koom$harvestfactor[is.na(koom$harvestfactor)] <- "No dorsata present"

#plot the same map but with our GPS points on it
TN <- get_map(location=c(lon=102.450,lat=4.460), zoom=12, maptype="terrain")
cbbPalette <- c("red", "black","green")
TN1 <- ggmap(TN)+geom_point(data=koom, size=3, alpha=.9, aes(lon=lon, lat=lat, colour=harvestfactor))+labs(x="Longitude",y="Latitude")+scale_colour_manual(values=cbbPalette)
TN1


#you can also layer on shape files
setwd("/Users/thomaskraft/Downloads/RC Environment Natural Resources/Forest")
shape <- readOGR(dsn=".", "FORESTCATEGORIES_Project")

#plot the shape file
plot(shape)



# Let's move to a more complex example
# call in a map of houston
houston <- get_map("houston", zoom = 14)
HoustonMap <- ggmap(houston, extent = "device", legend = "topleft")

#take a look at the basic map
HoustonMap

#now let's load in some data. This data comes with the ggmap package:
data(crime)
str(crime)

#subset the data to be only violent crimes in the downtown
# only violent crimes
violent_crimes <- subset(crime, offense != "auto theft" & offense != "theft" & offense != "burglary")

# order violent crimes
violent_crimes$offense <- factor(violent_crimes$offense, levels = c("robbery", "aggravated assault", "rape", "murder"))

# restrict to downtown
violent_crimes <- subset(violent_crimes, -95.39681 <= lon & lon <= -95.34188 & 29.73631 <= lat & lat <= 29.78400)

#add the violent crimes onto the map
HoustonMap 

overlay <- stat_density2d(
  aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
  bins = 4, geom = "polygon",
  data = violent_crimes
)

HoustonMap + overlay + inset(
  grob = ggplotGrob(ggplot() + overlay + theme_inset()),
  xmin = -95.35836, xmax = Inf, ymin = -Inf, ymax = 29.75062
)


# see http://spatialanalysis.co.uk/wp-content/uploads/2012/02/bike_ggplot.png for how sophisticated mapping in R can get

#load in the CO2 dataset built into R on CO2 uptake in grasses
data(CO2)
str(CO2)   #check out the data

#Make a scatterplot with "uptake" as a function of "conc" with the following attributes:
1) denote different "Treatment" levels with different colors
2) denote different "Type" levels with different shapes
3) add linear trend lines through each 
4) Make a classical white background instead of the default grey ggplot
5) Make sure the legend has both a shape and a color
6) Make the X-axis label read "Concentration (ug/ml)" and have the y-axis read "Uptake (g C/plant/year)"

# The trendlines in the above plot don't look very linear... now modify the above plot to have nonlinear least squares trend lines with an asymptote. HINT: you must turn off 95% conf. intervals because nls() in R does not support them and you will want to use the following code for the model formula: formula=y~SSasymp(x, a, b, c)


# When you are finished with those: try the next plot
# here we will work with the "economics" dataset
data(economics)
str(economics)

# make a time series plot of unemployement as a function of date in the economics dataset. Now produce a second graph with "pop" as a function of date. Try combining these graphs to a single figure using multiplot.

