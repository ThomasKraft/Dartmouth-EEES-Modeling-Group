### This is an R tutorial focused on organizing and manipulating data. 
# Created by Tom Kraft, 11/10/15

#  Welcome to R!
#  Why use R?
#  How to use comments (always comment your code!!!!!)

#Basic operations -- see this website for a good list  (http://www.statmethods.net/management/operators.html)
#R can act as a big calculator for grown-ups!
2+5
log(1.4)     #natural log
17%%4        #modulus (remainder after dividing)
atan(1)      #arc tangent of 1 (radians)


#Storing data as variables
tom      #run this code -- this variable doesn't exist yet!
tom <- 2     #the variable "tom" is now a single numeric element
tom


#find out what class an object is 
class(tom)    #tom from above is numeric
tom <- class(as.factor(tom))


#storing data as vectors and data frame
marcus <- c(1,2,5,4,7)
marcus     #this is a vector of numbers

#operators can be used on vectors too 
marcus <- marcus+2   #this operation will add 2 to EACH element of marcus
marcus

carissa <- c(5,6,7,8)  #make a second vector
df <- data.frame(marcus, carissa)     #combine the vectors to make a 2 column dataframe (this is the main structure for ecological data)
df  #this is a data frame
str(df)   #get a summary of the "structure" of a dataframe


#square brackets are the best way to call specific elements of a dataframe, vector, or matrix
df[1,1]    #single element in row 1, column 1
df[1:3,]   #rows 1 through 3 in all columns

df[df$marcus==1 | df$marcus==2,]

#reference a specific column by name in the dataframe using the $ operator
df$marcus


########################################################################################################################
# Now we are going to introduce a dataset called "diamonds" and practice operations different data manipulations
########################################################################################################################

#What is a package, how to install one, and how to load it
install.packages("ggplot2")
install.packages("plyr")

library(ggplot2)
library(plyr)

#load dataset
data(diamonds)   #from the ggplot2 package
str(diamonds)

#look prices of different cuts
plot(price~cut, data=diamonds)

# How can we calculate the mean of each of these categories? 
# We will try different methods, working from worst to best

#approach 1: repeated lines of code. If I ever catch you doing this in practice I will throw whipped cream pies at you!
mean(diamonds[diamonds$cut == "Fair",]$price)
mean(diamonds[diamonds$cut == "Good",]$price)
mean(diamonds[diamonds$cut == "Very Good",]$price)
mean(diamonds[diamonds$cut == "Premium",]$price)
mean(diamonds[diamonds$cut == "Ideal",]$price)


#approach 2: use a "for loop".  ***NOTE*** for loops are notoriously slow in R, and are MUCH faster in Python or Matlab. However, I end up using for loops alot when I am being lazy in R because they are consistently easier for me to think through. Don't let yourself get addicted to for loops! Start out by learning the more efficient approaches right away.

#basic for loop structure
for(i in c(1:20)){
  print(i)
}

#now accomplish the task above (calculating means) using a for loop
for(j in levels(diamonds$cut)){
  avg.price <- mean(diamonds[diamonds$cut == j,]$price)
  print(c(j, avg.price))
}



#approach 3: use a function in the "apply" family. ***NOTE*** apply family functions are part of base R. They can be very fast, but are often less intuitive to use and you will probably find them frustrating when you need to do fancier operations. Really good to know these, however.

#basic apply function
X<-matrix(rnorm(30), nrow=5, ncol=6)
apply(X,2 ,sum)    #this calculates the sums of the columns. Remember, "X" is the name of the dataframe, "2" tells apply to work on columns, and "sum" tells apply to calculate the sums of the columns

# another example: divide all values by 2
apply(X, 1:2, function(x) x/2)   #1:2 means both rows and columns

#now accomplish the task above again. We now need another function in the apply family, called "tapply", which applies a function across a "ragged array" (fancy way of referencing a structure in which the sizes of subclasses are different). Here, the cut categories may be vectors of different lengths.
tapply(diamonds$price, diamonds$cut, mean)    #OK not bad, we did it in just a single line!

#we can also do more complex summarization this way:
tapply(diamonds$price, list(diamonds$cut, diamonds$color, diamonds$clarity), mean)



#approach 4: Hadley Wickham is a name you should get to know and love. He is a computer scientist/statistician who was a professor and now works for Rstudio. He is author of many of the packages we are using here and has really improved the things we are doing now in R. For big datasets this stuff is a must. Here we are going to use his "plyr" package and the function ddply.

# Why use plyr over base apply functions?
# 1. plyr has a common syntax — easier to remember
# 2. plyr requires less code since it takes care of the input and output format
# 3. plyr can easily be run in parallel — faster

ddply(diamonds, .(cut), summarize, price=mean(price))   #the first argument specifices the dataset, the second argument specifies a variable by which to split the dataset, the third argument says what to create from output (summarize means new dataframe with summary values, transform would modify an existing dataframe), and the fourth argument here specifies the functions to apply to each piece. 

#as above, we can get fancier easily
ddply(diamonds, .(cut, color), summarize, avgprice=mean(price), sum=sum(price))




#approach 5: The best (for most cases). This approaches using another Hadley Wickham special: the updated version of plyr called "dplyr"
# The chained dplyr version
library(dplyr)

diamonds %>% group_by(cut) %>% summarize(price = mean(price))

# a more complex use of dplyr
mods <- diamonds %>% 
  group_by(cut, clarity, color) %>%
  do(model = lm(price ~ depth, data = .)) %>%
  mutate(slope=summary(model)$coeff[2],
            SE=summary(model)$coeff[4],
            t=summary(model)$coeff[6],
            P=summary(model)$coeff[8])


#same with ddply. Here we save model outputs to a list
mods2 <- dlply(diamonds, .(cut, clarity, color), lm, formula= price~depth)
ldply(mods2, coef)

length(mods2)  #we just ran 276 models!


#compare the speed of these approaches
#1)
system.time(
  for(j in levels(diamonds$cut)){
  avg.price <- mean(diamonds[diamonds$cut == j,]$price)
  print(c(j, avg.price))
})

#2)
system.time(
  tapply(diamonds$price, diamonds$cut, mean) )

#3) 
system.time(
  ddply(diamonds, .(cut), summarize, price=mean(price)))

#4) 
system.time(
  diamonds %>% # start by specifying the data.frame
    group_by(cut) %>%  # The specify the grouping variables
    summarize(price = mean(price)))


## Now to learn some other capabilities of dplyr
#1) filter: this is a way of conveniently subsetting data with logical conditions
diamonds[diamonds$cut == "Fair",]
#is equivalent to:
filter(diamonds, cut == "Fair")

#can also do more complex combinations
filter(diamonds, cut=="Fair" & color=="D" & price < 3300)
filter(diamonds, cut=="Fair" | clarity=="SI2")


#2) arrange: reorder rows according to criteria
head(arrange(diamonds, price))
head(arrange(diamonds, desc(price)))
head(arrange(diamonds, price, carat, depth))



#3) select: subset out specific columns
head(select(diamonds, price))

#4) add new columns with mutate
head(mutate(diamonds, tyler=price-1000, braden=tyler+1000))

#compare this to the base R approach
diamonds$tyler <- diamonds$price-1000
diamonds$braden <- diamonds$tyler-1000



#5) Interested in bootstrapping? This can be powerfully applied to many different types of simulations (including potentially Carissa's case on Tuesday)
#use sample_frac() for an extremely convenient and blazing fast sample. Remember, if you are really bootstrapping then replacement should be set to TRUE.
sample_frac(diamonds, .001)
sample_n(diamonds, 10)


#6) Chaining commands. LEARN TO DO THIS EARLY ON MAKE MUCH MORE ELEGANT CODE!
diamonds %>%
  group_by(cut, clarity, color) %>%
  select(price, depth) %>%
  summarise(
    mean.price = mean(price, na.rm = TRUE),
    sd.price = sd(price, na.rm = TRUE),
    mean.depth = mean(depth, na.rm = TRUE),
    sd.depth = sd(depth, na.rm = TRUE)    
  ) %>%
  filter(mean.price > 3000 | mean.depth < 4000)   #that's alot of stuff all at once, and it's very easy to see the process or extend it




### Now we will move on to another Hadley Wickham package: tidyr. Tidyr is an abbreviated version of reshape2 (what Sam Fey mentioned in his email). These two packages will basically accomplish the same feats for you, and tidyr is generally simpler for me to use.
library(tidyr)

# Going from wide to long format is easy using the gather() function. In the old days I used to do this with custom written for-loops. That sucked. This is way better.
#first generate the data
messy <- data.frame(name = c("Wilbur", "Petunia", "Gregory"), a = c(67, 80, 64), b = c(56, 90, 50))
messy

# the first argument specifies the dataframe, the second specifies the "key" column which will have your categories in it, the third argument specifies the name of the value column (which will have the numbers from within the cells in it), and the last argument specifies the columns that you want to gather into pairs of categories with values
long.messy <- gather(messy, Categories, HappinessValue, c(a,b))


#convert the long format back to wide
spread(long.messy, key=Categories, value=HappinessValue)

#