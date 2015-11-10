### Test to help everyone get started!

install.packages("ggplot2")
install.packages("RCurl")
install.packages("foreign")

library(ggplot2)
library(RCurl)
library(foreign)

url1 <- getURL("https://raw.githubusercontent.com/ThomasKraft/Dartmouth-EEES-Modeling-Group/master/Data/serotiny.csv")
cdat <- read.csv(textConnection(url1))

str(cdat)  #if you dont get any errors then this is a success!