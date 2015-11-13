rm(list=ls())

# Make XSite x Xprov tables

# Read in table from excel
xtable <- read.csv("C:\\Users\\Carissa\\Dropbox\\Serotiny CANADA\\ExcelFromR\\CrossTable.csv")

# Make a list of all possible 6-wise combinations of sites
# "pairs" is a list of 134,596 elements
pairs <- combn(xtable[,2:25],6,simplify=FALSE)

# Make a vector of the prov "names"
provs <- xtable[,1]

# Attach the prov vector to each of the sextets of site vectors
interact <- list()
for (i in 1:length(pairs)){
	interact[[i]] <- cbind(provs,pairs[[i]])
}

# For each list element, select only rows where all six columns are not NA
NoNAs <- list()
for (i in 1:length(interact)){
	NoNAs[[i]] <- interact[[i]][!is.na(pairs[[i]][,1]) & !is.na(pairs[[i]][,2]) 
											& !is.na(pairs[[i]][,3]) & !is.na(pairs[[i]][,4])
											& !is.na(pairs[[i]][,5]) & !is.na(pairs[[i]][,6]),]
}

# Function to delete all list items where number of rows is <6
# (so that all list items will be at least 6x6)
delete.NULLs  <-  function(x.list){   
	x.list[unlist(lapply(x.list, nrow) >5)]
}
# Apply function
finalxtable <- delete.NULLs(NoNAs)

###############################################
# NOTE:  4x4 creates a list of 2353 elements; #
# 6x6 creates a list of 376 elements          #
# 8x8 has no possible combinations            #
###############################################

# Create new list of site/prov combinations (all possible pairwise combinations 
# of row and column names per list item; i.e. turn 6 by 6 table into X by 2 table)
siteprov <- list()
for (i in 1:length(finalxtable)){
	siteprov[[i]] <- expand.grid(colnames(finalxtable[[i]][,2:7]),finalxtable[[i]][,1])
}

# Read in serotiny data file
serotiny <- read.csv("C:\\Users\\Carissa2\\Documents\\My Dropbox\\Serotiny CANADA\\ExcelFromR\\serotiny.csv")
serotiny <- serotiny[,2:8]

# Apply final list to serotiny data
# 
library(dplyr)

# Create empty list to be filled by the loop
serotlist <- list(NULL)
# Loop through list of site/prov pairwise combinations (each list item contains
# the site/prov combos for one 6x6 group):
for (i in 1:length(siteprov)){
	# Make new rowlist each i time through the loop
	rowlist <- list(NULL)	
	# Loop through single group of pairwise combinations:
	for (j in 1:nrow(siteprov[[i]])){
				# use dplyr's filter fcn to select rows from serotiny dataframe,
				# then add results as new list item for pairwise combo
				rowlist[[j]] <- filter(serotiny, SITE_CODE==as.character(siteprov[[i]][j,1]) 
														& PROV==(siteprov[[i]][j,2]))
				# collapse "rowlist" list into a single dataframe, put results in the
				# ith list of "test"
				serotlist[[i]] <- do.call("rbind", rowlist)
	}
}


library(lme4)

# For each 6x6 set, check if all 1s or all 0s.  
# If not, store data in new list.
## NOTE:  For 6x6 grids, there is only one combination with all 1s or all 0s.  

calclist <- list(NULL)
for (i in 1:length(serotlist)){
	vector <- serotlist[[i]][,7]
	if(min(vector)==max(vector)) calclist[[i]]=NULL else 
		calclist[[i]] <- serotlist[[i]]
}

# calclist contains the glmerMod objects for each set of 6x6 prov/site combinations

# Remove null elements from list
clean <- calclist[!sapply(calclist, is.null)]


# Run this chunk of code to see the number of zeros and ones in each list element
zeroslist <- list(NULL)
for (i in 1:length(clean)){
	vector <- clean[[i]][,7]
	zeroslist[[i]] <- as.data.frame(table(vector))
}


# Code to change PROV, BLOCK and SEROT type to factor.  (SITE_CODE already as factor.)

for (i in 1:length(clean)){
  clean[[i]]$PROV <- as.factor(clean[[i]]$PROV)
}

for (i in 1:length(clean)){
	clean[[i]]$SEROT <- as.factor(clean[[i]]$SEROT)
}

for (i in 1:length(clean)){
	clean[[i]]$BLOCK <- as.factor(clean[[i]]$BLOCK)
}

# Remove elements of "clean" that do not converge. (Use code in warningList.R.) 
# Remove elements that don't converge for model A (with interaction).
finallistA <- clean[-c(11, 24, 29, 36, 49, 72, 97, 98, 109, 132, 144, 148, 
                     154, 162, 165, 217, 284, 321, 322, 348, 359, 361, 362, 370)]
# # Remove elements that don't converge for model B (no interaction).
finallistB <- finallistA[-c(153,226,287)]


### RUN MODEL ###############################
# 1. Run model with and without interaction
# 2. Obtain delta AIC between two models
# 3. Extract variance from model object
#############################################

# This code runs a/o Oct.15
runmodA <- list(NULL)
runmodB <- list(NULL)
dAIC <- list(NULL)
varianceA <- list(NULL)
varianceB <- list(NULL)
for (i in 1:length(finallistB)){
  modA <- glmer(SEROT ~ 1 + (1|SITE_CODE/BLOCK) + (1|PROV) + (1|SITE_CODE:PROV), 
						family="binomial",data=finallistB[[i]])
	runmodA[[i]] <- summary(modA)
	modB <- glmer(SEROT ~ 1 + (1|SITE_CODE/BLOCK) + (1|PROV), 
						family="binomial",data=finallistB[[i]])
	runmodB[[i]] <- summary(modB)
	dAIC[[i]] <- AICtab(modA,modB, weights=TRUE, base=TRUE, logLik=TRUE, sort=FALSE)
	varcorr1 <- as.data.frame(VarCorr(modA))
  varcorr2 <- as.data.frame(VarCorr(modB))
  vars1 <- varcorr1[,4]
	vars2 <- varcorr2[,4]
  names(vars1) <- c("site*prov","block*site","prov","site")
	names(vars2) <- c("block*site","site","prov")
  varianceA[[i]] <- vars1
	varianceB[[i]] <- vars2
	}

varA <- do.call("rbind", varianceA)
varB <- do.call("rbind", varianceB)


# save interaction column as single vector
interact <- varA[,1]


# Go through dAIC list, select elements where dAIC > 2.
deltaList <- list(NULL)
for (i in 1:length(dAIC)){
  if(abs(dAIC[[i]]$dAIC[1]-dAIC[[i]]$dAIC[2]) > 2) deltaList[[i]]=abs(dAIC[[i]]$dAIC[1]-dAIC[[i]]$dAIC[2]) else 
    deltaList[[i]] = "No"
}

# Collapse list into single vector
deltaVec <- do.call("rbind", deltaList)

# Append to variance list
deltaVar <- cbind(interact,deltaVec)
deltaVar <- as.data.frame(deltaVar)
names(deltaVar) <- c("interact.var","dAIC")

# Create df not including rows where dAIC not > 2.
deltaSig <- subset(deltaVar, deltaVar$dAIC!="No")

# Subset once more for interaction=0 (in all cases, dAIC was a very, very
# small amount greater than 2)
deltaSigFinal <- subset(deltaSig, deltaSig$interact.var!=0)

# For some reason the final numbers are factors.
deltaSigFinal$interact.var <- as.numeric(levels(deltaSigFinal$interact.var))[deltaSigFinal$interact.var]
deltaSigFinal$dAIC <-         as.numeric(levels(deltaSigFinal$dAIC))[deltaSigFinal$dAIC]


# Create single vector for "significant" variances.
sigVar <- deltaSigFinal$interact.var


##################################################
## Code to run site and prov vars from 6x6 data ##
##################################################

# Model run removing PROV.

# Deleting non-converging elements for modB
finallistC <- finallistB[-c(151,163,175,195,226,249,301,316,331,335,336)]

runmodA.sp1 <- list(NULL)
runmodB.sp1 <- list(NULL)
dAIC.sp1 <- list(NULL)
varianceA.sp1 <- list(NULL)
varianceB.sp1 <- list(NULL)
for (i in 1:length(finallistC)){
	modA <- glmer(SEROT ~ 1 + (1|SITE_CODE/BLOCK) + (1|PROV) + (1|SITE_CODE:PROV), 
								family="binomial",data=finallistC[[i]])
	runmodA.sp1[[i]] <- summary(modA)
	modB <- glmer(SEROT ~ 1 + (1|SITE_CODE/BLOCK), 
								family="binomial",data=finallistC[[i]])
	runmodB.sp1[[i]] <- summary(modB)
	dAIC.sp1[[i]] <- AICtab(modA,modB, weights=TRUE, base=TRUE, logLik=TRUE, sort=FALSE)
	varcorr1 <- as.data.frame(VarCorr(modA))
	varcorr2 <- as.data.frame(VarCorr(modB))
	vars1 <- varcorr1[,4]
	vars2 <- varcorr2[,4]
	names(vars1) <- c("site*prov","block*site","prov","site")
	names(vars2) <- c("block*site","site")
	varianceA.sp1[[i]] <- vars1
	varianceB.sp1[[i]] <- vars2
}

varA.sp1 <- do.call("rbind", varianceA.sp1)
varB.sp1 <- do.call("rbind", varianceB.sp1)

# save prov column as single vector
provOnly <- varA.sp1[,3]

# Go through dAIC list, select elements where dAIC > 2.
deltaList.sp1 <- list(NULL)
for (i in 1:length(dAIC.sp1)){
	if(abs(dAIC.sp1[[i]]$dAIC[1]-dAIC.sp1[[i]]$dAIC[2]) > 2) deltaList.sp1[[i]]=abs(dAIC.sp1[[i]]$dAIC[1]-dAIC.sp1[[i]]$dAIC[2]) else 
		deltaList.sp1[[i]] = "No"
}

# Collapse list into single vector
deltaVec.sp1 <- do.call("rbind", deltaList.sp1)

# Append to variance list
deltaVar.sp1 <- cbind(provOnly,deltaVec.sp1)
deltaVar.sp1 <- as.data.frame(deltaVar.sp1)
names(deltaVar.sp1) <- c("prov.var","dAIC")

# Create df not including rows where dAIC not > 2.
deltaSig.sp1 <- subset(deltaVar.sp1, deltaVar.sp1$dAIC!="No")

# Subset once more for interaction=0 (in all cases, dAIC was a very, very
# small amount greater than 2)
deltaSigFinal.sp1 <- subset(deltaSig.sp1, deltaSig.sp1$prov.var!=0)

# For some reason the final numbers are factors.
deltaSigFinal.sp1$prov.var <- as.numeric(levels(deltaSigFinal.sp1$prov.var))[deltaSigFinal.sp1$prov.var]
deltaSigFinal.sp1$dAIC <- as.numeric(levels(deltaSigFinal.sp1$dAIC))[deltaSigFinal.sp1$dAIC]

# Create single vector for "significant" variances.
sigVar.sp1 <- deltaSigFinal.sp1$prov.var


###########################
# Model run removing SITE #
###########################

# Deleting non-converging elements for modB
finallistD <- finallistB[-c(1,2,19,28,68,75,147,172,174,222,251,269,282,301,335)]
finallistE <- finallistD[-295]

runmodA.sp2 <- list(NULL)
runmodB.sp2 <- list(NULL)
dAIC.sp2 <- list(NULL)
varianceA.sp2 <- list(NULL)
varianceB.sp2 <- list(NULL)
for (i in 1:length(finallistE)){
	modA <- glmer(SEROT ~ 1 + (1|SITE_CODE/BLOCK) + (1|PROV) + (1|SITE_CODE:PROV), 
								family="binomial",data=finallistE[[i]])
	runmodA.sp2[[i]] <- summary(modA)
	modB <- glmer(SEROT ~ 1 + (1|PROV) + (1|SITE_CODE:BLOCK), 
								family="binomial",data=finallistE[[i]])
	runmodB.sp2[[i]] <- summary(modB)
	dAIC.sp2[[i]] <- AICtab(modA,modB, weights=TRUE, base=TRUE, logLik=TRUE, sort=FALSE)
	varcorr1 <- as.data.frame(VarCorr(modA))
	varcorr2 <- as.data.frame(VarCorr(modB))
	vars1 <- varcorr1[,4]
	vars2 <- varcorr2[,4]
	names(vars1) <- c("site*prov","block*site","prov","site")
	names(vars2) <- c("block*site","site")
	varianceA.sp2[[i]] <- vars1
	varianceB.sp2[[i]] <- vars2
}

varA.sp2 <- do.call("rbind", varianceA.sp2)
varB.sp2 <- do.call("rbind", varianceB.sp2)

# save prov column as single vector
siteOnly <- varA.sp2[,4]


# Go through dAIC list, select elements where dAIC > 2.
deltaList.sp2 <- list(NULL)
for (i in 1:length(dAIC.sp2)){
	if(abs(dAIC.sp2[[i]]$dAIC[1]-dAIC.sp2[[i]]$dAIC[2]) > 2) deltaList.sp2[[i]]=abs(dAIC.sp2[[i]]$dAIC[1]-dAIC.sp2[[i]]$dAIC[2]) else 
		deltaList.sp2[[i]] = "No"
}

# Collapse list into single vector
deltaVec.sp2 <- do.call("rbind", deltaList.sp2)

# Append to variance list
deltaVar.sp2 <- cbind(siteOnly,deltaVec.sp2)
deltaVar.sp2 <- as.data.frame(deltaVar.sp2)
names(deltaVar.sp2) <- c("prov.var","dAIC")

# Create df not including rows where dAIC not > 2.
deltaSig.sp2 <- subset(deltaVar.sp2, deltaVar.sp2$dAIC!="No")

# Subset once more for interaction=0 (in all cases, dAIC was a very, very
# small amount greater than 2)
deltaSigFinal.sp2 <- subset(deltaSig.sp2, deltaSig.sp2$prov.var!=0)

# For some reason the final numbers are factors.
deltaSigFinal.sp2$prov.var <- as.numeric(levels(deltaSigFinal.sp2$prov.var))[deltaSigFinal.sp2$prov.var]
deltaSigFinal.sp2$dAIC <- as.numeric(levels(deltaSigFinal.sp2$dAIC))[deltaSigFinal.sp2$dAIC]

# Create single vector for "significant" variances.
sigVar.sp2 <- deltaSigFinal.sp2$prov.var


# Display results
par(mfrow=c(1,3))


hist(siteOnly, ylim=c(0,50), xlim=c(0,50), breaks=50, col=rgb(1,1,0,.5), main="", xlab="Variance due to site")
hist(sigVar.sp2, ylim=c(0,50), xlim=c(0,50), breaks=50, col=rgb(0,0,1,.7), main="", xlab="", add=TRUE)

hist(provOnly, ylim=c(0,250), xlim=c(0,5), breaks = 50,col=rgb(1,1,0,.5), main="", xlab="Variance due to provenance")
hist(sigVar.sp1, ylim=c(0,250), xlim=c(0,5), breaks=50, col=rgb(0,0,1,.7), main="", xlab="", add=TRUE)

hist(interact, ylim=c(0,125), xlim=c(0,5), breaks=50, col=rgb(1,1,0,.5), main="", xlab="Variance due to interaction")
hist(sigVar, ylim=c(0,125), xlim=c(0,5), breaks=50, col=rgb(0,0,1,.7), main="", 
     xlab="", add=TRUE)


# Session saved, including all relevant lists:
save(list=ls(all=TRUE),file="C:\\Users\\Carissa2\\Documents\\My Dropbox\\Serotiny CANADA\\Code\\all.site.prov.Session")

