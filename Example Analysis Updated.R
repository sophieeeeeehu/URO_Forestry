#####################################
# Example analysis for REX project  #
#       January 20, 2023            #
#     Author: Kate Peterson         #
#                                   #
#   Description: example code for   #
#   running linear regression on    #
#   theoretical spruce provenance   #
#   data for REX project.           #
#                                   #
# Input file(s): 'Sx.htdata.csv     #
# Output file(s):                   #
#####################################

# install packages - I highly recommend dplyr and ggplot2!

# install.packages('dplyr', 'ggplot2', 'gridExtra') # you only need to run this line the first time you run this code - or if you install a new version of R. 

# Now load the package function libraries into R:

library(dplyr)
library(ggplot2)
library(gridExtra) # you may need to install this package
# This needs to be done every time you open this code and start running it!

setwd("C:/Users/Kate - Work/Documents/PhD/Mentoring") # Set the working directory to the folder that has your csv file in it - makes it simpler for loading and saving

sxData <- read.csv(file = 'Sx.htdata.csv') # load data into R environment

sxData <- within(sxData, {      # create transfer distance variables
  TX_MCMT <- S_MCMT - P_MCMT
  TX_MAT <- S_MAT - P_MAT
  TX_Lat <- S_Latitude - P_Latitude
  
})

# The previous function can also be done individually:
sxData$TX_lnMAP <- log(sxData$S_MAP) - log(sxData$P_MAP) # This created a new variable in the 'sxData' dataframe called 'TX_lnMAP'

# Let's visualize the data and see what we think a good model would be

# This can be done with base R functions:
plot(sxData$TX_MCMT, sxData$Height) 

# or ggplot, which is more complicated but good to learn as it is much more customizable

ggplot(data = sxData, aes(x = TX_MCMT, y = Height)) + geom_point() # same plot as above just with ggplot

# We can also look at a matrix of scatterplots that show us a few different relationships:

pairs(~ Height + S_MAP + P_MCMT + TX_MAT, data = sxData) # you can modify this to look at different variables 

# It looks like the transfer distance variables seem to be the best fit, but the provenance variables might show something interesting as well?
'I am here'
######## Model Fitting #########

# The lm (linear model) function in R needs at least 2 inputs - lm(formula, data)
# The formula needs to be in the format y ~ x1 + x2 + x3 + ... + xM
# The data is your dataframe

# For example:
model.1 <- lm(Height ~ TX_MCMT + I(TX_MCMT^2), data = sxData) # The I() in the formula statement allows you to transform one variable - 
# in this case we want a quadratic model (because of the U-shaped relationship) so we need to square it!

sum.1 <- summary(model.1)
sum.1 # Actually not terrible - ~0.5 R-squared is pretty good, but I think we could improve it

model.2 <- lm(Height ~ TX_MCMT + I(TX_MCMT^2) + P_MAT + I(P_MAT^2), data = sxData)
sum.2 <- summary(model.2)
sum.2

# You should also try things like including precipitation variables, transforming variables (i.e. using the log of precipitation, or the squareroot, etc.)


######### Model Selection ##########

# In order to choose a model, we need to look back at the summaries that we generated during the model fitting

sum.1 

sum.2

######### Prediction ###########

# read in climate data
# I chose 3 random sites across the province and generated the 
# 'normal' climate data for the period of 2071-2100

futureclimate <- read.csv('futureclimate.csv')

# now lets keep only the variables we need - I'm going to use model 2 that I fit above to make the predictions

fcdat <- subset(futureclimate, select = c(SITE, MCMT, MAT, RCP))

# For this example, I'm going to choose to examine planting three different populations at the test sites. 
# I selected 3 populations from the data, one that had a low TX_MCMT for each site
# you will probably want to do this differently or look at more populations, this is just an example!

sxData2 <- subset(sxData, PROV %in% c(47, 74, 46))
sxData2 <- unique(subset(sxData2, select = c(PROV, P_MAT, P_MCMT))) # select only the provenance information

fcdat2 <- merge(fcdat, sxData2) # this step creates a new dataframe with all possible combinations of provenances, test sites, and RCPs

fcdat2$TX_MCMT <- fcdat2$MCMT - fcdat2$P_MCMT # create the transfer distance variable

fcdat2$predHT <- predict(model.2, fcdat2) # predicts the new heights using model 2

# now we have predictions for different populations at different test sites that we can compare!
fcdat2$PROV <- as.factor(fcdat2$PROV)
fcdat2$RCP <- as.factor(fcdat2$RCP)

ggplot(data = fcdat2, aes(y = predHT, x = RCP)) + geom_boxplot() + facet_wrap(~SITE)

# Now try out testing different populations! 
# Or instead of selecting actual populations from the dataset, you may want to examine different transfer distances specifically. 
# See below:

tx.mcmt <- seq(-10, 10, 0.1)

fcdat3 <- merge(fcdat, tx.mcmt)
colnames(fcdat3)[5] <- 'TX_MCMT'

# To examine just TX_MCMT, we want to keep P_MAT constant - so I'll set it to 1 degree
fcdat3$P_MAT <- 1

fcdat3$predHT <- predict(model.2, fcdat3)
ggplot(data = fcdat3, aes(x = TX_MCMT, y = predHT)) + geom_line() + facet_wrap(~RCP)

# Suggestion for something to try: pick a location in the current hybrid spruce range, and predict what the height would be with a transfer distance of 0 (assuming no climate change) 
# and then compare to differing levels of climate change? How will that change the height/volume? 