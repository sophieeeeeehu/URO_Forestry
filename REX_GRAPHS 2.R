#### Modeling and fitting variables ####

setwd("/Users/gracefields/Desktop/REX")

sxData <- read.csv("Sx.htdata.csv")

library(dplyr)
library(ggplot2)
library(gridExtra)

##### Create TD Variables #####

sxData <- within(sxData, {
  TX_MCMT <- S_MCMT - P_MCMT
  TX_MAT <- S_MAT - P_MAT
  TX_Lat <- S_Latitude - P_Latitude
  TX_MAP <- S_MAP - P_MAP
  TX_MSP <- S_MSP - P_MSP
  TX_Lon <- S_Longitude - P_Longitude
  TX_Elev <- S_Elevation - P_Elevation })

sxData$TX_lnMAP <- log(sxData$S_MAP) - log(sxData$P_MAP)
sxData$TX_lnMSP <- log(sxData$S_MSP) - log(sxData$P_MSP)

# sxData$TX_lnMAT <- log(sxData$S_MAT) - log(sxData$P_MAT)
# sxData$TX_lnMCMT <- log(sxData$S_MCMT) - log(sxData$P_MCMT)

sxData$TX_lnLat <- log(sxData$S_Latitude) - log(sxData$P_Latitude)
# sxData$TX_lnLon <- log(sxData$S_Longitude) - log(sxData$P_Longitude)

sxData$TX_lnElev <- log(sxData$S_Elevation) - log(sxData$P_Elevation)


#### Plotting/visualizing data ####

ggplot(data = sxData, aes(x = TX_MAT, y = Height)) + geom_point()
ggplot(data = sxData, aes(y = TX_MAT, x = live)) + geom_bar(stat ="identity")

ggplot(data = sxData, aes(x = TX_MCMT, y = Height)) + geom_point()
ggplot(data = sxData, aes(x = TX_MCMT, y = live)) + geom_boxplot()

ggplot(data = sxData, aes(x = TX_MAP, y = Height)) + geom_point()
ggplot(data = sxData, aes(x = TX_MAP, y = live)) + geom_point()

ggplot(data = sxData, aes(x = TX_MSP, y = Height)) + geom_point()
ggplot(data = sxData, aes(x = TX_MSP, y = live)) + geom_point()

ggplot(data = sxData, aes(x = TX_Lat, y = Height)) + geom_point()
ggplot(data = sxData, aes(x = TX_Lat, y = live)) + geom_point()

ggplot(data = sxData, aes(x = TX_Lon, y = Height)) + geom_point()
ggplot(data = sxData, aes(x = TX_Lon, y = live)) + geom_point()

ggplot(data = sxData, aes(x = TX_Elev, y = Height)) + geom_point()
ggplot(data = sxData, aes(x = TX_Elev, y = live)) + geom_point()

##### Model Fitting #####

# TEMPERATURE #

  model.1 <- lm(Height ~ TX_MCMT + I(TX_MCMT^2), data = sxData) 
    sum.1 <- summary(model.1)
    sum.1 
  # R squared 0.49, standard error 253 #

  model.2 <- lm(Height ~ TX_MCMT + I(TX_MCMT^2) + P_MAT + I(P_MAT^2), data = sxData)
    sum.2 <- summary(model.2)
    sum.2
  # R squared 0.83, standard error 147.7 #

  model.8 <- lm(Height ~ TX_MAT + I(TX_MAT^2) + S_MCMT + I(S_MCMT^2), data = sxData)
    sum.8 <- summary(model.8)
    sum.8
  # R squared 0.83, standard error 147.3 #

# PRECIPITATION #

  model.3 <- lm(Height ~ TX_MAP + I(TX_MAP^2) + P_MSP + I(P_MSP^2), data = sxData)
    sum.3 <- summary(model.3)
    sum.3
  # R squared 0.13, standard error 333 #

  model.4 <- lm(Height ~ TX_MAP + I(TX_MAP^2) + S_MSP + I(S_MSP^2), data = sxData)
    sum.4 <- summary(model.4)
    sum.4
  # R squared 0.79, standard error 163.5 #

  model.5 <- lm(Height ~ TX_lnMAP + I(TX_lnMAP^2) + S_MSP + I(S_MSP^2), data = sxData)
    sum.5 <- summary(model.5)
    sum.5
  # R squared 0.79, standard error 164.1 #

  model.6 <- lm(live ~ TX_lnMSP + I(TX_lnMSP^2) + S_MAP + I(S_MAP^2), data = sxData)
    sum.6 <- summary(model.6)
    sum.6
  # R squared 0.2114, standard error 0.2455 #

  model.7 <- lm(live ~ TX_lnMAP + I(TX_lnMAP^2) + S_MSP + I(S_MSP^2), data = sxData)
    sum.7 <- summary(model.7)
    sum.7
  # R squared 0.2553, standard error 0.2385 #
    
# LONG/LAT/ELEV #
    
  model.9 <- lm(live ~ TX_lnLat + I(TX_lnLat^2) + S_Longitude + I(S_Longitude^2), data = sxData)
    sum.9 <- summary(model.9)
    sum.9
   # R squared 0.2553, standard error 0.2385 #
    
    model.10 <- lm(Height ~ TX_lnLat + I(TX_lnLat^2) + S_Longitude + I(S_Longitude^2), data = sxData)
    sum.10 <- summary(model.10)
    sum.10
    # R squared 161.7, standard error 0.79 #
  
