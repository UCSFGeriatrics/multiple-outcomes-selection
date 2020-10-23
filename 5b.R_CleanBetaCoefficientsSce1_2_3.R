setwd("path")

#install.packages("tidyr")
library(tidyr)


##################################################################################################################################
#baBIC Model
mydata <- read.csv('coefTablebaBICModel.csv')
str(mydata)

#round to 2 decimal places
mydata2<-format(round(mydata[, c(5:7)], 2), nsmall = 2)

#convert numeric to character
mydata2[, 1:ncol(mydata2)] <- lapply(mydata2[,1:ncol(mydata2)], as.character)
str(mydata2)

mydata2[, 1:ncol(mydata2)] <- lapply(mydata2[,1:ncol(mydata2)], trimws) #remove extra blank spaces

mydata3<-unite(mydata2[,c(1,2)], newvar, sep =" (", remove = TRUE, na.rm = FALSE) # concatenate Est and SE
mydata4<-cbind(mydata3, mydata2[,3]) #cbind est (SE with Chi
mydata5<-unite(mydata4, newvar2, sep =") ", remove = TRUE, na.rm = FALSE) #concatenate est (SE) Chi
mydata6<-cbind(mydata[,c(1:3)], mydata5) #cbind with name of estimates and outcomes

write.csv(mydata6, file = "coefTablebaBICclean.csv",row.names=FALSE)


##################################################################################################################################
#Individual Model

mydata <- read.csv('coefTableIndModel.csv')
str(mydata)

#round to 2 decimal places
mydata2<-format(round(mydata[, c(4:6)], 2), nsmall = 2)

#convert numeric to character
mydata2[, 1:ncol(mydata2)] <- lapply(mydata2[,1:ncol(mydata2)], as.character)
str(mydata2)

mydata2[, 1:ncol(mydata2)] <- lapply(mydata2[,1:ncol(mydata2)], trimws) #remove extra blank spaces

mydata3<-unite(mydata2[,c(1,2)], newvar, sep =" (", remove = TRUE, na.rm = FALSE) # concatenate Est and SE
mydata4<-cbind(mydata3, mydata2[,3]) #cbind est (SE with Chi
mydata5<-unite(mydata4, newvar2, sep =") ", remove = TRUE, na.rm = FALSE) #concatenate est (SE) Chi
mydata6<-cbind(mydata[,c(9,1,2)], mydata5) #cbind with name of estimates and outcomes

write.csv(mydata6, file = "coefTableIndClean.csv",row.names=FALSE)


##################################################################################################################################
#Full model
mydata <- read.csv('coefTableFullModel.csv')
str(mydata)

#round to 2 decimal places
mydata2<-format(round(mydata[, c(5:7)], 2), nsmall = 2)

#convert numeric to character
mydata2[, 1:ncol(mydata2)] <- lapply(mydata2[,1:ncol(mydata2)], as.character)
str(mydata2)

mydata2[, 1:ncol(mydata2)] <- lapply(mydata2[,1:ncol(mydata2)], trimws) #remove extra blank spaces

mydata3<-unite(mydata2[,c(1,2)], newvar, sep =" (", remove = TRUE, na.rm = FALSE) # concatenate Est and SE
mydata4<-cbind(mydata3, mydata2[,3]) #cbind est (SE with Chi
mydata5<-unite(mydata4, newvar2, sep =") ", remove = TRUE, na.rm = FALSE) #concatenate est (SE) Chi
mydata6<-cbind(mydata[,c(1:3)], mydata5) #cbind with name of estimates and outcomes

write.csv(mydata6, file = "coefTableFullClean.csv",row.names=FALSE)


