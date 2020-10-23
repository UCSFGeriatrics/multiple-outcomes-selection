############### PLOT BIC vs Number of predictors   #########################

setwd("path")

options(digits=22)

##### baBIC selection
datbaBIC <- read.csv('Report_BackElimBICComb39varsNew_gdr_20200218.csv')
View(datbaBIC)
colnames(datbaBIC)

colVals <- c("gray0")
#colVals <- c("gray0", "dodgerblue4", "darkorange", "forestgreen", "red4")
#Colors correspond with groups: black=baBIC, blue=ADL, orange=iadl, green=walk, red=death

#Add number of predictors
datbaBIC$numPred<-seq(39, 2, -1)
numpred<-seq(39, 2, -1)

#Create objects for each variable so that we can use the objects to create graph below
colnames(datbaBIC) 
baBIC<-datbaBIC [,23]
range(baBIC) #0.23544 1.29268
print(datbaBIC[which(datbaBIC$BIC_avg == 0.23544 ) ,  c("numPred", "BIC_avg", "VARINMODEL")] ) #15 variables in the model
min_baBIC <- with(datbaBIC, datbaBIC[BIC_avg == 0.23544 , ])
min_baBIC <-min_baBIC[,c(24,23)]
min_baBIC


##### ADL Individual outcome selection
datadl <- read.csv('Report_BackElimBICadl39vars_gdr_20190115.csv')
View(datadl)
colnames(datadl)
datadl$numPred<-seq(39, 2, -1)

#Create objects for each variable so that we can use the objects to create graph below
BIC_adl<-datadl [,8]
range(BIC_adl) #30824.38 31101.14
print(datadl[which(datadl$BIC_adl == 30824.38 ) ,  c("numPred", "BIC_adl", "VARINMODEL")] ) #10 variables in the model
min_adl <- with(datadl, datadl[BIC_adl == 30824.38 , ])
min_adl <-min_adl[,c(9,8)]
min_adl


##### IADL Individual outcome selection
datiadl <- read.csv('Report_BackElimBICiadl39vars_gdr_20190115.csv')
View(datiadl)
colnames(datiadl)
datiadl$numPred<-seq(39, 2, -1)

#Create objects for each variable so that we can use the objects to create graph below
BIC_iadl<-datiadl [,8]
range(BIC_iadl) #32256.51 32552.61: 32256.508099999999 32552.609899999999
print(datiadl[which(datiadl$BIC_iadl == 32256.508099999999 ) ,  c("numPred", "BIC_iadl", "VARINMODEL")] ) #10 variables in the model
min_iadl <- with(datiadl, datiadl[BIC_iadl == 32256.508099999999 , ])
min_iadl <-min_iadl[,c(9,8)]
min_iadl


##### WALK Individual outcome selection
datwalk <- read.csv('Report_BackElimBICwalk39vars_gdr_20190115.csv')
View(datwalk)
colnames(datwalk)
datwalk$numPred<-seq(39, 2, -1)

#Create objects for each variable so that we can use the objects to create graph below
BIC_walk<-datwalk [,8]
range(BIC_walk) #16892.152500000000 17184.999800000001
print(datwalk[which(datwalk$BIC_walk == 16892.152500000000 ) ,  c("numPred", "BIC_walk", "VARINMODEL")] ) #7 variables in the model
min_walk <- with(datwalk, datwalk[BIC_walk == 16892.152500000000 , ])
min_walk <-min_walk[,c(9,8)]
min_walk


##### DEATH Individual outcome selection
datdeath <- read.csv('Report_BackElimBICdeath39vars_gdr_20190115.csv')
View(datdeath)
colnames(datdeath)
datdeath$numPred<-seq(39, 2, -1)

#Create objects for each variable so that we can use the objects to create graph below
BIC_death<-datdeath [,8]
range(BIC_death) #59593.181900000003 60344.138099999996
print(datdeath[which(datdeath$BIC_death == 59593.181900000003 ) ,  c("numPred", "BIC_death", "VARINMODEL")] ) #10 variables in the model
min_death <- with(datdeath, datdeath[BIC_death == 59593.181900000003 , ])
min_death <-min_death[,c(9,8)]
min_death

##################################################### ADL #####################################################  
png('baBIC_BICadl_R.png')

par(las=1, mar = c(6.5,5.1,4.1,6.9)) # make right inner margin larger to accomadate legend and second y axis

plot(range(numpred), range(baBIC), pch='', xlab='', ylab='', xlim=c(39,2), ylim=c(0.2,1.3))
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(15, 0.2, "15", xpd=TRUE, cex=0.9)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_adl), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(30800,31110)) # Create second plot without axes
lines(numpred, BIC_adl,col=colVals[1],lwd=1, lty=2)
points(min_adl, col=colVals[1], pch=1, cex=1.5)
text(10, 30810, "10", xpd=TRUE, cex=0.9) 

axis(4) # Add second axis label
mtext("BIC", side = 4, line = 4, las=0) 


title(xlab='Number of predictors in model', ylab="baBIC")

legend('bottomright', lty=c(1,2), col=colVals, legend=(c("baBIC Selection", "BIC ADL Selection")), bty='n', inset=c(-0.3,-0.2), box.lty=0, xpd=TRUE)


dev.off()


pdf('baBIC_BICadl_R.pdf')

par(las=1, mar = c(6.5,5.1,4.1,6.9)) # make right inner margin larger to accomadate legend and second y axis

plot(range(numpred), range(baBIC), pch='', xlab='', ylab='', xlim=c(39,2), ylim=c(0.2,1.3))
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(15, 0.2, "15", xpd=TRUE, cex=0.9)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_adl), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(30800,31110)) # Create second plot without axes
lines(numpred, BIC_adl,col=colVals[1],lwd=1, lty=2)
points(min_adl, col=colVals[1], pch=1, cex=1.5)
text(10, 30810, "10", xpd=TRUE, cex=0.9) 

axis(4) # Add second axis label
mtext("BIC", side = 4, line = 4, las=0) 


title(xlab='Number of predictors in model', ylab="baBIC")

legend('bottomright', lty=c(1,2), col=colVals, legend=(c("baBIC Selection", "BIC ADL Selection")), bty='n', inset=c(-0.3,-0.2), box.lty=0, xpd=TRUE)


dev.off()

##################################################### IADL #####################################################  

png('baBIC_BICiadl_R.png')

par(las=1, mar = c(6.5,5.1,4.1,6.9)) # make right inner margin larger to accomadate legend and second y axis

plot(range(numpred), range(baBIC), pch='', xlab='', ylab='', xlim=c(39,2), ylim=c(0.2,1.3))
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(15, 0.2, "15", xpd=TRUE, cex=0.9)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_iadl), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(32250,32555)) # Create second plot without axes
lines(numpred, BIC_iadl,col=colVals[1],lwd=1, lty=2)
points(min_iadl, col=colVals[1], pch=1, cex=1.5)
text(9, 32246, "9", xpd=TRUE, cex=0.9) 

axis(4) # Add second axis label
mtext("BIC", side = 4, line = 4, las=0) 


title(xlab='Number of predictors in model', ylab="baBIC")

legend('bottomright', lty=c(1,2), col=colVals, legend=(c("baBIC Selection", "BIC IADL Selection")), bty='n', inset=c(-0.3,-0.2), box.lty=0, xpd=TRUE)


dev.off()


pdf('baBIC_BICiadl_R.pdf')

par(las=1, mar = c(6.5,5.1,4.1,6.9)) # make right inner margin larger to accomadate legend and second y axis

plot(range(numpred), range(baBIC), pch='', xlab='', ylab='', xlim=c(39,2), ylim=c(0.2,1.3))
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(15, 0.2, "15", xpd=TRUE, cex=0.9)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_iadl), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(32250,32555)) # Create second plot without axes
lines(numpred, BIC_iadl,col=colVals[1],lwd=1, lty=2)
points(min_iadl, col=colVals[1], pch=1, cex=1.5)
text(9, 32246, "9", xpd=TRUE, cex=0.9) 

axis(4) # Add second axis label
mtext("BIC", side = 4, line = 4, las=0) 


title(xlab='Number of predictors in model', ylab="baBIC")

legend('bottomright', lty=c(1,2), col=colVals, legend=(c("baBIC Selection", "BIC IADL Selection")), bty='n', inset=c(-0.3,-0.2), box.lty=0, xpd=TRUE)

dev.off()

##################################################### WALK #####################################################  

png('baBIC_BICwalk_R.png')

par(las=1, mar = c(6.5,5.1,4.1,6.9)) # make right inner margin larger to accomadate legend and second y axis

plot(range(numpred), range(baBIC), pch='', xlab='', ylab='', xlim=c(39,2), ylim=c(0.2,1.3))
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(15, 0.2, "15", xpd=TRUE, cex=0.9)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_walk), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(16885,17190)) # Create second plot without axes
lines(numpred, BIC_walk,col=colVals[1],lwd=1, lty=2)
points(min_walk, col=colVals[1], pch=1, cex=1.5)
text(7, 16881, "7", xpd=TRUE, cex=0.9) 

axis(4) # Add second axis label
mtext("BIC", side = 4, line = 4, las=0) 


title(xlab='Number of predictors in model', ylab="baBIC")

legend('bottomright', lty=c(1,2), col=colVals, legend=(c("baBIC Selection", "BIC Walk Selection")), bty='n', inset=c(-0.33,-0.2), box.lty=0, xpd=TRUE)


dev.off()


pdf('baBIC_BICwalk_R.pdf')

par(las=1, mar = c(6.5,5.1,4.1,6.9)) # make right inner margin larger to accomadate legend and second y axis

plot(range(numpred), range(baBIC), pch='', xlab='', ylab='', xlim=c(39,2), ylim=c(0.2,1.3))
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(15, 0.2, "15", xpd=TRUE, cex=0.9)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_walk), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(16885,17190)) # Create second plot without axes
lines(numpred, BIC_walk,col=colVals[1],lwd=1, lty=2)
points(min_walk, col=colVals[1], pch=1, cex=1.5)
text(7, 16881, "7", xpd=TRUE, cex=0.9) 

axis(4) # Add second axis label
mtext("BIC", side = 4, line = 4, las=0) 


title(xlab='Number of predictors in model', ylab="baBIC")

legend('bottomright', lty=c(1,2), col=colVals, legend=(c("baBIC Selection", "BIC Walk Selection")), bty='n', inset=c(-0.31,-0.2), box.lty=0, xpd=TRUE)


dev.off()



##################################################### DEATH #####################################################  

png('baBIC_BICdeath_R.png')

par(las=1, mar = c(6.5,5.1,4.1,6.9)) # make right inner margin larger to accomadate legend and second y axis

plot(range(numpred), range(baBIC), pch='', xlab='', ylab='', xlim=c(39,2), ylim=c(0.2,1.3))
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(15, 0.2, "15", xpd=TRUE, cex=0.9)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_death), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(59570,60350)) # Create second plot without axes
lines(numpred, BIC_death,col=colVals[1],lwd=1, lty=2)
points(min_death, col=colVals[1], pch=1, cex=1.5)
text(17, 59570, "16", xpd=TRUE, cex=0.9) 

axis(4) # Add second axis label
mtext("BIC", side = 4, line = 4, las=0) 


title(xlab='Number of predictors in model', ylab="baBIC")

legend('bottomright', lty=c(1,2), col=colVals, legend=(c("baBIC Selection", "BIC Death Selection")), bty='n', inset=c(-0.33,-0.2), box.lty=0, xpd=TRUE)


dev.off()


pdf('baBIC_BICdeath_R.pdf')

par(las=1, mar = c(6.5,5.1,4.1,6.9)) # make right inner margin larger to accomadate legend and second y axis

plot(range(numpred), range(baBIC), pch='', xlab='', ylab='', xlim=c(39,2), ylim=c(0.2,1.3))
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(15, 0.2, "15", xpd=TRUE, cex=0.9)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_death), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(59570,60350)) # Create second plot without axes
lines(numpred, BIC_death,col=colVals[1],lwd=1, lty=2)
points(min_death, col=colVals[1], pch=1, cex=1.5)
text(17, 59570, "16", xpd=TRUE, cex=0.9) 

axis(4) # Add second axis label
mtext("BIC", side = 4, line = 4, las=0) 


title(xlab='Number of predictors in model', ylab="baBIC")

legend('bottomright', lty=c(1,2), col=colVals, legend=(c("baBIC Selection", "BIC Death Selection")), bty='n', inset=c(-0.31,-0.2), box.lty=0, xpd=TRUE)


dev.off()


##################################################### ALL PNG #####################################################  
png('baBIC_BICall_R.png')

# Draw plots in 2 row and 2 columns.
par(mfrow=c(2,2), 
    # Set the bottom, left, top, and right inner margins.
    mar=c(1.5, 2.5, 1.5, 2.5), 
    # Set the bottom, left, top, and right outer margins.
    oma=c(4.5, 3.5, .5, 3.5),
    las=1)

## ADL
plot(range(numpred), range(baBIC), pch='', axes=T, xaxt="n", xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="ADL")
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(15, 0.18, "15", xpd=TRUE, cex=0.9) #min=0.23544
xtick<-seq(40, 10, by=-10)
axis(side=1, at=xtick, labels = FALSE) #axis=1 below

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_adl), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(30800,31110)) # Create second plot without axes
lines(numpred, BIC_adl,col=colVals[1],lwd=1, lty=2)
points(min_adl, col=colVals[1], pch=1, cex=1.5)
text(10, 30805, "10", xpd=TRUE, cex=0.9) #min=30824.38

axis(side=4) # axis(side,) an integer specifying which side of the plot the axis is to be drawn on. 
#The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
mtext("baBIC", side=2, outer=T, line=1, cex=1.1, las=0, font=2)

box() # Redraw the box around the plot

## IADL
plot(range(numpred), range(baBIC), pch='', axes = FALSE, xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="IADL")
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(15, 0.18, "15", xpd=TRUE, cex=0.9) 
xtick<-seq(40, 10, by=-10)
axis(side=1, at=xtick, labels = FALSE) #axis=1 below
ytick<-seq(0.2, 1.2, by=0.2)
axis(side=2, at=ytick, labels = FALSE)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_iadl), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(32225,32555)) # Create second plot without axes
lines(numpred, BIC_iadl,col=colVals[1],lwd=1, lty=2)
points(min_iadl, col=colVals[1], pch=1, cex=1.5)
text(9, 32240, "9", xpd=TRUE, cex=0.9) #min=32256.508099999999

axis(side=4) # Add second axis label
mtext("BIC", side=4, outer=T, line=1, cex=1.1, las=0, font=2) 

box() # Redraw the box around the plot


## WALK
plot(range(numpred), range(baBIC), pch='', axes = T,  xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="Walk")
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(15, 0.18, "15", xpd=TRUE, cex=0.9)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_walk), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(16850,17200)) # Create second plot without axes
lines(numpred, BIC_walk,col=colVals[1],lwd=1, lty=2)
points(min_walk, col=colVals[1], pch=1, cex=1.5)
text(7, 16875, "7", xpd=TRUE, cex=0.9) #min=16892.1525

axis(side=4) # Add second axis label
mtext("Number of predictors in model", side=1, outer=T, line=1, cex=1.1, las=0, font=2)

box() # Redraw the box around the plot


## Death
plot(range(numpred), range(baBIC), pch='', axes=T, yaxt="n", xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="Death")
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(14, 0.18, "15", xpd=TRUE, cex=0.9)
ytick<-seq(0.2, 1.2, by=0.2)
axis(side=2, at=ytick, labels = FALSE)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_death), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(59550,60350)) # Create second plot without axes
lines(numpred, BIC_death,col=colVals[1],lwd=1, lty=2)
points(min_death, col=colVals[1], pch=1, cex=1.5)
text(17, 59560, "16", xpd=TRUE, cex=0.9) #min=59593.1819

axis(side=4) # Add second axis label

box() # Redraw the box around the plot

legend('bottomright', col=colVals, lty=c(1,2), legend=(c("baBIC Selection", "BIC Individual Outcome Selection")), bty='n', xpd=NA, inset=c(-0.55,-0.45))
#Note:
#An important point to note here is that the xpd argument in the legend function which control if all plot elements
#(ie points, lines, legend, text .) are clipped to the plotting region if it is set to FALSE (the default value).
#If it is set to TRUE all plot elements are clipped to the figure region (plot + inner margins)
#and if it is set to NA you can basically add plot elements everywhere in the device region (plot + inner margins + outer margins).
#In the example above we set it to NA. Note that the xpd argument can also be set within the par function,
#it is then applied to all subsequent plots

dev.off()


##################################################### ALL PDF #####################################################  
pdf('baBIC_BICall_R.pdf')

# Draw plots in 2 row and 2 columns.
par(mfrow=c(2,2), 
    # Set the bottom, left, top, and right inner margins.
    mar=c(1.5, 2.5, 1.5, 2.5), 
    # Set the bottom, left, top, and right outer margins.
    oma=c(4.5, 3.5, .5, 3.5),
    las=1)

## ADL
plot(range(numpred), range(baBIC), pch='', axes=T, xaxt="n", xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="ADL")
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(15, 0.18, "15", xpd=TRUE, cex=0.9) #min=0.23544
xtick<-seq(40, 10, by=-10)
axis(side=1, at=xtick, labels = FALSE) #axis=1 below

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_adl), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(30800,31110)) # Create second plot without axes
lines(numpred, BIC_adl,col=colVals[1],lwd=1, lty=2)
points(min_adl, col=colVals[1], pch=1, cex=1.5)
text(10, 30805, "10", xpd=TRUE, cex=0.9) #min=30824.38

axis(side=4) # axis(side,) an integer specifying which side of the plot the axis is to be drawn on. 
#The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
mtext("baBIC", side=2, outer=T, line=1, cex=1.1, las=0, font=2)

box() # Redraw the box around the plot

## IADL
plot(range(numpred), range(baBIC), pch='', axes = FALSE, xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="IADL")
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(15, 0.18, "15", xpd=TRUE, cex=0.9) 
xtick<-seq(40, 10, by=-10)
axis(side=1, at=xtick, labels = FALSE) #axis=1 below
ytick<-seq(0.2, 1.2, by=0.2)
axis(side=2, at=ytick, labels = FALSE)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_iadl), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(32225,32555)) # Create second plot without axes
lines(numpred, BIC_iadl,col=colVals[1],lwd=1, lty=2)
points(min_iadl, col=colVals[1], pch=1, cex=1.5)
text(9, 32240, "9", xpd=TRUE, cex=0.9) #min=32256.508099999999

axis(side=4) # Add second axis label
mtext("BIC", side=4, outer=T, line=1, cex=1.1, las=0, font=2) 

box() # Redraw the box around the plot


## WALK
plot(range(numpred), range(baBIC), pch='', axes = T,  xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="Walk")
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(15, 0.18, "15", xpd=TRUE, cex=0.9)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_walk), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(16850,17200)) # Create second plot without axes
lines(numpred, BIC_walk,col=colVals[1],lwd=1, lty=2)
points(min_walk, col=colVals[1], pch=1, cex=1.5)
text(7, 16875, "7", xpd=TRUE, cex=0.9) #min=16892.1525

axis(side=4) # Add second axis label
mtext("Number of predictors in model", side=1, outer=T, line=1, cex=1.1, las=0, font=2)

box() # Redraw the box around the plot


## Death
plot(range(numpred), range(baBIC), pch='', axes=T, yaxt="n", xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="Death")
lines(numpred, baBIC,col=colVals[1],lwd=1, lty=1)
points(min_baBIC, col=colVals[1], pch=1, cex=1.5)
text(14, 0.18, "15", xpd=TRUE, cex=0.9)
ytick<-seq(0.2, 1.2, by=0.2)
axis(side=2, at=ytick, labels = FALSE)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_death), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(59550,60350)) # Create second plot without axes
lines(numpred, BIC_death,col=colVals[1],lwd=1, lty=2)
points(min_death, col=colVals[1], pch=1, cex=1.5)
text(17, 59560, "16", xpd=TRUE, cex=0.9) #min=59593.1819

axis(side=4) # Add second axis label

box() # Redraw the box around the plot

legend('bottomright', col=colVals, lty=c(1,2), legend=(c("baBIC Selection", "BIC Individual Outcome Selection")), bty='n', xpd=NA, inset=c(-0.50,-0.40))

dev.off()

#################################Color Version#########################################################

##################################################### ALL PNG #########################################  
colVals <- c('blue','#ff0000','darkgreen','brown','#ff00ff')
colVal2 <- c("gray0")
png('baBIC_BICall_R_color.png',width = 1500, height = 1600, res = 600,pointsize=4)

# Draw plots in 2 row and 2 columns.
par(mfrow=c(2,2), 
    # Set the bottom, left, top, and right inner margins.
    mar=c(1.5, 2.5, 1.5, 2.5), 
    # Set the bottom, left, top, and right outer margins.
    oma=c(4.5, 3.5, .5, 3.5),
    las=1)

## ADL
plot(range(numpred), range(baBIC), pch='', axes=T, xaxt="n", xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="ADL")
lines(numpred, baBIC,col=colVals[5],lwd=1, lty=1)
points(min_baBIC, col=colVals[5], pch=1, cex=1.5)
text(15, 0.18, "15", xpd=TRUE, cex=0.9) #min=0.23544
xtick<-seq(40, 10, by=-10)
axis(side=1, at=xtick, labels = FALSE) #axis=1 below

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_adl), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(30800,31110)) # Create second plot without axes
lines(numpred, BIC_adl,col=colVals[1],lwd=1, lty=2)
points(min_adl, col=colVals[1], pch=1, cex=1.5)
text(10, 30805, "10", xpd=TRUE, cex=0.9) #min=30824.38

axis(side=4) # axis(side,) an integer specifying which side of the plot the axis is to be drawn on. 
#The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
mtext("Average nBIC", side=2, outer=T, line=1, cex=1.1, las=0, font=2)

box() # Redraw the box around the plot

## IADL
plot(range(numpred), range(baBIC), pch='', axes = FALSE, xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="IADL")
lines(numpred, baBIC,col=colVals[5],lwd=1, lty=1)
points(min_baBIC, col=colVals[5], pch=1, cex=1.5)
text(15, 0.18, "15", xpd=TRUE, cex=0.9) 
xtick<-seq(40, 10, by=-10)
axis(side=1, at=xtick, labels = FALSE) #axis=1 below
ytick<-seq(0.2, 1.2, by=0.2)
axis(side=2, at=ytick, labels = FALSE)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_iadl), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(32225,32555)) # Create second plot without axes
lines(numpred, BIC_iadl,col=colVals[2],lwd=1, lty=2)
points(min_iadl, col=colVals[2], pch=1, cex=1.5)
text(9, 32240, "9", xpd=TRUE, cex=0.9) #min=32256.508099999999

axis(side=4) # Add second axis label
mtext("BIC", side=4, outer=T, line=1, cex=1.1, las=0, font=2) 

box() # Redraw the box around the plot


## WALK
plot(range(numpred), range(baBIC), pch='', axes = T,  xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="WALK")
lines(numpred, baBIC,col=colVals[5],lwd=1, lty=1)
points(min_baBIC, col=colVals[5], pch=1, cex=1.5)
text(15, 0.18, "15", xpd=TRUE, cex=0.9)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_walk), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(16850,17200)) # Create second plot without axes
lines(numpred, BIC_walk,col=colVals[3],lwd=1, lty=2)
points(min_walk, col=colVals[3], pch=1, cex=1.5)
text(7, 16875, "7", xpd=TRUE, cex=0.9) #min=16892.1525

axis(side=4) # Add second axis label
mtext("Number of predictors in model", side=1, outer=T, line=1, cex=1.1, las=0, font=2)

box() # Redraw the box around the plot


## Death
plot(range(numpred), range(baBIC), pch='', axes=T, yaxt="n", xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="DEATH")
lines(numpred, baBIC,col=colVals[5],lwd=1, lty=1)
points(min_baBIC, col=colVals[5], pch=1, cex=1.5)
text(14, 0.18, "15", xpd=TRUE, cex=0.9)
ytick<-seq(0.2, 1.2, by=0.2)
axis(side=2, at=ytick, labels = FALSE)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_death), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(59550,60350)) # Create second plot without axes
lines(numpred, BIC_death,col=colVals[4],lwd=1, lty=2)
points(min_death, col=colVals[4], pch=1, cex=1.5)
text(17, 59560, "16", xpd=TRUE, cex=0.9) #min=59593.1819

axis(side=4) # Add second axis label

box() # Redraw the box around the plot

legend('bottomright', col=colVal2[1], lty=c(1,2), legend=(c("baBIC Method", "Individual Outcome Method")), bty='n', xpd=NA, inset=c(-0.3,-0.35))
#Note:
#An important point to note here is that the xpd argument in the legend function which control if all plot elements
#(ie points, lines, legend, text .) are clipped to the plotting region if it is set to FALSE (the default value).
#If it is set to TRUE all plot elements are clipped to the figure region (plot + inner margins)
#and if it is set to NA you can basically add plot elements everywhere in the device region (plot + inner margins + outer margins).
#In the example above we set it to NA. Note that the xpd argument can also be set within the par function,
#it is then applied to all subsequent plots

dev.off()

##################################################### ALL PDF #####################################################  
pdf('baBIC_BICall_R_color.pdf')

# Draw plots in 2 row and 2 columns.
par(mfrow=c(2,2), 
    # Set the bottom, left, top, and right inner margins.
    mar=c(1.5, 2.5, 1.5, 2.5), 
    # Set the bottom, left, top, and right outer margins.
    oma=c(4.5, 3.5, .5, 3.5),
    las=1)

## ADL
plot(range(numpred), range(baBIC), pch='', axes=T, xaxt="n", xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="ADL")
lines(numpred, baBIC,col=colVals[5],lwd=1, lty=1)
points(min_baBIC, col=colVals[5], pch=1, cex=1.5)
text(15, 0.18, "15", xpd=TRUE, cex=0.9) #min=0.23544
xtick<-seq(40, 10, by=-10)
axis(side=1, at=xtick, labels = FALSE) #axis=1 below

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_adl), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(30800,31110)) # Create second plot without axes
lines(numpred, BIC_adl,col=colVals[1],lwd=1, lty=2)
points(min_adl, col=colVals[1], pch=1, cex=1.5)
text(10, 30805, "10", xpd=TRUE, cex=0.9) #min=30824.38

axis(side=4) # axis(side,) an integer specifying which side of the plot the axis is to be drawn on. 
#The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
mtext("Average nBIC", side=2, outer=T, line=1, cex=1.1, las=0, font=2)

box() # Redraw the box around the plot

## IADL
plot(range(numpred), range(baBIC), pch='', axes = FALSE, xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="IADL")
lines(numpred, baBIC,col=colVals[5],lwd=1, lty=1)
points(min_baBIC, col=colVals[5], pch=1, cex=1.5)
text(15, 0.18, "15", xpd=TRUE, cex=0.9) 
xtick<-seq(40, 10, by=-10)
axis(side=1, at=xtick, labels = FALSE) #axis=1 below
ytick<-seq(0.2, 1.2, by=0.2)
axis(side=2, at=ytick, labels = FALSE)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_iadl), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(32225,32555)) # Create second plot without axes
lines(numpred, BIC_iadl,col=colVals[2],lwd=1, lty=2)
points(min_iadl, col=colVals[2], pch=1, cex=1.5)
text(9, 32240, "9", xpd=TRUE, cex=0.9) #min=32256.508099999999

axis(side=4) # Add second axis label
mtext("BIC", side=4, outer=T, line=1, cex=1.1, las=0, font=2) 

box() # Redraw the box around the plot


## WALK
plot(range(numpred), range(baBIC), pch='', axes = T,  xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="WALK")
lines(numpred, baBIC,col=colVals[5],lwd=1, lty=1)
points(min_baBIC, col=colVals[5], pch=1, cex=1.5)
text(15, 0.18, "15", xpd=TRUE, cex=0.9)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_walk), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(16850,17200)) # Create second plot without axes
lines(numpred, BIC_walk,col=colVals[3],lwd=1, lty=2)
points(min_walk, col=colVals[3], pch=1, cex=1.5)
text(7, 16875, "7", xpd=TRUE, cex=0.9) #min=16892.1525

axis(side=4) # Add second axis label
mtext("Number of predictors in model", side=1, outer=T, line=1, cex=1.1, las=0, font=2)

box() # Redraw the box around the plot


## Death
plot(range(numpred), range(baBIC), pch='', axes=T, yaxt="n", xlab='', ylab='', xlim=c(39,2), ylim=c(0.15,1.3), main="DEATH")
lines(numpred, baBIC,col=colVals[5],lwd=1, lty=1)
points(min_baBIC, col=colVals[5], pch=1, cex=1.5)
text(14, 0.18, "15", xpd=TRUE, cex=0.9)
ytick<-seq(0.2, 1.2, by=0.2)
axis(side=2, at=ytick, labels = FALSE)

par(new = TRUE) # Add new plot

plot(range(numpred), range(BIC_death), pch='', axes = FALSE, xlab = "", ylab = "", xlim=c(39,2),  ylim=c(59550,60350)) # Create second plot without axes
lines(numpred, BIC_death,col=colVals[4],lwd=1, lty=2)
points(min_death, col=colVals[4], pch=1, cex=1.5)
text(17, 59560, "16", xpd=TRUE, cex=0.9) #min=59593.1819

axis(side=4) # Add second axis label

box() # Redraw the box around the plot

legend('bottomright', col=colVal2[1], lty=c(1,2), legend=(c("baBIC Method", "Individual Outcome Method")), bty='n', xpd=NA, inset=c(-0.50,-0.40))

dev.off()







