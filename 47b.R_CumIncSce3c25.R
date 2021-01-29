############### Cumulative Incidence Plot: Scenario 3(Full method) 25% censoring   #########################

setwd("path")

#install.packages("gghighlight")
library(ggplot2)
library(gghighlight)
## use ggnewscale to add several color scale in one plot
library(ggnewscale)

dataADL <- read.csv('CumIncSce3c25_adl.csv')
#dataADL0<-subset(dataADL, sim==0)
dataADLmean<-subset(dataADL, sim==501)

dataIADL <- read.csv('CumIncSce3c25_iadl.csv')
#dataIADL0<-subset(dataIADL, sim==0)
dataIADLmean<-subset(dataIADL, sim==501)

dataWalk <- read.csv('CumIncSce3c25_walk.csv')
#dataWalk0<-subset(dataWalk, sim==0)
dataWalkmean<-subset(dataWalk, sim==501)

dataDeath <- read.csv('CumIncSce3c25_death.csv')
#dataDeath0<-subset(dataDeath, sim==0)
dataDeathmean<-subset(dataDeath, sim==501)

dataAll <- read.csv('CumIncSce3c25_all.csv')
#dataAll0<-subset(dataAll, sim==0)
dataAllmean<-subset(dataAll, sim==501)

str(dataAll)
#str(dataAll0)
str(dataAllmean)

#Change the order of the outcomes
dataAll$outcome <- factor(dataAll$outcome, levels = c("adl", "iadl", "walk", "death"))
#dataAll0$outcome <- factor(dataAll0$outcome, levels = c("adl", "iadl", "walk", "death"))
dataAllmean$outcome <- factor(dataAllmean$outcome, levels = c("adl", "iadl", "walk", "death"))

# Modify labels for outcomes
dataAll$outcome <- factor(dataAll$outcome, levels = c("adl", "iadl", "walk","death"), labels = c("ADL", "IADL", "WALK", "DEATH"))
#dataAll0$outcome <- factor(dataAll0$outcome, levels = c("adl", "iadl", "walk","death"), labels = c("ADL", "IADL", "WALK", "DEATH"))
dataAllmean$outcome <- factor(dataAllmean$outcome, levels = c("adl", "iadl", "walk","death"), labels = c("ADL", "IADL", "WALK", "DEATH"))


########################### color version ##################################
png("CumIncSce3c25.png",width = 1800, height = 1500, res = 600)
ggplot() +
  # draw the original cif series with grey
  geom_line(aes(time, cif, group = sim, color=outcome), data = dataAll,alpha=0.7,size=0.01) +
  ##added color for simulation line (picked lighter color than the actual line)
  scale_color_manual(guide = FALSE,values=c('#7FB3D5','#F5B7B1','#A9DFBF','#EDBB99'))+
  # colorise only the filtered data
  ## added color for the actual line,
  ## new_scale_color() is used to use different color scale
  #new_scale_color() +
  #geom_line(aes(time, cif,color = outcome), data = dataAll0,size=0.2, lty=5) +
  #scale_color_manual(guide = FALSE,values=c('blue','#ff0000','darkgreen','brown'))+
  # do the same for the mean Cumulative Incidence function
  new_scale_color() +
  geom_line(aes(time, cif,color = outcome), data = dataAllmean,size=0.2) +
  scale_color_manual(guide = FALSE,values=c('blue','#ff0000','darkgreen','brown'))+
  #One panel / outcome
  facet_wrap(~ outcome) +
  #White background with grid lines
  theme_bw() +
  theme(strip.background =element_rect(color="black",fill="white")) +
  theme(strip.text = element_text(face="bold")) +
  ##removed box around the facet title;decrese the margin between panels and text;
  theme(strip.background = element_blank())+
  theme(panel.spacing = unit(0.2, "lines"))+
  theme(text = element_text(size=8))+
  theme(strip.text.x = element_text( margin = margin( b = 1, t = 0) ) )+
  #Change the x axis name
  scale_x_continuous(name ="Time to event", breaks=seq(0,14,2)) +
  labs(y="Cumulative Incidence") + 
  # Remove grid lines
  # theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),)
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold")) +
  theme(panel.grid.minor = element_line(size = 0.2), 
        panel.grid.major = element_line(size = 0.2))
dev.off()

pdf('CumIncSce3c25.pdf')
ggplot() +
  # draw the original cif series with grey
  geom_line(aes(time, cif, group = sim, color=outcome), data = dataAll,alpha=0.7,size=0.01) +
  ##added color for simulation line (picked lighter color than the actual line)
  scale_color_manual(guide = FALSE,values=c('#7FB3D5','#F5B7B1','#A9DFBF','#EDBB99'))+
  # colorise only the filtered data
  ## added color for the actual line,
  ## new_scale_color() is used to use different color scale
  #new_scale_color() +
  #geom_line(aes(time, cif,color = outcome), data = dataAll0,size=0.2, lty=5) +
  #scale_color_manual(guide = FALSE,values=c('blue','#ff0000','darkgreen','brown'))+
  # do the same for the mean Cumulative Incidence function
  new_scale_color() +
  geom_line(aes(time, cif,color = outcome), data = dataAllmean,size=0.2) +
  scale_color_manual(guide = FALSE,values=c('blue','#ff0000','darkgreen','brown'))+
  #One panel / outcome
  facet_wrap(~ outcome) +
  #White background with grid lines
  theme_bw() +
  theme(strip.background =element_rect(color="black",fill="white")) +
  theme(strip.text = element_text(face="bold")) +
  ##removed box around the facet title;decrese the margin between panels and text;
  theme(strip.background = element_blank())+
  theme(panel.spacing = unit(0.2, "lines"))+
  theme(text = element_text(size=12))+
  theme(strip.text.x = element_text( margin = margin( b = 1, t = 0) ) )+
  #Change the x axis name
  scale_x_continuous(name ="Time to event", breaks=seq(0,14,2)) +
  labs(y="Cumulative Incidence") + 
  # Remove grid lines
  # theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),)
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold")) +
  theme(panel.grid.minor = element_line(size = 0.2), 
        panel.grid.major = element_line(size = 0.2))
dev.off()

