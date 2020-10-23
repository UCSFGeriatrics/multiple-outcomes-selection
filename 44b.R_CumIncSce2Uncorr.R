############### CumInc Plot: Scenario 2 uncorrelated outcomes   #########################

setwd("path")

#install.packages("gghighlight")
library(ggplot2)
library(gghighlight)
##use ggnewscale to add several color scale in one plot
library(ggnewscale)

dataADL <- read.csv('CumIncbaBICuncorrby_adl.csv')
dataADL0<-subset(dataADL, sim==0)

dataIADL <- read.csv('CumIncbaBICuncorrby_iadl.csv')
dataIADL0<-subset(dataIADL, sim==0)

dataWalk <- read.csv('CumIncbaBICuncorrby_walk.csv')
dataWalk0<-subset(dataWalk, sim==0)

dataDeath <- read.csv('CumIncbaBICuncorrby_death.csv')
dataDeath0<-subset(dataDeath, sim==0)

dataAll <- read.csv('CumIncbaBICuncorrby_all.csv')
dataAll0<-subset(dataAll, sim==0)
str(dataAll)
str(dataAll0)

#Change the order of the outcomes
dataAll$outcome <- factor(dataAll$outcome, levels = c("adl", "iadl", "walk", "death"))
dataAll0$outcome <- factor(dataAll0$outcome, levels = c("adl", "iadl", "walk", "death"))

# Modify labels for outcomes
dataAll$outcome <- factor(dataAll$outcome, levels = c("adl", "iadl", "walk","death"), labels = c("ADL", "IADL", "WALK", "DEATH"))
dataAll0$outcome <- factor(dataAll0$outcome, levels = c("adl", "iadl", "walk","death"), labels = c("ADL", "IADL", "WALK", "DEATH"))


png('CumIncSce2uncorr_R.png')

ggplot() +
    # draw the original cif series with grey
    geom_line(aes(time, cif, group = sim, color = "grey"), data = dataAll ) +
    # colorise only the filtered data
    geom_line(aes(time, cif,color = "black"), data = dataAll0 ) + 
    #One panel / outcome
    facet_wrap(~ outcome) +
    #White background with grid lines
    theme_bw() +
    theme(strip.background =element_rect(color="black",fill="white")) +
    theme(strip.text = element_text(face="bold")) +
    #Change the x axis name
    scale_x_discrete(name ="Time to event", limits=seq(2,14,2)) +
    labs(y="Cumulative Incidence") + 
    # Remove grid lines
    #theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),)
    theme(axis.title.x = element_text(face="bold")) +
    theme(axis.title.y = element_text(face="bold")) +
    scale_color_identity(breaks = c("grey", "black"),
                         labels = c("Simulations", "Case-study data"),
                         guide = "legend") +
    theme(legend.position="bottom",
          legend.text=element_text(size=12),
          legend.title = element_blank(),
          legend.direction ="vertical",
          legend.justification = c(1, 0) )


dev.off()


#Notes:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
#setting the colors as constants is done outside aes()
#Add a legend by moving aesthetics into aes()
#The values are no longer recognized as colors since aes() treats these as string constants.
#To get the desired colors we'll need to turn to one of the scale_color_*() functions.
#One way to force ggplot to recognize the color names when they are inside aes() is to use scale_color_identity()
#To get a legend with an identity scale you must use guide = "legend". (The default is guide = "none" for identity scales.)
#The legend name can be changed via name, the order can be changes via breaks and
 #the labels can be changed via labels in scale_color_identity(). The order of the labels must be the same as the order of the breaks.



pdf('CumIncSce2uncorr_R.pdf')

ggplot() +
  # draw the original cif series with grey
  geom_line(aes(time, cif, group = sim, color = "grey"), data = dataAll ) +
  # colorise only the filtered data
  geom_line(aes(time, cif,color = "black"), data = dataAll0 ) + 
  #One panel / outcome
  facet_wrap(~ outcome) +
  #White background with grid lines
  theme_bw() +
  theme(strip.background =element_rect(color="black",fill="white")) +
  theme(strip.text = element_text(face="bold")) +
  #Change the x axis name
  scale_x_discrete(name ="Time to event", limits=seq(2,14,2)) +
  labs(y="Cumulative Incidence") + 
  # Remove grid lines
  #theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),)
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold")) +
  scale_color_identity(breaks = c("grey", "black"),
                       labels = c("Simulations", "Case-study data"),
                       guide = "legend") +
  theme(legend.position="bottom",
        legend.text=element_text(size=12),
        legend.title = element_blank(),
        legend.direction ="vertical",
        legend.justification = c(1, 0) )


dev.off()

###########################color version##################################
png("CumIncSce2uncorr_R_color.png",width = 1800, height = 1500, res = 600)
ggplot() +
  # draw the original cif series with grey
  geom_line(aes(time, cif, group = sim, color=outcome), data = dataAll,alpha=0.7,size=0.01) +
  ##added color for similation line (picked lighter color than the actual line)
  scale_color_manual(guide = FALSE,values=c('#7FB3D5','#F5B7B1','#A9DFBF','#EDBB99'))+
  # colorise only the filtered data
  ## added color for the actual line),
  ## new_scale_color() is used to use different color scale
  new_scale_color() +
  geom_line(aes(time, cif,color = outcome), data = dataAll0,size=0.2) +
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
  theme(text = element_text(size=5))+
  theme(strip.text.x = element_text( margin = margin( b = 1, t = 0) ) )+
  #Change the x axis name
  scale_x_discrete(name ="Time to event", limits=seq(2,14,2)) +
  labs(y="Cumulative Incidence") + 
  # Remove grid lines
  # theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),)
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold")) +
  theme(panel.grid.minor = element_line(size = 0.2), 
        panel.grid.major = element_line(size = 0.2))
dev.off()

pdf('CumIncSce2uncorr_R_color.pdf',width = 6, height = 5)
ggplot() +
  # draw the original cif series with grey
  geom_line(aes(time, cif, group = sim, color=outcome), data = dataAll,alpha=0.7,size=0.01) +
  ## added color for similation line (picked lighter color than the actual line)
  scale_color_manual(guide = FALSE,values=c('#7FB3D5','#F5B7B1','#A9DFBF','#EDBB99'))+
  # colorise only the filtered data
  ## added color for the actual line),
  ## new_scale_color() is used to use different color scale
  new_scale_color() +
  geom_line(aes(time, cif,color = outcome), data = dataAll0,size=0.2) +
  scale_color_manual(guide = FALSE,values=c('blue','#ff0000','darkgreen','brown'))+
  #One panel / outcome
  facet_wrap(~ outcome) +
  #White background with grid lines
  theme_bw() +
  theme(strip.background =element_rect(color="black",fill="white")) +
  theme(strip.text = element_text(face="bold")) +
  ## removed box around the facet title;decrese the margin between panels and text;
  theme(strip.background = element_blank())+
  theme(panel.spacing = unit(0.2, "lines"))+
  theme(text = element_text(size=7))+
  theme(strip.text.x = element_text( margin = margin( b = 1, t = 0) ) )+
  #Change the x axis name
  scale_x_discrete(name ="Time to event", limits=seq(2,14,2)) +
  labs(y="Cumulative Incidence") + 
  # Remove grid lines
  #theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),)
  theme(axis.title.x = element_text(face="bold")) +
  theme(axis.title.y = element_text(face="bold")) +
  theme(panel.grid.minor = element_line(size = 0.2), 
        panel.grid.major = element_line(size = 0.2))
dev.off()















    


