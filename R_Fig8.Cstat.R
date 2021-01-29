############### Line Plot of C-statistic   #########################

setwd("path")

#install.packages("ggplot2")
library(ggplot2)
#install.packages("stringr")
library("stringr")

#Appendix F
AppendixF<-read.csv('AppendixF.csv')
str(AppendixF)

#round to 3 decimal places
AppendixF$Mean<-round(AppendixF[, 4], 3)

#Change the order of the Methods and Outcome
AppendixF$Method <- factor(AppendixF$Method, levels = c("Individual", "Union", "baBIC", "Intersection", "Full"))
AppendixF$Outcome <- factor(AppendixF$Outcome, levels = c("ADL", "IADL", "WALK", "DEATH"))

# Modify labels for Data
AppendixF$Data <- factor(AppendixF$Data, 
                      levels = c("CaseStudy", "Scenario1_corig", "Scenario1_c25", "Scenario2_corig", "Scenario2_c25", "Scenario3_corig", "Scenario3_c25"),
                      labels = c("Case-study", "Scenario 1 Case-study censoring", "Scenario 1 25% censoring", "Scenario 2 Case-study censoring", "Scenario 2 25% censoring", "Scenario 3 Case-study censoring", "Scenario 3 25% censoring"))

  
png('Fig8.Cstat.png')

ggplot(data = AppendixF, aes(x = Method, y = Mean, group = Outcome, color=Outcome)) + 
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(strip.background =element_rect(color="black",fill="white"))+
  theme(strip.text = element_text(face="bold"))+ 
  ## removed box around the facet title;decrease the margin between panels and text;
  theme(strip.background = element_blank())+
  theme(panel.spacing = unit(0.2, "lines"))+
  theme(text = element_text(size=10.6))+
  theme(strip.text.x = element_text( margin = margin( b = 1, t = 0) ) )+
  labs(x="Method of model selection", y="Harrell's C-statistic") +  
  theme(axis.title.x = element_text(face="bold", vjust=-0.35)) +
  theme (axis.text.x = element_text (angle = 50, vjust = 1, hjust=1, size=8)) +
  theme(axis.title.y = element_text(face="bold", vjust=2)) +
  scale_y_continuous(limits=c(0.56, 0.72), breaks=seq(0.56,0.72,by=0.03)) +
  facet_wrap(~ Data, ncol=3,  strip.position = "top") +
  scale_color_manual(name = "Outcome:",  
                     labels = c("ADL", 
                                "IADL", 
                                "WALK", 
                                "DEATH"),  
                     values = c("ADL"="blue", 
                                "IADL"="#ff0000", 
                                "WALK"="darkgreen", 
                                "DEATH"="brown") ) +
  theme(legend.position = c(0.9,0),
        legend.title = element_text(size = rel(0.8), face = "bold", vjust = 1), 
        legend.key.size = unit(1.2, "lines"),
        legend.text=element_text(size = rel(0.78)) ) 


dev.off()

pdf('Fig8.Cstat.pdf')

ggplot(data = AppendixF, aes(x = Method, y = Mean, group = Outcome, color=Outcome)) + 
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(strip.background =element_rect(color="black",fill="white"))+
  theme(strip.text = element_text(face="bold"))+ 
  ## removed box around the facet title;decrease the margin between panels and text;
  theme(strip.background = element_blank())+
  theme(panel.spacing = unit(0.2, "lines"))+
  theme(text = element_text(size=10.6))+
  theme(strip.text.x = element_text( margin = margin( b = 1, t = 0) ) )+
  labs(x="Method of model selection", y="Harrell's C-statistic") +  
  theme(axis.title.x = element_text(face="bold", vjust=-0.35)) +
  theme (axis.text.x = element_text (angle = 50, vjust = 1, hjust=1, size=8)) +
  theme(axis.title.y = element_text(face="bold", vjust=2)) +
  scale_y_continuous(limits=c(0.56, 0.72), breaks=seq(0.56,0.72,by=0.03)) +
  facet_wrap(~ Data, ncol=3,  strip.position = "top") +
  scale_color_manual(name = "Outcome:",  
                     labels = c("ADL", 
                                "IADL", 
                                "WALK", 
                                "DEATH"),  
                     values = c("ADL"="blue", 
                                "IADL"="#ff0000", 
                                "WALK"="darkgreen", 
                                "DEATH"="brown") ) +
  theme(legend.position = c(0.9,0),
        legend.title = element_text(size = rel(0.8), face = "bold", vjust = 1), 
        legend.key.size = unit(1.2, "lines"),
        legend.text=element_text(size = rel(0.78)) ) 

dev.off()


