############### Line Point Plot Mean Number of predictors  #########################

setwd("path")

library(ggplot2)

#Appendix E
appendixE<-read.csv('AppendixE.csv')

#Change the order of the Methods
appendixE$Method <- factor(appendixE$Method, levels = c("ADL", "IADL", "WALK", "DEATH", "Union", "baBIC", "Intersect", "Full"))
#appendixE$Method2 <- factor(appendixE$Method2, levels = c("Individual ADL", "Individual IADL", "Individual WALK", "Individual DEATH", "Union", "baBIC", "Intersect", "Full"))

# Modify labels for Data
appendixE$Data <- factor(appendixE$Data, 
                  levels = c("CaseStudy", "Scenario1_corig", "Scenario1_c25", "Scenario2_corig", "Scenario2_c25", "Scenario3_corig", "Scenario3_c25"),
                  labels = c("Case-study", "Scenario 1 Case-study censoring", "Scenario 1 25% Censoring", "Scenario 2 Case-study censoring", "Scenario 2 25% Censoring", "Scenario 3 Case-study censoring", "Scenario 3 25% Censoring"))

#appendix E
png('Fig7.NumPred.png')

ggplot(appendixE, aes(x = Method, y = Mean,label=Mean, color=Method)) +
  geom_point(size = .8) +
  geom_errorbar(aes(ymax = upper, ymin = lower, width = 0.1)) +
  geom_text(aes(label=Mean),hjust=-0.3, vjust=0.5, size=2.8) +  #add labels to the points 
  facet_wrap(~ Data, ncol=3,  strip.position = "top") +
  theme_bw() +
  theme(strip.background =element_rect(color="black",fill="white"))+
  theme(strip.text = element_text(face="bold"))+ 
  ## removed box around the facet title;decrease the margin between panels and text;
  theme(strip.background = element_blank())+
  theme(panel.spacing = unit(0.2, "lines"))+
  theme(text = element_text(size=10.6))+
  theme(strip.text.x = element_text( margin = margin( b = 1, t = 0) ) )+
  #theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  labs(y="Mean of number of predictors") +  
  theme (axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.title.y = element_text(face="bold", vjust=2)) +
  scale_y_continuous(limits=c(0, 40), breaks=seq(2,40,by=10)) +
  scale_color_manual(name = "Method of model selection:",  
                     labels = c("Individual Outcome, ADL", 
                                "Individual Outcome, IADL", 
                                "Individual Outcome, WALK", 
                                "Individual Outcome, DEATH", 
                                "Union",
                                "baBIC",
                                "Intersection", 
                                "Full"),  
                     values = c("ADL"="blue", 
                                "IADL"="#ff0000", 
                                "WALK"="darkgreen", 
                                "DEATH"="brown", 
                                "Union"="orange",
                                "baBIC"="#ff00ff",
                                "Intersect"="#00AFBB",
                                "Full"="black")) +
  theme(legend.position = c(0.6,0.1), 
        legend.title = element_text(size = rel(0.8), face = "bold", vjust = 1), 
        legend.key.size = unit(1.2, "lines"),
        legend.text=element_text(size = rel(0.78)) ) +
  guides(col=guide_legend(ncol=2))

dev.off()


pdf('Fig7.NumPred.pdf')

ggplot(appendixE, aes(x = Method, y = Mean,label=Mean, color=Method)) +
  geom_point(size = .8) +
  geom_errorbar(aes(ymax = upper, ymin = lower, width = 0.1)) +
  geom_text(aes(label=Mean),hjust=-0.3, vjust=0.5, size=2.8) +  #add labels to the points 
  facet_wrap(~ Data, ncol=3,  strip.position = "top") +
  theme_bw() +
  theme(strip.background =element_rect(color="black",fill="white"))+
  theme(strip.text = element_text(face="bold"))+ 
  ## removed box around the facet title;decrease the margin between panels and text;
  theme(strip.background = element_blank())+
  theme(panel.spacing = unit(0.2, "lines"))+
  theme(text = element_text(size=10.6))+
  theme(strip.text.x = element_text( margin = margin( b = 1, t = 0) ) ) +
  #theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  labs(y="Mean of number of predictors") +  
  theme (axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.title.y = element_text(face="bold", vjust=2)) +
  scale_y_continuous(limits=c(0, 40), breaks=seq(2,40,by=10)) +
  scale_color_manual(name = "Method of model selection:",  
                     labels = c("Individual Outcome, ADL", 
                                "Individual Outcome, IADL", 
                                "Individual Outcome, WALK", 
                                "Individual Outcome, DEATH", 
                                "Union",
                                "baBIC",
                                "Intersection", 
                                "Full"),  
                     values = c("ADL"="blue", 
                                "IADL"="#ff0000", 
                                "WALK"="darkgreen", 
                                "DEATH"="brown", 
                                "Union"="orange",
                                "baBIC"="#ff00ff",
                                "Intersect"="#00AFBB",
                                "Full"="black")) +
  theme(legend.position = c(0.6,0.1), 
        legend.title = element_text(size = rel(0.8), face = "bold", vjust = 1), 
        legend.key.size = unit(1.2, "lines"),
        legend.text=element_text(size = rel(0.78)) ) +
  guides(col=guide_legend(ncol=2))


dev.off()
