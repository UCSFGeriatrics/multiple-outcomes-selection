############### Line Point Plot Mean Number of predictors [95% CI]   #########################

setwd("path")

library(ggplot2)


#######################################################################################################################################
#Appendix F results reformatted with 95% CI
appendixF<-read.csv('appendixF.csv')

#Change the order of the Methods
appendixF$Method <- factor(appendixF$Method, levels = c("ADL", "IADL", "WALK", "DEATH", "Union", "baBIC", "Intersect", "Full"))
#appendixF$Method2 <- factor(appendixF$Method2, levels = c("Individual ADL", "Individual IADL", "Individual WALK", "Individual DEATH", "Union", "baBIC", "Intersect", "Full"))

# Modify labels for Data
appendixF$Data <- factor(appendixF$Data, 
                  levels = c("CaseStudy", "Scenario1_corr", "Scenario1_uncorr", "Scenario2_corr", "Scenario2_uncorr", "Scenario3_corr", "Scenario3_uncorr"),
                  labels = c("Case-study", "Scenario 1 Correlated", "Scenario 1 Uncorrelated", "Scenario 2 Correlated", "Scenario 2 Uncorrelated", "Scenario 3 Correlated", "Scenario 3 Uncorrelated"))

#appendixF
png('FigNumPred_R.png')

ggplot(appendixF, aes(x = Method, y = Mean,label=Mean, color=Method)) +
  geom_point(size = .8) +
  geom_errorbar(aes(ymax = uci, ymin = lci, width = 0.1)) +
  geom_text(aes(label=Mean),hjust=-0.2, vjust=0.5, size=2.8) +  #add labels to the points 
  facet_wrap(~ Data, ncol=3,  strip.position = "top") +
  theme_bw() +
  theme(strip.background =element_rect(color="black",fill="white"))+
  theme(strip.text = element_text(face="bold"))+ 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
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


pdf('FigNumPred_R.pdf')

ggplot(appendixF, aes(x = Method, y = Mean,label=Mean, color=Method)) +
  geom_point(size = .8) +
  geom_errorbar(aes(ymax = uci, ymin = lci, width = 0.1)) +
  geom_text(aes(label=Mean),hjust=-0.2, vjust=0.5, size=2.8) +  #add labels to the points 
  facet_wrap(~ Data, ncol=3,  strip.position = "top") +
  theme_bw() +
  theme(strip.background =element_rect(color="black",fill="white"))+
  theme(strip.text = element_text(face="bold"))+ 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
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




