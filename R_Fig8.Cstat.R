############### Line Plot of C-statistic   #########################

setwd("path")

#install.packages("ggplot2")
library(ggplot2)
#install.packages("stringr")
library("stringr")


#######################################################################################################################################
#Appendix G reformatted
appendixG<-read.csv('appendixG.csv')
str(appendixG)

#convert Mean from factor to character
appendixG$Mean <- as.character(appendixG$Mean)
appendixG$Mean <- trimws(appendixG$Mean) #remove extra blank spaces

#Separate Mean from [se]
appendixGnew<-str_split_fixed(appendixG$Mean, "[ ]", 2)
appendixG$Mean2<-appendixGnew[,1]

#Separate SE from []
str(appendixGnew)
se<-appendixGnew[,2]
se2 <- str_extract_all(se, "(?<=\\[).+?(?=\\])")
#se2 <- as.data.frame(matrix(unlist(se2),byrow=T),stringsAsFactors=FALSE)
se2<-as.data.frame(matrix(c(se2, rep(NA, length(se2) %% 140)), 140)) #140: number of rows in appendixG
appendixG$se<-as.numeric(se2[,1])
str(appendixG)

#covert Mean2 from character to numeric
appendixG$Mean2 <- as.numeric(appendixG$Mean2) 

#round to 3 decimal places
appendixG$Mean2<-round(appendixG[, 5], 3)

#Keep only variables of interest
appendixG<-appendixG[,c(1:3,5)]

#Change the order of the Methods and Outcome
appendixG$Method <- factor(appendixG$Method, levels = c("Individual", "Union", "baBIC", "Intersection", "Full"))
appendixG$Outcome <- factor(appendixG$Outcome, levels = c("ADL", "IADL", "WALK", "DEATH"))

# Modify labels for Data
appendixG$Data <- factor(appendixG$Data, 
                      levels = c("CaseStudy", "Scenario1_corr", "Scenario1_uncorr", "Scenario2_corr", "Scenario2_uncorr", "Scenario3_corr", "Scenario3_uncorr"),
                      labels = c("Case-study", "Scenario 1 Correlated", "Scenario 1 Uncorrelated", "Scenario 2 Correlated", "Scenario 2 Uncorrelated", "Scenario 3 Correlated", "Scenario 3 Uncorrelated"))

  
png('FigCstat_R.png')

ggplot(data = appendixG, aes(x = Method, y = Mean2, group = Outcome, color=Outcome)) + 
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(strip.background =element_rect(color="black",fill="white"))+
  theme(strip.text = element_text(face="bold"))+ 
  labs(x="Method of model selection", y="Harrell's C-statistic") +  
  theme(axis.title.x = element_text(face="bold", vjust=-0.35)) +
  theme (axis.text.x = element_text (angle = 50, vjust = 1, hjust=1, size=8)) +
  theme(axis.title.y = element_text(face="bold", vjust=2)) +
  scale_y_continuous(limits=c(0.58, 0.72), breaks=seq(0.58,0.72,by=0.03)) +
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

pdf('FigCstat_R.pdf')

ggplot(data = appendixG, aes(x = Method, y = Mean2, group = Outcome, color=Outcome)) + 
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(strip.background =element_rect(color="black",fill="white"))+
  theme(strip.text = element_text(face="bold"))+ 
  labs(x="Method of model selection", y="Harrell's C-statistic") +  
  theme(axis.title.x = element_text(face="bold", vjust=-0.35)) +
  theme (axis.text.x = element_text (angle = 50, vjust = 1, hjust=1, size=8)) +
  theme(axis.title.y = element_text(face="bold", vjust=2)) +
  scale_y_continuous(limits=c(0.58, 0.72), breaks=seq(0.58,0.72,by=0.03)) +
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


