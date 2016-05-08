#Produce plots for all  environments
#clear existing data
rm(list=ls())
#requirements
library(plyr)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
require(plyr)
require(rje)
library(directlabels)
library(ggrepel)



#WHICH RESULTS to use?
resultsFile = "resultsRescaledMay4" #also used to save pdf

#Factors
environments <- c("Ackley", "Cross-in-Tray", "Drop-wave", "Eggholder", "Griewank", "Holder table", "Langermann", "Levy", "Levy n.13", "Rastrigin", "Schaffer n.2", "Schaffer n.4", "Schwefel", "Shubert", "Mason & Watts (2012)", "N=20,K=5", "N=20,K=10", "N=20,K=16")
excludedEnvironments <- c("Levy", "Levy n.13", "N=20,K=16")

#Models
models <- c("Imitate the Best", "Hybrid (rs=0,net=local)", "Hybrid (rs=0,net=full)","Hybrid (rs=.2,net=local)", "Hybrid (rs=.2,net=full)", "Local Search", "Random Search") #order saved from simulations



#load data
load(paste0(resultsFile,".Rdata", sep=""))
data <- colMeans(total) #total is loaded from simulation results and averaged over the 100 timesteps
colnames(data)<- orderedModels
row.names(data) <- environments


#initialize dataframe
df <- data.frame(data) 

#name columns
names(df)<- models
#add environments as factor
row.names(df)<- environments
#remove rows belonging to excluded environments
df <- df[!rownames(df) %in% excludedEnvironments, ]
df$Environment<- rownames(df) #make environments 

#Add modality (pre-computed)
df$Log_Modality <- log(c(4489, 64, 4391, 353, 50861, 56, 2461, 121, 88457, 87737, 64, 761, 1090, 1143, 7131))

#Melt data frame
m <- melt(df, id.vars=c("Environment", "Log_Modality"))
m <- rename(m, c("variable"="Model", "value" = "Avg.Perf"))

#Factor model and environment
m$Model <- factor(m$Model)
m$Environment <- factor(m$Environment)

#Plot comparing two different models
modelsubset <- c("Hybrid (rs=.2,net=full)", "Hybrid (rs=.2,net=local)","Hybrid (rs=0,net=full)", "Hybrid (rs=0,net=local)" )
m.subset <- subset(m, Model %in% modelsubset)

p<- ggplot(m, aes(Log_Modality, Avg.Perf)) + geom_point() + stat_smooth(method=lm, fullrange=TRUE) + geom_text_repel(aes(Log_Modality, Avg.Perf, label=Environment))+ labs(x="log10(Modality)", y= "Average Payoff") + theme_bw(base_size = 12) + facet_wrap(~Model, ncol=2)

ggsave("ModCorr.pdf", width=14, height=14, plot=p)

#iterate through each model type, and test correlation between modality ~ payoff

for (model in levels(m$Model)){
	print(model)
	print(cor(m[m$Model==model,]$Avg.Perf,log(c(4489, 64, 4391, 353, 50861, 56, 2461, 121, 88457, 87737, 64, 761, 1090, 1143, 7131))))
}




