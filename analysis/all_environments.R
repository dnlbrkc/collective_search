#Produce plots for all  environments
#clear existing data
rm(list=ls())
#requirements
library(plyr)
library(ggplot2)
theme_set(theme_bw(base_size=12))# use the b&w theme
library(reshape2)
library(grid)
library(gridExtra)
require(plyr)
require(rje)


#WHICH RESULTS to use?
resultsFile = "radius_3" #also used to save pdf

#Factors
environments <- c("Ackley", "Cross-in-Tray", "Drop-wave", "Eggholder", "Griewank", "Holder table", "Langermann", "Levy", "Levy n.13", "Rastrigin", "Schaffer n.2", "Schaffer n.4", "Schwefel", "Shubert", "Mason & Watts (2012)", "N=20,K=5")

excludedEnvironments <- c("Levy", "Levy n.13")

#load data
load(paste0(resultsFile,".Rdata", sep=""))

#initialize empty dataframe
df <- data.frame(time = integer(), model = character(), environment=character(), avg.Payoff=double())

#Models
models <- c("Imitate the Best", "Hybrid (rs=0,net=local)", "Hybrid (rs=0,net=full)","Hybrid (rs=.2,net=local)", "Hybrid (rs=.2,net=full)", "Local Search", "Random Search") #order saved from simulations
modelOrder <- c(6,7,1,2,3,4,5) #order used in plots

#1. loop through different model levels
for (model_n in modelOrder){
	temp_df <- data.frame(total[,,model_n])
	#format data.frame to be joined to the global df
	#rename column names to environments from listr
	names(temp_df) <- environments
	#add time column
	temp_df$time <- c(1:100)
	#add model column
	temp_df$model <- c(rep(models[model_n], 100))
	#melt temp_df
	melted_temp_df <- melt(temp_df, id.vars=c("time", "model"))
	renamed_temp_df <- rename(melted_temp_df, c("variable"="environment", "value"="avg.Payoff"))
	#rbind with df
	df <- rbind(df,renamed_temp_df)

}


df$model <- factor(df$model, models[modelOrder])
#exclude environments
df <- df[!(df$environment %in% excludedEnvironments),]

p <- ggplot(df[df$time %in% seq(10,100,10),], aes(x = time, y = avg.Payoff, col = model)) + geom_line(lwd=0.3) + geom_point(aes(shape=model)) + scale_shape_manual(values = c(0,1,2,3,13,4,9))+ ylim(0,1)  +guides(colour = guide_legend("", ncol=3), shape = guide_legend("",ncol=3))+ facet_wrap(~ environment, ncol=3) + scale_colour_manual(values=cubeHelix(length(modelOrder)+1, start = .5, r = -1.5 , hue=1.8, gamma = 1.8))+ labs(x = "Time Steps", y = "Average Payoff") +theme(legend.position="bottom",strip.background=element_blank(),legend.key=element_rect(color=NA))

outputFile <- paste0(resultsFile,".pdf", sep="")
ggsave(outputFile, plot = p, height =10, width = 7, units = "in")