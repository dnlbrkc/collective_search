#Produce plots for all  environments

#requirements
library(plyr)
library(ggplot2)
theme_set(theme_bw(base_size=12))# use the black and white theme
library(reshape2)
library(grid)
library(gridExtra)
require(plyr)
require(rje)

#clear existing data
rm(list=ls())


#Factors
environments <- c("ackley", "crossit", "drop", "egg", "griewank", "holder", "langer", "levy", "levy13", "rastr", "schaffer2", "schaffer4", "schwef", "shubert", "masonAndWatts", "N=20,K=5")

#load data
load("Results.Rdata")

#initialize empty dataframe
df <- data.frame(time = integer(), model = character(), environment=character(), avg.Payoff=double())

#Models
modelOrder = c("ImitateLocal", "ImitateFull", "LocalSearch", "Random", "Hybrid (rs=0,net=full)", "Hybrid (rs=.2,net=full)","Hybrid (rs=0,net=local)", "Hybrid (rs=.2,net=local)")

#parse the output data
fullDataList <- list()
fullDataList[[1]] <- total[[2]][,,1]
fullDataList[[2]] <- total[[1]][,,1]
fullDataList[[3]] <- total[[1]][,,4]
fullDataList[[4]] <- total[[1]][,,5]
fullDataList[[5]] <- total[[1]][,,2]
fullDataList[[6]] <- total[[1]][,,3]
fullDataList[[7]] <- total[[2]][,,2]
fullDataList[[8]] <- total[[2]][,,3]

#average over all environments, if we want to plot that
#test<- sapply(1:8,function(x) rowMeans(fullDataList[[x]]))

#1. loop through different model levels
for (model_n in 1:length(modelOrder)){
	temp_df <- data.frame(fullDataList[[model_n]])
	#format data.frame to be joined to the global df
	#rename column names to environments from listr
	names(temp_df) <- environments
	#add time column
	temp_df$time <- c(1:200)
	#add model column
	temp_df$model <- c(rep(modelOrder[model_n], 200))
	#melt temp_df
	melted_temp_df <- melt(temp_df, id.vars=c("time", "model"))
	renamed_temp_df <- rename(melted_temp_df, c("variable"="environment", "value"="avg.Payoff"))
	#rbind with df
	df <- rbind(df,renamed_temp_df)

}


df$model <- factor(df$model, modelOrder)
p <- ggplot(df[df$time %in% c(1:100), ], aes(x = time, y = avg.Payoff, col = model, linetype = model)) + geom_line(lwd=0.6)  + ylim(0,1) + theme(legend.position="bottom") + guides(col = guide_legend(ncol = 3)) + facet_wrap(~ environment, ncol=3) + scale_colour_manual(values=cubeHelix(length(modelOrder)+1, start = 2.8, r = -1.5 , hue=1.8, gamma = 1.6)) + labs(x = "Time Steps", y = "Average Payoff")


ggsave("allEnvironments.pdf", plot = p, height =10, width = 7, units = "in")