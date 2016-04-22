#Produce plots for one iteration
#clear existing data
rm(list=ls())

#requirements
library(plyr)
library(ggplot2)
theme_set(theme_bw(base_size=12))# use the black and white theme
library(reshape2)
library(grid)
library(gridExtra)
require(plyr)
require(rje)

setwd("resultsApr21")
filename <- "230.Rdata"
load(filename)
setwd("..")

#Factors
environments <- c("ackley", "crossit", "drop", "egg", "griewank", "holder", "langer", "levy", "levy13", "rastr", "schaffer2", "schaffer4", "schwef", "shubert", "masonAndWatts", "N=20,K=5")


#initialize empty dataframe
df <- data.frame(time = integer(), model = character(), environment=character(), avg.Payoff=double())

#Models
models <- c("Imitate the Best", "Hybrid (rs=0,net=local)", "Hybrid (rs=0,net=full)","Hybrid (rs=.2,net=local)", "Hybrid (rs=.2,net=full)", "Local Search", "Random Search")
modelOrder <- c(6,7,1,2,3,4,5)


#parse the output data


#average over all environments, if we want to plot that
#test<- sapply(1:8,function(x) rowMeans(output[[x]]))

#1. loop through different model levels
for (model_n in modelOrder){
	temp_df <- data.frame(output[,,model_n])
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
p <- ggplot(df[df$time %in% c(1:100), ], aes(x = time, y = avg.Payoff, col = model, linetype = model)) + geom_line(lwd=0.6)  + ylim(0,1) + theme(legend.position="bottom") + guides(col = guide_legend(ncol = 3)) + facet_wrap(~ environment, ncol=3) + scale_colour_manual(values=cubeHelix(length(modelOrder)+1, start = 2.8, r = -1.5 , hue=1.8, gamma = 1.6)) + labs(x = "Time Steps", y = "Average Payoff")

outputFile <- paste(filename, ".pdf", sep = "")
ggsave(outputFile, plot = p, height =10, width = 7, units = "in")