#Used by generate_NK.R to generate the NK fitness landscapes
library(gtools)
#Takes in the values N, K, LS (the list of all binary solution ids), and the dependencies between each solution
generate_landscape<-function(N,K,LS,fitness, depends){
	fitness.score<-vector()
	if(K==0){
	for (ag in 1:nrow(LS)){
		rows<-as.numeric(LS[ag,])+1	
		values<-sapply(1:N, function(y) fitness[rows[y],y] )
		fitness.score[ag]<-mean(values)
		}
	} else {
		indx1<-do.call(`paste0`,as.data.frame(fitness[,c(1:(K+1))]))
		indx2<-sapply(1:N, function(y) do.call(`paste0`,as.data.frame(LS[,depends[,y]])))
		fitness.score<-sapply(1:nrow(LS), function(o) mean(diag(sapply(indx2[o,], function(x) fitness[which(indx1 %in% x),(K+2):ncol(fitness)]))))
	 }
	landscape<-cbind(LS[,1:N],fitness.score)
	return(landscape)
}