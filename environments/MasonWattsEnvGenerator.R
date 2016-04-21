source("functions.R")

MasonWattsEnv <- list()

for(i in 1:100){
MasonWattsEnv[[i]] <-  MasonWatts(1001)
  
}

save(MasonWattsEnv,file="MasonWattsEnv.Rdata")