setwd("~/Desktop/ResultsApr22")

files <- list.files()

total <- array(0,dim=c(100,16,7))

for(f in files){

load(f)

total<- total + output

}

total <- total/length(files)

# save(total,file="~/Desktop/Results.Rdata")