folder<- "resultsOriginalMay4"

setwd(folder)
files <- list.files()
total <- array(0,dim=c(100,18,7))
for(f in files){
	load(f)
	total<- total + output
}

total <- total/length(files)

setwd("..")
filename <- paste0(folder, ".Rdata", sep="")
save(total,file=filename)

