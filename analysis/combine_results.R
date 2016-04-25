setwd("~/Desktop/res")

files <- list.files()

total <- array(0,dim=c(100,16,7))

for(f in files){

load(f)

total<- total + output

}

total <- total/length(files)

# save(total,file="~/Desktop/Results.Rdata")

par(mfrow=c(5,3),mar=c(1,1,1,1))
for(i in 1:15){
plot(total[,i,2],type='l',col="blue",ylim=c(0.5,1))
lines(total[,i,3],type='l')

lines(total[,i,4],type='l',col="blue")
lines(total[,i,5],type='l')
  
}

