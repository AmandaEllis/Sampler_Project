setwd('C:/Users/Amand/Dropbox/A_Ellis/Code/Sampler_Photographs/Sampler_Project/Output5')

CI<-.95
prob=1-CI
lb=prob/2
ub=1-prob/2
N<-200

total<-0
over<-0
under<-0

for(i in 1:100){
  load(paste0("output_",i,".Rdata"))
  
  CI.N<-quantile(output$N.gibbs,c(lb,ub),names=FALSE)
  
  if(N<=max(CI.N) && N>=min(CI.N)){total=total+1}
  
  if(N>max(CI.N)){under=under+1}
  
  if(N<min(CI.N)){over=over+1}
}

(posterior.mean.p<-mean(as.vector(p.gibbs),na.rm=TRUE))
plot(as.vector(output$N.gibbs),type='l')
CI<-.95
prob=1-CI
lb=prob/2
ub=1-prob/2
(CI.p<-quantile(p.gibbs,c(lb,ub),names=FALSE))
