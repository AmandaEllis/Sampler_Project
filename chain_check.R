setwd('C:/Users/Amanda/Dropbox/A_Ellis/Code/Sampler_Photographs/Sampler_Project/Output/Sim_100_50_1_trueX_t5')

CI<-.95
prob=1-CI
lb=prob/2
ub=1-prob/2
N<-100

total<-0
over<-0
under<-0

for(i in 1:100){
  load(paste0("output_",i,".Rdata"))
  
  CI.N<-quantile(output$N.gibbs,c(lb,ub),names=FALSE)
  
  if(N<=max(CI.N) && N>=min(CI.N)){total=total+1}
  
  if(N>max(CI.N)){
    under=under+1
    print(i)}
  
  if(N<min(CI.N)){
    over=over+1
    print(i)}
}

(posterior.mean.N<-mean(as.vector(output$N.gibbs),na.rm=TRUE))
(posterior.mean.p<-mean(as.vector(output$p.gibbs),na.rm=TRUE))
plot(as.vector(output$p.gibbs[,2]),type='l')
plot(as.vector(output$N.gibbs),type='l')
CI<-.95
prob=1-CI
lb=prob/2
ub=1-prob/2
(CI.p<-quantile(output$N.gibbs,c(lb,ub),names=FALSE))
