#hist(N.gibbs, freq=FALSE, xlab="N", col="gray", border="white", main="")
(posterior.mean.N<-mean(as.vector(N.gibbs[burn.in:iterations+burn.in]),na.rm=TRUE))
plot(as.vector(N.gibbs),type='l')
CI<-.95
prob=1-CI
lb=prob/2
ub=1-prob/2
(CI.N<-quantile(N.gibbs,c(lb,ub),names=FALSE))

(posterior.mean.p<-mean(as.vector(p.gibbs),na.rm=TRUE))
plot(as.vector(p.gibbs),type='l')
CI<-.95
prob=1-CI
lb=prob/2
ub=1-prob/2
(CI.p<-quantile(p.gibbs,c(lb,ub),names=FALSE))
