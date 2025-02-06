



M0  <- 1
MaX <-1.1 
NN  <- 1000


REP <- 100
DD <- PP <- numeric(0)
jjj=0
SEQ=seq(M0,MaX,0.0005)
for (iii in SEQ) {
  jjj=jjj+1
  m1 <- M0
  m2 <- iii
  sg  <- 2
  ns=c(NN)

D.valores=matrix(0,ncol=length(ns),nrow=REP)
p.valores=matrix(0,ncol=length(ns),nrow=REP)

j=0
for (n in ns) {
j=j+1

for (K in 1:REP){
x1 <- rnorm(n,m1,sg)
x2 <- rnorm(n,m2,sg)

x.barra1 <- mean(x1)
x.barra2 <- mean(x2)
s1 <- sd(x1)
s2 <- sd(x2)


D.valor <- pnorm((x.barra1-x.barra2)/sqrt(s1+s2))
p.valor <- t.test(x1,x2,alternative = 'less')$p.value


D.valores[K,j]=D.valor
p.valores[K,j]=p.valor

}

# par(mfrow=c(1,1))
# plot(seq(-4,6,length.out = 100),
#      dnorm(seq(-4,6,length.out = 100),m1,sg),
#      lwd=4,main='',ylab='',xlab='',type='l')
# lines(seq(-4,6,length.out = 100),dnorm(seq(-4,6,length.out = 100),m2,sg),
#       lwd=2,col='orange')

#cat("running n=",n," ...\n")
}


DD[jjj]=median(D.valores)
PP[jjj]=median(p.valores)

cat(jjj/length(SEQ),"\n")
}



plot(PP,DD,xlab="p-valor",ylab="D-valor",pch=".",cex=3, xlim = c(0,0.6))
mod <- loess(DD~PP,span = 0.5)


# p.valor > 0.100 Não existe evidência contra H0
# p.valor < 0.100 Fraca evidência contra H0
# p.valor < 0.050 Evidência significativa ... 
# p.valor < 0.010 Evidência altamente significativa ... 
# p.valor < 0.001 Evidência muito altamente significativa ...

abline(v=0.10, h=predict(mod,data.frame(PP=0.10)),lty=2, col="green",lwd=2)
abline(v=0.05, h=predict(mod,data.frame(PP=0.05)),lty=2, col="orange",lwd=2)
abline(v=0.01, h=predict(mod,data.frame(PP=0.01)),lty=2, col="red",lwd=2)

