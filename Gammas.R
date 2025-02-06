##################################################
###      Simulação da Distribuição Gamma       ###
##################################################

# Parâmetros de referência
alfa0 <- c(1, 100, 0.01, 1) 
beta0 <- c(1, 10, 0.01, 0.1)

# Parâmetros de comparação - mesma media e variância
aux1 <- c(1.01, 1.03, 1.1, 1.31, 2)
aux2 <- aux1+9
aux3 <- aux1*aux1
aux4 <- 1/aux3

alfa1 <- t(aux3%*%t(aux4))
beta1 <- t(aux1%*%t(aux4))

aux5 <- aux2*aux2
alfa2 <- t(aux5%*%t(aux4)) 
beta2 <- t(aux2%*%t(aux4))

aux6 <- 1/aux5
alfa3 <- t(aux3%*%t(aux6)) 
beta3 <- t(aux1%*%t(aux6))  

alfa4 <- t(aux5%*%t(aux6))
beta4 <- t(aux2%*%t(aux6))

a <- rbind(alfa1, alfa2, alfa3, alfa4)
b <- rbind(beta1, beta2, beta3, beta4)

par(mfrow=c(2,3))
#Cálculo iterativo
l = 1
alfa <- alfa0[l]
beta <- beta0[l]
curve(dgamma(x,shape=alfa,rate=beta),from = 0, to = 3, col = 1, ylim=c(0,2),
        lwd = 5, xlab = "x", ylab = "Densidade", frame.plot = FALSE)
curve(dgamma(x,shape=a[l,1],rate=b[l,1]),add = T, from = 0, to = 3, lty = 2,
          col = "#A3C5F1", lwd = 3)
curve(dgamma(x,shape=a[l,2],rate=b[l,2]),add = T, from = 0, to = 3, lty = 3,
      col = "#83A2CD", lwd = 3)
curve(dgamma(x,shape=a[l,3],rate=b[l,3]),add = T, from = 0, to = 3, lty = 4,
      col = "#6582AA", lwd = 3)
curve(dgamma(x,shape=a[l,4],rate=b[l,4]),add = T, from = 0, to = 3, lty = 5,
      col = "#4B6387", lwd = 3)
curve(dgamma(x,shape=a[l,5],rate=b[l,5]),add = T, from = 0, to = 3, lty = 6,
      col = "#334663", lwd = 3)
legend(1.5,2,lty = c(1,2,3,4,5,6), col=c("#000000","#A3C5F1","#83A2CD","#6582AA",
                                         "#4B6387","#334663"),box.col = 0, lwd=3,
       legend = c(expression(mu~"= 1;      "~sigma~" = 1"), 
                  expression(mu~"= 1,01; "~sigma~" = 1,01"),
                  expression(mu~"= 1,03; "~sigma~" = 1,01"),
                  expression(mu~"= 1,1;   "~sigma~" = 1,01"),
                  expression(mu~"= 1,31; "~sigma~" = 1,01"),
                  expression(mu~"= 2;      "~sigma~" = 1,01")))

l=2
curve(dgamma(x,shape=alfa,rate=beta),from = 0, to = 3, col = 1, ylim=c(0,2),
      lwd = 5, xlab = "x", ylab = "Densidade", frame.plot = FALSE)
curve(dgamma(x,shape=a[l,1],rate=b[l,1]),add = T, from = 0, to = 3, lty = 2,
      col = "#A3C5F1", lwd = 3)
curve(dgamma(x,shape=a[l,2],rate=b[l,2]),add = T, from = 0, to = 3, lty = 3,
      col = "#83A2CD", lwd = 3)
curve(dgamma(x,shape=a[l,3],rate=b[l,3]),add = T, from = 0, to = 3, lty = 4,
      col = "#6582AA", lwd = 3)
curve(dgamma(x,shape=a[l,4],rate=b[l,4]),add = T, from = 0, to = 3, lty = 5,
      col = "#4B6387", lwd = 3)
curve(dgamma(x,shape=a[l,5],rate=b[l,5]),add = T, from = 0, to = 3, lty = 6,
      col = "#334663", lwd = 3)
legend(1.5,2,lty = c(1,2,3,4,5,6), col=c("#000000","#A3C5F1","#83A2CD","#6582AA",
                                         "#4B6387","#334663"),box.col = 0, lwd=3,
       legend = c(expression(mu~"= 1;      "~sigma~" = 1"), 
                  expression(mu~"= 1,01; "~sigma~" = 1,03"),
                  expression(mu~"= 1,03; "~sigma~" = 1,03"),
                  expression(mu~"= 1,1;   "~sigma~" = 1,03"),
                  expression(mu~"= 1,31; "~sigma~" = 1,03"),
                  expression(mu~"= 2;      "~sigma~" = 1,03")))

l=3
curve(dgamma(x,shape=alfa,rate=beta),from = 0, to = 3, col = 1, ylim=c(0,2),
      lwd = 5, xlab = "x", ylab = "Densidade", frame.plot = FALSE)
curve(dgamma(x,shape=a[l,1],rate=b[l,1]),add = T, from = 0, to = 3, lty = 2,
      col = "#A3C5F1", lwd = 3)
curve(dgamma(x,shape=a[l,2],rate=b[l,2]),add = T, from = 0, to = 3, lty = 3,
      col = "#83A2CD", lwd = 3)
curve(dgamma(x,shape=a[l,3],rate=b[l,3]),add = T, from = 0, to = 3, lty = 4,
      col = "#6582AA", lwd = 3)
curve(dgamma(x,shape=a[l,4],rate=b[l,4]),add = T, from = 0, to = 3, lty = 5,
      col = "#4B6387", lwd = 3)
curve(dgamma(x,shape=a[l,5],rate=b[l,5]),add = T, from = 0, to = 3, lty = 6,
      col = "#334663", lwd = 3)
legend(1.5,2,lty = c(1,2,3,4,5,6), col=c("#000000","#A3C5F1","#83A2CD","#6582AA",
                                         "#4B6387","#334663"),box.col = 0, lwd=3,
       legend = c(expression(mu~"= 1;      "~sigma~" = 1"), 
                  expression(mu~"= 1,01; "~sigma~" = 1,1"),
                  expression(mu~"= 1,03; "~sigma~" = 1,1"),
                  expression(mu~"= 1,1;   "~sigma~" = 1,1"),
                  expression(mu~"= 1,31; "~sigma~" = 1,1"),
                  expression(mu~"= 2;      "~sigma~" = 1,1")))

l=4
curve(dgamma(x,shape=alfa,rate=beta),from = 0, to = 3, col = 1, ylim=c(0,2),
      lwd = 5, xlab = "x", ylab = "Densidade", frame.plot = FALSE)
curve(dgamma(x,shape=a[l,1],rate=b[l,1]),add = T, from = 0, to = 3, lty = 2,
      col = "#A3C5F1", lwd = 3)
curve(dgamma(x,shape=a[l,2],rate=b[l,2]),add = T, from = 0, to = 3, lty = 3,
      col = "#83A2CD", lwd = 3)
curve(dgamma(x,shape=a[l,3],rate=b[l,3]),add = T, from = 0, to = 3, lty = 4,
      col = "#6582AA", lwd = 3)
curve(dgamma(x,shape=a[l,4],rate=b[l,4]),add = T, from = 0, to = 3, lty = 5,
      col = "#4B6387", lwd = 3)
curve(dgamma(x,shape=a[l,5],rate=b[l,5]),add = T, from = 0, to = 3, lty = 6,
      col = "#334663", lwd = 3)
legend(1.5,2,lty = c(1,2,3,4,5,6), col=c("#000000","#A3C5F1","#83A2CD","#6582AA",
                                         "#4B6387","#334663"),box.col = 0, lwd=3,
       legend = c(expression(mu~"= 1;      "~sigma~" = 1"), 
                  expression(mu~"= 1,01; "~sigma~" = 1,31"),
                  expression(mu~"= 1,03; "~sigma~" = 1,31"),
                  expression(mu~"= 1,1;   "~sigma~" = 1,31"),
                  expression(mu~"= 1,31; "~sigma~" = 1,31"),
                  expression(mu~"= 2;      "~sigma~" = 1,31")))

l=5
curve(dgamma(x,shape=alfa,rate=beta),from = 0, to = 3, col = 1, ylim=c(0,2),
      lwd = 5, xlab = "x", ylab = "Densidade", frame.plot = FALSE)
curve(dgamma(x,shape=a[l,1],rate=b[l,1]),add = T, from = 0, to = 3, lty = 2,
      col = "#A3C5F1", lwd = 3)
curve(dgamma(x,shape=a[l,2],rate=b[l,2]),add = T, from = 0, to = 3, lty = 3,
      col = "#83A2CD", lwd = 3)
curve(dgamma(x,shape=a[l,3],rate=b[l,3]),add = T, from = 0, to = 3, lty = 4,
      col = "#6582AA", lwd = 3)
curve(dgamma(x,shape=a[l,4],rate=b[l,4]),add = T, from = 0, to = 3, lty = 5,
      col = "#4B6387", lwd = 3)
curve(dgamma(x,shape=a[l,5],rate=b[l,5]),add = T, from = 0, to = 3, lty = 6,
      col = "#334663", lwd = 3)
legend(1.5,2,lty = c(1,2,3,4,5,6), col=c("#000000","#A3C5F1","#83A2CD","#6582AA",
                                         "#4B6387","#334663"),box.col = 0, lwd=3,
       legend = c(expression(mu~"= 1;      "~sigma~" = 1"), 
                  expression(mu~"= 1,01; "~sigma~" = 2"),
                  expression(mu~"= 1,03; "~sigma~" = 2"),
                  expression(mu~"= 1,1;   "~sigma~" = 2"),
                  expression(mu~"= 1,31; "~sigma~" = 2"),
                  expression(mu~"= 2;      "~sigma~" = 2")))
