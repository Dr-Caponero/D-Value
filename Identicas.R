tam = 1000050
tamostra = seq(50, tam, 5000)
k = 1
PV = NULL
for (i in tamostra){
  teste = t.test(rnorm(i,1,1), rnorm(i,1,1))
  PV[k] = teste$p.value
  k= k+1
}
# Criando o gráfico de P-Valor
plot(tamostra, PV ,col="#1F78B4",pch=16 , cex=1.3, ylim = c(0,1), 
     xlab = "Tamanho da amostra", ylab = "P-Valor", main = "",
     xlim = c(0,tam)) 
par(new = TRUE)
plot(tamostra, y=rep(.05, length(tamostra)), col = 2, type = "l", ylim = c(0,1), xlab="", 
     ylab="", lwd = 2.5, xlim = c(0,tam))


k = 1
PV = NULL
for (i in tamostra){
  teste = t.test(rnorm(i,1,1), rnorm(i,1.01,1))
  PV[k] = teste$p.value
  k= k+1
}
# Criando o gráfico de P-Valor
plot(tamostra, PV ,col="#1F78B4",pch=16 , cex=1.3, ylim = c(0,1), 
     xlab = "Tamanho da amostra", ylab = "P-Valor", main = "",
     xlim = c(0,tam)) 
par(new = TRUE)
plot(tamostra, y=rep(.05, length(tamostra)), col = 2, type = "l", ylim = c(0,1), xlab="", 
     ylab="", lwd = 2.5, xlim = c(0,tam))

k = 1
PV = NULL
for (i in tamostra){
  teste = t.test(rnorm(i,1,1), rnorm(i,1,10))
  PV[k] = teste$p.value
  k= k+1
}
# Criando o gráfico de P-Valor
plot(tamostra, PV ,col="#1F78B4",pch=16 , cex=1.3, ylim = c(0,1), 
     xlab = "Tamanho da amostra", ylab = "P-Valor", main = "",
     xlim = c(0,tam)) 
par(new = TRUE)
plot(tamostra, y=rep(.05, length(tamostra)), col = 2, type = "l", ylim = c(0,1), xlab="", 
     ylab="", lwd = 2.5, xlim = c(0,tam))
