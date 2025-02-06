##################################
####   Testes de normalidade  ####
##################################


#### Teste de Kolmogorov-Smirnov

tt <- as.vector(tt)
tt <- tt$IndiceCI
ks.test(tt, "pnorm", mean(tt), sd(tt))
t.ks
#Avalia o grau de concordância entre a distribuição de um conjunto de valores observados e determinada distribuição teórica.
#Consiste em comparar a distribuição de frequência acumulada da distribuição teórica com aquela observada.
#Realizado o teste obteve-se um p-valor de aproximadamente `r round(t.ks[[2]][1],5)`, o que inviabiliza rejeitar a hipótese de que haja normalidade entre os dados, com um grau de confiabilidade minimamente razoável.

#### Teste de Shapiro-Wilks
w <- NULL
p <- NULL
for (i in 1:10000){
  aux <-  shapiro.test(sample(tt, 5000))
  w[i] <- aux$statistic[[1]][1]
  p[i] <- aux$p.value[[1]][1]
}
hist(w)
hist(p)


#O teste de Shapiro-Wilks é um procedimento alternativo ao teste de Kolmogorov-Smirnov para avaliar normalidade.
#Realizado o teste obteve-se um p-valor de aproximadamente `r round(t.sw[[2]][1],5)`, o que, semelhantemente, inviabiliza rejeitar a hipótese de que haja normalidade entre os dados, com um grau de confiabilidade minimamente razoável.


# Verificando as diferenças entre as categorias
# Teste de hipóteses
# H0 = referencia e demais integralizam a mesma carga horária anualmente

