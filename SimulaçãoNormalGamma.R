library(ggplot2)
library(patchwork)
library(moments)
library(tidyverse)
##################################################
###      Simulação da Distribuição Normal      ###
##################################################

# Parâmetros referêcia
md <- c(1, 10, 1, 10)
dp <- c(1, 1, 10, 10)

# Parâmetros comparação
m2 <- rbind(c(1.01,1.03,1.1,1.31,2),
            c(10.01,10.03,10.1,10.31,11),
            c(1.01,1.03,1.1,1.31,2),
            c(10.01,10.03,10.1,10.31,11))
sg  <- rbind(c(1.01,1.03,1.1,1.31,2),
             c(1.01,1.03,1.1,1.31,2),
             c(10.01,10.03,10.1,10.31,11),
             c(10.01,10.03,10.1,10.31,11))

#Tamanho da amostra
ns <- 1000
#inicialização de variáveis
DV <- NULL
media1 <- NULL
media2 <- NULL
dev1 <- NULL
dev2 <- NULL
ske1 = NULL
ske2 = NULL
kur1 = NULL
kur2 = NULL
i <- 0

#Cálculo iterativo para cada grupo
for (l in 1:4){
  #parâmetros da distribuição de referência
  m1 <- md[l]
  s1 <- dp[l]
  
  for (s in sg[l,]){
    j <- 0
    for (m in m2[l,]){
      j <- j+1
      D.valores <- NULL
      for (K in 1:1000){
        # distribuição de referência
        x1 <- rnorm(ns,m1,s1)
        # distribuição de comparação
        x2 <- rnorm(ns,m,s)
        #comparação
        D.valores[K]= mean(x1 < x2)
        media1[K] <- mean(x1)
        media2[K] <- mean(x2)
        dev1[K] <- sd(x1)
        dev2[K] <- sd(x2)
        ske1[K] <- skewness(x1)
        ske2[K] <- skewness(x2)
        kur1[K] <- kurtosis(x1)
        kur2[K] <- kurtosis(x2)
      }
      #armazenamento dos valores obtidos
      DV$dv[j+(i*5)] <- mean(D.valores)
      DV$media[j+(i*5)] <- m
      DV$dp[j+(i*5)] <- s
      DV$Grupo[j+(i*5)] <- l
      DV$m1[i*5+j] <- mean(media1)
      DV$m2[i*5+j] <- mean(media2)
      DV$s1[i*5+j] <- mean(dev1)
      DV$s2[i*5+j] <- mean(dev2)
      DV$a1[i*5+j] <- mean(ske1)
      DV$a2[i*5+j] <- mean(ske2)
      DV$c1[i*5+j] <- mean(kur1)
      DV$c2[i*5+j] <- mean(kur2)
      
    } 
    i <- i+1
  }
}
dist.norm <- D.valores
DV.norm <- as.data.frame(DV)

DV.norm$d_ske = 100*(DV.norm$a1-DV.norm$a2)
DV.norm$d_kur = 100*(DV.norm$c1-DV.norm$c2)


ggplot(data=DV.norm)+
  geom_point(aes(x = d_ske, y=d_kur, colour = as.factor(Grupo), size = dv))+
  scale_colour_manual(values = c("orange", "seagreen3", "cyan", "#723934"))



DV_1 = DV.norm %>% filter(Grupo==1)
ggplot(data=DV_1)+
  geom_point(aes(x = d_ske, y=d_kur, colour = as.factor(Grupo), size = dv))+
  scale_colour_manual(values = c("orange", "seagreen3", "cyan", "#723934")) 


DV_4 = DV.norm %>% filter(Grupo==4)
ggplot(data=DV_4)+
  geom_point(aes(x = d_ske, y=d_kur, colour = as.factor(Grupo), size = dv))+
  scale_colour_manual(values = c("#723934", "seagreen3", "cyan", "#723934")) 

DV_2 = DV.norm %>% filter(Grupo==2)
ggplot(data=DV_2)+
  geom_point(aes(x = d_ske, y=d_kur, colour = as.factor(Grupo), size = dv))+
  scale_colour_manual(values = c("seagreen3", "seagreen3", "cyan", "#723934")) 

DV_3 = DV.norm %>% filter(Grupo==3)
ggplot(data=DV_3)+
  geom_point(aes(x = d_ske, y=d_kur, colour = as.factor(Grupo), size = dv))+
  scale_colour_manual(values = c("cyan", "seagreen3", "cyan", "#723934")) 

#Tamanho da amostra
ns <- 50
#inicialização de variáveis
DV <- NULL
i <- 0

#Cálculo iterativo para cada grupo
for (l in 1:4){
  #parâmetros da distribuição de referência
  m1 <- md[l]
  s1 <- dp[l]
  
  for (s in sg[l,]){
    j <- 0
    for (m in m2[l,]){
      j <- j+1
      D.valores <- NULL
      for (K in 1:1000){
        # distribuição de referência
        x1 <- rnorm(ns,m1,s1)
        # distribuição de comparação
        x2 <- rnorm(ns,m,s)
        #comparação
        D.valores[K]= mean(x1 < x2)
      }
      #armazenamento dos valores obtidos
      DV$dv[j+(i*5)] <- mean(D.valores)
      DV$media[j+(i*5)] <- m
      DV$dp[j+(i*5)] <- s
      DV$Grupo[j+(i*5)] <- l
    } 
    i <- i+1
  }
}

DV.norm2 <- as.data.frame(DV)


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

# Tamanho da amostra
ns <- 1000
#Inicializando variável
DV <- NULL
media1 <- NULL
media2 <- NULL
dev1 <- NULL
dev2 <- NULL
ske1 = NULL
ske2 = NULL
kur1 = NULL
kur2 = NULL
#Cálculo iterativo
for (l in 1:4){ 
  #parametros de referência
  alfa <- alfa0[l]
  beta <- beta0[l]
  li <- (l-1)*5 + 1
  lf <- l*5 
  for (i in li:lf){
    for (j in 1:5){
      D.valores <- NULL
        for (K in 1:1000){
        x1 <- rgamma(ns,alfa,beta)
        x2 <- rgamma(ns,a[i,j],b[i,j])
        D.valores[K] <- mean(x1 < x2)
        media1[K] <- mean(x1)
        media2[K] <- mean(x2)
        dev1[K] <- sd(x1)
        dev2[K] <- sd(x2)
        ske1[K] <- skewness(x1)
        ske2[K] <- skewness(x2)
        kur1[K] <- kurtosis(x1)
        kur2[K] <- kurtosis(x2)
        
      }
      DV$dv[(i-1)*5+j] <- mean(D.valores)
      DV$media[(i-1)*5+j] <- a[i,j]/b[i,j]
      DV$dp[(i-1)*5+j] <- sqrt(a[i,j]/(b[i,j]*b[i,j]))
      DV$Grupo[(i-1)*5+j] <- l
      DV$m1[(i-1)*5+j] <- mean(media1)
      DV$m2[(i-1)*5+j] <- mean(media2)
      DV$s1[(i-1)*5+j] <- mean(dev1)
      DV$s2[(i-1)*5+j] <- mean(dev2)
      DV$a1[(i-1)*5+j] <- mean(ske1)
      DV$a2[(i-1)*5+j] <- mean(ske2)
      DV$c1[(i-1)*5+j] <- mean(kur1)
      DV$c2[(i-1)*5+j] <- mean(kur2)
    } 
  }
}

dist.gama <- D.valores
DV.gama <- as.data.frame(DV)

DV.gama$d_ske = 100*(DV.gama$a1-DV.gama$a2)
DV.gama$d_kur = 100*(DV.gama$c1-DV.gama$c2)


ggplot(data=DV.gama)+
  geom_point(aes(x = d_ske, y=d_kur, colour = as.factor(Grupo), size = dv))+
  scale_colour_manual(values = c("orange", "seagreen3", "cyan", "#723934"))


DV_1 = DV.gama %>% filter(Grupo==1)
ggplot(data=DV_1)+
  geom_point(aes(x = d_ske, y=d_kur, colour = as.factor(Grupo), size = dv))+
  scale_colour_manual(values = c("orange", "seagreen3", "cyan", "#723934")) 


DV_4 = DV.gama %>% filter(Grupo==4)
ggplot(data=DV_4)+
  geom_point(aes(x = d_ske, y=d_kur, colour = as.factor(Grupo), size = dv))+
  scale_colour_manual(values = c("#723934", "seagreen3", "cyan", "#723934")) 

DV_2 = DV.gama %>% filter(Grupo==2)
ggplot(data=DV_2)+
  geom_point(aes(x = d_ske, y=d_kur, colour = as.factor(Grupo), size = dv))+
  scale_colour_manual(values = c("seagreen3", "seagreen3", "cyan", "#723934")) 

DV_3 = DV.gama %>% filter(Grupo==3)
ggplot(data=DV_3)+
  geom_point(aes(x = d_ske, y=d_kur, colour = as.factor(Grupo), size = dv))+
  scale_colour_manual(values = c("cyan", "seagreen3", "cyan", "#723934")) 


# Tamanho da amostra
ns <- 50
#Inicializando variável
DV <- NULL
#Cálculo iterativo
for (l in 1:4){ 
  #parametros de referência
  alfa <- alfa0[l]
  beta <- beta0[l]
  li <- (l-1)*5 + 1
  lf <- l*5 
  for (i in li:lf){
    for (j in 1:5){
      D.valores <- NULL
      for (K in 1:1000){
        x1 <- rgamma(ns,alfa,beta)
        x2 <- rgamma(ns,a[i,j],b[i,j])
        D.valores[K]= mean(x1 < x2)
      }
      DV$dv[(i-1)*5+j] <- mean(D.valores)
      DV$media[(i-1)*5+j] <- a[i,j]/b[i,j]
      DV$dp[(i-1)*5+j] <- sqrt(a[i,j]/(b[i,j]*b[i,j]))
      DV$Grupo[(i-1)*5+j] <- l
    } 
  }
}

DV.gama2 <- as.data.frame(DV)


##################################################
####       Gráficos das distribuições         ####
##################################################

#Distribuição Normal
#Amostra pequena (50)


df <- data.frame(inicio = c(0.0000,.29001,.36001,.49001,.56001,.64001,.71001),
                 fim =    c(.29000,.36000,.50000,.56000,.64000,.71000,.81),
                 Efeito = c("Grande", "Médio", "Pequeno", "Ínfimo", "Pequeno", "Médio", "Grande"),
                 stringsAsFactors = FALSE)




g1 <- DV.norm2 %>% filter (Grupo==1) %>% 
  ggplot() +
  geom_rect(data = df, aes(NULL,NULL,ymin =inicio, ymax = fim, fill=Efeito, xmin=1, xmax=2), alpha = 0.2)+
  scale_fill_manual(breaks = c("Grande","Médio","Pequeno","Ínfimo"),
                    values=c("Grande" = "black", "Médio" = "#626262", "Pequeno" = "#a7a7a7", "Ínfimo" = "white")) +
  aes(x = media) +
  geom_line(
    aes(y = dv, color= factor(dp)), linewidth = 1)+
  scale_y_continuous(limits = c(0.49, .81),
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(colour = "Desvio Padrão",
    x = "µ*",
    y = "d-valor"
  )+
  scale_color_brewer(palette = "Blues")+
  theme_minimal(base_size = 14)

  

g2 <- DV.norm2 %>% filter (Grupo==4) %>% 
  ggplot() +
  geom_rect(data = df, aes(NULL,NULL,ymin =inicio, ymax = fim, fill=Efeito, xmin=10, xmax=11), alpha = 0.2)+
  scale_fill_manual(breaks = c("Grande","Médio","Pequeno","Ínfimo"),
                    values=c("Grande" = "black", "Médio" = "#626262", "Pequeno" = "#a7a7a7", "Ínfimo" = "white")) +
  aes(x = media) +
  geom_line(
    aes(y = dv, color= factor(dp)), linewidth = 1)+
  scale_y_continuous(limits = c(0.49,.81),
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(colour = "Desvio Padrão",
    x = "µ*",
    y = "d-valor"
  )+
  scale_color_brewer(palette = "Reds")+
  theme_minimal(base_size = 14)

#Amostra grande (10000)

g3 <- DV.norm %>% filter (Grupo==1) %>% 
  ggplot() +
  geom_rect(data = df, aes(NULL,NULL,ymin =inicio, ymax = fim, fill=Efeito, xmin=1, xmax=2), alpha = 0.2)+
  scale_fill_manual(breaks = c("Grande","Médio","Pequeno","Ínfimo"),
                    values=c("Grande" = "black", "Médio" = "#626262", "Pequeno" = "#a7a7a7", "Ínfimo" = "white")) +
  aes(x = media) +
  geom_line(
    aes(y = dv, color= factor(dp)), linewidth = 1)+
  scale_y_continuous(limits = c(0.49,.81),
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(colour = "Desvio Padrão",
    x = "µ*",
    y = "d-valor"
  )+
  scale_color_brewer(palette = "Blues")+
  theme_minimal(base_size = 14)

g4 <- DV.norm %>% filter (Grupo==4) %>% 
  ggplot() +
  geom_rect(data = df, aes(NULL,NULL,ymin =inicio, ymax = fim, fill=Efeito, 
                           xmin=10, xmax=11), alpha = 0.2)+
  scale_fill_manual(breaks = c("Grande","Médio","Pequeno","Ínfimo"),
                    values=c("Grande" = "black", "Médio" = "#626262", "Pequeno" = "#a7a7a7", "Ínfimo" = "white")) +
  aes(x = media) +
  geom_line(
    aes(y = dv, color= factor(dp)), linewidth = 1)+
  scale_y_continuous(limits = c(0.49,.81),
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(colour = "Desvio Padrão",
    x = "µ*",
    y = "d-valor"
  )+
  scale_color_brewer(palette = "Reds")+
  theme_minimal(base_size = 14)

g1 + g2 + 
  plot_layout(ncol = 2, guides = "collect")+
  plot_annotation(
    tag_levels = c("A", "1"), tag_prefix = "(", tag_sep = ".",
    tag_suffix = ")")&
  theme(
    plot.tag.position = c(0.25, 0.8),
    plot.tag = element_text(size = 14, hjust = 0.5, vjust = 1))

g3 + g4 +
  plot_layout(ncol = 2, guides = "collect")+
  plot_annotation(
    tag_levels = c("A", "1"), tag_prefix = "(", tag_sep = ".",
    tag_suffix = ")")&
  theme(
    plot.tag.position = c(0.25, 0.8),
    plot.tag = element_text(size = 14, hjust = 0.5, vjust = 1))

#Distribuição Gamma
#Amostra pequena (50)

df <- data.frame(inicio = c(0.2000,.32001,.36001,.44001,.56001,.64001,.71001),
                 fim =    c(.29000,.36000,.44000,.56000,.64000,.71000,.81),
                 Efeito = c("Grande", "Médio", "Pequeno", "Ínfimo", "Pequeno", "Médio", "Grande"),
                 stringsAsFactors = FALSE)



g1 <- DV.gama2 %>% filter (Grupo==1) %>% 
  ggplot() +
  geom_rect(data = df, aes(NULL,NULL,ymin =inicio, ymax = fim, fill=Efeito, xmin=1, xmax=2), alpha = 0.2)+
  scale_fill_manual(breaks = c("Grande","Médio","Pequeno","Ínfimo"),
                    values=c("Grande" = "black", "Médio" = "#626262", "Pequeno" = "#a7a7a7", "Ínfimo" = "white")) +
  aes(x = media) +
  geom_line(
    aes(y = dv, color= factor(dp)), linewidth = 1)+
  scale_y_continuous(limits = c(0.32, .81),
                     labels = scales::number_format(
                       big.mark = ".",
                       decimal.mark = ","
                     )) +
  labs(colour = "Desvio Padrão",
       x = "µ*",
       y = "d-valor"
  )+ 
  scale_color_brewer(palette = "Blues")+
  theme_minimal(base_size = 14)


g2 <- DV.gama2 %>% filter (Grupo==4) %>% 
  ggplot() +
  geom_rect(data = df, aes(NULL,NULL,ymin =inicio, ymax = fim, fill=Efeito, xmin=10, xmax=11), alpha = 0.2)+
  scale_fill_manual(breaks = c("Grande","Médio","Pequeno","Ínfimo"),
                    values=c("Grande" = "black", "Médio" = "#626262", "Pequeno" = "#a7a7a7", "Ínfimo" = "white")) +
  aes(x = media) +
  geom_line(
    aes(y = dv, color= factor(dp)), linewidth = 1)+
  scale_y_continuous(limits = c(0.32, .81),
                     labels = scales::number_format(
                       big.mark = ".",
                       decimal.mark = ","
                     )) +
  labs(colour = "Desvio Padrão",
       x = "µ*",
       y = "d-valor"
  )+
  scale_color_brewer(palette = "Reds")+
  theme_minimal(base_size = 14)

#Amostra grande (10000)

g3 <- DV.gama %>% filter (Grupo==1) %>% 
  ggplot() +
  geom_rect(data = df, aes(NULL,NULL,ymin =inicio, ymax = fim, fill=Efeito, xmin=1, xmax=2), alpha = 0.2)+
  scale_fill_manual(breaks = c("Grande","Médio","Pequeno","Ínfimo"),
                    values=c("Grande" = "black", "Médio" = "#626262", "Pequeno" = "#a7a7a7", "Ínfimo" = "white")) +
  aes(x = media) +
  geom_line(
    aes(y = dv, color= factor(dp)), linewidth = 1)+
  scale_y_continuous(limits = c(0.32,.81),
                     labels = scales::number_format(
                       big.mark = ".",
                       decimal.mark = ","
                     )) +
  labs(colour = "Desvio Padrão",
       x = "µ*",
       y = "d-valor"
  )+
  scale_color_brewer(palette = "Blues")+
  theme_minimal(base_size = 14)

g4 <- DV.gama %>% filter (Grupo==4) %>% 
  ggplot() +
  geom_rect(data = df, aes(NULL,NULL,ymin =inicio, ymax = fim, fill=Efeito, xmin=10, xmax=11), alpha = 0.2)+
  scale_fill_manual(breaks = c("Grande","Médio","Pequeno","Ínfimo"),
                    values=c("Grande" = "black", "Médio" = "#626262", "Pequeno" = "#a7a7a7", "Ínfimo" = "white")) +
  aes(x = media) +
  geom_line(
    aes(y = dv, color= factor(dp)), linewidth = 1)+
  scale_y_continuous(limits = c(0.32, .81),
                     labels = scales::number_format(
                       big.mark = ".",
                       decimal.mark = ","
                     )) +
  labs(colour = "Desvio Padrão",
       x = "µ*",
       y = "d-valor"
  )+
  scale_color_brewer(palette = "Reds")+
  theme_minimal(base_size = 14)

g1 + g2 + 
  plot_layout(ncol = 2, guides = "collect")+
  plot_annotation(
    tag_levels = c("A", "1"), tag_prefix = "(", tag_sep = ".",
    tag_suffix = ")")&
  theme(
    plot.tag.position = c(0.25, 0.8),
    plot.tag = element_text(size = 14, hjust = 0.5, vjust = 1))
g3 + g4 +
  plot_layout(ncol = 2, guides = "collect")+
  plot_annotation(
    tag_levels = c("A", "1"), tag_prefix = "(", tag_sep = ".",
    tag_suffix = ")")&
  theme(
    plot.tag.position = c(0.25, 0.8),
    plot.tag = element_text(size = 14, hjust = 0.5, vjust = 1))


##################################
#### Distribuições do D-valor
#################################


dist.norm <- as.data.frame(dist.norm)

g2 <- dist.norm %>% 
  ggplot() +
  aes(sample = dist.norm) +
  stat_qq(
    fill = "#1F78B4",
    colour = "#1F78B4") +
  stat_qq_line(color="tomato", size=1)+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    y = "D-Valor",
    x = "Quantis Teóricos"
  )+
  theme_minimal(base_size = 14)


g1 <- dist.norm|>
  ggplot() +
  aes(x =dist.norm) +
  geom_histogram(
    aes(y = ..density..),
    bins = 35,
    fill = "#A6CEE3",
    colour = "#000099") +
  geom_density(
    alpha = 0.2,
    fill = "#1F78B4",
    colour = "#1F78B4") +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    x = "D-Valor",
    y = "Densidade"
  )+theme_minimal(base_size = 14)

g1 + g2 +
  plot_layout(ncol = 2) + 
  theme(
    plot.tag.position = c(0.8, 0.8),
    plot.tag = element_text(size = 14, hjust = 0.5, vjust = 1))


dist.gama <- as.data.frame(dist.gama)
g2 <- dist.gama %>% 
  ggplot() +
  aes(sample = dist.gama) +
  stat_qq(
    fill = "#1F78B4",
    colour = "#1F78B4") +
  stat_qq_line(color="tomato", size=1)+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    y = "D-Valor",
    x = "Quantis Teóricos"
  )+
  theme_minimal(base_size = 14)


g1 <- dist.gama|>
  ggplot() +
  aes(x =dist.gama) +
  geom_histogram(
    aes(y = ..density..),
    bins = 35,
    fill = "#A6CEE3",
    colour = "#000099") +
  geom_density(
    alpha = 0.2,
    fill = "#1F78B4",
    colour = "#1F78B4") +
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    )) +
  labs(
    x = "D-Valor",
    y = "Densidade"
  )+theme_minimal(base_size = 14)

g1 + g2 +
  plot_layout(ncol = 2) + 
  theme(
    plot.tag.position = c(0.8, 0.8),
    plot.tag = element_text(size = 14, hjust = 0.5, vjust = 1))



#tabela de a e b utilizados nas simulações gamma
options(scipen = 999)

colnames(alfa1) = c("$\\mu = 1,01$", "$\\mu = 1,03$","$\\mu = 1,10$","$\\mu = 1,31$","$\\mu = 2,00$")
rownames(alfa1) = c("$dp = 1,01$", '$dp = 1,03$', "$dp = 1,10$","$dp = 1,31$","$dp = 2,00$")
knitr::kable(alfa1, escape = FALSE)
#teste de assimetria da maior diferença de d-valor
# pa <- rgamma(1000,a[1,5],b[1,5])
# pb <- rgamma(1000,1,1)
# par(mfrow=c(1,2))
# hist(pa)
# hist(pb)
# library(moments)
# skewness(pa)
# skewness(pb)
# skewness(pb)/skewness(pa)
