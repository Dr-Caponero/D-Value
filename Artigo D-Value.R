# Definindo o diretório
# setwd("~/Estatística/2021.1/Big data/Trabalhos/microdados_educacao_superior_2019")

# Lendo os pacotes
library(patchwork)
library(sparklyr)
library(sparkedatools)
library(tidyverse)


# Conectando o spark
sc <- spark_connect(master = "local")

#lendo o dataset
D1 <- spark_read_parquet(sc, "Aluno_p", "dadosfinal_parquet", memory = FALSE)

# selecionando amostras de alunos e alunas
Referencia = D1 %>% filter(TP_SEXO == 1)
Demais = D1 %>% filter(TP_SEXO != 1)

# calculando a quantidade de alunos e alunas
qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]


# calculando o D-valor para amostras de 50 a 10000 alunos.
k = 1
tamostra = seq(50, 12050, 150)
PV = c(0,0)
PVP = c(0,0)
DV = c(0,0)
x = c(0,0)

for (j in tamostra){
  
  # cálculo das frações amostrais
  fR = j/(qR*2)
  fD = j/(qD*2)
    # amostragem
  Re1 = Referencia %>% sdf_sample(fR) %>% collect()
  De1 = Demais %>% sdf_sample(fD) %>% collect()
  
  
  if (nrow(Re1) > nrow(De1)) l1 = nrow(De1) else l1 = nrow(Re1)
  DV[k] = mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])
  x[k] <- l1*2
  k <- k+1
} #?????????????????????????????????  
DV

  # calculo da diferença dos valores de alunos e alunas
  TE=abs(mean(Re1$IndiceCI, na.rm = TRUE) - mean(De1$IndiceCI, na.rm = TRUE))
  S = rbind(Re1[1:l1,], De1[1:l1,])
  l2 = nrow(S)

  

  
  
# 1000 iterações da amostra com 10000
  D.int3 = NULL
  tamostra = 10100
  # cálculo das frações amostrais
  fR = 5500/(qR)
  fD = 5500/(qD)
  l1 <- 5000
  
  for (i in 1:200){
    # amostragem
    Re1 = Referencia %>% sdf_sample(fR) %>% collect()
    De1 = Demais %>% sdf_sample(fD) %>% collect()
    D.int3[i] = mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])
    cat(i,", ")
  }
  
  
D.int3 <- as.data.frame(D.int3)
aux <- D.int2 %>% filter (!is.na(D.int2))
colnames(aux) <- "D.int"
D.int4 <- rbind(D.int4,aux)

aux <- as.vector(D.int4)

aux <- as.numeric(D.int4$D.int)

g2 <- D.int4 %>% 
  ggplot() +
  aes(sample = D.int) +
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

g2 + 
  plot_layout(ncol = 1) + 
  theme(
    plot.tag.position = c(0.8, 0.8),
    plot.tag = element_text(size = 14, hjust = 0.5, vjust = 1))


mean(as.numeric(D.int4$D.int))
quantile(as.numeric(D.int4$D.int), c(0.025, 0.975))
  
g1 <- D.int4|>
     ggplot() +
     aes(x =D.int) +
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

g1 + 
     plot_layout(ncol = 1) + 
     theme(
       plot.tag.position = c(0.8, 0.8),
       plot.tag = element_text(size = 14, hjust = 0.5, vjust = 1))
   
  
  
  
  # permutação 
  N= 100
  estat<-numeric(0)
  for (i in 1:N) {
    Sr <- sample(S$IndiceCI)#sob H0
    estat[i] <- abs(mean(Sr[1:l1]) - mean(Sr[(l2-l1+1):l2], na.rm = TRUE))
  }
  
  #cálculo do P-Valor Permutado
  PVP[k] = sum(estat>TE)/N
  x[k] = l2
  # Cálculo do P-Valor
  t = t.test(Re1$IndiceCI[1:l2], De1$IndiceCI[1:l2], alternative = "greater")
  PV[k] = t$p.value
  k = k + 1
}

  
dv <- as.data.frame(cbind(x,DV))

g1 <-  dv %>% 
  ggplot(aes(y = DV, x = x)) +
  geom_point(color = "#1F78B4")+
  labs(
    title = "",
    y = 'D-Valor',
    x = 'Número de estudantes'
  )+
  ggpubr::stat_cor(
    aes(label = ..r.label..),
    cor.coef.name = c("rho"),
    label.sep = "; ", geom = "text",
    color="tomato", method = "pearson", 
    label.x = 14, label.y = 14, show.legend = F,
    p.accuracy = 0.001, r.accuracy = 0.0001,
    size = 2)+
  scale_y_continuous(limits=c(0,1),
                     labels = scales::number_format(
                       big.mark = ".",
                       decimal.mark = ","
                     ))+
  geom_smooth(method=lm, se=F, color="#1F78B4", size = 0.6)+
  geom_line(y=0.5, color="tomato", size = 0.6, )+
  theme_minimal(base_size = 14)

g1 + 
  plot_layout(ncol = 1) + 
  # plot_annotation(
  # tag_levels = c("A", "1"), tag_sep = ".") &
  theme(
    legend.position = "none",
    plot.tag.position = c(0.9, 0.8),
    plot.tag = element_text(size = 12, hjust = 0, vjust = -0.4))

pv <- as.data.frame(cbind(x,PV))

g2 <-  pv %>% 
  ggplot(aes(y = PV, x = x)) +
  geom_point(color = "#1F78B4")+
  labs(
    title = "",
    y = 'P-Valor',
    x = 'Número de estudantes'
  )+
  geom_hline(yintercept = 0.05, color="tomato")+
  geom_text(x=10000, y=0.1, label="Nivel de Significância de 5%", family = "Comfortaa")+
  scale_y_continuous(limits=c(0,1),
                     labels = scales::number_format(
                       big.mark = ".",
                       decimal.mark = ","
                     ))+
  theme_minimal(base_size = 14)


g2 + 
  plot_layout(ncol = 1) + 
  # plot_annotation(
  # tag_levels = c("A", "1"), tag_sep = ".") &
  theme(
    legend.position = "none",
    plot.tag.position = c(0.9, 0.8),
    plot.tag = element_text(size = 12, hjust = 0, vjust = -0.4))

pvp <- as.data.frame(cbind(x,PVP))

g3 <-  pvp %>% 
  ggplot(aes(y = PVP, x = x)) +
  geom_point(color = "#1F78B4")+
  labs(
    title = "",
    y = 'P-Valor Permutado',
    x = 'Número de estudantes'
  )+
  geom_hline(yintercept = 0.05, color="tomato")+
  geom_text(x=10000, y=0.1, label="Nivel de Significância de 5%", family = "Comfortaa")+
  scale_y_continuous(limits=c(0,1),
                     labels = scales::number_format(
                       big.mark = ".",
                       decimal.mark = ","
                     ))+
  theme_minimal(base_size = 14)


g3 + 
  plot_layout(ncol = 1) + 
  # plot_annotation(
  # tag_levels = c("A", "1"), tag_sep = ".") &
  theme(
    legend.position = "none",
    plot.tag.position = c(0.9, 0.8),
    plot.tag = element_text(size = 12, hjust = 0, vjust = -0.4))

# D-Valor da variável Sexo já calculada para 10100 observações
DV_v = NULL
DV_v[1] = DV[68] 
# calculando o D-Valor para as demais variáveis em amostras com 10100 observações

# Variável Turno (3 = noturno)
Referencia = D1 %>% filter(TP_TURNO == 3)
Demais = D1 %>% filter(TP_TURNO != 3)

qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]

fR = 5050/(qR*2)
fD = 5050/(qD*2)
# amostragem
Re1 = Referencia %>% sdf_sample(fR) %>% collect()
De1 = Demais %>% sdf_sample(fD) %>% collect()

if (nrow(Re1) > nrow(De1)) l1 = nrow(De1) else l1 = nrow(Re1)
DV_v[2] = mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])

# Variável Grau Acadêmico (3 = tecnólogo)
Referencia = D1 %>% filter(TP_GRAU_ACADEMICO == 3)
Demais = D1 %>% filter(TP_GRAU_ACADEMICO != 3)

qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]

fR = 5050/(qR*2)
fD = 5050/(qD*2)
# amostragem
Re1 = Referencia %>% sdf_sample(fR) %>% collect()
De1 = Demais %>% sdf_sample(fD) %>% collect()

if (nrow(Re1) > nrow(De1)) l1 = nrow(De1) else l1 = nrow(Re1)
DV_v[3] = mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])

# Variável modalidade de Ensino (1 = presencial)
Referencia = D1 %>% filter(TP_MODALIDADE_ENSINO == 1)
Demais = D1 %>% filter(TP_MODALIDADE_ENSINO != 1)

qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]

fR = 5050/(qR*2)
fD = 5050/(qD*2)
# amostragem
Re1 = Referencia %>% sdf_sample(fR) %>% collect()
De1 = Demais %>% sdf_sample(fD) %>% collect()

if (nrow(Re1) > nrow(De1)) l1 = nrow(De1) else l1 = nrow(Re1)
DV_v[4] = mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])

# Variável Cor/Raça (1 = branco)
Referencia = D1 %>% filter(TP_COR_RACA == 1)
Demais = D1 %>% filter(TP_COR_RACA != 1)

qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]

fR = 5050/(qR*2)
fD = 5050/(qD*2)
# amostragem
Re1 = Referencia %>% sdf_sample(fR) %>% collect()
De1 = Demais %>% sdf_sample(fD) %>% collect()

if (nrow(Re1) > nrow(De1)) l1 = nrow(De1) else l1 = nrow(Re1)
DV_v[5] = mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])
# 
# # Variável Situação (2 = cursando)
# Referencia = D1 %>% filter(TP_SITUACAO == 2)
# Demais = D1 %>% filter(TP_SITUACAO != 2)
# 
# qR = Referencia %>% sdf_dim ()
# qD = Demais %>% sdf_dim ()
# qR = qR[1]
# qD = qD[1]
# 
# fR = 5050/(qR*2)
# fD = 5050/(qD*2)
# # amostragem
# Re1 = Referencia %>% sdf_sample(fR) %>% collect()
# De1 = Demais %>% sdf_sample(fD) %>% collect()
# 
# if (nrow(Re1) > nrow(De1)) l1 = nrow(De1) else l1 = nrow(Re1)
# DV_v[6] = mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])

# Variável Financiamento Estudantil (1 = recebendo)
Referencia = D1 %>% filter(IN_FINANCIAMENTO_ESTUDANTIL == 1)
Demais = D1 %>% filter(IN_FINANCIAMENTO_ESTUDANTIL != 1)

qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]

fR = 5050/(qR*2)
fD = 5050/(qD*2)
# amostragem
Re1 = Referencia %>% sdf_sample(fR) %>% collect()
De1 = Demais %>% sdf_sample(fD) %>% collect()

if (nrow(Re1) > nrow(De1)) l1 = nrow(De1) else l1 = nrow(Re1)
DV_v[7] = mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])

# Variável Apoio Social (1 = recebendo)
Referencia = D1 %>% filter(IN_APOIO_SOCIAL == 1)
Demais = D1 %>% filter(IN_APOIO_SOCIAL != 1)

qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]

fR = 5050/(qR*2)
fD = 5050/(qD*2)
# amostragem
Re1 = Referencia %>% sdf_sample(fR) %>% collect()
De1 = Demais %>% sdf_sample(fD) %>% collect()

if (nrow(Re1) > nrow(De1)) l1 = nrow(De1) else l1 = nrow(Re1)
DV_v[8] = mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])




# Variável Estado  (22 = Piaui/43 = RS)
Referencia = D1 %>% filter(UF == 22)
Demais = D1 %>% filter(UF == 43)

qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]

fR = 5050/(qR*2)
fD = 5050/(qD*2)
# amostragem
Re1 = Referencia %>% sdf_sample(fR) %>% collect()
De1 = Demais %>% sdf_sample(fD) %>% collect()

if (nrow(Re1) > nrow(De1)) l1 = nrow(De1) else l1 = nrow(Re1)
DV_v[9] = mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])





DV_v <- DV_v[-6]
Variaveis = c("Sexo", "Turno", "Grau Acadêmico", "Modalidade", "Cor/Raça", "Fiananciamento", "Apoio Social")
DV2 = data.frame(DV_v, Variaveis)

DV2 = DV2[order(DV2$DV_v),]


# Desconectando do Spark
spark_disconnect(sc)


DV.tab
DV2$DV_v <- DV.tab$med[1:7] 
DV2$Variaveis <- DV.tab$nome[1:7]
rownames(DV2) <- DV.tab$nome[1:7]

g1 <- DV2 %>%
  mutate(Variaveis = fct_reorder(Variaveis, DV_v)) %>%
  ggplot( aes(x=Variaveis, y=DV_v)) +
  geom_bar(stat="identity", fill="#1F78B4", alpha=.6, width=.4) +
  geom_text(aes(label=DV_v), vjust=0.5, hjust=-0.2)+
  labs(title = "", y = 'D-Valor')+
  scale_y_continuous(limits=c(0,0.55),
                     labels = scales::number_format(
                       big.mark = ".",
                       decimal.mark = ","
                     ))+
  coord_flip() +
  xlab("") +
  geom_hline(yintercept = 0.5, color="tomato")+
  theme_minimal(base_size = 14)



g1 + 
  plot_layout(ncol = 1) + 
  theme(
    legend.position = "none")



ddd <- DV2$DV_v
ddd <- as.data.frame(ddd)
rownames(ddd) <- DV2$Variaveis
colnames(ddd) <- "ddd"


b.cor <- ifelse(ddd$ddd < 0.5, "lightpink", "#A6CEE3")
ddd %>%
  rownames_to_column(var = 'make') %>%
  ggplot(aes(x = ddd - 0.5 , y = fct_reorder(make, ddd),fill = 0 > ddd - .5)) +
  geom_col(show.legend = FALSE,  fill = b.cor) +
  scale_x_continuous(labels = seq(0.4, 0.6, 0.05)) +
  labs(x = 'D-valor', y = '') +
  theme_minimal(base_size = 14) 



