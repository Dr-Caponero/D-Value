# Lendo os pacotes
library(sparklyr)
library(tidyverse)
#library(sparkedatools)
library(patchwork)

# Conectando o spark
sc <- spark_connect(master = "local")

#dados trabalhados
D1 <- spark_read_parquet(sc, "Aluno_p", "dadosfinal_parquet", memory = FALSE)
D1 |> sdf_dim()

# Variável Turno (3 = noturno)
Referencia = D1 %>% filter(TP_TURNO == 3)
Demais = D1 %>% filter(TP_TURNO != 3)

qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]
l1 <- 5000
fR = 5500/(qR)
fD = 5500/(qD)
DV_turno <- NULL
# amostragem
for (i in 1:1000){
  Re1 <-  Referencia %>% sdf_sample(fR) %>% collect()
  De1 <- Demais %>% sdf_sample(fD) %>% collect()
  DV_turno[i] <-  mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])
  cat(i,",")
}
DV_turno <- as.data.frame(DV_turno)
write_csv(DV_turno, "DV_turno.csv")

# Variável Grau Acadêmico (3 = tecnólogo)
Referencia = D1 %>% filter(TP_GRAU_ACADEMICO == 3)
Demais = D1 %>% filter(TP_GRAU_ACADEMICO != 3)

qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]
l1 <- 5000
fR = 5500/(qR)
fD = 5500/(qD)
DV_grau <- NULL
# amostragem
for (i in 1:1000){
  Re1 <-  Referencia %>% sdf_sample(fR) %>% collect()
  De1 <- Demais %>% sdf_sample(fD) %>% collect()
  DV_grau[i] <-  mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])
  cat(i,",")
}
DV_grau <- as.data.frame(DV_grau)
write_csv(DV_grau, "DV_grau.csv")


# Variável modalidade de Ensino (1 = presencial)
Referencia = D1 %>% filter(TP_MODALIDADE_ENSINO == 1)
Demais = D1 %>% filter(TP_MODALIDADE_ENSINO != 1)

qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]
l1 <- 5000
fR = 5500/(qR)
fD = 5500/(qD)
DV_modal <- NULL
# amostragem
for (i in 1:1000){
  Re1 <-  Referencia %>% sdf_sample(fR) %>% collect()
  De1 <- Demais %>% sdf_sample(fD) %>% collect()
  DV_modal[i] <-  mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])
  cat(i,",")
}
DV_modal <- as.data.frame(DV_modal)
write_csv(DV_modal, "DV_modal.csv")


# Variável Cor/Raça (1 = branco)
Referencia = D1 %>% filter(TP_COR_RACA == 1)
Demais = D1 %>% filter(TP_COR_RACA != 1)

qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]
l1 <- 5000
fR = 5500/(qR)
fD = 5500/(qD)
DV_raca <- NULL
# amostragem
for (i in 1:1000){
  Re1 <-  Referencia %>% sdf_sample(fR) %>% collect()
  De1 <- Demais %>% sdf_sample(fD) %>% collect()
  DV_raca[i] <-  mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])
  cat(i,",")
}
DV_raca <- as.data.frame(DV_raca)
write_csv(DV_raca, "DV_raca.csv")

# Variável Financiamento Estudantil (1 = recebendo)
Referencia = D1 %>% filter(IN_FINANCIAMENTO_ESTUDANTIL == 1)
Demais = D1 %>% filter(IN_FINANCIAMENTO_ESTUDANTIL != 1)

qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]
l1 <- 5000
fR = 5500/(qR)
fD = 5500/(qD)
DV_finan <- NULL
# amostragem
for (i in 1:1000){
  Re1 <-  Referencia %>% sdf_sample(fR) %>% collect()
  De1 <- Demais %>% sdf_sample(fD) %>% collect()
  DV_finan[i] <-  mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])
  cat(i,",")
}
DV_finan <- as.data.frame(DV_finan)
write_csv(DV_finan, "DV_finan.csv")


# Variável Apoio Social (1 = recebendo)
Referencia = D1 %>% filter(IN_APOIO_SOCIAL == 1)
Demais = D1 %>% filter(IN_APOIO_SOCIAL != 1)

qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]
l1 <- 5000
fR = 5500/(qR)
fD = 5500/(qD)
DV_social <- NULL
# amostragem
for (i in 1:1000){
  Re1 <-  Referencia %>% sdf_sample(fR) %>% collect()
  De1 <- Demais %>% sdf_sample(fD) %>% collect()
  DV_social[i] <-  mean(Re1$IndiceCI[1:l1] > De1$IndiceCI[1:l1])
  cat(i,",")
}
DV_social <- as.data.frame(DV_social)
write_csv(DV_social, "DV_social.csv")

# Variável Estado  (22 = Piaui/43 = RS)
Referencia = D1 %>% filter(UF == 22)
Demais = D1 %>% filter(UF == 43)

qR = Referencia %>% sdf_dim ()
qD = Demais %>% sdf_dim ()
qR = qR[1]
qD = qD[1]
l1 <- 5000
fR = 5500/(qR)
fD = 5500/(qD)
DV_uf <- NULL
# amostragem
for (i in 1:1000){
  Re1 <-  Referencia %>% sdf_sample(fR) %>% collect()
  De1 <- Demais %>% sdf_sample(fD) %>% collectcat(i,",")
}DV_uf <- as.data.frame(DV_uf)
write_csv(DV_uf, "DV_uf.csv")


# Desconectando do Spark
spark_disconnect(sc)
DV_sexo <- D.int4

fator <- dt(0.975,999)/sqrt(1000)
DV.tab <- c(mean(DV_sexo$D.int), mean(DV_turno$DV_turno), mean(DV_grau$DV_grau), mean(DV_modal$DV_modal),
            mean(DV_raca$DV_raca),mean(DV_finan$DV_finan),mean(DV_social$DV_social),mean(DV_uf$DV_uf))
DV.tab <- as.data.frame(DV.tab)
DV.tab <- cbind(DV.tab, c(var(DV_sexo$D.int), var(DV_turno$DV_turno), var(DV_grau$DV_grau), var(DV_modal$DV_modal),
                          var(DV_raca$DV_raca),var(DV_finan$DV_finan),var(DV_social$DV_social),var(DV_uf$DV_uf)))
colnames(DV.tab) <- c("med", "var")
DV.tab$li <- DV.tab$med - fator * sqrt(DV.tab$var)
DV.tab$ls <- DV.tab$med + fator * sqrt(DV.tab$var)
rownames(DV.tab) <- c("Sexo", "Turno", "Grau Acadêmico", "Modalidade", "Cor/Raça", "Financiamento", "Auxílio Social", "UF")


DV.tab$li <- DV.tab$li+0.5
DV.tab$ls <- DV.tab$ls-0.5
b.cor <- ifelse(DV.tab$med < 0.5, "lightpink", "#A6CEE3")
DV.tab$nome <- rownames(DV.tab)

DV.tab %>%
  ggplot(aes(y = med - 0.5 , x = fct_reorder(nome, med), fill = 0 > med - 0.5)) +
  geom_bar(show.legend = FALSE,  fill = b.cor) +
  geom_errorbar(aes(y = med-0.5, ymin=li-0.5, ymax=ls-0.5), width=0.4, colour="tomato", alpha=0.9, size=1)+
#  coord_flip()+
  scale_y_continuous(labels =seq(-1, 2, 0.5)) +
  labs(x = 'D-valor', y = '') +
  theme_minimal(base_size = 14) 


