# Lendo os pacotes
library(sparklyr)
library(tidyverse)
library(patchwork)



# Conectando o spark
sc <- spark_connect(master = "local")

#dados trabalhados
D1 <- spark_read_parquet(sc, "Aluno_p", "dadosfinal_parquet", memory = FALSE)
D1 |> sdf_dim()

compara = function (refe, dema, n, iteracoes) {
        qR = refe %>% sdf_dim ()
        qD = dema %>% sdf_dim ()
        qR = qR[1]
        qD = qD[1]
        l1 <- n/2
        fR = n*1.1/(2*(qR))
        fD = n*1.1/(2*(qD))
        PV <- NULL
        # amostragem
        for (i in 1:iteracoes){
          Re1 <-  Referencia %>% sdf_sample(fR) %>% collect()
          De1 <- Demais %>% sdf_sample(fD) %>% collect()
          aux <-  wilcox.test(Re1$IndiceCI[1:l1],De1$IndiceCI[1:l1],alternative = "two.sided")
          PV[i] = aux$p.value
        }
        PV <- as.data.frame(PV)
    return(PV)
}


Referencia = D1 %>% filter(TP_SEXO == 1)
Demais = D1 %>% filter(TP_SEXO != 1)
PV_sexo = compara(Referencia,Demais,10000,50)
mean(PV_sexo$PV)

Referencia = D1 %>% filter(TP_TURNO == 3)
Demais = D1 %>% filter(TP_TURNO != 3)
PV_turno = compara(Referencia,Demais,20000,100)
mean(PV_turno$PV)


Referencia = D1 %>% filter(TP_GRAU_ACADEMICO == 3)
Demais = D1 %>% filter(TP_GRAU_ACADEMICO != 3)
PV_grau = compara(Referencia,Demais,10000,50)
mean(PV_grau$PV)


Referencia = D1 %>% filter(TP_MODALIDADE_ENSINO == 1)
Demais = D1 %>% filter(TP_MODALIDADE_ENSINO != 1)
PV_modal = compara(Referencia,Demais,10000,50)
mean(PV_modal$PV)


Referencia = D1 %>% filter(TP_COR_RACA == 1)
Demais = D1 %>% filter(TP_COR_RACA != 1)
PV_raca = compara(Referencia,Demais,10000,50)
mean(PV_raca$PV)

Referencia = D1 %>% filter(IN_FINANCIAMENTO_ESTUDANTIL == 1)
Demais = D1 %>% filter(IN_FINANCIAMENTO_ESTUDANTIL != 1)
PV_finan = compara(Referencia,Demais,20000,100)
mean(PV_finan$PV)

Referencia = D1 %>% filter(IN_APOIO_SOCIAL == 1)
Demais = D1 %>% filter(IN_APOIO_SOCIAL != 1)
PV_social = compara(Referencia,Demais,10000,50)
mean(PV_social$PV)


# Desconectando do Spark
spark_disconnect(sc)







