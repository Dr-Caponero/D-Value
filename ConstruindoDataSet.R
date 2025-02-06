# Criando o banco de dados de trabalho

# Lendo os pacotes
library(sparklyr)
library(sparkedatools)
library(tidyverse)

# Conectando o spark
sc <- spark_connect(master = "local")

# Desconectando do Spark
# spark_disconnect(sc)

# Lendo a tabela de cursos
Cursos <- NULL
Cursos <- read_delim("SUP_CURSO_2019.CSV", "|", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
Cursos = Cursos[, c(2, 6, 9, 18, 34, 35, 36, 37, 38, 39, 40, 41, 42)]
colnames(Cursos) = c("IES", "UF", "CO_CURSO", "CH", "INT", "MAT", "VES", "NOT", "TINT", "TMAT", "TVES", "TNOT", "TEAD")

# disponibilizando os cursos para o spark
cursosSPK <- copy_to(sc, Cursos, "cursosSPK")


# Lendo conjunto de Big Data
dados_csv <- spark_read_csv(sc, "Aluno","SUP_ALUNO_2019.CSV", delim  = "|", memory=FALSE)
spark_write_parquet(dados_csv, "dados_parquet")
Aluno1 <- spark_read_parquet(sc, "Aluno_p", "dados_parquet", memory = FALSE)

# Selecionando campos de interesse dos alunos
Aluno1 = Aluno1 %>%
  select(ID_ALUNO,CO_CURSO, TP_SEXO, QT_CARGA_HORARIA_TOTAL,
         QT_CARGA_HORARIA_INTEG,DT_INGRESSO_CURSO,
         CO_IES, TP_TURNO, TP_GRAU_ACADEMICO,
         TP_MODALIDADE_ENSINO, TP_COR_RACA, TP_SITUACAO,
         IN_FINANCIAMENTO_ESTUDANTIL, IN_APOIO_SOCIAL)

# unindo os bancos de alunos e cursos
D1 <- inner_join(Aluno1, cursosSPK, by = "CO_CURSO")

# data de ingresso no curso
D1 <- D1 %>% mutate(mes = substr(DT_INGRESSO_CURSO, 3, 5))
D1 <- D1 %>% mutate(ano = substr(DT_INGRESSO_CURSO, 6, 9))
D1 <- D1 %>% mutate(mes = ifelse(mes=="JAN",1,2))
D1 <- D1 %>% mutate(ano = as.numeric(ano))


######### eliminando dados anômalos
# carga horária de curso fora dos limites usuais
D1 <- D1 %>% filter(QT_CARGA_HORARIA_TOTAL <= 5200)
D1 <- D1 %>% filter(QT_CARGA_HORARIA_TOTAL >= 1200)
# carga horaria cumprida superior ao máximo
D1 <- D1 %>% filter(QT_CARGA_HORARIA_INTEG<=5200)
#alunos que ingressaram a mais de ~10 anos
D1 <- D1 %>% filter(ano>=2010)
# Desprezando os não ativos
D1 <- D1 %>% filter(TP_SITUACAO == 2)


#determinando o número de alunos do dataset e a % restante
da = D1 %>% sdf_dim ()
dc = D1 %>% group_by(CO_CURSO) %>% summarize () %>% sdf_dim ()
r.estudantes <- da2/da
r.cursos <- dc2/dc


# Calculando a Taxa de integralização anual
D1 <- D1 %>% mutate(alfa = (QT_CARGA_HORARIA_INTEG /
                   (2020 - ano - 0.5*(as.numeric(mes)-1))))

#Definindo o tempo de curso
aux1 <- D1 %>% filter(TP_TURNO == 1) %>% mutate(TEMPO = TMAT)
aux2 <- D1 %>% filter(TP_TURNO == 2) %>% mutate(TEMPO = TVES)
aux3 <- D1 %>% filter(TP_TURNO == 3) %>% mutate(TEMPO = TNOT)
aux4 <- D1 %>% filter(TP_TURNO == 4) %>% mutate(TEMPO = TINT)
aux5 <- D1 %>% filter(is.na(TP_TURNO)) %>% mutate(TEMPO = TEAD)
D1 <- sdf_bind_rows(aux1, aux2, aux3, aux4, aux5)

#calculando o indice de integralização
D1 <- D1 %>% mutate(IndiceCI = (alfa/(QT_CARGA_HORARIA_TOTAL/TEMPO)))


# gravando versão intermediária
D2 <- D1 |> 
  select(QT_CARGA_HORARIA_TOTAL, QT_CARGA_HORARIA_INTEG,
           TEMPO, mes, ano)
spark_write_parquet(D2, "dadosinter_parquet")
D2 <- NULL

# eliminando campos desnecessários
D1 <- D1 %>% select(ID_ALUNO, CO_CURSO, TP_SEXO, CO_IES, UF,
                    TP_TURNO, TP_GRAU_ACADEMICO, TP_MODALIDADE_ENSINO,
                    TP_COR_RACA, IN_FINANCIAMENTO_ESTUDANTIL, IN_APOIO_SOCIAL,
                    alfa,IndiceCI, ano, mes)
D1 <- D1 %>% mutate(entrada=ano+0.5*(mes-1))

# Desprezando os recém ingressos
D1 <- D1 %>% filter(entrada<2019.5)

# eliminando campos desnecessários
D1 <- D1 %>% select(ID_ALUNO, CO_CURSO, TP_SEXO, CO_IES, UF,
                    TP_TURNO, TP_GRAU_ACADEMICO, TP_MODALIDADE_ENSINO,
                    TP_COR_RACA, IN_FINANCIAMENTO_ESTUDANTIL, IN_APOIO_SOCIAL,
                    IndiceCI)

# gravando versão final
spark_write_parquet(D1, "dadosfinal_parquet")
# D1 <- spark_read_parquet(sc, "Aluno_p", "dadosfinal_parquet", memory = FALSE)

# Desconectando do Spark
spark_disconnect(sc)
