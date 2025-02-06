# Definindo o diretório
# setwd("~/Estatística/2021.1/Big data/Trabalhos/microdados_educacao_superior_2019")

# Lendo os pacotes
library(sparklyr)
library(tidyverse)
library(readr)
library(sparkedatools)
library(rgdal) 
library(RColorBrewer)
library(leaflet)

# Conectando o spark
sc <- spark_connect(master = "local")

#lendo o dataset
D1 <- spark_read_parquet(sc, "Aluno_p", "dadosfinal_parquet", memory = FALSE)

# Agrupando por IES e vinculado a média da Taxa de integralização anual

Tic_IES <- D1 %>%
  group_by(CO_IES) %>%
  summarize(I_med = mean(IndiceCI, na.rm = TRUE),
            n=n_distinct(ID_ALUNO)) %>% collect()

# Desconectando do Spark
spark_disconnect(sc)

# Lendo os dados das IES
D2 <- read_delim("SUP_IES_2019.CSV", "|", escape_double = FALSE, trim_ws = TRUE)

# Selecionando campos de interesse
D2 = D2 %>% select (CO_IES, CO_UF)
D2$CO_IES = as.integer(D2$CO_IES)
D2 = as.data.frame(D2)

# Unindo as tabelas pelo código das IES
Dados <- merge(Tic_IES, D2, by = "CO_IES", all = TRUE, sort = FALSE)

#sumarizando os dodos por estado da federação
Resultado <- Dados %>%
  group_by(CO_UF) %>%
  summarize(I_med = mean(I_med, na.rm = TRUE))

Resultado$I_med = round(Resultado$I_med,2)

# Lendo o shapefile do mapa do Brasil fornecido pelo IBGE
mapabr = readOGR("Mapa/.", stringsAsFactors=FALSE, encoding="UTF-8")

# Lendo uma tabela com os códigos do IBGE para os estados da federação
ibge = read.csv("estados.csv", header=T, sep=";")
ibge = ibge[,-3]
colnames(ibge) = c("UF", "Nome UF")
taxa = merge(ibge, Resultado, by.x = "UF", by.y = "CO_UF")
# Associando a alíquota ao mapa do Brasil

mapa <- merge(mapabr,taxa, by.x = "CD_UF", by.y = "UF")

# Ajustando a formatação dos dados do shapefile
proj4string(mapa) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
Encoding(mapa$NM_UF) <- "UTF-8"


# Definindo uma paleta de cores para mapa de calor
cores <- colorBin("Blues",domain = mapa$I_med ,n=5) 

# Criando pop-ups interativos com valor da taxa
estado_popup <- paste0("<strong>Estado: </strong>", 
                       mapa$NM_UF, 
                       "<br><strong>Taxa de Integralização: </strong>", 
                       mapa$I_med)

# Desenhando o mapa do Brasil com mapa de calor definido pela taxa
leaflet(data = mapa) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~cores(I_med), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = estado_popup) %>%
  addLegend("bottomright", values = ~I_med, pal=cores,
            title = "Índice de Desempenho",
            opacity = 1)



