#################################
####   Análise descritiva    ####
#################################
#Pacotes
# Lendo os pacotes
library(patchwork)
library(sparklyr)
library(hrbrthemes)
library(viridis)
library(kableExtra)
library(tidyverse)

# Medidas Resumo ----
summarytools::st_options(lang = "pt")

# Conectando o spark
sc <- spark_connect(master = "local")

#lendo o dataset
D1 <- spark_read_parquet(sc, "Aluno_p", "dadosinter_parquet", memory = FALSE)
D1 <- D1 |> mutate(entrada= ano+0.5*(mes-1))
campos <- c("QT_CARGA_HORARIA_TOTAL", "QT_CARGA_HORARIA_INTEG",
                    "TEMPO", "entrada")


tab <- c(0,0,0,0,0,0,0,0,0,0)
for (i in campos){
  tt <- D1 |>  select(i) |> collect()
  aux <- tt |> summarytools::descr(
      stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv", "Skewness", "Kurtosis"),
      justify = "c",
      transpose = T)
  aux <- as.data.frame(aux)
  tab <- rbind(tab, aux)
}

# Desconectando do Spark
spark_disconnect(sc)

# criando a tabela de medidas de resumo
tab <- tab[-1,]
rownames(tab) <- c("Carga Horária dos Cursos", "Carga Horária dos Integralizada",
                   "Tempo de Integralização", "Ano de Ingresso")
tab |> 
    kbl(
      digits = 2,
      format.args=list(big.mark=".", decimal.mark=","),
      align = "c", row.names = T, booktabs = T
    )|>
    kable_styling(
        full_width = F, position = 'center', 
        latex_options = c("striped", "HOLD_position", "scale_down", "repeat_header")
      )|>
    column_spec(1, bold = T
      )|>
    kable_material()




# Conectando o spark
sc <- spark_connect(master = "local")

#lendo o dataset
D1 <- spark_read_parquet(sc, "Aluno_p", "dadosfinal_parquet", memory = FALSE)
tt <- D1 |>  select(IndiceCI) |> collect()

# Desconectando do Spark
spark_disconnect(sc)



# Plot
tt |> 
  ggplot(aes(y=IndiceCI, x=1)) +
  geom_violin(width=1.4, color="#1F78B4", fill= "#A6CEE3") +
  geom_boxplot(width=0.1, color="tomato", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  scale_x_continuous(labels = "", breaks = 0)+
  labs(y = "Índice de Desempenho", x="")


# 
# g1 <- tt|>
#   ggplot() +
#   aes(x = IndiceCI) +
#   geom_histogram(
#     aes(y = ..density..),
#     bins = 35,
#     fill = "#A6CEE3",
#     colour = "#000099") +
#   geom_density(
#     alpha = 0.2,
#     fill = "#1F78B4",
#     colour = "#1F78B4") +
#   scale_y_continuous(
#     labels = scales::number_format(
#       big.mark = ".",
#       decimal.mark = ","
#     )) +
#   labs(
#     x = "Valor do Índice",
#     y = "Densidade"
#   )+theme_minimal(base_size = 12)
# 
# b1 <- tt|>
#   ggplot(aes(x = IndiceCI)) +
#   geom_boxplot(col="#1F78B4", fill="#A6CEE3", alpha = 0.5)+
#   labs(
#     x = "Valor do Índice",
#     y = "Densidade"
#   )+theme_minimal(base_size = 12)
# 
# g1 + b1 +
#   plot_layout(ncol = 2) +
#   plot_annotation(
#     tag_levels = c("A", "1"), tag_prefix = "(", tag_sep = ".",
#     tag_suffix = ")")&
#   theme(
#     plot.tag.position = c(0.8, 0.8),
#     plot.tag = element_text(size = 14, hjust = 0.5, vjust = 1))
# 
# 

