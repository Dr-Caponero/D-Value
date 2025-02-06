library(tidyverse)
library(ggnewscale)

DV_sexo <- read_csv("DV_sexo.csv")
DV_turno <- read_csv("DV_turno.csv")
DV_grau <- read_csv("DV_grau.csv")
DV_modal <- read_csv("DV_modal.csv")
DV_raca <- read_csv("DV_raca.csv")
DV_finan <- read_csv("DV_finan.csv")
DV_social <- read_csv("DV_social.csv")
DV_uf <- read_csv("DV_uf.csv")

fator <- dt(0.975,999)/sqrt(1000)
DV.tab <- c(mean(DV_sexo$D.int), mean(DV_turno$DV_turno), 
            mean(DV_grau$DV_grau), mean(DV_modal$DV_modal),
            mean(DV_raca$DV_raca),mean(DV_finan$DV_finan),
            mean(DV_social$DV_social),mean(DV_uf$DV_uf))
DV.tab <- as.data.frame(DV.tab)
DV.tab <- cbind(DV.tab, c(var(DV_sexo$D.int), var(DV_turno$DV_turno), 
                          var(DV_grau$DV_grau), var(DV_modal$DV_modal),
                          var(DV_raca$DV_raca),var(DV_finan$DV_finan),
                          var(DV_social$DV_social),var(DV_uf$DV_uf)))
colnames(DV.tab) <- c("med", "var")
DV.tab$li <- DV.tab$med - fator * sqrt(DV.tab$var/1000)
DV.tab$ls <- DV.tab$med + fator * sqrt(DV.tab$var/1000)
rownames(DV.tab) <- c("Sexo", "Turno", "Grau Acadêmico", "Modalidade", 
                      "Cor/Raça", "Financiamento", "Auxílio Social", "UF")

DV.tab$nome <- rownames(DV.tab)

b.cor <- ifelse(DV.tab$med < 0.5, "lightpink", "#A6CEE3")
baseline = 0.5
df <- data.frame(inicio = c(0.45000,.56001),
                 fim =    c(0.56000,.60000),
                 Efeito = c("Ínfimo", "Pequeno"),
                 stringsAsFactors = FALSE)
fundo = c("#ff93ff", "#a7a7a7")



DV.tab %>%
  ggplot(aes(x = med -0.5 , y = fct_reorder(nome, med), fill = 0 > med - 0.5)) +
    geom_rect(data = df, aes(NULL,NULL,xmin =inicio-.5, xmax = fim-.5, fill=Efeito,
                           ymin=0, ymax=8.5), alpha = 0.2)+
    scale_fill_manual(values=c("Pequeno" = "#ff93ff", "Ínfimo" = "#666666")) + 
    labs(x = 'D-valor', y = '', fill='Efeito')+
    new_scale("fill")+
    geom_col(aes(x = med -0.5 , y = fct_reorder(nome, med), fill = 0 > med - 0.5),show.legend = FALSE )+
    scale_fill_manual(values=c("#A6CEE3","lightpink","#A6CEE3","#A6CEE3","#A6CEE3",
                             "#A6CEE3","#A6CEE3","#A6CEE3")) +
    geom_text(aes(label =round(med,2), hjust = ifelse(med<0.5, -.2, -.1), 
                vjust = .5))+
    scale_x_continuous(labels = seq(.45,.6,.05)) +
    theme_minimal(base_size = 14) 



#DV.tab = DV.tab[-8,]
DV.tab %>%
  ggplot(aes(x = med -0.5 , y = fct_reorder(nome, med), fill = 0 > med - 0.5)) +
  geom_col(show.legend = FALSE)+
  scale_x_continuous(labels = seq(.40,.60,.05)) +
 geom_text(aes(label =round(med,2), hjust = ifelse(med<0.5, -.2, -.1), 
               vjust = .5))+
  labs(x = 'D-valor', y = '', fill='Efeito')+
  theme_minimal(base_size = 14) 

