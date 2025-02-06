library(readr)
dados <- read_delim("Documentos/heatmap.csv", 
                      delim = ";", escape_double = FALSE, col_types = cols(...1 = col_skip(), 
                                                                           ...2 = col_skip()), locale = locale(decimal_mark = ","), 
                      trim_ws = TRUE)
dados = as.matrix(dados)
rownames(dados) = colnames(dados)
dados = apply(dados, MARGIN = c(1,2), function(x) ifelse(x<0.5,1-x,x))
dados = apply(dados, MARGIN = c(1,2), function(x) ifelse(x<0.54,0,ifelse(x<0.64,1,ifelse(x<0.71,2,3))))
paleta = c("#bbbbbb", "#ff93ff", "#e352f7", "#990e8b")
heatmap(dados, Rowv = NA, Colv = NA, symm = TRUE, col = paleta)


