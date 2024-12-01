library(tidyverse)
library(grid)
library(writexl)
library(goftest)
library(nortest)



#Definir semente para manter a reprodutibilidade do processo. 
set.seed(123)

# Amostras
lucas <- read_csv("amostra_222025665.csv")
erica <- read_csv("amostra_222015284.csv")
andre <- read_csv("amostra_190084235.csv")


#juntar as amostras (No caso esse agora é a nossa população)

saeb <- bind_rows(andre,
                  erica, 
                  lucas
)

saeb <- na.omit(saeb)


#Calculando os valores verdadeiros da amostra:


#proproção de nascidos antes ou em 2001:

proporcao_verdade_2001 <- round((sum(saeb$ANO_NASC <= 2001, na.rm = TRUE)/nrow(saeb)),4)

#proporção de femininos:

proporção_verdade_feminino <- round((sum(saeb$SEXO == "Feminino", na.rm = T)/nrow(saeb)),4)

# media notas lp:

media_verdadeira_notaMT <- round(mean(saeb$NOTA_MT),2)

#media nota mt:

media_verdadeira_notaMT <- round(mean(saeb$NOTA_MT),2)

# A partir de sua amostra de dados do SAEB 9o. ano, 

# A) extraia 50 amostras aleatórias de tamanho 15, 

# lapply aplica a função sample_n 50 vezes
amostra_15 <- lapply(1:50, function(x) sample_n(saeb, 15))


# B) outras 50 amostras aleatórias de tamanho 200 (total 100 amostras).

amostra_200 <- lapply(1:50, function(x) sample_n(saeb,200))


# Para cada amostra construa um IC 95% para:
  
# Proporção de alunos que nasceram em 2001 ou antes
# Proporção de alunas (sexo feminino)

#para a proporção vamos usar o prop.test

#Criando uma função que vai calcular o intervalo de confiança de todas as amostras, baseado no parametro
# Função para calcular o IC de proporção
calcular_IC_Proporcao <- function(amostra, parametro){
  
  # Verificando a proporção de alunos nascidos em 2001 ou antes
  if (parametro == "proporcao_ano"){
    # Calcular o número de alunos nascidos em 2001 ou antes
    n_sucessos <- sum(amostra$ANO_NASC <= 2001)
    
    # Se houver sucessos e a amostra não for composta totalmente por um dos grupos
    if (n_sucessos > 0 & n_sucessos < nrow(amostra)) {
      resultado <- prop.test(n_sucessos, n = nrow(amostra), conf.level = 0.95)
      # Verificando se o resultado tem o componente conf.int
      if (is.list(resultado) && !is.null(resultado$conf.int)) {
        return(resultado$conf.int)
      } else {
        return(c(NA, NA))  # Caso não tenha intervalo de confiança, retorna NA
      }
    } else {
      # Caso não haja eventos suficientes (zero ou todos), retorna NA
      return(c(NA, NA))
    }
    
  } else if(parametro == "proporcao_feminino"){
    # Calcular o número de alunas
    n_sucessos <- sum(amostra$SEXO == "Feminino")
    
    # Se houver sucessos e a amostra não for composta totalmente por um dos grupos
    if (n_sucessos > 0 & n_sucessos < nrow(amostra)) {
      resultado <- prop.test(n_sucessos, n = nrow(amostra), conf.level = 0.95)
      # Verificando se o resultado tem o componente conf.int
      if (is.list(resultado) && !is.null(resultado$conf.int)) {
        return(resultado$conf.int)
      } else {
        return(c(NA, NA))  # Caso não tenha intervalo de confiança, retorna NA
      }
    } else {
      # Caso não haja eventos suficientes (zero ou todos), retorna NA
      return(c(NA, NA))
    }
  }
}



ic_proporcao_ano_15 <- sapply(amostra_15, calcular_IC_Proporcao, parametro = "proporcao_ano")
ic_proporcao_ano_200 <- sapply(amostra_200, calcular_IC_Proporcao, parametro = "proporcao_ano")

ic_proporcao_feminino_15 <- sapply(amostra_15, calcular_IC_Proporcao, parametro = "proporcao_feminino")
ic_proporcao_feminino_200 <- sapply(amostra_200, calcular_IC_Proporcao, parametro = "proporcao_feminino")




# Média NOTA_LP
# Média NOTA_MT

# Função para calcular o IC para média
calcular_IC_media <- function(amostra, parametro) {
  if (parametro == "media_LP") {
    if (!is.null(amostra$NOTA_LP) && length(amostra$NOTA_LP) > 0) {
      media <- mean(amostra$NOTA_LP, na.rm = TRUE)
      ic <- t.test(amostra$NOTA_LP)$conf.int
    } else {
      return(c(NA, NA))  # Retorna NA caso a coluna esteja vazia ou ausente
    }
  } else if (parametro == "media_MT") {
    if (!is.null(amostra$NOTA_MT) && length(amostra$NOTA_MT) > 0) {
      media <- mean(amostra$NOTA_MT, na.rm = TRUE)
      ic <- t.test(amostra$NOTA_MT)$conf.int
    } else {
      return(c(NA, NA))  # Retorna NA caso a coluna esteja vazia ou ausente
    }
  }
  return(ic)
}

# Aplicar a função para amostras de tamanho 15 e 200
ic_media_LP_15 <- sapply(amostra_15, calcular_IC_media, parametro = "media_LP")
ic_media_LP_200 <- sapply(amostra_200, calcular_IC_media, parametro = "media_LP")

ic_media_MT_15 <- sapply(amostra_15, calcular_IC_media, parametro = "media_MT")
ic_media_MT_200 <- sapply(amostra_200, calcular_IC_media, parametro = "media_MT")



#Apresente graficamente esses IC 95% (agrupados por parâmetro e tamanho da amostra - total 8 gráficos).

# Organizar os resultados em um formato adequado para o ggplot
resultados_15 <- data.frame(
  amostra = rep(1:50, each = 1),
  IC_inferior = c(ic_proporcao_ano_15[1,], ic_proporcao_feminino_15[1,], 
                  ic_media_LP_15[1,], ic_media_MT_15[1,]),
  IC_superior = c(ic_proporcao_ano_15[2,], ic_proporcao_feminino_15[2,], 
                  ic_media_LP_15[2,], ic_media_MT_15[2,]),
  parametro = rep(c("Proporção Ano <= 2001", "Proporção Feminino", "Média Nota_LP", "Média Nota_MT"), each = 50)
)


write_xlsx(resultados_15, "resultados_15.xlsx")


resultados_200 <- data.frame(
  amostra = rep(1:50, each = 1),
  IC_inferior = c(ic_proporcao_ano_200[1,], ic_proporcao_feminino_200[1,], 
                  ic_media_LP_200[1,], ic_media_MT_200[1,]),
  IC_superior = c(ic_proporcao_ano_200[2,], ic_proporcao_feminino_200[2,], 
                  ic_media_LP_200[2,], ic_media_MT_200[2,]),
  parametro = rep(c("Proporção Ano <= 2001", "Proporção Feminino", "Média Nota_LP", "Média Nota_MT"), each = 50)
)

write_xlsx(resultados_200, "resultados_200.xlsx")


#Proporção dos Nascidos em 2001 ou Antes"

resultados_15 %>% 
  filter(parametro == "Proporção Ano <= 2001") %>%
  ggplot(aes(x = amostra, 
             y= proporcao_verdade_2001, 
             ymin = IC_inferior, 
             ymax = IC_superior, 
  )
  ) +
  geom_pointrange(size = 0.1, color = "blue")+
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(size = 9),
    plot.subtitle = element_text(size = 7)
  )+
  labs(
    x = "Amostra",
    title = "Intervalo de Confiança para Proporção dos Nascidos em 2001 ou Antes",
    subtitle = "50 Amostras com tamanho 15 "
  )+
  # annotation_custom(
  #   grob = rectGrob(gp = gpar(fill = "lightblue", alpha = 0.5)), 
  #   xmin = 10, xmax = 15, ymin = 0.5, ymax = 0.8) +  # Define a posição e o tamanho do quadro
  annotation_custom(
    grob = textGrob(paste("Parâmetro:", proporcao_verdade_2001), gp = gpar(col = "black", fontsize = 8)), 
    xmin = 10, xmax = 77, ymin = 0.8, ymax = 0.7)  # Posição do texto dentro do quadro

ggsave("proporção_amostra15_ano.png")



#Proporção feminino
resultados_15 %>% 
  filter(parametro == "Proporção Feminino") %>%
  ggplot(aes(x = amostra, 
             y= proporção_verdade_feminino, 
             ymin = IC_inferior, 
             ymax = IC_superior, 
             )
         ) +
  geom_pointrange(size = 0.1, color = "blue")+
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 8)
  )+
  labs(
    x = "Amostra",
    title = "Intervalo de Confiança para Proporção do Sexo Feminino",
    subtitle = "50 Amostras com tamanho 15 "
  )+
  # annotation_custom(
  #   grob = rectGrob(gp = gpar(fill = "lightblue", alpha = 0.5)), 
  #   xmin = 10, xmax = 15, ymin = 0.5, ymax = 0.8) +  # Define a posição e o tamanho do quadro
  annotation_custom(
    grob = textGrob(paste("Parâmetro:", proporção_verdade_feminino), gp = gpar(col = "black", fontsize = 8)), 
    xmin = 10, xmax = 77, ymin = 1.3, ymax = 0.7)  # Posição do texto dentro do quadro

ggsave("proporção_amostra15_feminino.png")


# Média Nota_LP

resultados_15 %>% 
  filter(parametro == "Média Nota_LP") %>%
  ggplot(aes(x = amostra, 
             y= media_verdadeira_notaLP, 
             ymin = IC_inferior, 
             ymax = IC_superior, 
  )
  ) +
  geom_pointrange(size = 0.1, color = "blue")+
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 9)
  )+
  labs(
    x = "Amostra",
    title = "Intervalo de Confiança para Média das Notas em Língua Portuguesa",
    subtitle = "50 Amostras com tamanho 15 "
  )+
  # annotation_custom(
  #   grob = rectGrob(gp = gpar(fill = "lightblue", alpha = 0.5)), 
  #   xmin = 10, xmax = 15, ymin = 0.5, ymax = 0.8) +  # Define a posição e o tamanho do quadro
  annotation_custom(
    grob = textGrob(paste("Parâmetro:", media_verdadeira_notaLP), gp = gpar(col = "black", fontsize = 8)), 
    xmin = 10, xmax = 80, ymin = 270, ymax = 365)  # Posição do texto dentro do quadro

ggsave("media_amostra15_notaLP.png")

# Média Nota_LP

resultados_15 %>% 
  filter(parametro == "Média Nota_MT") %>%
  ggplot(aes(x = amostra, 
             y= media_verdadeira_notaMT, 
             ymin = IC_inferior, 
             ymax = IC_superior, 
  )
  ) +
  geom_pointrange(size = 0.1, color = "blue")+
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 9)
  )+
  labs(
    x = "Amostra",
    title = "Intervalo de Confiança para Média das Notas em Matemática",
    subtitle = "50 Amostras com tamanho 15 "
  )+
  # annotation_custom(
  #   grob = rectGrob(gp = gpar(fill = "lightblue", alpha = 0.5)), 
  #   xmin = 10, xmax = 15, ymin = 0.5, ymax = 0.8) +  # Define a posição e o tamanho do quadro
  annotation_custom(
    grob = textGrob(paste("Parâmetro:", media_verdadeira_notaMT), gp = gpar(col = "black", fontsize = 8)), 
    10, xmax = 82, ymin = 250, ymax = 375)  # Posição do texto dentro do quadro

ggsave("media_amostra15_notaMT.png")


# Gráficos de IC para amostras de tamanho 200

#Proporção dos Nascidos em 2001 ou Antes"

resultados_200 %>% 
  filter(parametro == "Proporção Ano <= 2001") %>%
  ggplot(aes(x = amostra, 
             y= proporcao_verdade_2001, 
             ymin = IC_inferior, 
             ymax = IC_superior, 
  )
  ) +
  geom_pointrange(size = 0.1, color = "tomato3")+
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(size = 9),
    plot.subtitle = element_text(size = 7)
  )+
  labs(
    x = "Amostra",
    title = "Intervalo de Confiança para Proporção dos Nascidos em 2001 ou Antes",
    subtitle = "50 Amostras com tamanho 200 "
  )+
  # annotation_custom(
  #   grob = rectGrob(gp = gpar(fill = "lightblue", alpha = 0.5)), 
  #   xmin = 10, xmax = 15, ymin = 0.5, ymax = 0.8) +  # Define a posição e o tamanho do quadro
  annotation_custom(
    grob = textGrob(paste("Parâmetro:", proporcao_verdade_2001), gp = gpar(col = "black", fontsize = 8)), 
    xmin = 10, xmax = 83, ymin = 0.15, ymax = 0.52)  # Posição do texto dentro do quadro

ggsave("proporção_amostra200_ano.png")


#Proporção feminino
resultados_200 %>% 
  filter(parametro == "Proporção Feminino") %>%
  ggplot(aes(x = amostra, 
             y= proporção_verdade_feminino, 
             ymin = IC_inferior, 
             ymax = IC_superior, 
  )
  ) +
  geom_pointrange(size = 0.1, color = "tomato3")+
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 8)
  )+
  labs(
    x = "Amostra",
    title = "Intervalo de Confiança para Proporção do Sexo Feminino",
    subtitle = "50 Amostras com tamanho 200 "
  )+
  # annotation_custom(
  #   grob = rectGrob(gp = gpar(fill = "lightblue", alpha = 0.5)), 
  #   xmin = 10, xmax = 15, ymin = 0.5, ymax = 0.8) +  # Define a posição e o tamanho do quadro
  annotation_custom(
    grob = textGrob(paste("Parâmetro:", proporção_verdade_feminino), gp = gpar(col = "black", fontsize = 8)), 
    xmin = 10, xmax = 83, ymin = 0.6, ymax = 0.8)  # Posição do texto dentro do quadro

ggsave("proporção_amostra200_feminino.png")


# Média Nota_LP

resultados_200 %>% 
  filter(parametro == "Média Nota_LP") %>%
  ggplot(aes(x = amostra, 
             y= media_verdadeira_notaLP, 
             ymin = IC_inferior, 
             ymax = IC_superior, 
  )
  ) +
  geom_pointrange(size = 0.1, color = "tomato3")+
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 9)
  )+
  labs(
    x = "Amostra",
    title = "Intervalo de Confiança para Média das Notas em Língua Portuguesa",
    subtitle = "50 Amostras com tamanho 200 "
  )+
  # annotation_custom(
  #   grob = rectGrob(gp = gpar(fill = "lightblue", alpha = 0.5)), 
  #   xmin = 10, xmax = 15, ymin = 0.5, ymax = 0.8) +  # Define a posição e o tamanho do quadro
  annotation_custom(
    grob = textGrob(paste("Parâmetro:", media_verdadeira_notaLP), gp = gpar(col = "black", fontsize = 8)), 
    10, xmax = 83, ymin = 250, ymax = 286)  # Posição do texto dentro do quadro

ggsave("media_amostra200_notaLP.png")

# Média Nota_LP

resultados_200 %>% 
  filter(parametro == "Média Nota_MT") %>%
  ggplot(aes(x = amostra, 
             y= media_verdadeira_notaMT, 
             ymin = IC_inferior, 
             ymax = IC_superior, 
  )
  ) +
  geom_pointrange(size = 0.1, color = "tomato3")+
  theme_classic()+
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 9)
  )+
  labs(
    x = "Amostra",
    title = "Intervalo de Confiança para Média das Notas em Matemática",
    subtitle = "50 Amostras com tamanho 200 "
  )+
  # annotation_custom(
  #   grob = rectGrob(gp = gpar(fill = "lightblue", alpha = 0.5)), 
  #   xmin = 10, xmax = 15, ymin = 0.5, ymax = 0.8) +  # Define a posição e o tamanho do quadro
  annotation_custom(
    grob = textGrob(paste("Parâmetro:", media_verdadeira_notaMT), gp = gpar(col = "black", fontsize = 8)), 
    10, xmax = 83, ymin = 250, ymax = 286) # Posição do texto dentro do quadro

ggsave("media_amostra200_notaMT.png")




#------------------------------------------------------------------------------#
#                         Teste de Aderencia                                   #
#------------------------------------------------------------------------------#

#Escolha duas subamostras geradas na atividade 2.2 (uma com n= 15 e a outra com n=200):
set.seed(123)

#Selecionando uma subamostra


#Escolhendo um indice aleatório 
random_indice <- sample(seq_along(amostra_15), 1) #(Estamos trabalhando com a amostra número 12)


# n = 15
subAmostra_n15 <- amostra_15[[random_indice]] 

write.table(subAmostra_n15, file='subAmostra_n15.xlsx', sep=';', dec=',', row.names=FALSE)


# n = 20 
subAmostra_n200 <- amostra_200[[random_indice]]

write.table(subAmostra_n200, file='subAmostra_n200.csv', sep=';', dec=',', row.names=FALSE)



# 1) Para a amostra com n=200, construa uma distribuição de frequências para uma das variáveis 
# nota em língua portuguesa ou em matemática (como na Atividade 1.3; escolher uma das variáveis) e 
# verifique se os dados podem ser descritos pela distribuição Normal.



#-----------------------------------------------------n200---------------------------------------------


#Histograma Nota Lingua Portuguesa

subAmostra_n200 %>% 
  ggplot(aes(x = NOTA_LP)) +
  geom_histogram(bins = 10, color = "white", fill = "#88B4E7")+
  geom_density(aes(y = ..count..), stat = "bin", bins = 10, color = "#222222", size = 1)+
  theme_classic()+
  #scale_x_continuous(breaks = lp_intervalos)+
  #scale_y_continuous(breaks = seq(from = 0,to = 400,by = 50))+
  xlab("Nota Língua Portuguesa")+
  ylab("Frequência")+
  scale_x_continuous(breaks = seq(100,400, 20 ), limits = c(100, 400))

ggsave("hist-uni-lp-n200.png", width = 158, height = 93, units = "mm")

#Definindo os intervalos automaticamente 
lp_intervalos_n200 = pretty(subAmostra_n200$NOTA_LP, n = 12)

# Dividindo os dados e calculando suas frequencias
freq_intervalos_lp_n200 <- cut(subAmostra_n200$NOTA_LP, 
                          breaks = lp_intervalos,
                          right = T)

#Formatando como tabela
tabela_freq_lp_n200 <- as.data.frame(table(freq_intervalos_lp_n200))

#Exportando para csv
write.table(tabela_freq_lp_n200, file='Intervalos_Frequencia_lp_n200.csv', sep=';', dec=',', row.names=FALSE)


#média = 252,58
lp_media_n200 <- round(mean(subAmostra_n200$NOTA_LP),2)

#Desvio Padrão = 46,52
lp_desviop_n200 <- round(sd(subAmostra_n200$NOTA_LP),2)

#Q1 = 219.32
lp_q1_n200 <- quantile(subAmostra_n200$NOTA_LP, probs = 0.25)

#Q2 = 254.31
lp_mediana_n200 <- round(median(subAmostra_n200$NOTA_LP),2)

#Q2 = 286.35
lp_q2_n200 <- quantile(subAmostra_n200$NOTA_LP, probs = 0.75)

#Minimo da nota de LP
min_lp_n200 <- round(min(subAmostra_n200$NOTA_LP),2)

#Max da nota LP
max_lp_n200 <- round(max(subAmostra_n200$NOTA_LP),2)

#Coeficiente de variação = 48,52%
lp_cv_n200 <- (lp_desviop_n200/lp_media_n200)*100


#Para calcular assimetria usei as formulas do pacote moments (https://cran.r-project.org/web/packages/moments/moments.pdf)


#Assimetria = -0,101
# Se a assimetria estiver entre -0,5 e 0,5, os dados são bastante simétricos (distribuição normal).
# Se a distorção estiver entre -1 e -0,5 (distorção negativa) ou entre 0,5 e 1 (distorção positiva), os dados estão moderadamente distorcidos.
# Se a distorção for menor que -1 (distorção negativa) ou maior que 1 (distorção positiva), os dados estão altamente distorcidos.

lp_assimetria_n200 <- round(moments::skewness(subAmostra_n200$NOTA_LP, na.rm = FALSE),3)

#Curtose = 2,48

# C < 0, 263, a curva ou distribui¸c˜ao ´e leptoc´urtica (mais afilada);
# C = 0, 263, a curva ou distribui¸c˜ao ´e mesoc´urtica;
# C > 0, 263, a curva ou distribui¸c˜ao ´e platic´urtica (mais achatada).

lp_curtose_n200 <- round(moments::kurtosis(subAmostra_n200$NOTA_LP, na.rm = FALSE),3)


medidas_resumo_lp_n200 <- data.frame(Media = lp_media_n200, 
                                Desvio_Padrao = lp_desviop_n200,
                                Quantil_1 = lp_q1_n200,
                                Mediana = lp_mediana_n200,
                                Quantil_2 = lp_q2_n200,
                                Minimo = min_lp_n200,
                                Maximo = max_lp_n200,
                                Coef_Variacao = lp_cv_n200,
                                Assimetria = lp_assimetria_n200,
                                Curtose = lp_curtose_n200
                                )
rownames(medidas_resumo_lp_n200) <- "1"


#Exportando para csv
write.table(medidas_resumo_lp_n200, file='Medidas_Resumo_lp_n200.csv', sep=';', dec=',', row.names=FALSE)

#Histograma Nota Matemática

subAmostra_n200 %>% 
  ggplot(aes(x = NOTA_MT)) +
  geom_histogram(bins = 10, color = "white", fill = "#88B4E7")+
  geom_density(aes(y = ..count..), stat = "bin", bins = 10, color = "#222222", size = 1)+
  theme_classic()+
  #scale_x_continuous(breaks = lp_intervalos)+
  #scale_y_continuous(breaks = seq(from = 0,to = 400,by = 50))+
  xlab("Nota Matemática")+
  ylab("Frequência")+
  scale_x_continuous(breaks = seq(100,400, 20 ), limits = c(100, 400))

ggsave("hist-uni-mt-n200.png", width = 158, height = 93, units = "mm")

#Definindo os intervalos automaticamente 
mt_intervalos_n200 = pretty(subAmostra_n200$NOTA_MT, n = 12)

# Dividindo os dados e calculando suas frequencias
freq_intervalos_mt_n200 <- cut(subAmostra_n200$NOTA_MT, 
                          breaks = mt_intervalos,
                          right = T)

#Formatando como tabela
tabela_freq_mt_n200 <- as.data.frame(table(freq_intervalos_mt_n200))

#Exportando para csv
write.table(tabela_freq_mt_n200, file='Intervalos_Frequencia_mt_n200.csv', sep=';', dec=',', row.names=FALSE)


#média = 252,58
mt_media_n200 <- round(mean(subAmostra_n200$NOTA_MT),2)

#Desvio Padrão = 46,52
mt_desviop_n200 <- round(sd(subAmostra_n200$NOTA_MT),2)

#Q1 = 219.32
mt_q1_n200 <- quantile(subAmostra_n200$NOTA_MT, probs = 0.25)

#Q2 = 254.31
mt_mediana_n200 <- round(median(subAmostra_n200$NOTA_MT),2)

#Q2 = 286.35
mt_q2_n200 <- quantile(subAmostra_n200$NOTA_MT, probs = 0.75)

#Minimo da nota de MT
min_mt_n200 <- round(min(subAmostra_n200$NOTA_MT),2)

#Max da nota MT
max_mt_n200 <- round(max(subAmostra_n200$NOTA_MT),2)

#Coeficiente de variação = 48,52%
mt_cv_n200 <- (mt_desviop/mt_media)*100


#Para calcular assimetria usei as formulas do pacote moments (https://cran.r-project.org/web/packages/moments/moments.pdf)


#Assimetria = -0,101
# Se a assimetria estiver entre -0,5 e 0,5, os dados são bastante simétricos (distribuição normal).
# Se a distorção estiver entre -1 e -0,5 (distorção negativa) ou entre 0,5 e 1 (distorção positiva), os dados estão moderadamente distorcidos.
# Se a distorção for menor que -1 (distorção negativa) ou maior que 1 (distorção positiva), os dados estão altamente distorcidos.

mt_assimetria_n200 <- round(moments::skewness(subAmostra_n200$NOTA_MT, na.rm = FALSE),3)

#Curtose = 2,48

# C < 0, 263, a curva ou distribui¸c˜ao ´e leptoc´urtica (mais afilada);
# C = 0, 263, a curva ou distribui¸c˜ao ´e mesoc´urtica;
# C > 0, 263, a curva ou distribui¸c˜ao ´e platic´urtica (mais achatada).

mt_curtose_n200 <- round(moments::kurtosis(subAmostra_n200$NOTA_MT, na.rm = FALSE),3)


medidas_resumo_mt_n200 <- data.frame(Media = mt_media_n200, 
                                Desvio_Padrao = mt_desviop_n200,
                                Quantil_1 = mt_q1_n200,
                                Mediana = mt_mediana_n200,
                                Quantil_2 = mt_q2_n200,
                                Minimo = min_mt_n200,
                                Maximo = max_mt_n200,
                                Coef_Variacao = mt_cv_n200,
                                Assimetria = mt_assimetria_n200,
                                Curtose = mt_curtose_n200
)
rownames(medidas_resumo_mt_n200) <- "1"


#Exportando para csv

write.table(medidas_resumo_mt_n200, file='Medidas_Resumo_mt_n200.csv', sep=';', dec=',', row.names=FALSE)


#-------------------------------------------------n15--------------------------------------------------

#Histograma Nota Lingua Portuguesa

subAmostra_n15 %>% 
  ggplot(aes(x = NOTA_LP)) +
  geom_histogram(bins = 10, color = "white", fill = "#88B4E7")+
  geom_density(aes(y = ..count..), stat = "bin", bins = 10, color = "#222222", size = 1)+
  theme_classic()+
  #scale_x_continuous(breaks = lp_intervalos)+
  #scale_y_continuous(breaks = seq(from = 0,to = 400,by = 50))+
  xlab("Nota Língua Portuguesa")+
  ylab("Frequência")+
  scale_x_continuous(breaks = seq(100,400, 20 ), limits = c(100, 400))

ggsave("hist-uni-lp-n15.png", width = 158, height = 93, units = "mm")

#Definindo os intervalos automaticamente 
lp_intervalos_n15 = pretty(subAmostra_n15$NOTA_LP, n = 12)

# Dividindo os dados e calculando suas frequencias
freq_intervalos_lp_n15 <- cut(subAmostra_n15$NOTA_LP, 
                               breaks = lp_intervalos,
                               right = T)

#Formatando como tabela
tabela_freq_lp_n15 <- as.data.frame(table(freq_intervalos_lp_n15))

#Exportando para csv
write.table(tabela_freq_lp_n15, file='Intervalos_Frequencia_lp_n15.csv', sep=';', dec=',', row.names=FALSE)


#média = 252,58
lp_media_n15 <- round(mean(subAmostra_n15$NOTA_LP),2)

#Desvio Padrão = 46,52
lp_desviop_n15 <- round(sd(subAmostra_n15$NOTA_LP),2)

#Q1 = 219.32
lp_q1_n15 <- quantile(subAmostra_n15$NOTA_LP, probs = 0.25)

#Q2 = 254.31
lp_mediana_n15 <- round(median(subAmostra_n15$NOTA_LP),2)

#Q2 = 286.35
lp_q2_n15 <- quantile(subAmostra_n15$NOTA_LP, probs = 0.75)

#Minimo da nota de LP
min_lp_n15 <- round(min(subAmostra_n15$NOTA_LP),2)

#Max da nota LP
max_lp_n15 <- round(max(subAmostra_n15$NOTA_LP),2)

#Coeficiente de variação = 48,52%
lp_cv_n15 <- (lp_desviop_n15/lp_media_n15)*100


#Para calcular assimetria usei as formulas do pacote moments (https://cran.r-project.org/web/packages/moments/moments.pdf)


#Assimetria = -0,101
# Se a assimetria estiver entre -0,5 e 0,5, os dados são bastante simétricos (distribuição normal).
# Se a distorção estiver entre -1 e -0,5 (distorção negativa) ou entre 0,5 e 1 (distorção positiva), os dados estão moderadamente distorcidos.
# Se a distorção for menor que -1 (distorção negativa) ou maior que 1 (distorção positiva), os dados estão altamente distorcidos.

lp_assimetria_n15 <- round(moments::skewness(subAmostra_n15$NOTA_LP, na.rm = FALSE),3)

#Curtose = 2,48

# C < 0, 263, a curva ou distribui¸c˜ao ´e leptoc´urtica (mais afilada);
# C = 0, 263, a curva ou distribui¸c˜ao ´e mesoc´urtica;
# C > 0, 263, a curva ou distribui¸c˜ao ´e platic´urtica (mais achatada).

lp_curtose_n15 <- round(moments::kurtosis(subAmostra_n15$NOTA_LP, na.rm = FALSE),3)


medidas_resumo_lp_n15 <- data.frame(Media = lp_media_n15, 
                                     Desvio_Padrao = lp_desviop_n15,
                                     Quantil_1 = lp_q1_n15,
                                     Mediana = lp_mediana_n15,
                                     Quantil_2 = lp_q2_n15,
                                     Minimo = min_lp_n15,
                                     Maximo = max_lp_n15,
                                     Coef_Variacao = lp_cv_n15,
                                     Assimetria = lp_assimetria_n15,
                                     Curtose = lp_curtose_n15
)
rownames(medidas_resumo_lp_n15) <- "1"


#Exportando para csv
write.table(medidas_resumo_lp_n15, file='Medidas_Resumo_lp_n15.csv', sep=';', dec=',', row.names=FALSE)

#Histograma Nota Matemática

subAmostra_n15 %>% 
  ggplot(aes(x = NOTA_MT)) +
  geom_histogram(bins = 10, color = "white", fill = "#88B4E7")+
  geom_density(aes(y = ..count..), stat = "bin", bins = 10, color = "#222222", size = 1)+
  theme_classic()+
  #scale_x_continuous(breaks = lp_intervalos)+
  #scale_y_continuous(breaks = seq(from = 0,to = 400,by = 50))+
  xlab("Nota Matemática")+
  ylab("Frequência")+
  scale_x_continuous(breaks = seq(100,400, 20 ), limits = c(100, 400))

ggsave("hist-uni-mt-n15.png", width = 158, height = 93, units = "mm")

#Definindo os intervalos automaticamente 
mt_intervalos_n15 = pretty(subAmostra_n15$NOTA_MT, n = 12)

# Dividindo os dados e calculando suas frequencias
freq_intervalos_mt_n15 <- cut(subAmostra_n15$NOTA_MT, 
                               breaks = mt_intervalos,
                               right = T)

#Formatando como tabela
tabela_freq_mt_n15 <- as.data.frame(table(freq_intervalos_mt_n15))

#Exportando para csv
write.table(tabela_freq_mt_n15, file='Intervalos_Frequencia_mt_n15.csv', sep=';', dec=',', row.names=FALSE)


#média = 252,58
mt_media_n15 <- round(mean(subAmostra_n15$NOTA_MT),2)

#Desvio Padrão = 46,52
mt_desviop_n15 <- round(sd(subAmostra_n15$NOTA_MT),2)

#Q1 = 219.32
mt_q1_n15 <- quantile(subAmostra_n15$NOTA_MT, probs = 0.25)

#Q2 = 254.31
mt_mediana_n15 <- round(median(subAmostra_n15$NOTA_MT),2)

#Q2 = 286.35
mt_q2_n15 <- quantile(subAmostra_n15$NOTA_MT, probs = 0.75)

#Minimo da nota de MT
min_mt_n15 <- round(min(subAmostra_n15$NOTA_MT),2)

#Max da nota MT
max_mt_n15 <- round(max(subAmostra_n15$NOTA_MT),2)

#Coeficiente de variação = 48,52%
mt_cv_n15 <- (mt_desviop/mt_media)*100


#Para calcular assimetria usei as formulas do pacote moments (https://cran.r-project.org/web/packages/moments/moments.pdf)


#Assimetria = -0,101
# Se a assimetria estiver entre -0,5 e 0,5, os dados são bastante simétricos (distribuição normal).
# Se a distorção estiver entre -1 e -0,5 (distorção negativa) ou entre 0,5 e 1 (distorção positiva), os dados estão moderadamente distorcidos.
# Se a distorção for menor que -1 (distorção negativa) ou maior que 1 (distorção positiva), os dados estão altamente distorcidos.

mt_assimetria_n15 <- round(moments::skewness(subAmostra_n15$NOTA_MT, na.rm = FALSE),3)

#Curtose = 2,48

# C < 0, 263, a curva ou distribui¸c˜ao ´e leptoc´urtica (mais afilada);
# C = 0, 263, a curva ou distribui¸c˜ao ´e mesoc´urtica;
# C > 0, 263, a curva ou distribui¸c˜ao ´e platic´urtica (mais achatada).

mt_curtose_n15 <- round(moments::kurtosis(subAmostra_n15$NOTA_MT, na.rm = FALSE),3)


medidas_resumo_mt_n15 <- data.frame(Media = mt_media_n15, 
                                     Desvio_Padrao = mt_desviop_n15,
                                     Quantil_1 = mt_q1_n15,
                                     Mediana = mt_mediana_n15,
                                     Quantil_2 = mt_q2_n15,
                                     Minimo = min_mt_n15,
                                     Maximo = max_mt_n15,
                                     Coef_Variacao = mt_cv_n15,
                                     Assimetria = mt_assimetria_n15,
                                     Curtose = mt_curtose_n15
)
rownames(medidas_resumo_mt_n15) <- "1"


#Exportando para csv

write.table(medidas_resumo_mt_n15, file='Medidas_Resumo_mt_n15.csv', sep=';', dec=',', row.names=FALSE)


# ---------------------------------Exercicio 2------------------------------------------------------

# 2) Teste normalidade das variáveis notas em língua portuguesa e matemática para a amostra (n=15). 
# Apresente os testes de Shapiro-Wilk, Anderson-Darling e Kolmogorov (Lilliefors). 

# subAmostra_n15

#Shapiro-Wilk (Usando o R sabe)

shapiro_result_lp_n15 <- shapiro.test(subAmostra_n15$NOTA_LP) # W = 0.95509, p-value = 0.6078

shapiro_result_mt_n15 <- shapiro.test(subAmostra_n15$NOTA_MT) #W = 0.91053, p-value = 0.138

#Anderson-Darling

ad_result_lp_n15 <- goftest::ad.test(subAmostra_n15$NOTA_LP, "pnorm")
      # RESULTADO:
      # Anderson-Darling test of goodness-of-fit
      # Null hypothesis: Normal distribution
      # Parameters assumed to be fixed
      # 
      # data:  subAmostra_n15$NOTA_LP
      # An = Inf, p-value = 4e-05
      #An é a estatistica do teste, quando assume o valor INF é pq deu um valor MUITO grande de estatística do teste


ad_result_mt_n15 <- goftest::ad.test(subAmostra_n15$NOTA_MT, "norm")
    # RESULTADO
    # Anderson-Darling test of goodness-of-fit
    # Null hypothesis: Normal distribution
    # Parameters assumed to be fixed
    # 
    # data:  subAmostra_n15$NOTA_MT
    # An = Inf, p-value = 4e-05
    #An é a estatistica do teste, quando assume o valor INF é pq deu um valor MUITO grande de estatística do teste


#Kolmogorov (Lilliefors)

ks_lillie_result_lp_n15 <- lillie.test(subAmostra_n15$NOTA_LP)
      # Lilliefors (Kolmogorov-Smirnov) normality test
      # 
      # data:  subAmostra_n15$NOTA_LP
      # D = 0.11792, p-value = 0.8313



ks_lillie_result_mt_n15 <- lillie.test(subAmostra_n15$NOTA_MT)
      # data:  subAmostra_n15$NOTA_MT
      # D = 0.18171, p-value = 0.2015
  


#RESULTADOS LINGUA PORTUGUESA
resultados_LP_n15 <- data.frame(
  teste = c("Shapiro-Wilk", "Anderson-Darling", "Kolmogorov-(Lilliefors)"),
  estatistica = c(shapiro_result_lp_n15$statistic, ad_result_lp_n15$statistic, ks_lillie_result_lp_n15$statistic),
  p_valor = c(shapiro_result_lp_n15$p.value, ad_result_lp_n15$p.value, ks_lillie_result_lp_n15$p.value)
)

rownames(resultados_LP_n15) <- seq_len(nrow(resultados_LP_n15))

write.table(resultados_LP_n15, file='resultados_testes_LP_n15.csv', sep=';', dec=',', row.names=FALSE)


#RESULTADOS MATEMÁTICA
resultados_MT_n15 <- data.frame(
  teste = c("Shapiro-Wilk", "Anderson-Darling", "Kolmogorov-(Lilliefors)"),
  estatistica = c(shapiro_result_mt_n15$statistic, ad_result_mt_n15$statistic, ks_lillie_result_mt_n15$statistic),
  p_valor = c(shapiro_result_mt_n15$p.value, ad_result_mt_n15$p.value, ks_lillie_result_mt_n15$p.value)
)

rownames(resultados_MT_n15) <- seq_len(nrow(resultados_MT_n15))

write.table(resultados_MT_n15, file='resultados_testes_MT_n15.csv', sep=';', dec=',', row.names=FALSE)


# subAmostra_n200

subAmostra_n200

#Shapiro-Wilk (Usando o R sabe)

shapiro_result_lp_n200 <- shapiro.test(subAmostra_n200$NOTA_LP) # W = 0.99071, p-value = 0.2262

shapiro_result_mt_n200 <- shapiro.test(subAmostra_n200$NOTA_MT) # W = 0.99066, p-value = 0.2224

#Anderson-Darling

ad_result_lp_n200 <- goftest::ad.test(subAmostra_n200$NOTA_LP, "norm")
    # RESULTADO:
    # Anderson-Darling test of goodness-of-fit
    # Null hypothesis: Normal distribution
    # Parameters assumed to be fixed
    # 
    # data:  subAmostra_n200$NOTA_LP
    # An = Inf, p-value = 3e-06
    #An é a estatistica do teste, quando assume o valor INF é pq deu um valor MUITO grande de estatística do teste


ad_result_mt_n200 <- goftest::ad.test(subAmostra_n200$NOTA_MT, "norm")
    # RESULTADO
    # Anderson-Darling test of goodness-of-fit
    # Null hypothesis: Normal distribution
    # Parameters assumed to be fixed
    # 
    # data:  subAmostra_n200$NOTA_MT
    # An = Inf, p-value = 3e-06
    #An é a estatistica do teste, quando assume o valor INF é pq deu um valor MUITO grande de estatística do teste


#Kolmogorov (Lilliefors)

ks_lillie_result_lp_n200 <- lillie.test(subAmostra_n200$NOTA_LP)
      # Lilliefors (Kolmogorov-Smirnov) normality test
      # 
      # data:  subAmostra_n200$NOTA_LP
      # D = 0.045171, p-value = 0.4092


ks_lillie_result_mt__n200 <- lillie.test(subAmostra_n200$NOTA_MT)
      # Lilliefors (Kolmogorov-Smirnov) normality test
      # 
      # data:  subAmostra_n200$NOTA_MT
      # D = 0.056827, p-value = 0.119



#RESULTADOS LINGUA PORTUGUESA
resultados_LP_n200 <- data.frame(
  teste = c("Shapiro-Wilk", "Anderson-Darling", "Kolmogorov-(Lilliefors)"),
  estatistica = c(shapiro_result_lp_n200$statistic, ad_result_lp_n200$statistic, ks_lillie_result_lp_n200$statistic),
  p_valor = c(shapiro_result_lp_n200$p.value, ad_result_lp_n200$p.value, ks_lillie_result_lp_n200$p.value)
)

rownames(resultados_LP_n200) <- seq_len(nrow(resultados_LP_n200))

write.table(resultados_LP_n200, file='resultados_testes_LP_n200.csv', sep=';', dec=',', row.names=FALSE)


#RESULTADOS MATEMÁTICA
resultados_MT_n200 <- data.frame(
  teste = c("Shapiro-Wilk", "Anderson-Darling", "Kolmogorov-(Lilliefors)"),
  estatistica = c(shapiro_result_mt_n200$statistic, ad_result_mt_n200$statistic, ks_lillie_result_mt_n200$statistic),
  p_valor = c(shapiro_result_mt_n200$p.value, ad_result_mt_n200$p.value, ks_lillie_result_mt_n200$p.value)
)

rownames(resultados_MT_n200) <- seq_len(nrow(resultados_MT_n200))

write.table(resultados_MT_n200, file='resultados_testes_MT_n200.csv', sep=';', dec=',', row.names=FALSE)


# 3) Comente os resultados obtidos, à luz da análise descritiva realizada com a amostra com 1000 
# observações (Atividade 1.3).

