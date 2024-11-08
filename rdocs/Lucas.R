#Carregar Pacotes
library(tidyverse)
library(moments)
library(xlsReadWrite)

#carregar as amostras
lucas <- read_csv("banco/amostra_222025665.csv")
erica <- read_csv("banco/amostra_222015284.csv")
andre <- read_csv("banco/amostra_190084235.csv")

caminho_lucas <- "resultados/Lucas"

#juntar as amostras

saeb <- bind_rows(andre,
                  erica, 
                  lucas
                  )

#------------------------------------------------------------------------------#
#                             Notas LP                                         #
#------------------------------------------------------------------------------#----


#------------------Distribuição de frequencias----------------------------------

#Minimo da nota de LP
min_lp <- round(min(saeb$NOTA_LP),2)

#Max da nota LP
max_lp <- round(max(saeb$NOTA_LP),2)

#Definindo a quantidade de intervalos


#Definindo os intervalos automaticamente 
lp_intervalos = pretty(saeb$NOTA_LP, n = 10)

# Dividindo os dados e calculando suas frequencias
freq_intervalos_lp <- cut(saeb$NOTA_LP, 
                      breaks = lp_intervalos,
                      right = T)

#Formatando como tabela
tabela_freq_lp <- as.data.frame(table(freq_intervalos_lp))

#Exportando para csv
write.table(tabela_freq_lp, file='Intervalos_Frequencia_lp.csv', sep=';', dec=',', row.names=FALSE)



#------------------------------Histograma---------------------------------------


saeb %>% 
  ggplot(aes(x = NOTA_LP)) +
  geom_histogram(bins = 10, color = "white", fill = "#A11D21")+
  geom_density(aes(y = ..count..), stat = "bin", bins = 10, color = "#222222", size = 1)+
  theme_classic()+
  #scale_x_continuous(breaks = lp_intervalos)+
  #scale_y_continuous(breaks = seq(from = 0,to = 400,by = 50))+
  xlab("Nota Língua Portuguesa")+
  ylab("Frequência")+
  scale_x_continuous(breaks = seq(100,400, 20 ), limits = c(100, 400))


ggsave(filename = file.path(caminho_lucas, "hist-uni-lp.png"), width = 158, height = 93, units = "mm")

#------------------------Medidas de Posição-------------------------------------
#média = 252,58
lp_media <- round(mean(saeb$NOTA_LP),2)

#Desvio Padrão = 46,52
lp_desviop <- round(sd(saeb$NOTA_LP),2)

#Q1 = 219.32
lp_q1 <- quantile(saeb$NOTA_LP, probs = 0.25)

#Q2 = 254.31
lp_mediana <- round(median(saeb$NOTA_LP),2)

#Q2 = 286.35
lp_q2 <- quantile(saeb$NOTA_LP, probs = 0.75)

#Máximo (Calculamos lá em cima) = 374,36
#Mínimo (Calculamos lá em cima) = 131,9

#Coeficiente de variação = 48,52%
lp_cv <- (lp_desviop/lp_media)*100


#Para calcular assimetria usei as formulas do pacote moments (https://cran.r-project.org/web/packages/moments/moments.pdf)


#Assimetria = -0,101
# Se a assimetria estiver entre -0,5 e 0,5, os dados são bastante simétricos (distribuição normal).
# Se a distorção estiver entre -1 e -0,5 (distorção negativa) ou entre 0,5 e 1 (distorção positiva), os dados estão moderadamente distorcidos.
# Se a distorção for menor que -1 (distorção negativa) ou maior que 1 (distorção positiva), os dados estão altamente distorcidos.

lp_assimetria <- round(moments::skewness(saeb$NOTA_LP, na.rm = FALSE),3)

#Curtose = 2,48

# C < 0, 263, a curva ou distribui¸c˜ao ´e leptoc´urtica (mais afilada);
# C = 0, 263, a curva ou distribui¸c˜ao ´e mesoc´urtica;
# C > 0, 263, a curva ou distribui¸c˜ao ´e platic´urtica (mais achatada).

lp_curtose <- round(moments::kurtosis(saeb$NOTA_LP, na.rm = FALSE),3)

#------------------------------BOXPLOT------------------------------------------


ggplot(saeb) +
  aes(
    x = factor(""),
    y = NOTA_LP
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  guides(fill = none) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  theme_bw()+
  labs(x = "", y = "Notas Lingua Portuguesa")+
   scale_y_continuous(breaks = seq(100,400, 50 ), limits = c(100, 400))

ggsave(filename = file.path(caminho_lucas, "boxplot-uni-lp.png"), width = 158, height = 93, units = "mm")


#------------------------------------------------------------------------------#----
#                             Notas MT                                         #
#------------------------------------------------------------------------------#----


#------------------Distribuição de frequencias----------------------------------

#Minimo da nota de MT = 127,56
min_mt <- round(min(saeb$NOTA_MT),2)

#Max da nota MT = 419,78
max_mt <- round(max(saeb$NOTA_MT),2)

#Definindo a quantidade de intervalos


#Definindo os intervalos automaticamente 
mt_intervalos = pretty(saeb$NOTA_MT, n = 10)

# Dividindo os dados e calculando suas frequencias
freq_intervalos_mt <- cut(saeb$NOTA_MT, 
                          breaks = mt_intervalos,
                          right = T)

#Formatando como tabela
tabela_freq_mt <- as.data.frame(table(freq_intervalos_mt))

#Exportando para csv
write.table(tabela_freq_mt, file='Intervalos_Frequencia_mt.csv', sep=';', dec=',', row.names=FALSE)

#------------------------------Histograma---------------------------------------


saeb %>% 
  ggplot(aes(x = NOTA_MT, y = after_stat(density))) +
  geom_histogram(aes(y = ..count..), bins = 10, color = "white", fill = "#A11D21") +
  geom_density(aes(y = ..count..), stat = "bin", bins = 10, color = "#222222", size = 1)+
  theme_classic()+
  scale_x_continuous(breaks = mt_intervalos)+
  #scale_y_continuous(breaks = seq(from = 0,to = max_mt,by = 50))+
  xlab("Nota Matemática")+
  ylab("Frequência")

ggsave(filename = file.path(caminho_lucas, "hist-uni-mt.png"), width = 158, height = 93, units = "mm")


#------------------------Medidas de Posição-------------------------------------
#média = 251,65
mt_media <- round(mean(saeb$NOTA_MT),2)

#Desvio Padrão = 47,91
mt_desviop <- round(sd(saeb$NOTA_MT),2)

#Q1 = 218,82
mt_q1 <- quantile(saeb$NOTA_MT, probs = 0.25)

#Q2 = 249,99
mt_mediana <- round(median(saeb$NOTA_MT),2)

#Q2 = 283,98
mt_q2 <- quantile(saeb$NOTA_MT, probs = 0.75)

#Máximo (Calculamos lá em cima) = 419,78
#Mínimo (Calculamos lá em cima) = 127,56

#Coeficiente de variação = 19,03%
mt_cv <- (mt_desviop/mt_media)*100

#Para calcular assimetria usei as formulas do pacote moments (https://cran.r-project.org/web/packages/moments/moments.pdf)


#Assimetria = 0,143
# Se a assimetria estiver entre -0,5 e 0,5, os dados são bastante simétricos (distribuição normal).
# Se a distorção estiver entre -1 e -0,5 (distorção negativa) ou entre 0,5 e 1 (distorção positiva), os dados estão moderadamente distorcidos.
# Se a distorção for menor que -1 (distorção negativa) ou maior que 1 (distorção positiva), os dados estão altamente distorcidos.

mt_assimetria <- round(moments::skewness(saeb$NOTA_MT, na.rm = FALSE),3)

#Curtose = 2,785

# C < 0, 263, a curva ou distribui¸c˜ao ´e leptoc´urtica (mais afilada);
# C = 0, 263, a curva ou distribui¸c˜ao ´e mesoc´urtica;
# C > 0, 263, a curva ou distribui¸c˜ao ´e platic´urtica (mais achatada).

mt_curtose <- round(moments::kurtosis(saeb$NOTA_MT, na.rm = FALSE),3)

#------------------------------BOXPLOT------------------------------------------

ggplot(saeb) +
  aes(
    x = factor(""),
    y = NOTA_MT
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  guides(fill = none) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  theme_bw()+
  labs(x = "", y = "Nota Matemática")+
  scale_y_continuous(breaks = seq(100,450, 50 ), limits = c(100, 450))

ggsave(filename = file.path(caminho_lucas, "boxplot-uni-mt.png"), width = 158, height = 93, units = "mm")
