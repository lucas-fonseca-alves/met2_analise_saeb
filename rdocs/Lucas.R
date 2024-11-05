#Carregar Pacotes
library(tidyverse)


#carregar as amostras
lucas <- read_csv("amostra_222025665.csv")
erica <- read_csv("amostra_222015284.csv")

#juntar as amostras

saeb <- bind_rows(erica, 
                  lucas
                  #andré
                  )

#------------------------------------------------------------------------------#
#                             Notas LP                                         #
#------------------------------------------------------------------------------#


#------------------Distribuição de frequencias----------------------------------

#Minimo da nota de LP
min_lp <- min(saeb$NOTA_LP)

#Max da nota LP
max_lp <- max(saeb$NOTA_LP)

#Definindo a quantidade de intervalos


#Definindo os intervalos automaticamente 
lp_intervalos = pretty(saeb$NOTA_LP, n = 10)

# Dividindo os dados e calculando suas frequencias
freq_intervalos_lp <- cut(saeb$NOTA_LP, 
                      breaks = lp_intervalos,
                      right = T)

#Formatando como tabela
tabela_freq_lp <- as.data.frame(table(freq_intervalos_lp))



#------------------------------Histograma--------------------------------------


saeb %>% 
  ggplot(aes(x = NOTA_LP)) +
  geom_histogram(bins = 10, color = "white") +
  theme_classic()+
  scale_x_continuous(breaks = lp_intervalos)+
  scale_y_continuous(breaks = seq(from = 0,to = 400,by = 50))+
  xlab("Nota Língua Portuguesa")+
  ylab("Frequência")















