setwd("D:/UnB/ME2/atividade 1.3")
library(tidyverse)
library(ggplot2)
library(formattable)
frame <- read.csv("amostra_190084235.csv")

# Escolhendo as cinco variáveis categoricas que iremos trabalhar
frame <- frame[,c(2, 5, 6, 15, 16)]
colnames(frame) <- c("Região", "Área", "Dependencia adm.", "Escolaridade da mãe", "Escolaridade do pai")

# Etendendo como é a "cara" da nossa amostra, analisando por regiões
df_regioes <- as.data.frame(table(frame$Região))
colnames(df_regioes) <- c("Região", "Frequência")

ggpie(
  data = df_regioes, x = "Frequência",
  label = "Região",
  color = "white",
  fill = "Região",
  palette = c("#00AFBB", "#E7B800", "#FC4E07", "#88B4E7", "#FFF05A")
)

# Tabelando por Área
df_area <- as.data.frame(table(frame$Área))
colnames(df_area) <- c("Área", "Frequência")
total <- data.frame(Área = "Total", Frequência = sum(df_area$Frequência))
df_area <- bind_rows(df_area, total)

formattable(df_area,
            list(Área = formatter("span", style = x ~ style(font.weight = ifelse(x == "Total", "bold", "normal"))),
                 Frequência = formatter("span", style = x ~ style(font.weight = ifelse(x == "1000", "bold", "normal"))))
           )

# grafico em barras da dependencia administrativa por regição
df_dependencia_adm <- as.data.frame(table(frame[, c("Região", "Dependencia adm.")]))
colnames(df_dependencia_adm)[c(2, 3)] <- c("Dependencia adm.", "Frequência")
df_dependencia_adm$`Dependencia adm.` <- factor(df_dependencia_adm$`Dependencia adm.`, levels = c("Estadual", "Municipal", "Federal"))

ggplot(df_dependencia_adm, aes(x = Região, y = Frequência, fill = `Dependencia adm.`)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Estadual" = "#FC4E07", "Municipal" = "#00AFBB", "Federal" = "#E7B800")) +
  labs(title = "Dependencias administrativas por região", x = "Região", y = "Frequência") +
  theme_minimal()

#analise das escolaridades dos pais
df_escolaridade_mae <- as.data.frame(table(frame$`Escolaridade da mãe`))
colnames(df_escolaridade_mae) <- c("Escolaridade", "Frequência")

df_escolaridade_pai <- as.data.frame(table(frame$`Escolaridade do pai`))
colnames(df_escolaridade_pai) <- c("Escolaridade", "Frequência")

#limpando os nossos dados 
df_escolaridade_mae <- df_escolaridade_mae %>% filter(Escolaridade != " ")
df_escolaridade_pai <- df_escolaridade_pai %>% filter(Escolaridade != " ")

correção <- df_escolaridade_mae$Frequência[df_escolaridade_mae$Escolaridade == "Não completou a 4.ª série/5.º ano do Ensino Fundametal"]
df_escolaridade_mae$Frequência[df_escolaridade_mae$Escolaridade == "Não completou a 4.ª série/5.º ano do Ensino Fundamental"] <- df_escolaridade_mae$Frequência[df_escolaridade_mae$Escolaridade == "Não completou a 4.ª série/5.º ano do Ensino Fundamental"] + correção
df_escolaridade_mae <- df_escolaridade_mae[df_escolaridade_mae$Escolaridade != "Não completou a 4.ª série/5.º ano do Ensino Fundametal", ]

df_escolaridade_pai$Escolaridade <- as.character(df_escolaridade_pai$Escolaridade)
df_escolaridade_pai$Escolaridade[df_escolaridade_pai$Escolaridade == "Não completou a 4.ª série/5.º ano do Ensino Fundametal"] <- "Não completou a 4.ª série/5.º ano do Ensino Fundamental"

#selecionando os pais dos alunos que sabem
df_escolaridade_mae_sabem <- df_escolaridade_mae %>% filter(Escolaridade != "Não sei")
df_escolaridade_mae_sabem$Escolaridade <- factor(df_escolaridade_mae_sabem$Escolaridade, levels = c("Nunca estudou", "Não completou a 4.ª série/5.º ano do Ensino Fundamental", "Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano do Ensino Fundamental", "Completou a 8.ª série/9.º ano do Ensino Fundamental, mas não completou o Ensino Médio", "Completou o Ensino Médio, mas não completou a Faculdade", "Completou a Faculdade"))
levels(df_escolaridade_mae_sabem$Escolaridade) <- c(1:6)
total_mae <- sum(df_escolaridade_mae_sabem$Frequência)

df_escolaridade_pai_sabem <- df_escolaridade_pai %>% filter(Escolaridade != "Não sei")
df_escolaridade_pai_sabem$Escolaridade <- factor(df_escolaridade_pai_sabem$Escolaridade, levels = c("Nunca estudou", "Não completou a 4.ª série/5.º ano do Ensino Fundamental", "Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano do Ensino Fundamental", "Completou a 8.ª série/9.º ano do Ensino Fundamental, mas não completou o Ensino Médio", "Completou o Ensino Médio, mas não completou a Faculdade", "Completou a Faculdade"))
levels(df_escolaridade_pai_sabem$Escolaridade) <- c(1:6)
total_pai <- sum(df_escolaridade_pai_sabem$Frequência)


df_escolaridade_mae_sabem$Frequência <- round((df_escolaridade_mae_sabem$Frequência / total_mae) * 100, digits = 2)
colnames(df_escolaridade_mae_sabem)[2] <- "Porcentagem"
df_escolaridade_pai_sabem$Frequência <- round((df_escolaridade_pai_sabem$Frequência / total_pai) * 100, digits = 2)
colnames(df_escolaridade_pai_sabem)[2] <- "Porcentagem"
  
#juntar os data frames
df_escolaridade_mae_sabem$indicador <- "Mãe"
df_escolaridade_pai_sabem$indicador <- "Pai"

df_escolaridade_juntos <- rbind(df_escolaridade_mae_sabem, df_escolaridade_pai_sabem)

ggplot(df_escolaridade_juntos, aes(x = Escolaridade, y = Porcentagem, fill = indicador)) +
  geom_bar(stat = "identity", position = "Dodge") +
  labs(title = "Escolaridade dos pais", subtitle = "dos alunos que sabiam", x = "Escolaridade", y = "Porcentagem (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("Pai" = "#88B4E7", "Mãe" = "#FC4E07"))

#tabulando os alunos que não sabiam
m_sei <- sum(df_escolaridade_mae$Frequência[df_escolaridade_mae$Escolaridade != "Não sei"])
m_nsei <- sum(df_escolaridade_mae$Frequência[df_escolaridade_mae$Escolaridade == "Não sei"])

p_sei <- sum(df_escolaridade_pai$Frequência[df_escolaridade_pai$Escolaridade != "Não sei"])
p_nsei <- sum(df_escolaridade_pai$Frequência[df_escolaridade_pai$Escolaridade == "Não sei"])

df_sabem <- data.frame(Mãe = c(m_sei, m_nsei), Pai = c(p_sei, p_nsei), row.names = c("Sabem", "Não sabem"))
formattable(df_sabem)
