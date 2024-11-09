# Definindo o diretório e carregando pacotes
setwd("D:/UnB/ME2/atividade 1.3")
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(formattable)

# Carregando o conjunto de dados
data <- read.csv("amostra_190084235.csv")

# Selecionando e renomeando variáveis de interesse
data <- data[, c(2, 5, 6, 15, 16)]
colnames(data) <- c("Região", "Área", "Dependência_Adm", "Escolaridade_Mãe", "Escolaridade_Pai")

# Análise por região
regioes_frequencia <- as.data.frame(table(data$Região))
colnames(regioes_frequencia) <- c("Região", "Frequência")

ggpie(
  data = regioes_frequencia, x = "Frequência",
  label = "Região", color = "white", fill = "Região",
  palette = c("#00AFBB", "#E7B800", "#FC4E07", "#88B4E7", "#FFF05A")
)

# Tabela por área
area_frequencia <- as.data.frame(table(data$Área))
colnames(area_frequencia) <- c("Área", "Frequência")

# Adicionando linha total e formatando tabela
area_total <- data.frame(Área = "Total", Frequência = sum(area_frequencia$Frequência))
area_frequencia <- bind_rows(area_frequencia, area_total)

formattable(area_frequencia,
            list(
              Área = formatter("span", style = x ~ style(font.weight = ifelse(x == "Total", "bold", "normal"))),
              Frequência = formatter("span", style = x ~ style(font.weight = ifelse(x == "1000", "bold", "normal")))
            )
)

# Gráfico em barras: Dependência administrativa por região
dep_adm_frequencia <- as.data.frame(table(data[, c("Região", "Dependência_Adm")]))
colnames(dep_adm_frequencia) <- c("Região", "Dependência_Adm", "Frequência")
dep_adm_frequencia$Dependência_Adm <- factor(dep_adm_frequencia$Dependência_Adm, levels = c("Municipal", "Estadual", "Federal"))

ggplot(dep_adm_frequencia, aes(x = Região, y = Frequência, fill = Dependência_Adm)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Municipal" = "#FC4E07", "Estadual" = "#00AFBB", "Federal" = "#E7B800")) +
  labs(title = "Dependências administrativas por região", x = "Região", y = "Frequência") +
  theme_minimal()

# Análise das escolaridades dos pais
escolaridade_mae <- as.data.frame(table(data$Escolaridade_Mãe))
colnames(escolaridade_mae) <- c("Escolaridade", "Frequência")

escolaridade_pai <- as.data.frame(table(data$Escolaridade_Pai))
colnames(escolaridade_pai) <- c("Escolaridade", "Frequência")

# Limpando dados imcompletos de escolaridade
escolaridade_mae <- escolaridade_mae %>% filter(Escolaridade != " ")
escolaridade_pai <- escolaridade_pai %>% filter(Escolaridade != " ")

# Correção de erros ortográficos na escolaridade
correção <- escolaridade_mae$Frequência[escolaridade_mae$Escolaridade == "Não completou a 4.ª série/5.º ano do Ensino Fundametal"]
escolaridade_mae$Frequência[escolaridade_mae$Escolaridade == "Não completou a 4.ª série/5.º ano do Ensino Fundamental"] <-
  escolaridade_mae$Frequência[escolaridade_mae$Escolaridade == "Não completou a 4.ª série/5.º ano do Ensino Fundamental"] + correção
escolaridade_mae <- escolaridade_mae[escolaridade_mae$Escolaridade != "Não completou a 4.ª série/5.º ano do Ensino Fundametal", ]

escolaridade_pai$Escolaridade <- as.character(escolaridade_pai$Escolaridade)
escolaridade_pai$Escolaridade[escolaridade_pai$Escolaridade == "Não completou a 4.ª série/5.º ano do Ensino Fundametal"] <- "Não completou a 4.ª série/5.º ano do Ensino Fundamental"

# Selecionando pais com escolaridade conhecida
mae_sabe <- escolaridade_mae %>% filter(Escolaridade != "Não sei")
mae_sabe$Escolaridade <- factor(mae_sabe$Escolaridade, levels = c(
  "Nunca estudou", "Não completou a 4.ª série/5.º ano do Ensino Fundamental",
  "Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano do Ensino Fundamental",
  "Completou a 8.ª série/9.º ano do Ensino Fundamental, mas não completou o Ensino Médio",
  "Completou o Ensino Médio, mas não completou a Faculdade", "Completou a Faculdade"
))
total_mae <- sum(mae_sabe$Frequência)

pai_sabe <- escolaridade_pai %>% filter(Escolaridade != "Não sei")
pai_sabe$Escolaridade <- factor(pai_sabe$Escolaridade, levels = c(
  "Nunca estudou", "Não completou a 4.ª série/5.º ano do Ensino Fundamental",
  "Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano do Ensino Fundamental",
  "Completou a 8.ª série/9.º ano do Ensino Fundamental, mas não completou o Ensino Médio",
  "Completou o Ensino Médio, mas não completou a Faculdade", "Completou a Faculdade"
))
total_pai <- sum(pai_sabe$Frequência)

# Mudança dos níves para valores numéricos (1 a 6)
levels(mae_sabe$Escolaridade) <- c(1:6)
levels(pai_sabe$Escolaridade) <- c(1:6)

# Calculando proporções
mae_sabe$Porcentagem <- round((mae_sabe$Frequência / total_mae) * 100, 2)
pai_sabe$Porcentagem <- round((pai_sabe$Frequência / total_pai) * 100, 2)

# Adicionando rótulos e unindo os data frames
mae_sabe$Parente <- "Mãe"
pai_sabe$Parente <- "Pai"
escolaridade_juntos <- rbind(mae_sabe, pai_sabe)

ggplot(escolaridade_juntos, aes(x = Escolaridade, y = Porcentagem, fill = Parente)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Escolaridade dos pais", subtitle = "dos alunos que sabem a escolaridade dos pais", x = "Escolaridade", y = "Porcentagem (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("Pai" = "#88B4E7", "Mãe" = "#FC4E07"))

# Tabelando os alunos que não sabem
escolaridade_nao_sabe <- data.frame(
  Escolaridade_Mãe = data$Escolaridade_Mãe,
  Escolaridade_Pai = data$Escolaridade_Pai
)
escolaridade_nao_sabe <- as.data.frame(table(escolaridade_nao_sabe))
colnames(escolaridade_nao_sabe) <- c("Escolaridade_Mãe", "Escolaridade_Pai", "Frequência")

escolaridade_nao_sabe <- escolaridade_nao_sabe %>%
  filter(Escolaridade_Mãe != " ", Escolaridade_Pai != " ")

# Contagem total de alunos que sabem e não sabem
total_sabe <- sum(escolaridade_nao_sabe$Frequência[
  escolaridade_nao_sabe$Escolaridade_Mãe != "Não sei" & escolaridade_nao_sabe$Escolaridade_Pai != "Não sei"
])
total_nao_sabe <- sum(escolaridade_nao_sabe$Frequência[
  escolaridade_nao_sabe$Escolaridade_Mãe == "Não sei" | escolaridade_nao_sabe$Escolaridade_Pai == "Não sei"
])

# Tabela final com total de alunos que sabem e não sabem
dados_sabe <- data.frame(Sabe = total_sabe, Não_Sabe = total_nao_sabe)
colnames(dados_sabe) <- c("Sabe", "Não sabe")
formattable(dados_sabe)
