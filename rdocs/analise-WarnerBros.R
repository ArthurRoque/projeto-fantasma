library(dplyr)
library(ggplot2)
library(tibble)
library(stringr)
library(tidyverse)
source("rdocs/padrao_estat.R")

warner_bros <- read.csv2('banco/banco_final.csv', sep=',')

# Transformando variáveis quantitativas
vas_quantitativas <- c('imdb', 'engagement','run_time')
warner_bros[, vas_quantitativas] <- lapply(warner_bros[, vas_quantitativas], as.numeric)


# Funções

prefixo_decada <- function(decadas) {
  prefixo <- ifelse(decadas >= 60, "19", ifelse(decadas < 10, "200", "20"))
  return(paste0(prefixo, decadas))
}

get_decade <- function(date_string) {
  year <- as.numeric(substr(date_string, 1, 4))
  decade <- floor(year / 10) * 10
  return(decade)
}

# Número de lançamentos a cada década por formato de lançamento
warner_bros['decade'] <- get_decade(warner_bros$date_aired)

df_subset <- warner_bros[, c("date_aired", "decade")]

# Agora visualize o novo dataframe usando View
View(df_subset)

lancamentos_por_decada_por_tipo <- warner_bros %>%
  group_by(decade, format)%>%
  summarize(contagem = n(), .groups = 'drop')

ggplot(lancamentos_por_decada_por_tipo) +
  aes(x = decade, y = contagem, group = format, colour = format) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Década", y = "Número de lançamentos", colour = "Formato") +
  scale_x_continuous(breaks = seq(1970, 
                                  2020, 
                                  by = 10)) +
  theme_estat()
ggsave("formato_de_lancamento_decadas.pdf", width = 158, height = 93, units = "mm")

# Variação da nota IMDB por temporada
imdb_por_temp <- warner_bros %>%
  filter(!season %in% c("Crossover", "Movie", "Special")) %>%
  group_by(season)

medidas_imdb_por_temp <- imdb_por_temp %>%
  summarize(`Média` = round(mean(imdb),2),
            `Desvio Padrão` = round(sd(imdb),2),
            `Variância` = round(var(imdb),2),
            `Mínimo` = round(min(imdb),2),
            `1º Quartil` = round(quantile(imdb, probs = .25),2),
            `Mediana` = round(quantile(imdb, probs = .5),2),
            `3º Quartil` = round(quantile(imdb, probs = .75),2),
            `Máximo` = round(max(imdb),2),
            `Número de episódios` = n()) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column()

medidas_imdb_por_temp
print_quadro_resumo(medidas_imdb_por_temp)

imdb_por_temp$imdb


ggplot(imdb_por_temp) +
  aes(x = reorder(season, imdb, FUN = mean), y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporada", y = "Nota IMDB") +
  theme_estat()

ggsave("boxplot_imdb.pdf", width = 158, height = 93, units = "mm")


t4 <- imdb_por_temp %>%
  filter(season == 4)

t4$imdb

ggplot(mpg) +
  aes(x = cty) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 7) +
  labs(x = "Consumo em Cidade (milhas/galão)", y = "Frequência
Absoluta") +
  theme_estat()
ggsave("hist_uni.pdf", width = 158, height = 93, units = "mm")
20

# Top 3 terrenos mais frequentes pela ativação da armadilha
terrenos <- warner_bros %>%
  group_by(setting_terrain)%>%
  summarize(contagem = n())%>%
  mutate(frequencia_relativa = contagem / sum(contagem)) %>%
  arrange(desc(contagem))
sum(terrenos$frequencia_relativa)

top_3terrenos <- warner_bros %>%
  filter(setting_terrain %in% c("Urban", "Rural", "Forest")) %>%
  filter(trap_work_first != "") %>%
  group_by(setting_terrain, trap_work_first)%>%
  summarize(contagem = n(),.groups = 'drop') %>%
  mutate(frequencia_relativa = round(contagem / sum(contagem), 2))

porcentagens <- str_c(top_3terrenos$frequencia_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(top_3terrenos$contagem, " (", porcentagens, ")"))
                    
top_3terrenos

porcentagens
top_3terrenos$trap_work_first <- ifelse(top_3terrenos$trap_work_first == "True", "Sim", "Não")
top_3terrenos

ggplot(top_3terrenos) +
  aes(
    aes(label = trap_work_first),
    x = fct_reorder(setting_terrain, contagem, .desc = T), y = contagem,
    fill = trap_work_first, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Tipo de terreno", y = "Frequência", fill="Sucesso na primeira armadilha") +
  theme_estat()


ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")

# Relação entre as notas IMDB e engajamento
ggplot(warner_bros) +
  aes(x = imdb, y = engagement) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Notas imdb",
    y = "Engajamento"
  ) +
  theme_estat()
cor(warner_bros$imdb, warner_bros$engagement, method = "spearman")

ggsave("imdb_vs_engajamento.pdf", width = 158, height = 93, units = "mm")

# Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro
warner_bros <- warner_bros %>%
  mutate(hero = case_when(
    caught_fred == "True" ~ "Fred",
    caught_daphnie == "True" ~ "Daphnie",
    caught_velma == "True" ~ "Velma",
    caught_shaggy == "True" ~ "Shaggy",
    caught_scooby == "True" ~ "Scooby",
    caught_other == "True" ~ "Outro",
    caught_not == "True" ~ "Nenhum"
  ))

heros <- warner_bros %>%
  group_by(hero)%>%
  filter(hero != "NaN")
heros <- heros[, c("engagement", "hero")]

ggplot(heros) +
  aes(x = reorder(hero, engagement, FUN = min), y = engagement) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagem", y = "Engajamento") +
  theme_estat()

ggsave("engajamento_por_personagem.pdf", width = 158, height = 93, units = "mm")
