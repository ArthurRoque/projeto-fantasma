library(dplyr)
library(ggplot2)
library(tibble)
library(stringr)
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



# Número de lançamentos a cada década por formato de lançamento
warner_bros['decade'] = substr(warner_bros$date_aired, 3, 4)
warner_bros <- mutate(warner_bros, decade = as.integer(decade))

warner_bros['decade'] <- as.integer(prefixo_decada(warner_bros$decade))

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

# Variação da nota IMDB por temporada dos episódios
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
            `Máximo` = round(max(imdb),2)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column()

medidas_imdb_por_temp
print_quadro_resumo(medidas_imdb_por_temp)

imdb_por_temp$imdb


view(mpg)

ggplot(imdb_por_temp) +
  aes(x = reorder(season, imdb, FUN = mean), y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporada", y = "Nota IMDB") +
  theme_estat()

ggsave("boxplot_imdb.pdf", width = 158, height = 93, units = "mm")






