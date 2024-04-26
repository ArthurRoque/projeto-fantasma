warner_bros <- read.csv2('banco/banco_final.csv', sep=',')

# Transformando variáveis quantitativas
vas_quantitativas <- c('imdb', 'engagement')
warner_bros[, vas_quantitativas] <- lapply(warner_bros[, vas_quantitativas], as.numeric)

summary(warner_bros$imdb)
summary(warner_bros$engagement)
