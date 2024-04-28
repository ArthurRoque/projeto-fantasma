# Padrões ESTAT
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091",
  "#041835", "#666666" )

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5)
      ,
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}

# Funções ESTAT

# A DATA DEVE ESTAR NO FORMATO:
# data <- data %>%
#   summarize(`Média` = round(mean(displ),2),
#             `Desvio Padrão` = round(sd(displ),2),
#             `Variância` = round(var(displ),2),
#             `Mínimo` = round(min(displ),2),
#             `1º Quartil` = round(quantile(displ, probs = .25),2),
#             `Mediana` = round(quantile(displ, probs = .5),2),
#             `3º Quartil` = round(quantile(displ, probs = .75),2),
#             `Máximo` = round(max(displ),2)) %>%
#   t() %>%
#   as.data.frame() %>%
#   rownames_to_column()

print_quadro_resumo <- function(data, title="Medidas resumo da(o) [
  nome da variável]", label="quad:quadro_resumo1")
  {
    latex <- str_c("\\begin{quadro}[H]
  \t\\caption{", title, "}
  \t\\centering
  \t\\begin{adjustbox}{max width=\\textwidth}
  \t\\begin{tabular}{", sep="")
    col_count <- ncol(data)
    row_count <- nrow(data)
    latex <- str_c(latex, "| l", "|", sep=" ")
    for (i in seq(2, col_count))
    {
      latex <- str_c(latex, "S", sep=" ")
    }
    latex <- str_c(latex, "|}\n\t\\toprule\n\t\t", sep="")
    if (col_count > 2)
    {
      32
      for (i in seq(1,col_count))
      {
        if (i == 1)
          latex <- str_c(latex, "\\textbf{Estatística}", sep="")
        else
          latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
        if (i < col_count)
          latex <- str_c(latex, "&", sep=" ")
        else
          latex <- str_c(latex, "\\\\\n", sep=" ")
      }
    }
    else
    {
      latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor}
  \\\\\n", sep="")
    }
    latex <- str_c(latex, "\t\t\\midrule\n", sep="")
    if (col_count > 2)
      starting_number <- 2
    else
      starting_number <- 1
    for (i in seq(starting_number, row_count))
    {
      latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse =
                                                  " & "), " \\\\\n")
    }
    latex <- str_c(latex, "\t\\bottomrule
  \t\\end{tabular}
  \t\\label{", label, "}
  \t\\end{adjustbox}
  \\end{quadro}", sep="")
    writeLines(latex)
  }