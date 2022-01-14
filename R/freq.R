#' Tabela de frequência
#'
#' Lê conjunto de dados e retorna uma tabela de frequência com observações, frequência absoluta e proporcional
#'
#' @param dados Dataframe com ao menos duas colunas, uma com a informação a priori e outra(s) com valores para plotar histograma e boxplot
#'
#' @param condicional Coluna em que está a informação conhecida
#'
#' @param vars Lista com nomes das colunas em que estão os valores com os quais serão plotados o gráfico de barras e construida a tabela de frequência
#'
#' @export
#'
#' @examples
#'
#' mtcars_ <- mtcars
#' mtcars_$am[mtcars_$am == 0] <- "automatico"
#' mtcars_$am[mtcars_$am == 1] <- "manual"
#' freq.descritiva(mtcars_, "am", c("cyl", "gear"))
#'
freq.descritiva <- function(dados, condicional, vars){

  #limitar dataframe às informações de interesse

  dados <- subset(dados, select = c(vars, condicional))

  #criar dataframe com a informação a priori

  priori <- as.data.frame(table(dados[[condicional]]))

  #preparar saída com todos os gráficos na mesma janela

  graphics::par(mfrow = c(nrow(priori),length(vars)))

  for (i in 1:nrow(priori)) {

    dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

    for (j in 1:length(vars)) {

      tab_ <- table(dados_temp[[j]])    #tabela que será utilizada para gráfico
      tab_freq <- as.data.frame(tab_)   #tabela para imprimir tabela de frequências

      names(tab_freq)[1] <- vars[j]     #renomear colunas da tabela de frequência
      names(tab_freq)[2] <- "Freq_abs"  #renomear colunas da tabela de frequência

      #inserir coluna de frequência relativa da tabela de frequência

      tab_freq$Freq_rel<-format(round(tab_freq$Freq_abs/sum(tab_freq$Freq_abs), 4), nsmall=4)

      #desenhar tabela

      cat("_________________________\n")
      cat(vars[j], "|", as.character(priori[i,1]),"\n" )
      cat("-------------------------\n")
      print(tab_freq)
      cat("_________________________\n")

      #plot do grafico de barras

      graphics::barplot(tab_)

      #ajuste e contrução do subtítulo do gráfico

      graphics::mtext(paste(vars[j],' | ', priori[i, 1], sep = ""), side = 1, line = 3)
    }
  }
}
