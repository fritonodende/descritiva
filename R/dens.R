#' Sobreposição de histograma e curva da função de densidade
#'
#' Lê um conjunto de dados e plota a sobreposição do seu histograma e curva de densidade para observar o comportamento da distribuição dos dados
#'
#' @param dados Dataframe com ao menos duas colunas, uma com a informação a priori e outra(s) com valores para plotar histograma e boxplot
#'
#' @param condicional Coluna em que está a informação conhecida
#'
#' @param vars Lista com nomes das colunas em que estão os valores com os quais serão plotados o histograma e curva de densidade
#'
#' @return Plot com histograma e curva de densidade
#'
#' @export
#'
#' @examples
#'
#' dens.descritiva(iris, "Species", c("Petal.Length", "Sepal.Length"))
#'
dens.descritiva <- function(dados, condicional, vars){

  #limitar dataframe as informações de interesse

  dados <- subset(dados, select = c(vars, condicional))

  #criar dataframe com a informação a priori

  priori <- as.data.frame(table(dados[[condicional]]))

  #preparar saída com todos os gráficos na mesma janela

  graphics::par(mfrow = c(nrow(priori),length(vars)))

  #plotar o histograma e curva de densidade, o for acessa cada priori por vez

  for (i in 1:nrow(priori)) {

    dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

    #print(dados_temp)

    for (j in 1:length(vars)) {

      #  lim_min_x <- min(dados[[vars[j]]])
      #  lim_max_x <- max(dados[[vars[j]]])

      graphics::hist(dados_temp[[vars[j]]], #histograma
                prob=TRUE,
                main=NULL,
                ylab="Densidade",
                xlab="")

      graphics::lines(stats::density(dados_temp[[vars[j]]])) #curva de densidade

      #ajuste e contrução do subtítulo do gráfico

      graphics::mtext(paste(vars[j],' | ', priori[i, 1], sep = ""), side = 1, line = 3)

    }
  }

  #resetar janela de saída dos plots

  graphics::par(mfrow=c(1,1))
}
