#' Diagramas de dispersão a partir de uma informação conhecida
#'
#' Essa função lê um conjunto de dados e plota diagramas de dispersão para observação do comportamento de uma determinada relação entre variáveis de acordo com informações a priori
#'
#' @param dados Dataframe com ao menos três colunas, uma com a informação a priori e no mínimo outras duas com valores para coordenadas x e y
#'
#' @param condicional Coluna em que está a informação conhecida (priori)
#'
#' @param vars Lista com os nomes das duas colunas ( x e y, nessa ordem) que formarão as coordenadas do plano cartesiano
#'
#' @export
#'
#' @examples
#'
#' #Exemplo 1:
#' #função recebe: dataset iris,
#' #               info a priori "Species"
#' #               colunas para coordenadas x e y "Petal.Length" e "Petal.Width"
#'
#' disp.descritiva(iris, "Species", c("Sepal.Length", "Sepal.Width"))
#'
#' #Exemplo 2:
#' #função recebe: dataset mtcars,
#' #               info a priori "cyl"
#' #               colunas para coordenadas x e y "mpg" e "qsec"
#'
#' disp.descritiva(mtcars, "cyl", c("mpg", "qsec"))
#'
disp.descritiva <- function(dados, condicional, vars){

  #confirmar se objeto enviado para função é um df

  if (is.data.frame(dados) == FALSE){
    stop("A função disp.descritiva() só recebe dataframe")
  }

  #confirmar que só duas colunas foram passadas para (x,y)

  if (length(vars) != 2){
    stop("A função disp.descritiva() só pode receber duas colunas para coordenadas (x, y)")
  }

  #limitar dataframe às informações de interesse

  dados <- subset(dados, select=c(vars, condicional))

  #criar dataframe com a informação a priori

  priori <- as.data.frame(table(dados[[condicional]]))

  #preparar saída com todos os gráficos na mesma janela

  graphics::par(mfrow = c(1, nrow(priori)))

  #definir limites eixos x e y

  lim_min_x <- min(dados[[vars[1]]])
  lim_max_x <- max(dados[[vars[1]]])
  lim_min_y <- min(dados[[vars[2]]])
  lim_max_y <- max(dados[[vars[2]]])

  #plotar o diagrama de dispersão , o for acessa cada priori por vez

  for (i in 1:nrow(priori)) {

    dados_temp<-dplyr::filter(dados, dados[[condicional]]==as.character(priori[i,1]))

    graphics::plot(x=dados_temp[[1]], y=dados_temp[[2]],
             xlim=c(lim_min_x,lim_max_x),
             ylim=c(lim_min_y,lim_max_y),
             xlab="",
             ylab="",
             col=i)

    #ajuste e contrução do subtítulo do gráfico

    graphics::mtext(paste(vars[1], ' x ', vars[2],' | ', priori[i, 1], sep = ""), cex=.8, side = 1, line = 3)
  }

  #resetar janela de saída dos plots

  graphics::par(mfrow=c(1,1))
}
