#' Breve análise descritiva de uma variável aleatória contínua
#'
#' Essa função lê um conjunto de dados e com base em uma informação a priori retorna algumas medidas de posição além de desvio padrão, coeficiente de assimetria de Bowley e coeficiente percentílico de curtose; a função ainda plota boxplots do conjunto de dados
#'
#' Para comparar diagramas de dispersão duas variáveis e uma informação conhecida de um conjunto de dados utilizar [disp.descritiva()][dens.descritiva()]
#'
#' Para comparar histograma e curva da função de densidade do conjunto de dados utilizar [dens.descritiva()][dens.descritiva()]. A quantidade de classes é definida pela regra de Sturges
#'
#' Para \\U00fdvariáveis qualitativas ou categóricas utilizar [freq.descritiva()][dens.descritiva()] para obter tabela de frequência e gráfico de barras
#'
#' @param dados Dataframe com ao menos duas colunas, uma com a informação a priori e outra(s) com valores para calcular estatísticas
#'
#' @param condicional Coluna em que está a informação conhecida
#'
#' @param vars Lista com nomes das colunas em que estão os valores com os quais serão calculadas as estatísticas
#'
#' @seealso
#'
#' \code{\link{dens.descritiva}} Retorna histograma e curva de densidade
#'
#' @seealso
#'
#' \code{\link{disp.descritiva}} Retorna diagrama de dispersão
#'
#' @seealso
#'
#' \code{\link{freq.descritiva}} Retorna tabela de frequência e gráfico de barra
#'
#' @return Dataframe 14 x n -onde n é a quantidade de variáveis observadas- com mínimo, máximo, amplitude, percentil 10, 1o quartil, mediana, 3o quartil, percentil 90, média, variância, desvio padrão, coefiente de variação (apenas quando variância e média tem mesmo sinal), coeficiente de assimetria de Bowley e coeficiente percentílico de curtose.
#'
#' @export
#'
#' @examples
#'
#' #Exemplo 1:
#' #função recebe: dataset iris,
#' #               info a priori "Species"
#' #               observações das colunas "Petal.Length" e "Sepal.Length"
#'
#' descritiva(iris, "Species", c("Petal.Length", "Sepal.Length"))
#'
#' #Exemplo 2:
#' #função recebe: dataset mtcars,
#' #               info a priori "cyl"
#' #               observações da colunas "mpg"
#'
#' descritiva(mtcars, "cyl", "mpg")
#'
descritiva <- function(dados, condicional, vars){

  #confirmar se objeto enviado para função é um df

  if (is.data.frame(dados) == FALSE){
    stop("A função descritiva() só recebe dataframe")
  }

  #limitar dataframe às informações de interesse

  dados <- subset(dados, select=c(vars, condicional))

  #criar dataframe com a informação a priori

  priori<-as.data.frame(table(dados[[condicional]]))

  #iniciar dataframe de resumo das estatísticas com nomes das linhas

  resumo<-as.data.frame(matrix(ncol = 0, nrow = 14))

  row.names(resumo)<-c("qntd_obs",
                       "min",
                       "max",
                       "amplitude",
                       "percentil_10",
                       "1o_qrtl",
                       "mediana",
                       "3o_qrtl",
                       "percentil_90",
                       "media",
                       "variancia",
                       "sd",
                       "coef_assimetria",
                       "coef_curtose")

  #a seguir o primeiro for() acessa informações de cada condicional por vez

  for (j in 1:nrow(priori)) {

    #já o segundo acessa as colunas com os valores utilizados para construção do resumo descritivo

    for (i in 1:length(vars)) {

      #contrução de nome para título das colunas do df resumo

      col_nome <- paste(vars[i],'|', priori[j, 1], sep = "")

      #dados_temp é um df temporário que assume os dados de cada priori por ciclo do primeiro for

      dados_temp<-dplyr::filter(dados, dados[[condicional]]==as.character(priori[j,1]))

      #aqui ele é resumido à coluna definida por cada ciclo do segundo for

      dados_temp<-subset(dados_temp, select=c(vars[i]))

      #inserção de nova coluna no df resumo com as estatísticas calculadas

      resumo[, col_nome] <- c(nrow(dplyr::filter(dados, dados[[condicional]]==as.character(priori[j,1]))),
                              min(dados_temp[[vars[i]]]),
                              max(dados_temp[[vars[i]]]),
                              max(dados_temp[[vars[i]]])-min(dados_temp[[vars[i]]]),
                              stats::quantile(dados_temp[[vars[i]]], c(.1)),
                              stats::quantile(dados_temp[[vars[i]]], c(.25)),
                              stats::quantile(dados_temp[[vars[i]]], c(.5)),
                              stats::quantile(dados_temp[[vars[i]]], c(.75)),
                              stats::quantile(dados_temp[[vars[i]]], c(.9)),
                              mean(dados_temp[[vars[i]]], na.rm = TRUE),
                              stats::var(dados_temp[[vars[i]]], na.rm = TRUE),
                              stats::sd(dados_temp[[vars[i]]], na.rm = TRUE),
                              (stats::quantile(dados_temp[[vars[i]]], c(.75))-stats::quantile(dados_temp[[vars[i]]], c(.5)))-(stats::quantile(dados_temp[[vars[i]]], c(.5))-stats::quantile(dados_temp[[vars[i]]], c(.25)))/(stats::quantile(dados_temp[[vars[i]]], c(.75))-stats::quantile(dados_temp[[vars[i]]], c(.5)))+(stats::quantile(dados_temp[[vars[i]]], c(.5))-stats::quantile(dados_temp[[vars[i]]], c(.25))),
                              (stats::quantile(dados_temp[[vars[i]]], c(.75))-stats::quantile(dados_temp[[vars[i]]], c(.25)))/(2*(stats::quantile(dados_temp[[vars[i]]], c(.9))-stats::quantile(dados_temp[[vars[i]]], c(.1)))))
    }
  }

  #preparar saída com todos boxplots na mesma janela

  graphics::par(mfrow=c(nrow(priori),length(vars)))

  #primeiro for para navegar entre priores

  for (k in 1:nrow(priori)) {

    #segundo for para navegar pelas colunas com os valores

    for (i in 1:length(vars)) {

      #definir limites da área de plotagem para melhor comparação visual entre as proris

      lim_min_y <- min(dados[[vars[i]]])
      lim_max_y <- max(dados[[vars[i]]])

      #dados_temp é um df temporário que assume os dados de cada priori por ciclo do primeiro for

      dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[k,1]))

      #aqui ele é resumido à coluna definida por cada ciclo do segundo for

      dados_temp<-subset(dados_temp, select = c(vars[i]))

      #plot do boxplot

      graphics::boxplot(dados_temp,
                ylim=c(lim_min_y,lim_max_y))

      #ajuste e contrução do título do boxplot

      graphics::mtext(paste(vars[i],' | ', priori[k, 1], sep = ""), side = 1, line = 1)
    }
  }

  #resetar janela de saída dos plots

  graphics::par(mfrow=c(1,1))

  #retorno da função

  resumo<-round(resumo, 2)

  return(resumo)
}
