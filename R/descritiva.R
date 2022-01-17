#' Breve análise descritiva de uma variável aleatória contínua
#'
#' Essa função lê um conjunto de dados e com base em uma informação a priori retorna algumas medidas de posição além de desvio padrão, coeficiente de assimetria de Bowley e coeficiente percentílico de curtose; a função ainda plota boxplots do conjunto de dados
#'
#' Para comparar diagramas de dispersão duas variáveis e uma informação conhecida de um conjunto de dados utilizar [disp.descritiva()][dens.descritiva()]
#'
#' Para comparar histograma e curva da função de densidade do conjunto de dados utilizar [dens.descritiva()][dens.descritiva()]. A quantidade de classes é definida pela regra de Sturges
#'
#' Para variáveis qualitativas ou categóricas utilizar [freq.descritiva()][dens.descritiva()] para obter tabela de frequência e gráfico de barras
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

  for (i in 1:nrow(priori)) {

    #já o segundo acessa as colunas com os valores utilizados para construção do resumo descritivo

    for (j in 1:length(vars)) {

      #contrução de nome para título das colunas do df resumo

      col_nome <- paste(vars[j],'|', priori[i, 1], sep = "")

      #dados_temp é um df temporário que assume os dados de cada priori por ciclo do primeiro for

      dados_temp<-dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

      #aqui ele é resumido à coluna definida por cada ciclo do segundo for

      dados_temp<-subset(dados_temp, select=c(vars[j]))

      #inserção de nova coluna no df resumo com as estatísticas calculadas

      resumo[, col_nome] <- c(nrow(dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))),
                              min(dados_temp[[vars[j]]]),
                              max(dados_temp[[vars[j]]]),
                              max(dados_temp[[vars[j]]])-min(dados_temp[[vars[j]]]),
                              stats::quantile(dados_temp[[vars[j]]], c(.1)),
                              stats::quantile(dados_temp[[vars[j]]], c(.25)),
                              stats::quantile(dados_temp[[vars[j]]], c(.5)),
                              stats::quantile(dados_temp[[vars[j]]], c(.75)),
                              stats::quantile(dados_temp[[vars[j]]], c(.9)),
                              mean(dados_temp[[vars[j]]], na.rm = TRUE),
                              stats::var(dados_temp[[vars[j]]], na.rm = TRUE),
                              stats::sd(dados_temp[[vars[j]]], na.rm = TRUE),
                              (stats::quantile(dados_temp[[vars[j]]], c(.75))-stats::quantile(dados_temp[[vars[j]]], c(.5)))-(stats::quantile(dados_temp[[vars[j]]], c(.5))-stats::quantile(dados_temp[[vars[j]]], c(.25)))/(stats::quantile(dados_temp[[vars[j]]], c(.75))-stats::quantile(dados_temp[[vars[j]]], c(.5)))+(stats::quantile(dados_temp[[vars[j]]], c(.5))-stats::quantile(dados_temp[[vars[j]]], c(.25))),
                              (stats::quantile(dados_temp[[vars[j]]], c(.75))-stats::quantile(dados_temp[[vars[j]]], c(.25)))/(2*(stats::quantile(dados_temp[[vars[j]]], c(.9))-stats::quantile(dados_temp[[vars[j]]], c(.1)))))
    }
  }

  #iniciar lista que vai armazenar plot de todos os gráficos juntos

  plot_td <- list()

  ind <- 1 #iniciar contador que define quando armazenar o plot

  #preparar saída com todos boxplots na mesma janela

  graphics::par(mfrow=c(nrow(priori),length(vars)))

  #primeiro for para navegar entre priores

  for (i in 1:nrow(priori)) {

    #já o segundo acessa as colunas com os valores utilizados para construção da tabela e gráficos

    for (j in 1:length(vars)) {

      #definir limites da área de plotagem para melhor comparação visual entre as proris

      lim_min_y <- min(dados[[vars[j]]])
      lim_max_y <- max(dados[[vars[j]]])

      #dados_temp é um df temporário que assume os dados de cada priori por ciclo do primeiro for

      dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

      #aqui ele é resumido à coluna definida por cada ciclo do segundo for

      dados_temp<-subset(dados_temp, select = c(vars[j]))

      #plot do boxplot

      graphics::boxplot(dados_temp,
                ylim=c(lim_min_y,lim_max_y))
      graphics::title(xlab = (paste(vars[j],' | ', priori[i, 1], sep = "")), line = 1) #xlabel

      #na última repetição, armazenar plotagem geral para retorno da função

      if(nrow(priori)*length(vars) == ind){
        graphics::mtext(paste("Evento(s)",' | ', condicional, sep = ""), side = 3, line = -2, outer = TRUE) #título geral
        plot_td[[1]] <- grDevices::recordPlot() #armazenar plot geral para retorno na função
        names(plot_td)[1] <- "plot geral" #renomear coluna para identificar melhor o retorno
      }

      ind <- ind+1
    }
  }

  #iniciar lista que vai armazenar cada um dos gráficos indivualmente

  graficos <- list()

  #iniciar o contador que vai permitir navegar pela lista graficos

  ind <- 1

  #resetar janela de saída dos plots

  graphics::par(mfrow=c(1,1))

  #primeiro for para navegar entre priores

  for (i in 1:nrow(priori)) {

    #segundo for para navegar pelas colunas com os valores

    for (j in 1:length(vars)) {

      #definir limites da área de plotagem para melhor comparação visual entre as proris

      lim_min_y <- min(dados[[vars[j]]])
      lim_max_y <- max(dados[[vars[j]]])

      #dados_temp é um df temporário que assume os dados de cada priori por ciclo do primeiro for

      dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

      #aqui ele é resumido à coluna definida por cada ciclo do segundo for

      dados_temp<-subset(dados_temp, select = c(vars[j]))

      #armazenar os gráficos sem plotar na janela. indicações passo a passo

      grDevices::win.metafile()                        #passo 1
      grDevices::dev.control('enable')                 #passo 2
      graphics::par(bg = "white")                      #passo 3
      graphics::boxplot(dados_temp,                    #passo 4 plotar boxplot
                        ylim = c(lim_min_y,lim_max_y)) #passo 5 ajustar limites
      graphics::title(xlab = (paste(vars[j],' | ', priori[i, 1], sep = "")), line = 1) #passo 6 legenda eixo x
      graficos[[ind]] <- grDevices::recordPlot()       #passo 7 armazenar cada plot para retorno na função
      grDevices::dev.off()                             #passo 8 fim dessa etapa

      #construindo nome das colunas para cada A|B

      colnome <- paste(vars[j], "|", as.character(priori[i,1]))

      #renomear coluna com colnome definindo acima

      names(graficos)[ind]<-colnome

      ind <- ind+1
    }
  }

  #retorno da função

  resumo<-round(resumo, 2)

  #exibir resumo para quando o usuário não associar a função à uma variável

  print(resumo)

  retorno<-list("resumo" = resumo, "graficos" = graficos, "plot geral" = plot_td)

  return(invisible(retorno))
}
