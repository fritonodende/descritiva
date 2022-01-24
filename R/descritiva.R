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
#' @param dg  Lógico, default = FALSE, TRUE para exibir resumo e gráficos incluindo dados gerais
#'
#' @param hor Lógico, default = FALSE, TRUE para exibir boxplot horizontal
#'
#' @param tabs Lógico, default = TRUE, FALSE para não exibir tabelas de resumo quando a função é executada
#'
#' @param grafs vetor c(1:n) indicando os gráficos que serão exibidos com o plot customizado, o índice do gráfico corresponde a sua posição no plot geral que a função retorna
#'
#' @param grade vetor c(x, y) indicando a quantidade linhas e colunas em que serão impressos os gráficos customizados definos pelo parâmetro grafs
#'
#' @param titulo Lógico ou string, default = FALSE, TRUE para exibir título no plot geral no formato evento|condicional ou string no formato "título customizado"
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
descritiva <- function(dados, condicional, vars, dg = FALSE, hor = FALSE, tabs = TRUE, grafs = NULL, grade = c(1,1), titulo = FALSE){

  #confirmar se objeto enviado para função é um df

  if (is.data.frame(dados) == FALSE){
    stop("A função descritiva() só recebe dataframe")
  }

  #limitar dataframe às informações de interesse

  dados <- subset(dados, select=c(vars, condicional))

  #iniciar, definir e ocnfigurar a tabela geral, sem definição de priori, que faz parte do retorno da função

  tab_geral <- list()
  tab_geral[[1]] <- dados
  names(tab_geral)[1] <- "tabela.geral"

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

      #construção de nome para título das colunas do df resumo

      col_nome <- paste(vars[j],"|", priori[i, 1], sep = "")

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

  #para o caso do parâmetro dg (dados gerais) estar definido como TRUE

  if(dg == TRUE){

    for(j in 1:length(vars)){

    resumo[, paste(vars[j],".dados.gerais", sep = "")] <- c(nrow(dados),
                                                            min(dados[[vars[j]]]),
                                                            max(dados[[vars[j]]]),
                                                            max(dados[[vars[j]]])-min(dados[[vars[j]]]),
                                                            stats::quantile(dados[[vars[j]]], c(.1)),
                                                            stats::quantile(dados[[vars[j]]], c(.25)),
                                                            stats::quantile(dados[[vars[j]]], c(.5)),
                                                            stats::quantile(dados[[vars[j]]], c(.75)),
                                                            stats::quantile(dados[[vars[j]]], c(.9)),
                                                            mean(dados[[vars[j]]], na.rm = TRUE),
                                                            stats::var(dados[[vars[j]]], na.rm = TRUE),
                                                            stats::sd(dados[[vars[j]]], na.rm = TRUE),
                                                            (stats::quantile(dados[[vars[j]]], c(.75))-stats::quantile(dados[[vars[j]]], c(.5)))-(stats::quantile(dados[[vars[j]]], c(.5))-stats::quantile(dados[[vars[j]]], c(.25)))/(stats::quantile(dados[[vars[j]]], c(.75))-stats::quantile(dados[[vars[j]]], c(.5)))+(stats::quantile(dados[[vars[j]]], c(.5))-stats::quantile(dados[[vars[j]]], c(.25))),
                                                            (stats::quantile(dados[[vars[j]]], c(.75))-stats::quantile(dados[[vars[j]]], c(.25)))/(2*(stats::quantile(dados[[vars[j]]], c(.9))-stats::quantile(dados[[vars[j]]], c(.1)))))
    }
  }

  #iniciar lista que vai armazenar plot de todos os gráficos juntos

  plot_td <- list()

  #iniciar lista que vai armazanar as tabelas definidas por cada priori

  tabs_ <- list()

  ind <- 1 #iniciar contador que define quando armazenar o plot

  #preparar saída com todos boxplots na mesma janela, definição defende do estado do parâmetro dg

  if (dg == TRUE){
    graphics::par(mfrow=c(nrow(priori)+1,length(vars)))
  }else{
    graphics::par(mfrow=c(nrow(priori),length(vars)))
  }

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

      #aqui a lista de tabelas recebe cada tabela A|B e tem os nomes de cada coluna definido

      tabs_[[ind]] <- dados_temp
      names(tabs_)[ind] <- paste(vars[j], "|", as.character(priori[i,1]), sep = "")

      #plot do boxplot

      graphics::boxplot(dados_temp,
                        horizontal = hor,
                        xlab = paste(vars[j],"|", priori[i, 1], sep = ""),
                        ylim=c(lim_min_y,lim_max_y))

      #na última repetição, armazenar plotagem geral para retorno da função

      if(nrow(priori)*length(vars) == ind){
        graphics::mtext(paste("Evento(s)","|", condicional, sep = ""), side = 3, line = -2, outer = TRUE) #título geral
        plot_td[[1]] <- grDevices::recordPlot() #armazenar plot geral para retorno na função
        names(plot_td)[1] <- "plot geral" #renomear coluna para identificar melhor o retorno
      }

      ind <- ind + 1 #incremento do índice que percorre a lista com as tabelas A|B
    }
  }

  #para dg definido como TRUE acrescentar gráficos e tabelas com dados gerais

  if(dg == TRUE){

    for(i in 1:length(vars)){
    lim_min_y <- min(dados[[vars[i]]])
    lim_max_y <- max(dados[[vars[i]]])
    graphics::boxplot(dados[[vars[i]]],
                      horizontal = hor,
                      xlab = paste(vars[i],".dados.gerais", sep = ""),
                      ylim=c(lim_min_y,lim_max_y))
    }

    #atualização da captura do plot geral

    plot_td[[1]] <- grDevices::recordPlot() #armazenar plot geral para retorno na função
    names(plot_td)[1] <- "plot geral" #renomear coluna para identificar melhor o retorno


    for(i in 1:length(vars)){
        tabs_ [[paste(vars[i],".dados.gerais", sep = "")]]<- dados[[vars[i]]]
    }
  }

  #preparar retorno da função

  resumo <- round(resumo, 2) #tabela com estatísticas de resumo

  #exibir resumo para quando o usuário não associar a função à uma variável, desativado quando tabs = FALSE

  if(tabs == TRUE){

  print(resumo)

  }

  #organizar lista retornada pela função

  retorno<-list("resumo" = resumo, "plot geral" = plot_td, "tabela geral" = tab_geral, "tabelas" = tabs_)

  #ajustes para gráfico customizado, parametro graf definido pelo usuário

  if(!(is.null(grafs))){

    #preparar teste para saber se há apenas um tipo de evento no gráfico
    #quando só há um evento o eixo y mantém os mesmos limites em todos os boxplots
    #facilitando a comparação

    teste_ <- list()
    contrateste_ <- list()

    for(i in 1:length(grafs)){
      teste_[i] <- names(retorno[[4]][[grafs[i]]])
      contrateste_[i] <- names(retorno[[4]][[grafs[1]]])
    }

    #resultado do teste define decisões sobre limites dos plots a seguir

    teste <- isTRUE(all.equal(teste_, contrateste_))

    if(teste){

      #iniciar e preencher dataframe com y max e min

      ymin_ymax <- data.frame()
      for(i in 1:length(grafs)){
       ymin_ymax <- rbind(ymin_ymax, retorno[[4]][[grafs[i]]])
      }
    }

    #definir grade para graficos customizados

    graphics::par(mfrow = grade)

    #plotar gráfico customizado

    for(i in 1:length(grafs)){

      dados_custom <- retorno[[4]][[grafs[i]]]

      if(teste){
          graphics::boxplot(dados_custom,
                          horizontal = hor,
                          xlab=names(retorno[[4]][grafs[i]]),
                          ylim=c(min(ymin_ymax), max(ymin_ymax)))
      }else{
          graphics::boxplot(dados_custom,
                          horizontal = hor,
                          xlab=names(retorno[[4]][grafs[i]]))
      }
    }

    #titulo do grafico customizado definido pelo parâmetro titulo = TRUE

    if( titulo != FALSE){
        if(titulo == TRUE)
        {
        graphics::mtext(paste("Eventos","|", condicional, sep = ""), side = 3, line = -2, outer = TRUE) #título geral
        }else{
          graphics::mtext(titulo, side = 3, line = -2, outer = TRUE) #título geral
        }
    }
  }

  #retorno da função

  return(invisible(retorno))
}
