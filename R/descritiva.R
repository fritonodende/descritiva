#' Breve analise descritiva de uma variavel aleatoria continua
#'
#' Essa função lê um conjunto de dados e com base em uma informação a priori retorna algumas medidas de posição além de desvio padrão, coeficiente de assimetria de Bowley e coeficiente percentílico de curtose; a função ainda plota boxplots do conjunto de dados
#'
#' Para comparar diagramas de dispersão duas variáveis e uma informação conhecida de um conjunto de dados utilizar [disp.descritiva()][disp.descritiva()]
#'
#' Para comparar histograma e curva da função de densidade do conjunto de dados utilizar [dens.descritiva()][dens.descritiva()]. A quantidade de classes é definida pela regra de Sturges
#'
#' Para variáveis qualitativas ou categóricas utilizar [freq.descritiva()][freq.descritiva()] para obter tabela de frequência e gráfico de barras
#'
#' Utilizar [asg.est()][asg.est()] para estimar parâmetros mi, sigma e alpha para a função densidade de probabilidade da distribuição Alpha Skew Gaussian (ASG)
#'
#' @param dados Dataframe com ao menos duas colunas, uma com a informação a priori e outra(s) com valores para calcular estatísticas
#'
#' @param vars Lista com nomes das colunas em que estão os valores com os quais serão calculadas as estatísticas
#'
#' @param condicional Coluna em que está a informação conhecida
#'
#' @param classe Caso queira definir as classes que deseja analisar
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
#' @seealso
#'
#' \code{\link{asg.est}} Retorna valores de mi, sigma e alpha para a função densidade de probabilidade da distribuição Alpha Skew Gaussian (ASG)
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
#' descritiva(iris, c("Petal.Length", "Sepal.Length"), "Species")
#'
#' #Exemplo 2:
#' #função recebe: dataset mtcars,
#' #               info a priori "cyl"
#' #               observações da colunas "mpg"
#'
#' descritiva(mtcars, "mpg", "cyl")
#'
descritiva <- function(dados,
                       vars,
                       condicional,
                       classe = NULL,
                       dg = FALSE,
                       hor = FALSE,
                       tabs = TRUE,
                       grafs = NULL,
                       grade = c(1,1),
                       titulo = FALSE){

  #confirmar se objeto enviado para função é um df

  if (is.data.frame(dados) == FALSE){
    stop("A função descritiva() só recebe dataframe")
  }

  #limitar dataframe às informações de interesse

  dados <- subset(dados, select=c(vars, condicional))

  #iniciar, definir e configurar a tabela geral, sem definição de priori, que faz parte do retorno da função

  tab_geral <- list()
  tab_geral[[1]] <- dados
  names(tab_geral)[1] <- "tabela.geral"

  #criar dataframe com a informação a priori

  priori <- as.data.frame(table(dados[[condicional]]))

  #caso a classe seja definida

  if(!is.null(classe)){
      priori <- dplyr::filter(priori, priori[[1]] %in% classe)
  }

  #iniciar dataframe de resumo das estatísticas com nomes das linhas

  resumo<-as.data.frame(matrix(ncol = 0, nrow = 14))

  row.names(resumo)<-c("qtd_obs",
                       "min",
                       "max",
                       "amplitude",
                       "perc_10",
                       "1o_qrtl",
                       "mediana",
                       "3o_qrtl",
                       "perc_90",
                       "media",
                       "variancia",
                       "sd",
                       "coef_assimetria",
                       "coef_curtose")

  #teste para incluir estatísticas dos dados gerais (parâmetro)

  if(dg == TRUE){

    for(j in 1:length(vars)){


      resumo[, paste0(vars[j])] <- c(nrow(dados),
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
                                              (stats::quantile(dados[[vars[j]]], c(.75)) - 2 * stats::quantile(dados[[vars[j]]], c(.50)) + stats::quantile(dados[[vars[j]]], c(.25))) / (stats::quantile(dados[[vars[j]]], c(.75)) - stats::quantile(dados[[vars[j]]], c(.25))), # Coeficiente de Assimetria de Bowley
                                              (stats::quantile(dados[[vars[j]]], c(.75)) - stats::quantile(dados[[vars[j]]], c(.25)))/(2*(stats::quantile(dados[[vars[j]]], c(.9))-stats::quantile(dados[[vars[j]]], c(.1))))) # Coeficiente Percentílico de Curtose
    }
  }

  #a seguir o primeiro for() acessa informações de cada condicional por vez

  for (i in 1:nrow(priori)) {

    #já o segundo acessa as colunas com os valores utilizados para construção do resumo descritivo

    for (j in 1:length(vars)) {

      #construção de nome para título das colunas do df resumo

      col_nome <- paste(vars[j],"|", priori[i, 1], sep = "")

      #dados_temp é um df temporário que assume os dados de cada priori por ciclo do primeiro for

      dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

      #aqui ele é resumido à coluna definida por cada ciclo do segundo for

      dados_temp <- subset(dados_temp, select = c(vars[j]))

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

  #iniciar lista que vai armazanar as tabelas definidas por cada priori

  tabs_ <- list()

  ind <- 1 #iniciar contador que define quando armazenar o plot

  #preparar saída com todos boxplots na mesma janela, definição defende do estado do parâmetro dg

  min_max <- vector() #iniciar vetor que vai armazenar valores para definir min e max eixo y boxplot

  #concatenar tds os valores das variáveis observadas

  for (i in 1:length(vars)) {
    min_max <- rbind(min_max,dados[[vars[i]]])
  }

  #deifinir efetivamente mínimo e máximo dos plots

  lim_min_y <- min(min_max)
  lim_max_y <- max(min_max)

  #teste para incluir gráfico de dados gerais (parâmetro)

  if (dg == TRUE){

    graphics::par(mfrow=c(nrow(priori)+1,length(vars))) #grade com dados gerais

    for(i in 1:length(vars)){

      graphics::boxplot(dados[[vars[i]]],
                        horizontal = hor,
                        xlab = paste0(vars[i]),
                        ylim=c(lim_min_y,lim_max_y))


      tabs_ [[paste0(vars[i])]]<- as.data.frame(dados[[vars[i]]])
      names(tabs_)[[ind]] <- paste0(vars[i])

      ind <- ind + 1
    }

  }else{

    graphics::par(mfrow=c(nrow(priori),length(vars))) #grade sem dados gerais

  }

  #primeiro for para navegar entre priores, gerar tabelas condicionais e criar um plot geral

  for (i in 1:nrow(priori)) {

    #já o segundo acessa as colunas com os valores utilizados para construção da tabela e gráficos

    for (j in 1:length(vars)) {

      #dados_temp é um df temporário que assume os dados de cada priori por ciclo do primeiro for

      dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

      #aqui ele é resumido à coluna definida por cada ciclo do segundo for

      dados_temp <- subset(dados_temp, select = c(vars[j]))

      #aqui a lista de tabelas recebe cada tabela A|B e tem os nomes de cada coluna definido

      tabs_[[ind]] <- dados_temp
      names(tabs_)[ind] <- paste(vars[j], "|", as.character(priori[i,1]), sep = "")

      #plot do boxplot

      graphics::boxplot(dados_temp,
                        horizontal = hor,
                        xlab = paste(vars[j],"|", priori[i, 1], sep = ""),
                        ylim=c(lim_min_y,lim_max_y))

      ind <- ind + 1 #incremento do índice que percorre a lista com as tabelas A|B
    }
  }

  #armazenar plotagem geral para retorno da função

  graphics::mtext(paste("Evento(s)","|", condicional, sep = ""), side = 3, line = -2, outer = TRUE) #título geral
  plot_td[[1]] <- grDevices::recordPlot() #armazenar plot geral para retorno na função
  names(plot_td)[1] <- "plot geral" #renomear coluna para identificar melhor o retorno

  #preparar retorno da função

  resumo <- round(resumo, 2) #tabela com estatísticas de resumo

  #exibir resumo para quando o usuário não associar a função à uma variável, desativado quando tabs = FALSE

  if(tabs == TRUE){

    print(t(resumo))

  }

  #organizar lista retornada pela função

  retorno <- list("resumo" = as.data.frame(t(resumo)),
                  "plot geral" = plot_td,
                  "tabela geral" = tab_geral,
                  "tabelas" = tabs_)

  #ajustes para gráfico customizado, parametro graf definido pelo usuário

  if(!(is.null(grafs))){

    #iniciar lista que vai armazenar plots

    plot_personal <- list()

    #definir grade para gráficos customizados

    graphics::par(mfrow = grade)

    #plotar gráfico customizado

    for(i in 1:length(grafs)){

      dados_custom <- retorno[[4]][[grafs[i]]]

      graphics::boxplot(dados_custom,
                        horizontal = hor,
                        xlab=names(retorno[[4]][grafs[i]]),
                        ylim=c(lim_min_y, lim_max_y))

      #testar parâmetro para inserir título no gráficos personalizados

      if(sum(grade) == 2 && titulo != FALSE){

        if(titulo == TRUE){

          graphics::mtext(paste("Eventos","|", condicional, sep = ""), side = 3, line = -2, outer = TRUE) #título automático

        }else{

          graphics::mtext(titulo, side = 3, line = -2, outer = TRUE) #título personalizado

        }
      }

      if(sum(grade) == 2){ #testar se plots são individuais para captura de um por vez

        plot_personal[[i]] <- grDevices::recordPlot() #armazenar cada plot para retorno na função
        names(plot_personal)[i] <- names(retorno[[4]][grafs[i]]) #renomear coluna para identificar melhor o retorno

      }
    }

    #titulo geral do grafico customizado definido pelo parâmetro titulo = TRUE caso multiplos gráficos na mesma janela

    if (sum(grade) !=2){

      if( titulo != FALSE){

        if(titulo == TRUE){

          graphics::mtext(paste("Eventos","|", condicional, sep = ""), side = 3, line = -2, outer = TRUE) #título geral

        }else{

          graphics::mtext(titulo, side = 3, line = -2, outer = TRUE) #título geral personalizado
        }
      }

      plot_personal[[1]] <- grDevices::recordPlot() #armazenar plot geral para retorno na função

    }

    names(plot_personal)[1] <- "plot personalizado" #renomear coluna para identificar melhor o retorno

    retorno <- list("resumo" =  as.data.frame(t(resumo)),
                    "plot geral" = plot_td,
                    "tabela geral" = tab_geral,
                    "tabelas" = tabs_,
                    "plot personalizado" = plot_personal)

  }

  #resetar par(mfrow

  graphics::par(mfrow=c(1, 1))

  #retorno da função

  return(invisible(retorno))
}
