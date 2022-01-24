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
#' @param dg  Lógico, default = FALSE, TRUE para exibir resumo e gráficos incluindo dados gerais
#'
#' @param grafs vetor c(1:n) indicando os gráficos que serão exibidos com o plot customizado, o índice do gráfico corresponde a sua posição no plot geral que a função retorna
#'
#' @param grade vetor c(x, y) indicando a quantidade linhas e colunas em que serão impressos os gráficos customizados definos pelo parâmetro grafs
#'
#' @param titulo Lógico ou string, default = FALSE, TRUE para exibir título no plot geral no formato evento|condicional ou string no formato "título customizado"
#'
#' @return Lista com Plots de histogramas e curvas de densidade
#'
#' @export
#'
#' @examples
#'
#' dens.descritiva(iris, "Species", c("Petal.Length", "Sepal.Length"))
#'
dens.descritiva <- function(dados, condicional, vars, dg = FALSE, grafs = NULL, grade = c(1,1), titulo = FALSE){

  #confirmar se objeto enviado para função é um df

  if (is.data.frame(dados) == FALSE){
    stop("A função descritiva() só recebe dataframe")
  }

  #limitar dataframe as informações de interesse

  dados <- subset(dados, select = c(vars, condicional))

  #criar dataframe com a informação a priori

  priori <- as.data.frame(table(dados[[condicional]]))

  #iniciar lista que vai armazenar plot de todos os gráficos juntos

  plot_td <- list()

  #iniciar e armazenar lista com dados gerais

  dados_geral <- list()
  dados_geral[[1]] <- dados
  names(dados_geral)[[1]] <- "dados.gerais"

  #iniciar lista que vai armazenar dados para plots customizados

  dados_plot <- list()

  ind <- 1 #iniciar contador que define quando armazenar o plot

  #preparar saída com todos os gráficos na mesma janela

  if(dg == TRUE){ #teste para dados gerais

    graphics::par(mfrow = c(nrow(priori)+1,length(vars)))

  }else{

    graphics::par(mfrow = c(nrow(priori),length(vars)))

  }

  #primeiro for para navegar entre priores

  for (i in 1:nrow(priori)) {

    #dados_temp é um df temporário que assume os dados de cada priori por ciclo do primeiro for

    dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

    #já o segundo acessa as colunas com os valores utilizados para construção do histograma

    for (j in 1:length(vars)) {

      #lista recebe dados para o plot

      dados_plot[[ind]] <- dados_temp[[vars[j]]]

      #renomeando coluna para rápida identificação

      names(dados_plot)[ind]<-paste(vars[j],"|", priori[i, 1], sep = "")

      #mínimo e máximmo para definir limites do eixo x

      min_ <- min(dados_temp[[vars[j]]])
      max_ <- max(dados_temp[[vars[j]]])

      #número de classes definido pela regra de sturges

      classes <- grDevices::nclass.Sturges(dados_temp[[vars[j]]])

      #definição da amplitude de classe e vetor com breaks

      passo <- (max_-min_)/classes
      vetorBrk <- seq(min_, max_, by=passo)

      #construir histogramas que serão exibidos

      graphics::hist(dados_temp[[vars[j]]],
                breaks = vetorBrk,
                xaxp = c(min_,max_,classes),
                prob = TRUE,
                main = NULL,
                ylab = "Densidade",
                xlab = paste(vars[j],"|", priori[i, 1], sep = ""))

      #curva de densidade

      graphics::lines(stats::density(dados_temp[[vars[j]]]))

      #na última repetição, armazenar plotagem geral para retorno da função

      if(nrow(priori)*length(vars) == ind){
        graphics::mtext(paste("Evento(s)","|", condicional, sep = ""), side = 3, line = -2, outer = TRUE) #título geral
        plot_td[[1]] <- grDevices::recordPlot() #armazenar plot geral para retorno na função
        names(plot_td)[1] <- "plot geral" #renomear coluna para identificar melhor o retorno
      }

      ind <- ind + 1 #atualizar contador da lista de dados dos plots
    }
  }

  #teste para plotar dados gerais

  if(dg == TRUE){

      #for para percorrer lista de dados dos plots

      for (j in 1:length(vars)) {

        #incluir dados gerais em dados dos plots

        dados_plot[[paste(vars[j],".dados.gerais", sep = "")]] <- dados[[vars[j]]]

        #mínimo e máximmo para definir limites do eixo x

        min_ <- min(dados[[vars[j]]])
        max_ <- max(dados[[vars[j]]])

        #numero de classes definido pela regra de sturges

        classes <- grDevices::nclass.Sturges(dados[[vars[j]]])

        #encontrar passos e definir vetor de breaks no eixo x

        passo <- (max_-min_)/classes
        vetorBrk <- seq(min_, max_, by=passo)
        vetorBrk <- vetorBrk

        #plotar histograma

        graphics::hist(dados[[vars[j]]],
                       breaks = vetorBrk,
                       xaxp = c(min_,max_,classes),
                       prob = TRUE,
                       main = NULL,
                       ylab = "Densidade",
                       xlab = paste(vars[j],".dados.gerais", sep = ""))

        #curva de densidade

        graphics::lines(stats::density(dados[[vars[j]]]))

        plot_td[[1]] <- grDevices::recordPlot() #atualizar plot geral para retorno na função
      }
  }

  #contruir lista que a função retorna

  retorno <- list("plot geral" = plot_td, "dados plots" = dados_plot, "dados gerais" = dados_geral)

  #testar para gráficos customizados

  if(!(is.null(grafs))){

    graphics::par(mfrow = grade) #definido pelo parâmetro grade passado para a função

    for (i in 1:length(grafs)) {

      #mínimo e máximmo para definir limites do eixo x

      min_ <- min(retorno[[2]][[grafs[i]]])
      max_ <- max(retorno[[2]][[grafs[i]]])

      #numero de classes definido pela regra de sturges

      classes <- grDevices::nclass.Sturges(retorno[[2]][[grafs[i]]])

      #encontrar passos e definir vetor de breaks no eixo x

      passo <- (max_-min_)/classes
      vetorBrk <- seq(min_, max_, by=passo)

      #construir histogramas que serão exibidos

      graphics::hist(retorno[[2]][[grafs[i]]],
                     breaks = vetorBrk,
                     xaxp = c(min_,max_,classes),
                     prob = TRUE,
                     main = NULL,
                     ylab = "Densidade",
                     xlab = paste(names(retorno[[2]][grafs[i]]), sep = ""))

      #curva de densidade

      graphics::lines(stats::density(retorno[[2]][[grafs[i]]]))

    }

    #teste do parâmerto titulo

    if( titulo != FALSE){

      if(titulo == TRUE){

        graphics::mtext(paste("Eventos","|", condicional, sep = ""), side = 3, line = -2, outer = TRUE) #título geral

      }else{

        graphics::mtext(titulo, side = 3, line = -2, outer = TRUE) #título personalizado

      }
    }
  }

  #retorno da função

  return(invisible(retorno))
}
