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
#' @return Lista com Plots de histogramas e curvas de densidade
#'
#' @export
#'
#' @examples
#'
#' dens.descritiva(iris, "Species", c("Petal.Length", "Sepal.Length"))
#'
dens.descritiva <- function(dados, condicional, vars){

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

  ind <- 1 #iniciar contador que define quando armazenar o plot

  #preparar saída com todos os gráficos na mesma janela

  graphics::par(mfrow = c(nrow(priori),length(vars)))

  #primeiro for para navegar entre priores

  for (i in 1:nrow(priori)) {

    #dados_temp é um df temporário que assume os dados de cada priori por ciclo do primeiro for

    dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

    #já o segundo acessa as colunas com os valores utilizados para construção do histograma

    for (j in 1:length(vars)) {

      #iniciar rotina para armazenar informções do histograma sem o plotar

      grDevices::win.metafile()        #passo 1
      grDevices::dev.control('enable') #passo 2

      #apoio_graf é um objeto de apoio para acessar número classes pela regra de
      #sturges e definir passo para break no eixo x nclass.Sturges não funcionou

      apoio_graf <- graphics::hist(dados_temp[[vars[j]]], prob=TRUE) #passo 3
      grDevices::dev.off()             #passo 4

      apoio_graf <- as.data.frame(summary(apoio_graf)) #funciona melhor como df

      #mínimo e máximmo para definir limites do eixo x

      min_ <- min(dados_temp[[vars[j]]])
      max_ <- max(dados_temp[[vars[j]]])

      #numero de classes retirado de apoio_graf

      classes <- as.integer(apoio_graf[2,3])

      #encontrar passos e definir vetor de breaks no eixo x

      passo <- (max_-min_)/classes
      vetorBrk <- seq(min_, max_, by=passo)

      #construir histogramas que serão exibidos

      graphics::hist(dados_temp[[vars[j]]],
                breaks = vetorBrk,
                xaxp = c(min_,max_,classes),
                prob = TRUE,
                main = NULL,
                ylab = "Densidade",
                xlab = paste(vars[j],' | ', priori[i, 1], sep = ""))

      #curva de densidade

      graphics::lines(stats::density(dados_temp[[vars[j]]]))

      #na última repetição, armazenar plotagem geral para retorno da função

      if(nrow(priori)*length(vars) == ind){
        graphics::mtext(paste("Evento(s)",' | ', condicional, sep = ""), side = 3, line = -2, outer = TRUE) #título geral
        plot_td[[1]] <- grDevices::recordPlot() #armazenar plot geral para retorno na função
        names(plot_td)[1] <- "plot geral" #renomear coluna para identificar melhor o retorno
      }
      ind <- ind+1
    }
  }

  # a próxima etapa apenas armazena os gráficos na lista gráficos

  # iniciar lista que vai armazenar gráficos individuais

  graficos <- list()

  #iniciar contador que permite navegar pela lista

  ind <- 1

  #reseta janela de saida para apenas um plot

  graphics::par(mfrow = c(1,1))

  #primeiro for para navegar entre priores

  for (i in 1:nrow(priori)) {

    dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

    #já o segundo acessa as colunas com os valores utilizados para construção dos histogramas

    for (j in 1:length(vars)) {

      #armazenar os gráficos sem plotar na janela. indicações passo a passo

      grDevices::win.metafile()        #passo 1
      grDevices::dev.control('enable') #passo 2
      graphics::par(bg="white")        #passo 3

      #apoio_graf é um objeto de apoio para acessar número classes pela regra de
      #sturges e definir passo para break no eixo x nclass.Sturges não funcionou

      apoio_graf <- graphics::hist(dados_temp[[vars[j]]], prob=TRUE) #passo 4
      grDevices::dev.off()             #passo 5

      apoio_graf <- as.data.frame(summary(apoio_graf)) #funciona melhor como df

      #mínimo e máximmo para definir limites do esxo x

      min_ <- min(dados_temp[[vars[j]]])
      max_ <- max(dados_temp[[vars[j]]])

      #numero de classes retirado de apoio_graf

      classes <- as.integer(apoio_graf[2,3])

      #encontrar passos e definir vetor de breaks no eixo x

      passo <- (max_-min_)/classes
      vetorBrk <- seq(min_, max_, by=passo)

      #armazenar os gráficos sem plotar na janela. indicações passo a passo

      grDevices::win.metafile()                #passo 1
      grDevices::dev.control('enable')         #passo 2
      graphics::par(bg="white")                #passo 3

      #construir histograma

      graphics::hist(dados_temp[[vars[j]]],    #passo 4
                     breaks=vetorBrk,
                     xaxp=c(min_,max_,classes),
                     prob=TRUE,
                     main=NULL,
                     ylab="Densidade",
                     xlab=paste(vars[j],' | ', priori[i, 1], sep = ""))

      #curva de densidade

      graphics::lines(stats::density(dados_temp[[vars[j]]])) #passo 5

      graficos[[ind]] <- grDevices::recordPlot() #passo 6 armazenar o plot na lista graficos

      grDevices::dev.off()                     #passo 7

      #construindo nome das colunas para cada A|B

      colnome <- paste(vars[j], "|", as.character(priori[i,1]))

      #renomear coluna com colnome definindo acima

      names(graficos)[ind] <- colnome

      ind <- ind + 1 #incrementar contador de acesso à lista gráficos
    }
  }

  #contruir lista que a função retorna

  retorno <- list("graficos" = graficos, "plot geral" = plot_td)

  return(invisible(retorno))
}
