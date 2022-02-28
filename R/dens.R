#' Sobreposicao de histograma e curva da funcao de densidade
#'
#' Lê um conjunto de dados e plota a sobreposição do seu histograma e curva de densidade para observar o comportamento da distribuição dos dados
#'
#' @param dados Dataframe com ao menos duas colunas, uma com a informação a priori e outra(s) com valores para plotar histograma e boxplot
#'
#' @param vars Lista com nomes das colunas em que estão os valores com os quais serão plotados o histograma e curva de densidade
#'
#' @param condicional Coluna em que está a informação conhecida
#'
#' @param classe Caso queira definir as classes que deseja analisar
#'
#' @param dg  Lógico, default = FALSE, TRUE para exibir resumo e gráficos incluindo dados gerais
#'
#' @param fc.densidade Função densidade de probabilidade, defalut = "ASGNB". fc.densidade = "KDE" para Kernel density estimation
#'
#' @param grafs vetor c(1:n) indicando os gráficos que serão exibidos com o plot customizado, o índice do gráfico corresponde a sua posição no plot geral que a função retorna
#'
#' @param grade vetor c(x, y) indicando a quantidade linhas e colunas em que serão impressos os gráficos customizados definos pelo parâmetro grafs
#'
#' @param titulo Lógico ou string, default = FALSE, TRUE para exibir título no plot geral no formato evento|condicional ou string no formato "título customizado"
#'
#' @param fonte.orientacao formatação da orientação do valores impressos no eixo x, default = "H" para valores na horizontal, ou seja, paralelos ao eixo x. fonte.orientacao = "V" para valores na vertical, ou seja, perpendiculares ao eixo x.
#'
#' @param fonte.tamanho formatação dos tamanho dos valores impressos no eixo x
#'
#' @return Lista com Plots de histogramas e curvas de densidade
#'
#' @export
#'
#' @examples
#'
#' dens.descritiva(iris, c("Petal.Length", "Sepal.Length"), "Species")
#'
dens.descritiva <- function(dados,
                            vars,
                            condicional,
                            classe = NULL,
                            dg = FALSE,
                            fc.densidade = "ASGNB",
                            grafs = NULL,
                            grade = c(1,1),
                            titulo = FALSE,
                            fonte.tamanho = 1,
                            fonte.orientacao = "H"){

  #confirmar se objeto enviado para função é um df

  if (is.data.frame(dados) == FALSE){
    stop("A função descritiva() só recebe dataframe")
  }

  #transformar os valores do parâmetro fonte.orientação de H e V, mais intuitivos, para valores reconhecidos pelas funções que irão recebê-lo

  if (fonte.orientacao == "H"){
    fonte.orientacao <- 1
  }else{
    fonte.orientacao <- 2
  }

  #limitar dataframe as informações de interesse

  dados <- subset(dados, select = c(vars, condicional))

  #criar dataframe com a informação a priori

  priori <- as.data.frame(table(dados[[condicional]]))

  #caso a classe seja definida

  if(!is.null(classe)){
    priori <- dplyr::filter(priori, priori[[1]] %in% classe)
  }

  #iniciar lista que vai armazenar plot de todos os gráficos juntos

  plot_td <- list()

  #iniciar e armazenar lista com dados gerais

  dados_geral <- list()
  dados_geral[[1]] <- dados
  names(dados_geral)[[1]] <- "dados.gerais"

  #iniciar lista que vai armazenar dados para plots customizados

  dados_plot <- list()

  ind <- 1 #iniciar contador do índice da lista que armazena os dados para plots

  min_max <- vector() #iniciar vetor que vai armazenar valores para definir min e max eixo y boxplot

  #concatenar tds os valores das variáveis observadas para definir limites no eixo x

  for (i in 1:length(vars)) {
    min_max <- rbind(min_max,dados[[vars[i]]])
  }
  lim_min_x <- min(min_max)
  lim_max_x <- max(min_max)

  #definir limite maximo do eixo y

  dmax = 0

  for (i in 1:nrow(priori)) {

    dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

    for (j in 1:length(vars)) {

      brks <- grDevices::nclass.Sturges(dados_temp[[vars[j]]])

      dscrto <- as.data.frame(table(arules::discretize(dados_temp[[vars[j]]],
                                                       breaks = brks,
                                                       method = "interval")))

      #frequência relativa do intervalo com maior densidade

      frel <- max(dscrto[[2]]) / nrow(dados_temp)

      #isolar e manipular string para tranformar em valor e calcula intervalo da classe

      intervalo <- dplyr::filter(dscrto, dscrto$Freq == max(dscrto[[2]])) #isolar linha

      if(substr(intervalo[[1]][1], nchar(intervalo), nchar(intervalo)) != "]"){ #teste pq último intervalo é fechado (tem um símbolo diferente)

        intervalo <- as.numeric(qdapRegex::ex_between(intervalo[[1]], c("[", ","), c(",", ")"))[[1]]) #captura string entre caracteres

      }else{

        intervalo <- as.numeric(qdapRegex::ex_between(intervalo[[1]], c("[", ","), c(",", "]"))[[1]]) #captura string entre caracteres

      }

      #calcular densidade

      dens <- frel / (intervalo[2] - intervalo[1])

      #testar para armazenar densidade máxima

      if(dens > dmax){

        dmax <- dens

      }
    }
  }

  #preparar saída com todos os gráficos na mesma janela

  if(dg == TRUE){   #teste para plotar dados gerais, com ou sem interfere no mfrow

    graphics::par(mfrow = c(nrow(priori)+1,length(vars)))

    #for para percorrer lista de dados dos plots

    for (j in 1:length(vars)) {

      #teste para fc densidade ASGN ou KDE

      if (fc.densidade == "ASGNB"){

        descritiva::asg.est(dados, vars[j], dg = dg, de_densidade = c(lim_min_x, lim_max_x, dmax, fonte.orientacao))

        }else{

          #mínimo e máximmo para definir limites do eixo x

          min_ <- min(dados[[vars[j]]])
          max_ <- max(dados[[vars[j]]])

          #número de classes definido pela regra de sturges

          nclasses <- grDevices::nclass.Sturges(dados[[vars[j]]])

          #encontrar passos e definir vetor de breaks no eixo x

          passo <- (max_ - min_)/nclasses
          vetorBrk <- seq(min_, max_, by = passo)
          vetorBrk <- vetorBrk

          #plotar histograma

          graphics::hist(dados[[vars[j]]],
                        col = NA,
                        border = "blue",
                        breaks = vetorBrk,
                        xaxp = c(min_,max_,nclasses),
                        xaxt = "n",
                        xlim = c(lim_min_x,lim_max_x),
                        ylim = c(0, dmax),
                        prob = TRUE,
                        main = NULL,
                        ylab = "Densidade",
                        xlab = paste0(vars[j]),
                        las = 1,
                        cex.axis = fonte.tamanho)

          graphics::axis(1, at = vetorBrk,
                       labels = format(round(vetorBrk, 2), nsmall = 2),
                       las = fonte.orientacao, cex.axis = fonte.tamanho)

          graphics::axis(1, at = c(lim_min_x,lim_max_x),
                       format(round(c(lim_min_x,lim_max_x), 2), nsmall = 2),
                       las = fonte.orientacao, cex.axis = fonte.tamanho)

          #curva de densidade

          graphics::lines(stats::density(dados[[vars[j]]]), col = "black", lwd = 2)
      }

      #armazenar dados do plot

      dados_plot[[ind]] <- dados[[vars[j]]]

      #renomeando coluna para rápida identificação

      names(dados_plot)[ind]<-paste0(vars[j])

      ind <- ind + 1 #incrementar contador do armazenamento dos dados para plots

    }

  }else{

    graphics::par(mfrow = c(nrow(priori),length(vars)))

  }

  #primeiro for para navegar entre priores e criar um plot geral

  for (i in 1:nrow(priori)) {

    #dados_temp é um df temporário que assume os dados de cada priori por ciclo do primeiro for

    dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

    #já o segundo acessa as colunas com os valores utilizados para construção do histograma

    for (j in 1:length(vars)) {

      #lista recebe dados para o plot

      dados_plot[[ind]] <- dados_temp[[vars[j]]]

      #renomeando coluna para rápida identificação

      names(dados_plot)[ind]<-paste(vars[j],"|", priori[i, 1], sep = "")

      ind <- ind + 1 #atualizar contador da lista de dados dos plots

      #mínimo e máximmo para definir limites do eixo x

      min_ <- min(dados_temp[[vars[j]]])
      max_ <- max(dados_temp[[vars[j]]])

      #número de classes definido pela regra de sturges

      nclasses <- grDevices::nclass.Sturges(dados_temp[[vars[j]]])

      #definição da amplitude de classe e vetor com breaks

      passo <- (max_-min_)/nclasses
      vetorBrk <- seq(min_, max_, by=passo)

      #construir histogramas que serão exibidos

      if (fc.densidade == "ASGNB"){

        descritiva::asg.est(dados_temp, vars[j], condicional, as.character(priori[i, 1]), de_densidade = c(lim_min_x, lim_max_x, dmax, fonte.orientacao))

      }else{

        graphics::hist(dados_temp[[vars[j]]],
                       col = NA,
                       border = "blue",
                       breaks = vetorBrk,
                       xaxp = c(min_,max_,nclasses),
                       xaxt = "n",
                       xlim = c(lim_min_x,lim_max_x),
                       ylim = c(0, dmax),
                       prob = TRUE,
                       main = NULL,
                       ylab = "Densidade",
                       xlab = paste(vars[j],"|", priori[i, 1], sep = ""),
                       las = 1,
                       cex.axis = fonte.tamanho)

        graphics::axis(1, at = vetorBrk,
                       labels = format(round(vetorBrk, 2), nsmall = 2),
                       las = fonte.orientacao,
                       cex.axis = fonte.tamanho)

        graphics::axis(1, at = c(lim_min_x,lim_max_x),
                       format(round(c(lim_min_x,lim_max_x), 2), nsmall = 2),
                       las = fonte.orientacao,
                       cex.axis = fonte.tamanho)

        #curva de densidade

        graphics::lines(stats::density(dados_temp[[vars[j]]]), col = "black", lwd = 2)

      }
    }
  }

  graphics::mtext(paste("Evento(s)","|", condicional, sep = ""), side = 3, line = -2, outer = TRUE) #título geral
  plot_td[[1]] <- grDevices::recordPlot() #armazenar plot geral para retorno na função
  names(plot_td)[1] <- "plot geral" #renomear coluna para identificar melhor o retorno

  #contruir lista que a função retorna

  retorno <- list("plot geral" = plot_td,
                  "dados plots" = dados_plot,
                  "dados gerais" = dados_geral)

  #testar para gráficos customizados

  if(!(is.null(grafs))){

    #iniciar lista que vai armazenar plots

    plot_personal <- list()

    graphics::par(mfrow = grade) #definido pelo parâmetro grade passado para a função

    for (i in 1:length(grafs)) {

      if (fc.densidade == "ASGNB"){

        #dados tempo para gráfico personalizado

        dados_temp <- as.data.frame(unlist(retorno[[2]][grafs[i]]))

        #renomear coluna de dados temp

        names(dados_temp)[1] <- names(retorno[[2]][grafs[i]])

        #nome temp para passar para asg.est

        nome_temp <- names(retorno[[2]][grafs[i]])

        descritiva::asg.est(dados_temp, nome_temp, dg = TRUE, de_densidade = c(lim_min_x, lim_max_x, dmax, fonte.orientacao))

      }else{

        #mínimo e máximmo para definir limites do eixo x

        min_ <- min(retorno[[2]][[grafs[i]]])
        max_ <- max(retorno[[2]][[grafs[i]]])

        #numero de classes definido pela regra de sturges

        classes <- grDevices::nclass.Sturges(retorno[[2]][[grafs[i]]])

        #encontrar passos e definir vetor de breaks no eixo x

        passo <- (max_-min_)/nclasses
        vetorBrk <- seq(min_, max_, by=passo)

        graphics::hist(retorno[[2]][[grafs[i]]],
                       col = NA,
                       border = "blue",
                       breaks = vetorBrk,
                       xaxp = c(min_,max_,nclasses),
                       xaxt = "n",
                       xlim = c(lim_min_x, lim_max_x),
                       ylim = c(0, dmax),
                       prob = TRUE,
                       main = NULL,
                       ylab = "Densidade",
                       las = 1,
                       cex.axis = fonte.tamanho,
                       xlab = paste(names(retorno[[2]][grafs[i]]), sep = ""))

        #curva de densidade

        graphics::lines(stats::density(retorno[[2]][[grafs[i]]]), lwd = 2)

        graphics::axis(1, at = vetorBrk, labels = format(round(vetorBrk, 2), nsmall = 2),
                      las = fonte.orientacao,
                      cex.axis = fonte.tamanho)
        graphics::axis(1, at = c(lim_min_x,lim_max_x),
                      format(round(c(lim_min_x,lim_max_x), 2), nsmall = 2),
                      las = fonte.orientacao,
                      cex.axis = fonte.tamanho)
      }

      #teste para inserir título nos gráficos personalizados

      if(sum(grade) == 2 && titulo != FALSE){

        if(titulo == TRUE){

          graphics::mtext(paste("Eventos","|", condicional, sep = ""),  #título automatico
                          side = 3,
                          line = -2,
                          outer = TRUE)

        }else{

          graphics::mtext(titulo, side = 3, line = -2, outer = TRUE) #título personalizado

        }
      }

      if(sum(grade) == 2){ #testar se plots são individuais para captura de um por vez

        plot_personal[[i]] <- grDevices::recordPlot() #armazenar cada plot para retorno na função
        names(plot_personal)[i] <- names(retorno[[2]][grafs[i]]) #renomear coluna para identificar melhor o retorno

       }
    }

    #teste do parâmerto titulo/criar título geral para o caso de multiplos plots na mesma janela

    if (sum(grade) !=2){

      if( titulo != FALSE){

        if(titulo == TRUE){

          graphics::mtext(paste("Eventos","|", condicional, sep = ""), side = 3, line = -2, outer = TRUE) #título geral

        }else{

          graphics::mtext(titulo, side = 3, line = -2, outer = TRUE) #título geral personalizado

        }
      }

      plot_personal[[1]] <- grDevices::recordPlot() #atualizar captura com título geral

    }

    names(plot_personal)[1] <- "plot personalizado" #renomear coluna para identificar melhor o retorno

    #atualização retorno

    retorno <- list("plot geral" = plot_td,
                    "dados plots" = dados_plot,
                    "dados gerais" = dados_geral,
                    "plot personalizado" = plot_personal)

  }

  #resetar par(mfrow)

  graphics::par(mfrow=c(1, 1))

  #retorno da função

  return(invisible(retorno))
}
