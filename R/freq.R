#' Tabelas de frequencia e graficos de barra
#'
#' Lê conjunto de dados e retorna uma tabela de frequência com observações, frequência absoluta e proporcional
#'
#' @param dados Dataframe com ao menos duas colunas, uma com a informação a priori e outra(s) com valores para plotar histograma e boxplot
#'
#' @param condicional Coluna em que está a informação conhecida
#'
#' @param classe Caso queira definir as classes que deseja analisar
#'
#' @param vars Lista com nomes das colunas em que estão os valores com os quais serão plotados o gráfico de barras e construida a tabela de frequência
#'
#' @param dg  Lógico, default = FALSE, TRUE para exibir resumo e gráficos incluindo dados gerais
#'
#' @param tabs Lógico, default = FALSE, TRUE para exibir tabelas de frequência quando a função é executada
#'
#' @param grafs vetor c(1:n) indicando os gráficos que serão exibidos com o plot customizado, o índice do gráfico corresponde a sua posição no plot geral que a função retorna
#'
#' @param grade vetor c(x, y) indicando a quantidade linhas e colunas em que serão impressos os gráficos customizados definos pelo parâmetro grafs
#'
#' @param titulo Lógico ou string, default = FALSE, TRUE para exibir título no plot geral no formato evento|condicional ou string no formato "título customizado"
#'
#' @return Lista com as tabelas de frequência de cada evento A dado B
#'
#' @export
#'
#' @examples
#'
#' mtcars_ <- mtcars
#' mtcars_$am[mtcars_$am == 0] <- "automatico"
#' mtcars_$am[mtcars_$am == 1] <- "manual"
#' freq.descritiva(mtcars_, c("cyl", "gear"), "am")
#'
freq.descritiva <- function(dados,
                            vars,
                            condicional,
                            classe = NULL,
                            dg = FALSE,
                            tabs = FALSE,
                            grafs = NULL,
                            grade = c(1,1),
                            titulo = FALSE){

  #limitar dataframe às informações de interesse

  dados <- subset(dados, select = c(vars, condicional))

  #definir valor máximo de y para todos gráficos na mesma escala

  y_max = 0

  for (i in 1:length(vars)) {
    y_max_ <- max(table(dados[i]))
    if(y_max_ > y_max){ y_max <- y_max_}
  }

  #iniciar tabela geral para armazenar no retorno

  tab_geral <- as.data.frame(dados[,c(vars, condicional)])

  #criar dataframe com a informação a priori

  priori <- as.data.frame(table(dados[[condicional]]))

  #caso a classe seja definida

  if(!is.null(classe)){
    priori <- dplyr::filter(priori, priori[[1]] %in% classe)
  }

  #iniciar variaveis de retorno onde estarão tabelas de frequências e gráficos respectivamente

  saida <- list()       #lista com tabelas de frequência
  tab_graf <- list()  #lista com tabelas de cada gráfico
  plot_td <- list()     #lista com plot de todos os gráficos juntos

  #contador que permite navegar pelas listas

  ind <- 1 #iniciar contador para navegar na lista saida

  #preparar saída com todos os gráficos na mesma janela
  #teste para exibição de dados gerais

  if(dg == TRUE){

    graphics::par(mfrow = c(nrow(priori)+1, length(vars)))

    for(i in 1:length(vars)){

      #objetos auxiliares apenas para facilitar

      plot_aux <- table(dados[vars[i]])
      freq_aux <- as.data.frame(plot_aux)
      names(freq_aux)[1] <- vars[i]     #renomear colunas da tabela de frequência
      names(freq_aux)[2] <- "Freq_abs"  #renomear colunas da tabela de fre
      freq_aux$Freq_rel <- format(round(freq_aux$Freq_abs/sum(freq_aux$Freq_abs), 4), nsmall = 4)

      #adicionar observações gerais em suas respectivas listas

      saida[[ind]] <- freq_aux #lista de saida armazena cada tabela de frequência
      tab_graf[[ind]] <- plot_aux #lista que armazenara as tabelas prontas para plotar gráficos

      #construindo nome das colunas para cada A|B

      colnome <- paste(vars[i], ".dados.gerais", sep = "")

      #renomear coluna com colnome definindo acima

      names(saida)[ind] <- colnome
      names(tab_graf)[ind] <- colnome

      ind <- ind + 1 #incrementar contador das listas saida e tab_graf

      #definir limite do plot para não cortar valores nas barras

      y_lim <- c (0, 1.25*(y_max))

      #plotar e armazenar barplot na variável plotar, importante para colocar valores nas barras

      plotar <- graphics::barplot(plot_aux, ylim = y_lim)
      graphics::title(xlab = (paste(vars[i], ".dados.gerais", sep = "")), line = 2.5) #xlabel
      graphics::text(x = plotar,   #plotar frequência absoluta na barra
                     y = as.data.frame(plot_aux)[[2]],
                     label = as.data.frame(plot_aux)[[2]],
                     pos = 3,
                     cex = 0.9,
                     col = "black",
                     offset = 0.35)

      graphics::text(x = plotar,   #plotar frequência relativa na barra
                     y = as.data.frame(plot_aux)[[2]],
                     label = round(as.data.frame(plot_aux)[[2]]/sum(as.data.frame(plot_aux)[[2]]), 2),
                     pos = 1,
                     cex = 0.9,
                     col = "black",
                     offset = 0.35)
    }

  }else{

    graphics::par(mfrow = c(nrow(priori), length(vars)))

  }

  #a seguir o primeiro for() acessa informações de cada condicional por vez para plotar e armazenar gráfico geral

  for (i in 1:nrow(priori)) {

    #filtro de cada informação conhecida

    dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

    for (j in 1:length(vars)) {

      tab_ <- table(dados_temp[[j]])    #tabela que será utilizada para gráfico
      tab_freq <- as.data.frame(tab_)   #tabela para imprimir tabela de frequências

      names(tab_freq)[1] <- vars[j]     #renomear colunas da tabela de frequência
      names(tab_freq)[2] <- "Freq_abs"  #renomear colunas da tabela de frequência

      #inserir coluna de frequência relativa da tabela de frequência

      tab_freq$Freq_rel <- format(round(tab_freq$Freq_abs/sum(tab_freq$Freq_abs), 4), nsmall = 4)

      saida[[ind]] <- tab_freq #lista de saida armazena cada tabela de frequência
      tab_graf[[ind]] <- tab_ #lista que armazenara as tabelas prontas para plotar gráficos

      #construindo nome das colunas para cada A|B

      colnome <- paste(vars[j], "|", as.character(priori[i,1]), sep = "")

      #renomear coluna com colnome definindo acima

      names(saida)[ind] <- colnome
      names(tab_graf)[ind] <- colnome

      #definir limite do plot para não cortar valores nas barras

      y_lim <- c (0, 1.25*(y_max))

      #plotar e armazenar barplot na variável plotar, importante para colocar valores nas barras

      plotar <- graphics::barplot(tab_, ylim = y_lim)
      graphics::title(xlab = (paste(vars[j], "|", priori[i, 1], sep = "")), line = 2.5) #xlabel
      graphics::text(x = plotar,   #plotar frequência absoluta na barra
                     y = as.data.frame(tab_)[[2]],
                     label = as.data.frame(tab_)[[2]],
                     pos = 3,
                     cex = 0.9,
                     col = "black",
                     offset = 0.35)
      graphics::text(x = plotar,   #plotar frequência relativa na barra
                     y = as.data.frame(tab_)[[2]],
                     label = round(as.data.frame(tab_)[[2]]/sum(as.data.frame(tab_)[[2]]), 2),
                     pos = 1,
                     cex = 0.9,
                     col = "black",
                     offset = 0.35)

      ind <- ind + 1 #incrementar contador para navegar nas listas saida e tab_graf
    }

    #título do plot geral

    graphics::mtext(paste("Evento(s)", "|", condicional, sep = ""),
                    side = 3,
                    line = -2,
                    outer = TRUE)

    plot_td[[1]] <- grDevices::recordPlot() #armazenar plot geral para retorno na função
    names(plot_td)[1] <- "plot geral" #renomear coluna para identificar melhor o retorno
  }

  #organizar lista que a função retorna

  retorno <- list("tabelas de frequencia" = saida,
                  "tabelas para graficos" = tab_graf,
                  "plot geral"= plot_td,
                  "tabela geral" = tab_geral)

  #testar se há gráficos personalizados

  if(!(is.null(grafs))){

    #iniciar lista que vai armazenar plots

    plot_personal <- list()

    #definição de layout para plot personalizado

    graphics::par(mfrow = grade)

      for(i in 1:length(grafs)){

        #definir limite do plot para não cortar valores nas barras

        y_lim <- c (0, 1.25 * (y_max))

        #plotar e armazenar barplot na variável plotar, importante para colocar valores nas barras

        plotar <- graphics::barplot(retorno[[2]][[grafs[i]]], ylim = y_lim)
        graphics::title(xlab = (paste(names(retorno[[2]][grafs[i]]))), line = 2.5) #xlabel
        graphics::text(x = plotar,   #plotar frequência absoluta na barra
                       y = retorno[[2]][[grafs[i]]],
                       label = retorno[[2]][[grafs[i]]],
                       pos = 3,
                       cex = 0.9,
                       col = "black",
                       offset = 0.35)
        graphics::text(x = plotar,   #plotar frequência relativa na barra
                       y = retorno[[2]][[grafs[i]]],
                       label = round(retorno[[2]][[grafs[i]]]/sum(retorno[[2]][[grafs[i]]]), 4),
                       pos = 1,
                       cex = 0.9,
                       col = "black",
                       offset = 0.35)

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

    #atualizar lista que a função retorna

    retorno <- list("tabelas de frequencia" = saida,
                    "tabelas para graficos" = tab_graf,
                    "plot geral"= plot_td,
                    "tabela geral" = tab_geral,
                    "plot personalizado" = plot_personal)
  }

  #imprimi tabelas de ferquência se o argumento tabs = TRUE

  if(tabs){print(retorno[[1]])}

  #resetar par(mfrow=c())

  graphics::par(mfrow = c(1,1))

  #retorno da função

  return(invisible(retorno))
}
