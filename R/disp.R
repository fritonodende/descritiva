#' Diagramas de dispersão a partir de uma informação conhecida
#'
#' Essa função lê um conjunto de dados e plota diagramas de dispersão para observação do comportamento de uma determinada relação entre variáveis de acordo com informações conhecidas a priori
#'
#' @param dados Dataframe com ao menos três colunas, uma com a informação a priori e no mínimo outras duas com valores para coordenadas x e y
#'
#' @param condicional Coluna em que está a informação conhecida (priori)
#'
#' @param classe Caso queira definir as classes que deseja analisar
#'
#' @param vars Lista com os nomes das duas colunas ( x e y, nessa ordem) que formarão as coordenadas do plano cartesiano
#'
#' @param grafs vetor c(1:n) indicando os gráficos que serão exibidos com o plot customizado, o índice do gráfico corresponde a sua posição no plot geral que a função retorna
#'
#' @param grade vetor c(x, y) indicando a quantidade linhas e colunas em que serão impressos os gráficos customizados definos pelo parâmetro grafs
#'
#' @param titulo Lógico ou string, default = FALSE, TRUE para exibir título no plot geral no formato evento|condicional ou string no formato "título customizado"
#'
#' @return A função retorna uma lista com diagramas de dispersão combinados e individuais do conjunto de dados observado bem como as tabelas definidas por cada priori
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
#' disp.descritiva(iris, c("Sepal.Length", "Sepal.Width"), "Species")
#'
#' #Exemplo 2:
#' #função recebe: dataset mtcars,
#' #               info a priori "cyl"
#' #               colunas para coordenadas x e y "mpg" e "qsec"
#'
#' disp.descritiva(mtcars, c("mpg", "qsec"), "cyl")
#'
disp.descritiva <- function(dados,
                            vars,
                            condicional,
                            classe = NULL,
                            grafs = NULL,
                            grade = c(1,1),
                            titulo = FALSE){

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

  #garantir que a coluna com a informação conhecida é fator ou dá erro com priori numérica

  dados[[3]]<-as.factor(dados[[3]])

  #criar dataframe com a informação a priori

  priori <- as.data.frame(table(dados[[condicional]]))

  #caso a classe seja definida

  if(!is.null(classe)){
    priori <- dplyr::filter(priori, priori[[1]] %in% classe)
  }

  #definir limites eixos x e y

  lim_min_x <- min(dados[[vars[1]]])
  lim_max_x <- max(dados[[vars[1]]])
  lim_min_y <- min(dados[[vars[2]]])
  lim_max_y <- max(dados[[vars[2]]])

  #iniciar listas que serão parâmetros dos gráficos

  shapes <- vector()          #formas
  config_legenda <- vector()  #legenda

  #for preenche listas de configuração

  for (i in 1:nrow(priori)) {
    shapes[i] <- i+14
    config_legenda[i] <- i+14
  }

  #configuração de shapes (se passar sem esses parâmetros não funciona)

  shapes <- shapes[as.numeric(dados[[3]])]

  #iniciar listas que vão armazenar plot geral e tabelas

  plot_td <- list()
  tabelas <- list()

  #preparar saída para gráfico geral

  graphics::par(mfrow = c(1, nrow(priori)+1))

  #plotar o diagrama de dispersão geral

  graphics::plot(x = dados[[1]], y = dados[[2]],      #plot do gráfico
                 main = paste(vars[1], ' x ', vars[2],' | ', condicional, sep = ""),
                 font.main = 1,
                 cex.main = .99,
                 xlim = c(lim_min_x,lim_max_x),
                 ylim = c(lim_min_y,lim_max_y),
                 xlab = vars[1],
                 ylab = vars[2],
                 col = as.integer(dados[[3]]) + 1,
                 pch = shapes,
                 cex = 1)

  #construção da legenda e título geral

   graphics::legend("bottomright",
                   adj = 0,
                   cex = .8,
                   legend = c(priori[[1]]),
                   col = as.integer(config_legenda)+nrow(priori),
                   pch = as.integer(config_legenda))

  #armazenar plot geral

  tabelas[[1]] <- dados   #lista com tabelas utilizadas para construção de cada gráfico
  names(tabelas)[1] <- "plot geral" #renomear coluna para identificar melhor o retorno

  #for para construir os gráficos de cada condicional por vez

  for (i in 1:nrow(priori)) {

    #filtrar dados de interesse

    dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

    graphics::plot(x = dados_temp[[1]], y = dados_temp[[2]],
                   main = paste(vars[1], ' x ', vars[2],' | ', priori[i, 1], sep = ""),
                   font.main = 1,
                   cex.main = .99,
                   xlim = c(lim_min_x,lim_max_x),
                   ylim = c(lim_min_y,lim_max_y),
                   xlab = vars[1],
                   ylab = vars[2],
                   col = as.integer(config_legenda[i])+nrow(priori),
                   pch = i+14,
                   cex = 1)

    #construção e atribuição das colunas das listas iniciadas anteriormente

    colnome <- paste(paste(vars[1], ' x ', vars[2], ' | ', priori[i, 1], sep = ""))
    tabelas[[i+1]] <- dados_temp
    names(tabelas)[i+1] <- colnome
  }

  graphics::mtext(paste("Evento A x Evento B"," | ", condicional, sep = ""),   #título geral
                  side = 3, line = -1.4, outer = TRUE)

  plot_td[[1]] <- grDevices::recordPlot() #armazenar plot geral para retorno na função
  names(plot_td)[1] <- "plot geral" #renomear coluna para identificar melhor o retorno

  #preparar lista que a função retorna

  retorno<-list("tabelas" = tabelas, "plot geral"=plot_td)

  #ajustes para gráfico customizado, parametro graf definido pelo usuário

  if(!(is.null(grafs))){

    #iniciar lista que vai armazenar plots

    plot_personal <- list()

    graphics::par(mfrow = grade)

    #testar se na lista de gráficos personalizados há o grafico geral

    if("1" %in% grafs){ #if para exibir gráfico geral quando solicitado

      index<-match(c(1), grafs)

      graphics::plot(x = (unlist(retorno[[1]][[grafs[index]]][1])), y = (unlist(retorno[[1]][[grafs[index]]][2])),
                     main = paste(names(retorno[[1]][grafs[index]]), sep = ""),
                     font.main = 1,
                     cex.main = .99,
                     xlim = c(lim_min_x,lim_max_x),
                     ylim = c(lim_min_y,lim_max_y),
                     xlab = vars[1],
                     ylab = vars[2],
                     col = as.integer(dados[[3]]) + 1,
                     pch = shapes,
                     cex = 1)

      graphics::legend("bottomright",
                       adj = 0,
                       cex = .8,
                       x.intersp = .80,
                       y.intersp = .80,
                       legend = c(priori[[1]]),
                       col = as.integer(config_legenda)+nrow(priori),
                       pch = as.integer(config_legenda))


      plot_personal[[1]] <- grDevices::recordPlot() #armazenar plot geral para retorno na função
      names(plot_personal)[1] <- names(retorno[[1]][1]) #renomear coluna para identificar melhor o retorno

      #remover solicitação do gráfico geral da lista grafs

      grafs<-grafs[-index]
    }

    #ajuste mas listas shapes e grafs para manter mesmas figuras pch e cores em todos os gráficos

    shapes <- shapes[!duplicated(shapes)]
    grafs_aux<-grafs-1
    teste<-as.numeric(dados[[3]])
    index<-which(teste %in% grafs_aux)
    graf.color<-teste[index]
    graf.color <- graf.color[!duplicated(graf.color)]

    #plotar demais gáficos personalizados

    for(i in 1:length(grafs)){

      graphics::plot(x = (unlist(retorno[[1]][[grafs[i]]][1])), y = (unlist(retorno[[1]][[grafs[i]]][2])),
                     main = paste(names(retorno[[1]][grafs[i]]), sep = ""),
                     font.main = 1,
                     cex.main = .99,
                     xlim = c(lim_min_x,lim_max_x),
                     ylim = c(lim_min_y,lim_max_y),
                     xlab = vars[1],
                     ylab = vars[2],
                     col = as.integer(graf.color[i]+1),
                     pch = shapes[grafs[i]-1],
                     cex = 1)

      plot_personal[[i]] <- grDevices::recordPlot() #armazenar plot para retorno na função
      names(plot_personal)[i] <- names(retorno[[1]][i]) #renomear coluna para identificar melhor o retorno
    }

    #teste para inserir título

    if( titulo != FALSE){

      if(sum(grade) > 2){

        if (is.logical(titulo)){

            graphics::mtext(paste("Eventos A x B"," | ", condicional, sep = ""),
                            side = 3, line = -1.4, outer = TRUE) #título geral padrão
         }else{

            graphics::mtext(titulo, side = 3, line = -1.4, outer = TRUE) #título geral personalizado
        }
      }
    }

    retorno<-list("tabelas" = tabelas,
                  "plot geral"=plot_td,
                  "plot personalizado" = plot_personal)

  }

  return(invisible(retorno))

}
