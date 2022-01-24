#' Diagramas de dispersão a partir de uma informação conhecida
#'
#' Essa função lê um conjunto de dados e plota diagramas de dispersão para observação do comportamento de uma determinada relação entre variáveis de acordo com informações conhecidas a priori
#'
#' @param dados Dataframe com ao menos três colunas, uma com a informação a priori e no mínimo outras duas com valores para coordenadas x e y
#'
#' @param condicional Coluna em que está a informação conhecida (priori)
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
#' disp.descritiva(iris, "Species", c("Sepal.Length", "Sepal.Width"))
#'
#' #Exemplo 2:
#' #função recebe: dataset mtcars,
#' #               info a priori "cyl"
#' #               colunas para coordenadas x e y "mpg" e "qsec"
#'
#' disp.descritiva(mtcars, "cyl", c("mpg", "qsec"))
#'
disp.descritiva <- function(dados, condicional, vars, grafs = NULL, grade = c(1,1), titulo = FALSE){

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

  #definir limites eixos x e y

  lim_min_x <- min(dados[[vars[1]]])
  lim_max_x <- max(dados[[vars[1]]])
  lim_min_y <- min(dados[[vars[2]]])
  lim_max_y <- max(dados[[vars[2]]])

  #iniciar listas que serão parâmetros dos gráficos

  shapes <-vector()           #formas
  config_legenda <- vector()  #cores

  #for preenche listas de configuração

  for (i in 1:nrow(priori)) {
    shapes[i] <- i+14
    config_legenda[i] <- i+14
  }

  #configuração de shapes (se passar sem esses parâmetros não funciona)

  shapes <- shapes[as.numeric(dados[[3]])]

  #iniciar lista que vai armazenar plot geral

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
                 col = as.integer(dados[[3]])+1,
                 pch = shapes,
                 cex = 1.5)

  #construção da legenda e título geral

  graphics::legend("bottomright", title = condicional, legend = levels(dados[[3]]), col = as.integer(config_legenda)+nrow(priori), pch = as.integer(config_legenda)) #legenda
  plot_td[[1]] <- grDevices::recordPlot() #armazenar plot geral para retorno na função
  names(plot_td)[1] <- "plot geral" #renomear coluna para identificar melhor o retorno
  tabelas[[1]] <- dados   #lista com tabelas utilizadas para construção de cada gráfico
  names(tabelas)[1] <- "plot geral" #renomear coluna para identificar melhor o retorno

  #for para construir um gráfico por vez

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
                   cex = 1.5)

    #contrução e atribuição das colunas das listas ciniciadas anteriormente

    colnome <- paste(paste(vars[1], ' x ', vars[2],' | ', priori[i, 1], sep = ""))
    tabelas[[i+1]]<-dados_temp
    names(tabelas)[i+1] <- colnome
  }

  #preparar lista que a função retorna

  retorno<-list("tabelas" = tabelas, "plot geral"=plot_td)

  if(!(is.null(grafs))){

    graphics::par(mfrow = grade)

    for(i in 1:length(grafs)){

      graphics::plot(x = (unlist(retorno[[1]][[grafs[i]]][1])), y = (unlist(retorno[[1]][[grafs[i]]][2])), #passo 4
                     main = paste(names(retorno[[1]][grafs[i]]), sep = ""),
                     font.main = 1,
                     cex.main = .99,
                     xlim = c(lim_min_x,lim_max_x),
                     ylim = c(lim_min_y,lim_max_y),
                     xlab = vars[1],
                     ylab = vars[2],
                     col = as.integer(dados[[3]])+i,
                     pch = shapes+i,
                     cex = 1.5)
    }

    if( titulo != FALSE){
      if(titulo == TRUE)
      {
        graphics::mtext(paste("Eventos A x B","|", condicional, sep = ""), side = 3, line = -1.4, outer = TRUE) #título geral
      }else{
        graphics::mtext(titulo, side = 3, line = -1.4, outer = TRUE) #título geral
      }
    }
  }
  return(invisible(retorno))
}
