#' Tabela de frequência
#'
#' Lê conjunto de dados e retorna uma tabela de frequência com observações, frequência absoluta e proporcional
#'
#' @param dados Dataframe com ao menos duas colunas, uma com a informação a priori e outra(s) com valores para plotar histograma e boxplot
#'
#' @param condicional Coluna em que está a informação conhecida
#'
#' @param vars Lista com nomes das colunas em que estão os valores com os quais serão plotados o gráfico de barras e construida a tabela de frequência
#'
#' @returns Lista com as tabelas de frequência de cada evento A dado B
#'
#' @export
#'
#' @examples
#'
#' mtcars_ <- mtcars
#' mtcars_$am[mtcars_$am == 0] <- "automatico"
#' mtcars_$am[mtcars_$am == 1] <- "manual"
#' freq.descritiva(mtcars_, "am", c("cyl", "gear"))
#'
freq.descritiva <- function(dados, condicional, vars){

  #limitar dataframe às informações de interesse

  dados <- subset(dados, select = c(vars, condicional))

  #criar dataframe com a informação a priori

  priori <- as.data.frame(table(dados[[condicional]]))

  #iniciar variaveis de retorno onde estarão tabelas de frequências e gráficos respectivamente

  saida <- list()     #lista com tabelas de frequência
  graficos <- list()  #lista com gráficos individuais de cada A|B
  plot_td <- list()   #lista com plot de todos os gráficos juntos

  #contador que permite navegar pelas listas

  ind <- 1 #iniciar contador para navegar na lista saida

  #preparar saída com todos os gráficos na mesma janela

  graphics::par(mfrow = c(nrow(priori),length(vars)))

  #a seguir o primeiro for() acessa informações de cada condicional por vezz

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

      #construindo nome das colunas para cada A|B

      colnome <- paste(vars[j], "|", as.character(priori[i,1]))

      #renomear coluna com colnome definindo acima

      names(saida)[ind] <- colnome

      #definir limite do plot para não cortar valores nas barras

      y_lim <- c (0, 1.2*(max(tab_freq$Freq_abs)))

      #plotar e armazenar barplot na variável plotar, importante para colocar valores nas barras

      plotar <- graphics::barplot(tab_, ylim = y_lim)
      graphics::title(xlab = (paste(vars[j],' | ', priori[i, 1], sep = "")), line = 2.5) #xlabel
      graphics::text(x = plotar, y = as.data.frame(tab_)[[2]], label = as.data.frame(tab_)[[2]], pos = 3, cex = 0.8, col = "red") #frequência absoluta na barra
      graphics::text(x = plotar, y = as.data.frame(tab_)[[2]], label = round(as.data.frame(tab_)[[2]]/sum(as.data.frame(tab_)[[2]]), 2), pos = 1, cex = 0.8, col = "red") #frequência relativa na barra

      #na última repetição, armazenar plotagem geral para retorno da função

      if(nrow(priori)*length(vars) == ind){
        graphics::mtext(paste("Evento(s)",' | ', condicional, sep = ""), side = 3, line = -2, outer = TRUE) #título geral
        plot_td[[1]] <- grDevices::recordPlot() #armazenar plot geral para retorno na função
        names(plot_td)[1] <- "plot geral" #renomear coluna para identificar melhor o retorno
      }
      ind <- ind+1 #incrementar contador para navegar na lista saida
    }
  }

  #etapa para armazenar plots individualmente para retorno da função

  ind = 1 #reiniciar contador para navegar na lista gráficos
  graphics::par(mfrow = c(1,1)) #reseta janela de saida para apenas um plot

  for (i in 1:nrow(priori)) {

    #filtro de cada informação conhecida

    dados_temp <- dplyr::filter(dados, dados[[condicional]] == as.character(priori[i,1]))

    for (j in 1:length(vars)) {

      #tabela que será utilizada para gráfico

      tab_ <- table(dados_temp[[j]])

      #armazenar os gráficos sem plotar na janela. indicações passo a passo

      grDevices::win.metafile()                #passo 1
      grDevices::dev.control('enable')         #passo 2
      graphics::par(bg="white")                #passo 3
      graphics::barplot(tab_)                  #passo 4 plotar gráfico
      graphics::title(xlab=(paste(vars[j],' | ', priori[i, 1], sep = "")), line = 2.5) #passo 5 legenda eixo x
      graficos[[ind]]<-grDevices::recordPlot() #passo 6 armazenar o plot na lista graficos
      grDevices::dev.off()                     #passo 7 fim dessa etapa

      #construindo nome das colunas para cada A|B

      colnome <- paste(vars[j], "|", as.character(priori[i,1]))

      #renomear coluna com colnome definindo acima

      names(graficos)[ind]<-colnome

      ind = ind+1 #incrementar contador para navegar na lista graficos
    }
  }

  #organizar listas de saída

  saida <- saida[order(names(saida))]           #lista com tabelas de frequencia
  graficos <- graficos[order(names(graficos))]  #lista com gráficos individuais
  retorno <- list("tabelas de frequencia" = saida, "graficos" = graficos, "plot geral"= plot_td)        #lista que a função retorna. plot_td foi definido anteriormente

  return(invisible(retorno))
}
