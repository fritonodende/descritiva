% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disp.R
\name{disp.descritiva}
\alias{disp.descritiva}
\title{Diagramas de dispersão a partir de uma informação conhecida}
\usage{
disp.descritiva(
  dados,
  vars,
  condicional,
  classe = NULL,
  grafs = NULL,
  grade = c(1, 1),
  titulo = FALSE
)
}
\arguments{
\item{dados}{Dataframe com ao menos três colunas, uma com a informação a priori e no mínimo outras duas com valores para coordenadas x e y}

\item{vars}{Lista com os nomes das duas colunas ( x e y, nessa ordem) que formarão as coordenadas do plano cartesiano}

\item{condicional}{Coluna em que está a informação conhecida (priori)}

\item{classe}{Caso queira definir as classes que deseja analisar}

\item{grafs}{vetor c(1:n) indicando os gráficos que serão exibidos com o plot customizado, o índice do gráfico corresponde a sua posição no plot geral que a função retorna}

\item{grade}{vetor c(x, y) indicando a quantidade linhas e colunas em que serão impressos os gráficos customizados definos pelo parâmetro grafs}

\item{titulo}{Lógico ou string, default = FALSE, TRUE para exibir título no plot geral no formato evento|condicional ou string no formato "título customizado"}
}
\value{
A função retorna uma lista com diagramas de dispersão combinados e individuais do conjunto de dados observado bem como as tabelas definidas por cada priori
}
\description{
Essa função lê um conjunto de dados e plota diagramas de dispersão para observação do comportamento de uma determinada relação entre variáveis de acordo com informações conhecidas a priori
}
\examples{

#Exemplo 1:
#função recebe: dataset iris,
#               info a priori "Species"
#               colunas para coordenadas x e y "Petal.Length" e "Petal.Width"

disp.descritiva(iris, c("Sepal.Length", "Sepal.Width"), "Species")

#Exemplo 2:
#função recebe: dataset mtcars,
#               info a priori "cyl"
#               colunas para coordenadas x e y "mpg" e "qsec"

disp.descritiva(mtcars, c("mpg", "qsec"), "cyl")

}
