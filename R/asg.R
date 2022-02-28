#' Parametros mi, sigma e alpha da funcao densidade de probabilidade da distribuicao Alpha Skew Gaussian (ASG)
#'
#' Estimação de parâmetros mi, sigma e alpha para a função densidade de probabilidade da distribuição Alpha Skew Gaussian (ASG)
#'
#' @param dados Dataframe onde estão a variável observada e a variável conhecida (condicional)
#'
#' @param var String formato "nome da coluna", parâmetro indica coluna onde está a variável observada
#'
#' @param condicional String no formato "nome da coluna", parâmetro indica coluna onde está a variavel conhecida
#'
#' @param classe String no formato "valor", valor condicional conhecido
#'
#' @param dg Lógico, default = FALSE, se TRUE função não recebe condicional.
#'
#' @param de_densidade Lista, parâmetros passados quando asg.est é chama por dens.descritiva. Não passe esses parâmetros
#'
#' @return Retorna valores de mi, sigma e alpha para a função densidade de probabilidade da distribuição Alpha Skew Gaussian (ASG)
#'
#' @export
#'
#' @examples
#'
#' #Exemplo 1
#'
#' asg.est(iris, "Petal.Length", "Species", "setosa")
#'
#' #Exemplo 2
#'
#' asg.est(iris, "Sepal.Length", dg = TRUE)
#'
asg.est<-function(dados,
                  var,
                  condicional,
                  classe = NULL,
                  dg = FALSE,
                  de_densidade = NULL){ #função de estimação

  TOL=1e-15

  V = dados[var]

  cont <- 1

  if (is.null(classe) && dg == FALSE) {

    classe <- names(table(dados[condicional]))
    cont <- length(classe)
    print(cont)

  }

  for (i in 1:cont) {

    #teste para dados gerais. se dg = TRUE a função não utiliza condicionais

    if(dg == TRUE){

      X=V[V==V]

    }else{

      Y = dados[condicional]

      k = classe[i]

      X=V[Y==k]

    }

    z=(X-mean(X))/stats::sd(X)
    S=mean(z^3)
    v=S

    #MÉTODO DOS MOMENTOS PARA VALORES INICIAIS
    chutes=c((Re(solve(polynom::as.polynomial(c(64*v^2, 192*v^2,  336*v^2,-64 + 352*v^2,-192 + 252*v^2,-144 + 108*v^2,27*v^2)))))^(1/2),-(Re(solve(polynom::as.polynomial(c(64*v^2, 192*v^2,  336*v^2,-64 + 352*v^2,-192 + 252*v^2,-144 + 108*v^2,27*v^2)))))^(1/2))
    chutes #chutes via método dos momentos

    d.ASG=function(x,me=0,des=1,a=A) {
      (((1-a*(x-me)/des)^2+1)/(2+a*a))*(1/(sqrt(2*pi)*des)*exp(-((x-me)^2)/(2*des^2)))
    }
    ch=unique(chutes[is.na(chutes)==F])
    MM=numeric(0)
    for (i in 1:length(ch)) MM[i]=sum(log(Vectorize(d.ASG(z,a=ch[i]))))

    alpha.c=ch[MM==min(MM)][1] ###máximo?
    if(is.na(alpha.c)==T) alpha.c=S
    sig.c=stats::sd(X)*((2 + alpha.c^2)^2/(4 + 4*alpha.c^2 + 3*alpha.c^4))^(1/2)
    mi.c=mean(X)+2*alpha.c*sig.c/(2+alpha.c^2)

    #MLE
    LL <- function(param) {
      mi <- param[1]
      sig <- param[2]
      alpha<-param[3]
      sum(log((1-alpha*(X-mi)/sig)^2+1))-length(X)*(log(sig)+log(2+alpha^2))-0.5*sum(((X-mi)/sig)^2) -0.5*length(X)*log(2*pi)
    }
    aa=0
    A <- matrix(c(0,1,0),ncol=3)
    B <- 0
    mle <- maxLik::maxLik(logLik = LL, start = c(mi = mi.c, sig = sig.c,alpha=alpha.c),tol = TOL, constraints=list(ineqA=A, ineqB=B), method='BFGS')
    m1=mle$estimate[1]
    s1=mle$estimate[2]
    a1=mle$estimate[3]

    est=c(m1,s1,a1)

    #construção do gráfico

    dens<-function(x,mi,sig,alpha) {
      res=((1-alpha*(x-mi)/sig)^2+1)/(sig*(2+alpha^2))*stats::dnorm((x-mi)/sig)
      return(res)
    }

    valores=matrix(seq(min(X),max(X),length.out=500))
    densidade <-
      apply(valores,1,
            function (x) dens(x,mi=est[1],sig=est[2],alpha=est[3]))

    #mínimo e máximmo para definir limites do eixo x

    min_ <- min(X)
    max_ <- max(X)

    #número de classes definido pela regra de sturges

    classes <- grDevices::nclass.Sturges(X)

    #encontrar passos e definir vetor de breaks no eixo x

    passo <- (max_-min_)/classes
    vetorBrk <- seq(min_, max_, by = passo)
    vetorBrk <- vetorBrk

    #plot do histograma + curva de densidade

    if (dg == TRUE){

      xlab_ <- paste0(var)

    }else{

      xlab_ <- paste0(var,"|",k)

    }

    if (is.null(de_densidade)){
      graphics::hist(X,
                     main='',
                     prob=T,
                     border='blue',
                     col=NA,
                     xlab = xlab_,
                     ylab = "Densidade",
                     sub = paste0("mi=", round(est[1], 2), " sig=", round(est[2], 2), " alpha=", round(est[3], 2)),
                     las = 1,
                     breaks = vetorBrk,
                     xaxp = c(min_,max_,classes),
                     xaxt = "n",
                     ylim = c(0,max(densidade*1.6)))

      graphics::axis(1, at = vetorBrk,
                     labels = format(round(vetorBrk, 2), nsmall = 2))

    }else{

      graphics::hist(X,
                     main='',
                     prob=T,
                     border='blue',
                     col=NA,
                     xlab = xlab_,
                     ylab = "Densidade",
                     sub = paste0("mi=", round(est[1], 2), " sig=", round(est[2], 2), " alpha=", round(est[3], 2)),
                     las = 1,
                     breaks = vetorBrk,
                     xaxp = c(min_,max_,classes),
                     xaxt = "n",
                     xlim = c(de_densidade[1],de_densidade[2]),
                     ylim = c(0, de_densidade[3]))

        graphics::axis(1, at = c(de_densidade[1],de_densidade[2]),
                    format(round(c(de_densidade[1],de_densidade[2]), 2), nsmall = 2),
                    las = de_densidade[4])

        graphics::axis(1, at = vetorBrk,
                       labels = format(round(vetorBrk, 2), nsmall = 2),
                       las = de_densidade[4])

    }

    graphics::lines(valores,densidade,col='orange',lwd=2)

    if (is.null(de_densidade)){
      if (dg == TRUE){
        print(paste0(var))
      }else{
      print(paste0(var, "|", k))}
      print(est)
    }

  }

  #retorno da função

  return(invisible(est))
}
