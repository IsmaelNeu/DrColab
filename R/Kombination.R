###############################################################################
###                                                                         ###
###                           Script                                        ###
###                       FUNCAO - COMBINACAO DE CARACTERES                 ###
###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
###  Author: Ismael Neu                                                     ###
###  Date: 27/03/2020                                                       ###
###  Version: 1.0                                                           ###
###  E-mail: ismaelmmneu@hotmail.com                                        ###
###############################################################################
#'
#' @title Combina\enc{çã}{ca}o entre Vari\enc{á}{a}veis.
#'
#' @name Kombination
#'
#' @aliases Kombination
#'
#' @param Caract Vetor num\enc{é}{e}rico  ou string. Cont\enc{ê}{e}m os 
#'     caracteres que ser\enc{ã}{a}o combinados.
#'
#' @param nVar Vetor num\enc{é}{e}rico. O vetor poder\enc{á}{a} ser unit\enc{á}{a} 
#'     ou maior, representa o n\enc{ú}{u}mero de caracetres que ser\enc{ã}{a}o 
#'     combinados.
#'
#' @param saida String. Os caracetres combinados, dentro de cada `Grupo` 
#'     poder\enc{ã}{a}o ser retornados em uma `lista` ou em `tabela`. 
#'
#' @description O vetor \code{Caract} conter\enc{á}{a} os caracteres que 
#'     ser\enc{ã}{a}o combinados, de tamanhos \code{nVar}. Com o argumento 
#'     \code{saida} \enc{é}{e} poss\enc{í}{i} especificar o formato em que 
#'     os resultados de cada `Caso` dever\enc{á}{a} ser retornado. T\enc{ê}{e}m-se 
#'     as possibilidades de \code{lista} ou em \code{tabela}.
#'     Na combina\enc{çã}{ca}o n\enc{ã}{a}o importa a ordem em que os elementos
#'     s\enc{ã}{a}o escolhidos.
#'     Neste caso, o n\enc{ú}{u}mero de \code{Caract} ser\enc{á}{a} tomado a 
#'     <i>i-\enc{é}{e}simo</i> \code{nVar}, ou seja, considerando \code{nVar} 
#'     um vetor unit\enc{á}{a}rio, será tomado de \code{nVar}
#'     a \code{nVar}.
#'     T\enc{ê}{e}m-se que: \eqn{C_(n, p) = \frac{n!}{(p! * (n - )!)}}
#'
#'
#' @return Retorna \code{Caract} combinados a cada \code{nVar}, em formato de `tabela
#'     ou de `lista`, conforme argumento especificado em \code{saida}.
#'
#' @seealso \link[gtools]{combinations}
#'
#' @examples
#' # Exemplo 1:
#' caracteres <- paste0('M', 1:10)
#' Kombination(Caract = caracteres, nVar = 5, saida = 'lista')
#' Kombination(Caract = caracteres, nVar = 5, saida = 'tabela')
#' # Exemplo 2:
#' data(mtcars)
#' NomesVar <- colnames(mtcars)
#' Kombination(Caract = NomesVar, nVar = c(5,10), saida = 'tabela')
#'
#' @export

Kombination <- function(Caract, nVar, saida = c('lista', 'tabela')){
  # Restorna a combinacao de caracteres

  nCaract <- c(1:length(Caract))
  if (saida == 'lista') {
    ResultGroup <- list()
  }else if (saida == 'tabela'){
    ResultGroupTab <- list()
  }else{
    print('O tipo de saida nao eh valido!')
  }


  for (k in 1:length(nVar)){
    for (i in nVar){
      xx <- gtools::combinations(n=max(nCaract), r=nVar[k], v = Caract, repeats.allowed = FALSE)
      colnames(xx) <- paste0('V', 1:nVar[k])
      rownames(xx) <- paste0('Caso', 1:dim(xx)[1])

      if (saida == 'lista'){
        # x3 <- rep(NA,dim(xx)[1])
        x3 <-list()
        for (m1 in 1:dim(xx)[1]){
          x3[m1] <- list(xx[m1,]) #paste0(xx[m1,], collapse = ":")
        }
        names(x3) <- paste0('Caso', 1:dim(xx)[1])
        ResultGroup[k] <- list(x3)
      }

      if (saida == 'tabela'){ResultGroupTab[k] <- list(xx)}
    }
  }

  if (saida == 'lista'){
    names(ResultGroup) <- paste0('Grupo', nVar)
    return(ResultGroup)
  }else if (saida == 'tabela'){
    names(ResultGroupTab) <- paste0('Grupo', nVar)
    return(ResultGroupTab)
  }else{
    return(print('A saida nao valida'))
  }

}
