###############################################################################
###                                                                         ###
###                           Script                                        ###
###                       FUNCAO - IDENTIFICACAO GRUPO CASO                 ###
###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
###  Author: Ismael Neu                                                     ###
###  Date: 27/03/2020                                                       ###
###  Version: 1.0                                                           ###
###  E-mail: ismaelmmneu@hotmail.com                                        ###
###############################################################################
#'
#' @title Identifica\enc{çã}{ca}o de Grupos e Casos.
#'
#' @name id_Gruppe_Fall
#'
#' @aliases id_Gruppe_Fall
#'
#' @author Ismael Neu.
#'
#' @param x Lista. Deve conter informa\enc{çõ}{co}es de caracteres em lista, para
#'     as diferentes combina\enc{çõ}{co}es.
#'
#' @param concatena L\enc{ó}{o}gico. Se `TRUE`, traz uma segunda coluna, com os
#'     caracteres concatenados, separados por \code{separador}.
#'
#' @param str_sep String. Colocado entre os caracteres, como forma de indentificar
#'     os nomes de cada caracter que compoem a identifica\enc{çã}{ca}o. O par\enc{â}{a}metro
#'     \enc{é}{e} necess\enc{á}{a}rio quando \code{concatena = TRUE}.
#'
#' @description O par\enc{â}{a}metro \code{x} necessita de uma lista,
#'     contendo no n\enc{í}{i}vel superior os `Grupos` e no subn\enc{í}{i}vel,
#'     os `Casos` (em uma tabela). O formato para este par\enc{â}{a}metro pode
#'     ser obtido \link[INeuDr]{Kombination}, tendo
#'     o par\enc{â}{a}metro \code{saida = 'tabela'}.
#'
#' @details A identifica\enc{çã}{ca}o de cada combina\enc{çã}{ca}o entre o
#'     grupo e o caso, ser\enc{á}{a} dada pela jun\enc{çã}{ca}o do nome do
#'     <i>j-\enc{é}{e}simo</i> \enc{í}{i}tem da lista (`Grupo`) e a nome da
#'     <i>i-\enc{é}{e}sima</i>
#'     linha da tabela dentro de cada \enc{í}{i}tem (`Caso`).
#'
#'
#' @return Data frame de uma coluna, contendo em cada unidade a
#'     identifica\enc{çã}{ca}o de `Grupo` e `Caso`. Por \code{default}, o
#'     nome das linhas \enc{é}{e} o mesmo da respectiva identifica\enc{çã}{ca}o.
#'
#' @seealso \link[INeuDr]{Kombination}
#' @examples
#' # Dados:
#' tabela1 <- data.frame(matrix(1:8, ncol = 2))
#' rownames(tabela1) <- paste0('Row', 1:dim(tabela1)[1])
#' tabela2 <- data.frame(matrix(1:10, ncol = 2))
#' rownames(tabela2) <- paste0('Row', 1:dim(tabela2)[1])
#' tabela <- list(Grupo1 = tabela1, Grupo2 = tabela2)
#' # Resultado da funcao
#' teste <- id_Gruppe_Fall(x = tabela)
#'
#'
#' @export
id_Gruppe_Fall <- function(x, concatena = FALSE, str_sep = ''){

  if(concatena == TRUE){Variaveis <- c()}
  IDGrupoCaso <- c()
  for (j in names(x)){
    for (i in rownames(x[[j]])){
      IDGrupoCaso <- c(IDGrupoCaso, paste0(j, '_', i))
      if(concatena == TRUE){
        Variaveis <- c(Variaveis, paste(x[[j]][i, ], collapse = str_sep))
      }
    }
  }
  if(concatena == FALSE){
    resultado <- data.frame(IDGrupoCaso)
    rownames(resultado) <- resultado[,1]
  }else{
    resultado <- data.frame(cbind(IDGrupoCaso, Variaveis))
    rownames(resultado) <- resultado[,1]
  }
  return(resultado)
}
