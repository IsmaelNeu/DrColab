###############################################################################
###                                                                         ###
###                           Script                                        ###
###                   FUNCAO - DETECÇÃO DE STRING                           ###
###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
###  Author: Ismael Neu                                                     ###
###  Date: 19/05/2020                                                       ###
###  Version: 1.0                                                           ###
###  E-mail: ismaelmmneu@hotmail.com                                        ###
###############################################################################
#'
#' @title N\enc{ú}{u}mero do Grupo e Caso
#'
#' @name str_GC
#'
#' @description Retorna a identifica\enc{çã}{ca}o do n\enc{ú}{u}mero do grupo
#'    e do caso.
#'
#' @param x Matriz de dados.
#'
#' @param coluna Num\enc{é}{e}rico. Indica qual a coluna deve ser analisada.
#'
#' @param padrao String. O valor a ser procurado, como delimitador. Por
#'     padr\enc{ã}{a}o \code{padrao = '_Caso'}.
#'
#' @examples
#' # Dados
#' dados <- data.frame(cbind(Col1 = c("Grupo1_Caso1", "Grupo2_Caso54",
#' "Grupo12_Caso1", "Grupo21_Caso9988", "Grupo9_Caso9989",
#' "Grupo9_Caso999"), Col2 = 1:6))
#' # Grupos
#' str_GC(x = dados, coluna = 1, padrao = '_Caso')$nameG
#' # Casos
#' str_GC(x = dados, coluna = 1, padrao = '_Caso')$nameC
#'
#' @export
str_GC <-function(x, coluna, padrao = '_Caso'){
  # x: matriz de dados
  # coluna: numero da coluna em que se encontra a identificacao
  # padrao: a ser buscado
  xid <- x[ , coluna]
  xcaso <- stringr::str_locate(string = xid, pattern = padrao)
  xlen <- stringr::str_length(string = xid)
  xGrupo <- stringr::str_sub(string = xid, start = 6, end = (xcaso[ ,1] - 1))
  xCaso <- stringr::str_sub(string = xid, start = (xcaso[ ,2] + 1), end = xlen)
  result <- list(nameG = as.numeric(xGrupo), nameC = as.numeric(xCaso))
  return(result)
}
