###############################################################################
###                                                                         ###
###                           Script                                        ###
###                       FUNCAO - GRAU DE MULTICIOLINEARIDADE              ###
###+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++###
###  Author: Ismael Neu                                                     ###
###  Date: 18/04/2020                                                       ###
###  Version: 1.0                                                           ###
###  E-mail: ismaelmmneu@hotmail.com                                        ###
###############################################################################
#'
#' @title Grau de Multicolinearidade em Grupos de Caracteres.
#'
#' @name tab_Grau_Multicolinear
#'
#' @aliases tab_Grau_Multicolinear
#'
#' @author Ismael Neu.
#'
#' @param Dados Data frame ou matriz de dados.
#'
#' @param ListComb Lista de matrizes ou data frames.
#'
#' @param Indicador String. Refer\enc{ê}{e}ncia ao indicador do grau de
#'     multicolinearidade: Determinante da Matriz de Correla\enc{çõ}{co}es
#'     (\code{DET}), N\enc{ú}{u}mero de Condi\enc{çã}{ca}o (\code{NC}),
#'     Fator de Infla\enc{çã}{ca}o da Vari\enc{â}{a}ncia (\code{FIV}),
#'     Teste de Farrar-Glauber (\code{FG}) e o Teste de Haitovsky (\code{HAIT}).
#'
#' @description A matriz de dados (\code{Dados}), deve conter as
#'     observa\enc{çõ}{co}es que se deseja determinar o grau de multicolinearidade,
#'     de acordo com as vari\enc{á}{a}veis contidas em \code{ListComb}.
#'     Em \code{ListComb}, cont\enc{ê}{e}m as combina\enc{çõ}{co}es de
#'     caracteres, dever\enc{á}{a} conter objetos em listas e estes objetos,
#'     em matriz ou `data frame`. O indicador ou o teste do grau de
#'     multicolinearidade deve ser repassado pelo par\enc{â}{a}metro
#'     \code{Indicador} indicador ou teste de multicolinearidade
#'
#' @details O grau de multicolinearidade podera ser realizado para cinco indicadores
#'     ou testes estatisticos, individualmente ou para todos.
#'     O grau de multicolinearidade, por todos os indicadores, pode ser obtido por
#'     \code{Indicador = 'all'}, ou individualmente, especificando
#'     o indicador ou teste de interesse.
#'     Entre os indicadores, poder\enc{á}{a} ser realizado o diagn\enc{ó}{o}stico
#'     pelo Determinante da Matriz de Correla\enc{çõ}{co}es (\code{DET}),
#'     N\enc{ú}{u}mero de Condi\enc{çã}{ca}o (\code{NC}) (\link[pracma]{cond})
#'     e o Fator de Infla\enc{çã}{ca}o da Vari\enc{â}{a}ncia (\code{FIV}) (\link[faraway]{vif}),
#'     enquanto que para a realiza\enc{çã}{ca}o de testes, poder\enc{á}{a}
#'     ser utilizado o Teste de Farrar-Glauber (\code{FG}) (\link[INeuStatBasic]{test_FG})
#'     e o Teste de Haitovsky (\code{HAIT}) (\link[INeuStatBasic]{test_Haitovsky}). Para
#'     \code{NC}, al\enc{é}{e}m do grau de multicolinearidade, tamb\enc{é}{e}m \enc{é}{e}
#'     determinado a respectiva classifica\enc{çã}{ca}o.
#'     Para os testes de \code{FG} e \code{HAIT}, s\enc{ã}{a}o retornados as
#'     estat\enc{í}{i}sticas e o valor-p`.
#'
#' @return Retorna uma tabela com o grau de multicolinearidade para cada
#'     combina\enc{çã}{ca}o e o n\enc{ú}{u}mero de observa\enc{çõ}{co}es utilizadas na
#'     determina\enc{çã}{ca}o.
#'
#' @examples
#' # Importando dados
#' \dontrun{
#' data(mtcars)
#' data(CaracterMTCARS)
#' # Exemplo 1:
#' # tab_Grau_Multicolinear(Dados = mtcars, ListComb = CaracterMTCARS, Indicador = 'NC')
#' }
#' @export

tab_Grau_Multicolinear <- function(Dados, ListComb, Indicador = c('all', 'DET', 'NC', 'FIV', 'FG', 'HAIT')){
        ###################################################################
  ## ListComb: lista contendo as combinacoes de caracteres
  ## Dados: matriz de dados
  ## Resultado: saida
  ## Indicador: Incador do grau de multicolinearidade
        ###################################################################
  # Resultado <- INeuDr::id_Gruppe_Fall(ListComb)
  Resultado <- id_Gruppe_Fall(ListComb)
  Resultado$n <- rep(NA, dim(Resultado)[1])
  # NCinfinitos <- c()
  ## Testa para qual teste ou indicador de multicolinearidade
  if(Indicador == 'ALL' | Indicador == 'all'){
    Resultado$DET <- rep(NA, dim(Resultado)[1])
    Resultado$NC <- rep(NA, dim(Resultado)[1])
    Resultado$Classe <- rep(NA, dim(Resultado)[1])
    Resultado$VIF <- rep(NA, dim(Resultado)[1])
    Resultado$FG <- rep(NA, dim(Resultado)[1])
    Resultado$sigFG <- rep(NA, dim(Resultado)[1])
    Resultado$HAIT <- rep(NA, dim(Resultado)[1])
    Resultado$sigHAIT <- rep(NA, dim(Resultado)[1])
  }else if(Indicador == 'DET' | Indicador == 'det' | Indicador == 'Det' | Indicador == 'DETERMINANTE' | Indicador == 'Determinante'){
    Resultado$DET <- rep(NA, dim(Resultado)[1])
  }else if(Indicador == 'NC' | Indicador == 'nc' | Indicador == 'Nc' | Indicador == 'NUMERO DE CONDICAO' | Indicador == 'Numero de condicao'){
    Resultado$NC <- rep(NA, dim(Resultado)[1])
    Resultado$Classe <- rep(NA, dim(Resultado)[1])
  }else if(Indicador == 'VIF' | Indicador == 'FIV' |Indicador == 'vif' | Indicador == 'fiv'| Indicador == 'Vif' | Indicador == 'Fiv' | Indicador == 'FATOR DE INFLACAO DE VARIACNIA' | Indicador == 'Fator de inflacao de variancia'){
    Resultado$VIF <- rep(NA, dim(Resultado)[1])

  }else if(Indicador == 'FG' | Indicador == 'Farrar e Glaubber' |Indicador == 'Farrar & Glauber'){
    Resultado$FG <- rep(NA, dim(Resultado)[1])
    Resultado$sigFG <- rep(NA, dim(Resultado)[1])
  }else if(Indicador == 'HAIT' | Indicador == 'Hait' |Indicador == 'Haitovsky'){
    Resultado$HAIT <- rep(NA, dim(Resultado)[1])
    Resultado$sigHAIT <- rep(NA, dim(Resultado)[1])
  }else{
    stop('O Indicador nao foi especificado de forma correta')
  }

  conta <- 0
  for (j in names(ListComb)){
    for (i in rownames(ListComb[[j]])){
      Linha <- paste0(j, '_', i)
      Variaveis <- as.vector(ListComb[[j]][i,])
      MatSelec <- Dados %>% dplyr::select(Variaveis)
      Resultado[Linha, 'n'] <- as.numeric(dim(stats::na.omit(MatSelec))[1])
      Correlacoes <- stats::cor(MatSelec, use = "complete.obs")
      if(any(!is.finite(Correlacoes))){
        conta <- conta + 1
      }else{
        ## DETERMINANTE
        if(Indicador == 'ALL' | Indicador == 'all' | Indicador == 'DET' | Indicador == 'det' | Indicador == 'Det' | Indicador == 'DETERMINANTE' | Indicador == 'Determinante'){
          # Calculo do determinate da matriz de correlacoes
          determinante <- as.numeric(det(Correlacoes))
          Resultado [Linha, 'DET'] <- ifelse(determinante > 1.0e-10, determinante, 0.0000)
        }
        ## NUMERO DE CONDICAO
        if(Indicador == 'ALL' | Indicador == 'all'| Indicador == 'NC' | Indicador == 'nc' | Indicador == 'Nc' | Indicador == 'NUMERO DE CONDICAO' | Indicador == 'Numero de condicao'){
          # Calcula o Numero de condicao (NC), utilizando a biblioteca pracma
          NC <- pracma::cond(M = Correlacoes, p = "2")
          # ifelse(is.infinite(NC), NCinfinitos <- c(NCinfinitos, Linha), Resultado [Linha, 'NC'] <- NC)
          Resultado [Linha, 'NC'] <- NC
          if(NC < 100){
            Resultado [Linha, 'Classe'] <- "FRACA"
          }else if(NC>= 1000){
            Resultado [Linha, 'Classe'] <- "SEVERA"
          }else if(NC < 1000 & NC >= 100){
            Resultado [Linha, 'Classe'] <- "MODFORTE"
            # }else if(is.infinite(NC)){
            #   Resultado [Linha, 'Classe'] <- 'Infinite'
          }else{
            Resultado [Linha, 'Classe'] <- NA
          }
        }
        ## FATOR DE INFLACAO DA VARIANCIA
        if(Indicador == 'ALL' | Indicador == 'all' | Indicador == 'VIF' | Indicador == 'FIV' |Indicador == 'vif' | Indicador == 'fiv'| Indicador == 'Vif' | Indicador == 'Fiv' | Indicador == 'FATOR DE INFLACAO DE VARIACNIA' | Indicador == 'Fator de inflacao de variancia'){
          # Calculo do Fator de Inflacada da Variancia, por meio de um funcao
          Resultado [Linha, 'VIF'] <- as.numeric(max(faraway::vif(MatSelec)))
          # if(is.infinite(max(INeuStatBasic::FARAWAY.vif(MatSelec)))){NCinfinitos <- c(NCinfinitos, Linha)}
        }
        ## TESTE DE FARRAR E GLAUBER
        # if(Indicador == 'ALL' | Indicador == 'all' | Indicador == 'FG' | Indicador == 'Farrar e Glaubber' |Indicador == 'Farrar & Glauber'){
        #   # Teste Farrar-Glauber 1967
        #   Resultado [Linha, 'FG'] <- INeuStatBasic::test_FG(MatSelec)$statistic
        #   Resultado [Linha, 'sigFG'] <- INeuStatBasic::test_FG(MatSelec)$p.value
        # }
        ## TESTE DE HAITOVSKY
        # if(Indicador == 'ALL' | Indicador == 'all' | Indicador == 'HAIT' | Indicador == 'Hait' |Indicador == 'Haitovsky'){
        #   # Teste Haitovsky 1969
        #   Resultado [Linha, 'HAIT'] <- INeuStatBasic::test_Haitovsky(MatSelec)$statistic
        #   Resultado [Linha, 'sigHAIT'] <- INeuStatBasic::test_Haitovsky(MatSelec)$p.value
        # }
      }
    }
  }
  return(Resultado)
}
