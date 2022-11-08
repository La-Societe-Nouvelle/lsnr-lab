#'Matching function to translate Insee NACE codes to the OECD NACE typology.
#'
#' @param Input Initial character string.
#' @param Mode Destination format : Insee or OCDE (OECD).
#'
#' @return A `character` object containing the translated code of considered initial string.
#'
#' @seealso \code{\link{BuildGHGData}}
#'
#' @examples
#' TransitionFunction("A","Insee"))
#' ###"AZ"###
#' @noRd

TransitionFunction=function(Input,Mode){
  TableTransition=read.csv("https://raw.githubusercontent.com/La-Societe-Nouvelle/LaSocieteNouvelle-defautdata/master/DefaultData-LSN/donnees/TableFonctionTransition.csv",sep=";",header=T,col.names = c("Numero.de.la.division","Libelle.de.la.division","Code.de.la.branche.associee","Code.OCDE"))

  if(Mode=="OCDE" | Mode=="OECD"){
    if(is.numeric(Input)){
    Oecd=TableTransition$Code.OCDE[TableTransition$Numero.de.la.division==Input]
    }else{
      if(substr(Input,1,1) %in% as.character(0:9)){
        Oecd=TableTransition$Code.OCDE[TableTransition$Numero.de.la.division==as.numeric(Input)]
      }
      if(substr(Input,1,1) %in% unique(substr(TableTransition$Code.de.la.branche.associee,1,1))){
        if(nchar(Input)==1){
          Oecd=unique(TableTransition$Code.OCDE[TableTransition$Code.OCDE==Input])
        }
        if(nchar(Input)==3){
          Divisions=as.numeric(substr(Input,2,3))
          Oecd=unique(TableTransition$Code.OCDE[TableTransition$Numero.de.la.division==Divisions])
        }
        if(nchar(Input)==7){
          Divisions=as.numeric(substr(Input,2,3)):as.numeric(substr(Input,6,7))
          Oecd=unique(TableTransition$Code.OCDE[TableTransition$Numero.de.la.division %in% Divisions])
          }
        }
    }

  return(Oecd)
    }else{
    if(is.numeric(Input)){
    Nace=TableTransition$Code.de.la.branche.associee[TableTransition$Numero.de.la.division==Input]
    }else{

      if(substr(Input,1,1) %in% as.character(0:9)){
      Nace=TableTransition$Code.de.la.branche.associee[TableTransition$Numero.de.la.division==as.numeric(Input)]
      }

      if(substr(Input,1,1) %in% unique(substr(TableTransition$Code.de.la.branche.associee,1,1))){
        if(nchar(Input)==1){
          Nace=unique(TableTransition$Code.de.la.branche.associee[TableTransition$Code.OCDE==Input])
        }
        if(nchar(Input)==3){
          Divisions=as.numeric(substr(Input,2,3))
          Nace=unique(TableTransition$Code.de.la.branche.associee[TableTransition$Numero.de.la.division==Divisions])
        }
        if(nchar(Input)==7){
          Divisions=as.numeric(substr(Input,2,3)):as.numeric(substr(Input,6,7))
          Nace=unique(TableTransition$Code.de.la.branche.associee[TableTransition$Numero.de.la.division %in% Divisions])
        }
      }
    }
    return(Nace)
  }
}
