#' Une fonction permettant de discrétiser des variables d'une table.
#'
#' @param data est une\code{data.frame}.
#' @param var est un vecteur de \code{character} de noms de colonnes `data` dont les types sont tous \code{numeric}.
#' @param method est un \code{character} représentant la méthode. Valeurs possibles : à définir avec Régis car les sépcifications sont incomplètes sur ce point (Je mets néanmoins une valeur par défaut \code{"Amplitudes égales"}).
#' @param labelled un booléan. Si \code{TRUE} le recodage sera un \code{factor} dont les \code{levels} devront indiquer la signification du regadage (par exemple les \code{levels} pourront être \code{c("Age<=20","Age dans ]20 ; 50]", "Age > 50")}). Si \code{FALSE} le recodage sera un entier (entre \code{1} et \code{n}).
#' @param  n est un entier strictement positif représentant le nombre de classe à créer.
#' @param new_name soit \code{NULL} soit un vecteur de \code{character} de même longueur que \code{var} (permet de soit remplacer les variables par leurs recodages, soit créer de nouvelles variables).
#' @param ... sont des paramètres complémentaires dont la nature dépend de la méthode (à définir donc selon les besoin des développements).

#' @export discretisation
#' @examples
#' #Pas d'exemple pour le moment


discretisation <- function(data,
                           var ,
                           n = 5,
                           method = c("Amplitudes égales","Effectifs égaux"),
                           labelled = TRUE,
                           new_name = NULL, ...){

  # match les methodes
  method <- match.arg(method)
  #message(method)
  method <- switch(method,
                   "Effectifs égaux" = "quantile",
                   "Amplitudes égales" = "equal"
                   )
  #message(method)
  # verification des types des variables
  supported_type <- c("numeric", "integer", "POSIXct", "POSIXt")
  if(sum(sapply(var, function(v) return(!class(data[,v]) %in% supported_type ))) > 0)
    stop("AU MOINS UNE VARIABLE N EST PAS SUPPORTEE A CAUSE DE SON TYPE")

  for(v in seq_along(var)){

    if( length(unique(data[,var[v]])) > 1 ){

      # Compute breaks
      brks <- unique(classInt::classIntervals(data[,var[v]], n = n, style = method)$brks)

      # Cut variable with computed breaks
      if(labelled)
        res.cut <- cut(data[,var[v]],
                       breaks = brks,
                       include.lowest = T,
                       dig.lab = 10
                       )
      else
        res.cut <- cut(data[,var[v]],
                       breaks = brks,
                       labels = FALSE,
                       include.lowest = T,
                       dig.lab = 10
        )
    }else{
      res.cut <- as.factor(data[,var[v]])
    }
    # Save result
    if(!is.null(new_name))
      data[,new_name[v]] <- res.cut
    else
      data[,var[v]] <- res.cut

    #levels(cut(a,breaks = s$brks,include.lowest=T,dig.lab=10))

  }

  # A compléter
  return(data)
}




# L'instruction eval(formals(discretisation)$method) permet de retrouver les valeurs possibles de method
