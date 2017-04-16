#' Une fonction permettant de cacluler des détecter et traiter les valeurs extrêmes dans une table
#'
#' @param data
#' @param var
#' @param method
#' @param winsorisation
#' @param new_name
#' @param ...
#' @export valeur_extreme
#' @examples
#' #Rien
#Fonction intermédiaire #1
DoubleMAD <- function(x, zero.mad.action="warn"){
  # The zero.mad.action determines the action in the event of an MAD of zero.
  # Possible values: "stop", "warn", "na" and "warn and na".
  x         <- x[!is.na(x)]
  m         <- median(x)
  abs.dev   <- abs(x - m)
  left.mad  <- median(abs.dev[x<=m])
  right.mad <- median(abs.dev[x>=m])
  if (left.mad == 0 || right.mad == 0){
    if (zero.mad.action == "stop") stop("MAD is 0")
    if (zero.mad.action %in% c("warn", "warn and na")) warning("MAD is 0")
    if (zero.mad.action %in% c(  "na", "warn and na")){
      if (left.mad  == 0) left.mad  <- NA
      if (right.mad == 0) right.mad <- NA
    }
  }
  return(c(left.mad, right.mad))
}

#Fonction intermédiaire #2
DoubleMADsFromMedian <- function(x, zero.mad.action="warn"){
  # The zero.mad.action determines the action in the event of an MAD of zero.
  # Possible values: "stop", "warn", "na" and "warn and na".
  two.sided.mad <- DoubleMAD(x, zero.mad.action)
  m <- median(x, na.rm=TRUE)
  x.mad <- rep(two.sided.mad[1], length(x))
  x.mad[x > m] <- two.sided.mad[2]
  #mad.distance <- abs(x - m) / x.mad
  mad.distance <- (x - m) / x.mad
  mad.distance[x==m] <- 0
  return(mad.distance)
}

# Fonction intermédiaire #3 : à partir d'un vecteur detecte les valeurs extreme
get_extrem_bounds <- function(vec, method = "mad"){
  #return (c(min(vec),max(vec)))
  #Gestion NAs
  vec <- vec[!is.na(vec)]
  return (suppressWarnings(c(max(vec[DoubleMADsFromMedian(vec) < -3]), min(vec[DoubleMADsFromMedian(vec) > 3]))))
  }

# Fonction globale
valeur_extreme <- function(data, var , method = c("mad"), winsorisation = TRUE, new_name = NULL, ...){
  method <- match.arg(method)

  # les types supportes
  supported_type <- c("numeric", "integer")
  # envoyer une exception
  if(sum(sapply(var, function(v) return(!class(data[,v]) %in% supported_type))) > 0)
    stop("AU MOINS UNE VARIABLE N EST PAS SUPPORTEE A CAUSE DE SON TYPE")

  #...remplacer method1 et method2 (voir en ajouter) par de vrais noms qui pourront être afficher dans l'interface

  for(v in seq_along(var)){
    # 1. calculate interval
    bounds = get_extrem_bounds(data[,var[v]])

    if(winsorisation){
      if(is.null(new_name)){
        # 2.bornage avec winsorization avec remplacement
        data[data[!is.na(data[,var[v]]),var[v]]<bounds[1],var[v]] <- bounds[1]
        data[data[!is.na(data[,var[v]]),var[v]]>bounds[2],var[v]] <- bounds[2]
      }else{
        # 2.bornage avec winsorization sans remplacement
        data[,new_name[v]] <- data[,var[v]]
        data[data[!is.na(data[,new_name[v]]),new_name[v]] < bounds[1],new_name[v]] <- bounds[1]
        data[data[!is.na(data[,new_name[v]]),new_name[v]] > bounds[2],new_name[v]] <- bounds[2]
      }
    }else{
      if(is.null(new_name)){
        # 2.bornage sans winsorization (NAs) sans remplacement de variable
        data[ (data[!is.na(data[,var[v]]),var[v]] < bounds[1]) | (data[!is.na(data[,var[v]]),var[v]] > bounds[2]) , var[v]] <- NA
      }else{
        # 2.bornage sans winsorization (NAs) avec remplacement de variable
        # Création variable
        data[,new_name[v]] <- data[,var[v]]
        # Remplacement par NA
        data[ (data[!is.na(data[,new_name[v]]),new_name[v]] < bounds[1]) | (data[!is.na(data[,new_name[v]]),new_name[v]] > bounds[2]) ,new_name[v]] <- NA
      }
    }
  }
  return (data)
}
