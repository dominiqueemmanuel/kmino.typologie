#' Une fonction permettant d'imputer les valeurs manquantes d'une table
#'
#' @param data
#' @param var
#' @param method
#' @param seuil
#' @param ...
#' @export imputation
#' @examples
#' #Rien
imputation <- function(data
                       , var=colnames(data) # variables à imputer
                       , var_comp=colnames(data) # variables explicatives l'imputation (peut être plus grand que var, les colonnes en plus ici ne seront pas imputées)
                       , method = c("Médiane / Mode","Hot Deck","MCMC","Forêts Aléatoires"), seuil=NULL, ...){
  method <- match.arg(method)

  # on s'assure que var_comp cotient var_comp
  var_comp <- unique(c(var,var_comp))
  if(length(var)==0)return(data)


  # Chargement package
  library("hot.deck")
  # verification des types des variables
  supported_type <- c("numeric", "integer", "ordered", "factor", "character") # pas de date car
  if(sum(sapply(var, function(x) return(! any( class(x)[1] %in% supported_type )))) > 0)
    stop("AU MOINS UNE VARIABLE N EST PAS SUPPORTEE A CAUSE DE SON TYPE")

  # I. Définition des constantes de la fonction : réfléchir à normer la factorisation
  nb_var <- length(var)

  # II. Initialisation des objets internes de la fonction
  result <-  list()
  result_i <- 1

  # Suggestion par rappot à la fonction Mode :
  # a déclarer en dehors et ne pas faire un export

  Mode <- function(x,na.rm=FALSE) { # Renvoie le mode d'une variable quanti quali ou date
    # verification des types des variables
    supported_type <- c("numeric", "integer", "ordered", "factor", "character")
    if(sum(sapply(var, function(v) return(! any( class(x)[1] %in% supported_type )))) > 0)
      stop("AU MOINS UNE VARIABLE N EST PAS SUPPORTEE A CAUSE DE SON TYPE")
    if(!na.rm){
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    if(na.rm){
      ux <- unique(x[which(!is.na(x))])
      ux[which.max(tabulate(match(x[which(!is.na(x))], ux)))]
    }
  }

  # "Hot Deck" gère les factor ? sinon faudrait-il pas gèrer ce cas pour faire un STOP ou c'est au niveau
  # de l'interface que ça doit etre fait


  # III. Imputation selon la classe de la variable et la méthode retenue
  # upgrade : filtrer les variables avec plus de k% de valeurs manquantes
  data_imputed <- data
  if (isTRUE(class(data) == "data.frame" & nb_var > 1)){


    #1. Méthodes ensemblistes : quel que soit le type de la variable
    # Imputation par Hot Deck
    if(method == "Hot Deck"){
      # data_imputed_temporaire <- as.data.frame(hot.deck::hot.deck(data[,var_comp], m=1, optimizeSD=TRUE, weightedAffinity=FALSE, method='best.cell')$data)

      # le package hot.deck bug das certains cas, je change donc de package (DOM)
      data_imputed_temporaire<-data
      colnames(data_imputed_temporaire)<-make.names(colnames(data_imputed_temporaire))
      data_imputed_temporaire<-simputation::impute_shd(dat=data_imputed_temporaire
                                     ,formula = as.formula(paste0(paste0(paste0(make.names(var),collapse="+"),"~",paste0("`",make.names(var_comp),"`",collapse="+"))))
                                     ,pool="univariate",backend="simputation")
      colnames(data_imputed_temporaire)<-colnames(data)
    }

    if(method == "Hot Deck"){
      # data_imputed_temporaire <- as.data.frame(hot.deck::hot.deck(data[,var_comp], m=1, optimizeSD=TRUE, weightedAffinity=FALSE, method='best.cell')$data)

      # le package hot.deck bug das certains cas, je change donc de package (DOM)
      data_imputed_temporaire<-data
      colnames(data_imputed_temporaire)<-make.names(colnames(data_imputed_temporaire))
      data_imputed_temporaire<-simputation::impute_shd(dat=data_imputed_temporaire
                                                       ,formula = as.formula(paste0(paste0(paste0(make.names(var),collapse="+"),"~",paste0("`",make.names(var_comp),"`",collapse="+"))))
                                                       ,pool="univariate",backend="simputation")
      colnames(data_imputed_temporaire)<-colnames(data)
    }

    #2. Méthodes d'imputation par variable

    for (i in 1:nb_var){
      # A. Variables quantitatives
      #if(class(data[,var[i]]) == "numeric" & anyNA(data[,var[i]]) = TRUE){
      if(class(data[,var[i]]) %in% c("numeric", "integer") & anyNA(data[,var[i]]) == TRUE){
        # 1. Imputation par médiane/ mode
        if(method == "Médiane / Mode"){
          data_imputed[which(is.na(data_imputed[,var[i]])),var[i]] <- median(data[,var[i]],na.rm = TRUE)
        }
        # 2. Imputation par Hot Deck : est-ce qu'on permet au hot deck de ne tourner que sur une cible ?
        if(method == "Hot Deck"){
          data_imputed[,var[i]] <- data_imputed_temporaire[,var[i]]
        }
        # 3. Imputation par MCMC
        if(method == "MCMC"){
          # data[which(is.na(data[,var[i]])),var[i]] <-
        }
        # 4. Imputation par Forêts Aléatoires
        if(method == "Forêts Aléatoires"){
          # data[which(is.na(data[,var[i]])),var[i]] <- randomForestSRC::impute.rfsrc()
        }
      }

      # B. Variables qualitatives
      #if(class(data[,var[i]]) == "factor" & anyNA(data[,var[i]]) = TRUE){
      if(class(data[,var[i]]) %in% c("ordered", "factor", "character") & anyNA(data[,var[i]]) == TRUE){
        # 1. Imputation par médiane/ mode
        if(method == "Médiane / Mode"){
          data_imputed[which(is.na(data_imputed[,var[i]])),var[i]] <- Mode(data[,var[i]],na.rm = TRUE)
        }
        # 2. Imputation par Hot Deck
        if(method == "Hot Deck"){
          data_imputed[,var[i]] <- data_imputed_temporaire[,var[i]]
        }
        # 3. Imputation par MCMC
        if(method == "MCMC"){
          # data[which(is.na(data[,var[i]])),var[i]] <-
        }
        # 4. Imputation par Forêts Aléatoires
        if(method == "Forêts Aléatoires"){
          # data[which(is.na(data[,var[i]])),var[i]] <-
        }
      }

      # C.Variables qualitatives ordonnées (ordered = TRUE)
      # D. Binaires (il faut caractériser le cas binaire - ex: factor a 2 modalités)
    }

    # Upgrade : Implémenter un test sur les types des variables de data et de data_imputed


    # Detach
    detach("package:hot.deck")
  }
  return(data_imputed)
}
