#' Une fonction permettant de réaliser toutes les analyses factorielles.
#'
#' @param data est une\code{data.frame}.
#' @param var est un vecteur de \code{character} de noms de colonnes `data` constituant les variables actives de l'analyses. (Les autres variables seront donc à ignorer complètement dans cette partie)/
#' @param method est un \code{character} représentant la méthode
#' \code{"ACP/ACM global"} : suppose que les variables sont qualitatives ou quantitative
#' \code{"ACP/ACM répétées par groupe"} : suppose que les variables sont qualitatives ou quantitative
#' \code{"Analyse Canonique"} : suppose que les variables sont toutes quantitatives
#' \code{"ACP Rotation"} : suppose que les variables sont toutes quantitatives
#' \code{"AFM Duale"} : suppose que les variables sont toutes quantitatives
#' @param weight soit \code{NULL} (pas  de pondération des indivudus) soit un vecteur de \code{numeric} positif de même longueur que \code{nrow(data)} .
#' @param  group_var_id  soit \code{NULL} (pas  groupe de variable) soit un vecteur de \code{integer} de longueur  \code{length(var)} indiquant pour chaque variable à quel groupe elle appartient.
#' @param  group_ind_id  soit \code{NULL} (pas  groupe d'individu) soit un vecteur de \code{integer} (utile uniquement si  \code{method = "AFM Duale"}) de longueur  \code{ncol(data)} indiquant pour chaque individu à quel groupe il appartient.
#' @param  nb_axe  soit \code{"Automatique"} (le nombre d'axes devra être fixé automatiquement selon un bon critère à définir) soit un nombre dans \code{]0, 1]} (cela représent la proportion d'information à conserver) un entier supérieur ou égal à 2 (cela représente le nombre d'axes à conserver)
#' @param ... sont des paramètres complémentaires dont la nature dépend de la méthode (à définir donc selon les besoin des développements).

#' @export factor_analysis
#' @examples
#' #Pas d'exemple pour le moment


factor_analysis <- function(data,
                           var ,
                           method = c("ACP/ACM globale","ACP/ACM répétées par groupe","Analyse Canonique","ACP Rotation","AFM Duale"),
                           weight = NULL,
                           group_var_id = NULL,
                           group_ind_id = NULL,
                           nb_axe = "Automatique",
                           ...){

res <-  list(
    var = var
    , method = method
    , weight = weight
    , group_var_id = group_var_id
    , group_ind_id = group_ind_id
    , var_coord = NULL#une matrice dont les colonnes sont les axes factoriels, les lignes sont les variables avec les rownames = {noms des variables quanti} ou {nom des variables quali == nom de la modalité}
      , ind_coord =NULL # une matrix dont les colonnes sont les axes factoriels, les lignes les individus
      , eig =NULL)
  
  # fonction de normalisation
  ### scale


  # match les methodes
  method <- match.arg(method)
  # message(method)

  # faire un test d'abord si :
  ### quali
  ### quanti
  ### quali/quanti (factominer)
  # Filtrer sur les valeur propre sqrt(v) > 2


  #### Specs détaillés

  #typeVar <- typeDF(df) > DF(| name | class |)
  ##### reconnaissance de type
  # | name | class |


  if(class(nb_axe) == "numeric" & nb_axe >= 2){
    # Appliquer FAMD et
    #

    ###############################
    # tester le full quali/quanti #
    ###############################
    # verification des types des variables
    supported_type_quanti <- c("numeric", "integer")
    supported_type_quali <- c("ordered", "factor", "character")
    # function
    isMixte <- function(data = NULL, supported_type = NULL){
      classe_colonne <- sapply(data, function(x) {return(class(x) %in% supported_type) })
      return(!all(classe_colonne))
    }
    #call
    if(isMixte(data[,var],supported_type_quanti)){
      resFAMD <- FAMD(base = data[,var],
                      ncp = nb_axe,
                      graph = FALSE)
      res$eig <- resFAMD$eig$eigenvalue
      res$var_coord <- resFAMD$var$coord
      res$ind_coord <- resFAMD$ind$coord
    }
    else if (class(data[,var[1]]) %in% supported_type_quanti){
      resPCA <- PCA(X = data[,var],
                    ncp = nb_axe,
                    graph = FALSE)
      res$eig <- resPCA$eig$eigenvalue
      res$var_coord <- resPCA$var$coord
      res$ind_coord <- resPCA$ind$coord
      
    }
    else if (class(data[,var[1]]) %in% supported_type_quali) {
      charVec <- which(sapply(data[,var], class) == "character")
      if(length(charVec)>0){
        for(e in names(charVec)){
          data[,e] <- as.factor(data[,e])
        }
      }
      
      resMCA <- MCA(X = data[,var],
                    ncp = nb_axe,
                    graph = FALSE) 
      res$eig <- resMCA$eig$eigenvalue
      res$var_coord <- resMCA$var$coord
      res$ind_coord <- resMCA$ind$coord
    }
    
    return (res)
  }else{

    #calcul nombre max d'axe
    #   nb quanti + sum(quali{j}-1)
    #
    computeNbCP <- function(data = NULL, type_quali = NULL){
      nbMod <- sapply(data, function(x) { if(class(x) %in% type_quali){ return(length(unique(x))-1) }else{return(1)}})
      return (sum(nbMod))
    }
    nbAxeMax <- computeNbCP(data = data[,var],type_quali=supported_type_quali)

    
    ###############################
    # tester le full quali/quanti #
    ###############################
    # verification des types des variables
    supported_type_quanti <- c("numeric", "integer")
    supported_type_quali <- c("ordered", "factor", "character")
    # function
    isMixte <- function(data = NULL, supported_type = NULL){
      classe_colonne <- sapply(data, function(x) {return(class(x) %in% supported_type) })
      return(!all(classe_colonne))
    }
    #call
    if(isMixte(data[,var],supported_type_quanti)){
      resFAMD <- FAMD(base = data[,var],
                      ncp = nbAxeMax,
                      graph = FALSE)
      #Filtre sur 80% de l'inertie
      dim_80 <- min(which(resFAMD$eig$`cumulative percentage of variance` > 80))
      
      #Stockage
      res$eig <- resFAMD$eig$eigenvalue
      res$var_coord <- resFAMD$var$coord[,1:dim_80]
      res$ind_coord <- resFAMD$ind$coord[,1:dim_80]
    }
    else if (class(data[,var[1]]) %in% supported_type_quanti){
      resPCA <- PCA(X = data[,var],
                    ncp = nbAxeMax,
                    graph = FALSE)
      
      #Filtre sur 80% de l'inertie
      dim_80 <- min(which(resPCA$eig$`cumulative percentage of variance` > 80))
      
      #Stockage
      res$eig <- resPCA$eig$eigenvalue
      res$var_coord <- resPCA$var$coord[,1:dim_80]
      res$ind_coord <- resPCA$ind$coord[,1:dim_80]
      
    }
    else if (class(data[,var[1]]) %in% supported_type_quali) {
      charVec <- which(sapply(data[,var], class) == "character")
      if(length(charVec)>0){
        for(e in names(charVec)){
          data[,e] <- as.factor(data[,e])
        }
      }
      
      resMCA <- MCA(X = data[,var],
                    ncp = nbAxeMax, graph = FALSE) 
      
      #Filtre sur 80% de l'inertie
      dim_80 <- min(which(resMCA$eig$`cumulative percentage of variance` > 80))
      
      #Stockage
      res$eig <- resMCA$eig$eigenvalue
      res$var_coord <- resMCA$var$coord[,1:dim_80]
      res$ind_coord <- resMCA$ind$coord[,1:dim_80]
    }

    # filtre sur le critère

  return (res)
  }

# 
#   # A compléter
#   return(list(
#       var = var
#     , method = method
#     , weight = weight
#     , group_var_id = group_var_id
#     , group_ind_id = group_ind_id
#     , var_coord = #une matrice dont les colonnes sont les axes factoriels, les lignes sont les variables avec les rownames = {noms des variables quanti} ou {nom des variables quali == nom de la modalité}
#     , ind_coord = # une matrix dont les colonnes sont les axes factoriels, les lignes les individus
#     , eig = # Valeurs propres des axes factoriels
#   ))
}


########################  TESTS

# #Quali
# res1 <- factor_analysis(data=tea,
#                         var =c("exciting","relaxing","effect.on.health"),
#                         method = c("ACP/ACM globale","ACP/ACM répétées par groupe","Analyse Canonique","ACP Rotation","AFM Duale"),
#                         weight = NULL,
#                         group_var_id = NULL,
#                         group_ind_id = NULL,
#                         nb_axe = "Automatique")
# 
# 
# #Quanti
# res2 <-factor_analysis(data=wine,
#                        var =c("Harmony","Typical","Bitterness"),
#                        method = c("ACP/ACM globale","ACP/ACM répétées par groupe","Analyse Canonique","ACP Rotation","AFM Duale"),
#                        weight = NULL,
#                        group_var_id = NULL,
#                        group_ind_id = NULL,
#                        nb_axe = "Automatique")
# 
# #Mixte
# res3 <- factor_analysis(data=wine,
#                         var =c("Harmony","Typical","Label"),
#                         method = c("ACP/ACM globale","ACP/ACM répétées par groupe","Analyse Canonique","ACP Rotation","AFM Duale"),
#                         weight = NULL,
#                         group_var_id = NULL,
#                         group_ind_id = NULL,
#                         nb_axe = "Automatique")




