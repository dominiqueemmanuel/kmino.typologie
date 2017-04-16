#' Une fonction permettant de réaliser toutes les analyses factorielles.
#'
#' @param data est une\code{data.frame}.
#' @param var_act est un vecteur de \code{character} de noms de colonnes `data` constituant les variables actives de l'analyses./
#' @param var_supp est un vecteur de \code{character} de noms de colonnes `data` constituant les variables passive de l'analyses./
#' @param method est un \code{character} représentant la méthode
#' \code{"ACP/ACM global"} : suppose que les variables sont qualitatives ou quantitative
#' \code{"ACP/ACM répétées par blocs"} : suppose que les variables sont qualitatives ou quantitative
#' \code{"Analyse Canonique"} : suppose que les variables sont toutes quantitatives
#' \code{"ACP Rotation"} : suppose que les variables sont toutes quantitatives
#' \code{"AFM Duale"} : suppose que les variables sont toutes quantitatives
#' @param weight soit \code{NULL} (pas  de pondération des indivudus) soit un vecteur de \code{numeric} positif de même longueur que \code{nrow(data)} .
#' @param  group_var_id  soit \code{NULL} (pas  groupe de variable) soit une liste donc chaque élément est une liste de type list(var_act=...,var_supp=...)
#' @param  group_ind_id  soit \code{NULL} (pas  groupe d'individu) soit un vecteur de \code{integer} (utile uniquement si  \code{method = "AFM Duale"}) de longueur  \code{ncol(data)} indiquant pour chaque individu à quel bloc il appartient.
#' @param  nb_axe  soit \code{"Automatique"} (le nombre d'axes devra être fixé automatiquement selon un bon critère à définir) soit un nombre dans \code{]0, 1]} (cela représent la proportion d'information à conserver) un entier supérieur ou égal à 2 (cela représente le nombre d'axes à conserver)
#' @param ... sont des paramètres complémentaires dont la nature dépend de la méthode (à définir donc selon les besoin des développements).

#' @export factor_analysis
#' @examples
#' #Pas d'exemple pour le moment



factor_analysis <- function(data,
                            var_act , # variabke actives
                            var_supp = NULL, #variables passives
                            method = c("ACP/ACM globale","ACP/ACM répétées par blocs","Analyse Canonique","ACP Rotation","AFM Duale"),
                            weight = NULL,
                            group_var_id = NULL,
                            group_ind_id = NULL,
                            nb_axe = "Automatique",
                            ...){

var <- sort(unique(c(var_act,var_supp)))
index_var_supp <- which(var %in% var_supp)

if(length(index_var_supp)==0)index_var_supp<-NULL
if(length(group_var_id)==0)group_var_id<-NULL
if(length(group_ind_id)==0)group_ind_id<-NULL

library(FactoMineR)

  res <-  list(
    var_act = var_act
    ,var_supp = var_supp
    , method = method
    , weight = weight
    , group_var_id = group_var_id
    , group_ind_id = group_ind_id
    , var_coord = NULL#une matrice dont les colonnes sont les axes factoriels, les lignes sont les variables avec les rownames = {noms des variables quanti} ou {nom des variables quali == nom de la modalité}
    , var_contrib = NULL#une matrice dont les colonnes sont les axes factoriels, les lignes sont les variables avec les rownames = {noms des variables quanti} ou {nom des variables quali == nom de la modalité}
    , ind_coord =NULL # une matrix dont les colonnes sont les axes factoriels, les lignes les individus
    , ind_contrib =NULL # une matrix dont les colonnes sont les axes factoriels, les lignes les individus
    , var_coord_sup = NULL
    , eig =NULL
    , info_axis = NULL
    , total_info_kept = NULL

    , more_results = NULL)

  # fonction de normalisation
  ### scale


  # match les methodes
  method <- match.arg(method)
  # message(method)


  re_matrix<-function(mat,nrow){
    if(is.null(mat))return(NULL)
    e<-rownames(mat)
    if(length(mat)==NROW(mat)){
      e<-names(mat)

      mat <- as.data.frame(matrix(mat,nrow=if(length(e)==0) 1 else length(e)))
    }
    rownames(mat) <- e
    colnames(mat) <- paste0("Dim.",seq(NCOL(mat)))
    mat
  }

  re_matrix_ind<-function(mat){
    if(is.null(mat))return(NULL)
    e<-rownames(mat)
    if(length(mat)==NROW(mat)){
      e<-names(mat)
     mat <-  as.data.frame(matrix(mat,ncol=1))
    }
    rownames(mat) <- e
    colnames(mat) <- paste0("Dim.",seq(NCOL(mat)))
    mat
  }


  ## pour les ACP/ACM répéterées on utilise la fonciton de manière récursive
  if (method == "ACP/ACM répétées par blocs") {
    u <- names(group_var_id)
    more_results <- lapply(seq_along(u), function(t) {
      factor_analysis(
        data,
        var_act = group_var_id[[t]]$var_act,
        # variabke actives
        var_supp = group_var_id[[t]]$var_sup,
        #variables passives
        method = "ACP/ACM globale",
        weight = weight,
        group_var_id = NULL,
        group_ind_id = NULL,
        nb_axe = "Automatique"
      )

    })
    names(more_results) <- u
    res$more_results <- more_results
    return(res)

  }



  # faire un test d'abord si :
  ### quali
  ### quanti
  ### quali/quanti (factominer)
  # Filtrer sur les valeur propre sqrt(v) > 2


  #### Specs détaillés

  #typeVar <- typeDF(df) > DF(| name | class |)
  ##### reconnaissance de type
  # | name | class |
  ###############################
  # tester le full quali/quanti #
  ###############################
  # verification des types des variables
  supported_type_quanti <- c("numeric", "integer")
  supported_type_quali <- c("ordered", "factor", "character")

  if(class(nb_axe) == "numeric" & nb_axe >= 2){
 is_auto <- FALSE
  } else {
    is_auto <- TRUE
  }

    #calcul nombre max d'axe
    #   nb quanti + sum(quali{j}-1)
    #
    computeNbCP <- function(data = NULL, type_quali = NULL){
      nbMod <- sapply(data, function(x) { if(class(x) %in% type_quali){ return(length(unique(x))-1) }else{return(1)}})
      return (sum(nbMod))
    }
    nbAxeMax <- computeNbCP(data = data[,var_act,drop=FALSE],type_quali=supported_type_quali)





    ###############################
    # tester le full quali/quanti #
    ###############################
    # verification des types des variables
    supported_type_quanti <- c("numeric", "integer")
    supported_type_quali <- c("ordered", "factor", "character")
    # function
    isMixte <- function(data = NULL, supported_type = NULL){
      classe_colonne <- sapply(data, function(x) {return(class(x) %in% supported_type) })
      # return(!all(classe_colonne))
      return(any(classe_colonne) & !all(classe_colonne))
    }
    #call

    if(method == "ACP/ACM globale"){
    if(isMixte(data[,var],supported_type_quanti)){
      charVec <- which(sapply(data[,var,drop=FALSE], class) == "character")
      if(length(charVec)>0){
        for(e in names(charVec)){
          data[,e] <- as.factor(data[,e])
        }
      }

      resFAMD <- FAMD(base = data[,var,drop=FALSE],
                      sup.var = index_var_supp,
                      ncp = nbAxeMax,
                      graph = FALSE)
      #Filtre sur 80% de l'inertie


      #Stockage


      if(!is_auto){
        dim_80 <- nb_axe
      } else {
        dim_80 <- min(which(resFAMD$eig$`cumulative percentage of variance` > 80))
      }



      res$eig <- resFAMD$eig$eigenvalue

      res$info_axis  <- resFAMD$eig[1:dim_80,2]/100
      res$total_info_kept   <- max(resFAMD$eig[1:dim_80,3]/100)



      res$var_coord <- re_matrix(resFAMD$var$coord)[,1:dim_80,drop=FALSE]
      res$var_contrib <- re_matrix(resFAMD$var$contrib)[,1:dim_80,drop=FALSE]
      res$ind_coord <- re_matrix_ind(resFAMD$ind$coord)[,1:dim_80,drop=FALSE]
      res$ind_contrib <- re_matrix_ind(resFAMD$ind$contrib)[,1:dim_80,drop=FALSE]
      res$var_coord_sup <- re_matrix(resFAMD$var$coord.sup)[,1:dim_80,drop=FALSE]
    }
    else if (class(data[,var[1]]) %in% supported_type_quanti){
      resPCA <- PCA(X = data[,var,drop=FALSE],
                    ncp = nbAxeMax,
                    quanti.sup = index_var_supp,
                    graph = FALSE)

      #Filtre sur 80% de l'inertie
      if(!is_auto){
        dim_80 <- nb_axe
      } else {
        dim_80 <- min(which(resPCA$eig$`cumulative percentage of variance` > 80))
      }



      #Stockage
      res$eig <- resPCA$eig$eigenvalue
      res$info_axis  <- resPCA$eig[1:dim_80,2]/100
      res$total_info_kept   <- max(resPCA$eig[1:dim_80,3]/100)


      res$var_coord <- re_matrix(resPCA$var$coord)[,1:dim_80,drop=FALSE]
      res$var_contrib <- re_matrix(resPCA$var$contrib)[,1:dim_80,drop=FALSE]
      res$ind_coord <- re_matrix_ind(resPCA$ind$coord)[,1:dim_80,drop=FALSE]
      res$ind_contrib <- re_matrix_ind(resPCA$ind$contrib)[,1:dim_80,drop=FALSE]
      res$var_coord_sup <-  re_matrix(resPCA$quanti.sup$coord)[,1:dim_80,drop=FALSE]

    }
    else if (class(data[,var[1]]) %in% supported_type_quali) {
      charVec <- which(sapply(data[,var,drop=FALSE], class) == "character")
      if(length(charVec)>0){
        for(e in names(charVec)){
          data[,e] <- as.factor(data[,e])
        }
      }

      resMCA <- MCA(X = data[,var,drop=FALSE],
                    ncp = nbAxeMax,
                    quali.sup  = index_var_supp,
                    graph = FALSE)



      p<-length(var_act)
      if(p==1){
        dim_80<-1
        res$eig <- resMCA$eig$eigenvalue
        res$info_axis  <- resMCA$eig[1:dim_80,2]/100
        res$total_info_kept   <- max(resMCA$eig[1:dim_80,3]/100)
      } else {
      info_axis  <- (p/(p-1))^2*(resMCA$eig[,1]-1/p)^2*(resMCA$eig[,1]>1/p)
      e<-sum(res$info_axis)
      info_axis <- info_axis/e

      if(!is_auto){
        dim_80 <- nb_axe
      } else {
        dim_80 <- min(which(info_axis> 0.80))
      }


      #Stockage
      res$eig <- resMCA$eig$eigenvalue
      res$info_axis  <- info_axis[1:dim_80,2]
      res$total_info_kept   <- sum(res$info_axis)

      }

      res$var_coord <- re_matrix(resMCA$var$coord)[,1:dim_80,drop=FALSE]
      res$var_contrib <- re_matrix(resMCA$var$contrib)[,1:dim_80,drop=FALSE]
      res$ind_coord <- re_matrix_ind(resMCA$ind$coord)[,1:dim_80,drop=FALSE]
      res$ind_contrib <- re_matrix_ind(resMCA$ind$contrib)[,1:dim_80,drop=FALSE]
      res$var_coord_sup <- re_matrix(resMCA$quali.sup$coord)[,1:dim_80,drop=FALSE]
    }
    }
    # filtre sur le critère

    return (res)

}
