#' @title Profil d'une variable
#' @description Fonction permettant de créer des résumés sous forme de texte (affichables) des distributions des variables d'une table.
#'
#' @param data est une \code{data.frame}.
#' @param var est un vecteur de \code{character} de noms de colonnes présents dans `data` et dont les types sont tous \code{numeric} ou \code{factor}.
#' @param ... sont des paramètres complémentaires dont la nature dépend de la méthode (à définir donc selon les besoin des développements).

#' @export profil_variable

#' @examples
#' #Pas d'exemple pour le moment

profil_variable <- function(data, var, ...){
  # method <- match.arg(method)

  # Définition des constantes de la fonction : réfléchir à normer la factorisation
  nb_var <- length(var)

  # Initialisation des objets internes de la fonction
  result <-  list()
  result_i <- 1
  Mode <- function(x) { #upgrade : gestion NAs
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  # Table des msg v0 :
  profil_var_msg <- data.frame(profil_var_msg_id=""
                               ,profil_var_msg_fr=""
                               ,stringsAsFactors = FALSE)

  profil_var_msg[1,]$profil_var_msg_id <- "profil_var_msg_000001"
  profil_var_msg[2,]$profil_var_msg_id <- "profil_var_msg_000002"
  profil_var_msg[3,]$profil_var_msg_id <- "profil_var_msg_000003"
  profil_var_msg[4,]$profil_var_msg_id <- "profil_var_msg_000004"
  profil_var_msg[5,]$profil_var_msg_id <- "profil_var_msg_000005"
  profil_var_msg[6,]$profil_var_msg_id <- "profil_var_msg_000006"
  profil_var_msg[7,]$profil_var_msg_id <- "profil_var_msg_000007"
  profil_var_msg[8,]$profil_var_msg_id <- "profil_var_msg_000008"
  profil_var_msg[9,]$profil_var_msg_id <- "profil_var_msg_000009"
  profil_var_msg[10,]$profil_var_msg_id<- "profil_var_msg_000010"
  profil_var_msg[11,]$profil_var_msg_id<- "profil_var_msg_000011"
  profil_var_msg[12,]$profil_var_msg_id<- "profil_var_msg_000012"
  profil_var_msg[13,]$profil_var_msg_id<- "profil_var_msg_000013"
  profil_var_msg[14,]$profil_var_msg_id<- "profil_var_msg_000014"

  profil_var_msg[1,]$profil_var_msg_fr <- "Variable quantitative"
  profil_var_msg[2,]$profil_var_msg_fr <- "Variable qualitative"
  profil_var_msg[3,]$profil_var_msg_fr <- "Variable qualitative ordonnée"
  profil_var_msg[4,]$profil_var_msg_fr <- "Variable date"
  profil_var_msg[5,]$profil_var_msg_fr <- "Variable caractère"
  profil_var_msg[6,]$profil_var_msg_fr <- "Variable présentant des valeurs manquantes"
  profil_var_msg[7,]$profil_var_msg_fr <- "Variable sans valeurs manquantes"
  profil_var_msg[8,]$profil_var_msg_fr <- "Variable dont les modalités sont à effectifs égaux"
  profil_var_msg[9,]$profil_var_msg_fr <- "Variable constante"
  profil_var_msg[10,]$profil_var_msg_fr<- "Variable présentant des valeurs extrêmes"
  profil_var_msg[11,]$profil_var_msg_fr<- "Variable présentant des points d'accumulation"
  profil_var_msg[12,]$profil_var_msg_fr<- "Variable présentant des doublons"
  profil_var_msg[13,]$profil_var_msg_fr<- "Variable présentant des valeurs nulles"
  profil_var_msg[14,]$profil_var_msg_fr<- "profil_var_msg_000014"
  # Patch sur bug (?) rownames
  rownames(profil_var_msg) <- seq(1:dim(profil_var_msg)[1])
  # A terme : source("../packages/kmino_packages_development/kmino.typologie/R/profil_variable_msg.r",encoding = "UTF-8",local=TRUE)$value

  # Caractérisation de la variable, selon sa classe
  if (isTRUE(class(data) == "data.frame" & nb_var >= 1)){ #modification du filtre : > devient >=
    list_complete <- NULL
    for (i in 1:nb_var){ #i <- 131
      list_text_var <- NULL # Ensemble des id_txt caractérisant la variable
      #A. Variables quantitatives
      if (class(data[,var[i]]) == "numeric"){
        list_text_var <- "profil_var_msg_000001"
        # Liste des verifs : presence VM, nb valeurs nulles, nb valeurs positives & négatives, ajustement ln, présence valeurs extremes, coefficient Gini, caractérisation kurt/sk
        # 1. Présence de valeurs manquantes
        if (anyNA(data[,var[i]])==TRUE ){
          list_text_var <- c(unlist(list_text_var),"profil_var_msg_000006")
        }else{
          list_text_var <- c(unlist(list_text_var),"profil_var_msg_000007")
        }
        # 2. Constante
        if(min(data[,var[i]],na.rm = TRUE) == max(data[,i],na.rm = TRUE)){
          list_text_var <- c(unlist(list_text_var),"profil_var_msg_000009")
        }
        # 3. Valeurs nulles : test si VM (ajout which NAs: ok)
        # if(length(data[,var[i]]) != length(data[,var[i]][which(data[,var[i]] != 0 || is.na(data[,var[i]]) ==TRUE)]) ){
        if(length(which(data[,var[i]] == 0)) > 0) {
          list_text_var <- c(unlist(list_text_var),"profil_var_msg_000013")
        }
        # 4. Point d'accumulation : seuil choisi à 1/4 de la distribution sans VM
        if(length(data[,var[i]][which(data[,var[i]] == Mode(data[,var[i]]))]) > length(data[,var[i]][which(!is.na(data[,var[i]]))]) /4 ){
          list_text_var <- c(unlist(list_text_var),"profil_var_msg_000011")
        }
        # 5. Valeurs extremes : mediane +/- 3 IQR (upgrade : MAD)
        if(length(data[,var[i]][which(data[,var[i]] < median(data[,var[i]],na.rm =TRUE)-3*IQR(data[,var[i]],na.rm =TRUE))]) > 0
           | length(data[,var[i]][which(data[,var[i]] > median(data[,var[i]],na.rm = TRUE)+3*IQR(data[,var[i]],na.rm =TRUE))]) > 0){
          list_text_var <- c(unlist(list_text_var),"profil_var_msg_000010")
        }

      }
      #B. Variables qualitatives
      if (class(data[,var[i]]) == "factor"){
        list_text_var <- "profil_var_msg_000002"
        # Liste des verifs : présence VM, faible effectif, effectifs égaux, modalités sans effectifs
        # 1. Présence de valeurs manquantes
        if (anyNA(data[,var[i]])==TRUE){
          list_text_var <- c(unlist(list_text_var),"profil_var_msg_000006")
        }else{
          list_text_var <- c(unlist(list_text_var),"profil_var_msg_000007")
        }
        #2. Effectifs égaux
        if (min(summary(data[,var[i]])) == max(summary(data[,var[i]]))){
          list_text_var <- c(unlist(list_text_var),"profil_var_msg_000008")
        }
        #3. Effectifs nuls
        if (any(summary(data[,var[i]])==0)){
          list_text_var <- c(unlist(list_text_var),"profil_var_msg_000013")
        }
      }
      #C. Variables caractères
      if (class(data[,var[i]]) == "character"){
        # Liste des verifs : présence VM, présence doublons exacts
        # 1. Présence de valeurs manquantes
        if (anyNA(data[,var[i]])==TRUE ){
          list_text_var <- c(unlist(list_text_var),"profil_var_msg_000006")
        }else{
          list_text_var <- c(unlist(list_text_var),"profil_var_msg_000007")
        }
        # 2. Présence de doublons exacts
        if (unique(data[,var[i]]) < length(data[,var[i]]) ){
          list_text_var <- c(unlist(list_text_var),"profil_var_msg_000012")
        }
      }
      # Objet retourné par la fonction profil_variable : liste de couples (variable, liste de character)
      # Conversion id <- texte
      # print(profil_var_msg$profil_var_msg_fr[match(list_text_var,profil_var_msg$profil_var_msg_id)])
      liste_var_text <- c(var[i], unlist(profil_var_msg$profil_var_msg_fr[match(list_text_var,profil_var_msg$profil_var_msg_id)]))
      # print(liste_var_text)
      # Stockage hors boucle
      list_complete[[result_i]] <- liste_var_text
      result_i <- result_i + 1
      # print(result_i)
    } #Fin boucle de parcours des variables
  }
  return(list_complete)
}
# p <- profil_variable(data,var)
# p
