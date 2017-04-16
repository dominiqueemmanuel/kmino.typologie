#' Une fonction permettant de réaliser toutes les analyses factorielles.
#'
#' @param data est une\code{data.frame}.
#' @param var est un vecteur de \code{character} de noms de colonnes `data` constituant les variables actives de l'analyses./
#' @param weight soit \code{NULL} (pas  de pondération des indivudus) soit un vecteur de \code{numeric} positif de même longueur que \code{nrow(data)} .
#' @param  min_pop  est soit \code{NULL} (pas de contrainte) soit un \code{integer} indiquant la taille minimale de chaque cluster.
#' @param  nb_decoup  est un \code{integer} représentant le nombre de clusters à construire.
#' @param ... sont des paramètres complémentaires dont la nature dépend de la méthode (à définir donc selon les besoin des développements).

#' @export split_clustering
#' @examples
#' #Pas d'exemple pour le moment



split_clustering <- function(data,
                            var = colnames(data), # variabke actives
                            weight = NULL,
                            nb_decoup = 2,
                            min_pop = round(min(NROW(data)/1000, NROW(data)/nb_decoup/10)),
                            ...){

res <- factor_analysis(data=data,var_act = var,weight = weight,method="ACP/ACM globale")
clust <- clustering(data = res$ind_coord,var=colnames(res$ind_coord),min_pop=min_pop,nb_cluster = nb_decoup)


rownames(clust$centers)<-sort(unique(clust$id_cluster))

parangons <- lapply(sort(unique(clust$id_cluster)),function(tt){
  tt<-as.character(tt)
  id<-which(clust$id_cluster==tt)
  x<-res$ind_coord[id,,drop=FALSE]

  manual_centers0<-clust$centers[tt,,drop=FALSE]
  a<-proxy::dist(x,manual_centers0)[,1]
  nb_parangons<-5
  id[order(a)[seq_along(a)<=nb_parangons]]


})
names(parangons)<-sort(unique(clust$id_cluster))




return(list(id_cluster = clust$id_cluster,centers = clust$centers , ind_coord =res$ind_coord , var_coord =res$var_coord
            ,parangons = parangons
            ))
}
