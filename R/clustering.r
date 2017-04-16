#'clustering
#'
#' Une fonction permettant de réaliser toutes les analyses factorielles.
#' Version 0.01 de la fonction Clustering, cette version est softe et ne comprend pas en compte les contraintes sur la taille des clusters
#'
#' @param data est une \code{data.frame}.
#' @param var est est un vecteur de \code{character} de noms de colonnes \code{data} constituant les variables à prendre en compte dans le clustering. Les variables sont ici supposées être toutes nummériques.
#' @param weight soit \code{NULL} (pas  de pondération des indivudus) soit un vecteur de \code{numeric} positif de même longueur que \code{nrow(data)} .
#' @param  nb_cluster  est un \code{integer} représentant le nombre de clusters à construire.
#' @param  min_pop  est soit \code{NULL} (pas de contrainte) soit un \code{integer} indiquant la taille minimale de chaque cluster.
#' @param  max_pop  est soit \code{NULL} (pas de contrainte) soit un \code{integer} indiquant la taille maximale de chaque cluster.
#' @param ... sont des paramètres complémentaires dont la nature dépend de la méthode (à définir donc selon les besoin des développements).

#' @export clustering
#' @examples
#' #Pas d'exemple pour le moment


clustering <- function(data,
                       var =colnames(data),
                       weight = NULL,
                       nb_cluster = 5,
                       min_pop = round(min(NROW(data)/1000, NROW(data)/nb_cluster/10)),
                       max_pop =  round(2*NROW(data)/nb_cluster),
                       seed=123,
                       ...){

  if(!is.null(seed))set.seed(seed)

  # verification des types des variables
  supported_type_quanti <- c("numeric", "integer")
  if( ! all(sapply(var, function(x){return( class(data[,x]) %in% supported_type_quanti)})) ){
    stop("AU MOINS UNE VARIABLE N EST PAS SUPPORTEE A CAUSE DE SON TYPE")
  }

  # Version 0.01 de la fonction Clustering



  nb_distinct_row <- NROW(unique(data[,var,drop=FALSE]))
  nb_cluster <- min(nb_distinct_row-1,nb_cluster)
if(nb_cluster==1){
if(is.null(weight)){
  weight<-rep(1,NROW(data))
} else {
  weight<-data[[weight]]
}
  e<-as.data.frame(matrix(colSums(data[,var,drop=FALSE]*weight)/sum(weight),nrow=1))
  colnames(e)<-var
  return(list(
    data
    ,var = var
    ,weight = weight
    ,nb_cluster = nb_cluster
    ,min_pop = min_pop
    ,max_pop = max_pop
    ,id_cluster =rep(1,NROW(data))# un vecteur (de mlongueur nrow(data)) d'entiers de 1 à nb_cluster représentant les identifiants des clusters.
    ,centers = e# une matrice de taile nb_cluster X length(var) représentant les centres des clusters
    ,converged = TRUE
  ))

}

  # Initialisation K-means : objectif est de réduire les dimensions avant le recours à la CAH

  res.kmeans <- kmeans(x = data[,var,drop=FALSE], centers = min(nb_distinct_row-1,500), nstart = 15, iter.max = 500)

  converged <- res.kmeans$ifault

  # CAH
  res.cah <- hclust(d = dist(x = res.kmeans$centers, method = "euclidean"), method = "ward.D")

  # Découpage de la CAH en k classes
  res.cut.cah <- cutree(tree = res.cah, k = nb_cluster)

  # Creation data frame resultat CAH et K-means
  data.res.cah <- data.frame(.class_kmeans = as.integer(row.names(res.kmeans$centers)), .class_cah = res.cut.cah )

  # Jointure avec le individus initiaux
  data.res.cah.ind <- merge(
    data.frame(data[,var,drop=FALSE], .class_kmeans = res.kmeans$cluster),
    data.res.cah
  )

  # Calcul des centroides sur les individus du resultat de CAH
  centers.res.cah <- aggregate.data.frame(data.res.cah.ind[,var,drop=FALSE],by = list(.class_cah=data.res.cah.ind[,".class_cah"]),FUN = mean)

  # Lissage du resultat final à l'aide d'un k-means

  res.kmeans <- kmeans(data[,var,drop=FALSE]
                       , centers = as.matrix(subset( centers.res.cah, select = -.class_cah ))
                       # , nstart = 15
                        , iter.max = 1
                       )




  centers <- res.kmeans$centers
  id_cluster <- res.kmeans$cluster




  x0<-data[,var,drop=FALSE]

  parangons <- lapply(sort(unique(as.numeric(id_cluster))),function(tt){
    tt<-as.character(tt)
    id<-which(id_cluster==tt)
    x<-x0[id,,drop=FALSE]

    manual_centers0<-centers[tt,,drop=FALSE]
    a<-proxy::dist(x,manual_centers0)[,1]
    nb_parangons<-5
    id[order(a)[seq_along(a)<=nb_parangons]]


  })
  names(parangons)<-sort(unique(as.numeric(id_cluster)))





  # A compléter
  return(list(
    data
    ,var = var
	  ,weight = weight
	  ,nb_cluster = nb_cluster
	  ,min_pop = min_pop
	  ,max_pop = max_pop
	  ,id_cluster = id_cluster # un vecteur (de mlongueur nrow(data)) d'entiers de 1 à nb_cluster représentant les identifiants des clusters.
    ,centers = centers# une matrice de taile nb_cluster X length(var) représentant les centres des clusters
    , parangons = parangons
    ,converged = (converged==0)
  ))
}


