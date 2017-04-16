#'clustering
#'
#' Une fonction permettant de réaliser toutes les analyses factorielles.
#' Version 0.01 de la fonction Clustering, cette version est softe et ne comprend pas en compte les contraintes sur la taille des clusters
#'
#' @param data est une \code{data.frame}.
#' @param var_to_caricature est un vecteur de \code{character} de noms de colonnes \code{data} constituant la ou les variables à caricaturer (dans le cas de plusieurs variables celles-ci seront sytnthéisée en une typologie  de \code{nb_cluster classes} au prélaable).
#' @param var_X est un vecteur de \code{character} de noms de colonnes \code{data} constituant la ou les variables explicatives pour la caricature.
#' @param weight soit \code{NULL} (pas  de pondération des indivudus) soit un vecteur de \code{numeric} positif de même longueur que \code{nrow(data)} .
#' @param  nb_cluster  est un \code{integer} représentant le nombre de clusters à construire.
#' @param  min_pop  est soit \code{NULL} (pas de contrainte) soit un \code{integer} indiquant la taille minimale de chaque cluster.
#' @param ... sont des paramètres complémentaires non utilisés.

#' @export caricature_clustering
#' @examples
#' #Pas d'exemple pour le moment


caricature_clustering <- function(data,
                                   var_to_caricature ,
                                   var_X,
                                   weight = NULL,
                                   nb_cluster = 5,
                                   min_pop = round(min(NROW(data)/1000, NROW(data)/nb_cluster/10)),
                                  names_bloc =  NULL ,#permet de donner des noms aux variables intermédiares
                                   ...){


  # verification des types des variables
  if(length(var_to_caricature)>1){
    res <- factor_analysis(data
                           ,var_act =var_to_caricature
                           , method = "ACP/ACM globale",
                           weight = weight
    )
    dataX <- res$ind_coord
    out <- clustering(data=dataX
                      ,var=colnames(dataX)
                      ,weight = weight
                      ,nb_cluster =nb_cluster
                      ,min_pop = min_pop
    )
    X <-  out$id_cluster
  } else {
    X<-data[,var_to_caricature]

  }
  data <-data[,var_X,drop=FALSE]
if(!is.null(names_bloc)){
  for(k in seq_along(var_X)){
    colnames(data)[which(colnames(data)==var_X[k])] <- names_bloc[k]
    data[[names_bloc[k]]] <- factor(data[[names_bloc[k]]])
  }
}

  for(k in colnames(data)){
    if(length(unique(data[[k]]))<=1)data[[k]]<-NULL
  }
  for(k in colnames(data)){
   if(class(data[[k]])[1]=="character")data[[k]]<-factor(data[[k]])
  }
data0<-data
colnames(data)<-paste0("`",colnames(data),"`")
data[["var_to_predict_dom"]]<-factor(X)
is_err<-FALSE
tryCatch({
model <- partykit::ctree(var_to_predict_dom~.
                           ,data=data,weights=weight
                           ,control = partykit::ctree_control(
                             minsplit=2
                             ,minprob=0
                             , minbucket=min_pop
                             ,mincriterion=0
                             ,maxdepth=ceiling(log2(nb_cluster))+1
                             ,maxsurrogate=0
                               ,splittry=20

                           )

  )
# plot(model)
nid <- partykit::nodeids(model)
iid0 <- nid[(nid %in% partykit::nodeids(model, terminal = TRUE))]
iid <- nid[!(nid %in% partykit::nodeids(model, terminal = TRUE))]
pv <- partykit::nodeapply(model, ids = iid,FUN = function(n) partykit::info_node(n)$p.value)
stat <- partykit::nodeapply(model, ids = iid,FUN = function(n) max(partykit::info_node(n)$criterion[1,]))

n<-partykit::nodeapply(model, ids = iid,FUN = function(n) n)[[1]]

while(length(iid0)>nb_cluster){
  # k<-iid[which.max(pv)]
  k<-iid[which.min(stat)]
  iid <- setdiff(iid,k)
  model <- partykit::nodeprune(model, ids = c(iid0,k))

  nid <- partykit::nodeids(model)
  iid0 <- nid[(nid %in% partykit::nodeids(model, terminal = TRUE))]
  iid <- nid[!(nid %in% partykit::nodeids(model, terminal = TRUE))]
  pv <- partykit::nodeapply(model, ids = iid,FUN = function(n) partykit::info_node(n)$p.value)
  stat <- partykit::nodeapply(model, ids = iid,FUN = function(n) max(partykit::info_node(n)$criterion[1,]))

}




},error=function(e)is_err<-TRUE)
if(is_err)return(list(id_cluster=NULL,rules=NULL ,centers = NULL ))
# plot(model)
  l<-partykit:::.list.rules.party(model)
  l<-gsub(', \"NA\",',",",l,fixed=TRUE)
  l<-gsub(', \"NA\"',"",l,fixed=TRUE)
  l<-gsub('\"NA\",',"",l,fixed=TRUE)
  L<-sapply(seq_along(l),function(t){
    1*eval(parse(text=l[t]),data0)
  })%>%matrix(ncol=length(l))
  L<-L%>%apply(1,function(t)l[which(t==1)])
  L<-factor(L,levels=l)
  L
  # save(file="E:/dom",list=ls())
  if(is.null(weight))weight<-rep(1,NROW(data))
  e<-unique(strsplit(paste0(levels(L),collapse=" & ")," & `")[[1]])
  e[!grepl("^`",e)]<-paste0("`",e[!grepl("^`",e)])
  centers <- data.frame((lapply(e,function(t){
    cat(".")
    1*eval(parse(text=t),data0)
}
    )%>%do.call(cbind,.)))
  x0<-prcomp(centers)
  x0<-x0$x[,1:min(which(cumsum(x0$sdev)/sum(x0$sdev)>0.9)),drop=FALSE]
  centers <- x0 * weight
  centers <- lapply(levels(L),function(t){
    id<-which(L==t)

    data.frame(t(apply(centers[id,,drop=FALSE],2,sum)))
  })%>%do.call(rbind,.)
  centers<-centers/sum(weight)
rownames(centers) <- sort(unique(as.numeric(L)))



  parangons <- lapply(sort(unique(as.numeric(L))),function(tt){
    tt<-as.character(tt)
    id<-which(L==tt)
    x<-x0[id,,drop=FALSE]

    manual_centers0<-centers[tt,,drop=FALSE]
    a<-proxy::dist(x,manual_centers0)[,1]
    nb_parangons<-5
    id[order(a)[seq_along(a)<=nb_parangons]]


  })
  names(parangons)<-sort(unique(as.numeric(L)))










  out<-list(id_cluster=as.numeric(L),rules=levels(L) ,centers = centers ,parangons = parangons)

  return(out)
}




