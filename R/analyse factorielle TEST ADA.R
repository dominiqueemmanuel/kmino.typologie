# library(FactoMineR)
# data(wine)
#
# ##############
# res <- 1
#
#
# # verification des types des variables
# supported_type_quanti <- c("numeric", "integer")
# supported_type_quali <- c("ordered", "factor", "character")
#
# # function is mixte variables
# isMixte <- function(data = NULL, supported_type = NULL){
#   classe_colonne <- sapply(data, function(x) {return(class(x) %in% supported_type) })
#   return(!all(classe_colonne))
# }
#
# # test
# isMixte(data = wine[,c(1,2,3,4,5,6,7,8)], supported_type = supported_type_quanti)
#
#
#
# ##
# min(which(res$eig$`cumulative percentage of variance` > 80))
#
#
#
# # function is ddl
# computeNbCP <- function(data = NULL, type_quali = NULL){
#   nbMod <- sapply(data, function(x) { if(class(x) %in% type_quali){ return(length(unique(x))-1) }else{return(1)}})
#   return (sum(nbMod))
# }
#
#
# data = wine#[,c(1,2,3,4,5,6,7,8)]
# var <- colnames(data)
# type_quali = supported_type_quali
#
#
# computeNbCP(data = data, type_quali = type_quali)
#
#
#
# colnames(wine)
#
#
# charVec <- which(sapply(data[,var], class) == "factor")
#
# charVec <- which(sapply(data[,var], class) == "character")
# if(length(charVec)>0){
#   for(e in names(charVec)){
#     data[,e] <- as.factor(data[,e])
#   }
# }
#
#
# sample(c("aa","bb","cc","dd"),nrow(wine), replace = TRUE)
#
#
#
# class("ddl")
#
#
#
#
# res <- list(
#     var = var
#     , method = method
#     , weight = weight
#     , group_var_id = group_var_id
#     , group_ind_id = group_ind_id
#     , var_coord = #une matrice dont les colonnes sont les axes factoriels, les lignes sont les variables avec les rownames = {noms des variables quanti} ou {nom des variables quali == nom de la modalité}
#       , ind_coord = # une matrix dont les colonnes sont les axes factoriels, les lignes les individus
#       , eig = # Valeurs propres des axes factoriels
#   )
#
#
#
#
#
# model1 <- factor_analysis(
#      data=iris
#      ,var = c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width" )
#      ,method = "ACP/ACM globale"
#      ,group_var_id = NULL
#      ,group_ind_id = NULL
#      ,nb_axe = "Automatique"
#     )




clustering <- function(data,
                       var ,
                       weight = NULL,
                       nb_cluster = 5,
                       min_pop = round(min(NROW(data)/1000, NROW(data)/nb_cluster/10)),
                       max_pop =  round(2*NROW(data)/nb_cluster),
                       ...){

  data <- iris
  var <- c("Petal.Width","Petal.Length","Sepal.Width","Sepal.Length")#,"Species")
  #data <- data[sample(1:nrow(data),50000, replace = TRUE),]
  library(MASS)
  #data <- mvrnorm(n = 100000, rnorm(5), matrix(runif(25),nrow = 5))
  data <- cbind(data.frame(mvrnorm(n = 5000, mu = c(-200,0,0,0), diag(x = runif(4, max = 1.6))), class = 1))
  data <- rbind(data,cbind(data.frame(mvrnorm(n = 5000, mu = c(0,5,0,0), diag(x = runif(4, max = 2.6))), class = 2)))
  data <- rbind(data,cbind(data.frame(mvrnorm(n = 5000, mu = c(0,0,22,0), diag(x = runif(4, max = .6))), class = 3)))
  data <- rbind(data,cbind(data.frame(mvrnorm(n = 5000, mu = c(0,0,0,-50), diag(x = runif(4, max = 1.6))), class = 4)))
  data <- rbind(data,cbind(data.frame(mvrnorm(n = 5000, mu = c(0,0,20,0), diag(x = runif(4, max = 3))), class = 5)))

  colnames(data) <- c(var,"class")
  head(data)

  nb_cluster <- 5

  #plot(data)
  # Basic Scatterplot Matrix
  pairs( ~ Petal.Width+Petal.Length+Sepal.Width+Sepal.Length,data=data)

  # verification des types des variables
  supported_type_quanti <- c("numeric", "integer")
  if( ! all(sapply(var, function(x){return( class(data[,x]) %in% supported_type_quanti)})) ){
    stop("AU MOINS UNE VARIABLE N EST PAS SUPPORTEE A CAUSE DE SON TYPE")
  }

  # Initialisation K-means
res.kmeans <- kmeans(x = data[,var], centers = 500, nstart = 5,iter.max = 15)
res.kmeans
res.kmeans$ifault


res.kmeans <- 2

identical(z1,z1)

res.kmeans <- tryCatch({ kmeans(x = data[,var], centers = 500, nstart = 2,iter.max = 10) },
                         warning=function(w) {
                           ## do something about the warning, maybe return 'NA'
                           message("handling warning: ", conditionMessage(w))
                           NA
                           }
                         )


  #res.kmeans$centers

  # CAH
  res.cah <- hclust(dist(res.kmeans$centers)^2, "cen")
  #plot(res.cah)
  #
  res.cut.cah <- cutree(res.cah, k = nb_cluster)



  #data.res.cah <- data.frame(res.kmeans$centers, class_res_cah = res.cut.cah )
  data.res.cah <- data.frame(.class_kmeans = as.integer(row.names(res.kmeans$centers)), .class_cah = res.cut.cah )


  data.res.cah.ind <- merge(
    data.frame(data, .class_kmeans = res.kmeans$cluster),
    data.res.cah
    )

  table(data.res.cah.ind$class,data.res.cah.ind$.class_cah)


  centers.res.cah <- aggregate.data.frame(data.res.cah.ind[,var],by = list(.class_cah=data.res.cah.ind[,".class_cah"]),FUN = mean)

  res.kmeans <- kmeans(data[,var], centers = subset( centers.res.cah, select = -.class_cah ))


  table(data$class,res.kmeans$cluster)


  res <- clustering(data = data,var = var, nb_cluster = 5)

  res$converged

  #data.frame(class_kmeans = as.integer(row.names(res.kmeans$centers)), class_res_cah = res.cut.cah )
  #data.res.cah <- data.frame(class_kmeans = names(res.kmeans$centers), class_res_cah = res.cut.cah )



  # A compléter
  return(list(
    var = var
    ,weight = weight
    ,nb_cluster = nb_cluster
    ,min_pop = min_pop
    ,max_pop = max_pop
    ,id_cluster = # un vecteur (de mlongueur nrow(data)) d'entiers de 1 à nb_cluster représentant les identifiants des clusters.
      ,centers = # une matrice de taile nb_cluster X length(var) représentant les centres des clusters
  ))
}



####################
# - intégration de la fonction clustering
#### - la fonction basique
#### -
# - tester sur un jeu de données de bout en bout
####################








