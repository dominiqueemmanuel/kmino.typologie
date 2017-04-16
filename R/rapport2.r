#' @title Profil d'une variable
#' @description Fonction permettant de créer des résumés sous forme de texte (affichables) des distributions des variables d'une table.
#'
#' @param out est le résultat fournit par un appel de la fonction \code{factor_analysis}.
#' @param data_role est une \code{data.frame} issue directement de l'interface et fournissant des informaitons sur les stauts des variabkes.
#' @param ... sont des paramètres complémentaires dont la nature dépend de la méthode (à définir donc selon les besoin des développements).

#' @export rapport2

#' @examples
#' #Pas d'exemple pour le moment

rapport2 <- function(file, detail,param_init,data_role,ttl,id_analysis, init,all_data, nom,weight=NULL,var_selected_parangon = NULL,lang = "fr",...){
  # rm(list=ls())
  # load("E:/dom20")
  # weight=NULL
  # file <- 'E:/test.pptx'
  var_selected_parangon<-colnames(all_data$table)[seq_along(colnames(all_data$table))<=10]
  title = "Clustering"
  method<-name <- nom
library(ggplot2)
  if(is.null(weight)){
    weight <- rep(1,NROW(all_data$table ))
  } else {
    weight<-all_data$table[,weight]
  }

  model_file <- system.file("data/modelR2.pptx",package = "kmino.typologie")
  # model_file<-"./data/modelR2.pptx"
   # model_file<-"../../packages/kmino_packages_development/kmino.typologie/data/modelR2.pptx"
  library(ReporteRs)
  mydoc <- pptx(  title = title
                  ,template = model_file
  )
  date<-format(Sys.Date(),"%d/%m/%Y")

  ## titre
  mydoc <- addSlide(mydoc,slide.layout = 'titre_global')
  mydoc <- addParagraph( mydoc,  paste0(title))
  mydoc <- addParagraph( mydoc, date)



  mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
  mydoc <- addParagraph( mydoc,  paste0(""))
  mydoc <- addParagraph( mydoc,  paste0("Rappel de la demande : initialisation"))
  e<-param_init
  colnames(e)<-stringr::str_wrap(colnames(e),20)
  MyFTable <- vanilla.table(e)
  # MyFTable = setZebraStyle( MyFTable, odd = '#eeeeee', even = 'white' )

  MyFTable <- setFlexTableWidths( MyFTable, widths = c(2, 2,2,2 ))

  mydoc = addFlexTable( doc = mydoc,MyFTable ,height=4,offx=0.5,offy=1.5,width=6)



  r<-sapply(ttl[-1],function(x)paste0(x$type," : Typologie = ",x$params$Typologie," / ID Classe = ",paste0(x$params[[2]],collapse=", ")))
  if(length(r)>0){
    r<-r%>%paste0(collapse="\n")
    mydoc <- addSlide(mydoc,slide.layout = 'rendu1b')
    mydoc <- addParagraph( mydoc,  paste0(r))
    mydoc <- addParagraph( mydoc,  paste0("Rappel de la demande : retouche(s)"))

  }

  typo <- unique(detail$Typologie)

  t <- typo[length(typo)]

  if(isTRUE(method=="ACP/ACM répétées par blocs")){


    ## global
    mydoc <- addSlide(mydoc,slide.layout = 'titre_section')
    mydoc <- addParagraph( mydoc,  paste0("Résultat typologie '[GLOBAL]'"))

    d1<-subset(detail,Typologie==t)[,c("ID Classe","Libellé Classe")]
    d1<-lapply(d1$`ID Classe`,function(x){
      data.frame("Effectif"=sum(init[[t]]$id_cluster==x),"Effectif Pondéré"=sum(weight*(init[[t]]$id_cluster==x)),check.names = FALSE)
    })%>%do.call(rbind,.)%>%cbind(d1,.)
    d1<-subset(d1,`ID Classe`>0)


a<-"[GLOBAL]"
sfx <- paste0("\n > ",a)
mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
mydoc <- addParagraph( mydoc,  paste0(""))
mydoc <- addParagraph( mydoc,  paste0("Effectifs obtenus",sfx))
MyFTable <- vanilla.table(d1)
MyFTable = setZebraStyle( MyFTable, odd = '#eeeeee', even = 'white' )
MyFTable <- setFlexTableWidths( MyFTable, widths = c(2, 2,2,2 ))
mydoc = addFlexTable( doc = mydoc,MyFTable ,height=4,offx=0.5,offy=1.5,width=6)


if(length(init)==3){



  d1<-subset(detail,Typologie==names(init)[1])[,c("ID Classe","Libellé Classe")]
  d1<-subset(d1,`ID Classe`>0)

  d2<-subset(detail,Typologie==names(init)[2])[,c("ID Classe","Libellé Classe")]
  d2<-subset(d2,`ID Classe`>0)


  e<-cbind(sapply(init[-length(init)],function(x)x$id_cluster),init[[length(init)]]$id_cluster)
  colnames(e)[NCOL(e)]<-"[GLOBAL]"
  e<-e[e[,1]>0 & e[,2]>0 & e[,3]>0,,drop=FALSE]
  head(e)


  e0<-as.data.frame(e)
  colnames(e0)<-c("x","y","z")


  e1<-reshape2::dcast(e0,x~y,fun.aggregate =length,value.var="z")
  rownames(e1) <- e1[,1]
  e1<-e1[,-1]

  e2<-reshape2::dcast(e0,x~y,fun.aggregate =function(t)t[1],value.var="z")
  rownames(e2) <- e2[,1]
  e2<-e2[,-1]


  rownames(e1) <- rownames(e2) <- paste0(d1$`Libellé Classe`," (ID=",d1$`ID Classe`,")")%>%stringr::str_wrap(12)

  d2<-d2[match(as.numeric(colnames(e2)),d2$`ID Classe`),]
  colnames(e1) <- colnames(e2) <- paste0(d2$`Libellé Classe`," (ID=",d2$`ID Classe`,")")%>%stringr::str_wrap(12)

  x<-sapply(sort(unique(e0$x)),function(t)e0$x==t)
  y<-sapply(sort(unique(e0$y)),function(t)e0$y==t)
  z<-sapply(sort(unique(e0$z)),function(t)e0$z==t)


  m<-rpart::rpart(z~x+y,data=transform(e0,x=factor(x),y=factor(y),z=factor(z)))
  a<-predict(m,type='class')
  b<-m$where
  e00<-e0[order(a,e0$x,e0$y,b,e0$z),]
  id1<-match(unique(e00$x),d1$`ID Classe`)
  id2<-match(unique(e00$y),d2$`ID Classe`)

  unique(e00$x)
  rownames(e1)[id1]


  e1<-e1[id1,id2]
  e2<-e2[id1,id2]
  a<-colnames(e2)
  e2$ID_TYPO1<-rownames(e2)

  e2b<-reshape2::melt(e2,id.vars="ID_TYPO1",variable.name ="ID_TYPO2")
  e2b$ID_TYPO1 <- factor(as.character(e2b$ID_TYPO1),levels=rownames(e2))
  e2b$ID_TYPO2 <- factor(as.character(e2b$ID_TYPO2),levels=a)


  e1$ID_TYPO1<-rownames(e1)
  e1b<-reshape2::melt(e1,id.vars="ID_TYPO1",variable.name ="ID_TYPO2",value.name="value2")
  e1b$ID_TYPO1 <- factor(as.character(e1b$ID_TYPO1),levels=rownames(e2))
  e1b$ID_TYPO2 <- factor(as.character(e1b$ID_TYPO2),levels=a)
  e2b<-merge(e1b,e2b)
  library(ggplot2)
  e2b$value2<-sqrt(e2b$value2)
  e2b$value2<-e2b$value2-min(e2b$value2)
  if(max(e2b$value2,na.rm=TRUE)>0)e2b$value2<-e2b$value2/max(e2b$value2,na.rm=TRUE)
  p<-ggplot(na.omit(e2b), aes(x=ID_TYPO1,y= ID_TYPO2)) + geom_point(aes(color = factor(value),size=value2))+scale_size(range=c(5,20),guide="none")
  p<-p+xlab(colnames(e)[1])+ylab(colnames(e)[2])
  p<-p+ggthemes::theme_gdocs()+ggthemes::scale_color_gdocs()
  p<-p+labs(color="[GLOBAL]")


  mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
  mydoc = addPlot( doc = mydoc, fun = print, x =p)
  mydoc <- addParagraph( mydoc,  paste0("Heatmap",sfx))



} else {




  e<-cbind(sapply(init[-length(init)],function(x)x$id_cluster),init[[length(init)]]$id_cluster)
  colnames(e)[NCOL(e)]<-"[GLOBAL]"
  e<-as.data.frame(e)
  e$weight <- weight
  e<-e[e[,1]>0 & e[,2]>0 & e[,3]>0,,drop=FALSE]

  for(kk in seq(NCOL(e)-1)){
    d1<-subset(detail,Typologie==colnames(e)[kk])[,c("ID Classe","Libellé Classe")]
    d1<-subset(d1,`ID Classe`>0)
    e[,kk]<-e[,kk]%>%plyr::mapvalues(d1$`ID Classe`, paste0(d1$`Libellé Classe`," (ID=",d1$`ID Classe`,")"))%>%stringr::str_wrap(25)
    }
  e<-as.data.frame(eval(parse(text=paste0("e%>%dplyr::group_by("
  ,paste0("`",colnames(e)[-length(e)],"`",collapse=",")
  ,")%>%summarise(Effectif=n(),`Effectif pondéré`=sum(weight))"))))


  nnk<-12
  nk<-floor((NROW(e)-1)/nnk)
  for(k in seq(0,nk)){
    mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
    id<-seq(nnk*k+1,min(nnk*(k+1),NROW(e)))
    print(length(id))
    a<-""
    if(nk>0)a<-paste0(" (",k+1,"/",nk+1,")")

    mydoc <- addParagraph( mydoc,  paste0(""))
    mydoc <- addParagraph( mydoc,  paste0("Croisement des typologies",a,sfx))
    MyFTable <- vanilla.table(e[id,,drop=FALSE])
     MyFTable = setZebraStyle( MyFTable, odd = '#eeeeee', even = 'white' )

     MyFTable <- setFlexTableWidths( MyFTable, widths = c(rep(2,NCOL(e)-2),1,1))

    mydoc = addFlexTable( doc = mydoc,MyFTable ,height=4,offx=0.5,offy=1.5,width=6)

  }
}

d1<-subset(detail,Typologie==t)[,c("ID Classe","Libellé Classe")]
d1<-lapply(d1$`ID Classe`,function(x){
  data.frame("Effectif"=sum(init[[t]]$id_cluster==x),"Effectif Pondéré"=sum(weight*(init[[t]]$id_cluster==x)),check.names = FALSE)
})%>%do.call(rbind,.)%>%cbind(d1,.)
d1<-subset(d1,`ID Classe`>0)

  parangons <- init[[length(init)]]$parangons
  ee<-names(parangons)
  ee<-ee[as.numeric(ee)>0]
  for(ke in ee){

    e<-all_data$table[parangons[[ke]],var_selected_parangon]
    e<-t(e)
    e
    rownames(e)<-plyr::mapvalues(rownames(e),colnames(all_data$table),all_data$column_label,warn_missing = FALSE)
    rownames(e)<-str_wrap(rownames(e),15)
    e<-data.frame(Variable = rownames(e),e,check.names = FALSE)
    e<-as.matrix(e)
    e[,]<-str_wrap(as.vector(e),15)
    e<-as.data.frame(e)
    colnames(e)[-1]<-paste0("Parangon ",seq(NCOL(e)-1))
    rownames(e)<-NULL
    nnk<-8
    nk<-floor((NROW(e)-1)/nnk)
    for(k in seq(0,nk)){
      mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
      id<-seq(nnk*k+1,min(nnk*(k+1),NROW(e)))
      print(length(id))
      a<-""
      if(nk>0)a<-paste0(" (",k+1,"/",nk+1,")")

      mydoc <- addParagraph( mydoc,  paste0(""))
      mydoc <- addParagraph( mydoc,  paste0(subset(d1,`ID Classe`==ke)$`Libellé Classe`[1]," - Parangons",a,sfx))
      MyFTable <- vanilla.table(e[id,,drop=FALSE])
      MyFTable = setZebraStyle( MyFTable, odd = '#eeeeee', even = 'white' )

      MyFTable <- setFlexTableWidths( MyFTable, widths = c(1.5,rep(1,NCOL(e)-1)))

      mydoc = addFlexTable( doc = mydoc,MyFTable ,height=4,offx=0.5,offy=1.5,width=6)

    }


  }


  }

### cluster manuel

d1<-subset(detail,Typologie==t)[,c("ID Classe","Libellé Classe")]
d1<-lapply(d1$`ID Classe`,function(x){
  data.frame("Effectif"=sum(init[[t]]$id_cluster==x),"Effectif Pondéré"=sum(weight*(init[[t]]$id_cluster==x)),check.names = FALSE)
})%>%do.call(rbind,.)%>%cbind(d1,.)
d1<-subset(d1,`ID Classe`<0)
if(NROW(d1)>0){


mydoc <- addSlide(mydoc,slide.layout = 'titre_section')
mydoc <- addParagraph( mydoc,  paste0("Résultat classe(s) manuelle(s)"))


a<-"[GLOBAL]"
sfx <- paste0("\n > ",a)
mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
mydoc <- addParagraph( mydoc,  paste0(""))
mydoc <- addParagraph( mydoc,  paste0("Effectifs obtenus",sfx))
MyFTable <- vanilla.table(d1)
MyFTable = setZebraStyle( MyFTable, odd = '#eeeeee', even = 'white' )
MyFTable <- setFlexTableWidths( MyFTable, widths = c(2, 2,2,2 ))
mydoc = addFlexTable( doc = mydoc,MyFTable ,height=4,offx=0.5,offy=1.5,width=6)


## par blocs
for( a in names(init)[-length(init)]){
  d1<-subset(detail,Typologie==a)[,c("ID Classe","Libellé Classe")]
  d1<-lapply(d1$`ID Classe`,function(x){
    data.frame("Effectif"=sum(init[[a]]$id_cluster==x),"Effectif Pondéré"=sum(weight*(init[[a]]$id_cluster==x)),check.names = FALSE)
  })%>%do.call(rbind,.)%>%cbind(d1,.)
  d1<-subset(d1,`ID Classe`>0)

  mydoc <- addSlide(mydoc,slide.layout = 'titre_section')
  mydoc <- addParagraph( mydoc,  paste0("Résultat par bloc : ",a))


  sfx <- paste0("\n > ",a)
  mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
  mydoc <- addParagraph( mydoc,  paste0(""))
  mydoc <- addParagraph( mydoc,  paste0("Effectifs obtenus",sfx))
  MyFTable <- vanilla.table(d1)
  MyFTable = setZebraStyle( MyFTable, odd = '#eeeeee', even = 'white' )
  MyFTable <- setFlexTableWidths( MyFTable, widths = c(2, 2,2,2 ))
  mydoc = addFlexTable( doc = mydoc,MyFTable ,height=4,offx=0.5,offy=1.5,width=6)

  #dendro
  centers <-  init[[a]]$centers
  centers <- centers[as.numeric(rownames(centers))>0,,drop=FALSE]
  rownames(centers) <- plyr::mapvalues(rownames(centers),d1$`ID Classe`,d1$`Libellé Classe`)%>%str_wrap(20)


  d<-dist(centers)
  d<-hclust(d,method="ward.D")
  p<-ggdendro::ggdendrogram(d)


  mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
  mydoc = addPlot( doc = mydoc, fun = print, x =p)
  mydoc <- addParagraph( mydoc,  paste0("Dendrogramme",sfx))



  #heatmap 1
for(type_var in c("Active","Passive")){
  if(type_var=="Active"){
  var_act <- (data_role%>%subset(Bloc==a & `Rôle`==type_var))$`ID variable`
  } else {
    var_act <- (data_role%>%subset(`Rôle`==type_var))$`ID variable`

  }
  if(length(var_act)>0){
  x<-all_data$table[,var_act,drop=FALSE]


for(z in seq(NCOL(x))){
  if(is.numeric(x[,z]) & length(unique(x[,z]))>5){
    x<-kmino.typologie::discretisation(data=x,var=var_act[z])

  }
  x[,z]<-factor(x[,z])
}

  # x<-model.matrix(~ (-1+.),x)
  colnames(x)<-str_wrap(colnames(x))
  x<-as.data.frame(x)
x$KMINO_CLUSTER <-init[[a]]$id_cluster

head(x)
x$weight <-weight
x<-subset(x,KMINO_CLUSTER>0)
y<-reshape2::melt(x,id.vars=c("KMINO_CLUSTER","weight"))
z1<-y%>%group_by(KMINO_CLUSTER,variable,value)%>%summarise(w1=sum(weight))
z2<-y%>%group_by(variable,value)%>%summarise(w2=sum(weight))
z<-inner_join(z1,z2)%>%mutate(val=w1/w2)%>%dplyr::select(-w1,-w2)
z
z1<-y%>%group_by(KMINO_CLUSTER,variable)%>%summarise(w1=sum(weight))
z2<-y%>%group_by(variable)%>%summarise(w2=sum(weight))
z2<-inner_join(z1,z2)%>%mutate(val2=w1/w2)%>%dplyr::select(-w1,-w2)
z<-inner_join(z,z2)%>%mutate(Indice = round(100*val/val2,1))%>%select(-val,-val2)
z<-reshape2::dcast(z,variable+value~KMINO_CLUSTER,fun.aggregate = function(t)t[1],value.var="Indice")
z$variable<-str_wrap(z$variable,15)
z$value<-str_wrap(z$value,15)
colnames(z)[-c(1,2)]<-plyr::mapvalues(colnames(z)[-c(1,2)],d1$`ID Classe`,d1$`Libellé Classe`)
colnames(z)
z<-z[which(sapply(seq(NROW(z)),function(x){
  min(z[x,-c(1,2)],na.rm=TRUE)>90 | max(z[x,-c(1,2)],na.rm=TRUE)>110
})),,drop=FALSE]

z2<-z

imax<-min(500,max(z[,-c(1,2)],na.rm=TRUE))
imin<-as.vector(as.matrix(z[,-c(1,2)]))
imin<-min(imin[imin>0],na.rm=TRUE)
# dominque
nnk<-15
nk<-floor((NROW(z)-1)/nnk)
for(k in seq(0,nk)){
    id<-seq(nnk*k+1,min(nnk*(k+1),NROW(z)))

  aa<-""
  if(nk>0)aa<-paste0(" (",k+1,"/",nk+1,")")
z2<-z[id,,drop=FALSE]
q<-z2[,c(-1,-2)]
rownames(q)<-z2[,1]%>%paste0(" : ",z2[,2])
q<-as.matrix(q)
rownames(q)<-rownames(q)%>%str_wrap(35)
colnames(q)<-colnames(q)%>%str_wrap(18)
q<-data.frame(Var1=rownames(q),q,check.names = FALSE)
rownames(q)<-NULL
q2<-reshape2::melt(q,id.vars="Var1",variable.name = "Var2", value.name="Indice")
q2$Var1<-factor(q2$Var1,levels = rev(as.character(q$Var1)))

library(ggthemes)



p<-ggplot(q2, aes(Var2, Var1)) +
  geom_tile(aes(fill = Indice)) +
  geom_text(aes(label = round(Indice, 1))) +
  scale_fill_gradient2(
    na.value ="white",low = "red", high = "green"
    ,midpoint =100
    ,trans=scales::trans_new("my_trans",transform = function(x)ifelse(x>500,500,x),inverse=function(x)x)
     ,limits=c(imin,imax)
    ) +xlab("")+ylab("")+theme_bw()
mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
mydoc = addPlot( doc = mydoc, fun = print, x =p)
mydoc <- addParagraph( mydoc,  paste0("Heatmap Variables ",tolower(type_var),"s vs. Classes",aa,sfx))

}
} # fin if(length(var_act)>0)
} # fin for(type_var %in% c("Active","Passive")
}
}
  writeDoc( mydoc,file)


}
