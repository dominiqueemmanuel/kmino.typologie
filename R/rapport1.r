#' @title Profil d'une variable
#' @description Fonction permettant de créer des résumés sous forme de texte (affichables) des distributions des variables d'une table.
#'
#' @param out est le résultat fournit par un appel de la fonction \code{factor_analysis}.
#' @param data_role est une \code{data.frame} issue directement de l'interface et fournissant des informaitons sur les stauts des variabkes.
#' @param ... sont des paramètres complémentaires dont la nature dépend de la méthode (à définir donc selon les besoin des développements).

#' @export rapport1

#' @examples
#' #Pas d'exemple pour le moment

rapport1 <- function(file, out, data_role,lang = "fr",...){
  title = "Analyse de données"
  name <- out$method
library(ggplot2)

  model_file <- system.file("data/modelR1.pptx",package = "kmino.typologie")
  # model_file<-"./data/modelR1.pptx"
  library(ReporteRs)
  mydoc <- pptx(  title = title
                  ,template = model_file
  )
  date<-format(Sys.Date(),"%d/%m/%Y")

  ## titre
  mydoc <- addSlide(mydoc,slide.layout = 'titre_global')
  mydoc <- addParagraph( mydoc,  paste0(title,"\n",name))
  mydoc <- addParagraph( mydoc, date)




  method<-out$method

  more_results <- out$more_results

  e<-data_role[,c("ID variable","Libellé variable","Rôle","Bloc")]
  # e<-subset(e,`Rôle`!="A ignorer")
  e<-e[e[["Rôle"]]!="A ignorer",,drop=FALSE]
  e<-e[order(e$Bloc,as.numeric(rownames(e))),]

  e<-paste0(   "\n\n\nMéthode : ",method
               ,"\n\nVariable(s) active(s) : ",sum(e[["Rôle"]]=="Active")
               ,"\n\nVariable(s) passive(s) : ",sum(e[["Rôle"]]=="Passive")
               ,"\n\nPrésence d'un poids : ",ifelse(sum(e[["Rôle"]]=="Poids")>0,"Oui","Non")
  )
  mydoc <- addSlide(mydoc,slide.layout = 'rendu1b')
  mydoc <- addParagraph( mydoc,  e)
  mydoc <- addParagraph( mydoc,  paste0("Rappel de la demande"))




  if(isTRUE(method=="ACP/ACM répétées par blocs")){



    e<-data_role[,c("ID variable","Libellé variable","Rôle","Bloc")]
    e<-e[e[["Rôle"]]!="A ignorer",,drop=FALSE]
    e<-e[order(e$Bloc,as.numeric(rownames(e))),]

    nnk<-20
    nk<-floor((NROW(e)-1)/nnk)
    for(k in seq(0,nk)){
      mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
      id<-seq(nnk*k+1,min(nnk*(k+1),NROW(e)))
      print(length(id))
      a<-""
      if(nk>0)a<-paste0(" (",k+1,"/",nk+1,")")

      mydoc <- addParagraph( mydoc,  paste0(""))
      mydoc <- addParagraph( mydoc,  paste0("Rappel de la demande : liste et rôles des variables",a))
      MyFTable <- vanilla.table(e[id,,drop=FALSE])
      # MyFTable = setZebraStyle( MyFTable, odd = '#eeeeee', even = 'white' )

      MyFTable <- setFlexTableWidths( MyFTable, widths = c(3, 3,1,2 ))

      mydoc = addFlexTable( doc = mydoc,MyFTable ,height=4,offx=0.5,offy=1.5,width=6)

    }






    for(i in seq_along(more_results)){
      # i<-1

      out0<-more_results[[i]]
      a<-names(more_results)[i]

      # sfx<-pot(paste0()format=textProperties(color="black",font.size=12, font.family="Calibri"))
      sfx <- paste0("\n > ",a)

      mydoc <- addSlide(mydoc,slide.layout = 'titre_section')
      mydoc <- addParagraph( mydoc,  paste0("Résultat bloc '",a,"'"))
      e<-data.frame(Information = out0$info_axis)
      e$Axe <- paste0(seq(NROW(e)))
      e$Axe<-factor(e$Axe,levels=e$Axe)
      p<-ggplot(e,aes(x=Axe,y=Information))+geom_bar(stat='identity',fill="steelblue")+ylab("Information")
      p<-p+theme_classic(base_size = 16)
      p<-p+theme(axis.text=element_text(size=9))
      p<-p+ scale_y_continuous(labels = scales::percent)
      p<-p+xlab(paste0("Axes\n(",round(100*out0$total_info_kept,1)," % de l'information retenue au total)"))
      # p<-p+ theme(panel.grid.major.x = element_line(colour = "grey90"))
      mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
      mydoc = addPlot( doc = mydoc, fun = print, x =p)
      mydoc <- addParagraph( mydoc,  paste0("Information retenue par axe",sfx))




      za<-min(5,NCOL(out0$var_contrib))
# save(list=ls(),file="E:/dom16")
      contrib <- as.data.frame(out0$var_contrib[,seq(za),drop=FALSE])
      colnames(contrib) <- paste0("Axe ",seq(za))
      contrib<-contrib[order(contrib[,1]),,drop=FALSE]
      e<-contrib$Variable <- rownames(contrib)%>%stringr::str_wrap(50)
      contrib<-reshape2::melt(contrib,id.vars="Variable",variable.name = "Axe")
      contrib$Variable<-factor(contrib$Variable,levels=e)
      contrib$Axe<-factor(contrib$Axe,levels=paste0("Axe ",seq(za)))
      p<-ggplot(contrib,aes(x=Variable,y=value))+geom_bar(stat='identity',fill="steelblue")+facet_grid(.~Axe) + coord_flip()+ylab("Contributions")
      p<-p+theme_classic(base_size = 16)
      p<-p+theme(axis.text=element_text(size=9))
      p<-p+ theme(panel.grid.major.y = element_line(colour = "grey90"))
      mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
      mydoc = addPlot( doc = mydoc, fun = print, x =p)
      mydoc <- addParagraph( mydoc,  paste0("Contributions des variables aux premiers axes",sfx))




      e<-which(sapply(seq(NCOL(out0$ind_contrib)),function(tt){
        e<-which(is.na(kmino.typologie::valeur_extreme(data.frame(x=out0$ind_contrib[,tt]),var="x",winsorisation=FALSE)$x))
        e<-e[out0$ind_contrib[,tt][e]>mean(out0$ind_contrib[,tt])]
        e<-length(e)
        e<=0.05*NROW(out0$ind_contrib)
      }))

      if(length(e)>0){
        e<-paste0(paste0("Axe ",e),collapse="\n")
        mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
        mydoc <- addParagraph( mydoc,  paste0("ATTENTION, les axes suivants sont sensibles à un petit nombre d'obervations :\n",e))
        mydoc <- addParagraph( mydoc,  paste0("Axes sensibles à un petit nombre d'observations",sfx))
      }



      total_info <- out0$total_info_kept
      info_axis <- out0$info_axis
      barplot(info_axis)
      if(NCOL(out0$var_coord)>=2){

        e<-as.data.frame(out0$var_coord[,1:2])
        colnames(e)<-c("x","y")
        e$label <- rownames(e)%>%stringr::str_wrap(20)
        library(ggplot2)
        library(ggrepel)
        add.alpha <- function(col, alpha=1){
          if(missing(col))
            stop("Please provide a vector of colours.")
          apply(sapply(col, col2rgb)/255, 2,
                function(x)
                  rgb(x[1], x[2], x[3], alpha=alpha))
        }
        p<-ggplot()   + geom_segment(data=e,aes(x=0,xend=x,y=0,yend=y),size=1,arrow=arrow(length = unit(0.01, "npc")))+
          xlab(paste0("Axe 1 (",round(100*info_axis[1],1),"%)"))+
          ylab(paste0("Axe 2 (",round(100*info_axis[2],1),"%)"))+
          geom_label_repel(aes(x=x,y=y,label=label)
                           ,force=0.2
                           ,size=4
                           ,label.padding =  unit(0.15, "lines")
                           ,fill=add.alpha("lightblue",0.75)
                           ,data=e
                           , segment.color ="grey80"
          )
        # p<-p+xlim(-1,1)+ylim(-1,1)+ ggforce::geom_circle(data=data.frame(x0=0,y0=0,r=1),mapping=aes(x0=x0,y0=y0,r=r))
        p<-p+theme_classic(base_size = 16)

        mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
        mydoc = addPlot( doc = mydoc, fun = print, x =p)
        mydoc <- addParagraph( mydoc,  paste0("Mapping des variables (Axes 1 et 2)",sfx))



        e<-as.data.frame(out0$ind_coord[,1:2])
        colnames(e)<-c("x","y")

        library(ggplot2)
        library(ggrepel)
        p<-ggplot(data=e)   + geom_point(aes(x=x,y=y),color=rgb(0,0,0,0.25))+
          xlab(paste0("Axe 1 (",round(100*info_axis[1],1),"%)"))+
          ylab(paste0("Axe 2 (",round(100*info_axis[2],1),"%)"))

        p<-p+theme_classic(base_size = 16)

        mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
        mydoc = addPlot( doc = mydoc, fun = print, x =p)
        mydoc <- addParagraph( mydoc,  paste0("Mapping des individus (Axes 1 et 2)",sfx))


        if(NCOL(out0$ind_coord)>=3){
          # 1 3

          e<-as.data.frame(out0$var_coord[,c(1,3)])
          colnames(e)<-c("x","y")
          e$label <- rownames(e)%>%stringr::str_wrap(20)
          library(ggplot2)
          library(ggrepel)
          add.alpha <- function(col, alpha=1){
            if(missing(col))
              stop("Please provide a vector of colours.")
            apply(sapply(col, col2rgb)/255, 2,
                  function(x)
                    rgb(x[1], x[2], x[3], alpha=alpha))
          }
          p<-ggplot()   + geom_segment(data=e,aes(x=0,xend=x,y=0,yend=y),size=1,arrow=arrow(length = unit(0.01, "npc")))+
            xlab(paste0("Axe 1 (",round(100*info_axis[1],1),"%)"))+
            ylab(paste0("Axe 3 (",round(100*info_axis[3],1),"%)"))+
            geom_label_repel(aes(x=x,y=y,label=label)
                             ,force=0.2
                             ,size=4
                             ,label.padding =  unit(0.15, "lines")
                             ,fill=add.alpha("lightblue",0.75)
                             ,data=e
                             , segment.color ="grey80"
            )
          # p<-p+xlim(-1,1)+ylim(-1,1)+ ggforce::geom_circle(data=data.frame(x0=0,y0=0,r=1),mapping=aes(x0=x0,y0=y0,r=r))
          p<-p+theme_classic(base_size = 16)

          mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
          mydoc = addPlot( doc = mydoc, fun = print, x =p)
          mydoc <- addParagraph( mydoc,  paste0("Mapping des variables (Axes 1 et 3)",sfx))



          e<-as.data.frame(out0$ind_coord[,c(1,3)])
          colnames(e)<-c("x","y")

          library(ggplot2)
          library(ggrepel)
          p<-ggplot(data=e)   + geom_point(aes(x=x,y=y),color=rgb(0,0,0,0.25))+
            xlab(paste0("Axe 1 (",round(100*info_axis[1],1),"%)"))+
            ylab(paste0("Axe 3 (",round(100*info_axis[3],1),"%)"))

          p<-p+theme_classic(base_size = 16)

          mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
          mydoc = addPlot( doc = mydoc, fun = print, x =p)
          mydoc <- addParagraph( mydoc,  paste0("Mapping des individus (Axes 1 et 3)",sfx))


          #2 3


          e<-as.data.frame(out0$var_coord[,c(2,3)])
          colnames(e)<-c("x","y")
          e$label <- rownames(e)%>%stringr::str_wrap(20)
          library(ggplot2)
          library(ggrepel)
          add.alpha <- function(col, alpha=1){
            if(missing(col))
              stop("Please provide a vector of colours.")
            apply(sapply(col, col2rgb)/255, 2,
                  function(x)
                    rgb(x[1], x[2], x[3], alpha=alpha))
          }
          p<-ggplot()   + geom_segment(data=e,aes(x=0,xend=x,y=0,yend=y),size=1,arrow=arrow(length = unit(0.01, "npc")))+
            xlab(paste0("Axe 2 (",round(100*info_axis[2],1),"%)"))+
            ylab(paste0("Axe 3 (",round(100*info_axis[3],1),"%)"))+
            geom_label_repel(aes(x=x,y=y,label=label)
                             ,force=0.2
                             ,size=4
                             ,label.padding =  unit(0.15, "lines")
                             ,fill=add.alpha("lightblue",0.75)
                             ,data=e
                             , segment.color ="grey80"
            )
          # p<-p+xlim(-1,1)+ylim(-1,1)+ ggforce::geom_circle(data=data.frame(x0=0,y0=0,r=1),mapping=aes(x0=x0,y0=y0,r=r))
          p<-p+theme_classic(base_size = 16)

          mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
          mydoc = addPlot( doc = mydoc, fun = print, x =p)
          mydoc <- addParagraph( mydoc,  paste0("Mapping des variables (Axes 2 et 3)",sfx))



          e<-as.data.frame(out0$ind_coord[,c(2,3)])
          colnames(e)<-c("x","y")

          library(ggplot2)
          library(ggrepel)
          p<-ggplot(data=e)   + geom_point(aes(x=x,y=y),color=rgb(0,0,0,0.25))+
            xlab(paste0("Axe 2 (",round(100*info_axis[2],1),"%)"))+
            ylab(paste0("Axe 3 (",round(100*info_axis[3],1),"%)"))

          p<-p+theme_classic(base_size = 16)

          mydoc <- addSlide(mydoc,slide.layout = 'rendu1')
          mydoc = addPlot( doc = mydoc, fun = print, x =p)
          mydoc <- addParagraph( mydoc,  paste0("Mapping des individus (Axes 2 et 3)",sfx))


        }


      }



    }

  }
  writeDoc( mydoc,file)


}
