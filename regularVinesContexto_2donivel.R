rm(list = ls())

library(ggplot2)
library(foreign)
library(reshape2)
library(tidyverse)
library(readxl)
library(corrplot)
library(rvinecopulib)
library(ggraph)
library(dplyr)
library(igraph)

datoscxto<- read.csv2("https://www.ineed.edu.uy/images/BasesDeDatos/Aristas2018/Estudiantes/Contexto/Datos_Estudiantes_CXTO.csv")
datoshse <- read.csv2("https://www.ineed.edu.uy/images/BasesDeDatos/Aristas2018/Estudiantes/Socioemocional/Datos_Estudiantes_HSE.csv")

datosto<-left_join(datoscxto,datoshse)

datosto = datosto[datosto$IdAlumnoDes != 0,]

datosto = datosto[datosto$IND_Estudiantes_EST_9 != 0,]

genero<-c("EC44_1","EC44_2", "EC44_3","EC44_4","EC44_5", "EC44_6")
diversidad<-c("EC45_1","EC45_2","EC45_3","EC45_4","EC45_5","EC45_6","EC45_7")
empatia<-c('ES9_1', 'ES9_2', 'ES9_3', 'ES9_4')

#vardiv <- c('ESCS_IMP','theta_LEN_E300','EMPATIA_ESC50', genero, diversidad)
vardiv <- c('AlumnoGenero15',  'ESCS_IMP','theta_LEN_E300', empatia, genero, diversidad)

datos1 = datosto[,names(datosto) %in% vardiv]
datos1 = datos1[complete.cases(datos1),]
datos1 = datos1 %>% na.omit()

datos1$AlumnoGenero15[datos1$AlumnoGenero15 =="M"]=0
datos1$AlumnoGenero15[datos1$AlumnoGenero15 =="F"]=1

datos_gen = datos1[,names(datos1) %in% genero]
datos_div = datos1[,names(datos1) %in% diversidad]
datos_emp = datos1[,names(datos1) %in% empatia]


acumulada<-function(x,u){sum(u<=x) / length(u)}
acumuladav<-function(u){ sapply(u,acumulada,u)} 

acumuladai<-function(x,u){sum(u<x) / length(u)}
acumuladavi<-function(u){ sapply(u,acumuladai,u)} 



datos_genE <- apply(datos_gen,  2, acumuladav)
datos_genE2 <- apply(datos_gen, 2, acumuladavi)

datos_divE <- apply(datos_div,   2, acumuladav)
datos_divE2 <- apply(datos_div, 2, acumuladavi)

datos_empE <- apply(datos_emp,   2, acumuladav)
datos_empE2 <- apply(datos_emp, 2, acumuladavi)


fitd_gen <- vinecop(cbind(datos_genE, datos_genE2), family_set = "archimedean",tree_crit="tau",  var_types = rep("d", ncol(datos_gen)))
fitd_div <- vinecop(cbind(datos_divE, datos_divE2), family_set = "archimedean",tree_crit="tau",  var_types = rep("d", ncol(datos_div)))
fitd_emp <- vinecop(cbind(datos_empE, datos_empE2), family_set = "archimedean",tree_crit="tau",  var_types = rep("d", ncol(datos_emp)))



contour(fitd_gen)
contour(fitd_div)
contour(fitd_emp)

summary(fitd_gen)
summary(fitd_div)
summary(fitd_emp)


fitd_gen
fitd_div
fitd_emp

( plgen = plot(fitd_gen, tree=1,var_names = "use", edge_labels="tau") )
( pldiv = plot(fitd_div, tree=1,var_names = "use", edge_labels="tau") )
( plemp = plot(fitd_emp, tree=1,var_names = "use", edge_labels="tau") )
####Se puede hacer copy-paste y borrar desde aqui
igr_obj1 <- get("g", plgen$plot_env)[[1]]
igr_obj1 # print object
igraph::E(igr_obj1)$name #
dd1<-fitd_gen$structure
names(fitd_gen)

igr_obj2 <- get("g", pldiv$plot_env)[[1]]
igr_obj2
igraph::E(igr_obj2)$name #
dd2<-fitd_div$structure
names(fitd_div)

igr_obj3 <- get("g", plemp$plot_env)[[1]]
igr_obj3
igraph::E(igr_obj3)$name
dd3<-fitd_emp$structure
names(fitd_emp)

( nombres1<-V(igr_obj1)$name )
( nombres2<-V(igr_obj2)$name )
( nombres3<-V(igr_obj3)$name )

varsigdiv = c(genero, diversidad)
conserva = c('EC44_1', 'EC44_3', 'EC44_4', 'EC44_5','EC45_6')
empatia = c("ES9_1","ES9_2","ES9_3","ES9_4","ES9_5")
noconserva = varsigdiv[!varsigdiv %in% conserva]

cols1<-rep('gray',length(nombres1))
cols2<-rep('gray',length(nombres2))
cols3<-rep('gray',length(nombres3))

cols1[nombres1 %in% conserva] <- 'deepskyblue'
cols1[nombres1 %in% noconserva] <- 'magenta'
cols1[nombres1 %in% empatia] <- 'green'
cols2[nombres2 %in% conserva] <- 'deepskyblue'
cols2[nombres2 %in% noconserva] <- 'magenta'
cols2[nombres2 %in% empatia] <- 'green'
cols3[nombres3 %in% conserva] <- 'deepskyblue'
cols3[nombres3 %in% noconserva] <- 'magenta'
cols3[nombres3 %in% empatia] <- 'green'

#genero
par(mar=c(0,0,0,0)+.1)
plot(igr_obj1,  
     vertex.label = nombres1,
     vertex.size = 25,
     vertex.color= cols1,
     vertex.frame.color= "white",
     vertex.label.color = "black",
     vertex.label.family = "sans",
     edge.width=igraph:: E(igr_obj1)$weight, 
     edge.length=500,
     edge.color="black",
     edge.label=igraph::E(igr_obj1)$name,
     vertex.label.cex=1.4, vertex.label.dist=0,
     edge.curved=0,
     edge.label.cex=2,
     layout = layout_as_tree
)

#diversidad
par(mar=c(0,0,0,0)+.1)
plot(igr_obj2,  
     vertex.label = nombres2,
     vertex.size = 25,
     vertex.color= cols2,
     vertex.frame.color= "white",
     vertex.label.color = "black",
     vertex.label.family = "sans",
     edge.width=igraph:: E(igr_obj2)$weight, 
     edge.length=500,
     edge.color="black",
     edge.label=igraph::E(igr_obj2)$name,
     vertex.label.cex=1.4, vertex.label.dist=0,
     edge.curved=0,
     edge.label.cex=2,
     layout = layout_as_tree
)

#empatia
par(mar=c(0,0,0,0)+.1)
plot(igr_obj3,  
     vertex.label = nombres3,
     vertex.size = 25,
     vertex.color= cols3,
     vertex.frame.color= "white",
     vertex.label.color = "black",
     vertex.label.family = "sans",
     edge.width=igraph:: E(igr_obj3)$weight, 
     edge.length=500,
     edge.color="black",
     edge.label=igraph::E(igr_obj3)$name,
     vertex.label.cex=1.4, vertex.label.dist=0,
     edge.curved=0,
     edge.label.cex=2,
     layout = layout_as_tree
)

#### hasta aca

C_genero = pvinecop(cbind(datos_genE, datos_genE2), fitd_gen)
C_diversidad = pvinecop(cbind(datos_divE, datos_divE2), fitd_div)
C_empatia = pvinecop(cbind(datos_empE, datos_empE2), fitd_emp)


#cont<-c("ESCS_IMP", "theta_LEN_E300", 'EMPATIA_ESC50')
cont<-c("EDAD", "ESCS_IMP", "theta_LEN_E300")
disc<-c('AlumnoGenero15')

#datosc <- cbind(C_genero, C_diversidad, datos1[, names(datos1) %in% cont  ])
datosd<- cbind(datos1[, names(datos1) %in% disc  ,drop=FALSE])
datosdE<-apply(datosd,2,acumuladav)
datosdE2<-apply(datosd,2,acumuladavi)

datosc <- cbind(C_genero, C_diversidad, C_empatia, datos1[, names(datos1) %in% cont ,drop=FALSE  ])
datoscE <- apply(datosc,2,acumuladav)

fitd2 <- vinecop(cbind(datoscE,datosdE,datosdE2), family_set = "archimedean",
                 tree_crit="tau", selcrit = "bic", 
                 var_types = c(rep("c", ncol(datosc)) ,rep("d", ncol(datosd)) ) )

plot(fitd_gen, tree=1,var_names = "use", edge_labels="tau")
plot(fitd_div, tree=1,var_names = "use", edge_labels="tau")
plot(fitd_emp, tree=1,var_names = "use", edge_labels="tau")

#tree 1
plot(fitd2, tree=1,var_names = "use", edge_labels="tau")
#tree 2
plot(fitd2, tree=2,var_names = "use", edge_labels="tau")

# setwd("~/Dropbox/2022_Agustin/R")
# save.image("arboldiscretas_2donivel.Rdata")


mat <- fitd2$structure
pcs <- fitd2$pair_copulas
vc <- vinecop_dist(pcs, mat)
simulados <- rvinecop(100000, vc)

dim(simulados)
bb4<-cbind(datosc,datosd)
bb4$AlumnoGenero15<-as.numeric(bb4$AlumnoGenero15)
dfdf<-list()
mm<-1:ncol(simulados)

for(ii in 1:length(mm)){
  jj<-mm[ii]
  dfdf[[ii]]<-quantile( bb4[,jj], probs=simulados[,jj] )
}

dfdf2<-data.frame(do.call(cbind,dfdf))
names(dfdf2)<-names(bb4)[mm]

library(gtools)
dfdf2$Cuantiles<-as.numeric(quantcut(dfdf2$ESCS_IMP, q = 5, na.rm =F))
dfdf2$Cuantiles<-as.factor(dfdf2$Cuantiles)

dfdf2$Cuantiles_gen<-as.numeric(quantcut(dfdf2$C_genero, q = 3, na.rm =F))
dfdf2$Cuantiles_gen<-as.factor(dfdf2$Cuantiles_gen)

dfdf2$Cuantiles_div<-as.numeric(quantcut(dfdf2$C_diversidad, q = 3, na.rm =F))
dfdf2$Cuantiles_div<-as.factor(dfdf2$Cuantiles_div)

dfdf2$Cuantiles_emp<-as.numeric(quantcut(dfdf2$C_empatia, q = 3, na.rm =F))
dfdf2$Cuantiles_emp<-as.factor(dfdf2$Cuantiles_emp)


ggplot(data=dfdf2, aes(x=C_diversidad, y = C_genero, col=Cuantiles))+ stat_smooth(method='gam', se = FALSE)+theme_bw()



ggplot(dfdf2, aes(x = Cuantiles, y=theta_LEN_E300, fill =Cuantiles ), alpha=0.1) + 
  scale_fill_brewer(palette="RdBu") +
  geom_boxplot() +facet_wrap(~Cuantiles_div)+theme_bw()

ggplot(dfdf2, aes(x = Cuantiles, y=theta_LEN_E300, fill =Cuantiles ), alpha=0.1) + 
  scale_fill_brewer(palette="RdBu") +
  geom_boxplot() +facet_wrap(~Cuantiles_gen)+theme_bw()


