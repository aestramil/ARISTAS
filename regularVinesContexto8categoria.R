rm(list = ls())

library(tidyr)
library(dplyr)
datoscxto  <- read.csv2("~/Dropbox/2022_Agustin/R/ineed/Datos_Estudiantes_CXTO.csv")
datoshse <- read.csv2("~/Dropbox/2022_Agustin/R/ineed/Datos_Estudiantes_HSE.csv")

datosto<-left_join(datoscxto,datoshse)

datosto = datosto[datosto$IdAlumnoDes != 0,]

datosto = datosto[datosto$IND_Estudiantes_EST_9 != 0,]


vardiv1<-c("EC44_1","EC44_2", "EC44_3","EC44_4","EC44_5", "EC44_6")
vardiv2<-c("EC45_1","EC45_2","EC45_3","EC45_4","EC45_5","EC45_6","EC45_7")
matlen = c('VALTMAT_ESC50', 'VALTIDE_ESC50')
vardiv<-c('ESCS_IMP','theta_LEN_E300','categoria', 'EMPATIA_ESC50', 'AlumnoGenero15' ,matlen, vardiv1, vardiv2)
datos1 = datosto[,names(datosto) %in% vardiv]


datos1 = datos1[complete.cases(datos1),]

datos1 = datos1 %>% drop_na()

table(datos1$AlumnoGenero15)
table(datos1$categoria)
#table(datos1$EF6)
#table(datos1$EF11)


datos1<-subset(datos1, categoria %in% c(1,2) )
#datos1<-subset(datos1, EF11<=7 )
#datos1<-subset(datos1, EF6<=3 )
datos1$AlumnoGenero15<-ifelse(datos1$AlumnoGenero15== "M",1,0)


#########################################################################
#########################################################################
library(ggplot2)
library(foreign)
library(reshape2)
library(tidyverse)
library(readxl)
library(corrplot)
library(rvinecopulib)
library(ggraph)


acumulada<-function(x,u){sum(u<=x) / length(u)}
acumuladav<-function(u){ sapply(u,acumulada,u)} 

acumuladai<-function(x,u){sum(u<x) / length(u)}
acumuladavi<-function(u){ sapply(u,acumuladai,u)} 

# 
# continuas<-c("ESCS_IMP", "theta_MAT_E300",        "theta_LEN_E300", "ACTIGUALGEN_ESC50",     "ESTSENTPER_ESC50" ,     "EVINCESTADS_ESC50"  , 
#              "PRACTCOTIDIANAS_ESC50", "VINCENTREEST_ESC50" ,   "VINESTPROF_ESC50" ,     "VOZESTUD_ESC50"  ) 
# 
# continuas<-c("ESCS_IMP", "theta_MAT_E300",        "theta_LEN_E300" ) 
# vardiv<-c('ESCS_IMP','theta_LEN_E300', "AlumnoGenero15", "EDAD" ,'EMPATIA_ESC50','categoria', "EC43_1", "EC43_3", "EC44_2", "EC44_1", "EC45_1", 'EC45_3', 'EC45_6')

cont<-c("ESCS_IMP", "theta_LEN_E300", 'EMPATIA_ESC50', "ACTIGUALGEN_ESC50", 'EDAD', 'EF6', 'EF11','VALTMAT_ESC50', 'VALTIDE_ESC50')

datosc<- datos1[, names(datos1) %in% cont  ]
datosd<- datos1[, names(datos1) %in%  setdiff( names(datos1) ,cont) ]
datos2 <- cbind(datosc, datosd)

datoscE<-apply(datosc,2,acumuladav)
datosdE<-apply(datosd,2,acumuladav)
datosdE2<-apply(datosd,2,acumuladavi)

# fit <- vinecop(datoscE,  var_types = rep("c", ncol(datosc)))
# 
# contour(fit)
# summary(fit)
# fit
# plot(fit, tree=1,var_names = "use", edge_labels="tau")


fitd <- vinecop(cbind(datoscE,datosdE,datosdE2),  var_types = c(rep("c", ncol(datosc)) ,rep("d", ncol(datosd))))

contour(fitd)
summary(fitd)
fitd
plot(fitd, tree=1,var_names = "use", edge_labels="tau")

#setwd("~/Dropbox/2022_Agustin/R")
save.image("arboldiscretas8categoria.Rdata")
load("arboldiscretas8categoria.Rdata")

mat<-fitd$structure
pcs<-fitd$pair_copulas
vc <- vinecop_dist(pcs, mat)
simulados<-rvinecop(100000, vc)

dim(simulados)
bb4<-datos2
bb4$EDAD<-as.numeric(bb4$EDAD)
bb4$AlumnoGenero15<-as.numeric(bb4$AlumnoGenero15)

dfdf<-list()
mm<-1:ncol(simulados)
for( ii in 1:length(mm) ){ 
  
jj<-mm[ii]


dfdf[[ii]]<-quantile( bb4[,jj], probs=simulados[,jj] )

}

dfdf2<-data.frame(do.call(cbind,dfdf))
names(dfdf2)<-names(bb4)[mm]

library(gtools)
dfdf2$Cuantiles<-as.numeric(quantcut(dfdf2$ESCS_IMP, q = 5, na.rm =F))
dfdf2$Cuantiles<-as.factor(dfdf2$Cuantiles)
ggplot(data=dfdf2, aes(x=theta_LEN_E300, y = ACTIGUALGEN_ESC50, col=Cuantiles))+ stat_smooth(method='lm', se = FALSE)+theme_bw()


dfprueba = datoscxto
dfprueba$Cuantiles<-as.numeric(quantcut(dfprueba$ESCS_IMP, q = 5, na.rm =F))
dfprueba$Cuantiles<-as.factor(dfprueba$Cuantiles)
ggplot(data=dfprueba, aes(x=theta_LEN_E300, y = ACTIGUALGEN_ESC50, col=Cuantiles))+ stat_smooth(method='lm', se = FALSE)+theme_bw()

#dfdf2 = as.data.frame(apply(apply(dfdf2,2,as.numeric),2,round,0))


dfdf2$EDAD<-as.factor(round(as.numeric(dfdf2$EDAD,0)))
dfdf2$AlumnoGenero15<-as.factor(round(as.numeric(dfdf2$AlumnoGenero15,0)))
dfdf2$categoria<-as.factor(round(as.numeric(dfdf2$categoria,0)))

table(dfdf2$EDAD)
table(dfdf2$AlumnoGenero15)
table(dfdf2$categoria)

dfdf3<-melt(dfdf2, id=c("ESCS_IMP", "theta_LEN_E300", "Cuantiles","ACTIGUALGEN_ESC50", "EMPATIA_ESC50", "AlumnoGenero15", "EDAD", 'categoria'))

dfdf4<-subset(dfdf3,  variable != "EC45_7")

dfdf4$value<-as.factor( round(dfdf4$value,0))
ggplot(dfdf4, aes(x = Cuantiles, y=theta_LEN_E300, fill =value ), alpha=0.1) + scale_fill_brewer(palette="RdBu") + geom_boxplot() +facet_wrap(~variable)+theme_bw()
ggplot(dfdf4, aes(x = Cuantiles, y=theta_LEN_E300, fill =value ), alpha=0.1) + scale_fill_brewer(palette="RdBu") + geom_boxplot() +facet_wrap(~variable+EDAD)+theme_bw()


ggplot(dfdf4, aes(x = AlumnoGenero15, y=ACTIGUALGEN_ESC50, fill =value ), alpha=0.1) + scale_fill_brewer(palette="RdBu") + geom_boxplot() +facet_wrap(~variable)+theme_bw()
ggplot(dfdf4, aes(x = Cuantiles, y=EMPATIA_ESC50, fill =value ), alpha=0.1) + scale_fill_brewer(palette="RdBu") + geom_boxplot() +facet_wrap(~variable)+theme_bw()

ggplot(dfdf4, aes(x =AlumnoGenero15 , y=ACTIGUALGEN_ESC50, fill =EDAD), alpha=0.1) + scale_fill_brewer(palette="RdBu") + geom_boxplot() +facet_wrap(~Cuantiles)+theme_bw()





