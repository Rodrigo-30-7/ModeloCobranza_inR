library(data.table)
library(caTools)
library(bit64)
library(dplyr)
library(dummies)

setwd("C:/Users/FelipeHernandez/Desktop/Predictable Media/Saesa - Cobranza/Energía Modelación/regulado")

##################################################################################
#################### Llamado de BBDD y entrenamiento y prueba ####################
##################################################################################

cob=fread("ENERGIA_REGULADO.csv", sep=",", header = T)
names(cob)
colnames(cob)[10]<-"recupero"

############################################
###### HACER DUMMY LA VARIABLE ZONAL #######
############################################
data=subset(cob, select= c(1,2))

data$ZONAL = factor(data$ZONAL)
dummies_ZONALs = model.matrix(~data$ZONAL )
dummies_ZONALs=data.table(dummies_ZONALs)
colnames(dummies_ZONALs) <- gsub ("data$ZONAL", "", fixed  = TRUE, colnames (dummies_ZONALs)) 
dummies_ZONALs$`(Intercept)`<-NULL
dummies_ZONALs<-data.table(dummies_ZONALs)

### Pasarla a factor ###
cols <- names(dummies_ZONALs)
setDT(dummies_ZONALs)
for(j in cols){
  set(dummies_ZONALs, i=NULL, j=j, value=factor(dummies_ZONALs[[j]]))
}
##UNIRLAS
data=cbind(data,dummies_ZONALs)
data=data[, c(3:11)]
cob=cbind(cob,data)

cob$ZONAL<-NULL

colnames(cob)[10]<-"Z1"
colnames(cob)[11]<-"Z2"
colnames(cob)[12]<-"Z3"
colnames(cob)[13]<-"Z4"
colnames(cob)[14]<-"Z5"
colnames(cob)[15]<-"Z6"
colnames(cob)[16]<-"Z7"
colnames(cob)[17]<-"Z8"
colnames(cob)[18]<-"Z9"

names(cob)


############################################
###### HACER DUMMY LA VARIABLE ESTADO #######
############################################
data=subset(cob, select= c(1,2))

data$ESTADO = factor(data$ESTADO)
dummies_ESTADOs = model.matrix(~data$ESTADO )
dummies_ESTADOs=data.table(dummies_ESTADOs)
colnames(dummies_ESTADOs) <- gsub ("data$ESTADO", "", fixed  = TRUE, colnames (dummies_ESTADOs)) 
dummies_ESTADOs$`(Intercept)`<-NULL
dummies_ESTADOs<-data.table(dummies_ESTADOs)

### Pasarla a factor ###
cols <- names(dummies_ESTADOs)
setDT(dummies_ESTADOs)
for(j in cols){
  set(dummies_ESTADOs, i=NULL, j=j, value=factor(dummies_ESTADOs[[j]]))
}

##UNIRLAS
data=cbind(data,dummies_ESTADOs)
names(data)

data=data[, c(3:8)]
cob=cbind(cob,data)

cob$ESTADO<-NULL

colnames(cob)[18]<-"E1"
colnames(cob)[19]<-"E2"
colnames(cob)[20]<-"E3"
colnames(cob)[21]<-"E4"
colnames(cob)[22]<-"E5"
colnames(cob)[23]<-"E6"


names(cob)


####################################################
###### HACER DUMMY LA VARIABLE MACROSEGMENTO #######
####################################################
data=subset(cob, select= c(1,2))

data$MACROSEGMENTO = factor(data$MACROSEGMENTO)
dummies_MACROSEGMENTOs = model.matrix(~data$MACROSEGMENTO )
dummies_MACROSEGMENTOs=data.table(dummies_MACROSEGMENTOs)
colnames(dummies_MACROSEGMENTOs) <- gsub ("data$MACROSEGMENTO", "", fixed  = TRUE, colnames (dummies_MACROSEGMENTOs)) 
dummies_MACROSEGMENTOs$`(Intercept)`<-NULL
dummies_MACROSEGMENTOs<-data.table(dummies_MACROSEGMENTOs)

### Pasarla a factor ###
cols <- names(dummies_MACROSEGMENTOs)
setDT(dummies_MACROSEGMENTOs)
for(j in cols){
  set(dummies_MACROSEGMENTOs, i=NULL, j=j, value=factor(dummies_MACROSEGMENTOs[[j]]))
}

##UNIRLAS
data=cbind(data,dummies_MACROSEGMENTOs)
names(data)

data=data[, c(3:8)]
cob=cbind(cob,data)

names(cob)
cob$MACROSEGMENTO<-NULL

colnames(cob)[23]<-"M1"
colnames(cob)[24]<-"M2"
colnames(cob)[25]<-"M3"
colnames(cob)[26]<-"M4"
colnames(cob)[27]<-"M5"
colnames(cob)[28]<-"M6"


names(cob)

####################################################
###### HACER DUMMY LA VARIABLE RIESGO #######
####################################################
data=subset(cob, select= c(1,2))

data$RIESGO = factor(data$RIESGO)
dummies_RIESGOs = model.matrix(~data$RIESGO )
dummies_RIESGOs=data.table(dummies_RIESGOs)
colnames(dummies_RIESGOs) <- gsub ("data$RIESGO", "", fixed  = TRUE, colnames (dummies_RIESGOs)) 
dummies_RIESGOs$`(Intercept)`<-NULL
dummies_RIESGOs<-data.table(dummies_RIESGOs)

### Pasarla a factor ###
cols <- names(dummies_RIESGOs)
setDT(dummies_RIESGOs)
for(j in cols){
  set(dummies_RIESGOs, i=NULL, j=j, value=factor(dummies_RIESGOs[[j]]))
}

##UNIRLAS
data=cbind(data,dummies_RIESGOs)
names(data)

data=data[, c(3:8)]
cob=cbind(cob,data)

names(cob)
cob$RIESGO<-NULL

colnames(cob)[28]<-"R1"
colnames(cob)[29]<-"R2"
colnames(cob)[30]<-"R3"
colnames(cob)[31]<-"R4"
colnames(cob)[32]<-"R5"
colnames(cob)[33]<-"R6"

names(cob)


##################################################################################
########################### ENTRENAMIENTO Y PRUEBA ###############################
##################################################################################

cob$TARIFA<-as.factor(cob$TARIFA)
cob$RURALIDAD<-as.factor(cob$RURALIDAD)
cob$GENERO<-as.factor(cob$GENERO)
names(cob)
cob=cob[, c(6,1:5,7:33)]


base_completa_regulado<-cob

#base para entrenar el modelo sin id persona
cob$IDUSER<-NULL #mientras entreno el modelo no necesito el id despues cargo denuevo la dta
cob$GENERO<-NULL
cob$EDAD<-NULL
names(cob)



set.seed(69)
split = sample.split(cob$recupero, SplitRatio = 0.75) #divide la data 75% training
training_set = subset(cob, split == TRUE)
test_set = subset(cob, split == FALSE)
#write.table(training_set,"C:/Users/jibacachep/Desktop/Resumen fuga/Modelo de fuga completo/training_set.csv",sep=";", row.names=F)
################### Arbol de desición  
sapply(cob, function(x) sum(is.na(x))) #Valores faltantes

######### UNBALALANCED SAMPLE - Cuando la data esta desbalanceada ahi te envio un paper
library(ROSE)
library(caret)

#### ROSE proporcionan una mejor estimación de los datos originales- muestreo sintético
data.rose <- ROSE(recupero ~ ., data = training_set, seed = 1)$data
table(data.rose$recupero); prop.table(table(data.rose$recupero))
training_set=data.rose
rm(data.rose)


names(training_set)
##################################################################################
############################# REGRESIÓN LOGÍSTICA ################################
##################################################################################


glm_logit = glm(formula = recupero ~. ,
                family=binomial(link="logit"), data = training_set)
summary(glm_logit)
library(pscl)
pR2(glm_logit) #pseudo r cuadrado
library(car)
vif(glm_logit) #multicolinealidad
require(MASS)
#exp(cbind(coef(glm_logit), confint(glm_logit)))
options(width=80)
exp(coef(glm_logit)) # odds
#chisq.test(training_set$FORMA_PAGO_CO,training_set$FUGADO)
prob_pred = predict(glm_logit, type = 'response', newdata = test_set[,-1])
y_pred = ifelse(prob_pred > 0.5 , 1, 0)
library(e1071)
c_glm <- confusionMatrix(factor(y_pred),test_set$recupero, positive = "1")      #  matriz de confusion
cat(" Accuracy de test : ",c_glm[[3]][1],"\n") 
print(c_glm)  

library(InformationValue)
plotROC(test_set$fugado, prob_pred)
detach("package:InformationValue", unload=TRUE)

#if (p < ncol(X) && !(missing(newdata) || is.null(newdata))) 
#  warning("prediction from a rank-deficient fit may be misleading")
####### PREDECIR TODA LA DATA - # sin incluir la variable fuga ni el id de la persona
######################################################################################
predecir_base = predict(glm_logit, type = 'response', newdata = base_completa_regulado[,c(-1,-2)])

###asignar pbb_fuga a toda la base

base_completa_regulado=cbind(base_completa_regulado,predecir_base)

View(training_set)

dim(base_completa_regulado)
View(base_completa_regulado)
names(base_completa_regulado)
#ordenar base
base_completa_regulado=base_completa_regulado[, c(2,1,34,3:33)]
colnames(base_completa_regulado)[3]<-"pbb_nopago"

dim(base_completa_regulado)
View(base_completa_regulado)



write.table(base_completa_regulado, "Pbb_cobranza_regulado.csv", row.names = F, sep=";")
