
library(compositions)
library(Hmisc, pos=4)
library(foreign, pos=4)
library(corrplot)
library(ggplot2)
library(readxl)

#1.Dados eleições 1er turno no Brasil

#https://resultados.tse.jus.br/oficial/app/index.html#/eleicao;e=e544;uf=ce;ufbu=ce/resultados
#getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
ElectionsdataBrazil <- read_excel("votes2022Brazil.xlsx")
#View(ElectionsdataBrazil)
ElectionsdataBrazil
UF=ElectionsdataBrazil[,1]
State=ElectionsdataBrazil[,2]
#sum(ElectionsdataBrazil[,6])/sum(ElectionsdataBrazil[,10])
#View(ElectionsdataBrazil)
ElectionsdataBrazil <-ElectionsdataBrazil[,c(-1,-2)]
dim(ElectionsdataBrazil)
Others=as.matrix(ElectionsdataBrazil[,8]) - 
 (as.matrix(ElectionsdataBrazil[,1]) +as.matrix(ElectionsdataBrazil[,2]) +
  as.matrix(ElectionsdataBrazil[,3]) +as.matrix(ElectionsdataBrazil[,4]) )

databrazil<- cbind(as.matrix(ElectionsdataBrazil[,c(1,2,3,4)]),Others,ElectionsdataBrazil[,8])
colnames(databrazil)= c("PT","PL","MDB","PDT","OTHERS","TOTAL")
databrazil
#matriz_prop_br =matrix(numeric(27),nrow = 27,ncol = 7)
#matriz_prop_br 
col_prop1<-(databrazil[,1]/databrazil[,6])*100
col_prop2<-(databrazil[,2]/databrazil[,6])*100
col_prop3<-(databrazil[,3]/databrazil[,6])*100
col_prop4<-(databrazil[,4]/databrazil[,6])*100
col_prop5<-(databrazil[,5]/databrazil[,6])*100

dados1<-cbind(UF,State,databrazil)

#2. Transformações
### Transformação em proporções
matriz_prop_br<-cbind(col_prop1,col_prop2,col_prop3,col_prop4,col_prop5)
matriz_prop_br
colnames(matriz_prop_br)= c("PT","PL","MDB","PDT","OTHERS")
matriz_prop_br

matriz_tr_br=clr(matriz_prop_br)
matriz_tr_br

rownames(matriz_tr_br)<-c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG",
                          "PA","PB","PR","PE","PI",
                           "RJ","RN","RS","RO","RR","SC","SP","SE","TO")

### Transformação clr
TransformedCPA_br<- princomp(matriz_tr_br,cor =TRUE)
summary(TransformedCPA_br)
cargas=TransformedCPA_br$loadings
TransformedCPA_br$scores

#library(gridExtra)

#table_data <- cbind(UF,State)
#View(table_data)
par(mfrow = c(1,2))
biplot(TransformedCPA_br,xlab='First Component',
       ylab='Second Component',main ="Biplot of the Brazilian elections 2022",cex = 0.5)

# 4. Normalização dos escores

#####Obtaining the scores 
lam =((TransformedCPA_br$sdev))*sqrt(TransformedCPA_br$n.obs)
lam
#scale != 0 >> lam^scale 
lam = lam
score1_br = TransformedCPA_br$scores[,1]/ lam[1]
score2_br =TransformedCPA_br$scores[,2] / lam[2]
score1_br 
score2_br
#####standardizing scores 
score1_t1 <- (score1_br - min(score1_br))/(max(score1_br) - min(score1_br))
score1_t1
score1_t1[23]= score1_t1[23]+0.0001
score1_t1[6] =score1_t1[6] -0.0001
score1_t1
hist(score1_t1, main = "Histograma de z1",ylim = c(0, 8), xlim = c(0, 1.0),breaks = 10,xlab = "", ylab = "")

par(mfrow=c(1,1))
par(asp = 0.5)
plot(density(score1_t1),main="Densidade de z1", xlab="", ylab="",col="lightgray", lwd=2)
polygon(density(score1_t1),main="Densidade de z1", xlab="", ylab="",col="lightgray", lwd=2)


score2_t1 <- (score2_br - min(score2_br))/(max(score2_br) - min(score2_br))
score2_t1
score2_t1[6]= score2_t1[6]+0.0001
score2_t1[17] =score2_t1[17] -0.0001
score2_t1#hist(score2_t1, main = "Histograma de z2",ylim = c(0, 6),xlim = c(0, 1.0),breaks = 10)


#5. Dados para implementar o modelo

#http://www.atlasbrasil.org.br/ranking

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
indicators <- as.data.frame(read_excel("IDHbrasil.xlsx"))
UF = indicators[,1]
str(indicators)
summary(indicators)
indicators<- indicators[,c(3,4,5)]
indicators<-cbind(as.numeric(indicators[,1]),as.numeric(indicators[,2]),as.numeric(indicators[,3]))

colnames(indicators)= c("renda","educação","longevidade")
renda = cbind(indicators[,1])
educação = cbind(indicators[,2])
longevidade = cbind(indicators[,3])


Data_indicators =as.data.frame(cbind(indicators,score1_t1,score2_t1 ))
summary(Data_indicators) 

# 6. GAMLSS
library(gamlss)
###Escore1
#####################################################################
score1_m1<- gamlss(score1_t1 ~ renda+educação+longevidade, family= BE,data=Data_indicators )
summary(score1_m1)

score1_m2<- gamlss(score1_t1~ renda+educação,family=BE,data=Data_indicators )
summary(score1_m2)

# 7. Diagnóstico
par(mfrow=c(2,2))
plot(fitted(score1_m2),residuals(score1_m2),xlab="Fitted values",ylab="Residuals",main="a. Fitted values vs Residuals",ylim = c(-3,3))#### graph 1
plot(1:nrow(Data_indicators),resid(score1_m2),xlab="Index",ylab="Residuals",main="b. Index vs Residuals",ylim = c(-3,3))#### graph 2
plot(density(resid(score1_m2)),main="c. Density Estimate")#### graph 3
qqPlot(resid(score1_m2), ylab = "Normalized quantile residuals", main ="d. Normal Q-Q Plot", ylim =c(-3,3),id=list(method="y", n=25, cex=0.5), )#### graph 4





