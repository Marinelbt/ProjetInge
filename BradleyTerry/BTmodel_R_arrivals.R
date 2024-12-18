library(openxlsx)
library(BradleyTerry2)
## read covariates
datiBT <- read.xlsx("~/Desktop/ACO M2 /ProjetInge/BradleyTerry/covariates.xlsx", sheet = "DATA")
rownames(datiBT)<-datiBT[,1]
datiBT<-datiBT[,-1]
attach(datiBT)

################################## BT #####################?
### matrice dei dati
mat_dati<-cbind(V62)
##################################################
## regions' labels
origine<-rownames(datiBT)
or<-(origine)

## read tourist flow matrix between regions (diagonal=0)
a<-read.table("~/Desktop/ACO M2 /ProjetInge/BradleyTerry/Arrivi_2012_BIS.txt", sep="\t", dec=".", na.strings="", header=FALSE)
  a<-a[1:20,]
    rownames(a)<-or
    colnames(a)<-or
  ta<-t(a)

 prova<-countsToBinomial(ta) 
 names(prova)[1:2] <- c("destination1", "destination2")

#######################classic BT model (without covariates)
prova<-countsToBinomial(ta) 
names(prova)[1:2] <- c("destination1", "destination2")
citeModel <- BTm(cbind(win1, win2), destination1, destination2, ~ destination, id = "destination", data = prova)


### BT with covariate: population (V62)
cova<-as.data.frame(V62)
regions<-list(sedi=prova, covariate=cova)
citeModelP<-BTm(cbind(win1, win2), player1=destination1, player2=destination2, 
formula=~ V62[..]+(1|..),data=regions)




