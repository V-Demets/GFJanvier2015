load("Tables/gfDonneesBrutes3.Rdata")
library(reshape)
#-------Mise à jour Table AccD
# AccD$Cycle <- 1

#-------Calcul des valeurs d'accroissement sur le diamètre
a <- ValArbres[,1:3]
Tab <- merge(IdArbres, a, all=T)
# Récupère la strate de chaque placette
Tab$Strate <- Placettes$Strate[match(paste0(Tab$NumForet,Tab$NumPlac),
                                       paste0(Placettes$NumForet,Placettes$NumPlac))]
# Diam unique + Classe
Tab$Diam <- (Tab[,"Diam1"]+Tab[,"Diam2"])/2
Tab <- Tab[,-c(9:10)]
# Tab$Classe <- floor(Tab$Diam/5+0.5)*5

#Préparation de Tab2 :
Tab2 <- Tab
Tab2$Classe <- floor(Tab2$Diam/5+0.5)*5
Tab2 <- summaryBy(Azimut ~ NumForet+Strate+Essence+Classe+Cycle, data=Tab2, FUN=sum)
Tab2[,"Azimut.sum"] <- NULL

#Retour Tab - Mise en forme pour lancement fonction Evolution
ListeCycles <- unique(Tab$Cycle)
names(Tab)[c(5,10)] <- c("variable","value")
Tab$IdArbre <- NULL
Tab <- as.data.frame(cast(Tab, NumForet+NumPlac+Strate+NumArbre+Essence+Azimut ~ variable))
names(Tab)[7:dim(Tab)[2]] <- paste0("Cycle", names(Tab)[7:dim(Tab)[2]])


# e <- Evolution(Tab,ListeCycles,variable="Diam")
# Table <- Tab
# Table <- test
# variable="Diam"

# Calcul de l'accroissement en diamètre : utilisation fonction Evolution
Evolution <- function(Table, ListeCycles, variable){ # Nouvelle fonction evolution : temps / 60!
    fin <- dim(Table)[2]
  for (i in min(ListeCycles):(max(ListeCycles)-1)){
    Temps <- Table
    Table[,fin+i] <- NA #Pblme : si que 2eme cycle enregistre? saute une colonne vide... Faut remplir cycle 1 quoi qu'il arrive
    Cycles$Id <- paste(Cycles$NumForet,Cycles$Cycle,sep="")
    Temps$Id3 <- paste(Table$NumForet,i,sep="")
    Temps$Id4 <- paste(Table$NumForet,i+1,sep="")
    Temps$Année3 <- Cycles$Année[match(Temps$Id3,Cycles$Id)]
    Temps$Année4 <- Cycles$Année[match(Temps$Id4,Cycles$Id)]
        #Temps$Année4 <- 2023
    Table[,fin+i] <- with(Temps,Année4-Année3)

    pos <- which(Table[,(fin-(max(ListeCycles)-i))] > 0 & Table[,(fin-(max(ListeCycles)-(i+1)))] > 0 &
                 Table[,(fin-(max(ListeCycles)-(i+1)))] >= Table[,(fin-(max(ListeCycles)-i))])
      Table[pos,fin+i]=(Table[pos,(fin-(max(ListeCycles)-(i+1)))]
                     -Table[pos,(fin-(max(ListeCycles)-i))])/Table[pos,(fin+i)]
      Table[-pos,fin+i] <- NA
    names(Table)[fin+i] <- paste0("AccD", i+1) #insere le nom de la nouvelle colonne
  }
  for (i in min(ListeCycles):(max(ListeCycles)-1)){
    Table[,fin+i+(length(ListeCycles)-1)]=(Table[,(fin-(max(ListeCycles)-(i+1)))]+
                                                Table[,(fin-(max(ListeCycles)-i))])/2
    Table[,fin+i+(length(ListeCycles)-1)]=floor(Table[,fin+i+(length(ListeCycles)-1)]/5+0.5)*5
    #TB2.evDiam[which(TB2.evDiam[,finD+i]<0),finD+i]=NA
    #names(Table)[fin+i] <- paste(variable, i, "-", i+1,sep="")
    names(Table)[fin+i+(length(ListeCycles)-1)] <- paste0("ClasseAcc", i+1)
  }
  return(Table)
}
d <- Evolution(Tab,ListeCycles,variable="Diam")
Tab <- d
Tab <- na.omit(Tab)
Tab <- Tab[,-c(7:(7+length(ListeCycles)-1))]
Tab3 <- Tab[,1:(dim(Tab)[2]-(length(ListeCycles)-1))] # Table avec les valeurs d'AccD pour les différents cycles (autres que 1er cycle)
names(Tab3)[7:(dim(Tab)[2]-(length(ListeCycles)-1))] <- substr(names(Tab3)[7:(dim(Tab)[2]-(length(ListeCycles)-1))],
                                                               5,5)
Tab4 <- Tab[,c(1:6,(7+(length(ListeCycles)-1)):dim(Tab)[2])] # Table avec les classes d'AccD pour les différents cycles
names(Tab4)[7:(dim(Tab)[2]-(length(ListeCycles)-1))] <- substr(names(Tab4)[7:(dim(Tab)[2]-(length(ListeCycles)-1))],
                                                               10,10)
Tab3 <- melt(Tab3, id=c("NumForet","NumPlac","Strate","NumArbre","Essence","Azimut"))
names(Tab3)[7:8] <- c("Cycle","AccD")
Tab4 <- melt(Tab4, id=c("NumForet","NumPlac","Strate","NumArbre","Essence","Azimut"))
names(Tab4)[7:8] <- c("Cycle","ClasseAcc")

Tab5 <- merge(Tab3,Tab4, all=T)
Tab6 <- Tab5[c(1,3,5,7:9)]
names(Tab6)[6] <- "Classe"
Tab62 <- merge(Tab2,AccD, all=T) #Table avec valeurs cycle 1+ toutes les valeurs possibles pour les cycles suivants
Tab7 <- merge(Tab62,Tab6, all=T)
Tab8 <- summaryBy(AccD ~ NumForet+Strate+Essence+Classe+Cycle,data=Tab7,FUN=mean,na.rm=T,keep.names=T)
# test <- as.vector(paste0(Tab8$NumForet,Tab8$Strate,Tab8$Cycle,Tab8$Essence))
# Tab9$AccD[pos] <-
#   a <- as.data.frame(with(Tab9,tapply(AccD,paste0(NumForet,Strate,Cycle,Essence),mean,na.rm=T)),row.names=T)
# a <- with(Tab9,tapply(AccD,paste0(NumForet,Strate,Cycle,Essence),mean,na.rm=T))
# by(Tab8$AccD,as.factor(Tab8$Essence,Tab8$Cycle),mean,na.rm=T)
pos8 <- which(is.na(Tab8$AccD))
if (length(pos8) >0) {
  Tab9 <- summaryBy(AccD ~ NumForet+Cycle+Essence+Strate, data=Tab8, FUN=mean,
                    keep.names=T,na.rm=T)
  pos9 <- which(is.na(Tab9$AccD))
  if (length(pos9)>0) {
    Tab10 <- summaryBy(AccD ~ NumForet+Cycle+Strate, data=Tab9, FUN=mean,
                       keep.names=T, na.rm=T) # Il vaut mieux se priver du critère ou cycle ?
    Tab9$Id <- with(Tab9,paste0(NumForet,Cycle,Strate))
    Tab10$Id <- with(Tab10,paste0(NumForet,Cycle,Strate))
    Tab9$AccD[pos9] <- Tab10$AccD[match(Tab9$Id[pos9],Tab10$Id)]
  }
    Tab8$Id <- with(Tab8,paste0(NumForet,Cycle,Essence,Strate))
    Tab9$Id <- with(Tab9,paste0(NumForet,Cycle,Essence,Strate))
    Tab8$AccD[pos8] <- Tab9$AccD[match(Tab8$Id[pos8],Tab9$Id)]
}
Tab8$Id <- NULL
AccD <- Tab8
AccD <- AccD[order(AccD$NumForet,AccD$Cycle),]
save(Forets,Echantillonnages,Cycles,Placettes,Coords,PCQM,Cercles,Reges,
     BMSLineaires,BMSCercles,Tarifs,Tiers,Hauts,AccD,Essences,EssReg,EssInd,
     CodeEcolos,Quals,Prix,IdArbres,ValArbres,Reperes,
     file = "Tables/gfDonneesBrutes3.Rdata")
#-----Fin
#
#
#   summaryBy(AccD ~ NumForet+Cycle, data=Tab8, FUN=mean, na.rm=T)
# pos2 <- which(is.na(Tab9$AccD))
# if (pos2>2) {Tab9$AccD[pos]=}
# # Construction tab
# Tab3 <- merge(Tab2,Tab, by.x="Classe", by.y=)
# Tab3 <- Tab[,c(1,9)]
# ac <- melt(Tab3, id="NumForet")
# #
# # melt(a, id=c("IdArbre","NumForet","NumPlac","Essence","Azimut","Dist")) #une colonne AccD et une colonne Cycle
# # summaryBy(AccD~+NumForet+NumPlac+Essence+Azimut+Dist) #je fais la moyenne par classe et par essence,etc...
