#' Aggrégation des arbres à l'échelle de la placette.
#' @description Sous-programme permettant d'aggréger toutes les variables à l'échelle de la placette.
#' Il fournit de nombreux tableaux croisés.
#' @return La fonction construit les tables suivantes. Elles sont enregistrées dans le dossier Tables
#'
#' gfDen, gfPla, gfPlaDen, gfPlaCat, gfPlaCatDen, gfPlaCatEssReg, gfPlaCatEssRegDen,
#' gfPlaEssReg, gfPlaEssRegDen, gfPlaEssRegQual, gfPlaEssRegQualDen, gfPlaCatQual1,
#' gfPlaCatQual2, gfPlaCatQual2Den, gfPlaEss, gfPlaEssDen, gfPlaClasseQual1,
#' gfPlaClasseQual1Den, gfPlaClasseQual2, gfPlaClasseQual2Den, gfPlaClasseEssReg,
#' gfPlaEssRegQual1, gfPlaEssRegQual2, gfPerchesPla, gfPerchesPlaEss, gfPerchesPlaQual,
#' gfTaillisPla, gfTaillisPlaEss, gfTaillisPlaEssReg, gfRegePla, gfRegePlaEss, gfRegePlaEssReg
#'
#' @author Bruciamacchie Max
#' @import doBy
#' @export

gf_AgregArbres <- function() {
  ################### Creation des tables par placettes #####################
  tab <- subset(arbres, select = c("NumForet","NumPlac","NumArbre","Cycle","Strate","Essence","Type",
                                   "Reg1","Reg2","Classe","Cat","Poids","Gha","Vha","VhaIFN",
                                   "VcHa","VpHa","Gain","AcctV"))
  names(tab)[which(names(tab)=="Poids")] <-"Nha"
  den <- tab[which(tab$Cat!="PER"),]

  # --------- Tableaux par placettes -------
  # --- Totaux
  gfPla <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                       NumForet + NumPlac + Strate + Cycle,
                      data=tab, FUN= sum, na.rm=T, keep.names=T)
  gfPlaDen <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                          NumForet + NumPlac + Strate + Cycle,
                         data=den, FUN= sum, na.rm=T, keep.names=T)
  # --- Par essence
  gfPlaEss <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                          NumForet + NumPlac + Strate + Cycle + Essence,
                         data=tab, FUN= sum, na.rm=T, keep.names=T)
  gfPlaEssDen <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                             NumForet + NumPlac + Strate + Cycle + Essence,
                            data=den, FUN= sum, na.rm=T, keep.names=T)
  # --- Par essence regroupee
  tab <- merge(tab, EssReg, by=c("NumForet","Essence"), all.x=T)
  den <- merge(den, EssReg, by=c("NumForet","Essence"), all.x=T)
  gfPlaEssReg <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                             NumForet + NumPlac + Strate + Cycle + EssReg,
                            data=tab, FUN= sum, na.rm=T, keep.names=T)
  gfPlaEssRegDen <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                NumForet + NumPlac + Strate + Cycle + EssReg,
                               data=den, FUN= sum, na.rm=T, keep.names=T)
  # --- Par Cat
  gfPlaCat <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                          NumForet + NumPlac + Strate + Cycle + Cat,
                         data=tab, FUN= sum, na.rm=T, keep.names=T)
  gfPlaCatDen <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                             NumForet + NumPlac + Strate + Cycle + Cat,
                            data=den, FUN= sum, na.rm=T, keep.names=T)
  # --- Par Cat et EssReg
  gfPlaCatEssReg <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                 NumForet + NumPlac + Strate + Cycle + Cat + EssReg,
                               data=tab, FUN= sum, na.rm=T, keep.names=T)
  gfPlaCatEssRegDen <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                    NumForet + NumPlac + Strate + Cycle + Cat + EssReg,
                                  data=den, FUN= sum, na.rm=T, keep.names=T)
  # --- Par EssReg et Qual Reg1
  gfPlaEssRegQual <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                  NumForet + NumPlac + Strate + Cycle + EssReg + Reg1,
                                data=tab, FUN= sum, na.rm=T, keep.names=T)
  gfPlaEssRegQualDen <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                     NumForet + NumPlac + Strate + Cycle + EssReg + Reg1,
                                   data=den, FUN= sum, na.rm=T, keep.names=T)
  # --- Par Classe et Qual Reg1
  gfPlaClasseQual1 <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                   NumForet + NumPlac + Strate + Cycle + Classe + Reg1,
                                 data=tab, FUN= sum, na.rm=T, keep.names=T)
  gfPlaClasseQual1Den <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                      NumForet + NumPlac + Strate + Cycle + Classe + Reg1,
                                    data=den, FUN= sum, na.rm=T, keep.names=T)
  # --- Par Classe et Qual Reg2
  gfPlaClasseQual2 <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                   NumForet + NumPlac + Strate + Cycle + Classe + Reg2,
                                 data=tab, FUN= sum, na.rm=T, keep.names=T)
  gfPlaClasseQual2Den <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                      NumForet + NumPlac + Strate + Cycle + Classe + Reg2,
                                    data=den, FUN= sum, na.rm=T, keep.names=T)
  # --- Par Classe et EssReg
  gfPlaClasseEssReg <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                    NumForet + NumPlac + Cycle + Classe + EssReg,
                                  data=tab, FUN= sum, na.rm=T, keep.names=T)
  # --- Par Cat et Qual Reg1
  gfPlaCatQual1 <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                NumForet + NumPlac + Strate + Cycle + Cat + Reg1,
                              data=tab, FUN= sum, na.rm=T, keep.names=T)
  # --- Par Cat et Qual Reg2
  gfPlaCatQual2 <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                NumForet + NumPlac + Strate + Cycle + Cat + Reg2,
                              data=tab, FUN= sum, na.rm=T, keep.names=T)
  gfPlaCatQual2Den <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                   NumForet + NumPlac + Strate + Cycle + Cat + Reg2,
                                 data=den, FUN= sum, na.rm=T, keep.names=T)
  # --- Par EssReg et Qual Reg1
  gfPlaEssRegQual1 <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                   NumForet + NumPlac + Strate + Cycle + EssReg + Reg1,
                                 data=tab, FUN= sum, na.rm=T, keep.names=T)
  # --- Par EssReg et Qual Reg2
  gfPlaEssRegQual2 <- summaryBy(Nha + Gha + Vha + VhaIFN + VcHa + VpHa + Gain + AcctV ~
                                   NumForet + NumPlac + Strate + Cycle + EssReg + Reg2,
                                 data=tab, FUN= sum, na.rm=T, keep.names=T)

  # --------- Tableaux des perches par placettes -------
  Perches <- subset(arbres, Cat=="PER")
  names(Perches)[which(names(Perches)=="Poids")] <- "Nha"
  Perches <- merge(Perches, EssReg, by=c("NumForet","Essence"), all.x=T)
  gfPerchesPla <- summaryBy(Nha + Gha + Vha + VhaIFN ~ NumForet + Cycle + NumPlac,
                          data=Perches, FUN= sum, na.rm=T, keep.names=T)
  gfPerchesPlaEss <- summaryBy(Nha + Gha + Vha + VhaIFN ~ NumForet + Cycle + NumPlac + EssReg,
                             data=Perches, FUN= sum, na.rm=T, keep.names=T)
  gfPerchesPlaQual <- summaryBy(Nha + Gha + Vha + VhaIFN ~ NumForet + Cycle + NumPlac + Reg1,
                              data=Perches, FUN= sum, na.rm=T, keep.names=T)

  # --------- BMS echantillonnage lineaire -------
  if(dim(BMSLineaires)[1] > 0) {
    temp <- subset(BMSLineaires, select=c(1:6,10,12))
    temp$Classe <- floor(temp$Diam/5+0.5)*5
    temp$StadeE <- floor(temp$Stade/10)
    temp$StadeD <- temp$Stade - temp$StadeE*10
    BMSolLin <- summaryBy(Vha ~ NumForet + NumPlac + Cycle + NumPlac + Strate + Classe + StadeE + StadeD,
    											 data=temp, FUN= sum, na.rm=T, keep.names=T)
    BMSolLin <- merge(BMSolLin, Placettes[,c(1:9,12)], by=c("NumForet","NumPlac"))
  } else {
    BMSolLin <- data.frame()
  }

  # --------- BMS echantillonnage cercle -------
  if(dim(BMSsup30)[1] > 0) {
    BMSsup30$StadeE <- floor(BMSsup30$Stade/10)
    BMSsup30$StadeD <- BMSsup30$Stade - BMSsup30$StadeE*10
    BMSsup30V     <- summaryBy(Vha ~ NumForet + NumPlac + Cycle + Classe + StadeD + StadeE,
                               data=BMSsup30, FUN=sum, keep.names=T)
    BMSsup30VEss  <- summaryBy(Vha ~ NumForet + NumPlac + Cycle + Essence,
                               data=BMSsup30, FUN=sum, keep.names=T)
    BMSsup30VClasse <- summaryBy(Vha ~ NumForet + NumPlac + Cycle + Classe,
                                 data=BMSsup30, FUN=sum, keep.names=T, na.rm=T)
    BMSsup30VStadeD <- summaryBy(Vha ~ NumForet + NumPlac + Cycle + StadeD,
                                 data=BMSsup30, FUN=sum, keep.names=T, na.rm=T)
    BMSsup30VStadeE <- summaryBy(Vha ~ NumForet + NumPlac + Cycle + StadeE,
                                 data=BMSsup30, FUN=sum, keep.names=T, na.rm=T)
  } else {
    BMSsup30V       <- data.frame()
    BMSsup30VEss    <- data.frame()
    BMSsup30VClasse <- data.frame()
    BMSsup30VStadeD <- data.frame()
    BMSsup30VStadeE <- data.frame()
  }


  if(dim(BMP)[1] > 0) {
    temp <- BMP
    temp$Stade[which(is.na(temp$Stade))] <- 11
    temp$Classe <- floor(temp$Diam/5+0.5)*5
    temp$StadeE <- floor(temp$Stade/10)
    temp$StadeD <- temp$Stade - temp$StadeE*10
    BMPpla <- summaryBy(Gha + Vha ~ NumForet + NumPlac + Cycle + Classe + StadeE + StadeD,
  									data=temp, FUN= sum, na.rm=T, keep.names=T)
  } else {
    BMPpla <- data.frame()
  }


# --------- Tableaux codes ecologiques par placettes -------
# ncol <- dim(Codes)[2]
# EcoPla <- aggregate(Codes[,8:(ncol-1)], by = list(Codes$NumForet,Codes$Cycle,Codes$NumPlac), FUN = sum)
# names(EcoPla)[1:3] <- c("NumForet","Cycle","NumPlac")
# EcoPla <- merge(EcoPla,Placettes[,1:5], by = c("NumForet","NumPlac"), all.x=T)
#
# EcoPlaClasse <- aggregate(Codes[,8:(ncol-1)], by = list(Codes$NumForet,Codes$Cycle,Codes$NumPlac,Codes$Classe), FUN = sum)
# names(EcoPlaClasse)[1:4] <- c("NumForet","Cycle","NumPlac","Classe")
# EcoPlaClasse <- merge(EcoPlaClasse,Placettes[,1:5], by = c("NumForet","NumPlac"), all.x=T)
#
# Codes <- merge(Codes, EssReg, by = c("NumForet","Essence"), all.x=T)
# EcoPlaEss <- aggregate(Codes[,8:(ncol-1)], by = list(Codes$NumForet,Codes$Cycle,Codes$NumPlac,Codes$EssReg), FUN = sum)
# names(EcoPlaEss)[1:4] <- c("NumForet","Cycle","NumPlac","EssReg")
# EcoPlaEss <- merge(EcoPlaEss,Placettes[,1:5], by = c("NumForet","NumPlac"), all.x=T)


  # --------- Tableaux taillis par placettes -------
if (dim(Taillis)[1] >0) {
  temp <- Taillis
  temp <- merge(temp, Placettes[,c(1:4)], by=c("NumForet","NumPlac"))
  temp <- merge(temp, EssReg, by=c("NumForet","Essence"), all.x=T)
  names(temp)[which(names(temp)=="Poids")] <- "Nha"
  gfTaillisPlaEss <- summaryBy(Nha + Gha + Vha ~  NumForet + NumPlac + Strate + Cycle + Essence,
                               data=temp, FUN= sum, na.rm=T, keep.names=T)
  gfTaillisPlaEssReg <- summaryBy(Nha + Gha + Vha ~  NumForet + NumPlac + Strate + Cycle + EssReg,
                                  data=temp, FUN= sum, na.rm=T, keep.names=T)
  gfTaillisPla <- summaryBy(Nha + Gha + Vha ~  NumForet + NumPlac + Strate + Cycle,
                            data=temp, FUN= sum, na.rm=T, keep.names=T)
} else {
  gfTaillisPla <- data.frame()
  gfTaillisPlaEss <- data.frame()
  gfTaillisPlaEssReg <- data.frame()
}


  # --------- Tableaux regeneration par placettes -------
  if (dim(Reges)[1] >0) {
    gfRegePlaEss <- summaryBy(Recouv + Classe1 + Classe2 + Classe3 ~
                                NumForet + NumPlac + Strate + Cycle + Essence + EssReg,
                             data=Reges, FUN= sum, na.rm=T, keep.names=T)
    gfRegePlaEss$Total <- gfRegePlaEss$Recouv + gfRegePlaEss$Classe1 + gfRegePlaEss$Classe2 + gfRegePlaEss$Classe3
    gfRegePlaEss <- gfRegePlaEss[which(gfRegePlaEss$Total >0),]
    gfRegePlaEss$Total <- NULL
    gfRegePlaEss <- merge(gfRegePlaEss, Placettes[,c(1:4)], by=c("NumForet","NumPlac","Strate"), all.x=T)
    gfRegePlaEss <- merge(gfRegePlaEss, EssReg, by=c("NumForet","Essence"), all.x=T)
    gfRegePlaEss <- merge(gfRegePlaEss, Echantillonnages[,c(1:3,14)], by=c("NumForet","Cycle","Strate"), all.x=T)
    gfRegePlaEss[,6:9] <- gfRegePlaEss[,6:9]/gfRegePlaEss$NbSousPlac
    gfRegePlaEssReg <- summaryBy(Recouv + Classe1 + Classe2 + Classe3 ~
                                   NumForet + NumPlac + Strate + Cycle + EssReg,
                             data=gfRegePlaEss, FUN= sum, na.rm=T, keep.names=T)
    gfRegePla <- summaryBy(Recouv + Classe1 + Classe2 + Classe3 ~ NumForet + NumPlac + Strate + Cycle,
                             data=gfRegePlaEss, FUN= sum, na.rm=T, keep.names=T)
  } else {
    gfRegePla <- data.frame()
    gfRegePlaEss <- data.frame()
    gfRegePlaEssReg <- data.frame()
  }

  # --------- Sauvegarde -------
  gfDen <- den
  save(Placettes,Coords,
		gfDen,gfPla,gfPlaDen,gfPlaCat,gfPlaCatDen,
    gfPlaCatEssReg,gfPlaCatEssRegDen,gfPlaEssReg,gfPlaEssRegDen,gfPlaEssRegQual,gfPlaEssRegQualDen,
    gfPlaCatQual1,gfPlaCatQual2,gfPlaCatQual2Den,gfPlaEss,gfPlaEssDen,
    gfPlaClasseQual1,gfPlaClasseQual1Den,gfPlaClasseQual2,gfPlaClasseQual2Den,gfPlaClasseEssReg,
    gfPlaEssRegQual1,gfPlaEssRegQual2,
    gfPerchesPla,gfPerchesPlaEss,gfPerchesPlaQual,
    gfTaillisPla, gfTaillisPlaEss, gfTaillisPlaEssReg,
    gfRegePla,gfRegePlaEss,gfRegePlaEssReg,
     file="Tables/gfTablesElaboreesPlac.RData")
}
