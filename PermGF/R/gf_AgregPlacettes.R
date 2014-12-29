#' Aggrégation des placettes à une échelle choisie par l'opérateur.
#' @description Sous-programme permettant de calculer les moyennes, coefficient de variations et erreurs relatives par groupes de placettes
#' tels que choisis dans la feuille Placettes.
#' @return La fonction construit les tables suivantes. Elles sont enregistrées dans le dossier Tables
#'
#' gffNGVdisp, gffNGVstrate, gffNGVcc,
#' gffNGVdispEss, gffNGVdispEssReg, gffNGVdispCat,
#' gffHistRB, gffHistStrate, gffHistRBEssReg,
#' gffBMRB, gffBMRBstadeD, gffBMRBstadeE
#'
#' @author Bruciamacchie Max
#' @export

gf_AgregPlacettes <- function() {
############################ Fonction calcul de moyenne et d'ecart-type pour un tableau et un regroupement donnés
gf_AgregMoySdEr <- function(df, Regroup="Strate", var.sup=NULL, debut=5, ncol=4) {
  if (Regroup=="Foret") {Placettes$Foret <- Placettes$NumForet}
  t1 <- subset(Placettes, select=c(Regroup,"NumForet","NumPlac","PoidsPlacette"))
  names(t1)[1] <- "Regroup"
  df <- merge(df, t1, by=c("NumForet","NumPlac"), all.x=T, sort=F)
  debut <- debut+length(var.sup)
  xnam <- names(df)[debut:(debut+ncol-1)]
  fmla1 <- paste0(xnam,"*PoidsPlacette", collapse= " + ")
  fmla2 <- paste0(xnam,"^2*PoidsPlacette", collapse= " + ")
  fmla <- paste(paste(fmla1,fmla2, sep=" + "), " ~ NumForet + Regroup + Cycle")
  if (length(var.sup)>0) {
    for (i in 1:length(var.sup)) {
        fmla <- paste(fmla,var.sup [i],sep=" + ")
      }
  }
  fmla <- as.formula(fmla)

  TabSum <- summaryBy(fmla, data=df, FUN= sum, na.rm=T, keep.names=T)
  Nbre <- summaryBy(PoidsPlacette ~ NumForet + Regroup, data=t1, FUN=c(length,sum))
  names(Nbre)[3:4] <- c("Nbre","Poids")
#   Nbre <- merge(Nbre, Forets[,1:2], by="NumForet", all.x=T)
  TabSum <- merge(TabSum, Nbre, by=c("NumForet", "Regroup"), all.x=T)
  b <- ((TabSum$Poids*TabSum[,(debut+ncol-1):(debut-2+2*ncol)] - TabSum[,(debut-1):(debut-2+ncol)]^2)/TabSum$Poids/(TabSum$Poids-1))^0.5
  Moy <- TabSum[,(debut-1):(debut-2+ncol)]/TabSum$Poids
  CV <- b/Moy*100
  Er <- qt(0.975, TabSum$Nbre)* CV/TabSum$Nbre^0.5
  TabSum <- cbind(TabSum[,1:(debut-2)],Moy,CV,Er,TabSum$Nbre)
  names(TabSum)[(debut-1):(debut-1+3*ncol)] <- c(paste("Moy",xnam,sep="_"),paste("CV",xnam,sep="_"),
                                                 paste("Er",xnam,sep="_"),"NbrePlacettes")
  names(TabSum)[names(TabSum)=="Regroup"] <- Regroup
  TabSum
}

# gf_AgregMoySdEr <- function(df, Regroup="Strate", var.sup=NULL,debut=4, ncol=4) {
#     if (Regroup=="RB") {Placettes$RB <- Placettes$NumDisp}
#     t1 <- subset(Placettes, select=c(Regroup,"NumDisp","NumPlac","poids"))
#     names(t1)[1] <- "Regroup"
#     df <- merge(df, t1, by=c("NumDisp","NumPlac"), all.x=T, sort=F)
#     #   debut <- ifelse(var.sup=="", debut,debut+1)
#     debut <- debut + length(var.sup)
#     xnam <- names(df)[debut:(debut+ncol-1)]
#     fmla1 <- paste0(xnam,"*poids", collapse= " + ")
#     fmla2 <- paste0(xnam,"^2*poids", collapse= " + ")
#     fmla <- paste(paste(fmla1,fmla2, sep=" + "), " ~ NumDisp + Regroup + Cycle")
#     if (length(var.sup) > 0) {
#       for (i in 1:length(var.sup)) {
#         fmla <- paste(fmla,var.sup [i],sep=" + ")
#       }
#     }
#     fmla <- as.formula(fmla)
#
#     TabSum <- summaryBy(fmla, data=df, FUN= sum, na.rm=T, keep.names=T)
#     Nbre <- summaryBy(poids ~ NumDisp + Regroup, data=t1, FUN=c(length,sum))
#     names(Nbre)[3:4] <- c("Nbre", "poids")
#     TabSum <- merge(TabSum, Nbre, by=c("NumDisp", "Regroup"), all.x=T)
#     b <- ((TabSum$poids*TabSum[,(debut+ncol):(debut-1+2*ncol)] - TabSum[,debut:(debut-1+ncol)]^2)/TabSum$poids/(TabSum$poids-1))^0.5
#     Moy <- TabSum[,debut:(debut-1+ncol)]/TabSum$poids
#     CV <- b/Moy* 100
#     Er <- qt(0.975, TabSum$Nbre)* CV/TabSum$Nbre^0.5
#     TabSum <- cbind(TabSum[,1:(debut-1)],Moy,CV,Er,TabSum$Nbre)
#     names(TabSum)[debut:(debut+3*ncol)] <- c( paste0("Moy",xnam),paste0("CV",xnam),paste0("Er",xnam),"Nbre")
#     TabSum
#   }
# ---------- Agregation dendrométrie ----------
tab <- gfPlaDen[,c(1:8,12)] #df <-tab
gfDendroForet <- gf_AgregMoySdEr(tab,"Foret",ncol=5)
gfDendroGroupe <- gf_AgregMoySdEr(tab,"Groupe",ncol=5)
gfDendroStrate <- gf_AgregMoySdEr(tab,"Strate",ncol=5)
gfDendroStation <- gf_AgregMoySdEr(tab,"Station",ncol=5)
gfDendroTypo <- gf_AgregMoySdEr(tab,"Typologie",ncol=5)
gfDendroGroupe1 <- gf_AgregMoySdEr(tab,"Groupe1",ncol=5)
gfDendroGroupe2 <- gf_AgregMoySdEr(tab,"Groupe2",ncol=5)

# ---------- Agregation économie ----------
tab <- gfPla[,c(1:4,9:11)]
gfEcoForet <- gf_AgregMoySdEr(tab,"Foret",ncol=3)
gfEcoGroupe <- gf_AgregMoySdEr(tab,"Groupe",ncol=3)
gfEcoStrate <- gf_AgregMoySdEr(tab,"Strate",ncol=3)
gfEcoStation <- gf_AgregMoySdEr(tab,"Station",ncol=3)
gfEcoTypo <- gf_AgregMoySdEr(tab,"Typologie",ncol=3)
gfEcoGroupe1 <- gf_AgregMoySdEr(tab,"Groupe1",ncol=3)
gfEcoGroupe2 <- gf_AgregMoySdEr(tab,"Groupe2",ncol=3)

# --------- Donnees par essences detaillées -------------
tab <- gfPlaEss[,c(1:9,13)]
gfDendroEss <- gf_AgregMoySdEr(tab, Regroup="Foret", var.sup="Essence")
gfDendroEssDen <- gf_AgregMoySdEr(tab,"Foret",var.sup="Essence")
tab <- gfPlaEss[,c(1:5,10:12)]
gfEcoEss <- gf_AgregMoySdEr(tab,"Foret",var.sup="Essence")
gfEcoEssDen <- gf_AgregMoySdEr(tab,"Foret",var.sup="Essence")

# --------- Donnees par essences regroupées -------------
tab <- gfPlaEssReg[,c(1:9,13)]
gfDendroEssReg <- gf_AgregMoySdEr(tab,"Foret",var.sup="EssReg",ncol=5)
gfDendroEssRegDen <- gf_AgregMoySdEr(tab,"Foret",var.sup="EssReg") #Prendre que Den ? (prend en compte perches)
tab <- gfPlaEssReg[,c(1:5,10:12)]
gfEcoEssReg <- gf_AgregMoySdEr(tab,"Foret",var.sup="EssReg")
gfEcoEssRegDen <- gf_AgregMoySdEr(tab,"Foret",var.sup="EssReg")

# --------- Taillis -------
gfTaillis <- gf_AgregMoySdEr(gfTaillisPla,"Foret", ncol=3)
gfTaillisEss <- gf_AgregMoySdEr(gfTaillisPlaEss,"Foret",var.sup="Essence",ncol=3)
gfTaillisEssReg <- gf_AgregMoySdEr(gfTaillisPlaEssReg,"Foret",var.sup="EssReg",ncol=3)

# --------- Regeneration -------
gfRege <- gf_AgregMoySdEr(gfRegePla,"Foret",ncol=4)
gfRegeEss <- gf_AgregMoySdEr(gfRegePlaEss[,c(1:9)],"Foret",var.sup="Essence",ncol=4)
gfRegeEssReg <- gf_AgregMoySdEr(gfRegePlaEssReg,"Foret",var.sup="EssReg",ncol=4)

# --------- Histogrammes Catégories, Qualités -------
gfDendroCat <- gf_AgregMoySdEr(gfPlaCatDen[,c(1:9,13)],"Foret",var.sup="Cat")
gfDendroCatEssReg <- gf_AgregMoySdEr(gfPlaCatEssRegDen[,c(1:10,14)], Regroup="Foret",
                                     var.sup=c("EssReg","Cat"), ncol=5)
gfDendroCatQual1 <- gf_AgregMoySdEr(gfPlaCatQual1[,c(1:10,14)],"Foret",var.sup=c("Reg1","Cat"))
gfDendroCatQual2Den <- gf_AgregMoySdEr(gfPlaCatQual2Den[,c(1:10,14)],"Foret",var.sup=c("Reg2","Cat"))

gfEcoCat <- gf_AgregMoySdEr(gfPlaCatDen[,c(1:5,10:12)],"Foret",var.sup="Cat", ncol=3)
gfEcoCatEssReg <- gf_AgregMoySdEr(gfPlaCatEssRegDen[,c(1:6,11:13)],"Foret",var.sup=c("EssReg","Cat"), ncol=3)
gfEcoCatQual1 <- gf_AgregMoySdEr(gfPlaCatQual1[,c(1:6,11:13)],"Foret",var.sup=c("Reg1","Cat"), ncol=3)
gfEcoCatQual2Den <- gf_AgregMoySdEr(gfPlaCatQual2Den[,c(1:6,11:13)],"Foret",var.sup=c("Reg2","Cat"), ncol=3)

# # --------- Donnees par essences detaillées -------------
# tab <- merge(gfPlaEss, Placettes[,1:4], by=c("NumForet","NumPlac","Strate"), all.x=T, sort=F)
# gfEssMoy <- summaryBy(Nha*PoidsPlacette + Gha*PoidsPlacette + Vha*PoidsPlacette + AcctV*PoidsPlacette +
#                       VcHa*PoidsPlacette + VpHa*PoidsPlacette + Gain*PoidsPlacette ~ NumForet + Cycle + Essence,
#                       data=tab, FUN= sum, na.rm=T, keep.names=T)
# names(gfEssMoy)[4:10] <- c("Nha","Gha","Vha","AcctV","VcHa","VpHa","Gain")
# gfEssMoy[,4:10] <- gfEssMoy[,4:10]/sum(Placettes$PoidsPlacette)
#
# # --------- Donnees par essences regroupées -------------
# tab <- merge(gfPlaEssReg, Placettes[,1:4], by=c("NumForet","NumPlac","Strate"), all.x=T, sort=F)
# gfEssRegMoy <- summaryBy(Nha*PoidsPlacette + Gha*PoidsPlacette + Vha*PoidsPlacette + AcctV*PoidsPlacette +
#                       VcHa*PoidsPlacette + VpHa*PoidsPlacette + Gain*PoidsPlacette ~ NumForet + Cycle + EssReg,
#                       data=tab, FUN= sum, na.rm=T, keep.names=T)
# names(gfEssRegMoy)[4:10] <- c("Nha","Gha","Vha","AcctV","VcHa","VpHa","Gain")
# gfEssRegMoy[,4:10] <- gfEssRegMoy[,4:10]/sum(Placettes$PoidsPlacette)
# gfEssRegMoy <- gfEssRegMoy[order(-gfEssRegMoy$VcHa),]
#
# # --------- Taillis -------
# if (dim(gfTaillisPla)[1] > 0) {
#   Titre <- c("Nha","Gha","Vha","Nha_cv","Gha_cv","Vha_cv","Nha_er","Gha_er","Vha_er")
#   gfTaillisEssReg <- gf_AgregMoySdEr(gfTaillisPlaEssReg,"Groupe","EssReg")
#   names(gfTaillisEssReg)[5:13] <- Titre
#   gfTaillisEss <- gf_AgregMoySdEr(gfTaillisPlaEss,"Foret","Essence")
#   names(gfTaillisEss)[5:13] <- Titre
# }
#
#
# # --------- Regeneration -------
# if (dim(gfRegePla)[1] > 0) {
# #   gfRegePlaEss <- merge(gfRegePlaEss, Placettes[,1:4], by=c("NumForet","NumPlac"), all.x=T, sort=F)
#
#   gfRegePlaEss$NbSemis <- gfRegePlaEss$Classe1 + gfRegePlaEss$Classe2 + gfRegePlaEss$Classe3
#   gfRegePlaEss <- subset(gfRegePlaEss, NbSemis >0)
#   gfRegeEss <- summaryBy(NbSemis*PoidsPlacette ~ NumForet + Cycle + Essence,
#                          data=gfRegePlaEss, FUN= sum, na.rm=T, keep.names=T)
#   names(gfRegeEss)[4] <-"NbSemis"
#   gfRegeEss$NbSemis <- gfRegeEss$NbSemis/sum(Placettes$PoidsPlacette)
# }
#
#
# --------- Histogrammes Toutes qualites -------
tab <- merge(gfPlaClasseEssReg, Placettes[,c(1,2,4)], by=c("NumForet","NumPlac"), all.x=T, sort=F)

gfHistClasseEssReg <- summaryBy(Nha*PoidsPlacette + Gha*PoidsPlacette + Vha*PoidsPlacette + VcHa*PoidsPlacette +
                             VpHa*PoidsPlacette + Gain*PoidsPlacette + AcctV*PoidsPlacette ~
                             NumForet + Cycle + Classe + EssReg,
                             data=tab, FUN= sum, na.rm=T, keep.names=T)
names(gfHistClasseEssReg)[5:11] <- c("Nha","Gha","Vha","VcHa","VpHa","Gain","AcctV")
gfHistClasseEssReg[,5:11] <- gfHistClasseEssReg[,5:11]/sum(Placettes$PoidsPlacette)

gfHistClasse <- summaryBy(Nha*PoidsPlacette + Gha*PoidsPlacette + Vha*PoidsPlacette + VcHa*PoidsPlacette +
                             VpHa*PoidsPlacette + Gain*PoidsPlacette + AcctV*PoidsPlacette ~
                             NumForet + Cycle + Classe,
                             data=tab, FUN= sum, na.rm=T, keep.names=T)
names(gfHistClasse)[4:10] <- c("Nha","Gha","Vha","VcHa","VpHa","Gain","AcctV")
gfHistClasse[,4:10] <- gfHistClasse[,4:10]/sum(Placettes$PoidsPlacette)

# --------- Histogrammes qualites ABC -------
tab <- merge(gfPlaClasseQual1, Placettes[,c(1,2,4)], by=c("NumForet","NumPlac"), all.x=T, sort=F)
temp <- subset(tab, Reg1 != "D")
gfHistClasseABC <- summaryBy(Nha*PoidsPlacette + Gha*PoidsPlacette + Vha*PoidsPlacette + AcctV*PoidsPlacette +
                               VcHa*PoidsPlacette + VpHa*PoidsPlacette + Gain*PoidsPlacette ~
                               NumForet + Strate + Cycle + Classe,
                             data=temp, FUN= sum, na.rm=T, keep.names=T)
names(gfHistClasseABC)[5:11] <- c("Nha","Gha","Vha","AcctV","VcHa","VpHa","Gain")
gfHistClasseABC[5:11] <- gfHistClasseABC[5:11]/sum(Placettes$PoidsPlacette)

# --------- Histogrammes qualite D -------
temp <- subset(tab, Reg1 == "D")
gfHistClasseD <- summaryBy(Nha*PoidsPlacette + Gha*PoidsPlacette + Vha*PoidsPlacette + AcctV*PoidsPlacette +
                               VcHa*PoidsPlacette + VpHa*PoidsPlacette + Gain*PoidsPlacette ~
                               NumForet + Strate + Cycle + Classe,
                             data=temp, FUN= sum, na.rm=T, keep.names=T)
names(gfHistClasseD)[5:11] <- c("Nha","Gha","Vha","AcctV","VcHa","VpHa","Gain")
gfHistClasseD[5:11] <- gfHistClasseD[5:11]/sum(Placettes$PoidsPlacette)


# # --------- Donnees par categories -------
# gf_MoyPond <- function(df,debut=6,retrait=7, Regroup="Groupe", Categ="Cat") {
#   if (Regroup=="Foret") {Placettes$Foret <- Placettes$NumForet}
#   t1 <- subset(Placettes, select=c(Regroup,"NumForet","NumPlac","PoidsPlacette"))
#   names(t1)[1] <- "Regroup"
#   Poids <- summaryBy(PoidsPlacette ~ Regroup, data=t1, FUN= sum, na.rm=T, keep.names=T)
#   df <- merge(df, t1, by=c("NumForet","NumPlac"), all.x=T, sort=F)
#   ncol = dim(df)[2]-retrait
#   xnam <- names(df)[debut:(debut+ncol-1)]
#   fmla1 <- paste0(xnam,"*PoidsPlacette", collapse= " + ")
#   fmla <- paste(fmla1, " ~ NumForet + Regroup + Cycle + ", Categ)
#   fmla <- as.formula(fmla)
#   TabSum <- summaryBy(fmla, data=df, FUN= sum, na.rm=T, keep.names=T)
#   TabSum <- merge(TabSum, Poids, by="Regroup", all=T)
#   names(TabSum)[5:(5+ncol-1)] <- c("Nha","Gha","Vha","VcHa","VpHa","Gain","AcctV")
#   TabSum[,5:(5+ncol-1)] <- TabSum[,5:(5+ncol-1)]/TabSum$PoidsPlacette
#   TabSum
# }
#
# gfCatMoy <- gf_MoyPond(gfPlaCat, Regroup="Foret")
# gfCatGroupeMoy <- gf_MoyPond(gfPlaCat)
#
#
# tab <- merge(gfPlaCatEssReg, Placettes[,1:4], by=c("NumForet","NumPlac","Strate"), all.x=T, sort=F)
# gfCatMoyEssReg <- summaryBy(Nha*PoidsPlacette + Gha*PoidsPlacette + Vha*PoidsPlacette + AcctV*PoidsPlacette +
#                       VcHa*PoidsPlacette + VpHa*PoidsPlacette + Gain*PoidsPlacette ~
#                       NumForet + Strate + Cycle + Cat + EssReg,
#                       data=tab, FUN= sum, na.rm=T, keep.names=T)
# names(gfCatMoyEssReg)[6:12] <- c("Nha","Gha","Vha","AcctV","VcHa","VpHa","Gain")
# gfCatMoyEssReg[,6:12] <- gfCatMoyEssReg[,6:12]/sum(Placettes$PoidsPlacette)
#
#





#
#   # Calcul de la somme des valeurs au carre ponderees en distinguant avec ou sans perches
#   TabCatCar <- summaryBy(Nha^2*PoidsPlacette + Gha^2*PoidsPlacette + Vha^2*PoidsPlacette + AcctV^2*PoidsPlacette +
#                            VcHa^2*PoidsPlacette + VpHa^2*PoidsPlacette + Gain^2*PoidsPlacette ~ NumForet + Strate + Cycle +Cat,
#                          data=gfPlaCat, FUN= sum, na.rm=T, keep.names=T)
#   names(TabCatCar)[5:11] <- c("Nha","Gha","Vha","AcctV","VcHa","VpHa","Gain")
#   TabCatCar <- merge(TabCatCar, Poids, by=c("NumForet","Strate"))
#   b <- ((TabCatCar$Poids*TabCatCar[,5:11] - (TabCatCar$Poids*TabCatMoy[,5:11])^2)/TabCatCar$Poids/(TabCatCar$Poids-1))^0.5
#   TabCatCar[,5:11] <- b
#
#   gfPlaCatQual1 <- merge(gfPlaCatQual1, Placettes[,1:4], by=c("NumForet","NumPlac","Strate"), all.x=T, sort=F)
#   gfCatQual1Moy <- summaryBy(Nha*PoidsPlacette + Gha*PoidsPlacette + Vha*PoidsPlacette + AcctV*PoidsPlacette +
#                                 VcHa*PoidsPlacette + VpHa*PoidsPlacette + Gain*PoidsPlacette ~ NumForet + Strate + Cycle + Cat + Reg1,
#                               data=gfPlaCatQual1, FUN= sum, na.rm=T, keep.names=T)
#   names(gfCatQual1Moy)[6:12] <- c("Nha","Gha","Vha","AcctV","VcHa","VpHa","Gain")
#   gfCatQual1Moy <- merge(gfCatQual1Moy, Poids, by=c("NumForet","Strate"))
#   gfCatQual1Moy[,6:12] <- gfCatQual1Moy[,6:12]/gfCatQual1Moy$Poids
#
#   # --------- Donnees par essences regroupees -------
#   gfPlaCatEssReg <- merge(gfPlaCatEssReg, Placettes[,1:4], by=c("NumForet","NumPlac","Strate"), all.x=T, sort=F)
#   gfCatEssRegMoy <- summaryBy(Nha*PoidsPlacette + Gha*PoidsPlacette + Vha*PoidsPlacette + AcctV*PoidsPlacette +
#                                  VcHa*PoidsPlacette + VpHa*PoidsPlacette + Gain*PoidsPlacette ~ NumForet + Strate + Cycle + EssReg +Cat,
#                                data=gfPlaCatEssReg, FUN= sum, na.rm=T, keep.names=T)
#   names(gfCatEssRegMoy)[6:12] <- c("Nha","Gha","Vha","AcctV","VcHa","VpHa","Gain")
#   gfCatEssRegMoy <- merge(gfCatEssRegMoy, Poids, by=c("NumForet","Strate"))
#   gfCatEssRegMoy[,6:12] <- gfCatEssRegMoy[,6:12]/gfCatEssRegMoy$Poids
#
#   gfEssRegMoy <- summaryBy(Nha + Gha + Vha + AcctV + VcHa +VpHa + Gain ~ NumForet + Strate + Cycle + EssReg,
#                             data=gfCatEssRegMoy, FUN= sum, na.rm=T, keep.names=T)
#
#   gfPlaEssRegQual1 <- merge(gfPlaEssRegQual1, Placettes[,1:4], by=c("NumForet","NumPlac","Strate"), all.x=T, sort=F)
#   gfEssRegQual1Moy <- summaryBy(Nha*PoidsPlacette + Gha*PoidsPlacette + Vha*PoidsPlacette + AcctV*PoidsPlacette +
#                                    VcHa*PoidsPlacette + VpHa*PoidsPlacette + Gain*PoidsPlacette ~ NumForet + Strate + Cycle + EssReg + Reg1,
#                                  data=gfPlaEssRegQual1, FUN= sum, na.rm=T, keep.names=T)
#   names(gfEssRegQual1Moy)[6:12] <- c("Nha","Gha","Vha","AcctV","VcHa","VpHa","Gain")
#   gfEssRegQual1Moy <- merge(gfEssRegQual1Moy, Poids, by=c("NumForet","Strate"))
#   gfEssRegQual1Moy[,6:12] <- gfEssRegQual1Moy[,6:12]/gfEssRegQual1Moy$Poids
#
#   gfPlaClasseEssReg <- merge(gfPlaClasseEssReg, Placettes[,1:4], by=c("NumForet","NumPlac","Strate"), all.x=T, sort=F)
#   gfClasseEssRegMoy <- summaryBy(Nha*PoidsPlacette + Gha*PoidsPlacette + Vha*PoidsPlacette + AcctV*PoidsPlacette +
#                                     VcHa*PoidsPlacette + VpHa*PoidsPlacette + Gain*PoidsPlacette ~ NumForet + Strate + Cycle + EssReg +Classe,
#                                   data=gfPlaClasseEssReg, FUN= sum, na.rm=T, keep.names=T)
#   names(gfClasseEssRegMoy)[6:12] <- c("Nha","Gha","Vha","AcctV","VcHa","VpHa","Gain")
#   gfClasseEssRegMoy <- merge(gfClasseEssRegMoy, Poids, by=c("NumForet","Strate"))
#   gfClasseEssRegMoy[,6:12] <- gfClasseEssRegMoy[,6:12]/gfClasseEssRegMoy$Poids
#

#




#     gfDen <- merge(gfDen, Placettes[,c("NumForet","NumPlac","Groupe")], by=c("NumForet","NumPlac"), all.x=T)

#   TabDenGroupe <- summaryBy(Nha + Gha + Vha + VcHa + VpHa + Gain + AcctV ~ NumForet + Groupe + Cycle,
#                             data=gfDen, FUN= sum, na.rm=T, keep.names=T)
#   Nbres <- data.frame(table(Placettes[,"Groupe"]))
#   TabDenGroupe <- merge(TabDenGroupe, Nbres, by.x="Groupe", by.y="Var1", all.x=T)
#   TabDenGroupe[,4:10] <- TabDenGroupe[,4:10]/TabDenGroupe$Freq
#
#   denHorsCharme = subset(gfDen, Essence!="Charme")
#   TabDenGroupeHorsCharme <- summaryBy(Nha + Gha + Vha + VcHa + VpHa + Gain + AcctV ~ NumForet + Groupe + Cycle,
#                                       data=denHorsCharme, FUN= sum, na.rm=T, keep.names=T)
#
#   TabDenGroupeHorsCharme <- merge(TabDenGroupeHorsCharme, Nbres, by.x="Groupe", by.y="Var1", all.x=T)
#   TabDenGroupeHorsCharme[,4:10] <- TabDenGroupeHorsCharme[,4:10]/TabDenGroupeHorsCharme$Freq


# --------- Sauvegarde -------
#   save(Placettes,
#      gfDendroForet,gfDendroGroupe,gfDendroGroupe1,gfDendroGroupe2,gfDendroStation,gfDendroTypo,gfDendroStrate,
#      gfEcoForet,gfEcoGroupe,gfEcoGroupe1,gfEcoGroupe2,gfEcoStation,gfEcoTypo,gfEcoStrate,
#      gfEssMoy,gfEssRegMoy,
#      gfTaillisEss,gfTaillisEssReg,
#      gfRegeEss,
#      gfHistClasseEssReg,gfHistClasse,gfHistClasseABC,gfHistClasseD,
#      gfCatMoy,
#      file="Tables/gfTablesElaborees.RData")
save(Placettes,
     gfDendroGroupe2,gfDendroGroupe1,gfDendroTypo,gfDendroStation,gfDendroStrate,gfDendroGroupe,gfDendroForet,
     gfEcoGroupe2,gfEcoGroupe1,gfEcoTypo,gfEcoStation,gfEcoStrate,gfEcoGroupe,gfEcoForet,
     gfEcoEssDen,gfEcoEss,gfDendroEssDen,gfDendroEss,
     gfRegeEssReg,gfRegeEss,gfRege,
     gfTaillisEssReg,gfTaillisEss,gfTaillis,
     gfEcoEssRegDen,gfEcoEssReg,
     gfDendroEssRegDen,gfDendroEssReg,
     gfDendroCatQual2Den,gfDendroCatQual1,gfDendroCatEssReg,gfDendroCat,
     gfEcoCatQual2Den,gfEcoCatQual1,gfEcoCatEssReg,gfEcoCat,
     gfHistClasseEssReg,gfHistClasse,gfHistClasseABC,gfHistClasseD,
     file="Tables/gfTablesElaborees.RData")
}
