#' Calcul des variables dendrométriques, économiques et écologiques.
#' @description Cette fonction commence par calculer le poids de chaque arbre,
#' puis pour chacun d'eux, calcule les variables dendrométriques, économiques et écologiques ramenées à l'hectare.
#' @return La fonction construit les tables suivantes.
#'
#' arbres, Reges, Taillis, Reperes, BMSLineaires, BMSsup30, BMP, Codes
#'
#' Elles sont enregistrées dans le dossier Tables sous le nom : gfTablesBrutes.RData.
#' @param TauxR = taux d'actualisation (0.03 par défaut)
#' @author Bruciamacchie Max
#' @export


gf_Calculs <- function(TauxR=0.03) {
  ########### Paramètres ################
  Coeffts <- data.frame(Vides=c(0:4), Coeffts=c(1,0.58159,0.33930,0.15351,0))
  ########### Construction du fichier arbres - Fusion des tables ############
  arbres <- merge(IdArbres, ValArbres, by="IdArbre", all.y = T, sort=F)
  #--------------- Variables dendrometriques
  arbres$Diam <- with(arbres, (Diam1 + Diam2)/2)
  arbres <- subset(arbres, Diam>=7.5) # suppression des arbres trop petits
  arbres$Classe <- floor(arbres$Diam/5+0.5)*5
  arbres$Cat <- cut(arbres$Diam, breaks = c(0, 17.5, 27.5, 47.5, 67.5,200),
                    labels = c("PER", "PB", "BM", "GB","TGB"), include.lowest = T, right = F)
  #--------------- Corrections
  pos <- which(arbres$Type=="M")
  if (length(pos) > 0) {arbres$Qual <- "D"} # Valeurs par defaut : D pour arbres morts}
  pos <- which(arbres$Classe<7.5)
  if (length(pos) > 0) {arbres$PU <- 0}
  if (dim(Reges)[1] >0) {Reges <- Reges[-which(is.na(Reges$Essence)),]}
  rm(pos)
  #--------------- Suite fusion
  arbres <- merge(arbres, Placettes[,c(1:9,11:12)], by=c("NumForet","NumPlac"), all.x=T, sort=F)
  arbres <- merge(arbres, Echantillonnages[,1:13], by=c("NumForet","Cycle","Strate"), all.x = T, sort=F)
  arbres <- merge(arbres, Tarifs, by=c("NumForet","Cycle","Strate","Essence"), all.x=T, sort=F)
  arbres <- merge(arbres, Quals, by.x="Qual", by.y = "Nom", all.x=T, sort=F)
  arbres <- merge(arbres, Prix, by.x=c("Essence","Classe","Reg1"), by.y=c("Essence","Classe","Qual"), all.x=T, sort=F)
  #--------------- Tri
  arbres <- arbres[order(arbres$NumForet,arbres$Cycle,arbres$NumPlac,arbres$Azimut),]

  ########### Calcul du poids ############
  arbres$Poids <- NA
  # arbres$Poids <- 0
  # cas des perches sans mesure de distance
  pos <- which(arbres$Cat=="PER" & is.na(arbres$Dist))
  if (length(pos) > 0) arbres[pos,"Poids"] <- 10000/pi/arbres$Rayon1[pos]^2
#   # cas des perches avec mesure de distance
#   pos <- which(arbres$Cat=="PER" & !is.na(arbres$Dist) & arbres$Dist > arbres$Rayon1 * arbres$CoeffPente)
#   if (length(pos) > 0) arbres[pos,"Poids"] <- 0

  # ------------ Cercles uniques
  pos <- which(is.na(arbres$Poids) & is.na(arbres$DiamLim2) &
                 arbres$Diam >= arbres$DiamLim1 &
                 arbres$Dist <= arbres$Rayon1 * arbres$CoeffPente)
  if (length(pos) > 0) arbres[pos,"Poids"] <- 10000/pi/arbres$Rayon1[pos]^2
#   # ------------ Cercles concentriques
  pos <- which(is.na(arbres$Poids) & !is.na(arbres$DiamLim2) &
                 arbres$Diam >= arbres$DiamLim1 & arbres$Diam <= arbres$DiamLim2 &
                 arbres$Dist <= arbres$Rayon1 * arbres$CoeffPente)
  if (length(pos) > 0) arbres[pos,"Poids"] <- 10000/pi/arbres$Rayon1[pos]^2
  pos <- which(is.na(arbres$Poids) & is.na(arbres$DiamLim3) &
                 arbres$Diam > arbres$DiamLim2 & arbres$Diam <= arbres$DiamLim3 &
                 arbres$Dist <= arbres$Rayon2 * arbres$CoeffPente)
  if (length(pos) > 0) arbres[pos,"Poids"] <- 10000/pi/arbres$Rayon2[pos]^2
  pos <- which(is.na(arbres$Poids) & !is.na(arbres$DiamLim3) &
                 arbres$Diam > arbres$DiamLim3 &
                 arbres$Dist <= arbres$Rayon3 * arbres$CoeffPente)
  if (length(pos) > 0) arbres[pos,"Poids"] <- 10000/pi/arbres$Rayon3[pos]^2
  # ------------ Angle fixe
  pos <- which(arbres$Diam < arbres$DiamLim)
  if (length(pos) > 0) arbres[pos,"Coeff"] <- NA
#   pos <- which(is.na(arbres$Poids) & arbres$Diam < arbres$DiamLim & arbres$Dist <= arbres$Rayon1 * arbres$CoeffPente)
#   if (length(pos) > 0) arbres[pos,"Poids"] <- 10000/pi/arbres$Rayon1[pos]^2
  pos <- which(!is.na(arbres$Coeff) & arbres$Diam1 >= arbres$Dist * arbres$Coeff * 100)
  if (length(pos) > 0) arbres[pos,"Poids"] <- 10^8*arbres$Coeff[pos]^2/pi/arbres$Diam[pos]^2
  pos <- which(!is.na(arbres$Coeff) & arbres$Diam1 < arbres$Dist * arbres$Coeff * 100)
  if (length(pos) > 0) arbres[pos,"Poids"] <- 0
  rm(pos)
  ########### Donnees /ha ############
  arbres$Gha <- pi*arbres$Diam^2/40000 * arbres$Poids
  arbres$Vha <- NA
  pos <- which(arbres$TypeTarif=="SchR")
  if (length(pos) > 0) {
    arbres$Vha[pos] <-  5/70000*(8+arbres$NumTarif[pos])*(arbres$Diam[pos]-5)*
                        (arbres$Diam[pos]-10)*arbres$Poids[pos]}
  pos <- which(arbres$TypeTarif=="SchI")
  if (length(pos) > 0) {
    arbres$Vha[pos] <-  5/80000*(8+arbres$NumTarif[pos])*(arbres$Diam[pos]-2.5)*
                        (arbres$Diam[pos]-7.5)*arbres$Poids[pos]}
  pos <- which(arbres$TypeTarif=="SchL")
  if (length(pos) > 0) {
    arbres$Vha[pos] <-  5/90000*(8+arbres$NumTarif[pos])*(arbres$Diam[pos]-5)*
                        arbres$Diam[pos]*arbres$Poids[pos]}
  pos <- which(arbres$TypeTarif=="SchTL")
  if (length(pos) > 0) {
    arbres$Vha[pos] <-  5/101250*(8+arbres$NumTarif[pos])*arbres$Diam[pos]^2*arbres$Poids[pos]}

  arbres$Vha[which(arbres$Vha<0)] <- 0
  # ------ Valeur consommation
  arbres$VcHa <- arbres$Vha*arbres$PU
  # ------ Volume de la classe supérieure
  arbres$DiamSup <- arbres$Diam + 5
  arbres$ClasseSup <- arbres$Classe + 5
  arbres$VhaSup <- NA
  pos <- which(arbres$TypeTarif=="SchR")
  if (length(pos) > 0) {
    arbres$VhaSup[pos] <- 5/70000*(8+arbres$NumTarif[pos])*(arbres$DiamSup[pos]-5)*
                        (arbres$DiamSup[pos]-10)*arbres$Poids[pos]}
  pos <- which(arbres$TypeTarif=="SchI")
  if (length(pos) > 0) {
    arbres$VhaSup[pos] <- 5/80000*(8+arbres$NumTarif[pos])*(arbres$DiamSup[pos]-2.5)*
                        (arbres$DiamSup[pos]-7.5)*arbres$Poids[pos]}
  pos <- which(arbres$TypeTarif=="SchL")
  if (length(pos) > 0) {
    arbres$VhaSup[pos] <- 5/90000*(8+arbres$NumTarif[pos])*(arbres$DiamSup[pos]-5)*
                        arbres$DiamSup[pos]*arbres$Poids[pos]}
  pos <- which(arbres$TypeTarif=="SchTL")
  if (length(pos) > 0) {
    arbres$VhaSup[pos] <- 5/101250*(8+arbres$NumTarif[pos])*arbres$DiamSup[pos]^2*arbres$Poids[pos]}
  rm(pos)
  # ------ Taux
  arbres$TauxV <- 0
  pos <- which(arbres$Vha>0)
  arbres$TauxV[pos] <- log(arbres$VhaSup[pos]/arbres$Vha[pos])/5
  rm(pos)
  # ------ Valeur potentielle
  PrixSup <- Prix
  names(PrixSup)[4] <- "PUSup"
  arbres <- merge(arbres, PrixSup, by.x = c("Essence", "ClasseSup", "Reg1"),
                  by.y = c("Essence", "Classe", "Qual"), all.x = T)
  rm(PrixSup) # suppression de l'objet
  arbres <- merge(arbres, AccD, by = c("NumForet", "Strate", "Essence","Classe"), all.x = T)
  pos <- which(arbres$Type=="M")
  if (length(pos)>0) {arbres[pos,"AccD"] <- 0}
  arbres$TauxPU  <- log(arbres$PUSup/arbres$PU)/5
  arbres$Taux    <- (arbres$TauxPU + arbres$TauxV) * arbres$AccD
  arbres$AcctV   <- arbres$TauxV * arbres$Vha * arbres$AccD
  arbres$Gain    <- arbres$Taux * arbres$VcHa
  arbres$VpHa    <- arbres$Gain/TauxR

  ########### Regeneration ################
  if (dim(Reges)[1] > 0) {
    Reges <- merge(Reges, Placettes[,1:9,12], by=c("NumForet","NumPlac"), all.x=T, sort=F)
  Reges <- merge(Reges, Echantillonnages[,c("NumForet","Cycle","Strate","NbPlac","NbSousPlac","RayonSousPlac")],
                 by=c("NumForet","Cycle","Strate"), all.x = T, sort=F)
  Reges$EssValor <- 0
  for (i in 1:dim(Forets)[1]) {
    EssEnTour <- subset(EssInd, NumForet==Forets$NumForet[i], select="Essence")
    pos <- which(Reges$NumForet==Forets$NumForet[i] & is.element(Reges$Essence, t(EssEnTour)))
    Reges$EssValor[pos] <- 1
  }

  Reges$Surf <- ifelse(Reges$Class1 + Reges$Class2+ Reges$Class3 >=5, 1, 0)
  Reges$Surf <- Reges$Surf/Reges$NbPlac
  SurfRege <- summaryBy(Surf + Surf*EssValor ~ NumForet+ Cycle + Ss.Plac+ Essence ,
                        data=Reges, FUN= sum, na.rm=T, keep.names=T)

  # SurfRege1 <- data.frame(Pourc= c(sum(Reges$Surf)/Echantillonnages[1,12]/Echantillonnages[1,13],
  #   															sum(Reges$Surf*Reges$EssValor)/Echantillonnages[1,12]/Echantillonnages[1,13]))
  # ----- Donnees hectare
  Reges$Classe1 <- Reges$Class1* 10000/pi/Reges$RayonSousPlac^2
  Reges$Classe2 <- Reges$Class2* 10000/pi/Reges$RayonSousPlac^2
  Reges$Classe3 <- Reges$Class3* 10000/pi/Reges$RayonSousPlac^2
  Reges <- subset(Reges, select=c("NumForet","Strate","Cycle","NumPlac","Ss-Plac",
                                  "Parcelle","Groupe","Typologie","Groupe1","Station",
                                  "Essence","EssValor","Recouv","Classe1","Classe2","Classe3"))
  } else {
    Reges <- data.frame()
  }


  ########### taillis ################
  # ---- PCQM
  if (dim(PCQM)[1] > 0) {
    Taillis <- subset(PCQM, Population=="Taillis",
                select=c("NumForet","NumPlac","Cycle","Quart","Essence","Azimut","Distance","Diam"))
    if (dim(Taillis)[1] >0) {
      Taillis <- merge(Taillis, Placettes[,c(1:4)], by=c("NumForet","NumPlac"), all.x=T, sort=F)
      Corr <- data.frame(Coeff=table(Taillis$NumPlac))
      names(Corr) <- c("NumPlac","Nbre")
      Corr$Vides <- 4-Corr$Nbre
      Corr <- merge(Corr, Coeffts, by="Vides", all.x=T, sort=F)
      Tab <- summaryBy(Distance^2 ~ NumForet + NumPlac + Cycle,
                       data=Taillis, FUN= sum, na.rm=T, keep.names=T)
      names(Tab)[dim(Tab)[2]] <- "Poids"
      Tab$Poids <- 10000*3/pi/Tab$Poids
      Taillis <- merge(Taillis, Tab, by=c("NumForet","NumPlac","Cycle"), all.x=T, sort=F)
      Taillis <- merge(Taillis, Corr[,c(2,4)], by="NumPlac", all.x=T)
      Taillis$Poids <-Taillis$Poids * Taillis$Coeffts
      #     Taillis$Coeffts <- NULL
      Taillis$Gha <- pi/40000*Taillis$Diam^2 * Taillis$Poids
      Taillis$Vha <- Taillis$Gha * 7
      Taillis <- subset(Taillis, select=c("NumForet","NumPlac","Cycle","Essence","Diam","Poids","Gha","Vha"))
    } else {
    Taillis <- data.frame()
    }
  }
    # ---- Cercles
  if (dim(Cercles)[1] > 0) {
    Cercles <- merge(Cercles, Placettes[,c(1:9,11:12)], by=c("NumForet","NumPlac"), all.x=T, sort=F)
    Taillis <- merge(Cercles, Echantillonnages[,c("NumForet","Cycle","Strate","Taillis")],
                     by=c("NumForet","Strate","Cycle"), all.x=T, sort=F)
    Taillis$Poids <- 10000/pi/Taillis$Taillis*Taillis$Nbre
    Taillis$Gha <- pi/40000*Taillis$Diam^2 * Taillis$Poids
    Taillis$Vha <- Taillis$Gha * 7
    Taillis <- subset(Taillis, select=c("NumForet","NumPlac","Cycle","Essence","Diam","Poids","Gha","Vha"))
  }

  ########### Bois mort au sol ###########
  # lineaire
  if (dim(BMSLineaires)[1] > 0) {
    LongLig <- Echantillonnages$Linéaire[1]
    BMSLineaires$Vha <- pi^2*BMSLineaires$Diam^2/8/LongLig/cos(BMSLineaires$Angle*pi/180)
  } else {
    BMSLineaires <- data.frame()
  }

  # Cercle
  if (dim(BMSCercles)[1] > 0) {
    Rayon <- Echantillonnages$Cercle
    t <- BMSCercles
    t$DiamIni[is.na(t$DiamIni)] <- 0
    t$DiamFin[is.na(t$DiamFin)] <- 0
    t$DiamMed[is.na(t$DiamMed)] <- 0
    t$Vha <- 0
    t$Classe <- 0
    # ---- formule de Huber
    pos <- which((t$DiamIni + t$DiamFin)==0)
    t$Vha[pos] <- pi/40000*t$DiamMed[pos]^2*t$Longueur[pos] * 10000/pi/20^2
    t$Classe[pos] <- floor(t$DiamMed[pos]/5+0.5)*5
    # ---- formule de Smalian
    pos <- which((t$DiamIni+ t$DiamFin)!=0 & t$DiamMed==0)
    t$Vha[pos] <- pi/80000*(t$DiamIni[pos]^2+t$DiamFin[pos]^2)*t$length[pos] * 10000/pi/20^2
    t$Classe[pos] <- floor((t$DiamIni[pos]+t$DiamFin[pos])/2/5+0.5)*5
    # ---- formule de Newton
    pos <- which((t$DiamIni+ t$DiamFin)!=0 & t$DiamMed!=0)
    t$Vha[pos] <- pi/240000*(t$DiamIni[pos]^2+t$DiamFin[pos]^2 + 4*t$DiamMed[pos]^2)*t$Longueur[pos] * 10000/pi/20^2
    t$Classe[pos] <- floor((t$DiamIni[pos]+t$DiamFin[pos]+t$DiamIni[pos])/3/5+0.5)*5
    BMSsup30 <- t
  } else {
    BMSsup30 <- data.frame()
  }


  ########### Bois mort sur pied ###########
  if (dim(PCQM)[1] > 0) {
    BMP <- subset(PCQM, Population=="BMP")
    if (dim(BMP)[1] > 0) {
      BMP <- merge(BMP, Placettes[,c(1:9,11:12)], by=c("NumForet","NumPlac"), all.x=T)
      Corr <- data.frame(Coeff=table(BMP$NumForet,BMP$NumPlac))
      names(Corr) <- c("NumForet","NumPlac","Nbre")
      Corr$Vides <- 4-Corr$Nbre
      Corr <- merge(Corr, Coeffts, by="Vides", all.x=T, sort=F)
      Tab <- summaryBy(Distance^2 ~ NumForet + NumPlac + Cycle,
  							 data=BMP, FUN= sum, na.rm=T, keep.names=T)
      names(Tab)[4] <- "Poids"
      Tab$Poids <- 10000*3/pi/Tab$Poids
      BMP <- merge(BMP, Tab, by=c("NumForet","NumPlac","Cycle"), all.x=T, sort=F)
      BMP <- merge(BMP, Corr[,c(2,3,5)], by=c("NumForet","NumPlac"), all.x=T)
      BMP$Poids <-BMP$Poids * BMP$Coeffts
      BMP$Coeffts <- NULL
      BMP$Gha <- pi/40000*BMP$Diam^2 * BMP$Poids
      pos <- which(BMP$Type=="A")
      BMP$Vha[pos] <- 5/90000*(8+6)*BMP$Diam[pos]*(BMP$Diam[pos]-5)*BMP$Poids[pos]
      pos <- which(BMP$Type=="V")
      BMP$Vha[pos] <- pi/40000*(BMP$Diam[pos]-BMP$Haut[pos]/2)^2*BMP$Haut[pos]*BMP$Poids[pos]
      BMP <- subset(BMP, select=c("NumForet","NumPlac","Cycle","Essence","Diam","Stade","Gha","Vha"))
      rm(Tab,pos,Corr)
    } else {
        BMP <- data.frame()
      }
  } else {
        BMP <- data.frame()
      }

  ####### Note ecologique ######
  Codes <- subset(arbres, NoteEcolo != "", c(NumForet,Strate,Cycle,NumPlac,NumArbre,Essence,Diam,NoteEcolo,Poids))
  # ---- Liste des niveaux
  if (dim(Codes)[1] > 0) {
    Niveaux <- c("g1","g2","g3","h1","h2","h3","f1","f2","f3","a1","a2","a3","p1","p2","p3","i1","i2","i3","c1",
  					  "c2","c3","e1","e2","e3","b1","b2","b3","l1","l2","l3","r1","r2","r3","k","ts","tc","tn","tx","d")
    # ---- Decomposition
    NbCodes <-length(Niveaux)
    for (i in 1:NbCodes) {
	    Codes$Temp <- ifelse (str_detect(Codes$NoteEcolo, Niveaux[i]),Codes$Poids,0)
	    if (sum(Codes$Temp, na.rm=T) == 0) {
		    Codes$Temp <- NULL
	    } else {
		    names(Codes)[dim(Codes)[2]] <- Niveaux[i]
	    }
    }
  Codes$NoteEcolo <- NULL
  Codes$Poids <- NULL
  Codes$Classe <- floor(Codes$Diam/5+0.5)*5
  } else {
        Codes <- data.frame()
      }
  ########### Nettoyage ################
  arbres <- subset(arbres, select=c("NumForet","NumPlac","NumArbre","Cycle","Strate",
                                    "IdArbre","Azimut","Dist","Observation",
                                    "Essence","Qual","Type","Reg1", "Reg2","Haut","Stade","Limite",
                                    "Diam1","Diam2","Diam","Classe","Cat","NoteEcolo","Vitalité","PU",
                                    "Poids","Gha","Vha","VcHa","VpHa","Gain","AcctV","Taux","TauxPU","TauxV","AccD"))
  save(TauxR,arbres,Reges,Taillis,Reperes,BMSLineaires,BMSsup30,BMP,Codes,
       file="Tables/gfTablesBrutes.RData")
}
