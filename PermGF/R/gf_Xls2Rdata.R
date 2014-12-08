#' Conversion des données brutes de la base GF en 2 archives RData.
#' @description Import des classeurs Excel de chaque forêt et sauvegarde dans l'archive
#' gfDonneesBrutes.Rdata.
#'
#' Import des différentes tables de codification et sauvegarde dans l'archive psdrfCodes.Rdata.
#' @return La fonction exporte dans le dossier Tables l'archive suivante :
#'
#' gfDonneesBrutes.Rdata
#'
#' @author Bruciamacchie Max
#' @param rep = répertoire contenant les données
#' @import tcltk
#' @import openxlsx
#' @export
gf_Xls2Rdata <- function(rep) {
  old <- getwd()
  setwd(rep)
  file <- file.choose()
  setwd(dirname(file))

  # ---------------- Import
  Forets            <- read.xlsx(file, sheet="Foret")
  Cycles            <- read.xlsx(file, sheet="Cycle")
  Echantillonnages  <- read.xlsx(file, sheet="Echantillonnage")
  Placettes         <- read.xlsx(file, sheet="Placette")
  Tiges             <- read.xlsx(file, sheet="Arbres")
  Tarifs            <- read.xlsx(file, sheet="Tarif")
  AccD              <- read.xlsx(file, sheet="AccD")
  Prix              <- read.xlsx(file, sheet="Prix")
  EssReg            <- read.xlsx(file, sheet="EssReg")
  EssInd            <- read.xlsx(file, sheet="EssInd")
  Tiers             <- read.xlsx(file, sheet="Tiers")
  Essences          <- read.xlsx(file, sheet="Essences")
  Quals             <- read.xlsx(file, sheet="Qual")
  CodeEcolos        <- read.xlsx(file, sheet="CodeEcolo")
  Reges             <- read.xlsx(file, sheet="Rege")
  PCQM              <- read.xlsx(file, sheet="PCQM")
  Cercles           <- read.xlsx(file, sheet="Cercle")
  BMSLineaires      <- read.xlsx(file, sheet="BMSLineaire")
  BMSCercles        <- read.xlsx(file, sheet="BMSCercle")
  Hauts             <- read.xlsx(file, sheet="Haut")
  Coords            <- read.xlsx(file, sheet="Coord")
  Reperes           <- read.xlsx(file, sheet="Reperes")
  # --------------- Réorganisation
  Tiges$IdArbre     <- 1:dim(Tiges)[1]
  IdArbres          <- subset(Tiges, select=c(dim(Tiges)[2],1:7))
  ValArbres         <- subset(Tiges, select=c(dim(Tiges)[2],8:(dim(Tiges)[2]-1)))
  # --------------- Corrections
  if (dim(BMSLineaires)[1]>0) {BMSLineaires$Angle[which(is.na(BMSLineaires$Angle))] <- 0}
  Placettes$Pente[which(is.na(Placettes$Pente))] <- 0
  Placettes$CoeffPente <- 1/(1+Placettes$Pente^2)^0.25
  t1 <- Reges[,6:11]
  if (dim(t1)[1] >0) {t1[is.na(t1)] <- 0}
  Reges <- cbind(Reges[,1:5],t1,Reges$Observation)
  names(Reges)[12] <- "Observation"
  # --------------- Sauvegarde
  setwd(old)
  dir.create("Tables", showWarnings = F)
  rm(Tiges, file)
  save(Forets,Echantillonnages,Cycles,Placettes,Coords,PCQM,Cercles,Reges,
                BMSLineaires,BMSCercles,Tarifs,Tiers,Hauts,AccD,Essences,EssReg,EssInd,
                CodeEcolos,Quals,Prix,IdArbres,ValArbres,Reperes,
                file = "Tables/gfDonneesBrutes.Rdata")
}
