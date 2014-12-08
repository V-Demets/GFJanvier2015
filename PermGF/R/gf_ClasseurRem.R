gf_ClasseurRem <-
function() {
  library(openxlsx)
  # Sous-programme
  vidange <- function(tab){
    if(dim(tab)[1]>1) {
      tab <- tab[1,]
      tab[,3:dim(tab)[2]] <- NA
      tab[1,3] <- DernierCycle + 1
      tab
    }
  }
  # ---------------- Import
  Arbres <- merge(IdArbres, ValArbres, by="IdArbre", all.y = T, sort=F)
  # ---------------- Préparation du fichier Arbres
  DernierCycle = max(Cycles$Cycle)
  Arbres <- subset(Arbres, Cycle == DernierCycle, select=-1)
  Arbres$Cycle <- DernierCycle +1
  a <- Arbres[,8:19]
  names(a) <- paste0(names(a),2)
  Arbres <- cbind(Arbres,a)
  Arbres[,20:31] <- NA
  Arbres <- subset(Arbres, select = c("NumForet","NumPlac","NumArbre","Cycle","Essence","Azimut","Dist",
                                      "Diam1","Diam12","Diam2","Diam22","Qual","Qual2","NoteEcolo","NoteEcolo2",
                                      "Vitalité","Vitalité2","Type","Type2","Haut","Haut2","Stade","Stade2",
                                      "Caract1","Caract12","Caract2","Caract22","Limite","Limite2",
                                      "Observation","Observation2"))
  # ---------------- Préparation des autres fichiers
  if (dim(BMSCercles)[1]>0) {
    BMSCercles <- subset(BMSCercles, Cycle == DernierCycle)
    BMSCercles   <- vidange(BMSCercles)} 
  if (dim(BMSLineaires)[1]>0) {
    BMSLineaires <- subset(BMSLineaires, Cycle == DernierCycle)
    BMSLineaires <- vidange(BMSLineaires)}
  if (dim(Cercles)[1]>0) {
    Cercles <- subset(Cercles, Cycle == DernierCycle)
    Cercles      <- vidange(Cercles)}
  if (dim(Reges)[1]>0) {
    Reges <- subset(Reges, Cycle == DernierCycle)
    Reges        <- vidange(Reges)}
  if (dim(PCQM)[1]>0) {
    PCQM <- subset(PCQM, Cycle == DernierCycle)
    PCQM         <- vidange(PCQM)}
  if (dim(Hauts)[1]>0) {
    Hauts <- subset(Hauts, Cycle == DernierCycle)
    Hauts        <- vidange(Hauts)}
  # ------------------- Sauvegarde
  Tabs <- list(Forets,Cycles,Echantillonnages,Placettes,Arbres,Tarifs,Prix,EssReg,EssInd,Tiers,
               Reges,PCQM,Cercles,BMSLineaires,BMSCercles,Hauts,Coords,Reperes)
  Noms <- c("Forets","Cycles","Echantillonnages","Placettes","Arbres","Tarifs","Prix","EssReg","EssInd","Tiers",
            "Reges","PCQM","Cercles","BMSLineaires","BMSCercles","Hauts","Coords","Reperes")
  wb <- createWorkbook()
  for (i in 1:length(Tabs)) {
    addWorksheet(wb, Noms[i])
    writeData(wb, Noms[i], Tabs[[i]])
  }
  dir.create("Out", showWarnings = F)
  saveWorkbook(wb, "Out/Remesures.xlsx", overwrite = TRUE)
}
