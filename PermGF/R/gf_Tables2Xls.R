#' Export des tables elaborees au format xlsx.
#' @description Sauvegarde des tables élaborées dans un classeur Excel au format xlsx, dans un dossier Out.
#' @return La fonction exporte dans un classeur au format .xlsx les tables suivantes :
#'
#' gfDendroForet,gfDendroGroupe,gfDendroGroupe1,gfDendroGroupe2,gfDendroStation,
#' gfDendroTypo,gfDendroStrate,gfEcoForet,gfEcoGroupe,gfEcoGroupe1,gfEcoGroupe2,
#' gfEcoStation,gfEcoTypo,gfEcoStrate,gfEssMoy,gfEssRegMoy,gfTaillisEss,gfTaillisEssReg,
#' gfRegeEss,gfHistClasseEssReg,gfHistClasse,gfHistClasseABC,gfHistClasseD,gfCatMoy
#'
#' Le classeur est enregistré dans le dossier Out/Resultats sous le nom : TabElaborees.xlsx.
#' @author Bruciamacchie Max
#' @export

gf_Tables2Xls <- function() {
  load("Tables/gfTablesElaborees.RData")
  dir.create("Out", showWarnings = F)
  dir.create("Out/Resultats", showWarnings = F)
  Tabs <- list(gfDendroForet,gfDendroGroupe,gfDendroGroupe1,gfDendroGroupe2,gfDendroStation,
               gfDendroTypo,gfDendroStrate,gfEcoForet,gfEcoGroupe,gfEcoGroupe1,gfEcoGroupe2,
               gfEcoStation,gfEcoTypo,gfEcoStrate,gfEssMoy,gfEssRegMoy,gfTaillisEss,gfTaillisEssReg,
               gfRegeEss,gfHistClasseEssReg,gfHistClasse,gfHistClasseABC,gfHistClasseD,gfCatMoy)
  Noms <- c("gfDendroForet","gfDendroGroupe","gfDendroGroupe1","gfDendroGroupe2","gfDendroStation",
               "gfDendroTypo","gfDendroStrate","gfEcoForet","gfEcoGroupe","gfEcoGroupe1","gfEcoGroupe2",
               "gfEcoStation","gfEcoTypo","gfEcoStrate","gfEssMoy","gfEssRegMoy","gfTaillisEss","gfTaillisEssReg",
               "gfRegeEss","gfHistClasseEssReg","gfHistClasse","gfHistClasseABC","gfHistClasseD","gfCatMoy")
  wb <- createWorkbook()
  for (i in 1:length(Tabs)) {
    addWorksheet(wb, Noms[i])
    writeData(wb, Noms[i], Tabs[[i]])
  }
  saveWorkbook(wb, "Out/Resultats/gfTabElaborees.xlsx", overwrite = TRUE)
}
