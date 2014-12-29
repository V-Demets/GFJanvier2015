# --------------- Librairies
library(splines)
library(survival)
library(MASS)
library(PermGF)
library(doBy)
library(stringr)
library(reshape2)
library(tcltk)
library(openxlsx)
library(rmarkdown)
# --------------- Initialisation
# rm(list = ls()) # Efface tous les objets
rep <- tclvalue(tkchooseDirectory(title="Choix du répertoire de travail"))
setwd(rep)
file <- tclvalue(tkgetOpenFile(title="Choix du fichier contenant les données"))
########################## Import ##########################
gf_Xls2Rdata(file)        # Reconstruction des Donnees brutes
########################## Premières sorties ###############
load("Tables/gfDonneesBrutes.Rdata")
gf_Verif(file, modif=F)           # Vérification classeur(s) avec gf_Verif.Rmd
# gf_ClasseurRem()      # Edition d'un classeur Excel facilitant la remesure
# fiches de remesure : gfFicheRem.Rnw
# plan de localisation des arbres : gf_PlanArbres.Rnw()
########################## Traitement ######################
load("Tables/gfDonneesBrutes.Rdata")
gf_Calculs()
# --------------- Aggregation par placettes
load("Tables/gfTablesBrutes.Rdata")
gf_AgregArbres()
# --------------- creation des tables foret, massif, ...
load("Tables/gfTablesElaboreesPlac.RData")
gf_AgregPlacettes()
########################## Sorties plus élaborées ######################
# --------------- Shapes par placettes
load("Tables/gfTablesElaboreesPlac.RData")
gf_ShapesPlac()
load("Tables/gfTablesElaborees.RData")
gf_Tables2Xls() # pas forcément nécessaire

