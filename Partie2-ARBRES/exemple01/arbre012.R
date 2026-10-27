# ============================================================
# TP I - Fouille de donnees avec le langage R
# Partie B - Arbre de regression : accessibilite du logement
# Jeu : accessibilit-economica-alloggi-26-paesi-1975-2025.csv
#
# Objectif :
# Representer textuellement et graphiquement un arbre qui estime
# affordability_proxy selon le pays et l'annee.
#
# Remarque :
# country et year sont utilises comme variables explicatives.
# Les indices de prix et de revenu ne sont pas utilises comme
# predicteurs afin d'eviter d'utiliser directement des composantes
# qui peuvent entrer dans le calcul de affordability_proxy.
# ============================================================

# 1. Charger les packages necessaires dans une bibliotheque personnelle
bibliotheque_perso <- file.path(path.expand("~"), "R", "library")
if (!dir.exists(bibliotheque_perso)) {
  dir.create(bibliotheque_perso, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(bibliotheque_perso, .libPaths()))

if (!requireNamespace("rpart", quietly = TRUE)) {
  install.packages("rpart", lib = bibliotheque_perso,
                   repos = "https://cloud.r-project.org")
}
if (!requireNamespace("rpart.plot", quietly = TRUE)) {
  install.packages("rpart.plot", lib = bibliotheque_perso,
                   repos = "https://cloud.r-project.org")
}

library(rpart)
library(rpart.plot)

# 2. Importer le fichier CSV
fichier_csv <- "data012.csv"
if (!file.exists(fichier_csv)) {
  fichier_csv <- file.choose()
}
donnees_brutes <- read.csv(
  fichier_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# 3. Explorer rapidement les donnees
cat("\n========== DESCRIPTION DU JEU DE DONNEES ==========\n")
cat("Dimensions initiales :", nrow(donnees_brutes), "lignes et",
    ncol(donnees_brutes), "colonnes\n")
cat("\nPremieres observations :\n")
print(head(donnees_brutes))
cat("\nStructure :\n")
str(donnees_brutes)

# 4. Selectionner et preparer les variables
colonnes <- c("country", "year", "affordability_proxy")
if (!all(colonnes %in% names(donnees_brutes))) {
  stop("Le CSV doit contenir les colonnes country, year et affordability_proxy.")
}

donnees <- donnees_brutes[, colonnes]
donnees$country <- as.factor(donnees$country)
donnees$year <- as.numeric(donnees$year)
donnees$affordability_proxy <- as.numeric(donnees$affordability_proxy)

cat("\nValeurs manquantes avant nettoyage :\n")
print(colSums(is.na(donnees)))

donnees <- na.omit(donnees)

cat("\nDimensions apres nettoyage :", nrow(donnees), "lignes et",
    ncol(donnees), "colonnes\n")
cat("Nombre de pays :", nlevels(droplevels(donnees$country)), "\n")
cat("Periode couverte :", min(donnees$year), "-", max(donnees$year), "\n")
cat("\nResume de affordability_proxy :\n")
print(summary(donnees$affordability_proxy))

# 5. Construire l'arbre de regression
# La cible est numerique, donc method = "anova".
# cp = 0 evite d'appliquer un seuil de complexite pour arreter les divisions.
# Il n'y a aucun appel explicite a prune() dans ce script.
arbre_accessibilite <- rpart(
  affordability_proxy ~ country + year,
  data = donnees,
  method = "anova",
  control = rpart.control(
    cp = 0,
    minsplit = 40,
    maxdepth = 4
  )
)

# 6. Afficher la representation textuelle dans la console
cat("\n========== REPRESENTATION TEXTUELLE DE L'ARBRE ==========\n")
sortie_arbre <- capture.output(print(arbre_accessibilite))
cat(paste(sortie_arbre, collapse = "\n"), "\n")

cat("\n========== TABLE DES PARAMETRES DE COMPLEXITE ==========\n")
sortie_complexite <- capture.output(printcp(arbre_accessibilite))
cat(paste(sortie_complexite, collapse = "\n"), "\n")

texte_arbre <- c(
  "ARBRE DE REGRESSION - ACCESSIBILITE ECONOMIQUE DU LOGEMENT",
  "Jeu de donnees : accessibilit-economica-26-paesi-1975-2025.csv",
  "Variable cible : affordability_proxy",
  "Variables explicatives : country et year",
  "",
  "Representation textuelle :",
  sortie_arbre,
  "",
  "Table des parametres de complexite :",
  sortie_complexite
)

# 7. Mise en forme des etiquettes longues pour le graphique seulement.
# Les noms complets des pays restent inchanges dans l'arbre et la sortie texte.
# Les longues listes sont simplement reparties sur plusieurs lignes.
formater_etiquettes <- function(x, labs, digits, varlen, faclen) {
  labs <- gsub(",", ", ", labs, fixed = TRUE)

  for (i in seq_along(labs)) {
    labs[i] <- paste(
      strwrap(labs[i], width = 34),
      collapse = "\n"
    )
  }

  labs
}

# Fonction qui affiche l'arbre avec une police adaptee et des etiquettes repliees.
dessiner_arbre <- function() {
  rpart.plot(
    arbre_accessibilite,
    type = 2,
    fallen.leaves = TRUE,
    digits = 2,
    main = "Arbre de regression - Accessibilite du logement",
    extra = 101,
    split.fun = formater_etiquettes,
    cex = 0.72,
    split.cex = 0.72,
    tweak = 0.95,
    clip.right.labs = TRUE
  )
}

cat("\n========== REPRESENTATION GRAPHIQUE ==========\n")

# Ouvrir une fenetre plus large si le script est lance interactivement.
if (interactive()) {
  try(grDevices::dev.new(width = 14, height = 9, pointsize = 10),
      silent = TRUE)
}

dessiner_arbre()

# Laisser le temps d'examiner les deux representations avant sauvegarde.
if (interactive()) {
  invisible(readline(
    prompt = paste0(
      "\nExaminez la representation textuelle et le graphique. ",
      "Appuyez sur Entree pour enregistrer les fichiers.\n"
    )
  ))
}

# 8. Enregistrer les representations apres leur affichage.
writeLines(
  texte_arbre,
  "arbre_accessibilite_texte.txt",
  useBytes = TRUE
)

# Image de grande taille pour zoomer sans perte excessive de lisibilite.
png(
  "arbre_accessibilite_logement.png",
  width = 3600,
  height = 2400,
  res = 240,
  pointsize = 12
)
dessiner_arbre()
dev.off()

cat("\nRepresentations enregistrees :\n")
cat("- arbre_accessibilite_texte.txt : representation textuelle\n")
cat("- arbre_accessibilite_logement.png : representation graphique haute resolution\n")
cat("\nFin du programme.\n")

