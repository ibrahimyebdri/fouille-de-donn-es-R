# ============================================================
# TP I - Fouille de donnees avec le langage R
# Partie B - Arbre de classification : jeu de donnees Dengue
# Objectif : classifier la variable NoYes (0 ou 1)
# Affichage textuel et graphique comme dans arbre012.R
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
fichier_csv <- "dengue.csv"
if (!file.exists(fichier_csv)) {
  fichier_csv <- file.choose()
}

donnees_brutes <- read.csv(
  fichier_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# 3. Explorer rapidement les donnees
cat("\n========== DESCRIPTION DU JEU DE DONNEES DENGUE ==========\n")
cat("Fichier charge :", fichier_csv, "\n")
cat("Dimensions initiales :", nrow(donnees_brutes), "lignes et",
    ncol(donnees_brutes), "colonnes\n")
cat("\nPremieres observations :\n")
print(head(donnees_brutes))
cat("\nStructure :\n")
str(donnees_brutes)
cat("\nValeurs manquantes avant nettoyage :\n")
print(colSums(is.na(donnees_brutes)))

# 4. Selectionner et preparer les variables
colonnes <- c(
  "humid", "humid90", "temp", "temp90",
  "h10pix", "h10pix90", "trees", "trees90",
  "NoYes", "Xmin", "Xmax", "Ymin", "Ymax"
)

if (!all(colonnes %in% names(donnees_brutes))) {
  stop(paste(
    "Le CSV doit contenir les colonnes :",
    paste(colonnes, collapse = ", ")
  ))
}

donnees <- donnees_brutes[, colonnes, drop = FALSE]

# Convertir les variables en numerique et traiter les valeurs non finies
for (variable in colonnes) {
  donnees[[variable]] <- suppressWarnings(as.numeric(donnees[[variable]]))
  donnees[[variable]][!is.finite(donnees[[variable]])] <- NA
}

cat("\nValeurs manquantes dans les variables selectionnees :\n")
print(colSums(is.na(donnees)))

donnees <- na.omit(donnees)

if (nrow(donnees) < 2) {
  stop("Pas assez de lignes valides apres le nettoyage.")
}
if (!all(donnees$NoYes %in% c(0, 1))) {
  stop("La variable NoYes doit contenir uniquement les valeurs 0 et 1.")
}
donnees$NoYes <- factor(donnees$NoYes, levels = c(0, 1))
donnees$NoYes <- droplevels(donnees$NoYes)
if (nlevels(donnees$NoYes) < 2) {
  stop("La variable NoYes doit contenir les deux classes 0 et 1 apres nettoyage.")
}

cat("\nDimensions apres nettoyage :", nrow(donnees), "lignes et",
    ncol(donnees), "colonnes\n")
cat("Repartition de NoYes (0 / 1) :\n")
print(table(donnees$NoYes))
cat("\nResume statistique :\n")
print(summary(donnees))

# 5. Diviser les donnees en apprentissage (80 %) et test (20 %)
# Echantillonnage stratifie en base R : aucune dependance a caret.
set.seed(123)
indices_par_classe <- split(seq_len(nrow(donnees)), donnees$NoYes)
indices_train <- unlist(lapply(indices_par_classe, function(indices) {
  n_train <- floor(0.80 * length(indices))
  if (length(indices) >= 2) {
    n_train <- max(1, min(n_train, length(indices) - 1))
  }
  sample(indices, size = n_train)
}), use.names = FALSE)

train <- donnees[indices_train, , drop = FALSE]
test <- donnees[-indices_train, , drop = FALSE]
if (nrow(test) == 0) {
  stop("Le jeu de test est vide.")
}

cat("\n========== DIVISION DES DONNEES ==========\n")
cat("Observations d'apprentissage :", nrow(train), "\n")
cat("Observations de test :", nrow(test), "\n")
cat("Repartition des classes dans l'apprentissage :\n")
print(table(train$NoYes))
cat("Repartition des classes dans le test :\n")
print(table(test$NoYes))

# 6. Construire l'arbre de classification
arbre_dengue <- rpart(
  NoYes ~ ., 
  data = train,
  method = "class",
  control = rpart.control(
    cp = 0.01,
    minsplit = 20,
    maxdepth = 5
  )
)

# 7. Afficher la representation textuelle dans la console, comme arbre012.R
cat("\n========== REPRESENTATION TEXTUELLE DE L'ARBRE ==========\n")
sortie_arbre <- capture.output(print(arbre_dengue))
cat(paste(sortie_arbre, collapse = "\n"), "\n")

cat("\n========== TABLE DES PARAMETRES DE COMPLEXITE ==========\n")
sortie_complexite <- capture.output(printcp(arbre_dengue))
cat(paste(sortie_complexite, collapse = "\n"), "\n")

texte_arbre <- c(
  "ARBRE DE CLASSIFICATION - JEU DE DONNEES DENGUE",
  "Variable cible : NoYes (classes 0 et 1)",
  paste("Variables explicatives :", paste(setdiff(colonnes, "NoYes"), collapse = ", ")),
  "",
  "Representation textuelle :",
  sortie_arbre,
  "",
  "Table des parametres de complexite :",
  sortie_complexite
)

# 8. Fonction de dessin de l'arbre
# Elle est reutilisee pour afficher l'arbre et enregistrer une image PNG.
dessiner_arbre <- function() {
  rpart.plot(
    arbre_dengue,
    type = 2,
    fallen.leaves = TRUE,
    digits = 2,
    main = "Arbre de classification - Jeu de donnees Dengue",
    extra = 104,
    cex = 0.75,
    tweak = 0.95,
    clip.right.labs = TRUE
  )
}

cat("\n========== REPRESENTATION GRAPHIQUE ==========\n")

# Ouvrir une fenetre graphique plus grande en mode interactif, comme arbre012.R
if (interactive()) {
  try(grDevices::dev.new(width = 14, height = 9, pointsize = 10),
      silent = TRUE)
}

dessiner_arbre()

# IMPORTANT : laisser le graphique visible avant de continuer.
# Dans la console R interactive, appuyer sur Entree apres avoir examine l'arbre.
if (interactive()) {
  invisible(readline(
    prompt = paste0(
      "\nExaminez la representation textuelle dans le terminal et le graphique. ",
      "Appuyez sur Entree pour enregistrer les fichiers et afficher l'evaluation.\n"
    )
  ))
}

# 9. Enregistrer les representations apres leur affichage
writeLines(
  texte_arbre,
  "arbre_dengue_texte.txt",
  useBytes = TRUE
)

png(
  "arbre_dengue.png",
  width = 3600,
  height = 2400,
  res = 240,
  pointsize = 12
)
dessiner_arbre()
dev.off()

cat("\nRepresentations enregistrees :\n")
cat("- arbre_dengue_texte.txt : representation textuelle\n")
cat("- arbre_dengue.png : representation graphique haute resolution\n")

# 10. Effectuer les predictions sur le jeu de test
predictions <- predict(arbre_dengue, newdata = test, type = "class")
predictions <- factor(predictions, levels = levels(donnees$NoYes))
reels <- factor(test$NoYes, levels = levels(donnees$NoYes))

# 11. Afficher la matrice de confusion dans le terminal
matrice_confusion <- table(Predit = predictions, Reel = reels)
cat("\n========== MATRICE DE CONFUSION ==========\n")
print(matrice_confusion)

# 12. Calculer les metriques par classe en base R
classes <- levels(donnees$NoYes)
metriques <- data.frame(
  Classe = classes,
  Precision = NA_real_,
  Recall = NA_real_,
  F1_Score = NA_real_,
  Support = as.numeric(table(reels)[classes]),
  stringsAsFactors = FALSE
)

for (i in seq_along(classes)) {
  classe <- classes[i]
  TP <- matrice_confusion[classe, classe]
  FP <- sum(matrice_confusion[classe, ]) - TP
  FN <- sum(matrice_confusion[, classe]) - TP

  metriques$Precision[i] <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
  metriques$Recall[i] <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_

  somme_pr <- metriques$Precision[i] + metriques$Recall[i]
  metriques$F1_Score[i] <- if (is.finite(somme_pr) && somme_pr > 0) {
    2 * metriques$Precision[i] * metriques$Recall[i] / somme_pr
  } else {
    NA_real_
  }
}

accuracy <- sum(diag(matrice_confusion)) / sum(matrice_confusion)
total <- sum(matrice_confusion)
accord_observe <- accuracy
accord_attendu <- sum(rowSums(matrice_confusion) *
                       colSums(matrice_confusion)) / (total^2)
kappa <- if (is.finite(accord_attendu) && (1 - accord_attendu) != 0) {
  (accord_observe - accord_attendu) / (1 - accord_attendu)
} else {
  NA_real_
}

precision_macro <- if (all(is.na(metriques$Precision))) NA_real_ else
  mean(metriques$Precision, na.rm = TRUE)
recall_macro <- if (all(is.na(metriques$Recall))) NA_real_ else
  mean(metriques$Recall, na.rm = TRUE)
f1_macro <- if (all(is.na(metriques$F1_Score))) NA_real_ else
  mean(metriques$F1_Score, na.rm = TRUE)

cat("\n========== METRIQUES PAR CLASSE ==========\n")
print(metriques, row.names = FALSE)

cat("\n========== INDICATEURS GLOBAUX ==========\n")
cat(sprintf("Accuracy          : %.4f (%.2f%%)\n", accuracy, 100 * accuracy))
cat(sprintf("Precision macro   : %.4f\n", precision_macro))
cat(sprintf("Recall macro      : %.4f\n", recall_macro))
cat(sprintf("F1-score macro    : %.4f\n", f1_macro))
cat(sprintf("Kappa de Cohen    : %.4f\n", kappa))

# 13. Exporter les predictions et les indicateurs
resultats <- data.frame(
  NoYes_Reel = reels,
  NoYes_Predit = predictions
)
write.csv(
  resultats,
  "predictions_dengue.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

indicateurs <- data.frame(
  Jeu_de_donnees = "Dengue",
  Observations_apprentissage = nrow(train),
  Observations_test = nrow(test),
  Accuracy = round(accuracy, 4),
  Precision_macro = round(precision_macro, 4),
  Recall_macro = round(recall_macro, 4),
  F1_Score_macro = round(f1_macro, 4),
  Kappa = round(kappa, 4)
)
write.csv(
  indicateurs,
  "indicateurs_dengue.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\n========== FIN DU PROGRAMME ==========\n")
cat("Fichiers sauvegardes :\n")
cat("- arbre_dengue.png\n")
cat("- arbre_dengue_texte.txt\n")
cat("- predictions_dengue.csv\n")
cat("- indicateurs_dengue.csv\n")

