# ============================================================
# TP I - Fouille de donnees avec le langage R
# Partie B - Arbre de classification : detection des cyberattaques
# Objectif : classifier les flux reseau (Benign vs Attaques)
# ============================================================

# 1. Charger les bibliotheques necessaires
# Ce script utilise uniquement rpart et rpart.plot comme packages externes.
# caret est remplace par des fonctions de base R pour eviter ses nombreuses dependances.

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
fichier_csv <- "data032.csv"
if (!file.exists(fichier_csv)) {
  fichier_csv <- file.choose()
}
trafic <- read.csv(
  fichier_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

cat("\n========== DESCRIPTION DU JEU DE DONNEES CYBERSECURITE ==========\n")
cat("Dimensions initiales :", nrow(trafic), "lignes et", ncol(trafic), "colonnes\n")
cat("\nPremieres observations :\n")
print(head(trafic))
cat("\nStructure du jeu de donnees :\n")
str(trafic)

# 3. Selectionner et nettoyer les variables
colonnes <- c(
  "Flow.Duration",
  "Total.Fwd.Packets",
  "Flow.Bytes.s",
  "Packet.Length.Mean",
  "Label"
)

if (!all(colonnes %in% names(trafic))) {
  stop(paste(
    "Le CSV doit contenir les colonnes :",
    paste(colonnes, collapse = ", ")
  ))
}

donnees <- trafic[, colonnes, drop = FALSE]
donnees$Label <- as.factor(donnees$Label)
donnees$Flow.Duration <- suppressWarnings(as.numeric(donnees$Flow.Duration))
donnees$Total.Fwd.Packets <- suppressWarnings(as.numeric(donnees$Total.Fwd.Packets))
donnees$Flow.Bytes.s <- suppressWarnings(as.numeric(donnees$Flow.Bytes.s))
donnees$Packet.Length.Mean <- suppressWarnings(as.numeric(donnees$Packet.Length.Mean))

# Remplacer les valeurs infinies par NA, puis supprimer les lignes incompletes
variables_numeriques <- c(
  "Flow.Duration",
  "Total.Fwd.Packets",
  "Flow.Bytes.s",
  "Packet.Length.Mean"
)
for (variable in variables_numeriques) {
  donnees[[variable]][!is.finite(donnees[[variable]])] <- NA
}
donnees <- na.omit(donnees)
donnees$Label <- droplevels(donnees$Label)

if (nrow(donnees) < 2) {
  stop("Pas assez de lignes valides apres le nettoyage.")
}
if (nlevels(donnees$Label) < 2) {
  stop("La colonne Label doit contenir au moins deux classes apres nettoyage.")
}

cat("\nDimensions apres nettoyage :", nrow(donnees), "lignes et", ncol(donnees), "colonnes\n")
cat("Classes detectees :", paste(levels(donnees$Label), collapse = ", "), "\n")

# 4. Diviser les donnees en apprentissage (80 %) et test (20 %)
# Echantillonnage separe par classe, sans utiliser caret.
set.seed(123)

indices_par_classe <- split(seq_len(nrow(donnees)), donnees$Label)
indices_train <- unlist(
  lapply(indices_par_classe, function(indices) {
    n_train <- floor(0.80 * length(indices))
    # Pour une classe contenant au moins deux lignes, garder si possible
    # au moins une observation dans chaque ensemble.
    if (length(indices) >= 2) {
      n_train <- max(1, min(n_train, length(indices) - 1))
    } else {
      n_train <- length(indices)
    }
    sample(indices, size = n_train)
  }),
  use.names = FALSE
)

train <- donnees[indices_train, , drop = FALSE]
test <- donnees[-indices_train, , drop = FALSE]

if (nrow(test) == 0) {
  stop("Aucune observation dans le jeu de test. Ajoutez des donnees.")
}

cat("\nNombre d'observations d'apprentissage :", nrow(train), "\n")
cat("Nombre d'observations de test :", nrow(test), "\n")

# 5. Entrainement de l'arbre de classification
arbre_cyber <- rpart(
  Label ~ Flow.Duration + Total.Fwd.Packets + Flow.Bytes.s + Packet.Length.Mean,
  data = train,
  method = "class",
  control = rpart.control(cp = 0.01, minsplit = 4, maxdepth = 4)
)

# 6. Representation textuelle
cat("\n========== REPRESENTATION TEXTUELLE DE L'ARBRE ==========\n")
sortie_arbre <- capture.output(print(arbre_cyber))
sortie_complexite <- capture.output(printcp(arbre_cyber))
cat(paste(sortie_arbre, collapse = "\n"), "\n")
cat("\n========== PARAMETRES DE COMPLEXITE ==========\n")
cat(paste(sortie_complexite, collapse = "\n"), "\n")

texte_arbre <- c(
  "ARBRE DE CLASSIFICATION - DETECTION DES CYBERATTAQUES (CICIDS2017)",
  paste("Variables explicatives :", paste(variables_numeriques, collapse = ", ")),
  paste("Variable cible : Label (", paste(levels(donnees$Label), collapse = ", "), ")"),
  "",
  "Representation de l'arbre :",
  sortie_arbre,
  "",
  "Table des parametres de complexite :",
  sortie_complexite
)

# 7. Representation graphique
cat("\n========== REPRESENTATION GRAPHIQUE ==========\n")
rpart.plot(
  arbre_cyber,
  type = 4,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Arbre de classification - Detection d'intrusions IDS"
)

# 8. Sauvegarder les representations
writeLines(texte_arbre, "arbre_cybersecurity_texte.txt", useBytes = TRUE)

png(
  "arbre_cybersecurity.png",
  width = 1600,
  height = 1100,
  res = 150
)
rpart.plot(
  arbre_cyber,
  type = 4,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Arbre de classification - Detection d'intrusions IDS"
)
dev.off()

# 9. Evaluation sur le jeu de test
predictions <- predict(arbre_cyber, newdata = test, type = "class")
predictions <- factor(predictions, levels = levels(donnees$Label))
reels <- factor(test$Label, levels = levels(donnees$Label))
matrice_confusion <- table(Predit = predictions, Reel = reels)

cat("\n========== MATRICE DE CONFUSION ==========\n")
print(matrice_confusion)

# Calcul des metriques par classe en base R
classes <- levels(donnees$Label)
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

# Kappa de Cohen calcule sans caret
total <- sum(matrice_confusion)
accord_observe <- accuracy
accord_attendu <- if (total > 0) {
  sum(rowSums(matrice_confusion) * colSums(matrice_confusion)) / (total^2)
} else {
  NA_real_
}
kappa <- if (is.finite(accord_attendu) && (1 - accord_attendu) != 0) {
  (accord_observe - accord_attendu) / (1 - accord_attendu)
} else {
  NA_real_
}

precision_macro <- mean(metriques$Precision, na.rm = TRUE)
recall_macro <- mean(metriques$Recall, na.rm = TRUE)
f1_macro <- mean(metriques$F1_Score, na.rm = TRUE)

cat("\n========== METRIQUES PAR CLASSE ==========\n")
print(metriques)
cat("\nAccuracy :", round(accuracy, 4), "\n")
cat("Precision macro :", round(precision_macro, 4), "\n")
cat("Recall macro :", round(recall_macro, 4), "\n")
cat("F1-score macro :", round(f1_macro, 4), "\n")
cat("Kappa de Cohen :", round(kappa, 4), "\n")

# 10. Exporter les predictions et les indicateurs
resultats <- data.frame(
  Flow_Duration = test$Flow.Duration,
  Total_Fwd_Packets = test$Total.Fwd.Packets,
  Flow_Bytes_s = test$Flow.Bytes.s,
  Packet_Length_Mean = test$Packet.Length.Mean,
  Label_Reel = reels,
  Label_Predit = predictions
)
write.csv(
  resultats,
  "predictions_cybersecurity.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

indicateurs <- data.frame(
  Jeu_de_donnees = "CICIDS2017_Cybersecurity",
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
  "indicateurs_cybersecurity.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("\n========== FIN DU SCRIPT ==========\n")
cat("Fichiers sauvegardes :\n")
cat("- arbre_cybersecurity.png\n")
cat("- arbre_cybersecurity_texte.txt\n")
cat("- predictions_cybersecurity.csv\n")
cat("- indicateurs_cybersecurity.csv\n")

