# ============================================================
# Analyse de la consommation electrique
# Fichier de donnees : consommation_electrique_100_lignes.csv
# ============================================================

# 1. Importer les donnees
consommation <- read.csv(
  "consommation_electrique_100_lignes.csv",
  stringsAsFactors = FALSE
)

# 2. Explorer les donnees
head(consommation)
str(consommation)
summary(consommation)

# 3. Verifier les donnees manquantes
colSums(is.na(consommation))

# 4. Calculer la consommation totale et moyenne
consommation_totale <- sum(consommation$consommation_kwh, na.rm = TRUE)
consommation_moyenne <- mean(consommation$consommation_kwh, na.rm = TRUE)

cat("Consommation totale :", round(consommation_totale, 2), "kWh\n")
cat("Consommation moyenne par jour :", round(consommation_moyenne, 2), "kWh\n")

# 5. Calculer le cout total estime
cout_total <- sum(consommation$cout_estime_dzd, na.rm = TRUE)
cat("Cout total estime :", round(cout_total, 2), "DZD\n")

# 6. Comparer la consommation selon le type de jour
bilan_jours <- aggregate(
  consommation_kwh ~ type_jour,
  data = consommation,
  FUN = mean
)
bilan_jours$consommation_kwh <- round(bilan_jours$consommation_kwh, 2)
print(bilan_jours)

# 7. Calculer la consommation moyenne par mois
consommation$date <- as.Date(consommation$date)
consommation$mois <- format(consommation$date, "%Y-%m")

bilan_mois <- aggregate(
  consommation_kwh ~ mois,
  data = consommation,
  FUN = mean
)
bilan_mois$consommation_kwh <- round(bilan_mois$consommation_kwh, 2)
print(bilan_mois)

# 8. Graphique de l'evolution journaliere
plot(
  consommation$date,
  consommation$consommation_kwh,
  type = "l",
  col = "#157A6E",
  lwd = 2,
  main = "Evolution de la consommation electrique",
  xlab = "Date",
  ylab = "Consommation (kWh)"
)

# 9. Graphique de la consommation moyenne par type de jour
barplot(
  bilan_jours$consommation_kwh,
  names.arg = bilan_jours$type_jour,
  col = c("#142238", "#C4A35A"),
  main = "Consommation moyenne selon le type de jour",
  xlab = "Type de jour",
  ylab = "Consommation moyenne (kWh)"
)

# 10. Histogramme de la consommation electrique
hist(
  consommation$consommation_kwh,
  breaks = 10,
  col = "#157A6E",
  border = "white",
  main = "Distribution de la consommation electrique",
  xlab = "Consommation journaliere (kWh)",
  ylab = "Frequence"
)


# 11. Afficher la journee de consommation maximale
indice_max <- which.max(consommation$consommation_kwh)
jour_max <- consommation[indice_max, ]
print(jour_max)

# 12. Enregistrer les tableaux de synthese
write.csv(bilan_jours, "bilan_consommation_par_type_jour.csv", row.names = FALSE)
write.csv(bilan_mois, "bilan_consommation_par_mois.csv", row.names = FALSE)
