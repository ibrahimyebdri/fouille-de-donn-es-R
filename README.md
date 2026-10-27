# TP1 — Fouille de données avec le langage R

## Présentation

Ce projet a été réalisé dans le cadre du TP de **Fouille de données avec le langage R** du Master Systèmes d'Information et Données (SID) à l'Université des Sciences et de la Technologie d'Oran — Mohamed Boudiaf (USTO-MB).

L'objectif est de mettre en pratique les principales étapes d'une démarche de fouille de données : exploration, préparation, traitement des valeurs manquantes, construction de modèles, visualisation et interprétation des résultats.

## Objectifs

- Découvrir les principales fonctionnalités du langage R pour la fouille de données.
- Importer et explorer des jeux de données au format CSV.
- Examiner les structures et les statistiques descriptives.
- Détecter et traiter les valeurs manquantes.
- Construire des arbres de régression et de classification.
- Afficher les représentations graphiques et textuelles des arbres.
- Évaluer et interpréter les résultats des modèles.

## Contenu du projet

### 1. Analyse de la consommation électrique

Un exemple simple étudie la consommation électrique journalière d'un établissement, par exemple une entreprise.

Cette partie comprend :
- l'importation et l'exploration des données ;
- le calcul de statistiques descriptives ;
- la création d'un histogramme de la consommation journalière ;
- l'interprétation de la distribution des consommations.

### 2. Accessibilité économique du logement

Un arbre de régression est construit à partir d'un jeu de données couvrant 26 pays sur la période 1975–2025.

**Variable cible :** `affordability_proxy`

**Variables explicatives :** `country` et `year`

Le modèle permet d'examiner les différences de l'indice d'accessibilité selon les pays et les années. Le projet présente la représentation graphique de l'arbre, sa représentation textuelle et l'interprétation des règles de séparation.

### 3. Classification du jeu de données Dengue

Un arbre de classification binaire est utilisé pour prédire la variable cible `NoYes`, codée 0 ou 1.

Les variables explicatives comprennent notamment des mesures environnementales et des coordonnées géographiques.

Cette partie comprend le nettoyage des données, la séparation en ensembles d'apprentissage et de test, la construction de l'arbre, la matrice de confusion et les indicateurs de classification.

### 4. Détection d'intrusions réseau — IDS

Un arbre de classification est utilisé pour analyser des flux réseau et distinguer six catégories :

- Benign
- Bot
- DDoS
- DoS Hulk
- PortScan
- Web Attack

L'analyse comprend la préparation des données, la construction de l'arbre, la représentation graphique et textuelle, ainsi que l'évaluation sur un jeu de test.

Une attention particulière est portée au rappel par classe, afin d'identifier les catégories d'attaques que le modèle ne détecte pas correctement.

## Technologies et outils

- **Langage :** R
- **Modélisation :** `rpart`
- **Visualisation des arbres :** `rpart.plot`
- **Format des données :** CSV
- **Environnement de travail :** Ubuntu et R

## Exécution

1. Installer R sur son système.
2. Installer les bibliothèques nécessaires :

   ```r
   install.packages("rpart")
   install.packages("rpart.plot")
   ```

3. Télécharger ou cloner le dépôt GitHub.
4. Placer chaque script R dans le même dossier que le fichier CSV correspondant.
5. Ouvrir R dans le dossier du projet et exécuter le script souhaité.

   Exemple :

   ```r
   source("arbre_dengue.R")
   ```

   Adapter le nom du script à l'exemple que l'on souhaite exécuter.

Certains scripts peuvent demander de sélectionner le fichier CSV lorsque celui-ci n'est pas trouvé dans le répertoire courant.

## Sources et références

### Accessibilité économique du logement

Jeu de données : *Global Housing Affordibility Dashboard 1975–2025*.

Source :
https://github.com/imk24/Global-Housing-Affordibility-Dashboard-1975---2025-

### Dengue

Les références associées au jeu utilisé comprennent le package R **DAAG** et la publication suivante :

Hales, S., de Wet, N., Maindonald, J., & Woodward, A. (2002). *Potential effect of population and climate changes on global distribution of dengue fever: an empirical model*. The Lancet, 360(9336), 830–834.

DOI : https://doi.org/10.1016/S0140-6736(02)09964-6

Package DAAG :
https://CRAN.R-project.org/package=DAAG

### Cybersécurité — CICIDS2017

**Dataset Benchmark Source:** Canadian Institute for Cybersecurity, University of New Brunswick. *CICIDS2017 Dataset*.

Source officielle :
https://www.unb.ca/cic/datasets/ids-2017.html

**Reference Paper / Benchmark:** Sharafaldin, A., Lashkari, A. H., & Ghorbani, A. A. (2018). *Toward Generating a New Intrusion Detection Dataset and Intrusion Traffic Characterization*. 4th International Conference on Information Systems Security and Privacy (ICISSP).

DOI : https://doi.org/10.5220/0006639801080116

*Remarque : les fichiers CSV locaux utilisés dans les exercices sont ceux fournis pour le projet. Leur correspondance exacte avec les versions originales des sources doit être vérifiée séparément.*

## Auteurs

- YEBDRI Ibrahim
- BENAMAR Anis Chakib
- Nacer Soufiane

**Groupe 1**

## Contexte académique

Université des Sciences et de la Technologie d'Oran — Mohamed Boudiaf (USTO-MB)  
Master Systèmes d'Information et Données (SID)  
Année universitaire 2026–2027
