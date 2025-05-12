# Analyse automatique du traitement médiatique des violences sexistes et sexuelles post MeToo

## Contexte

Ce projet s’inscrit dans une démarche d’analyse critique du traitement médiatique des violences sexistes et sexuelles (VSS) dans la presse française. Il repose sur les grilles d’évaluation qualitatives proposées par l’association **DécadréE**, notamment dans son rapport 2020, pour objectiver la manière dont les articles présentent les violences, les victimes, les agresseurs, les relations de pouvoir ou encore la contextualisation structurelle des faits.

L’objectif est de passer à l’échelle à l’aide d’outils de traitement automatique du langage, en combinant apprentissage supervisé et analyse statistique. Le corpus comprend plusieurs milliers d’articles issus de la presse nationale généraliste, publiés entre 2000 et 2023.

---

## Objectifs

1. Automatiser l’annotation d’un corpus large à partir de dimensions qualitatives inspirées de l’analyse critique des VSS dans les médias.
2. Étudier comment ces dimensions évoluent dans le temps, et notamment s’il existe une inflexion notable après le mouvement **#MeToo** (octobre 2017).
3. Fournir un outil reproductible pour évaluer des corpus similaires dans d’autres contextes ou périodes.

---

## Description des fichiers

### `downoald_and_cleaning_data.ipynb`

**Téléchargement et nettoyage du corpus**

Ce notebook constitue la première étape du pipeline :
- Téléchargement automatique d’un fichier CSV brut hébergé sur Google Drive ;
- Suppression des variables inutiles (date, heure, auteur, URL, etc.) ;
- Filtrage pour ne conserver que les articles publiés par les journaux généralistes suivants :
  *Le Monde, Le Figaro, Libération, L’Humanité, Le Point, Le Nouvel Obs* ;
- Sauvegarde dans un fichier `data_clean.csv` utilisé comme base pour les étapes suivantes.

---

### `training_theme_classification_post_pred.ipynb`

**Vectorisation des textes et préparation à la classification thématique**

Ce notebook sert à transformer les articles en représentations numériques (embeddings) pour des usages ultérieurs :
- Fusion du fichier nettoyé avec des annotations thématiques manuelles (`theme.csv`) ;
- Nettoyage textuel léger (mise en minuscule, suppression des valeurs manquantes) ;
- Création d’un champ `texte_total` combinant titre et contenu pour chaque article ;
- Génération de vecteurs de texte à l’aide du modèle pré-entraîné **CamemBERT** (sentence-level embeddings) ;
- Stockage des résultats dans un fichier `theme_token_embeddings.pkl` contenant les embeddings et leurs thèmes associés.

Ces représentations peuvent ensuite être utilisées pour :
- Visualiser des clusters thématiques,
- Entraîner un classifieur sur les thèmes,
- Réaliser des analyses exploratoires de similarité entre articles.

---

### `training_score.ipynb`

**Modélisation et prédiction du traitement médiatique**

Ce fichier contient l’entraînement du **modèle central du projet**, un modèle de **classification multitâche** basé sur CamemBERT.

#### Ce que fait le modèle :
- À partir du texte d’un article, il prédit **plusieurs dimensions d’analyse** correspondant aux critères qualitatifs du rapport DécadréE :
  - Par exemple : si le vocabulaire est approprié, si le comportement de la victime est légitimé, si des ressources sont mentionnées, etc.
- Chaque dimension est prédite comme une **classe ordinale** :  
  **-1** (traitement problématique), **0** (absence ou neutralité), **1** (traitement exemplaire).
- Une **note globale** est ensuite calculée pour chaque article en faisant **la moyenne des scores** sur toutes les dimensions.

#### Ce que contient le notebook :
- Préparation des données d’entraînement (fusion avec les scores manuels, nettoyage, split train/test) ;
- Définition du modèle multitâche :
  - Un backbone CamemBERT,
  - Une tête de classification produisant une prédiction pour chaque dimension ;
- Création d’une classe `Dataset` adaptée pour PyTorch ;
- Initialisation des `DataLoaders` pour l’entraînement et les tests.

Le modèle ainsi entraîné permet de produire une évaluation automatique de n’importe quel article, en restituant un score synthétique et un diagnostic détaillé par dimension.

---

### `Untitled.ipynb`

**Analyse de l’évolution temporelle : impact du mouvement #MeToo**

Ce notebook propose une première évaluation statistique de l’effet de #MeToo sur le traitement médiatique des violences sexistes et sexuelles :
- Création d’une variable temporelle `post_metoo` (avant/après octobre 2017) ;
- Séparation du corpus en deux sous-ensembles (pré- et post-MeToo) ;
- Calcul de la moyenne des scores (globaux et par dimension) pour chaque période ;
- Application de tests statistiques (t-tests) pour évaluer la significativité des écarts.

L’objectif est d’objectiver l’évolution de la qualité du traitement médiatique dans le temps et d’identifier si #MeToo a constitué une rupture dans les représentations véhiculées par la presse.

---

##


