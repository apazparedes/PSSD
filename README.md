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
Pour reproduire l’ensemble du traitement dans le bon ordre, il suffit d’exécuter le fichier `main.ipynb`, qui appelle automatiquement les autres notebooks étape par étape.
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

Ce notebook a pour objectif de **préparer les données nécessaires à la classification des articles par rubrique journalistique**. L’objectif final est de pouvoir **isoler automatiquement les articles d’actualité**, en les distinguant des tribunes, analyses, critiques, ou autres formats éditoriaux.

#### Étapes réalisées :
- Fusion du corpus nettoyé avec des **annotations manuelles de rubrique** (`theme.csv`) ;
- Nettoyage textuel de base (minuscule, suppression des valeurs manquantes) ;
- Création d’un champ `texte_total` combinant le titre et le contenu de chaque article ;
- Génération de **vecteurs de représentation** (embeddings) à l’aide du modèle pré-entraîné **CamemBERT**, appliqué sur chaque texte complet ;
- Stockage des embeddings et de leur rubrique associée dans un fichier `theme_token_embeddings.pkl`.

#### Utilité des représentations produites :
- Entraîner un **modèle de classification thématique** pour prédire automatiquement la rubrique d’un article ;
- Identifier et **extraire les seuls articles d’actualité**, en excluant les formats non pertinents pour l’analyse des faits divers (tribunes, éditoriaux, etc.) ;
- Faciliter des analyses ultérieures sur les différences de traitement selon les rubriques ou les types de contenu.

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

### `pre_post_analysis.ipynb` 

Ce notebook propose une double approche statistique pour évaluer l’impact du mouvement **#MeToo** (octobre 2017) sur le traitement médiatique des violences sexistes et sexuelles.

---

### 1. Analyse comparative (pré-/post-2017)
- Création d’une variable binaire `post_metoo` distinguant les articles publiés **avant** et **après** octobre 2017 ;
- Calcul des **scores moyens** (globaux et par dimension) pour chaque période ;
- Réalisation de **tests t** pour évaluer la significativité des différences observées ;
- Résultats présentés sous forme de **tableaux comparatifs** et **graphiques**.

---

### 2. Régressions temporelles
- Estimation de modèles de régression linéaire (OLS) pour capturer l’évolution du score de traitement dans le temps ;
- Formule utilisée :

  ```python
  model = smf.ols("score_final ~ C(annee, Treatment(reference=2017))", 
                  data=articles).fit(cov_type='cluster', cov_kwds={'groups': articles['journal_clean']})

---

##


