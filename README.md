README pour l’Application d’Échantillonnage Shiny
================
Technologies Des SIG Appliqués
2025-08-11

- [Application d’Échantillonnage pour Études
  Statistiques](#application-déchantillonnage-pour-études-statistiques)
  - [🚀 Fonctionnalités](#rocket-fonctionnalités)
  - [🛠️ Installation et
    Utilisation](#hammer_and_wrench-installation-et-utilisation)
  - [📈 Formules des Méthodes
    d’Échantillonnage](#chart_with_upwards_trend-formules-des-méthodes-déchantillonnage)
    - [Méthode de Swartz & Aléatoire simple
      (Cochran)](#méthode-de-swartz--aléatoire-simple-cochran)
    - [Stratifié (allocation
      proportionnelle)](#stratifié-allocation-proportionnelle)
    - [Par Grappes (Cluster)](#par-grappes-cluster)
    - [Systématique](#systématique)
    - [Puissance Statistique](#puissance-statistique)
  - [✉️ Contact](#envelope-contact)

# Application d’Échantillonnage pour Études Statistiques

Cette application Shiny interactive simplifie le calcul de la taille
d’échantillon pour diverses méthodes statistiques et permet le tirage
aléatoire de points d’échantillonnage géospatialement. Elle est conçue
pour aider les chercheurs, statisticiens et analystes de données à
planifier leurs études de manière efficace et précise.

## 🚀 Fonctionnalités

- \[cite_start\]**Calcul de la Taille d’Échantillon :** Calcule la
  taille d’échantillon nécessaire en fonction de la taille de la
  population, du niveau de confiance, de la marge d’erreur, de la
  proportion attendue et d’un “buffer” (marge de sécurité)\[cite: 7, 8,
  9, 10, 11\].
- \[cite_start\]**Multiples Méthodes de Calcul :** Supporte plusieurs
  méthodes, incluant **Swartz**, **Aléatoire simple (Cochran)**,
  **Stratifié**, par **Grappes**, **Systématique** et **Puissance
  statistique**\[cite: 9, 10, 11\].
- \[cite_start\]**Tirage de Points Aléatoires :** Permet de charger une
  zone d’étude (KML/GPKG) et de générer des points d’échantillonnage
  aléatoires à l’intérieur de cette zone\[cite: 25, 26, 101, 102\].
- \[cite_start\]**Visualisation Cartographique :** Affiche la zone
  d’étude et les points d’échantillonnage générés sur une carte
  interactive (Leaflet)\[cite: 27, 97\].
- \[cite_start\]**Téléchargement des Résultats :** Exporte les résultats
  des calculs et les points d’échantillonnage générés au format
  Excel\[cite: 12, 26\].
- \[cite_start\]**Aide Détaillée sur les Méthodes :** Fournit des
  explications détaillées sur chaque méthode d’échantillonnage, incluant
  leurs formules, composants, forces, faiblesses et domaines
  d’application\[cite: 110, 115, 122, 129, 137, 141\].
- \[cite_start\]**Informations sur le Buffer :** Explique l’utilité et
  les valeurs typiques du “buffer” dans les calculs
  d’échantillonnage\[cite: 19, 21\].
- \[cite_start\]**À Propos du Concepteur :** Présente le développeur et
  les services offerts par “Technologies Des SIG Appliqués”\[cite: 28,
  29, 30\].

## 🛠️ Installation et Utilisation

Pour faire tourner cette application Shiny localement, vous devez avoir
**R** installé sur votre machine.

1.  **Installez R et RStudio** (RStudio est facultatif mais fortement
    recommandé pour le développement Shiny).

2.  **Clonez ce dépôt Git** ou téléchargez les fichiers de
    l’application.

3.  \[cite_start\]**Ouvrez le fichier `app.R`** dans RStudio\[cite: 1\].

4.  **Installez les packages R nécessaires** en exécutant le code
    suivant dans la console R de RStudio :

    ``` r
    install.packages(c("shiny", "shinydashboard", "leaflet", "sf", "openxlsx", "leaflet.extras", "ggplot2"))
    ```

5.  **Exécutez l’application** en cliquant sur le bouton “Run App” dans
    RStudio, ou en exécutant la commande suivante dans la console R :

    ``` r
    shiny::runApp()
    ```

## 📈 Formules des Méthodes d’Échantillonnage

### Méthode de Swartz & Aléatoire simple (Cochran)

Ces méthodes utilisent la même formule pour les calculs de base. La
taille d’échantillon initiale est calculée comme suit :
$$ n_0 = \frac{Z^2 p(1-p)}{e^2} $$ Ensuite, une correction pour la
population finie est appliquée :
$$ n = \frac{n_0}{1 + \frac{n_0-1}{N}} $$ \* \[cite_start\]**Z** : Score
Z correspondant au niveau de confiance (ex : 1.96 pour 95%)\[cite:
110\]. \* \[cite_start\]**p** : Proportion estimée dans la population
(0.5 par défaut si inconnue)\[cite: 111, 117\]. \* \[cite_start\]**e** :
Marge d’erreur acceptable (ex : 0.05 pour 5%)\[cite: 111\]. \*
\[cite_start\]**N** : Taille de la population\[cite: 111\]. \*
\[cite_start\]**n_0** : Taille d’échantillon initiale sans
correction\[cite: 111\]. \* \[cite_start\]**n** : Taille d’échantillon
finale avec correction\[cite: 112\].

### Stratifié (allocation proportionnelle)

La taille d’échantillon pour chaque strate est calculée de manière
proportionnelle à sa taille dans la population totale :
$$ n_h = n \times \frac{N_h}{N} $$ \* \[cite_start\]**n_h** : Taille
d’échantillon pour la strate h\[cite: 124\]. \* \[cite_start\]**n** :
Taille d’échantillon totale\[cite: 124\]. \* \[cite_start\]**N_h** :
Taille de la population dans la strate h\[cite: 124\]. \*
\[cite_start\]**N** : Taille totale de la population\[cite: 124\].

### Par Grappes (Cluster)

Cette méthode ajuste la taille d’échantillon en utilisant l’effet de
plan (DEFF) : $$ n_{cluster} = n \times DEFF $$ Le DEFF est généralement
un facteur entre 1.5 et 2.0. $$ DEFF = 1 + (m-1)\rho $$ \*
\[cite_start\]**n\_{cluster}** : Taille d’échantillon ajustée pour les
grappes\[cite: 132\]. \* \[cite_start\]**n** : Taille d’échantillon
calculée par une méthode standard\[cite: 132\]. \* \[cite_start\]**m** :
Taille moyenne des grappes\[cite: 133\]. \* \[cite_start\]**ρ** (rho) :
Coefficient de corrélation intra-classe\[cite: 133\].

### Systématique

L’intervalle de sélection (pas d’échantillonnage) est calculé comme suit
: $$ k = \frac{N}{n} $$ \* \[cite_start\]**k** : Intervalle de
sélection\[cite: 137\]. \* \[cite_start\]**N** : Taille de la
population\[cite: 138\]. \* \[cite_start\]**n** : Taille d’échantillon
souhaitée\[cite: 138\].

### Puissance Statistique

Cette méthode est utilisée pour les tests d’hypothèse et les
comparaisons de groupes :
$$ n = \left(\frac{Z_{\alpha/2} + Z_{\beta}}{\delta}\right)^2 p(1-p) $$
\* \[cite_start\]**Z<sub>α/2</sub>** : Score Z pour l’erreur de type I
(généralement 1.96)\[cite: 142\]. \* \[cite_start\]**Z<sub>β</sub>** :
Score Z pour l’erreur de type II (généralement 0.84 pour 80% de
puissance)\[cite: 143\]. \* \[cite_start\]**δ** (delta) : Taille d’effet
minimale détectable\[cite: 143\]. \* \[cite_start\]**p** : Proportion de
référence\[cite: 143\]. \* \[cite_start\]**n** : Taille d’échantillon
requise\[cite: 143\].

## ✉️ Contact

- **Email :** <tech.sig226@gmail.com>
- **Téléphone :** +226 71837672
- **Réseaux Sociaux :**
  - \[cite_start\]\[Facebook\](<https://www.facebook.com/profile.php?id=100082958875395>)
    \[cite: 75\]
  - \[cite_start\]\[WhatsApp\](<https://wa.me/message/7P4NBHPHQHA5D1>)
    \[cite: 76\]
  - \[cite_start\]\[Telegram\](<https://t.me/+-yyMUU4sp8k1Yjg0>) \[cite:
    77\]
  - \[cite_start\]\[YouTube\](<https://youtube.com/@techsig226?si=cnIQA5Xt5vbbP9C8>)
    \[cite: 77\]
  - \[cite_start\]\[LinkedIn\](<https://www.linkedin.com/in/fa%C3%AFsale-damiba-61b555154/>)
    \[cite: 78\]
