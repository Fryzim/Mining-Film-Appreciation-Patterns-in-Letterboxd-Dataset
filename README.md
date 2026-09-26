# Mining Film Appreciation Patterns in the Letterboxd Dataset

Analyse en R d'un jeu de données de films Letterboxd, cherchant les facteurs associés aux films les mieux notés (genre, époque, origine géographique) et comparant plusieurs méthodes de classification pour prédire l'appréciation critique.

## Démarche

1. **Nettoyage** : filtrage des durées et années aberrantes, extraction du pays principal, comptage du nombre de genres/studios/acteurs par film.
2. **Analyse exploratoire** : distribution des notes, évolution par décennie (régression linéaire), notes moyennes par pays.
3. **Règles d'association** (`arules`, Apriori) : quelles combinaisons de genres sont sur-représentées parmi les films les mieux notés (facteur de croissance g(X) = support dans le top 25% / support global), avec vérification de la stabilité de ce facteur dans le temps.
4. **Clustering** : segmentation des films par profil (méthode du coude pour choisir k).
5. **Classification** : prédiction d'un film "bien noté" (top 25%) à partir de ses caractéristiques, comparant arbre de décision (`rpart`), k-NN et random forest, évalués par F1-score et validation croisée à 5 plis.

## Résultat principal

Le nombre de genres associés à un film et sa décennie de sortie sont des signaux plus informatifs que son pays d'origine pour prédire l'appréciation critique ; le random forest domine les autres classifieurs en F1-score sur la validation croisée.

## Contenu du dépôt

- `analysis.R` — script complet, exécutable de bout en bout
- `report.pdf` — rapport détaillant la méthodologie et l'interprétation des résultats

## Stack

R — `dplyr`, `tidyr`, `arules`, `rpart`, `class` (k-NN), `randomForest`
