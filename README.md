# Matchs_football_domicile
Analyse des Résultats de Matchs de Football  à domicile 

Ce projet explore les facteurs influençant les résultats des matchs de football à domicile en utilisant l'analyse de données et des modèles de régression logistique. L'objectif est d'examiner l'impact des "Expected Goals" (xG), de l'assistance au stade et d'autres variables contextuelles sur les performances des équipes.

📌 Fonctionnalités

Nettoyage et prétraitement des données

Création de variables dérivées (différence de xG, catégorisation de l'assistance, classification temporelle des matchs)

Visualisation des données avec ggplot2

Analyse des corrélations entre variables

Modélisation avec des régressions logistiques

Évaluation des modèles via matrices de confusion et courbes ROC

📂 Structure du projet

resultats_combines.xlsx : Données sources contenant les statistiques des matchs

script.R : Script principal effectuant le traitement et l'analyse des données

README.md : Ce fichier expliquant le projet

🔧 Installation et Dépendances

Ce projet nécessite R et les bibliothèques suivantes :

install.packages(c("dplyr", "readxl", "caret", "ggplot2", "pROC"))
--
🚀 Utilisation

Cloner le dépôt :

git clone https://github.com/votre-utilisateur/nom-du-repo.git
--
Ouvrir script.R dans RStudio ou un environnement R.

Exécuter le script pour nettoyer les données, analyser les résultats et entraîner les modèles.

📊 Résultats

Le projet compare deux modèles de régression logistique :

Modèle 1 : Basé sur Home_xG
--
Modèle 2 : Basé sur xG_Diff
--
Les performances des modèles sont évaluées à l'aide de matrices de confusion et d'AUC des courbes ROC.

📝 À améliorer

Ajouter d'autres variables contextuelles (ex. météo, fatigue des joueurs)

Tester des modèles plus avancés (Random Forest, XGBoost)

Comparer les performances sur différentes ligues
