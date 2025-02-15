install.packages("dplyr")
library(readxl)
library(dplyr)
library(caret)
library(ggplot2)

# ajouter les ocrrelations de pearson sur les xg
# Vider la mémoire

gc()
rm(list = ls())

# Charger les données
resultats_combines <- read_excel("économetrie appliquée/resultats_combines.xlsx")
df <- resultats_combines

# Convertir la colonne "Time" en format horaire
df$Time <- as.POSIXct(df$Time, format = "%H:%M", tz = "UTC")
if (any(is.na(df$Time))) {
  stop("La colonne 'Time' contient des valeurs invalides ou manquantes.")
}

# Classifier le temps (matin, après-midi, soir)
df$Time_of_Day <- ifelse(format(df$Time, "%H") >= 6 & format(df$Time, "%H") < 12, "Matin",
                         ifelse(format(df$Time, "%H") >= 12 & format(df$Time, "%H") < 18, "Après-midi", "Soir"))
df$MatchResult <- ifelse(df$HomeGoals > df$AwayGoals, 1, 0)

# Gérer les valeurs manquantes
df$AwayGoals[is.na(df$AwayGoals)] <- mean(df$AwayGoals, na.rm = TRUE)
df$HomeGoals[is.na(df$HomeGoals)] <- mean(df$HomeGoals, na.rm = TRUE)
df$Home_xG[is.na(df$Home_xG)] <- median(df$Home_xG, na.rm = TRUE)
df$Away_xG[is.na(df$Away_xG)] <- mean(df$Away_xG, na.rm = TRUE)
df$Attendance[is.na(df$Attendance)] <- mean(df$Attendance, na.rm = TRUE)

# Créer une nouvelle variable pour la différence de xG
df$xG_Diff <- df$Home_xG - df$Away_xG

# Catégorisation de l'Attendance en 3 groupes (faible, moyen, élevé)
df$Attendance_Category <- cut(df$Attendance, 
                              breaks = quantile(df$Attendance, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE), 
                              labels = c("Faible", "Moyen", "Élevé"),
                              include.lowest = TRUE)
#----------------------------------------------------------------------

### Graph 
##### ATTENDANCE PAR PAYS 
install.packages("ggplot2")
library(ggplot2)
#frequentation des stades par pays moyenne 

ggplot(df, aes(x = reorder(Country, Attendance, FUN = mean), y = Attendance, fill = Country)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.7) +
  labs(title = "Moyenne de l'Attendance par Pays", x = "Pays", y = "Moyenne de Spectateurs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

colnames(df)


## frequentation des stades par pays mediane 
library(ggplot2)

ggplot(df, aes(x = reorder(Country, Attendance, median), y = Attendance, fill = Country)) +
  geom_boxplot() +
  theme_minimal() +
  coord_flip() +  
  labs(title = "Mediane de l'Attendance par Pays",
       x = "Pays",
       y = "Nombre de spectateurs") +
  theme(legend.position = "none")


# evolution de la moyenne de l'attendnace par saisons 

ggplot(df, aes(x = Season_End_Year, y = Attendance)) +
  geom_line(stat = "summary", fun = mean, color = "blue", size = 1) +
  geom_point(stat = "summary", fun = mean, color = "red", size = 2) +
  theme_minimal() +
  labs(title = "Évolution de l'Attendance par Saison",
       x = "Année de fin de saison",
       y = "Nombre moyen de spectateurs")

ggplot(df, aes(x = Attendance, y = Home_xG)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Corrélation entre HOME_XG et Attendance",
       x = "Nombre de spectateurs",
       y = "Expected Goals  de l'équipe à domicile")
ggplot(df, aes(x = Attendance, y = Away_xG)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Corrélation entre AWAY_XG et Attendance",
       x = "Nombre de spectateurs",
       y = "Expected Goals  de l'équipe extérieur")
#relation xg et resultat du match
ggplot(df, aes(x = as.factor(MatchResult), y = xG_Diff, fill = as.factor(MatchResult))) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Différence de l'éstimation du nombre de buts des deux equipes  selon le résultat du match",
       x = "Résultat (0 = Défaite, 1 = Victoire)",
       y = "Différence de Expected Goals")
ggplot(df, aes(x = Home_xG, y = Away_xG)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Corrélation entre AWAY_XG et Home_XG",
       x = "Nombre de spectateurs",
       y = "Expected Goals  de l'équipe extérieur") # ces var sont correlées donc on ne choisi pas les deux pour le modele 


library(ggplot2)
ggplot(df, aes(x = Time_of_Day, fill = Country)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Répartition des matchs par Heure et Pays",
       x = "Moment de la journée",
       y = "Nombre de matchs",
       fill = "Pays") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df, aes(x = Season_End_Year, y = xG_Diff, color = Country, group = Country)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  theme_minimal() +
  labs(title = "Évolution du Niveau des Équipes par Pays",
       x = "Année de Fin de Saison",
       y = "Niveau Moyen (xG_Diff)",
       color = "Pays") +
  theme(legend.position = "right")
#----------------------------------------------------------------------






library(dplyr)
library(caret)
# ---- Modèle 1 : Régression logistique avec Home_xG ----
selected_columns1 <- c("MatchResult", "Gender", "Home_xG", "Time_of_Day", "Attendance_Category")
data1 <- df %>% select(all_of(selected_columns1))

# Division des données en ensembles d'entraînement et de test
set.seed(123)
trainIndex1 <- createDataPartition(data1$MatchResult, p = 0.8, list = FALSE)
data_train1 <- data1[trainIndex1, ]
data_test1 <- data1[-trainIndex1, ]

# Modélisation
logistic_model1 <- glm(MatchResult ~ ., data = data_train1, family = binomial)
summary(logistic_model1)

# Prédictions
predictions1 <- predict(logistic_model1, newdata = data_test1, type = "response")
predictions_class1 <- ifelse(predictions1 > 0.5, 1, 0)
conf_matrix1 <- confusionMatrix(as.factor(predictions_class1), as.factor(data_test1$MatchResult))
print(conf_matrix1)

# ---- Modèle 2 : Régression logistique avec xG_Diff ----
selected_columns2 <- c("MatchResult", "Gender", "xG_Diff", "Time_of_Day", "Attendance_Category")
data2 <- df %>% select(all_of(selected_columns2))

trainIndex2 <- createDataPartition(data2$MatchResult, p = 0.8, list = FALSE)
data_train2 <- data2[trainIndex2, ]
data_test2 <- data2[-trainIndex2, ]

logistic_model2 <- glm(MatchResult ~ ., data = data_train2, family = binomial)
summary(logistic_model2)

predictions2 <- predict(logistic_model2, newdata = data_test2, type = "response")
predictions_class2 <- ifelse(predictions2 > 0.5, 1, 0)
conf_matrix2 <- confusionMatrix(as.factor(predictions_class2), as.factor(data_test2$MatchResult))
print(conf_matrix2)

# Comparaison des Odds Ratios
odds_ratios1 <- exp(coef(logistic_model1))
odds_ratios2 <- exp(coef(logistic_model2))

print("Odds Ratios - Modèle avec Home_xG:")
print(odds_ratios1)

print("Odds Ratios - Modèle avec xG_Diff:")
print(odds_ratios2)



install.packages("pROC")
library(pROC)

# Vérifier si les données existent
print(head(data_test1$MatchResult))
print(head(predictions1))

# Calcul des probabilités de prédiction
roc_curve1 <- roc(data_test1$MatchResult, predictions1)

# Tracer la courbe ROC avec des personnalisations
plot(roc_curve1, main = "Courbe ROC - Modèle 1 (Home_xG)", col = "red", lwd = 5, 
     cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)
abline(a = 0, b = 1, col = "green", lty = 2)  # Ligne de référence
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve1), 2)), col = "red", lwd = 2)




# Vérifier si les données existent
print(head(data_test2$MatchResult))
print(head(predictions2))

# Calcul des probabilités de prédiction
roc_curve2 <- roc(data_test2$MatchResult, predictions2)

# Tracer la courbe ROC avec des personnalisations
plot(roc_curve2, main = "Courbe ROC - Modèle 2 (Home_xG)", col = "red", lwd = 5, 
     cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.2)
abline(a = 0, b = 1, col = "green", lty = 2)  # Ligne de référence
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve1), 2)), col = "red", lwd = 2)


ggplot(df, aes(x = Season_End_Year, y = xG_Diff, color = Country, group = Country)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  theme_minimal() +
  labs(title = "Évolution du Niveau des Équipes par Pays",
       x = "Année de Fin de Saison",
       y = "Niveau Moyen (xG_Diff)",
       color = "Pays") +
  theme(legend.position = "right")

