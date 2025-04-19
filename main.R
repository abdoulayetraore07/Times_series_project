# 📌 Chargement des bibliothèques nécessaires
library(ggplot2)    # 📊 Visualisation des données
library(tseries)    # 📉 Tests de stationnarité (Dickey-Fuller)
library(forecast)   # 🔮 Modèles ARIMA et lissage exponentiel
library(lubridate)  # 📆 Manipulation des dates
library(dplyr)      # 🔍 Manipulation des données

# Chargement du fichier CSV contenant les données financières
financial_data <- read.csv("financial_regression.csv", stringsAsFactors = FALSE)

# Conversion de la colonne `date` en format Date 
financial_data$date <- as.Date(financial_data$date, format="%Y-%m-%d")

# Sélection des colonnes 
financial_data <- data.frame(
  date = financial_data$date,
  gold_open = financial_data$gold.open,
  gold_high = financial_data$gold.high,
  gold_low = financial_data$gold.low,
  gold_close = financial_data$gold.close,
  gold_volume = financial_data$gold.volume
)

# Suppression des valeurs manquantes pour éviter des erreurs dans les calculs
str(financial_data)   # Structure du dataset avant suppression
financial_data <- na.omit(financial_data)

# Vérification rapide des données
head(financial_data)  # Voir les premières lignes du dataset
str(financial_data)   # Structure du dataset après suppression
summary(financial_data)  # Statistiques descriptives générales


# Visualisation de l'évolution du prix de clôture de l'or dans le temps
ggplot(financial_data, aes(x = date, y = gold_close)) +
  geom_line(color = "blue") +
  ggtitle("            Évolution du prix de clôture du Gold") +
  xlab("Date") + ylab("Prix de clôture") +
  theme_minimal()

# Histogramme de la distribution des prix de clôture pour voir leur répartition
hist(financial_data$gold_close, 
     main="Distribution du prix de clôture du Gold", 
     col="lightblue", breaks=30)

# Boxplot pour détecter d'éventuelles valeurs aberrantes
boxplot(financial_data$gold_close, 
        main="Boxplot du prix de clôture du Gold", 
        col="lightblue")

# Moyenne mobile pour lisser la série temporelle et observer la tendance
financial_data$gold_ma30 <- rollmean(financial_data$gold_close, k = 30, fill = NA)

ggplot(financial_data, aes(x = date)) +
  geom_line(aes(y = gold_close), color = "blue", alpha=0.5) +  # Prix réel
  geom_line(aes(y = gold_ma30), color = "red", size=1) +       # Moyenne mobile 30 jours
  ggtitle("          Moyenne mobile (30 jours) du prix de l'or") +
  xlab("Date") + ylab("Prix de clôture")


# Calcul du taux de variation journalier du prix de l'or
financial_data$gold_return <- c(NA, diff(financial_data$gold_close) / head(financial_data$gold_close, -1))

ggplot(financial_data, aes(x = date, y = gold_return)) +
  geom_line(color = "blue",size=0.5) +
  ggtitle("               Variation journalière du prix de l'or") +
  xlab("Date") + ylab("Taux de variation (%)") +
  theme_minimal()


# Volatilité sur 30 jours (moyenne mobile de l'écart-type)
financial_data$volatility_30 <- rollapply(financial_data$gold_return, 
                                          width = 30, FUN = sd, fill = NA)
ggplot(financial_data, aes(x = date, y = volatility_30)) +
  geom_line(color = "red",size=0.3) +
  ggtitle("Volatilité du prix de l'or (écart-type sur 30 jours)") +
  xlab("Date") + ylab("Volatilité")


# Extraire l'année et le mois
financial_data$month <- format(financial_data$date, "%m")  

monthly_avg <- financial_data %>%
  group_by(month) %>%
  summarise(mean_price = mean(gold_close, na.rm = TRUE))

# Remplacer les numéros de mois par les noms des mois
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

ggplot(monthly_avg, aes(x = factor(month, levels = sprintf("%02d", 1:12), labels = month_names), 
                        y = mean_price, group=1)) +
  geom_line(color = "purple", size = 0.7) +
  ggtitle("Évolution du prix moyen mensuel de l'or") +
  xlab("Mois") + ylab("Prix moyen de clôture") +
  theme_minimal()


# Décomposition de la série temporelle pour analyser la tendance et la saisonnalité
gold_ts <- ts(financial_data$gold_close, frequency = 252)  # 252 jours de trading/an
decomposed_gold <- decompose(gold_ts)
plot(decomposed_gold)  # Affiche les composantes : tendance, saisonnalité et résidu


# Test de Dickey-Fuller pour vérifier si la série est stationnaire
adf_test <- adf.test(na.omit(financial_data$gold_close))
print(adf_test) 

# Différenciation pour rendre la série stationnaire
financial_data$gold_close_diff <- c(NA, diff(financial_data$gold_close))

# Nouveau test Dickey-Fuller sur la série différenciée
adf_test_diff <- adf.test(na.omit(financial_data$gold_close_diff))
print(adf_test_diff)  


# Filtrer les données de 2024
financial_data_2024 <- financial_data[format(financial_data$date, "%Y") == "2024", ]

# Convertir en série temporelle (avec fréquence 17 observations/mois)
ts_2024 <- ts(financial_data_2024$gold_close, frequency=17)
decomposed_ts_2024<-decompose(ts_2024)
plot(decomposed_ts_2024)

# Déterminer la taille de l'échantillon d'entraînement (90% des données)
train_size <- 195  # 90% des données
train_ts <- ts_2024[1:train_size]  # Partie entraînement
train_ts <- ts(train_ts,frequency=17)
test_ts <- ts_2024[(train_size+1):length(ts_2024)]  # Partie test


# LISSAGE HOLT-WINTERS sur les 90% d'entraînement
hw_train <- HoltWinters(train_ts)

# PRÉVISION SUR LES 10% RESTANTS
hw_forecast <- forecast(hw_train, h=length(test_ts))  


# VISUALISATION DES RÉSULTATS
# Affichage des données réelles
plot(ts_2024, type="l", col="black", lwd=1.5, main="Holt-Winters : Entraînement vs. Test en 2024")

# Lissage Holt-Winters sur l'entraînement
lines(hw_train$fitted[,1], col="red", lwd=1.5, lty=1)

# Affichage des prévisions Holt-Winters
lines(time(ts_2024)[(train_size+1):(train_size+length(hw_forecast$mean))], 
      hw_forecast$mean, col="blue", lwd=1.5, lty=1)

# Affichage des vraies valeurs du test
lines(time(ts_2024)[(train_size+1):(train_size+length(test_ts))], 
      test_ts, col="green", lwd=1.5, lty=1)

# Ajouter une légende
legend("topleft", legend=c("Données réelles", "Lissage Holt-Winters", "Prévisions Holt-Winters", "Données Test","MAE : 6.7328","RMSE : 7.589"),
       col=c("black", "red", "blue", "green","white", "white"), lty=c(1,1,2,1), lwd=c(1.5,1.5,1.5,1.5))



# Calcul de l'erreur (MAE et RMSE)
mae <- mean(abs(hw_forecast$mean - test_ts))  # Erreur absolue moyenne
rmse <- sqrt(mean((hw_forecast$mean - test_ts)^2))  # Erreur quadratique moyenne

# Afficher les erreurs
print(paste("MAE (Mean Absolute Error):", round(mae, 4)))
print(paste("RMSE (Root Mean Squared Error):", round(rmse, 4)))

