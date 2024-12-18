# =========================== Étape 1 : Nettoyage du dataset ===========================

# Importer le fichier CSV
mydata <- read.csv("/Users/andreicosmin/Andrei/ICHEC-ECAM/MA2/IA/Projet Exam/Online Retail 3.csv", 
                   sep = ";", header = TRUE, stringsAsFactors = FALSE)

# ----------------- Conversion et nettoyage des colonnes -----------------

# Convertir UnitPrice en numérique après avoir remplacé les virgules par des points
mydata$UnitPrice <- as.numeric(gsub(",", ".", mydata$UnitPrice))

# Supprimer les lignes où UnitPrice ou Quantity ne sont pas valides (NA ou <= 0)
mydata <- mydata[!is.na(mydata$UnitPrice) & mydata$UnitPrice > 0, ]
mydata <- mydata[mydata$Quantity > 0, ]

# Convertir la colonne InvoiceDate en format Date/Time
mydata$InvoiceDate <- as.POSIXct(mydata$InvoiceDate, format = "%d/%m/%Y %H:%M")

# ----------------- Suppression des doublons -----------------
mydata <- mydata[!duplicated(mydata), ]

# ----------------- Gestion des valeurs manquantes -----------------
mydata <- na.omit(mydata)

# =========================== Étape 2 : Création des nouvelles variables ===========================

library(dplyr)
library(lubridate)

mydata <- mydata %>%
  group_by(InvoiceNo) %>%
  mutate(
    # Variables monétaires
    TotalAmount = Quantity * UnitPrice,  # Montant total par ligne
    ItemsPerTransaction = n(),           # Nombre d'items différents par transaction
    
    # Variables temporelles
    Year = year(InvoiceDate),
    Month = month(InvoiceDate),
    DayOfMonth = day(InvoiceDate),
    DayOfWeek = wday(InvoiceDate),      # 1 = Dimanche, 7 = Samedi
    Hour = hour(InvoiceDate),
    
    # Métriques par transaction
    TransactionTotal = sum(Quantity * UnitPrice),  # Montant total de la transaction
    QuantityPerTransaction = sum(Quantity),        # Quantité totale par transaction
    AverageUnitPrice = mean(UnitPrice)            # Prix unitaire moyen par transaction
  ) %>%
  ungroup() %>%
  group_by(CustomerID) %>%
  mutate(
    # Métriques par client
    CustomerTotalPurchases = n(),                  # Nombre total d'achats par client
    CustomerTotalAmount = sum(TotalAmount),        # Montant total dépensé par client
    CustomerAverageAmount = mean(TotalAmount)      # Montant moyen par achat pour chaque client
  ) %>%
  ungroup()

# Ajouter des catégories temporelles
mydata <- mydata %>%
  mutate(
    TimeOfDay = case_when(
      Hour >= 5 & Hour < 12 ~ "Matin",
      Hour >= 12 & Hour < 17 ~ "Après-midi",
      Hour >= 17 & Hour < 22 ~ "Soir",
      TRUE ~ "Nuit"
    ),
    
    WeekPart = case_when(
      DayOfWeek %in% c(1, 7) ~ "Weekend",
      TRUE ~ "Semaine"
    ),
    
    # Catégorisation des montants
    TransactionSizeCategory = case_when(
      TransactionTotal < quantile(TransactionTotal, 0.33) ~ "Petit",
      TransactionTotal < quantile(TransactionTotal, 0.66) ~ "Moyen",
      TRUE ~ "Grand"
    )
  )

# =========================== Étape 3 : Vérification des nouvelles variables ===========================

# Afficher un résumé des nouvelles variables
summary_new_vars <- summary(mydata[c("TotalAmount", "ItemsPerTransaction", "TransactionTotal", 
                                     "CustomerTotalAmount", "CustomerAverageAmount")])
print("Résumé des nouvelles variables numériques:")
print(summary_new_vars)

# Afficher la distribution des variables catégorielles
cat("\nDistribution des périodes de la journée:\n")
print(table(mydata$TimeOfDay))

cat("\nDistribution Semaine/Weekend:\n")
print(table(mydata$WeekPart))

cat("\nDistribution des catégories de transaction:\n")
print(table(mydata$TransactionSizeCategory))

# =========================== Étape 4 : Export du dataset enrichi ===========================

write.csv(mydata, "Online_Retail_Clean_Enhanced.csv", row.names = FALSE)