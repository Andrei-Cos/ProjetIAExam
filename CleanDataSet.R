# =========================== Étape 1 : Nettoyage du dataset ===========================

# Importer le fichier CSV
mydata <- read.csv("/Users/andreicosmin/Andrei/ICHEC-ECAM/MA2/IA/Projet Exam/Online Retail 3.csv", 
                   sep = ";", header = TRUE, stringsAsFactors = FALSE)

# Afficher les premières lignes pour vérification
head(mydata)

# ----------------- Conversion et nettoyage des colonnes -----------------

# Convertir UnitPrice en numérique après avoir remplacé les virgules par des points
mydata$UnitPrice <- as.numeric(gsub(",", ".", mydata$UnitPrice))

# Supprimer les lignes où UnitPrice ou Quantity ne sont pas valides (NA ou <= 0)
mydata <- mydata[!is.na(mydata$UnitPrice) & mydata$UnitPrice > 0, ]
mydata <- mydata[mydata$Quantity > 0, ]

# Convertir la colonne InvoiceDate en format Date/Time
mydata$InvoiceDate <- as.POSIXct(mydata$InvoiceDate, format = "%d/%m/%Y %H:%M")

# ----------------- Suppression des doublons -----------------

# Vérifier et supprimer les doublons
mydata <- mydata[!duplicated(mydata), ]

# ----------------- Gestion des valeurs manquantes -----------------

# Supprimer les lignes avec des valeurs manquantes
mydata <- na.omit(mydata)

# =========================== Fin du nettoyage du dataset ===========================

# Exporter le dataset nettoyé pour utilisation ultérieure
write.csv(mydata, "Online_Retail_Clean.csv", row.names = FALSE)

# Afficher un résumé des données nettoyées
str(mydata)
summary(mydata)
