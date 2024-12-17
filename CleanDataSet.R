# ============================== Étape 1 : Nettoyage du dataset ==============================
# Importer le fichier CSV
# On importe le fichier CSV en spécifiant le séparateur correct (";") pour éviter les erreurs d'importation.
mydata <- read.csv("/Users/andreicosmin/Andrei/ICHEC-ECAM/MA2/IA/Projet Exam/Online Retail 3.csv", 
                   sep = ";", header = TRUE, stringsAsFactors = FALSE)

# Vérifier les premières lignes pour s'assurer que l'importation a fonctionné correctement.
head(mydata)

# -------------------------------------------------------------------------------------------
# Remplacer les virgules par des points et convertir en numérique
# Certaines valeurs numériques (comme UnitPrice) utilisent des virgules au lieu de points pour les décimales.
# On utilise la fonction gsub() pour remplacer les virgules par des points, puis as.numeric() pour convertir.
mydata$UnitPrice <- as.numeric(gsub(",", ".", mydata$UnitPrice))

# -------------------------------------------------------------------------------------------
# Convertir la colonne InvoiceDate en format Date/Time
# La colonne InvoiceDate est au format texte, on la convertit en POSIXct pour faciliter les analyses temporelles.
mydata$InvoiceDate <- as.POSIXct(mydata$InvoiceDate, format = "%d/%m/%Y %H:%M")

# -------------------------------------------------------------------------------------------
# Vérifier la structure des données
# Cette étape permet de confirmer que les modifications précédentes ont bien été appliquées.
str(mydata)

# Afficher les premières lignes pour une dernière vérification visuelle.
head(mydata)

# -------------------------------------------------------------------------------------------
# Vérifier et supprimer les doublons
# On utilise la fonction duplicated() pour identifier les doublons.
# sum(duplicated()) permet de compter le nombre de doublons présents dans le dataset.
sum(duplicated(mydata)) # Affiche le nombre de doublons avant suppression.

# Supprimer les doublons
# On utilise le filtre "!" pour conserver uniquement les lignes non dupliquées.
mydata <- mydata[!duplicated(mydata), ]

# Vérifier à nouveau le nombre de doublons pour s'assurer qu'ils ont été supprimés.
sum(duplicated(mydata))

# -------------------------------------------------------------------------------------------
# Vérifier et gérer les valeurs manquantes
# colSums(is.na()) permet de compter le nombre de valeurs manquantes dans chaque colonne.
colSums(is.na(mydata))

# Supprimer les lignes contenant des valeurs manquantes
# La fonction na.omit() retire toutes les lignes où au moins une valeur est manquante.
mydata <- na.omit(mydata)

# Exporter le dataset nettoyé en CSV
write.csv(mydata, "Online_Retail_Clean.csv", row.names = FALSE)

# ============================== Fin de l'étape 1 : Nettoyage du dataset ==============================
