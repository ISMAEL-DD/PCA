#/******Chargement des packages******/
library("tidyverse")
library(readxl)
library("FactoMineR")
library("Factoshiny")
library("factoextra")
library("qcc")
require("DescTools")
require(vtable)
require(Hmisc)
require(PerformanceAnalytics)
library(ggbiplot)
setwd("D:/M2SEP/ADD Gauth/ADD Gauth")
#/******Importation du jeu de donnees et selection des colonnes des 5 criteres******/

data <- read_excel("Alloc_doctorales.xlsx")
View(data)
# Exctraction des individus actifs et les variables actives pour l'ACP
dataACP<-dplyr::select(data, c(11:15))
attach(dataACP)
View(dataACP)
#Structure du jeu de donnEes
str(dataACP)
summary(dataACP)

#/******Staistiques descriptives******/
#Donnees manquantes
X11() 
PlotMiss(dataACP, main=" Pas de donnees manquantes")

#Tableau des statistiques descriptives
sumtable(dataACP, out='csv', file="StatdesACP.csv")
#Etdue de la correlation entre les criteres
library(PerformanceAnalytics)
X11()
PerformanceAnalytics::chart.Correlation(dataACP, pch=1, method =c("pearson", "kendall", "spearman"))

#Analyse en Composantes Principales
ACP<-PCA(dataACP, scale.unit = F, graph = T)

#Qualites et contribution des variables aux 2 premieres composantes principales
X11()
fviz_pca_var(ACP, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Évite le chevauchement de texte
#Qualites et contribution des variables a la 2em et 3em premieres composantes principales

ACP2_3<-PCA(dataACP, scale.unit = F, graph = T, axes = c(2,3))
X11()
fviz_pca_var(ACP2_3, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Évite le chevauchement de texte

#https://redirect.viglink.com/?format=go&jsonp=vglnk_166769528421615&key=18d81f55cfc0fd275a864f494f914b76&libId=la4m0fwc0100icfe000DLb8195b9z&loc=https%3A%2F%2Fstatistiques.forumpro.fr%2Ft1360-facteur-taille-acp%23google_vignette&gdprConsent=CPiBboAPiBboABcAIBFRCpCgAP_AAH_AAAqIIyEB7C9MTWNgcH5qCZsQQYxHwRAEImAADAAJgQABABJAMIQAAGAAIAFAAIAKAAAAIBJAAQEBCAlAAAAAIAAAACAIAAAAAAAAICAAAAARAgAACABIAQAAAAAAAABAAhAAgAAEYAoAQAQAAQAAAAAAAAAAAIABQAQEAAAAAAAQAAAAAAgAAAAAAAAAAAAAAEEZAARBUmIAGwKDAkAACIAEAIIAgCABAABAAAwAAAAAgAEEIACDAAAAAAAEAEAAAAEAEAAAAAgAQgAAAAEAAAAAAEAAAAAAAAABAAAAAAgAAAAAAAAIAAAAAAAAAgAAAAQAAgEAAEIAAAAAAAAAAAAAAAAEAgAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA&gdprApplies=true&v=1&type=U&opt=true&out=http%3A%2F%2Fpbil.univ-lyon1.fr%2FR%2Fpdf%2Ftdr61.pdf&ref=https%3A%2F%2Fwww.google.com%2F&title=Facteur%20taille%20ACP&txt=http%3A%2F%2Fpbil.univ-lyon1.fr%2FR%2Fpdf%2Ftdr61.pdf
install.packages("ade4")
library(ade4)
# ACP de A.B. Dufour & D. Chessel : log  et double centrage du dataset
dataACP.log <- log(dataACP) #transformation log
dataACP.log0 <- bicenter.wt(dataACP.log) #double centrage
dataACP.log0 <- data.frame(dataACP.log0) #conversion en data.frame
X11()
ACP.dataACP.log0<-PCA(dataACP.log0, scale.unit = F, graph = T)
#Qualites et contribution des variables aux 2 premieres composantes principales
X11()
fviz_pca_var(ACP.dataACP.log0, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Évite le chevauchement de texte
x11()
fviz_cos2(ACP.dataACP.log0, choice = "var", axes = 1:2, title="")

# les valeurs propres
get_eigenvalue(ACP.dataACP.log0)
x11()
fviz_eig(ACP.dataACP.log0, addlabels = TRUE, ylim = c(0, 80))

#Contributions des variables aux composantes
varacp <- get_pca_var(ACP.dataACP.log0)
head(round(varacp$contri,3))

contribDM1.DM2<-fviz_contrib(ACP.dataACP.log0, choice = "var", axes = 1:2, title="")
x11()
contribDM1.DM2

#Qualites de et contribution des individus à la construction de DIM1 et DIM2
x11()
fviz_pca_ind(ACP.dataACP.log0,
             col.ind = "cos2", pointsize = "contrib",
             # geom="point",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,title="")

#clustering
install.packages('cluster')
library(cluster)
ACP.dataACP.log0_1.2 = as.data.frame(-ACP.dataACP.log0$ind$coord[,1:2])
ACP.dataACP.log0_1.2
# choix du k optimal
fviz_nbclust(ACP.dataACP.log0_1.2, kmeans, method = 'silhouette') #On cherche le pic du score silhouette

k = 10 #correspond à la valeur de k optimale
#On fait la classification
kmeans_ACP.dataACP.log0_1.2 = kmeans(ACP.dataACP.log0_1.2, centers = k, nstart = 50)
x11()
fviz_cluster(kmeans_ACP.dataACP.log0_1.2, data = ACP.dataACP.log0_1.2)
#ACP par factoshiny
analyse_systematique = Factoshiny(dataACP.log0)
