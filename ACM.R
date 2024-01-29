library(readr)
library("FactoMineR")
library("factoextra")
setwd("C:/Users/Acer/Downloads")
# Etude de l'inclusion financière en Afrique subharienne
inclusion_Africa <- read_csv("C:/Users/Acer/Downloads/LowIDH_economiesss.csv")
View(inclusion_Africa)
res.mca<-MCA(inclusion_Africa[,6:16])
res.mca$var$coord
write.table(res.mca$var$coord,"res.mca_var_coord.csv",sep=",")
X11()
plot(res.mca)
# Valeurs propres / Variances
eig.val <- get_eigenvalue(res.mca)
# On visualise les pourcentages de variances expliquées par chaque dimension de l’ACM
X11()
fviz_screeplot (res.mca, addlabels = TRUE, ylim = c (0, 45))

#Corrélation entre les variables et les axes principaux
X11()
fviz_mca_var (res.mca, choice = "mca.cor",
              repel = TRUE, 
              ggtheme = theme_minimal ())

# Coordonnées des catégories des variables
X11()
fviz_mca_var(res.mca,
              repel = TRUE, 
              ggtheme = theme_minimal ())

# Qualité de représentation des catégories des variables
X11()
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal())

# Contribution des catégories des modalités des variables
X11()
fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal()
)
# Qualité de la contribution des individus
X11()
fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, 
             ggtheme = theme_minimal())


###############  Analyse avec des variables supplémentaires############## 

res.mca <- MCA(inclusion_Africa,quanti.sup = c(5,4), quali.sup = c(1,3))
res.mca$quali.sup$coord
write.table(res.mca$quali.sup$coord,"res.mca_var_coord_supp_qualiii.csv",sep=",")

eig.val <- get_eigenvalue(res.mca)
# On visualise les pourcentages de variances expliquées par chaque dimension de l’ACM
X11()
fviz_screeplot (res.mca, addlabels = TRUE, ylim = c (0, 45))

#Corrélation entre les variables et les axes principaux
X11()
fviz_mca_var (res.mca, choice = "mca.cor",
              repel = TRUE, 
              ggtheme = theme_minimal ())

# Coordonnées des catégories des variables
X11()
fviz_mca_var (res.mca,
              repel = TRUE, 
              ggtheme = theme_minimal ())

# Qualité de représentation des catégories des variables
X11()
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal())

# Contribution des catégories des modalités des variables
X11()
fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal()
)
####################################################################################### 

# Etude de l'inclusion financière en dans les pays aux plus bas niveaux d'IDH

LowIDH_economies <- read_csv("C:/Users/Acer/Downloads/LowIDH_economies.csv")
View(LowIDH_economies)
res.mca<-MCA(LowIDH_economies[,6:16])
X11()
plot(res.mca)
# Valeurs propres / Variances
eig.val <- get_eigenvalue(res.mca)
# On visualise les pourcentages de variances expliquées par chaque dimension de l’ACM
X11()
fviz_screeplot (res.mca, addlabels = TRUE, ylim = c (0, 45))

#Corrélation entre les variables et les axes principaux
X11()
fviz_mca_var (res.mca, choice = "mca.cor",
              repel = TRUE, 
              ggtheme = theme_minimal ())

# Coordonnées des catégories des variables
X11()
fviz_mca_var (res.mca,
              repel = TRUE, 
              ggtheme = theme_minimal ())

# Qualité de représentation des catégories des variables
X11()
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal())

# Contribution des catégories des modalités des variables
X11()
fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal()
)
# Qualité de la contribution des individus
X11()
fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, 
             ggtheme = theme_minimal())


############## Analyse avec des variables supplémentaires ############## 

res.mca <- MCA(LowIDH_economies,quanti.sup = c(4,5), quali.sup = c(1,2,3))

eig.val <- get_eigenvalue(res.mca)
# On visualise les pourcentages de variances expliquées par chaque dimension de l’ACM
X11()
fviz_screeplot (res.mca, addlabels = TRUE, ylim = c (0, 45))

#Corrélation entre les variables et les axes principaux
X11()
fviz_mca_var (res.mca, choice = "mca.cor",
              repel = TRUE, 
              ggtheme = theme_minimal ())

# Coordonnées des catégories des variables
X11()
fviz_mca_var (res.mca,
              repel = TRUE, 
              ggtheme = theme_minimal ())

# Qualité de représentation des catégories des variables
X11()
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal())

# Contribution des catégories des modalités des variables
X11()
fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             ggtheme = theme_minimal()
)
