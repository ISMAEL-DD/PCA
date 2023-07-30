library(readxl)
data <- read_excel("C:/Users/Acer/Downloads/nf_data.xlsx")
View(data)
library(tidyverse)

#On crée une colonne pour centilisation
data0<-data %>% mutate("Centilisation" = case_when(data$`Lib Centilisation` == "75 Cl" ~ "Blles",
                                                            data$`Lib Centilisation` == "18,75 CL" ~ "Quart_Blles",
                                                            data$`Lib Centilisation`=="20 Cl" ~ "Piccola",
                                                            data$`Lib Centilisation`== "150 CL Magnum / 2Blles" ~ "Magnum",
                                                            data$`Lib Centilisation`=="37,5 Cl" ~ "Demi_Blles",
                                                            TRUE ~ "Jeroboam"))


centilisation<-data0$Centilisation
mois<-data0$Mois

data2<-as_tibble(cbind(mois,centilisation))
#On exclu les modalités rares
a_exclure<-c("Quart_Blles","Piccola")
data2<-as.data.frame(data2[!(data2$centilisation %in% a_exclure),])
View(data2)

#Tableau de contingence
data_contin1 <- table(data2$mois, data2$centilisation)
data_contin1
write.csv(data_contin1, "data_contin1.csv")
tableau_contin <- xtabs(Freq ~ Var1 + Var2, data_contin) 
View(data_contin)
tableau_contin
library(PerformanceAnalytics)
X11()
PerformanceAnalytics::chart.Correlation(data_contin1, pch=1, method =c("pearson", "kendall", "spearman"))

library("gplots")

balloonplot(t(tableau_contin), main = "tableau_contin", xlab = "", ylab = "",label = FALSE, show.margins = FALSE)

#significativité des variables
chisq <- chisq.test (tableau_contin)

chisq
library("Rcmdr")

profil_ligne <-rowPercents(tableau_contin)
profil_colonne <- colPercents(tableau_contin)
View(profil_ligne)
View(profil_colonne)
profil_ligne <- profil_ligne[,-(5:6)]
View(profil_ligne)
profil_colonne <- profil_colonne[-(13:14),]
#AFC sur profil ligne 
X11()
PerformanceAnalytics::chart.Correlation(profil_ligne, pch=1, method =c("pearson", "kendall", "spearman"))
chisq <- chisq.test (profil_ligne)

chisq

library("FactoMineR")
library("factoextra")
X11()
res.ca <- CA (profil_ligne, ncp = 5, graph = TRUE)

X11()
fviz_screeplot (res.ca, addlabels = TRUE, ylim = c(0, 70))

X11()
fviz_ca_biplot (res.ca, repel = TRUE)

X11()
fviz_ca_row(res.ca, repel = TRUE)

X11()
fviz_ca_row (res.ca, col.row = "cos2",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

x11()
plot.CA (res.ca, selectRow = "contrib 10") 