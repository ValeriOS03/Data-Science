library(lme4)
library(FactoMineR)       
library(FactoInvestigate)    
library(ggrepel)
library(factoextra)
library(readr)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(plotly)
library(GGally)
library(corrplot)
library(cowplot)
library(caret)
library(viridis)
library(gridExtra)
library(knitr)
library(scales)
library(NbClust)
library(glmnet)

library(NbClust)
library(tidyverse)
library(magrittr)
library(factoextra)
library(cluster)
library(mclust) 
library(RColorBrewer)
library(teigen)
library(ContaminatedMixt)
library(vscc)  
library(clustvarsel)
library(plotly)
library(glmnet)
library(sparcl)
library(ggcorrplot)
library(FactoMineR)
library(FactoInvestigate)

library(factoextra)
library(readr)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(plotly)
library(GGally)
library(corrplot)
library(cowplot)
library(viridis)
library(gridExtra)
library(knitr)
library(scales)
library(NbClust)

library(tidyverse)
library(magrittr)
library(sf)
library(scales)
library(lmerTest)
library(lme4)
library(plotly)
library(gridExtra)



load("~/Desktop/R/Data Mining/PRESENTAZIONE FINALE GRUPPO/DatiPresentazione2024.RData")

dati = Economics

nomi_variabili = c("tasso_persistenza_anni_1_2", "crediti", "studenti_iscr_triennali", "studenti_iscr_magistrali", "studenti_laureati_triennali", "studenti_laureati_magistrali", 
                  "professori_permanenti_crediti", "professori_permanenti_studenti", "posti_2009_10", "posti_2008_9", "rapp_ricercatori_professori", "attivita_didattiche", "tipologia_universita")

colnames(dati) = nomi_variabili

"""
P1: Tasso di persistenza tra il primo e il secondo anno accademico.
P2: Crediti ottenuti.
P3A: Tasso di studenti regolari iscritti ai corsi triennali di laurea.
P3B: Tasso di studenti regolari iscritti ai corsi magistrali biennali.
P4A: Tasso di studenti regolari laureati nei corsi triennali di laurea.
P4B: Tasso di laureati regolari nei corsi magistrali biennali.
D1: Professori permanenti per crediti.
D2: Professori permanenti per studente iscritto.
D3: Posti a sedere per studente iscritto nell'anno accademico 2009/2010.
D4: Posti a sedere per studente iscritto nell'anno accademico 2008/2009.
D5: Rapporto ricercatori/professori.
D6: Attività didattiche monitorate.
"""

# grafici -----------------------------------------------------------------


# grafico italia ---------------------------


dataset_grafico <- read_delim("Desktop/R/Data Mining/PRESENTAZIONE FINALE GRUPPO/Economics.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)

dataset_grafico$Università = c("Marche", "Puglia", "Lombardia", "Emilia-Romagna", "Emilia-Romagna", "Emilia-Romagna", "Lombardia", "Sardegna", "Calabria", "Lazio", "Sicilia", "Abruzzo", 
                               "Emilia-Romagna", "Toscana", "Puglia", "Liguria", "Lombardia", "Puglia", "Marche", "Sicilia", "Lombardia", "Emilia-Romagna", "Molise", "Campania", "Campania", 
                               "Campania", "Veneto", "Sicilia", "Emilia-Romagna", "Lombardia", "Umbria", "Piemonte", "Toscana", "Lazio", "Lazio", "Lazio", "Campania", "Campania", "Sardegna",
                               "Toscana", "Piemonte", "Trentino-Alto Adige", "Friuli Venezia Giulia", "Lazio", "Friuli Venezia Giulia", "Marche", "Veneto", "Veneto", 
                               "Trentino-Alto Adige", "Puglia", "Lombardia", "Lombardia", "Lombardia", "Lazio", "Lazio")



regioni <- st_read("//Users/valeriovalentini/Desktop/R/Data Mining/PRESENTAZIONE FINALE GRUPPO/Limiti01012023_g/Reg01012023_g/")


media_regione <- dataset_grafico %>%
  group_by(Università) %>%
  summarise(across(c("P1",    "P2"  , "P3A",   "P3B"  , "P4A" ,  "P4B" ,   "D1"   , "D2"  ,  "D3"   , "D4"    ,"D5"   , "D6" ), mean))


data_merge <- merge(regioni, media_regione, by.x = "DEN_REG", by.y = "Università")



ggplotly(ggplot(data_merge) +
  geom_sf(aes(fill = P1)) +
  scale_fill_viridis_c() +  
  labs(title = "Media del tasso di persistenza tra il primo e il secondo anno accademico per Regione", fill = "Media"))

ggplotly(ggplot(data_merge) +
  geom_sf(aes(fill = P2)) +
  scale_fill_viridis_c() +  
  labs(title = "Media dei crediti per Regione", fill = "Media"))

ggplotly(ggplot(data_merge) +
  geom_sf(aes(fill = P3A)) +
  scale_fill_viridis_c() +  
  labs(title = "Media degli studenti iscritti alla triennale per Regione", fill = "Media"))

ggplotly(ggplot(data_merge) +
  geom_sf(aes(fill = P3B)) +
  scale_fill_viridis_c() +  
  labs(title = "Media degli studenti iscritti alla magistrale per Regione", fill = "Media"))

ggplotly(ggplot(data_merge) +
  geom_sf(aes(fill = P4A)) +
  scale_fill_viridis_c() +  
  labs(title = "Media degli studenti laureati alla triennale per Regione", fill = "Media"))

ggplotly(ggplot(data_merge) +
  geom_sf(aes(fill = P4B)) +
  scale_fill_viridis_c() +  
  labs(title = "Media degli studenti laureati alla magistrale per Regione", fill = "Media"))

ggplotly(ggplot(data_merge) +
  geom_sf(aes(fill = D1)) +
  scale_fill_viridis_c() +  
  labs(title = "Media dei professori permanenti per crediti per Regione", fill = "Media"))

ggplotly(ggplot(data_merge) +
  geom_sf(aes(fill = D2)) +
  scale_fill_viridis_c() +  
  labs(title = "Media dei professori permanenti per studente iscritto per Regione", fill = "Media"))

ggplotly(ggplot(data_merge) +
  geom_sf(aes(fill = D3)) +
  scale_fill_viridis_c() +  
  labs(title = "Media dei posti a sedere per studente iscritto 2009-2010 per Regione", fill = "Media"))

ggplotly(ggplot(data_merge) +
  geom_sf(aes(fill = D4)) +
  scale_fill_viridis_c() +  
  labs(title = "Media dei posti a sedere per studente iscritto 2008-2009 per Regione", fill = "Media"))

ggplotly(ggplot(data_merge) +
  geom_sf(aes(fill = D5)) +
  scale_fill_viridis_c() +  
  labs(title = "Media del rapporto ricercatori-professori per Regione", fill = "Media"))

ggplotly(ggplot(data_merge) +
  geom_sf(aes(fill = D6)) +
  scale_fill_viridis_c() +  
  labs(title = "Media delle attività didattiche monitorate per Regione", fill = "Media"))


# grafico variabili per tipologia universita

media_universita_P <- dati %>%
  group_by(tipologia_universita) %>%
  summarise(across(c("tasso_persistenza_anni_1_2",    "crediti"  , "studenti_iscr_triennali",   "studenti_iscr_magistrali"  , "studenti_laureati_triennali" ,  "studenti_laureati_magistrali" ), mean))


dati_long_P <- tidyr::gather(media_universita_P, key = "Variable", value = "Value", -tipologia_universita)


ggplotly(ggplot(dati_long_P, aes(x = Variable, y = Value, fill = tipologia_universita)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black") +
  labs(title = "Valori delle Variabili P per Tipologia di Università", x = "Variabile", y = "Valore", fill = "Tipologia Università") +
  scale_fill_manual(values = c("Private" = "blue", "Public" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))



media_universita_D1 <- dati %>% 
  group_by(tipologia_universita) %>%
  summarise(across(c("rapp_ricercatori_professori"   , "attivita_didattiche" ), mean))

dati_long_D1 <- tidyr::gather(media_universita_D1, key = "Variable", value = "Value", -tipologia_universita)


ggplotly(ggplot(dati_long_D1, aes(x = Variable, y = Value, fill = tipologia_universita)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black") +
  labs(title = "Valori delle Variabili D per Tipologia di Università", x = "Variabile", y = "Valore", fill = "Tipologia Università") +
  scale_fill_manual(values = c("Private" = "blue", "Public" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))






media_universita_D2 <- dati %>% 
  group_by(tipologia_universita) %>%
  summarise(across(c("professori_permanenti_crediti"   , "professori_permanenti_studenti"  ,  "posti_2009_10"   , "posti_2008_9"), mean))

dati_long_D2 <- tidyr::gather(media_universita_D2, key = "Variable", value = "Value", -tipologia_universita)


ggplotly(ggplot(dati_long_D2, aes(x = Variable, y = Value, fill = tipologia_universita)) +
           geom_bar(stat = "identity", position = "dodge", width = 0.7, color = "black") +
           labs(title = "Valori delle Variabili D per Tipologia di Università", x = "Variabile", y = "Valore", fill = "Tipologia Università") +
           scale_fill_manual(values = c("Private" = "blue", "Public" = "red")) +
           theme_minimal() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)))












# clustering gerarchico --------------------------------------------------------------


clustering = NbClust(dati[,-13], distance = "manhattan", method = "ward.D2")


metodi_gerarchici = c("single","complete","average","ward.D") 
misure_distanza = c("euclidean","manhattan","minkowski")   
i = 0
lista_risultati <- list()                                           # lista dove mettere i deondogrammi 
matrice_cluster <- matrix(NA,nrow=nrow(dati),ncol=(length(misure_distanza)*length(metodi_gerarchici)))      #matrice dove mettere i deondogrammi tagliati   
j = 0


for (misura in misure_distanza) {
  
  distanza <- dist(dati[,-13], method = misura)           # calcolo la distanza con una delle misure alla volta
  
  for(metodo in metodi_gerarchici) {            # applico un metodo per ogni misura di distanza 
    i <- i+1
    lista_risultati[[i]] <- hclust(distanza,method = metodo)        # metto il deodnogramma nella lista 
    
    matrice_cluster[,i] <- cutree(lista_risultati[[i]],k= 2)           # taglio il deondogramma in base al numero di classi (nella prima variabile)
    
    matrice_confusione = table(dati[,13],matrice_cluster[,i])
    
    accuratezza_per_classe_1 = matrice_confusione[1, 1] / sum(matrice_confusione[1, ])
    accuratezza_per_classe_2 = matrice_confusione[2, 2] / sum(matrice_confusione[2, ])
    accuratezza_totale = accuratezza_per_classe_1 * (length(dati$tipologia_universita[dati$tipologia_universita == "Private"])/nrow(dati))  + accuratezza_per_classe_2 * (length(dati$tipologia_universita[dati$tipologia_universita == "Public"])/nrow(dati))        
    cat("accuracy complessiva per la distanza: ", misura, " e il metodo", metodo, " è: ", accuratezza_totale, "\n")
    
    
    } 
}  

    
deondogramma = hclust(dist(dati[,-13], method = "euclidean"), method = "complete")

fviz_dend( deondogramma, palette = c("green", "blue"),k = 2,  rect = T, show_labels = F) 

fviz_cluster(list(data = dati[,-13], cluster = cutree(deondogramma, k = 2)), geom = "point", palette = c("orange", "blue"))





# Lasso


lasso_model <- cv.glmnet(as.matrix(dati[,-13]), as.numeric(dati$tipologia_universita), alpha = 1)   

coefficients <- coef(lasso_model, s = lasso_model$lambda.min)     

variabili_selezionate <- rownames(coefficients)[coefficients[, 1] != 0]   

names(dati[variabili_selezionate[-1]])




metodi_gerarchici = c("single","complete","average","ward.D") 
misure_distanza = c("euclidean","manhattan","minkowski")   
i = 0
lista_risultati <- list()                                           # lista dove mettere i deondogrammi 
matrice_cluster <- matrix(NA,nrow=nrow(dati),ncol=(length(misure_distanza)*length(metodi_gerarchici)))      #matrice dove mettere i deondogrammi tagliati   
j = 0


for (misura in misure_distanza) {
  
  distanza <- dist(dati[variabili_selezionate[-1]], method = misura)           # calcolo la distanza con una delle misure alla volta
  
  for(metodo in metodi_gerarchici) {            # applico un metodo per ogni misura di distanza 
    i <- i+1
    lista_risultati[[i]] <- hclust(distanza,method = metodo)        # metto il deodnogramma nella lista 
    
    matrice_cluster[,i] <- cutree(lista_risultati[[i]],k= 2)           # taglio il deondogramma in base al numero di classi (nella prima variabile)
    
    matrice_confusione = table(dati[,13],matrice_cluster[,i])
    
    accuratezza_per_classe_1 = matrice_confusione[1, 1] / sum(matrice_confusione[1, ])
    accuratezza_per_classe_2 = matrice_confusione[2, 2] / sum(matrice_confusione[2, ])
    accuratezza_totale = accuratezza_per_classe_1 * (length(dati$tipologia_universita[dati$tipologia_universita == "Private"])/nrow(dati))  + accuratezza_per_classe_2 * (length(dati$tipologia_universita[dati$tipologia_universita == "Public"])/nrow(dati))        
    cat("accuracy complessiva per la distanza: ", misura, " e il metodo", metodo, " è: ", accuratezza_totale, "\n")
    
    
  } 
}  

deondogramma = hclust(dist(dati[variabili_selezionate[-1]], method = "manhattan"), method = "complete")

fviz_dend( deondogramma, palette = c("green", "blue"),k = 2,  rect = T, show_labels = F) 

fviz_cluster(list(data = dati[variabili_selezionate[-1]], cluster = cutree(deondogramma, k = 2)), geom = "point", palette = c("orange", "blue"))




# kmean

k_medie = kmeans(dati[,-13],centers=2,nstart = 20)  

(tabella_confusione_kmeans = table(dati[,13],k_medie$cluster)) 

accuratezza_per_classe_1 = tabella_confusione_kmeans[1, 1] / sum(tabella_confusione_kmeans[1, ])
accuratezza_per_classe_2 = tabella_confusione_kmeans[2, 2] / sum(tabella_confusione_kmeans[2, ])
accuratezza_totale = accuratezza_per_classe_1 * (length(dati$tipologia_universita[dati$tipologia_universita == "Private"])/nrow(dati))  + accuratezza_per_classe_2 * (length(dati$tipologia_universita[dati$tipologia_universita == "Public"])/nrow(dati))        
cat("accuracy complessiva  con il metodo kmean è: ", accuratezza_totale)





# pam

pam = pam(dati[,-13],k=2,nstart = 20)  

(tabella_confusione_pam = table(dati[,13],pam$cluster)) 

accuratezza_per_classe_1 = tabella_confusione_pam[1, 1] / sum(tabella_confusione_pam[1, ])
accuratezza_per_classe_2 = tabella_confusione_pam[2, 2] / sum(tabella_confusione_pam[2, ])
accuratezza_totale = accuratezza_per_classe_1 * (length(dati$tipologia_universita[dati$tipologia_universita == "Private"])/nrow(dati))  + accuratezza_per_classe_2 * (length(dati$tipologia_universita[dati$tipologia_universita == "Public"])/nrow(dati))        
cat("accuracy complessiva  con il metodo pam è: ", accuratezza_totale)








# clustering model based --------------------------------------------------

dati[,-13] %>% scale() %>% data.frame %>% 
  gather(Variabili, Valori) %>%
  ggplot(aes(x=Variabili, y=Valori, fill=Variabili)) +
  geom_boxplot(show.legend=FALSE) + 
  labs(y = "Valore", x = "Variabile", fill = "Variabili", title= "Boxplot delle Variabili per osservare gli outlier") +
  theme_bw() +
  scale_fill_manual(values = colorRampPalette(c("green", "purple"))(12)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Imposta l'angolo di rotazione a 45 gradi



# mclust con selezione variabili

ec_gaussiana_varsel <- Mclust(dati[variabili_selezionate[-1]],G=2)

fviz_cluster(list(data = dati[,-13], cluster = ec_gaussiana_varsel$classification), geom = "point", palette = c("orange", "blue", "pink"))

table(unlist(dati[,13]),ec_gaussiana_varsel$classification)

(indice_rand_lasso_gauss_varsel =  adjustedRandIndex(unlist(dati[,13]),ec_gaussiana_varsel$classification))


# teigen con selezione variabili

ec_tstudent_varsel <- teigen(dati[,variabili_selezionate[-1]], G=2, models = "all", verbose = FALSE)

fviz_cluster(list(data = dati[,-13], cluster = ec_tstudent_varsel$iclresults$classification), geom = "point", palette = c("orange", "blue", "pink"))

table(unlist(dati[,13]),ec_tstudent_varsel$iclresults$classification)

(indice_rand_lasso_teigen_varsel = adjustedRandIndex(unlist(dati[,13]),ec_tstudent_varsel$iclresults$classification))


# CNmix con selezione variabili

ec_contaminata_varsel <- CNmixt(dati[,variabili_selezionate[-1]], G = 2, parallel = FALSE, verbose = FALSE)

fviz_cluster(list(data = dati[,-13], cluster = getCluster(ec_contaminata_varsel)), geom = "point", palette = c("orange", "blue", "pink"))

table(unlist(dati[,13]),getCluster(ec_contaminata_varsel))

(indice_rand_lasso_contaminata_varsel = adjustedRandIndex(dati[,13],getCluster(ec_contaminata_varsel)))





# vscc

selezione_variabili_vscc <- vscc(dati[,-13], G=2, automate = "mclust", initial = NULL, train = NULL, forcereduction = FALSE)

colnames(selezione_variabili_vscc$topselected)

table(unlist(dati[,13]), selezione_variabili_vscc$bestmodel$classification) 

fviz_cluster(list(data = dati[,-13], cluster = selezione_variabili_vscc$bestmodel$classification), geom = "point", palette = c("orange", "blue"))

(indice_rand_vscc_varsel = adjustedRandIndex(unlist(dati[,13]),selezione_variabili_vscc$bestmodel$classification))


# Clustvarsel

selezione_variabili_clustvarsel <- clustvarsel(dati[,-13],G=2)

selezione_variabili_clustvarsel$subset

table(unlist(dati[,13]),selezione_variabili_clustvarsel$model$classification)

fviz_cluster(list(data = dati[,-13], cluster = selezione_variabili_clustvarsel$model$classification), geom = "point", palette = c("orange", "blue"))

(indice_rand_custvarsel_varsel = adjustedRandIndex(unlist(dati[,13]),selezione_variabili_clustvarsel$model$classification))



# regressione -------------------------------------------------------------



# binomiale



t.test(dati$tasso_persistenza_anni_1_2~tipologia_universita, data = dati)
t.test(dati$crediti~tipologia_universita, data = dati)
t.test(dati$studenti_iscr_triennali~tipologia_universita, data = dati)
t.test(dati$studenti_iscr_magistrali~tipologia_universita, data = dati)
t.test(dati$studenti_laureati_triennali~tipologia_universita, data = dati)
t.test(dati$studenti_laureati_magistrali~tipologia_universita, data = dati)
t.test(dati$professori_permanenti_crediti~tipologia_universita, data = dati)
t.test(dati$professori_permanenti_studenti~tipologia_universita, data = dati)
t.test(dati$posti_2009_10~tipologia_universita, data = dati)
t.test(dati$posti_2008_9~tipologia_universita, data = dati)
t.test(dati$rapp_ricercatori_professori~tipologia_universita, data = dati)
t.test(dati$attivita_didattiche~tipologia_universita, data = dati)


dati$tipologia_universita <- as.factor(dati$tipologia_universita)


dati_oversampled <- upSample(dati[, -13], dati$tipologia_universita) 

modello_binomiale <- glmnet(as.matrix(scale(dati_oversampled[,-13])), y = dati_oversampled$Class,  family = "binomial", alpha = 1)

plot(modello_binomiale, label = TRUE, main = "traccia dei coefficienti al variare di Lambda \n", xlab = "Lambda - Penalizzazione Lasso", ylab = "Valore dei coefficienti",)                                  #visualizzazione della "traccia" o del "percorso" (path) dei coefficienti delle variabili rispetto alla norma ℓ1 dell'intero vettore dei coefficienti mentre il parametro di regolarizzazione 𝜆 varia
# In generale, all'aumentare del valore di lambda, la penalizzazione aumenta, il che rende il modello più regolarizzato, L'asse superiore del grafico indica il numero di coefficienti non zero (effective degrees of freedom, df) al valore corrente di 𝜆.
coef(modello_binomiale, s = 0.1)




 

# PCA ---------------------------------------------------------------------




ggplotly(ggcorr(cor(dati[1:12], use = "complete.obs"), hjust = 1, vjust = 1) +
           ggtitle("Grafico della correlazione delle variabili") 
)


#PCA


pca_economics = PCA(dati, quali.sup = 13, graph = FALSE )
summary(pca_economics)




# ANALISI DIMENSIONI ---------------------------------------------------------------

# screeplot

fviz_screeplot(pca_economics, addlabels = TRUE, ylim = c(0, 65), 
               xlab = "Dimensioni", ylab = "Percentuale di varianza spiegata da ogni dimensione",
               barfill = "orange", barcolor = "orange", linecolor = "black")  

dev.off()

# correlazione variabili - dimensioni

corrplot(pca_economics$var$cor, method="circle",tl.col="black", tl.cex = 0.9, 
         cl.cex = 0.5, cl.align.text="l", 
         title = "Grafico della correlazione  tra le Dimensioni e le variabili",
         mar = c(1, 0, 5, 1))



# analisi delle categorie  ------------------------------------------------

plot(pca_economics, choix="ind",habillage = "tipologia_universita", invisible = "ind", title = "Grafico della tipologia dell'università")

ggplotly(
  fviz_pca_biplot(pca_economics, col.var = "black", geom.ind = "point", pointshape = 21, pointsize = 2, axes = c(1,2),
               col.ind = "black", fill.ind = dati$tipologia_universita, palette = "jco", addEllipses = TRUE, title = "Grafico delle categorie"))



# analisi variabili 

fviz_pca_var(pca_economics, col.var = "cos2", axes = c(1,2),
             gradient.cols = c("orange", "green", "blue"),
             title="Contributo delle variabili alla varianza")


# analisi osservazioni

fviz_pca_ind(pca_economics, axes = c(1,2), title="Contributo delle osservazioni alla varianza",col.ind = "cos2",
             ggtheme=theme_minimal(base_size = 11),gradient.cols = c("orange", "green", "blue"))


# HCPC --------------------------------------------------------------------


pca_economics_clustering = HCPC(pca_economics, graph = FALSE)

fviz_cluster(pca_economics_clustering,
             show.clust.cent = TRUE,
             palette = c("darkorange", "darkgreen", "purple"),
             ggtheme = theme_minimal(),
             main = "Grafico del clustering")



fviz_pca_biplot(pca_economics, col.var = "black", col.ind = pca_economics_clustering$data.clust$clust )           








