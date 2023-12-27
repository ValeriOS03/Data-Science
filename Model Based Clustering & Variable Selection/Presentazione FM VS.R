



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



# import e clean ----------------------------------------------------------



load("~/Desktop/R/Data Mining/Mixture Models - Variable Selection /FiniteMixtureL31.RData")

summary(wine)

data = wine[,-1]

Type = wine %>% mutate(Type = case_when(
  Type == "1" ~ "Barolo",
  Type == "2" ~ "Grignolino",
  Type == "3" ~ "Barbera")) %>% select("Type")


# Grafici


# Barplot sulla concentrazione di elementi chimici nei tipi di vino

Wine_Type_Long <- wine %>% select(Calcium, Potassium, Magnesium, Phosphate, Type) %>% 
  group_by(Type) %>% summarise_all(mean) %>% 
  pivot_longer(cols = c(Calcium, Potassium, Magnesium, Phosphate),
               names_to = "Elemento", values_to = "Concentrazione") %>%
  mutate(Type = as.character(Type)) %>%
  mutate(Type = case_when(
    Type == "1" ~ "Barolo",
    Type == "2" ~ "Grignolino",
    Type == "3" ~ "Barbera",
  ))

conc_elem_c <- ggplot(Wine_Type_Long, aes(x = Type, y = Concentrazione, fill = Elemento)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Concentrazione di elementi chimici nei tipi di vino",
       x = "Tipo di vino", y = "Concentrazione") +
  scale_fill_manual(values = c("Calcium" = "orange", "Potassium" = "blue", "Magnesium" = "pink", "Phosphate" = "green"),
                    guide = guide_legend(title = "Elementi Chimici")) +
  theme_minimal()

ggplotly(conc_elem_c) 



# Grafico sulla Densità di Alcohol e Intensità di Colore

dens_alc_col <- ggplot(wine, aes(x = Alcohol, y = `Color Intensity`)) +
  geom_density_2d_filled() +
  geom_point(aes(col = as.factor(Type))) +
  labs(title = "Densità di Alcohol e Intensità di Colore", x = "Alcohol", y = "Intensità di Colore") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(1, 13)) +
  scale_color_manual(name = "Tipo di Vino", values = c("1" = "orange", "2" = "blue", "3" = "pink"), labels = c("Barolo", "Grignolino", "Barbera"))

dens_alc_col




# visualization -----------------------------------------------------------



# numero cluster

n_cluster_reali = dim(unique(Type))[1]

test_nbclust <- NbClust(scale(data), distance = "euclidean", method = "ward.D2")

n_cluster_predetti = max(test_nbclust$Best.partition)

n_cluster_predetti == n_cluster_reali





#meotodo gerarchico 

deondogramma = hclust(dist(scale(data), method = "euclidean"), method = "ward.D2")

fviz_dend( deondogramma, palette = c("orange", "blue", "pink"), k = n_cluster_predetti,  rect = T, show_labels = F) 

table(cutree(deondogramma, k = n_cluster_predetti), unlist(Type))

(indice_rand_met_gerarchici = adjustedRandIndex(cutree(deondogramma, k = n_cluster_predetti), wine$Type))

fviz_cluster(list(data = data, cluster = cutree(deondogramma, k = n_cluster_predetti)), geom = "point", palette = c("orange", "blue", "pink"))





# visualizzazione outliner con boxplot 

data %>% scale() %>% data.frame %>% 
  gather(Variabili, Valori) %>%
  ggplot(aes(x=Variabili, y=Valori, fill=Variabili)) +
  geom_boxplot(show.legend=FALSE) + 
  labs(y = "Valore", x = "Variabile", fill = "Variabili", title= "Boxplot delle Variabili per osservare gli outliner") +
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +    
  scale_fill_manual(values = colorRampPalette(c("blue", "red"))(27))




# visualizzazione distrubizione varibili con istogrammi

library(tidyverse)

data %>%
  scale() %>%
  data.frame() %>%
  gather(key = "variabile", value = "valore") %>%
  ggplot(aes(x = valore, fill = variabile)) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 20, color = "white") +
  geom_density(aes(y = ..count..), alpha = 0.3, color = "black") +
  facet_wrap(~variabile, scales = "free") +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_blank())




# CLUSTERING SENZA SELEZIONE

# Guassiana

wine_gaussiana <- Mclust(data,G=1:4)

summary(wine_gaussiana)

fviz_mclust(wine_gaussiana, "classification", geom = "point", palette = c("orange", "blue", "pink") )

fviz_mclust(wine_gaussiana, "uncertainty", palette = c("orange", "blue", "pink") )

table(unlist(Type),wine_gaussiana$classification)

(indice_rand_Mclust_novarsel =  adjustedRandIndex(unlist(Type),wine_gaussiana$classification))


# T Student

wine_tstudent <- teigen(data, G=1:4, models = "all", verbose = FALSE)

summary(wine_tstudent)

fviz_cluster(list(data = data, cluster = wine_tstudent$iclresults$classification), geom = "point", palette = c("orange", "blue", "pink"))

table(unlist(Type),wine_tstudent$iclresults$classification)

(indice_rand_teigen_novarsel = adjustedRandIndex(unlist(Type),wine_tstudent$iclresults$classification))


# Contaminata

wine_contaminata <- CNmixt(data, G = 1:3, parallel = FALSE, verbose = TRUE)

summary(wine_contaminata)

fviz_cluster(list(data = data, cluster = getCluster(wine_contaminata)), geom = "point", palette = c("orange", "blue", "pink"))

table(unlist(Type),getCluster(wine_contaminata))

(indice_rand_contaminata_novarsel = adjustedRandIndex(wine[,1],getCluster(wine_contaminata)))








# SELEZIONE VARIABILI 


# vscc

selezione_variabili_vscc <- vscc(data, G=1:5, automate = "mclust", initial = NULL, train = NULL, forcereduction = FALSE)

table(unlist(Type), selezione_variabili_vscc$bestmodel$classification) 

fviz_cluster(list(data = data, cluster = selezione_variabili_vscc$bestmodel$classification), geom = "point", palette = c("orange", "blue", "pink"))

(indice_rand_vscc_varsel = adjustedRandIndex(unlist(Type),selezione_variabili_vscc$bestmodel$classification))


# Clustvarsel

selezione_variabili_clustvarsel <- clustvarsel(data,G=3)

selezione_variabili_clustvarsel$subset

table(unlist(Type),selezione_variabili_clustvarsel$model$classification)

fviz_cluster(list(data = data, cluster = selezione_variabili_clustvarsel$model$classification), geom = "point", palette = c("orange", "blue", "pink"))

(indice_rand_custvarsel_varsel = adjustedRandIndex(unlist(Type),selezione_variabili_clustvarsel$model$classification))


# Lasso


lasso_model <- cv.glmnet(as.matrix(data), wine[,1], alpha = 1)   

coefficients <- coef(lasso_model, s = lasso_model$lambda.min)     

variabili_selezionate <- rownames(coefficients)[coefficients[, 1] != 0]    


# mclust con selezione variabili

wine_gaussiana_varsel <- Mclust(data[variabili_selezionate[-1]],G=2:4)

fviz_cluster(list(data = data, cluster = wine_gaussiana_varsel$classification), geom = "point", palette = c("orange", "blue", "pink"))

table(unlist(Type),wine_gaussiana_varsel$classification)

(indice_rand_lasso_gauss_varsel =  adjustedRandIndex(unlist(Type),wine_gaussiana_varsel$classification))


# teigen con selezione variabili

wine_tstudent_varsel <- teigen(data[,variabili_selezionate[-1]], G=1:3, models = "all", verbose = FALSE)

fviz_cluster(list(data = data, cluster = wine_tstudent_varsel$iclresults$classification), geom = "point", palette = c("orange", "blue", "pink"))

table(unlist(Type),wine_tstudent_varsel$iclresults$classification)

(indice_rand_lasso_teigen_varsel = adjustedRandIndex(unlist(Type),wine_tstudent_varsel$iclresults$classification))


# CNmix con selezione variabili

wine_contaminata_varsel <- CNmixt(data[,variabili_selezionate[-1]], G = 1:3, parallel = FALSE, verbose = FALSE)

fviz_cluster(list(data = data, cluster = getCluster(wine_contaminata_varsel)), geom = "point", palette = c("orange", "blue", "pink"))

table(unlist(Type),getCluster(wine_contaminata_varsel))

(indice_rand_lasso_contaminata_varsel = adjustedRandIndex(wine[,1],getCluster(wine_contaminata_varsel)))





