library(FactoMineR)       
library(FactoInvestigate)    
library(Factoshiny)   
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

#brand -- Pizza brand (class label)
#id -- Sample analysed
#mois -- Amount of water per 100 grams in the sample
#prot -- Amount of protein per 100 grams in the sample
#fat -- Amount of fat per 100 grams in the sample
#ash -- Amount of ash per 100 grams in the sample
#sodium -- Amount of sodium per 100 grams in the sample
#carb -- Amount of carbohydrates per 100 grams in the sample
#cal -- Amount of calories per 100 grams in the sample



# import and clean --------------------------------------------------------



Pizza <- read_csv("Desktop/R/Data Mining/PCA - MCA/Pizza.csv")
summary(Pizza)

Pizza$brand = factor(Pizza$brand)

summary(Pizza$brand)

Pizza %<>% select(-id) 

summary(Pizza)


# grafici -----------------------------------------------------------------



ggplotly(
Pizza %>% group_by(brand) %>% summarise_all(mean) %>% gather("micronutritions", "value", 2:8) %>% 
  ggplot(aes(x = micronutritions, y = value, fill = micronutritions)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~brand, nrow = 2, ncol = 5) +
  labs(title = "Medie dei Nutrienti per ogni brand di pizza",
       x = "Brand di Pizza",
       y = "Media del Nutriente",
       fill = "Nutriente") +
  scale_fill_brewer(palette = "Set3") + 
  theme(legend.position = "top",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
)
 


cor(Pizza[2:8])

ggplotly(ggcorr(cor(Pizza[2:8], use = "complete.obs"), hjust = 1, vjust = 1) +
  ggtitle("Grafico della Correlazione dei nutrienti") 
)



# PCA ---------------------------------------------------------------------



pca_pizza = PCA(Pizza, quali.sup = 1, graph = FALSE )
summary(pca_pizza)


plot(pca_pizza, choix = "var")      #variabili



# HCPC --------------------------------------------------------------------

 


pca_pizza_clustering = HCPC(pca_pizza, graph = FALSE)

pca_pizza_clustering$data.clust$clust

fviz_cluster(pca_pizza_clustering,
             show.clust.cent = TRUE,
             palette = c("darkorange", "darkgreen", "purple"),
             ggtheme = theme_minimal(),
             main = "Grafico del clustering")
plot(pca_pizza, choix="ind",habillage = "brand", invisible = "ind", title = "Grafico dei brand")


fviz_pca_biplot(pca_pizza, col.var = "black", col.ind = pca_pizza_clustering$data.clust$clust, )           


# ANALISI DIMENSIONI ---------------------------------------------------------------

# screeplot

fviz_screeplot(pca_pizza, addlabels = TRUE, ylim = c(0, 65), 
               xlab = "Dimensioni", ylab = "Percentuale di varianza spiegata da ogni dimensione",
               barfill = "orange", barcolor = "orange", linecolor = "black")  



#correlazione variabili - dimensioni ------------------------------------

kable(pca_pizza$var$cos2, digits=3, format="simple")

par(mfrow = c(1,3))
corrplot(pca_pizza$var$cor, method="ellipse",tl.col="black", tl.cex = 0.9, 
         cl.cex = 0.5, cl.align.text="l", 
         title = "Grafico della correlazione \n tra le Dimensioni e le variabili \n con ellissi",
         mar = c(1, 1, 5, 1))
corrplot(pca_pizza$var$cor, method="circle",tl.col="black", tl.cex = 0.9, 
         cl.cex = 0.5, cl.align.text="l", 
         title = "Grafico della correlazione \n tra le Dimensioni e le variabili \n con cerchi",
         mar = c(1, 1, 5, 1))
corrplot(pca_pizza$var$cor, method="color",tl.col="black", tl.cex = 0.9, 
         cl.cex = 0.5, cl.align.text="l", 
         title = "Grafico della correlazione \n tra le Dimensioni e le variabili \n con i colori",
         mar = c(1, 1, 5, 1))
dev.off()



# analisi delle categorie  ------------------------------------------------

ggplotly(
fviz_pca_ind(pca_pizza, geom.ind = "point", pointshape = 21, pointsize = 2, axes = c(1,2),
             col.ind = "black", fill.ind = Pizza$brand, palette = "jco", addEllipses = TRUE, title = "Grafico delle categorie"))

# analisi variabili 

plot(pca_pizza, choix="var", select="contrib 3", axes = 1:2, title = "3 variabili che contribuiscono maggiormente \n alla prima e seconda dimensione") 
plot(pca_pizza, choix="var", select="cos2 0.95", axes = 1:2, title = "variabili il cui contributo \n alla varianza è superiore al 95%") 

fviz_pca_var(pca_pizza, col.var = "cos2", axes = c(1,2),
             gradient.cols = c("orange", "green", "blue"),
             title="Contributo delle variabili alla varianza")


# analisi osservazioni

plot(pca_pizza, cex=0.8, habillage="brand", select="cos2 0.95", title = "Individui che contribuiscono a spiegare il 95% ")               

fviz_pca_ind(pca_pizza, axes = c(1,2), title="Contributo delle osservazioni alla varianza",col.ind = "cos2",
             ggtheme=theme_minimal(base_size = 11),gradient.cols = c("orange", "green", "blue"))

fviz_pca_ind(pca_pizza, axes = c(1,2), title="50 osservazioni con maggiore contributo alla varianza",select.ind = list(cos2 = 50))


# analisi osservazioni e variabili 

fviz_pca_biplot(pca_pizza, 
                pca_pizza, geom.ind = "point", pointshape = 21, pointsize = 2, axes = c(1,2),col.ind = "black", fill.ind = Pizza$brand, palette = "jco", addEllipses = TRUE,
                col.var = "contrib",gradient.cols = c("orange", "green", "blue"),legend.title = list(fill = "brand", color = "Contributo \ndelle variabili"))



# SHINY

PCAshiny(pca_pizza)
