library(mhsmm)
library(highcharter)
library(factoextra)
library(NbClust)
library(mclust)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(MASS)
library(quantreg)
library(GGally)
library(ggcorrplot)
library(plotly)
library(glmnet)
library(vscc)
library(clustvarsel)
library(teigen)
library(ContaminatedMixt)
library(FactoMineR)       
library(FactoInvestigate)    
library(factoextra)
library(corrplot)


##################

# scegliere directory 

load("~/directory/DatiPresentazione2024.RData")

dati = DatiRieti



dati$data = seq(as.Date("2011-01-01"), by = "days", length.out = 365)

dati$mese = format(dati$data, "%B")





# HIGHCHART

highchart() %>%
  hc_chart(type = "line") %>%
  hc_title(text = "Highcharter Concentrazione Agenti Inquinanti") %>%
  hc_xAxis(categories = dati$mese) %>% 
  hc_yAxis(title = list(text = "μg/m3")) %>% 
  hc_add_series(data = dati[,1], name = "Nitrogen dioxide ", type = "line", color = "blue") %>% 
  hc_add_series(data = dati[,2], name = "Sulphur dioxide", type = "line", color = "red") %>% 
  hc_add_series(data = dati[,3], name = "Particulate matter less than 10 μm", type = "line", color = "green") %>% 
  hc_add_series(data = dati[,9], name = "Particulate matter less than 2.5 μm", type = "line", color = "yellow") %>% 
  hc_add_series(data = dati[,4], name = "Ozone", type = "line", color = "purple") %>% 
  hc_add_series(data = dati[,5], name = "Carbon monoxide", type = "line", color = "grey") %>% 
  hc_add_series(data = dati[,6], name = "Toluene", type = "line", color = "orange") %>% 
  hc_add_series(data = dati[,7], name = "Etilbenzene", type = "line", color = "black") %>% 
  hc_add_series(data = dati[,8], name = "Oxylene", type = "line", color = "brown")


highchart() %>%
  hc_chart(type = "line") %>%
  hc_title(text = "Highcharter Valori Agenti Atmosferici") %>%
  hc_xAxis(categories = dati$mese) %>% 
  hc_add_series(data = dati[,10], name = "Wind Speed (m/s)", type = "line", color = "blue") %>% 
  hc_add_series(data = dati[,11], name = "Average Temperature (C)", type = "line", color = "red") %>% 
  hc_add_series(data = dati[,13], name = "Temperature excursion (C)", type = "line", color = "yellow")

highchart() %>%
  hc_chart(type = "line") %>%
  hc_title(text = "Highcharter Pressione Atmosferica") %>%
  hc_xAxis(categories = dati$mese) %>% 
  hc_yAxis(title = list(text = "millibar")) %>% 
  hc_add_series(data = dati[,12], name = "Pressure", type = "line", color = "green")


# CORRELAZIONE 

ggplotly(ggcorr(cor(dati[1:13], use = "complete.obs"), hjust = 1, vjust = 1) +
           ggtitle("Grafico della correlazione delle variabili") 
          
)


# PLOT REGRESSIONE

 
par(mar = c(1, 1, 1, 1))
par(mfrow = c(9, 9))

for (i in colnames(dati[, 1:9])) {
  for (j in colnames(dati[, 1:9])) {
    if (i != j) {
     
        plot(dati[, i], dati[, j], axes = FALSE, frame.plot = TRUE)
        Axis(side = 1, labels = FALSE)
        Axis(side = 2, labels = FALSE)
        
        
        model =lm(dati[, j] ~ dati[, i])
        
       
        abline(model, col = "blue", lwd = 3)
        
        
        predict_interval =predict(model, interval = "confidence", level = 0.95)
        
        
        lines(dati[, i], predict_interval[, "lwr"], col = "green", lty = 2)
        lines(dati[, i], predict_interval[, "upr"], col = "green", lty = 2)
      
    } else {
      
        plot(density(dati[, i]), axes = FALSE, frame.plot = TRUE, main = i)
        Axis(side = 1, labels = FALSE)
        Axis(side = 2, labels = FALSE)
      
    }
  }
}







# Modello Markov 



lista_variabili= list(
  NO2 = dati[,1],
  SO2 = dati[,2],
  PM10 = dati[,3],
  O3 = dati[,4],
  CO =  dati[,5], 
  Toluene =  dati[,6], 
  Etilbenzene =  dati[,7],
  Oxylene =  dati[,8], 
  PM25 =  dati[,9], 
  Wind = dati[,10], 
  Temperature = dati[,11], 
  Pressure = dati[,12], 
  Temperature_Excursion = dati[,13]
  
)


matrice_mu = matrix(c(-1,2, 
                      -1.5,0.5,
                      2.5,3,  
                      3,4,  
                      -3,1, 
                      1,2, 
                      -1.5,0.5,  
                      -1,1,  
                      2,4, 
                      10,20, 
                      10,25, 
                      1010,1020, 
                      10,15), nrow = 13, ncol = 2, byrow = TRUE)





numero_parametri =c()  

for (i in 2:3) {
  numero =(i - 1) + i * (i - 1) + i
  
  numero_parametri =c(numero_parametri, numero)
}


par(mfrow= c(3,2))

for (v in 1:length(lista_variabili)) {
  
  
  
  for (K in 2:3) {
    
    
    
    valori_iniziali =hmmspec(init = rep(1/K, K), trans = matrix(1/K, nrow = K, ncol = K), parms.emis = list(mu = seq(matrice_mu[v,][1], matrice_mu[v,][2], length.out = K), sigma=rep(1,K)),dens.emis = dnorm.hsmm)
    
    modello_Hidden_Markov =hmmfit(lista_variabili[[v]], valori_iniziali, mstep = mstep.norm)
    aic = -2*modello_Hidden_Markov$loglik[length(modello_Hidden_Markov$loglik)]+2*numero_parametri[K-1]
    
    
    cat("\n", "----------------------------------------------------", "\n")
    cat("variabile: ", names(lista_variabili[v])," - ", "Numero di Stati: ", K,  "\n", "\n")
    print(modello_Hidden_Markov$model)
    cat("Matrice di Tansizione: ", "\n", modello_Hidden_Markov$model$transition, "\n", "\n")
    cat("Diagonale principale della Matrice di Transizione : ", "\n", diag(modello_Hidden_Markov$model$transition), "\n", "\n")
    cat("Previsione del modello: ", "\n", modello_Hidden_Markov$yhat, "\n", "\n")
    cat("Indice AIC: ", aic, "\n", "\n" )
    
    
    
    plot(dati$data, dati[, v], col = modello_Hidden_Markov$yhat, pch = 16, xlab = "Data", ylab = names(dati)[v])
  
    mtext(text = paste(names(dati)[v]," - Aic:", round(aic,2)), side = 3, line = -2, adj = 0.5)
    
    
    for (n in 1:K) {
      abline(h=modello_Hidden_Markov$model$parms.emission$mu[n],col=n)
    }
  }
}











# SEMI Markov Model



numero_parametri =c()  

for (i in 2:4) {
  numero =(i - 1) + i * (i - 1) + i
  
  numero_parametri =c(numero_parametri, numero)
}


par(mfrow= c(3,2))

for (v in 1:length(lista_variabili)) {
  
  
  
  for (K in 3:4) {
    
    matrice_trans = matrix(1/(K-1), nrow = K, ncol = K)
    diag(matrice_trans) = 0
    
    valori_iniziali_SMM =hsmmspec(init = rep(1/K, K), trans = matrice_trans, parms.emis = list(mu = seq(matrice_mu[v,][1], matrice_mu[v,][2], length.out = K), sigma=rep(1,K)),sojourn = list(lambda = rep(0.5,K), shift = rep(1,K), type= "poisson"), dens.emis = dnorm.hsmm)
    
    modello_Hidden_Markov_SMM =hsmmfit(lista_variabili[[v]], valori_iniziali_SMM, mstep = mstep.norm)
    aic = -2*modello_Hidden_Markov_SMM$loglik[length(modello_Hidden_Markov_SMM$loglik)]+2*numero_parametri[K-1]
    
    
    cat("\n", "----------------------------------------------------", "\n")
    cat("variabile: ", names(lista_variabili[v])," - ", "Numero di Stati: ", K,  "\n", "\n")
    print(modello_Hidden_Markov_SMM$model)
    cat("Matrice di Tansizione: ", "\n", modello_Hidden_Markov_SMM$model$transition, "\n", "\n")
    cat("Diagonale principale della Matrice di Transizione : ", "\n", diag(modello_Hidden_Markov_SMM$model$transition), "\n", "\n")
    cat("Previsione del modello di SEMI Markov: ", "\n", modello_Hidden_Markov_SMM$yhat, "\n", "\n")
    cat("Indice AIC: ", aic, "\n", "\n" )
    
    
    
    plot(dati$data, dati[, v], col = modello_Hidden_Markov_SMM$yhat, pch = 16, xlab = "Data", ylab = names(dati)[v])
    
    mtext(text = paste(names(dati)[v]," - Aic:", round(aic,2)), side = 3, line = -2, adj = 0.5)
    
    
    for (n in 1:K) {
      abline(h=modello_Hidden_Markov_SMM$model$parms.emission$mu[n],col=n)
    }
  }
}




# CLUSTERING GERARCHICO per stagioni 


dati$stagioni =cut(dati$data, 
                     breaks = as.Date(c("2011-01-01", "2011-03-20", "2011-06-21", "2011-09-22", "2011-12-21", "2011-12-31")), 
                     labels = c( "Inverno", "Primavera", "Estate", "Autunno", "Inverno"), 
                     include.lowest = TRUE)


selezione_variabili_clustvarsel =clustvarsel(dati[,1:13],G=2)

selezione_variabili_clustvarsel$subset



deondogramma = hclust(dist(dati[selezione_variabili_clustvarsel$subset], method = "euclidean"), method = "ward.D2")

fviz_dend( deondogramma, palette = c("green", "blue", "red", "yellow"),k = 2,  rect = T, show_labels = F) 


fviz_cluster(list(data = dati[,1:13], cluster = (cutree(deondogramma, k = 2))), geom = "point", palette = c("green", "blue", "red", "yellow"))

(matrice_confusione_cluster_gerarchico = table(dati$stagioni, cutree(deondogramma, k = 2)))




# REGRESSIONE per temperatura media

dev.off()

modello_regressione_normale =glmnet(as.matrix(scale(dati[,1:10])), y = dati$Average.Temp,  family = "gaussian",    alpha = 0.5,  lambda = .5) 

coefficienti = coef(modello_regressione_normale, s = 0.1)

barplot(coefficienti[,1][-1], col = "aquamarine2", main = "Coefficienti della Rgressione Lasso", 
        xlab = " ", ylab = "Valore del Coefficiente")



# PCA 

dati_pca = dati[,-14]

pca_dati_rieti = PCA(dati_pca, quali.sup = c(14,15), graph = FALSE )
summary(pca_dati_rieti)


# correlazione variabili - dimensioni

corrplot(pca_dati_rieti$var$cor, method="ellipse",tl.col="black", tl.cex = 0.9, 
         cl.cex = 0.5, cl.align.text="l", 
         title = "Grafico della correlazione  tra le Dimensioni e le variabili con ellissi",
         mar = c(1, 0, 5, 1))


fviz_pca_biplot(pca_dati_rieti, geom.ind = "point",pointshape = 21, pointsize = 2, col.var = "black", col.ind = dati_pca$stagioni, addEllipses = TRUE)


# variabili

fviz_pca_var(pca_dati_rieti, col.var = "cos2", axes = c(1,2),
             gradient.cols = c("orange", "green", "blue"),
             title="Contributo delle variabili alla varianza")


# osservazioni

fviz_pca_ind(pca_dati_rieti, axes = c(1,2), col.ind = dati_pca$stagioni, title="50 osservazioni con maggiore contributo alla varianza",select.ind = list(cos2 = 50))




# clustering pca

pca_clustering = HCPC(pca_dati_rieti, graph = FALSE)

fviz_pca_biplot(pca_dati_rieti, col.var = "black", addEllipses = TRUE,col.ind = pca_clustering$data.clust$clust, )


