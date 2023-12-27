library(mhsmm)
library(highcharter)
library(factoextra)
library(NbClust)
library(mclust)
library(kableExtra)
# import ------------------------------------------------------------------



dati <- data.frame(read.table("Desktop/R/Data Mining/Serie storiche/births.txt", header = TRUE))


date_iniziale <- as.Date("1977-02-01")
date_finali <- as.Date("1986-01-01")
date_mensili <- seq.Date(from = date_iniziale, to = date_finali, by = "1 month")

dati$data = date_mensili


lista_variabili= list(
  nascite_totali = dati[,1],
  parti_cesari = dati[,2],
  nascite_podaliche = dati[,3],
  parti_forcide = dati[,4],
  erogazioni_aspirazione =  dati[,5] 
)


# visualization -----------------------------------------------------------

par(mfrow = c(1,5))
boxplot(dati$tot, main = "nascite_totali", ylab = "", col = "red", border = "black", tl.cex = 0.7)
boxplot(dati$cae, main = "parti_cesari", ylab = "", col = "purple", border = "black", tl.cex = 0.7)
boxplot(dati$bb, main="nascite_podaliche", ylab="", col="orange", border="black", tl.cex = 0.7)
boxplot(dati$fd, main="parti_forcide", ylab="", col="green", border="black", tl.cex = 0.7)
boxplot(dati$ve, main="erogazioni_aspirazione", ylab="", col="blue", border="black", tl.cex = 0.7)
dev.off()





par(mfrow = c(1,5))
plot(dati$data,lista_variabili[[1]], type = "l", col = "blue", xlab = "Tempo", ylab = "Valore", main = "nascite totali")
plot(dati$data,lista_variabili[[2]], type = "l", col = "red", xlab = "Tempo", ylab = "Valore", main = "parti cesari")
plot(dati$data,lista_variabili[[3]], type = "l", col = "green", xlab = "Tempo", ylab = "Valore", main = "nascite podaliche")
plot(dati$data,lista_variabili[[4]], type = "l", col = "orange", xlab = "Tempo", ylab = "Valore", main = "parti forcide")
plot(dati$data,lista_variabili[[5]], type = "l", col = "purple", xlab = "Tempo", ylab = "Valore", main = "erogazioni aspirazione")
dev.off()

matrice_lamda = matrix(c(600,800,120,200,15,25,25,50,20,50), nrow = 5, ncol = 2, byrow = TRUE)




highchart() %>%
  hc_chart(type = "line") %>%
  hc_title(text = "Highcharter Nascite Totali") %>%
  hc_xAxis(categories = dati$data) %>%
  hc_yAxis(title = list(text = "Numero di Nascite Totali")) %>% 
  hc_add_series(data = dati[,1], name = "nascite totali", type = "line", color = "green") 
  





highchart() %>%
  hc_chart(type = "line") %>%
  hc_title(text = "Highcharter Tipologia di Parto") %>%
  hc_xAxis(categories = dati$data) %>%
  hc_yAxis(title = list(text = "Numero di Nascite")) %>% 
  hc_add_series(data = dati[,2], name = "parti_cesari", type = "line",color = "purple") %>%
  hc_add_series(data = dati[,3], name = "nascite_podaliche", type = "line",color = "orange") %>%
  hc_add_series(data = dati[,4], name = "parti_forcide", type = "line",color = "green") %>%
  hc_add_series(data = dati[,5], name = "erogazioni_aspirazione", type = "line",color = "blue") 
  











# hidden markov model -----------------------------------------------------


numero_parametri <- c()  

for (i in 2:10) {
  numero <- (i - 1) + i * (i - 1) + i

  numero_parametri <- c(numero_parametri, numero)
}




for (v in 1:length(lista_variabili)) {
  
  
  
  for (K in 2:10) {
    
    
    
    valori_iniziali <- hmmspec(init = rep(1/K, K), trans = matrix(1/K, nrow = K, ncol = K), parms.emis = list(lambda = seq(matrice_lamda[v,][1], matrice_lamda[v,][2], length.out = K)), dens.emis = dpois.hsmm)
    modello_Hidden_Markov <- hmmfit(lista_variabili[[v]], valori_iniziali, mstep = mstep.pois)
    aic = -2*modello_Hidden_Markov$loglik[length(modello_Hidden_Markov$loglik)]+2*numero_parametri[K-1]

    
    try({
      
      K_succ <- K + 1
      valori_iniziali_succ <- hmmspec(init = rep(1/K_succ, K_succ), trans = matrix(1/K_succ, nrow = K_succ, ncol = K_succ), parms.emis = list(lambda = seq(matrice_lamda[v,][1], matrice_lamda[v,][2], length.out = K_succ)), dens.emis = dpois.hsmm)
      modello_Hidden_Markov_succ <- hmmfit(lista_variabili[[v]], valori_iniziali_succ, mstep = mstep.pois)
      aic_mod_succ = -2*modello_Hidden_Markov_succ$loglik[length(modello_Hidden_Markov_succ$loglik)]+2*numero_parametri[K-2]
      
      
      if ((any(diag(modello_Hidden_Markov_succ$model$transition) < 0.01)) | aic_mod_succ > aic) {
        break
      }
      
    })
  }
  
  
  cat("\n", "----------------------------------------------------", "\n")
  cat(names(lista_variabili[v]), "\n", "\n")
  print(modello_Hidden_Markov$model)
  cat("Matrice di Tansizione: ", "\n", modello_Hidden_Markov$model$transition, "\n", "\n")
  cat("Diagonale principale della Matrice di Transizione : ", "\n", diag(modello_Hidden_Markov$model$transition), "\n", "\n")
  cat("Previsione del modello: ", "\n", modello_Hidden_Markov$yhat, "\n", "\n")
  cat("Indice AIC: ", aic, "\n", "\n" )
  
  
          
  plot(modello_Hidden_Markov$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")
    
    
  plot(dati$data,dati[,v],col=modello_Hidden_Markov$yhat, main = paste("K = ", K ),ylab = names(lista_variabili[v]), xlab = "Tempo")
  
  for (n in 1:K) {
    abline(h=modello_Hidden_Markov$model$parms.emission$lambda[n],col=n)
  }
}





# BOOTSTRAP ---------------------------------------------------------------





K = 6
valori_inziali_bootstrap <- hmmspec(init = rep(1/K, K), trans = matrix(1/K, nrow = K, ncol = K), parms.emis = list(lambda = seq(600,800, length.out = K)), dens.emis = dpois.hsmm)
mod_bootstrap <- hmmfit(dati[,1], valori_inziali_bootstrap, mstep = mstep.pois)
plot(dati$data,dati[,1],col=mod_bootstrap$yhat, main = "Markov Model \n nascite totali", xlab = " ", ylab = " ")


#eseguiamo boostrap sulla variabile nascite totali, con una simulazione ripetuta 1000 volte ( per 200 volte simulate ), cambiando ogni volta il seed

Numero_bootstrap <- 1000
lambda_bootstrap <- matrix(NA,6,Numero_bootstrap)

for (n in 1:Numero_bootstrap)
{
  
  # inizializzazione del modello HMM "vero" utilizzando i parametri stimati dal modello HMM originale
  
  parametri_veri <- hmmspec(init = mod_bootstrap$model$init,
                      trans = mod_bootstrap$model$transition,
                      parms.emis = list(lambda = mod_bootstrap$model$parms.emission$lambda),
                      dens.emis = dpois.hsmm)
  
  # genero un nuovo set di dati sintetici (train) utilizzando il modello HMM "vero" con la funzione simulate
  
  dataset_training <- simulate(parametri_veri, nsim = 108, seed = n, rand.emis = rpois.hsmm)
  
  # adatto il modello HMM ai dati sintetici generati
  
  modello_adattato <- hmmfit(dataset_training, parametri_veri, mstep = mstep.pois)
  
  #registrando i valori stimati di λ nella matrice
  
  lambda_bootstrap[,n] <- modello_adattato$model$parms.emission$lambda
}


dataframe_kable <- data.frame(
  Lambda_Bootstrap = apply(lambda_bootstrap,1,mean),
  Lambda_Originali = mod_bootstrap$model$parms.emission$lambda,
  Deviazione_Standard_Lambda_Boostrap = apply(lambda_bootstrap,1,sd)
)



kable(dataframe_kable, caption = "Risultati del Bootstrap", align = "c") %>%
  kable_paper(full_width = F)










# clustering gerarchico  --------------------------------------------------





clustering = NbClust(dati[,1], distance = "manhattan", method = "ward.D2")

(numero_cluster = max(unique(clustering$Best.partition)))

deondogramma = hclust(dist(dati[,1], method = "manhattan"), method = "ward.D2")

fviz_dend( deondogramma, palette = c("green", "blue", "red"),k = numero_cluster,  rect = T, show_labels = F) 

par(mfrow = c(1,2))
plot(dati[,6], dati[,1], col = (cutree(deondogramma, k = numero_cluster)), main = "Clustering gerarchico \n nascite totali \n k => nbclust", xlab = " ", ylab = " ")
plot(dati$data,dati[,1],col=mod_bootstrap$yhat, main = "Markov Model \n nascite totali", xlab = " ", ylab = " ")

par(mfrow = c(1,2))
plot(dati[,6], dati[,1], col = (cutree(deondogramma, k = 6)), main = "Clustering gerarchico \n nascite totali \n k => markov", xlab = " ", ylab = " ")
plot(dati$data,dati[,1],col=mod_bootstrap$yhat, main = "Markov Model \n nascite totali", xlab = " ", ylab = " ")




# clustering model based --------------------------------------------------






clustering_modelbased <- Mclust(dati[,1],G = numero_cluster)
clustering_modelbased_6<- Mclust(dati[,1],G = 6)

par(mfrow = c(1,2))
plot(dati[,6], dati[,1], col = clustering_modelbased$classification, main = "Clustering Model Based  \n nascite totali \n k => nbclust", xlab = " ", ylab = " ")
plot(dati$data,dati[,1],col=mod_bootstrap$yhat, main = "Markov Model \n nascite totali", xlab = " ", ylab = " ")

par(mfrow = c(1,2))
plot(dati[,6], dati[,1], col = clustering_modelbased_6$classification, main = "Clustering Model Based  \n nascite totali \n k => markov", xlab = " ", ylab = " ")
plot(dati$data,dati[,1],col=mod_bootstrap$yhat, main = "Markov Model \n nascite totali", xlab = " ", ylab = " ")











