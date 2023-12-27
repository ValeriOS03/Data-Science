
# n_sim = numero di simulazioni
# n = numero di osservazioni
# p = numero di variabili predittive


sim_reg = function(n = 150, p = 4){
  
  matr_cov = matrix(c(1,-0.5,0.2,0.8,
                      -0.5,1,0.3,-0.4,
                      0.2,0.3,1,0.6,
                      0.8,-0.4,0.6,1),p,p)          # matrice di covarianza, una matrice simmetrica rispetto alla diagonale principale
  
  X =  MASS::mvrnorm(n, mu = rep(0, p), Sigma = matr_cov)  # variabili indipendenti
  
  beta = c(0.4,0.6,0.3,0.9)      # coefficenti veri
  
  epsilon = rnorm(n,1,0.2)       # errori
  
  Y = X %*% beta + epsilon         # variabili dipendenti (prod matriciale)
  
  modello = lm(Y~X)               # stimo i coefficenti e l'intercetta della retta di regressione 
  
  beta_cappello = coef(modello)[-1]   # prendo i coefficienti stimati rimuovendo l'intercetta
  
  Y_cappello = X %*% beta_cappello + coef(modello)[1]          #calcolo il valore del modello previsto
  
  confronto = rbind(beta, beta_cappello)
  
  MSE = mean((Y_cappello-Y)^2)          #calcolo la varianza residua, MSE
  
  RMSE = sqrt(MSE)          #calcolo il RMSE
  
  int_conf = confint(modello, level = 0.95)[-1,]
  

  
  return(list(beta_hat = beta_cappello, beta_veri = beta, confronto, intervalli_confidenza = int_conf, Mean_squared_error = MSE, Root_mean_squared_error = RMSE))
  
}
  

  
  
  

rep_sim = function(n_sim = 2000, p = 4){
  
  matrice_coef = matrix(rep(0, p*n_sim),n_sim,p)      #creo la matrice dei coefficienti vuota
  
  for (i in 1:n_sim){
    
    set.seed(i)          #imposto ogni volta un seed diverso
    
    matrice_coef[i,] = sim_reg()$beta_hat   # inserisco i coefficienti trovati nella matrice delle simulazioni
    
  }
  
  # modifico il nome delle colonne e delle righe della matrice
  
  colnames(matrice_coef) <- paste0(" beta stimato nr.", 1:p)
  rownames(matrice_coef) <- paste0("simulazione nr.", 1:n_sim)
  
  
  return(list(matrice_coefficienti = matrice_coef))
}  






analisi_distribuzione = function(p= 4){
  
  par(mfrow = c(2, 2))                 #divido lo spazio per meterci 4 grafici
  
  
  for (j in 1:p) {
    hist(rep_sim()$matrice_coefficienti[,j], main = paste0("beta ", j), xlab = "Coefficienti stimati", ylab = "numero di simulazioni")
    abline(v = sim_reg()$beta_veri[j], col = "orange", lwd = 3, lty = 2)

    # confronto i coefficienti ottenuti dalle 2000 simulazioni con i coefficienti veri
    
  }
  
  #eseguo il test di Kolmogorov-Smirnov per vedere se i beta stimati si distribuiscono come normali nell'arco delle n_sim simulazioni
  
  risposte = NULL 
  
  for (j in 1:p) {
    beta_test = rep_sim()$matrice_coefficienti[,j]
    risultato_ks = ks.test(beta_test, "pnorm", mean(beta_test), sd(beta_test))
    
    if (risultato_ks$p.value >= 0.05) {
      risultato = TRUE
      distribuzione = "si distribuiscono come una normale"
    } 
    else {
      risultato = FALSE
      distribuzione = "non si distribuiscono come una normale"
    }
   
    
    risposte = c(risposte, sprintf("Risultato test di Kolmogorov-Smirnov per il beta %d = %s quindi i beta %d ricavati dalle precedenti simulazioni %s attorno al beta %d", j, risultato,j,distribuzione,j))
  }
  
  return(risposte)
}


