
# varibili dataset --------------------------------------------------------

#WINE
# Tipo, Alcol, Estratto senza zucchero, Acidità fissa, Acido tartarico, Acido malico, Acidi uronici, PH, Cenere, Alcalinità delle ceneri, Potassio, Calcio, Magnesio, Fosfato, Cloruro, Fenoli totali, Flavanoidi, Fenoli non flavanoidi, Proantociani, Intensità del colore, Tinta, Numero di vini diluiti, Numero dei flavanoidi, Glicerolo, Butandiolo, Azoto totale, Prolina, Metanolo

#F.VOLES
# Specie, età, Lunghezza incisiva del condilo, Lunghezza del forame incisivo, Lunghezza alveolare della fila dei denti molari superiori, Larghezza zigomatica, Larghezza interorbitale, Altezza del cranio

# OLIVE
# I dati sull'olio di oliva consistono nella composizione percentuale di 8 acidi grassi (palmitico, palmitoleico, stearico, oleico, linoleico, linolenico, arachidico, eicosenoico) presenti nella frazione lipidica di 572 oli di oliva italiani. Le aree di raccolta sono 9, 4 del Sud Italia (Nord e Sud Puglia, Calabria, Sicilia), due della Sardegna (Interna e Costiera) e 3 del Nord Italia (Umbria, Liguria Orientale e Occidentale).

# COFFEE
# varietà, nazionalità, acqua, peso del chicco, resa dell'estratto, valore ph, acido libero, contenuto di minerali, grasso, caffeina, acido trigonellino, acido clorogenico, acido neoclorogenico, acido isoclorogenico


# Librerie ----------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(magrittr)
library(plotly)
library(gridExtra)
library(NbClust)
library(cluster)  
library(pamr)
library(factoextra)


set.seed(1234)

# import ------------------------------------------------------------------


dataset = file.path(getwd(), "ClusterData_L31.RData")    # carico i dataset (deve stare nella stessa directory di questo file)
load(dataset)

input = readline(prompt = "Scegli il dataset tra i seguenti: coffee, f.voles, wine, olive   \n")            # faccio scegliere il dataset al utente (scrivere nella console)     
data_original = get(input)                    # estraggo il dataset scelto

if (input == "wine") {                                #rimuovo eventuali colonne e faccio scaling per i differenti dataset
  data = data.frame(scale(data_original[,-1]))
} else if (input == "coffee" || input == "olive") {
  data = data.frame(scale(data_original[,-c(1,2)]))
} else if (input == "f.voles") {
  data = data.frame(scale(data_original[,-1]))
  data_original$Species = as.numeric(factor(f.voles$Species))
} 


# bilanciamento -----------------------------------------------------

if (input != "olive") {                         # se ho scelto un dataset supervisionato
  
  classi = unique(data_original[,1])               # verifico bilanciamento dataset
  conteggio_classi = list()                    # lista con numero di istanze per classe 
  
  for (i in 1:length(classi)) {
    conteggio_classi[[i]] = sum(data_original[,1] == classi[i])
  }
  
  rapporto_classi = list()                    # lista con rapporto delle classi alla prima classe
  
  for (i in 2:length(classi)) {
    rapporto_classi[[i]] = conteggio_classi[[i]] / conteggio_classi[[1]]
  }
  
  bilanciato = TRUE                # variabile bilanciato, se rapporto sta tra 0,5 e 1,5 è bilanciato altrimenti bilanciato = FALSE
  
  for (i in 2:length(classi)) {
    if (!(rapporto_classi[[i]] >= 0.5 && rapporto_classi[[i]] <= 1.5)) {
      cat("Il dataset non è bilanciato.")
      bilanciato <- FALSE
      break  # Esci dal ciclo se trovi una classe non bilanciata
    }
  }
  
  if (bilanciato) {
    cat("Il dataset è bilanciato.")
  }
}
  


# grafici -----------------------------------------------------------------

if (input == "olive") {
  
  
  #Acidi saturi (dannosi per la salute): Acido Palmitico, Acido Stearico ecc
  #Acidi insaturi (buoni per la salute): Acido Linoleico, Acido Palmitoleico, Acido Oleico ecc
  
  #Region 1: Sud Italia
  #Region 2: Sardegna
  #Region 3: Nord Italia
  
  #Area 1: Nord Puglia
  #Area 2: Calabria
  #Area 3: Sud Puglia 
  #Area 4: Sicilia
  
  SudItalia <- olive %>%
    filter(Region =='1' ) %>%
    mutate(Area = as.character(Area)) %>%
    mutate(Area = case_when(
      Area == "1" ~ "Nord Puglia",
      Area == "2" ~ "Calabria",
      Area == "3" ~ "Sud Puglia",
      Area == "4" ~ "Sicilia",
      TRUE ~ Area  # Mantieni gli altri valori invariati
    ))
  
  
  #Grafici acidi insaturi
  
  grafico_olive_1 <- ggplot(SudItalia, aes(x = Oleic ,y = Palmitoleic, group=Region))+
    geom_point(aes(colour = Area))+
    ylab("Acido Palmitoleico")+
    xlab("Acido Oleico")+
    ggtitle("Acido Oleico e Palmitoleico nell'olio di oliva italiano nel Sud Italia")+
    theme_grey()
  print(ggplotly(grafico_olive_1))
  
  #Grafici acidi saturi
  
  grafico_olive_2 <- ggplot(SudItalia, aes(x = Stearic ,y = Palmitic, group=Region))+
    geom_point(aes(colour = Area))+
    ylab("Acido Palmitico")+
    xlab("Acido Stearico")+
    ggtitle("Acido Stearico e Palmitico nell'olio di oliva italiano nel Sud Italia")+
    theme_grey()
  print(ggplotly(grafico_olive_2))
  
  
  Sardegna <- olive %>%
    filter(Region =='2' )
  
  #Grafici acidi insaturi
  grafico_olive_3 <- ggplot(Sardegna, aes(x = Oleic ,y = Palmitoleic, group=Region))+
    geom_point(aes(colour = as.factor(Area)))+
    ylab("Acido Palmitoleico")+
    xlab("Acido Oleico")+
    ggtitle("Acido Oleico e Palmitoleico nell'olio di oliva italiano in Sardegna")+
    guides(color = guide_legend(title = "Area")) +
    theme_light()
  print(ggplotly(grafico_olive_3))
  
  #Grafici acidi saturi
  grafico_olive_4 <- ggplot(Sardegna, aes(x = Stearic ,y = Palmitic, group=Region))+
    geom_point(aes(colour = as.factor(Area)))+
    ylab("Acido Palmitico")+
    xlab("Acido Stearico")+
    ggtitle("Acido Stearico e Palmitico nell'olio di oliva italiano in Sardegna")+
    guides(color = guide_legend(title = "Area")) +
    theme_light()
  print(ggplotly(grafico_olive_4))
  
  
} 


if (input == "wine") {
  
  #Barplot Wine: Media di Total Phenols per Classe di Alcool 
  data_original$alcohol_class <- cut(data_original$Alcohol, breaks = 5)
  total_phenols_means <- aggregate(data_original$`Total Phenols`, by = list(data_original$alcohol_class), FUN = mean)
  total_phenols_means = merge(total_phenols_means, wine$Type)
  colnames(total_phenols_means) <- c("Alcohol_Class", "Average_Total_Phenols", "Type")
  grafico_wine_1 <- ggplot(total_phenols_means, aes(x = Alcohol_Class, y = Average_Total_Phenols, fill = as.factor(Type))) +
    geom_bar(stat = "identity") +
    labs(title = "Media di Total Phenols per Classe di Alcool", x = "Classe di Alcool", y = "Media di Total Phenols") + 
    scale_fill_discrete(name = "Tipologia di Vino") + theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ruota le etichette di 45 gradi
  ggplotly(grafico_wine_1)
  
  
  # Crea un grafico di densità con alcohol sull'asse x e Intensità di Colore sull'asse y
  grafico_wine_2 <- ggplot(wine, aes(x = Alcohol, y = `Color Intensity`)) +
    geom_density_2d_filled() + geom_point() +
    labs(title = "Densità di Alcohol e Intensità di Colore", x = "Alcohol", y = "Intensità di Colore") +
    theme_minimal() +
    scale_y_continuous(breaks = seq(1,13))
  grafico_wine_2
  

  

  
  
}   
if (input == "f.voles") {
  

  
  # Crea un grafico di densità con dimensioni arcata zigomatica e osso alveolare
  grafico_f.voles_1 <- ggplot(f.voles, aes(x = B3.Zyg, y = L7.Alveolar)) +
    geom_density_2d_filled() + geom_point() +
    labs(title = "Dimensioni arcata zigomatica e osso alveolare", x = "arcata zigomatica", y = "osso alveolare") +
    theme_minimal()
  print(grafico_f.voles_1)
  

  
  # Crea un grafico a barre con forame incisivo sull'asse x e "Species" sull'asse y-----------------
  grafico_f.voles_2 <- ggplot(f.voles, aes(x = L9.Inc.Foramen, fill = Species)) +
    geom_bar() +
    labs(x = "Lunghezza del forame incisivo", y = "Quantità di osservazioni", title = "Dimensioni forame incisivo in base alla specie") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  print(grafico_f.voles_2)
  
}   
if (input == "coffee") {
  
  colori <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
    "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
    "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", "#c49c94",
    "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5", "#aec7e8",
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
    "#e377c2", "#7f7f7f", "#bcbd22"
  )
  
  
  # Grafico che mostra la presenza di acqua nel caffè in base alla nazionalità
  grafico_coffee_1 <- ggplot(coffee, aes(x = Variety, y = Water , fill = Country)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Presenza di acqua in base alla nazionalità",
         x = "Varietà",
         y = "Concentrato di acqua") + scale_color_manual(values = colori)+
    scale_x_discrete(breaks = NULL) 
  print(ggplotly(grafico_coffee_1))
  
  
  # Grafico a barre con la concentrazione di caffeina secondo la nazionalità
  grafico_coffee_2 <- ggplot(coffee, aes(x = Country, y = Caffine, fill = Country)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Concentrato di caffeina secondo la nazionalità",
         x = "Nazionalità",
         y = "Concentrato di caffeina") + scale_color_manual(values = colori) +
    scale_x_discrete(breaks = NULL)
  print(ggplotly(grafico_coffee_2))
  
}   



# NbClust -----------------------------------------------------------------

misure_distanza = c("euclidean","manhattan","minkowski")                 #liste con misure di distanza e metodi 
 


                                            
{  
  metodi_gerarchici_2 = c("single","complete","average","ward.D", "kmeans")
  lista_cluster = list()
  
  for (misura in misure_distanza) {                           
    
    for(metodo in metodi_gerarchici_2) {
      
      
      cat("misura di distanza = ",misura, ",",  "metodo = ", metodo,"\n")          
      nbclust = NbClust(data, distance = misura, method = metodo)                     # calcolo il numero di cluster previsti, per ogni misura e metodo, con nbclust
      
      n_cluster_element= max(nbclust$Best.partition)                              # prendo i numeri di cluster suggeriti per ogni nbclust e li metto in una lista
      lista_cluster = c(lista_cluster, n_cluster_element)  
      
    }
  }
  tabella_cluster = table(unlist(lista_cluster))                    # mi costruisco la tabella con il numero dei cluster suggeriti e quante volte sono stati suggeriti
  n_cluster = as.integer(names(which.max(tabella_cluster)))               # prendo il numero di cluster suggerito più volte
  cat("il numero di cluster più probabile é: ", n_cluster) 
  print(tabella_cluster)
  barplot(tabella_cluster)
}

# come leggere grafici nbclust: 
# def DINDEX: Dindex è una metrica di validazione del clustering che tiene conto della densità all'interno dei cluster e della separazione tra i cluster. (sarebbe derivata di index)
# def SECOND DIFFERENCES DINDEX VALUES: la "differenza di secondo ordine" delle Dindex Values rappresenta il tasso di variazione delle variazioni nei valori dell'indice Dindex al variare del numero di cluster (sarebbe derivata seconda)
# primo grafico: Il punto in cui la curva delle Dindex Values inizia a cambiare pendenza è detto punto di flesso o gomito e indica il punto in cui il numero di cluster è equilibrato tra densità e separazione
# secondo grafico: cerchiamo il punto in cui le differenze di secondo ordine raggiungono un massimo locale o un punto di flesso. Questo punto è indicato come il "gomito" del grafico e rappresenta il numero ottimale di cluster






# metodi gerarchici -------------------------------------------------------


metodi_gerarchici = c("single","complete","average","ward.D")           
i = 0
lista_risultati <- list()                                           # lista dove mettere i deondogrammi 
matrice_cluster <- matrix(NA,nrow=nrow(data),ncol=(length(misure_distanza)*length(metodi_gerarchici)))      #matrice dove mettere i deondogrammi tagliati   
j = 0


if (input != "olive") {
  
  for (misura in misure_distanza) {
    
    distanza <- dist(data, method = misura)           # calcolo la distanza con una delle misure alla volta
    
    for(metodo in metodi_gerarchici) {            # applico un metodo per ogni misura di distanza 
      i <- i+1
      lista_risultati[[i]] <- hclust(distanza,method = metodo)        # metto il deodnogramma nella lista 
      plot(lista_risultati[[i]], main = paste("misura di distanza = ",misura, "\n", "metodo = ", metodo))          # plotto i deondogramma 
      
      matrice_cluster[,i] <- cutree(lista_risultati[[i]],k=(length(unique(data_original[,1]))))             # taglio il deondogramma in base al numero di classi (nella prima variabile)
    
      plot(data, col = matrice_cluster[,i], main = paste(input, "\n","misura di distanza = ",misura, "\n", "metodo = ", metodo))      # plotto il dataset colorato con i risultati del taglio del deondogramma
      print(paste("misura di distanza = ",misura, "| ", "metodo = ", metodo))        
      
      matrice_confusione = table(data_original[,1],matrice_cluster[,i])         # faccio la matrice di confusione 
      print(matrice_confusione)
      cat("\n")
      if (bilanciato == TRUE) {                                                   # se dataset bilanciato calcolo accuracy totale
        Accuracy = sum(diag(matrice_confusione)) / sum(matrice_confusione)       
        print(paste("Accuracy = ", Accuracy))
      } else{                                                                    # altrimenti calcolo accuracy per ogni classe
        for (classe in 1:nrow(matrice_confusione)) {
          accuratezza_per_classe = matrice_confusione[classe, classe] / sum(matrice_confusione[classe, ])
          cat("Acc per classe", classe, ": ", accuratezza_per_classe, "\n")
        }
        
      }
      
    }
  }
} else if (input == "olive") {
  
  lista_combinazioni = list()
  
  for (n in seq(2, n_cluster+5)) {                 # provo a clusterizzare i dati a partire da due cluster fino a (numero dei clusetr previsti da nbclust) +5
    
    i = 0
    
    for (misura in misure_distanza) {
      
      distanza <- dist(data, method = misura)           # calcolo la distanza con una delle misure alla volta
      
      for(metodo in metodi_gerarchici) {           # applico un metodo per ogni misura di distanza  
        i <- i+1
        lista_risultati[[i]] <- hclust(distanza,method = metodo)        # metto il deodnogramma nella lista 
        plot(lista_risultati[[i]], main = paste(input, "\n", "misura di distanza = ",misura, "\n", "metodo = ", metodo))          # plotto i deondogramma 
        
        risultato_silhouette <- silhouette(cutree(lista_risultati[[i]], k = n), distanza)                                                 #introduco un nuovo indice, l'indice silhouette, utilizzato per clustering non supervisionato 
        i_sil = mean(risultato_silhouette[, "sil_width"])                                                       # valori vicini a 1 indicano assegnazione perfetta ai cluster, vicini a 0 indicano sovrapposizione tra i cluster, vicini a -1 indicano assegnazione errata ai cluster
                                                                                                   #l'indice silhouette viene calcolato per ogni osservazione, quindi ne facciamo una media 
        lista_combinazioni = c(lista_combinazioni, paste(misura, metodo, n, i_sil))                        # appendo nella lista combinazioni, le varie combinazioni di misura, metood, indice silhouette, mi servirà dopo per decretare la migliore combinazione                 
        
        cat("Indice silhouette per la misura: ", misura, "per il metodo: ", metodo, "e per ", n, " cluster è: ", i_sil, "\n")        
        
        
        matrice_cluster[,i] <- cutree(lista_risultati[[i]],k=n)                                                        # taglio il deondogramma in base al numero di cluster (ottenuto con nbclust)
    
        plot(data, col = matrice_cluster[,i], main = paste("misura di distanza = ",misura, "\n", "metodo = ", metodo, "\n", "numero di cluster = ", n))      # plotto il dataset colorato con i risultati del taglio del deondogramma
      }
    }
  }
  
  valori_numerici <- as.numeric(sub(".*\\s", "", lista_combinazioni))          # la funzione sub è l'equivalente di regex, in questo caso rimuove tutto ciò che precede l'ultimo spazio e trasfoma quello che rimane (il valore dell'indice silhouette) in numeric
  indice_max <- which.max(valori_numerici)                            # prendo indice del indice di silhouette massimo trovato prima
  comb_best <- as.character(lista_combinazioni[indice_max])                # prendo la riga della lista combinazione con quell'indice, in modo da poter stampare la combinazione migliore 
  
  cat( "\n", "la combinazione di misura e metodo e numero di cluster migliore è: ", comb_best,  "\n")       
}


# metodi ripartizione - Kmean e Pam -----------------------------------------------------



if (input != "olive") {
    
  k_medie = kmeans(data,centers=(max(dim(unique(data_original[1])))),nstart = 20)                          # calcolo i k mean ( con numero di cluster pari al numero delle classi della prima variabile) 
  print(fviz_cluster(k_medie, data = data, geom = "point", stand = FALSE, main = "Kmeans"))
  
  #plot(data,col=k_medie$cluster, main = "Kmeans")                                                                # plotto dataset col colore asseganto dai k mean 
  tabella_confusione_kmeans = table(data_original[,1],k_medie$cluster)                           #creo tabella di confusione 
  
  etichette_corrette <- sapply(1:nrow(tabella_confusione_kmeans), function(i) {                                        # dato che k mean assegna etichette casuali alle classi, riordino le classi in modo da poter calcolare accuracy su diagonale principale  
    etichetta_cluster_corretta <- rownames(tabella_confusione_kmeans)[which.max(tabella_confusione_kmeans[i, ])]        # applico a tutte le righe della tabella di confusione una funzione che prende per ogni riga l'indice (della colonna) del numero più grande
  })
  tabella_corretta = tabella_confusione_kmeans[, etichette_corrette]                              # riordino la tabella con il nuovo ordine di indici 
  print(tabella_corretta)
  
  if (bilanciato == TRUE) {                                                  # come prima se dataset è bilanciato calcolo accuracy totale, altrimenti calcolo accuracy per classe 
    Accuracy = sum(diag(tabella_corretta)) / sum(tabella_corretta)
    print(paste("Accuracy = ", Accuracy))
  } else {
    for (classe in 1:nrow(tabella_corretta)) {
      accuratezza_per_classe = tabella_corretta[classe, classe] / sum(tabella_corretta[classe, ])
      cat("Acc per classe", classe, ": ", accuratezza_per_classe, "\n")
    }
  }
} else if (input == "olive") {
  
  for (n in seq(2, n_cluster+5)) {
  
    k_medie = kmeans(data,centers=n,nstart = 20)                          # calcolo i k mean ( con numero di cluster da 2 a  (numero dei clusetr previsti da nbclust) +5) 
    print(fviz_cluster(k_medie, data = data, geom = "point", stand = FALSE, main = "Kmeans"))
    
    #plot(data,col=k_medie$cluster, main = "Kmeans")                   # plotto i dati e coloro le varie osservazioni in base alla loro assegnazione nelle classi determinata dal clustering
    
    valori_silhouette_kmean <- silhouette(k_medie$cluster, dist(data))            #calcoliamo l'indice silhouette anche per i kmean calcolati variando il numero di cluster
    
    cat(n, mean(valori_silhouette_kmean[, "sil_width"]), "\n")                 # stampiamo i numero di cluster e i relativi indici di silhouette
     
  } 
} 
  
  
  

if (input != "olive") {
  
  pam = pam(data,k = (max(dim(unique(data_original[1])))),nstart = 50)                   # calcolo il pam ( con numero di cluster pari al numero delle classi della prima variabile) 
  print(fviz_cluster(pam, data = data, geom = "point", stand = FALSE, main = "Pam"))
  
  #plot(data,col=pam$clustering, main = "Pam")                                                                # plotto dataset col colore assegnato dai pam 
  tabella_confusione_pam = table(data_original[,1],pam$clustering)                           #creo tabella di confusione 
  print(tabella_confusione_pam)
  
  if (bilanciato == TRUE) {                                                           # come prima se dataset è bilanciato calcolo accuracy totale, altrimenti calcolo accuracy per classe 
    Accuracy = sum(diag(tabella_confusione_pam)) / sum(tabella_confusione_pam)
    print(paste("Accuracy = ", Accuracy))
  } else {
    for (classe in 1:nrow(tabella_confusione_pam)) {
      accuratezza_per_classe = tabella_confusione_pam[classe, classe] / sum(tabella_confusione_pam[classe, ])
      cat("Acc per classe", classe, ": ", accuratezza_per_classe, "\n")
    }
  }
} else if (input == "olive") {
  
  for (n in seq(2, n_cluster+5)) {
  
    pam = pam(data,k = n,nstart = 50)                   # calcolo il pam ( con numero di cluster da 2 a  (numero dei clusetr previsti da nbclust) +5) 
    print(fviz_cluster(pam, data = data, geom = "point", stand = FALSE, main = "Pam"))
    
    #plot(data,col=pam$clustering, main = "Pam")                  # plotto i dati e coloro le varie osservazioni in base alla loro assegnazione nelle classi determinata dal clustering
    
    valori_silhouette_pam <- silhouette(pam$cluster, dist(data))        #calcoliamo l'indice silhouette anche per i kmean calcolati variando il numero di cluster
    
    cat(n, mean(valori_silhouette_pam[, "sil_width"]), "\n")            # stampiamo i numero di cluster e i relativi indici di silhouette
  } 
}
