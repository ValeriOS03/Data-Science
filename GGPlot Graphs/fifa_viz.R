# packages ----------------------------------------------------------------

require(tidyverse)
require(magrittr) 
require(plotly) # per rendere dinamico il grafico
require(maps) # contiene le mappe geofrafiche
require(MAP) # permette di creare le mappe
require(ggplot2)
require(countrycode) # permette di identificare le varie nazioni
require(GGally)
require(dplyr) 
require(ggthemes) # serve per theme_map()
require(viridis) # per la palette di colori
library(gridExtra)




# visualization  -------------------------------------------------------------------

load("~/Desktop/rstudio-export-2/fifa_19_clean.rda") 


# VALERIO -----------------------------------------------------------------


# relazione tra abilità e tra piede destro e sinistro  --------------------


(grafico_pref_foot = ggplotly(data_clean %>% select(Preferred.Foot, LongPassing, ShortPassing, Crossing, Dribbling, Curve, BallControl) %>%
  pivot_longer(c(LongPassing, ShortPassing, Crossing, Dribbling, Curve, BallControl), names_to = "abilita", values_to = "valore") %>% 
  ggplot() + geom_boxplot(aes(x = Preferred.Foot, y = valore, fill = abilita ), position = "dodge")+
  labs(y = "Score", x = "Preferred foot", fill = "Ability", title= "Abilities depending on preferred foot") +
  scale_fill_brewer(palette = "Accent") +
  theme_light() + facet_wrap(~abilita))
)



# grafico di dispersione del salario settimanale in milioni in rel --------


(grafico_reg_position = ggplotly(data_clean %>% ggplot(aes(x = Overall, y = Wage, ids = Name, col = Position)) +
                                  geom_point() +
                                  labs(y = "Overall score", x = "Weekly Wage", fill = "Ability", title= "Scatterplot of Weekly Wage and Overall Score") +
                                  theme_light()) )



# relaione tra BMI e Agilità ----------------------------------------------


rel_bmi = function(posizione = "Striker", abilita = "ShotPower") {
  
  ggplotly(data_clean %>% mutate(BMI = Weight / (Height / 100)^2) %>% filter(Position == posizione) %>% ggplot(aes(x = BMI, y = !!sym(abilita), fill = Work.Rate)) + 
    geom_point() + geom_smooth(method = "lm") +
    labs(title= sprintf("Scatterplot of BMI and %s for %s", abilita, posizione)) +
    theme_light()) 

}
  
#Nella funzione geom_smooth di ggplot2, la parte grigia attorno alla linea di regressione rappresenta l'intervallo di confidenza.
#L'intervallo di confidenza indica l'incertezza della stima della linea di regressione e fornisce un'indicazione di quanto attendibile sia la linea rispetto ai dati.


rel_bmi("Striker", "Curve")




# scatterplot tra coppie di abilità ---------------------------------------



(ability_pairs = ggplotly(data_clean %>% select(SprintSpeed,Dribbling,BallControl,Work.Rate) %>% ggpairs(aes(col = Work.Rate)) +
  scale_fill_brewer(palette = "Paired") +
  theme_light()) )









# DANIELE -----------------------------------------------------------------




# Grafico Dinamico Media dell'Overall per Età ---------------------

# Salvo la media dell'overall gruppato per Età

fifa_mean <- data_clean %>%
  group_by(Age) %>%
  summarize(mean_overall = mean(Overall))

# Creiamo il Grafico, faccio uno scatterplot

# Uso direttamente plot_ly, ma potrei anche usare ggplot() e poi ggplotly()
(fifa_mean %>% plot_ly(y = ~mean_overall, x = ~Age,
                      type = "scatter", mode = "markers", 
                      marker = list(size = 8, opacity = 0.8, color = ~Age, colors = viridis_pal()(length(unique(fifa_mean$Age)))),
                      hoverinfo = "text", 
                      text = ~paste("Età: ", Age, "<br>",
                                    "Media Overall: ", round(mean_overall, 2))) %>% 
  layout(title = "Media dell'Overall per Età", 
         xaxis = list(title = "Età", range = c(15, 40)),
         yaxis = list(title = "Media dell'Overall")))




# Conteggio Giocatori per Nazionalità -------------------------------------

# Salvo le medie dell'Overall, Potential e faccio anche la differenza tra Potential e Overall effettivo (per vedere lo sviluppo del giocatore o quanto potrebbe diventare forte) gruppati per Nazionalità:

overall_data <- data_clean %>% 
  group_by(Nationality) %>% 
  summarise(Count = n(), 
            Avg_Overall = mean(Overall),
            Avg_Potential = mean(Potential),
            Avg_Pot_Diff = mean(Potential-Overall))

# Importo il dataset world che contiene longitudine e latitudine di tutto il mondo
worldmap = map_data("world")
# Mergiamo i dati del dataset world con la variabile overall_data che abbiamo creato prima, perchè contiene il Count dei giocatori per Nazionalità
merged_data <- merge(x = worldmap, y = overall_data, by.x = "region", by.y = "Nationality", all.x = TRUE) %>% arrange(order)

# Grafico conteggio del numero di giocatori per Nazionalità. Lo applichiamo a merged_data perchè lì c'è il Count dei giocatori per Nazionalità:
nat_count <- merged_data %>% ggplot() + aes(x = long, y = lat, group = group, ) +
  geom_polygon(aes(fill = Count)) + scale_fill_viridis_c(na.value = "grey50") +
  labs(fill='Total Player Counts') + theme_map()
ggplotly(nat_count, height = 400, width = 600) # Rendo dinamico il grafico


# Grafici Average Overall, Potential e Differenza tra Potential e  Overall

# Average Overall per Nazionalità
avg_overall <- merged_data %>% ggplot() + aes(x = long, y = lat, group = group) +
  geom_polygon(aes(fill = Avg_Overall)) + scale_fill_viridis_c(na.value = "grey50") +
  labs(fill='Average Overall') + theme_map()
ggplotly(avg_overall, height = 400, width = 600)


# Average Potential per Nazionalità
avg_potential <- merged_data %>% ggplot() + aes(x = long, y = lat, group = group) +
  geom_polygon(aes(fill = Avg_Potential)) + scale_fill_viridis_c(na.value = "grey50") +
  labs(fill='Average Potential') + theme_map()
ggplotly(avg_potential, height = 400, width = 600)

# Average Differenza tra Potenziale e Overall effettivo per Nazionalità
avg_pot_diff <- merged_data %>% ggplot() + aes(x = long, y = lat, group = group) +
  geom_polygon(aes(fill = Avg_Pot_Diff)) + 
  scale_fill_viridis_c(na.value = "grey50") +
  labs(fill='Average Diff') + theme_map()
ggplotly(avg_pot_diff, height = 400, width = 600)



# Confronto tra Messi e Ronaldo -------------------------------------------

# Filtro Cristiano Ronaldo e Messi tra i giocatori
players <- data_clean %>% 
  filter(Name %in% c("Cristiano Ronaldo", "L. Messi")) %>% 
  # Modifico la colonna Name incollandoci il Nome e il Club del giocatore
  mutate(Name = paste0(Name, ", ", Club)) %>%
  # Seleziono ciò che ci serve per il confronto, Nome e Abilità da Crossing a Short Passing
  select(Name,Crossing:ShortPassing) %>% 
  # Correggo la punteggiatura
  # Tranform from Variable to Observation
  gather(Skill, Exp, Crossing:`ShortPassing`, -Name)

# Layout del grafico
options(repr.plot.width = 15, repr.plot.height = 8)

# Grafico effettivo Messi VS Ronaldo secondo le Skills
(mvsr <- players %>% ggplot() + aes(Skill, Exp, fill = Name)+
  geom_col(position = "fill")+
  coord_flip()+
  scale_fill_manual(values = c("red3", "lightblue"))+
  theme_minimal()+
  geom_hline(yintercept = 0.5, color = "black", linewidth = 1, linetype = 2)+
  theme(legend.position = "top", axis.text.x=element_blank())+
  labs(title = "Messi VS Ronaldo", 
       caption = "@AnalistiAnonimi",
       fill = NULL,x = NULL, y = NULL))


# Grafico di Correlazione tra Age e Wage -----------------------------

# Possiamo vedere che abbiamo una correlazione "neutra" (anche se leggermente positiva), quindi all'aumentare di X (Età) la Y (Stipendio) aumenta e diminuisce. 

age_Wage <- data_clean %>% select(Age, Wage) # seleziono dal dataset solo le colonne Age e Wage
age_wage_lm <- age_Wage %>% ggplot() + 
  aes(x = Age, y = Wage, color = Age) +  # Specificare l'estetica di colore e la variabile su cui scalare il colore
  geom_point() +
  labs(title = "Age vs Wage", x = "Age", y = "Wage (in migliaia di euro settimanali)") + 
  scale_color_viridis_c() +  # Applicare lo scale di colore all'estetica specificata
  theme_bw()

ggplotly(age_wage_lm)









# AURELIO -----------------------------------------------------------------
# Correlazione fra Peso e Altezza diviso in base al Work Rate --------------------------------------------------------------

# Per osservare la correlazione del peso con l'altezza
ggplotly(data_clean %>% ggplot(aes(Height, Weight, col=Work.Rate)) +
           geom_smooth() + # Aggiunge una curva di regressione ai dati      
           scale_fill_brewer(palette="Spectral") +
           theme_light() +
           labs(title="Height vs Weight",
                x = "Height",
                y = "Weight") + facet_wrap(~Work.Rate)) # Questo codice crea una visualizzazione separata per ogni valore univoco nella variabile 

# Correlazione fra Overall e Età diviso in base al Work Rate----------------------------------------------------------------

# per osservare la correlazione dell'età con l'overall del giocatore
ggplotly(data_clean %>% ggplot(aes(Age,Overall, col=Work.Rate)) +
           geom_smooth() +
           scale_fill_brewer(palette="Spectral") +
           theme_light() +
           labs(title="Overall Rating vs Age",
                x = "Age",
                y = "Overall Rating") + facet_wrap(~Work.Rate))

# Grafico di calore tra Finishing e ShotPower/curve/Vision-------------------------------------------



# geom_density_2d() = grafico della densità di probabilità bidimensionale
# la densità di probabilità bidimensionale rappresenta la distribuzione congiunta delle probabilità di due variabili continue
# fornisce informazioni sulla probabilità relativa di diverse combinazioni di valori per le variabili.


# Finishing vs ShotPower

ggplotly(ggplot(data_clean, aes(x = Finishing, y = ShotPower, col=Work.Rate)) +
           geom_density_2d() +
           labs(title="Finishing  vs ShotPower") +
           scale_fill_brewer(palette="Spectral") +
           theme_light())

# Finishing vs Curve

ggplotly(ggplot(data_clean, aes(x = Finishing, y = Curve, col=Work.Rate)) +
           geom_density_2d() +
           labs(title="Finishing  vs Curve") +
           scale_fill_brewer(palette="Spectral") +
           theme_light())

# Finishing vs Vision

ggplotly(ggplot(data_clean, aes(x = Finishing, y = Vision, col=Work.Rate)) +
           geom_density_2d() +
           labs(title="Finishing  vs Vision") +
           scale_fill_brewer(palette="Spectral") +
           theme_light())

# Migliore giocatore per Posizione -----------------------------------------------------

# funzione per trovare il miglior giocatore per una posizione specifica

find_best_player <- function(position) {
  best_player <- data_clean %>%
    filter(Position == position) %>%
    slice_max(Overall)
  return(best_player)
}

posizioni <- unique(data_clean$Position)

# Creare una lista per salvare i migliori giocatori per ogni posizione
migliori_giocatori <- list()

# Ripetere la funzione find_best_player() per ogni posizione
for (i in posizioni) {
  best_player <- find_best_player(i)
  migliori_giocatori[[i]] <- best_player
}

# Creare un dataframe con i migliori giocatori per ogni posizione
df_migliori_giocatori <- do.call(rbind, migliori_giocatori)

# Creare un grafico complessivo
grafico_1 = ggplotly(ggplot(df_migliori_giocatori, 
                            aes(x = Position, y = Overall, fill = Position, ids=Name)) +
                       geom_bar(stat = "identity") +
                       labs(x = "Position", y = "Overall score", title = "Best Player by Position") +
                       theme(axis.title.x = element_blank()) + # elimina etichetta sull'asse x
                       theme_light())

grafico_1 %>% layout(xaxis = list(showticklabels = FALSE))








# EMANUELE ----------------------------------------------------------------
#  Altezza vs stamina-----------------------------------------------
ggplotly(
  ggplot(data_clean, aes(Height, Stamina)) + # Setta le variabili di base per il grafico (x = Height, y = Stamina)
    geom_smooth(fill = "green") + # Aggiunge una curva di regressione ai dati
    theme_light() + # Setta lo stile del grafico
    labs(title = "Height vs Stamina", x = "Height", y = "Stamina") # Aggiunge i titoli alle assi
)

# Peso vs stamina--------------------------------------------------
ggplotly(
  ggplot(data_clean, aes(Weight, Stamina)) + # Setta le variabili di base per il grafico (x = Weight, y = Stamina)
    geom_smooth(fill = "purple") + # Aggiunge una curva di regressione ai dati
    theme_light() + # Setta lo stile del grafico
    labs(title = "Weight vs Stamina", x = "Weight", y = "Stamina") # Aggiunge i titoli alle assi
)

# Età vs stamina---------------------------------------------------
ggplotly(
  ggplot(data_clean, aes(Age, Stamina)) + # Setta le variabili di base per il grafico (x = Age, y = Stamina)
    geom_smooth(fill = "brown") + # Aggiunge una curva di regressione ai dati
    theme_light() + # Setta lo stile del grafico
    labs(title = "Età vs Stamina", x = "Età", y = "Resistenza") + # Aggiunge i titoli alle assi
    scale_x_continuous(breaks = seq(10, 80, by = 2)) # Setta i valori per l'asse x
)

#---Calcolo della media degli stipendi per ogni nazionalità---------------------
df_total_wage <- data_clean %>%
  group_by(Nationality) %>%
  summarize(Total_Wage = mean(Wage))

# Ordinamento in base alla media degli stipendi decrescente e generazione dei grafici

num_graphs <- ceiling(nrow(df_total_wage) / 30) # calcola il numero totale di grafici necessari, arrotondando verso l'alto il rapporto tra il numero di righe nel dataframe df_total_wage e 30 (il numero di righe per grafico)
plot_list <- list()
for (i in 1:num_graphs) { 
  start_row <- (i - 1) * 30 + 1                                         # Calcola l'indice della prima riga da selezionare per il grafico corrente
  end_row <- min(i * 30, nrow(df_total_wage))                           # Calcola l'indice dell'ultima riga da selezionare per il grafico corrente. L'indice finale viene calcolato come il minimo tra l'indice iniziale del prossimo grafico e il numero totale di righe nel dataframe
  df_subset <- df_total_wage[start_row:end_row, ]                       # Seleziona le righe corrispondenti al grafico corrente, creando un sotto-dataframe df_subset
  graph_title <- paste0("Somma stipendi per nazionalità - Parte ", i)
  
  p <- ggplot(data = df_subset, aes(x = Nationality, y = Total_Wage / 1000)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_line(aes(group = 1), color = "orange") +
    labs(title = graph_title, x = "Nazionalità", y = "Somma stipendio settimanale (in milioni di euro)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4, size = 8)) +
    theme(legend.position = "none")
  
  plot_list[[i]] <- p
}

# Disposizione dei grafici in una griglia
grid.arrange(grobs = plot_list, ncol = 3)     #grobs accetta una lista di oggetti grafici 









