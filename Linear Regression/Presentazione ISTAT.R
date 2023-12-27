
# descrizione variabili ---------------------------------------------------



#c_Ncmp_fatto = numero totale componenti famiglia di fatto
#sesso_1 = sesso componente 1
#pnasc_1 = stato di nascita del componente
#c_etacale_1 = età calcolata componente 1
#cittad_1 = cittadinanza componente 1
#pnasc_padre_1 = stato di nascita del padre del componente 1
#pnasc_madre_1 = stato di nascita della madre del componente 1
#staciv_1 = stato civile del componente 1
#c_titstu_1 = titolo di studio del componente 1

#ci sono NA:
#  (
#    c_ateco_1 = attività economica del componente 1
#    c_pospro_1 = posizione professionale del componente 1
#    pers_dipend_1 = personale alle dipendenze del componente 1
#    orario_1 = orario di lavoro del componente 1
#    rapp_1 = rapporto di lavoro del componente 1
#    contratto_1 = come è regolato il rapporto di lavoro del componente 1
#  )

#c_redd_1 = fonte di reddito del componente 1
#cond_1 = condizione del componente 1
#D179_bf = spesa per assicurazioni sanitarie
#Risecon = risorse economiche attuali 
#Sitecon = variazione risorse economiche
#spe_impreviste = spese impreviste 
#sp_tot_str_aggr_1 = spesa familiare totale
#d_01 = prodotti alimentari e bevande analcoliche
#d_02 = bevande alcoliche e tabacchi
#d_03 = abigliamento e calzature 
#d_05 = mobili, articoli e servizi per la casa
#d_06_aggr_1 = servizi sanitari e spese per la salute 
#d_07 = trasporti
#d_08 = comunicazioni 
#d_09 = ricreazione, spettacoli e cultura 
#d_10 = istruzione
#d_11 = servizi ricettivi e di ristorazione 
#d_12 = altri beni e servizi
#poveri = povertà relativa
#povassc = povertà assoluta
#rgn = regione di residenza
#w_anno = quante famiglie vengono rappresentate da quella famiglia peso di riporto all'universo 
#ratio = rapporto spese sanitarie e totale di spesa = d_06/(p_tot_str_aggr_1-d_01) 


# librerie ----------------------------------------------------------------



library(tidyverse)
library(magrittr)
library(sf)
library(scales)
library(lmerTest)
library(lme4)
library(plotly)
library(gridExtra)


# import e clean ----------------------------------------------------------



data = ISTAT2020_red

data %>% glimpse()
data %>% summary()
data %>% head(10)

data %<>% select(-where(anyNA))
  
data %>% head(10)


# grafici -----------------------------------------------------------------



# grafici mappa italia regioni per spese
  
media_regione <- data %>%
  group_by(rgn) %>%
  summarise(across(c(d_01, d_02, d_03, d_05, d_06_aggr_1, d_07, d_08, d_09, d_10, d_11, d_12), mean))





reg2023 <- st_read("//Users/valeriovalentini/Desktop/R/Data Mining/Presentazioni/Istat2020/Limiti01012023_g/Reg01012023_g")

reg2023 %<>% rename(rgn = COD_REG ) %>% right_join(media_regione)

prodotti = media_regione %>% select(-rgn) %>% names()
  
grafici <- lapply(prodotti, function(x) {
  ggplot(reg2023) +
    geom_sf(aes_string(fill = x), show.legend = TRUE) +
    geom_sf_text(aes_string(label = paste("round(", x, ", 1)")), size = 3) + 
    labs(title="Distribuzione delle spese medie per regione", subtitle="medie mensili per diversi tipi di spese") + 
    scale_fill_gradient(low = "blue", high = "orange") +
    theme_light()
})
grafici



# grafici spese medie per stato di poverta 

spese_poverta = data %>%
  group_by(povassc) %>%
  summarise(across(c(d_01, d_02, d_03, d_05, d_06_aggr_1, d_07, d_08, d_09, d_10, d_11, d_12), mean)) %>%
  pivot_longer(cols = starts_with("d_"), 
               names_to = "Tipo_Spesa", 
               values_to = "Valore_Spesa")





grafico_poverta_spese1 = ggplot(spese_poverta, aes(x = povassc, y = Valore_Spesa, fill = Tipo_Spesa)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(itle = "Spesa Media per Categoria di Povertà",
       x = "Stato di povertà",
       y = "Spesa Media") + scale_fill_brewer(palette = "Spectral") +
  scale_x_discrete(breaks = NULL)  
ggplotly(grafico_poverta_spese1)


grafico_poverta_spese2 = ggplot(spese_poverta, aes(x = Tipo_Spesa, y = Valore_Spesa, fill = povassc)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(itle = "Spesa Media per Categoria di Povertà",
       x = "Tipologia di Spesa",
       y = "Spesa Media", fill = "Stato di Povertà")+
  guides(fill = "none") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(grafico_poverta_spese2)


grid.arrange(grafico_poverta_spese1, grafico_poverta_spese2, nrow = 2)



# grafico spesa media per numero componenti famiglia


spesa_media_per_componenti <- data %>%
  group_by(c_Ncmp_fatto, rgn) %>%
  summarize(Spesa_Media = mean(sp_tot_str_aggr_1)) %>%
  rename(Numero_Componenti = c_Ncmp_fatto, Regioni = rgn)


grafico_barre <- spesa_media_per_componenti %>%
  ggplot(aes(x = Numero_Componenti, y = Spesa_Media, fill = Numero_Componenti)) +
  geom_bar(stat = "identity") +
  labs(x = "Numero di Componenti della Famiglia", y = "Spesa Totale Media", title = "Spesa Media per Numero di Componenti") +
  facet_wrap(~Regioni) + theme_light() + scale_fill_gradient(low = "yellow", high = "brown") + theme(legend.position = "none") +
  scale_x_continuous(breaks = unique(spesa_media_per_componenti$Numero_Componenti))

ggplotly(grafico_barre)











# modello binomiale -------------------------------------------------------



sp_tot_str_aggr_2 = scale(data$sp_tot_str_aggr_1)
comp = scale(data$c_Ncmp_fatto)
cond = scale(data$cond_1)

t.test(data$c_Ncmp_fatto~povassc, data = data)
t.test(data$cond_1~povassc, data = data)
t.test(data$sp_tot_str_aggr_1~povassc, data = data)



mod_bi <- glmer(povassc~comp + as.factor(cond_1)+(1|rgn),family=binomial, data=data, weights = w_anno)
summary(mod_bi)

pred_prob_povass = predict(mod_bi)
data$pred_var_povass = exp(pred_prob_povass)/(1+exp(pred_prob_povass))

fitted(mod_bi)

(tab_conf_ass = table(data$povassc, data$pred_var_povass>=.5))

(accuracy <- sum(diag(tab_conf_ass)) / sum(tab_conf_ass))
(Sensibilita <- tab_conf_ass[2, 2] / sum(tab_conf_ass[2, ]))
(Specificita <- tab_conf_ass[1, 1] / sum(tab_conf_ass[1, ]))

# modello poisson ---------------------------------------------------------


c_etacalc_2 = scale(data$c_etacalc_1)
c_titstu_2 = scale(data$c_titstu_1)
cond_2 = scale(data$cond_1)
D179_bf2 = scale(data$D179_bf)
Sitecon2 = scale(data$Sitecon)
povassc2 = scale(data$povassc)
sp_tot_str_aggr_2 = scale(data$sp_tot_str_aggr_1)


mod_po = glmer(c_Ncmp_fatto~ c_etacalc_2 + c_titstu_2 + D179_bf2  + Sitecon2 + sp_tot_str_aggr_2 + povassc2+  c_titstu_2:povassc2 +(1+ sp_tot_str_aggr_2 + povassc|rgn),family=poisson, data = data, weights = w_anno)
summary(mod_po)

qqnorm(ranef(mod_po)$rgn[,1])
qqline(ranef(mod_po)$rgn[,1])

# modello normale ---------------------------------------------------------


boxplot(data$sp_tot_str_aggr_1)
boxplot(log(data$sp_tot_str_aggr_1))

logsp = log(data$sp_tot_str_aggr_1)

mod_norm = lm(logsp~ as.factor(pnasc_1) + c_etacalc_1 + c_titstu_1   + D179_bf + Sitecon + povassc, data = data, weights = w_anno)

summary(mod_norm)

qqnorm(resid(mod_norm))
qqline(resid(mod_norm))

