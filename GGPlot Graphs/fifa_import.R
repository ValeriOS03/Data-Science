
# packages ----------------------------------------------------------------

require(tidyverse)


# import ------------------------------------------------------------------

data = read_csv("fifa.csv")

#non c'è stato bisogno di rimuovere righe iniziali ne finali 


# diamo un'occhiata al dataset

data %>% glimpse()

data %>%  summary()


#controlliamo per sicurezza la testa e la coda del nostro dataset

data %>% head(20)

data %>% tail(20)

# salviamo il dataset in formato .rda

save(data, file = "fifa_19_import.rda")
