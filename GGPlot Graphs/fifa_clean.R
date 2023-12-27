
# packages ----------------------------------------------------------------

require(tidyverse)
require(magrittr)


# clean -------------------------------------------------------------------

load("fifa_19_import.rda")

#ripuliamo il datset rimuovendo le righe contenenti NA

data_clean = data %>% filter(if_all(everything(), function(x) !is.na(x)))

# rimuoviamo le variabili che non ci interessano

data_clean %<>% select(c(-(LS:RB),-Special))

# scriviamo per esteso le posizioni

data_clean$Position %>% unique()

acronimi_posizioni <- c("RF", "ST", "LW", "RCM", "LF", "RS", "RCB", "LCM", "CB", "LDM", "CAM", "CDM", "LS", "LCB", "RM", "LM", "LB", "RDM", "RW", "CM", "RB", "RAM", "CF", "LAM", "RWB", "LWB")

posizioni_inglesi <- c("Right_Forward", "Striker", "Left_Wing", "Right_Center_Midfielder", "Left_Forward", "Right_Striker", "Right_Center_Back", "Left_Center_Midfielder", "Center_Back", "Left_Defensive_Midfielder", "Center_Attacking_Midfielder", "Center_Defensive_Midfielder", "Left_Striker", "Left_Center_Back", "Right_Midfielder", "Left_Midfielder", "Left_Back", "Right_Defensive_Midfielder", "Right_Wing", "Center_Midfielder", "Right_Back", "Right_Attacking_Midfielder", "Center_Forward", "Left_Attacking_Midfielder", "Right_Wing_Back", "Left_Wing_Back")


data_clean %<>% mutate(Position = case_when(
  Position == "RF" ~ "Right Forward",
  Position == "LS" ~ "Left_Striker",
  Position == "ST" ~ "Striker",
  Position == "LW" ~ "Left Wing",
  Position == "RCM" ~ "Right Center Midfielder",
  Position == "LF" ~ "Left Forward",
  Position == "RS" ~ "Right Striker",
  Position == "RCB" ~ "Right Center Back",
  Position == "LCM" ~ "Left Center Midfielder",
  Position == "CB" ~ "Center Back",
  Position == "LDM" ~ "Left Defensive Midfielder",
  Position == "CAM" ~ "Center Attacking Midfielder",
  Position == "CDM" ~ "Center Defensive Midfielder",
  Position == "LCB" ~ "Left Center Back",
  Position == "RM" ~ "Right Midfielder",
  Position == "LM" ~ "Left Midfielder",
  Position == "LB" ~ "Left Back",
  Position == "RDM" ~ "Right Defensive Midfielder",
  Position == "RW" ~ "Right Wing",
  Position == "CM" ~ "Center Midfielder",
  Position == "RB" ~ "Right Back",
  Position == "RAM" ~ "Right Attacking Midfielder",
  Position == "CF" ~ "Center Forward",
  Position == "LAM" ~ "Left Attacking Midfielder",
  Position == "RWB" ~ "Right Wing Back",
  Position == "LWB" ~ "Left Wing Back"
))

#trasformiamo le variabili character in factor

data_clean %<>%mutate(across(where(is.character), as.factor))

# controlliamo di non aver introdotto errori durante il clean

data_clean %>% summary()


#salviamo il dataset pulito in un file .rda


save(data_clean, file = "fifa_19_clean.rda")

  
  




