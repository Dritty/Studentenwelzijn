
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### VOORBEREIDINGEN ####

# installeren en laden benodigde packages ------------------------------------------
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(tidyverse, 
               tidyr,
               haven,
               Hmisc,
               janitor,
               psych,
               GDAtools)


## Bepaal netwerklocatie (zodat niet elke keer hele pad moet worden getypt)
Netwerkpad <- "//groepdir.ad.hva.nl/group_mdw_001/BS/IR/Algemeen/22 Studentenpanel/Panelonderzoeken/Studiejaar 2019-2020/Studentenwelzijn/Data/"


## FUNCTIES ####

## Functie om gewogen gemiddelde en SD per groep te berekenen

wtd_mean_sd <- function(df, variabele, group, weight){
  
  group <- enquo(group)
  variabele <- enquo(variabele)
  weight <- enquo(weight)
  
  df <- df %>% 
    group_by(!!group) %>% 
    summarise(Gemiddelde = Hmisc:: wtd.mean(x= !!variabele, weights=!!weight, na.rm = TRUE),
              SD = sqrt(Hmisc::wtd.var(x= !!variabele, weights=!!weight, na.rm = TRUE)))
  
  df
  
}
