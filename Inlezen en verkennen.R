###########################################################################
##
## Onderwerp: Studentenpanel Studentenwelzijn 2019-2020
## Doel: Inlezen en verkennen van data
##
##
############################################################################

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### VOORBEREIDINGEN ####
library(tidyverse)
library(tidyr)
library(haven)
library(Hmisc)
library(janitor)
library(psych)

## Bepaal netwerklocatie (zodat niet elke keer hele pad moet worden getypt)
Netwerkpad <- "//groepdir.ad.hva.nl/group_mdw_001/BS/IR/Algemeen/22 Studentenpanel/Panelonderzoeken/Studiejaar 2019-2020/Studentenwelzijn/Data/"

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
## INLEZEN #####

Studentenwelzijn <- haven:: read_sav(paste0(Netwerkpad,"Studentenwelzijn.sav"))

Studentenwelzijn_foreign <- foreign::read.spss(paste0(Netwerkpad,"Studentenwelzijn.sav"),
                                               to.data.frame = TRUE)

labels <- setNames(stack(lapply(Studentenwelzijn, label))[2:1], c("Varcode", "Variables"))


## Vul onbekende labels op met de varcode
labels <- labels %>% 
  mutate(Variables = na_if(Variables, ""),
         Varcode = as.character(Varcode)) %>% 
  mutate(Variables = coalesce(Variables, Varcode))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
## BEWERKEN #####

## Recoden van vooropleiding, geslacht, SKC en faculteit
Studentenwelzijn <- Studentenwelzijn %>% 
  mutate(HP_FAC = case_when(HP_FAC == 2 ~ "FBE--Business en Economie",
                            HP_FAC == 1 ~ "FBSV--Bewegen Sport en Voeding",
                            HP_FAC == 5 ~ "FDMCI--Dig Media & Creat Indus",
                            HP_FAC == 3 ~ "FG--Gezondheid",
                            HP_FAC == 4 ~ "FMR--Maatschappij en Recht",
                            HP_FAC == 6 ~ "FOO--Onderwijs en Opvoeding",
                            HP_FAC == 7 ~ "FT--Techniek"),
         HP_GESLACHT = if_else(HP_GESLACHT == 1, "Man", "Vrouw"))

## Maak een versie waarin de kolommen de labels krijgen
Studentenwelzijn2 <- as_tibble(Studentenwelzijn)
colnames(Studentenwelzijn2) <- labels$Variables

## Responsoverzichten
Respons_faculteit <- Studentenwelzijn %>% 
  group_by(HP_FAC) %>% 
  summarise(Aantal=n())%>% 
  mutate(Percentage = paste0(round(Aantal/sum(Aantal)*100, digits = 0), "%"))%>% 
  adorn_totals("row", fill = "100%", name = "Totaal")


Respons_studiejaar <- Studentenwelzijn %>% 
  group_by(as.character(HP_JAAR)) %>% 
  summarise(Aantal=n())%>% 
  mutate(Percentage = paste0(round(Aantal/sum(Aantal)*100, digits = 0), "%"))%>% 
  adorn_totals("row", fill = "100%", name = "Totaal")


Respons_geslacht <- Studentenwelzijn %>% 
  group_by(HP_GESLACHT) %>% 
  summarise(Aantal=n())%>% 
  mutate(Percentage = paste0(round(Aantal/sum(Aantal)*100, digits = 0), "%"))%>% 
  adorn_totals("row", fill = "100%", name = "Totaal")

## Descriptive statistics
Overzichten <- Studentenwelzijn %>% 
  describeBy(group= Studentenwelzijn$HP_FAC)

Overzichten <- map(Overzichten, as.data.frame)

Overzichten_faculteiten <- Overzichten %>% 
  map_df(tibble::rownames_to_column, 'rowname', .id = 'name')

## Bereken gewogen gemiddelde
test <- Studentenwelzijn %>% 
  group_by(HP_FAC) %>% 
  summarise(gemiddelde = weighted.mean(Q1, weight, na.rm = TRUE))


# install.packages("GDAtools")
library(GDAtools)


## V1 (cijfer) per faculteit
prop.wtable(Studentenwelzijn$Q1,Studentenwelzijn$HP_FAC,w=Studentenwelzijn$weight,
            dir = 2, na=FALSE, digits = 0)

prop.table(table(Studentenwelzijn$Q1, Studentenwelzijn$HP_FAC), 2)


test <- as_tibble(prop.wtable(Studentenwelzijn$Q1,Studentenwelzijn$HP_FAC,w=Studentenwelzijn$weight,
                              dir = 2, na=FALSE, digits = 0))


test <- as_tibble(prop.wtable(Studentenwelzijn$HP_GESLACHT,Studentenwelzijn$HP_FAC,w=Studentenwelzijn$weight,
                              dir = 2, na=FALSE, digits = 0))


## Maak een overzicht van de gewogen gekozen antwoorden van vraag Q3_1 in percentages per faculteit
Q3_1 <- as_tibble(prop.wtable(Studentenwelzijn_foreign$Q3_1,Studentenwelzijn_foreign$HP_FAC,w=Studentenwelzijn_foreign$weight,
                              dir = 2, na=FALSE, digits = 0), rownames = "Antwoorden") %>% 
  rename("HvA" = tot) %>% 
  mutate_at(vars(`Faculteit Bewegen, Sport en Voeding`: `HvA`), paste0, "%")


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


## Gemiddelde en SD per faculteit
Gemiddelde_Q1 <- wtd_mean_sd(Studentenwelzijn, Q1, HP_FAC, weight)

## TODO: Bekijken: https://cran.r-project.org/web/packages/compareGroups/vignettes/compareGroups_vignette.html

