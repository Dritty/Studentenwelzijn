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
  mutate(HP_FAC = case_when(HP_FAC == 1 ~ "FBE--Business en Economie",
                            HP_FAC== 2 ~ "FBSV--Bewegen Sport en Voeding",
                            HP_FAC == 3 ~ "FDMCI--Dig Media & Creat Indus",
                            HP_FAC == 4 ~ "FG--Gezondheid",
                            HP_FAC == 5 ~ "FMR--Maatschappij en Recht",
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

prop.table(table(Studentenwelzijn$Q4_Q8_1, Studentenwelzijn$HP_FAC), 2)

table(Studentenwelzijn$Q4_Q8_1, Studentenwelzijn$HP_FAC)
