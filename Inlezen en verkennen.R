## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Onderwerp: Studentenpanel Studentenwelzijn 2019-2020
## Doel: Inlezen en verkennen van data
##
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("C:/Github/Studentenwelzijn/Voorbereidingen.R")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
## INLEZEN #####

Studentenwelzijn <- haven:: read_sav(paste0(Netwerkpad,"Studentenwelzijn.sav"))

## Lees bestand op een manier in waarin labels in cellen staan
Studentenwelzijn_foreign <- foreign::read.spss(paste0(Netwerkpad,"Studentenwelzijn.sav"),
                                               to.data.frame = TRUE)

## Maak legenda:

## Haal kolomlabels uit SPSS file en zet in een tabel
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

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
## VERKENNEN #####

## Responsoverzichten
Respons_faculteit <- Studentenwelzijn %>% 
  group_by(HP_FAC) %>% 
  summarise(Aantal=n())%>% 
  mutate(Percentage = paste0(round(Aantal/sum(Aantal)*100, digits = 0), "%"))%>% 
  adorn_totals("row", fill = "100%", name = "Totaal") %>% 
  arrange(Aantal)

write.csv2(Respons_faculteit, "C:/temp/responsoverzicht.csv", row.names = FALSE)

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



## Gemiddelde en SD per faculteit
Gemiddelde_Q1 <- wtd_mean_sd(Studentenwelzijn, Q1, HP_FAC, weight)


## Histrogram per faculteit met gewogen gemiddelde
df_mean <- Studentenwelzijn %>% 
  group_by(HP_FAC) %>% 
  summarise(mean = weighted.mean(Q1, weight, na.rm = TRUE))

ggplot(Studentenwelzijn, aes(Q1))+geom_histogram(position = "dodge",
                                                                binwidth = 1)+
  facet_grid(.~HP_FAC)+
  geom_vline(data = df_mean, aes(xintercept = mean),
            colour = "red")+
  geom_text(y=Inf, aes(x=mean, label=round(mean, digits = 1)),
            data=df_mean, vjust = 2)+
  scale_x_continuous(breaks=seq(0,10,1))+
  theme_classic()

ggplot(Studentenwelzijn, aes(Q1))+geom_histogram(position = "dodge",
                                                 binwidth = 1)+
  # geom_vline(data = df_mean, aes(xintercept = mean),
  #            colour = "red")+
  # geom_text(y=Inf, aes(x=mean, label=round(mean, digits = 1)),
  #           data=df_mean, vjust = 2)+
  scale_x_continuous(breaks=seq(0,10,1))+
  theme_classic()
