#Fullt datasett

library(tidyverse)
library(PxWebApiData)

tabell_full<-ApiData("https://data.ssb.no/api/v0/no/table/12441/", 
        Kjonn=list('item', c("0", "1", "2")), 
        NACE2007=list('item', c("00-99", "01-03", "05-09", "10-33", "35-39", "41-43", "45-47", "49-53", "55-56", "58-63", "64-66", "68-75", "77-82", "84", "85", "86-88", "90-99", "00")), 
        Sykefraver2=list('item', c("Alt", "E", "L")), 
        ContentsCode=TRUE, 
        Tid=TRUE)

data_sf_full<- tabell_full[[1]]
glimpse(data_sf_full)
names(data_sf_full)

# Rydder i datasettet
data_sf_full<- data_sf_full %>% 
  rename("næringer" = "næring (SN2007)", 
          "sykefraværstyper" = 'type sykefravær',
       "sykefraværsprosent" = "value") %>% 
  select(-c("statistikkvariabel", "NAstatus"))

glimpse(data_sf_full)
names(data_sf_full)

# Alle typer sykefravær, alle næringer, begge kjønn

oversikt<-data_sf_full %>% 
  filter(kjønn == "Begge kjønn", 
         næringer == "Alle næringer")

sf_linje_alle<- 
  ggplot(oversikt,aes(x=år,y=sykefraværsprosent, group=sykefraværstyper))+
  geom_line(aes(color= sykefraværstyper), size = 1.0)+
  geom_point() +
  ylim(0, 10)+
  labs(title = "Figur 1: Sykefravær totalt fordelt på fraværstyper", 
       x = "År", y = "Sykefraværsprosent")
sf_linje_alle

# oversikt kvinner

oversikt_kv<-data_sf_full %>% 
  filter(kjønn == "Kvinner", 
         næringer == "Alle næringer")

sf_linje_kvinner<- 
  ggplot(oversikt_kv,aes(x=år,y=sykefraværsprosent, group=sykefraværstyper))+
  geom_line(aes(color= sykefraværstyper), size = 1.0)+
  geom_point() +
  ylim(0, 10)+
  labs(title = "Figur 2: Sykefravær blant kvinner fordelt på fraværstyper", 
       x = "År", y = "Sykefraværsprosent")
sf_linje_kvinner

# oversikt menn

oversikt_m<-data_sf_full %>% 
  filter(kjønn == "Menn", 
         næringer == "Alle næringer")

sf_linje_menn<- 
  ggplot(oversikt_m,aes(x=år,y=sykefraværsprosent, group=sykefraværstyper))+
  geom_line(aes(color= sykefraværstyper), size = 1.0)+
  geom_point() +
  ylim(0, 10)+
  labs(title = "Figur 3: Sykefravær blant menn fordelt på fraværstyper", 
       x = "År", y = "Sykefraværsprosent")
sf_linje_menn

# oversikt legemeldt fravær fordelt på kjønn

oversikt_legemeldt<-data_sf_full %>% 
  filter(næringer == "Alle næringer", sykefraværstyper == "Legemeldt")

glimpse(oversikt_legemeldt)

sf_linje_legemeldt<- 
  ggplot(oversikt_legemeldt,aes(x=år,y=sykefraværsprosent, group=kjønn))+
  geom_line(aes(color= kjønn), size = 1.0)+
  geom_point() +
  ylim(0, 10)+
  labs(title = "Figur 4: Legemeldt sykefravær fordelt på kjønn", 
       x = "År", y = "Sykefraværsprosent")
sf_linje_legemeldt


