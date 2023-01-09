
library(tidyverse)
library(PxWebApiData)
tabell<- ApiData("https://data.ssb.no/api/v0/no/table/12441/", 
        Kjonn=list('item', c("1", "2")), 
        NACE2007=list('item', c("00-99")), 
        Sykefraver2=list('item', c("Alt")), 
        Tid=list('item', c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")), 
        ContentsCode=TRUE)
data_sf<- tabell[[1]]
glimpse(data_sf)
names(data_sf)

test<-data_sf %>% 
  rename("næringer" = "næring (SN2007)", 
         "sykefraværstyper" = 'type sykefravær',
         "sykefraværsprosent" = "value") %>% 
  select(-"statistikkvariabel") %>% 
  filter(kjønn == "Kvinner") 
  



# kolonner 
sf_kolonne_pl<- 
  ggplot(data_sf,aes(x=år,y=value, fill=kjønn))+
  geom_col(position = "dodge")+
  labs(title = "Sykefravær fordelt på kjønn", 
       x = "År", y = "Sykefraværsprosent")
sf_kolonne_pl

# linjer
sf_linje_pl<- 
  ggplot(data_sf,aes(x=år,y=value, group=kjønn))+
  geom_line(aes(color=kjønn), size = 1.0)+
  geom_point() +
  ylim(0, 10)+
  labs(title = "Sykefravær fordelt på kjønn", 
       x = "År", y = "Sykefraværsprosent")
sf_linje_pl
