library(readxl)
library(zoo)
library(tidyverse)
library(lubridate)
library(eurostat)

# Ovi podaci se skidaju sa DZS -> Strukturne poslovne statistike

# dohvat i populacija sifrarnika - treba učitati šifrarnik i nadopuniti rupe
sfr <- read_excel(path = "poduzeca.xlsx",sheet = "sifrarnik")
sfr$razina1 <- na.locf(sfr$razina1)
sfr$razina2[-1] <- na.locf(sfr$razina2[-1])
sfr$razina3[-c(1,2)] <- na.locf(sfr$razina3[-c(1,2)])
# sad je on popunio sve pa treba ubaciti neke NA-ove kada prelazi iz jedne kategorije u drugu
for(i in 2:nrow(sfr)){
  if(sfr$razina1[i] != sfr$razina1[i-1]){
    sfr$razina2[i] <- NA
    sfr$razina3[i] <- NA
  }
}
for(i in 3:nrow(sfr)){
  if(is.na(sfr$razina2[i-1])){
    sfr$razina3[i] <- NA
  } else if(is.na(sfr$razina2[i])){
    sfr$razina3[i] <- NA
  } else if (sfr$razina2[i] != sfr$razina2[i-1]){
    sfr$razina3[i] <- NA
  }
}
# sad ćemo brojčanim oznakama u razinama pridružiti opisne oznake
pom1 <- sfr %>% filter(is.na(razina2) & is.na(razina3) & is.na(razina4)) %>% select(razina1,razina1_naziv=naziv) %>% distinct()
pom2 <- sfr %>% filter(is.na(razina3) & is.na(razina4)) %>% select(razina1,razina2,razina2_naziv=naziv) %>% distinct() %>% na.omit()
pom3 <- sfr %>% filter(is.na(razina4)) %>% select(razina1,razina2,razina3,razina3_naziv=naziv) %>% distinct() %>% na.omit()
sfr <- sfr %>% left_join(pom1,by=c("razina1")) %>% left_join(pom2,by=c("razina1","razina2")) %>% left_join(pom3,by=c("razina1","razina2","razina3")) %>% mutate(razina4_naziv=naziv) %>% unite("razina1",razina1,razina1_naziv,sep = ". ",remove = T) %>% unite("razina2",razina2,razina2_naziv,sep = ". ",remove = T) %>% unite("razina3",razina3,razina3_naziv,sep = ". ",remove = T) %>% unite("razina4",razina4,razina4_naziv,sep = ". ",remove = T) %>% na.omit() %>% select(-naziv)
# dohvat podataka
temp1 <- read_excel(path = "poduzeca.xlsx", sheet = "bcde") %>% gather(key = "varijabla",value = "iznos",-djelatnost,-godina)
temp2 <- read_excel(path = "poduzeca.xlsx", sheet = "f") %>% gather(key = "varijabla",value = "iznos",-djelatnost,-godina)
temp3 <- read_excel(path = "poduzeca.xlsx", sheet = "g") %>% gather(key = "varijabla",value = "iznos",-djelatnost,-godina)
temp4 <- read_excel(path = "poduzeca.xlsx", sheet = "hijlmns") %>% gather(key = "varijabla",value = "iznos",-djelatnost,-godina)
temp5 <- read_excel(path = "poduzeca.xlsx", sheet = "k") %>% gather(key = "varijabla",value = "iznos",-djelatnost,-godina)
poduzeca=rbind(temp1,temp2,temp3,temp4,temp5) %>% mutate(iznos=ifelse(iznos=="z",NA,ifelse(iznos=="-",NA,iznos))) %>% mutate(iznos=as.numeric(iznos))
# sklapanje sa šiframa
poduzeca <- left_join(poduzeca,sfr,by="djelatnost") %>% na.omit()
rm(temp1,temp2,temp3,temp4,temp5,sfr,i,pom1,pom2,pom3)
# kraljevi su neke tablic množili sa 000, a neke nisu, pa trebamo i to uzeti u obzir
poduzeca <- poduzeca %>% mutate(tisuce=case_when(str_detect(varijabla,"(tis. kuna)")~"da",T~"ne")) %>% mutate(iznos=ifelse(tisuce=="da",iznos*1000,iznos),varijabla=str_remove(varijabla,pattern = "\\ \\(tis.\\ kuna\\)")) %>% mutate(razina0="Poduzeća") %>% select(razina0,razina1,razina2,razina3,razina4,godina,djelatnost,nacerev2,varijabla,iznos)
save(poduzeca,file="poduzeca.Rda")

# analiza
# plaće
pom <- poduzeca  %>% group_by(godina) %>% filter(varijabla=="13320 Nadnice i plaće") %>% summarise(iznos=sum(iznos,na.rm=T))
# broj_zaposlenih
pom <- poduzeca  %>% group_by(godina) %>% filter(varijabla=="16110 Broj zaposlenih osoba") %>% summarise(iznos=sum(iznos,na.rm=T))
# dodana vrijednost - WINNER!
pom <- poduzeca  %>% group_by(godina) %>% filter(varijabla=="12150 Dodana vrijednost prema troškovima proizvodnih čimbenika") %>% summarise(iznos=sum(iznos,na.rm=T))
# bruto poslovni višak
pom <- poduzeca  %>% group_by(godina) %>% filter(varijabla=="12170 Bruto poslovni višak") %>% summarise(iznos=sum(iznos,na.rm=T))

# dodana vrijednost - WINNER!
pom <- poduzeca  %>% group_by(godina,razina1) %>% filter(varijabla=="12150 Dodana vrijednost prema troškovima proizvodnih čimbenika") %>% summarise(iznos=sum(iznos,na.rm=T)) %>% spread(godina,iznos)
skopiraj(pom)

# poslovni višak
pom <- poduzeca  %>% group_by(godina,razina1) %>% filter(godina>=2013 & varijabla=="12110 Promet") %>% summarise(iznos=sum(iznos,na.rm=T)) %>% spread(godina,iznos)
skopiraj(pom)

# ispis BDV-a u excel za pondere

pom <- poduzeca %>% group_by(razina1,razina2,razina3,razina4) %>% filter(godina=="2017" & varijabla=="12150 Dodana vrijednost prema troškovima proizvodnih čimbenika") %>% summarise(iznos=sum(iznos,na.rm=T))
write.xlsx2(data.frame(pom),file = "radni.xlsx",append=T)

# umanjenje bdp-a
ponderi <- read_excel(path = "radni.xlsx",sheet = "umanjenje") %>% mutate(iznos1=iznos*(1+ponder)) %>% group_by(razina1) %>% summarise(iznos=sum(iznos,na.rm=T),iznos1=sum(iznos1,na.rm = T)) %>% mutate(ponder=(iznos1/iznos-1)*100)
skopiraj(ponderi)

bdv <- get_eurostat(id="namq_10_a10") %>% filter(s_adj=="NSA" & na_item=="B1G" & unit=="CP_MNAC" & geo=="HR") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,sektor=nace_r2,values) %>% filter(sektor!="TOTAL") %>% mutate(sektor=case_when(sektor=="A"~"A. Poljoprivreda, šumarstvo i ribarstvo",sektor=="B-E"~" B,D,E. Industrija i rudarstvo",sektor=="C"~"C. Prerađivačka industrija",sektor=="F"~"F. Građevinarstvo",sektor=="G-I"~"G,H,I. Trgovina, prijevoz, smještaj, priprema hrane",sektor=="J"~"J. Informacije i komunikacije",sektor=="K"~"K. Financijske djelatnosti",sektor=="L"~"L. Poslovanje nekretninama",sektor=="M_N"~" M,N. Stručne i administrativne djelatnosti",sektor=="O-Q"~"O,P,Q. Javna uprava",sektor=="R-U"~"R,S,T,U. Ostale uslužne djelatnosti")) %>% arrange(datum,sektor) %>% spread(sektor,values) %>% mutate(` B,D,E. Industrija i rudarstvo`=` B,D,E. Industrija i rudarstvo`-`C. Prerađivačka industrija`)
skopiraj(bdv)
