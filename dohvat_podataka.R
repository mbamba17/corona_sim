library(tidyverse)
library(scales)
library(ggthemes)
library(extrafont)
library(eurostat)
library(lubridate)

# Funkcija za kopiranje u excel ####
skopiraj <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# paleta boja
boje_fill <- scale_fill_manual(values = c("#116979","#ce5a57","#00bdaa","#ffd868","#65587f","#f56c57","#698474","#c06c84","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314","#62374e"))
boje_col <- scale_color_manual(values = c("#116979","#ce5a57","#00bdaa","#ffd868","#65587f","#f56c57","#698474","#c06c84","#01d28e","#cd8d7b","#bbbbbb","#f7be16","#b5525c","#4f81c7","#ff8a5c","#32ff6a","#393e46","#df0054","#f69314","#62374e"))

# loadiranje fontova
# font_import(paths = "C:/Users/bambas/Documents/R/fonts",prompt = F)
# loadfonts(device = "win")

# postavljanje defaultne teme - linen
pozadina <-"#eae2d6"
tekst <- "#212F3C"
gtema <- theme_minimal() + theme(legend.position = "top",panel.background = element_rect(fill=pozadina,linetype = 0),plot.background = element_rect(fill=pozadina,linetype = 0),legend.box.background = element_rect(fill=pozadina,linetype = 0),text = element_text(colour = tekst,family = "Gravity"),plot.caption = element_text(hjust = 0),legend.title = element_blank(),panel.border = element_blank(),axis.line = element_blank(),panel.grid.major = element_line(size = 0.5, linetype = "dashed",colour = tekst), panel.grid.minor = element_line(size = 0.25, linetype = "dotted", colour = tekst))
theme_set(gtema)

# 1. Struktura BDP-a proizvodna metoda ####

# u mlrd. HRK
pom <- get_eurostat(id="namq_10_a10") %>% filter(s_adj=="SCA" & na_item=="B1G" & unit=="CP_MNAC" & geo=="HR") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,sektor=nace_r2,values) %>% filter(sektor!="TOTAL") %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda, šumarstvo i ribarstvo",sektor=="B-E"~"Industrija i rudarstvo",sektor=="C"~"Prerađivačka industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz, smještaj, priprema hrane",sektor=="J"~"Informacije i komunikacije",sektor=="K"~"Financijske djelatnosti",sektor=="L"~"Poslovanje nekretninama",sektor=="M_N"~"Stručne i administrativne djelatnosti",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale uslužne djelatnosti")) %>% spread(sektor,values) %>% mutate(`Industrija i rudarstvo`=`Industrija i rudarstvo`-`Prerađivačka industrija`) %>% gather(key = sektor,value = values,-datum)
ggplot(pom,aes(x=datum,y=values,fill=sektor)) + geom_area() + boje_fill
# u %
pom <- get_eurostat(id="namq_10_a10") %>% filter(s_adj=="SCA" & na_item=="B1G" & unit=="PC_TOT" & geo=="HR") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,sektor=nace_r2,values) %>% filter(sektor!="TOTAL") %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda, šumarstvo i ribarstvo",sektor=="B-E"~"Industrija i rudarstvo",sektor=="C"~"Prerađivačka industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz, smještaj, priprema hrane",sektor=="J"~"Informacije i komunikacije",sektor=="K"~"Financijske djelatnosti",sektor=="L"~"Poslovanje nekretninama",sektor=="M_N"~"Stručne i administrativne djelatnosti",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale uslužne djelatnosti")) %>% spread(sektor,values) %>% mutate(`Industrija i rudarstvo`=`Industrija i rudarstvo`-`Prerađivačka industrija`) %>% gather(key = sektor,value = values,-datum)
ggplot(pom,aes(x=datum,y=values,fill=sektor)) + geom_col() + boje_fill
# usporedba po zemljama
pom <- get_eurostat(id="namq_10_a10") %>% filter(s_adj=="SCA" & na_item=="B1G" & unit=="PC_TOT") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,geo,sektor=nace_r2,values) %>% filter(sektor!="TOTAL" & datum==max(datum)) %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda, šumarstvo i ribarstvo",sektor=="B-E"~"Industrija i rudarstvo",sektor=="C"~"Prerađivačka industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz, smještaj, priprema hrane",sektor=="J"~"Informacije i komunikacije",sektor=="K"~"Financijske djelatnosti",sektor=="L"~"Poslovanje nekretninama",sektor=="M_N"~"Stručne i administrativne djelatnosti",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale uslužne djelatnosti")) %>% spread(sektor,values) %>% mutate(`Industrija i rudarstvo`=`Industrija i rudarstvo`-`Prerađivačka industrija`) %>% gather(key = sektor,value = values,-datum,-geo)
ggplot(pom,aes(x=geo,y=values,fill=sektor)) + geom_col() + boje_fill

# 2. Plaće po djelatnostima ####

# u mlrd. HRK
pom <- get_eurostat(id="namq_10_a10") %>% filter(s_adj=="SCA" & na_item=="D11" & unit=="CP_MNAC" & geo=="HR") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,sektor=nace_r2,values) %>% filter(sektor!="TOTAL") %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda, šumarstvo i ribarstvo",sektor=="B-E"~"Industrija i rudarstvo",sektor=="C"~"Prerađivačka industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz, smještaj, priprema hrane",sektor=="J"~"Informacije i komunikacije",sektor=="K"~"Financijske djelatnosti",sektor=="L"~"Poslovanje nekretninama",sektor=="M_N"~"Stručne i administrativne djelatnosti",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale uslužne djelatnosti")) %>% spread(sektor,values) %>% mutate(`Industrija i rudarstvo`=`Industrija i rudarstvo`-`Prerađivačka industrija`) %>% gather(key = sektor,value = values,-datum)
ggplot(pom,aes(x=datum,y=values,fill=sektor)) + geom_area() + boje_fill
# u %
pom <- get_eurostat(id="namq_10_a10") %>% filter(s_adj=="SCA" & na_item=="D11" & unit=="PC_TOT" & geo=="HR") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,sektor=nace_r2,values) %>% filter(sektor!="TOTAL") %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda, šumarstvo i ribarstvo",sektor=="B-E"~"Industrija i rudarstvo",sektor=="C"~"Prerađivačka industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz, smještaj, priprema hrane",sektor=="J"~"Informacije i komunikacije",sektor=="K"~"Financijske djelatnosti",sektor=="L"~"Poslovanje nekretninama",sektor=="M_N"~"Stručne i administrativne djelatnosti",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale uslužne djelatnosti")) %>% spread(sektor,values) %>% mutate(`Industrija i rudarstvo`=`Industrija i rudarstvo`-`Prerađivačka industrija`) %>% gather(key = sektor,value = values,-datum)
ggplot(pom,aes(x=datum,y=values,fill=sektor)) + geom_col() + boje_fill
# usporedba po zemljama
pom <- get_eurostat(id="namq_10_a10") %>% filter(s_adj=="SCA" & na_item=="D11" & unit=="PC_TOT") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,geo,sektor=nace_r2,values) %>% filter(sektor!="TOTAL" & datum==max(datum)) %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda, šumarstvo i ribarstvo",sektor=="B-E"~"Industrija i rudarstvo",sektor=="C"~"Prerađivačka industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz, smještaj, priprema hrane",sektor=="J"~"Informacije i komunikacije",sektor=="K"~"Financijske djelatnosti",sektor=="L"~"Poslovanje nekretninama",sektor=="M_N"~"Stručne i administrativne djelatnosti",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale uslužne djelatnosti")) %>% spread(sektor,values) %>% mutate(`Industrija i rudarstvo`=`Industrija i rudarstvo`-`Prerađivačka industrija`) %>% gather(key = sektor,value = values,-datum,-geo)
ggplot(pom,aes(x=geo,y=values,fill=sektor)) + geom_col() + boje_fill

# 3. Broj zaposlenih ####

# u 000 osoba
pom <- get_eurostat(id="namq_10_a10_e") %>% filter(s_adj=="SCA" & na_item=="EMP_DC" & unit=="THS_PER" & geo=="HR") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,sektor=nace_r2,values) %>% filter(sektor!="TOTAL") %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda, šumarstvo i ribarstvo",sektor=="B-E"~"Industrija i rudarstvo",sektor=="C"~"Prerađivačka industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz, smještaj, priprema hrane",sektor=="J"~"Informacije i komunikacije",sektor=="K"~"Financijske djelatnosti",sektor=="L"~"Poslovanje nekretninama",sektor=="M_N"~"Stručne i administrativne djelatnosti",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale uslužne djelatnosti")) %>% spread(sektor,values) %>% mutate(`Industrija i rudarstvo`=`Industrija i rudarstvo`-`Prerađivačka industrija`) %>% gather(key = sektor,value = values,-datum)
ggplot(pom,aes(x=datum,y=values,fill=sektor)) + geom_area() + boje_fill
# u %
pom <- get_eurostat(id="namq_10_a10_e") %>% filter(s_adj=="SCA" & na_item=="EMP_DC" & unit=="PC_TOT_PER" & geo=="HR") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,sektor=nace_r2,values) %>% filter(sektor!="TOTAL") %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda, šumarstvo i ribarstvo",sektor=="B-E"~"Industrija i rudarstvo",sektor=="C"~"Prerađivačka industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz, smještaj, priprema hrane",sektor=="J"~"Informacije i komunikacije",sektor=="K"~"Financijske djelatnosti",sektor=="L"~"Poslovanje nekretninama",sektor=="M_N"~"Stručne i administrativne djelatnosti",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale uslužne djelatnosti")) %>% spread(sektor,values) %>% mutate(`Industrija i rudarstvo`=`Industrija i rudarstvo`-`Prerađivačka industrija`) %>% gather(key = sektor,value = values,-datum)
ggplot(pom,aes(x=datum,y=values,fill=sektor)) + geom_area() + boje_fill
# usporedba po zemljama
pom <- get_eurostat(id="namq_10_a10_e") %>% filter(s_adj=="SCA" & na_item=="EMP_DC" & unit=="PC_TOT_PER") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,geo,sektor=nace_r2,values) %>% filter(sektor!="TOTAL" & datum==max(datum)) %>% mutate(sektor=case_when(sektor=="A"~"Poljoprivreda, šumarstvo i ribarstvo",sektor=="B-E"~"Industrija i rudarstvo",sektor=="C"~"Prerađivačka industrija",sektor=="F"~"Građevinarstvo",sektor=="G-I"~"Trgovina, prijevoz, smještaj, priprema hrane",sektor=="J"~"Informacije i komunikacije",sektor=="K"~"Financijske djelatnosti",sektor=="L"~"Poslovanje nekretninama",sektor=="M_N"~"Stručne i administrativne djelatnosti",sektor=="O-Q"~"Javna uprava",sektor=="R-U"~"Ostale uslužne djelatnosti")) %>% spread(sektor,values) %>% mutate(`Industrija i rudarstvo`=`Industrija i rudarstvo`-`Prerađivačka industrija`) %>% gather(key = sektor,value = values,-datum,-geo)
ggplot(pom,aes(x=geo,y=values,fill=sektor)) + geom_col() + boje_fill

# 4. Potrošnja po vrstama dobara ####
pom <- get_eurostat(id="namq_10_fcs") %>% filter(s_adj=="SCA" & unit=="CP_MNAC" & geo=="HR") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% select(datum,na_item,values) %>% mutate(vrsta=case_when(na_item=="P311_S14"~"Trajna dobra",na_item=="P312N_S14"~"Ostala dobra i usluge",T~as.character(na_item))) %>% select(-na_item) %>% filter(vrsta!="P31_S14")
ggplot(pom,aes(x=datum,y=values,fill=vrsta)) + geom_area() + boje_fill
# usporedba po zemljama - ovo treba prilagoditi jer naši nisu unijeli sva polja 
pom <- get_eurostat(id="namq_10_fcs") %>% filter(s_adj=="SCA" & unit=="CP_MNAC") %>% mutate(datum = ceiling_date(time,"quarter")-1) %>% filter(datum==max(datum)) %>% select(datum,geo,na_item,values) %>% mutate(vrsta=case_when(na_item=="P311_S14"~"Trajna dobra",na_item=="P312N_S14"~"Ostala dobra i usluge",T~as.character(na_item))) %>% select(-na_item) %>% filter(vrsta!="P31_S14")
ggplot(pom,aes(x=geo,y=values,fill=vrsta)) + geom_col(position="fill") + boje_fill

# 5. Annual enterprise statistics ####
podaci <- get_eurostat(id="sbs_sc_sca_r2") %>% filter(geo=="HR" & size_emp!="TOTAL") %>% mutate(size_emp=case_when(size_emp=="0-9"~"Micro",size_emp=="10-19"~"Small",size_emp=="20-49"~"Small",size_emp=="50-249"~"Medium",size_emp=="GE250"~"Large"))



################################# Poduzeća za DZS-a ############################

# dohvat i populacija sifrarnika - treba učitati šifrarnik i nadopuniti rupe
library(readxl)
library(zoo)
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
